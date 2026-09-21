;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun mux-dir ()
  "Where the sockets live, made if it is not there and shut to everybody else.

A socket in it is a way to type into somebody's shell, so who may open the
directory is the whole of the access control there is: the sockets themselves
carry no idea of who is asking."
  (let* ((run (sb-ext:posix-getenv "XDG_RUNTIME_DIR"))
         (dir (ensure-directories-exist
               (pathname (if (and run (plusp (length run)))
                             (format nil "~A/atty/" (string-right-trim "/" run))
                             (format nil "/tmp/atty-~D/" (sb-posix:getuid)))))))
    (ignore-errors (sb-posix:chmod (namestring dir) #o700))
    dir))

(defvar *server-name* nil
  "Which server, when -L said. One server holds every session, the way tmux's
does; a second one is for somebody who asks for it by name.")

(defun a-name (said what)
  "SAID as a file name in the socket directory. A name with a directory in it
would put the socket somewhere nobody is looking for it and under permissions
nobody set."
  (let ((name (file-namestring (princ-to-string said))))
    (when (zerop (length name))
      (error "~S is not a name ~A can have" said what))
    name))

(defun server-name ()
  (let ((env (sb-ext:posix-getenv "ATTY_SERVER")))
    (or *server-name* (and env (plusp (length env)) env) "default")))

(defun socket-path (&optional (name (server-name)))
  "Where the socket for the server called NAME is."
  (namestring (merge-pathnames (a-name name "a server") (mux-dir))))

(defun where-the-server-is ()
  "The server this invocation talks to: the one -L or ATTY_SERVER names, else
the one the pane it is running in belongs to, else the user's own."
  (let ((inside (sb-ext:posix-getenv "ATTY_SOCKET")))
    (if (and (null *server-name*)
             (null (sb-ext:posix-getenv "ATTY_SERVER"))
             inside (plusp (length inside)))
        inside
        (socket-path))))

(defun log-path (&optional (name (server-name)))
  (namestring (merge-pathnames (format nil "~A.log" (a-name name "a server"))
                               (mux-dir))))

(defun knocked (path &optional (patience 3))
  "What a server at PATH says when knocked on, or nil when nothing answers.

Something listening is not the same as a server: one wedged on its way out
still holds its socket, so a connection to it succeeds and then nothing ever
comes back, and a client that took that for a living server would sit at a
screen that never arrives."
  (and (probe-file path)
       (handler-case
           (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
             (sb-bsd-sockets:socket-connect socket path)
             (let ((wire (make-wire (sb-bsd-sockets:socket-file-descriptor socket)
                                    socket))
                   (deadline (+ (get-internal-real-time)
                                (* patience internal-time-units-per-second)))
                   (here nil))
               (unwind-protect
                    (progn
                      (wire-send wire '(:knock))
                      (wire-flush wire)
                      (loop until here
                            do (when (> (get-internal-real-time) deadline) (return))
                               (when (pty:pty-wait (wire-fd wire) 100)
                                 (unless (wire-fill wire) (return))
                                 (loop for form = (wire-take wire)
                                       while form
                                       do (when (and (consp form)
                                                     (eq :here (first form)))
                                            (setf here form))))))
                 (wire-close wire))
               here))
         (error () nil))))

(defun answering-p (path &optional (patience 3))
  (and (knocked path patience) t))

(defun other-servers ()
  "Every other socket here that answers, as (name path legacyp). A legacy one is
a server from before one server held every session: it holds the one session its
name is, and says so by not saying it is the other kind."
  (loop :for path :in (mapcar #'namestring (directory (merge-pathnames "*" (mux-dir))))
        :for name := (file-namestring path)
        ;; by name, not by path: the directory listing answers the path with
        ;; every link resolved (/private/tmp on a mac), which is not how it
        ;; was spelt when it was made
        :for here := (and (not (search ".log" name))
                          (string/= name (file-namestring (where-the-server-is)))
                          (knocked path 1))
        :when here
          :collect (list name path (not (member :one-server here)))))

(defun asked (path forms &key (patience 3) (done (constantly t)))
  "Say FORMS to the server at PATH and gather what it says back until DONE
likes a form or PATIENCE seconds pass. What was said is all sent before the
patience starts: a long prompt is more than a socket takes in one write, and
cutting it off halfway would leave the server holding half a message."
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (sb-bsd-sockets:socket-connect socket path)
        (let ((wire (make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket))
              (heard nil))
          (unwind-protect
               (progn
                 (dolist (form forms) (wire-send wire form))
                 (let ((sending (+ (get-internal-real-time)
                                   (* 10 internal-time-units-per-second))))
                   (loop :until (or (wire-flush wire)
                                    (not (wire-open wire))
                                    (> (get-internal-real-time) sending))
                         :do (sb-unix:unix-simple-poll (wire-fd wire) :output 100)))
                 (let ((deadline (+ (get-internal-real-time)
                                    (* patience internal-time-units-per-second))))
                   (loop
                     (when (> (get-internal-real-time) deadline) (return))
                     (when (pty:pty-wait (wire-fd wire) 100)
                       (unless (wire-fill wire) (return))
                       (loop for form = (wire-take wire)
                             while form
                             do (push form heard)
                                (when (funcall done form)
                                  (return-from asked (nreverse heard))))))))
            (wire-close wire))
          (nreverse heard)))
    (error () nil)))

(defun agents-here ()
  "Every pane in every session of this server, each row led by the server's
path."
  (let* ((path (where-the-server-is))
         (said (asked path '((:agents))
                      :done (lambda (form) (eq :agents (first form))))))
    (mapcar (lambda (row) (cons path row))
            (second (find :agents said :key #'first)))))

(defun pane-address (said)
  (let ((colon (position #\: said :from-end t)))
    (unless (and colon (parse-integer said :start (1+ colon) :junk-allowed t))
      (error "~S is not a pane: a pane is <session>:<number>, as ATTY_PANE says" said))
    (values (subseq said 0 colon) (parse-integer said :start (1+ colon)))))

(defun a-state (said)
  (let ((state (intern (string-upcase said) :keyword)))
    (unless (member state '(:working :blocked :idle))
      (error "~A is not something a program can be doing: working, blocked or idle" said))
    state))

(defun agent-state-in (form id)
  (case (first form)
    (:agent (and (eql (third form) id) (fifth form)))
    (:agents (let ((row (find id (second form) :key #'second)))
               (and row (fourth row))))))

(defun pane-found (address)
  (unless address (error "which pane? <session>:<number>, as atty agent list says"))
  (multiple-value-bind (session id) (pane-address address)
    (let ((row (find-if (lambda (r) (and (string= session (second r)) (eql id (third r))))
                        (agents-here))))
      (unless row (error "there is no pane called ~A running" address))
      (values (first row) session id (fifth row)))))

(defun caller ()
  "The pane this is running in, as ATTY_PANE says, or nil outside one. It goes
last in what an agent verb sends, so a server from before it is not bothered by
it, and the pane that was acted on can say who by."
  (let ((pane (sb-ext:posix-getenv "ATTY_PANE")))
    (and pane (plusp (length pane)) pane)))

(defun unescaped (said)
  (with-output-to-string (out)
    (loop :with i := 0
          :while (< i (length said))
          :do (let ((c (char said i)))
                (if (and (char= c #\\) (< (1+ i) (length said)))
                    (let ((next (char said (1+ i))))
                      (write-char (case next
                                    (#\r #\Return) (#\n #\Newline) (#\t #\Tab)
                                    (#\e (code-char 27)) (t next))
                                  out)
                      (incf i 2))
                    (progn (write-char c out) (incf i)))))))

(defun agent-verbs (args)
  (let ((what (first args)))
    (cond
      ((string= what "read")
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((n (or (and (third args) (parse-integer (third args) :junk-allowed t)) 24))
                (said (asked path (list (list :agent-read session id n))
                             :done (lambda (f) (eq :agent-lines (first f)))))
                (lines (fourth (find :agent-lines said :key #'first))))
           (dolist (line lines) (format t "~&~A~%" line)))))
      ((string= what "explain")
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((said (asked path (list (list :agent-explain session id))
                             :done (lambda (f) (eq :agent-explained (first f)))))
                (form (find :agent-explained said :key #'first)))
           (unless form (error "~A did not say" (second args)))
           (destructuring-bind (published seen rows) (cdddr form)
             (format t "~&~(~A~); the screen says ~(~A~)~%" published seen)
             (dolist (row rows)
               (destructuring-bind (rid priority region state hit text) row
                 (format t "~&~A ~4D ~(~8A~) ~A  ~(~S~)~%"
                         (if hit "*" " ") priority state rid region)
                 (when hit
                   (dolist (line (agent:lines-of text))
                     (format t "~&        ~A~%" line)))))))))
      ((string= what "trace")
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((said (asked path (list (list :agent-trace session id))
                             :done (lambda (f) (eq :agent-traced (first f)))))
                (rows (fourth (find :agent-traced said :key #'first)))
                (first-ms (or (first (first rows)) 0)))
           (dolist (row rows)
             (destructuring-bind (ms moved said state) row
               (format t "~&~8D ~A ~(~A~) ~(~A~)~%"
                       (- ms first-ms) (if moved "+" " ")
                       (if (consp said) (format nil "~A:~A" (first said) (second said)) said)
                       state))))))
      ((string= what "say")
       (unless (third args) (error "atty agent say <session>:<pane> <keys>"))
       (multiple-value-bind (path session id) (pane-found (second args))
         (asked path (list (list :agent-keys session id (unescaped (third args)) (caller)))
                :patience 0)))
      ((string= what "prompt")
       (unless (third args)
         (error "atty agent prompt <session>:<pane> <text> [--until <state>...]"))
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((wanted (mapcar #'a-state
                                (rest (member "--until" (cdddr args) :test #'string=))))
                (said (asked path (list (list :go session)
                                        (list :agent-prompt session id (third args) (caller)))
                             :patience (if wanted 3600 3)
                             :done (lambda (f)
                                     (case (first f)
                                       (:agent-prompted (or (null wanted) (not (eq t (fourth f)))))
                                       (:agent (and (eql (third f) id)
                                                    (member (fifth f) wanted)))))))
                (answer (fourth (find :agent-prompted said :key #'first))))
           (cond
             ((eq answer :blocked)
              (format *error-output*
                      "~&atty: ~A is blocked: it wants an answer, not a prompt.~%~
                       atty agent read it, then atty agent say what it is waiting for.~%"
                      (second args))
              (sb-ext:quit :unix-status 2))
             ((not (eq answer t)) (error "~A is gone" (second args)))
             (wanted
              (let ((state (agent-state-in (car (last said)) id)))
                (if (member state wanted)
                    (format t "~&~(~A~)~%" state)
                    (sb-ext:quit :unix-status 1))))))))
      ((or (null what) (string= what "list"))
       (let ((rows (agents-here)))
         (if rows
             (dolist (row rows)
               (destructuring-bind (path session id kind state) row
                 (declare (ignore path))
                 (format t "~&~A:~D  ~A  ~(~A~)~%" session id kind state)))
             (format t "~&nothing is running~%"))))
      ((string= what "signal")
       (let ((socket (sb-ext:posix-getenv "ATTY_SOCKET"))
             (pane (sb-ext:posix-getenv "ATTY_PANE")))
         (unless (and (second args) socket pane (plusp (length socket)) (plusp (length pane)))
           (error "atty agent signal <working|blocked|idle> is said from inside a pane"))
         (multiple-value-bind (session id) (pane-address pane)
           (asked socket (list (list :agent-signal session id (a-state (second args)) (caller)))
                  :patience 0))))
      ((string= what "wait")
       (unless (second args) (error "atty agent wait <session>:<pane> [<state>...]"))
       (multiple-value-bind (session id) (pane-address (second args))
         (let* ((wanted (or (mapcar #'a-state (cddr args)) '(:blocked :idle)))
                (row (find-if (lambda (r) (and (string= session (second r)) (eql id (third r))))
                              (agents-here))))
           (unless row (error "there is no pane called ~A running" (second args)))
           (let* ((said (asked (first row) (list (list :go session) '(:agents))
                               :patience 3600
                               :done (lambda (form)
                                       (member (agent-state-in form id) wanted))))
                  (state (and said (agent-state-in (car (last said)) id))))
             (if (member state wanted)
                 (format t "~&~(~A~)~%" state)
                 (sb-ext:quit :unix-status 1))))))
      (t (error "atty agent list, wait, read, prompt, say, explain, trace or signal; not ~A" what)))))

(defun self ()
  "This program, when it is a program.

An executable core is its own runtime, so the server a client starts is another
of this told to serve rather than an sbcl told what to load. Out of a repl it is
neither, and there is nothing to run."
  (let ((runtime (and sb-ext:*runtime-pathname*
                      (namestring sb-ext:*runtime-pathname*)))
        (core (and sb-ext:*core-pathname* (namestring sb-ext:*core-pathname*))))
    (when (and runtime core (string= runtime core)) runtime)))

(defun start-a-server ()
  "Start this user's server, holding nothing yet: the client that started it
opens the first session. Answers its path."
  (let ((me (self))
        (path (socket-path)))
    (if me
        (pty:spawn-in-its-own-session me (list "-L" (server-name) "serve")
                                      :output (log-path))
        (let ((form (format nil "(atty:serve ~S ~S :name nil)" path (a-shell))))
          (pty:spawn-in-its-own-session
           (namestring sb-ext:*runtime-pathname*)
           (list "--no-userinit" "--disable-debugger"
                 "--eval" "(require :asdf)"
                 "--eval" "(asdf:load-system :atty)"
                 "--eval" form
                 "--quit")
           :output (log-path))))
    (loop repeat 400
          until (answering-p path)
          do (sleep 0.01))
    path))

(defun the-server ()
  "This user's server, started when it is not running."
  (let ((path (where-the-server-is)))
    (unless (answering-p path)
      (ignore-errors (delete-file path))
      (start-a-server))
    (unless (answering-p path)
      (error "no server came up. ~A says why." (log-path)))
    path))

(defun say-why (name why)
  (case why
    (:detached (format t "~&detached from ~A~%" name))
    (:done nil)
    (:asked-to-stop nil)
    (:stopped (format t "~&~A was stopped~%" name))
    (:no-answer
     (format *error-output*
             "~&atty: the server took the connection and then said nothing.~%~
              It is most likely older than this client: it has been running~%~
              since whenever, and what it made of what we sent it is in~%~
              ~A. The programs in it are still running.~%"
             (log-path)))
    (:no-such-session
     (format *error-output* "~&atty: there is no session called ~A.~%" name))
    (:server-gone
     (format *error-output* "~&atty: the server stopped. ~A says why.~%" (log-path)))
    (t (when why (format t "~&~A~%" why))))
  why)

(defun a-shell ()
  (or (sb-ext:posix-getenv "SHELL") "/bin/sh"))

(defun stop-a-server (path)
  "Ask the server at PATH to go. Answers whether it went.

The programs in it go with it, which is what stopping it means, so it is never
something anything else does on your behalf."
  (and (answering-p path)
       (progn (asked path '((:stop)) :patience 0)
              (loop repeat 300
                    while (answering-p path 1)
                    do (sleep 0.01))
              (not (answering-p path 1)))))

(defun stop-a-session (name)
  "Stop the session called NAME and the programs in it. A server from before
one server held them all is stopped whole, since that is all it holds."
  (let* ((path (where-the-server-is))
         (said (and (answering-p path)
                    (asked path (list (list :kill-session name))
                           :done (lambda (f) (eq :killed (first f))))))
         (killed (third (find :killed said :key #'first))))
    (or killed
        (let ((legacy (find name (other-servers) :key #'first :test #'string=)))
          (and legacy (third legacy) (stop-a-server (second legacy)))))))

(defun the-sessions ()
  "What this server holds, as (name rows cols panes attached blocked) rows."
  (let ((path (where-the-server-is)))
    (and (answering-p path)
         (second (find :these (asked path '((:sessions))
                                     :done (lambda (f) (eq :these (first f))))
                       :key #'first)))))

(defun list-sessions ()
  (let ((rows (the-sessions))
        (others (other-servers)))
    (dolist (row rows)
      (destructuring-bind (name rows cols panes attached &optional (blocked 0)) row
        (declare (ignore rows cols))
        (format t "~&~12A ~D pane~:P  ~D attached~A~%"
                name panes attached
                (if (plusp blocked)
                    (format nil "  ▲ ~D need~A you" blocked (if (= blocked 1) "s" ""))
                    ""))))
    (dolist (other others)
      (destructuring-bind (name path legacyp) other
        (declare (ignore path))
        (if legacyp
            (format t "~&~12A a server from before one held them all; atty stop ~A~%"
                    name name)
            (format t "~&~12A another server; atty -L ~A list~%" name name))))
    (unless (or rows others)
      (format t "~&nothing is running~%"))))

(defun cwd ()
  (ignore-errors (sb-posix:getcwd)))

(defun run (&key (name "0") (command (a-shell)))
  "Join NAME in this user's server, making the server and the session when
they are not there."
  (say-why name (attach (the-server) :name name :open (list command (cwd)))))

(defun usage (s)
  (format s "~&atty: many terminals inside one~%~%")
  (format s "  atty                 a shell in a session called 0, made if it is not there~%")
  (format s "  atty <name>          the same, under another name~%")
  (format s "  atty run <name> <command>   the same, running COMMAND~%")
  (format s "  atty attach [<name>] join a session already running~%")
  (format s "  atty list            the sessions, and how many need you~%")
  (format s "  atty stop <name>     stop a session, and the programs in it~%")
  (format s "  atty kill-server     stop every session~%")
  (format s "  atty serve           the server itself, in the foreground~%")
  (format s "  atty -L <server> ... any of these, against another server than your own~%")
  (format s "  atty agent list      what the program in each pane is doing~%")
  (format s "  atty agent wait <session>:<pane> [<state>...]   until it is blocked or idle~%")
  (format s "  atty agent read <session>:<pane> [<lines>]      what is on its screen~%")
  (format s "  atty agent prompt <session>:<pane> <text> [--until <state>...]   new work~%")
  (format s "  atty agent say <session>:<pane> <keys>   an answer: raw keys, \\r for enter~%")
  (format s "  atty agent explain <session>:<pane>      which rule says what it is doing~%")
  (format s "  atty agent trace <session>:<pane>        every look at it: ms, moved, said, state~%")
  (format s "  atty agent signal <state>   from inside a pane: what its program is doing~%~%")
  (format s "  ~C-b d detaches, ~C-b r redraws, ~C-b ~C-b types a ~C-b,~%"
          #\^ #\^ #\^ #\^ #\^)
  (format s "  ~C-b : runs a command by name and ~C-b ? says what every key does.~%"
          #\^ #\^)
  (format s "  Panes are windows: ~C-b 2 splits below, ~C-b 3 beside, ~C-b 0 closes~%"
          #\^ #\^ #\^)
  (format s "  one, ~C-b 1 leaves only this one, ~C-b o goes to the next.~%"
          #\^ #\^)
  (format s "  ~C-b c starts another session, ~C-b b chooses one, ~C-b t the bar.~%"
          #\^ #\^ #\^))

(defun serve-here (args)
  "atty serve [<name> [<command> [<rows> <cols>]]]: this user's server, in the
foreground. With a name it holds that session from the start."
  (let ((path (socket-path)))
    (when (answering-p path 1)
      (error "a server is already running at ~A; atty run <name> <command> adds ~
              a session to it" path))
    (multiple-value-bind (rows cols)
        (terminal-size tty:+stdin+)
      (let ((said-rows (and (third args) (parse-integer (third args) :junk-allowed t)))
            (said-cols (and (fourth args) (parse-integer (fourth args) :junk-allowed t))))
        (serve path (or (second args) (a-shell))
               :name (first args)
               :rows (or said-rows rows)
               :cols (or said-cols cols))))))

(defun main (&optional (args (rest sb-ext:*posix-argv*)))
  (handler-case
      (let ((*server-name* *server-name*))
        (loop :while (and (first args) (string= (first args) "-L"))
              :do (unless (second args) (error "-L wants the name of a server"))
                  (setf *server-name* (a-name (second args) "a server")
                        args (cddr args)))
        (let ((what (first args)))
          (cond
            ((null what) (run))
            ((string= what "list") (list-sessions))
            ((string= what "stop")
             (let ((name (or (second args) (error "atty stop <name>: which session?"))))
               (if (stop-a-session name)
                   (format t "~&stopped ~A~%" name)
                   (error "nothing called ~A is running" name))))
            ((string= what "kill-server")
             (if (stop-a-server (where-the-server-is))
                 (format t "~&stopped every session~%")
                 (error "no server is running")))
            ((string= what "attach")
             (let ((path (where-the-server-is)))
               (unless (answering-p path)
                 (error "nothing is running"))
               (say-why (or (second args) "the first session")
                        (attach path :name (second args)))))
            ((string= what "serve") (serve-here (rest args)))
            ((string= what "run")
             (run :name (or (second args) "0")
                  :command (or (third args) (a-shell))))
            ((string= what "agent") (agent-verbs (rest args)))
            ((or (string= what "-h") (string= what "--help")) (usage *standard-output*))
            (t (run :name what)))))
    (stream-error ()
      (sb-ext:quit :unix-status 0 :recklessly-p t))
    (error (e)
      (format *error-output* "~&atty: ~A~%" e)
      (sb-ext:quit :unix-status 1))))
