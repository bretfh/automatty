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

;;; The agent verbs: everything the panes' UI does, from a command line, so a
;;; program in a pane can do for another what a person does at the keyboard.

(defun flag-p (args name)
  (and (member name args :test #'string=) t))

(defun option (args name)
  "The word after NAME in ARGS, or nil."
  (second (member name args :test #'string=)))

(defun words (args &rest options)
  "ARGS without its flags, and without each of OPTIONS and the word after it."
  (loop :with skip := nil
        :for a :in args
        :if skip :do (setf skip nil)
        :else :if (member a options :test #'string=) :do (setf skip t)
        :else :unless (and (> (length a) 2) (string= "--" a :end2 2)) :collect a))

(defun the-panes ()
  "Every pane of this server as the plist it tells a client."
  (second (find :panes (asked (where-the-server-is) '((:panes))
                              :done (lambda (f) (eq :panes (first f))))
                :key #'first)))

(defun json (thing out)
  "THING as JSON: a plist is an object, a list an array, a keyword a string."
  (cond ((null thing) (write-string "null" out))
        ((eq thing t) (write-string "true" out))
        ((numberp thing) (format out "~D" thing))
        ((keywordp thing) (json (string-downcase (symbol-name thing)) out))
        ((stringp thing)
         (write-char #\" out)
         (loop :for c :across thing
               :do (case c
                     (#\" (write-string "\\\"" out))
                     (#\\ (write-string "\\\\" out))
                     (#\Newline (write-string "\\n" out))
                     (#\Return (write-string "\\r" out))
                     (#\Tab (write-string "\\t" out))
                     (t (if (< (char-code c) 32)
                            (format out "\\u~4,'0X" (char-code c))
                            (write-char c out)))))
         (write-char #\" out))
        ((and (consp thing) (keywordp (first thing)) (evenp (length thing)))
         (write-char #\{ out)
         (loop :for (key value) :on thing :by #'cddr
               :for first := t :then nil
               :do (unless first (write-char #\, out))
                   (json (substitute #\_ #\- (string-downcase (symbol-name key))) out)
                   (write-char #\: out)
                   (json value out))
         (write-char #\} out))
        ((consp thing)
         (write-char #\[ out)
         (loop :for it :in thing
               :for first := t :then nil
               :do (unless first (write-char #\, out))
                   (json it out))
         (write-char #\] out))
        (t (json (princ-to-string thing) out))))

(defun list-agents (args)
  "atty agent list [--blocked] [--session <name>] [--json]"
  (let* ((rows (the-panes))
         (rows (if (flag-p args "--blocked")
                   (remove-if-not (lambda (r) (eq :blocked (getf r :state))) rows)
                   rows))
         (rows (let ((only (option args "--session")))
                 (if only (remove-if-not (lambda (r) (equal only (getf r :session))) rows) rows))))
    (cond
      ((flag-p args "--json")
       (json (mapcar (lambda (r)
                       (list :address (format nil "~A:~D" (getf r :session) (getf r :id))
                             :session (getf r :session) :id (getf r :id)
                             :name (getf r :label) :says (getf r :says) :kind (getf r :kind)
                             :state (getf r :state) :for-ms (getf r :for)
                             :asks (let ((asks (getf r :asks)))
                                     (and asks
                                          (list :subject (getf asks :subject)
                                                :question (getf asks :question)
                                                :detail (getf asks :detail)
                                                :options (mapcar (lambda (o)
                                                                   (list :n (first o)
                                                                         :text (second o)))
                                                                 (getf asks :options)))))
                             :doing (getf r :doing)
                             :driven-by (getf r :driven-by)
                             :queued (getf r :queued)))
                     rows)
             *standard-output*)
       (terpri))
      ((null rows) (format t "~&nothing is running~%"))
      (t
       (let* ((addresses (mapcar (lambda (r) (format nil "~A:~D" (getf r :session) (getf r :id))) rows))
              (names (mapcar (lambda (r) (or (getf r :label) "-")) rows))
              (wa (reduce #'max addresses :key #'length))
              (wn (reduce #'max names :key #'length))
              (wk (reduce #'max rows :key (lambda (r) (length (getf r :kind))))))
         (loop :for r :in rows
               :for address :in addresses
               :for name :in names
               :do (format t "~&~vA  ~vA  ~vA  ~(~7A~)  ~4A~@[  ~A~]~%"
                           wa address wn name wk (getf r :kind) (getf r :state)
                           (duration (getf r :for))
                           (getf (getf r :asks) :subject))))))))

(defun spawn-agent (args)
  "atty agent spawn <session> [--name <name>] [--cwd <dir>] -- <command>"
  (let* ((dashes (member "--" args :test #'string=))
         (before (ldiff args dashes))
         (session (first (words before "--name" "--cwd")))
         (command (format nil "~{~A~^ ~}" (or (rest dashes) (rest (words before "--name" "--cwd"))))))
    (unless session
      (error "atty agent spawn <session> [--name <name>] [--cwd <dir>] -- <command>"))
    (let* ((path (the-server))
           (said (asked path (list (list :spawn session
                                         (if (plusp (length command)) command (a-shell))
                                         (or (option before "--cwd") (cwd))
                                         (option before "--name")))
                        :done (lambda (f) (eq :spawned (first f)))))
           (id (third (find :spawned said :key #'first))))
      (unless id (error "no pane was made in ~A" session))
      (format t "~&~A:~D~%" session id))))

(defun name-agent (args)
  "atty agent name <pane> <name>, or from inside a pane atty agent name <name>"
  (multiple-value-bind (address label)
      (if (third args)
          (values (second args) (third args))
          (values (or (caller) (error "atty agent name <session>:<pane> <name>")) (second args)))
    (multiple-value-bind (path session id) (pane-found address)
      (asked path (list (list :name-pane session id (or label "")))
             :done (lambda (f) (eq :named (first f))))
      (format t "~&~A:~D is ~A~%" session id (if (plusp (length (or label ""))) label "unnamed")))))

(defun answer-agent (args)
  "atty agent answer <pane> <n>: type answer N to what the pane is asking. Exits
3, having typed nothing, when it is not asking or has no such answer."
  (let ((n (and (third args) (parse-integer (third args) :junk-allowed t))))
    (unless n (error "atty agent answer <session>:<pane> <n>"))
    (multiple-value-bind (path session id) (pane-found (second args))
      (let ((outcome (fifth (find :answered
                                  (asked path (list (list :answer session id n (caller)))
                                         :done (lambda (f) (eq :answered (first f))))
                                  :key #'first))))
        (case outcome
          ((t) (format t "~&answered ~D~%" n))
          (:not-blocked
           (format *error-output* "~&atty: ~A is not blocked; nothing was typed.~%" (second args))
           (sb-ext:quit :unix-status 3))
          (:no-such-option
           (format *error-output* "~&atty: ~A has no answer ~D; nothing was typed.~%"
                   (second args) n)
           (sb-ext:quit :unix-status 3))
          (t (error "~A is gone" (second args))))))))

(defun who-said-here (who)
  (case (first who)
    (:pane (second who))
    (:client (or (third who) (format nil "client ~D" (second who))))
    (t "the command line")))

(defun log-of-agent (args)
  "atty agent log <pane> [<n>]: who typed into it, newest first."
  (multiple-value-bind (path session id) (pane-found (second args))
    (let* ((n (or (and (third args) (parse-integer (third args) :junk-allowed t)) 20))
           (entries (fourth (find :pane-log
                                  (asked path (list (list :pane-log session id n))
                                         :done (lambda (f) (eq :pane-log (first f))))
                                  :key #'first))))
      (if entries
          (loop :for (age who verb summary outcome) :in entries
                :do (format t "~&~A  ~12A ~(~7A~) ~A~:[~;  refused~]~%"
                            (wall-clock-ago age) (who-said-here who) verb
                            (if (eq verb :keys) (format nil "~D bytes" summary) (or summary ""))
                            (eq outcome :refused)))
          (format t "~&nobody has typed into ~A:~D~%" session id)))))

(defun done-since-prompt-p (said wanted)
  "Whether what :SINCE-PROMPT SAID shows the pane in a WANTED state it came to
after the last prompt: not an idle left over from before it, nor the idle the
paste itself makes before the program has started on it."
  (destructuring-bind (state prompted history) said
    (let ((since (first (first history))))
      (and prompted since
           (member state wanted)
           (< since prompted)
           (or (not (eq state :idle))
               (some (lambda (h) (and (< (first h) prompted) (> (first h) since)
                                      (member (second h) '(:working :blocked))))
                     history))))))

(defun wait-after-prompt (path session id wanted)
  (loop :with deadline := (+ (get-internal-real-time) (* 3600 internal-time-units-per-second))
        :for said := (fourth (find :since-prompt
                                   (asked path (list (list :since-prompt session id))
                                          :done (lambda (f) (eq :since-prompt (first f))))
                                   :key #'first))
        :do (cond ((null said) (error "~A:~D is gone" session id))
                  ((null (second said)) (error "nothing was ever prompted into ~A:~D" session id))
                  ((done-since-prompt-p said wanted)
                   (format t "~&~(~A~)~%" (first said))
                   (return))
                  ((> (get-internal-real-time) deadline) (sb-ext:quit :unix-status 1)))
            (sleep 0.3)))

(defun agent-verbs (args)
  (let ((what (first args)))
    (cond
      ((or (null what) (string= what "list")) (list-agents (rest args)))
      ((string= what "spawn") (spawn-agent (rest args)))
      ((string= what "name") (name-agent args))
      ((string= what "answer") (answer-agent args))
      ((string= what "log") (log-of-agent args))
      ((string= what "read")
       (let ((words (words args)))
         (multiple-value-bind (path session id) (pane-found (second words))
           (let* ((n (or (and (third words) (parse-integer (third words) :junk-allowed t)) 24))
                  (said (asked path (list (list :agent-read session id n
                                                (flag-p args "--plain")))
                               :done (lambda (f) (eq :agent-lines (first f)))))
                  (lines (fourth (find :agent-lines said :key #'first))))
             (dolist (line lines) (format t "~&~A~%" line))))))
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
                       atty agent read it, then atty agent answer it.~%"
                      (second args))
              (sb-ext:quit :unix-status 2))
             ((not (eq answer t)) (error "~A is gone" (second args)))
             (wanted
              (let ((state (agent-state-in (car (last said)) id)))
                (if (member state wanted)
                    (format t "~&~(~A~)~%" state)
                    (sb-ext:quit :unix-status 1))))))))
      ((string= what "signal")
       (let ((socket (sb-ext:posix-getenv "ATTY_SOCKET"))
             (pane (sb-ext:posix-getenv "ATTY_PANE")))
         (unless (and (second args) socket pane (plusp (length socket)) (plusp (length pane)))
           (error "atty agent signal <working|blocked|idle> is said from inside a pane"))
         (multiple-value-bind (session id) (pane-address pane)
           (asked socket (list (list :agent-signal session id (a-state (second args)) (caller)))
                  :patience 0))))
      ((string= what "wait")
       (let ((words (words args)))
         (unless (second words) (error "atty agent wait <session>:<pane> [<state>...] [--after-prompt]"))
         (multiple-value-bind (path session id) (pane-found (second words))
           (let ((wanted (or (mapcar #'a-state (cddr words)) '(:blocked :idle))))
             (if (flag-p args "--after-prompt")
                 (wait-after-prompt path session id wanted)
                 (let* ((said (asked path (list (list :go session) '(:agents))
                                     :patience 3600
                                     :done (lambda (form)
                                             (member (agent-state-in form id) wanted))))
                        (state (and said (agent-state-in (car (last said)) id))))
                   (if (member state wanted)
                       (format t "~&~(~A~)~%" state)
                       (sb-ext:quit :unix-status 1))))))))
      (t (error "atty agent list, spawn, name, wait, read, prompt, answer, say, log, ~
                 explain, trace or signal; not ~A" what)))))

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

(defun run (&key (name "0") (command (a-shell)) label)
  "Join NAME in this user's server, making the server and the session when
they are not there, its first pane called LABEL."
  (say-why name (attach (the-server) :name name :open (list command (cwd) label))))

(defun usage (s)
  (format s "~&atty: many terminals inside one~%~%")
  (format s "  atty                 a shell in a session called 0, made if it is not there~%")
  (format s "  atty <name>          the same, under another name~%")
  (format s "  atty run <name> <command> [--name <n>]   the same, running COMMAND~%")
  (format s "  atty attach [<name>] join a session already running~%")
  (format s "  atty list            the sessions, and how many need you~%")
  (format s "  atty stop <name>     stop a session, and the programs in it~%")
  (format s "  atty kill-server     stop every session~%")
  (format s "  atty serve           the server itself, in the foreground~%")
  (format s "  atty -L <server> ... any of these, against another server than your own~%")
  (format s "  atty agent list [--blocked] [--session <s>] [--json]   what every pane is doing~%")
  (format s "  atty agent spawn <session> [--name <n>] [--cwd <d>] -- <command>   a new pane~%")
  (format s "  atty agent name <session>:<pane> <name>    what to call it~%")
  (format s "  atty agent wait <session>:<pane> [<state>...] [--after-prompt]   until it is~%")
  (format s "                       blocked or idle; after a prompt, only once it took it up~%")
  (format s "  atty agent read <session>:<pane> [<lines>] [--plain]   what is on its screen;~%")
  (format s "                       --plain leaves out what is only suggested, drawn faint~%")
  (format s "  atty agent prompt <session>:<pane> <text> [--until <state>...]   new work~%")
  (format s "  atty agent answer <session>:<pane> <n>   answer n to what it asks; exit 3 if~%")
  (format s "                       it is asking nothing~%")
  (format s "  atty agent say <session>:<pane> <keys>   raw keys, \\r for enter~%")
  (format s "  atty agent log <session>:<pane> [<n>]    who typed into it~%")
  (format s "  atty agent explain <session>:<pane>      which rule says what it is doing~%")
  (format s "  atty agent trace <session>:<pane>        every look at it: ms, moved, said, state~%")
  (format s "  atty agent signal <state>   from inside a pane: what its program is doing~%~%")
  (format s "  ~C-b n what needs you, ~C-b w every pane, ~C-b a the one blocked longest,~%"
          #\^ #\^ #\^)
  (format s "  ~C-b e why a pane is what it is, ~C-b z zoom, ~C-b , name a pane.~%"
          #\^ #\^ #\^)
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
             (let ((words (words (rest args) "--name")))
               (run :name (or (first words) "0")
                    :command (or (second words) (a-shell))
                    :label (option args "--name"))))
            ((string= what "agent") (agent-verbs (rest args)))
            ((or (string= what "-h") (string= what "--help")) (usage *standard-output*))
            (t (run :name what)))))
    (stream-error ()
      (sb-ext:quit :unix-status 0 :recklessly-p t))
    (error (e)
      (format *error-output* "~&atty: ~A~%" e)
      (sb-ext:quit :unix-status 1))))
