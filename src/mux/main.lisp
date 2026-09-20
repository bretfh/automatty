;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx)

(defun mux-dir ()
  "Where the sockets live, made if it is not there and shut to everybody else.

A socket in it is a way to type into somebody's shell, so who may open the
directory is the whole of the access control there is: the sockets themselves
carry no idea of who is asking."
  (let* ((run (sb-ext:posix-getenv "XDG_RUNTIME_DIR"))
         (dir (ensure-directories-exist
               (pathname (if (and run (plusp (length run)))
                             (format nil "~A/cl-vt/" (string-right-trim "/" run))
                             (format nil "/tmp/cl-vt-~D/" (sb-posix:getuid)))))))
    (ignore-errors (sb-posix:chmod (namestring dir) #o700))
    dir))

(defun socket-path (name)
  "Where the socket for NAME is. NAME names one session, not a path: a name with
a directory in it would put the socket somewhere nobody is looking for it and
under permissions nobody set."
  (let ((name (file-namestring (princ-to-string name))))
    (when (zerop (length name))
      (error "~S is not a name a session can have" name))
    (namestring (merge-pathnames name (mux-dir)))))

(defun log-path (name)
  (namestring (merge-pathnames (format nil "~A.log" name) (mux-dir))))

(defun answering-p (path &optional (patience 3))
  "Whether a server at PATH answers, which is not the same as whether something
is listening there.

A server wedged on its way out still holds its socket, so a connection to it
succeeds and then nothing ever comes back, and a client that took that for a
living server would sit at a screen that never arrives. So it is knocked on."
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
                                            (setf here t))))))
                 (wire-close wire))
               here))
         (error () nil))))

(defun sessions-here ()
  (remove-if-not #'answering-p
                 (mapcar #'namestring (directory (merge-pathnames "*" (mux-dir))))))

(defun asked (path forms &key (patience 3) (done (constantly t)))
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (sb-bsd-sockets:socket-connect socket path)
        (let ((wire (make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket))
              (deadline (+ (get-internal-real-time)
                           (* patience internal-time-units-per-second)))
              (heard nil))
          (unwind-protect
               (progn
                 (dolist (form forms) (wire-send wire form))
                 (wire-flush wire)
                 (loop
                   (when (> (get-internal-real-time) deadline) (return))
                   (when (pty:pty-wait (wire-fd wire) 100)
                     (unless (wire-fill wire) (return))
                     (loop for form = (wire-take wire)
                           while form
                           do (push form heard)
                              (when (funcall done form) (return-from asked (nreverse heard)))))))
            (wire-close wire))
          (nreverse heard)))
    (error () nil)))

(defun agents-here ()
  (loop :for path :in (sessions-here)
        :append (let ((said (asked path '((:agents))
                                   :done (lambda (form) (eq :agents (first form))))))
                  (mapcar (lambda (row) (cons path row))
                          (second (find :agents said :key #'first))))))

(defun pane-address (said)
  (let ((colon (position #\: said :from-end t)))
    (unless (and colon (parse-integer said :start (1+ colon) :junk-allowed t))
      (error "~S is not a pane: a pane is <session>:<number>, as VTX_PANE says" said))
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
  (unless address (error "which pane? <session>:<number>, as vtx agent list says"))
  (multiple-value-bind (session id) (pane-address address)
    (let ((row (find-if (lambda (r) (and (string= session (second r)) (eql id (third r))))
                        (agents-here))))
      (unless row (error "there is no pane called ~A running" address))
      (values (first row) session id (fifth row)))))

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
       (unless (third args) (error "vtx agent say <session>:<pane> <keys>"))
       (multiple-value-bind (path session id) (pane-found (second args))
         (asked path (list (list :agent-keys session id (unescaped (third args))))
                :patience 0)))
      ((string= what "prompt")
       (unless (third args)
         (error "vtx agent prompt <session>:<pane> <text> [--until <state>...]"))
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((wanted (mapcar #'a-state
                                (rest (member "--until" (cdddr args) :test #'string=))))
                (said (asked path (list (list :go session)
                                        (list :agent-prompt session id (third args)))
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
                      "~&vtx: ~A is blocked: it wants an answer, not a prompt.~%~
                       vtx agent read it, then vtx agent say what it is waiting for.~%"
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
       (let ((socket (sb-ext:posix-getenv "VTX_SOCKET"))
             (pane (sb-ext:posix-getenv "VTX_PANE")))
         (unless (and (second args) socket pane (plusp (length socket)) (plusp (length pane)))
           (error "vtx agent signal <working|blocked|idle> is said from inside a pane"))
         (multiple-value-bind (session id) (pane-address pane)
           (asked socket (list (list :agent-signal session id (a-state (second args))))
                  :patience 0))))
      ((string= what "wait")
       (unless (second args) (error "vtx agent wait <session>:<pane> [<state>...]"))
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
      (t (error "vtx agent list, wait, read, prompt, say, explain, trace or signal; not ~A" what)))))

(defun self ()
  "This program, when it is a program.

An executable core is its own runtime, so the server a client starts is another
of this told to serve rather than an sbcl told what to load. Out of a repl it is
neither, and there is nothing to run."
  (let ((runtime (and sb-ext:*runtime-pathname*
                      (namestring sb-ext:*runtime-pathname*)))
        (core (and sb-ext:*core-pathname* (namestring sb-ext:*core-pathname*))))
    (when (and runtime core (string= runtime core)) runtime)))

(defun start-a-server (name command &key rows cols)
  (let ((me (self)))
    (if me
        (pty:spawn-in-its-own-session
         me
         (list "serve" name command (princ-to-string rows) (princ-to-string cols))
         :output (log-path name))
        (let ((form (format nil "(vtx:serve ~S ~S :name ~S :rows ~D :cols ~D)"
                            (socket-path name) command name rows cols)))
          (pty:spawn-in-its-own-session
           (namestring sb-ext:*runtime-pathname*)
           (list "--no-userinit" "--disable-debugger"
                 "--eval" "(require :asdf)"
                 "--eval" "(asdf:load-system :vtx)"
                 "--eval" form
                 "--quit")
           :output (log-path name)))))
  (loop repeat 400
        until (answering-p (socket-path name))
        do (sleep 0.01))
  (socket-path name))

(defun say-why (name why)
  (case why
    (:detached (format t "~&detached from ~A~%" name))
    (:done nil)
    (:asked-to-stop nil)
    (:no-answer
     (format *error-output*
             "~&vtx: the server for ~A took the connection and then said~%~
              nothing. It is most likely older than this client: it has been~%~
              running since whenever, and what it made of what we sent it is in~%~
              ~A. The programs in it are still running.~%"
             name (log-path name)))
    (:no-such-session
     (format *error-output* "~&vtx: there is no session called ~A there.~%" name))
    (:server-gone
     (format *error-output* "~&vtx: the server for ~A stopped. ~A says why.~%"
             name (log-path name)))
    (t (when why (format t "~&~A~%" why))))
  why)

(defun a-shell ()
  (or (sb-ext:posix-getenv "SHELL") "/bin/sh"))

(defun stop-a-server (name)
  "Ask the server for NAME to go. Answers whether there was one to ask.

The programs in it go with it, which is what stopping it means, so it is never
something anything else does on your behalf."
  (let ((path (socket-path name)))
    (and (answering-p path)
         (handler-case
             (let ((socket (make-instance 'sb-bsd-sockets:local-socket
                                          :type :stream)))
               (sb-bsd-sockets:socket-connect socket path)
               (let ((wire (make-wire (sb-bsd-sockets:socket-file-descriptor socket)
                                      socket)))
                 (unwind-protect
                      (progn (wire-send wire '(:stop)) (wire-flush wire))
                   (wire-close wire)))
               (loop repeat 300
                     while (answering-p path 1)
                     do (sleep 0.01))
               (not (answering-p path 1)))
           (error () nil)))))

(defun run (&key (name "0") (command (a-shell)))
  (let ((path (socket-path name)))
    (multiple-value-bind (rows cols)
        (if (tty:a-terminal-p tty:+stdin+) (tty:host-size tty:+stdin+) (values 24 80))
      (unless (answering-p path)
        (ignore-errors (delete-file path))
        (start-a-server name command :rows rows :cols cols))
      (unless (answering-p path)
        (error "no server came up. ~A says why." (log-path name)))
      (say-why name (attach path :name name)))))

(defun usage (s)
  (format s "~&vtx: many terminals inside one~%~%")
  (format s "  vtx                 a shell in a session called 0, made if it is not there~%")
  (format s "  vtx <name>          the same, under another name~%")
  (format s "  vtx run <name> <command>~%")
  (format s "  vtx attach <name>   join a session already running~%")
  (format s "  vtx serve <name> <command>   the server itself, in the foreground~%")
  (format s "  vtx list            what is running~%")
  (format s "  vtx stop <name>     stop a session, and the programs in it~%")
  (format s "  vtx agent list      what the program in each pane is doing~%")
  (format s "  vtx agent wait <session>:<pane> [<state>...]   until it is blocked or idle~%")
  (format s "  vtx agent read <session>:<pane> [<lines>]      what is on its screen~%")
  (format s "  vtx agent prompt <session>:<pane> <text> [--until <state>...]   new work~%")
  (format s "  vtx agent say <session>:<pane> <keys>   an answer: raw keys, \\r for enter~%")
  (format s "  vtx agent explain <session>:<pane>      which rule says what it is doing~%")
  (format s "  vtx agent trace <session>:<pane>        every look at it: ms, moved, said, state~%")
  (format s "  vtx agent signal <state>   from inside a pane: what its program is doing~%~%")
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

(defun main (&optional (args (rest sb-ext:*posix-argv*)))
  (handler-case
      (let ((what (first args)))
        (cond
          ((null what) (run))
          ((string= what "list")
           (let ((running (sessions-here)))
             (if running
                 (dolist (path running) (format t "~&~A~%" (pathname-name path)))
                 (format t "~&nothing is running~%"))))
          ((string= what "stop")
           (let ((name (or (second args) "0")))
             (if (stop-a-server name)
                 (format t "~&stopped ~A~%" name)
                 (format *error-output* "~&vtx: nothing called ~A is running~%"
                         name))))
          ((string= what "attach")
           (let ((path (socket-path (or (second args) "0"))))
             (unless (answering-p path)
               (error "no session called ~A is running" (or (second args) "0")))
             (say-why (or (second args) "0")
                      (attach path :name (or (second args) "0")))))
          ((string= what "serve")
           (multiple-value-bind (rows cols)
               (if (tty:a-terminal-p tty:+stdin+)
                   (tty:host-size tty:+stdin+)
                   (values 24 80))
             (let ((said-rows (and (fourth args)
                                   (parse-integer (fourth args) :junk-allowed t)))
                   (said-cols (and (fifth args)
                                   (parse-integer (fifth args) :junk-allowed t))))
               (serve (socket-path (or (second args) "0"))
                      (or (third args) (a-shell))
                      :name (or (second args) "0")
                      :rows (or said-rows rows)
                      :cols (or said-cols cols)))))
          ((string= what "run")
           (run :name (or (second args) "0")
                :command (or (third args) (a-shell))))
          ((string= what "agent") (agent-verbs (rest args)))
          ((or (string= what "-h") (string= what "--help")) (usage *standard-output*))
          (t (run :name what))))
    (stream-error ()
      (sb-ext:quit :unix-status 0 :recklessly-p t))
    (error (e)
      (format *error-output* "~&vtx: ~A~%" e)
      (sb-ext:quit :unix-status 1))))
