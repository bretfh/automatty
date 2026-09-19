;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

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
        (let ((form (format nil "(vt/mux:serve ~S ~S :name ~S :rows ~D :cols ~D)"
                            (socket-path name) command name rows cols)))
          (pty:spawn-in-its-own-session
           (namestring sb-ext:*runtime-pathname*)
           (list "--no-userinit" "--disable-debugger"
                 "--eval" "(require :asdf)"
                 "--eval" "(asdf:load-system :vt/mux)"
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
             "~&vt-mux: the server for ~A took the connection and then said~%~
              nothing. It is most likely older than this client: it has been~%~
              running since whenever, and what it made of what we sent it is in~%~
              ~A. The programs in it are still running.~%"
             name (log-path name)))
    (:no-such-session
     (format *error-output* "~&vt-mux: there is no session called ~A there.~%" name))
    (:server-gone
     (format *error-output* "~&vt-mux: the server for ~A stopped. ~A says why.~%"
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
  (format s "~&vt-mux: many terminals inside one~%~%")
  (format s "  vt-mux                 a shell in a session called 0, made if it is not there~%")
  (format s "  vt-mux <name>          the same, under another name~%")
  (format s "  vt-mux run <name> <command>~%")
  (format s "  vt-mux attach <name>   join a session already running~%")
  (format s "  vt-mux serve <name> <command>   the server itself, in the foreground~%")
  (format s "  vt-mux list            what is running~%")
  (format s "  vt-mux stop <name>     stop a session, and the programs in it~%~%")
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
                 (format *error-output* "~&vt-mux: nothing called ~A is running~%"
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
          ((or (string= what "-h") (string= what "--help")) (usage *standard-output*))
          (t (run :name what))))
    (error (e)
      (format *error-output* "~&vt-mux: ~A~%" e)
      (sb-ext:quit :unix-status 1))))
