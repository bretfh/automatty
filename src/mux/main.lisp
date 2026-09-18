;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

(defun mux-dir ()
  (let ((run (sb-ext:posix-getenv "XDG_RUNTIME_DIR")))
    (ensure-directories-exist
     (pathname (if (and run (plusp (length run)))
                   (format nil "~A/cl-vt/" (string-right-trim "/" run))
                   (format nil "/tmp/cl-vt-~D/" (sb-posix:getuid)))))))

(defun socket-path (name)
  (namestring (merge-pathnames name (mux-dir))))

(defun log-path (name)
  (namestring (merge-pathnames (format nil "~A.log" name) (mux-dir))))

(defun answering-p (path &optional (patience 3))
  "Whether a server at PATH answers, which is not the same as whether something
is listening there.

A server wedged on its way out still holds its socket, so a connection to it
succeeds and then nothing ever comes back -- a client that took that for a
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

(defun start-a-server (name command &key rows cols)
  (let ((form (format nil "(vt/mux:serve ~S ~S :rows ~D :cols ~D)"
                      (socket-path name) command rows cols)))
    (sb-ext:run-program (namestring sb-ext:*runtime-pathname*)
                        (list "--no-userinit" "--disable-debugger"
                              "--eval" "(require :asdf)"
                              "--eval" "(asdf:load-system :vt/mux)"
                              "--eval" form
                              "--quit")
                        :wait nil
                        :input nil
                        :output (log-path name)
                        :error :output
                        :if-output-exists :append
                        :environment (sb-ext:posix-environ)))
  (loop repeat 400
        until (answering-p (socket-path name))
        do (sleep 0.01))
  (socket-path name))

(defun say-why (name why)
  (case why
    (:detached (format t "~&detached from ~A~%" name))
    (:done nil)
    (:asked-to-stop nil)
    (:server-gone
     (format *error-output* "~&vt-mux: the server for ~A stopped. ~A says why.~%"
             name (log-path name)))
    (t (when why (format t "~&~A~%" why))))
  why)

(defun a-shell ()
  (or (sb-ext:posix-getenv "SHELL") "/bin/sh"))

(defun run (&key (name "0") (command (a-shell)))
  (let ((path (socket-path name)))
    (multiple-value-bind (rows cols)
        (if (a-terminal-p +stdin+) (host-size +stdin+) (values 24 80))
      (unless (answering-p path)
        (ignore-errors (delete-file path))
        (start-a-server name command :rows rows :cols cols))
      (unless (answering-p path)
        (error "no server came up. ~A says why." (log-path name)))
      (say-why name (attach path)))))

(defun usage (s)
  (format s "~&vt-mux -- many terminals inside one~%~%")
  (format s "  vt-mux                 a shell in a session called 0, made if it is not there~%")
  (format s "  vt-mux <name>          the same, under another name~%")
  (format s "  vt-mux run <name> <command>~%")
  (format s "  vt-mux attach <name>   join a session already running~%")
  (format s "  vt-mux serve <name> <command>   the server itself, in the foreground~%")
  (format s "  vt-mux list            what is running~%~%")
  (format s "  ~C-b d detaches, ~C-b r redraws, ~C-b ~C-b types a ~C-b.~%"
          #\^ #\^ #\^ #\^ #\^))

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
          ((string= what "attach")
           (let ((path (socket-path (or (second args) "0"))))
             (unless (answering-p path)
               (error "no session called ~A is running" (or (second args) "0")))
             (say-why (or (second args) "0") (attach path))))
          ((string= what "serve")
           (multiple-value-bind (rows cols)
               (if (a-terminal-p +stdin+) (host-size +stdin+) (values 24 80))
             (serve (socket-path (or (second args) "0"))
                    (or (third args) (a-shell))
                    :rows rows :cols cols)))
          ((string= what "run")
           (run :name (or (second args) "0")
                :command (or (third args) (a-shell))))
          ((or (string= what "-h") (string= what "--help")) (usage *standard-output*))
          (t (run :name what))))
    (error (e)
      (format *error-output* "~&vt-mux: ~A~%" e)
      (sb-ext:quit :unix-status 1))))
