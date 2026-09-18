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

(defun answering-p (path)
  (and (probe-file path)
       (handler-case
           (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
             (sb-bsd-sockets:socket-connect socket path)
             (sb-bsd-sockets:socket-close socket)
             t)
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
      (attach path))))

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
             (attach path)))
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
