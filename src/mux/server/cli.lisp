;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun request-stop-session (name)
  "Stop the session called NAME and the programs in it. A server from before
one server held them all is stopped whole, since that is all it holds."
  (let* ((path (server-socket-path))
         (said (and (server-alive-p path)
                    (request path (list (list :kill-session name))
                           :done (lambda (f) (eq :killed (first f))))))
         (killed (third (find :killed said :key #'first))))
    (or killed
        (let ((legacy (find name (other-servers) :key #'first :test #'string=)))
          (and legacy (third legacy) (stop-server (second legacy)))))))

(defun request-sessions ()
  "What this server holds, as (name rows cols panes attached blocked) rows."
  (let ((path (server-socket-path)))
    (and (server-alive-p path)
         (second (find :these (request path '((:sessions))
                                     :done (lambda (f) (eq :these (first f))))
                       :key #'first)))))

(defun request-clients ()
  "Who is attached to this server, as (id tty rows cols session window
since-ms idle-ms) rows."
  (let ((path (server-socket-path)))
    (and (server-alive-p path)
         (second (find :clients (request path '((:clients))
                                       :done (lambda (f) (eq :clients (first f))))
                       :key #'first)))))

(defun client-name (row)
  "What to call an attached terminal: its tty without the /dev/, else its id."
  (let ((tty (second row)))
    (if (and (stringp tty) (plusp (length tty)))
        (if (and (> (length tty) 5) (string= "/dev/" tty :end2 5)) (subseq tty 5) tty)
        (format nil "client ~D" (first row)))))

(defun list-clients ()
  "atty clients: every terminal attached, where it is looking, and for how long."
  (let ((rows (request-clients)))
    (if (null rows)
        (format t "~&nobody is attached~%")
        (dolist (row rows)
          (destructuring-bind (id tty rows cols session window since idle &optional following) row
            (declare (ignore id tty))
            (format t "~&~10A ~Dx~D  ~A~@[ › ~D~]  attached ~A~@[  idle ~A~]~@[  follows ~A~]~%"
                    (client-name row) cols rows (or session "no session") window
                    (format-duration since) (and idle (format-duration idle))
                    (and following (let ((led (find following rows :key #'first)))
                                     (if led (client-name led) following)))))))))

(defun event-line (kind actor text)
  "What an event says, in a few words, and who did it when somebody did."
  (format nil "~A~@[ by ~A~]" (format-event kind text) (and actor (caller-actor actor))))

(defun list-events (args)
  "atty events [<n>]: what happened lately across the server, newest first."
  (let* ((path (server-socket-path))
         (n (or (and (second args) (parse-integer (second args) :junk-allowed t)) 20))
         (events (and (server-alive-p path)
                      (second (find :events (request path (list (list :events n))
                                                   :done (lambda (f) (eq :events (first f))))
                                    :key #'first)))))
    (if events
        (loop :for (nil clock kind session id window actor text) :in events
              :do (format t "~&~A  ~A ~A:~@[~D.~]~D  ~A~%"
                          (format-clock-hms clock) (event-glyph kind) session window id
                          (event-line kind actor text)))
        (format t "~&nothing has happened yet~%"))))

(defun request-rename-session (args)
  "atty rename <old> <new>: call a session something else."
  (destructuring-bind (&optional old new) (rest args)
    (unless (and old new) (error "atty rename <old> <new>: which session, and what to call it?"))
    (let* ((path (server-socket-path))
           (said (and (server-alive-p path)
                      (find :session-named (request path (list (list :name-session old new))
                                                  :done (lambda (f) (eq :session-named (first f))))
                            :key #'first))))
      (case (fourth said)
        ((t) (format t "~&~A is now ~A~%" old new))
        (:taken (error "there is already a session called ~A" new))
        (:empty (error "a session needs a name"))
        (:gone (error "nothing is called ~A" old))
        (t (error "no server answered"))))))

(defun server-version (path)
  "Which build the server at PATH is, when one answers: it says so when
knocked. A server from before this said nothing, and is \"unknown\"."
  (let ((here (probe-socket path 1)))
    (and here (or (fourth here) "unknown"))))

(defun platform-name ()
  "The system and the machine this build is for, said the way uname says
them, since that is how make names a release: darwin-arm64, linux-x86_64."
  (format nil "~A-~A"
          #+darwin "darwin" #+linux "linux" #-(or darwin linux) (string-downcase (software-type))
          #+(and arm64 darwin) "arm64" #+(and arm64 (not darwin)) "aarch64"
          #+x86-64 "x86_64" #-(or arm64 x86-64) (string-downcase (machine-type))))

(defun print-version ()
  "atty version: this build, the platform it is for, and the server's build
when a server is running and is another."
  (format t "~&atty ~A (~A, ~A ~A)~%"
          *version* (platform-name)
          (lisp-implementation-type) (lisp-implementation-version))
  (let ((theirs (server-version (server-socket-path))))
    (when (and theirs (not (equal theirs *version*)))
      (format t "~&the server is ~A; atty restart-server starts it again from this build~%"
              theirs))))

(defun list-sessions ()
  (let ((rows (request-sessions))
        (clients (request-clients))
        (others (other-servers)))
    (dolist (row rows)
      (destructuring-bind (name rows cols panes attached &optional (blocked 0) windows) row
        (declare (ignore rows cols attached))
        (let ((here (remove name clients :key #'fifth :test-not #'equal)))
          (format t "~&~12A ~D window~:P  ~D pane~:P~A~@[  ~{~A~^, ~}~]~%"
                  name (max 1 (length windows)) panes
                  (if (plusp blocked)
                      (format nil "  ▲ ~D need~A you" blocked (if (= blocked 1) "s" ""))
                      "")
                  (mapcar #'client-name here)))))
    (dolist (other others)
      (destructuring-bind (name path legacyp) other
        (declare (ignore path))
        (if legacyp
            (format t "~&~12A a server from before one held them all; atty stop ~A~%"
                    name name)
            (format t "~&~12A another server; atty -L ~A list~%" name name))))
    (unless (or rows others)
      (let ((saved (saved-sessions)))
        (if saved
            (loop :for (name windows panes at) :in saved
                  :do (format t "~&~12A ~D window~:P  ~D pane~:P  saved ~A; not running, atty ~A brings it back~%"
                              name windows panes (format-day-time at) name))
            (format t "~&nothing is running~%"))))
    (let ((theirs (and rows (server-version (server-socket-path)))))
      (when (and theirs (not (equal theirs *version*)))
        (format t "~&the server is ~A and this atty is ~A; atty restart-server~%"
                theirs *version*)))))

(defun stop-session (name)
  "atty stop <name>: stop the session, or forget it when only saved."
  (cond ((request-stop-session name)
         (format t "~&stopped ~A~%" name))
        ;; not running, but kept on disk: stopping it is forgetting it
        ((delete-saved-session name)
         (format t "~&~A was not running; what was saved of it is gone~%" name))
        (t (error "nothing called ~A is running or saved" name))))
