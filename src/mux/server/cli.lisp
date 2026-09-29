;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun request-sessions ()
  "What this server holds, as (name rows cols panes attached blocked) rows."
  (let ((path (server-socket-path)))
    (and (server-alive-p path)
         (second (find :these (request path '((:sessions))
                                :done (lambda (f) (eq :these (first f))))
                       :key #'first)))))

(defun run-in-server (name arguments)
  "Run the command called NAME with ARGUMENTS in this user's server and print
what it says. Answers t, or the status to exit with when it did not go through,
:no-command when the server has none by that name, and nil when no server
answered."
  (let* ((path (server-socket-path))
         (said (and (server-alive-p path)
                    (find :ran (request path (list (list :run name arguments
                                                         (caller-pane) (cwd)))
                                        :patience 30
                                        :done (lambda (f) (eq :ran (first f))))
                          :key #'first))))
    (cond ((null said) nil)
          ((eq (third said) :no-command) :no-command)
          (t (write-string (second said))
             (finish-output)
             (third said)))))

(defun command-word (word)
  "A command's name as it is typed on a command line: dashes for spaces."
  (substitute #\Space #\- word))

(defun client-name (row)
  "What to call an attached terminal: its tty without the /dev/, else its id."
  (let ((tty (second row)))
    (if (and (stringp tty) (plusp (length tty)))
        (if (and (> (length tty) 5) (string= "/dev/" tty :end2 5)) (subseq tty 5) tty)
      (format nil "client ~D" (first row)))))

(defun event-line (kind actor text)
  "What an event says, in a few words, and who did it when somebody did."
  (format nil "~A~@[ by ~A~]" (format-event kind text) (and actor (caller-actor actor))))

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

;;; Commands a command line runs by name: each says what it has to say, and
;;; that is what the command line prints.

(defcommand (list-sessions :group sessions) ()
  "every session, its windows and panes, how many need you, who is attached"
  (let* ((server (here-server))
         (rows (session-list server))
         (clients (encode-clients server (now-ms))))
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
    (dolist (other (other-servers))
      (destructuring-bind (name path legacyp) other
        (declare (ignore path))
        (if legacyp
            (format t "~&~12A a server from before one held them all; atty stop ~A~%" name name)
            (format t "~&~12A another server; atty -L ~A list~%" name name))))))

(defcommand (list-clients :group sessions) ()
  "every terminal attached, where it is looking, and for how long"
  (let ((rows (encode-clients (here-server) (now-ms))))
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

(defcommand (list-events :group sessions) (&optional n)
  "what happened lately across the server, newest first"
  (let* ((n (or (and n (parse-integer (princ-to-string n) :junk-allowed t)) 20))
         (events (encode-events (here-server) n (now-ms))))
    (if events
        (loop :for (nil clock kind session id window actor text) :in events
              :do (format t "~&~A  ~A ~A:~@[~D.~]~D  ~A~%"
                          (format-clock-hms clock) (event-glyph kind) session window id
                          (event-line kind actor text)))
        (format t "~&nothing has happened yet~%"))))

(defcommand (stop-session :group sessions) (name)
  "stop the session called NAME and the programs in it"
  (let* ((server (here-server))
         (it (session-named server name)))
    (unless it (error "nothing called ~A is running" name))
    (end-session server it :stopped)
    (when (server-saving server) (save-tree server))
    (format t "~&stopped ~A~%" name)))

(defcommand (save :group sessions) ()
  "what every session holds, to disk, now"
  (let ((server (here-server)))
    (when (server-saving server) (save-all server))
    (format t "~&~:[nothing is kept on disk~;saved~]~%" (server-saving server))))
