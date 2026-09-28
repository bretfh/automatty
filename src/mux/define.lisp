;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What somebody has: servers, the sessions in them, their windows and the
;;; panes in those, each named. Loading the init keeps what it defines; the
;;; server reading it then makes whatever of its own is not there yet, by
;;; name, and starts the other servers defined that are not running. Nothing
;;; already there is closed or started again, so loading it twice is loading
;;; it once.

(defvar *defining* nil)

(defparameter +part-kinds+ '(:pane :columns :rows :window :session))

(defun options-and-parts (args)
  "ARGS as the keyword options leading them and the parts after, with a list
of parts spliced in and nothing left out, so a LOOP that collects them can
stand where one would."
  (let ((options (loop :while (keywordp (first args))
                       :collect (pop args) :collect (pop args))))
    (values options
            (labels ((flat (it)
                       (cond ((null it) nil)
                             ((stringp it) (list it))
                             ((and (consp it) (member (first it) +part-kinds+)) (list it))
                             ((consp it) (mapcan #'flat it))
                             (t (error "~S is not a pane, a window or a session" it)))))
              (mapcan #'flat args)))))

(defun as-part (it)
  "A string in a layout is a pane running it."
  (if (stringp it) (pane it) it))

(defun pane (&rest args)
  "(pane [command] &key label directory env size hold focus): a pane running
COMMAND, or the shell."
  (let ((command (and (not (keywordp (first args))) (pop args))))
    (list* :pane :command command args)))

(defun columns (&rest args)
  "(columns [:size s] part…): the parts side by side."
  (multiple-value-bind (options parts) (options-and-parts args)
    (list* :columns :parts (mapcar #'as-part parts) options)))

(defun rows (&rest args)
  "(rows [:size s] part…): the parts one above the next."
  (multiple-value-bind (options parts) (options-and-parts args)
    (list* :rows :parts (mapcar #'as-part parts) options)))

(defun window-form (name args)
  (multiple-value-bind (options parts) (options-and-parts args)
    (when (rest parts)
      (error "window ~A holds one part, a pane or columns or rows of them; it was given ~D"
             name (length parts)))
    (list* :window :name (string name)
           :arrangement (and parts (as-part (first parts)))
           options)))

(defun session-form (name args)
  (multiple-value-bind (options parts) (options-and-parts args)
    (list* :session :name (string name)
           :windows (mapcar (lambda (it) (if (eq (first it) :window) it
                                             (error "~S is not a window" it)))
                            parts)
           options)))

(defun defined-name (name)
  (if (stringp name) name (string-downcase (symbol-name name))))

(defmacro defserver (name &body options-and-sessions)
  "(defserver name [:directory d] [:env alist] session…): the server called
NAME, what -L NAME reaches, and what it holds."
  `(define-server ,(defined-name name)
     (let ((*defining* t)) (list ,@options-and-sessions))))

(defmacro session (name &body options-and-windows)
  "(session name [:directory d] [:env alist] window…). In a server's
definition it is part of it; anywhere else it is made there and gone to."
  `(made-or-kept (let ((*defining* t)) (session-form ,name (list ,@options-and-windows)))))

(defmacro window (name &body options-and-arrangement)
  "(window name [:directory d] [:env alist] [:focus t] [arrangement]). In a
session's definition it is part of it; anywhere else it is made in the
session at hand, and shown."
  `(made-or-kept (let ((*defining* t)) (window-form ,name (list ,@options-and-arrangement)))))

(defun define-server (name args)
  (multiple-value-bind (options parts) (options-and-parts args)
    (let ((form (list* :server :name name
                       :sessions (mapcar (lambda (it) (if (eq (first it) :session) it
                                                          (error "~S is not a session" it)))
                                         parts)
                       options)))
      (setf *definitions* (append (remove name *definitions* :key #'car :test #'string=)
                                  (list (cons name form))))
      name)))

(defun made-or-kept (form)
  (if *defining* form (make-here form)))

;;; Where things are.

(defun expand-home (dir)
  (cond ((string= dir "~") (namestring (user-homedir-pathname)))
        ((and (> (length dir) 1) (string= "~/" dir :end2 2))
         (concatenate 'string (namestring (user-homedir-pathname)) (subseq dir 2)))
        (t dir)))

(defun join-directory (parent dir)
  "DIR from PARENT: itself when it is absolute or from home, else under PARENT,
or under home when there is none."
  (if (or (null dir) (zerop (length dir)))
      parent
      (let ((dir (expand-home dir)))
        (namestring
         (uiop:ensure-directory-pathname
          (if (char= #\/ (char dir 0))
              dir
              (merge-pathnames (uiop:ensure-directory-pathname dir)
                               (or parent (user-homedir-pathname)))))))))

(defun join-env (parent env)
  "ENV over PARENT, a name said twice keeping what ENV says."
  (loop :for pair :in (append env parent)
        :for seen := nil :then seen
        :unless (member (car pair) seen :test #'string=)
          :collect pair :and :do (push (car pair) seen)))

;;; Making them.

(defun build-arrangement (form directory env rows cols)
  "FORM as a layout of panes not yet started: (values layout panes focus)."
  (let ((panes nil) (focus nil))
    (labels ((build (it)
               (ecase (first it)
                 (:pane
                  (destructuring-bind (&key command label ((:directory dir)) hold
                                         ((:focus focusp)) ((:env more)) size)
                      (rest it)
                    (declare (ignore size))
                    (let ((p (make-pane (or command (default-shell))
                                        :rows rows :cols cols
                                        :directory (join-directory directory dir))))
                      (setf (pane-label p) label
                            (pane-hold p) (and hold t)
                            (pane-env p) (join-env env more))
                      (when focusp (setf focus p))
                      (push p panes)
                      p)))
                 ((:columns :rows)
                  (let ((parts (getf (rest it) :parts)))
                    (when (null parts) (error "~(~A~) with nothing in it" (first it)))
                    (if (null (rest parts))
                        (build (first parts))
                        (let ((built (mapcar #'build parts))
                              (sizes (mapcar (lambda (part) (getf (rest part) :size)) parts)))
                          (make-split (if (eq (first it) :columns) :across :down)
                                      built (and (some #'identity sizes) sizes)))))))))
      (let ((layout (build (or form (pane)))))
        (values layout (nreverse panes) (or focus (first (layout-panes layout))))))))

(defun start-panes (session panes)
  (dolist (p panes)
    (pane-start p :environment (pane-environment session p))
    (run-hook 'pane-started session p)))

(defun add-defined-window (session form directory env &key show)
  "The window FORM says, after SESSION's others, its panes started."
  (destructuring-bind (&key name arrangement ((:directory dir)) ((:env more)) focus)
      (rest form)
    (let ((directory (join-directory directory dir))
          (env (join-env env more)))
      (multiple-value-bind (layout panes focus-pane)
          (build-arrangement arrangement directory env
                             (session-rows session) (session-cols session))
        (let ((window (%make-window :label name :layout layout :focus focus-pane)))
          (setf (session-windows session) (append (session-windows session) (list window)))
          (when (or show focus (null (session-window session)))
            (session-show-window session window))
          (session-compose session)
          (start-panes session panes)
          (run-hook 'window-made session window)
          (dolist (w (session-watchers session)) (setf (watcher-behind w) t))
          window)))))

(defun room-for-new-session (server)
  "How big a session made with nobody attached starts: as big as one somebody
is looking at, else a terminal's default."
  (let ((seen (find-if #'session-watchers (server-sessions server))))
    (if seen
        (values (session-rows seen) (session-cols seen))
        (values 24 80))))

(defun add-defined-session (server form directory env)
  "The session FORM says, with its windows, in SERVER."
  (destructuring-bind (&key name windows ((:directory dir)) ((:env more)))
      (rest form)
    (multiple-value-bind (rows cols) (room-for-new-session server)
      (let ((directory (join-directory directory dir))
            (env (join-env env more))
            (session (%make-session :name name :rows rows :cols cols
                                    :socket (server-path server)
                                    :bar-p +bar-by-default+
                                    :scrollbars-p +scrollbars-by-default+
                                    :windows nil :window nil :server server
                                    :screen (tty:make-screen :width cols :height rows))))
        (setf (server-sessions server) (append (server-sessions server) (list session))
              (server-had-sessions server) t)
        (dolist (w (or windows (list (list :window :name nil))))
          (add-defined-window session w directory env))
        (run-hook 'session-made session)
        session))))

(defun fill-in-session (server form directory env)
  "SESSION as FORM says, by name: made when it is not there, and when it is,
given the windows it does not have. What is there is left as it is."
  (let ((session (find (getf (rest form) :name) (server-sessions server)
                       :key #'session-name :test #'string=)))
    (if (null session)
        (add-defined-session server form directory env)
        (destructuring-bind (&key windows ((:directory dir)) ((:env more)) &allow-other-keys)
            (rest form)
          (dolist (w windows session)
            (unless (find (getf (rest w) :name) (session-windows session)
                          :key #'window-label :test #'equal)
              (add-defined-window session w (join-directory directory dir)
                                  (join-env env more))))))))

(defun fill-in-server (server form)
  (destructuring-bind (&key sessions directory env &allow-other-keys) (rest form)
    (let ((directory (or (join-directory nil directory)
                         (namestring (user-homedir-pathname)))))
      (dolist (s sessions)
        (handler-case (fill-in-session server s directory env)
          (error (e)
            (report-error e)
            (push (list (format nil "session ~A as the init defines it could not be made: ~A"
                                (getf (rest s) :name) e)
                        :warning)
                  (server-notes server))))))))

(defun server-own-name (server)
  (file-namestring (server-path server)))

(defun starting-file (name)
  (namestring (merge-pathnames (format nil "~A.starting" (server-file-name name "a server"))
                               (socket-directory))))

(defun claim-start (name)
  "Whether this process is the one to start the server called NAME: the first
to make its starting file, or to find one left long enough ago that whoever
made it is not coming."
  (let ((file (starting-file name)))
    (flet ((make-it ()
             (handler-case
                 (progn (sb-posix:close
                         (sb-posix:open file (logior sb-posix:o-creat sb-posix:o-excl
                                                     sb-posix:o-wronly)
                                        #o600))
                        t)
               (sb-posix:syscall-error () nil))))
      (or (make-it)
          (let ((made (ignore-errors (sb-posix:stat-mtime (sb-posix:stat file)))))
            (when (and made (> (- (sb-posix:time) made) 30))
              (ignore-errors (delete-file file))
              (make-it)))))))

(defun start-defined-server (name)
  (let ((*server-name* name))
    (unless (or (server-alive-p (socket-path name) 1)
                (not (claim-start name)))
      (spawn-server (socket-path name)))))

(defun fill-in-definitions (server)
  "Make what the init defines for SERVER, and start every other server it
defines that is not running."
  (let ((me (server-own-name server)))
    (ignore-errors (delete-file (starting-file me)))
    (loop :for (name . form) :in *definitions*
          :do (if (string= name me)
                  (fill-in-server server form)
                  (handler-case (start-defined-server name)
                    (error (e)
                      (push (list (format nil "server ~A could not be started: ~A" name e)
                                  :warning)
                            (server-notes server))))))))

;;; Where a command or a hook is.

(defun current-session ()
  (or *here*
      (and (typep *client* 'watcher) (watcher-session *client*))
      (and *server* (first (server-sessions *server*)))))

(defun current-server ()
  (let ((s (current-session)))
    (if s (session-server s) *server*)))

(defun current-window ()
  (let ((s (current-session))) (and s (session-window s))))

(defun current-pane ()
  (let ((s (current-session))) (and s (session-focus s))))

(defun make-here (form)
  "FORM made where things are now: a window in the session at hand, shown; a
session in the server, gone to. One already there by that name is gone to."
  (ecase (first form)
    (:window
     (let* ((session (or (current-session) (error "there is no session to make a window in")))
            (there (find (getf (rest form) :name) (session-windows session)
                         :key #'window-label :test #'equal)))
       (if there
           (progn (session-show-window session there) there)
           (add-defined-window session form
                               (and (session-focus session) (pane-directory (session-focus session)))
                               nil :show t))))
    (:session
     (let* ((server (or (current-server) (error "there is no server to make a session in")))
            (session (fill-in-session server form nil nil)))
       (when (typep *client* 'watcher)
         (join-session server *client* session))
       session))))

;;; What an init does to panes.

(defun call-watcher (session does pane args)
  (let ((*here* session))
    (handler-case (apply does pane args)
      (error (e)
        (format *error-output* "~&atty: what watches pane ~D came apart: ~A~%" (pane-id pane) e)))))

(defun send-text (pane text)
  "TEXT typed into PANE, as it is."
  (pane-write pane text))

(defparameter +key-bytes+
  '(("RET" . #.(string #\Return)) ("TAB" . #.(string #\Tab)) ("SPC" . " ")
    ("DEL" . #.(string (code-char 127))) ("Escape" . #.(string (code-char 27)))
    ("Up" . #.(format nil "~C[A" (code-char 27))) ("Down" . #.(format nil "~C[B" (code-char 27)))
    ("Right" . #.(format nil "~C[C" (code-char 27))) ("Left" . #.(format nil "~C[D" (code-char 27)))
    ("Home" . #.(format nil "~C[H" (code-char 27))) ("End" . #.(format nil "~C[F" (code-char 27)))
    ("PageUp" . #.(format nil "~C[5~~" (code-char 27)))
    ("PageDown" . #.(format nil "~C[6~~" (code-char 27)))))

(defun key-text (spec)
  "What a terminal sends for the key SPEC says: \"C-c\", \"M-x\", \"RET\", \"Up\"."
  (let* ((key (atty/mode:parse-key spec))
         (sym (atty/mode:key-sym key))
         (plain (or (cdr (assoc sym +key-bytes+ :test #'string=))
                    (if (= 1 (length sym)) sym (error "~S is not a key atty can send" spec))))
         (ctrl (if (and (atty/mode:key-ctrl key) (= 1 (length plain)))
                   (let ((c (char-upcase (char plain 0))))
                     (string (code-char (logand (char-code c) #x1f))))
                   plain)))
    (if (atty/mode:key-meta key) (concatenate 'string (string (code-char 27)) ctrl) ctrl)))

(defun send-keys (pane &rest specs)
  "The keys SPECS name typed into PANE, each spec a key or several with spaces
between: (send-keys pane \"C-c\" \"q RET\")."
  (pane-write pane (apply #'concatenate 'string
                          (loop :for spec :in specs
                                :append (mapcar #'key-text (uiop:split-string spec :separator " "))))))

(defun run-program (program &rest arguments)
  "Run PROGRAM with ARGUMENTS, ~/ in them meaning home, and answer what it
printed, without the newline at the end."
  (uiop:run-program (cons (expand-home program) (mapcar #'expand-home arguments))
                    :output '(:string :stripped t) :error-output nil :ignore-error-status t))

(defun watch-output (pane regex does)
  "Call DOES with PANE and every line its program finishes that REGEX matches."
  (push (list :output (ppcre:create-scanner regex) does) (pane-watches pane))
  does)

(defun when-silent (pane seconds does)
  "Call DOES with PANE each time it has written nothing for SECONDS after it
last did."
  (push (list :silent (* 1000 seconds) does nil) (pane-watches pane))
  does)

(defun when-active (pane seconds does)
  "Call DOES with PANE each time it writes after SECONDS of nothing."
  (push (list :active (* 1000 seconds) does nil) (pane-watches pane))
  does)

(defun pane-check-quiet (session pane now)
  (let ((moved (pane-moved-at pane)))
    (dolist (watch (pane-watches pane))
      (destructuring-bind (kind ms does &optional seen) watch
        (case kind
          (:silent
           (when (and (plusp moved) (> (- now moved) ms) (not (eql seen moved)))
             (setf (fourth watch) moved)
             (call-watcher session does pane nil)))
          (:active
           (cond ((null seen) (setf (fourth watch) moved))
                 ((and (/= moved seen) (> (- moved seen) ms))
                  (setf (fourth watch) moved)
                  (call-watcher session does pane nil))
                 ((/= moved seen) (setf (fourth watch) moved)))))))))
