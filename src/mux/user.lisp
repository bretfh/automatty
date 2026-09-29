;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; What somebody's own init.lisp is read in: the keys and modes, the commands,
;;; the settings and the hooks, with nothing else of atty's in the way. The
;;; server loads it, from source, when it starts and when asked to again.

(defpackage #:atty/user
  (:use #:cl #:atty/mode)
  (:import-from #:atty
                #:defcommand #:run-command #:show-note #:*client*
                #:configure #:setting #:settings
                #:add-hook #:remove-hook
                #:pane-mode #:scroll-mode #:board-mode #:note-mode)
  (:export #:defcommand #:run-command #:show-note #:*client*
           #:configure #:setting #:settings #:add-hook #:remove-hook
           #:pane-mode #:scroll-mode #:board-mode #:note-mode))

(in-package #:atty)

(defun config-dir ()
  "Where somebody's own files are: $XDG_CONFIG_HOME/atty/, or ~/.config/atty/."
  (let ((config (sb-ext:posix-getenv "XDG_CONFIG_HOME")))
    (merge-pathnames "atty/"
                     (if (and config (plusp (length config)))
                         (concatenate 'string (string-right-trim "/" config) "/")
                         (merge-pathnames ".config/" (user-homedir-pathname))))))

(defun user-init-file ()
  (merge-pathnames "init.lisp" (config-dir)))

(defparameter +absent+ (make-symbol "ABSENT"))

(defvar *init-changes* nil)

(defun place-table (kind)
  (ecase kind
    (:command *commands*)
    (:doc *command-docs*)
    (:group *command-groups*)
    (:unlisted *unlisted*)))

(defun init-places ()
  (let ((places (make-hash-table :test 'equal)))
    (dolist (mode (atty/mode:modes))
      (loop :for (chord . does) :in (atty/mode:mode-bindings mode)
            :do (setf (gethash (list :key mode chord) places) does)))
    (dolist (kind '(:command :doc :group :unlisted))
      (maphash (lambda (name value) (setf (gethash (list kind name) places) value))
               (place-table kind)))
    (loop :for (key symbol) :in *settings*
          :do (setf (gethash (list :setting key) places) (symbol-value symbol)))
    (loop :for (name symbol) :in *hooks*
          :do (setf (gethash (list :hook name) places) (symbol-value symbol)))
    places))

(defun changes-between (before after)
  (let ((changes nil))
    (maphash (lambda (place was)
               (let ((now (gethash place after +absent+)))
                 (unless (equal was now) (push (list place was now) changes))))
             before)
    (maphash (lambda (place now)
               (unless (nth-value 1 (gethash place before))
                 (push (list place +absent+ now) changes)))
             after)
    changes))

(defun put-place (place value)
  (let ((absent (eq value +absent+)))
    (destructuring-bind (kind name &optional chord) place
      (ecase kind
        (:key (atty/mode:set-binding name chord (unless absent value)))
        (:setting (unless absent (setf (setting name) value)))
        (:hook (unless absent (setf (symbol-value (hook-symbol name)) value)))
        ((:command :doc :group :unlisted)
         (if absent
             (remhash name (place-table kind))
             (setf (gethash name (place-table kind)) value)))))))

(defun undo-changes (changes)
  (loop :for (place was) :in changes
        :when (eq (first place) :setting) :do (ignore-errors (put-place place was)))
  (loop :for (place was) :in changes
        :unless (eq (first place) :setting) :do (put-place place was)))

(defun load-user-init (&optional (file (user-init-file)))
  "Load FILE if there is one, as source, in the user's package. Answers whether
it loaded. One that does not load is said, kept as *INIT-PROBLEM* for whoever
attaches next to be shown, and passed over: a slip in somebody's own keys is
not a reason not to start."
  (setf *init-error* nil)
  (undo-changes *init-changes*)
  (setf *init-changes* nil)
  (let ((before (init-places))
        (loaded nil))
    (when (probe-file file)
      (handler-case (let ((*package* (find-package '#:atty/user)))
                      (load file)
                      (setf loaded t))
        (error (e)
          (setf *init-error* (format nil "~A did not load: ~A" file e))
          (format *error-output* "~&atty: ~A~%" *init-error*))))
    (setf *init-changes* (changes-between before (init-places)))
    loaded))

(defun init-load-note ()
  "What to say after loading the init file: (text face)."
  (cond (*init-error* (list *init-error* :warning))
        ((probe-file (user-init-file))
         (list (format nil "~A loaded" (user-init-file)) :accent))
        (t (list (format nil "there is no ~A" (user-init-file)) :accent))))

(defun encode-settings ()
  (flet ((shown (key value)
           (if (eq key :prefix) (prefix-string value) (prin1-to-string value))))
    (loop :for (key value default doc) :in (settings)
          :collect (list key (shown key value)
                         (and (not (equal value default)) (shown key default))
                         doc))))

(defun init-help (s &optional (rows (encode-settings)) (from :server))
  "What an init file can say, with every setting there is and what it is now."
  (format s "~&~A is loaded by the server when it starts, as source, in the~%~
             package atty/user.~%~%" (user-init-file))
  (format s "  (define-key 'pane-mode \"M-wheel-up\" \"scroll page up\")   a key or button to a command~%")
  (format s "  (undefine-key 'pane-mode \"C-b t\")~%")
  (format s "  (defcommand say-hello () (show-note *client* \"hi\" \"hello\"))  a command of your own~%")
  (format s "  (configure :wheel-rows 6 :prefix \"C-a\")                 a setting~%")
  (format s "  (add-hook 'pane-started (lambda (session pane) ...))~%~%")
  (format s (if (eq from :server)
                "Settings, with what they are now:~%"
                "Settings; no server is running, so these are their defaults:~%"))
  (loop :for (key now default doc) :in rows
        :do (format s "~%  :~(~A~) ~A~@[  (default ~A)~]~%      ~A~%" key now default doc))
  (format s "~%Hooks:~%")
  (loop :for (name nil doc) :in *hooks*
        :do (format s "  ~(~A~)  ~A~%" name doc))
  (format s "~%C-b R has the server load it again.~%"))
