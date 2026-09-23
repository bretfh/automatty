;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; What somebody's own init.lisp is read in: the keys and modes, the commands,
;;; the settings and the hooks, with nothing else of atty's in the way. It is
;;; loaded when atty starts, in the client and in the server alike, from source.

(defpackage #:atty/user
  (:use #:cl #:atty/mode)
  (:import-from #:atty
                #:defcommand #:run-command #:send-to-server #:show-note #:*client*
                #:configure #:setting #:settings
                #:add-hook #:remove-hook
                #:pane-mode #:scroll-mode #:board-mode #:note-mode)
  (:export #:defcommand #:run-command #:send-to-server #:show-note #:*client*
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

(defvar *init-generation* 0)

(defvar *config-changes* nil)

(defun place-table (kind)
  (ecase kind
    (:command *commands*)
    (:doc *command-docs*)
    (:group *command-groups*)
    (:unlisted *unlisted*)
    (:init-command *init-commands*)))

(defun init-places ()
  (let ((places (make-hash-table :test 'equal)))
    (dolist (mode (atty/mode:modes))
      (loop :for (chord . does) :in (atty/mode:mode-bindings mode)
            :do (setf (gethash (list :key mode chord) places) does)))
    (dolist (kind '(:command :doc :group :unlisted :init-command))
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
        ((:command :doc :group :unlisted :init-command)
         (if absent
             (remhash name (place-table kind))
             (setf (gethash name (place-table kind)) value)))))))

(defun undo-changes (changes)
  (loop :for (place was) :in changes
        :when (eq (first place) :setting) :do (ignore-errors (put-place place was)))
  (loop :for (place was) :in changes
        :unless (eq (first place) :setting) :do (put-place place was)))

(defun condition-text (handler)
  (or (atty/mode:handler-name handler) (command-name-of handler)))

(defun name-bare-keys (changes)
  (loop :for (place nil now) :in changes
        :when (and (eq (first place) :key) (functionp now) (null (condition-text now)))
          :do (let ((name (format nil "~(~A~) ~A" (second place) (third place))))
                (setf (gethash name *commands*) now
                      (gethash name *unlisted*) t
                      (gethash name *init-commands*) t))))

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
      (handler-case (let ((*package* (find-package '#:atty/user))
                          (*loading-init* t))
                      (load file)
                      (setf loaded t))
        (error (e)
          (setf *init-error* (format nil "~A did not load: ~A" file e))
          (format *error-output* "~&atty: ~A~%" *init-error*))))
    (name-bare-keys (changes-between before (init-places)))
    (setf *init-changes* (changes-between before (init-places)))
    (incf *init-generation*)
    loaded))

(defun init-load-note ()
  "What to say after loading the init file: (text face)."
  (cond (*init-error* (list *init-error* :warning))
        ((probe-file (user-init-file))
         (list (format nil "~A loaded" (user-init-file)) :accent))
        (t (list (format nil "there is no ~A" (user-init-file)) :accent))))

(defun writable-p (value)
  ;; printed to a stream, not to a string: sbcl knows prin1-to-string has no
  ;; effect but its answer, and leaves the call out when the answer is not
  ;; needed, so nothing was ever printed and nothing could fail
  (handler-case (with-standard-io-syntax
                  (let ((*print-readably* t)
                        (*package* (find-package '#:atty)))
                    (prin1 value (make-broadcast-stream))
                    t))
    (error () nil)))

(defun encode-config ()
  (list :config
        (loop :for (key value) :in (settings)
              :when (writable-p value) :collect (list key value))
        (loop :for name :being :the :hash-keys :of *init-commands*
              :when (gethash name *commands*)
                :collect (list name (command-doc name) (command-group name)
                               (and (gethash name *unlisted*) t)))
        (loop :for (place nil now) :in *init-changes*
              :when (eq (first place) :key)
                :collect (list (symbol-name (second place)) (third place)
                               (and (functionp now) (condition-text now))))))

(defun send-config (watcher)
  (when (and (plusp *init-generation*)
             (not (eql (watcher-config-sent watcher) *init-generation*)))
    (setf (watcher-config-sent watcher) *init-generation*)
    (send-message watcher (encode-config))))

(defun remote-command-function (name)
  (lambda () (send-to-server (list :command name))))

(defun apply-config (settings commands keys)
  (undo-changes *config-changes*)
  (let ((before (init-places)))
    (loop :for (key value) :in settings
          :do (ignore-errors (setf (setting key) value)))
    (loop :for (name doc group unlisted) :in commands
          :do (setf (gethash name *commands*) (remote-command-function name))
              (if doc (setf (gethash name *command-docs*) doc) (remhash name *command-docs*))
              (if group (setf (gethash name *command-groups*) group) (remhash name *command-groups*))
              (if unlisted (setf (gethash name *unlisted*) t) (remhash name *unlisted*)))
    (loop :for (mode chord name) :in keys
          :for m := (atty/mode:mode-named mode)
          :when m :do (atty/mode:set-binding m chord (and name (atty/mode:as-handler name))))
    (setf *config-changes* (changes-between before (init-places)))))

(defun run-init-command (watcher name)
  (let ((does (and (stringp name) (gethash name *init-commands*) (gethash name *commands*))))
    (if does
        (let ((*client* watcher)) (call-guarded does name))
        (send-message watcher (list :note "no command"
                            (format nil "The init file has no command called ~A." name)
                            :warning)))))

(defmethod show-note ((watcher watcher) title text &key (face :warning))
  (send-message watcher (list :note title text face)))

(defmethod message-for-command ((watcher watcher) form)
  (let ((session (watcher-session watcher)))
    (when session (handle-message (session-server session) watcher form))))

(defmethod run-for ((watcher watcher) name)
  (if (gethash name *init-commands*)
      (call-next-method)
      (send-message watcher (list :do name))))

(defun encode-settings ()
  (flet ((shown (key value)
           (if (eq key :prefix) (prefix-string value) (prin1-to-string value))))
    (loop :for (key value default doc) :in (settings)
          :collect (list key (shown key value)
                         (and (not (equal value default)) (shown key default))
                         doc))))

(defun init-help (s &optional (rows (encode-settings)) (from :server))
  "What an init file can say, with every setting there is and what it is now."
  (format s "~&~A is read by the server when it starts, as source, in the~%~
             package atty/user. Every client attached is told what it says.~%~%" (user-init-file))
  (format s "  (define-key 'pane-mode \"M-wheel-up\" \"scroll page up\")   a key or button to a command~%")
  (format s "  (undefine-key 'pane-mode \"C-b t\")~%")
  (format s "  (defcommand say-hello (show-note *client* \"hi\" \"hello\"))  a command of your own~%")
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
  (format s "~%C-b R has the server read it again, and every client is told.~%"))
