;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

;;; What somebody's own init.lisp is read in: the keys and modes, the commands,
;;; the settings and the hooks, with nothing else of atty's in the way. It is
;;; loaded when atty starts, in the client and in the server alike, from source.

(defpackage #:atty/user
  (:use #:cl #:atty/mode)
  (:import-from #:atty
                #:defcommand #:run-command #:tell-the-server #:show-note #:*client*
                #:configure #:setting #:settings
                #:add-hook #:remove-hook
                #:pane-mode #:scroll-mode #:board-mode #:note-mode)
  (:export #:defcommand #:run-command #:tell-the-server #:show-note #:*client*
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

(defun load-user-init (&optional (file (user-init-file)))
  "Load FILE if there is one, as source, in the user's package. Answers whether
it loaded. One that does not load is said, kept as *INIT-PROBLEM* for whoever
attaches next to be shown, and passed over: a slip in somebody's own keys is
not a reason not to start."
  (setf *init-problem* nil)
  (when (probe-file file)
    (handler-case (let ((*package* (find-package '#:atty/user)))
                    (load file)
                    t)
      (error (e)
        (setf *init-problem* (format nil "~A did not load: ~A" file e))
        (format *error-output* "~&atty: ~A~%" *init-problem*)
        nil))))

(defun init-loaded-note ()
  "What to say after loading the init file: (text face)."
  (cond (*init-problem* (list *init-problem* :warning))
        ((probe-file (user-init-file))
         (list (format nil "~A loaded" (user-init-file)) :accent))
        (t (list (format nil "there is no ~A" (user-init-file)) :accent))))

(defun init-help (s)
  "What an init file can say, with every setting there is and what it is now."
  (format s "~&~A is loaded when atty starts, in the client and in the server,~%~
             as source. It is read in the package atty/user.~%~%" (user-init-file))
  (format s "  (define-key 'pane-mode \"M-wheel-up\" \"scroll page up\")   a key or button to a command~%")
  (format s "  (undefine-key 'pane-mode \"C-b t\")~%")
  (format s "  (defcommand say-hello (show-note *client* \"hi\" \"hello\"))  a command of your own~%")
  (format s "  (configure :wheel-rows 6 :prefix \"C-a\")                 a setting~%")
  (format s "  (add-hook 'pane-started (lambda (session pane) ...))~%~%")
  (format s "Settings, with what they are now:~%")
  (loop :for (key value default doc) :in (settings)
        :do (format s "~%  :~(~A~) ~S~@[  (default ~S)~]~%      ~A~%"
                    key value (and (not (equal value default)) default) doc))
  (format s "~%Hooks:~%")
  (loop :for (name nil doc) :in *hooks*
        :do (format s "  ~(~A~)  ~A~%" name doc))
  (format s "~%C-b R loads it again without restarting.~%"))
