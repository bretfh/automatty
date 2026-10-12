;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:automatty)

(defun options-and-children (what args allowed)
  (let ((options nil))
    (loop :while (and args (keywordp (first args)))
          :do (let ((key (pop args)))
                (unless args (error "~A: ~(~S~) wants a value" what key))
                (push key options)
                (push (pop args) options)))
    (setf options (nreverse options))
    (loop :for (key) :on options :by #'cddr
          :unless (member key allowed)
            :do (error "~A takes no ~(~S~); it takes ~{~(~S~)~^ ~}" what key allowed))
    (values options args)))

(defun check-name (what name)
  (unless (and (stringp name) (plusp (length name)))
    (error "~A wants a name, a string, not ~S" what name))
  name)

(defun check-node (what node kinds)
  (unless (and (consp node) (member (first node) kinds))
    (error "~A cannot hold ~S; it holds ~{~(~A~)~^ or ~}" what node kinds))
  node)

(defun check-place (what options)
  (destructuring-bind (&key directory env &allow-other-keys) options
    (unless (or (null directory) (stringp directory))
      (error "~A: :directory is a string, not ~S" what directory))
    (unless (and (listp env)
                 (every (lambda (e) (and (stringp e) (position #\= e))) env))
      (error "~A: :env is a list of \"NAME=value\" strings, not ~S" what env)))
  options)

(defun pane (command &rest options)
  (multiple-value-bind (options rest)
      (options-and-children "pane" options '(:name :directory :env))
    (when rest (error "pane holds nothing, not ~S" rest))
    (unless (or (null command) (stringp command))
      (error "pane wants a command, a string, not ~S" command))
    (list* :pane command (check-place "pane" options))))

(defun layout-of (what kind args)
  (multiple-value-bind (options parts) (options-and-children what args '())
    (unless parts (error "~A wants at least one pane" what))
    (dolist (part parts) (check-node what part '(:pane :columns :rows)))
    (list* kind :parts parts options)))

(defun columns (&rest options-and-parts)
  (layout-of "columns" :columns options-and-parts))

(defun rows (&rest options-and-parts)
  (layout-of "rows" :rows options-and-parts))

(defun window (name &rest options-and-layout)
  (check-name "window" name)
  (multiple-value-bind (options layout)
      (options-and-children "window" options-and-layout '(:directory :env))
    (when (rest layout)
      (error "window ~S holds one pane, columns or rows; put ~D in columns or rows"
             name (length layout)))
    (list* :window name
           :layout (if layout
                       (check-node "window" (first layout) '(:pane :columns :rows))
                       (pane nil))
           (check-place "window" options))))

(defun session (name &rest options-and-windows)
  (check-name "session" name)
  (multiple-value-bind (options windows)
      (options-and-children "session" options-and-windows '(:directory :env))
    (unless windows (error "session ~S wants at least one window" name))
    (dolist (w windows) (check-node "session" w '(:window)))
    (let ((names (mapcar #'second windows)))
      (when (/= (length names) (length (remove-duplicates names :test #'string=)))
        (error "session ~S has two windows with one name" name)))
    (list* :session name :windows windows (check-place "session" options))))

(defun server-form (name options-and-sessions)
  (multiple-value-bind (options sessions)
      (options-and-children "defserver" options-and-sessions '(:directory :env))
    (dolist (s sessions) (check-node "defserver" s '(:session)))
    (let ((names (mapcar #'second sessions)))
      (when (/= (length names) (length (remove-duplicates names :test #'string=)))
        (error "server ~S has two sessions with one name" name)))
    (list* :server name :sessions sessions (check-place "defserver" options))))

(defmacro defserver (name &body options-and-sessions)
  `(atty:declare-server
    (server-form ,(if (stringp name) name (string-downcase (symbol-name name)))
                 (list ,@options-and-sessions))))
