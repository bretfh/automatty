;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What can be done and what asks for it. A command is a function with a name
;;; you can type; which keys reach it is a mode's business, and a mode inherits
;;; from the modes it was defined on top of, so a keymap is a tree rather than a
;;; table.

(defvar *client* nil
  "The client a command is running for. Commands take no arguments and read
this, the way a command in an editor reads which buffer it is in.")

(defvar *commands* (make-hash-table :test 'equal)
  "Every command, by the name it is asked for by.")

(declaim (ftype function show-broke))

(defvar *unlisted* (make-hash-table :test 'equal)
  "Commands that are not offered when asking for one by name: the ones that only
mean anything while something is up that the asking would have closed.")

(defmacro defcommand (name &body body)
  "Define a command, and register it under its name with the dashes read as
spaces: DETACH is asked for as \"detach\", BAR-OFF as \"bar off\".

A name written as (NAME :unlisted) is a command that still has a name and can
still be bound, but is not among the ones offered when somebody asks for one."
  (destructuring-bind (name &rest marks) (if (listp name) name (list name))
    (let ((said (string-downcase (substitute #\Space #\- (symbol-name name)))))
      `(progn
         (defun ,name () ,@body)
         (setf (gethash ,said *commands*) (function ,name))
         ,@(when (member :unlisted marks) `((setf (gethash ,said *unlisted*) t)))
         ',name))))

(defun command-names (&optional (offered t))
  "What the commands are called. OFFERED leaves out the ones that cannot be run
from a prompt, which is where being asked for a command happens."
  (sort (loop :for name :being :the :hash-keys :of *commands*
              :unless (and offered (gethash name *unlisted*)) :collect name)
        #'string<))

(defun tried (does what)
  "Run DOES. One that comes apart is shown rather than fatal: these run in the
client, which is holding somebody's terminal in raw mode, and letting a bad one
out would take the screen with it."
  (handler-case (funcall does)
    (error (e)
      (when *client* (show-broke *client* what e))
      nil)))

(defun run-command (name &optional (client *client*))
  (let ((does (gethash name *commands*))
        (*client* client))
    (when does (tried does name))))

(setf atty/mode:*run* (lambda (does) (tried does (or (atty/mode:pending) "that key"))))

;;; What libatty calls a key, and what a mode calls one.

(defparameter +key-names+
  '((:up . "Up") (:down . "Down") (:left . "Left") (:right . "Right")
    (:home . "Home") (:end . "End") (:insert . "Insert") (:delete . "Delete")
    (:page-up . "PageUp") (:page-down . "PageDown")
    (:enter . "RET") (:tab . "TAB") (:backspace . "DEL") (:escape . "Escape")))

(defun key-named (what)
  (or (cdr (assoc what +key-names+))
      (string-capitalize (symbol-name what))))

(defun key-of (event)
  "A key as the emulator reads it, as a key as a mode knows it."
  (etypecase event
    (character
     (let ((code (char-code event)))
       (cond ((= code 27) (atty/mode:make-key "Escape"))
             ((= code 13) (atty/mode:make-key "RET"))
             ((= code 9) (atty/mode:make-key "TAB"))
             ((= code 127) (atty/mode:make-key "DEL"))
             ((= code 32) (atty/mode:make-key "SPC"))
             ((< code 32) (atty/mode:make-key (string (code-char (+ 96 code)))
                                            :ctrl t))
             (t (atty/mode:make-key (string event))))))
    (cons
     (let ((mods (rest event)))
       (atty/mode:make-key (key-named (first event))
                         :ctrl (and (member :ctrl mods) t)
                         :meta (and (member :meta mods) t)
                         :shift (and (member :shift mods) t))))))

(defvar *mouse-at* nil
  "Where the click a command is running for landed, as (X . Y). Read the way
*CLIENT* is, rather than passed: a mouse binding takes no arguments either.")

(defun mouse-key-of (event)
  "A decoded mouse EVENT as a key a mode can bind, or nil for a gesture nothing
is named for yet (a drag). Unlike KEY-OF, EVENT's tail is a plist, not a list
of the modifiers that are down, so it is read with GETF rather than MEMBER."
  (let* ((e (rest event))
         (wheel (getf e :wheel))
         (sym (cond
                (wheel (if (eq wheel :up) "wheel-up" "wheel-down"))
                ((getf e :drag) nil)
                (t (case (getf e :button)
                     (:left (if (getf e :release) "mouse-1-up" "mouse-1"))
                     (:middle (if (getf e :release) "mouse-2-up" "mouse-2"))
                     (:right (if (getf e :release) "mouse-3-up" "mouse-3")))))))
    (when sym
      (atty/mode:make-key sym :ctrl (getf e :ctrl) :meta (getf e :meta)
                             :shift (getf e :shift)))))

(atty/mode:define-mode pane-mode ())
