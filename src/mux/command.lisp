;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

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

(defmacro defcommand (name &body body)
  "Define a command, and register it under its name with the dashes read as
spaces: DETACH is asked for as \"detach\", BAR-OFF as \"bar off\"."
  (let ((said (string-downcase (substitute #\Space #\- (symbol-name name)))))
    `(progn
       (defun ,name () ,@body)
       (setf (gethash ,said *commands*) (function ,name))
       ',name)))

(defun command-names ()
  (sort (loop :for name :being :the :hash-keys :of *commands* :collect name)
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

(setf vt/mode:*run* (lambda (does) (tried does (or (vt/mode:pending) "that key"))))

;;; What vt calls a key, and what a mode calls one.

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
       (cond ((= code 27) (vt/mode:make-key "Escape"))
             ((= code 13) (vt/mode:make-key "RET"))
             ((= code 9) (vt/mode:make-key "TAB"))
             ((= code 127) (vt/mode:make-key "DEL"))
             ((= code 32) (vt/mode:make-key "SPC"))
             ((< code 32) (vt/mode:make-key (string (code-char (+ 96 code)))
                                            :ctrl t))
             (t (vt/mode:make-key (string event))))))
    (cons
     (let ((mods (rest event)))
       (vt/mode:make-key (key-named (first event))
                         :ctrl (and (member :ctrl mods) t)
                         :meta (and (member :meta mods) t)
                         :shift (and (member :shift mods) t))))))

(vt/mode:define-mode pane-mode ())
