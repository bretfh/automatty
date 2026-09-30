;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What can be done and what asks for it. A command is a function with a name
;;; you can type; which keys reach it is a mode's business, and a mode inherits
;;; from the modes it was defined on top of, so a keymap is a tree rather than a
;;; table.

(defvar *client* nil
  "The attached terminal a command is running for, the way a command in an
editor reads which buffer it is in.")

(defvar *commands* (make-hash-table :test 'equal)
  "Every command, by the name it is asked for by.")

(defvar *unlisted* (make-hash-table :test 'equal)
  "Commands that are not offered when asking for one by name: the ones that only
mean anything while something is up that the asking would have closed.")

(defvar *command-docs* (make-hash-table :test 'equal)
  "One line on each command, by name: what the prompt that offers them shows
beside each.")

(defvar *command-groups* (make-hash-table :test 'equal)
  "What each command acts on, by name: panes, windows, sessions, agents,
reading or asking. The keys help groups by it.")

(defmacro defcommand (name lambda-list &body body)
  "Define a command, a function of LAMBDA-LIST, and register it under its name
with the dashes read as spaces: DETACH is asked for as \"detach\", BAR-OFF as
\"bar off\". Whatever calls it by name passes what follows the name.

A name written as (NAME :unlisted) is a command that still has a name and can
still be bound, but is not among the ones offered when somebody asks for one.
A name written as (NAME :group panes) says what it acts on. A string first in
BODY is one line on what it does, shown beside it when it is offered."
  (destructuring-bind (name &rest marks) (if (listp name) name (list name))
    (let ((said (string-downcase (substitute #\Space #\- (symbol-name name))))
          (doc (and (stringp (first body)) (rest body) (first body)))
          (group (second (member :group marks))))
      `(progn
         (defun ,name ,lambda-list ,@(if doc (rest body) body))
         (setf (gethash ,said *commands*) (function ,name))
         ,@(when doc `((setf (gethash ,said *command-docs*) ,doc)))
         ,@(when group `((setf (gethash ,said *command-groups*) ',group)))
         ,@(when (member :unlisted marks) `((setf (gethash ,said *unlisted*) t)))
         ',name))))

(define-condition refused (error)
  ((code :initarg :code :reader refused-code)
   (text :initarg :text :reader refused-text))
  (:report (lambda (c s) (write-string (refused-text c) s))))

(defun refuse (code control &rest arguments)
  "End the command, the command line exiting with CODE and saying why."
  (error 'refused :code code :text (apply #'format nil control arguments)))

(defun command-doc (name) (gethash name *command-docs*))
(defun command-group (name) (gethash name *command-groups*))

(defun command-names (&optional (offered t))
  "What the commands are called. OFFERED leaves out the ones that cannot be run
from a prompt, which is where being asked for a command happens."
  (sort (loop :for name :being :the :hash-keys :of *commands*
              :unless (and offered (gethash name *unlisted*)) :collect name)
        #'string<))

(defun call-guarded (does what &rest arguments)
  "Run DOES. One that comes apart is shown to whoever asked rather than taking
the server down."
  (handler-case (apply does arguments)
    (error (e)
      (when *client* (show-error *client* what e))
      nil)))

(defun run-command (name &optional (client *client*) arguments)
  "Run the command called NAME for CLIENT with ARGUMENTS."
  (if (and client (watcher-elsewhere-p client))
      (on-watcher client (lambda () (run-command name client arguments)))
      (let ((does (gethash name *commands*))
            (*client* client))
        (when does (apply #'call-guarded does name arguments)))))

(setf atty/mode:*run* (lambda (does) (call-guarded does (or (atty/mode:pending) "that key"))))

(setf atty/mode:*named* (lambda (name)
                          (if (gethash name *commands*)
                              (run-command name)
                              (error "there is no command called ~S" name))))

(atty/mode:define-mode pane-mode ())

(defparameter +palette-kinds+
  '((#\: "commands" "commands" "run a command…")
    (#\@ "windows" "switch window" "go to a window…")
    (#\# "clients" "clients" "a terminal…")
    (#\/ "find" "find in pane" "find in the pane…"))
  "The palette's kinds: the prefix that opens each, what its tab says, the
command that opens it, and what the field says while nothing is typed.")

(defparameter +palette-prefixes+
  (mapcar (lambda (kind) (cons (first kind) (third kind))) +palette-kinds+)
  "The prefix that opens each kind of prompt, and the command that opens it.
Typing one as the first character of an empty query, in a prompt of a
different kind, switches to it instead of being searched for.")
