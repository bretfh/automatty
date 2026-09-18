;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

;;; What can be done, by name, and which key asks for it. Both are tables rather
;;; than a case in the middle of the loop, so a config file is a program that
;;; adds to them and nothing has to be recompiled to bind a key.

(defvar *commands* (make-hash-table :test 'equal)
  "What can be run, by the name it is asked for by.")

(defvar *keys* (make-hash-table :test 'eql)
  "What the key after the prefix means: the name of a command.")

(declaim (ftype function show-broke))

(defmacro defcommand (name (&rest args) &body body)
  "Give a name to something that can be run. The name is what gets typed."
  `(setf (gethash ,name *commands*) (lambda (,@args) ,@body)))

(defun command-names ()
  (sort (loop :for name :being :the :hash-keys :of *commands* :collect name)
        #'string<))

(defun bind (key name)
  "Say that KEY, after the prefix, runs the command called NAME."
  (setf (gethash key *keys*) name))

(defun bound (key)
  (gethash key *keys*))

(defun bindings ()
  (sort (loop :for key :being :the :hash-keys :of *keys*
                :using (:hash-value name)
              :collect (cons key name))
        #'string< :key #'cdr))

(defun run-command (name client)
  "Run the command called NAME.

A command that comes apart is shown rather than fatal. These run in the client,
which is holding somebody's terminal in raw mode; letting a bad one out would
take the screen with it."
  (let ((it (gethash name *commands*)))
    (when it
      (handler-case (funcall it client)
        (error (e) (show-broke client name e))))))
