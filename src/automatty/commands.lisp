;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:automatty)

(dolist (name (atty:command-names))
  (let ((symbol (find-symbol (string-upcase (substitute #\- #\Space name)) '#:atty)))
    (when symbol
      (import symbol)
      (export symbol))))
