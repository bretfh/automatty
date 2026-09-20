(defpackage #:libatty/test
  (:use #:cl #:fiveam)
  (:export #:run-them #:emulator
           #:a-term #:say #:csi #:esc #:osc #:row #:rows #:at #:face-at #:cursor))
(in-package #:libatty/test)

(def-suite emulator)

(defun run-them ()
  (let ((results (run 'emulator)))
    (explain! results)
    (unless (results-status results)
      (uiop:quit 1))))

(defun a-term (&rest args)
  (apply #'term:make-term args))

(defun say (term &rest strings)
  (dolist (string strings term)
    (term:term-process-output term string)))

(defun csi (fmt &rest args)
  (format nil "~C[~A" #\Escape (apply #'format nil fmt args)))

(defun esc (fmt &rest args)
  (format nil "~C~A" #\Escape (apply #'format nil fmt args)))

(defun osc (fmt &rest args)
  (format nil "~C]~A~C" #\Escape (apply #'format nil fmt args) #\Bel))

(defun row (term y)
  (string-right-trim " " (term:term-dump-row-string term y)))

(defun rows (term)
  (loop :for y :below (term:term-height term) :collect (row term y)))

(defun at (term x y)
  (term:row-char (term:term-grid-row term y) x))

(defun face-at (term x y)
  (term:row-face (term:term-grid-row term y) x))

(defun cursor (term)
  (list (term:term-cursor-x term) (term:term-cursor-y term)))
