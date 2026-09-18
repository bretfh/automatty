(defpackage #:vt/test
  (:use #:cl #:fiveam)
  (:local-nicknames (#:pty #:vt/pty) (#:mux #:vt/mux) (#:cells #:vt/cells))
  (:export #:run-them #:all))
(in-package #:vt/test)

(def-suite all)

(defun run-them ()
  (let ((results (run 'all)))
    (explain! results)
    (unless (results-status results)
      (uiop:quit 1))))

(defun a-term (&rest args)
  (apply #'vt:make-term args))

(defun say (term &rest strings)
  (dolist (string strings term)
    (vt:term-process-output term string)))

(defun csi (fmt &rest args)
  (format nil "~C[~A" #\Escape (apply #'format nil fmt args)))

(defun esc (fmt &rest args)
  (format nil "~C~A" #\Escape (apply #'format nil fmt args)))

(defun osc (fmt &rest args)
  (format nil "~C]~A~C" #\Escape (apply #'format nil fmt args) #\Bel))

(defun row (term y)
  (string-right-trim " " (vt:term-dump-row-string term y)))

(defun rows (term)
  (loop :for y :below (vt:term-height term) :collect (row term y)))

(defun at (term x y)
  (vt:cell-char (aref (vt:term-grid-row term y) x)))

(defun face-at (term x y)
  (vt:cell-face (aref (vt:term-grid-row term y) x)))

(defun cursor (term)
  (list (vt:term-cursor-x term) (vt:term-cursor-y term)))

(defun a-screen (&rest args)
  (apply #'mux:make-screen args))

(defun shown (screen y)
  (let* ((row (mux:screen-row screen y))
         (s (make-string (mux:screen-width screen))))
    (dotimes (x (length s) (string-right-trim " " s))
      (setf (schar s x) (vt:cell-char (aref row x))))))

(defun cell-at (screen x y)
  (aref (mux:screen-row screen y) x))

(defun laid-out (runs)
  (mapcar (lambda (r) (list (mux:run-row r) (mux:run-start r) (mux:run-end r)))
          runs))

(defun at-screen (screen x y)
  (vt:cell-char (aref (mux:screen-row screen y) x)))
