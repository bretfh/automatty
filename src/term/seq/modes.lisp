;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

;;; Modes a program turns on and off. A mode is a number and whether it came
;;; with the DEC marker, so mode 4 and mode ?4 are two different modes and get
;;; two methods.

(defmethod handle-csi ((term term) (final (eql #\h)) format params)
  (dolist (p params)
    (when p (handle-mode term p (eql format #\?) t))))

(defmethod handle-csi ((term term) (final (eql #\l)) format params)
  (dolist (p params)
    (when p (handle-mode term p (eql format #\?) nil))))

(defmethod handle-mode ((term term) (number (eql 4)) (privatep null) set)
  (setf (term-insert-mode term) set))

(defmethod handle-mode ((term term) (number (eql 1)) (privatep (eql t)) set)
  (setf (term-keypad-mode term) set))

(defmethod handle-mode ((term term) (number (eql 7)) (privatep (eql t)) set)
  (setf (term-auto-margin term) set))

(defparameter +blinking-cursors+
  '((:block . :blinking-block)
    (:underline . :blinking-underline)
    (:bar . :blinking-bar)))

(defmethod handle-mode ((term term) (number (eql 12)) (privatep (eql t)) set)
  (let ((had (term-cursor-style term)))
    (setf (term-cursor-style term)
          (if set
              (or (cdr (assoc had +blinking-cursors+)) had)
              (or (car (rassoc had +blinking-cursors+)) had)))))

(defmethod handle-mode ((term term) (number (eql 25)) (privatep (eql t)) set)
  (setf (term-cursor-visible term) set))

(defmethod handle-mode ((term term) (number (eql 1047)) (privatep (eql t)) set)
  (if set (term-enter-alt-screen term) (term-exit-alt-screen term)))

(defmethod handle-mode ((term term) (number (eql 1048)) (privatep (eql t)) set)
  (if set (term-save-cursor term) (term-restore-cursor term)))

(defmethod handle-mode ((term term) (number (eql 1049)) (privatep (eql t)) set)
  (if set
      (progn (term-save-cursor term) (term-enter-alt-screen term))
      (progn (term-exit-alt-screen term) (term-restore-cursor term))))

(defmethod handle-mode ((term term) (number (eql 2004)) (privatep (eql t)) set)
  (setf (term-bracketed-paste term) set))
