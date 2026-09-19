;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

;;; Putting characters in and taking them out.

(defmethod handle-csi ((term term) (final (eql #\@)) format params)
  (declare (ignore format))
  (term-insert-char term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\P)) format params)
  (declare (ignore format))
  (term-delete-char term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\X)) format params)
  (declare (ignore format))
  (term-erase-char term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\L)) format params)
  (declare (ignore format))
  (term-insert-line term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\M)) format params)
  (declare (ignore format))
  (term-delete-line term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\b)) format params)
  "REP: the last character again, N times. It is the one count that repeats
rather than clamping to the row, so it is the one told what a screen is."
  (declare (ignore format))
  (let ((n (min (or (first params) 1)
                (* (term-width term) (term-height term)))))
    (when (graphic-char-p (term-last-char term))
      (term-write term (make-string n :initial-element (term-last-char term))))))
