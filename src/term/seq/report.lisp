;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

;;; What the terminal says back. A program asks and expects an answer on its
;;; input, so these are the sequences that need TERM-INPUT-FN to be there.

(defmethod handle-csi ((term term) (final (eql #\c)) format params)
  (let ((n (or (first params) 0)))
    (when (zerop n)
      (case format
        ((nil) (term-answers term (format nil "~C[?12;4c" #\Escape)))
        (#\> (term-answers term (format nil "~C[>0;0;0c" #\Escape)))))))

(defmethod handle-csi ((term term) (final (eql #\n)) format params)
  (declare (ignore format))
  (case (or (first params) 0)
    (5 (term-answers term (format nil "~C[0n" #\Escape)))
    (6 (term-answers term (format nil "~C[~D;~DR" #\Escape
                                  (1+ (term-cursor-y term))
                                  (1+ (term-cursor-x term)))))))
