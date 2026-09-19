;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

;;; Erasing, scrolling, and the region the scrolling happens in.

(defmethod handle-csi ((term term) (final (eql #\J)) format params)
  (declare (ignore format))
  (term-erase-in-display term (or (first params) 0)))

(defmethod handle-csi ((term term) (final (eql #\K)) format params)
  (declare (ignore format))
  (term-erase-in-line term (or (first params) 0)))

(defmethod handle-csi ((term term) (final (eql #\S)) format params)
  (unless (eql format #\?)
    (term-scroll-up term (or (first params) 1))))

(defmethod handle-csi ((term term) (final (eql #\T)) format params)
  (declare (ignore format))
  (term-scroll-down term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\r)) format params)
  (declare (ignore format))
  (term-set-scroll-region term (first params) (second params)))

(defmethod handle-esc ((term term) (final (eql #\D)))
  (term-index term))

(defmethod handle-esc ((term term) (final (eql #\M)))
  (term-reverse-index term))

(defmethod handle-esc ((term term) (final (eql #\E)))
  (term-carriage-return term)
  (term-line-feed term))

(defmethod handle-esc ((term term) (final (eql #\c)))
  (term-reset term))

(defmethod handle-esc ((term term) (final (eql #\n)))
  (setf (term-active-charset term) :g2))

(defmethod handle-esc ((term term) (final (eql #\o)))
  (setf (term-active-charset term) :g3))

(defmethod handle-hash ((term term) (final (eql #\8)))
  (term-align-test term))
