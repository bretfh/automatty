;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

;;; Where the cursor is, and what it looks like.

(defmethod handle-csi ((term term) (final (eql #\A)) format params)
  (declare (ignore format))
  (term-cursor-up term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\k)) format params)
  (handle-csi term #\A format params))

(defmethod handle-csi ((term term) (final (eql #\B)) format params)
  (declare (ignore format))
  (term-cursor-down term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\e)) format params)
  (handle-csi term #\B format params))

(defmethod handle-csi ((term term) (final (eql #\C)) format params)
  (declare (ignore format))
  (term-cursor-right term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\a)) format params)
  (handle-csi term #\C format params))

(defmethod handle-csi ((term term) (final (eql #\D)) format params)
  (declare (ignore format))
  (term-cursor-left term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\j)) format params)
  (handle-csi term #\D format params))

(defmethod handle-csi ((term term) (final (eql #\E)) format params)
  (declare (ignore format))
  (term-cursor-down term (or (first params) 1))
  (term-carriage-return term))

(defmethod handle-csi ((term term) (final (eql #\F)) format params)
  (declare (ignore format))
  (term-cursor-up term (or (first params) 1))
  (term-carriage-return term))

(defmethod handle-csi ((term term) (final (eql #\G)) format params)
  (declare (ignore format))
  (term-cursor-horizontal-abs term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\`)) format params)
  (handle-csi term #\G format params))

(defmethod handle-csi ((term term) (final (eql #\d)) format params)
  (declare (ignore format))
  (term-cursor-vertical-abs term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\H)) format params)
  (declare (ignore format))
  (term-goto term (or (first params) 1) (or (second params) 1)))

(defmethod handle-csi ((term term) (final (eql #\f)) format params)
  (handle-csi term #\H format params))

(defmethod handle-csi ((term term) (final (eql #\I)) format params)
  (declare (ignore format))
  (term-horizontal-tab term (or (first params) 1)))

(defmethod handle-csi ((term term) (final (eql #\Z)) format params)
  (declare (ignore format))
  (term-horizontal-backtab term (or (first params) 1)))

(defmethod handle-esc ((term term) (final (eql #\H)))
  (term-set-tab-stop term))

(defmethod handle-csi ((term term) (final (eql #\g)) format params)
  (declare (ignore format))
  (term-clear-tab-stop term (or (first params) 0)))

(defmethod handle-csi ((term term) (final (eql #\s)) format params)
  (declare (ignore params))
  (when (null format) (term-save-cursor term)))

(defmethod handle-csi ((term term) (final (eql #\u)) format params)
  (declare (ignore params))
  (when (null format) (term-restore-cursor term)))

(defparameter +cursor-styles+
  #(:blinking-block :blinking-block :block
    :blinking-underline :underline
    :blinking-bar :bar))

(defmethod handle-csi ((term term) (final (eql #\q)) format params)
  (let ((style (first params)))
    (when (and params (null format) style (<= 0 style 6))
      (setf (term-cursor-style term) (aref +cursor-styles+ style)))))

(defmethod handle-esc ((term term) (final (eql #\7)))
  (term-save-cursor term))

(defmethod handle-esc ((term term) (final (eql #\8)))
  (term-restore-cursor term))
