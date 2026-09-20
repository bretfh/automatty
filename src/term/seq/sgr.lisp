;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:term)

(declaim (optimize (speed 3) (safety 1)))

;;; Colours and attributes. What the numbers mean is src/term/sgr.lisp; this is
;;; where the sequence that carries them is answered.

(defmethod handle-csi ((term term) (final (eql #\m)) format params)
  (unless format
    (process-sgr term params)))
