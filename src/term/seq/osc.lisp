;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

;;; Operating system commands: a number, a semicolon, and the rest.

(defun dispatch-osc (term said)
  (let ((semi (position #\; said)))
    (when semi
      (let ((code (parse-integer said :end semi :junk-allowed t)))
        (when code
          (handle-osc term code (subseq said (1+ semi))))))))

(defmethod handle-osc ((term term) (code (eql 0)) said)
  (setf (term-title term) said)
  (term-titled term said))

(defmethod handle-osc ((term term) (code (eql 2)) said)
  (handle-osc term 0 said))

(defmethod handle-osc ((term term) (code (eql 7)) said)
  (setf (term-cwd term) said)
  (term-moved term said))

(defmethod handle-osc ((term term) (code (eql 10)) said)
  (when (string= said "?")
    (term-answers term (format nil "~C]10;rgb:d8d8/d8d8/d8d8~C\\"
                               #\Escape #\Escape))))

(defmethod handle-osc ((term term) (code (eql 11)) said)
  (when (string= said "?")
    (term-answers term (format nil "~C]11;rgb:1818/1818/1818~C\\"
                               #\Escape #\Escape))))

(defmethod handle-osc ((term term) (code (eql 8)) said)
  (let* ((semi (position #\; said))
         (params (if semi (subseq said 0 semi) ""))
         (uri (if semi (subseq said (1+ semi)) "")))
    (term-linked term (if (zerop (length uri)) nil uri) params)))

(defmethod handle-osc ((term term) (code (eql 52)) said)
  (let ((semi (position #\; said)))
    (when semi
      (term-copied term (subseq said 0 semi) (subseq said (1+ semi))))))
