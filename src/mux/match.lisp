;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx)

;;; Which of a list of things somebody meant by what they have typed so far.
;;; A letter at the start of a word counts for more than one in the middle, a
;;; run of letters counts for more than the same letters scattered, and a short
;;; answer beats a long one that matched as well.

(defun %word-start-p (text at)
  (or (zerop at)
      (let ((before (char text (1- at))))
        (or (member before '(#\Space #\- #\_ #\. #\/ #\:) :test #'char=)
            (and (lower-case-p before) (upper-case-p (char text at)))))))

(defun score (query text)
  (let ((qn (length query)) (tn (length text)))
    (cond ((zerop qn) 0)
          ((> qn tn) nil)
          (t (let ((at 0) (points 0) (run 0) (last -2))
               (dotimes (i qn (+ points (if (< tn 24) (- 24 tn) 0)))
                 (let ((found (position (char query i) text :start at :test #'char-equal)))
                   (unless found (return nil))
                   (incf points (if (%word-start-p text found) 10 1))
                   (when (zerop found) (incf points 15))
                   (setf run (if (= found (1+ last)) (1+ run) 0))
                   (incf points (* 4 run))
                   (setf last found at (1+ found)))))))))

(defun matches (query items text-of)
  (if (zerop (length query))
      items
    (let ((scored (loop :for it :in items
                        :for s := (score query (princ-to-string (funcall text-of it)))
                        :when s :collect (cons s it))))
      (mapcar #'cdr (stable-sort scored #'> :key #'car)))))
