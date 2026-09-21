;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/agent)

(defun screen-lines (term)
  (let ((lines (loop :for y :below (term:term-height term)
                     :collect (string-right-trim " " (term:term-dump-row-string term y)))))
    (subseq lines 0 (1+ (or (position-if (lambda (l) (plusp (length l))) lines :from-end t)
                            -1)))))

(defun last-lines (term n)
  (let* ((screen (screen-lines term))
         (above (max 0 (- n (length screen))))
         (size (term:term-scrollback-size term)))
    (append (loop :for i :from (max 0 (- size above)) :below size
                  :collect (string-right-trim " " (term:term-scrollback-row-string term i)))
            (last screen (min n (length screen))))))

(defparameter +rule-chars+ "─━═╌╍┄┅┈┉▄▀")

(defun rule-line-p (line)
  (let ((said (string-trim " " line)))
    (and (plusp (length said))
         (find (char said 0) +rule-chars+)
         (let ((run (or (position (char said 0) said :test-not #'char=) (length said))))
           (or (= run (length said)) (>= run 3))))))

(defun bottom-non-empty (lines n)
  (let ((from (loop :with seen := 0
                    :for i :from (1- (length lines)) :downto 0
                    :when (plusp (length (string-trim " " (nth i lines))))
                      :do (incf seen)
                          (when (= seen n) (return i))
                    :finally (return (if (plusp seen) 0 (length lines))))))
    (nthcdr from lines)))

(defun after-last-rule (lines)
  (let ((at (position-if #'rule-line-p lines :from-end t)))
    (if at (nthcdr (1+ at) lines) lines)))

(defun prompt-box-top (lines)
  (loop :with seen := 0
        :for i :from (1- (length lines)) :downto 0
        :when (rule-line-p (nth i lines))
          :do (incf seen)
              (when (= seen 2) (return i))))

(defun prompt-box-body (lines)
  (let ((top (prompt-box-top lines)))
    (when top
      (let* ((rest (nthcdr (1+ top) lines))
             (end (position-if #'rule-line-p rest)))
        (subseq rest 0 end)))))

(defun above-prompt-box (lines)
  (let ((top (prompt-box-top lines)))
    (if top (subseq lines 0 top) lines)))

(defun rule-row-p (line)
  (let ((at (position #\Space line :test-not #'char=)))
    (and at (<= at 2) (rule-line-p line))))

(defun left-column (line &optional (gap 6))
  (let* ((start (or (position #\Space line :test-not #'char=) (length line)))
         (at (search (make-string gap :initial-element #\Space) line :start2 start)))
    (string-right-trim " " (subseq line 0 (or at (length line))))))

(defun cell-face (term x y)
  (term:row-face (term:term-grid-row term y) x))

(defun row-inverse-p (term y &optional (from 0) to)
  (let ((row (term:term-grid-row term y)))
    (loop :for x :from from :below (or to (term:term-width term))
          :for face := (term:row-face row x)
          :thereis (and face (term:face-inverse face)))))
