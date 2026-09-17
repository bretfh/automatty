;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

(defvar *dec-line-drawing-table* (make-hash-table :test 'eql))

(macrolet ((def-chars (&rest pairs)
             `(progn
                ,@(loop for (from to) on pairs by #'cddr
                        collect `(setf (gethash ,from *dec-line-drawing-table*)
                                       ,to)))))
  (def-chars
    #\+ #\→  #\, #\←  #\- #\↑  #\. #\↓  #\0 #\█
    #\` #\◆  #\a #\▒  #\b #\␉  #\c #\␌  #\d #\␍
    #\e #\␊  #\f #\°  #\g #\±  #\h #\░  #\i #\#
    #\j #\┘  #\k #\┐  #\l #\┌  #\m #\└  #\n #\┼
    #\o #\⎺  #\p #\⎻  #\q #\─  #\r #\⎼  #\s #\⎽
    #\t #\├  #\u #\┤  #\v #\┴  #\w #\┬  #\x #\│
    #\y #\≤  #\z #\≥  #\{ #\π  #\| #\≠  #\} #\£
    #\~ #\•))

(defun char-display-width (ch)
  (let ((code (char-code ch)))
    (cond ((< code 32) 0)
          ((= code #x7F) 0)
          ((or (and (<= #x0300 code) (<= code #x036F))
               (and (<= #x0483 code) (<= code #x0489))
               (and (<= #x0591 code) (<= code #x05C7))
               (and (<= #x064B code) (<= code #x065F))
               (and (<= #x200B code) (<= code #x200F))
               (and (<= #x20D0 code) (<= code #x20FF))
               (and (<= #xFE20 code) (<= code #xFE2F)))
           0)
          ((or (and (<= #x1100 code) (<= code #x115F))
               (and (<= #x2E80 code) (<= code #x2FFF))
               (and (<= #x3000 code) (<= code #x9FFF))
               (and (<= #xAC00 code) (<= code #xD7A3))
               (and (<= #xF900 code) (<= code #xFAFF))
               (and (<= #xFF01 code) (<= code #xFF60))
               (and (<= #x1F300 code) (<= code #x1F9FF)))
           2)
          (t 1))))

(defun translate-charset (ch charset)
  (case charset
    (:dec-line-drawing
     (or (gethash ch *dec-line-drawing-table*) ch))
    (t ch)))

(defun current-charset-mapping (term)
  (case (term-active-charset term)
    (:g0 (term-g0 term))
    (:g1 (term-g1 term))
    (:g2 (term-g2 term))
    (:g3 (term-g3 term))
    (t :us-ascii)))

(defun term-write (term str &optional (start 0) (end (length str)))
  (let* ((w (term-width term))
         (charset (current-charset-mapping term))
         (face (intern-face term)))
    (do ((idx start (1+ idx)))
        ((>= idx end))
      (let* ((raw-ch (char str idx))
             (ch (translate-charset raw-ch charset))
             (cw (char-display-width ch)))
        (unless (zerop cw)
          (setf (term-last-char term) ch)
          (when (term-insert-mode term)
            (term-insert-char term cw))
          (when (>= (term-cursor-x term) w)
            (if (term-auto-margin term)
                (progn
                  (setf (term-cursor-x term) 0)
                  (if (= (term-cursor-y term) (term-scroll-bottom term))
                      (term-scroll-up term 1)
                      (incf (term-cursor-y term))))
                (setf (term-cursor-x term) (1- w))))
          (let ((x (term-cursor-x term))
                (y (term-cursor-y term))
                (row (aref (term-grid term) (term-cursor-y term))))
            (when (and (= cw 2) (>= (1+ x) w))
              (setf (cell-char (aref row x)) #\Space
                    (cell-face (aref row x)) face)
              (setf (term-cursor-x term) 0)
              (if (= y (term-scroll-bottom term))
                  (term-scroll-up term 1)
                  (incf (term-cursor-y term)))
              (setf x (term-cursor-x term)
                    y (term-cursor-y term)
                    row (aref (term-grid term) y)))
            (setf (cell-char (aref row x)) ch
                  (cell-face (aref row x)) face)
            (when (= cw 2)
              (when (< (1+ x) w)
                (setf (cell-char (aref row (1+ x))) #\Space
                      (cell-face (aref row (1+ x))) face)))
            (incf (term-cursor-x term) cw)))))))
