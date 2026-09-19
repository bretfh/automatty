;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

(defun face-plist (face)
  (when face
    (let ((props nil))
      (when (face-bold face)
        (push t props) (push :bold props))
      (when (face-faint face)
        (push t props) (push :faint props))
      (when (face-italic face)
        (push t props) (push :italic props))
      (when (face-underline face)
        (push t props) (push :underline props))
      (when (face-crossed face)
        (push t props) (push :strike-through props))
      (when (face-inverse face)
        (push t props) (push :inverse props))
      (let ((fg (resolve-color (face-fg face)))
            (bg (resolve-color (face-bg face))))
        (when (face-inverse face)
          (rotatef fg bg))
        (when (face-conceal face)
          (setf fg bg))
        (when fg (push fg props) (push :fg props))
        (when bg (push bg props) (push :bg props)))
      props)))

(defun resolve-color (color)
  (cond
   ((null color) nil)
   ((integerp color) color)
   ((and (listp color) (= (length color) 3))
    color)
   (t nil)))

(defun term-render-line (term y &optional (chars (make-string (term-width term)
                                                              :initial-element #\Space)))
  (let* ((w (term-width term))
         (row (aref (term-grid term) y))
         (font-changes nil)
         (prev-face nil))
    (dotimes (x w)
      (let ((ch (row-char row x))
            (face (row-face row x)))
        (setf (schar chars x) (if (graphic-char-p ch) ch #\Space))
        (unless (face-equal face prev-face)
          (push (list x (face-plist face)) font-changes)
          (setf prev-face face))))
    (values chars (nreverse font-changes))))

(defun term-dump-to-string (term)
  (with-output-to-string (s)
                         (dotimes (y (term-height term))
                           (let ((row (aref (term-grid term) y)))
                             (dotimes (x (term-width term))
                               (write-char (row-char row x) s)))
                           (unless (= y (1- (term-height term)))
                             (terpri s)))))

(defun term-dump-row-string (term y)
  (let* ((w (term-width term))
         (row (aref (term-grid term) y))
         (s (make-string w)))
    (dotimes (x w s)
      (setf (schar s x) (row-char row x)))))

(defun term-scrollback-row-string (term n)
  (let ((row (term-scrollback-row term n)))
    (when row
      (copy-seq (row-chars row)))))

(defun rgb-to-color-index (r g b)
  "The palette index nearest R G B"
  (let ((levels #(0 95 135 175 215 255)))
    (flet ((level (v) (cond ((< v 48) 0) ((< v 115) 1) (t (floor (- v 35) 40))))
           (off (a b) (let ((d (- a b))) (* d d))))
          (let* ((ri (level r)) (gi (level g)) (bi (level b))
                 (cr (aref levels ri)) (cg (aref levels gi)) (cb (aref levels bi))
                 (step (max 0 (min 23 (round (- (/ (+ r g b) 3) 8) 10))))
                 (grey (+ 8 (* 10 step))))
            (if (<= (+ (off cr r) (off cg g) (off cb b))
                    (+ (off grey r) (off grey g) (off grey b)))
                (+ 16 (* 36 ri) (* 6 gi) bi)
              (+ 232 step))))))

(defun write-number (n s)
  "N as digits on S, without consing a string to do it.

Public because anybody writing escape sequences needs it and FORMAT on this
path costs more than the sequence does: vtx/tty writes a cursor address for
every run of a frame."
  (declare (type (integer 0 #.most-positive-fixnum) n))
  (when (>= n 10) (write-number (floor n 10) s))
  (write-char (code-char (+ 48 (mod n 10))) s))

(defun write-string-chars (str s)
  (declare (type simple-string str))
  (dotimes (i (length str))
    (write-char (schar str i) s)))

(defun write-color-code (color base s)
  (let ((short (case base (38 30) (48 40) (t nil))))
    (cond
     ((and short (integerp color) (<= 0 color 7))
      (write-number (+ short color) s))
     ((and short (integerp color) (<= 8 color 15))
      (write-number (+ (if (= base 38) 90 100) (- color 8)) s))
     ((and (integerp color) (<= 0 color 255))
      (write-number base s) (write-string-chars ";5;" s) (write-number color s))
     ((and (listp color) (= (length color) 3))
      (write-number base s) (write-string-chars ";2;" s)
      (write-number (first color) s) (write-char #\; s)
      (write-number (second color) s) (write-char #\; s)
      (write-number (third color) s))
     (t (write-number (+ base 1) s)))))

(defun write-sgr (face s)
  "Write FACE's SGR escape sequence directly to stream S, no intermediates.

The whole face is written. Which of it the terminal it is going to will accept
is not this library's business: whoever is driving that terminal brings a face
down to what it can wear before it gets here.

It always leads with a reset, because a face is what the cell is, not what
changed: without it an attribute the last face set and this one does not stays
on, and every cell after it is wrong.

Inverse and conceal are said rather than worked out: the colours written are the
ones in the face, and the terminal does the swapping, so what is read back is
what was there."
  (write-char #\Escape s)
  (write-char #\[ s)
  (write-char #\0 s)
  (when face
    (flet ((sep () (write-char #\; s)))
          (when (face-bold face) (sep) (write-char #\1 s))
          (when (face-faint face) (sep) (write-char #\2 s))
          (when (face-italic face) (sep) (write-char #\3 s))
          (let ((underline (face-underline face)))
            (when underline
              (sep)
              (if (eq underline :single)
                  (write-char #\4 s)
                  (progn (write-string-chars "4:" s)
                         (write-number (case underline
                                             (:double 2) (:curly 3)
                                             (:dotted 4) (:dashed 5) (t 1))
                                       s)))))
          (let ((blink (face-blink face)))
            (when blink
              (sep)
              (write-char (if (eq blink :fast) #\6 #\5) s)))
          (when (face-inverse face) (sep) (write-char #\7 s))
          (when (face-conceal face) (sep) (write-char #\8 s))
          (when (face-crossed face) (sep) (write-char #\9 s))
          (when (face-fg face) (sep) (write-color-code (face-fg face) 38 s))
          (when (face-bg face) (sep) (write-color-code (face-bg face) 48 s))
          (let ((under (face-underline-color face)))
            (when under
              (sep)
              (write-color-code under 58 s)))))
  (write-char #\m s))

(defun term-render-ansi-line (term y)
  (let* ((w (term-width term))
         (row (aref (term-grid term) y))
         (prev-face nil)
         (x 0))
    (with-output-to-string (s)
                           (loop while (< x w) do
                                 (let ((ch (row-char row x))
                                       (face (row-face row x)))
                                   (unless (face-equal face prev-face)
                                     (write-sgr face s)
                                     (setf prev-face face))
                                   (write-char (if (graphic-char-p ch) ch #\Space) s)
                                   (incf x (if (and (= 2 (char-display-width ch)) (< (1+ x) w)) 2 1))))
                           (write-sgr nil s))))
