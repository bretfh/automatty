;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:atty/cells
            (:use #:cl)
            (:local-nicknames (#:ui #:atty/ui))
            (:export
             #:cells
             #:make-cells
             #:cells-grid
             #:cells-cols
             #:cells-rows
             #:columns-of
             #:say-at
             #:fill-rect
             #:blit
             #:face-of
             #:draw))
(in-package #:atty/cells)

;;; A rule carries the glyph it is made of, a choice carries the two characters
;;; that mark it, and what is under the pointer is asked for by line and column.
;;; All this has to say is how wide a string is and where its characters go.

(defclass cells (ui:medium)
  ((grid :initarg :grid :reader cells-grid)
   (cols :initarg :cols :reader cells-cols)
   (rows :initarg :rows :reader cells-rows)))

(defun make-cells (grid cols rows)
  (make-instance 'cells :grid grid :cols cols :rows rows))

(defun columns-of (text)
  "How many columns TEXT takes, which is not how many characters it has."
  (let ((n 0))
    (map nil (lambda (ch) (incf n (vt:char-display-width ch))) text)
    n))

(defmethod ui:text-size ((m cells) text font &optional bold family)
           (declare (ignore font bold family))
           (values (columns-of (princ-to-string text)) 1))

(defvar *under* nil
  "The background in force where the painting is happening. A widget that sets
one puts it here for whatever it holds, because a label with no background of
its own should sit on whatever it was drawn onto rather than punch a hole in
it.")

(defun face-of (widget)
  "The face WIDGET wears, as a terminal says it. A background of -1 is the theme
saying there is none, which means whatever it is being drawn onto."
  (multiple-value-bind (fr fg fb br bg bb attr)
                       (ui:ink (or (ui:face widget) :default))
                       (let* ((style (ui:style-of widget))
                              (fill (or (ui:background-color widget)
                                        (getf style :background-color))))
                         (vt:make-face
                          :fg (or (getf style :color) (list fr fg fb))
                          :bg (cond ((consp fill) (subseq fill 0 3))
                                    ((minusp br) *under*)
                                    (t (list br bg bb)))
                          :bold (ui:boldp widget)
                          :italic (ui:italicp widget)
                          :underline (ui:underline-of widget)
                          :crossed (and (logtest 8 attr) t)))))

(defun say-at (m col line text face)
  "Write TEXT into the grid at COL LINE, stopping at the edge."
  (let ((grid (cells-grid m)))
    (when (and (<= 0 line) (< line (cells-rows m)))
      (let ((row (svref grid line))
            (x col))
        (map nil
             (lambda (ch)
               (when (and (<= 0 x) (< x (cells-cols m)))
                 (setf (vt:row-char row x) ch
                       (vt:row-face row x) face))
               (incf x (max 1 (vt:char-display-width ch))))
             text)
        x))))

(defun fill-rect (m col line width height face &optional (char #\Space))
  (let ((grid (cells-grid m)))
    (loop :for y :from (max 0 line) :below (min (cells-rows m) (+ line height))
          :do (let ((row (svref grid y))
                    (from (max 0 col))
                    (to (min (cells-cols m) (+ col width))))
                (when (< from to)
                  (fill (vt:row-chars row) char :start from :end to)
                  (fill (vt:row-faces row) face :start from :end to))))))

(defun blit (m term col line width height)
  "Copy what TERM holds into the grid at COL LINE, clipped to WIDTH by HEIGHT
and to the grid itself.

The cells are copied, never shared: the program goes on writing into its own
grid, and a grid that pointed at those cells would show every later frame as
already sent and repaint nothing ever again."
  (declare (type fixnum col line width height)
           (optimize (speed 3) (safety 1)))
  (let ((grid (the simple-vector (cells-grid m)))
        (rows (min (vt:term-height term) height (- (cells-rows m) line)))
        (cols (min (vt:term-width term) width (- (cells-cols m) col))))
    (declare (type fixnum rows cols))
    (loop :for y :of-type fixnum :from (max 0 (- line)) :below rows
          :do (let* ((from (vt:term-grid-row term y))
                     (into (svref grid (+ line y)))
                     (at (max 0 (- col)))
                     (n (- cols at)))
                (declare (type fixnum at n))
                (when (plusp n)
                  (replace (vt:row-chars into) (vt:row-chars from)
                           :start1 (+ col at) :start2 at :end2 cols)
                  (replace (vt:row-faces into) (vt:row-faces from)
                           :start1 (+ col at) :start2 at :end2 cols))))
    m))

(defmethod ui:paint :around ((w ui:widget) (m cells))
           (let* ((style (ui:style-of w))
                  (fill (or (ui:background-color w) (getf style :background-color))))
             (if (consp fill)
                 (let ((*under* (subseq fill 0 3)))
                   (when (and (plusp (ui:width w)) (plusp (ui:height w)))
                     (fill-rect m (ui:left w) (ui:top w) (ui:width w) (ui:height w)
                                (vt:make-face :bg *under*)))
                   (call-next-method))
               (call-next-method))))

(defmethod ui:paint ((w ui:label) (m cells))
           (say-at m (ui:left w) (ui:top w) (ui:text w) (face-of w)))

(defmethod ui:paint ((w ui:rule) (m cells))
           (let ((face (face-of w))
                 (glyph (ui:rule-glyph w)))
             (if (ui:upright w)
                 (fill-rect m (ui:left w) (ui:top w) 1 (max 1 (ui:height w)) face glyph)
               (fill-rect m (ui:left w) (ui:top w) (max 1 (ui:width w)) 1 face glyph))))

(defmethod ui:paint ((w ui:choice) (m cells))
           (say-at m (ui:left w) (ui:top w) (ui:mark-of w) (face-of w))
           (call-next-method))

(defmethod ui:paint ((w ui:slider) (m cells))
           (let* ((face (face-of w))
                  (width (max 1 (ui:width w)))
                  (full (round (* (ui:fraction w) width))))
             (fill-rect m (ui:left w) (ui:top w) full 1 face (code-char #x2588))
             (fill-rect m (+ (ui:left w) full) (ui:top w) (- width full) 1 face
                        (code-char #x2591))))

(defun draw (tree grid cols rows &key (left 0) (top 0))
  "Measure TREE against a grid that size, lay it out, and paint it in. Answers
the medium, so a caller may go on drawing on the same grid."
  (let ((m (make-cells grid cols rows)))
    (ui:with-pass
     (ui:restyle tree)
     (multiple-value-bind (w h) (ui:measure tree m (- cols left) (- rows top))
                          (declare (ignore w h))
                          (ui:lay tree m left top (- cols left) (- rows top))
                          (ui:paint tree m)))
    m))
