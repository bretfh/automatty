;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

(declaim (optimize (speed 3) (safety 1)))

(defstruct (screen (:constructor %make-screen))
           (width 80 :type fixnum)
           (height 24 :type fixnum)
           (grid #() :type simple-vector)
           (cursor-x 0 :type fixnum)
           (cursor-y 0 :type fixnum)
           (cursor-visible t :type boolean)
           (cursor-style :block))

(defun make-screen-grid (width height)
  (let ((grid (make-array height)))
    (dotimes (y height grid)
      (let ((row (make-array width)))
        (dotimes (x width)
          (setf (svref row x) (vt:make-cell)))
        (setf (svref grid y) row)))))

(defun make-screen (&key (width 80) (height 24))
  (%make-screen :width width :height height
                :grid (make-screen-grid width height)))

(declaim (inline screen-row))
(defun screen-row (screen y)
  (the simple-vector (svref (screen-grid screen) y)))

(defun screen-resize (screen width height)
  (setf (screen-width screen) width
        (screen-height screen) height
        (screen-grid screen) (make-screen-grid width height)
        (screen-cursor-x screen) (min (screen-cursor-x screen) (1- width))
        (screen-cursor-y screen) (min (screen-cursor-y screen) (1- height)))
  screen)

(defun screen-clear (screen)
  (dotimes (y (screen-height screen) screen)
    (let ((row (screen-row screen y)))
      (dotimes (x (screen-width screen))
        (let ((cell (svref row x)))
          (setf (vt:cell-char cell) #\Space
                (vt:cell-face cell) nil))))))

(defun screen-blit (screen term &key (top 0) (left 0))
  "Copy what TERM holds into SCREEN at TOP LEFT, clipped to both.

The cells are copied, never shared: the pane goes on writing into its own grid,
and a screen that pointed at those cells would show every later frame as
already sent and repaint nothing ever again."
  (declare (type fixnum top left))
  (let ((rows (min (vt:term-height term) (- (screen-height screen) top)))
        (cols (min (vt:term-width term) (- (screen-width screen) left))))
    (declare (type fixnum rows cols))
    (dotimes (y rows screen)
      (when (>= (+ top y) 0)
        (let ((from (vt:term-grid-row term y))
              (into (screen-row screen (+ top y))))
          (dotimes (x cols)
            (let ((a (svref from x))
                  (b (svref into (+ left x))))
              (setf (vt:cell-char b) (vt:cell-char a)
                    (vt:cell-face b) (vt:cell-face a)))))))))

(defstruct (run (:constructor make-run (row start end)))
           (row 0 :type fixnum)
           (start 0 :type fixnum)
           (end 0 :type fixnum))

(defvar *gap* 4
  "How many cells that did not move are carried inside a run rather than
started over. Ending a run and beginning another costs a cursor address, so a
few unchanged cells are cheaper written again than jumped over.")

(declaim (inline same-cell-p))
(defun same-cell-p (a b)
  (and (char= (vt:cell-char a) (vt:cell-char b))
       (let ((fa (vt:cell-face a)) (fb (vt:cell-face b)))
         (or (eq fa fb) (vt:face-attrs-equal fa fb)))))

(defun widened (row start)
  "START, or one column back when what sits there is the right half of a wide
character: writing that half alone would put the terminal a column out."
  (declare (type simple-vector row) (type fixnum start))
  (if (and (plusp start)
           (= 2 (vt:char-display-width (vt:cell-char (svref row (1- start))))))
      (1- start)
    start))

(defun screen-diff (was now &optional (gap *gap*))
  "The runs of cells by which NOW differs from WAS, and WAS brought up to NOW.

One pass: a cell that moved is both reported and copied, so the caller holds
what it is about to send and the shadow already says it was sent."
  (declare (type fixnum gap))
  (let ((w (screen-width now))
        (runs nil))
    (declare (type fixnum w))
    (dotimes (y (screen-height now))
      (let ((old (screen-row was y))
            (new (screen-row now y))
            (start -1)
            (end -1))
        (declare (type fixnum start end))
        (dotimes (x w)
          (let ((a (svref old x))
                (b (svref new x)))
            (unless (same-cell-p a b)
              (setf (vt:cell-char a) (vt:cell-char b)
                    (vt:cell-face a) (vt:cell-face b))
              (when (and (not (minusp start)) (> (- x end) gap))
                (push (make-run y (widened new start) end) runs)
                (setf start -1))
              (when (minusp start)
                (setf start x))
              (setf end (1+ x)))))
        (unless (minusp start)
          (push (make-run y (widened new start) end) runs))))
    (nreverse runs)))

(defun screen-dump-to-string (screen)
  (with-output-to-string (s)
                         (dotimes (y (screen-height screen))
                           (let ((row (screen-row screen y)))
                             (dotimes (x (screen-width screen))
                               (write-char (vt:cell-char (svref row x)) s)))
                           (unless (= y (1- (screen-height screen)))
                             (terpri s)))))
