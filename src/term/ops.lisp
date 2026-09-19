;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

(defun term-cursor-right (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let ((max-x (1- (term-width term))))
    (setf (term-cursor-x term)
          (min (+ (term-cursor-x term) n) max-x))))

(defun term-cursor-left (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (setf (term-cursor-x term)
        (max (- (term-cursor-x term) n) 0)))

(defun term-cursor-down (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let ((max-y (1- (term-height term))))
    (setf (term-cursor-y term)
          (min (+ (term-cursor-y term) n) max-y))))

(defun term-cursor-up (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (setf (term-cursor-y term)
        (max (- (term-cursor-y term) n) 0)))

(defun term-cursor-horizontal-abs (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf (term-cursor-x term)
        (min (max (1- n) 0) (1- (term-width term)))))

(defun term-cursor-vertical-abs (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf (term-cursor-y term)
        (min (max (1- n) 0) (1- (term-height term)))))

(defun term-goto (term &optional (y 1) (x 1))
  (setf (term-wrap-pending term) nil)
  (setf (term-cursor-y term)
        (min (max (1- (or y 1)) 0) (1- (term-height term))))
  (setf (term-cursor-x term)
        (min (max (1- (or x 1)) 0) (1- (term-width term)))))

(defun term-save-cursor (term)
  (if (term-in-alt-screen term)
      (setf (term-alt-saved-cursor-x term) (term-cursor-x term)
            (term-alt-saved-cursor-y term) (term-cursor-y term)
            (term-alt-saved-attrs term) (copy-face (term-attrs term)))
      (setf (term-saved-cursor-x term) (term-cursor-x term)
            (term-saved-cursor-y term) (term-cursor-y term)
            (term-saved-attrs term) (copy-face (term-attrs term)))))

(defun term-restore-cursor (term)
  "Put the cursor back where it was saved, inside the screen it is on now.

The screen may have been resized since, and a position saved on a bigger one is
not an index into this one."
  (setf (term-wrap-pending term) nil)
  (let ((alt (term-in-alt-screen term)))
    (let ((x (if alt (term-alt-saved-cursor-x term) (term-saved-cursor-x term)))
          (y (if alt (term-alt-saved-cursor-y term) (term-saved-cursor-y term)))
          (attrs (if alt (term-alt-saved-attrs term) (term-saved-attrs term))))
      (setf (term-cursor-x term) (max 0 (min x (1- (term-width term))))
            (term-cursor-y term) (max 0 (min y (1- (term-height term))))
            (term-face-now term) nil)
      (when attrs (face-into (term-attrs term) attrs)))))

(defun term-current-bg-face (term)
  "Return a shared face for the current background, or nil when default."
  (when (face-bg (term-attrs term))
    (intern-face term)))

(defun term-erase-in-line (term &optional (mode 0))
  (setf (term-wrap-pending term) nil)
  (let* ((y (term-cursor-y term))
         (x (term-cursor-x term))
         (w (term-width term))
         (row (term-grid-row term y))
         (bg-face (term-current-bg-face term)))
    (case mode
      (0
       (loop for i from x below w do
         (setf (cell-char (aref row i)) #\Space
               (cell-face (aref row i)) bg-face)))
      (1
       (loop for i from 0 to x do
         (setf (cell-char (aref row i)) #\Space
               (cell-face (aref row i)) bg-face)))
      (2
       (loop for i from 0 below w do
         (setf (cell-char (aref row i)) #\Space
               (cell-face (aref row i)) bg-face))))))

(defun term-erase-in-display (term &optional (mode 0))
  (setf (term-wrap-pending term) nil)
  (let* ((y (term-cursor-y term))
         (h (term-height term))
         (grid (term-grid term))
         (bg-face (term-current-bg-face term)))
    (case mode
      (0
       (term-erase-in-line term 0)
       (loop for row-idx from (1+ y) below h do
         (clear-row (the simple-vector (aref grid row-idx)) bg-face)))
      (1
       (loop for row-idx from 0 below y do
         (clear-row (the simple-vector (aref grid row-idx)) bg-face))
       (term-erase-in-line term 1))
      ((2 3)
       (clear-grid grid bg-face)
       (term-goto term 1 1)
       (when (= mode 3)
         (setf (term-scrollback-size term) 0
               (term-scrollback-head term) 0))))))

(defun term-erase-char (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let* ((y (term-cursor-y term))
         (x (term-cursor-x term))
         (w (term-width term))
         (row (term-grid-row term y))
         (count (min n (- w x)))
         (bg-face (term-current-bg-face term)))
    (loop for i from x below (+ x count) do
      (setf (cell-char (aref row i)) #\Space
            (cell-face (aref row i)) bg-face))))

(defun term-insert-char (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let* ((y (term-cursor-y term))
         (x (term-cursor-x term))
         (w (term-width term))
         (row (term-grid-row term y))
         (count (min n (- w x)))
         (bg-face (term-current-bg-face term)))
    (loop for i from (1- w) downto (+ x count) do
      (let ((src (aref row (- i count))))
        (setf (cell-char (aref row i)) (cell-char src)
              (cell-face (aref row i)) (cell-face src))))
    (loop for i from x below (+ x count) do
      (setf (cell-char (aref row i)) #\Space
            (cell-face (aref row i)) bg-face))))

(defun term-delete-char (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let* ((y (term-cursor-y term))
         (x (term-cursor-x term))
         (w (term-width term))
         (row (term-grid-row term y))
         (count (min n (- w x)))
         (bg-face (term-current-bg-face term)))
    (loop for i from x below (- w count) do
      (let ((src (aref row (+ i count))))
        (setf (cell-char (aref row i)) (cell-char src)
              (cell-face (aref row i)) (cell-face src))))
    (loop for i from (- w count) below w do
      (setf (cell-char (aref row i)) #\Space
            (cell-face (aref row i)) bg-face))))

(defun push-scrollback (term row)
  "Take ownership of ROW into the scrollback ring and return a clean replacement
row for the grid, or nil when scrollback is off (caller keeps ROW). Rows are
allocated only until the ring reaches max-scrollback; at capacity the evicted
oldest row is recycled as the replacement, so steady-state scrolling is O(1)
pointer moves with no consing."
  (let ((ring (term-scrollback term))
        (max (term-max-scrollback term)))
    (when (and ring (plusp max))
      (let ((cap (length ring))
            (size (term-scrollback-size term))
            (head (term-scrollback-head term)))
        (declare (type fixnum cap size head))
        (cond
          ((>= size max)                ; at capacity: evict oldest, recycle it
           (let ((evicted (aref ring head)))
             (setf (aref ring (mod (+ head size) cap)) row
                   (term-scrollback-head term) (mod (1+ head) cap))
             (if (= (length (the simple-vector evicted)) (term-width term))
                 (progn (clear-row evicted) evicted)
                 (make-row (term-width term)))))
          (t
           (when (= size cap)           ; grow the ring array toward max
             (let ((new (make-array (min (* 2 (max cap 64)) max)
                                    :initial-element nil)))
               (dotimes (i size)
                 (setf (aref new i) (aref ring (mod (+ head i) cap))))
               (setf (term-scrollback term) new
                     (term-scrollback-head term) 0
                     ring new cap (length new) head 0)))
           (setf (aref ring (mod (+ head size) cap)) row
                 (term-scrollback-size term) (1+ size))
           (make-row (term-width term))))))))

(defun term-scrollback-row (term n)
  (let ((ring (term-scrollback term))
        (size (term-scrollback-size term)))
    (when (and ring (<= 0 n) (< n size))
      (aref ring (mod (+ (term-scrollback-head term) n) (length ring))))))

(defun term-scroll-up (term &optional (n 1) (keep t))
  "Move the scroll region up N lines.

KEEP says whether what goes off the top is kept in the scrollback. It is when
the screen itself scrolled; it is not when a line was deleted out from under the
cursor, which takes a line out of the region rather than pushing one off the
screen, and a line that never left the screen has no business in what is behind
it."
  (setf n (max n 1))
  (let* ((top (term-scroll-top term))
         (bot (term-scroll-bottom term))
         (region-height (1+ (- bot top)))
         (count (min n region-height))
         (grid (term-grid term)))
    (when (plusp count)
      (let ((saved (make-array count :initial-element nil))
            (record (and keep (zerop top) (not (term-in-alt-screen term)))))
        (dotimes (i count)
          (setf (aref saved i) (aref grid (+ top i))))
        (loop for i from top to (- bot count) do
          (setf (aref grid i) (aref grid (+ i count))))
        (dotimes (i count)
          (let* ((row (the simple-vector (aref saved i)))
                 (repl (and record (push-scrollback term row))))
            (unless repl
              (clear-row row)
              (setf repl row))
            (setf (aref grid (+ bot (- count) 1 i)) repl)))))))

(defun term-scroll-down (term &optional (n 1))
  (setf n (max n 1))
  (let* ((top (term-scroll-top term))
         (bot (term-scroll-bottom term))
         (region-height (1+ (- bot top)))
         (count (min n region-height))
         (grid (term-grid term)))
    (when (plusp count)
      (let ((saved (make-array count :initial-element nil)))
        (dotimes (i count)
          (setf (aref saved i) (aref grid (+ bot (- count) 1 i))))
        (loop for i from bot downto (+ top count) do
          (setf (aref grid i) (aref grid (- i count))))
        (dotimes (i count)
          (let ((row (the simple-vector (aref saved i))))
            (clear-row row)
            (setf (aref grid (+ top i)) row)))))))

(defun term-insert-line (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let ((y (term-cursor-y term))
        (top (term-scroll-top term))
        (bot (term-scroll-bottom term)))
    (when (<= top y bot)
      (let ((old-top (term-scroll-top term)))
        (setf (term-scroll-top term) y)
        (term-scroll-down term (min n (1+ (- bot y))))
        (setf (term-scroll-top term) old-top)))))

(defun term-delete-line (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let ((y (term-cursor-y term))
        (top (term-scroll-top term))
        (bot (term-scroll-bottom term)))
    (when (<= top y bot)
      (let ((old-top (term-scroll-top term)))
        (setf (term-scroll-top term) y)
        (term-scroll-up term (min n (1+ (- bot y))) nil)
        (setf (term-scroll-top term) old-top)))))

(defun term-horizontal-tab (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let* ((x (term-cursor-x term))
         (w (term-width term))
         (next-tab (+ x (- 8 (mod x 8)))))
    (dotimes (i (1- n))
      (setf next-tab (+ next-tab 8)))
    (setf (term-cursor-x term) (min next-tab (1- w)))))

(defun term-horizontal-backtab (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let* ((x (term-cursor-x term))
         (prev-tab (if (zerop (mod x 8))
                       (- x 8)
                       (- x (mod x 8)))))
    (dotimes (i (1- n))
      (decf prev-tab 8))
    (setf (term-cursor-x term) (max prev-tab 0))))

(defun term-index (term)
  (setf (term-wrap-pending term) nil)
  (let ((y (term-cursor-y term))
        (bot (term-scroll-bottom term)))
    (if (= y bot)
        (term-scroll-up term 1)
        (when (< y (1- (term-height term)))
          (incf (term-cursor-y term))))))

(defun term-reverse-index (term)
  (setf (term-wrap-pending term) nil)
  (let ((y (term-cursor-y term))
        (top (term-scroll-top term)))
    (if (= y top)
        (term-scroll-down term 1)
        (when (> y 0)
          (decf (term-cursor-y term))))))

(defun term-line-feed (term)
  (setf (term-wrap-pending term) nil)
  (let* ((y (term-cursor-y term))
         (bot (term-scroll-bottom term)))
    (setf (term-cursor-x term) 0)
    (if (= y bot)
        (term-scroll-up term 1)
        (when (< y (1- (term-height term)))
          (incf (term-cursor-y term))))))

(defun term-carriage-return (term)
  (setf (term-wrap-pending term) nil)
  (setf (term-cursor-x term) 0))

(defun term-set-scroll-region (term &optional top bottom)
  (let ((t-val (or top 1))
        (b-val (or bottom (term-height term))))
    (when (< 0 t-val b-val (1+ (term-height term)))
      (setf (term-scroll-top term) (1- t-val)
            (term-scroll-bottom term) (1- b-val))
      (term-goto term 1 1))))

(defun term-enter-alt-screen (term)
  (unless (term-in-alt-screen term)
    (setf (term-main-grid term) (term-grid term)
          (term-grid term) (make-grid (term-width term) (term-height term))
          (term-in-alt-screen term) t
          (term-alt-saved-cursor-x term) 0
          (term-alt-saved-cursor-y term) 0)
    (term-goto term 1 1)))

(defun term-exit-alt-screen (term)
  (when (term-in-alt-screen term)
    (setf (term-grid term) (term-main-grid term)
          (term-main-grid term) nil
          (term-in-alt-screen term) nil)))

(defun term-reset (term)
  (setf (term-wrap-pending term) nil)
  (setf (term-parser-state term) nil
        (term-cursor-x term) 0
        (term-cursor-y term) 0
        (term-scroll-top term) 0
        (term-scroll-bottom term) (1- (term-height term))
        (term-auto-margin term) t
        (term-insert-mode term) nil
        (term-keypad-mode term) nil
        (term-bracketed-paste term) nil
        (term-cursor-visible term) t
        (term-cursor-style term) :block
        (term-active-charset term) :g0
        (term-g0 term) :us-ascii
        (term-g1 term) :us-ascii
        (term-g2 term) :us-ascii
        (term-g3 term) :us-ascii)
  (reset-face (term-attrs term))
  (setf (term-face-now term) nil)
  (clear-grid (term-grid term))
  (when (term-in-alt-screen term)
    (term-exit-alt-screen term)))

(defun regrid (old width height shift)
  "A grid WIDTH by HEIGHT holding OLD from row SHIFT down, clipped to both."
  (let ((new (make-grid width height)))
    (when old
      (dotimes (y (min height (max 0 (- (length old) shift))))
        (let ((from (aref old (+ y shift)))
              (into (aref new y)))
          (dotimes (x (min width (length from)))
            (let ((src (aref from x))
                  (dst (aref into x)))
              (setf (cell-char dst) (cell-char src)
                    (cell-face dst) (cell-face src)))))))
    new))

(defun term-resize (term width height)
  "Make the screen WIDTH by HEIGHT, keeping what the cursor is standing on.

A shorter screen loses rows off the top, not the bottom: what is under the
cursor is the prompt somebody is typing at, and a shell whose prompt was cut
away has been scrambled rather than resized. The rows that go, go to the
scrollback, which is where they would have gone had the screen scrolled."
  (when (and (plusp width) (plusp height)
             (or (/= width (term-width term))
                 (/= height (term-height term))))
    (let* ((alt (term-in-alt-screen term))
           (main (if alt (term-main-grid term) (term-grid term)))
           (shift (if alt 0 (max 0 (- (1+ (term-cursor-y term)) height)))))
      (dotimes (y (min shift (if main (length main) 0)))
        (push-scrollback term (aref main y)))
      (let ((new-main (regrid main width height shift)))
        (if alt
            (setf (term-main-grid term) new-main
                  (term-grid term) (regrid (term-grid term) width height 0))
            (setf (term-grid term) new-main)))
      (setf (term-width term) width
            (term-height term) height
            (term-scroll-top term) 0
            (term-scroll-bottom term) (1- height)
            (term-wrap-pending term) nil
            (term-cursor-x term) (min (term-cursor-x term) (1- width))
            (term-cursor-y term) (max 0 (min (- (term-cursor-y term) shift)
                                             (1- height)))
            (term-saved-cursor-x term) (min (term-saved-cursor-x term)
                                            (1- width))
            (term-saved-cursor-y term) (max 0 (min (- (term-saved-cursor-y term)
                                                      shift)
                                                   (1- height)))
            (term-alt-saved-cursor-x term) (min (term-alt-saved-cursor-x term)
                                                (1- width))
            (term-alt-saved-cursor-y term) (max 0
                                                (min (term-alt-saved-cursor-y term)
                                                     (1- height)))))))
