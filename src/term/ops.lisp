;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:term)

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
  "Put the cursor at row Y, column X, both one-based.

In origin mode Y counts from the scroll region's top and cannot leave it: a
program that only ever addresses inside its margins never needs to know where
on the real screen they are."
  (setf (term-wrap-pending term) nil)
  (setf (term-cursor-x term)
        (min (max (1- (or x 1)) 0) (1- (term-width term))))
  (if (term-origin-mode term)
      (let ((top (term-scroll-top term))
            (bot (term-scroll-bottom term)))
        (setf (term-cursor-y term)
              (min (max (+ top (1- (or y 1))) top) bot)))
      (setf (term-cursor-y term)
            (min (max (1- (or y 1)) 0) (1- (term-height term))))))

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
  "A face carrying only the current background, or nil when it is default.

BCE means the background paints what is erased; nothing else about the pen
does, so an erase under bold underlined red-on-blue leaves the blanks blue and
nothing more."
  (let ((bg (face-bg (term-attrs term))))
    (when bg (make-face :bg bg))))

(defun term-erase-in-line (term &optional (mode 0))
  (setf (term-wrap-pending term) nil)
  (let* ((y (term-cursor-y term))
         (x (term-cursor-x term))
         (w (term-width term))
         (row (term-grid-row term y))
         (bg-face (term-current-bg-face term)))
    (case mode
      (0 (blank-span row x w bg-face))
      (1 (blank-span row 0 (min w (1+ x)) bg-face))
      (2 (blank-span row 0 w bg-face)))))

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
         (clear-row (svref grid row-idx) bg-face)))
      (1
       (loop for row-idx from 0 below y do
         (clear-row (svref grid row-idx) bg-face))
       (term-erase-in-line term 1))
      ((2 3)
       ;; ED never moves the cursor, on any mode. It only erases.
       (clear-grid grid bg-face)
       (when (= mode 3)
         (when (term-scrollback term)
           (fill (term-scrollback term) nil))
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
    (blank-span row x (+ x count) bg-face)))

(defun term-insert-char (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let* ((y (term-cursor-y term))
         (x (term-cursor-x term))
         (w (term-width term))
         (row (term-grid-row term y))
         (count (min n (- w x)))
         (bg-face (term-current-bg-face term)))
    (move-span row x (+ x count) (- w x count))
    (blank-span row x (+ x count) bg-face)))

(defun term-delete-char (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let* ((y (term-cursor-y term))
         (x (term-cursor-x term))
         (w (term-width term))
         (row (term-grid-row term y))
         (count (min n (- w x)))
         (bg-face (term-current-bg-face term)))
    (move-span row (+ x count) x (- w x count))
    (blank-span row (- w count) w bg-face)))

(defun compact-faces (term)
  (let* ((old (term-face-table term))
         (map (make-array (length old) :element-type 'fixnum :initial-element -1))
         (new (make-array 64 :adjustable t :fill-pointer 1 :initial-element nil))
         (ring (term-scrollback term)))
    (setf (aref map 0) 0)
    (when ring
      (dotimes (n (term-scrollback-size term))
        (let* ((line (aref ring (mod (+ (term-scrollback-head term) n) (length ring))))
               (runs (line-runs line)))
          (dotimes (i (line-count line))
            (let* ((run (aref runs i))
                   (id (ash run -16)))
              (when (minusp (aref map id))
                (setf (aref map id) (fill-pointer new))
                (vector-push-extend (aref old id) new))
              (setf (aref runs i) (logior (ash (aref map id) 16) (logand run #xFFFF))))))))
    (setf (term-face-table term) new
          (term-face-last term) nil)
    (clrhash (term-face-ids term))
    (loop for id from 1 below (fill-pointer new)
          for face = (aref new id)
          do (push (cons face id) (gethash (face-key face) (term-face-ids term))))))

(defun face-id (term face)
  (cond ((or (null face) (face-default-p face)) 0)
        ((eq face (term-face-last term)) (term-face-last-id term))
        (t
         (let* ((key (face-key face))
                (id (or (cdr (assoc face (gethash key (term-face-ids term)) :test #'face-equal))
                        (progn
                          (when (>= (fill-pointer (term-face-table term)) +most-faces+)
                            (compact-faces term))
                          (if (>= (fill-pointer (term-face-table term)) +most-faces+)
                              0
                              (let ((id (fill-pointer (term-face-table term))))
                                (vector-push-extend face (term-face-table term))
                                (push (cons face id) (gethash key (term-face-ids term)))
                                id))))))
           (setf (term-face-last term) face
                 (term-face-last-id term) id)))))

(defun freeze-row (term row &optional reuse)
  (let* ((w (row-width row))
         (end (loop for x of-type fixnum from (1- w) downto 0
                    unless (and (char= (row-char row x) #\Space)
                                (let ((f (row-face row x))) (or (null f) (face-default-p f))))
                      return (1+ x)
                    finally (return 0)))
         (plain (let ((from (row-chars row)))
                  (declare (type (simple-array character (*)) from))
                  (loop for x of-type fixnum below end always (< (char-code (schar from x)) 128))))
         (scratch (if (>= (length (term-runs term)) w)
                      (term-runs term)
                      (setf (term-runs term) (make-array w :element-type '(unsigned-byte 32)))))
         (n 0)
         (last-face '#:none)
         (last-id -1))
    (declare (type fixnum w end n last-id) (type (simple-array (unsigned-byte 32) (*)) scratch))
    (dotimes (x end)
      (let ((f (row-face row x)))
        (unless (eq f last-face)
          (let ((id (face-id term f)))
            (declare (type fixnum id))
            (setf last-face f)
            (unless (= id last-id)
              (setf (aref scratch n) (logior (ash id 16) x)
                    last-id id)
              (incf n))))))
    (let ((line (if (and reuse
                         (>= (length (line-chars reuse)) end)
                         (or plain (not (typep (line-chars reuse) 'simple-base-string)))
                         (>= (length (line-runs reuse)) n))
                    reuse
                    (%make-line w
                                (if plain
                                    (make-string w :element-type 'base-char)
                                    (make-string w))
                                (make-array (max n 4) :element-type '(unsigned-byte 32))))))
      (let ((chars (line-chars line))
            (from (row-chars row)))
        (declare (type (simple-array character (*)) from))
        (etypecase chars
          (simple-base-string
           (dotimes (x end) (setf (schar chars x) (code-char (char-code (schar from x))))))
          ((simple-array character (*))
           (replace chars from :end1 end :end2 end))))
      (replace (line-runs line) scratch :end2 n)
      (setf (line-width line) w
            (line-end line) end
            (line-count line) n)
      line)))

(defun thaw-line (term line)
  (let* ((row (make-row (line-width line)))
         (chars (line-chars line))
         (runs (line-runs line))
         (count (line-count line))
         (table (term-face-table term))
         (end (min (line-end line) (line-width line))))
    (replace (row-chars row) chars :end2 end)
    (dotimes (i count row)
      (let* ((run (aref runs i))
             (from (logand run #xFFFF))
             (to (if (< (1+ i) count) (logand (aref runs (1+ i)) #xFFFF) end)))
        (fill (row-faces row) (aref table (ash run -16)) :start (min from end) :end (min to end))))))

(defun push-scrollback (term row)
  (let ((ring (term-scrollback term))
        (max (term-max-scrollback term)))
    (when (and ring (plusp max))
      ;; counted whether or not the ring was full, so whoever is looking at a
      ;; row in it can tell how far that row has moved
      (setf (term-scrollback-pushed term)
            (logand (1+ (term-scrollback-pushed term)) most-positive-fixnum))
      (let ((cap (length ring))
            (size (term-scrollback-size term))
            (head (term-scrollback-head term)))
        (declare (type fixnum cap size head))
        (cond
          ((>= size max)
           (setf (aref ring (mod (+ head size) cap))
                 (freeze-row term row (aref ring (mod (+ head size) cap)))
                 (term-scrollback-head term) (mod (1+ head) cap))
           (clear-row row)
           row)
          (t
           (when (= size cap)           ; grow the ring array toward max
             (let ((new (make-array (min (* 2 (max cap 64)) max)
                                    :initial-element nil)))
               (dotimes (i size)
                 (setf (aref new i) (aref ring (mod (+ head i) cap))))
               (setf (term-scrollback term) new
                     (term-scrollback-head term) 0
                     ring new cap (length new) head 0)))
           (setf (aref ring (mod (+ head size) cap)) (freeze-row term row)
                 (term-scrollback-size term) (1+ size))
           (clear-row row)
           row))))))

(defun term-trim-scrollback (term keep)
  (let ((ring (term-scrollback term))
        (size (term-scrollback-size term)))
    (when (and ring (> size keep))
      (let ((drop (- size (max 0 keep)))
            (cap (length ring)))
        (dotimes (i drop)
          (setf (aref ring (mod (+ (term-scrollback-head term) i) cap)) nil))
        (setf (term-scrollback-head term) (mod (+ (term-scrollback-head term) drop) cap)
              (term-scrollback-size term) (- size drop))
        drop))))

(defun term-scrollback-row (term n)
  (let ((ring (term-scrollback term))
        (size (term-scrollback-size term)))
    (when (and ring (<= 0 n) (< n size))
      (thaw-line term (aref ring (mod (+ (term-scrollback-head term) n) (length ring)))))))

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
          (let* ((row (the row (svref saved i)))
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
          (let ((row (the row (svref saved i))))
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
  (let* ((stops (term-tab-stops term))
         (w (term-width term))
         (x (term-cursor-x term)))
    (dotimes (i n)
      (setf x (or (position 1 stops :start (min w (1+ x))) (1- w))))
    (setf (term-cursor-x term) (min x (1- w)))))

(defun term-horizontal-backtab (term &optional (n 1))
  (setf (term-wrap-pending term) nil)
  (setf n (max n 1))
  (let* ((stops (term-tab-stops term))
         (x (term-cursor-x term)))
    (dotimes (i n)
      (setf x (or (position 1 stops :end x :from-end t) 0)))
    (setf (term-cursor-x term) (max x 0))))

(defun term-set-tab-stop (term)
  (setf (aref (term-tab-stops term) (term-cursor-x term)) 1))

(defun term-clear-tab-stop (term &optional (mode 0))
  (case mode
    (0 (setf (aref (term-tab-stops term) (term-cursor-x term)) 0))
    (3 (fill (term-tab-stops term) 0))))

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
  "Move down a line. Only carriage-returns as well when LNM is set -- most
programs send their own CR and expect LF to be nothing more than a line down."
  (setf (term-wrap-pending term) nil)
  (when (term-newline-mode term)
    (setf (term-cursor-x term) 0))
  (let* ((y (term-cursor-y term))
         (bot (term-scroll-bottom term)))
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

(defun term-align-test (term)
  "DECALN: every cell becomes E, in the default face, margins and cursor home."
  (let ((grid (term-grid term)))
    (dotimes (y (term-height term))
      (let ((row (svref grid y)))
        (fill (row-chars row) #\E)
        (fill (row-faces row) nil))))
  (setf (term-scroll-top term) 0
        (term-scroll-bottom term) (1- (term-height term))
        (term-wrap-pending term) nil)
  (term-goto term 1 1))

(defun term-soft-reset (term)
  "DECSTR: put the pen and the cursor back to how a program should assume they
start, without touching what is on the screen or in the scrollback."
  (setf (term-wrap-pending term) nil
        (term-insert-mode term) nil
        (term-origin-mode term) nil
        (term-auto-margin term) t
        (term-cursor-visible term) t
        (term-scroll-top term) 0
        (term-scroll-bottom term) (1- (term-height term))
        (term-g0 term) :us-ascii
        (term-g1 term) :us-ascii
        (term-g2 term) :us-ascii
        (term-g3 term) :us-ascii
        (term-active-charset term) :g0)
  (reset-face (term-attrs term))
  (setf (term-face-now term) nil)
  (term-goto term 1 1))

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
        (term-g3 term) :us-ascii
        (term-origin-mode term) nil
        (term-newline-mode term) nil
        (term-reverse-video term) nil
        (term-reverse-wraparound term) nil
        (term-synchronized-output term) nil
        (term-keypad-application-mode term) nil
        (term-mouse-mode term) nil
        (term-mouse-utf8 term) nil
        (term-mouse-sgr term) nil
        (term-mouse-urxvt term) nil
        (term-mouse-sgr-pixels term) nil
        (term-tab-stops term) (make-tab-stops (term-width term)))
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
        (let* ((from (svref old (+ y shift)))
               (into (svref new y))
               (n (min width (row-width from))))
          (replace (row-chars into) (row-chars from) :end1 n :end2 n)
          (replace (row-faces into) (row-faces from) :end1 n :end2 n))))
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
            (term-tab-stops term) (make-tab-stops width)
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
