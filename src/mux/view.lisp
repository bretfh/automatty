;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; A pane is a widget like any other. What that buys is the layout pass: a
;;; split is a row or a column of these, the bar is the last child of the
;;; column they sit in, and the arithmetic that hands each of them its rows and
;;; columns is the one src/ui/layout.lisp already does for everything else.

(defclass pane-view (atty/ui:widget)
  ((pane :initarg :pane :reader view-pane)))

(defun pane-view (pane &rest props)
  (apply #'make-instance 'pane-view :pane pane :expand 1 props))

(defmethod atty/ui:measure ((w pane-view) m aw ah)
  "Nothing of its own, and it grows. A program has no size it wants: it is given
one and told what it is, so what it asks for is the room left over."
  (declare (ignore m aw ah))
  (values 0 0))

(defun hit-face (currentp)
  (term:make-face :fg (atty/ui:unhex (atty/ui:color 'atty/ui::bg))
                  :bg (atty/ui:unhex (atty/ui:color (if currentp 'atty/ui::yellow 'atty/ui::yellow-cooler)))
                  :bold currentp))

(defun selection-face ()
  (term:make-face :bg (atty/ui:unhex (atty/ui:color 'atty/ui::bg-active))))

(defmethod atty/ui:paint ((w pane-view) (m atty/cells:cells))
  (let* ((pane (view-pane w))
         (top (atty/ui:top w)) (left (atty/ui:left w))
         (height (atty/ui:height w)) (width (atty/ui:width w)))
    (atty/cells:blit m (pane-term pane) left top width height (pane-scrolled pane))
    ;; what a find found, and what is being selected, over the top: a hit is
    ;; lit where it is, the one gone to brightest, and selected rows are shaded
    (let* ((find (pane-find pane))
           (hits (getf find :hits))
           (at (getf find :at))
           (first-shown (pane-top-row pane))
           (grid (atty/cells:cells-grid m)))
      (when (pane-selecting pane)
        (let* ((mark (pane-selecting pane))
               (from (min mark first-shown))
               (to (+ (max mark first-shown) height)))
          (loop :for a :from from :below to
                :for y := (+ top (- a first-shown))
                :when (and (<= top y) (< y (+ top height)) (< y (atty/cells:cells-rows m)))
                  :do (let ((row (svref grid y)))
                        (loop :for x :from left :below (min (+ left width) (atty/cells:cells-cols m))
                              :do (let ((was (term:row-face row x)))
                                    (setf (term:row-face row x)
                                          (term:make-face :fg (and was (term:face-fg was))
                                                          :bg (term:face-bg (selection-face))))))))))
      (loop :for hit :in hits
            :for i :from 0
            :for y := (+ top (- (first hit) first-shown))
            :when (and (<= top y) (< y (+ top height)) (< y (atty/cells:cells-rows m)))
              :do (let ((row (svref grid y))
                        (face (hit-face (eql i at))))
                    (loop :for x :from (+ left (second hit)) :below (min (+ left (third hit))
                                                                          (+ left width)
                                                                          (atty/cells:cells-cols m))
                          :do (setf (term:row-face row x) face)))))))

(defmethod atty/ui:under ((w pane-view) line col)
  "A pane-view covers whatever it was laid out to, same test the click-through
widgets already use, just answering with itself rather than an action to run."
  (when (and (<= (atty/ui:top w) line) (< line (atty/ui:bottom w))
             (<= (atty/ui:left w) col) (< col (atty/ui:right w)))
    w))

;;; A scrollbar, in a column of its own down the right of the pane. The column
;;; is the scrollbar's and the program is told it is that much narrower, so
;;; nothing it draws is ever under it and nothing moves when the scrolling
;;; starts. An arrow at each end, a track between them, and a thumb on the track
;;; as long as the share of the history the pane shows and as far up as the pane
;;; is scrolled back.

(defparameter +scrollbar-glyphs+ '(:up #\▲ :down #\▼ :track #\░ :thumb #\█))

(defparameter +arrows-from+ 4
  "A scrollbar shorter than this is all track: two arrows would leave no room
for a thumb to be anywhere.")

(defclass scrollbar (atty/ui:widget)
  ((pane :initarg :pane :reader view-pane)))

(defun scrollbar (pane &rest props)
  (apply #'make-instance 'scrollbar :pane pane props))

(defmethod atty/ui:measure ((w scrollbar) m aw ah)
  (declare (ignore m aw ah))
  (values 1 0))

(defmethod atty/ui:under ((w scrollbar) line col)
  (when (and (<= (atty/ui:top w) line) (< line (atty/ui:bottom w))
             (<= (atty/ui:left w) col) (< col (atty/ui:right w)))
    w))

(defun scrollbar-thumb (track rows history back)
  "Where the thumb is on a track TRACK cells long, for a pane of ROWS with
HISTORY behind it that is BACK rows up: how far down the track it starts, and
how long it is. At the foot when the pane is live and at the head when it is as
far back as there is."
  (let* ((total (+ history rows))
         (long (max 1 (min track (round (* track rows) (max 1 total)))))
         (room (- track long)))
    (values (if (plusp history)
                (round (* room (- history (min back history))) history)
                room)
            long)))

(defun scrollbar-track (bar)
  "Which of BAR's lines its track is: the first, and how many."
  (let ((high (atty/ui:height bar)))
    (if (>= high +arrows-from+)
        (values (1+ (atty/ui:top bar)) (- high 2))
        (values (atty/ui:top bar) high))))

(defun scrollbar-part (bar line)
  "What of BAR is at LINE: :up or :down for an arrow, :thumb, or :above or
:below for the track either side of it. Nil when there is nothing to scroll."
  (let* ((pane (view-pane bar))
         (history (pane-history pane)))
    (when (plusp history)
      (multiple-value-bind (from track) (scrollbar-track bar)
        (cond
          ((< line from) :up)
          ((>= line (+ from track)) :down)
          (t (multiple-value-bind (top long)
                 (scrollbar-thumb track (term:term-height (pane-term pane))
                                  history (pane-scrolled pane))
               (let ((at (- line from)))
                 (cond ((< at top) :above)
                       ((>= at (+ top long)) :below)
                       (t :thumb))))))))))

(defun scrollbar-back-at (bar line grabbed)
  "How far back the pane is with the thumb dragged so the cell of it GRABBED
cells down from its head is at LINE."
  (let* ((pane (view-pane bar))
         (history (pane-history pane)))
    (multiple-value-bind (from track) (scrollbar-track bar)
      (multiple-value-bind (top long)
          (scrollbar-thumb track (term:term-height (pane-term pane))
                           history (pane-scrolled pane))
        (declare (ignore top))
        (let* ((room (- track long))
               (head (max 0 (min room (- line from grabbed)))))
          (if (plusp room)
              (round (* history (- room head)) room)
              0))))))

(defmethod atty/ui:paint ((w scrollbar) (m atty/cells:cells))
  (let* ((pane (view-pane w))
         (history (pane-history pane))
         (back (pane-scrolled pane))
         (col (atty/ui:left w)))
    (when (plusp (atty/ui:width w))
      (flet ((ink (face) (setf (atty/ui:face w) face) (atty/cells:face-of w))
             (glyph (name) (getf +scrollbar-glyphs+ name)))
        (multiple-value-bind (from track) (scrollbar-track w)
          (let ((arrows (/= from (atty/ui:top w))))
            (atty/cells:fill-rect m col from 1 track (ink :scroll-track) (glyph :track))
            (when arrows
              (atty/cells:fill-rect m col (atty/ui:top w) 1 1
                                    (ink (if (< back history) :scroll-arrow :scroll-track))
                                    (glyph :up))
              (atty/cells:fill-rect m col (1- (atty/ui:bottom w)) 1 1
                                    (ink (if (plusp back) :scroll-arrow :scroll-track))
                                    (glyph :down)))
            (when (plusp history)
              (multiple-value-bind (top long)
                  (scrollbar-thumb track (term:term-height (pane-term pane)) history back)
                (atty/cells:fill-rect m col (+ from top) 1 long
                                      (ink (if (plusp back) :scroll-thumb-back :scroll-thumb))
                                      (glyph :thumb))))))))))

;;; What says a pane is being read back, and takes it to the foot when clicked.

(defclass live-chip (atty/ui:label)
  ((pane :initarg :pane :reader view-pane)))

(defun live-chip (pane)
  (make-instance 'live-chip :pane pane :face :chip-scrolled
                            :text (format nil " ↓ ~D to live " (pane-scrolled pane))))

(declaim (ftype function find-marker))

(defun reading-strip (pane)
  "What a pane read back says over its last line when it has no frame to say
it in: what was found, a way to find, and the way back to live."
  (apply #'atty/ui:row :spacing 0
         (remove nil
                 ;; the ways out first: what was found is cut when the pane is narrow
                 (list (and (plusp (pane-scrolled pane)) (live-chip pane))
                       (bar-button "find in pane" (atty/ui:label " ⌕ find " :face :quiet))
                       (and (pane-find pane) (find-marker pane))))))

(defmethod atty/ui:under ((w live-chip) line col)
  (when (and (<= (atty/ui:top w) line) (< line (atty/ui:bottom w))
             (<= (atty/ui:left w) col) (< col (atty/ui:right w)))
    w))

;;; A pane and its scrollbar, as the one thing a frame goes round or a split
;;; holds.

(defparameter +scrollbar-from+ 4
  "A pane narrower than this keeps every column for its program.")

(defclass pane-area (atty/ui:widget)
  ((view :initarg :view :reader area-view)
   (bar :initarg :bar :reader area-bar)
   (chip :initarg :chip :reader area-chip)))

(defun pane-area (pane &key (scrollbarp t) chipp)
  "PANE with a scrollbar down its right when SCROLLBARP. CHIPP is for a pane
with no frame to say it in: the chip goes over the pane's own last line."
  (let ((view (pane-view pane))
        (bar (and scrollbarp (scrollbar pane)))
        (chip (and chipp (or (plusp (pane-scrolled pane)) (pane-find pane))
                   (reading-strip pane))))
    (make-instance 'pane-area :expand 1 :view view :bar bar :chip chip
                              ;; the chip last, so it is painted over the pane
                              :parts (remove nil (list view bar chip)))))

(defmethod atty/ui:measure ((w pane-area) m aw ah)
  (declare (ignore m aw ah))
  (values 0 0))

(defmethod atty/ui:lay ((w pane-area) m x y width height)
  (call-next-method)
  (let* ((bar (and (>= width +scrollbar-from+) (area-bar w)))
         (room (if bar (1- width) width)))
    (atty/ui:lay (area-view w) m x y room height)
    (when (area-bar w)
      (atty/ui:lay (area-bar w) m (+ x room) y (if bar 1 0) height))
    (when (area-chip w)
      (let ((wide (min room (atty/ui:measure (area-chip w) m room 1))))
        (atty/ui:lay (area-chip w) m (+ x (- room wide)) (+ y (max 0 (1- height)))
                     wide 1)))))

;;; A pane sits inside a frame of its own, all four sides, coloured by what its
;;; program is doing and drawn double where the focus is. What the corners say
;;; is src/mux/frames.lisp's business.

(declaim (ftype function pane-titles frame-face pane-top-row))

(defvar *scrollbars* t
  "Whether panes are drawn with a scrollbar. The session's to say, and bound
while one is laid out.")

(defun pane-frame (pane focusp &optional session)
  (atty/ui:framed (pane-area pane :scrollbarp *scrollbars*)
                  :face (frame-face pane focusp)
                  :line (if focusp :double :single)
                  :titles (and session (pane-titles session pane focusp))))

(defun views-in (tree)
  "Every pane-view in TREE, in the order they were put there."
  (let ((out nil))
    (labels ((walk (w)
               (when (typep w 'pane-view) (push w out))
               (dolist (part (atty/ui:parts w)) (walk part))))
      (walk tree))
    (nreverse out)))

(defun view-of (tree pane)
  (find pane (views-in tree) :key #'view-pane))

;;; How the panes in a session are arranged. A split is a way and the things
;;; laid out that way, each of which is a pane or another split, so any
;;; arrangement is a tree and any tree becomes a row or column of widgets.

(defstruct (split (:constructor make-split (way parts)))
  (way :across)
  (parts nil :type list))

(defun panes-in (it)
  "Every pane under IT, left to right and top to bottom. A layout with nothing
left in it holds no panes, which is not the same as holding one that is nothing."
  (cond ((null it) nil)
        ((split-p it)
         (loop :for part :in (split-parts it) :append (panes-in part)))
        (t (list it))))

(defun put-beside (it pane way new)
  "IT with NEW put beside PANE, the way given."
  (cond ((eq it pane) (make-split way (list pane new)))
        ((split-p it)
         (setf (split-parts it)
               (mapcar (lambda (p) (put-beside p pane way new)) (split-parts it)))
         it)
        (t it)))

(defun without-pane (it pane)
  "IT with PANE taken out, or nothing when that leaves nothing. A split down to
one part is that part: nobody wants a border around a single pane."
  (cond ((eq it pane) nil)
        ((split-p it)
         (let ((kept (remove nil (mapcar (lambda (p) (without-pane p pane))
                                         (split-parts it)))))
           (cond ((null kept) nil)
                 ((null (rest kept)) (first kept))
                 (t (setf (split-parts it) kept) it))))
        (t it)))

(defun layout-tree (it &optional focus session zoomed)
  "IT as widgets: a pane alone is a view with nothing around it, since there is
nothing for a border to tell it apart from. A split is a row or a column of
panes each in a frame of its own. A ZOOMED pane is the whole of it, framed, so
what it is doing still shows while the others are out of sight."
  (cond (zoomed (pane-frame zoomed t session))
        ((split-p it) (framed-tree it focus session))
        (t (pane-area it :scrollbarp *scrollbars* :chipp t))))

(defun framed-tree (it focus &optional session)
  (if (split-p it)
      (apply (if (eq (split-way it) :across) #'atty/ui:row #'atty/ui:column)
             :align :stretch :spacing 0 :expand 1
             (mapcar (lambda (part) (framed-tree part focus session)) (split-parts it)))
      (pane-frame it (eql it focus) session)))
