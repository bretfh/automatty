;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; A pane is a widget like any other. What that buys is the layout pass: a
;;; split is a row or a column of these, the bar is the first child of the
;;; column they sit in, and the arithmetic that hands each of them its rows and
;;; columns is the one src/ui/layout.lisp already does for everything else.

(defclass pane-view (atty/ui:widget)
  ((pane :initarg :pane :reader view-pane)
   (focusp :initarg :focusp :initform nil :reader view-focus-p)))

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

(defun pane-ground (focusp)
  "What a pane's program draws on where it says nothing: lit where the focus is,
a step darker everywhere else."
  (atty/ui:unhex (atty/ui:color (if focusp 'atty/ui::bg 'atty/ui::bg-well))))

(defvar *grounded* nil)

(declaim (ftype (function (t) hash-table) grounded-faces))
(defun grounded-faces (ground)
  (let ((kept (assoc ground *grounded* :test #'equal)))
    (if kept
        (cdr kept)
        (let ((table (make-hash-table :test 'eq :weakness :key)))
          (push (cons ground table) *grounded*)
          table))))

(defun ground-blanks (m left top width height ground)
  "Every cell in the rectangle with no background of its own given GROUND, one
face for each face found there, kept for the next time it is found, never the
one found changed."
  (let ((grid (atty/cells:cells-grid m))
        (made (grounded-faces ground))
        (last-was 0)
        (last-made nil))
    (flet ((grounded (was)
             (unless (eq was last-was)
               (setf last-was was
                     last-made (or (gethash (or was made) made)
                                   (setf (gethash (or was made) made)
                                         (let ((it (if was (term:copy-face was) (term:make-face))))
                                           (setf (term:face-bg it) ground)
                                           it)))))
             last-made))
      (loop :for y :from (max 0 top) :below (min (+ top height) (atty/cells:cells-rows m))
            :do (let ((row (svref grid y)))
                  (loop :for x :from (max 0 left) :below (min (+ left width) (atty/cells:cells-cols m))
                        :do (let ((was (term:row-face row x)))
                              (when (or (null was) (null (term:face-bg was)))
                                (setf (term:row-face row x) (grounded was))))))))))

(defun paint-shown (m screen left top width height)
  (let* ((grid (atty/cells:cells-grid m))
         (rows (min (tty:screen-height screen) height (- (atty/cells:cells-rows m) top)))
         (cols (min (tty:screen-width screen) width (- (atty/cells:cells-cols m) left))))
    (dotimes (y rows)
      (let ((from (tty:screen-row screen y))
            (into (svref grid (+ top y))))
        (replace (term:row-chars into) (term:row-chars from) :start1 left :end2 cols)
        (replace (term:row-faces into) (term:row-faces from) :start1 left :end2 cols)))
    (when (< cols width)
      (atty/cells:fill-rect m (+ left cols) top (- width cols) rows (term:make-face)))))

(defun selection-face ()
  (term:make-face :bg (atty/ui:unhex (atty/ui:color 'atty/ui::bg-active))))

(defmethod atty/ui:paint ((w pane-view) (m atty/cells:cells))
  (let* ((pane (view-pane w))
         (top (atty/ui:top w)) (left (atty/ui:left w))
         (height (atty/ui:height w)) (width (atty/ui:width w)))
    (let ((shown (pane-shown-now pane)))
      (if shown
          (paint-shown m (shown-screen shown) left top width height)
          (atty/cells:blit m (pane-term pane) left top width height (pane-scrolled pane))))
    (ground-blanks m left top width height (pane-ground (view-focus-p w)))
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
;;; starts. It is a rail bound to a pane: how far along it is, how much it
;;; shows and how much there is are read off the pane whenever it is drawn or
;;; asked about, so the arithmetic is the rail's and only the reading is here.

(defclass scrollbar (rail)
  ((pane :initarg :pane :reader view-pane)))

(defun scrollbar (pane &rest props)
  (apply #'make-instance 'scrollbar :pane pane props))

(defmethod rail-at-of ((r scrollbar))
  (let ((pane (view-pane r)))
    (- (pane-history pane) (pane-scrolled pane))))

(defmethod rail-extent-of ((r scrollbar))
  (pane-height (view-pane r)))

(defmethod rail-total-of ((r scrollbar))
  (let ((pane (view-pane r)))
    (+ (pane-history pane) (pane-height pane))))

(defmethod rail-thumb-face ((r scrollbar))
  (if (plusp (pane-scrolled (view-pane r))) :scroll-thumb-back :scroll-thumb))

(defun scrollbar-thumb (track rows history back)
  "Where the thumb is on a track TRACK cells long, for a pane of ROWS with
HISTORY behind it that is BACK rows up: how far down the track it starts, and
how long it is. At the foot when the pane is live and at the head when it is as
far back as there is."
  (rail-thumb track rows (+ history rows) (- history (min back history))))

(defun scrollbar-track (bar)
  "Which of BAR's lines its track is: the first, and how many."
  (rail-track bar))

(defun scrollbar-part (bar line)
  "What of BAR is at LINE: :up or :down for an arrow, :thumb, or :above or
:below for the track either side of it. Nil when there is nothing to scroll."
  (rail-part bar line))

(defun scrollbar-back-at (bar line grabbed)
  "How far back the pane is with the thumb dragged so the cell of it GRABBED
cells down from its head is at LINE."
  (- (pane-history (view-pane bar)) (rail-at bar line grabbed)))

;;; What says a pane is being read back, and takes it to the foot when clicked.

(defclass live-chip (atty/ui:label)
  ((pane :initarg :pane :reader view-pane)))

(defun live-chip (pane)
  (make-instance 'live-chip :pane pane :face :chip-scrolled
                            :text (format nil " ↓ ~D to live " (pane-scrolled pane))))

(defclass pane-position (atty/ui:label)
  ((pane :initarg :pane :reader view-pane)))

(defun pane-position (pane)
  (make-instance 'pane-position :pane pane :face :quiet :text ""))

(defmethod atty/ui:measure ((w pane-position) m aw ah)
  (declare (ignore m aw ah))
  (let ((digits (1+ (length (princ-to-string (pane-row-count (view-pane w)))))))
    (values (+ 11 (* 2 digits)) 1)))

(defmethod atty/ui:paint ((w pane-position) (m atty/cells:cells))
  (let* ((pane (view-pane w))
         (text (format nil " line ~D of ~D " (1+ (pane-top-row pane)) (pane-row-count pane)))
         (face (atty/cells:face-of w))
         (width (atty/ui:width w)))
    (unless (term:face-bg face)
      (setf (term:face-bg face) (bar-face :bg-dim)))
    (atty/cells:fill-rect m (atty/ui:left w) (atty/ui:top w) width 1 face)
    (atty/cells:say-at m (+ (atty/ui:left w) (max 0 (- width (length text)))) (atty/ui:top w)
                       (subseq text (max 0 (- (length text) width))) face)))

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
   (bar :initarg :bar :reader area-bar)))

(defun pane-area (pane &key (scrollbarp t) focusp)
  "PANE with a scrollbar down its right when SCROLLBARP."
  (let ((view (pane-view pane :focusp focusp))
        (bar (and scrollbarp (scrollbar pane))))
    (make-instance 'pane-area :expand 1 :view view :bar bar
                              :parts (remove nil (list view bar)))))

(defmethod atty/ui:measure ((w pane-area) m aw ah)
  (declare (ignore m aw ah))
  (values 0 0))

(defmethod atty/ui:lay ((w pane-area) m x y width height)
  (call-next-method)
  (let* ((bar (and (>= width +scrollbar-from+) (area-bar w)))
         (room (if bar (1- width) width)))
    (atty/ui:lay (area-view w) m x y room height)
    (when (area-bar w)
      (atty/ui:lay (area-bar w) m (+ x room) y (if bar 1 0) height))))

;;; A pane sits inside a frame of its own, all four sides, coloured by what its
;;; program is doing and drawn double where the focus is, on the pane's ground,
;;; with a header row and a footer row inside it. What they say is
;;; src/mux/frames.lisp's business.

(defvar *scrollbars* t
  "Whether panes are drawn with a scrollbar. The session's to say, and bound
while one is laid out.")

(defun pane-frame (pane focusp &optional session)
  (let ((area (pane-area pane :scrollbarp *scrollbars* :focusp focusp)))
    (atty/ui:framed (if session
                        (destructuring-bind (&key tl tr bl br) (frame-corners session pane focusp)
                          (atty/ui:column :align :stretch :expand 1
                                          (unwidened (band (list (atty/ui:label (if focusp " ◆" "  ") :face :here) (squeezed tl)) tr
                                                           :ground (if focusp :bg-alt :bg-dim)))
                                          area
                                          (unwidened (band (list (atty/ui:label "") bl) br))))
                        area)
                    :background-color (bar-face (if focusp :bg :bg-well))
                    :face (frame-face pane focusp)
                    :line (if focusp :double :single))))

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

(defun layout-panes (it)
  "Every pane under IT, left to right and top to bottom. A layout with nothing
left in it holds no panes, which is not the same as holding one that is nothing."
  (cond ((null it) nil)
        ((split-p it)
         (loop :for part :in (split-parts it) :append (layout-panes part)))
        (t (list it))))

(defun layout-insert (it pane way new)
  "IT with NEW put beside PANE, the way given: into PANE's own split when that
already goes that way, so the panes in it share alike."
  (cond ((eq it pane) (make-split way (list pane new)))
        ((and (split-p it) (eq (split-way it) way) (member pane (split-parts it)))
         (let ((at (1+ (position pane (split-parts it)))))
           (setf (split-parts it)
                 (append (subseq (split-parts it) 0 at) (list new) (nthcdr at (split-parts it))))
           it))
        ((split-p it)
         (setf (split-parts it)
               (mapcar (lambda (p) (layout-insert p pane way new)) (split-parts it)))
         it)
        (t it)))

(defun layout-remove (it pane)
  "IT with PANE taken out, or nothing when that leaves nothing. A split down to
one part is that part: nobody wants a border around a single pane."
  (cond ((eq it pane) nil)
        ((split-p it)
         (let ((kept (remove nil (mapcar (lambda (p) (layout-remove p pane))
                                         (split-parts it)))))
           (cond ((null kept) nil)
                 ((null (rest kept)) (first kept))
                 (t (setf (split-parts it) kept) it))))
        (t it)))

(defun layout-tree (it &optional focus session zoomed)
  "IT as widgets: a row or a column of panes each in a frame of its own, a pane
alone framed the same. A ZOOMED pane is the whole of it, framed, so what it is
doing still shows while the others are out of sight."
  (cond (zoomed (pane-frame zoomed t session))
        ((split-p it) (framed-tree it focus session))
        (t (pane-frame it t session))))

(defun framed-tree (it focus &optional session)
  (if (split-p it)
      (apply (if (eq (split-way it) :across) #'atty/ui:row #'atty/ui:column)
             :align :stretch :spacing 0 :expand 1
             (mapcar (lambda (part) (framed-tree part focus session)) (split-parts it)))
      (pane-frame it (eql it focus) session)))
