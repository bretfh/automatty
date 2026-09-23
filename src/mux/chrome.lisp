;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; The grammar every surface is drawn in: a band across the top or the foot,
;;; key caps and hints that are buttons, a selector, a toggle, a field, a
;;; toolbar of those, rows with a gutter of marks, a path, who somebody is,
;;; a rail that scrolls anything, a card, and a sparkline of the last while. The
;;; bar, the frames, the prompts, the queue, the drawer and the board are made
;;; of these and nothing else, so what a thing looks like says what it does
;;; wherever it is.

(declaim (ftype function state-face state-glyph command-key send-to-server close-overlay))

(defun bar-face (role)
  "The hex of the theme's colour called ROLE, as a widget's background wants it."
  (atty/ui:unhex (atty/ui:color role)))

;;; A button is a widget like any other, except a click on it does not run
;;; anything itself: what it does is said by RUNS, a form for the server to
;;; do as though whoever clicked had said it, or the name of a command for
;;; whoever clicked to run, or a keyword for the thing it is drawn on to make
;;; sense of.

(defclass bar-button (atty/ui:widget)
  ((runs :initarg :runs :reader bar-button-runs)))

(defun bar-button (runs part &rest props)
  (apply #'make-instance 'bar-button :runs runs :parts (list part) props))

(defmethod atty/ui:measure ((w bar-button) m aw ah)
  (let ((part (first (atty/ui:parts w))))
    (if part (atty/ui:measure part m aw ah) (values 0 1))))

(defmethod atty/ui:lay ((w bar-button) m x y width height)
  (call-next-method)
  (let ((part (first (atty/ui:parts w))))
    (when part (atty/ui:lay part m x y width height))))

(defmethod atty/ui:under ((w bar-button) line col)
  (when (and (<= (atty/ui:top w) line) (< line (atty/ui:bottom w))
             (<= (atty/ui:left w) col) (< col (atty/ui:right w)))
    w))

(defun button-at (tree line col)
  "The innermost bar-button in TREE at LINE, COL, titles of frames included:
what a click on something drawn client-side lands on."
  (let ((found nil))
    (labels ((walk (w)
               (when (and (typep w 'bar-button)
                          (<= (atty/ui:top w) line) (< line (atty/ui:bottom w))
                          (<= (atty/ui:left w) col) (< col (atty/ui:right w)))
                 (setf found w))
               (dolist (part (atty/ui:parts w)) (walk part))
               (when (typep w 'atty/ui:framed)
                 (loop :for (nil title) :on (atty/ui:titles w) :by #'cddr
                       :do (walk title)))))
      (walk tree))
    found))

(defun widget-at (tree line col type)
  "The innermost widget of TYPE in TREE at LINE, COL."
  (let ((found nil))
    (labels ((walk (w)
               (when (and (typep w type)
                          (<= (atty/ui:top w) line) (< line (atty/ui:bottom w))
                          (<= (atty/ui:left w) col) (< col (atty/ui:right w)))
                 (setf found w))
               (dolist (part (atty/ui:parts w)) (walk part))))
      (walk tree))
    found))

;;; A part that takes whatever room its row has left and is cut there,
;;; rather than pushing what is beside it off the edge: the hints in a
;;; footer, the path in a header.

(defclass squeezed (atty/ui:clip) ())

(defun squeezed (child)
  (make-instance 'squeezed :parts (list child) :expand 1))

(defmethod atty/ui:measure ((w squeezed) m aw ah)
  (let ((part (first (atty/ui:parts w))))
    (values 0 (if part (nth-value 1 (atty/ui:measure part m aw ah)) 1))))

;;; A box of a set size whatever is in it wants: what does not fit is cut at
;;; its edge, never allowed to widen it.

(defclass fixed (atty/ui:clip)
  ((wide :initarg :wide :reader fixed-wide)
   (high :initarg :high :reader fixed-high)))

(defun fixed (child wide high &rest props)
  (apply #'make-instance 'fixed :parts (list child) :wide wide :high high props))

(defmethod atty/ui:measure ((w fixed) m aw ah)
  (declare (ignore m aw ah))
  (values (fixed-wide w) (fixed-high w)))

(defmethod atty/ui:lay ((w fixed) m x y width height)
  (declare (ignore width height))
  (setf (atty/ui:left w) x (atty/ui:top w) y
        (atty/ui:right w) (+ x (fixed-wide w)) (atty/ui:bottom w) (+ y (fixed-high w)))
  (let ((part (first (atty/ui:parts w))))
    (when part (atty/ui:lay part m x y (fixed-wide w) (fixed-high w)))))

;;; Bands: one row across the whole width, on the band's ground, what is at
;;; the left then what is at the right.

(defun ensure-list (it)
  "IT as a list of widgets: nothing, one, or several."
  (cond ((null it) nil)
        ((listp it) (remove nil it))
        (t (list it))))

(defun band (left right &key (ground :bg-dim))
  "A row on the band's ground: LEFT's parts, a gap, RIGHT's parts. A squeezed
part among LEFT's takes the gap's place, so it has the room the gap would."
  (let ((left (ensure-list left)))
    (apply #'atty/ui:row :spacing 0 :background-color (bar-face ground)
           (append left
                   (unless (some (lambda (p) (typep p 'squeezed)) left) (list (atty/ui:gap)))
                   (ensure-list right)))))

(defun close-button ()
  "The way out, at the right of a header: a click closes whatever it is on."
  (bar-button :close (atty/ui:label " ✕ " :face :key)))

(defun header-band (title &key path right (close t))
  "The band at the top of anything on top: what it is, where, and the way out."
  (band (list (atty/ui:label (format nil " ~A " title) :face :strong)
              (and path (atty/ui:label " " ))
              (and path (squeezed path)))
        (append (ensure-list right)
                (when close (list (atty/ui:label " ") (close-button) (atty/ui:label " "))))))

(defun footer-band (hints &key right)
  "The band at the foot: the keys, and at the right the ones every surface has."
  (band (list (atty/ui:label " ") (squeezed hints)) (append (ensure-list right) (list (atty/ui:label " ")))))

;;; Keys as things to click: a cap has the key on it and says what it does;
;;; a hint is the key lit and the verb dim. Both run what they say.

(defun keycap (key verb &key runs (face :key))
  "A button that says its KEY and its VERB, the way an answer is drawn."
  (bar-button runs
              (atty/ui:row :spacing 0
                           (atty/ui:label (format nil " ~A" key) :face (if (eq face :key) :key-number face))
                           (atty/ui:label (if verb (format nil " ~A " verb) " ") :face face))))

(defun hint (key verb &key runs)
  "A key and what it does, as a hint says it; a click does it."
  (bar-button runs
              (atty/ui:row :spacing 0
                           (atty/ui:label (format nil " ~A" key) :face :key-hint)
                           (atty/ui:label (format nil " ~A  " verb) :face :quiet))))

(defun hints (mode &rest pairs)
  "A row of what the keys in MODE do: PAIRS is a command, then what it does,
each a hint that runs the command when clicked. A string in place of the
command is said as it is, for keys no one command is bound to, such as the
digits, and runs nothing."
  (apply #'atty/ui:row :spacing 0
         (loop :for (command does) :on pairs :by #'cddr
               :collect (if (stringp command)
                            (hint command does)
                            (hint (key-hint mode command) does
                                  :runs (string-downcase (substitute #\Space #\- (symbol-name command))))))))

;;; The controls a toolbar holds.

(defun selector (label value &key runs key)
  "A control that cycles through what VALUE can be: click or KEY."
  (bar-button runs
              (atty/ui:row :spacing 0
                           (atty/ui:label (format nil " ~A ▾ " label) :face :key)
                           (atty/ui:label (format nil " ~A" value))
                           (atty/ui:label (if key (format nil " ~A " key) " ") :face :quiet))))

(defun toggle (label on &key runs key)
  "A control that is on or off: a box with a tick when on, click or KEY."
  (bar-button runs
              (atty/ui:row :spacing 0
                           (atty/ui:label (if on " ✓ " "   ") :face (if on :toggle-on :key))
                           (atty/ui:label (format nil " ~A" label))
                           (atty/ui:label (if key (format nil " ~A " key) " ") :face :quiet))))

(defun field (prefix text &key width (cursor t) runs)
  "Where what is typed lands: the PREFIX that says which kind, the TEXT so far,
the cursor after it."
  (let ((it (apply #'atty/ui:row :spacing 0 :background-color (bar-face :bg-alt)
                   (append (when width (list :min-width width))
                           (list (atty/ui:label (format nil " ~A " prefix) :face :quiet)
                                 (atty/ui:label text)
                                 (atty/ui:label (if cursor "_" "") :face :quiet)
                                 (atty/ui:label " "))))))
    (if runs (bar-button runs it) it)))

(defun toolbar (&rest parts)
  "A row of controls on the field's ground; :right in PARTS puts what follows
at the right."
  (let ((at (position :right parts)))
    (apply #'atty/ui:row :spacing 1 :background-color (bar-face :bg-alt)
           (append (list (atty/ui:label ""))
                   (list (squeezed (apply #'atty/ui:row :spacing 1
                                          (ensure-list (if at (subseq parts 0 at) parts)))))
                   (when at (ensure-list (subseq parts (1+ at))))
                   (list (atty/ui:label ""))))))

;;; Rows with a gutter: two cells for a mark at the left, the row, and two
;;; cells at the right before the rail. The chosen row is a whole band.

(defun gutter-row (mark body &key selected runs)
  "MARK in the gutter, BODY across the row, a band when SELECTED."
  (let ((it (apply #'atty/ui:row :spacing 0
                   (append (when selected (list :background-color (bar-face :bg-active)))
                           (list (if (stringp mark)
                                     (atty/ui:label (format nil "~2A" mark) :face :cursor)
                                     (or mark (atty/ui:label "  ")))
                                 (if (stringp body) (atty/ui:label body) body)
                                 (atty/ui:gap)
                                 (atty/ui:label "  "))))))
    (if runs (bar-button runs it) it)))

(defun section (name)
  "A section's heading: small, dim, and the thing under it indented past it."
  (atty/ui:label (format nil " ~A" (string-upcase name)) :face :quiet))

;;; Where something is, and who somebody is.

(defun path (&rest parts)
  "PARTS with › between them, each bold; (:quiet \"…\") for an aside said
dimly, with no › before it. Nil parts are left out."
  (apply #'atty/ui:row :spacing 0
         (loop :for part :in (remove nil parts)
               :for first := t :then nil
               :for quiet := (and (consp part) (eq :quiet (first part)))
               :append (list (atty/ui:label (cond (first "") (quiet " ") (t " › ")) :face :quiet)
                             (if quiet
                                 (atty/ui:label (princ-to-string (second part)) :face :quiet)
                                 (atty/ui:label (princ-to-string part) :face :strong))))))

;;; A rail: what says how far along something long a view is, and takes the
;;; mouse to move it. Down the side of a pane it is the scrollbar; under a
;;; lane or beside a list it is the same thing. AT is how far from the start
;;; the view is, EXTENT how much of the whole it shows, TOTAL the whole.

;;; One vocabulary for every rail: a thin line is the whole length, a heavy
;;; line is the part in view, and the ends are lit only when there is more
;;; that way. When nothing is past the view it is a thin line and no more.

(defparameter +scrollbar-glyphs+
  '(:up #\▲ :down #\▼ :left #\‹ :right #\›
    :track-up #\│ :thumb-up #\┃ :track #\─ :thumb #\━))

(defparameter +arrows-from+ 4
  "A rail shorter than this is all track: two arrows would leave no room
for a thumb to be anywhere.")

(defclass rail (atty/ui:widget)
  ((at :initarg :at :initform 0 :accessor rail-at-slot)
   (extent :initarg :extent :initform 1 :accessor rail-extent-slot)
   (total :initarg :total :initform 1 :accessor rail-total-slot)
   (upright :initarg :upright :initform t :accessor rail-upright)
   (runs :initarg :runs :initform nil :accessor rail-runs)))

(defun rail (at extent total &key (upright t) runs (expand 0) thumb-face)
  (make-instance 'rail :at at :extent extent :total total :upright upright :runs runs
                       :expand expand :face thumb-face))

(defgeneric rail-at-of (rail) (:method ((r rail)) (rail-at-slot r)))
(defgeneric rail-extent-of (rail) (:method ((r rail)) (rail-extent-slot r)))
(defgeneric rail-total-of (rail) (:method ((r rail)) (rail-total-slot r)))
(defgeneric rail-thumb-face (rail)
  (:documentation "What the part in view is drawn in.")
  (:method ((r rail)) (or (atty/ui:face r) :scroll-thumb)))
(defgeneric rail-track-face (rail)
  (:documentation "What the whole length is drawn in.")
  (:method ((r rail)) :scroll-track))

(defmethod atty/ui:measure ((w rail) m aw ah)
  (declare (ignore m aw ah))
  (if (rail-upright w) (values 1 0) (values 0 1)))

(defmethod atty/ui:under ((w rail) line col)
  (when (and (<= (atty/ui:top w) line) (< line (atty/ui:bottom w))
             (<= (atty/ui:left w) col) (< col (atty/ui:right w)))
    w))

(defun rail-thumb (track extent total at)
  "Where the thumb is on a track TRACK cells long for a view EXTENT long of
TOTAL that is AT from the start: how far along the track it starts, and how
long it is."
  (let* ((long (max 1 (min track (round (* track extent) (max 1 total)))))
         (room (- track long))
         (behind (max 0 (- total extent))))
    (values (if (plusp behind) (round (* room (min at behind)) behind) 0)
            long)))

(defun rail-length (r)
  (if (rail-upright r) (atty/ui:height r) (atty/ui:width r)))

(defun rail-start (r)
  (if (rail-upright r) (atty/ui:top r) (atty/ui:left r)))

(defun rail-track (r)
  "Which cells of R are its track: the first, and how many."
  (let ((long (rail-length r)))
    (if (>= long +arrows-from+)
        (values (1+ (rail-start r)) (- long 2))
        (values (rail-start r) long))))

(defun rail-part (r pos)
  "What of R is at POS, a line down an upright rail or a column along one
lying down: :up or :down for an arrow, :thumb, or :above or :below for the
track either side of it. Nil when there is nothing to scroll."
  (when (> (rail-total-of r) (rail-extent-of r))
    (multiple-value-bind (from track) (rail-track r)
      (cond
        ((< pos from) :up)
        ((>= pos (+ from track)) :down)
        (t (multiple-value-bind (top long)
               (rail-thumb track (rail-extent-of r) (rail-total-of r) (rail-at-of r))
             (let ((at (- pos from)))
               (cond ((< at top) :above)
                     ((>= at (+ top long)) :below)
                     (t :thumb)))))))))

(defun rail-at (r pos grabbed)
  "How far along the whole the view is with the thumb dragged so the cell of
it GRABBED cells from its head is at POS."
  (multiple-value-bind (from track) (rail-track r)
    (multiple-value-bind (top long)
        (rail-thumb track (rail-extent-of r) (rail-total-of r) (rail-at-of r))
      (declare (ignore top))
      (let* ((room (- track long))
             (head (max 0 (min room (- pos from grabbed))))
             (behind (max 0 (- (rail-total-of r) (rail-extent-of r)))))
        (if (plusp room) (round (* behind head) room) 0)))))

(defmethod atty/ui:paint ((w rail) (m atty/cells:cells))
  (let* ((at (rail-at-of w)) (extent (rail-extent-of w)) (total (rail-total-of w))
         (upright (rail-upright w))
         (x (atty/ui:left w)) (y (atty/ui:top w)))
    (when (and (plusp (atty/ui:width w)) (plusp (atty/ui:height w)))
      (let ((own (atty/ui:face w))
            (thumb-face (rail-thumb-face w))
            (track-face (rail-track-face w)))
        (flet ((ink (face) (setf (atty/ui:face w) face) (atty/cells:face-of w))
               (glyph (name) (getf +scrollbar-glyphs+ name))
               (put (pos n face ch)
                 (if upright
                     (atty/cells:fill-rect m x pos 1 n face ch)
                     (atty/cells:fill-rect m pos y n 1 face ch))))
          (multiple-value-bind (from track) (rail-track w)
            (let ((arrows (/= from (rail-start w)))
                  (more (> total extent)))
              (put (rail-start w) (rail-length w) (ink track-face)
                   (glyph (if upright :track-up :track)))
              (when (and arrows more)
                (put (rail-start w) 1 (ink (if (plusp at) :scroll-arrow :scroll-arrow-dim))
                     (glyph (if upright :up :left)))
                (put (+ (rail-start w) (rail-length w) -1) 1
                     (ink (if (< (+ at extent) total) :scroll-arrow :scroll-arrow-dim))
                     (glyph (if upright :down :right))))
              (when more
                (multiple-value-bind (top long) (rail-thumb track extent total at)
                  (put (+ from top) long (ink thumb-face) (glyph (if upright :thumb-up :thumb))))))))
        (setf (atty/ui:face w) own)))))

;;; A card: a window as a box, its name in the top border, who looks at it
;;; at the right, what is inside in its real splits, and its answers in the
;;; bottom border the way a frame carries them.

(defun card (title right body &key (face :card) cursor bl br runs (width 0) (height 0))
  "BODY framed, TITLE at the top left and RIGHT at the top right, BL and BR
along the bottom; a thin rounded line in FACE, the state's colour, and a heavy
one when it is the CURSOR's (lit, when its state has no colour of its own)."
  (let* ((framed (atty/ui:framed
                  (apply #'atty/ui:column :align :stretch :expand 1 (ensure-list body))
                  :face (if (and cursor (eq face :card)) :card-cursor face)
                  :line (if cursor :heavy :rounded)
                  :titles (list :tl title :tr right :bl bl :br br)))
         (it (if (and (plusp width) (plusp height)) (fixed framed width height) framed)))
    (if runs (bar-button runs it) it)))

;;; A foot: the row that closes a lane, which is its rail with room on it for
;;; where the view is and, when it is the cursor's, what can be pressed here.

(defun foot (rail text &optional hints)
  "RAIL stretched along the row, then TEXT, then HINTS, then the line closing."
  (atty/ui:row :spacing 0
               (atty/ui:label "  ")
               rail
               (atty/ui:label (format nil " ~A" text) :face :quiet)
               (and hints (atty/ui:row :spacing 0 (atty/ui:label "   ") hints))
               (atty/ui:label " ─" :face :scroll-track)))

;;; A well: rows sunk into a darker ground, a dark edge above and a light one
;;; below, the way an inset panel is drawn.

(defun well (rows &key rail)
  "ROWS on the well's ground between its two edges; RAIL, when there is one,
stands at the right of the rows."
  (atty/ui:column :align :stretch :background-color (bar-face :bg-well)
                  (atty/ui:rule :glyph #\▔ :face :well-edge-dark)
                  (if rail
                      (atty/ui:row :spacing 0 :align :stretch
                                   (apply #'atty/ui:column :align :stretch :expand 1 rows)
                                   rail)
                      (apply #'atty/ui:column :align :stretch rows))
                  (atty/ui:rule :glyph #\▁ :face :well-edge-light)))

;;; An event, as the activity log and the command line say it: a glyph for
;;; what kind of thing it was, and a few words.

;;; A sparkline: the last twenty minutes in sixteen cells, how much output
;;; there was as height and the most urgent state then as colour. Panes roll
;;; up into windows, windows into sessions and sessions into the server, by
;;; adding the output and keeping the worst state.

(defparameter +spark-cells+ 16)
(defparameter +spark-glyphs+ "▁▂▃▄▅▆▇█")

(defun spark-face (state)
  (case state
    (:blocked :state-blocked-strong)
    (:working :state-working)
    (:idle :state-idle)
    ((nil :quiet) :spark-quiet)
    (t :quiet)))

(defun merge-cells (lists)
  "One list of cells from many: each cell's amount added up, and the worst
state of any of them kept. Cells are (amount . state), oldest first."
  (let ((out (make-list +spark-cells+ :initial-element (cons 0 nil))))
    (dolist (cells lists out)
      (setf out (loop :for (amount . state) :in cells
                      :for (sum . worst) :in out
                      :collect (cons (+ sum amount)
                                     (if (< (state-rank-of state) (state-rank-of worst)) state worst)))))))

(defun spark (cells)
  "CELLS, oldest first, as a row of block glyphs: height scaled to the
busiest cell of these, colour by state; a cell with no output is the lowest
block and dark, unless something asked then."
  (let ((most (max 1 (reduce #'max cells :key #'car :initial-value 0))))
    (apply #'atty/ui:row :spacing 0
           (loop :for (amount . state) :in cells
                 :collect (atty/ui:label
                           (string (char +spark-glyphs+
                                         (if (zerop amount) 0 (max 1 (round (* 7 amount) most)))))
                           :face (if (and (zerop amount) (not (eq state :blocked)))
                                     :spark-quiet
                                     (spark-face state)))))))

;;; The whole of something on top: a header band, a toolbar, the body, a
;;; status row and a footer of hints, filling the room under the bar.

(defun key-hint (mode command)
  "The chord COMMAND is bound to in MODE, or its name when nothing is: a hint
names whatever somebody has set up, not what this file set up."
  (let* ((does (and (fboundp command) (symbol-function command)))
         (chord (car (find does (atty/mode:keys-in-force (atty/mode:mode-named mode)) :key #'cdr))))
    (cond ((null chord) (string-downcase (substitute #\Space #\- (symbol-name command))))
          ((string= chord "RET") "↵")
          ((string= chord "TAB") "⇥")
          (t chord))))
