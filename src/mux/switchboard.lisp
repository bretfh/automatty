;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; The switchboard: the whole server at once, as what a multiplexer manages.
;;; Down the left, a side pane: the server, its sessions as a tree that folds,
;;; who is attached and where, and the view's controls. Beside it, a lane for
;;; every session: a header band, then its windows as cards of one size that
;;; slide sideways to keep the cursor's in sight, the way niri does, under a
;;; rail of their own; the lanes slide up and down under the rail at the
;;; right. One cursor is shared by the tree and the lanes. Three zooms: the
;;; overview, where a lane is a row of chips; the cards; and one session on
;;; its own, its cards big enough to read and answer from.
;;;
;;; Like the queue, it is drawn over the session for whoever opened it, from
;;; what the server keeps this client told: the pane rows, the layouts of each
;;; session's windows, the last rows of every pane, and who is attached.

(declaim (ftype function short-tty confirm open-the-drawer ask-a-window-name ask-a-session-name))
(declaim (special +asked-every+))
(declaim (ftype function where-the-server-is))

(defparameter +side-width+ 34 "How wide the side pane is.")
(defparameter +tree-width+ 22 "How wide the side pane is folded to its tree, in the one-session zoom.")
(defparameter +side-from+ 110 "A terminal narrower than this has no room for the side pane.")
(defparameter +card-width+ 35)
(defparameter +card-height+ 8)
(defparameter +big-card-width+ 72)

(defstruct (board (:constructor %make-board))
  (cursor nil)                          ; (session window-n pane-id)
  (slid (make-hash-table :test 'equal)) ; session -> how far its lane is slid, in cells
  (y0 0 :type fixnum)                   ; how far the lanes are slid up
  (zoom :cards)                         ; :overview, :cards or :one
  (side t)                              ; whether the side pane is shown
  (folded nil :type list)               ; sessions folded in the tree
  (picked nil :type list)               ; (session . id) keys picked for a prompt
  (query "" :type string)
  (filtering nil)
  (composing nil)
  (sort :asking)                        ; :asking, :name or :order
  (fold-quiet nil)                      ; whether lanes with nothing doing are folded
  (live t)                              ; whether windows show their panes' last rows
  (starting nil)
  (following t)                         ; whether the view is to slide to the cursor when next drawn
  (held nil)                            ; a rail's thumb taken hold of: (:rail-h session rail grabbed) or (:rail-v rail grabbed)
  (area-top 0 :type fixnum)             ; the screen row the lanes start on, as last drawn
  (log-y0 0 :type fixnum)               ; how far the activity log is scrolled
  (asked 0 :type integer)
  (regions nil :type list))             ; where things were drawn, for clicks

(defun row-key (row) (cons (getf row :session) (getf row :id)))

(defun row-name (row)
  "What to call a pane on the board: what somebody called it, else its
program. Not its title: a shell's title is a path nobody wants forty of."
  (or (getf row :label) (getf row :command) (getf row :says) ""))


(defun state-rank (row)
  (or (position (row-state row) +state-rank+) (length +state-rank+)))

(defun row-state (row)
  "What ROW's pane is doing, when that means anything: nil for a program
nobody knows how to read, whose screen moving or not is all there is."
  (and (getf row :known) (getf row :state)))

;;; What the client knows, arranged: sessions in the order asked for, each
;;; with its windows, each with its layout of panes.

(defun board-rows (b client)
  (let ((rows (rows-of client)))
    (if (plusp (length (board-query b)))
        (matches (board-query b) rows
                 (lambda (r) (format nil "~A ~@[~A~] ~@[~A~]" (row-text r) (getf r :doing)
                                     (getf r :window-name))))
        rows)))

(defun rows-in-session (client session)
  (remove-if-not (lambda (r) (equal session (getf r :session))) (rows-of client)))

(defun session-counts (client session)
  "How many panes of SESSION are asking, working, idle, and not read."
  (let ((rows (rows-in-session client session)))
    (list (count :blocked rows :key #'row-state)
          (count :working rows :key #'row-state)
          (count :idle rows :key #'row-state)
          (count nil rows :key #'row-state))))

(defun board-sessions (b client)
  "Every session's name: in the server's order, or asking first, or by name,
as the sort says; narrowed to the ones with a pane the filter matches."
  (let* ((rows (board-rows b client))
         (named (mapcar #'car
                        (sort (remove-duplicates (mapcar (lambda (r) (cons (getf r :session) (or (getf r :order) 0)))
                                                         rows)
                                                 :test #'equal :key #'car)
                              #'< :key #'cdr))))
    (case (board-sort b)
      (:name (sort (copy-list named) #'string<))
      (:asking (stable-sort (copy-list named) #'>
                            :key (lambda (s) (first (session-counts client s)))))
      (t named))))

(defun windows-of (client session)
  "SESSION's windows as (n label tree focus-id shownp), from the layouts the
server said; from the pane rows alone, side by side, when it has not yet."
  (or (gethash session (client-layouts client))
      (let* ((rows (rows-in-session client session))
             (ns (sort (remove-duplicates (mapcar (lambda (r) (or (getf r :window) 1)) rows)) #'<)))
        (loop :for n :in ns
              :collect (let ((in (sort (remove-if-not (lambda (r) (eql n (or (getf r :window) 1))) rows)
                                       #'< :key (lambda (r) (or (getf r :at) 0)))))
                         (list n (getf (first in) :window-name)
                               (if (rest in) (cons :across (mapcar (lambda (r) (getf r :id)) in))
                                   (getf (first in) :id))
                               (getf (find-if (lambda (r) (getf r :focus)) in) :id)
                               t))))))

(defun panes-in-tree (tree)
  (cond ((null tree) nil)
        ((consp tree) (loop :for part :in (rest tree) :append (panes-in-tree part)))
        (t (list tree))))

(defun pane-row-of (client session id)
  (gethash (cons session id) (client-panes client)))

(defun window-rows (client session window)
  "The pane rows of window WINDOW of SESSION, in layout order."
  (remove nil (mapcar (lambda (id) (pane-row-of client session id)) (panes-in-tree (third window)))))

(defun window-worst-row (client session window)
  (first (sort (copy-list (window-rows client session window)) #'< :key #'state-rank)))

(defun window-name (client session window)
  "What to call WINDOW: its label, else its first pane's name."
  (or (second window)
      (let ((rows (window-rows client session window))) (and rows (row-name (first rows))))
      ""))

(defun clients-on (client session &optional n)
  "The attached terminals looking at SESSION, or at its window N, this one included."
  (remove-if-not (lambda (c) (and (equal session (fifth c)) (or (null n) (eql n (sixth c)))))
                 (client-clients client)))

(defun quiet-p (client session)
  "Whether nothing in SESSION is asking or working."
  (let ((counts (session-counts client session)))
    (and (zerop (first counts)) (zerop (second counts)))))

;;; Where the cursor is.

(defun board-place (b client)
  "The cursor as (session window-n pane-id), put somewhere real when it is on
nothing: where this client is looking, else the first pane of the first
window of the first session."
  (let ((cursor (board-cursor b)))
    (or (and cursor
             (pane-row-of client (first cursor) (third cursor))
             cursor)
        (let* ((sessions (board-sessions b client))
               (session (or (find (board-starting b) sessions :test #'equal) (first sessions)))
               (mine (find (client-id client) (client-clients client) :key #'first))
               (focused (find-if (lambda (r) (and (equal session (getf r :session)) (getf r :focus)))
                                 (rows-of client)))
               (n (or (and mine (equal (fifth mine) session) (sixth mine))
                      (and focused (getf focused :window))
                      1))
               (window (and session (or (find n (windows-of client session) :key #'first)
                                        (first (windows-of client session)))))
               (id (and window (or (and focused (member (getf focused :id) (panes-in-tree (third window)))
                                        (getf focused :id))
                                   (first (panes-in-tree (third window)))))))
          (and id (setf (board-cursor b) (list session (first window) id)))))))

(defun board-row (b client)
  "The pane row under the cursor."
  (let ((place (board-place b client)))
    (and place (pane-row-of client (first place) (third place)))))

(defun key-of-cursor (b client)
  (let ((row (board-row b client))) (and row (row-key row))))

(defun board-targets (b client)
  "What a prompt from the composer goes to: what is picked, or the pane under
the cursor when nothing is."
  (let ((rows (board-rows b client)))
    (or (remove-if-not (lambda (r) (member (row-key r) (board-picked b) :test #'equal)) rows)
        (let ((it (board-row b client))) (and it (list it))))))

;;; Cards: a window as a box of one size, its panes inside in their real
;;; splits. A pane is a title line in its state's colour and its last rows.

(defun answer-buttons (row width)
  (let ((options (getf (getf row :asks) :options)))
    (apply #'atty/ui:row :spacing 1
           (loop :for (n) :in options
                 :for text :in (options-fitted options (max 8 (- width 4)))
                 :collect (keycap n text :runs (list :option (row-key row) n))))))

(defun state-chip (row)
  "ROW's state as a chip: the glyph, on the state's ground."
  (let ((state (row-state row)))
    (atty/ui:label (format nil " ~A " (state-glyph state))
                   :face (if state (number-face state) :number-unknown))))

(defun since-text (row now)
  (let ((state (row-state row)))
    (if state (duration (row-for row now)) "")))

(defun pane-title (b client row width &key ruled)
  "One line naming a pane inside a card: number, name, kind, and its state
and how long at the right. RULED draws it as a rule with the title in it,
for a pane under another."
  (let* ((now (ms-here))
         (state (row-state row))
         (place (board-place b client))
         (cursor (and place (equal (row-key row) (cons (first place) (third place)))))
         (name (format nil "~D ~A" (1+ (or (getf row :at) 0)) (row-name row)))
         (since (since-text row now)))
    (if ruled
        (atty/ui:row :spacing 0
                     (atty/ui:label "┄┄ " :face :card)
                     (atty/ui:label (format nil "~A ~A" (state-glyph state) name)
                                    :face (if cursor :cursor (if state (state-face state) :quiet)))
                     (atty/ui:label (format nil " ~A " since) :face :quiet)
                     (atty/ui:rule :glyph #\┄ :face :card :expand 1))
        (let* ((kind (shortened-to (or (getf row :kind) "") 12))
               (room (- width 3 1 (length since) 1))
               (with-kind (>= (- room (1+ (length kind))) 6))
               (name (shortened-to name (max 3 (if with-kind (- room (1+ (length kind))) room)))))
          (atty/ui:row :spacing 0
                       (state-chip row)
                       (atty/ui:label (format nil " ~A" name) :face (if cursor :cursor :strong))
                       (atty/ui:label (if with-kind (format nil " ~A" kind) "") :face :quiet)
                       (atty/ui:gap)
                       (atty/ui:label (format nil "~A " since)
                                      :face (if (eq state :blocked) :state-blocked-strong (state-face state))))))))

(defun pane-rows-view (b client row)
  "The last rows of ROW's pane, as the server sends them; or what it says it
is doing when the board is not showing screens."
  (let ((screen (and (board-live b) (gethash (row-key row) (client-screens client)))))
    (cond (screen (screen-view screen))
          ((getf row :asks)
           (atty/ui:column :expand 1
                           (atty/ui:label (format nil " ▲ ~A" (getf (getf row :asks) :subject))
                                          :face :state-blocked-strong)
                           (atty/ui:label (format nil "   ~A" (or (getf (getf row :asks) :question) "")))
                           (atty/ui:gap :expand 1)))
          (t (atty/ui:column :expand 1
                             (atty/ui:label (format nil " ~A" (shortened-to (or (getf row :doing) "") 60)) :face :quiet)
                             (atty/ui:gap :expand 1))))))

(defun pane-block (b client row width &key (title t) ruled)
  "A pane inside a card: its title line and its rows."
  (apply #'atty/ui:column :align :stretch :expand 1
         (remove nil (list (and title (pane-title b client row width :ruled ruled))
                           (pane-rows-view b client row)))))

(defun tree-block (b client session tree width &key (first t))
  "A window's layout as blocks: a pane is one, a split a row or column of
them, a rule between; a pane under another has its title in the rule."
  (cond
    ((null tree) (atty/ui:gap :expand 1))
    ((consp tree)
     (let* ((parts (rest tree))
            (across (eq (first tree) :across))
            (n (max 1 (length parts))))
       (apply (if across #'atty/ui:row #'atty/ui:column)
              :align :stretch :expand 1 :spacing 0
              (loop :for part :in parts
                    :for i :from 0
                    :append (append
                             (when (and across (plusp i)) (list (atty/ui:rule :upright t :face :card)))
                             (list (tree-block b client session part
                                               (if across (floor (- width (1- n)) n) width)
                                               :first (and first (or across (zerop i))))))))))
    (t (let ((row (pane-row-of client session tree)))
         (if row
             (pane-block b client row width :title (not first) :ruled (not first))
             (atty/ui:gap :expand 1))))))

(defun client-glyph (c client)
  (atty/ui:label (if (here-p c client) "◆" "⌨") :face (if (here-p c client) :here :client)))

(defun window-card (b client session window width height)
  "One window as a card: its number and name in the top border with who is
looking at it, its panes inside, the answers of the one that asks in the
bottom border."
  (destructuring-bind (n label tree focus shownp) window
    (declare (ignore label focus shownp))
    (let* ((place (board-place b client))
           (cursor (and place (equal session (first place)) (eql n (second place))))
           (rows (window-rows client session window))
           (worst (window-worst-row client session window))
           (state (and worst (row-state worst)))
           (asking (find :blocked rows :key #'row-state))
           (single (and rows (null (rest rows)) (first rows)))
           (now (ms-here))
           (looking (clients-on client session n))
           (tags (loop :for c :in looking
                       :append (list (atty/ui:label " ") (client-glyph c client))))
           (title (atty/ui:row :spacing 0
                               (atty/ui:label (format nil " ~D ~A" n (shortened-to (window-name client session window) 14))
                                              :face (if cursor :cursor :strong))
                               (atty/ui:label (if single
                                                  (format nil " · ~A " (shortened-to (or (getf single :kind) "") 12))
                                                  (format nil " · ~D panes " (length rows)))
                                              :face :quiet)))
           (right (apply #'atty/ui:row :spacing 0
                         (append (when state
                                   (list (atty/ui:label (format nil " ~A ~A" (state-glyph state)
                                                                (since-text (or single worst) now))
                                                        :face (if (eq state :blocked) :state-blocked-strong (state-face state)))))
                                 tags
                                 (list (atty/ui:label " ")))))
           (body (if single
                     (pane-rows-view b client single)
                     (tree-block b client session tree (- width 2)))))
      (card title right (list body)
            :face (if state (state-face state) :card)
            :cursor cursor
            :bl (and asking (atty/ui:row :spacing 0 (atty/ui:label " ") (answer-buttons asking (- width 4)) (atty/ui:label " ")))
            :br (and cursor (card-keys b (>= width 60)))
            :runs (list :cursor session n)
            :width width :height height))))

(defun card-keys (b wide)
  "What can be pressed on the card with the cursor, in its bottom edge: go,
and on a wide card the rest of what acts on it."
  (declare (ignore b))
  (atty/ui:row :spacing 0
               (atty/ui:label " ")
               (if wide
                   (hints 'board-mode 'switchboard-go "go" 'switchboard-close-pane "close"
                          'switchboard-name "name" 'switchboard-prompt "prompt"
                          'switchboard-select "pick" 'switchboard-explain "details")
                   (hints 'board-mode 'switchboard-go "go"))))

(defun overview-chip (b client session window)
  "A window as a chip, for the overview: its number and name, yellow when a
pane in it asks, lit where the cursor is."
  (let* ((n (first window))
         (place (board-place b client))
         (cursor (and place (equal session (first place)) (eql n (second place))))
         (worst (window-worst-row client session window))
         (state (and worst (row-state worst))))
    (bar-button (list :cursor session n)
                (atty/ui:row :spacing 0
                             (atty/ui:label (format nil " ~D ~A " n (shortened-to (window-name client session window) 12))
                                            :face (cond ((eq state :blocked) :chip-blocked)
                                                        (cursor :cursor)
                                                        (t :key))
                                            :background-color (bar-face (if cursor :bg-active :bg-alt)))
                             (atty/ui:label (state-glyph state) :face (state-face state))
                             (atty/ui:label " ")))))

;;; Lanes: a header band, then the cards or the chips, then a rail when the
;;; lane is wider than the view.

(defun lane-header (b client session)
  (let* ((counts (session-counts client session))
         (windows (windows-of client session))
         (place (board-place b client))
         (cursor (and place (equal session (first place))))
         (looking (clients-on client session)))
    (destructuring-bind (asking working idle other) counts
      (band (list (atty/ui:label (if cursor "▶" " ") :face :cursor)
                  (bar-button (list :go session) (atty/ui:label (format nil " ~A " session) :face :strong))
                  (unless (side-shown-p b client)
                    (atty/ui:row :spacing 0 (atty/ui:label " ") (spark (session-cells client session)) (atty/ui:label " ")))
                  (atty/ui:label (format nil " ~D window~:P   " (length windows)) :face :quiet)
                  (atty/ui:label (if (plusp asking) (format nil "▲~D " asking) "") :face :state-blocked-strong)
                  (atty/ui:label (if (plusp working) (format nil "◐~D " working) "") :face :state-working)
                  (atty/ui:label (if (plusp idle) (format nil "○~D " idle) "") :face :state-idle)
                  (atty/ui:label (if (plusp other) (format nil "·~D " other) "") :face :quiet)
                  (apply #'atty/ui:row :spacing 1
                         (loop :for c :in looking :collect (who-client c client))))
            (list (keycap "+" "window" :runs (list :new-window session)) (atty/ui:label " "))))))

(defun board-room (client)
  "The rows the lanes have: under the bar and the header, above the status
and the footer."
  (max 3 (- (client-rows client) (if (client-barp client) 1 0) 3)))

(defun card-size (b client)
  "How wide and tall a card is at this zoom."
  (ecase (board-zoom b)
    (:cards (values +card-width+ +card-height+))
    (:one (values +big-card-width+ (max +card-height+ (- (board-room client) 3))))
    (:overview (values 0 1))))

(defun lane-folded-p (b client session)
  "A lane is folded to its header and its foot when somebody folded it, or
when it is quiet and quiet lanes are folded and the cursor is not in it."
  (or (member session (board-folded b) :test #'equal)
      (and (board-fold-quiet b) (quiet-p client session)
           (not (equal session (first (board-place b client)))))))

(defun lane-height (b client session)
  "A lane's rows: its header, its cards or chips, and its foot."
  (if (lane-folded-p b client session)
      2
      (+ 2 (nth-value 1 (card-size b client)))))

(defun lane-strip-width (b client session width)
  "How wide SESSION's lane is with every window in it."
  (let ((windows (windows-of client session)))
    (if (eq :overview (board-zoom b))
        (+ 2 (loop :for w :in windows
                   :sum (+ 7 (length (format nil "~D" (first w)))
                           (length (shortened-to (window-name client session w) 12)))))
        (+ 2 (* (length windows) (1+ width))))))

;;; The pulses, rolled up: a pane's cells are the server's; a window is its
;;; panes together, a session its windows, the server its sessions.

(defun pane-cells (client session id)
  (or (gethash (cons session id) (client-pulses client))
      (loop :repeat +spark-cells+ :collect (cons 0 nil))))

(defun window-cells (client session window)
  (roll-up (mapcar (lambda (id) (pane-cells client session id)) (panes-in-tree (third window)))))

(defun session-cells (client session)
  (roll-up (mapcar (lambda (w) (window-cells client session w)) (windows-of client session))))

(defun server-cells (client sessions)
  (roll-up (mapcar (lambda (s) (session-cells client s)) sessions)))

(defun lane-foot (b client session x0 wide folded)
  "The row that closes a lane: its rail, where the view is, and on the
cursor's lane what can be pressed here. Answers the tree and the rail."
  (let* ((place (board-place b client))
         (cursor (and place (equal session (first place))))
         (windows (windows-of client session))
         (n (length windows))
         (cw (card-size b client))
         (strip (lane-strip-width b client session cw))
         (fits (<= strip wide))
         (r (rail x0 wide (max wide strip) :upright nil :expand 1 :thumb-face (and cursor :here)))
         (text (cond (folded (format nil "folded · ~D window~:P" n))
                     ((or fits (eq :overview (board-zoom b))) (format nil "all ~D window~:P" n))
                     (t (let ((from (min (1- n) (floor x0 (1+ cw))))
                              (to (min (1- n) (floor (+ x0 wide -2) (1+ cw)))))
                          (format nil "windows ~D–~D of ~D" (1+ from) (1+ to) n)))))
         (keys (and cursor (hints 'board-mode "← →" "window" 'switchboard-new-window "new window"
                                  'switchboard-rename "rename"
                                  'switchboard-fold (if folded "unfold" "fold")))))
    (values (foot r text keys) r)))

(defun lane-strip (b client session width height)
  "The cards of SESSION's lane side by side, as one tree, and how wide it is."
  (let* ((windows (windows-of client session))
         (cards (loop :for w :in windows
                      :collect (if (eq :overview (board-zoom b))
                                   (overview-chip b client session w)
                                   (window-card b client session w width height))))
         (tree (apply #'atty/ui:row :align :start :spacing 1 (atty/ui:label " ") cards)))
    (values tree (lane-strip-width b client session width))))

(defun side-shown-p (b client)
  (and (board-side b) (>= (client-cols client) +side-from+)))

(defun side-width (b client)
  (cond ((not (side-shown-p b client)) 0)
        ((eq :one (board-zoom b)) +tree-width+)
        (t +side-width+)))

(defun lanes-left (b client)
  (let ((side (side-width b client)))
    (if (plusp side) (1+ side) 0)))

(defun lanes-width (b client)
  "The columns the lanes have: after the side pane and its rule, before the rail."
  (max 10 (- (client-cols client) (lanes-left b client) 1)))

(declaim (special *server-name*))

;;; The side pane: what is going on everywhere, not only where the cursor
;;; is. The server and every session with a sparkline each, the cursor's
;;; session unfolded to its windows, an activity log in a well, and who is
;;; attached.

(defparameter +side-name+ 11 "How many columns a name in the side pane has before its sparkline.")

(defun side-row (mark name spark right &key runs)
  "One row of the side pane: MARK in the gutter, NAME in its column, the
SPARK, and RIGHT after it."
  (gutter-row mark
              (atty/ui:row :spacing 0
                           (atty/ui:box :fixed-width +side-name+ name)
                           (or spark (atty/ui:label ""))
                           (atty/ui:label " ")
                           (or right (atty/ui:label "")))
              :runs runs))

(defun count-chip (counts)
  "The one count that matters most of COUNTS, (asking working idle other)."
  (destructuring-bind (a w i o) counts
    (cond ((plusp a) (atty/ui:label (format nil "▲~D" a) :face :state-blocked-strong))
          ((plusp w) (atty/ui:label (format nil "◐~D" w) :face :state-working))
          ((plusp i) (atty/ui:label (format nil "○~D" i) :face :state-idle))
          (t (atty/ui:label (format nil "·~D" o) :face :quiet)))))

(defun here-glyph (looking client)
  "◆ when this terminal is among LOOKING, else nothing."
  (if (find-if (lambda (c) (here-p c client)) looking)
      (atty/ui:label " ◆" :face :here)
      (atty/ui:label "")))

(defun session-tree-rows (b client session &key narrow)
  "The side pane's rows for SESSION: its own with its sparkline, and its
windows' when it is the cursor's session and not folded."
  (let* ((place (board-place b client))
         (on (and place (equal session (first place))))
         (unfolded (and on (not (member session (board-folded b) :test #'equal))))
         (windows (windows-of client session)))
    (cons (side-row (if on "▶" "")
                    (atty/ui:row :spacing 0
                                 (atty/ui:label (shortened-to session (if narrow 8 9)) :face :strong)
                                 (here-glyph (clients-on client session) client))
                    (unless narrow (spark (session-cells client session)))
                    (count-chip (session-counts client session))
                    :runs (list :cursor session (if on (second place) nil)))
          (when unfolded
            (loop :for w :in windows
                  :for last := (eq w (car (last windows)))
                  :collect (let* ((n (first w))
                                  (worst (window-worst-row client session w))
                                  (state (and worst (row-state worst)))
                                  (here (and place (eql n (second place)))))
                             (side-row ""
                                       (atty/ui:row :spacing 0
                                                    (atty/ui:label (if last "└ " "├ ") :face :tree)
                                                    (atty/ui:label (format nil "~D ~A" n (shortened-to (window-name client session w) (if narrow 5 7)))
                                                                   :face (if here :cursor :default)))
                                       (unless narrow (spark (window-cells client session w)))
                                       (atty/ui:row :spacing 0
                                                    (atty/ui:label (state-glyph state) :face (state-face state))
                                                    (here-glyph (clients-on client session n) client))
                                       :runs (list :cursor session n))))))))

(defun event-row (event client width)
  "One line of the activity log: when, what kind, which window, what, and
who when somebody did it. A click puts the cursor on that window."
  (destructuring-bind (age clock kind session id window who text) event
    (declare (ignore age id))
    (let* ((where (format nil "~A›~@[~D~]" (shortened-to session 5) window))
           (typed (eq kind :typed))
           (said (shortened-to (event-says kind text)
                               (max 4 (- width 10 (length where) (if (and who (not typed)) 2 0))))))
      (bar-button (list :cursor session window)
                  (atty/ui:row :spacing 0
                               (atty/ui:label (format nil " ~A " (clock-hm clock)) :face :quiet)
                               (if (and who typed)
                                   (atty/ui:label (who-text who client :short t) :face (who-face who client))
                                   (atty/ui:label (event-glyph kind) :face (event-face kind)))
                               (atty/ui:label (format nil " ~A " where) :face :strong)
                               (atty/ui:label said)
                               (if (and who (not typed))
                                   (atty/ui:label (format nil " ~A" (who-text who client :short t)) :face (who-face who client))
                                   (atty/ui:label "")))))))

(defun activity-log (b client width rows)
  "The activity log as a well ROWS rows deep, scrolled as far as the board
has it, with its own rail when there is more than fits."
  (let* ((events (client-events client))
         (n (length events))
         (y0 (max 0 (min (board-log-y0 b) (max 0 (- n rows)))))
         (shown (subseq events (min y0 n) (min n (+ y0 rows))))
         (railed (> n rows)))
    (setf (board-log-y0 b) y0)
    (values
     (atty/ui:column :align :stretch
                     (atty/ui:row :spacing 0
                                  (section "activity log")
                                  (atty/ui:gap)
                                  (atty/ui:label (if (plusp n)
                                                     (format nil "~D of ~D " (length shown) n)
                                                     "nothing yet ")
                                                 :face :quiet))
                     (well (or (loop :for e :in shown :collect (event-row e client (- width (if railed 1 0))))
                               (list (atty/ui:label "")))
                           :rail (and railed (rail y0 rows n :upright t))))
     (max 1 (min n rows)))))

(defun side-pane (b client sessions room)
  "The whole side pane as a column: the server, the sessions, the activity
log, the clients, in ROOM rows."
  (let* ((rows (rows-of client))
         (windows (loop :for s :in sessions :sum (length (windows-of client s))))
         (clients (client-clients client))
         (tree-only (eq :one (board-zoom b)))
         (width (side-width b client))
         (place (board-place b client))
         (on (first place))
         (unfolded (and on (not (member on (board-folded b) :test #'equal)) (length (windows-of client on))))
         (used (+ (if tree-only 0 3) 2 (length sessions) (or unfolded 0) 1 1 3 1 1 (max 1 (length clients))))
         (log-rows (max 3 (- room used))))
    (apply #'atty/ui:column :align :stretch :min-width width
           :background-color (bar-face :bg)
           (append
            (unless tree-only
              (list (section "servers")
                    (side-row "◉"
                              (atty/ui:label (or *server-name* "default") :face :strong)
                              (spark (server-cells client sessions))
                              (atty/ui:label (format nil "~Dw" windows) :face :quiet))
                    (atty/ui:label "")))
            (list (section "sessions")
                  (if tree-only
                      (atty/ui:label "")
                      (side-row "" (atty/ui:label "") (atty/ui:label "20m ago      now" :face :quiet) nil)))
            (loop :for s :in sessions :append (session-tree-rows b client s :narrow tree-only))
            (list (atty/ui:label ""))
            (unless tree-only
              (list (activity-log b client width log-rows)
                    (atty/ui:label "")))
            (list (section "clients"))
            (if clients
                (loop :for c :in clients
                      :collect (atty/ui:row :spacing 0
                                            (atty/ui:label " ")
                                            (who-client c client :pad (if tree-only 0 12))
                                            (if tree-only
                                                (atty/ui:label "")
                                                (atty/ui:label (format nil " ~A~@[ › ~D~]" (or (fifth c) "nothing") (sixth c)) :face :quiet))))
                (list (atty/ui:label "   nobody" :face :quiet)))
            (list (atty/ui:gap :expand 1))))))

;;; The status row and the composer.

(defun board-status (b client)
  (let* ((row (board-row b client))
         (place (board-place b client))
         (window (and place (find (second place) (windows-of client (first place)) :key #'first)))
         (asks (and row (getf row :asks))))
    (cond
      ((board-composing b) (composer b client))
      ((null row)
       (atty/ui:row :spacing 0 :background-color (bar-face :bg-alt)
                    (atty/ui:label " nothing here yet" :face :quiet)))
      (t
       (band (list (atty/ui:label " ")
                   (squeezed
                    (atty/ui:row :spacing 0
                                 (path '(:quiet "default") (first place)
                                       (format nil "~D~@[ ~A~]" (second place) (or (and window (second window)) (getf row :window-name)))
                                       (format nil "~D ~A" (1+ (or (getf row :at) 0)) (row-name row))
                                       (list :quiet (format nil "· ~A" (or (getf row :kind) ""))))
                                 (atty/ui:label "   ")
                                 (if asks
                                     (atty/ui:row :spacing 1
                                                  (atty/ui:label (format nil "▲ asks ~A" (getf asks :subject)) :face :state-blocked-strong)
                                                  (atty/ui:label (shortened-to (or (first (getf asks :detail)) (getf asks :question) "") 40) :face :quiet))
                                     (atty/ui:label (shortened-to (or (getf row :doing) "") 60) :face :quiet)))))
             (append (and asks (list (answer-buttons row 60) (atty/ui:label " ")))
                     (let* ((sessions (board-sessions b client))
                            (windows (windows-of client (first place))))
                       (list (atty/ui:label (format nil "session ~D of ~D · window ~D of ~D "
                                                    (1+ (or (position (first place) sessions :test #'equal) 0))
                                                    (length sessions)
                                                    (1+ (or (position (second place) windows :key #'first) 0))
                                                    (length windows))
                                            :face :quiet))))
             :ground :bg-alt)))))

(defun composer (b client)
  (let ((targets (board-targets b client)))
    (atty/ui:row :spacing 1 :background-color (bar-face :bg-alt)
                 (atty/ui:label (format nil " prompt ~D " (length targets)) :face :number-working)
                 (atty/ui:label (format nil "~{~A~^, ~}" (mapcar #'row-path targets)) :face :quiet)
                 (atty/ui:label "›" :face :quiet)
                 (atty/ui:label (format nil "~A_" (board-composing b)))
                 (atty/ui:gap)
                 (atty/ui:label "RET sends · a busy one gets it when it is next idle "
                                :face :quiet))))

(defun board-keys (b)
  "The keys that work anywhere on the board, for its footer. The lane's keys
are on the lane's foot and the card's on the card."
  (if (eq :one (board-zoom b))
      (hints 'board-mode "↑↓" "session" 'switchboard-next-asking "next ▲" 'switchboard-zoom-out "every session"
             'switchboard-filter "filter" 'switchboard-side "side pane")
      (hints 'board-mode "↑↓" "session" 'switchboard-next-asking "next ▲" "- +" "zoom"
             'switchboard-filter "filter" 'switchboard-side "side pane")))

;;; Drawing. The side pane, the header, the status and the footer are laid
;;; in place. Each lane's strip is drawn whole on a sheet of its own and the
;;; part in view copied in, slid as far as the lane is; the lanes are slid up
;;; by y0 and cut to the room. Where everything landed is kept in regions so
;;; a click can be taken back to the tree it fell on.

(defun copy-cells (from into fx fy tx ty width height)
  "WIDTH by HEIGHT cells of FROM at FX, FY into INTO at TX, TY, clipped to both."
  (loop :for y :below height
        :for sy := (+ fy y) :for dy := (+ ty y)
        :when (and (<= 0 sy) (< sy (tty:screen-height from)) (<= 0 dy) (< dy (tty:screen-height into)))
          :do (let ((srow (aref (tty:screen-grid from) sy))
                    (drow (aref (tty:screen-grid into) dy)))
                (loop :for x :below width
                      :for sx := (+ fx x) :for dx := (+ tx x)
                      :when (and (<= 0 sx) (< sx (tty:screen-width from)) (<= 0 dx) (< dx (tty:screen-width into)))
                        :do (setf (term:row-char drow dx) (term:row-char srow sx)
                                  (term:row-face drow dx) (term:row-face srow sx))))))

(defun draw-in (tree screen left top right bottom)
  "Lay TREE at LEFT, TOP, painted no further right than RIGHT nor lower than BOTTOM."
  (let ((atty/cells::*right-edge* right))
    (atty/cells:draw tree (tty:screen-grid screen) right bottom :left left :top top))
  tree)

(defun lane-tops (b client sessions)
  "Where each lane starts, counting from the top of the lanes, by session."
  (let ((at 0) (out nil))
    (dolist (s sessions (nreverse out))
      (push (cons s at) out)
      (incf at (lane-height b client s)))))

(defun keep-cursor-in-view (b client sessions)
  "Slide the cursor's lane and the lanes the least it takes for the cursor's
card to be in sight."
  (let ((place (board-place b client)))
    (when place
      (let* ((cw (card-size b client))
             (session (first place))
             (windows (windows-of client session))
             (i (or (position (second place) windows :key #'first) 0))
             (wide (lanes-width b client))
             (room (board-room client))
             (x0 (gethash session (board-slid b) 0))
             (left (+ 1 (* i (1+ cw))))
             (top (or (cdr (assoc session (lane-tops b client sessions) :test #'equal)) 0))
             (height (lane-height b client session)))
        (unless (eq :overview (board-zoom b))
          (when (< left x0) (setf x0 (max 0 (1- left))))
          (when (> (+ left cw) (+ x0 wide)) (setf x0 (max 0 (- (+ left cw 1) wide))))
          (setf (gethash session (board-slid b)) x0))
        (when (< top (board-y0 b)) (setf (board-y0 b) top))
        (when (> (+ top height) (+ (board-y0 b) room))
          (setf (board-y0 b) (max 0 (- (+ top height) room))))))))

(defun board-header (b client sessions)
  (let ((counts (reduce (lambda (a c) (mapcar #'+ a c))
                        (mapcar (lambda (s) (session-counts client s)) sessions)
                        :initial-value '(0 0 0 0)))
        (n-windows (loop :for s :in sessions :sum (length (windows-of client s)))))
    (destructuring-bind (asking working idle other) counts
      (header-band "switchboard"
                   :path (atty/ui:row :spacing 0
                                      (atty/ui:label "server " :face :quiet)
                                      (atty/ui:label "default" :face :strong)
                                      (atty/ui:label (format nil " · ~D session~:P · ~D window~:P · ~D pane~:P    "
                                                             (length sessions) n-windows (length (rows-of client)))
                                                     :face :quiet)
                                      (atty/ui:label (if (plusp asking) (format nil "▲ ~D asking  " asking) "") :face :state-blocked-strong)
                                      (atty/ui:label (if (plusp working) (format nil "◐ ~D working  " working) "") :face :state-working)
                                      (atty/ui:label (if (plusp idle) (format nil "○ ~D idle  " idle) "") :face :state-idle)
                                      (atty/ui:label (if (plusp other) (format nil "· ~D other" other) "") :face :quiet))
                   :right (list (bar-button '(:side) (atty/ui:label "\\ side pane" :face :quiet))
                                (atty/ui:label "  ")
                                (atty/ui:label (ecase (board-zoom b) (:overview "sessions") (:cards "windows") (:one "one session"))
                                               :face :quiet))))))

(defmethod draw-over ((b board) screen)
  (let* ((client *drawing-for*)
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (top (if (client-barp client) (min 1 (max 0 (1- rows))) 0))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows))
         (regions nil))
    (board-place b client)
    (board-ask b client)
    (let* ((sessions (let ((all (board-sessions b client)))
                       (if (eq :one (board-zoom b))
                           (let ((on (first (board-place b client)))) (if on (list on) all))
                           all)))
           (room (board-room client))
           (area-top (1+ top))
           (area-bottom (+ area-top room))
           (left (lanes-left b client))
           (wide (lanes-width b client))
           (side (side-width b client)))
      (when (board-following b)
        (keep-cursor-in-view b client sessions)
        (setf (board-following b) nil))
      ;; never slid past the end of what there is
      (let ((tall (loop :for s :in sessions :sum (lane-height b client s))))
        (setf (board-y0 b) (max 0 (min (board-y0 b) (- tall room)))))
      (multiple-value-bind (cw ch) (card-size b client)
        (declare (ignore ch))
        (dolist (s sessions)
          (let ((strip (if (eq :overview (board-zoom b)) 0 (+ 2 (* (length (windows-of client s)) (1+ cw))))))
            (setf (gethash s (board-slid b))
                  (max 0 (min (gethash s (board-slid b) 0) (- strip wide)))))))
      (atty/cells:fill-rect m 0 top cols (- rows top) (term:make-face :bg (bar-face :bg)))
      ;; the header
      (push (list :tree (draw-in (board-header b client sessions) screen 0 top cols (1+ top))) regions)
      ;; the side pane and its rule
      (when (plusp side)
        (push (list :tree (draw-in (side-pane b client sessions room) screen 0 area-top side area-bottom)) regions)
        (atty/cells:fill-rect m side area-top 1 room (atty/cells:face-of (atty/ui:label "" :face :card)) #\│))
      ;; the lanes
      (multiple-value-bind (cw ch) (card-size b client)
        (dolist (it (lane-tops b client sessions))
          (destructuring-bind (session . lane-top) it
            (let* ((y (+ area-top (- lane-top (board-y0 b))))
                   (folded (lane-folded-p b client session))
                   (x0 (if (eq :overview (board-zoom b)) 0 (gethash session (board-slid b) 0))))
              (when (and (< y area-bottom) (> (+ y (lane-height b client session)) area-top))
                (when (>= y area-top)
                  (push (list :tree (draw-in (lane-header b client session) screen left y (+ left wide) (1+ y)))
                        regions))
                (unless folded
                  (multiple-value-bind (tree strip-width) (lane-strip b client session cw ch)
                    (let* ((sw (max strip-width wide))
                           (sheet (tty:make-screen :width sw :height ch)))
                      (atty/cells:fill-rect (atty/cells:make-cells (tty:screen-grid sheet) sw ch)
                                            0 0 sw ch (term:make-face :bg (bar-face :bg)))
                      (atty/cells:draw tree (tty:screen-grid sheet) sw ch)
                      (let ((from (max area-top (1+ y)))
                            (to (min area-bottom (+ y 1 ch))))
                        (when (< from to)
                          (copy-cells sheet screen x0 (- from (1+ y)) left from wide (- to from))
                          (push (list :sheet tree left from x0 (- from (1+ y)) wide (- to from)) regions))))))
                ;; the foot: the lane's rail, where it is, and its keys
                (let ((ry (+ y (1- (lane-height b client session)))))
                  (when (and (>= ry area-top) (< ry area-bottom))
                    (multiple-value-bind (tree r) (lane-foot b client session x0 wide folded)
                      (draw-in tree screen left ry (+ left wide) (1+ ry))
                      (push (list :tree tree) regions)
                      (push (list :rail-h r session (atty/ui:left r) ry (atty/ui:width r)) regions))))))))
        (let* ((tall (loop :for s :in sessions :sum (lane-height b client s)))
               (r (rail (board-y0 b) room (max room tall))))
          (draw-in (atty/ui:row :align :stretch r) screen (1- cols) area-top cols area-bottom)
          (push (list :rail-v r (1- cols) area-top room) regions)))
      ;; the status and the footer
      (push (list :tree (draw-in (board-status b client) screen 0 (- rows 2) cols (1- rows))) regions)
      (push (list :tree (draw-in (footer-band (board-keys b)
                                              :right (list (hint "?" "keys" :runs "keys of this mode")
                                                           (hint "Esc" "close" :runs :close)))
                                 screen 0 (1- rows) cols rows))
            regions)
      (setf (board-regions b) (if (plusp side) (cons (list :side 0 area-top side room) regions) regions)
            (board-area-top b) area-top
            (tty:screen-cursor-visible screen) nil))))

(defun board-ask (b client)
  "Every so often, ask for the layouts of every session known and who is
attached: what the board is drawn from beyond the pane rows."
  (when (and (client-wire client) (>= (- (ms-here) (board-asked b)) +asked-every+))
    (setf (board-asked b) (ms-here))
    (let ((*client* client))
      (dolist (s (board-sessions b client))
        (tell-the-server-if-it-knows (list :layouts s)))
      (tell-the-server-if-it-knows (list :clients))
      (tell-the-server-if-it-knows (list :pulses))
      (tell-the-server-if-it-knows (list :events 64)))))

(defmethod ticks-p ((b board)) t)
(defmethod over-name ((b board)) "switchboard")
(defmethod close-over ((b board) client) (board-close-it b client))

;;; What a key or a click does.

(atty/mode:define-mode board-mode ())
(atty/mode:define-mode board-type-mode ()
  (:documentation "Typing a filter or a prompt: every letter is what is typed."))

(defmethod mode-of ((b board))
  (cond ((or (board-filtering b) (board-composing b)) 'board-type-mode)
        (t 'board-mode)))

(defun the-board ()
  (let ((it (first (client-over *client*))))
    (when (typep it 'board) it)))

(defmethod unbound ((b board) chord client)
  (let ((said (atty/mode:self-inserting chord)))
    (when said
      (cond
        ((board-composing b)
         (setf (board-composing b) (concatenate 'string (board-composing b) said)))
        ((board-filtering b)
         (setf (board-query b) (concatenate 'string (board-query b) said)))
        ((and (= 1 (length said)) (digit-char-p (char said 0))
              (plusp (digit-char-p (char said 0))))
         (let ((*client* client)
               (row (board-row b client)))
           (when (and row (getf row :asks))
             (tell-the-server (list :answer (getf row :session) (getf row :id)
                                    (digit-char-p (char said 0))))))))
      (setf (client-dirty client) t)
      t)))

;;; Moving: across windows, down sessions, through the panes of a window.

(defun board-go-to (b client session n &optional id)
  "The cursor onto window N of SESSION, on pane ID or its first; the view
follows it when it is drawn."
  (let* ((windows (windows-of client session))
         (window (or (find n windows :key #'first) (first windows))))
    (when window
      (let ((id (or (and id (member id (panes-in-tree (third window))) id)
                    (first (panes-in-tree (third window))))))
        (setf (board-cursor b) (list session (first window) id))))
    (setf (board-following b) t
          (client-dirty client) t)))

(defcommand (switchboard-left :unlisted)
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place (board-go-to b *client* (first place) (max 1 (1- (second place)))))))

(defcommand (switchboard-right :unlisted)
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place (board-go-to b *client* (first place) (1+ (second place))))))

(defcommand (switchboard-first-window :unlisted)
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place (board-go-to b *client* (first place) 1))))

(defcommand (switchboard-last-window :unlisted)
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place (board-go-to b *client* (first place) (length (windows-of *client* (first place)))))))

(defun switchboard-session-by (by)
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place
      (let* ((sessions (board-sessions b *client*))
             (at (position (first place) sessions :test #'equal))
             (to (max 0 (min (1- (length sessions)) (+ (or at 0) by)))))
        (board-go-to b *client* (nth to sessions) (second place))))))

(defcommand (switchboard-up :unlisted) (switchboard-session-by -1))
(defcommand (switchboard-down :unlisted) (switchboard-session-by 1))

(defcommand (switchboard-next-pane :unlisted)
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place
      (let* ((window (find (second place) (windows-of *client* (first place)) :key #'first))
             (ids (and window (panes-in-tree (third window))))
             (at (position (third place) ids)))
        (when ids
          (board-go-to b *client* (first place) (second place)
                       (nth (mod (1+ (or at -1)) (length ids)) ids)))))))

(defcommand (switchboard-select :unlisted)
  "pick the cursor's pane, for a prompt to several at once"
  (let* ((b (the-board)) (key (and b (key-of-cursor b *client*))))
    (when key
      (setf (board-picked b)
            (if (member key (board-picked b) :test #'equal)
                (remove key (board-picked b) :test #'equal)
                (cons key (board-picked b)))))))

(defun board-close-it (b client)
  (client-over-drop client b)
  (stop-told client))

(defcommand (switchboard-go :unlisted)
  "go to the cursor's pane"
  (let* ((b (the-board)) (row (and b (board-row b *client*))))
    (when b
      (board-close-it b *client*)
      (when row (tell-the-server (list :focus-pane (getf row :session) (getf row :id)))))))

(defun board-zoom-to (b client level)
  (setf (board-zoom b) level
        (board-y0 b) 0
        (board-following b) t
        (client-dirty client) t))

(defcommand (switchboard-zoom-in :unlisted)
  "closer: the sessions as rows of windows, then the windows themselves, then one session"
  (let ((b (the-board)))
    (when b (board-zoom-to b *client* (ecase (board-zoom b) (:overview :cards) ((:cards :one) :one))))))

(defcommand (switchboard-zoom-out :unlisted)
  "further: one session to every session's windows, then to a row of windows each"
  (let ((b (the-board)))
    (when b (board-zoom-to b *client* (ecase (board-zoom b) (:one :cards) ((:cards :overview) :overview))))))

(defcommand (switchboard-side :unlisted)
  "the side pane off or on"
  (let ((b (the-board)))
    (when b (setf (board-side b) (not (board-side b)) (client-dirty *client*) t))))

(defcommand (switchboard-sort :unlisted)
  "the sessions asking first, by name, or as made"
  (let ((b (the-board)))
    (when b (setf (board-sort b) (ecase (board-sort b) (:asking :name) (:name :order) (:order :asking))
                  (client-dirty *client*) t))))

(defcommand (switchboard-fold-quiet :unlisted)
  "lanes with nothing doing folded to their header, or not"
  (let ((b (the-board)))
    (when b (setf (board-fold-quiet b) (not (board-fold-quiet b)) (client-dirty *client*) t))))

(defcommand (switchboard-live :unlisted)
  "windows showing their panes' last rows, or what each says it is doing"
  (let ((b (the-board)))
    (when b (setf (board-live b) (not (board-live b)) (client-dirty *client*) t))))

(defcommand (switchboard-fold :unlisted)
  "fold the cursor's session to its header and foot, or unfold it"
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place
      (setf (board-folded b) (if (member (first place) (board-folded b) :test #'equal)
                                 (remove (first place) (board-folded b) :test #'equal)
                                 (cons (first place) (board-folded b)))
            (client-dirty *client*) t))))

(defcommand (switchboard-new-window :unlisted)
  "another window in the cursor's session"
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place
      (tell-the-server (list :new-window (first place)))
      (setf (board-asked b) 0))))

(defcommand (switchboard-new-session :unlisted)
  "another session"
  (when (the-board) (tell-the-server (list :new))))

(defcommand (switchboard-clients :unlisted)
  (let ((b (the-board)))
    (when b (board-close-it b *client*))
    (run-command "clients" *client*)))

(defcommand (switchboard-prompt :unlisted)
  "prompt the picked panes, or the cursor's, from here"
  (let ((b (the-board)))
    (when (and b (board-targets b *client*))
      (setf (board-composing b) "")
      (client-in-mode *client*))))

(defcommand (switchboard-read :unlisted)
  "read the cursor's pane whole"
  (let* ((b (the-board)) (row (and b (board-row b *client*))))
    (when row (tell-the-server (list :pane-read (getf row :session) (getf row :id))))))

(defcommand (switchboard-name :unlisted)
  "name the cursor's window; TAB names its pane instead"
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place
      (let ((window (find (second place) (windows-of *client* (first place)) :key #'first)))
        (ask-a-window-name *client* (first place) (second place) (and window (second window)))))))

(defcommand (switchboard-rename :unlisted)
  "name the cursor's session"
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place (ask-a-session-name *client* (first place)))))

(defmethod session-renamed-over ((b board) old new)
  "The cursor, the lane slid, the folds and where it opened follow the name."
  (flet ((renamed (name) (if (equal name old) new name)))
    (when (board-cursor b) (setf (first (board-cursor b)) (renamed (first (board-cursor b)))))
    (setf (board-starting b) (renamed (board-starting b))
          (board-folded b) (mapcar #'renamed (board-folded b))
          (board-picked b) (mapcar (lambda (k) (cons (renamed (car k)) (cdr k))) (board-picked b)))
    (let ((slid (gethash old (board-slid b))))
      (when slid
        (remhash old (board-slid b))
        (setf (gethash new (board-slid b)) slid)))))

(defcommand (switchboard-next-asking :unlisted)
  "the cursor to whatever has been asking longest, in any session"
  (let* ((b (the-board))
         (asking (and b (remove-if-not (lambda (r) (getf r :asks)) (board-rows b *client*))))
         (oldest (first (sort (copy-list asking) #'> :key (lambda (r) (or (getf r :for) 0))))))
    (when oldest
      (board-go-to b *client* (getf oldest :session) (getf oldest :window) (getf oldest :id)))))

(defcommand (switchboard-explain :unlisted)
  "why the cursor's pane is what it is, and who typed into it"
  (let* ((b (the-board)) (row (and b (board-row b *client*))))
    (when row (open-the-drawer *client* (row-key row)))))

(defcommand (switchboard-close-pane :unlisted)
  "close the cursor's pane, after a yes"
  (let* ((b (the-board)) (row (and b (board-row b *client*))))
    (when row
      (let ((session (getf row :session)) (id (getf row :id)))
        (confirm *client* (format nil "close ~A and what runs in it?" (row-path row))
                 :yes (lambda (c)
                        (let ((*client* c)) (tell-the-server (list :close-pane session id)))))))))

(defcommand (switchboard-filter :unlisted)
  "narrow the board to what matches"
  (let ((b (the-board)))
    (when b
      (setf (board-filtering b) t)
      (client-in-mode *client*))))

(defcommand (switchboard-close :unlisted)
  (let ((b (the-board)))
    (when b (board-close-it b *client*))))

(defcommand (switchboard-typed :unlisted)
  ;; RET while typing: a prompt goes to its targets, a filter is kept
  (let ((b (the-board)))
    (when b
      (cond
        ((board-composing b)
         (let ((text (board-composing b)))
           (when (plusp (length text))
             (dolist (row (board-targets b *client*))
               (tell-the-server (list :prompt-when-idle (getf row :session) (getf row :id) text))))
           (setf (board-composing b) nil
                 (board-picked b) nil)))
        (t (setf (board-filtering b) nil)))
      (client-in-mode *client*))))

(defcommand (switchboard-untyped :unlisted)
  ;; Esc while typing: the prompt or the filter is dropped
  (let ((b (the-board)))
    (when b
      (if (board-composing b)
          (setf (board-composing b) nil)
          (setf (board-filtering b) nil (board-query b) ""))
      (client-in-mode *client*))))

(defcommand (switchboard-rub-out :unlisted)
  (let ((b (the-board)))
    (when b
      (flet ((less (s) (subseq s 0 (max 0 (1- (length s))))))
        (if (board-composing b)
            (setf (board-composing b) (less (board-composing b)))
            (setf (board-query b) (less (board-query b))))))))

;;; The mouse: what was clicked is found in the regions the last drawing
;;; kept, each a tree laid where it was drawn, or a sheet a lane was drawn on
;;; and how far it was slid, or a rail.

(defun rail-clicked (r pos &key (step 3))
  "Where a rail R is after a click at POS along it: an arrow steps, the track
pages, the thumb stays where it is to be dragged; never past either end."
  (let ((most (max 0 (- (rail-total-of r) (rail-extent-of r)))))
    (min most
         (case (rail-part r pos)
           (:up (max 0 (- (rail-at-of r) step)))
           (:down (+ (rail-at-of r) step))
           (:above (max 0 (- (rail-at-of r) (rail-extent-of r))))
           (:below (+ (rail-at-of r) (rail-extent-of r)))
           (t (rail-at-of r))))))

(defun board-hit (b col line)
  "What is under a click at COL, LINE: (:runs form) for a button, (:rail-h
session at) or (:rail-v at) for a rail, with the rail and how far along its
thumb was taken hold of when it was the thumb, else nil."
  (dolist (region (board-regions b))
    (case (first region)
      (:side nil)
      (:tree
       (let ((hit (button-at (second region) line col)))
         (when hit (return (list :runs (bar-button-runs hit))))))
      (:sheet
       (destructuring-bind (tree left top x0 y0 width height) (rest region)
         (when (and (<= left col) (< col (+ left width)) (<= top line) (< line (+ top height)))
           (let ((hit (button-at tree (+ y0 (- line top)) (+ x0 (- col left)))))
             (when hit (return (list :runs (bar-button-runs hit))))))))
      (:rail-h
       (destructuring-bind (r session left line-at width) (rest region)
         (when (and (= line line-at) (<= left col) (< col (+ left width)))
           (return (list :rail-h session (rail-clicked r col :step 4)
                         (and (eq :thumb (rail-part r col)) (list r (rail-grabbed r col))))))))
      (:rail-v
       (destructuring-bind (r col-at top height) (rest region)
         (when (and (= col col-at) (<= top line) (< line (+ top height)))
           (return (list :rail-v (rail-clicked r line)
                         (and (eq :thumb (rail-part r line)) (list r (rail-grabbed r line)))))))))))

(defun rail-grabbed (r pos)
  "How far along the thumb of R the cell at POS is."
  (multiple-value-bind (from track) (rail-track r)
    (let ((top (rail-thumb track (rail-extent-of r) (rail-total-of r) (rail-at-of r))))
      (max 0 (- pos from top)))))

(defun board-do (b runs client)
  "What a button on the board means."
  (destructuring-bind (what &rest it) runs
    (case what
      (:cursor
       (destructuring-bind (session &optional n) it
         (let ((place (board-place b client)))
           (if (and place (equal session (first place)) (or (null n) (eql n (second place))))
               (let ((*client* client)) (switchboard-go))
               (board-go-to b client session (or n 1))))))
      (:option (let ((*client* client)) (tell-the-server (list :answer (car (first it)) (cdr (first it)) (second it)))))
      (:go (board-close-it b client) (let ((*client* client)) (tell-the-server (list :go (first it)))))
      (:zoom (board-zoom-to b client (first it)))
      (:side (setf (board-side b) (not (board-side b)) (client-dirty client) t))
      (:sort (let ((*client* client)) (switchboard-sort)))
      (:toggle (let ((*client* client))
                 (ecase (first it) (:fold-quiet (switchboard-fold-quiet)) (:live (switchboard-live)))))
      (:filter (let ((*client* client)) (switchboard-filter)))
      (:new-window (let ((*client* client))
                     (tell-the-server runs)
                     (setf (board-asked b) 0)))
      (t (generic-click b runs client)))))

(defcommand (switchboard-click :unlisted)
  "a click on a card or a row puts the cursor there, and on the cursor goes
there; on an answer, answers; on a control, does what it says; on a rail,
slides"
  (let* ((b (the-board))
         (hit (and b *mouse-at* (board-hit b (car *mouse-at*) (cdr *mouse-at*)))))
    (setf (board-held b) nil)
    (when hit
      (ecase (first hit)
        (:runs (board-do b (second hit) *client*))
        (:rail-h (setf (gethash (second hit) (board-slid b)) (max 0 (third hit))
                       (board-held b) (and (fourth hit) (list* :rail-h (second hit) (fourth hit)))
                       (board-following b) nil (client-dirty *client*) t))
        (:rail-v (setf (board-y0 b) (max 0 (second hit))
                       (board-held b) (and (third hit) (cons :rail-v (third hit)))
                       (board-following b) nil (client-dirty *client*) t))))))

(defcommand (switchboard-drag :unlisted)
  "a thumb taken hold of goes where the pointer goes"
  (let* ((b (the-board)) (held (and b (board-held b))))
    (when (and held *mouse-at*)
      (ecase (first held)
        (:rail-h (destructuring-bind (session r grabbed) (rest held)
                   (setf (gethash session (board-slid b)) (rail-at r (car *mouse-at*) grabbed))))
        (:rail-v (destructuring-bind (r grabbed) (rest held)
                   (setf (board-y0 b) (rail-at r (cdr *mouse-at*) grabbed)))))
      (setf (board-following b) nil (client-dirty *client*) t))))

(defcommand (switchboard-let-go :unlisted)
  (let ((b (the-board))) (when b (setf (board-held b) nil))))

(defcommand (switchboard-nothing :unlisted) nil)

(defun board-scroll (b client dy)
  (setf (board-y0 b) (max 0 (+ (board-y0 b) dy))
        (board-following b) nil
        (client-dirty client) t))

(defun lane-at-line (b client line)
  "The session whose lane is on screen row LINE, or nil."
  (let ((sessions (board-sessions b client)))
    (loop :for (s . top) :in (lane-tops b client sessions)
          :for y := (+ (board-area-top b) (- top (board-y0 b)))
          :when (and (<= y line) (< line (+ y (lane-height b client s))))
            :return s)))

(defun board-slide (b client dx)
  "Slide the lane under the pointer, or the cursor's when the pointer is on none."
  (let ((session (or (and *mouse-at* (lane-at-line b client (cdr *mouse-at*)))
                     (first (board-place b client)))))
    (when session
      (setf (gethash session (board-slid b))
            (max 0 (+ (gethash session (board-slid b) 0) dx))
            (board-following b) nil
            (client-dirty client) t))))

(defun over-the-side-p (b)
  "Whether the pointer is over the side pane."
  (let ((side (find :side (board-regions b) :key #'first)))
    (and side *mouse-at*
         (destructuring-bind (left top width height) (rest side)
           (and (<= left (car *mouse-at*)) (< (car *mouse-at*) (+ left width))
                (<= top (cdr *mouse-at*)) (< (cdr *mouse-at*) (+ top height)))))))

(defun board-wheel (b client dy)
  "The wheel over the side pane scrolls the activity log; anywhere else, the lanes."
  (if (over-the-side-p b)
      (setf (board-log-y0 b) (max 0 (+ (board-log-y0 b) dy))
            (client-dirty client) t)
      (board-scroll b client dy)))

(defcommand (switchboard-wheel-up :unlisted)
  (let ((b (the-board))) (when b (board-wheel b *client* -3))))
(defcommand (switchboard-wheel-down :unlisted)
  (let ((b (the-board))) (when b (board-wheel b *client* 3))))
(defcommand (switchboard-wheel-left :unlisted)
  (let ((b (the-board))) (when b (board-slide b *client* -4))))
(defcommand (switchboard-wheel-right :unlisted)
  (let ((b (the-board))) (when b (board-slide b *client* 4))))
(defcommand (switchboard-page-up :unlisted)
  (let ((b (the-board))) (when b (board-scroll b *client* (- (board-room *client*))))))
(defcommand (switchboard-page-down :unlisted)
  (let ((b (the-board))) (when b (board-scroll b *client* (board-room *client*)))))

(atty/mode:define-key 'board-mode "Left"   #'switchboard-left)
(atty/mode:define-key 'board-mode "Right"  #'switchboard-right)
(atty/mode:define-key 'board-mode "Up"     #'switchboard-up)
(atty/mode:define-key 'board-mode "Down"   #'switchboard-down)
(atty/mode:define-key 'board-mode "Home"   #'switchboard-first-window)
(atty/mode:define-key 'board-mode "End"    #'switchboard-last-window)
(atty/mode:define-key 'board-mode "TAB"    #'switchboard-fold)
(atty/mode:define-key 'board-mode "o"      #'switchboard-next-pane)
(atty/mode:define-key 'board-mode "SPC"    #'switchboard-select)
(atty/mode:define-key 'board-mode "RET"    #'switchboard-go)
(atty/mode:define-key 'board-mode "+"      #'switchboard-zoom-in)
(atty/mode:define-key 'board-mode "="      #'switchboard-zoom-in)
(atty/mode:define-key 'board-mode "-"      #'switchboard-zoom-out)
(atty/mode:define-key 'board-mode "\\"     #'switchboard-side)
(atty/mode:define-key 'board-mode "c"      #'switchboard-new-window)
(atty/mode:define-key 'board-mode "C"      #'switchboard-new-session)
(atty/mode:define-key 'board-mode "p"      #'switchboard-prompt)
(atty/mode:define-key 'board-mode "n"      #'switchboard-next-asking)
(atty/mode:define-key 'board-mode "r"      #'switchboard-rename)
(atty/mode:define-key 'board-mode ","      #'switchboard-name)
(atty/mode:define-key 'board-mode "e"      #'switchboard-explain)
(atty/mode:define-key 'board-mode "x"      #'switchboard-close-pane)
(atty/mode:define-key 'board-mode "s"      #'switchboard-sort)
(atty/mode:define-key 'board-mode "f"      #'switchboard-fold-quiet)
(atty/mode:define-key 'board-mode "l"      #'switchboard-live)
(atty/mode:define-key 'board-mode "/"      #'switchboard-filter)
(atty/mode:define-key 'board-mode "?"      "keys of this mode")
(atty/mode:define-key 'board-mode "Escape" #'switchboard-close)
(atty/mode:define-key 'board-mode "C-g"    #'switchboard-close)
(atty/mode:define-key 'board-mode "mouse-1" #'switchboard-click)
(atty/mode:define-key 'board-mode "mouse-1-up" #'switchboard-let-go)
(atty/mode:define-key 'board-mode "mouse-1-drag" #'switchboard-drag)
(atty/mode:define-key 'board-mode "wheel-left"  #'switchboard-wheel-left)
(atty/mode:define-key 'board-mode "wheel-right" #'switchboard-wheel-right)
(atty/mode:define-key 'board-mode "PageUp" #'switchboard-page-up)
(atty/mode:define-key 'board-mode "PageDown" #'switchboard-page-down)
(atty/mode:define-key 'board-mode "wheel-up"   #'switchboard-wheel-up)
(atty/mode:define-key 'board-mode "wheel-down" #'switchboard-wheel-down)
(atty/mode:define-key 'board-mode "S-wheel-up"   #'switchboard-wheel-left)
(atty/mode:define-key 'board-mode "S-wheel-down" #'switchboard-wheel-right)

(atty/mode:define-key 'board-type-mode "RET"    #'switchboard-typed)
(atty/mode:define-key 'board-type-mode "DEL"    #'switchboard-rub-out)
(atty/mode:define-key 'board-type-mode "Escape" #'switchboard-untyped)
(atty/mode:define-key 'board-type-mode "C-g"    #'switchboard-untyped)

(defun open-the-board (client &key (session (client-session client)))
  "Put the switchboard over CLIENT's session, the cursor on where it is
looking once there are rows."
  (keep-told client)
  (client-over-put client (%make-board :starting session)))

(defcommand (switchboard :group sessions)
  "the whole server: every session, window and pane, and who is looking"
  (open-the-board *client*))
