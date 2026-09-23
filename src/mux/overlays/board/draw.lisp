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

;;; What the client knows, arranged: sessions in the order asked for, each
;;; with its windows, each with its layout of panes.

;;; Where the cursor is.

;;; Cards: a window as a box of one size, its panes inside in their real
;;; splits. A pane is a title line in its state's colour and its last rows.

(defun answer-buttons (row width)
  (let ((options (getf (getf row :asks) :options)))
    (apply #'atty/ui:row :spacing 1
           (loop :for (n) :in options
                 :for text :in (fit-options options (max 8 (- width 4)))
                 :collect (keycap n text :runs (list :option (row-key row) n))))))

(defun state-chip (row)
  "ROW's state as a chip: the glyph, on the state's ground."
  (let ((state (row-state row)))
    (atty/ui:label (format nil " ~A " (state-glyph state))
                   :face (if state (number-face state) :number-unknown))))

(defun row-duration-text (row now)
  (let ((state (row-state row)))
    (if state (format-duration (row-duration row now)) "")))

(defun pane-title (b client row width &key ruled)
  "One line naming a pane inside a card: number, name, kind, and its state
and how long at the right. RULED draws it as a rule with the title in it,
for a pane under another."
  (let* ((now (client-ms))
         (state (row-state row))
         (place (board-place b client))
         (cursor (and place (equal (row-key row) (cons (first place) (third place)))))
         (name (format nil "~D ~A" (1+ (or (getf row :at) 0)) (row-name row)))
         (since (row-duration-text row now)))
    (if ruled
        (atty/ui:row :spacing 0
                     (atty/ui:label "┄┄ " :face :card)
                     (atty/ui:label (format nil "~A ~A" (state-glyph state) name)
                                    :face (if cursor :cursor (if state (state-face state) :quiet)))
                     (atty/ui:label (format nil " ~A " since) :face :quiet)
                     (atty/ui:rule :glyph #\┄ :face :card :expand 1))
        (let* ((kind (truncate-string (or (getf row :kind) "") 12))
               (room (- width 3 1 (length since) 1))
               (with-kind (>= (- room (1+ (length kind))) 6))
               (name (truncate-string name (max 3 (if with-kind (- room (1+ (length kind))) room)))))
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
                             (atty/ui:label (format nil " ~A" (truncate-string (or (getf row :doing) "") 60)) :face :quiet)
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
  (atty/ui:label (if (this-client-p c client) "◆" "⌨") :face (if (this-client-p c client) :here :client)))

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
           (now (client-ms))
           (looking (clients-on client session n))
           (tags (loop :for c :in looking
                       :append (list (atty/ui:label " ") (client-glyph c client))))
           (title (atty/ui:row :spacing 0
                               (atty/ui:label (format nil " ~D ~A" n (truncate-string (window-name client session window) 14))
                                              :face (if cursor :cursor :strong))
                               (atty/ui:label (if single
                                                  (format nil " · ~A " (truncate-string (or (getf single :kind) "") 12))
                                                  (format nil " · ~D panes " (length rows)))
                                              :face :quiet)))
           (right (apply #'atty/ui:row :spacing 0
                         (append (when state
                                   (list (atty/ui:label (format nil " ~A ~A" (state-glyph state)
                                                                (row-duration-text (or single worst) now))
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
                   (hints 'board-mode 'switchboard-goto "go" 'switchboard-close-pane "close"
                          'switchboard-name "name" 'switchboard-prompt "prompt"
                          'switchboard-select "pick" 'switchboard-explain "details")
                   (hints 'board-mode 'switchboard-goto "go"))))

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
                             (atty/ui:label (format nil " ~D ~A " n (truncate-string (window-name client session window) 12))
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
                         (loop :for c :in looking :collect (client-label c client))))
            (list (keycap "+" "window" :runs (list :new-window session)) (atty/ui:label " "))))))

(defun board-height (client)
  "The rows the lanes have: under the bar and the header, above the status
and the footer."
  (max 3 (- (client-rows client) (if (client-barp client) 1 0) 3)))

(defun card-size (b client)
  "How wide and tall a card is at this zoom."
  (ecase (board-zoom b)
    (:cards (values +card-width+ +card-height+))
    (:one (values +big-card-width+ (max +card-height+ (- (board-height client) 3))))
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
                           (length (truncate-string (window-name client session w) 12)))))
        (+ 2 (* (length windows) (1+ width))))))

;;; The pulses, rolled up: a pane's cells are the server's; a window is its
;;; panes together, a session its windows, the server its sessions.

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

(defun client-marker (looking client)
  "◆ when this terminal is among LOOKING, else nothing."
  (if (find-if (lambda (c) (this-client-p c client)) looking)
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
                                 (atty/ui:label (truncate-string session (if narrow 8 9)) :face :strong)
                                 (client-marker (clients-on client session) client))
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
                                                    (atty/ui:label (format nil "~D ~A" n (truncate-string (window-name client session w) (if narrow 5 7)))
                                                                   :face (if here :cursor :default)))
                                       (unless narrow (spark (window-cells client session w)))
                                       (atty/ui:row :spacing 0
                                                    (atty/ui:label (state-glyph state) :face (state-face state))
                                                    (client-marker (clients-on client session n) client))
                                       :runs (list :cursor session n))))))))

(defun event-row (event client width)
  "One line of the activity log: when, what kind, which window, what, and
who when somebody did it. A click puts the cursor on that window."
  (destructuring-bind (age clock kind session id window actor text) event
    (declare (ignore age id))
    (let* ((where (format nil "~A›~@[~D~]" (truncate-string session 5) window))
           (typed (eq kind :typed))
           (said (truncate-string (format-event kind text)
                               (max 4 (- width 10 (length where) (if (and actor (not typed)) 2 0))))))
      (bar-button (list :cursor session window)
                  (atty/ui:row :spacing 0
                               (atty/ui:label (format nil " ~A " (format-clock-hm clock)) :face :quiet)
                               (if (and actor typed)
                                   (atty/ui:label (format-actor actor client :short t) :face (actor-face actor client))
                                   (atty/ui:label (event-glyph kind) :face (event-face kind)))
                               (atty/ui:label (format nil " ~A " where) :face :strong)
                               (atty/ui:label said)
                               (if (and actor (not typed))
                                   (atty/ui:label (format nil " ~A" (format-actor actor client :short t)) :face (actor-face actor client))
                                   (atty/ui:label "")))))))

(defun activity-log (b client width rows)
  "The activity log as a well ROWS rows deep, scrolled as far as the board
has it, with its own rail when there is more than fits."
  (let* ((events (client-events client))
         (n (length events))
         (y0 (max 0 (min (board-log-scroll-y b) (max 0 (- n rows)))))
         (shown (subseq events (min y0 n) (min n (+ y0 rows))))
         (railed (> n rows)))
    (setf (board-log-scroll-y b) y0)
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
  (let* ((rows (client-pane-rows client))
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
                                            (client-label c client :pad (if tree-only 0 12))
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
                                                  (atty/ui:label (truncate-string (or (first (getf asks :detail)) (getf asks :question) "") 40) :face :quiet))
                                     (atty/ui:label (truncate-string (or (getf row :doing) "") 60) :face :quiet)))))
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

(defun board-footer-hints (b)
  "The keys that work anywhere on the board, for its footer. The lane's keys
are on the lane's foot and the card's on the card."
  (if (eq :one (board-zoom b))
      (hints 'board-mode "↑↓" "session" 'switchboard-next-blocked "next ▲" 'switchboard-zoom-out "every session"
             'switchboard-filter "filter" 'switchboard-toggle-side "side pane")
      (hints 'board-mode "↑↓" "session" 'switchboard-next-blocked "next ▲" "- +" "zoom"
             'switchboard-filter "filter" 'switchboard-toggle-side "side pane")))

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

(defun scroll-to-cursor (b client sessions)
  "Slide the cursor's lane and the lanes the least it takes for the cursor's
card to be in sight."
  (let ((place (board-place b client)))
    (when place
      (let* ((cw (card-size b client))
             (session (first place))
             (windows (windows-of client session))
             (i (or (position (second place) windows :key #'first) 0))
             (wide (lanes-width b client))
             (room (board-height client))
             (x0 (gethash session (board-offsets b) 0))
             (left (+ 1 (* i (1+ cw))))
             (top (or (cdr (assoc session (lane-tops b client sessions) :test #'equal)) 0))
             (height (lane-height b client session)))
        (unless (eq :overview (board-zoom b))
          (when (< left x0) (setf x0 (max 0 (1- left))))
          (when (> (+ left cw) (+ x0 wide)) (setf x0 (max 0 (- (+ left cw 1) wide))))
          (setf (gethash session (board-offsets b)) x0))
        (when (< top (board-scroll-y b)) (setf (board-scroll-y b) top))
        (when (> (+ top height) (+ (board-scroll-y b) room))
          (setf (board-scroll-y b) (max 0 (- (+ top height) room))))))))

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
                                                             (length sessions) n-windows (length (client-pane-rows client)))
                                                     :face :quiet)
                                      (atty/ui:label (if (plusp asking) (format nil "▲ ~D asking  " asking) "") :face :state-blocked-strong)
                                      (atty/ui:label (if (plusp working) (format nil "◐ ~D working  " working) "") :face :state-working)
                                      (atty/ui:label (if (plusp idle) (format nil "○ ~D idle  " idle) "") :face :state-idle)
                                      (atty/ui:label (if (plusp other) (format nil "· ~D other" other) "") :face :quiet))
                   :right (list (bar-button '(:side) (atty/ui:label "\\ side pane" :face :quiet))
                                (atty/ui:label "  ")
                                (atty/ui:label (ecase (board-zoom b) (:overview "sessions") (:cards "windows") (:one "one session"))
                                               :face :quiet))))))

(defun clamp-scroll (b client sessions room wide)
  "Neither the lanes nor any one lane slid past the end of what there is."
  (let ((tall (loop :for s :in sessions :sum (lane-height b client s))))
    (setf (board-scroll-y b) (max 0 (min (board-scroll-y b) (- tall room)))))
  (multiple-value-bind (cw ch) (card-size b client)
    (declare (ignore ch))
    (dolist (s sessions)
      (let ((strip (if (eq :overview (board-zoom b)) 0 (+ 2 (* (length (windows-of client s)) (1+ cw))))))
        (setf (gethash s (board-offsets b))
              (max 0 (min (gethash s (board-offsets b) 0) (- strip wide))))))))

(defun draw-lanes (b client sessions screen left wide area-top area-bottom)
  "Every lane in view: its header, its strip drawn whole on a sheet of its own
with the part in view copied in, and its foot. Answers the regions drawn,
newest first."
  (let ((regions '()))
    (multiple-value-bind (cw ch) (card-size b client)
      (flet ((header (session y)
               (push (list :tree (draw-in (lane-header b client session) screen left y (+ left wide) (1+ y)))
                     regions))
             (strip (session y x0)
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
             (foot (session y x0 folded)
               ;; the lane's rail, where it is, and its keys
               (let ((ry (+ y (1- (lane-height b client session)))))
                 (when (and (>= ry area-top) (< ry area-bottom))
                   (multiple-value-bind (tree r) (lane-foot b client session x0 wide folded)
                     (draw-in tree screen left ry (+ left wide) (1+ ry))
                     (push (list :tree tree) regions)
                     (push (list :rail-h r session (atty/ui:left r) ry (atty/ui:width r)) regions))))))
        (loop :for (session . lane-top) :in (lane-tops b client sessions)
              :for y := (+ area-top (- lane-top (board-scroll-y b)))
              :for folded := (lane-folded-p b client session)
              :for x0 := (if (eq :overview (board-zoom b)) 0 (gethash session (board-offsets b) 0))
              :when (and (< y area-bottom) (> (+ y (lane-height b client session)) area-top))
                :do (when (>= y area-top) (header session y))
                    (unless folded (strip session y x0))
                    (foot session y x0 folded))))
    regions))

(defmethod draw-overlay ((b board) screen)
  (let* ((client *overlay-client*)
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (top (if (client-barp client) (min 1 (max 0 (1- rows))) 0))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows))
         (regions nil))
    (board-place b client)
    (board-request b client)
    (let* ((sessions (let ((all (board-sessions b client)))
                       (if (eq :one (board-zoom b))
                           (let ((on (first (board-place b client)))) (if on (list on) all))
                           all)))
           (room (board-height client))
           (area-top (1+ top))
           (area-bottom (+ area-top room))
           (left (lanes-left b client))
           (wide (lanes-width b client))
           (side (side-width b client)))
      (when (board-following b)
        (scroll-to-cursor b client sessions)
        (setf (board-following b) nil))
      (clamp-scroll b client sessions room wide)
      (atty/cells:fill-rect m 0 top cols (- rows top) (term:make-face :bg (bar-face :bg)))
      ;; the header
      (push (list :tree (draw-in (board-header b client sessions) screen 0 top cols (1+ top))) regions)
      ;; the side pane and its rule
      (when (plusp side)
        (push (list :tree (draw-in (side-pane b client sessions room) screen 0 area-top side area-bottom)) regions)
        (atty/cells:fill-rect m side area-top 1 room (atty/cells:face-of (atty/ui:label "" :face :card)) #\│))
      ;; the lanes, and the rail they slide under
      (setf regions (append (draw-lanes b client sessions screen left wide area-top area-bottom) regions))
      (let* ((tall (loop :for s :in sessions :sum (lane-height b client s)))
             (r (rail (board-scroll-y b) room (max room tall))))
        (draw-in (atty/ui:row :align :stretch r) screen (1- cols) area-top cols area-bottom)
        (push (list :rail-v r (1- cols) area-top room) regions))
      ;; the status and the footer
      (push (list :tree (draw-in (board-status b client) screen 0 (- rows 2) cols (1- rows))) regions)
      (push (list :tree (draw-in (footer-band (board-footer-hints b)
                                              :right (list (hint "?" "keys" :runs "describe mode")
                                                           (hint "Esc" "close" :runs :close)))
                                 screen 0 (1- rows) cols rows))
            regions)
      (setf (board-regions b) (if (plusp side) (cons (list :side 0 area-top side room) regions) regions)
            (board-area-top b) area-top
            (tty:screen-cursor-visible screen) nil))))

(defun board-request (b client)
  "Every so often, ask for the layouts of every session known and who is
attached: what the board is drawn from beyond the pane rows."
  (when (and (client-wire client) (>= (- (client-ms) (board-requested-at b)) +drawer-poll-interval+))
    (setf (board-requested-at b) (client-ms))
    (let ((*client* client))
      (dolist (s (board-sessions b client))
        (send-if-supported (list :layouts s)))
      (send-if-supported (list :clients))
      (send-if-supported (list :pulses))
      (send-if-supported (list :events 64)))))

(defmethod overlay-ticks-p ((b board)) t)
(defmethod overlay-name ((b board)) "switchboard")
(defmethod close-overlay ((b board) client) (close-board b client))

;;; What a key or a click does.

;;; Moving: across windows, down sessions, through the panes of a window.

;;; The mouse: what was clicked is found in the regions the last drawing
;;; kept, each a tree laid where it was drawn, or a sheet a lane was drawn on
;;; and how far it was slid, or a rail.
