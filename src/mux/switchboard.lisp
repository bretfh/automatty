;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; The switchboard: the whole server at once, as what a multiplexer manages.
;;; The server holds sessions; a session is a row; its windows run right along
;;; the row, each a box; a window's panes sit inside it in their real splits;
;;; and the terminals attached are shown on the window each is looking at.
;;; Down the left, the server, its sessions and its clients. Sessions scroll
;;; down, windows scroll right, and one session can be zoomed to on its own,
;;; its windows big enough to read the last lines of every pane.
;;;
;;; Like the queue, it is drawn over the session for whoever opened it, from
;;; what the server keeps this client told: the pane rows, the layouts of each
;;; session's windows, and who is attached.

(declaim (ftype function short-tty))
(declaim (special +asked-every+))

(defparameter +side-width+ 30
  "How wide the side panel is: the server, the sessions, the clients.")

(defparameter +box-width+ 40
  "How wide a window's box is on the board.")

(defparameter +box-height+ 10
  "How tall a window's box is, its title included.")

(defparameter +row-label+ 12
  "How wide a session row's label is.")

(defparameter +strip-cells+ 30
  "How many cells a strip of the last twenty minutes has.")

(defstruct (board (:constructor %make-board))
  (cursor nil)                          ; (session window pane-id)
  (row0 0 :type fixnum)                 ; the first session row shown
  (col0 0 :type fixnum)                 ; the first window column shown
  (zoom nil)                            ; a session's name, when zoomed to it
  (picked nil :type list)               ; (session . id) keys picked for a prompt
  (query "" :type string)
  (filtering nil)
  (composing nil)
  (closing nil)
  (starting nil)
  (asked 0 :type integer)
  (laid nil))

(defun row-key (row) (cons (getf row :session) (getf row :id)))

(defparameter +state-rank+ '(:blocked :working :idle :unknown))

(defun state-rank (row)
  (or (position (row-state row) +state-rank+) (length +state-rank+)))

(defun row-state (row)
  "What ROW's pane is doing, when that means anything: nil for a program
nobody knows how to read, whose screen moving or not is all there is."
  (and (getf row :known) (getf row :state)))

;;; What the client knows, arranged: sessions in the server's order, each
;;; with its windows, each with its layout of panes.

(defun board-rows (b client)
  (let ((rows (rows-of client)))
    (if (plusp (length (board-query b)))
        (matches (board-query b) rows
                 (lambda (r) (format nil "~A ~@[~A~] ~@[~A~]" (row-text r) (getf r :doing)
                                     (getf r :window-name))))
        rows)))

(defun board-sessions (b client)
  "Every session's name, in the server's order; narrowed to the ones with a
pane the filter matches, when there is one."
  (let ((rows (board-rows b client)))
    (mapcar #'car
            (sort (remove-duplicates (mapcar (lambda (r) (cons (getf r :session) (or (getf r :order) 0)))
                                             rows)
                                     :test #'equal :key #'car)
                  #'< :key #'cdr))))

(defun windows-of (client session)
  "SESSION's windows as (n label tree focus-id shownp), from the layouts the
server said; from the pane rows alone, side by side, when it has not yet."
  (or (gethash session (client-layouts client))
      (let* ((rows (remove-if-not (lambda (r) (equal session (getf r :session))) (rows-of client)))
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

(defun clients-on (client session n)
  "The attached terminals looking at window N of SESSION, this one included."
  (remove-if-not (lambda (c) (and (equal session (fifth c)) (eql n (sixth c))))
                 (client-clients client)))

(defun client-tag (client c)
  (if (eql (first c) (client-id client))
      (atty/ui:label " ⌨ client you " :face :number-driven)
      (atty/ui:label (format nil " ⌨ client ~A " (short-tty (second c) (first c))) :face :driven)))

;;; Where the cursor is.

(defun board-place (b client)
  "The cursor as (session window-n pane-id), put somewhere real when it is on
nothing: the first pane of the first window of the session this began on."
  (let ((cursor (board-cursor b)))
    (or (and cursor
             (pane-row-of client (first cursor) (third cursor))
             cursor)
        (let* ((sessions (board-sessions b client))
               (session (or (find (board-starting b) sessions :test #'equal) (first sessions)))
               (window (and session (first (windows-of client session))))
               (id (and window (first (panes-in-tree (third window))))))
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

;;; A pane inside a window's box.

(defun state-at (history ago)
  "What a pane was AGO milliseconds back, from HISTORY, newest change first."
  (second (find-if (lambda (h) (>= (first h) ago)) history)))

(defun strip (row now)
  "The last twenty minutes of ROW's pane, a cell for every stretch of it,
coloured by what it was then."
  (let* ((late (max 0 (- now (getf row :heard-at))))
         (history (mapcar (lambda (h) (list (+ (first h) late) (second h)))
                          (getf row :history)))
         (step (floor +history-said+ +strip-cells+))
         (states (loop :for i :from (1- +strip-cells+) :downto 0
                       :collect (state-at history (* i step))))
         (runs nil))
    (dolist (s states)
      (if (and runs (eq s (car (first runs))))
          (incf (cdr (first runs)))
          (push (cons s 1) runs)))
    (apply #'atty/ui:row :spacing 0
           (append
            (loop :for (s . n) :in (nreverse runs)
                  :collect (atty/ui:label (make-string n :initial-element #\Space)
                                          :background-color
                                          (bar-face (case s
                                                      (:working :blue)
                                                      (:blocked :yellow)
                                                      (:idle :green)
                                                      (t :bg-alt)))))
            (list (atty/ui:label " 20m" :face :quiet))))))

(defun answer-buttons (row width)
  (let ((options (getf (getf row :asks) :options)))
    (apply #'atty/ui:row :spacing 1
           (loop :for (n) :in options
                 :for text :in (options-fitted options (max 8 (- width 4)))
                 :collect (bar-button (list :option (row-key row) n)
                                      (atty/ui:row :spacing 0
                                                   (atty/ui:label (format nil " ~D" n) :face :key-number)
                                                   (atty/ui:label (format nil " ~A " text) :face :key)))))))

(defun pane-box (b client row width &key big)
  "One pane as it sits in its window: its command, its name, its state, what
it asks; BIG, its last while and its screen too. The whole of it is a button
that puts the cursor on it."
  (let* ((key (row-key row))
         (now (ms-here))
         (state (row-state row))
         (asks (getf row :asks))
         (place (board-place b client))
         (cursor (and place (equal key (cons (first place) (third place)))))
         (picked (member key (board-picked b) :test #'equal))
         (screen (and big (gethash key (client-screens client))))
         (room (max 4 (- width 4))))
    (bar-button
     (list :pane key)
     (atty/ui:framed
      (apply #'atty/ui:column :align :stretch :expand 1
             (remove nil
                     (list (atty/ui:label (format nil "> ~A" (shortened-to (or (getf row :command) (getf row :kind) "") room))
                                          :face :quiet)
                           (and asks
                                (atty/ui:row :spacing 0
                                             (atty/ui:label (shortened-to (format nil "▲ asks ~A " (getf asks :subject)) room)
                                                            :face :state-blocked-strong)
                                             (atty/ui:label (shortened-to (or (first (getf asks :detail))
                                                                              (getf asks :question) "")
                                                                          (max 1 (- room 8 (length (or (getf asks :subject) ""))))))))
                           (and (not asks) (getf row :doing) state
                                (atty/ui:label (shortened-to (getf row :doing) room) :face :strong))
                           (and big state (strip row now))
                           (and big (if screen (screen-view screen) (atty/ui:gap :expand 1)))
                           (and big asks (answer-buttons row width)))))
      :face (cond (cursor :strong) ((eq state :blocked) :state-blocked) (t (state-face state)))
      :line (if cursor :double :single)
      :titles (list :tl (atty/ui:row :spacing 0
                                     (if picked (atty/ui:label " ✓ " :face :number-working) (atty/ui:label ""))
                                     (atty/ui:label (format nil " ~D " (1+ (or (getf row :at) 0)))
                                                    :face (if (and state (not cursor)) (number-face state) :strong))
                                     (atty/ui:label (format nil " ~A " (getf row :says)) :face :strong))
                    :tr (and state
                             (atty/ui:label (format nil " ~A ~A " (state-glyph state) (duration (row-for row now)))
                                            :face (if (eq state :blocked) :state-blocked-strong (state-face state))))))
     :expand 1)))

(defun tree-box (b client session tree width height &key big)
  "A window's layout as boxes: a pane is one, a split a row or column of them."
  (cond
    ((null tree) (atty/ui:gap :expand 1))
    ((consp tree)
     (let* ((parts (rest tree))
            (n (max 1 (length parts)))
            (across (eq (first tree) :across)))
       (apply (if across #'atty/ui:row #'atty/ui:column)
              :align :stretch :expand 1 :spacing 0
              (mapcar (lambda (part)
                        (tree-box b client session part
                                  (if across (floor width n) width)
                                  (if across height (floor height n))
                                  :big big))
                      parts))))
    (t (let ((row (pane-row-of client session tree)))
         (if row
             (pane-box b client row width :big big)
             (atty/ui:gap :expand 1))))))

(defun window-box (b client session window width height &key big)
  "One window: a box titled with its number and name, the clients looking at
it, and its panes in their splits. Double where the cursor is, yellow when a
pane in it asks."
  (destructuring-bind (n label tree focus shownp) window
    (declare (ignore focus))
    (let* ((place (board-place b client))
           (cursor (and place (equal session (first place)) (eql n (second place))))
           (worst (window-worst-row client session window))
           (asking (and worst (eq (row-state worst) :blocked)))
           (tags (mapcar (lambda (c) (client-tag client c)) (clients-on client session n))))
      (atty/ui:framed
       (tree-box b client session tree (- width 2) (- height 2) :big big)
       :face (cond (cursor :strong) (asking :state-blocked) (t :state-unknown))
       :line (if cursor :double :single)
       :min-width width :min-height height
       :titles (list :tl (atty/ui:row :spacing 0
                                      (atty/ui:label " window " :face :quiet)
                                      (atty/ui:label (format nil "~D~@[ ~A~] " n label)
                                                     :face (if cursor :number-unknown :strong))
                                      (atty/ui:label (if shownp " ● " "") :face :accent)
                                      (atty/ui:label (if asking " ▲ " "") :face :state-blocked-strong))
                     :tr (and tags (apply #'atty/ui:row :spacing 0 tags)))))))

;;; A session's row: its label, then its windows from the first column shown.

(defun session-label (b client session)
  (let* ((rows (remove-if-not (lambda (r) (equal session (getf r :session))) (rows-of client)))
         (windows (windows-of client session))
         (here (equal session (client-session client)))
         (blocked (count :blocked rows :key #'row-state))
         (working (count :working rows :key #'row-state))
         (idle (count :idle rows :key #'row-state))
         (clients (remove-if-not (lambda (c) (equal session (fifth c))) (client-clients client)))
         (place (board-place b client))
         (on (and place (equal session (first place)))))
    (bar-button (list :session session)
                (atty/ui:column :align :stretch :min-width +row-label+
                                (atty/ui:label " session" :face :quiet)
                                (atty/ui:label (format nil " ~A" session) :face (if on :strong-accent :strong))
                                (atty/ui:label (format nil " ~D window~:P" (length windows)) :face :quiet)
                                (atty/ui:row :spacing 0
                                             (atty/ui:label (if (plusp blocked) (format nil " ▲ ~D" blocked) "")
                                                            :face :state-blocked-strong)
                                             (atty/ui:label (if (plusp working) (format nil " ◐ ~D" working) "")
                                                            :face :state-working)
                                             (atty/ui:label (if (plusp idle) (format nil " ○ ~D" idle) "")
                                                            :face :state-idle))
                                (atty/ui:label (if clients (format nil " ⌨ ~D client~:P" (length clients)) "")
                                               :face :driven)
                                (atty/ui:label (if here " ● here" " go there") :face (if here :accent :quiet))))))

(defun session-row (b client session cols &key big)
  (let* ((windows (windows-of client session))
         (width (if big (max +box-width+ (floor (- cols +side-width+ +row-label+ 2) 2)) +box-width+))
         (height (if big (max +box-height+ (- (client-rows client) 8)) +box-height+))
         (per (max 1 (floor (- cols +side-width+ +row-label+ 2) width)))
         (from (if big 0 (min (board-col0 b) (max 0 (- (length windows) per)))))
         (shown (subseq windows from (min (length windows) (+ from per))))
         (boxes (mapcar (lambda (w) (window-box b client session w width height :big big)) shown)))
    (atty/ui:row
     :align :stretch :spacing 0
     (session-label b client session)
     (apply #'atty/ui:row :align :start :spacing 0
            (append boxes
                    (when (< (+ from (length shown)) (length windows))
                      (list (atty/ui:label " ▸" :face :quiet)))
                    (when (or big (< (length boxes) per))
                      (list (bar-button (list :new-window session)
                                        (atty/ui:label "  + window " :face :quiet)))))))))

;;; The side panel: the server, its sessions, its clients.

(defun side-panel (client sessions shown)
  (let* ((rows (rows-of client))
         (windows (loop :for s :in sessions :sum (length (windows-of client s))))
         (blocked (count :blocked rows :key #'row-state))
         (clients (client-clients client)))
    (apply #'atty/ui:column :align :stretch :min-width +side-width+
           :background-color (bar-face :bg-dim)
           (append
            (list (atty/ui:label " ◆ server" :face :strong)
                  (atty/ui:label (format nil "   ~D session~:P · ~D window~:P · ~D pane~:P"
                                         (length sessions) windows (length rows))
                                 :face :quiet)
                  (atty/ui:label (if (plusp blocked) (format nil "   ▲ ~D need~:[~;s~] you" blocked (= blocked 1)) "")
                                 :face :state-blocked-strong)
                  (atty/ui:label "")
                  (atty/ui:label " SESSIONS" :face :quiet))
            (loop :for s :in sessions
                  :collect (let* ((in (remove-if-not (lambda (r) (equal s (getf r :session))) rows))
                                  (on (member s shown :test #'equal))
                                  (b-count (count :blocked in :key #'row-state))
                                  (here (remove-if-not (lambda (c) (equal s (fifth c))) clients)))
                             (bar-button (list :session s)
                                         (atty/ui:row :spacing 0
                                                      (atty/ui:label (if on "▌" " ") :face :driven)
                                                      (atty/ui:label (format nil " ~A" s) :face (if on :strong :quiet))
                                                      (atty/ui:label (format nil " ~Dw" (length (windows-of client s))) :face :quiet)
                                                      (atty/ui:gap)
                                                      (atty/ui:label (if (plusp b-count) (format nil "▲~D " b-count) "")
                                                                     :face :state-blocked-strong)
                                                      (atty/ui:label (if here (format nil "~{~A~}" (loop :repeat (length here) :collect "⌨")) "")
                                                                     :face :driven)
                                                      (atty/ui:label " ")))))
            (list (atty/ui:label "")
                  (atty/ui:label " CLIENTS · attached terminals" :face :quiet))
            (if clients
                (loop :for c :in clients
                      :append (list (atty/ui:row :spacing 0 (atty/ui:label " ") (client-tag client c)
                                                 (atty/ui:label (format nil " ~Dx~D" (fourth c) (third c)) :face :quiet))
                                    (atty/ui:label (format nil "     sees ~A~@[ › ~D~]" (or (fifth c) "nothing") (sixth c))
                                                   :face :quiet)))
                (list (atty/ui:label "   nobody" :face :quiet)))
            (list (atty/ui:gap :expand 1))))))

;;; The foot: the full path of what the cursor is on, its question and answers.

(defun board-status (b client)
  (let* ((row (board-row b client))
         (place (board-place b client))
         (window (and place (find (second place) (windows-of client (first place)) :key #'first)))
         (asks (and row (getf row :asks))))
    (if (null row)
        (atty/ui:label " nothing yet" :face :quiet)
        (atty/ui:row :spacing 0 :background-color (bar-face :bg-alt)
                     (atty/ui:label " server " :face :quiet)
                     (atty/ui:label "default" :face :strong)
                     (atty/ui:label " › session " :face :quiet)
                     (atty/ui:label (first place) :face :strong)
                     (atty/ui:label " › window " :face :quiet)
                     (atty/ui:label (format nil "~D~@[ ~A~]" (second place)
                                            (or (and window (second window)) (getf row :window-name)))
                                    :face :strong)
                     (atty/ui:label " › pane " :face :quiet)
                     (atty/ui:label (format nil "~A " (getf row :says)) :face :strong)
                     (atty/ui:label (format nil "> ~A" (or (getf row :command) (getf row :kind) "")) :face :quiet)
                     (atty/ui:gap)
                     (if asks
                         (atty/ui:row :spacing 1
                                      (atty/ui:label (format nil " ▲ asks ~A" (getf asks :subject)) :face :state-blocked-strong)
                                      (answer-buttons row 60))
                         (atty/ui:label ""))))))

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

(defun board-foot (b)
  (cond
    ((board-closing b)
     (atty/ui:label (format nil " close ~A:~D and what runs in it?  y / n"
                            (car (board-closing b)) (cdr (board-closing b)))
                    :face :warning))
    ((board-filtering b)
     (atty/ui:label (format nil " find › ~A_   RET keeps it   Esc drops it" (board-query b))))
    (t
     (atty/ui:row :spacing 0 :background-color (bar-face :bg-dim)
                  (atty/ui:label (format nil "~@[ › ~A │~]" (and (plusp (length (board-query b))) (board-query b)))
                                 :face :accent)
                  (hints 'board-mode "←→" "window" "↑↓" "session" 'switchboard-next-pane "pane"
                         'switchboard-go "go there" "1-9" "answer"
                         'switchboard-zoom "zoom" 'switchboard-select "pick" 'switchboard-prompt "prompt"
                         'switchboard-filter "find" 'switchboard-close "close")
                  (atty/ui:gap)))))

(defun rows-shown (b client sessions)
  "How many session rows fit, and which they are from the first shown."
  (let* ((per (max 1 (floor (- (client-rows client) 5) (+ +box-height+ 1))))
         (from (min (board-row0 b) (max 0 (- (length sessions) per)))))
    (values (subseq sessions from (min (length sessions) (+ from per))) from per)))

(defun scrollbar-column (from shown total height)
  "A column of cells saying where FROM..FROM+SHOWN of TOTAL is."
  (let* ((thumb (max 1 (round (* height (min 1 (/ shown (max 1 total)))))))
         (top (min (- height thumb) (round (* height (/ from (max 1 total)))))))
    (apply #'atty/ui:column :spacing 0
           (loop :for y :below height
                 :collect (atty/ui:label (if (and (<= top y) (< y (+ top thumb))) "█" "░")
                                         :face (if (and (<= top y) (< y (+ top thumb))) :scroll-thumb :scroll-track))))))

(defun board-tree (b client cols)
  (let* ((sessions (board-sessions b client))
         (zoom (and (board-zoom b) (member (board-zoom b) sessions :test #'equal) (board-zoom b))))
    (multiple-value-bind (shown from per) (rows-shown b client sessions)
      (declare (ignore per))
      (let* ((rows (if zoom (list zoom) shown))
             (widest (loop :for s :in sessions :maximize (length (windows-of client s))))
             (height (max 1 (- (client-rows client) 4))))
        (atty/ui:column
         :align :stretch :expand 1
         :background-color (bar-face :bg)
         (atty/ui:row
          :align :stretch :expand 1 :spacing 0
          (side-panel client sessions rows)
          (atty/ui:label "│" :face :quiet)
          (apply #'atty/ui:column :align :stretch :expand 1 :spacing 0
                 (append
                  (list (atty/ui:row :spacing 0 :background-color (bar-face :bg-dim)
                                     (atty/ui:label (format nil " ~A" (if zoom "zoomed ↓" "sessions ↓")) :face :quiet
                                                    :min-width +row-label+)
                                     (apply #'atty/ui:row :spacing 0
                                            (loop :for n :from (1+ (board-col0 b)) :to widest
                                                  :collect (atty/ui:label (format nil "window ~D" n) :face :quiet
                                                                          :min-width +box-width+)))
                                     (atty/ui:gap)))
                  (if zoom
                      (list (session-row b client zoom cols :big t))
                      (if rows
                          (loop :for s :in rows
                                :append (list (session-row b client s cols)
                                              (atty/ui:rule :face :state-unknown)))
                          (list (atty/ui:center (atty/ui:label "nothing yet" :face :quiet)))))
                  (list (atty/ui:gap :expand 1))))
          (if zoom
              (atty/ui:label "")
              (scrollbar-column from (length shown) (length sessions) height)))
         (if (board-composing b) (composer b client) (atty/ui:label ""))
         (board-status b client)
         (board-foot b))))))

(defmethod draw-over ((b board) screen)
  (let* ((client *drawing-for*)
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (top (min 1 (max 0 (1- rows)))))
    (board-place b client)
    (board-ask b client)
    (let ((tree (board-tree b client cols)))
      (atty/cells:fill-rect (atty/cells:make-cells (tty:screen-grid screen) cols rows)
                            0 top cols (- rows top) (term:make-face :bg (bar-face :bg)))
      (atty/cells:draw tree (tty:screen-grid screen) cols rows :top top)
      (setf (board-laid b) tree
            (tty:screen-cursor-visible screen) nil))))

(defun board-ask (b client)
  "Every so often, ask for the layouts of every session known and who is
attached: what the board is drawn from beyond the pane rows."
  (when (and (client-wire client) (>= (- (ms-here) (board-asked b)) +asked-every+))
    (setf (board-asked b) (ms-here))
    (let ((*client* client))
      (dolist (s (board-sessions b client))
        (tell-the-server-if-it-knows (list :layouts s)))
      (tell-the-server-if-it-knows (list :clients)))))

(defmethod ticks-p ((b board)) t)
(defmethod over-name ((b board)) "switchboard")
(defmethod close-over ((b board) client) (board-close-it b client))

;;; What a key or a click does.

(atty/mode:define-mode board-mode ())
(atty/mode:define-mode board-type-mode ()
  (:documentation "Typing a filter or a prompt: every letter is what is typed."))
(atty/mode:define-mode board-confirm-mode ())

(defmethod mode-of ((b board))
  (cond ((board-closing b) 'board-confirm-mode)
        ((or (board-filtering b) (board-composing b)) 'board-type-mode)
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
  "The cursor onto window N of SESSION, on pane ID or its first; the rows and
columns shown follow it."
  (let* ((windows (windows-of client session))
         (window (or (find n windows :key #'first) (first windows)))
         (sessions (board-sessions b client))
         (at (position session sessions :test #'equal)))
    (when window
      (let ((id (or (and id (member id (panes-in-tree (third window))) id)
                    (first (panes-in-tree (third window))))))
        (setf (board-cursor b) (list session (first window) id)))
      (multiple-value-bind (shown from per) (rows-shown b client sessions)
        (declare (ignore shown))
        (when (and at (< at from)) (setf (board-row0 b) at))
        (when (and at (>= at (+ from per))) (setf (board-row0 b) (1+ (- at per)))))
      (let ((per (max 1 (floor (- (client-cols client) +side-width+ +row-label+ 2) +box-width+)))
            (col (1- (first window))))
        (when (< col (board-col0 b)) (setf (board-col0 b) col))
        (when (>= col (+ (board-col0 b) per)) (setf (board-col0 b) (1+ (- col per))))))
    (setf (client-dirty client) t)))

(defcommand (switchboard-left :unlisted)
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place (board-go-to b *client* (first place) (max 1 (1- (second place)))))))

(defcommand (switchboard-right :unlisted)
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when place (board-go-to b *client* (first place) (1+ (second place))))))

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
  (let* ((b (the-board)) (row (and b (board-row b *client*))))
    (when b
      (board-close-it b *client*)
      (when row (tell-the-server (list :focus-pane (getf row :session) (getf row :id)))))))

(defcommand (switchboard-zoom :unlisted)
  "one session on its own, big enough to read; again, the whole server"
  (let* ((b (the-board)) (place (and b (board-place b *client*))))
    (when b
      (setf (board-zoom b) (if (board-zoom b) nil (and place (first place)))
            (client-dirty *client*) t))))

(defcommand (switchboard-clients :unlisted)
  (let ((b (the-board)))
    (when b (board-close-it b *client*))
    (run-command "clients" *client*)))

(defcommand (switchboard-prompt :unlisted)
  (let ((b (the-board)))
    (when (and b (board-targets b *client*))
      (setf (board-composing b) "")
      (client-in-mode *client*))))

(defcommand (switchboard-read :unlisted)
  (let* ((b (the-board)) (row (and b (board-row b *client*))))
    (when row (tell-the-server (list :pane-read (getf row :session) (getf row :id))))))

(defcommand (switchboard-name :unlisted)
  (let* ((b (the-board)) (row (and b (board-row b *client*))))
    (when row
      (ask-a-name *client* (getf row :session) (getf row :id)
                  (getf row :label) (getf row :title) :address (row-address row)))))

(defcommand (switchboard-close-pane :unlisted)
  (let* ((b (the-board)) (key (and b (key-of-cursor b *client*))))
    (when key
      (setf (board-closing b) key)
      (client-in-mode *client*))))

(defcommand (switchboard-filter :unlisted)
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

(defcommand (switchboard-yes :unlisted)
  (let ((b (the-board)))
    (when (and b (board-closing b))
      (tell-the-server (list :close-pane (car (board-closing b)) (cdr (board-closing b))))
      (setf (board-closing b) nil)
      (client-in-mode *client*))))

(defcommand (switchboard-no :unlisted)
  (let ((b (the-board)))
    (when b
      (setf (board-closing b) nil)
      (client-in-mode *client*))))

(defun tag-at (tree line col)
  "The innermost button in TREE at LINE, COL: an answer on a pane rather than
the pane, a pane rather than its window."
  (button-at tree line col))

(defcommand (switchboard-click :unlisted)
  "a click on a pane puts the cursor on it, and on the one the cursor is on
goes there; on an answer, answers; on a session, goes; on + window, makes one"
  (let* ((b (the-board))
         (hit (and b (board-laid b) *mouse-at*
                   (tag-at (board-laid b) (cdr *mouse-at*) (car *mouse-at*)))))
    (when hit
      (destructuring-bind (what &rest it) (bar-button-runs hit)
        (ecase what
          (:pane
           (let* ((key (first it))
                  (row (pane-row-of *client* (car key) (cdr key)))
                  (was (key-of-cursor b *client*)))
             (when row
               (if (equal key was)
                   (switchboard-go)
                   (board-go-to b *client* (car key) (getf row :window) (cdr key))))))
          (:option (tell-the-server (list :answer (car (first it)) (cdr (first it)) (second it))))
          (:session
           (let ((s (first it)))
             (if (equal s (first (board-place b *client*)))
                 (progn (board-close-it b *client*) (tell-the-server (list :go s)))
                 (board-go-to b *client* s 1))))
          (:new-window (tell-the-server (list :new-window (first it)))))))))

(defcommand (switchboard-nothing :unlisted) nil)
(defcommand (switchboard-wheel-up :unlisted) (switchboard-session-by -1))
(defcommand (switchboard-wheel-down :unlisted) (switchboard-session-by 1))

(atty/mode:define-key 'board-mode "Left"   #'switchboard-left)
(atty/mode:define-key 'board-mode "Right"  #'switchboard-right)
(atty/mode:define-key 'board-mode "Up"     #'switchboard-up)
(atty/mode:define-key 'board-mode "Down"   #'switchboard-down)
(atty/mode:define-key 'board-mode "TAB"    #'switchboard-next-pane)
(atty/mode:define-key 'board-mode "SPC"    #'switchboard-select)
(atty/mode:define-key 'board-mode "RET"    #'switchboard-go)
(atty/mode:define-key 'board-mode "+"      #'switchboard-zoom)
(atty/mode:define-key 'board-mode "-"      #'switchboard-zoom)
(atty/mode:define-key 'board-mode "c"      #'switchboard-clients)
(atty/mode:define-key 'board-mode "p"      #'switchboard-prompt)
(atty/mode:define-key 'board-mode "r"      #'switchboard-read)
(atty/mode:define-key 'board-mode "n"      #'switchboard-name)
(atty/mode:define-key 'board-mode "x"      #'switchboard-close-pane)
(atty/mode:define-key 'board-mode "/"      #'switchboard-filter)
(atty/mode:define-key 'board-mode "Escape" #'switchboard-close)
(atty/mode:define-key 'board-mode "C-g"    #'switchboard-close)
(atty/mode:define-key 'board-mode "mouse-1" #'switchboard-click)
(atty/mode:define-key 'board-mode "mouse-1-up" #'switchboard-nothing)
(atty/mode:define-key 'board-mode "wheel-up" #'switchboard-wheel-up)
(atty/mode:define-key 'board-mode "wheel-down" #'switchboard-wheel-down)

(atty/mode:define-key 'board-type-mode "RET"    #'switchboard-typed)
(atty/mode:define-key 'board-type-mode "DEL"    #'switchboard-rub-out)
(atty/mode:define-key 'board-type-mode "Escape" #'switchboard-untyped)
(atty/mode:define-key 'board-type-mode "C-g"    #'switchboard-untyped)

(atty/mode:define-key 'board-confirm-mode "y"      #'switchboard-yes)
(atty/mode:define-key 'board-confirm-mode "n"      #'switchboard-no)
(atty/mode:define-key 'board-confirm-mode "Escape" #'switchboard-no)
(atty/mode:define-key 'board-confirm-mode "C-g"    #'switchboard-no)

(defun open-the-board (client &key (session (client-session client)))
  "Put the switchboard over CLIENT's session, the cursor on SESSION's first
pane once there are rows: the one the client is on unless another is asked."
  (keep-told client)
  (client-over-put client (%make-board :starting session)))

(defcommand (switchboard :group sessions)
  "the whole server: every session, window and pane, and who is looking"
  (open-the-board *client*))
