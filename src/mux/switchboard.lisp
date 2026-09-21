;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; The switchboard: every pane in every session at once, to work with them
;;; without going to any of them. A session is a band, and its label goes
;;; there, so choosing a session is this with nothing picked. A pane is a card:
;;; what it is doing in a line, how the last twenty minutes went, its screen,
;;; and who is driving it or what it drives. Several can be picked and given
;;; one prompt; one that is busy gets it when it is next idle.
;;;
;;; Like the queue, it is drawn over the session for whoever opened it, from
;;; what the server keeps this client told.

(defparameter +card-width+ 40
  "The least columns a card is given before another one is put beside it.")

(defparameter +band-label+ 12
  "How wide a band's label is.")

(defparameter +strip-cells+ 30
  "How many cells a card's strip of the last twenty minutes has.")

(defparameter +sorts+
  '((:needs-you "needs you first")
    (:laid-out "as laid out")
    (:recent "most recent change"))
  "The orders a band's cards can be in, and what each is called.")

(defstruct (board (:constructor %make-board))
  (cursor nil)
  (picked nil :type list)
  (query "" :type string)
  (filtering nil)
  (sort 0 :type fixnum)
  (composing nil)
  (closing nil)
  (starting nil)
  (laid nil))

(defun row-key (row) (cons (getf row :session) (getf row :id)))

(defparameter +state-rank+ '(:blocked :working :idle :unknown))

(defun state-rank (row)
  (or (position (row-state row) +state-rank+) (length +state-rank+)))

(defun board-bands (b client)
  "Every session the client has been told of, in the server's order, each with
its cards in the board's order and narrowed by its filter."
  (let* ((now (ms-here))
         (rows (rows-of client))
         (rows (if (plusp (length (board-query b)))
                   (matches (board-query b) rows
                            (lambda (r) (format nil "~A ~@[~A~]" (row-text r) (getf r :doing))))
                   rows))
         (sessions (sort (remove-duplicates (mapcar (lambda (r) (cons (getf r :session)
                                                                      (or (getf r :order) 0)))
                                                    rows)
                                            :test #'equal :key #'car)
                         #'< :key #'cdr)))
    (loop :for (session) :in sessions
          :collect (cons session
                         (let ((in (remove-if-not (lambda (r) (equal session (getf r :session)))
                                                  rows)))
                           (ecase (first (nth (board-sort b) +sorts+))
                             (:needs-you
                              (sort in (lambda (a z)
                                         (or (< (state-rank a) (state-rank z))
                                             (and (= (state-rank a) (state-rank z))
                                                  (> (or (row-for a now) 0)
                                                     (or (row-for z now) 0)))))))
                             (:laid-out (sort in #'< :key (lambda (r) (or (getf r :at) 0))))
                             (:recent (sort in #'< :key (lambda (r) (or (row-for r now) 0))))))))))

(defun board-cards (b client)
  "Every card, band after band, in the order the cursor goes through them."
  (loop :for (nil . rows) :in (board-bands b client) :append rows))

(defun board-row (b client)
  "The row under the cursor, the first card when the cursor is on none."
  (let ((cards (board-cards b client)))
    (or (find (board-cursor b) cards :key #'row-key :test #'equal)
        (first cards))))

(defun board-targets (b client)
  "What a prompt from the composer goes to: what is picked, or the card under
the cursor when nothing is."
  (let ((cards (board-cards b client)))
    (or (remove-if-not (lambda (r) (member (row-key r) (board-picked b) :test #'equal)) cards)
        (let ((it (board-row b client))) (and it (list it))))))

(defun columns-for (cols)
  (max 1 (floor (- cols +band-label+) +card-width+)))

;;; A card.

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

(defun card-foot (client row width)
  "What goes along the bottom of a card: the answers when it is asking, what
waits for it, who drives it or what it drives, or who last typed into it."
  (let ((key (row-key row))
        (now (ms-here)))
    (cond
      ((getf row :asks)
       (let ((options (getf (getf row :asks) :options)))
         (apply #'atty/ui:row :spacing 1
                (loop :for (n) :in options
                      :for text :in (options-fitted options (max 8 (- width 4)))
                      :collect (bar-button (list :option key n)
                                           (atty/ui:row :spacing 0
                                                        (atty/ui:label (format nil " ~D" n)
                                                                       :face :key-number)
                                                        (atty/ui:label (format nil " ~A " text)
                                                                       :face :key)))))))
      ((getf row :queued)
       (atty/ui:label (format nil " queued: ~A " (shortened-to (getf row :queued) 30))
                      :face :state-working))
      ((getf row :driven-by)
       (atty/ui:label (format nil " ⌁ driven by ~A " (getf row :driven-by)) :face :driven))
      ((getf row :drives)
       (atty/ui:label (format nil " drives ~{~A~^ ~} " (getf row :drives)) :face :quiet))
      ((getf row :last-input)
       (destructuring-bind (age who &rest more) (getf row :last-input)
         (declare (ignore more))
         (atty/ui:label (format nil " last input: ~A, ~A "
                                (said-by client who)
                                (duration (+ age (max 0 (- now (getf row :heard-at))))))
                        :face :quiet))))))

(defun row-state (row)
  "What ROW's pane is doing, when that means anything: nil for a program
nobody knows how to read, whose screen moving or not is all there is."
  (and (getf row :known) (getf row :state)))

(defun card-doing (row width)
  "The line at the top of a card, cut to the card: a line that wants more room
than the card has would push the cards beside it off the screen."
  (let ((asks (getf row :asks))
        (room (max 4 (- width 4))))
    (if asks
        (let ((subject (format nil "▲ ~A  " (getf asks :subject))))
          (atty/ui:row :spacing 0
                       (atty/ui:label (shortened-to subject room) :face :state-blocked)
                       (atty/ui:label (shortened-to (or (first (getf asks :detail))
                                                        (getf asks :question) "")
                                                    (max 1 (- room (length subject)))))))
        (let ((state (row-state row)))
          (atty/ui:label (shortened-to (if state
                                           (format nil "~A ~A" (state-glyph state)
                                                   (or (getf row :doing) ""))
                                           (or (getf row :doing) ""))
                                       room)
                         :face (if state (state-face state) :default))))))

(defun card (b client row width)
  (let* ((key (row-key row))
         (now (ms-here))
         (state (row-state row))
         (cursor (equal key (key-of-cursor b client)))
         (picked (member key (board-picked b) :test #'equal))
         (screen (gethash key (client-screens client))))
    (bar-button
     (list :card key)
     (atty/ui:framed
      (atty/ui:column :align :stretch :expand 1
                      (card-doing row width)
                      (if state (strip row now) (atty/ui:label ""))
                      (if screen (screen-view screen) (atty/ui:gap :expand 1)))
      ;; as a pane's frame: what a known agent is doing, the cursor the double line
      :face (state-face state)
      :line (if cursor :double :single)
      :titles (list :tl (atty/ui:row :spacing 0
                                     (if picked
                                         (atty/ui:label " ✓ " :face :number-working)
                                         (atty/ui:label ""))
                                     (atty/ui:label (format nil " ~@[~A ~]~A:~D "
                                                            (and state (state-glyph state))
                                                            (getf row :session) (getf row :id))
                                                    :face (if state (number-face state) :quiet))
                                     (atty/ui:label (format nil " ~A " (getf row :says)))
                                     (atty/ui:label (format nil "~A " (getf row :kind)) :face :quiet))
                    :tr (and state
                             (atty/ui:label (format nil " ~(~A~) ~A " state
                                                    (duration (row-for row now)))
                                            :face (state-face state)))
                    :bl (card-foot client row width)))
     :expand 1)))

(defun key-of-cursor (b client)
  (let ((row (board-row b client))) (and row (row-key row))))

(defun band (b client session rows cols)
  (let* ((per (columns-for cols))
         (width (floor (- cols +band-label+) per))
         (here (equal session (client-session client)))
         (cells (append (mapcar (lambda (r) (card b client r width)) rows)
                        (list (bar-button (list :new-pane session)
                                          (atty/ui:center
                                           (atty/ui:label (format nil "+ new pane in ~A" session)
                                                          :face :quiet))
                                          :expand 1)))))
    (atty/ui:row
     :align :stretch :expand 1 :spacing 0
     (bar-button (list :band session)
                 (atty/ui:column :align :stretch :min-width +band-label+
                                 (atty/ui:label (format nil " ~A" session) :face :accent)
                                 (atty/ui:label (format nil " ~D pane~:P" (length rows)) :face :quiet)
                                 (atty/ui:label (if here " ● here" " go there")
                                                :face (if here :accent :quiet))))
     (apply #'atty/ui:column :align :stretch :expand 1
            (loop :while cells
                  :collect (let ((line (subseq cells 0 (min per (length cells)))))
                             (setf cells (nthcdr per cells))
                             ;; every cell asks for the same width, so the room
                             ;; left over is shared evenly and the cards line up
                             (dolist (cell line)
                               (setf (atty/ui:min-width cell) (max 1 (1- width))))
                             (apply #'atty/ui:row :align :stretch :expand 1 :spacing 0
                                    (append line
                                            (loop :repeat (- per (length line))
                                                  :collect (atty/ui:gap :expand 1
                                                                        :min-width (max 1 (1- width))))))))))))

(defun composer (b client)
  (let ((targets (board-targets b client)))
    (atty/ui:row :spacing 1 :background-color (bar-face :bg-alt)
                 (atty/ui:label (format nil " prompt ~D " (length targets)) :face :number-working)
                 (atty/ui:label (format nil "~{~A~^, ~}"
                                        (mapcar (lambda (r) (format nil "~A:~D ~A" (getf r :session)
                                                                    (getf r :id) (getf r :says)))
                                                targets))
                                :face :quiet)
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
     (atty/ui:label (format nil " filter › ~A_   RET keeps it   Esc drops it" (board-query b))))
    (t
     ;; what order and what filter first: the hints run off a narrow terminal,
     ;; and what the board is showing must not be what is cut
     (atty/ui:row :spacing 0
                  (atty/ui:label (format nil " ~A~@[ · › ~A~] │"
                                         (second (nth (board-sort b) +sorts+))
                                         (and (plusp (length (board-query b)))
                                              (board-query b)))
                                 :face :accent)
                  (hints 'board-mode "←→↑↓" "move" 'switchboard-select "select"
                         'switchboard-go "go there" "1-9" "answer"
                         'switchboard-prompt "prompt" 'switchboard-read "read"
                         'switchboard-name "name" 'switchboard-close-pane "close"
                         'switchboard-filter "filter" 'switchboard-sort "sort"
                         'switchboard-close "close this")))))

(defun board-tree (b client cols)
  (apply #'atty/ui:column :align :stretch :expand 1
         :background-color (bar-face :bg)
         (append
          (let ((bands (board-bands b client)))
            (if bands
                (mapcar (lambda (it) (band b client (car it) (cdr it) cols)) bands)
                (list (atty/ui:center (atty/ui:label "nothing yet" :face :quiet)))))
          (when (board-composing b) (list (composer b client)))
          (list (board-foot b)))))

(defmethod draw-over ((b board) screen)
  (let* ((client *drawing-for*)
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (top (min 1 (max 0 (1- rows))))
         (tree (board-tree b client cols)))
    (unless (board-cursor b)
      (let ((first-here (find (board-starting b) (board-cards b client)
                              :key (lambda (r) (getf r :session)) :test #'equal)))
        (setf (board-cursor b) (if first-here
                                   (row-key first-here)
                                   (key-of-cursor b client)))))
    (atty/cells:fill-rect (atty/cells:make-cells (tty:screen-grid screen) cols rows)
                          0 top cols (- rows top) (term:make-face :bg (bar-face :bg)))
    (atty/cells:draw tree (tty:screen-grid screen) cols rows :top top)
    (setf (board-laid b) tree
          (tty:screen-cursor-visible screen) nil)))

(defmethod ticks-p ((b board)) t)

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

(defun board-moved (by)
  "Move the cursor BY cards; a whole line of them is the width of a band."
  (let ((b (the-board)))
    (when b
      (let* ((cards (board-cards b *client*))
             (at (or (position (key-of-cursor b *client*) cards :key #'row-key :test #'equal) 0))
             (to (max 0 (min (1- (length cards)) (+ at by)))))
        (when cards (setf (board-cursor b) (row-key (nth to cards))))))))

(defcommand (switchboard-left :unlisted) (board-moved -1))
(defcommand (switchboard-right :unlisted) (board-moved 1))
(defcommand (switchboard-up :unlisted) (board-moved (- (columns-for (client-cols *client*)))))
(defcommand (switchboard-down :unlisted) (board-moved (columns-for (client-cols *client*))))

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
                  (getf row :label) (getf row :title)))))

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

(defcommand (switchboard-sort :unlisted)
  (let ((b (the-board)))
    (when b (setf (board-sort b) (mod (1+ (board-sort b)) (length +sorts+))))))

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
  "The innermost button in TREE at LINE, COL: an answer on a card rather than
the card it is on."
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

(defcommand (switchboard-click :unlisted)
  (let* ((b (the-board))
         (hit (and b (board-laid b) *mouse-at*
                   (tag-at (board-laid b) (cdr *mouse-at*) (car *mouse-at*)))))
    (when hit
      (destructuring-bind (what &rest it) (bar-button-runs hit)
        (ecase what
          (:card (setf (board-cursor b) (first it)) (switchboard-go))
          (:option (tell-the-server (list :answer (car (first it)) (cdr (first it)) (second it))))
          (:band (board-close-it b *client*) (tell-the-server (list :go (first it))))
          (:new-pane (tell-the-server (list :split-in (first it)))))))))

(defcommand (switchboard-nothing :unlisted) nil)

(atty/mode:define-key 'board-mode "Left"   #'switchboard-left)
(atty/mode:define-key 'board-mode "Right"  #'switchboard-right)
(atty/mode:define-key 'board-mode "Up"     #'switchboard-up)
(atty/mode:define-key 'board-mode "Down"   #'switchboard-down)
(atty/mode:define-key 'board-mode "SPC"    #'switchboard-select)
(atty/mode:define-key 'board-mode "RET"    #'switchboard-go)
(atty/mode:define-key 'board-mode "p"      #'switchboard-prompt)
(atty/mode:define-key 'board-mode "r"      #'switchboard-read)
(atty/mode:define-key 'board-mode "n"      #'switchboard-name)
(atty/mode:define-key 'board-mode "x"      #'switchboard-close-pane)
(atty/mode:define-key 'board-mode "/"      #'switchboard-filter)
(atty/mode:define-key 'board-mode "s"      #'switchboard-sort)
(atty/mode:define-key 'board-mode "Escape" #'switchboard-close)
(atty/mode:define-key 'board-mode "C-g"    #'switchboard-close)
(atty/mode:define-key 'board-mode "mouse-1" #'switchboard-click)
(atty/mode:define-key 'board-mode "mouse-1-up" #'switchboard-nothing)

(atty/mode:define-key 'board-type-mode "RET"    #'switchboard-typed)
(atty/mode:define-key 'board-type-mode "DEL"    #'switchboard-rub-out)
(atty/mode:define-key 'board-type-mode "Escape" #'switchboard-untyped)
(atty/mode:define-key 'board-type-mode "C-g"    #'switchboard-untyped)

(atty/mode:define-key 'board-confirm-mode "y"      #'switchboard-yes)
(atty/mode:define-key 'board-confirm-mode "n"      #'switchboard-no)
(atty/mode:define-key 'board-confirm-mode "Escape" #'switchboard-no)
(atty/mode:define-key 'board-confirm-mode "C-g"    #'switchboard-no)

(defun open-the-board (client &key session)
  "Put the switchboard over CLIENT's session, the cursor on SESSION's first card
once there are cards, or on the first of all."
  (keep-told client)
  (client-over-put client (%make-board :starting session)))

(defcommand switchboard
  (open-the-board *client*))
