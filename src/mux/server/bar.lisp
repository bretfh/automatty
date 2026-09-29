;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; The bar is what the session looks like rather than what any one person is
;;; doing, so the server composes it and it crosses the wire as cells like
;;; everything else. Everyone attached sees the same one.

(defun field-slot (session)
  "The bar's own way in: one field, styled like a search bar and a shade
deeper than the bar it sits on, and one button naming what it currently opens.
Clicking the button cycles it through +PROMPT-TOGGLES+; clicking the field
runs whichever of them is current."
  (let* ((kind (nth (mod (session-field-kind session) (length +palette-prefixes+))
                    +palette-prefixes+))
         (prefix (car kind)) (runs (cdr kind)))
    (pill (atty/ui:row
           :spacing 0
           (bar-button :cycle-search-kind (atty/ui:label (format nil " ~C " prefix) :face :brand))
           (bar-button runs (atty/ui:label (format nil " ~A " runs)))))))

(defun pane-display-name (pane)
  "What to call PANE: the name somebody gave it, else the title its program
gave itself, else the program."
  (or (pane-label pane)
      (and (pane-named pane) (plusp (length (pane-named pane))) (pane-named pane))
      (program-display-name (pane-command pane))
      ""))

(defparameter +state-severity+ '(:blocked :working :idle :unknown)
  "The states, the one that most wants somebody first.")

(defun worst-pane (session)
  "The pane in SESSION that most wants somebody."
  (first (sort (copy-list (session-panes session)) #'<
               :key (lambda (p) (or (and (pane-known-p p)
                                         (position (agent:agent-state (pane-agent p)) +state-severity+))
                                    (length +state-severity+))))))

(defun blocked-count (server)
  (loop :for s :in (server-sessions server)
        :sum (count :blocked (session-panes s)
                    :key (lambda (p) (agent:agent-state (pane-agent p))))))

(defparameter +narrow-bar+ 80
  "A terminal narrower than this gets a bar without names: the glyphs and the
counts say what the names would, in the room there is.")

(defparameter +bar-left-min+ 28
  "The least room the left of the bar keeps for the session and its windows:
the brand, the name, the rule, one chip and the plus. The right of the bar is
folded up until the left has at least this.")

(defparameter +sessions-shown+ 4
  "How many other sessions the bar names before folding the rest to a count.")

;;; The bar is laid out in slots of set width, so nothing on it moves when
;;; what is in a slot changes: a state word growing, a chip lighting up, a
;;; count going from 9 to 10. What is longer than its slot is cut, what is
;;; shorter is padded, and a slot with nothing to say is still there, blank.

(defparameter +chip-width+ 14 "A window's chip: its number and name, and one glyph.")
(defparameter +focus-width+ 14 "What the shown window's one pane is doing, when it has one and no frame.")
(defparameter +mode-width+ 16 "Zoomed, or reading back, or nothing, and its ends.")
(defparameter +needs-width+ 21 "What needs you anywhere, or nothing.")
(defparameter +session-width+ 8 "One other session: its name and its worst pane's glyph.")
(defparameter +clients-width+ 14 "Who else is attached, or nothing.")
(defparameter +menu-width+ 9 "The one key to learn, always in the same place.")

(defun slot (text width &key (face :default) background)
  "TEXT in a slot exactly WIDTH wide: cut when longer, padded when shorter."
  (apply #'atty/ui:label (format nil "~vA" width (truncate-string (or text "") width))
         :face face
         (and background (list :background-color background))))

(defun window-worst (window)
  "The state of the pane in WINDOW that most wants somebody, or nil when none
of them is a known agent."
  (let ((known (remove-if-not #'pane-known-p (window-panes window))))
    (and known
         (agent:agent-state
          (pane-agent (first (sort (copy-list known) #'<
                                   :key (lambda (p) (position (agent:agent-state (pane-agent p))
                                                              +state-severity+)))))))))

(defun window-chip (session window &key narrow)
  "A window on the bar, in a slot: its number and name, and one glyph for the
pane in it that most wants somebody; yellow with a count when any is asking.
The one shown is a pill on the ground of the panes. A click shows it."
  (let* ((n (window-number session window))
         (panes (window-panes window))
         (asking (count :blocked panes :key (lambda (p) (agent:agent-state (pane-agent p)))))
         (worst (window-worst window))
         (shown (eq window (session-window session)))
         ;; an unnamed window goes by its first pane, which does not change
         ;; as the focus moves about in it, and by what somebody called it or
         ;; what it runs, never the title its program set
         (first-pane (first panes))
         (name (or (window-label window)
                   (and first-pane (pane-label first-pane))
                   (and first-pane (program-display-name (pane-command first-pane)))
                   ""))
         (width (if narrow 4 +chip-width+))
         (glyph (if (plusp asking) 4 2))
         (runs (lambda () (session-select-window session n)))
         (chip (if narrow
                   (if (plusp asking)
                       (slot (format nil " ~D▲" n) width :face :state-blocked-strong)
                       (atty/ui:row :spacing 0
                                    (slot (format nil " ~D" n) (- width 2) :face :strong)
                                    (slot (if worst (state-glyph worst) "") 2
                                          :face (if worst (state-face worst) :quiet))))
                   (atty/ui:row :spacing 0
                                (slot (format nil " ~D ~A" n (truncate-string name (- width glyph 3)))
                                      (- width glyph)
                                      :face (if shown :strong :default))
                                (if (plusp asking)
                                    (slot (format nil "▲~D " asking) glyph :face :state-blocked-strong)
                                    (slot (if worst (state-glyph worst) "") glyph
                                          :face (if worst (state-face worst) :quiet)))))))
    (bar-button runs (if shown (pill chip :ground :bg) chip))))

(defun plus-chip (session)
  "The way to another window, by mouse."
  (bar-button (lambda () (session-add-window session))
              (atty/ui:label " + " :face :quiet)))

(defun focus-slot (session now &key narrow)
  "What the shown window's one pane is doing, when it has one and there is no
frame to say it: the glyph, the state and how long, in a slot that is there
either way."
  (let* ((window (session-window session))
         (panes (window-panes window))
         (pane (and (null (rest panes)) (first panes)))
         (state (and pane (pane-known-p pane) (agent:agent-state (pane-agent pane))))
         (width (if narrow 10 +focus-width+)))
    (if state
        (slot (if narrow
                  (format nil " ~(~A~)" state)
                  (format nil " ~A ~(~A~) ~4A" (state-glyph state) state
                          (format-duration (agent:agent-for (pane-agent pane) now))))
              width :face (state-face state))
        (slot "" width))))

(defun mode-slot (session)
  "Zoomed, or somebody reading back, or nothing, in one slot: a click on
reading back, from whoever is, is back to live."
  (let ((zoomed (session-zoomed session)))
    (cond
      (zoomed
       (pill (slot (format nil " ⤢ ~A zoomed" (pane-display-name zoomed)) (- +mode-width+ 2) :face :strong)))
      ((session-readers session)
       (bar-button "exit scroll mode"
                   (pill (slot " reading back" (- +mode-width+ 2) :face :chip-scrolled) :ground :yellow)))
      (t (slot "" +mode-width+)))))

(defun queue-slot (server &key narrow)
  (let ((n (blocked-count server))
        (width (if narrow 6 +needs-width+)))
    (if (plusp n)
        (bar-button "show queue"
                    (atty/ui:row :spacing 0
                                 (pill (slot (if narrow (format nil " ▲ ~D" n)
                                                 (format nil " ▲ ~D need~A you" n (if (= n 1) "s" "")))
                                             (if narrow (- width 2) (- width 8))
                                             :face :chip-blocked)
                                       :ground :yellow)
                                 (if narrow
                                     (atty/ui:label "")
                                     (slot (let ((key (command-key 'show-queue))) (if key (format nil " ~A" key) ""))
                                           6 :face :quiet))))
        (slot "" width))))

(defun menu-slot ()
  "The one key to learn: the prefix, and that a menu follows it, in a slot of
its own so it is always in the same place."
  (bar-button "show menu"
              (slot (format nil " ~A menu" (prefix-string)) +menu-width+ :face :quiet)))

(defun clients-slot (session &key narrow)
  "Who else is attached to this session, when anybody is: the bar is
everybody's, so it names them all."
  (let ((here (remove-if-not #'watcher-interactive (session-watchers session)))
        (width (if narrow 4 +clients-width+)))
    (if (rest here)
        (bar-button "clients"
                    (slot (if narrow
                              (format nil " ⌨~D" (length here))
                              (format nil " ⌨ ~{~A~^,~}"
                                      (mapcar (lambda (w) (format nil "~A~:[~;←~]" (format-tty (watcher-tty w) (watcher-id w))
                                                                  (watcher-following w)))
                                              here)))
                          width :face :driven))
        (slot "" width))))

(defparameter +session-narrow-width+ 6
  "One other session on a narrow bar: three letters of its name and its glyph.")

(defun other-sessions-folded (session &key narrow (most +sessions-shown+))
  "Every other session in a slot of its own, the first MOST by name, the rest
as how many more; a click goes to the pane there that most wants somebody.
NARROW keeps three letters of each name."
  (let* ((server (session-server session))
         (others (and server (remove session (server-sessions server))))
         (shown (subseq others 0 (min (length others) most)))
         (rest (- (length others) (length shown)))
         (width (if narrow +session-narrow-width+ +session-width+)))
    (append
     (loop :for s :in shown
           :for worst := (worst-pane s)
           :collect (let ((state (and worst (pane-known-p worst) (agent:agent-state (pane-agent worst)))))
                      (bar-button (let ((s s) (worst worst))
                                    (if worst
                                        (lambda () (focus-pane (session-server s) *client* (session-name s) (pane-id worst)))
                                        (lambda () (go-to *client* (session-name s)))))
                                  (atty/ui:row :spacing 0
                                               (slot (format nil " ~A" (if narrow
                                                                           (subseq (session-name s) 0 (min 3 (length (session-name s))))
                                                                           (session-name s)))
                                                     (- width 2) :face :quiet)
                                               (slot (format nil "~A " (state-glyph state)) 2
                                                     :face (state-face state))))))
     (when (plusp rest)
       (list (bar-button "switch session"
                         (slot (format nil " +~D" rest) (if narrow 3 5) :face :quiet)))))))

(defun default-bar (session)
  "What the bar shows, left to right, each in a slot that keeps its place:
where you are; this session's windows and a way to another; what the shown
window's lone pane is doing; zoomed or reading back; the field for commands
and sessions; what needs you anywhere; the other sessions; who else is here;
the one key to learn; the time.

The right of the bar is pinned to the right edge whatever the width, and the
windows take what is left and are cut there, so nothing on the right is ever
pushed off or moves as windows come and go. When even the right does not
leave the windows their least room it is folded up in steps, and what goes
first is the room kept blank most of the time, never the field or the names
of the sessions: wide has every slot; tight drops the focus slot, folds what
needs you to its count, names two sessions and counts the other terminals;
narrow drops the mode slot and the menu, names three letters of each session
and numbers the windows. Rebind *BAR* to a function of the session answering
another tree, and it is another bar."
  (let* ((now (now-ms))
         (server (session-server session))
         (cols (session-cols session))
         (fit (bar-fit session cols))
         (narrow (eq fit :narrow))
         (tight (not (eq fit :wide)))
         (name (session-name session))
         (chip (if narrow 4 +chip-width+))
         (lead (+ 3 (+ 2 (length name)) 1))
         (room (max 0 (- cols (bar-right-width session fit) 3)))
         ;; the windows have what the right leaves, less the plus, and are
         ;; cut there; the plus follows them wherever they end. The one
         ;; shown is never what is cut: those before it go first
         (windows (let* ((all (session-windows session))
                         (at (or (position (session-window session) all) 0))
                         (fits (max 1 (floor (- room lead 2) chip))))
                    (nthcdr (max 0 (- (1+ at) fits)) all)))
         (natural (+ lead 2 (* (length windows) chip))))
    (apply #'atty/ui:row
           :spacing 0
           :background-color (bar-face :ground)
           (append
            (list (fixed (apply #'atty/ui:row :spacing 0
                                (append
                                 (list (atty/ui:label " λ " :face :brand)
                                       (atty/ui:label (format nil " ~A " name) :face :strong-accent)
                                       (atty/ui:label "│" :face :quiet))
                                 (mapcar (lambda (w) (window-chip session w :narrow narrow)) windows)))
                         (min natural room) 1)
                  (plus-chip session)
                  (atty/ui:gap))
            (unless tight (list (focus-slot session now)))
            (unless narrow (list (mode-slot session)))
            (list (field-slot session))
            (list (queue-slot server :narrow tight))
            (other-sessions-folded session :narrow narrow
                                          :most (ecase fit (:wide +sessions-shown+) (:tight 2) (:narrow 2)))
            (list (clients-slot session :narrow tight))
            (unless narrow (list (menu-slot)))
            (list (atty/ui:label " │ " :face :quiet)
                  (atty/ui:label (format-current-time))
                  (atty/ui:label " "))))))

(defparameter +field-width+ 18 "The field: its kind, and what it opens.")

(defun bar-right-width (session fit)
  "How many columns the right of the bar takes at FIT: what is pinned to the
right edge, everything but the session's windows."
  (let* ((server (session-server session))
         (others (length (and server (remove session (server-sessions server)))))
         (clock 9))
    (flet ((sessions (most width more-width)
             (let ((shown (min others most)))
               (+ (* width shown) (if (> others shown) more-width 0)))))
      (ecase fit
        (:wide (+ +focus-width+ +mode-width+ +field-width+ +needs-width+
                  (sessions +sessions-shown+ +session-width+ 5) +clients-width+ +menu-width+ clock))
        (:tight (+ +mode-width+ +field-width+ 6 (sessions 2 +session-width+ 5) 4 +menu-width+ clock))
        (:narrow (+ +field-width+ 6 (sessions 2 +session-narrow-width+ 3) 4 clock))))))

(defun bar-fit (session cols)
  "How the bar is folded at COLS: :wide when the whole right leaves the
windows their least room, :tight when its narrow forms do, else :narrow."
  (cond ((< cols +narrow-bar+) :narrow)
        ((>= (- cols (bar-right-width session :wide)) +bar-left-min+) :wide)
        ((>= (- cols (bar-right-width session :tight)) +bar-left-min+) :tight)
        (t :narrow)))
(defvar *bar* #'default-bar)

(declaim (ftype (function (session) t) session-bar))
(defun session-bar (session)
  "The bar for SESSION, or nothing when it is turned off. It is the first child
of the column the panes are in, so how many rows it takes is whatever it
measures to rather than a number somebody has to keep in step."
  (when (and (session-bar-p session) *bar*)
    (funcall *bar* session)))
