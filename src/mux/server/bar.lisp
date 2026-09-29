;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; The bar is what the session looks like rather than what any one person is
;;; doing, so the server composes it and it crosses the wire as cells like
;;; everything else. Everyone attached sees the same one.

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

(defparameter +bar-left-min+ 21
  "The least room the left of the bar keeps for the windows: the brand, one
chip and the plus. The right of the bar is folded up until the left has at
least this.")

(defparameter +sessions-shown+ 4
  "How many other sessions the bar names before folding the rest to a count.")

;;; The bar is laid out in slots of set width, so nothing on it moves when
;;; what is in a slot changes: a state word growing, a chip lighting up, a
;;; count going from 9 to 10. What is longer than its slot is cut, what is
;;; shorter is padded, and a slot with nothing to say is still there, blank.

(defparameter +chip-width+ 14 "A window's chip: its number and name, and one glyph.")
(defparameter +narrow-chip-width+ 5)
(defparameter +mode-width+ 16 "Zoomed, or reading back, or nothing, and its ends.")
(defparameter +needs-width+ 17 "What needs you anywhere, or nothing.")
(defparameter +session-width+ 8 "One other session: its name and its worst pane's glyph.")

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
  "A window on the bar, in a slot: its number on the colour of the pane in it
that most wants somebody, its name, and that pane's glyph; a count when any is
asking. The one shown is a pill on the ground of the panes. A click shows it."
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
         (width (if narrow +narrow-chip-width+ +chip-width+))
         (glyph (if (plusp asking) 4 2))
         (number (atty/ui:label (format nil " ~D " n)
                                :face (if worst (number-face worst) :number-unknown)))
         (runs (lambda () (session-select-window session n)))
         (chip (atty/ui:row :spacing 0
                            number
                            (if narrow
                                (atty/ui:label "")
                                (slot (format nil " ~A" (truncate-string name (- width glyph 4)))
                                      (- width glyph 3)
                                      :face (if shown :strong :default)))
                            (if (plusp asking)
                                (slot (format nil "▲~D " asking) glyph :face :state-blocked-strong)
                                (slot (if worst (state-glyph worst) "") glyph
                                      :face (if worst (state-face worst) :quiet))))))
    (bar-button runs (if shown (pill chip :ground :bg) chip))))

(defun plus-chip (session)
  "The way to another window, by mouse."
  (bar-button (lambda () (session-add-window session))
              (atty/ui:label " + " :face :quiet)))

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
        (width (if narrow 7 +needs-width+)))
    (if (plusp n)
        (bar-button (lambda ()
                      (loop :for s :in (server-sessions server)
                            :for pane := (find :blocked (session-panes s)
                                               :key (lambda (p) (agent:agent-state (pane-agent p))))
                            :when pane :do (return (asks-or-go *client* s pane))))
                    (pill (slot (if narrow (format nil " ▲ ~D " n)
                                    (format nil " ▲ ~D need~A you " n (if (= n 1) "s" "")))
                                (- width 2)
                                :face :chip-blocked)
                          :ground :yellow))
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

(declaim (ftype (function (session) boolean) rail-shown-p))
(defun rail-shown-p (session)
  (and (session-rail-p session) (>= (session-cols session) +narrow-bar+)))

(defun session-state (session)
  (let ((worst (worst-pane session)))
    (and worst (pane-known-p worst) (agent:agent-state (pane-agent worst)))))

(defclass rail-row (bar-button)
  ((session :initarg :session :reader rail-row-session)))

(defun rail-session-row (session here)
  (let* ((state (session-state session))
         (asking (count :blocked (session-panes session)
                        :key (lambda (p) (agent:agent-state (pane-agent p)))))
         (worst (worst-pane session))
         (yours (eq session here))
         (name (- +rail-width+ 8)))
    (make-instance 'rail-row
                   :session (session-name session)
                   :runs (cond ((eq state :blocked)
                                (lambda () (asks-or-go *client* session worst)))
                               (worst
                                (lambda () (focus-pane (session-server session) *client* (session-name session) (pane-id worst))))
                               (t (lambda () (go-to *client* (session-name session)))))
                   :parts (list (apply #'atty/ui:row :spacing 0
                                       (append (when yours (list :background-color (bar-face :bg)))
                                               (list (atty/ui:label " ")
                                                     (atty/ui:label (if yours "◆" " ") :face :here)
                                                     (atty/ui:label " ")
                                                     (slot (session-name session) name
                                                           :face (cond (yours :strong) (state :default) (t :quiet)))
                                                     (atty/ui:label " ")
                                                     (slot (state-glyph state) 1
                                                           :face (if (eq state :blocked) :state-blocked-strong (state-face state)))
                                                     (slot (if (> asking 1) (princ-to-string asking) "") 3
                                                           :face :state-blocked-strong))))))))

(defun session-rail (session)
  (let* ((server (session-server session))
         (all (if server (server-sessions server) (list session)))
         (terminals (remove-if-not #'watcher-interactive (session-watchers session)))
         (room (max 1 (- (session-rows session) (if (session-bar-p session) 1 0) 4
                         (if (rest terminals) (1+ (length terminals)) 0))))
         (at (or (position session all) 0))
         (from (max 0 (min (- (length all) room) (- at (floor room 2)))))
         (shown (subseq all from (min (length all) (+ from room))))
         (asking-p (lambda (s) (eq :blocked (session-state s))))
         (width +rail-width+))
    (flet ((edge (cut)
             (atty/ui:label (format nil "~vA" width (if (some asking-p cut) "   ▲" ""))
                            :face :state-blocked-strong)))
      (apply #'atty/ui:column :align :stretch :spacing 0 :min-width width
             :background-color (bar-face :ground)
             (append
              (list (if (plusp from) (edge (subseq all 0 from)) (atty/ui:label "")))
              (mapcar (lambda (s) (rail-session-row s session)) shown)
              (when (< (+ from (length shown)) (length all))
                (list (edge (nthcdr (+ from (length shown)) all))))
              (list (bar-button "new session" (slot "   + session" width :face :quiet)))
              (when (rest terminals)
                (cons (atty/ui:label "")
                      (mapcar (lambda (w)
                                (slot (format nil " ⌨ ~A" (format-tty (watcher-tty w) (watcher-id w)))
                                      width :face :driven))
                              terminals))))))))

(defparameter +mode-chip-width+ 8)

(defun kind-hints (prefix)
  (apply #'atty/ui:row :spacing 0
         (loop :for (other name opens) :in +palette-kinds+
               :unless (eql other prefix)
                 :collect (hint (string other) name :runs opens))))

(defun session-field (session)
  (let* ((kind (nth (mod (session-field-kind session) (length +palette-kinds+)) +palette-kinds+)))
    (destructuring-bind (prefix name runs placeholder) kind
      (declare (ignore name))
      (well (list (atty/ui:row :spacing 0
                               (atty/ui:label " ")
                               (bar-button :cycle-search-kind
                                           (atty/ui:label (format nil " ~C " prefix) :face :brand))
                               (atty/ui:label " ")
                               (bar-button runs
                                           (squeezed (atty/ui:row :spacing 0
                                                                  (atty/ui:label " " :face :field-cursor)
                                                                  (atty/ui:label " ")
                                                                  (atty/ui:label placeholder :face :placeholder)))
                                           :expand 1)
                               (kind-hints prefix)))))))

(defun default-bar (session)
  "What the bar shows, left to right, each in a slot that keeps its place: the
corner, which opens the menu; this session's windows and a way to another;
zoomed or reading back; what needs you anywhere; the other sessions; the time.

The right of the bar is pinned to the right edge whatever the width, and the
windows take what is left and are cut there, so nothing on the right is ever
pushed off or moves as windows come and go. When even the right does not
leave the windows their least room it is folded up in steps, never the names
of the sessions: wide has every slot; tight folds what needs you to its count
and names two sessions; narrow drops the mode slot, names three letters of
each session and numbers the windows. Rebind *BAR* to a function of the
session answering another tree, and it is another bar."
  (let* ((server (session-server session))
         (cols (session-cols session))
         (fit (bar-fit session cols))
         (narrow (eq fit :narrow))
         (tight (not (eq fit :wide)))
         (chip (if narrow +narrow-chip-width+ +chip-width+))
         (rail (rail-shown-p session))
         (lead (if rail +rail-width+ 4))
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
                                 (list (bar-button "show menu" (atty/ui:label " λ " :face :brand))
                                       (if rail
                                           (slot (format nil " ~A" (if server (file-namestring (server-path server)) ""))
                                                 (- +rail-width+ 3) :face :quiet)
                                           (atty/ui:label " ")))
                                 (mapcar (lambda (w) (window-chip session w :narrow narrow)) windows)))
                         (min natural room) 1)
                  (plus-chip session)
                  (atty/ui:gap))
            (unless narrow (list (mode-slot session)))
            (unless rail
              (other-sessions-folded session :narrow narrow
                                            :most (ecase fit (:wide +sessions-shown+) (:tight 2) (:narrow 2))))
            (list (atty/ui:label " ")
                  (queue-slot server :narrow tight)
                  (atty/ui:label "  ")
                  (atty/ui:label (format-current-time))
                  (atty/ui:label " "))))))

(defun bar-right-width (session fit)
  "How many columns the right of the bar takes at FIT: what is pinned to the
right edge, everything but the session's windows."
  (let* ((server (session-server session))
         (others (if (rail-shown-p session)
                     0
                     (length (and server (remove session (server-sessions server))))))
         (clock 10))
    (flet ((sessions (most width more-width)
             (let ((shown (min others most)))
               (+ (* width shown) (if (> others shown) more-width 0)))))
      (ecase fit
        (:wide (+ +mode-width+ +needs-width+ (sessions +sessions-shown+ +session-width+ 5) clock))
        (:tight (+ +mode-width+ 7 (sessions 2 +session-width+ 5) clock))
        (:narrow (+ 7 (sessions 2 +session-narrow-width+ 3) clock))))))

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
