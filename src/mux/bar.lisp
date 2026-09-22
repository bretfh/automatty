;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; The bar is what the session looks like rather than what any one person is
;;; doing, so the server composes it and it crosses the wire as cells like
;;; everything else. Everyone attached sees the same one.

(declaim (special +prompt-toggles+))
(declaim (ftype function key-in key-for state-glyph state-face known-p shortened-to duration pane-top-row pane-rows-kept prefix-spelled))

(defun search-segment (session)
  "The bar's own way in: one field, styled like a search bar and a shade
deeper than the bar it sits on, and one button naming what it currently opens.
Clicking the button cycles it through +PROMPT-TOGGLES+; clicking the field
runs whichever of them is current."
  (let* ((kind (nth (mod (session-search-kind session) (length +prompt-toggles+))
                    +prompt-toggles+))
         (prefix (car kind)) (runs (cdr kind)))
    (atty/ui:row
     :spacing 0 :background-color (bar-face :bg-alt)
     (bar-button :cycle-search-kind (atty/ui:label (format nil " ~C " prefix) :face :brand))
     (bar-button runs (atty/ui:label (format nil " ~A " runs))))))

(defun clock-says ()
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (format nil "~2,'0D:~2,'0D" hour minute)))

(defun shortened (command)
  "What to call a program on a bar. A shell out of the store is a path nobody
reads to the end, and its name is the last thing in it."
  (let* ((said (string-trim " " (or command "")))
         (space (position #\Space said))
         (first-word (subseq said 0 (or space (length said))))
         (slash (position #\/ first-word :from-end t))
         (name (if slash (subseq first-word (1+ slash)) first-word)))
    (if space
        (concatenate 'string name (subseq said space))
        name)))

(defun pane-says (pane)
  "What to call PANE: the name somebody gave it, else the title its program
gave itself, else the program."
  (or (pane-label pane)
      (and (pane-named pane) (plusp (length (pane-named pane))) (pane-named pane))
      (shortened (pane-command pane))
      ""))

(defparameter +chip-says+ 20
  "How much of a pane's name a chip on the bar has room for.")

(defun pane-chip (session pane now)
  "A pane on the bar: its number, what it is called, and what it is doing and
for how long. A click goes to it."
  (let* ((agent (pane-agent pane))
         (state (agent:agent-state agent))
         (says (shortened-to (pane-says pane) +chip-says+))
         (for (duration (agent:agent-for agent now)))
         (runs (list :focus-pane (session-name session) (pane-id pane))))
    (bar-button runs
                (cond
                  ((not (known-p pane))
                   ;; a program nobody knows how to read: which one, and
                   ;; nothing about what it is doing
                   (apply #'atty/ui:row :spacing 0
                          (append
                           (when (eq pane (session-focus session))
                             (list :background-color (bar-face :bg-active)))
                           (list (atty/ui:label (format nil " ~D" (pane-id pane)) :face :strong)
                                 (atty/ui:label (format nil " ~A " says))))))
                  ((eq state :blocked)
                   (atty/ui:label (format nil " ~D ~A ~A ~(~A~) ~A "
                                          (pane-id pane) says (state-glyph state) state for)
                                  :face :chip-blocked))
                  (t
                    (apply #'atty/ui:row :spacing 0
                           (append
                            (when (eq pane (session-focus session))
                              (list :background-color (bar-face :bg-active)))
                            (list (atty/ui:label (format nil " ~D" (pane-id pane))
                                                 :face :strong)
                                  (atty/ui:label (format nil " ~A " says))
                                  (atty/ui:label (format nil "~A ~(~A~)" (state-glyph state) state)
                                                 :face (state-face state))
                                  (atty/ui:label (format nil " ~A " for) :face :quiet)))))))))

(defparameter +worse+ '(:blocked :working :idle :unknown)
  "The states, the one that most wants somebody first.")

(defun worst-pane (session)
  "The pane in SESSION that most wants somebody."
  (first (sort (copy-list (session-panes session)) #'<
               :key (lambda (p) (or (and (known-p p)
                                         (position (agent:agent-state (pane-agent p)) +worse+))
                                    (length +worse+))))))

(defun blocked-count (server)
  (loop :for s :in (server-sessions server)
        :sum (count :blocked (session-panes s)
                    :key (lambda (p) (agent:agent-state (pane-agent p))))))

(defun needs-you-button (server &key narrow)
  (let ((n (blocked-count server)))
    (when (plusp n)
      (bar-button "needs you"
                  (atty/ui:row :spacing 0 :background-color (bar-face :bg-alt)
                               (atty/ui:label (if narrow
                                                  (format nil " ▲ ~D " n)
                                                  (format nil " ▲ ~D need~A you " n (if (= n 1) "s" "")))
                                              :face :state-blocked-strong)
                               (let ((key (key-for 'needs-you)))
                                 (atty/ui:label (if (and key (not narrow)) (format nil "~A " key) "")
                                                :face :quiet)))))))

(defun other-sessions (session)
  "A dot for every other session, coloured by the pane in it that most wants
somebody; a click goes there."
  (let ((server (session-server session)))
    (loop :for s :in (and server (server-sessions server))
          :for worst := (and (not (eq s session)) (worst-pane s))
          :when worst
            :collect (let ((state (and (known-p worst) (agent:agent-state (pane-agent worst)))))
                       (bar-button (list :focus-pane (session-name s) (pane-id worst))
                                   (atty/ui:row :spacing 0
                                                (atty/ui:label (format nil " ~A " (session-name s))
                                                               :face :quiet)
                                                (atty/ui:label (state-glyph state)
                                                               :face (state-face state))))))))

(defparameter +narrow-bar+ 80
  "A terminal narrower than this gets a bar without names: the glyphs and the
counts say what the names would, in the room there is.")

(defparameter +sessions-shown+ 4
  "How many other sessions the bar names before folding the rest to a count.")

;;; The bar is laid out in slots of set width, so nothing on it moves when
;;; what is in a slot changes: a state word growing, a chip lighting up, a
;;; count going from 9 to 10. What is longer than its slot is cut, what is
;;; shorter is padded, and a slot with nothing to say is still there, blank.

(defparameter +chip-width+ 14 "A window's chip: its number and name, and one glyph.")
(defparameter +focus-width+ 14 "What the shown window's one pane is doing, when it has one and no frame.")
(defparameter +mode-width+ 14 "Zoomed, or reading back, or nothing.")
(defparameter +needs-width+ 21 "What needs you anywhere, or nothing.")
(defparameter +session-width+ 8 "One other session: its name and its worst pane's glyph.")
(defparameter +clients-width+ 14 "Who else is attached, or nothing.")
(defparameter +menu-width+ 9 "The one key to learn, always in the same place.")

(defun slot (text width &key (face :default) background)
  "TEXT in a slot exactly WIDTH wide: cut when longer, padded when shorter."
  (apply #'atty/ui:label (format nil "~vA" width (shortened-to (or text "") width))
         :face face
         (and background (list :background-color background))))

(defun window-worst (window)
  "The state of the pane in WINDOW that most wants somebody, or nil when none
of them is a known agent."
  (let ((known (remove-if-not #'known-p (window-panes window))))
    (and known
         (agent:agent-state
          (pane-agent (first (sort (copy-list known) #'<
                                   :key (lambda (p) (position (agent:agent-state (pane-agent p))
                                                              +worse+)))))))))

(defun window-chip (session window &key narrow)
  "A window on the bar, in a slot: its number and name, and one glyph for the
pane in it that most wants somebody; yellow with a count when any is asking.
The one shown is lit. A click shows it."
  (let* ((n (window-number session window))
         (panes (window-panes window))
         (asking (count :blocked panes :key (lambda (p) (agent:agent-state (pane-agent p)))))
         (worst (window-worst window))
         (shown (eq window (session-window session)))
         ;; an unnamed window goes by its first pane, which does not change
         ;; as the focus moves about in it
         (name (or (window-label window)
                   (and panes (pane-says (first panes)))
                   ""))
         (width (if narrow 4 +chip-width+))
         (runs (list :go-window (session-name session) n)))
    (bar-button runs
                (cond
                  ((plusp asking)
                   (slot (if narrow (format nil " ~D▲" n)
                             (format nil " ~D ~A ▲~D" n (shortened-to name (- width 7)) asking))
                         width :face :chip-blocked))
                  (t
                   (atty/ui:row :spacing 0
                                (slot (if narrow (format nil " ~D" n)
                                          (format nil " ~D ~A" n (shortened-to name (- width 5))))
                                      (- width 2)
                                      :face :strong
                                      :background (and shown (bar-face :bg-active)))
                                (slot (if worst (state-glyph worst) "") 2
                                      :face (if worst (state-face worst) :quiet)
                                      :background (and shown (bar-face :bg-active)))))))))

(defun plus-chip (session)
  "The way to another window, by mouse."
  (bar-button (list :new-window (session-name session))
              (atty/ui:label " + " :face :quiet)))

(defun focus-slot (session now &key narrow)
  "What the shown window's one pane is doing, when it has one and there is no
frame to say it: the glyph, the state and how long, in a slot that is there
either way."
  (let* ((window (session-window session))
         (panes (window-panes window))
         (pane (and (null (rest panes)) (first panes)))
         (state (and pane (known-p pane) (agent:agent-state (pane-agent pane))))
         (width (if narrow 10 +focus-width+)))
    (if state
        (slot (if narrow
                  (format nil " ~(~A~)" state)
                  (format nil " ~A ~(~A~) ~4A" (state-glyph state) state
                          (duration (agent:agent-for (pane-agent pane) now))))
              width :face (state-face state))
        (slot "" width))))

(defun mode-slot (session)
  "Zoomed, or somebody reading back, or nothing, in one slot: a click on
reading back, from whoever is, is back to live."
  (let ((zoomed (session-zoomed session)))
    (cond
      (zoomed
       (slot (format nil " ⤢ ~A zoomed" (pane-says zoomed)) +mode-width+ :face :strong
             :background (bar-face :bg-alt)))
      ((session-readers session)
       (bar-button "leave scroll mode"
                   (slot " reading back" +mode-width+ :face :chip-scrolled)))
      (t (slot "" +mode-width+)))))

(defun needs-slot (server &key narrow)
  (let ((n (blocked-count server))
        (width (if narrow 6 +needs-width+)))
    (if (plusp n)
        (bar-button "needs you"
                    (atty/ui:row :spacing 0
                                 (slot (if narrow (format nil " ▲ ~D" n)
                                           (format nil " ▲ ~D need~A you" n (if (= n 1) "s" "")))
                                       (if narrow width (- width 6))
                                       :face :state-blocked-strong :background (bar-face :bg-alt))
                                 (if narrow
                                     (atty/ui:label "")
                                     (slot (let ((key (key-for 'needs-you))) (if key (format nil " ~A" key) ""))
                                           6 :face :quiet :background (bar-face :bg-alt)))))
        (slot "" width))))

(defun menu-slot ()
  "The one key to learn: the prefix, and that a menu follows it, in a slot of
its own so it is always in the same place."
  (bar-button "show the menu"
              (slot (format nil " ~A menu" (prefix-spelled)) +menu-width+ :face :quiet)))

(defun short-tty (tty id)
  (cond ((and (stringp tty) (> (length tty) 5) (string= "/dev/" tty :end2 5)) (subseq tty 5))
        ((and (stringp tty) (plusp (length tty))) tty)
        (t (format nil "client ~D" id))))

(defun clients-slot (session &key narrow)
  "Who else is attached to this session, when anybody is: the bar is
everybody's, so it names them all."
  (let ((here (remove-if-not #'watcher-here (session-watchers session)))
        (width (if narrow 4 +clients-width+)))
    (if (rest here)
        (bar-button "clients"
                    (slot (if narrow
                              (format nil " ⌨~D" (length here))
                              (format nil " ⌨ ~{~A~^,~}"
                                      (mapcar (lambda (w) (format nil "~A~:[~;←~]" (short-tty (watcher-tty w) (watcher-id w))
                                                                  (watcher-following w)))
                                              here)))
                          width :face :driven))
        (slot "" width))))

(defun other-sessions-folded (session &key narrow)
  "Every other session in a slot of its own, the first few by name, the rest
as how many more; a click goes to the pane there that most wants somebody."
  (let* ((server (session-server session))
         (others (and server (remove session (server-sessions server))))
         (shown (subseq others 0 (min (length others) +sessions-shown+)))
         (rest (- (length others) (length shown)))
         (width (if narrow 3 +session-width+)))
    (append
     (loop :for s :in shown
           :for worst := (worst-pane s)
           :collect (let ((state (and worst (known-p worst) (agent:agent-state (pane-agent worst)))))
                      (bar-button (if worst
                                      (list :focus-pane (session-name s) (pane-id worst))
                                      (list :go (session-name s)))
                                  (atty/ui:row :spacing 0
                                               (slot (if narrow "" (format nil " ~A" (session-name s)))
                                                     (- width 2) :face :quiet)
                                               (slot (format nil "~A " (state-glyph state)) 2
                                                     :face (state-face state))))))
     (when (plusp rest)
       (list (bar-button "choose a session"
                         (slot (format nil " +~D" rest) (if narrow 3 5) :face :quiet)))))))

(defun default-bar (session)
  "What the bar shows, left to right, each in a slot that keeps its place:
where you are; this session's windows and a way to another; what the shown
window's lone pane is doing; zoomed or reading back; the field for commands
and sessions; what needs you anywhere; the other sessions; who else is here;
the one key to learn; the time. Rebind *BAR* to a function of the session
answering another tree, and it is another bar."
  (let* ((now (now-ms))
         (server (session-server session))
         (narrow (< (session-cols session) +narrow-bar+)))
    (apply #'atty/ui:row
           :spacing 0
           :background-color (bar-face :bg-dim)
           (append
            (list (atty/ui:label " λ " :face :brand)
                  (atty/ui:label (format nil " ~A " (session-name session)) :face :strong-accent)
                  (atty/ui:label "│" :face :quiet))
            (mapcar (lambda (w) (window-chip session w :narrow narrow)) (session-windows session))
            (list (plus-chip session))
            (list (focus-slot session now :narrow narrow))
            (unless narrow (list (mode-slot session)))
            (list (atty/ui:gap))
            (unless narrow (list (search-segment session) (atty/ui:gap)))
            (list (needs-slot server :narrow narrow))
            (other-sessions-folded session :narrow narrow)
            (list (clients-slot session :narrow narrow))
            (unless narrow (list (menu-slot)))
            (list (atty/ui:label " │ " :face :quiet)
                  (atty/ui:label (clock-says))
                  (atty/ui:label " "))))))
(defvar *bar* #'default-bar)

(defun session-bar (session)
  "The bar for SESSION, or nothing when it is turned off. It is the last child
of the column the panes are in, so how many rows it takes is whatever it
measures to rather than a number somebody has to keep in step."
  (when (and (session-barp session) *bar*)
    (funcall *bar* session)))
