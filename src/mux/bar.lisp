;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; The bar is what the session looks like rather than what any one person is
;;; doing, so the server composes it and it crosses the wire as cells like
;;; everything else. Everyone attached sees the same one.

(declaim (special +prompt-toggles+))
(declaim (ftype function key-in key-for state-glyph state-face known-p shortened-to duration))

(defun bar-face (role)
  (atty/ui:unhex (atty/ui:color role)))

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

;;; A button on the bar is a widget like any other, except a click on it does
;;; not run anything itself: the bar is composed once and shared by everyone
;;; watching, so what a click on it means is the server's to say, and RUNS
;;; names a command for whichever watcher clicked it to be told to run.

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

(defun window-worst (window)
  "The state of the pane in WINDOW that most wants somebody, or nil when none
of them is a known agent."
  (let ((known (remove-if-not #'known-p (window-panes window))))
    (and known
         (agent:agent-state
          (pane-agent (first (sort (copy-list known) #'<
                                   :key (lambda (p) (position (agent:agent-state (pane-agent p))
                                                              +worse+)))))))))

(defun window-chip (session window now &key narrow)
  "A window on the bar: its number and name, how many of its panes are asking
when any is, and what its one pane is doing when it has one and nothing else
on screen says. The one shown is lit. A click shows it."
  (let* ((n (window-number session window))
         (panes (window-panes window))
         (asking (count :blocked panes :key (lambda (p) (agent:agent-state (pane-agent p)))))
         (worst (window-worst window))
         (shown (eq window (session-window session)))
         (name (or (window-label window)
                   (and (window-focus window) (shortened-to (pane-says (window-focus window)) +chip-says+))
                   ""))
         (label (if narrow (format nil " ~D" n) (format nil " ~D ~A" n name)))
         (runs (list :go-window (session-name session) n)))
    (bar-button runs
                (cond
                  ((plusp asking)
                   (atty/ui:label (format nil "~A ▲~D " label asking) :face :chip-blocked))
                  (t
                   (apply #'atty/ui:row :spacing 0
                          (append
                           (when shown (list :background-color (bar-face :bg-active)))
                           (list (atty/ui:label label :face :strong))
                           (cond
                             ;; one known pane and no frame to say it: the chip does
                             ((and (null (rest panes)) worst)
                              (let ((agent (pane-agent (first panes))))
                                (list (atty/ui:label (format nil " ~A ~(~A~)" (state-glyph worst) worst)
                                                     :face (state-face worst))
                                      (atty/ui:label (format nil " ~A " (duration (agent:agent-for agent now)))
                                                     :face :quiet))))
                             (worst (list (atty/ui:label (format nil " ~A " (state-glyph worst))
                                                         :face (state-face worst))))
                             (t (list (atty/ui:label " ")))))))))))

(defun plus-chip (session)
  "The way to another window, by mouse."
  (bar-button (list :new-window (session-name session))
              (atty/ui:label " + " :face :quiet)))

(defun zoomed-chip (session)
  (let ((pane (session-zoomed session)))
    (and pane
         (atty/ui:row :background-color (bar-face :bg-alt)
                      (atty/ui:label (format nil " ⤢ ~A zoomed " (shortened-to (pane-says pane) +chip-says+))
                                     :face :strong)))))

(defun reading-chip (session)
  "Said while somebody on the session is reading a pane back; a click, from
whoever is, is back to live."
  (and (session-readers session)
       (bar-button "leave scroll mode"
                   (atty/ui:row :spacing 0
                                (atty/ui:label " reading back " :face :chip-scrolled)
                                (let ((key (key-in 'scroll-mode 'leave-scroll-mode)))
                                  (atty/ui:label (if key (format nil " ~A leaves " key) " ")
                                                 :face :quiet))))))

(defun others-here (session &key narrow)
  "Who else is attached to this session, when anybody is: the bar is
everybody's, so it names them all."
  (let ((here (remove-if-not #'watcher-here (session-watchers session))))
    (when (rest here)
      (bar-button "clients"
                  (atty/ui:label (if narrow
                                     (format nil " ⌨ ~D " (length here))
                                     (format nil " ⌨ ~{~A~^, ~} "
                                             (mapcar (lambda (w) (short-tty (watcher-tty w) (watcher-id w)))
                                                     here)))
                                 :face :driven)))))

(defun short-tty (tty id)
  (cond ((and (stringp tty) (> (length tty) 5) (string= "/dev/" tty :end2 5)) (subseq tty 5))
        ((and (stringp tty) (plusp (length tty))) tty)
        (t (format nil "client ~D" id))))

(defun other-sessions-folded (session &key narrow)
  "Every other session as a dot, the first few by name, the rest as how many
more; the dots go to the pane there that most wants somebody."
  (let* ((server (session-server session))
         (others (and server (remove session (server-sessions server))))
         (shown (subseq others 0 (min (length others) +sessions-shown+)))
         (rest (- (length others) (length shown))))
    (append
     (loop :for s :in shown
           :for worst := (worst-pane s)
           :when worst
             :collect (let ((state (and (known-p worst) (agent:agent-state (pane-agent worst)))))
                        (bar-button (list :focus-pane (session-name s) (pane-id worst))
                                    (atty/ui:row :spacing 0
                                                 (atty/ui:label (if narrow " " (format nil " ~A " (session-name s)))
                                                                :face :quiet)
                                                 (atty/ui:label (state-glyph state)
                                                                :face (state-face state))))))
     (when (plusp rest)
       (list (bar-button "choose a session"
                         (atty/ui:label (format nil " +~D " rest) :face :quiet)))))))

(defun default-bar (session)
  "What the bar shows, left to right: where you are, this session's windows
and a way to another, what is zoomed or being read back, the field for
commands and sessions, what needs you anywhere, the other sessions, who else
is here, the time. Rebind *BAR* to a function of the session answering another
tree, and it is another bar."
  (let* ((now (now-ms))
         (server (session-server session))
         (narrow (< (session-cols session) +narrow-bar+)))
    (apply #'atty/ui:row
           :spacing 1
           :background-color (bar-face :bg-dim)
           (append
            (list (atty/ui:label " λ " :face :brand)
                  (atty/ui:label (format nil "~A " (session-name session)) :face :strong-accent)
                  (atty/ui:label "│" :face :quiet))
            (mapcar (lambda (w) (window-chip session w now :narrow narrow)) (session-windows session))
            (list (plus-chip session))
            (let ((z (zoomed-chip session))) (and z (list z)))
            (let ((r (reading-chip session))) (and r (list r)))
            (list (atty/ui:gap))
            (unless narrow (list (search-segment session) (atty/ui:gap)))
            (let ((it (and server (needs-you-button server :narrow narrow)))) (and it (list it)))
            (other-sessions-folded session :narrow narrow)
            (let ((o (others-here session :narrow narrow))) (and o (list o)))
            (list (atty/ui:label (if (pane-running (session-focus session)) "" "done")
                                 :face :warning)
                  (atty/ui:label "│" :face :quiet)
                  (atty/ui:label (clock-says))
                  (atty/ui:label " "))))))
(defvar *bar* #'default-bar)

(defun session-bar (session)
  "The bar for SESSION, or nothing when it is turned off. It is the last child
of the column the panes are in, so how many rows it takes is whatever it
measures to rather than a number somebody has to keep in step."
  (when (and (session-barp session) *bar*)
    (funcall *bar* session)))
