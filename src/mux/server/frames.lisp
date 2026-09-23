;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What a pane's frame says. The frame is the session's, composed once and
;;; shared by everybody attached, so it says what the pane is and never who is
;;; looking at it. Its colour is what the program is doing, a double line is
;;; where the focus is, and the corners carry the rest: which pane, what it is
;;; called and what it runs at the top left, its state and how long at the top
;;; right, and, when it is asking something, the question at the top and the
;;; answers along the bottom.
;;;
;;; A blocked pane is never made smaller to fit its answers in: that would tell
;;; the program it was resized, and it would draw the question again. They go
;;; in the border rows the frame already has.

(defun state-face (state)
  (case state
    (:working :state-working)
    (:blocked :state-blocked)
    (:idle :state-idle)
    (t :state-unknown)))

(defun number-face (state)
  (case state
    (:working :number-working)
    (:blocked :number-blocked)
    (:idle :number-idle)
    (t :number-unknown)))

(defun state-glyph (state)
  (case state
    (:working "◐")
    (:blocked "▲")
    (:idle "○")
    (t "·")))

(defun pane-known-p (pane)
  (agent:agent-known-p (pane-agent pane)))

(defun frame-face (pane focusp)
  "What colour PANE's frame is: what a known agent is doing, and a colour that
never changes for anything else, since for a program nobody knows how to read
working and idle would only say whether its screen moved. Where the focus is
is the double line, not a colour."
  (declare (ignore focusp))
  (if (pane-known-p pane)
      (state-face (agent:agent-state (pane-agent pane)))
      :state-unknown))

(defun command-key (command)
  "The chord COMMAND is bound to in the pane's mode, as a hint says it, or nil
when nothing is. Only ever a hint: the binding is whoever set it up's, and a
command with no key still has its name."
  (let ((does (and (fboundp command) (symbol-function command))))
    (car (find does (atty/mode:keys-in-force (atty/mode:mode-named 'pane-mode))
               :key #'cdr))))

(defun frame-actor-text (actor)
  "WHO from a pane's log, as somebody reading a frame would call it."
  (case (first actor)
    (:pane (second actor))
    (:client (or (third actor) (format nil "client ~D" (second actor))))
    (:atty "atty")
    (t "the command line")))

(defun last-input-marker (pane now)
  "The pane's last input, when it came from something other than a person at a
terminal: that is the one worth seeing on a frame everybody shares."
  (let ((newest (first (pane-log pane))))
    (when (and newest (member (first (second newest)) '(:pane :cli)))
      (atty/ui:label (format nil " ⌁ last input: ~A ~(~A~), ~A ago "
                             (frame-actor-text (second newest)) (third newest)
                             (format-duration (- now (first newest))))
                     :face :driven))))

(defun fit-options (options room)
  "The texts of OPTIONS, each shortened as little as it takes for all of them,
as buttons, to fit in ROOM columns. The longest gives way first, so a short
last option is never the one that goes."
  (let ((texts (mapcar #'second options)))
    (flet ((wide () (+ (reduce #'+ texts :key (lambda (s) (+ 4 (length s))))
                       (max 0 (1- (length texts))))))
      (loop :while (> (wide) room)
            :do (let* ((longest (reduce #'max texts :key #'length))
                       (at (position longest texts :key #'length)))
                  (when (<= longest 2) (return))
                  (setf (nth at texts)
                        (truncate-string (nth at texts)
                                      (max 2 (- longest (- (wide) room))))))))
    texts))

(defun option-buttons (session pane options room)
  (apply #'atty/ui:row :spacing 1
         (loop :for (n) :in options
               :for text :in (fit-options options room)
               :collect (bar-button (list :answer (session-name session) (pane-id pane) n)
                                    (atty/ui:row :spacing 0
                                                 (atty/ui:label (format nil " ~D" n)
                                                                :face :key-number)
                                                 (atty/ui:label (format nil " ~A " text)
                                                                :face :key))))))

(defun question-title (asks)
  (atty/ui:row :spacing 0
               (atty/ui:label " ▲ asks " :face :state-blocked-strong)
               (atty/ui:label (getf asks :subject) :face :strong)
               (atty/ui:label (format nil "  ~A "
                                      (or (first (getf asks :detail))
                                          (getf asks :question))))))

(defun answer-extras (session pane won)
  "What follows the answers, most wanted first: read the whole of it, give it
the whole session, and which rule decided it was asking."
  (let ((name (session-name session))
        (id (pane-id pane)))
    (remove nil
            ;; dim words beside the answers, not keys of their own: the
            ;; answers are what the eye should land on
            (list (bar-button (list :pane-read name id)
                              (atty/ui:label " read " :face :quiet))
                  (bar-button (list :zoom name id)
                              (atty/ui:label (if (eq pane (session-zoomed session))
                                                 " unzoom " " zoom ")
                                             :face :quiet))
                  (and won (atty/ui:label (format nil " matched: ~A " won)
                                          :face :quiet))))))

(defun columns-in (widget)
  "How many columns the labels in WIDGET take, laid side by side."
  (if (typep widget 'atty/ui:label)
      (atty/cells:columns-of (atty/ui:text widget))
      (reduce #'+ (atty/ui:parts widget) :key #'columns-in)))

(defun extras-width (extras)
  (+ (reduce #'+ extras :key #'columns-in)
     (max 0 (1- (length extras)))))

(defun search-marker (pane)
  "What was looked for in PANE and which hit this is, with the keys that move
between them."
  (let* ((find (pane-find pane))
         (hits (getf find :hits))
         (at (getf find :at)))
    (atty/ui:row :spacing 0
                 (atty/ui:label (format nil " / ~A " (getf find :query)) :face :chip-scrolled)
                 (atty/ui:label (if hits
                                    (format nil " ~D of ~D · ~A next · ~A back "
                                            (- (length hits) at) (length hits)
                                            (key-hint 'scroll-mode 'find-next)
                                            (key-hint 'scroll-mode 'find-previous))
                                    " nothing ")
                                :face :quiet))))

(defun frame-corners (session pane focusp)
  "The four corners of PANE's frame."
  (let* ((agent (pane-agent pane))
         (state (agent:agent-state agent))
         (term (pane-term pane))
         (now (now-ms))
         (asks (agent:agent-asks agent term))
         (width (term:term-width term)))
    (list
     :tl (atty/ui:row :spacing 0
                      (atty/ui:label (format nil " ~D " (or (pane-number session pane) (pane-id pane)))
                                     :face (cond ((not focusp) :strong)
                                                 ((pane-known-p pane) (number-face state))
                                                 (t :number-unknown)))
                      (atty/ui:label (format nil " ~A " (pane-display-name pane)))
                      (atty/ui:label (format nil "~A " (pane-kind pane)) :face :quiet)
                      (if (eq pane (session-zoomed session))
                          (atty/ui:label " ⤢ zoomed " :face :state-blocked-strong)
                          (atty/ui:label "")))
     :tr (cond (asks (question-title asks))
               ((pane-known-p pane)
                (atty/ui:label (format nil " ~A ~(~A~) ~A " (state-glyph state) state
                                       (format-duration (agent:agent-for agent now)))
                               :face (state-face state))))
     :bl (cond
           (asks
             (let* ((extras (answer-extras session pane (agent:agent-won agent term)))
                    (room (- width 2 (extras-width extras) 1)))
               ;; what follows the answers gives way before the answers do,
               ;; the rule first, then zoom; read stays
               (loop :while (and (rest extras)
                                 (> (+ (* 5 (length (getf asks :options))) 1) room))
                     :do (setf extras (butlast extras)
                               room (- width 2 (extras-width extras) 1)))
               (atty/ui:row :spacing 1
                            (option-buttons session pane (getf asks :options) room)
                            (apply #'atty/ui:row :spacing 1 extras))))
           ((pane-find pane) (search-marker pane))
           (t (last-input-marker pane now)))
     :br (cond
           ;; being read back says so before anything else does: what is on
           ;; the screen is not what the program has on it now, and finding
           ;; in it is a button beside that
           ((or (plusp (pane-scrolled pane)) (pane-find pane))
            (atty/ui:row :spacing 0
                         (bar-button "find in pane" (atty/ui:label " ⌕ find " :face :quiet))
                         (if (plusp (pane-scrolled pane)) (live-chip pane) (atty/ui:label ""))
                         (atty/ui:label (format nil " line ~D of ~D "
                                                (1+ (pane-top-row pane)) (pane-row-count pane))
                                        :face :quiet)))
           ((and (eq state :blocked) (null asks))
            (let ((answer (command-key 'goto-blocked-pane))
                  (zoom (command-key 'zoom-pane)))
              (when (or answer zoom)
                (atty/ui:label (format nil " ~@[~A answer~]~:[~; · ~]~@[~A zoom~] "
                                       answer (and answer zoom) zoom)
                               :face :state-blocked))))))))
