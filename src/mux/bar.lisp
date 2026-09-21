;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; The bar is what the session looks like rather than what any one person is
;;; doing, so the server composes it and it crosses the wire as cells like
;;; everything else. Everyone attached sees the same one.

(declaim (special +prompt-toggles+))

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

(defun bar-button (runs part)
  (make-instance 'bar-button :runs runs :parts (list part)))

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
                (if (eq state :blocked)
                    (atty/ui:label (format nil " ~D ~A ~A ~(~A~) ~A "
                                           (pane-id pane) says (state-glyph state) state for)
                                   :face :chip-blocked)
                    (apply #'atty/ui:row :spacing 0
                           (append
                            (when (eq pane (session-focus session))
                              (list :background-color (bar-face :bg-active)))
                            (list (atty/ui:label (format nil " ~D" (pane-id pane))
                                                 :face :accent)
                                  (atty/ui:label (format nil " ~A " says))
                                  (atty/ui:label (format nil "~A ~(~A~)" (state-glyph state) state)
                                                 :face (state-face state))
                                  (atty/ui:label (format nil " ~A " for) :face :quiet))))))))

(defparameter +worse+ '(:blocked :working :idle :unknown)
  "The states, the one that most wants somebody first.")

(defun worst-pane (session)
  "The pane in SESSION that most wants somebody."
  (first (sort (copy-list (session-panes session)) #'<
               :key (lambda (p) (or (position (agent:agent-state (pane-agent p)) +worse+)
                                    (length +worse+))))))

(defun blocked-count (server)
  (loop :for s :in (server-sessions server)
        :sum (count :blocked (session-panes s)
                    :key (lambda (p) (agent:agent-state (pane-agent p))))))

(defun needs-you-button (server)
  (let ((n (blocked-count server)))
    (when (plusp n)
      (bar-button "needs you"
                  (atty/ui:row :spacing 0 :background-color (bar-face :bg-alt)
                               (atty/ui:label (format nil " ▲ ~D need~A you " n
                                                      (if (= n 1) "s" ""))
                                              :face :state-blocked)
                               (let ((key (key-for 'needs-you)))
                                 (atty/ui:label (if key (format nil "~A " key) "")
                                                :face :quiet)))))))

(defun other-sessions (session)
  "A dot for every other session, coloured by the pane in it that most wants
somebody; a click goes there."
  (let ((server (session-server session)))
    (loop :for s :in (and server (server-sessions server))
          :for worst := (and (not (eq s session)) (worst-pane s))
          :when worst
            :collect (let ((state (agent:agent-state (pane-agent worst))))
                       (bar-button (list :focus-pane (session-name s) (pane-id worst))
                                   (atty/ui:row :spacing 0
                                                (atty/ui:label (format nil " ~A " (session-name s))
                                                               :face :quiet)
                                                (atty/ui:label (state-glyph state)
                                                               :face (state-face state))))))))

(defun default-bar (session)
  "What the bar shows: the session, a chip for every pane in it, the way in to
commands, how many panes anywhere are waiting on somebody, and every other
session. Rebind *BAR* to a function of the session answering another tree, and
it is another bar."
  (let ((now (now-ms))
        (server (session-server session)))
    (apply #'atty/ui:row
           :spacing 1
           :background-color (bar-face :bg-dim)
           (append
            (list (atty/ui:label " λ " :face :brand)
                  (atty/ui:label (format nil "~A " (session-name session)) :face :accent))
            (mapcar (lambda (p) (pane-chip session p now)) (session-panes session))
            (list (atty/ui:gap)
                  (search-segment session)
                  (atty/ui:gap))
            (let ((it (and server (needs-you-button server)))) (and it (list it)))
            (other-sessions session)
            (list (atty/ui:label (if (pane-running (session-focus session)) "" "done")
                                 :face :warning)
                  (atty/ui:label (let ((term (session-pane-term session)))
                                   (format nil "~Dx~D" (term:term-width term)
                                           (term:term-height term)))
                                 :face :quiet)
                  (atty/ui:label (clock-says))
                  (atty/ui:label " "))))))
(defvar *bar* #'default-bar)

(defun session-bar (session)
  "The bar for SESSION, or nothing when it is turned off. It is the last child
of the column the panes are in, so how many rows it takes is whatever it
measures to rather than a number somebody has to keep in step."
  (when (and (session-barp session) *bar*)
    (funcall *bar* session)))
