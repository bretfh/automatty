;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What the server says about every pane in every session, beyond the one
;;; screen a terminal is looking at.

(defun encode-log-entry (entry now)
  "A log ENTRY as it goes out: how long ago by the millisecond clock, which the
two ends do not share, and the time of day it was, which is shown as it is
and never worked out again from the age."
  (and entry
       (destructuring-bind (ms actor verb summary outcome clock) entry
         (list (max 0 (- now ms)) actor verb summary outcome clock))))

(defparameter +history-span+ (* 20 60 1000)
  "How far back a pane's history goes out with it: what a strip of its last
while is drawn from.")

(defun pane-activity (pane)
  "One line of what PANE is doing: what its agent says, or else, for a program
nobody recognised, what is running in front, or the last thing on its screen."
  (let ((agent (pane-agent pane)))
    (or (agent:agent-doing agent nil)
        (let ((front (first (pane-programs pane))))
          (and front (not (member (program-name front) +shells+ :test #'string=))
               front))
        (let ((lines (agent:agent-lines agent)))
          (and lines (string-trim " " (first (last lines))))))))

(defun pane-driver (pane)
  "The pane whose agent verbs last acted on PANE, as its address, when the last
thing that acted on it was one."
  (let ((newest (find :keys (pane-log pane) :key #'third :test-not #'eq)))
    (and newest (eq :pane (first (second newest))) (second (second newest)))))

(defun pane-row (session pane now &optional drives watcher)
  "What a client is told about PANE of SESSION, as a plist; :focus is whether
it has WATCHER's keys."
  (let ((agent (pane-agent pane))
        (server (session-server session)))
    (list :session (session-name session)
          :id (pane-id pane)
          :order (and server (position session (server-sessions server)))
          :window (nth-value 1 (window-of session pane))
          :window-name (let ((w (window-of session pane))) (and w (window-label w)))
          :at (let ((n (pane-number session pane))) (and n (1- n)))
          :label (pane-label pane)
          :says (pane-display-name pane)
          :command (program-display-name (pane-command pane))
          :kind (pane-kind pane)
          :state (agent:agent-state agent)
          :known (agent:agent-known-p agent)
          :for (agent:agent-for agent now)
          :since-clock (agent:agent-since-clock agent)
          :asks (agent:agent-asks agent nil)
          :doing (pane-activity pane)
          :history (loop :for (ms state) :in (agent:agent-history agent)
                         :for age := (- now ms)
                         :collect (list age state)
                         :until (> age +history-span+))
          :title (pane-named pane)
          :focus (and watcher (eq session (watcher-session watcher)) (eq pane (watcher-focus watcher)))
          :queued (first (pane-pending-prompt pane))
          :driven-by (pane-driver pane)
          :drives drives
          :last-input (encode-log-entry (first (pane-log pane)) now))))

(defun all-panes (server)
  (loop :for s :in (server-sessions server)
        :append (mapcar (lambda (p) (cons s p)) (session-panes s))))

(defun pane-rows (server now &optional watcher)
  (let* ((all (all-panes server))
         (drivers (mapcar (lambda (it) (pane-driver (cdr it))) all)))
    (mapcar (lambda (it)
              ;; a driver names the pane it was started in as ATTY_PANE said
              ;; then: by window and number now, by id on a pane from before
              (let ((address (pane-address-of (car it) (cdr it)))
                    (old (format nil "~A:~D" (session-name (car it)) (pane-id (cdr it)))))
                (pane-row (car it) (cdr it) now
                          (loop :for other :in all
                                :for driver :in drivers
                                :when (or (equal driver address) (equal driver old))
                                  :collect (pane-address-of (car other) (cdr other)))
                          watcher)))
            all)))

(defun all-watchers (server)
  (append (server-pending-watchers server)
          (loop :for s :in (server-sessions server) :append (session-watchers s))))

(defun blank-row-p (term y)
  (zerop (length (string-right-trim " " (term:term-dump-row-string term y)))))

(defun screen-last-rows (screen n)
  (let* ((w (tty:screen-width screen))
         (h (tty:screen-height screen))
         (bottom (or (loop :for y :from (1- h) :downto 0
                           :unless (every (lambda (ch) (char= ch #\Space))
                                          (term:row-chars (tty:screen-row screen y)))
                             :return y)
                     0))
         (top (max 0 (- bottom (1- (max 1 n)))))
         (count (1+ (- bottom top)))
         (into (tty:make-screen :width w :height count)))
    (dotimes (i count into)
      (let ((from (tty:screen-row screen (+ top i)))
            (row (tty:screen-row into i)))
        (replace (term:row-chars row) (term:row-chars from))
        (replace (term:row-faces row) (term:row-faces from))))))

(defun pane-last-rows (pane n)
  "The last N rows of PANE's screen that have anything on them, as a screen."
  (with-term (term pane)
   (let* ((w (term:term-width term))
         (h (term:term-height term))
         (bottom (or (loop :for y :from (1- h) :downto 0
                           :unless (blank-row-p term y) :return y)
                     0))
         (top (max 0 (- bottom (1- (max 1 n)))))
         (count (1+ (- bottom top)))
         (screen (tty:make-screen :width w :height count)))
    (dotimes (i count screen)
      (let ((from (term:term-grid-row term (+ top i)))
            (into (tty:screen-row screen i)))
        (replace (term:row-chars into) (term:row-chars from))
        (replace (term:row-faces into) (term:row-faces from)))))))

;;; Finding in a pane's history, and copying lines out of it. Rows are numbered
;;; from the oldest kept, so a hit has one address however far the pane is
;;; scrolled; the screen's own rows follow the scrollback's.

(defun encode-pulses (server)
  "Every pane's pulse: its session, its id and its cells, oldest first."
  (loop :for (session . pane) :in (all-panes server)
        :collect (list (session-name session) (pane-id pane)
                       (mapcar (lambda (cell) (list (car cell) (cdr cell))) (pane-pulse-now pane (now-ms))))))

(defun encode-events (server n now)
  "The last N things that happened anywhere on the server, newest first:
how long ago, the time of day, what kind of thing, which pane and the window
it is in, who did it when somebody did, and what it was."
  (let ((all (loop :for (session . pane) :in (all-panes server)
                   :for window := (nth-value 1 (window-of session pane))
                   :append (loop :for (ms clock kind actor text) :in (pane-events pane)
                                 :collect (list (max 0 (- now ms)) clock kind
                                                (session-name session) (pane-id pane) window
                                                actor text)))))
    (let ((sorted (sort all #'< :key #'first)))
      (subseq sorted 0 (min n (length sorted))))))

(defun recent-answers (server n now)
  "The last N answers anybody gave any pane, and prompts refused because a pane
was asking something, newest first: what somebody glancing at the queue wants
to know was just done for them, or by whom."
  (let ((all (loop :for (session . pane) :in (all-panes server)
                   :append (loop :for entry :in (pane-log pane)
                                 :when (or (eq :answer (third entry))
                                           (and (eq :prompt (third entry))
                                                (eq :refused (fifth entry))))
                                   :collect (list* (session-name session) (pane-id pane)
                                                   (encode-log-entry entry now))))))
    (let ((sorted (sort all #'< :key #'third)))
      (subseq sorted 0 (min n (length sorted))))))

(defun faint-p (face)
  "Whether FACE is one a program draws what it is only suggesting in: faint, or
the grey of the eight bright colours' black."
  (and face (or (term:face-faint face) (eql 8 (term:face-fg face)) (eql 90 (term:face-fg face)))))

(defun plain-row (row)
  (string-right-trim " " (coerce (loop :for x :below (term:row-width row)
                                       :collect (if (faint-p (term:row-face row x))
                                                    #\Space
                                                    (term:row-char row x)))
                                 'string)))

(defun plain-lines (term n)
  "The last N lines of TERM with what is only suggested, drawn faint, left out:
the greyed suggestion in a prompt box is not something anybody typed."
  (let* ((screen (loop :for y :below (term:term-height term)
                       :collect (plain-row (term:term-grid-row term y))))
         (screen (subseq screen 0 (1+ (or (position-if (lambda (l) (plusp (length l)))
                                                       screen :from-end t)
                                          -1))))
         (above (max 0 (- n (length screen))))
         (size (term:term-scrollback-size term)))
    (append (loop :for i :from (max 0 (- size above)) :below size
                  :collect (plain-row (term:term-scrollback-row term i)))
            (last screen (min n (length screen))))))

(defun pane-info (pane)
  "What PANE is running and how it was started: what its kind was decided from."
  (let* ((agent (pane-agent pane))
         (reader (agent:agent-reader agent)))
    (list :kind (pane-kind pane)
          :programs (pane-programs pane)
          :group (pane-group pane)
          :command (pane-command pane)
          :directory (pane-directory pane)
          :pid (and (plusp (pane-pid pane)) (pane-pid pane))
          :size (list (pane-width pane) (pane-height pane))
          :version (agent:agent-version agent)
          :reader (and reader (agent:reader-name reader)))))

(defun encode-history (agent now)
  (mapcar (lambda (it) (list (max 0 (- now (first it))) (second it)))
          (agent:agent-history agent)))
