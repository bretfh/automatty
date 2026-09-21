;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What the server tells a client about every pane in every session, beyond
;;; the one screen that client is looking at. A client that asks is kept told:
;;; each pane as a plist whenever something about it changes, and, when it has
;;; asked for them, the last rows of each pane's screen as they move.

(defparameter +screens-every+ 250
  "The least milliseconds between two screens of one pane to one client. A
pane writing without pause would otherwise be sent to everybody watching the
switchboard as fast as it writes.")

(defun input-said (entry now)
  "A log ENTRY as it goes out: how long ago rather than when, since the two
ends do not share a clock."
  (and entry
       (destructuring-bind (ms who verb summary outcome) entry
         (list (max 0 (- now ms)) who verb summary outcome))))

(defparameter +history-said+ (* 20 60 1000)
  "How far back a pane's history goes out with it: what a strip of its last
while is drawn from.")

(defun pane-doing (pane)
  "One line of what PANE is doing: what its agent says, or else, for a program
nobody recognised, what is running in front, or the last thing on its screen."
  (let ((agent (pane-agent pane))
        (term (pane-term pane)))
    (or (agent:agent-doing agent term)
        (let ((front (first (pane-programs pane))))
          (and front (not (member (program-name front) +shells+ :test #'string=))
               front))
        (let ((lines (agent:screen-lines term)))
          (and lines (string-trim " " (first (last lines))))))))

(defun driver-of (pane)
  "The pane whose agent verbs last acted on PANE, as its address, when the last
thing that acted on it was one."
  (let ((newest (find :keys (pane-log pane) :key #'third :test-not #'eq)))
    (and newest (eq :pane (first (second newest))) (second (second newest)))))

(defun pane-row (session pane now &optional drives)
  "What a client is told about PANE of SESSION, as a plist."
  (let ((agent (pane-agent pane))
        (server (session-server session)))
    (list :session (session-name session)
          :id (pane-id pane)
          :order (and server (position session (server-sessions server)))
          :at (position pane (session-panes session))
          :label (pane-label pane)
          :says (pane-says pane)
          :kind (pane-kind pane)
          :state (agent:agent-state agent)
          :for (agent:agent-for agent now)
          :asks (agent:agent-asks agent (pane-term pane))
          :doing (pane-doing pane)
          :history (loop :for (ms state) :in (agent:agent-history agent)
                         :for age := (- now ms)
                         :collect (list age state)
                         :until (> age +history-said+))
          :title (pane-named pane)
          :focus (eq pane (session-focus session))
          :queued (first (pane-queued pane))
          :driven-by (driver-of pane)
          :drives drives
          :last-input (input-said (first (pane-log pane)) now))))

(defun row-standing (row)
  "ROW without what moves every moment by itself, the times, so two rows can be
told apart by what actually changed. A history changes when the state does, and
goes out with it."
  (loop :for (key value) :on row :by #'cddr
        :unless (member key '(:for :history))
          :append (list key (if (eq key :last-input) (rest value) value))))

(defun every-pane (server)
  (loop :for s :in (server-sessions server)
        :append (mapcar (lambda (p) (cons s p)) (session-panes s))))

(defun pane-rows (server now)
  (let* ((all (every-pane server))
         (drivers (mapcar (lambda (it) (driver-of (cdr it))) all)))
    (mapcar (lambda (it)
              (let ((address (format nil "~A:~D" (session-name (car it)) (pane-id (cdr it)))))
                (pane-row (car it) (cdr it) now
                          (loop :for other :in all
                                :for driver :in drivers
                                :when (equal driver address)
                                  :collect (format nil "~A:~D" (session-name (car other))
                                                   (pane-id (cdr other)))))))
            all)))

(defun every-watcher (server)
  (append (server-knocking server)
          (loop :for s :in (server-sessions server) :append (session-watchers s))))

(defun tell-the-panes (server watcher now)
  "Tell WATCHER about every pane that changed since it was last told, and about
every pane that is gone."
  (let ((told (watcher-panes-told watcher))
        (seen nil))
    (dolist (row (pane-rows server now))
      (let ((key (cons (getf row :session) (getf row :id)))
            (standing (row-standing row)))
        (push key seen)
        (unless (equal standing (gethash key told))
          (setf (gethash key told) standing)
          (tell watcher (cons :pane row)))))
    (loop :for key :being :the :hash-keys :of told
          :unless (member key seen :test #'equal)
            :collect key :into gone
          :finally (dolist (key gone)
                     (remhash key told)
                     (tell watcher (list :pane-gone (car key) (cdr key)))))))

(defun blank-row-p (term y)
  (zerop (length (string-right-trim " " (term:term-dump-row-string term y)))))

(defun pane-screen-said (pane n)
  "The last N rows of PANE's screen that have anything on them, as cells the way
a frame goes out: (width said faces)."
  (let* ((term (pane-term pane))
         (w (term:term-width term))
         (h (term:term-height term))
         (bottom (or (loop :for y :from (1- h) :downto 0
                           :unless (blank-row-p term y) :return y)
                     0))
         (top (max 0 (- bottom (1- (max 1 n)))))
         (count (1+ (- bottom top)))
         (screen (tty:make-screen :width w :height count)))
    (dotimes (i count)
      (let ((from (term:term-grid-row term (+ top i)))
            (into (tty:screen-row screen i)))
        (replace (term:row-chars into) (term:row-chars from))
        (replace (term:row-faces into) (term:row-faces from))))
    (multiple-value-bind (said faces)
        (runs-said screen (loop :for y :below count :collect (tty:make-run y 0 w)))
      (list w said faces))))

(defun tell-the-screens (server watcher now)
  "Send WATCHER the screen of every pane that moved since it last had it, no
more often than +SCREENS-EVERY+."
  (let ((sent (watcher-screens-told watcher))
        (n (watcher-screen-rows watcher)))
    (dolist (it (every-pane server))
      (destructuring-bind (session . pane) it
        (let* ((key (cons (session-name session) (pane-id pane)))
               (last (gethash key sent)))
          (when (or (null last)
                    (and (> (pane-moved-at pane) last)
                         (>= (- now last) +screens-every+)))
            (setf (gethash key sent) now)
            (tell watcher (list* :pane-screen (session-name session) (pane-id pane)
                                 (pane-screen-said pane n)))))))))

(defun tell-the-watching (server now)
  "Everybody who asked to be kept told about the panes, told."
  (dolist (w (every-watcher server))
    (when (wire-open (watcher-wire w))
      (when (watcher-watch-panes w) (tell-the-panes server w now))
      (when (watcher-screen-rows w) (tell-the-screens server w now)))))

(defun answer-a-pane (server watcher name id n &optional caller)
  "Type the digit N into a blocked pane, which is how a numbered dialog is
answered. A pane that is not blocked is not typed into: an answer to a question
that has since gone would land in whatever is there now."
  (let* ((pane (pane-called server name id))
         (agent (and pane (pane-agent pane)))
         (asks (and pane (agent:agent-asks agent (pane-term pane))))
         (outcome (cond ((null pane) :gone)
                        ((not (eq :blocked (agent:agent-state agent))) :not-blocked)
                        ((and asks (not (assoc n (getf asks :options)))) :no-such-option)
                        (t t))))
    (when pane
      (pane-logged pane (now-ms) (who-of watcher caller) :answer
                   (let ((option (assoc n (getf asks :options))))
                     (if option (format nil "~D ~A" n (second option)) (princ-to-string n)))
                   (if (eq outcome t) t :refused)))
    (when (eq outcome t)
      (pane-say pane (princ-to-string n)))
    (tell watcher (list :answered name id n outcome))))

(defun focus-a-pane (server watcher name id)
  "Put WATCHER on the session called NAME, when it is not there already, with
pane ID in focus."
  (let* ((session (session-named server name))
         (pane (and session (find id (session-panes session) :key #'pane-id))))
    (when pane
      (unless (eq session (watcher-session watcher))
        (join-session server watcher session))
      (focus-on session pane))
    (tell watcher (list :focused name id (and pane t)))))

(defun longest-blocked (server)
  "The pane that has been blocked longest in any session, with its session, or
nil when nothing is."
  (let ((now (now-ms))
        (best nil) (best-for -1))
    (dolist (it (every-pane server) (and best (values (cdr best) (car best))))
      (let* ((agent (pane-agent (cdr it)))
             (for (or (agent:agent-for agent now) 0)))
        (when (and (eq :blocked (agent:agent-state agent)) (> for best-for))
          (setf best it best-for for))))))

(defun take-to-the-blocked (server watcher)
  (multiple-value-bind (pane session) (longest-blocked server)
    (if pane
        (focus-a-pane server watcher (session-name session) (pane-id pane))
        (tell watcher (list :say "nothing needs you")))))

(defun zoom-a-pane (server watcher &optional name id)
  "Give the pane NAME:ID, or the focused one, the whole of its session; or give
it back when it already has it."
  (when name (focus-a-pane server watcher name id))
  (let* ((session (if name (session-named server name) (watcher-session watcher)))
         (pane (and session (session-focus session))))
    (when pane
      (setf (session-zoomed session)
            (if (eq pane (session-zoomed session)) nil pane))
      (dolist (w (session-watchers session)) (setf (watcher-behind w) t)))))

(defun read-a-pane (server watcher name id)
  "What a pane holds, scrollback and all, for somebody to read in a note."
  (let ((pane (pane-called server name id)))
    (tell watcher (list :read-it name id
                        (and pane (agent:last-lines (pane-term pane) 500))))))

(defun prompt-a-pane (server pane text who)
  "Give PANE's program TEXT as new work: pasted, when it takes pastes, and
entered a moment later. Refused, and answers :blocked, while it is asking
something: a prompt then would be taken for the answer."
  (cond
    ((or (eq :blocked (agent:agent-state (pane-agent pane)))
         (agent:screen-blocked-p (pane-term pane)))
     (pane-logged pane (now-ms) who :prompt (summarised text) :refused)
     :blocked)
    (t (pane-logged pane (now-ms) who :prompt (summarised text))
       (agent:agent-prompted (pane-agent pane) (now-ms))
       (pane-say pane (if (term:term-bracketed-paste (pane-term pane))
                          (concatenate 'string (string #\Escape) "[200~"
                                       text (string #\Escape) "[201~")
                          text))
       (later server +enter-after+
              (lambda () (pane-say pane (string #\Return))))
       t)))

(defun prompt-when-idle (server pane text who)
  "Prompt PANE now when it is idle, and otherwise when it next is: :queued. A
pane that is asking something is refused as a prompt to it now would be."
  (case (agent:agent-state (pane-agent pane))
    (:blocked (prompt-a-pane server pane text who))
    (:working (setf (pane-queued pane) (list text who)) :queued)
    (t (prompt-a-pane server pane text who))))

(defun send-what-waited (server pane)
  "PANE has gone idle: what was queued for it goes now."
  (let ((queued (pane-queued pane)))
    (when (and queued (eq :idle (agent:agent-state (pane-agent pane))))
      (setf (pane-queued pane) nil)
      (prompt-a-pane server pane (first queued) (second queued)))))

(defun lately (server n now)
  "The last N answers anybody gave any pane, and prompts refused because a pane
was asking something, newest first: what somebody glancing at the queue wants
to know was just done for them, or by whom."
  (let ((all (loop :for (session . pane) :in (every-pane server)
                   :append (loop :for entry :in (pane-log pane)
                                 :when (or (eq :answer (third entry))
                                           (and (eq :prompt (third entry))
                                                (eq :refused (fifth entry))))
                                   :collect (list* (session-name session) (pane-id pane)
                                                   (input-said entry now))))))
    (let ((sorted (sort all #'< :key #'third)))
      (subseq sorted 0 (min n (length sorted))))))

(defun history-said (agent now)
  (mapcar (lambda (it) (list (max 0 (- now (first it))) (second it)))
          (agent:agent-history agent)))
