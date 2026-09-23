;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defparameter +action-timeout+ 5000)

(defparameter +action-settle+ 400)

(defun act-on-pane (server watcher name id action argument &optional until)
  (let* ((now (floor (monotonic-ns) 1000000))
         (until (or until (+ now +action-timeout+)))
         (pane (find-pane server name id))
         (still (and pane (agent:agent-still-since (pane-agent pane)))))
    (if (and still (not (eq action :interrupt)) (< (- now still) +action-settle+) (< now until))
        (schedule-task server 100 (lambda () (act-on-pane server watcher name id action argument until)))
        (perform-action server watcher name id action argument))))

(defun perform-action (server watcher name id action argument)
  (let* ((pane (find-pane server name id))
         (agent (and pane (pane-agent pane)))
         (reader (and agent (agent:agent-reader agent)))
         (seen (and reader (agent:observe reader (pane-term pane))))
         (keys (and seen (agent:action-keys reader seen action argument))))
    (cond
      ((null pane) (send-message watcher (list :agent-acted name id action :gone)))
      ((null reader) (send-message watcher (list :agent-acted name id action :no-reader)))
      ((null keys)
       (send-message watcher (list :agent-acted name id action :not-offered
                           (and seen (agent:offered reader seen)) (getf seen :screen))))
      (t
       (when (eq action :submit)
         (agent:agent-prompted agent (floor (monotonic-ns) 1000000)))
       (loop :for chunk :in keys
             :for at :from 0 :by +enter-delay+
             :do (let ((chunk chunk))
                   (if (zerop at)
                       (pane-write pane chunk)
                       (schedule-task server at (lambda () (pane-write pane chunk))))))
       (let ((deadline (+ (floor (monotonic-ns) 1000000) +action-timeout+
                          (* +enter-delay+ (length keys)))))
         (labels ((check ()
                    (let ((now-seen (agent:observe reader (pane-term pane))))
                      (cond ((not (equal now-seen seen))
                             (send-message watcher (list :agent-acted name id action :done
                                                 (getf now-seen :screen))))
                            ((> (floor (monotonic-ns) 1000000) deadline)
                             (send-message watcher (list :agent-acted name id action :failed
                                                 (getf seen :screen))))
                            (t (schedule-task server 100 #'check))))))
           (schedule-task server (+ 100 (* +enter-delay+ (1- (length keys)))) #'check)))))))

(defun agent-row (session pane)
  (let ((agent (pane-agent pane)))
    (list (session-name session) (pane-id pane)
          (pane-kind pane) (agent:agent-state agent)
          (agent:agent-reason agent) (agent:agent-turn agent))))

(defun agent-rows (server &optional session)
  (loop :for s :in (if session (list session) (server-sessions server))
        :append (mapcar (lambda (p) (agent-row s p)) (session-panes s))))

(defun answer-pane (server watcher name id n &optional caller-pane)
  "Type the digit N into a blocked pane, which is how a numbered dialog is
answered. A pane that is not blocked is not typed into: an answer to a question
that has since gone would land in whatever is there now."
  (let* ((pane (find-pane server name id))
         (agent (and pane (pane-agent pane)))
         (asks (and pane (agent:agent-asks agent (pane-term pane))))
         (outcome (cond ((null pane) :gone)
                        ((not (eq :blocked (agent:agent-state agent))) :not-blocked)
                        ((and asks (not (assoc n (getf asks :options)))) :no-such-option)
                        (t t))))
    (when pane
      (pane-push-log pane (now-ms) (actor-of watcher caller-pane) :answer
                   (let ((option (assoc n (getf asks :options))))
                     (if option (format nil "~D ~A" n (second option)) (princ-to-string n)))
                   (if (eq outcome t) t :refused)))
    (when (eq outcome t)
      (let* ((reader (agent:agent-reader agent))
             (seen (and reader (agent:observe reader (pane-term pane))))
             (keys (or (and seen (agent:action-keys reader seen :choose n))
                       (list (princ-to-string n)))))
        (loop :for chunk :in keys
              :for at :from 0 :by +enter-delay+
              :do (let ((chunk chunk))
                    (if (zerop at)
                        (pane-write pane chunk)
                        (schedule-task server at (lambda () (pane-write pane chunk))))))))
    (send-message watcher (list :answered name id n outcome))))

(defun focus-pane (server watcher name id)
  "Put WATCHER on the session called NAME, when it is not there already, with
pane ID in focus."
  (let* ((session (session-named server name))
         (pane (and session (find id (session-panes session) :key #'pane-id))))
    (when pane
      (unless (eq session (watcher-session watcher))
        (join-session server watcher session))
      (session-focus-pane session pane))
    (send-message watcher (list :focused name id (and pane t)))))

(defun oldest-blocked-pane (server)
  "The pane that has been blocked longest in any session, with its session, or
nil when nothing is."
  (let ((now (now-ms))
        (best nil) (best-for -1))
    (dolist (it (all-panes server) (and best (values (cdr best) (car best))))
      (let* ((agent (pane-agent (cdr it)))
             (for (or (agent:agent-for agent now) 0)))
        (when (and (eq :blocked (agent:agent-state agent)) (> for best-for))
          (setf best it best-for for))))))

(defun focus-oldest-blocked (server watcher)
  (multiple-value-bind (pane session) (oldest-blocked-pane server)
    (if pane
        (focus-pane server watcher (session-name session) (pane-id pane))
        (send-message watcher (list :say "nothing needs you")))))

(defun session-zoom-pane (server watcher &optional name id)
  "Give the pane NAME:ID, or the focused one, the whole of its session; or give
it back when it already has it."
  (when name (focus-pane server watcher name id))
  (let* ((session (if name (session-named server name) (watcher-session watcher)))
         (pane (and session (session-focus session))))
    (when pane
      (setf (session-zoomed session)
            (if (eq pane (session-zoomed session)) nil pane))
      (dolist (w (session-watchers session)) (setf (watcher-behind w) t)))))

(defun pane-row-count (pane)
  "How many rows PANE has all told: what is kept behind the screen and the screen."
  (+ (pane-history pane) (term:term-height (pane-term pane))))

(defun pane-row-text (pane a)
  "Row A of PANE as a string, oldest kept row first, then the screen's."
  (let* ((term (pane-term pane))
         (kept (pane-history pane)))
    (cond ((< a 0) "")
          ((< a kept) (or (term:term-scrollback-row-string term a) ""))
          ((< a (pane-row-count pane)) (term:term-dump-row-string term (- a kept)))
          (t ""))))

(defun pane-top-row (pane)
  "Which row is at the top of what PANE shows."
  (- (pane-history pane) (pane-scrolled pane)))

(defun pane-scroll-to-row (pane a)
  "Scroll PANE so row A is on the screen, near the middle."
  (let ((height (term:term-height (pane-term pane))))
    (pane-scroll-to pane (- (pane-history pane) a (- (floor height 2))))))

(defun search-hits (pane query)
  "Every row of PANE with QUERY in it, oldest first: (row start end)."
  (let ((q (string-downcase query)))
    (loop :for a :below (pane-row-count pane)
          :for text := (string-downcase (pane-row-text pane a))
          :for at := (and (plusp (length q)) (search q text))
          :when at :collect (list a at (+ at (length q))))))

(defun search-pane (server watcher name id query way)
  "Find QUERY in the pane called NAME:ID, or the focus's when they are nil.
:HERE is a new query, found from the newest hit; :NEXT is the next older hit,
:BACK the next newer; :CLEAR forgets it. The pane scrolls to the hit and the
watcher is told how many there are and which this is."
  (let ((pane (or (and name (find-pane server name id))
                  (and (watcher-session watcher) (session-focus (watcher-session watcher))))))
    (when pane
      (let ((find (pane-find pane)))
        (ecase (if (consp way) (first way) way)
          (:clear (setf (pane-find pane) nil (pane-selecting pane) nil (pane-dirty pane) t))
          (:here
           (let ((hits (search-hits pane query)))
             (setf (pane-find pane)
                   (and hits (list :query query :hits hits :at (1- (length hits))))
                   (pane-dirty pane) t)
             (when (and (null hits) (plusp (length query)))
               (setf (pane-find pane) (list :query query :hits nil :at nil)))))
          ((:next :back)
           (when (and find (getf find :hits))
             (let* ((n (length (getf find :hits)))
                    (at (getf find :at))
                    (to (+ at (if (eq way :next) -1 1))))
               (setf (getf (pane-find pane) :at) (mod to n)
                     (pane-dirty pane) t))))
          (:row
           ;; (:row n): the hit on row n, when there is one
           (let ((to (and find (position (second way) (getf find :hits) :key #'first))))
             (when to (setf (getf (pane-find pane) :at) to (pane-dirty pane) t)))))
        (let* ((find (pane-find pane))
               (hits (getf find :hits))
               (at (getf find :at))
               (hit (and at (nth at hits))))
          (when hit (pane-scroll-to-row pane (first hit)))
          (dolist (w (session-watchers (pane-session server pane)))
            (setf (watcher-behind w) t))
          ;; the hits nearest the one gone to go out with their text, for a
          ;; list of them: the nearest two hundred, and which of those it is
          ;; a clear is not a find: nothing is said back for one
          (unless (eq way :clear)
           (let* ((n (length hits))
                 (from (max 0 (- (or at 0) 100)))
                 (to (min n (+ (or at 0) 100)))
                 (said (loop :for h :in (subseq hits from to)
                             :collect (list (first h) (second h) (third h)
                                            (string-right-trim " " (pane-row-text pane (first h)))))))
            (send-message watcher (list :found (session-name (pane-session server pane)) (pane-id pane)
                                n (and at (- n at)) (and hit (first hit))
                                said (and at (- at from)))))))))))

(defun pane-session (server pane)
  (find-if (lambda (s) (member pane (session-panes s))) (server-sessions server)))

(defun select-pane-rows (server watcher what)
  "Lines out of the pane with WATCHER's focus. :START marks the top row shown
as one end of a selection; :COPY sends the lines from that mark to the other
end of what is shown now, or the screen when nothing was marked."
  (let* ((session (watcher-session watcher))
         (pane (and session (session-focus session))))
    (when pane
      (let ((top (pane-top-row pane))
            (height (term:term-height (pane-term pane))))
        (ecase (if (consp what) (first what) what)
          (:line
           ;; (:line row): that one row, as it is
           (send-message watcher (list :copied (string-right-trim " " (pane-row-text pane (second what))))))
          (:start (setf (pane-selecting pane) top (pane-dirty pane) t))
          (:copy
           (let* ((mark (or (pane-selecting pane) top))
                  (from (min mark top))
                  (to (+ (max mark top) height)))
             (send-message watcher
                   (list :copied
                         (format nil "~{~A~^~%~}"
                                 (loop :for a :from from :below (min to (pane-row-count pane))
                                       :collect (string-right-trim " " (pane-row-text pane a))))))
             (setf (pane-selecting pane) nil (pane-dirty pane) t))))
        (dolist (w (session-watchers session)) (setf (watcher-behind w) t))))))

(defun read-pane (server watcher name id)
  "What a pane holds, scrollback and all, for somebody to read in a note."
  (let ((pane (find-pane server name id)))
    (send-message watcher (list :read-it name id
                        (and pane (agent:last-lines (pane-term pane) 500))))))

(defun prompt-pane (server pane text actor)
  "Give PANE's program TEXT as new work: pasted, when it takes pastes, and
entered a moment later. Refused, and answers :blocked, while it is asking
something: a prompt then would be taken for the answer."
  (cond
    ((or (eq :blocked (agent:agent-state (pane-agent pane)))
         (agent:screen-blocked-p (pane-term pane)))
     (pane-push-log pane (now-ms) actor :prompt (summarize-text text) :refused)
     :blocked)
    (t (pane-push-log pane (now-ms) actor :prompt (summarize-text text))
       (agent:agent-prompted (pane-agent pane) (now-ms))
       ;; one line is typed, the way a person would give it. A coding agent
       ;; can take what arrives as a paste for something pasted in rather than
       ;; asked for, and decline to act on it: the field test saw exactly that.
       ;; More than one line is pasted, since a newline typed would send the
       ;; first line on its own.
       (pane-write pane (if (and (term:term-bracketed-paste (pane-term pane))
                               (or (find-if (lambda (c) (member c '(#\Newline #\Return))) text)
                                   (and (agent:agent-reader (pane-agent pane))
                                        (not (agent:agent-submits-typed-p (pane-agent pane))))))
                          (concatenate 'string (string #\Escape) "[200~"
                                       text (string #\Escape) "[201~")
                          text))
       (schedule-task server +enter-delay+
              (lambda () (pane-write pane (string #\Return))))
       t)))

(defun prompt-when-idle (server pane text actor)
  "Prompt PANE now when it is idle, and otherwise when it next is: :queued. A
pane that is asking something is refused as a prompt to it now would be."
  (case (agent:agent-state (pane-agent pane))
    (:blocked (prompt-pane server pane text actor))
    (:working (setf (pane-pending-prompt pane) (list text actor)) :queued)
    (t (prompt-pane server pane text actor))))

(defun send-pending-prompt (server pane)
  "PANE has gone idle: what was queued for it goes now."
  (let ((queued (pane-pending-prompt pane)))
    (when (and queued (eq :idle (agent:agent-state (pane-agent pane))))
      (setf (pane-pending-prompt pane) nil)
      (prompt-pane server pane (first queued) (second queued)))))

(defun spawn-pane (server name command directory label &optional window)
  "A pane running COMMAND in the session called NAME, beside the one with the
focus there, or the first pane of that session when there is no such session
yet. WINDOW puts it in that window of the session instead, by number, or in a
new window when it is :new. Answers the pane."
  (let* ((session (session-named server name))
         (pane (if session
                   (cond
                     ((eq window :new)
                      (window-focus (session-add-window session command directory nil)))
                     ((and (integerp window) (session-nth-window session window)
                           (not (eq (session-nth-window session window) (session-window session))))
                      ;; beside the focus of that window, without showing it
                      (let* ((w (session-nth-window session window))
                             (focus (window-focus w))
                             (it (make-pane command
                                            :rows (term:term-height (pane-term focus))
                                            :cols (term:term-width (pane-term focus))
                                            :directory (or directory (pane-directory focus)))))
                        (setf (window-layout w) (layout-insert (window-layout w) focus :across it))
                        (pane-start it :environment (pane-environment session it))
                        it))
                     (t
                   ;; beside the focus, the way a split puts one, but running
                   ;; what it was asked to; the focus stays where somebody put it
                   (let* ((focus (session-focus session))
                          (it (make-pane command
                                         :rows (term:term-height (pane-term focus))
                                         :cols (term:term-width (pane-term focus))
                                         :directory (or directory (pane-directory focus)))))
                     (setf (session-layout session)
                           (layout-insert (session-layout session) focus :across it))
                     (session-compose session)
                     (pane-start it :environment (pane-environment session it))
                     it)))
                   (session-focus (add-session server command :name name
                                                              :directory directory)))))
    (when label (setf (pane-label pane) label))
    (dolist (w (session-watchers (session-named server name))) (setf (watcher-behind w) t))
    pane))
