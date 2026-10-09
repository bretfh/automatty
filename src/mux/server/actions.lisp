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
         (seen (and reader (with-term (term pane) (agent:observe reader term))))
         (keys (and seen (agent:action-keys reader seen action argument))))
    (cond
     ((null pane) (send-message watcher (list :agent-acted name id action :gone)))
     ((null reader) (send-message watcher (list :agent-acted name id action :no-reader)))
     ((null keys)
      (send-message watcher (list :agent-acted name id action :not-offered
                                  (and seen (agent:offered reader seen)) (getf seen :screen))))
     (t
      (when (eq action :submit)
        (on-pane pane (lambda ()
                        (agent:agent-prompted agent (floor (monotonic-ns) 1000000))
                        (pane-look-soon pane))))
      (loop :for chunk :in keys
            :for at :from 0 :by +enter-delay+
            :do (let ((chunk chunk))
                  (if (zerop at)
                      (pane-write pane chunk)
                    (schedule-task server at (lambda () (pane-write pane chunk))))))
      (let ((deadline (+ (floor (monotonic-ns) 1000000) +action-timeout+
                         (* +enter-delay+ (length keys)))))
        (labels ((check ()
                        (let ((now-seen (with-term (term pane) (agent:observe reader term))))
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
that has since gone would land in whatever is there now. Answers t, or
:gone, :not-blocked or :no-such-option."
  (let* ((pane (find-pane server name id))
         (agent (and pane (pane-agent pane)))
         (asks (and pane (agent:agent-asks agent nil)))
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
             (seen (and reader (with-term (term pane) (agent:observe reader term))))
             (keys (or (and seen (agent:action-keys reader seen :choose n))
                       (list (princ-to-string n)))))
        (loop :for chunk :in keys
              :for at :from 0 :by +enter-delay+
              :do (let ((chunk chunk))
                    (if (zerop at)
                        (pane-write pane chunk)
                      (schedule-task server at (lambda () (pane-write pane chunk))))))))
    (unless (eq outcome t)
      (show-note watcher "not answered"
                 (format nil "~A:~D was not answered ~D: ~(~A~)." name id n
                         (case outcome
                           (:not-blocked "it is not asking anything now")
                           (:no-such-option "it has no such answer")
                           (:gone "it is gone")
                           (t outcome)))))
    (send-message watcher (list :answered name id n outcome))
    outcome))

(defun focus-pane (server watcher name id)
  "Put WATCHER on the session called NAME, when it is not there already, with
pane ID in focus."
  (when (watcher-elsewhere-p watcher)
    (return-from focus-pane
      (on-watcher watcher (lambda () (focus-pane server watcher name id)))))
  (let* ((session (session-named server name))
         (pane (and session (find id (session-panes session) :key #'pane-id))))
    (when pane
      (unless (eq session (watcher-session watcher))
        (join-session server watcher session))
      (setf (watcher-focus watcher) pane))
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
                         (show-note watcher "atty" "nothing needs you" :face :accent))))

(defun session-zoom-pane (server watcher &optional name id)
  "Give the pane NAME:ID, or the focused one, the whole of what WATCHER shows;
or give it back when it already has it."
  (when name (focus-pane server watcher name id))
  (let ((pane (watcher-focus watcher)))
    (when pane
      (setf (watcher-zoomed watcher)
            (if (eq pane (watcher-zoomed watcher)) nil pane)))))

(defun pane-row-count (pane)
  "How many rows PANE has all told: what is kept behind the screen and the screen."
  (+ (pane-history pane) (pane-height pane)))

(defun pane-row-text (pane a)
  "Row A of PANE as a string, oldest kept row first, then the screen's."
  (with-term (term pane)
    (let ((kept (pane-history pane)))
      (cond ((< a 0) "")
            ((< a kept) (or (term:term-scrollback-row-string term a) ""))
            ((< a (pane-row-count pane)) (term:term-dump-row-string term (- a kept)))
            (t "")))))

(defun scroll-to-row (watcher pane a)
  "Scroll PANE for WATCHER so row A is on the screen, near the middle."
  (let ((height (pane-height pane)))
    (scroll-to watcher pane (- (pane-history pane) a (- (floor height 2))))))

(defun search-hits (pane query)
  "Every row of PANE with QUERY in it, oldest first: (row start end)."
  (let ((q (string-downcase query)))
    (on-pane pane (lambda ()
                    (loop :for a :below (pane-row-count pane)
                          :for text := (string-downcase (pane-row-text pane a))
                          :for at := (and (plusp (length q)) (search q text))
                          :when at :collect (list a at (+ at (length q))))))))

(defun search-pane (server watcher name id query way)
  "Find QUERY in the pane called NAME:ID, or the focus's when they are nil, for
WATCHER. :HERE is a new query, found from the newest hit; :NEXT is the next
older hit, :BACK the next newer; :CLEAR forgets it. The pane scrolls to the hit
and the watcher is told how many there are and which this is."
  (let ((pane (or (and name (find-pane server name id))
                  (watcher-focus watcher))))
    (when pane
      (let* ((look (watcher-look watcher pane))
             (find (look-find look)))
        (flet ((at (n) (list :query (getf find :query) :hits (getf find :hits) :at n)))
          (ecase (if (consp way) (first way) way)
            (:clear (setf (look-find look) nil (look-selecting look) nil))
            (:here
             (let ((hits (search-hits pane query)))
               (setf (look-find look)
                     (cond (hits (list :query query :hits hits :at (1- (length hits))))
                           ((plusp (length query)) (list :query query :hits nil :at nil))))))
            ((:next :back)
             (when (and find (getf find :hits))
               (let* ((n (length (getf find :hits)))
                      (to (+ (getf find :at) (if (eq way :next) -1 1))))
                 (setf (look-find look) (at (mod to n))))))
            (:row
             ;; (:row n): the hit on row n, when there is one
             (let ((to (and find (position (second way) (getf find :hits) :key #'first))))
               (when to (setf (look-find look) (at to)))))))
        (setf (watcher-behind watcher) t)
        (let* ((find (look-find look))
               (hits (getf find :hits))
               (at (getf find :at))
               (hit (and at (nth at hits))))
          (when hit (scroll-to-row watcher pane (first hit)))
          ;; the hits nearest the one gone to go out with their text, for a
          ;; list of them: the nearest two hundred, and which of those it is
          ;; a clear is not a find: nothing is said back for one
          (unless (eq way :clear)
            (let* ((n (length hits))
                   (from (max 0 (- (or at 0) 100)))
                   (to (min n (+ (or at 0) 100)))
                   (said (on-pane pane
                                  (lambda ()
                                    (loop :for h :in (subseq hits from to)
                                          :collect (list (first h) (second h) (third h)
                                                         (string-right-trim " " (pane-row-text pane (first h)))))))))
              (found-in-pane watcher n said (and at (- at from))))))))))

(defun select-pane-rows (watcher what)
  "Lines out of the pane with WATCHER's focus. :START marks the top row shown
as one end of a selection; :COPY sends the lines from that mark to the other
end of what is shown now, or the screen when nothing was marked."
  (let ((pane (watcher-focus watcher)))
    (when pane
      (let ((look (watcher-look watcher pane))
            (top (top-row watcher pane))
            (height (pane-height pane)))
        (ecase (if (consp what) (first what) what)
          (:line
           ;; (:line row): that one row, as it is
           (copied watcher (string-right-trim " " (pane-row-text pane (second what)))))
          (:start (setf (look-selecting look) top))
          (:copy
           (let* ((mark (or (look-selecting look) top))
                  (from (min mark top))
                  (to (+ (max mark top) height)))
             (copied watcher
                     (format nil "~{~A~^~%~}"
                             (on-pane pane
                                      (lambda ()
                                        (loop :for a :from from :below (min to (pane-row-count pane))
                                              :collect (string-right-trim " " (pane-row-text pane a)))))))
             (setf (look-selecting look) nil))))
        (setf (watcher-behind watcher) t)))))

(defun copied (watcher text)
  "TEXT to the clipboard of WATCHER's terminal, and a note saying so."
  (send-message watcher (list :copied text))
  (show-note watcher "copied" (format nil "~D line~:P" (1+ (count #\Newline text))) :face :accent))

(defun read-pane (server watcher name id)
  "What a pane holds, the end of it, for somebody to read in a note."
  (let ((pane (find-pane server name id)))
    (show-note watcher (format nil "~A:~D" name id)
               (format nil "~{~A~%~}"
                       (last (and pane (with-term (term pane) (agent:last-lines term 500)))
                             (max 1 (- (watcher-rows watcher) 3))))
               :face :accent)))

(defun prompt-pane (server pane text actor)
  "Give PANE's program TEXT as new work: pasted, when it takes pastes, and
entered a moment later. Refused, and answers :blocked, while it is asking
something: a prompt then would be taken for the answer."
  (cond
   ((or (eq :blocked (agent:agent-state (pane-agent pane)))
        (with-term (term pane) (agent:screen-blocked-p term)))
    (pane-push-log pane (now-ms) actor :prompt (summarize-text text) :refused)
    :blocked)
   (t (pane-push-log pane (now-ms) actor :prompt (summarize-text text))
      (on-pane pane (lambda ()
                      (agent:agent-prompted (pane-agent pane) (now-ms))
                      (pane-look-soon pane)))
      ;; one line is typed, the way a person would give it. A coding agent
      ;; can take what arrives as a paste for something pasted in rather than
      ;; asked for, and decline to act on it: the field test saw exactly that.
      ;; More than one line is pasted, since a newline typed would send the
      ;; first line on its own.
      (pane-write pane (if (and (with-term (term pane) (term:term-bracketed-paste term))
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
        (:working (on-pane pane (lambda () (setf (pane-pending-prompt pane) (list text actor)))) :queued)
        (t (prompt-pane server pane text actor))))

(defun send-pending-prompt (server pane)
  "PANE has gone idle: what was queued for it goes now."
  (when (eq :idle (agent:agent-state (pane-agent pane)))
    (let ((queued (on-pane pane (lambda () (shiftf (pane-pending-prompt pane) nil)))))
      (when queued
        (prompt-pane server pane (first queued) (second queued))))))

(defun spawn-pane (server name command directory label &optional window)
  "A pane running COMMAND in the session called NAME, beside the pane the
command line was run in when that is there, else beside the focus of whoever
was there last, or the first pane of that session when there is no such session
yet. WINDOW puts it in that window of the session instead, by number, or in a
new window when it is :new. Nobody is shown it who was not already. Answers the
pane."
  (let* ((session (session-named server name))
         (caller (and session *caller*
                      (multiple-value-bind (pane there) (pane-by-address server *caller*)
                        (and (eq there session) pane))))
         (pane (cond
                 ((null session)
                  (first (session-panes (add-session server command :name name :directory directory))))
                 ((eq window :new)
                  (let* ((like (or caller (first (session-panes session))))
                         (it (make-pane command :rows (pane-height like) :cols (pane-width like)
                                                :directory (or directory (pane-directory like)))))
                    (session-add-window session it)
                    it))
                 (t
                  (let* ((w (if (integerp window)
                                (or (session-nth-window session window)
                                    (error "~A has no window ~D" name window))
                                (or (and caller (window-of session caller))
                                    (view-shown-window session (seed-for session nil)))))
                         (beside (or (and caller (member caller (window-panes w)) caller)
                                     (and (not (integerp window))
                                          (view-focus-in session (seed-for session nil) w))
                                     (car (last (window-panes w)))))
                         (it (make-pane command :rows (pane-height beside) :cols (pane-width beside)
                                                :directory (or directory (pane-directory beside)))))
                    (or (session-split session beside :across it)
                        (error "~A changed under the pane being made" name)))))))
    (when label (on-pane pane (lambda () (setf (pane-label pane) label))))
    pane))

(defun go-to (watcher name &optional n)
  "WATCHER onto the session called NAME, showing its window N when there is one."
  (when (watcher-elsewhere-p watcher)
    (return-from go-to
      (on-watcher watcher (lambda () (go-to watcher name n)))))
  (let* ((server (watcher-server watcher))
         (session (and server (session-named server name))))
    (when session
      (unless (eq session (watcher-session watcher))
        (join-session server watcher session))
      (when n (watcher-select-window watcher n)))
    session))

(defun name-pane (server name id label)
  (let ((pane (find-pane server name id)))
    (when pane
      (on-pane pane (lambda ()
                      (setf (pane-label pane) (and (stringp label)
                                                   (plusp (length (string-trim " " label)))
                                                   (string-trim " " label))
                            (pane-touched pane) (now-ms))))
      (dolist (w (session-watchers (session-named server name)))
        (draw-again w)))
    pane))

(defun prompt-named (watcher name id text &key when-idle)
  "TEXT as new work for the pane NAME:ID, now or once it is idle; a note to
WATCHER when it could not be."
  (let* ((server (watcher-server watcher))
         (pane (find-pane server name id))
         (said (cond ((null pane) :gone)
                     (when-idle (prompt-when-idle server pane text (actor-of watcher)))
                     (t (prompt-pane server pane text (actor-of watcher))))))
    (unless (member said '(t :queued))
      (show-note watcher "not prompted"
                 (format nil "~A:~D ~A" name id
                         (if (eq said :blocked)
                             "is asking something; answer it, not a prompt."
                             "is gone."))))
    said))

(defun close-pane-named (server name id)
  (let* ((session (session-named server name))
         (pane (and session (find id (session-panes session) :key #'pane-id))))
    (when pane (session-close-pane session pane))))
