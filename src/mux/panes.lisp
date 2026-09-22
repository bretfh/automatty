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
  "A log ENTRY as it goes out: how long ago by the millisecond clock, which the
two ends do not share, and the time of day it was, which is shown as it is
and never worked out again from the age."
  (and entry
       (destructuring-bind (ms who verb summary outcome clock) entry
         (list (max 0 (- now ms)) who verb summary outcome clock))))

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
          :window (nth-value 1 (window-of session pane))
          :window-name (let ((w (window-of session pane))) (and w (window-label w)))
          :at (let ((n (pane-number session pane))) (and n (1- n)))
          :label (pane-label pane)
          :says (pane-says pane)
          :command (shortened (pane-command pane))
          :kind (pane-kind pane)
          :state (agent:agent-state agent)
          :known (agent:agent-known-p agent)
          :for (agent:agent-for agent now)
          :since-clock (agent:agent-since-clock agent)
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
              ;; a driver names the pane it was started in as ATTY_PANE said
              ;; then: by window and number now, by id on a pane from before
              (let ((address (pane-address-of (car it) (cdr it)))
                    (old (format nil "~A:~D" (session-name (car it)) (pane-id (cdr it)))))
                (pane-row (car it) (cdr it) now
                          (loop :for other :in all
                                :for driver :in drivers
                                :when (or (equal driver address) (equal driver old))
                                  :collect (pane-address-of (car other) (cdr other))))))
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
      (let* ((reader (agent:agent-reader agent))
             (seen (and reader (agent:observe reader (pane-term pane))))
             (keys (or (and seen (agent:action-keys reader seen :choose n))
                       (list (princ-to-string n)))))
        (loop :for chunk :in keys
              :for at :from 0 :by +enter-after+
              :do (let ((chunk chunk))
                    (if (zerop at)
                        (pane-say pane chunk)
                        (later server at (lambda () (pane-say pane chunk))))))))
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

;;; Finding in a pane's history, and copying lines out of it. Rows are numbered
;;; from the oldest kept, so a hit has one address however far the pane is
;;; scrolled; the screen's own rows follow the scrollback's.

(defun pane-rows-kept (pane)
  "How many rows PANE has all told: what is kept behind the screen and the screen."
  (+ (pane-history pane) (term:term-height (pane-term pane))))

(defun pane-row-text (pane a)
  "Row A of PANE as a string, oldest kept row first, then the screen's."
  (let* ((term (pane-term pane))
         (kept (pane-history pane)))
    (cond ((< a 0) "")
          ((< a kept) (or (term:term-scrollback-row-string term a) ""))
          ((< a (pane-rows-kept pane)) (term:term-dump-row-string term (- a kept)))
          (t ""))))

(defun pane-top-row (pane)
  "Which row is at the top of what PANE shows."
  (- (pane-history pane) (pane-scrolled pane)))

(defun pane-show-row (pane a)
  "Scroll PANE so row A is on the screen, near the middle."
  (let ((height (term:term-height (pane-term pane))))
    (pane-scroll-to pane (- (pane-history pane) a (- (floor height 2))))))

(defun find-hits (pane query)
  "Every row of PANE with QUERY in it, oldest first: (row start end)."
  (let ((q (string-downcase query)))
    (loop :for a :below (pane-rows-kept pane)
          :for text := (string-downcase (pane-row-text pane a))
          :for at := (and (plusp (length q)) (search q text))
          :when at :collect (list a at (+ at (length q))))))

(defun find-in-a-pane (server watcher name id query way)
  "Find QUERY in the pane called NAME:ID, or the focus's when they are nil.
:HERE is a new query, found from the newest hit; :NEXT is the next older hit,
:BACK the next newer; :CLEAR forgets it. The pane scrolls to the hit and the
watcher is told how many there are and which this is."
  (let ((pane (or (and name (pane-called server name id))
                  (and (watcher-session watcher) (session-focus (watcher-session watcher))))))
    (when pane
      (let ((find (pane-find pane)))
        (ecase way
          (:clear (setf (pane-find pane) nil (pane-selecting pane) nil (pane-dirty pane) t))
          (:here
           (let ((hits (find-hits pane query)))
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
                     (pane-dirty pane) t)))))
        (let* ((find (pane-find pane))
               (hits (getf find :hits))
               (at (getf find :at))
               (hit (and at (nth at hits))))
          (when hit (pane-show-row pane (first hit)))
          (dolist (w (session-watchers (session-of-pane server pane)))
            (setf (watcher-behind w) t))
          (tell watcher (list :found (session-name (session-of-pane server pane)) (pane-id pane)
                              (length hits) (and at (- (length hits) at)) (and hit (first hit)))))))))

(defun session-of-pane (server pane)
  (find-if (lambda (s) (member pane (session-panes s))) (server-sessions server)))

(defun select-in-a-pane (server watcher what)
  "Lines out of the pane with WATCHER's focus. :START marks the top row shown
as one end of a selection; :COPY sends the lines from that mark to the other
end of what is shown now, or the screen when nothing was marked."
  (let* ((session (watcher-session watcher))
         (pane (and session (session-focus session))))
    (when pane
      (let ((top (pane-top-row pane))
            (height (term:term-height (pane-term pane))))
        (ecase what
          (:start (setf (pane-selecting pane) top (pane-dirty pane) t))
          (:copy
           (let* ((mark (or (pane-selecting pane) top))
                  (from (min mark top))
                  (to (+ (max mark top) height)))
             (tell watcher
                   (list :copied
                         (format nil "~{~A~^~%~}"
                                 (loop :for a :from from :below (min to (pane-rows-kept pane))
                                       :collect (string-right-trim " " (pane-row-text pane a))))))
             (setf (pane-selecting pane) nil (pane-dirty pane) t))))
        (dolist (w (session-watchers session)) (setf (watcher-behind w) t))))))

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
       ;; one line is typed, the way a person would give it. A coding agent
       ;; can take what arrives as a paste for something pasted in rather than
       ;; asked for, and decline to act on it: the field test saw exactly that.
       ;; More than one line is pasted, since a newline typed would send the
       ;; first line on its own.
       (pane-say pane (if (and (term:term-bracketed-paste (pane-term pane))
                               (or (find-if (lambda (c) (member c '(#\Newline #\Return))) text)
                                   (and (agent:agent-reader (pane-agent pane))
                                        (not (agent:agent-submits-typed-p (pane-agent pane))))))
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

(defun spawn-a-pane (server name command directory label &optional window)
  "A pane running COMMAND in the session called NAME, beside the one with the
focus there, or the first pane of that session when there is no such session
yet. WINDOW puts it in that window of the session instead, by number, or in a
new window when it is :new. Answers the pane."
  (let* ((session (session-named server name))
         (pane (if session
                   (cond
                     ((eq window :new)
                      (window-focus (add-window session command directory nil)))
                     ((and (integerp window) (window-called session window)
                           (not (eq (window-called session window) (session-window session))))
                      ;; beside the focus of that window, without showing it
                      (let* ((w (window-called session window))
                             (focus (window-focus w))
                             (it (make-pane command
                                            :rows (term:term-height (pane-term focus))
                                            :cols (term:term-width (pane-term focus))
                                            :directory (or directory (pane-directory focus)))))
                        (setf (window-layout w) (put-beside (window-layout w) focus :across it))
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
                           (put-beside (session-layout session) focus :across it))
                     (session-compose session)
                     (pane-start it :environment (pane-environment session it))
                     it)))
                   (session-focus (add-session server command :name name
                                                              :directory directory)))))
    (when label (setf (pane-label pane) label))
    (dolist (w (session-watchers (session-named server name))) (setf (watcher-behind w) t))
    pane))

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

(defun pane-about (pane)
  "What PANE is running and how it was started: what its kind was decided from."
  (let* ((agent (pane-agent pane))
         (reader (agent:agent-reader agent))
         (term (pane-term pane)))
    (list :kind (pane-kind pane)
          :programs (pane-programs pane)
          :group (pane-group pane)
          :command (pane-command pane)
          :directory (pane-directory pane)
          :pid (and (plusp (pane-pid pane)) (pane-pid pane))
          :size (list (term:term-width term) (term:term-height term))
          :version (agent:agent-version agent)
          :reader (and reader (agent:reader-name reader)))))

(defun history-said (agent now)
  (mapcar (lambda (it) (list (max 0 (- now (first it))) (second it)))
          (agent:agent-history agent)))
