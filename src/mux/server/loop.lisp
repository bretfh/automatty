;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun end-session (server session why)
  "SESSION is over. Whoever was watching it goes to another session when there
is one, the way a terminal left open on a closed tab shows the next; only when
it was the last are they told WHY and let go."
  (setf (server-sessions server) (remove session (server-sessions server)))
  (dolist (watcher (copy-list (session-watchers session)))
    (let ((next (first (server-sessions server))))
      (if next
          (join-session server watcher next)
          (drop-watcher server watcher why))))
  (mapc #'pane-close (session-panes session))
  (dolist (w (session-windows session))
    (setf (window-layout w) nil (window-focus w) nil))
  (unless (server-sessions server)
    (setf (server-running server) nil))
  server)

(defun session-observe (session now)
  (let ((changed nil)
        (ms (floor now 1000000)))
    (dolist (pane (session-panes session))
      (pane-update-programs pane ms)
      (let ((was (agent:agent-state (pane-agent pane))))
        (when (agent:agent-look (pane-agent pane) (pane-term pane) ms (pane-dirty pane))
          (push pane changed))
        ;; a state is only news of a pane something is read in: a shell has
        ;; none worth telling, and its pulse is coloured quiet
        (let* ((read (agent:agent-reader (pane-agent pane)))
               (is (and read (agent:agent-state (pane-agent pane)))))
          (when (and read (not (eq was is)))
            (pane-push-event pane ms
                        (case is (:blocked :asks) (:working :working) (:idle :idle) (t :quiet))
                        nil
                        (and (eq is :blocked)
                             (getf (agent:agent-asks (pane-agent pane) (pane-term pane)) :subject))))
          (pane-roll-pulse pane ms is))))
    (dolist (pane changed)
      (when (session-server session)
        (send-pending-prompt (session-server session) pane))
      (let ((events (agent:agent-take-events (pane-agent pane))))
        (dolist (w (session-watchers session))
          (setf (watcher-behind w) t)
          (when (wire-open (watcher-wire w))
            (send-message w (cons :agent (agent-row session pane)))
            (dolist (event events)
              (destructuring-bind (kind n what) event
                (declare (ignore kind))
                (send-message w (list :agent-turn (session-name session) (pane-id pane) n what))))))))
    ;; the focused pane's own bracketed-paste request, mirrored to whoever is
    ;; actually sitting at a real terminal: recomputed from the focus fresh
    ;; every tick, so a focus change resyncs it exactly as a flip would
    (let* ((focus (session-focus session))
           (want (and focus (term:term-bracketed-paste (pane-term focus)))))
      (dolist (w (session-watchers session))
        (when (and (watcher-interactive w) (wire-open (watcher-wire w))
                   (not (eq want (watcher-bracketed-sent w))))
          (send-message w (list :bracketed-paste want))
          (setf (watcher-bracketed-sent w) want))))
    changed))

(defun session-due-p (session)
  "Whether anybody watching SESSION is owed a frame."
  (let ((panes (session-panes session)))
    (some (lambda (c) (and (watcher-interactive c)
                           (or (watcher-behind c) (some #'pane-dirty panes))))
          (session-watchers session))))

(defun oldest-due (sessions)
  "When the longest-waiting watcher of any session that is owed a frame was last
sent one, or nothing when nobody is owed one."
  (let ((owed (mapcar #'oldest-watcher (remove-if-not #'session-due-p sessions))))
    (when owed (reduce #'min owed))))

(defun session-render (session gap)
  "Draw SESSION once, for whoever is behind and has waited GAP."
  (let ((panes (session-panes session))
        (composed nil)
        (then (monotonic-ns)))
    (when (some #'pane-dirty panes)
      (dolist (watcher (session-watchers session))
        (setf (watcher-behind watcher) t))
      (dolist (pane panes) (setf (pane-dirty pane) nil)))
    (dolist (watcher (session-watchers session))
      (when (and (watcher-behind watcher)
                 (watcher-interactive watcher)
                 (wire-open (watcher-wire watcher))
                 (zerop (wire-pending (watcher-wire watcher)))
                 (>= (- then (watcher-sent watcher)) gap))
        (unless composed
          (session-compose session)
          (setf composed t))
        (watcher-frame session watcher)
        (setf (watcher-sent watcher) then
              (watcher-behind watcher) nil)
        (wire-flush (watcher-wire watcher))))
    (when composed
      (dolist (pane panes) (setf (pane-rang pane) nil)))
    composed))

(defparameter +drain-budget+ 2000000)
(defparameter +typed-lately+ 1000)

(defun pane-urgent-p (session pane now)
  (or (< (- now (pane-typed-at pane)) +typed-lately+)
      (and (some #'watcher-interactive (session-watchers session))
           (member pane (layout-panes (session-layout session))))))

(defun input-waiting-p (server)
  (some (lambda (w)
          (and (wire-open (watcher-wire w))
               (pty:pty-wait (wire-fd (watcher-wire w)) 0)))
        (all-watchers server)))

(defun poll-descriptors (server sessions interval)
  "Wait until something needs the server: a client at the socket, a pane
with output, a wire to read or flush, a frame owed or a task due. Answers the
poll set and where the socket, each pane and each watcher are in it."
  (let* ((w (tty:waiting-clear (server-waiting server)))
         (now (monotonic-ns))
         (gap (* interval 1000000))
         (oldest (oldest-due sessions))
         (due (min (if oldest
                       (max 0 (ceiling (- gap (- now oldest)) 1000000))
                       100)
                   (if (server-tasks server) (next-task-delay server now) 100)))
         (listening (tty:waiting-add w (server-fd server)))
         (ptys (loop :for session :in sessions
                     :append (mapcar (lambda (p)
                                       (list session p
                                             (tty:waiting-add w (pane-fd p))))
                                     (session-panes session))))
         (wires (mapcar (lambda (c)
                          (cons c (tty:waiting-add
                                   w (wire-fd (watcher-wire c))
                                   (logior sb-unix:pollin
                                           (if (plusp (wire-pending
                                                       (watcher-wire c)))
                                               sb-unix:pollout
                                               0)))))
                        (append (server-pending-watchers server)
                                (loop :for session :in sessions
                                      :append (session-watchers session))))))
    (tty:wait-on w due)
    (values w listening ptys wires)))

(defun server-step (server &key (interval *interval*))
  (let ((sessions (server-sessions server))
        (gap (* interval 1000000)))
    (multiple-value-bind (w listening ptys wires) (poll-descriptors server sessions interval)
      (flet ((read-clients ()
               (loop :for (watcher . n) :in wires
                     :do (if (wire-open (watcher-wire watcher))
                             (progn
                               (when (tty:writable-p (tty:waiting-back w n))
                                 (wire-flush (watcher-wire watcher)))
                               (when (tty:readable-p (tty:waiting-back w n))
                                 (read-messages server watcher))
                               ;; a descriptor the kernel says is done with has nothing
                               ;; more to give, and polling it again is polling nothing
                               (when (and (tty:gone-p (tty:waiting-back w n))
                                          (wire-open (watcher-wire watcher)))
                                 (drop-watcher server watcher)))
                             ;; a write that came apart shuts the wire here; without this
                             ;; the watcher stays on the session and its closed descriptor
                             ;; is put to poll every wakeup for as long as the server runs
                             (drop-watcher server watcher))))
             (drain-panes ()
               (let* ((done nil)
                      (now (now-ms))
                      (ready (loop :for (session pane n) :in ptys
                                   :when (and (member pane (session-panes session))
                                              (tty:readable-p (tty:waiting-back w n)))
                                     :collect (cons session pane)))
                      (urgent (remove-if-not (lambda (it) (pane-urgent-p (car it) (cdr it) now))
                                             ready))
                      (others (remove-if (lambda (it) (member it urgent)) ready))
                      (turn (if others (mod (server-drain-turn server) (length others)) 0))
                      (others (append (nthcdr turn others) (subseq others 0 turn)))
                      (deadline (+ (monotonic-ns) +drain-budget+)))
                 (flet ((drain (it)
                          (setf (pane-moved-at (cdr it)) now)
                          (unless (pane-drain (cdr it))
                            (push it done))))
                   (mapc #'drain urgent)
                   (loop :for it :in others
                         :while (and (< (monotonic-ns) deadline)
                                     (not (input-waiting-p server)))
                         :do (drain it)
                             (incf (server-drain-turn server))))
                 (setf (server-drain-turn server) (mod (server-drain-turn server) 1000000))
                 (loop :for (session . pane) :in done
                       :do (session-close-pane session pane))))
             (step-sessions ()
               (dolist (session sessions)
                 (dolist (pane (remove-if-not #'pane-failed (session-panes session)))
                   (dolist (w (session-watchers session))
                     (send-message w (list :say (pane-failed pane) :warning)))
                   (session-close-pane session pane)))
               (dolist (session sessions)
                 (if (session-panes session)
                     (progn (session-observe session (monotonic-ns))
                            (session-render session gap))
                     (end-session server session :done)))))
        (run-due-tasks server (monotonic-ns))
        (dolist (session sessions) (session-tick session (monotonic-ns)))
        (when (tty:readable-p (tty:waiting-back w listening))
          (accept-watcher server))
        (read-clients)
        (drain-panes)
        (step-sessions)
        (send-updates server (now-ms))
        ;; what that said to anybody not on a session is sent now rather than when
        ;; the next wakeup finds their descriptor writable
        (dolist (watcher (all-watchers server))
          (when (and (wire-open (watcher-wire watcher))
                     (plusp (wire-pending (watcher-wire watcher))))
            (wire-flush (watcher-wire watcher))))
        server))))

(defun trim-scrollback (server)
  (let* ((now (now-ms))
         (panes (loop :for s :in (server-sessions server)
                      :append (mapcar (lambda (p) (cons s p)) (session-panes s))))
         (total (loop :for (nil . p) :in panes :sum (term:term-scrollback-size (pane-term p))))
         (share (floor +scrollback-budget+ (max 1 (length panes)))))
    (when (> total +scrollback-budget+)
      (dolist (it (sort (remove-if (lambda (it) (pane-urgent-p (car it) (cdr it) now)) panes)
                        #'< :key (lambda (it) (pane-typed-at (cdr it)))))
        (when (<= total +scrollback-budget+) (return))
        (let* ((pane (cdr it))
               (dropped (term:term-trim-scrollback (pane-term pane) share)))
          (when dropped
            (decf total dropped)
            (setf (pane-scrolled pane) (min (pane-scrolled pane) (pane-history pane))
                  (pane-dirty pane) t)))))
    total))

(defun schedule-trims (server)
  (labels ((tick ()
             (when (server-running server)
               (trim-scrollback server)
               (schedule-task server 1000 #'tick))))
    (schedule-task server 1000 #'tick)))

(defparameter +max-faults+ 10)

(defun report-error (e)
  (format *error-output* "~&atty: ~A~%" e)
  (ignore-errors
   (sb-debug:print-backtrace :stream *error-output* :count 30))
  (finish-output *error-output*))

(defun serve (path command &key (name "0") (rows 24) (cols 80)
                                (interval *interval*) (persist t) fresh)
  "Hold the sessions and feed whoever is watching them, until there are none.

With NAME nil it starts holding nothing, and the first client to open a session
makes one; see +FIRST-SESSION-PATIENCE+.

A fault in one wakeup is said and stepped over rather than taken as the end. The
panes are somebody's shells: losing them to a bug in the emulator is worse than
drawing one frame wrong, and the backtrace is in the log either way. Faults one
after another with nothing between them are a loop rather than a mishap, and
that does end it.

With PERSIST what the server holds is kept on disk, brought back before it
takes its name, and saved as it changes and when it stops; FRESH puts what was
on disk aside first. A signal to stop is heard as a request: the loop ends and
the state is saved on the way out."
  (let ((server (make-server path :listening nil))
        (faults 0))
    (setf (server-command server) command)
    (when *init-error*
      (push (list (format nil "in the server, ~A" *init-error*) :warning)
            (server-notes server)))
    (unwind-protect
         (progn
           (when persist
             (when fresh (move-state-aside (file-namestring path)))
             (handler-case (restore-state server)
               (error (e)
                 (report-error e)
                 (push (list (format nil "what was on disk could not be brought back: ~A" e)
                             :warning)
                       (server-notes server))))
             (setf (server-saving server) t)
             (tty:hear-the-end t)
             (schedule-saves server)
             (schedule-release-check server))
           (server-listen server)
           (schedule-trims server)
           (when (and name (not (session-named server name)))
             (add-session server command :name name :rows rows :cols cols))
           (loop while (and (server-needed-p server (monotonic-ns))
                            (not (and persist tty:*asked-to-stop*)))
                 do (handler-case
                        (progn (server-step server :interval interval)
                               (setf faults 0))
                      (error (e)
                        (report-error e)
                        (when (> (incf faults) +max-faults+)
                          (format *error-output*
                                  "~&atty: ~D faults with nothing between them; stopping.~%"
                                  faults)
                          (setf (server-running server) nil)))))
           server)
      (server-close server))))
