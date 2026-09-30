;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun end-session (server session why)
  "End a SESSION. Whoever was watching it goes to another session when there
is one, the way a terminal left open on a closed tab shows the next; only when
it was the last are they told WHY and let go."
  (sb-ext:atomic-update (server-sessions server) (lambda (all) (remove session all)))
  (dolist (watcher (copy-list (session-watchers session)))
    (let ((next (first (server-sessions server))))
      (if next
          (join-session server watcher next)
        (drop-watcher server watcher why))))
  (mapc #'pane-close (session-panes session))
  (dolist (w (session-windows session))
    (change-window session w (lambda (w) (setf (window-layout w) nil (window-focus w) nil))))
  (unless (server-sessions server)
    (setf (server-running server) nil))
  server)

(declaim (ftype (function (pane integer) t) look-at-agent))
(defun look-at-agent (pane ms)
  (setf (pane-look-at pane) nil)
  (pane-update-programs pane ms)
  (let* ((agent (pane-agent pane))
         (was (agent:agent-state agent))
         (moved (> (pane-moved-at pane) (pane-looked-at pane)))
         (changed (progn (setf (pane-looked-at pane) ms)
                         (agent:agent-look agent (pane-term pane) ms moved))))
    (let* ((read (agent:agent-reader agent))
           (is (and read (agent:agent-state agent))))
      (when (and read (not (eq was is)))
        (pane-push-event pane ms
                         (case is (:blocked :asks) (:working :working) (:idle :idle) (t :quiet))
                         nil
                         (and (eq is :blocked)
                              (getf (agent:agent-asks agent nil) :subject))))
      (pane-roll-pulse pane ms is))
    (let ((next (agent:agent-next-look agent ms)))
      (when next (pane-look-later pane next)))
    changed))

(defun session-look (session)
  (let ((server (session-server session)))
    (lambda (pane ms)
      (when (and (look-at-agent pane ms) server)
        (sb-ext:atomic-push pane (server-changed-panes server))
        (server-poke server)))))

(defun session-observe (session now &optional looked)
  (let ((changed nil)
        (ms (floor now 1000000)))
    (dolist (pane (session-panes session))
      (cond ((pane-thread pane)
             (when (member pane looked) (push pane changed)))
            ((pane-due-p pane ms)
             (when (look-at-agent pane ms) (push pane changed)))))
    (when changed
      (dolist (w (session-watchers session)) (setf (watcher-clocked w) 0))
      (when (session-server session)
        (dolist (w (all-watchers (session-server session)))
          (when (watcher-interactive w)
            (draw-again w)))))
    (dolist (pane changed)
      (when (session-server session)
        (send-pending-prompt (session-server session) pane))
      (let ((events (agent:agent-take-events (pane-agent pane))))
        (dolist (w (session-watchers session))
          (draw-again w)
          (when (and (not (watcher-interactive w)) (wire-open (watcher-wire w)))
            (send-message w (cons :agent (agent-row session pane)))
            (dolist (event events)
              (destructuring-bind (kind n what) event
                                  (declare (ignore kind))
                                  (send-message w (list :agent-turn (session-name session) (pane-id pane) n what))))))))
    changed))

(defun watcher-owed-p (watcher)
  (or (watcher-behind watcher)
      (let ((session (watcher-session watcher)))
        (and session (/= (watcher-seen watcher) (session-version session))))))

(declaim (ftype (function (watcher integer) integer) frame-due-at))
(defun frame-due-at (watcher gap)
  "When WATCHER may be sent its next frame: GAP after the last, or at once when
it has typed since, so what it typed is seen as soon as it is echoed."
  (let ((sent (watcher-sent watcher)))
    (if (> (watcher-keyed-at watcher) (watcher-answered watcher))
        sent
        (+ sent gap))))

(defparameter +typed-lately+ 1000)

(defun pane-urgent-p (session pane now)
  (or (< (- now (pane-typed-at pane)) +typed-lately+)
      (and (some #'watcher-interactive (session-watchers session))
           (member pane (layout-panes (session-layout session))))))

(declaim (ftype (function (server list integer integer) integer) wake-in))
(defun wake-in (server sessions now gap)
  "Milliseconds until something is due without a descriptor saying so: a frame
owed, a task, a pane to look at again, a bar that would read differently, or the
end of a first session's patience. -1 when nothing is."
  (let ((due nil))
    (flet ((at (ns)
             (when (and ns (or (null due) (< ns due)))
               (setf due ns))))
      (loop :for (when . nil) :in (server-tasks server) :do (at when))
      (dolist (session sessions)
        (dolist (pane (session-panes session))
          (let ((look (and (not (pane-thread pane)) (pane-look-at pane))))
            (when look (at (* look 1000000))))))
      (unless (server-had-sessions server)
        (at (+ (server-born server) +first-session-timeout+))))
    (if due (max 0 (ceiling (- due now) 1000000)) -1)))

(declaim (ftype (function (server list integer (or null integer)) (values t fixnum fixnum))
                poll-descriptors))
(defun poll-descriptors (server sessions interval most)
  "Wait until something needs the server: a client at the socket, a pane or a
client calling on it, or something due, or MOST milliseconds when that is
sooner. Answers the poll set, where the socket is in it, and how many came back."
  (let ((w (tty:waiting-clear (server-waiting server))))
    (let* ((listening (tty:waiting-add w (server-fd server)))
           (woken (tty:waiting-add w (or (tty:wake-fd) -1)))
           (poked (tty:waiting-add w (if (server-wake server) (car (server-wake server)) -1))))
      (let* ((due (wake-in server sessions (monotonic-ns) (* interval 1000000)))
             (ready (tty:wait-on w (if (and most (or (minusp due) (< most due))) most due))))
        (when (tty:readable-p (tty:waiting-back w woken))
          (tty:drain-wake))
        (when (and (server-wake server) (tty:readable-p (tty:waiting-back w poked)))
          (drain-wake-pipe (server-wake server)))
        (values w listening (or ready 0))))))

(defun close-ended-panes (server)
  (dolist (session (server-sessions server))
    (dolist (pane (session-panes session))
      (when (pane-ended pane)
        (session-close-pane session pane)))))

(defun step-sessions (server sessions)
  (dolist (session sessions)
    (dolist (pane (session-panes session))
      (when (pane-failed pane)
        (dolist (w (session-watchers session))
          (show-note w "atty" (pane-failed pane)))
        (session-close-pane session pane))))
  (let ((looked (loop :for had := (server-changed-panes server)
                      :until (eq had (sb-ext:compare-and-swap (server-changed-panes server) had nil))
                      :finally (return had))))
    (dolist (session sessions)
      (if (session-panes session)
          (session-observe session (monotonic-ns) looked)
          (end-session server session :done)))))

(defun server-step (server &key (interval *interval*) most)
  (setf (server-interval server) interval)
  (let ((sessions (server-sessions server)))
    (multiple-value-bind (w listening ready) (poll-descriptors server sessions interval most)
      (when (plusp ready) (stir server))
      (run-due-tasks server (monotonic-ns))
      (when (tty:readable-p (tty:waiting-back w listening))
        (accept-watcher server))
      (close-ended-panes server)
      (step-sessions server sessions)
      server)))

(defun trim-scrollback (server)
  (let* ((now (now-ms))
         (panes (loop :for s :in (server-sessions server)
                      :append (mapcar (lambda (p) (cons s p)) (session-panes s))))
         (kept (lambda (p) (if (pane-thread-p p) (pane-kept p) (term:term-scrollback-size (pane-term p)))))
         (total (loop :for (nil . p) :in panes :sum (funcall kept p)))
         (share (floor +scrollback-budget+ (max 1 (length panes)))))
    (when (> total +scrollback-budget+)
      (dolist (it (sort (remove-if (lambda (it) (pane-urgent-p (car it) (cdr it) now)) panes)
                        #'< :key (lambda (it) (pane-typed-at (cdr it)))))
        (when (<= total +scrollback-budget+) (return))
        (let ((pane (cdr it)))
          (when (> (funcall kept pane) share)
            (decf total (- (funcall kept pane) share))
            (on-pane pane (lambda ()
                            (when (term:term-trim-scrollback (pane-term pane) share)
                              (setf (pane-kept pane) (term:term-scrollback-size (pane-term pane))
                                    (pane-scrolled pane) (min (pane-scrolled pane) (pane-history pane))
                                    (pane-dirty pane) t)
                              (when (pane-thread pane) (pane-show pane))))
                     :wait nil)))))
    total))

(declaim (ftype (function (server) t) tend))
(defun tend (server)
  "Trim the scrollback and save what is owed, and come back while anything
happened since or anything is still owed."
  (setf (server-tending server) nil)
  (when (server-running server)
    (let ((stirred (server-stirred server)))
      (setf (server-stirred server) nil)
      (trim-scrollback server)
      (when (server-saving server)
        (handler-case (save-due server (now-ms))
          (error (e) (report-error e))))
      (when (or stirred (saves-owed-p server))
        (tend-later server)))))

(declaim (ftype (function (server) t) tend-later))
(defun tend-later (server)
  (unless (server-tending server)
    (setf (server-tending server) t)
    (schedule-task server +save-tick+ (lambda () (tend server)))))

(declaim (ftype (function (server) t) stir))
(defun stir (server)
  "Something happened: see to the scrollback and the saves a tick from now."
  (setf (server-stirred server) t)
  (tend-later server))

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
    (setf (server-command server) command
          (server-interval server) interval)
    (when *init-error*
      (push (list (format nil "in the server, ~A" *init-error*) :warning)
            (server-notes server)))
    (unwind-protect
        (progn
          (when persist
            (when fresh (move-state-aside (file-namestring path)))
            (take-handoff)
            (handler-case (restore-state server)
              (error (e)
                     (report-error e)
                     (push (list (format nil "what was on disk could not be brought back: ~A" e)
                                 :warning)
                           (server-notes server))))
            (let-go-of-unadopted)
            (setf (server-saving server) t)
            (tty:hear-the-end t)
            (schedule-release-check server))
          (server-listen server)
          (stir server)
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
