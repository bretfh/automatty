;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)


(declaim (ftype (function (watcher list) t) send-message))
(defun watcher-elsewhere-p (watcher)
  (let ((thread (watcher-thread watcher)))
    (and thread (not (eq thread sb-thread:*current-thread*)) (sb-thread:thread-alive-p thread))))

(defun watcher-poke (watcher)
  (let ((wake (watcher-wake watcher)))
    (when wake
      (sb-sys:with-pinned-objects (*poke-octet*)
        (sb-unix:unix-write (cdr wake) *poke-octet* 0 1)))))

(declaim (ftype (function (watcher function) t) on-watcher))
(defun on-watcher (watcher job)
  (if (watcher-elsewhere-p watcher)
      (progn (sb-ext:atomic-push job (watcher-inbox watcher))
             (watcher-poke watcher))
      (let ((*client* watcher)) (funcall job))))

(defun draw-again (watcher)
  (setf (watcher-behind watcher) t)
  (watcher-poke watcher))

(defun watcher-jobs (watcher)
  (let ((jobs (loop :for had := (watcher-inbox watcher)
                    :until (eq had (sb-ext:compare-and-swap (watcher-inbox watcher) had nil))
                    :finally (return had))))
    (let ((*client* watcher))
      (dolist (job (reverse jobs))
        (funcall job)))))

(defparameter +server-specials+ '(*state-home* *error-output* *handing-over*))

(defun watcher-start (server watcher)
  (setf (watcher-wake watcher) (open-wake-pipe))
  (let ((placed (sb-thread:make-semaphore))
        (held (mapcar #'symbol-value +server-specials+)))
    (setf (watcher-thread watcher)
          (sb-thread:make-thread
           (lambda ()
             (sb-thread:wait-on-semaphore placed)
             (progv +server-specials+ held
               (let ((*server* server)
                     (*client* watcher))
                 (watcher-run server watcher))))
           :name (format nil "atty client ~D" (watcher-id watcher))))
    (sb-thread:signal-semaphore placed)
    watcher))

(defun watcher-stop (watcher)
  (let ((thread (watcher-thread watcher))
        (wire (watcher-wire watcher)))
    (on-watcher watcher (lambda ()
                          (ignore-errors (wire-flush wire))
                          (wire-close wire)))
    (when (and thread (not (eq thread sb-thread:*current-thread*)))
      (sb-thread:join-thread thread :default nil :timeout 5))))

(defun panes-shown (session)
  (mapcar #'pane-shown (window-panes (session-window session))))

(defun watcher-due-at (server watcher)
  (let ((session (watcher-session watcher))
        (due nil))
    (flet ((at (ns) (when (and ns (or (null due) (< ns due))) (setf due ns))))
      (when (and session (session-panes session) (watcher-interactive watcher))
        (at (watcher-clocked watcher))
        (when (or (watcher-owed-p watcher)
                  (not (equal (panes-shown session) (watcher-drawn watcher))))
          (at (frame-due-at watcher (* (server-interval server) 1000000))))))
    due))

(defun watcher-draw (server watcher)
  (let ((session (watcher-session watcher)))
    (when session
      (drawing-session (session)
        (watcher-draw-seen server watcher session)))))

(defun watcher-draw-seen (server watcher session)
  (let ((wire (watcher-wire watcher)))
    (when (and (session-panes session) (watcher-interactive watcher) (wire-open wire))
      (let ((now (monotonic-ns)))
        (watcher-tick watcher session now)
        (let ((want (let ((focus (session-focus session)))
                      (and focus (pane-pastes-p focus)))))
          (unless (eq want (watcher-bracketed-sent watcher))
            (send-message watcher (list :bracketed-paste want))
            (setf (watcher-bracketed-sent watcher) want)))
        (let ((owed (watcher-owed-p watcher))
              (version (session-version session))
              (shown (panes-shown session)))
          (when (and (or owed (not (equal shown (watcher-drawn watcher))))
                     (zerop (wire-pending wire))
                     (>= now (frame-due-at watcher (* (server-interval server) 1000000))))
            (or (and (not owed) (session-repaint session watcher))
                (session-compose session watcher))
            (watcher-frame session watcher)
            (setf (watcher-sent watcher) now
                  (watcher-behind watcher) nil
                  (watcher-seen watcher) version
                  (watcher-drawn watcher) shown)
            (wire-flush wire)))))))

(defun watcher-turn (server watcher w)
  (let ((wire (watcher-wire watcher)))
    (tty:waiting-clear w)
    (tty:waiting-add w (wire-fd wire)
                     (if (plusp (wire-pending wire))
                         (logior sb-unix:pollin sb-unix:pollout)
                         sb-unix:pollin))
    (tty:waiting-add w (car (watcher-wake watcher)))
    (tty:wait-on w (let ((due (watcher-due-at server watcher)))
                     (if due
                         (max 0 (min 1000 (ceiling (- due (monotonic-ns)) 1000000)))
                         1000)))
    (when (tty:readable-p (tty:waiting-back w 1))
      (drain-wake-pipe (watcher-wake watcher)))
    (watcher-jobs watcher)
    (let ((back (tty:waiting-back w 0)))
      (when (and (wire-open wire) (tty:writable-p back))
        (wire-flush wire))
      (when (and (wire-open wire) (tty:readable-p back))
        (read-messages server watcher))
      (when (and (wire-open wire) (tty:gone-p back))
        (drop-watcher server watcher)))
    (watcher-draw server watcher)))

(defun watcher-run (server watcher)
  (let ((w (tty:make-waiting 2))
        (wire (watcher-wire watcher))
        (faults 0))
    (unwind-protect
         (loop :while (wire-open wire)
               :do (handler-case
                       (progn (watcher-turn server watcher w)
                              (setf faults 0))
                     (error (e)
                       (report-error e)
                       (when (> (incf faults) +max-faults+)
                         (drop-watcher server watcher)))))
      (watcher-jobs watcher)
      (when (or (watcher-session watcher) (member watcher (server-pending-watchers server)))
        (drop-watcher server watcher))
      (tty:free-waiting w)
      (close-wake-pipe (watcher-wake watcher))
      (setf (watcher-wake watcher) nil))))

(defun send-message (watcher form)
  (if (watcher-elsewhere-p watcher)
      (on-watcher watcher (lambda () (send-message watcher form)))
      (wire-send (watcher-wire watcher) form)))

(declaim (ftype (function (watcher session) t) send-hello))
(defun send-hello (watcher session)
  "Tell WATCHER how big what it is looking at is."
  (send-message watcher (list :hello (session-name session)
                      (session-rows session) (session-cols session))))

(defun leave-session (watcher)
  "Take WATCHER off whatever session it was on."
  (let ((session (watcher-session watcher)))
    (when session
      (setf (session-watchers session)
            (remove watcher (session-watchers session))
            (session-readers session) (remove watcher (session-readers session))
            (watcher-session watcher) nil)
      (session-fit session))
    session))

(defun drop-watcher (server watcher &optional why)
  (when (watcher-elsewhere-p watcher)
    (return-from drop-watcher
      (on-watcher watcher (lambda () (drop-watcher server watcher why)))))
  (when why
    (ignore-errors (send-message watcher (list :bye why))
                   (wire-flush (watcher-wire watcher))))
  (wire-close (watcher-wire watcher))
  (sb-ext:atomic-update (server-pending-watchers server) (lambda (all) (remove watcher all)))
  (leave-session watcher)
  (stop-following server watcher)
  (when (watcher-interactive watcher) (send-client-list server)))

(defun join-session (server watcher session)
  "Put WATCHER on SESSION and tell it what it is looking at."
  (when (watcher-elsewhere-p watcher)
    (return-from join-session
      (on-watcher watcher (lambda () (join-session server watcher session)))))
  (leave-session watcher)
  (sb-ext:atomic-update (server-pending-watchers server) (lambda (all) (remove watcher all)))
  (setf (watcher-session watcher) session)
  (push watcher (session-watchers session))
  ;; a fit that changed the size has already told everybody, this one included;
  ;; only a fit that changed nothing leaves it to be said here
  (setf (watcher-rung watcher) (loop :for pane :in (session-panes session) :sum (pane-rang pane)))
  (unless (session-fit session)
    (setf (watcher-shadow watcher)
          (tty:make-screen :width (session-cols session)
                           :height (session-rows session))
          (watcher-told watcher) nil
          (watcher-behind watcher) t)
    (send-hello watcher session))
  ;; what the server has had to say since anybody was here to hear it: the
  ;; first to arrive is told, and it is said once
  (let ((notes (loop :for had := (server-notes server)
                     :until (eq had (sb-ext:compare-and-swap (server-notes server) had nil))
                     :finally (return had))))
    (dolist (note (reverse notes))
      (destructuring-bind (text &optional (face :accent)) note
        (show-note watcher "atty" text :face face))))
  (send-client-list server)
  (let ((*client* watcher)) (run-hook 'client-attached watcher))
  (move-followers server watcher session)
  session)

(defun followers-of (server watcher)
  (remove-if-not (lambda (w) (eql (watcher-following w) (watcher-id watcher)))
                 (all-watchers server)))

(defun move-followers (server watcher session)
  "Everybody following WATCHER is put on SESSION with it."
  (dolist (w (followers-of server watcher))
    (unless (eq (watcher-session w) session)
      (join-session server w session))))

(defun follow (server watcher id)
  "WATCHER goes where the watcher called ID goes, from now; nil stops."
  (let ((leader (and id (find id (all-watchers server) :key #'watcher-id))))
    (setf (watcher-following watcher) (and leader (not (eq leader watcher)) id))
    (when (and leader (watcher-following watcher) (watcher-session leader)
               (not (eq (watcher-session leader) (watcher-session watcher))))
      (join-session server watcher (watcher-session leader)))
    (send-client-list server)))

(defun stop-following (server watcher)
  "WATCHER goes its own way, and nobody follows it any more."
  (let ((was (or (watcher-following watcher) (followers-of server watcher))))
    (setf (watcher-following watcher) nil)
    (dolist (w (followers-of server watcher))
      (on-watcher w (let ((w w)) (lambda () (setf (watcher-following w) nil)))))
    (when was (send-client-list server))))

(defun watcher-resize (watcher rows cols takes)
  (setf (watcher-rows watcher) (max 1 (min +max-pane-size+ rows))
        (watcher-cols watcher) (max 1 (min +max-pane-size+ cols))
        (watcher-takes watcher) takes
        (watcher-interactive watcher) t)
  (when (zerop (watcher-since watcher))
    (setf (watcher-since watcher) (now-ms))))

(defun encode-client (server watcher now)
  "One attached terminal as anybody is told of it: its id, its tty, its size,
where it is looking, how long it has been attached and how long since it
typed."
  (declare (ignore server))
  (let ((session (watcher-session watcher)))
    (list (watcher-id watcher) (watcher-tty watcher)
          (watcher-rows watcher) (watcher-cols watcher)
          (and session (session-name session))
          (and session (window-number session (session-window session)))
          (max 0 (- now (watcher-since watcher)))
          (if (plusp (watcher-typed-at watcher)) (max 0 (- now (watcher-typed-at watcher))) nil)
          (watcher-following watcher))))

(defun encode-clients (server now)
  (loop :for w :in (all-watchers server)
        :when (and (watcher-interactive w) (wire-open (watcher-wire w)))
          :collect (encode-client server w now)))

(defun send-client-list (server)
  "Everybody attached is drawn again: who is attached is on the bar and in
what lists the terminals."
  (dolist (w (all-watchers server))
    (when (watcher-interactive w) (draw-again w))))
