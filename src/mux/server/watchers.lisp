;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)


(declaim (ftype (function (watcher list) t) send-message))
(defun watcher-elsewhere-p (watcher)
  (let ((thread (watcher-thread watcher)))
    (and thread (not (eq thread sb-thread:*current-thread*)) (sb-thread:thread-alive-p thread))))

(defun watcher-poke (watcher)
  (sb-thread:with-mutex (*wake-lock*)
    (poke-wake-pipe (watcher-wake watcher))))

(declaim (ftype (function (watcher function &key (:wait t)) t) on-watcher))
(defun on-watcher (watcher job &key wait)
  (cond ((not (watcher-elsewhere-p watcher))
         (let ((*client* watcher)) (funcall job)))
        ((not wait)
         (sb-ext:atomic-push job (watcher-inbox watcher))
         (watcher-poke watcher))
        (t (let ((done (sb-thread:make-semaphore))
                 (claim (list nil))
                 (said nil)
                 (broke nil))
             (flet ((run ()
                      (when (null (sb-ext:compare-and-swap (car claim) nil t))
                        (handler-case (setf said (multiple-value-list
                                                  (let ((*client* watcher)) (funcall job))))
                          (serious-condition (c) (setf broke c)))
                        t)))
               (sb-ext:atomic-push (lambda () (unwind-protect (run) (sb-thread:signal-semaphore done)))
                                   (watcher-inbox watcher))
               (watcher-poke watcher)
               (loop :until (sb-thread:wait-on-semaphore done :timeout 1)
                     :when (and (not (watcher-elsewhere-p watcher)) (run)) :return nil))
             (when broke (error broke))
             (values-list said)))))

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

(defun panes-shown (watcher)
  (mapcar (lambda (it) (pane-shown (first it))) (watcher-fits watcher)))

(defun watcher-due-at (server watcher)
  (let ((session (watcher-session watcher))
        (due nil))
    (flet ((at (ns) (when (and ns (or (null due) (< ns due))) (setf due ns))))
      (when (and session (session-panes session) (watcher-interactive watcher))
        (at (watcher-clocked watcher))
        (when (and (zerop (wire-pending (watcher-wire watcher)))
                   (or (watcher-owed-p watcher)
                       (not (equal (panes-shown watcher) (watcher-drawn watcher)))))
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
        (let ((want (let ((focus (watcher-focus watcher)))
                      (and focus (pane-pastes-p focus)))))
          (unless (eq want (watcher-bracketed-sent watcher))
            (send-message watcher (list :bracketed-paste want))
            (setf (watcher-bracketed-sent watcher) want)))
        (let ((owed (watcher-owed-p watcher))
              (version (session-version session))
              (shown (panes-shown watcher))
              (echo (let ((focus (watcher-focus watcher)))
                      (and focus (pane-shown-now focus)))))
          (when (and (or owed (not (equal shown (watcher-drawn watcher))))
                     (zerop (wire-pending wire))
                     (>= now (frame-due-at watcher (* (server-interval server) 1000000))))
            (setf (watcher-behind watcher) nil)
            (or (and (not owed) (session-repaint watcher))
                (session-compose session watcher))
            (watcher-frame session watcher)
            (setf (watcher-sent watcher) now
                  (watcher-seen watcher) version
                  (watcher-drawn watcher) (panes-shown watcher))
            (when (or (null echo) (>= (shown-echoed echo) (watcher-keyed-at watcher)))
              (setf (watcher-answered watcher) (watcher-keyed-at watcher)))
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
      (sb-thread:with-mutex (*wake-lock*)
        (close-wake-pipe (shiftf (watcher-wake watcher) nil))))))

(defun send-message (watcher form)
  (if (watcher-elsewhere-p watcher)
      (on-watcher watcher (lambda () (send-message watcher form)))
      (wire-send (watcher-wire watcher) form)))

(declaim (ftype (function (watcher) t) send-hello))
(defun send-hello (watcher)
  "Tell WATCHER how big what it is looking at is."
  (let ((session (watcher-session watcher)))
    (send-message watcher (list :hello (and session (session-name session))
                                (watcher-rows watcher) (watcher-cols watcher)))))

(defparameter +seeds-kept+ 16)

(defun kept-seed (tty view seeds)
  (let ((kept (cons (cons tty view) (remove tty seeds :key #'car :test #'equal))))
    (subseq kept 0 (min +seeds-kept+ (length kept)))))

(defun seed-for (session watcher)
  (let* ((seeds (session-seeds session))
         (latest (loop :with best := nil
                       :for w :in (session-watchers session)
                       :when (and (watcher-interactive w) (not (eq w watcher))
                                  (or (null best) (> (watcher-typed-at w) (watcher-typed-at best))))
                         :do (setf best w)
                       :finally (return best))))
    (or (and watcher (watcher-tty watcher) (cdr (assoc (watcher-tty watcher) seeds :test #'equal)))
        (and latest (view-like (watcher-view latest)))
        (cdr (first seeds))
        (make-view))))

(defun forget-watcher (session watcher)
  (let ((seed (and (watcher-interactive watcher) (view-like (watcher-view watcher))))
        (tty (watcher-tty watcher)))
    (lambda ()
      (change-session session
                      (lambda (s)
                        (setf (state-watchers s) (remove watcher (state-watchers s)))
                        (when seed
                          (setf (state-seeds s) (kept-seed tty seed (state-seeds s)))))))))

(defun redraw-others (session watcher)
  (dolist (w (session-watchers session))
    (when (and (watcher-interactive w) (not (eq w watcher))) (draw-again w))))

(defun leave-session (watcher)
  "Take WATCHER off whatever session it was on."
  (let ((session (watcher-session watcher)))
    (when session
      (on-server (session-server session) (forget-watcher session watcher))
      (setf (watcher-session watcher) nil
            (watcher-fits watcher) nil)
      (redraw-others session watcher))
    session))

(defun drop-watcher (server watcher &optional why)
  (when (watcher-elsewhere-p watcher)
    (return-from drop-watcher
      (on-watcher watcher (lambda () (drop-watcher server watcher why)))))
  (when why
    (ignore-errors (send-message watcher (list :bye why))
                   (wire-flush (watcher-wire watcher))))
  (wire-close (watcher-wire watcher))
  (on-server server (lambda ()
                      (sb-ext:atomic-update (server-pending-watchers server)
                                            (lambda (all) (remove watcher all)))))
  (leave-session watcher)
  (stop-following server watcher)
  (when (watcher-interactive watcher) (send-client-list server)))

(defun join-session (server watcher session)
  "Put WATCHER on SESSION and tell it what it is looking at."
  (when (watcher-elsewhere-p watcher)
    (return-from join-session
      (on-watcher watcher (lambda () (join-session server watcher session)))))
  (let ((old (watcher-session watcher))
        (seed (seed-for session watcher)))
    (let ((forget (and old (forget-watcher old watcher))))
      (on-server server
                 (lambda ()
                   (when forget (funcall forget))
                   (sb-ext:atomic-update (server-pending-watchers server)
                                         (lambda (all) (remove watcher all)))
                   (change-session session (lambda (s) (push watcher (state-watchers s)))))))
    (setf (watcher-session watcher) session
          (watcher-fits watcher) nil)
    (adopt-view watcher seed)
    (when old (redraw-others old watcher)))
  (setf (watcher-rung watcher) (loop :for pane :in (session-panes session) :sum (pane-rang pane))
        (watcher-shadow watcher) (tty:make-screen :width (watcher-cols watcher)
                                                  :height (watcher-rows watcher))
        (watcher-told watcher) nil
        (watcher-behind watcher) t)
  (send-hello watcher)
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
  (view-changed watcher)
  session)

(defun followers-of (server watcher)
  (remove-if-not (lambda (w) (eql (watcher-following w) (watcher-id watcher)))
                 (all-watchers server)))

(defun follow (server watcher id)
  "WATCHER goes where the watcher called ID goes, from now; nil stops."
  (let ((leader (and id (find id (all-watchers server) :key #'watcher-id))))
    (setf (watcher-following watcher) (and leader (not (eq leader watcher)) id))
    (when (and leader (watcher-following watcher) (watcher-session leader))
      (unless (eq (watcher-session leader) (watcher-session watcher))
        (join-session server watcher (watcher-session leader)))
      (adopt-view watcher (view-like (watcher-view leader))))
    (send-client-list server)))

(defun stop-following (server watcher)
  "WATCHER goes its own way, and nobody follows it any more."
  (let ((was (or (watcher-following watcher) (followers-of server watcher))))
    (setf (watcher-following watcher) nil)
    (dolist (w (followers-of server watcher))
      (on-watcher w (let ((w w)) (lambda () (setf (watcher-following w) nil)))))
    (when was (send-client-list server))))

(defun watcher-resize (watcher rows cols takes)
  (let ((view (watcher-view watcher)))
    (setf (view-rows view) (max 1 (min +max-pane-size+ rows))
          (view-cols view) (max 1 (min +max-pane-size+ cols))
          (watcher-takes watcher) takes
          (watcher-interactive watcher) t))
  (when (zerop (watcher-since watcher))
    (setf (watcher-since watcher) (now-ms))))

(defun encode-client (server watcher now)
  "One attached terminal as anybody is told of it: its id, its tty, its size,
where it is looking, how long it has been attached and how long since it
typed."
  (declare (ignore server))
  (let ((session (watcher-session watcher))
        (window (watcher-window watcher)))
    (list (watcher-id watcher) (watcher-tty watcher)
          (watcher-rows watcher) (watcher-cols watcher)
          (and session (session-name session))
          (and session window (window-number session window))
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
