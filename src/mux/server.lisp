;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

(defvar *interval* 8
  "The least milliseconds between two frames to one client.

A least gap, never a tick: a client that has heard nothing for a while is
answered the moment a byte arrives, and only a client already being fed faster
than this waits. A tick would add half its length to every keystroke, which is
more than the terminal it is sitting inside costs in the first place.")

(defparameter +biggest-pane+ 1000)

(defstruct (watcher (:constructor %make-watcher))
  (wire nil)
  (socket nil)
  (rows 24 :type fixnum)
  (cols 80 :type fixnum)
  (takes t)
  (shadow nil)
  (told nil)
  (behind t :type boolean)
  (sent 0 :type fixnum)
  (here nil :type boolean))

(defstruct (session (:constructor %make-session))
  (name "0")
  (pane nil)
  (screen nil)
  (watchers nil)
  (rows 24 :type fixnum)
  (cols 80 :type fixnum))

(defstruct (server (:constructor %make-server))
  (path nil)
  (socket nil)
  (fd -1 :type fixnum)
  (sessions nil)
  (waiting nil)
  (going t :type boolean))

(defun nanos ()
  (multiple-value-bind (sec nsec) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* sec 1000000000) nsec)))

(defun listen-on (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (ignore-errors (delete-file path))
    (sb-bsd-sockets:socket-bind socket path)
    (sb-bsd-sockets:socket-listen socket 8)
    socket))

(defun make-server (path)
  (let ((socket (listen-on path)))
    (let ((fd (sb-bsd-sockets:socket-file-descriptor socket)))
      (sb-posix:fcntl fd sb-posix:f-setfl
                      (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                              sb-posix:o-nonblock))
      (%make-server :path path :socket socket :fd fd
                    :waiting (make-waiting 16)))))

(defun server-close (server)
  (dolist (session (server-sessions server))
    (dolist (w (session-watchers session))
      (wire-close (watcher-wire w)))
    (pane-close (session-pane session)))
  (setf (server-sessions server) nil)
  (free-waiting (server-waiting server))
  (ignore-errors (sb-bsd-sockets:socket-close (server-socket server)))
  (ignore-errors (delete-file (server-path server)))
  (setf (server-going server) nil))

(defun add-session (server command &key (name "0") (rows 24) (cols 80))
  (let ((session (%make-session :name name :rows rows :cols cols
                                :pane (make-pane command :rows rows :cols cols)
                                :screen (make-screen :width cols :height rows))))
    (push session (server-sessions server))
    session))

;;; how big the pane is: the smallest any watcher can show, so nobody is shown a
;;; screen with a piece missing.

(defun session-pane-term (session)
  (pane-term (session-pane session)))

(defun watcher-oldest (session)
  (let ((here (remove-if-not #'watcher-here (session-watchers session))))
    (if here (reduce #'min here :key #'watcher-sent) 0)))

(defun session-fit (session)
  (let ((rows (session-rows session))
        (cols (session-cols session))
        (here (remove-if-not #'watcher-here (session-watchers session))))
    (when here
      (setf rows (reduce #'min here :key #'watcher-rows)
            cols (reduce #'min here :key #'watcher-cols)))
    (setf rows (max 1 (min rows +biggest-pane+))
          cols (max 1 (min cols +biggest-pane+)))
    (unless (and (= rows (vt:term-height (session-pane-term session)))
                 (= cols (vt:term-width (session-pane-term session))))
      (pane-resize (session-pane session) rows cols)
      (screen-resize (session-screen session) cols rows)
      (dolist (w (session-watchers session))
        (setf (watcher-shadow w) (make-screen :width cols :height rows)
              (watcher-told w) nil
              (watcher-behind w) t)
        (when (watcher-here w)
          (ignore-errors
           (tell w (list :hello (session-name session) rows cols))))))
    (setf (session-rows session) rows
          (session-cols session) cols)))

(defun session-compose (session)
  (let ((screen (session-screen session))
        (term (session-pane-term session)))
    (screen-blit screen term)
    (setf (screen-cursor-x screen) (min (vt:term-cursor-x term)
                                        (1- (screen-width screen)))
          (screen-cursor-y screen) (min (vt:term-cursor-y term)
                                        (1- (screen-height screen)))
          (screen-cursor-visible screen) (vt:term-cursor-visible term)
          (screen-cursor-style screen) (vt:term-cursor-style term))
    screen))

(declaim (ftype function tell))

(defun tell (watcher form)
  (wire-send (watcher-wire watcher) form))

(defun frame-for (session watcher)
  (let* ((screen (session-screen session))
         (runs (screen-diff (watcher-shadow watcher) screen)))
    (when runs
      (multiple-value-bind (said faces) (runs-said screen runs)
        (tell watcher (list :frame said faces))))
    (let ((now (list :cursor (screen-cursor-y screen) (screen-cursor-x screen)
                     (screen-cursor-visible screen) (screen-cursor-style screen))))
      (unless (equal now (watcher-told watcher))
        (tell watcher now)
        (setf (watcher-told watcher) now)))
    (when (pane-rang (session-pane session))
      (tell watcher '(:bell)))
    runs))

(defun drop-watcher (session watcher &optional why)
  (when why
    (ignore-errors (tell watcher (list :bye why))
                   (wire-flush (watcher-wire watcher))))
  (wire-close (watcher-wire watcher))
  (setf (session-watchers session) (remove watcher (session-watchers session)))
  (session-fit session))

(defun heard (server session watcher form)
  (case (first form)
    (:attach
     (destructuring-bind (rows cols takes) (rest form)
       (setf (watcher-rows watcher) (max 1 (min +biggest-pane+ rows))
             (watcher-cols watcher) (max 1 (min +biggest-pane+ cols))
             (watcher-takes watcher) takes
             (watcher-here watcher) t)
       (session-fit session)
       (setf (watcher-shadow watcher)
             (make-screen :width (session-cols session)
                          :height (session-rows session))
             (watcher-told watcher) nil
             (watcher-behind watcher) t)
       (tell watcher (list :hello (session-name session)
                           (session-rows session) (session-cols session)))))
    (:keys
     (pane-say (session-pane session) (second form)))
    (:resize
     (destructuring-bind (rows cols) (rest form)
       (setf (watcher-rows watcher) (max 1 (min +biggest-pane+ rows))
             (watcher-cols watcher) (max 1 (min +biggest-pane+ cols)))
       (session-fit session)))
    (:detach (drop-watcher session watcher))
    (:stop (setf (server-going server) nil))
    (t nil)))

(defun take-in (server session watcher)
  (let ((wire (watcher-wire watcher)))
    (if (null (wire-fill wire))
        (drop-watcher session watcher)
        (loop for form = (handler-case (wire-take wire)
                           (error () (drop-watcher session watcher) nil))
              while form
              do (heard server session watcher form)
              while (wire-open wire)))))

(defun take-a-watcher (server session)
  (let ((socket (handler-case (sb-bsd-sockets:socket-accept (server-socket server))
                  (error () nil))))
    (when socket
      (let ((watcher (%make-watcher
                      :socket socket
                      :wire (make-wire (sb-bsd-sockets:socket-file-descriptor
                                        socket)
                                       socket))))
        (push watcher (session-watchers session))
        watcher))))

(defun server-step (server &key (interval *interval*))
  (let* ((session (first (server-sessions server)))
         (w (waiting-clear (server-waiting server)))
         (now (nanos))
         (gap (* interval 1000000))
         (due (if (some (lambda (c) (and (watcher-here c)
                                         (or (watcher-behind c)
                                             (pane-dirty (session-pane session)))))
                        (session-watchers session))
                  (max 0 (ceiling (- gap (- now (watcher-oldest session)))
                                  1000000))
                  100))
         (listening (waiting-add w (server-fd server)))
         (pty (waiting-add w (pane-fd (session-pane session))))
         (wires (mapcar (lambda (c)
                          (cons c (waiting-add
                                   w (wire-fd (watcher-wire c))
                                   (logior sb-unix:pollin
                                           (if (plusp (wire-pending
                                                       (watcher-wire c)))
                                               sb-unix:pollout
                                               0)))))
                        (session-watchers session))))
    (wait-on w due)
    (when (readable-p (waiting-back w listening))
      (take-a-watcher server session))
    (loop for (watcher . n) in wires
          do (when (wire-open (watcher-wire watcher))
               (when (writable-p (waiting-back w n))
                 (wire-flush (watcher-wire watcher)))
               (when (readable-p (waiting-back w n))
                 (take-in server session watcher))))
    (unless (if (readable-p (waiting-back w pty))
                (pane-drain (session-pane session))
                t)
      (dolist (watcher (copy-list (session-watchers session)))
        (drop-watcher session watcher :done))
      (setf (server-sessions server) (remove session (server-sessions server)))
      (pane-close (session-pane session))
      (unless (server-sessions server)
        (setf (server-going server) nil))
      (return-from server-step server))
    (when (pane-dirty (session-pane session))
      (dolist (watcher (session-watchers session))
        (setf (watcher-behind watcher) t))
      (setf (pane-dirty (session-pane session)) nil))
    (let ((composed nil)
          (then (nanos)))
      (dolist (watcher (session-watchers session))
        (when (and (watcher-behind watcher)
                   (watcher-here watcher)
                   (wire-open (watcher-wire watcher))
                   (zerop (wire-pending (watcher-wire watcher)))
                   (>= (- then (watcher-sent watcher)) gap))
          (unless composed
            (session-compose session)
            (setf composed t))
          (frame-for session watcher)
          (setf (watcher-sent watcher) then
                (watcher-behind watcher) nil)
          (wire-flush (watcher-wire watcher))))
      (when composed
        (setf (pane-rang (session-pane session)) nil)))
    server))

(defparameter +faults+ 10)

(defun say-what-broke (e)
  (format *error-output* "~&vt-mux: ~A~%" e)
  (ignore-errors
   (sb-debug:print-backtrace :stream *error-output* :count 30))
  (finish-output *error-output*))

(defun serve (path command &key (rows 24) (cols 80) (interval *interval*))
  "Hold the sessions and feed whoever is watching them, until there are none.

A fault in one wakeup is said and stepped over rather than taken as the end. The
panes are somebody's shells: losing them to a bug in the emulator is worse than
drawing one frame wrong, and the backtrace is in the log either way. Faults one
after another with nothing between them are a loop rather than a mishap, and
that does end it."
  (let ((server (make-server path))
        (faults 0))
    (unwind-protect
         (progn
           (add-session server command :rows rows :cols cols)
           (loop while (and (server-going server) (server-sessions server))
                 do (handler-case
                        (progn (server-step server :interval interval)
                               (setf faults 0))
                      (error (e)
                        (say-what-broke e)
                        (when (> (incf faults) +faults+)
                          (format *error-output*
                                  "~&vt-mux: ~D faults with nothing between them; stopping.~%"
                                  faults)
                          (setf (server-going server) nil)))))
           server)
      (server-close server))))
