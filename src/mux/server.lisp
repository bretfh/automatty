;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx)

(defvar *interval* 8
  "The least milliseconds between two frames to one client.

A least gap, never a tick: a client that has heard nothing for a while is
answered the moment a byte arrives, and only a client already being fed faster
than this waits. A tick would add half its length to every keystroke, which is
more than the terminal it is sitting inside costs in the first place.")

(declaim (ftype function session-bar session-compose greet tell))
(declaim (special +prompt-toggles+))

(defparameter +biggest-pane+ 1000)

(defparameter +understood+
  '(:want :attach :go :new :sessions :knock :detach :stop
    :keys :resize :bar :split :focus :close :only :mouse-at)
  "Every message this server knows what to do with.

It goes out with the greeting. A server outlives the builds that reach it: it
has been running since whenever, and the client is the one that was started a
moment ago. So a client has to be able to say which of its keys this server
cannot do, rather than sending one and leaving a key that looks broken.")

(defstruct (watcher (:constructor %make-watcher))
  (wire nil)
  (socket nil)
  (session nil)
  (want nil)
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
  (layout nil)
  (focus nil)
  (screen nil)
  (geometry nil)
  (barp t)
  (search-kind 0 :type fixnum)
  (watchers nil)
  (clocked 0 :type integer)
  (rows 24 :type fixnum)
  (cols 80 :type fixnum))

(defparameter +bar-gap+ 1000000000
  "How long the bar may stand before the session is composed again.

Everything else on the screen is drawn because something happened. The clock on
the bar is not: nothing tells the server the minute turned. So a session
somebody is watching is marked behind this often, and the diff decides whether
anything actually moved.")

(defstruct (server (:constructor %make-server))
  (path nil)
  (socket nil)
  (fd -1 :type fixnum)
  (sessions nil)
  (knocking nil)
  (command nil)
  (waiting nil)
  (going t :type boolean))

(defun nanos ()
  (multiple-value-bind (sec nsec) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* sec 1000000000) nsec)))

(defun listen-on (path)
  "Take PATH as the name to answer on, and let nobody but its owner open it.

Anybody who can open it can type into the shells behind it, so the permissions
on it are the whole of who may."
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (ignore-errors (delete-file path))
    (sb-bsd-sockets:socket-bind socket path)
    (ignore-errors (sb-posix:chmod path #o600))
    (sb-bsd-sockets:socket-listen socket 8)
    socket))

(defun make-server (path)
  (let ((socket (listen-on path)))
    (let ((fd (sb-bsd-sockets:socket-file-descriptor socket)))
      (sb-posix:fcntl fd sb-posix:f-setfl
                      (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                              sb-posix:o-nonblock))
      (%make-server :path path :socket socket :fd fd
                    :waiting (tty:make-waiting 16)))))

(defun server-close (server)
  ;; the name goes first. Whatever else takes a while, a shell that will not go
  ;; or a client that will not read, nobody new must be able to reach a server
  ;; that is already leaving.
  (ignore-errors (sb-bsd-sockets:socket-close (server-socket server)))
  (ignore-errors (delete-file (server-path server)))
  (dolist (w (server-knocking server)) (wire-close (watcher-wire w)))
  (dolist (session (server-sessions server))
    (dolist (w (session-watchers session))
      (wire-close (watcher-wire w)))
    (mapc #'pane-close (session-panes session)))
  (setf (server-sessions server) nil
        (server-knocking server) nil)
  (tty:free-waiting (server-waiting server))
  (setf (server-going server) nil))

(defun add-session (server command &key (name "0") (rows 24) (cols 80))
  (let* ((pane (make-pane command :rows rows :cols cols))
         (session (%make-session :name name :rows rows :cols cols
                                 :layout pane :focus pane
                                 :screen (tty:make-screen :width cols
                                                          :height rows))))
    (session-compose session)
    (pane-start pane)
    (setf (server-sessions server) (append (server-sessions server) (list session)))
    session))

(defun session-named (server name)
  "The session called NAME, or the first one when no name is asked for."
  (if (and name (plusp (length (princ-to-string name))))
      (find (princ-to-string name) (server-sessions server)
            :key #'session-name :test #'string=)
      (first (server-sessions server))))

(defun a-free-name (server)
  (loop :for n :from 0
        :for name := (princ-to-string n)
        :unless (session-named server name) :do (return name)))

(defun session-panes (session) (panes-in (session-layout session)))

(defun split-the-session (session way)
  "Another pane beside the one that has the cursor, running what that one runs."
  (let* ((focus (session-focus session))
         (term (pane-term focus))
         (new (make-pane (pane-command focus)
                         :rows (vt:term-height term)
                         :cols (vt:term-width term))))
    (setf (session-layout session)
          (put-beside (session-layout session) focus way new)
          (session-focus session) new)
    (session-compose session)
    (pane-start new)
    (dolist (w (session-watchers session)) (setf (watcher-behind w) t))
    new))

(defun close-the-pane (session pane)
  "Take PANE out of the session and let its program go."
  (setf (session-layout session) (without-pane (session-layout session) pane))
  (pane-close pane)
  (let ((left (session-panes session)))
    (when (eq (session-focus session) pane)
      (setf (session-focus session) (first left)))
    (dolist (w (session-watchers session)) (setf (watcher-behind w) t))
    left))

(defun only-the-pane (session pane)
  "Every other pane in the session goes, and PANE has the whole of it."
  (dolist (other (remove pane (session-panes session)))
    (close-the-pane session other))
  (session-panes session))

(defun mouse-at (session watcher x y)
  "Whatever was at (X, Y) the last time this session was drawn is told about
the click: the toggle on the search bar cycles which prompt it opens, another
bar button tells WATCHER, the one who clicked, to run what it is for, and a
pane becomes the focus. A geometry from before the first draw, or a click that
landed on a rule, does nothing."
  (let ((hit (and (session-geometry session)
                  (vtx/ui:under (session-geometry session) y x))))
    (cond
      ((and (typep hit 'bar-button) (eq (bar-button-runs hit) :cycle-search-kind))
       (setf (session-search-kind session)
             (mod (1+ (session-search-kind session)) (length +prompt-toggles+)))
       (dolist (w (session-watchers session)) (setf (watcher-behind w) t)))
      ((typep hit 'bar-button) (tell watcher (list :do (bar-button-runs hit))))
      ((and (typep hit 'pane-view) (not (eq (view-pane hit) (session-focus session))))
       (setf (session-focus session) (view-pane hit))
       (dolist (w (session-watchers session)) (setf (watcher-behind w) t))))))

(defun focus-the-next (session)
  (let* ((panes (session-panes session))
         (at (position (session-focus session) panes)))
    (when panes
      (setf (session-focus session)
            (nth (mod (1+ (or at -1)) (length panes)) panes))
      (dolist (w (session-watchers session)) (setf (watcher-behind w) t)))
    (session-focus session)))

;;; how big the pane is: the smallest any watcher can show, so nobody is shown a
;;; screen with a piece missing.

(defun session-pane-term (session)
  (pane-term (session-focus session)))

(defun watcher-oldest (session)
  (let ((here (remove-if-not #'watcher-here (session-watchers session))))
    (if here (reduce #'min here :key #'watcher-sent) 0)))

(defun session-start-over (session)
  "The session is a different size. Nobody watching knows what is on their own
screen any more, so every shadow goes and everybody is told the new size."
  (let ((rows (session-rows session))
        (cols (session-cols session)))
    (tty:screen-resize (session-screen session) cols rows)
    (dolist (pane (session-panes session)) (setf (pane-dirty pane) t))
    (dolist (w (session-watchers session))
      (setf (watcher-shadow w) (tty:make-screen :width cols :height rows)
            (watcher-told w) nil
            (watcher-behind w) t)
      (when (watcher-here w)
        (ignore-errors (greet w session))))))

(defun session-fit (session)
  "As big as the smallest watcher can show, so nobody is shown a screen with a
piece missing. What each pane gets out of that is the layout pass's business,
not this one's."
  (let ((rows (session-rows session))
        (cols (session-cols session))
        (here (remove-if-not #'watcher-here (session-watchers session))))
    (when here
      (setf rows (reduce #'min here :key #'watcher-rows)
            cols (reduce #'min here :key #'watcher-cols)))
    (setf rows (max 1 (min rows +biggest-pane+))
          cols (max 1 (min cols +biggest-pane+)))
    (unless (and (= rows (session-rows session))
                 (= cols (session-cols session))
                 (= rows (tty:screen-height (session-screen session)))
                 (= cols (tty:screen-width (session-screen session))))
      (setf (session-rows session) rows
            (session-cols session) cols)
      (session-start-over session)
      t)))

(defun session-clock (session now)
  "Put everybody watching behind when the bar has stood for +BAR-GAP+. Answers
whether it did."
  (when (>= (- now (session-clocked session)) +bar-gap+)
    (setf (session-clocked session) now)
    (dolist (watcher (session-watchers session))
      (setf (watcher-behind watcher) t))
    t))

(defun session-tree (session)
  "What the session looks like: the bar, and the panes under it."
  (vtx/ui:column
   :align :stretch
   (session-bar session)
   (layout-tree (session-layout session) (session-focus session))))

(defun fit-panes (tree)
  "Give each pane the room the layout gave its view."
  (dolist (v (views-in tree))
    (let ((pane (view-pane v))
          (rows (max 1 (vtx/ui:height v)))
          (cols (max 1 (vtx/ui:width v))))
      (unless (and (= rows (vt:term-height (pane-term pane)))
                   (= cols (vt:term-width (pane-term pane))))
        (pane-resize pane rows cols)))))

(defun put-the-cursor (session tree)
  "The cursor sits where the pane it belongs to says, moved to where that pane
was put."
  (let* ((screen (session-screen session))
         (v (view-of tree (session-focus session)))
         (term (session-pane-term session)))
    (setf (tty:screen-cursor-x screen)
          (min (+ (if v (vtx/ui:left v) 0) (vt:term-cursor-x term))
               (1- (tty:screen-width screen)))
          (tty:screen-cursor-y screen)
          (min (+ (if v (vtx/ui:top v) 0) (vt:term-cursor-y term))
               (1- (tty:screen-height screen)))
          (tty:screen-cursor-visible screen) (vt:term-cursor-visible term)
          (tty:screen-cursor-style screen) (vt:term-cursor-style term))))

(defun session-compose (session)
  "Measure the session, lay it out, give each pane what it was given, and paint
it. One pass: the panes are resized between the laying and the painting, so what
is drawn is what they have just been told they are."
  (let* ((screen (session-screen session))
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (tree (session-tree session))
         (m (vtx/cells:make-cells (tty:screen-grid screen) cols rows)))
    (vtx/ui:with-pass
      (vtx/ui:restyle tree)
      (vtx/ui:measure tree m cols rows)
      (vtx/ui:lay tree m 0 0 cols rows)
      (setf (session-geometry session) tree)
      (fit-panes tree)
      (vtx/ui:paint tree m))
    (put-the-cursor session tree)
    screen))

(declaim (ftype function tell))

(defun tell (watcher form)
  (wire-send (watcher-wire watcher) form))

(defun greet (watcher session)
  "Tell WATCHER what it is looking at, and what this server can be asked to do."
  (tell watcher (list :hello (session-name session)
                      (session-rows session) (session-cols session)
                      +understood+)))

(defun frame-for (session watcher)
  (let* ((screen (session-screen session))
         (runs (tty:screen-diff (watcher-shadow watcher) screen)))
    (when runs
      (multiple-value-bind (said faces) (runs-said screen runs)
        (tell watcher (list :frame said faces))))
    (let ((now (list :cursor (tty:screen-cursor-y screen) (tty:screen-cursor-x screen)
                     (tty:screen-cursor-visible screen) (tty:screen-cursor-style screen))))
      (unless (equal now (watcher-told watcher))
        (tell watcher now)
        (setf (watcher-told watcher) now)))
    (when (some #'pane-rang (session-panes session))
      (tell watcher '(:bell)))
    runs))

(defun leave-session (watcher)
  "Take WATCHER off whatever session it was on."
  (let ((session (watcher-session watcher)))
    (when session
      (setf (session-watchers session)
            (remove watcher (session-watchers session))
            (watcher-session watcher) nil)
      (session-fit session))
    session))

(defun drop-watcher (server watcher &optional why)
  (when why
    (ignore-errors (tell watcher (list :bye why))
                   (wire-flush (watcher-wire watcher))))
  (wire-close (watcher-wire watcher))
  (setf (server-knocking server) (remove watcher (server-knocking server)))
  (leave-session watcher))

(defun join-session (server watcher session)
  "Put WATCHER on SESSION and tell it what it is looking at."
  (leave-session watcher)
  (setf (server-knocking server) (remove watcher (server-knocking server))
        (watcher-session watcher) session)
  (push watcher (session-watchers session))
  ;; a fit that changed the size has already told everybody, this one included;
  ;; only a fit that changed nothing leaves it to be said here
  (unless (session-fit session)
    (setf (watcher-shadow watcher)
          (tty:make-screen :width (session-cols session)
                           :height (session-rows session))
          (watcher-told watcher) nil
          (watcher-behind watcher) t)
    (greet watcher session))
  session)

(defun heard (server watcher form)
  "What a client said. Which session it is on is the watcher's own; anything
that is about a session is passed on only once it has joined one."
  (let ((session (watcher-session watcher)))
    (case (first form)
      (:want (setf (watcher-want watcher) (second form)))
      (:attach
       (destructuring-bind (rows cols takes) (rest form)
         (setf (watcher-rows watcher) (max 1 (min +biggest-pane+ rows))
               (watcher-cols watcher) (max 1 (min +biggest-pane+ cols))
               (watcher-takes watcher) takes
               (watcher-here watcher) t)
         (let ((want (session-named server (watcher-want watcher))))
           (if want
               (join-session server watcher want)
               (drop-watcher server watcher :no-such-session)))))
      (:go
       (let ((want (session-named server (second form))))
         (when want (join-session server watcher want))))
      (:new
       (destructuring-bind (&optional name command) (rest form)
         (join-session server watcher
                       (add-session server (or command (server-command server))
                                    :name (or name (a-free-name server))
                                    :rows (watcher-rows watcher)
                                    :cols (watcher-cols watcher)))))
      (:sessions
       (tell watcher
             (list :these
                   (mapcar (lambda (s)
                             (list (session-name s) (session-rows s)
                                   (session-cols s)
                                   (length (session-panes s))
                                   (length (session-watchers s))))
                           (server-sessions server)))))
      (:knock (tell watcher (list :here (server-path server))))
      (:detach (drop-watcher server watcher))
      (:stop (setf (server-going server) nil))
      (t (and session (heard-about-a-session session watcher form))))
    t))

(defun heard-about-a-session (session watcher form)
  (case (first form)
    (:keys
     (pane-say (session-focus session) (second form)) t)
    (:resize
     (destructuring-bind (rows cols) (rest form)
       (setf (watcher-rows watcher) (max 1 (min +biggest-pane+ rows))
             (watcher-cols watcher) (max 1 (min +biggest-pane+ cols)))
       (session-fit session))
     t)
    (:bar
     ;; the bar is the session's, not the client's: whoever asked, everybody
     ;; attached is looking at the same one
     (setf (session-barp session) (if (eq (second form) :toggle)
                                      (not (session-barp session))
                                      (and (second form) t)))
     (session-start-over session)
     t)
    (:split (split-the-session session (or (second form) :across)) t)
    (:focus (focus-the-next session) t)
    (:close (close-the-pane session (session-focus session)) t)
    (:only (only-the-pane session (session-focus session)) t)
    (:mouse-at
     (destructuring-bind (x y) (rest form) (mouse-at session watcher x y))
     t)
    (t nil)))

(defun take-in (server watcher)
  "Read what the client said and act on it.

A message this server cannot make sense of is said to the log and passed over.
A client is not always the same build as the server it reached: it is the one
that was just started, and the server has been running since whenever. One
message it does not know must not take down the sessions everybody else is
looking at."
  (let ((wire (watcher-wire watcher)))
    (if (null (wire-fill wire))
        (drop-watcher server watcher)
        (loop for form = (handler-case (wire-take wire)
                           (error () (drop-watcher server watcher) nil))
              while form
              do (handler-case
                     (unless (heard server watcher form)
                       (format *error-output* "~&vtx: nothing here does ~S~%"
                               (and (consp form) (first form)))
                       (finish-output *error-output*))
                   (error (e)
                     (format *error-output* "~&vtx: ~S: ~A~%"
                             (and (consp form) (first form)) e)
                     (finish-output *error-output*)))
              while (wire-open wire)))))

(defun take-a-watcher (server)
  "Somebody has opened the socket. Which session they want is something they
have yet to say, so until they do they are the server's rather than any
session's."
  (let ((socket (handler-case (sb-bsd-sockets:socket-accept (server-socket server))
                  (error () nil))))
    (when socket
      (let ((watcher (%make-watcher
                      :socket socket
                      :wire (make-wire (sb-bsd-sockets:socket-file-descriptor
                                        socket)
                                       socket))))
        (push watcher (server-knocking server))
        watcher))))

(defun end-the-session (server session why)
  (dolist (watcher (copy-list (session-watchers session)))
    (drop-watcher server watcher why))
  (setf (server-sessions server) (remove session (server-sessions server)))
  (mapc #'pane-close (session-panes session))
  (setf (session-layout session) nil
        (session-focus session) nil)
  (unless (server-sessions server)
    (setf (server-going server) nil))
  server)

(defun session-due-p (session)
  "Whether anybody watching SESSION is owed a frame."
  (let ((panes (session-panes session)))
    (some (lambda (c) (and (watcher-here c)
                           (or (watcher-behind c) (some #'pane-dirty panes))))
          (session-watchers session))))

(defun oldest-owed (sessions)
  "When the longest-waiting watcher of any session that is owed a frame was last
sent one, or nothing when nobody is owed one."
  (let ((owed (mapcar #'watcher-oldest (remove-if-not #'session-due-p sessions))))
    (when owed (reduce #'min owed))))

(defun session-serve (session gap)
  "Draw SESSION once, for whoever is behind and has waited GAP."
  (let ((panes (session-panes session))
        (composed nil)
        (then (nanos)))
    (when (some #'pane-dirty panes)
      (dolist (watcher (session-watchers session))
        (setf (watcher-behind watcher) t))
      (dolist (pane panes) (setf (pane-dirty pane) nil)))
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
      (dolist (pane panes) (setf (pane-rang pane) nil)))
    composed))

(defun server-step (server &key (interval *interval*))
  (let* ((sessions (server-sessions server))
         (w (tty:waiting-clear (server-waiting server)))
         (now (nanos))
         (gap (* interval 1000000))
         (oldest (oldest-owed sessions))
         (due (if oldest
                  (max 0 (ceiling (- gap (- now oldest)) 1000000))
                  100))
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
                        (append (server-knocking server)
                                (loop :for session :in sessions
                                      :append (session-watchers session))))))
    (tty:wait-on w due)
    (dolist (session sessions) (session-clock session (nanos)))
    (when (tty:readable-p (tty:waiting-back w listening))
      (take-a-watcher server))
    (loop :for (watcher . n) :in wires
          :do (if (wire-open (watcher-wire watcher))
                  (progn
                    (when (tty:writable-p (tty:waiting-back w n))
                      (wire-flush (watcher-wire watcher)))
                    (when (tty:readable-p (tty:waiting-back w n))
                      (take-in server watcher))
                    ;; a descriptor the kernel says is done with has nothing
                    ;; more to give, and polling it again is polling nothing
                    (when (and (tty:gone-p (tty:waiting-back w n))
                               (wire-open (watcher-wire watcher)))
                      (drop-watcher server watcher)))
                  ;; a write that came apart shuts the wire here; without this
                  ;; the watcher stays on the session and its closed descriptor
                  ;; is put to poll every wakeup for as long as the server runs
                  (drop-watcher server watcher)))
    (let ((done nil))
      (loop :for (session pane n) :in ptys
            :do (when (and (member pane (session-panes session))
                           (tty:readable-p (tty:waiting-back w n))
                           (not (pane-drain pane)))
                  (push (cons session pane) done)))
      (loop :for (session . pane) :in done
            :do (close-the-pane session pane)))
    (dolist (session sessions)
      (if (session-layout session)
          (session-serve session gap)
          (end-the-session server session :done)))
    server))

(defparameter +faults+ 10)

(defun say-what-broke (e)
  (format *error-output* "~&vtx: ~A~%" e)
  (ignore-errors
   (sb-debug:print-backtrace :stream *error-output* :count 30))
  (finish-output *error-output*))

(defun serve (path command &key (name "0") (rows 24) (cols 80)
                                (interval *interval*))
  "Hold the sessions and feed whoever is watching them, until there are none.

A fault in one wakeup is said and stepped over rather than taken as the end. The
panes are somebody's shells: losing them to a bug in the emulator is worse than
drawing one frame wrong, and the backtrace is in the log either way. Faults one
after another with nothing between them are a loop rather than a mishap, and
that does end it."
  (let ((server (make-server path))
        (faults 0))
    (setf (server-command server) command)
    (unwind-protect
         (progn
           (add-session server command :name name :rows rows :cols cols)
           (loop while (and (server-going server) (server-sessions server))
                 do (handler-case
                        (progn (server-step server :interval interval)
                               (setf faults 0))
                      (error (e)
                        (say-what-broke e)
                        (when (> (incf faults) +faults+)
                          (format *error-output*
                                  "~&vtx: ~D faults with nothing between them; stopping.~%"
                                  faults)
                          (setf (server-going server) nil)))))
           server)
      (server-close server))))
