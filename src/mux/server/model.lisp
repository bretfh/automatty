;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defvar *interval* 8
  "The least milliseconds between two frames to one client.

A least gap, never a tick: a client that has heard nothing for a while is
answered the moment a byte arrives, and only a client already being fed faster
than this waits. A tick would add half its length to every keystroke, which is
more than the terminal it is sitting inside costs in the first place.")

(declaim (ftype function session-bar session-compose send-hello send-message load-user-init init-load-note
                        send-config run-init-command encode-settings))

(defparameter +max-pane-size+ 1000)

(defvar *watcher-count* 0)

(defun now-ms () (floor (monotonic-ns) 1000000))

(defun actor-of (watcher &optional caller-pane)
  "Who is doing what WATCHER asked: the pane CALLER names when an agent verb was
run inside one, the client when it is somebody attached, and otherwise the
command line."
  (cond ((and (stringp caller-pane) (plusp (length caller-pane))) (list :pane caller-pane))
        ((watcher-interactive watcher) (list :client (watcher-id watcher) (watcher-tty watcher)))
        (t (list :cli))))

(defstruct (watcher (:constructor %make-watcher))
  (wire nil)
  (socket nil)
  (session nil)
  (wanted-session nil)
  (rows 24 :type fixnum)
  (cols 80 :type fixnum)
  (takes t)
  (shadow nil)
  (told nil)
  (behind t :type boolean)
  (sent 0 :type fixnum)
  (interactive nil :type boolean)
  (id (incf *watcher-count*) :type fixnum)
  (tty nil)
  (since 0 :type integer)
  (typed-at 0 :type integer)
  (following nil)                       ; the id of the watcher this one goes where
  (watch-panes nil)
  (rows-sent (make-hash-table :test 'equal))
  (screen-rows nil)
  (screens-sent (make-hash-table :test 'equal))
  (config-sent nil)
  (bracketed-sent nil))

;;; A window is what a session shows at one time: a layout of panes, which of
;;; them has the focus, and whether one of them is zoomed. A session holds its
;;; windows in order and shows one of them, the way tmux does, and a client on
;;; the session sees whichever window it is showing.

(defstruct (window (:constructor %make-window))
  (label nil)
  (layout nil)
  (focus nil)
  (zoomed nil))

(defstruct (session (:constructor %make-session))
  (name "0")
  (socket nil)
  (windows nil)
  (window nil)
  (screen nil)
  (geometry nil)
  (bar-p t)
  (field-kind 0 :type fixnum)
  (watchers nil)
  (clocked 0 :type integer)
  (rows 24 :type fixnum)
  (cols 80 :type fixnum)
  (scrollbars-p t)
  (held nil)
  (readers nil)
  (server nil))

;;; The layout, the focus and the zoom are the current window's. They read and
;;; set as they always did, so everything that works on what is on screen
;;; works on the window being shown without knowing there are others.

(defun session-layout (session) (window-layout (session-window session)))
(defun (setf session-layout) (new session)
  (setf (window-layout (session-window session)) new))
(defun session-focus (session) (window-focus (session-window session)))
(defun (setf session-focus) (new session)
  (setf (window-focus (session-window session)) new))
(defun session-zoomed (session) (window-zoomed (session-window session)))
(defun (setf session-zoomed) (new session)
  (setf (window-zoomed (session-window session)) new))

(defun window-panes (window) (layout-panes (window-layout window)))

(defun window-number (session window)
  "Which window WINDOW is in SESSION, counting from 1, as the bar and an address say it."
  (let ((at (position window (session-windows session))))
    (and at (1+ at))))

(defun window-of (session pane)
  "The window PANE is in, and its number."
  (dolist (w (session-windows session))
    (when (member pane (window-panes w))
      (return (values w (window-number session w))))))

(defun pane-number (session pane)
  "PANE's number within its window, from 1: what its frame and its address say."
  (let ((w (window-of session pane)))
    (and w (1+ (position pane (window-panes w))))))

(defun pane-address-of (session pane)
  "PANE's address, session:window.pane, or session:id while it is in no window yet."
  (multiple-value-bind (w n) (window-of session pane)
    (if w
        (format nil "~A:~D.~D" (session-name session) n (pane-number session pane))
        (format nil "~A:~D" (session-name session) (pane-id pane)))))

(defparameter +bar-refresh-interval+ 1000000000
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
  (pending-watchers nil)
  (command nil)
  (waiting nil)
  (tasks nil)
  (born 0 :type integer)
  (had-sessions nil :type boolean)
  (notes nil)
  (saving nil :type boolean)
  (tree-saved nil)
  ;; where the next server is when this one was asked to restart: the atty
  ;; that asked, so that a newer build on PATH is what comes back
  (successor nil)
  ;; the latest release, once asked for, and whether it has been said
  (release nil)
  (release-noted nil)
  (release-checked-at 0 :type integer)
  (running t :type boolean))

(defparameter +first-session-timeout+ 10000000000
  "How long a server started with no session waits for somebody to ask for
one, in nanoseconds. A server nobody reaches in that time was started by a
client that has since gone, and holding the socket helps nobody.")

(defun executable-path ()
  "This program, when it is a program.

An executable core is its own runtime, so the server a client starts is another
of this told to serve rather than an sbcl told what to load. Out of a repl it is
neither, and there is nothing to run."
  (let ((runtime (and sb-ext:*runtime-pathname*
                      (namestring sb-ext:*runtime-pathname*)))
        (core (and sb-ext:*core-pathname* (namestring sb-ext:*core-pathname*))))
    (when (and runtime core (string= runtime core)) runtime)))

(defun executable-p (path)
  "Whether PATH names something this user may run."
  (and (stringp path)
       (handler-case (progn (sb-posix:access path sb-posix:x-ok) t)
         (error () nil))))

(defparameter +release-check-every+ (* 24 60 60 1000)
  "How often the server asks whether a newer release is out, in milliseconds.")

(defun schedule-release-check (server)
  "Every hour, when +CHECK-FOR-UPDATES+ says to and a day has passed, ask
for the latest release off the server's thread, and put one note about it
where the next client to attach sees it. Nothing is ever fetched or swapped
by this: that is somebody's to do."
  (labels ((tick ()
             (when (server-running server)
               (let ((now (now-ms)))
                 (when (and +check-for-updates+
                            (>= (- now (server-release-checked-at server)) +release-check-every+))
                   (setf (server-release-checked-at server) now)
                   (sb-thread:make-thread
                    (lambda ()
                      (let ((tag (ignore-errors (latest-release))))
                        (when tag (setf (server-release server) tag))))
                    :name "asking for the latest release")))
               (let ((tag (server-release server)))
                 (when (and tag (not (server-release-noted server))
                            (not (equal tag *version*)))
                   (setf (server-release-noted server) t)
                   (push (list (release-message tag) :accent) (server-notes server))))
               (schedule-task server (* 60 60 1000) #'tick))))
    ;; a minute in, not at once: a server has enough to do as it starts
    (setf (server-release-checked-at server) (- (now-ms) +release-check-every+ (* -60 1000)))
    (schedule-task server (* 60 1000) #'tick)))

(defun server-needed-p (server now)
  "Whether the server has a reason to go on: a session, or no session yet and
a client that may still be on its way."
  (and (server-running server)
       (or (server-sessions server)
           (and (not (server-had-sessions server))
                (< (- now (server-born server)) +first-session-timeout+)))))

(defparameter +enter-delay+ 300)

(defun schedule-task (server milliseconds thunk)
  (push (cons (+ (monotonic-ns) (* milliseconds 1000000)) thunk) (server-tasks server)))

(defun next-task-delay (server now)
  (loop :for (when . nil) :in (server-tasks server)
        :minimize (max 0 (ceiling (- when now) 1000000))))

(defun run-due-tasks (server now)
  (let ((due (remove-if-not (lambda (it) (<= (car it) now)) (server-tasks server))))
    (setf (server-tasks server) (set-difference (server-tasks server) due))
    (dolist (it (reverse due)) (funcall (cdr it)))))

(defun monotonic-ns ()
  ;; the internal real time is monotonic on every platform sbcl runs on, and
  ;; sb-unix names its clocks differently on each: this asks nothing of them
  (* (get-internal-real-time)
     #.(floor 1000000000 internal-time-units-per-second)))

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

(defun server-listen (server)
  "Take the server's name: from here on a client can reach it."
  (let* ((socket (listen-on (server-path server)))
         (fd (sb-bsd-sockets:socket-file-descriptor socket)))
    (sb-posix:fcntl fd sb-posix:f-setfl
                    (logior (sb-posix:fcntl fd sb-posix:f-getfl)
                            sb-posix:o-nonblock))
    (setf (server-socket server) socket
          (server-fd server) fd)
    server))

(defun make-server (path &key (listening t))
  "A server on PATH. With LISTENING nil it is not yet reachable: what it
brings back from disk is put in place first, so nobody sees half of it."
  (let ((server (%make-server :path path :born (monotonic-ns)
                              :waiting (tty:make-waiting 16))))
    (when listening (server-listen server))
    server))

(defun server-close (server)
  ;; the name goes first. Whatever else takes a while, a shell that will not go
  ;; or a client that will not read, nobody new must be able to reach a server
  ;; that is already leaving.
  (ignore-errors (sb-bsd-sockets:socket-close (server-socket server)))
  (ignore-errors (delete-file (server-path server)))
  ;; then what it holds goes to disk, before the programs are let go: once the
  ;; name is gone, the state is whole
  (when (server-saving server)
    (handler-case (save-all server)
      (error (e) (report-error e))))
  ;; what was already said is still sent: the last thing a server does is often
  ;; answer whoever asked it to stop, and a reply thrown away with the socket
  ;; reads to them as a server that never heard
  (dolist (w (append (server-pending-watchers server)
                     (loop :for s :in (server-sessions server)
                           :append (session-watchers s))))
    (ignore-errors (wire-flush (watcher-wire w))))
  (dolist (w (server-pending-watchers server)) (wire-close (watcher-wire w)))
  (dolist (session (server-sessions server))
    (dolist (w (session-watchers session))
      (wire-close (watcher-wire w)))
    (mapc #'pane-close (session-panes session)))
  (setf (server-sessions server) nil
        (server-pending-watchers server) nil)
  (tty:free-waiting (server-waiting server))
  (setf (server-running server) nil))

(defun pane-environment (session pane)
  "What a program is told about where it is: its address as it starts. A pane
moved to another window keeps the address it was born with; the server answers
either."
  (list (format nil "ATTY_PANE=~A" (pane-address-of session pane))
        (format nil "ATTY_SOCKET=~A" (or (session-socket session) ""))))

(defun add-session (server command &key (name "0") (rows 24) (cols 80) directory)
  (let* ((pane (make-pane command :rows rows :cols cols :directory directory))
         (window (%make-window :layout pane :focus pane))
         (session (%make-session :name name :rows rows :cols cols
                                 :socket (server-path server)
                                 :bar-p +bar-by-default+
                                 :scrollbars-p +scrollbars-by-default+
                                 :windows (list window) :window window :server server
                                 :screen (tty:make-screen :width cols
                                                          :height rows))))
    (session-compose session)
    (pane-start pane :environment (pane-environment session pane))
    (setf (server-sessions server) (append (server-sessions server) (list session))
          (server-had-sessions server) t)
    (run-hook 'pane-started session pane)
    (run-hook 'session-made session)
    session))

(defun session-named (server name)
  "The session called NAME, or the first one when no name is asked for."
  (if (and name (plusp (length (princ-to-string name))))
      (find (princ-to-string name) (server-sessions server)
            :key #'session-name :test #'string=)
      (first (server-sessions server))))

(defun unused-session-name (server)
  (loop :for n :from 0
        :for name := (princ-to-string n)
        :unless (session-named server name) :do (return name)))

(defun session-panes (session)
  "Every pane in SESSION, window by window: the ones on screen and the ones in
the other windows alike."
  (loop :for w :in (session-windows session) :append (window-panes w)))

;;; Windows: making one, showing one, closing one, naming one.

;;; Scrolling, and the mouse in a pane. A pane is read back by rows; what asks
;;; for that is a key, a wheel, or the scrollbar down the pane's right side. The
;;; wheel and the buttons are the program's when it asked for them, the way they
;;; would be with no multiplexer in between, and the multiplexer's otherwise.

;;; how big the pane is: the smallest any watcher can show, so nobody is shown a
;;; screen with a piece missing.

;;; Following: a terminal that follows another shows whatever session that
;;; one shows, as it moves, until it types something of its own. A session
;;; shows one window for everybody on it, so following is being kept on the
;;; same session.

;;; Clients: the terminals attached to this server. Each is a watcher that is
;;; here, on a session, looking at the window it shows.
