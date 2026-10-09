;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defvar *interval* 8
  "The least milliseconds between two frames to one client.

A least gap, never a tick: a client that has heard nothing for a while is
answered the moment a byte arrives, and only a client already being fed faster
than this waits. A tick would add half its length to every keystroke, which is
more than the terminal it is sitting inside costs in the first place.")

(defparameter +max-pane-size+ 1000)

(defparameter +max-faults+ 10)

(defvar *watcher-count* 0)

(declaim (ftype (function () integer) now-ms))
(defun now-ms () (floor (monotonic-ns) 1000000))

(defun actor-of (watcher &optional caller-pane)
  "Who is doing what WATCHER asked: the pane CALLER names when an agent verb was
run inside one, the client when it is somebody attached, and otherwise the
command line."
  (cond ((and (stringp caller-pane) (plusp (length caller-pane))) (list :pane caller-pane))
        ((watcher-interactive watcher) (list :client (watcher-id watcher) (watcher-tty watcher)))
        (t (list :cli))))

(defvar *server* nil
  "The server a message is being handled by.")

(defvar *caller* nil
  "The pane a command run from a command line was run in, as ATTY_PANE says.")

(defvar *caller-directory* nil
  "Where the command line a command was run from was.")

(defstruct (look (:copier nil))
  (back 0 :type fixnum)
  (pushed 0 :type fixnum)
  (find nil)
  (selecting nil)
  (shot nil))

(defstruct (view (:copier nil))
  (window -1 :type fixnum)
  (at 0 :type fixnum)
  (spots nil :type list)
  (bar-p +bar-by-default+)
  (rail-p +rail-by-default+)
  (scrollbars-p +scrollbars-by-default+)
  (field-kind 0 :type fixnum)
  (rows 24 :type fixnum)
  (cols 80 :type fixnum)
  (looks nil :type list))

(defun view-like (view &key (rows (view-rows view)) (cols (view-cols view)))
  (make-view :window (view-window view) :at (view-at view) :spots (view-spots view)
             :bar-p (view-bar-p view) :rail-p (view-rail-p view)
             :scrollbars-p (view-scrollbars-p view) :field-kind (view-field-kind view)
             :rows rows :cols cols))

(defstruct (watcher (:constructor %make-watcher))
  (wire nil)
  (socket nil)
  (session nil)
  (wanted-session nil)
  (view (make-view))
  (fits nil :type list)
  (takes t)
  (shadow nil)
  (told nil)
  (behind t :type boolean)
  (seen -1 :type fixnum)
  (sent 0 :type fixnum)
  (interactive nil :type boolean)
  (id (next-number '*watcher-count*) :type fixnum)
  (tty nil)
  (since 0 :type integer)
  (typed-at 0 :type integer)
  (keyed-at 0 :type integer)
  (answered 0 :type integer)
  (following nil)                       ; the id of the watcher this one goes where
  (bracketed-sent nil)
  (mode 'pane-mode)
  (partial-chord nil :type list)
  (pending-since nil)
  (menu nil)
  (menu-full nil)
  (field nil)
  (partial "" :type string)
  (overlays nil :type list)
  (screen nil)
  (found nil)
  (find-text nil)
  (session-screen nil)
  (geometry nil)
  (composed-at 0 :type integer)
  (clocked 0 :type integer)
  (held nil)
  (drawn nil)
  (rung 0 :type fixnum)
  (thread nil)
  (inbox nil)
  (wake nil)
  (reading nil)
  (keys-read nil))

(defun watcher-rows (watcher) (view-rows (watcher-view watcher)))
(defun watcher-cols (watcher) (view-cols (watcher-view watcher)))

;;; A window is a layout of panes, and a session holds its windows in order.
;;; Which of them a terminal shows, and which pane in it has the terminal's
;;; keys, is the terminal's own: its view.

(defvar *windows-made* (list 0))

(defstruct (window (:constructor %make-window))
  (id (sb-ext:atomic-incf (car *windows-made*)) :type fixnum)
  (label nil)
  (layout nil))

(defstruct (session-state (:conc-name state-) (:copier copy-state))
  (name "0")
  (windows nil)
  (watchers nil)
  (seeds nil)
  (version 0 :type fixnum))

(defstruct (session (:constructor %new-session))
  (now (make-session-state))
  (socket nil)
  (server nil))

(defun %make-session (&key (name "0") windows watchers seeds socket server)
  (%new-session :now (make-session-state :name name :windows windows
                                         :watchers watchers :seeds seeds)
                :socket socket :server server))

(declaim (ftype (function (session function) session-state) change-session))
(defun change-session (session change)
  (loop
    (let* ((was (session-now session))
           (now (copy-state was)))
      (funcall change now)
      (setf (state-version now) (1+ (state-version was)))
      (when (eq was (sb-ext:compare-and-swap (session-now session) was now))
        (return now)))))

(defvar *drawing* nil)

(defun session-seen (session)
  (let ((drawing *drawing*))
    (if (and drawing (eq (car drawing) session))
        (cdr drawing)
        (session-now session))))

(defmacro drawing-session ((session) &body body)
  (let ((it (gensym "SESSION")))
    `(let* ((,it ,session)
            (*drawing* (cons ,it (session-now ,it))))
       ,@body)))

(defun session-version (session) (state-version (session-seen session)))

(macrolet ((kept (name slot)
             `(progn
                (defun ,name (session) (,slot (session-seen session)))
                (defun (setf ,name) (new session)
                  (change-session session (lambda (s) (setf (,slot s) new)))
                  new))))
  (kept session-name state-name)
  (kept session-windows state-windows)
  (kept session-watchers state-watchers)
  (kept session-seeds state-seeds))

(defun same-window-p (a b)
  (and a b (= (window-id a) (window-id b))))

(defun window-now (session window)
  (and window (find (window-id window) (session-windows session) :key #'window-id)))

(declaim (ftype (function (session window function) session-state) change-window))
(defun change-window (session window change)
  (let ((id (window-id window)))
    (change-session session
                    (lambda (s)
                      (setf (state-windows s)
                            (mapcar (lambda (w)
                                      (if (= id (window-id w))
                                          (let ((new (copy-window w)))
                                            (funcall change new)
                                            new)
                                          w))
                                    (state-windows s)))))))

(defun view-shown-window (session view)
  (let ((windows (session-windows session)))
    (and windows
         (or (find (view-window view) windows :key #'window-id)
             (nth (min (view-at view) (1- (length windows))) windows)))))

(defun view-spot (view window)
  (cdr (assoc (window-id window) (view-spots view))))

(defun view-focus-in (session view &optional (window (view-shown-window session view)))
  (when window
    (let ((panes (window-panes window))
          (spot (view-spot view window)))
      (or (and spot (find (car spot) panes)) (first panes)))))

(defun view-zoomed-in (session view &optional (window (view-shown-window session view)))
  (when window
    (let ((spot (view-spot view window)))
      (and spot (cdr spot) (find (cdr spot) (window-panes window))))))

(defun view-place (session view window focus zoomed)
  (setf (view-window view) (window-id window)
        (view-at view) (or (position (window-id window) (session-windows session) :key #'window-id) 0)
        (view-spots view) (acons (window-id window) (cons focus zoomed)
                                 (remove (window-id window) (view-spots view) :key #'car))))

(defun watcher-window (watcher)
  (let ((session (watcher-session watcher)))
    (and session (view-shown-window session (watcher-view watcher)))))

(defun watcher-focus (watcher)
  (let ((session (watcher-session watcher)))
    (and session (view-focus-in session (watcher-view watcher)))))

(defun watcher-zoomed (watcher)
  (let ((session (watcher-session watcher)))
    (and session (view-zoomed-in session (watcher-view watcher)))))

(defun watcher-layout (watcher)
  (let ((window (watcher-window watcher)))
    (and window (window-layout window))))

(defun (setf watcher-window) (window watcher)
  (let ((session (watcher-session watcher))
        (view (watcher-view watcher)))
    (view-place session view window (view-focus-in session view window)
                (view-zoomed-in session view window))
    (view-changed watcher)
    window))

(defun (setf watcher-focus) (pane watcher)
  (let* ((session (watcher-session watcher))
         (view (watcher-view watcher))
         (window (window-of session pane)))
    (when window
      (let ((zoomed (view-zoomed-in session view window)))
        (view-place session view window pane (and (eq zoomed pane) pane)))
      (view-changed watcher))
    pane))

(defun (setf watcher-zoomed) (pane watcher)
  (let* ((session (watcher-session watcher))
         (view (watcher-view watcher))
         (window (view-shown-window session view)))
    (when window
      (view-place session view window (view-focus-in session view window) pane)
      (view-changed watcher))
    pane))

(macrolet ((toggle (name slot)
             `(progn
                (defun ,name (watcher) (,slot (watcher-view watcher)))
                (defun (setf ,name) (new watcher)
                  (setf (,slot (watcher-view watcher)) new)
                  (view-changed watcher)
                  new))))
  (toggle watcher-bar-p view-bar-p)
  (toggle watcher-rail-p view-rail-p)
  (toggle watcher-scrollbars-p view-scrollbars-p)
  (toggle watcher-field-kind view-field-kind))

(defun view-changed (watcher)
  (setf (watcher-behind watcher) t)
  (let ((server (watcher-server watcher)))
    (when server
      (let ((now (view-like (watcher-view watcher)))
            (session (watcher-session watcher)))
        (dolist (w (followers-of server watcher))
          (on-watcher w (let ((w w))
                          (lambda ()
                            (unless (eq (watcher-session w) session)
                              (join-session server w session))
                            (adopt-view w now)))))))))

(defun adopt-view (watcher seed)
  (let ((view (watcher-view watcher)))
    (setf (watcher-view watcher) (view-like seed :rows (view-rows view) :cols (view-cols view))
          (watcher-behind watcher) t)))

(defun view-look (view pane)
  (or (cdr (assoc pane (view-looks view)))
      (let ((look (make-look)))
        (push (cons pane look) (view-looks view))
        look)))

(declaim (ftype (function (t pane) fixnum) view-back))
(defun view-back (view pane)
  (let ((look (and view (cdr (assoc pane (view-looks view))))))
    (if (or (null look) (zerop (look-back look)))
        0
        (let* ((pushed (pane-pushed pane))
               (back (max 0 (min (pane-history pane)
                                 (+ (look-back look) (max 0 (- pushed (look-pushed look))))))))
          (setf (look-back look) back
                (look-pushed look) pushed)
          back))))

(defun watcher-look (watcher pane) (view-look (watcher-view watcher) pane))

(defun watcher-back (watcher pane) (view-back (and watcher (watcher-view watcher)) pane))

(defun scroll-to (watcher pane back)
  (let ((look (watcher-look watcher pane))
        (was (watcher-back watcher pane))
        (back (max 0 (min (pane-history pane) back))))
    (unless (= back was)
      (setf (look-back look) back
            (look-pushed look) (pane-pushed pane))
      (view-changed watcher)
      t)))

(defun scroll-back-by (watcher pane rows)
  (scroll-to watcher pane (+ (watcher-back watcher pane) rows)))

(defun top-row (watcher pane)
  (- (pane-history pane) (watcher-back watcher pane)))

(defun window-panes (window) (layout-panes (window-layout window)))

(defun window-number (session window)
  "Which window WINDOW is in SESSION, counting from 1, as the bar and an address say it."
  (let ((at (position (window-id window) (session-windows session) :key #'window-id)))
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
  "How often a terminal with something open over its session is drawn again,
since what is open may show how long ago things were.")

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
  (stirred nil :type boolean)
  (tending nil :type boolean)
  (wake nil)
  (interval 8 :type fixnum)
  (changed-panes nil)
  (thread nil)
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
                   (sb-ext:atomic-push (list (release-message tag) :accent) (server-notes server))))
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
  (sb-ext:atomic-push (cons (+ (monotonic-ns) (* milliseconds 1000000)) thunk) (server-tasks server))
  (server-poke server))

(defun next-task-delay (server now)
  (loop :for (when . nil) :in (server-tasks server)
        :minimize (max 0 (ceiling (- when now) 1000000))))

(defun run-due-tasks (server now)
  (let ((due (loop :for had := (server-tasks server)
                   :for kept := (remove-if (lambda (it) (<= (car it) now)) had)
                   :until (eq had (sb-ext:compare-and-swap (server-tasks server) had kept))
                   :finally (return (remove-if-not (lambda (it) (<= (car it) now)) had)))))
    (dolist (it (reverse due)) (funcall (cdr it)))))

(defun server-here-p (server)
  (let ((owner (server-thread server)))
    (or (null owner)
        (eq owner sb-thread:*current-thread*)
        (not (sb-thread:thread-alive-p owner)))))

(defun on-server (server job)
  (if (or (null server) (server-here-p server))
      (funcall job)
      (let ((done (sb-thread:make-semaphore))
            (claim (list nil))
            (said nil)
            (broke nil))
        (flet ((run ()
                 (when (null (sb-ext:compare-and-swap (car claim) nil t))
                   (handler-case (setf said (multiple-value-list (funcall job)))
                     (serious-condition (c) (setf broke c)))
                   t)))
          (schedule-task server 0 (lambda () (run) (sb-thread:signal-semaphore done)))
          (loop :until (sb-thread:wait-on-semaphore done :timeout 1)
                :when (and (server-here-p server) (run)) :return nil))
        (when broke (error broke))
        (values-list said))))

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
         (fd (pty:close-on-exec (sb-bsd-sockets:socket-file-descriptor socket))))
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
                              :waiting (tty:make-waiting 16)
                              :wake (open-wake-pipe))))
    (when listening (server-listen server))
    server))

(defvar *handing-over* nil
  "Whether a server asked to restart keeps its panes running for the build it
becomes, rather than letting them go.")

(defvar *handoff* nil
  "The panes a server keeps running for the build it becomes, as (id fd pid).")

(defun server-close (server)
  (setf (server-thread server) nil)
  ;; the name goes first. Whatever else takes a while, a shell that will not go
  ;; or a client that will not read, nobody new must be able to reach a server
  ;; that is already leaving.
  (ignore-errors (sb-bsd-sockets:socket-close (server-socket server)))
  (ignore-errors (delete-file (server-path server)))
  ;; then what it holds goes to disk, before the programs are let go: once the
  ;; name is gone, the state is whole
  (when (server-saving server)
    (handler-case (save-all server)
      (error (e) (report-error e)))
    (when (eql (server-pid-saved (file-namestring (server-path server))) (sb-posix:getpid))
      (ignore-errors (delete-file (pid-file (server-state-dir server))))))
  ;; what was already said is still sent: the last thing a server does is often
  ;; answer whoever asked it to stop, and a reply thrown away with the socket
  ;; reads to them as a server that never heard
  (dolist (w (append (server-pending-watchers server)
                     (loop :for s :in (server-sessions server)
                           :append (session-watchers s))))
    (watcher-stop w))
  (dolist (session (server-sessions server))
    (dolist (pane (session-panes session))
      (if (and *handing-over* (server-successor server)
               (pane-started pane) (pane-running pane))
          (progn (pane-stop pane)
                 (push (list (pane-id pane) (pane-fd pane) (pane-pid pane)) *handoff*))
          (pane-close pane))))
  (setf (server-sessions server) nil
        (server-pending-watchers server) nil)
  (tty:free-waiting (server-waiting server))
  (sb-thread:with-mutex (*wake-lock*)
    (close-wake-pipe (shiftf (server-wake server) nil)))
  (setf (server-running server) nil))

(defun server-poke (server)
  (sb-thread:with-mutex (*wake-lock*)
    (poke-wake-pipe (server-wake server))))

(defun session-woken (session pane)
  (let ((server (session-server session)))
    (lambda (&optional ended)
      (cond ((null server) (tty:wake))
            (ended (server-poke server)))
      (if server
          (dolist (s (server-sessions server))
            (dolist (w (session-watchers s))
              (when (watcher-sees-p w pane) (watcher-poke w))))
          (dolist (w (session-watchers session))
            (watcher-poke w))))))

(defun pane-environment (session pane)
  "What a program is told about where it is: its address as it starts. A pane
moved to another window keeps the address it was born with; the server answers
either."
  (list (format nil "ATTY_PANE=~A" (pane-address-of session pane))
        (format nil "ATTY_SOCKET=~A" (or (session-socket session) ""))))

(defun watcher-sees-p (watcher pane)
  (and (watcher-interactive watcher)
       (or (assoc pane (watcher-fits watcher))
           (some #'overlay-shows-panes-p (watcher-overlays watcher)))))

(defun pane-seen-p (server session pane)
  (if server
      (loop :for s :in (server-sessions server)
            :thereis (some (lambda (w) (watcher-sees-p w pane)) (session-watchers s)))
      (and (session-watchers session) t)))

(defun session-start-pane (session pane)
  (let ((server (session-server session)))
    (pane-start pane :environment (pane-environment session pane)
                     :woken (session-woken session pane)
                     :look (session-look session)
                     :watched (lambda () (pane-seen-p server session pane))
                     :urgent (lambda () (pane-urgent-p session pane (now-ms)))
                     :gap (if server (* (server-interval server) 1000000) 0))))

(defun add-session (server command &key (name "0") (rows 24) (cols 80) directory)
  (on-server server
             (lambda ()
               (or (session-named server name)
                   (let* ((pane (make-pane command :directory directory))
                          (session (%make-session :name name :socket (server-path server)
                                                  :windows (list (%make-window :layout pane))
                                                  :server server)))
                     (multiple-value-bind (high wide) (laid-size session (make-view :rows rows :cols cols) pane)
                       (pane-resize pane high wide))
                     (session-start-pane session pane)
                     (when (pane-failed pane)
                       (error "~A" (pane-failed pane)))
                     (sb-ext:atomic-update (server-sessions server) (lambda (all) (append all (list session))))
                     (setf (server-had-sessions server) t)
                     (run-hook 'pane-started session pane)
                     (run-hook 'session-made session)
                     session)))))

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

;;; Clients: the terminals attached to this server. Each is a watcher that is
;;; here, on a session, looking at the window it shows.
