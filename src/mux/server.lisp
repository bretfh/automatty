;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defvar *interval* 8
  "The least milliseconds between two frames to one client.

A least gap, never a tick: a client that has heard nothing for a while is
answered the moment a byte arrives, and only a client already being fed faster
than this waits. A tick would add half its length to every keystroke, which is
more than the terminal it is sitting inside costs in the first place.")

(declaim (ftype function session-bar session-compose greet tell load-user-init init-loaded-note))
(declaim (special +prompt-toggles+))

(defparameter +biggest-pane+ 1000)

(defparameter +understood+
  '(:want :attach :open :who :go :new :sessions :kill-session :knock :detach :stop
    :name-pane :naming
    :panes :watch-panes :watch-screens :pane-screen :pane-history :pane-log
    :answer :focus-pane :go-to-blocked :zoom :pane-read :lately :prompt-when-idle
    :close-pane :split-in :pane-about :spawn :since-prompt
    :new-window :go-window :next-window :previous-window :close-window :name-window :layouts
    :clients :detach-client :reading :naming-window
    :keys :resize :bar :split :focus :close :only :mouse-at
    :scroll :wheel :pointer :scrollbars :reload-init
    :agents :agent-signal :agent-read :agent-keys :agent-prompt :agent-explain :agent-snapshot
    :agent-act :agent-observe :readers-load
    :agent-trace)
  "Every message this server knows what to do with.

It goes out with the greeting. A server outlives the builds that reach it: it
has been running since whenever, and the client is the one that was started a
moment ago. So a client has to be able to say which of its keys this server
cannot do, rather than sending one and leaving a key that looks broken.")

(defvar *watchers-made* 0)

(defun now-ms () (floor (nanos) 1000000))

(defun who-of (watcher &optional caller)
  "Who is doing what WATCHER asked: the pane CALLER names when an agent verb was
run inside one, the client when it is somebody attached, and otherwise the
command line."
  (cond ((and (stringp caller) (plusp (length caller))) (list :pane caller))
        ((watcher-here watcher) (list :client (watcher-id watcher) (watcher-tty watcher)))
        (t (list :cli))))

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
  (here nil :type boolean)
  (id (incf *watchers-made*) :type fixnum)
  (tty nil)
  (since 0 :type integer)
  (typed-at 0 :type integer)
  (watch-panes nil)
  (panes-told (make-hash-table :test 'equal))
  (screen-rows nil)
  (screens-told (make-hash-table :test 'equal)))

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
  (barp t)
  (search-kind 0 :type fixnum)
  (watchers nil)
  (clocked 0 :type integer)
  (rows 24 :type fixnum)
  (cols 80 :type fixnum)
  (scrollbarsp t)
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

(defun window-panes (window) (panes-in (window-layout window)))

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
  (later nil)
  (born 0 :type integer)
  (had-sessions nil :type boolean)
  (notes nil)
  (going t :type boolean))

(defparameter +first-session-patience+ 10000000000
  "How long a server started with no session waits for somebody to ask for
one, in nanoseconds. A server nobody reaches in that time was started by a
client that has since gone, and holding the socket helps nobody.")

(defun server-wanted-p (server now)
  "Whether the server has a reason to go on: a session, or no session yet and
a client that may still be on its way."
  (and (server-going server)
       (or (server-sessions server)
           (and (not (server-had-sessions server))
                (< (- now (server-born server)) +first-session-patience+)))))

(defparameter +enter-after+ 300)

(defun later (server milliseconds thunk)
  (push (cons (+ (nanos) (* milliseconds 1000000)) thunk) (server-later server)))

(defun soon (server now)
  (loop :for (when . nil) :in (server-later server)
        :minimize (max 0 (ceiling (- when now) 1000000))))

(defun run-what-is-due (server now)
  (let ((due (remove-if-not (lambda (it) (<= (car it) now)) (server-later server))))
    (setf (server-later server) (set-difference (server-later server) due))
    (dolist (it (reverse due)) (funcall (cdr it)))))

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
      (%make-server :path path :socket socket :fd fd :born (nanos)
                    :waiting (tty:make-waiting 16)))))

(defun server-close (server)
  ;; the name goes first. Whatever else takes a while, a shell that will not go
  ;; or a client that will not read, nobody new must be able to reach a server
  ;; that is already leaving.
  (ignore-errors (sb-bsd-sockets:socket-close (server-socket server)))
  (ignore-errors (delete-file (server-path server)))
  ;; what was already said is still sent: the last thing a server does is often
  ;; answer whoever asked it to stop, and a reply thrown away with the socket
  ;; reads to them as a server that never heard
  (dolist (w (append (server-knocking server)
                     (loop :for s :in (server-sessions server)
                           :append (session-watchers s))))
    (ignore-errors (wire-flush (watcher-wire w))))
  (dolist (w (server-knocking server)) (wire-close (watcher-wire w)))
  (dolist (session (server-sessions server))
    (dolist (w (session-watchers session))
      (wire-close (watcher-wire w)))
    (mapc #'pane-close (session-panes session)))
  (setf (server-sessions server) nil
        (server-knocking server) nil)
  (tty:free-waiting (server-waiting server))
  (setf (server-going server) nil))

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
                                 :barp +bar-by-default+
                                 :scrollbarsp +scrollbars-by-default+
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

(defun a-free-name (server)
  (loop :for n :from 0
        :for name := (princ-to-string n)
        :unless (session-named server name) :do (return name)))

(defun session-panes (session)
  "Every pane in SESSION, window by window: the ones on screen and the ones in
the other windows alike."
  (loop :for w :in (session-windows session) :append (window-panes w)))

;;; Windows: making one, showing one, closing one, naming one.

(defun show-window (session window)
  "WINDOW is the one SESSION shows. Everybody watching is redrawn, and the
panes are fitted to the room when it is next composed."
  (unless (eq window (session-window session))
    (setf (session-window session) window)
    (dolist (pane (window-panes window)) (setf (pane-dirty pane) t))
    (dolist (w (session-watchers session)) (setf (watcher-behind w) t))))

(defun add-window (session &optional command directory)
  "Another window in SESSION, after the current one, with one pane running
COMMAND, or what the focus runs, where the focus is. It is the one shown."
  (let* ((focus (session-focus session))
         (term (and focus (pane-term focus)))
         (pane (make-pane (or command (and focus (pane-command focus))
                              (server-command (session-server session)))
                          :rows (if term (term:term-height term) (session-rows session))
                          :cols (if term (term:term-width term) (session-cols session))
                          :directory (or directory (and focus (pane-directory focus)))))
         (window (%make-window :layout pane :focus pane))
         (at (position (session-window session) (session-windows session))))
    (setf (session-windows session)
          (append (subseq (session-windows session) 0 (1+ (or at -1)))
                  (list window)
                  (subseq (session-windows session) (1+ (or at -1)))))
    (show-window session window)
    (session-compose session)
    (pane-start pane :environment (pane-environment session pane))
    (run-hook 'pane-started session pane)
    window))

(defun window-called (session n)
  "Window N of SESSION, counting from 1."
  (and (integerp n) (nth (1- n) (session-windows session))))

(defun go-to-window (session n)
  (let ((window (window-called session n)))
    (when window (show-window session window))
    window))

(defun step-window (session by)
  "The window BY after the current one, round the end."
  (let* ((windows (session-windows session))
         (at (position (session-window session) windows)))
    (when (rest windows)
      (show-window session (nth (mod (+ at by) (length windows)) windows)))
    (session-window session)))

(defun drop-window (session window)
  "WINDOW is gone from SESSION; if it was shown, the one before it is, or the
one after. The last window stays, empty, which is a session that is over."
  (let* ((windows (session-windows session))
         (at (position window windows))
         (left (remove window windows)))
    (when (and at left)
      (setf (session-windows session) left)
      (when (eq window (session-window session))
        (show-window session (nth (min at (1- (length left))) left))))
    (dolist (w (session-watchers session)) (setf (watcher-behind w) t))
    left))

(defun close-a-window (session window)
  "Let every program in WINDOW go, and take the window out."
  (dolist (pane (window-panes window))
    (close-the-pane session pane)))

(defun name-a-window (session window label)
  (setf (window-label window) (and label (plusp (length label)) label))
  (dolist (w (session-watchers session)) (setf (watcher-behind w) t)))

(defun window-says (session window)
  "What to call WINDOW: its name, else its number."
  (or (window-label window) (princ-to-string (window-number session window))))

(defun layout-said (it)
  "A layout as it goes out: a pane's id, or a split as its way and its parts."
  (cond ((null it) nil)
        ((split-p it) (cons (split-way it) (mapcar #'layout-said (split-parts it))))
        (t (pane-id it))))

(defun windows-said (session)
  "SESSION's windows as a client is told them: number, name, how many panes, how
many of them are asking, and whether it is the one shown."
  (loop :for w :in (session-windows session)
        :for n :from 1
        :collect (list n (window-label w) (length (window-panes w))
                       (count :blocked (window-panes w)
                              :key (lambda (p) (agent:agent-state (pane-agent p))))
                       (eq w (session-window session)))))

(defun split-the-session (session way)
  "Another pane beside the one that has the cursor, running what that one runs."
  (let* ((focus (session-focus session))
         (term (pane-term focus))
         (new (make-pane (pane-command focus)
                         :rows (term:term-height term)
                         :cols (term:term-width term)
                         :directory (pane-directory focus))))
    (setf (session-layout session)
          (put-beside (session-layout session) focus way new)
          (session-focus session) new)
    (session-compose session)
    (pane-start new :environment (pane-environment session new))
    (dolist (w (session-watchers session)) (setf (watcher-behind w) t))
    (run-hook 'pane-started session new)
    new))

(defparameter +act-patience+ 5000)
(defparameter +act-still+ 400)

(defun act-on-pane (server watcher name id action argument &optional until)
  (let* ((now (floor (nanos) 1000000))
         (until (or until (+ now +act-patience+)))
         (pane (pane-called server name id))
         (still (and pane (agent:agent-still-since (pane-agent pane)))))
    (if (and still (not (eq action :interrupt)) (< (- now still) +act-still+) (< now until))
        (later server 100 (lambda () (act-on-pane server watcher name id action argument until)))
        (act-now server watcher name id action argument))))

(defun act-now (server watcher name id action argument)
  (let* ((pane (pane-called server name id))
         (agent (and pane (pane-agent pane)))
         (reader (and agent (agent:agent-reader agent)))
         (seen (and reader (agent:observe reader (pane-term pane))))
         (keys (and seen (agent:action-keys reader seen action argument))))
    (cond
      ((null pane) (tell watcher (list :agent-acted name id action :gone)))
      ((null reader) (tell watcher (list :agent-acted name id action :no-reader)))
      ((null keys)
       (tell watcher (list :agent-acted name id action :not-offered
                           (and seen (agent:offered reader seen)) (getf seen :screen))))
      (t
       (when (eq action :submit)
         (agent:agent-prompted agent (floor (nanos) 1000000)))
       (loop :for chunk :in keys
             :for at :from 0 :by +enter-after+
             :do (let ((chunk chunk))
                   (if (zerop at)
                       (pane-say pane chunk)
                       (later server at (lambda () (pane-say pane chunk))))))
       (let ((deadline (+ (floor (nanos) 1000000) +act-patience+
                          (* +enter-after+ (length keys)))))
         (labels ((check ()
                    (let ((now-seen (agent:observe reader (pane-term pane))))
                      (cond ((not (equal now-seen seen))
                             (tell watcher (list :agent-acted name id action :done
                                                 (getf now-seen :screen))))
                            ((> (floor (nanos) 1000000) deadline)
                             (tell watcher (list :agent-acted name id action :failed
                                                 (getf seen :screen))))
                            (t (later server 100 #'check))))))
           (later server (+ 100 (* +enter-after+ (1- (length keys)))) #'check)))))))

(defun agent-row (session pane)
  (let ((agent (pane-agent pane)))
    (list (session-name session) (pane-id pane)
          (pane-kind pane) (agent:agent-state agent)
          (agent:agent-reason agent) (agent:agent-turn agent))))

(defun agent-rows (server &optional session)
  (loop :for s :in (if session (list session) (server-sessions server))
        :append (mapcar (lambda (p) (agent-row s p)) (session-panes s))))

(defun pane-called (server name id)
  (let ((session (session-named server name)))
    (and session (find id (session-panes session) :key #'pane-id))))

(defun close-the-pane (session pane)
  "Take PANE out of its window and let its program go. A window left with
nothing in it goes too, unless it is the last: an empty last window is a
session that is over."
  (let ((window (window-of session pane)))
    (unless window (return-from close-the-pane (session-panes session)))
    (setf (window-layout window) (without-pane (window-layout window) pane))
    (when (eq pane (window-zoomed window))
      (setf (window-zoomed window) nil))
    (when (eq pane (second (session-held session)))
      (setf (session-held session) nil))
    (pane-close pane)
    (run-hook 'pane-ended session pane)
    (let ((left (window-panes window)))
      (when (eq (window-focus window) pane)
        (setf (window-focus window) (first left)))
      (when (and (null left) (rest (session-windows session)))
        (drop-window session window))
      (dolist (w (session-watchers session)) (setf (watcher-behind w) t))
      (session-panes session))))

(defun only-the-pane (session pane)
  "Every other pane in PANE's window goes, and PANE has the whole of it."
  (let ((window (window-of session pane)))
    (dolist (other (remove pane (window-panes window)))
      (close-the-pane session other))
    (window-panes window)))

(defun mouse-at (session watcher x y)
  "Whatever was at (X, Y) the last time this session was drawn is told about
the click: the toggle on the search bar cycles which prompt it opens, another
bar button tells WATCHER, the one who clicked, to run what it is for, and a
pane becomes the focus. A geometry from before the first draw, or a click that
landed on a rule, does nothing."
  (let ((hit (and (session-geometry session)
                  (atty/ui:under (session-geometry session) y x))))
    (cond
      ((and (typep hit 'bar-button) (eq (bar-button-runs hit) :cycle-search-kind))
       (setf (session-search-kind session)
             (mod (1+ (session-search-kind session)) (length +prompt-toggles+)))
       (dolist (w (session-watchers session)) (setf (watcher-behind w) t)))
      ((typep hit 'bar-button)
       (let ((runs (bar-button-runs hit)))
         ;; a form is the server's to do, as though the one who clicked had
         ;; said it; a name is a command for them to run
         (if (consp runs)
             (heard (session-server session) watcher runs)
             (tell watcher (list :do runs)))))
      ((and (typep hit 'pane-view) (not (eq (view-pane hit) (session-focus session))))
       (focus-on session (view-pane hit))))))

;;; Scrolling, and the mouse in a pane. A pane is read back by rows; what asks
;;; for that is a key, a wheel, or the scrollbar down the pane's right side. The
;;; wheel and the buttons are the program's when it asked for them, the way they
;;; would be with no multiplexer in between, and the multiplexer's otherwise.

(defun thing-at (session x y)
  (and x y (session-geometry session)
       (atty/ui:under (session-geometry session) y x)))

(defun pane-at (session x y)
  "The pane whose view, scrollbar or chip is at (X, Y), and which of them it is."
  (let ((hit (thing-at session x y)))
    (when (typep hit '(or pane-view scrollbar live-chip))
      (values (view-pane hit) hit))))

(defun bar-of (session pane)
  "PANE's scrollbar as the session was last laid out."
  (labels ((walk (w)
             (if (and (typep w 'scrollbar) (eq pane (view-pane w)))
                 w
                 (some #'walk (atty/ui:parts w)))))
    (and (session-geometry session) (walk (session-geometry session)))))

(defun scroll-the-pane (pane amount)
  "Read PANE back by AMOUNT: a number of rows, further back when it is more than
nought, or :page-up, :page-down, :half-up, :half-down, :top or :bottom."
  (when pane
    (let ((rows (term:term-height (pane-term pane))))
      (case amount
        (:top (pane-scroll-to pane (pane-history pane)))
        (:bottom (pane-scroll-to pane 0))
        (:page-up (pane-scroll-by pane (max 1 (1- rows))))
        (:page-down (pane-scroll-by pane (- (max 1 (1- rows)))))
        (:half-up (pane-scroll-by pane (max 1 (floor rows 2))))
        (:half-down (pane-scroll-by pane (- (max 1 (floor rows 2)))))
        (t (when (integerp amount) (pane-scroll-by pane amount)))))))

(defun tell-the-program (session pane kind x y &rest keys)
  "Say to PANE's program that a mouse did KIND at (X, Y) of the session, where
it is in the pane's own rows and columns. Answers whether it wanted to know."
  (let* ((view (and (session-geometry session)
                    (view-of (session-geometry session) pane)))
         (term (pane-term pane))
         (said (and view
                    (zerop (pane-scrolled pane))
                    (apply #'term:mouse-report term kind
                           (max 0 (min (1- (term:term-width term))
                                       (- x (atty/ui:left view))))
                           (max 0 (min (1- (term:term-height term))
                                       (- y (atty/ui:top view))))
                           keys))))
    (when said
      (pane-say pane said)
      t)))

(defun wheel-at (session way x y mods)
  "A notch of the wheel, WAY being :up or :down, over whatever is at (X, Y), or
over the pane with the focus when nobody said where.

A program that asked for the mouse is told. One that has the whole screen and
did not is sent the arrow keys, which is what it scrolls by. Anything else has
its pane read back. With shift held it is always the pane that is read back,
and so it is over the scrollbar, which is nobody's but the multiplexer's."
  (multiple-value-bind (pane hit) (pane-at session x y)
    (let* ((pane (or pane (session-focus session)))
           (term (and pane (pane-term pane)))
           (rows (if (eq way :up) +wheel-rows+ (- +wheel-rows+))))
      (when pane
        (cond
          ((or (member :shift mods) (typep hit 'scrollbar) (plusp (pane-scrolled pane)))
           (pane-scroll-by pane rows))
          ((and (typep hit 'pane-view)
                (tell-the-program session pane :wheel x y :wheel way
                                  :meta (and (member :meta mods) t)
                                  :ctrl (and (member :ctrl mods) t))))
          ((term:term-in-alt-screen term)
           (let ((key (term:key-event-to-escape-sequence
                       term (list (if (eq way :up) :up :down)))))
             (dotimes (i +wheel-rows+) (pane-say pane key))))
          (t (pane-scroll-by pane rows)))))))

(defun hold-the-scrollbar (session pane part line)
  "An arrow or the track of PANE's scrollbar is being held at LINE: do what one
click of it does, and go on doing it until it is let go. The track stops when
the thumb has come to where the pointer is."
  (let ((held (list :bar pane part line)))
    (setf (session-held session) held)
    (labels ((once ()
               (let* ((bar (bar-of session pane))
                      (now (and bar (scrollbar-part bar (fourth held)))))
                 (ecase part
                   (:up (pane-scroll-by pane 1))
                   (:down (pane-scroll-by pane -1))
                   (:above (when (eq now :above) (scroll-the-pane pane :page-up)))
                   (:below (when (eq now :below) (scroll-the-pane pane :page-down))))))
             (again ()
               (when (eq held (session-held session))
                 (once)
                 (when (session-server session)
                   (later (session-server session) +hold-every+ #'again)))))
      (once)
      (when (session-server session)
        (later (session-server session) +hold-after+ #'again)))))

(defun pointer-at (session watcher what button x y mods)
  "A button of the mouse went down, moved while down, or came up: WHAT is
:press, :drag or :release.

On the scrollbar it is the scrollbar's. On the chip it is back to live. Anywhere
else a press of the left button is what a click has always been, and whichever
button it was, a program that asked for the mouse is told, from the press to
the release, wherever the pointer went in between."
  (let ((held (session-held session)))
    (ecase what
      (:press
       (multiple-value-bind (pane hit) (pane-at session x y)
         (setf (session-held session) nil)
         (cond
           ((and (typep hit 'scrollbar) (eq button :left))
            (let ((part (scrollbar-part hit y)))
              (case part
                ((nil))
                (:thumb
                 (multiple-value-bind (from track) (scrollbar-track hit)
                   (let ((top (scrollbar-thumb track (term:term-height (pane-term pane))
                                               (pane-history pane) (pane-scrolled pane))))
                     (setf (session-held session)
                           (list :thumb pane (- y from top))))))
                (t (hold-the-scrollbar session pane part y)))))
           ((typep hit 'scrollbar))
           ((typep hit 'live-chip)
            (when (eq button :left) (pane-scroll-to pane 0)))
           (t
            (when (eq button :left) (mouse-at session watcher x y))
            (when (and (typep hit 'pane-view)
                       (tell-the-program session pane :press x y :button button
                                         :shift (and (member :shift mods) t)
                                         :meta (and (member :meta mods) t)
                                         :ctrl (and (member :ctrl mods) t)))
              (setf (session-held session) (list :pane pane button)))))))
      (:drag
       (case (first held)
         (:thumb
          (let* ((pane (second held))
                 (bar (bar-of session pane)))
            (when bar
              (pane-scroll-to pane (scrollbar-back-at bar y (third held))))))
         (:bar (setf (fourth held) y))
         (:pane
          (tell-the-program session (second held) :drag x y :button (third held)))))
      (:release
       (when (eq (first held) :pane)
         (tell-the-program session (second held) :release x y :button (third held)))
       (setf (session-held session) nil)))))

(defun focus-on (session pane)
  "PANE has the focus. A zoom was of the pane that had it, and goes with it, the
way it does in every multiplexer: the one just chosen is to be seen in its place."
  (let ((window (window-of session pane)))
    (when window (show-window session window)))
  (unless (eq pane (session-focus session))
    (setf (session-focus session) pane)
    (unless (eq pane (session-zoomed session))
      (setf (session-zoomed session) nil))
    (dolist (w (session-watchers session)) (setf (watcher-behind w) t))))

(defun focus-the-next (session)
  (let* ((panes (window-panes (session-window session)))
         (at (position (session-focus session) panes)))
    (when panes
      (focus-on session (nth (mod (1+ (or at -1)) (length panes)) panes)))
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
  (atty/ui:column
   :align :stretch
   (session-bar session)
   (layout-tree (session-layout session) (session-focus session)
                session (session-zoomed session))))

(defun fit-panes (tree)
  "Give each pane the room the layout gave its view."
  (dolist (v (views-in tree))
    (let ((pane (view-pane v))
          (rows (max 1 (atty/ui:height v)))
          (cols (max 1 (atty/ui:width v))))
      (unless (and (= rows (term:term-height (pane-term pane)))
                   (= cols (term:term-width (pane-term pane))))
        (pane-resize pane rows cols)))))

(defun put-the-cursor (session tree)
  "The cursor sits where the pane it belongs to says, moved to where that pane
was put."
  (let* ((screen (session-screen session))
         (v (view-of tree (session-focus session)))
         (term (session-pane-term session)))
    (setf (tty:screen-cursor-x screen)
          (min (+ (if v (atty/ui:left v) 0) (term:term-cursor-x term))
               (1- (tty:screen-width screen)))
          (tty:screen-cursor-y screen)
          (min (+ (if v (atty/ui:top v) 0) (term:term-cursor-y term))
               (1- (tty:screen-height screen)))
          ;; a pane being read back is not showing the line the cursor is on
          (tty:screen-cursor-visible screen)
          (and (zerop (pane-scrolled (session-focus session)))
               (term:term-cursor-visible term))
          (tty:screen-cursor-style screen) (term:term-cursor-style term))))

(defun session-compose (session)
  "Measure the session, lay it out, give each pane what it was given, and paint
it. One pass: the panes are resized between the laying and the painting, so what
is drawn is what they have just been told they are."
  (let* ((screen (session-screen session))
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (tree (let ((*scrollbars* (session-scrollbarsp session)))
                 (session-tree session)))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows)))
    (atty/ui:with-pass
      (atty/ui:restyle tree)
      (atty/ui:measure tree m cols rows)
      (atty/ui:lay tree m 0 0 cols rows)
      (setf (session-geometry session) tree)
      (fit-panes tree)
      (atty/ui:paint tree m))
    (put-the-cursor session tree)
    screen))

(declaim (ftype function tell))

(defun tell (watcher form)
  (wire-send (watcher-wire watcher) form))

(defun greet (watcher session)
  "Tell WATCHER what it is looking at, and what this server can be asked to do."
  (tell watcher (list :hello (session-name session)
                      (session-rows session) (session-cols session)
                      +understood+))
  ;; on its own rather than on the end of the greeting: a client from before
  ;; this takes the greeting apart by its exact shape, and passes over a
  ;; message it has never heard of
  (tell watcher (list :you (watcher-id watcher))))

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
            (session-readers session) (remove watcher (session-readers session))
            (watcher-session watcher) nil)
      (session-fit session))
    session))

(defun drop-watcher (server watcher &optional why)
  (when why
    (ignore-errors (tell watcher (list :bye why))
                   (wire-flush (watcher-wire watcher))))
  (wire-close (watcher-wire watcher))
  (setf (server-knocking server) (remove watcher (server-knocking server)))
  (leave-session watcher)
  (when (watcher-here watcher) (tell-the-clients server)))

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
  ;; what the server has had to say since anybody was here to hear it: the
  ;; first to arrive is told, and it is said once
  (when (server-notes server)
    (dolist (note (reverse (server-notes server)))
      (tell watcher (list* :say note)))
    (setf (server-notes server) nil))
  (tell-the-clients server)
  session)

(defun watcher-sized (watcher rows cols takes)
  (setf (watcher-rows watcher) (max 1 (min +biggest-pane+ rows))
        (watcher-cols watcher) (max 1 (min +biggest-pane+ cols))
        (watcher-takes watcher) takes
        (watcher-here watcher) t)
  (when (zerop (watcher-since watcher))
    (setf (watcher-since watcher) (now-ms))))

;;; Clients: the terminals attached to this server. Each is a watcher that is
;;; here, on a session, looking at the window it shows.

(defun client-said (server watcher now)
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
          (if (plusp (watcher-typed-at watcher)) (max 0 (- now (watcher-typed-at watcher))) nil))))

(defun clients-said (server now)
  (loop :for w :in (every-watcher server)
        :when (and (watcher-here w) (wire-open (watcher-wire w)))
          :collect (client-said server w now)))

(defun tell-the-clients (server)
  "Everybody keeping up with the panes is told who is attached now: the same
watchers that hear about panes, since what they draw from one they draw
from the other."
  (let ((rows (clients-said server (now-ms))))
    (dolist (w (every-watcher server))
      (when (and (watcher-watch-panes w) (wire-open (watcher-wire w)))
        (tell w (list :clients rows))))))

(defun heard (server watcher form)
  "What a client said. Which session it is on is the watcher's own; anything
that is about a session is passed on only once it has joined one."
  (let ((session (watcher-session watcher)))
    (case (first form)
      (:want (setf (watcher-want watcher) (second form)))
      (:attach
       (destructuring-bind (rows cols takes) (rest form)
         (watcher-sized watcher rows cols takes)
         (let ((want (session-named server (watcher-want watcher))))
           (if want
               (join-session server watcher want)
               (drop-watcher server watcher :no-such-session)))))
      (:open
       ;; join NAME, making it first when it is not there. One message, so the
       ;; asking and the making are one step here: two clients opening the same
       ;; new name get one session, not two, and not an error
       (destructuring-bind (name command directory rows cols takes &optional label) (rest form)
         (watcher-sized watcher rows cols takes)
         (join-session server watcher
                       (or (and name (session-named server name))
                           (let ((made (add-session server (or command (server-command server))
                                                    :name (or name (a-free-name server))
                                                    :rows (watcher-rows watcher)
                                                    :cols (watcher-cols watcher)
                                                    :directory directory)))
                             (when label
                               (setf (pane-label (session-focus made)) label))
                             made)))))
      (:spawn
       (destructuring-bind (name command directory label) (rest form)
         (let* ((pane (spawn-a-pane server name command directory label))
                (session (and pane (session-named server name))))
           (tell watcher (list :spawned name (and pane (pane-id pane))
                               (and pane (pane-address-of session pane)))))))
      (:since-prompt
       (destructuring-bind (name id) (rest form)
         (let ((pane (pane-called server name id))
               (now (now-ms)))
           (tell watcher
                 (list :since-prompt name id
                       (and pane
                            (let ((prompted (find :prompt (pane-log pane) :key #'third)))
                              (list (agent:agent-state (pane-agent pane))
                                    (and prompted (- now (first prompted)))
                                    (history-said (pane-agent pane) now)))))))))
      (:go
       (let ((want (session-named server (second form))))
         (when want (join-session server watcher want))))
      (:new
       (destructuring-bind (&optional name command directory) (rest form)
         (join-session server watcher
                       (add-session server (or command (server-command server))
                                    :name (or name (a-free-name server))
                                    :rows (watcher-rows watcher)
                                    :cols (watcher-cols watcher)
                                    :directory (or directory
                                                   (and session
                                                        (pane-directory
                                                         (session-focus session))))))))
      (:sessions
       (tell watcher
             (list :these
                   (mapcar (lambda (s)
                             (list (session-name s) (session-rows s)
                                   (session-cols s)
                                   (length (session-panes s))
                                   (count-if #'watcher-here (session-watchers s))
                                   (count :blocked (session-panes s)
                                          :key (lambda (p) (agent:agent-state
                                                            (pane-agent p))))
                                   (windows-said s)))
                           (server-sessions server)))))
      (:kill-session
       (let ((it (and (second form) (session-named server (second form)))))
         (tell watcher (list :killed (second form) (and it t)))
         (when it (end-the-session server it :stopped))))
      ;; :one-server is how a client tells this server from one made before a
      ;; server held every session, which held only the session it was named
      (:knock (tell watcher (list :here (server-path server) :one-server)))
      (:reload-init
       (load-user-init)
       (tell watcher (list* :say (init-loaded-note))))
      (:agents (tell watcher (list :agents (agent-rows server session))))
      (:who (setf (watcher-tty watcher) (and (stringp (second form)) (second form)))
            ;; a tty said after attaching is a change to what is listed; one
            ;; said before is told with the attaching
            (when (watcher-here watcher) (tell-the-clients server)))
      (:clients (tell watcher (list :clients (clients-said server (now-ms)))))
      (:detach-client
       (let ((it (find (second form) (every-watcher server) :key #'watcher-id)))
         (when (and it (watcher-here it))
           (drop-watcher server it :detached)
           (tell-the-clients server))))
      (:agent-signal
       (destructuring-bind (name id state &optional caller) (rest form)
         (let ((pane (pane-called server name id)))
           (when (and pane (member state '(:working :blocked :idle)))
             (pane-logged pane (now-ms) (who-of watcher caller) :signal state)
             (agent:agent-hear (pane-agent pane) state)
             (session-observe (session-named server name) (nanos))))))
      (:name-pane
       (destructuring-bind (name id label) (rest form)
         (let ((pane (pane-called server name id)))
           (when pane
             (setf (pane-label pane) (and (stringp label)
                                          (plusp (length (string-trim " " label)))
                                          (string-trim " " label)))
             (dolist (w (session-watchers (session-named server name)))
               (setf (watcher-behind w) t)))
           (tell watcher (list :named name id (and pane t))))))
      (:naming
       ;; the client does not know which pane has the focus, only the server
       ;; does; so a rename is asked for here and the prompt is the client's
       (let ((pane (and session (session-focus session))))
         (when pane
           (tell watcher (list :name-it (session-name session) (pane-id pane)
                               (pane-label pane) (pane-named pane)
                               (pane-address-of session pane))))))
      (:naming-window
       (when session
         (tell watcher (list :name-window-of (session-name session)
                             (window-number session (session-window session))
                             (window-label (session-window session))))))
      (:agent-read
       (destructuring-bind (name id n &optional plain) (rest form)
         (let ((pane (pane-called server name id)))
           (tell watcher (list :agent-lines name id
                               (and pane (if plain
                                             (plain-lines (pane-term pane) n)
                                             (agent:last-lines (pane-term pane) n))))))))
      (:agent-snapshot
       (destructuring-bind (name id) (rest form)
         (let ((pane (pane-called server name id)))
           (tell watcher (list :agent-snapshotted name id
                               (and pane (agent:snapshot (pane-term pane))))))))
      (:agent-keys
       (destructuring-bind (name id text &optional caller) (rest form)
         (let ((pane (pane-called server name id)))
           (when pane
             (pane-logged pane (now-ms) (who-of watcher caller) :say (summarised text))
             (pane-say pane text)))))
      (:agent-prompt
       (destructuring-bind (name id text &optional caller) (rest form)
         (let ((pane (pane-called server name id)))
           (let ((said (if pane
                             (prompt-a-pane server pane text (who-of watcher caller))
                             :gone)))
             (tell watcher (list :agent-prompted name id said
                                 (and (eq said t) (agent:agent-reader (pane-agent pane))
                                      (agent:agent-turn (pane-agent pane)))))))))
      (:prompt-when-idle
       (destructuring-bind (name id text &optional caller) (rest form)
         (let ((pane (pane-called server name id)))
           (tell watcher (list :agent-prompted name id
                               (if pane
                                   (prompt-when-idle server pane text (who-of watcher caller))
                                   :gone))))))
      (:agent-observe
       (destructuring-bind (name id) (rest form)
         (let* ((pane (pane-called server name id))
                (agent (and pane (pane-agent pane)))
                (reader (and agent (agent:agent-reader agent)))
                (seen (and reader (agent:observe reader (pane-term pane)))))
           (tell watcher (list :agent-observed name id
                               (and agent (list :kind (agent:agent-kind agent)
                                                :version (agent:agent-version agent)
                                                :verified (agent:agent-verified agent)
                                                :state (agent:agent-state agent)
                                                :offered (and seen (agent:offered reader seen))
                                                :observation seen)))))))
      (:agent-act
       (destructuring-bind (name id action &optional argument) (rest form)
         (act-on-pane server watcher name id action argument)))
      (:readers-load
       (destructuring-bind (texts) (rest form)
         (multiple-value-bind (loaded refused) (agent:register-reader-texts texts)
           (dolist (session (server-sessions server))
             (dolist (pane (session-panes session))
               (agent:agent-become (pane-agent pane) :title (pane-named pane)
                                                     :command (pane-command pane)
                                                     :programs (pane-programs pane)
                                                     :paths (pane-paths pane))))
           (tell watcher (list :readers-loaded loaded refused)))))
      (:agent-trace
       (destructuring-bind (name id) (rest form)
         (let ((pane (pane-called server name id)))
           (tell watcher (list :agent-traced name id
                               (and pane (reverse (agent:agent-trace (pane-agent pane)))))))))
      (:agent-explain
       (destructuring-bind (name id) (rest form)
         (let ((pane (pane-called server name id)))
           (if pane
               (multiple-value-bind (seen rows)
                   (agent:agent-explain (pane-agent pane) (pane-term pane))
                 (tell watcher (list :agent-explained name id
                                     (agent:agent-state (pane-agent pane)) seen rows)))
               (tell watcher (list :agent-explained name id :gone nil nil))))))
      (:panes (tell watcher (list :panes (pane-rows server (now-ms)))))
      (:watch-panes
       (setf (watcher-watch-panes watcher) (and (second form) t))
       (clrhash (watcher-panes-told watcher))
       (when (second form) (tell-the-panes server watcher (now-ms))))
      (:watch-screens
       (let ((n (second form)))
         (setf (watcher-screen-rows watcher)
               (and (integerp n) (plusp n) (min n +biggest-pane+)))
         (clrhash (watcher-screens-told watcher))))
      (:pane-screen
       (destructuring-bind (name id n) (rest form)
         (let ((pane (pane-called server name id)))
           (tell watcher (list* :pane-screen name id
                                (and pane (pane-screen-said
                                           pane (max 1 (min n +biggest-pane+)))))))))
      (:pane-history
       (destructuring-bind (name id) (rest form)
         (let ((pane (pane-called server name id)))
           (tell watcher (list :pane-history name id
                               (and pane (history-said (pane-agent pane) (now-ms))))))))
      (:pane-log
       (destructuring-bind (name id n) (rest form)
         (let ((pane (pane-called server name id))
               (now (now-ms)))
           (tell watcher (list :pane-log name id
                               (and pane
                                    (mapcar (lambda (e) (input-said e now))
                                            (subseq (pane-log pane)
                                                    0 (min (max 0 n)
                                                           (length (pane-log pane)))))))))))
      (:answer
       (destructuring-bind (name id n &optional caller) (rest form)
         (answer-a-pane server watcher name id n caller)))
      (:focus-pane
       (destructuring-bind (name id) (rest form)
         (focus-a-pane server watcher name id)))
      (:go-to-blocked (take-to-the-blocked server watcher))
      (:lately (tell watcher (list :lately (lately server (or (second form) 5) (now-ms)))))
      (:pane-about
       (destructuring-bind (name id) (rest form)
         (let ((pane (pane-called server name id)))
           (tell watcher (list :pane-about name id (and pane (pane-about pane)))))))
      (:close-pane
       (destructuring-bind (name id) (rest form)
         (let* ((session (session-named server name))
                (pane (and session (find id (session-panes session) :key #'pane-id))))
           (when pane (close-the-pane session pane)))))
      (:split-in
       (destructuring-bind (name &optional (way :across)) (rest form)
         (let ((session (session-named server name)))
           (when session (split-the-session session way)))))
      ;; windows: every one names the session, so a client on another session,
      ;; or the command line, can ask the same
      (:new-window
       (destructuring-bind (name &optional command directory) (rest form)
         (let ((session (session-named server name)))
           (when session
             (let ((w (add-window session command directory)))
               (tell watcher (list :window name (window-number session w))))))))
      (:go-window
       (destructuring-bind (name n) (rest form)
         (let ((session (session-named server name)))
           (when session
             (unless (eq session (watcher-session watcher))
               (join-session server watcher session))
             (go-to-window session n)))))
      (:next-window
       (let ((session (session-named server (second form))))
         (when session (step-window session 1))))
      (:previous-window
       (let ((session (session-named server (second form))))
         (when session (step-window session -1))))
      (:close-window
       (destructuring-bind (name &optional n) (rest form)
         (let* ((session (session-named server name))
                (window (and session (if n (window-called session n) (session-window session)))))
           (when window (close-a-window session window)))))
      (:name-window
       (destructuring-bind (name n label) (rest form)
         (let* ((session (session-named server name))
                (window (and session (if n (window-called session n) (session-window session)))))
           (when window (name-a-window session window label)))))
      (:layouts
       (let ((session (session-named server (second form))))
         (tell watcher
               (list :layouts (second form)
                     (and session
                          (loop :for w :in (session-windows session)
                                :for n :from 1
                                :collect (list n (window-label w) (layout-said (window-layout w))
                                               (and (window-focus w) (pane-id (window-focus w)))
                                               (eq w (session-window session)))))))))
      (:zoom
       (destructuring-bind (&optional name id) (rest form)
         (zoom-a-pane server watcher name id)))
      (:pane-read
       (destructuring-bind (name id) (rest form)
         (read-a-pane server watcher name id)))
      (:detach (drop-watcher server watcher))
      (:stop (setf (server-going server) nil))
      (t (and session (heard-about-a-session session watcher form))))
    t))

(defun heard-about-a-session (session watcher form)
  (case (first form)
    (:keys
     (let ((pane (session-focus session))
           (said (second form)))
       (setf (watcher-typed-at watcher) (now-ms))
       (pane-logged pane (now-ms) (who-of watcher) :keys (length said))
       ;; somebody typing wants to see what they are typing at
       (pane-scroll-to pane 0)
       (pane-say pane said))
     t)
    (:resize
     (destructuring-bind (rows cols) (rest form)
       (setf (watcher-rows watcher) (max 1 (min +biggest-pane+ rows))
             (watcher-cols watcher) (max 1 (min +biggest-pane+ cols)))
       (session-fit session)
       (tell-the-clients (session-server session)))
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
    (:pointer
     (destructuring-bind (what button x y &optional mods) (rest form)
       (pointer-at session watcher what button x y mods))
     t)
    (:wheel
     (destructuring-bind (way &optional x y mods) (rest form)
       (wheel-at session way x y mods))
     t)
    (:scroll
     (destructuring-bind (amount &optional x y) (rest form)
       (scroll-the-pane (or (and x y (pane-at session x y)) (session-focus session))
                        amount))
     t)
    (:reading
     ;; a client reading a pane back says so, so the bar can say it for everybody
     (setf (session-readers session)
           (if (second form)
               (adjoin watcher (session-readers session))
               (remove watcher (session-readers session))))
     (dolist (w (session-watchers session)) (setf (watcher-behind w) t))
     t)
    (:scrollbars
     (setf (session-scrollbarsp session) (if (eq (second form) :toggle)
                                             (not (session-scrollbarsp session))
                                             (and (second form) t)))
     (dolist (w (session-watchers session)) (setf (watcher-behind w) t))
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
                       (format *error-output* "~&atty: nothing here does ~S~%"
                               (and (consp form) (first form)))
                       (finish-output *error-output*))
                   (error (e)
                     (format *error-output* "~&atty: ~S: ~A~%"
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
    (setf (server-going server) nil))
  server)

(defun session-observe (session now)
  (let ((changed nil))
    (dolist (pane (session-panes session))
      (pane-notice-programs pane (floor now 1000000))
      (when (agent:agent-look (pane-agent pane) (pane-term pane)
                              (floor now 1000000) (pane-dirty pane))
        (push pane changed)))
    (dolist (pane changed)
      (when (session-server session)
        (send-what-waited (session-server session) pane))
      (let ((events (agent:agent-take-events (pane-agent pane))))
        (dolist (w (session-watchers session))
          (setf (watcher-behind w) t)
          (when (wire-open (watcher-wire w))
            (tell w (cons :agent (agent-row session pane)))
            (dolist (event events)
              (destructuring-bind (kind n what) event
                (declare (ignore kind))
                (tell w (list :agent-turn (session-name session) (pane-id pane) n what))))))))
    changed))

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
         (due (min (if oldest
                       (max 0 (ceiling (- gap (- now oldest)) 1000000))
                       100)
                   (if (server-later server) (soon server now) 100)))
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
    (run-what-is-due server (nanos))
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
    (let ((done nil)
          (now (now-ms)))
      (loop :for (session pane n) :in ptys
            :do (when (and (member pane (session-panes session))
                           (tty:readable-p (tty:waiting-back w n)))
                  (setf (pane-moved-at pane) now)
                  (unless (pane-drain pane)
                    (push (cons session pane) done))))
      (loop :for (session . pane) :in done
            :do (close-the-pane session pane)))
    (dolist (session sessions)
      (if (session-panes session)
          (progn (session-observe session (nanos))
                 (session-serve session gap))
          (end-the-session server session :done)))
    (tell-the-watching server (now-ms))
    ;; what that said to anybody not on a session is sent now rather than when
    ;; the next wakeup finds their descriptor writable
    (dolist (watcher (every-watcher server))
      (when (and (wire-open (watcher-wire watcher))
                 (plusp (wire-pending (watcher-wire watcher))))
        (wire-flush (watcher-wire watcher))))
    server))

(defparameter +faults+ 10)

(defun say-what-broke (e)
  (format *error-output* "~&atty: ~A~%" e)
  (ignore-errors
   (sb-debug:print-backtrace :stream *error-output* :count 30))
  (finish-output *error-output*))

(defun serve (path command &key (name "0") (rows 24) (cols 80)
                                (interval *interval*))
  "Hold the sessions and feed whoever is watching them, until there are none.

With NAME nil it starts holding nothing, and the first client to open a session
makes one; see +FIRST-SESSION-PATIENCE+.

A fault in one wakeup is said and stepped over rather than taken as the end. The
panes are somebody's shells: losing them to a bug in the emulator is worse than
drawing one frame wrong, and the backtrace is in the log either way. Faults one
after another with nothing between them are a loop rather than a mishap, and
that does end it."
  (let ((server (make-server path))
        (faults 0))
    (setf (server-command server) command)
    (when *init-problem*
      (push (list (format nil "in the server, ~A" *init-problem*) :warning)
            (server-notes server)))
    (unwind-protect
         (progn
           (when name
             (add-session server command :name name :rows rows :cols cols))
           (loop while (server-wanted-p server (nanos))
                 do (handler-case
                        (progn (server-step server :interval interval)
                               (setf faults 0))
                      (error (e)
                        (say-what-broke e)
                        (when (> (incf faults) +faults+)
                          (format *error-output*
                                  "~&atty: ~D faults with nothing between them; stopping.~%"
                                  faults)
                          (setf (server-going server) nil)))))
           server)
      (server-close server))))
