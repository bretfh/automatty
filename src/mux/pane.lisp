;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defvar *panes-made* 0)

(defun next-number (symbol)
  (loop :for was := (symbol-value symbol)
        :until (eql was (sb-ext:compare-and-swap (symbol-value symbol) was (1+ was)))
        :finally (return (1+ was))))

(sb-alien:define-alien-routine ("sysconf" %sysconf) sb-alien:long (name sb-alien:int))

(defconstant +processors-online+
  #+darwin 58
  #+linux 84
  #-(or darwin linux) (error "atty has no _SC_NPROCESSORS_ONLN for this system."))

(defvar *drains* (sb-thread:make-semaphore :name "panes draining in the background"
                                           :count (max 2 (- (%sysconf +processors-online+) 2))))

(defvar *drain-octets* nil)
(defvar *drain-chars* (make-string 0))

(defparameter +pulse-cells+ 16 "How many cells a pane's pulse has.")
(defparameter +pulse-interval+ 75000 "How long one cell of the pulse covers, in milliseconds.")
(defparameter +max-events+ 64 "How many events a pane keeps.")

(defparameter +shells+
  '("sh" "bash" "zsh" "fish" "dash" "ksh" "mksh" "oksh" "tcsh" "csh" "yash"
    "nu" "elvish" "xonsh")
  "Programs that are a shell, and so are called one rather than by name.")

(defparameter +state-rank+ '(:blocked :working :idle :unknown)
  "States, the most urgent first; nil is quiet and comes last.")

(defun state-rank-of (state)
  (or (position state +state-rank+) (length +state-rank+)))

(defstruct (pane (:constructor %make-pane))
  (id 0 :type fixnum)
  (term nil)
  (fd -1 :type fixnum)
  (pid -1 :type fixnum)
  (running t :type boolean)
  (failed nil)
  (rang 0 :type fixnum)
  (named nil)
  (label nil)
  (log nil)
  (log-count 0 :type fixnum)
  (moved-at 0 :type integer)
  (typed-at 0 :type integer)
  (pending-prompt nil)
  (command nil)
  (directory nil)
  (agent nil)
  (group nil)
  (programs nil)
  (paths nil)
  (programs-at 0)
  (group-at 0 :type integer)
  (look-at nil :type (or null integer))
  (scrolled 0 :type fixnum)
  (find nil)
  (selecting nil)
  (pushed-seen 0 :type fixnum)
  (touched 0 :type integer)
  (titled-at 0 :type integer)
  (scrolled-at 0 :type integer)
  (saved-at 0 :type integer)
  ;; the last twenty minutes: a cell every +pulse-every+ of how much was
  ;; written and the worst state it was in, oldest first
  (pulse (loop :repeat +pulse-cells+ :collect (cons 0 nil)) :type list)
  (pulse-at -1 :type integer)
  (pulse-state nil)
  (output 0 :type integer)
  ;; what happened here lately, newest first: (ms clock kind who text)
  (events nil :type list)
  (events-count 0 :type fixnum)
  (decoder (term:make-decoder))
  (outbox nil :type (or null (simple-array (unsigned-byte 8) (*))))
  (out-start 0 :type fixnum)
  (out-end 0 :type fixnum)
  (thread nil)
  (woken nil)
  (look nil)
  (looked-at -1 :type integer)
  (shown nil)
  (inbox nil)
  (wake nil)
  (stopping nil)
  (ended nil)
  (changed nil)
  (gap 0 :type integer)
  (watched nil)
  (urgent nil)
  (kept 0 :type fixnum)
  (written-at 0 :type integer)
  (echoed 0 :type integer))

(defun pane-roll-pulse (pane now state)
  "Bring PANE's pulse up to NOW, in milliseconds: a fresh cell for every
+pulse-every+ that has passed, then what was read since is added to the newest
cell and STATE kept in it when it is worse than what was."
  (let ((cell (floor now +pulse-interval+)))
    (when (< (pane-pulse-at pane) cell)
      (let ((gap (if (minusp (pane-pulse-at pane))
                     0
                     (min +pulse-cells+ (- cell (pane-pulse-at pane))))))
        (setf (pane-pulse pane) (append (nthcdr gap (pane-pulse pane))
                                        (loop :repeat gap :collect (cons 0 (pane-pulse-state pane))))
              (pane-pulse-at pane) cell)))
    (setf (pane-pulse-state pane) state)
    (setf (pane-pulse pane) (pulse-with (pane-pulse pane) (pane-output pane) state)
          (pane-output pane) 0)
    (pane-pulse pane)))

(defun pulse-with (cells output state)
  (let ((newest (car (last cells))))
    (append (butlast cells)
            (list (cons (+ (car newest) output)
                        (if (< (state-rank-of state) (state-rank-of (cdr newest))) state (cdr newest)))))))

(declaim (ftype (function (pane integer) list) pane-pulse-now))
(defun pane-pulse-now (pane now)
  (let* ((agent (pane-agent pane))
         (cells (pane-pulse pane))
         (cell (floor now +pulse-interval+))
         (at (pane-pulse-at pane))
         (gap (if (or (minusp at) (>= at cell)) 0 (min +pulse-cells+ (- cell at)))))
    (pulse-with (append (nthcdr gap cells) (loop :repeat gap :collect (cons 0 (pane-pulse-state pane))))
                0
                (and (agent:agent-reader agent) (agent:agent-state agent)))))

(defun pane-push-event (pane now kind actor &optional text)
  "Put in PANE's events that KIND happened at NOW, done by WHO when somebody
did it, with TEXT saying what."
  (push (list now (get-universal-time) kind actor text) (pane-events pane))
  (when (> (incf (pane-events-count pane)) +max-events+)
    (setf (pane-events pane) (subseq (pane-events pane) 0 (floor +max-events+ 2))
          (pane-events-count pane) (floor +max-events+ 2)))
  (first (pane-events pane)))

(defparameter +program-poll-interval+ 1000)

(defun make-pane (command &key (rows 24) (cols 80) directory id)
  "A pane with a terminal that size and no program in it yet.

Starting it is a second step because the size it is started at is the size it is
told, once: a shell that asks stty for it on its first line must be told the
room the layout gave it rather than a guess it is corrected out of afterwards.

ID is for a pane brought back from disk, which keeps the number it had; one
made now takes the next."
  (let ((pane (%make-pane :id (or id (next-number '*panes-made*)) :command command
                          :directory directory
                          :agent (agent:make-agent :command command))))
    (setf (pane-term pane)
          (term:make-term :width cols :height rows :max-scrollback +max-scrollback+
                        :bell-fn (lambda (term)
                                   (declare (ignore term))
                                   (incf (pane-rang pane)))
                        :title-fn (lambda (term title)
                                    (declare (ignore term))
                                    (setf (pane-named pane) title
                                          (pane-touched pane) (now-ms)
                                          (pane-titled-at pane) (now-ms))
                                    (agent:agent-become (pane-agent pane) :title title
                                                                          :command command
                                                                          :programs (pane-programs pane)
                                                                          :paths (pane-paths pane)))
                        :input-fn (lambda (term said)
                                    (declare (ignore term))
                                    (pane-write pane said))))
    pane))

(defun pane-started (pane) (>= (pane-fd pane) 0))

(defparameter +group-poll-interval+ 200)

(declaim (ftype (function (pane integer) t) pane-look-later))
(defun pane-look-later (pane ms)
  (let ((at (pane-look-at pane)))
    (when (or (null at) (< ms at))
      (setf (pane-look-at pane) ms))))

(declaim (ftype (function (pane) t) pane-look-soon))
(defun pane-look-soon (pane)
  (setf (pane-look-at pane) 0)
  (when (pane-thread-p pane) (pane-poke pane)))

(declaim (ftype (function (pane keyword) t) pane-hear))
(defun pane-hear (pane state)
  "What PANE's program says it is doing, looked at on the next step."
  (on-pane pane (lambda ()
                  (agent:agent-hear (pane-agent pane) state)
                  (pane-look-soon pane))
           :wait nil))

(declaim (ftype (function (pane integer) boolean) pane-due-p))
(defun pane-due-p (pane now)
  (or (> (pane-moved-at pane) (pane-looked-at pane))
      (let ((at (pane-look-at pane)))
        (and at (<= at now)))))

(declaim (ftype (function (pane integer) t) pane-update-programs))
(defun pane-update-programs (pane now)
  (when (and (pane-started pane) (pane-running pane))
    (if (< (- now (pane-group-at pane)) +group-poll-interval+)
        (pane-look-later pane (+ (pane-group-at pane) +group-poll-interval+))
        (let ((group (pty:pty-foreground (pane-fd pane)))
              (reader (agent:agent-reader (pane-agent pane))))
          (setf (pane-group-at pane) now)
          (cond
            ((or (not (eql group (pane-group pane)))
                 (and (null reader)
                      (>= (- now (pane-programs-at pane)) +program-poll-interval+)))
             (let* ((running (and group (pty:group-processes group)))
                    (was (program-name (first (pane-programs pane))))
                    (is (program-name (first (mapcar (lambda (p) (getf p :line)) running)))))
               (setf (pane-group pane) group
                     (pane-programs-at pane) now
                     (pane-programs pane) (mapcar (lambda (p) (getf p :line)) running)
                     (pane-paths pane) (mapcar (lambda (p) (getf p :path)) running))
               (unless (string= was is)
                 (setf (pane-titled-at pane) now)
                 (when (and (plusp (length was)) (not (member was +shells+ :test #'string=)))
                   (pane-push-event pane now :finished nil was))
                 (when (and (plusp (length is)) (not (member is +shells+ :test #'string=)))
                   (pane-push-event pane now :started nil is))))
             (agent:agent-become (pane-agent pane) :title (pane-named pane)
                                                   :command (pane-command pane)
                                                   :programs (pane-programs pane)
                                                   :paths (pane-paths pane)))
            ((null reader)
             (pane-look-later pane (+ (pane-programs-at pane) +program-poll-interval+))))))))

(defun pane-start (pane &key environment woken look watched urgent (gap 0))
  "Run the pane's program on a terminal of its own, the size the pane is now."
  (when woken (setf (pane-woken pane) woken))
  (when look (setf (pane-look pane) look))
  (setf (pane-gap pane) gap
        (pane-watched pane) watched
        (pane-urgent pane) urgent)
  (unless (or (pane-started pane) (pane-failed pane))
    (pane-open pane environment))
  (when (and (pane-started pane) (pane-running pane) (null (pane-thread pane)))
    (pane-run pane))
  pane)

(defun pane-open (pane environment)
  (let ((term (pane-term pane)))
      (handler-case
          (multiple-value-bind (fd pid)
              (pty:spawn-pty-process (pane-command pane)
                                     :rows (term:term-height term)
                                     :cols (term:term-width term)
                                     :environment environment
                                     :directory (pane-directory pane))
            (setf (pane-fd pane) (pty:nonblocking fd)
                  (pane-pid pane) pid))
        (error (e)
          (setf (pane-running pane) nil
                (pane-failed pane) (format nil "~A could not start: ~A" (pane-command pane) e))))))

(defun open-wake-pipe ()
  (multiple-value-bind (in out) (sb-posix:pipe)
    (dolist (fd (list in out))
      (pty:nonblocking (pty:close-on-exec fd)))
    (cons in out)))

(defun drain-wake-pipe (wake)
  (let ((octets (make-array 64 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent octets))
    (sb-sys:with-pinned-objects (octets)
      (loop :while (let ((n (sb-unix:unix-read (car wake) (sb-sys:vector-sap octets) 64)))
                     (and n (plusp n)))))))

(defun close-wake-pipe (wake)
  (when wake
    (ignore-errors (sb-posix:close (car wake)))
    (ignore-errors (sb-posix:close (cdr wake)))))

(defvar *poke-octet* (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1))

(defun pane-poke (pane)
  (let ((wake (pane-wake pane)))
    (when wake
      (sb-sys:with-pinned-objects (*poke-octet*)
        (sb-unix:unix-write (cdr wake) *poke-octet* 0 1)))))

(defun pane-post (pane job)
  (sb-ext:atomic-push job (pane-inbox pane))
  (pane-poke pane))

(defun pane-thread-p (pane)
  (let ((thread (pane-thread pane)))
    (and thread (not (eq thread sb-thread:*current-thread*)) (sb-thread:thread-alive-p thread))))

(declaim (ftype (function (pane function &key (:wait t)) t) on-pane))
(defun on-pane (pane job &key (wait t))
  (cond ((not (pane-thread-p pane)) (funcall job))
        ((not wait) (pane-post pane job) nil)
        (t (let ((done (sb-thread:make-semaphore))
                 (said nil)
                 (broke nil))
             (pane-post pane (lambda ()
                               (unwind-protect
                                    (handler-case (setf said (multiple-value-list (funcall job)))
                                      (serious-condition (c) (setf broke c)))
                                 (sb-thread:signal-semaphore done))))
             (loop :until (sb-thread:wait-on-semaphore done :timeout 1)
                   :unless (pane-thread-p pane)
                     :do (return (setf said (multiple-value-list (funcall job)))))
             (when broke (error broke))
             (values-list said)))))

(defmacro with-term ((term pane) &body body)
  (let ((it (gensym "PANE")))
    `(let ((,it ,pane))
       (on-pane ,it (lambda () (let ((,term (pane-term ,it))) ,@body))))))

(defstruct (shown (:constructor %make-shown))
  screen (history 0 :type fixnum) (paste nil) (at 0 :type integer) (echoed 0 :type integer))

(defun pane-show (pane)
  (let* ((term (pane-term pane))
         (w (term:term-width term))
         (h (term:term-height term))
         (screen (tty:make-screen :width w :height h)))
    (atty/cells:blit (atty/cells:make-cells (tty:screen-grid screen) w h) term 0 0 w h
                     (pane-scrolled pane))
    (setf (tty:screen-cursor-x screen) (term:term-cursor-x term)
          (tty:screen-cursor-y screen) (term:term-cursor-y term)
          (tty:screen-cursor-visible screen) (and (term:term-cursor-visible term) t)
          (tty:screen-cursor-style screen) (term:term-cursor-style term)
          (pane-shown pane) (%make-shown :screen screen
                                         :at (monotonic-ns)
                                         :echoed (pane-echoed pane)
                                         :history (if (term:term-in-alt-screen term)
                                                      0
                                                      (term:term-scrollback-size term))
                                         :paste (and (term:term-bracketed-paste term) t)))))

(defun pane-show-history (pane)
  (let* ((term (pane-term pane))
         (had (pane-shown pane))
         (history (if (term:term-in-alt-screen term) 0 (term:term-scrollback-size term)))
         (paste (and (term:term-bracketed-paste term) t)))
    (unless (and (= history (shown-history had)) (eq paste (shown-paste had)))
      (setf (pane-shown pane) (%make-shown :screen (shown-screen had) :history history
                                           :paste paste :at (shown-at had)
                                           :echoed (shown-echoed had))))))

(defun pane-pastes-p (pane)
  (let ((shown (pane-shown-now pane)))
    (if shown (shown-paste shown) (and (term:term-bracketed-paste (pane-term pane)) t))))

(defun pane-tell (pane &optional ended)
  (let ((woken (pane-woken pane)))
    (if woken (funcall woken ended) (tty:wake))))

(defun pane-show-due (pane)
  (let ((shown (pane-shown pane)))
    (if (> (pane-echoed pane) (shown-echoed shown))
        (shown-at shown)
        (+ (shown-at shown) (pane-gap pane)))))

(defun pane-watched-p (pane)
  (let ((watched (pane-watched pane)))
    (or (null watched) (funcall watched))))

(defun pane-drain-now (pane)
  (let ((urgent (pane-urgent pane)))
    (if (or (null urgent) (funcall urgent))
        (pane-drain pane)
        (if (sb-thread:wait-on-semaphore *drains* :timeout 0.005)
            (unwind-protect (pane-drain pane)
              (sb-thread:signal-semaphore *drains*))
            :later))))

(defun pane-look-due (pane)
  (let ((at (pane-look-at pane))
        (moved (and (> (pane-moved-at pane) (pane-looked-at pane))
                    (+ (pane-looked-at pane) (floor (pane-gap pane) 1000000)))))
    (if (and at moved) (min at moved) (or at moved))))

(defun pane-wait (pane)
  (let* ((at (pane-look-due pane))
         (ms (if at (max 0 (min 1000 (- at (now-ms)))) 1000)))
    (if (and (pane-changed pane) (pane-watched-p pane))
        (min ms (max 0 (ceiling (- (pane-show-due pane) (monotonic-ns)) 1000000)))
        ms)))

(defun pane-shown-now (pane)
  (and (pane-thread-p pane) (pane-shown pane)))

(defun pane-jobs (pane)
  (let ((jobs (loop :for had := (pane-inbox pane)
                    :until (eq had (sb-ext:compare-and-swap (pane-inbox pane) had nil))
                    :finally (return had))))
    (dolist (job (reverse jobs))
      (funcall job))))

(defun pane-run (pane)
  (setf (pane-wake pane) (open-wake-pipe)
        (pane-stopping pane) nil)
  (let ((placed (sb-thread:make-semaphore)))
    (setf (pane-thread pane)
          (sb-thread:make-thread
           (lambda ()
             (sb-thread:wait-on-semaphore placed)
             (pane-run-here pane))
           :name (format nil "atty pane ~D" (pane-id pane))))
    (sb-thread:signal-semaphore placed)))

(defun pane-run-here (pane)
  (pane-show pane)
  (when (pane-look pane) (funcall (pane-look pane) pane (now-ms)))
  (let ((*drain-octets* nil)
        (*drain-chars* (make-string 0))
        (w (tty:make-waiting 2)))
    (unwind-protect
         (loop :until (pane-stopping pane)
               :do (tty:waiting-clear w)
                   (tty:waiting-add w (pane-fd pane)
                                    (if (pane-owing-p pane)
                                        (logior sb-unix:pollin sb-unix:pollout)
                                        sb-unix:pollin))
                   (tty:waiting-add w (car (pane-wake pane)))
                   (tty:wait-on w (pane-wait pane))
                   (when (tty:readable-p (tty:waiting-back w 1))
                     (drain-wake-pipe (pane-wake pane)))
                   (pane-jobs pane)
                   (let ((back (tty:waiting-back w 0)))
                     (when (and (pane-owing-p pane) (tty:writable-p back))
                       (pane-flush pane))
                     (when (tty:readable-p back)
                       (let ((drained (pane-drain-now pane)))
                         (unless drained
                           (setf (pane-ended pane) t)
                           (pane-tell pane t)
                           (return))
                         (unless (eq drained :later)
                           (setf (pane-changed pane) t
                                 (pane-echoed pane) (pane-written-at pane)
                                 (pane-moved-at pane) (now-ms)))))
                     (when (and (pane-changed pane) (>= (monotonic-ns) (pane-show-due pane)))
                       (if (pane-watched-p pane)
                           (progn (setf (pane-changed pane) nil)
                                  (pane-show pane)
                                  (pane-tell pane))
                           (pane-show-history pane)))
                     (let ((look (pane-look pane))
                           (due (pane-look-due pane))
                           (ms (now-ms)))
                       (when (and look due (<= due ms))
                         (funcall look pane ms)))))
      (pane-jobs pane)
      (tty:free-waiting w))))

(defun pane-stop (pane)
  (let ((thread (pane-thread pane)))
    (when (and thread (not (eq thread sb-thread:*current-thread*)))
      (setf (pane-stopping pane) t)
      (pane-poke pane)
      (sb-thread:join-thread thread :default nil :timeout 5))
    (setf (pane-thread pane) nil)
    (close-wake-pipe (pane-wake pane))
    (setf (pane-wake pane) nil)))

(defun pane-drain (pane &key (budget 16) (size 65536) (most 65536))
  "Read what the program wrote and give it to the term. Answers nil when the
program is done.

At most BUDGET reads a wakeup: the descriptor stays readable and the next poll
comes straight back, so one pane writing without pause cannot starve the rest."
  (dotimes (i budget t)
    (unless (plusp most)
      (return t))
    (let* ((octets (if (and *drain-octets* (>= (length *drain-octets*) size))
                       *drain-octets*
                       (setf *drain-octets* (make-array size :element-type '(unsigned-byte 8)))))
           (n (pty:pty-read-into (pane-fd pane) octets size)))
      (cond
        ((null n) (setf (pane-running pane) nil) (return nil))
        ((zerop n) (return t))
        (t (multiple-value-bind (chars count)
               (term:decode-utf-8-into (pane-decoder pane) octets n *drain-chars*)
             (setf *drain-chars* chars)
             (term:term-process-output (pane-term pane) chars count))
           (pane-scroll-settle pane)
           (setf (pane-kept pane) (term:term-scrollback-size (pane-term pane)))
           (incf (pane-output pane) n)
           (decf most n))))))

;;; How far back a pane is being read. Nought is the screen as the program has
;;; it now; anything more is that many rows up into what has scrolled off it.
;;; It is the pane's and not the client's, the way the focus and the zoom are:
;;; everybody attached is looking at the same pane.

(defun pane-height (pane)
  (let ((shown (pane-shown-now pane)))
    (if shown (tty:screen-height (shown-screen shown)) (term:term-height (pane-term pane)))))

(defun pane-width (pane)
  (let ((shown (pane-shown-now pane)))
    (if shown (tty:screen-width (shown-screen shown)) (term:term-width (pane-term pane)))))

(defun pane-history (pane)
  "How many rows there are behind PANE's screen to scroll back into. A program
that has the whole screen to itself has none: what it draws never scrolls off."
  (let ((shown (pane-shown-now pane)))
    (if shown
        (shown-history shown)
        (let ((term (pane-term pane)))
          (if (term:term-in-alt-screen term) 0 (term:term-scrollback-size term))))))

(defun pane-scroll-to (pane back)
  "Show PANE from BACK rows behind its screen, or as near as there is. Answers
whether that moved it."
  (on-pane pane
           (lambda ()
             (let ((back (max 0 (min (pane-history pane) back))))
               (unless (= back (pane-scrolled pane))
                 (setf (pane-scrolled pane) back
                       (pane-scrolled-at pane) (now-ms))
                 (when (pane-thread pane) (pane-show pane))
                 t)))))

(defun pane-scroll-by (pane rows)
  "ROWS further back, or nearer when it is negative."
  (on-pane pane (lambda () (pane-scroll-to pane (+ (pane-scrolled pane) rows)))))

(defun pane-scroll-settle (pane)
  "The program wrote something. A pane being read back stays on the rows it was
showing, which are now further back by however many went off the top; one whose
program has taken the whole screen is back at it."
  (let* ((term (pane-term pane))
         (pushed (term:term-scrollback-pushed term))
         (more (- pushed (pane-pushed-seen pane))))
    (setf (pane-pushed-seen pane) pushed)
    (when (plusp (pane-scrolled pane))
      (setf (pane-scrolled pane)
            (max 0 (min (pane-history pane)
                        (+ (pane-scrolled pane) (max 0 more))))))))

(declaim (ftype (function (pane) boolean) pane-owing-p))
(defun pane-owing-p (pane)
  (< (pane-out-start pane) (pane-out-end pane)))

(declaim (ftype (function (pane (simple-array (unsigned-byte 8) (*)) fixnum fixnum) t) pane-owe))
(defun pane-owe (pane octets start end)
  (let ((box (pane-outbox pane))
        (have (- (pane-out-end pane) (pane-out-start pane)))
        (more (- end start)))
    (when (or (null box) (> (+ (pane-out-end pane) more) (length box)))
      (let ((new (make-array (max 256 (* 2 (+ have more))) :element-type '(unsigned-byte 8))))
        (when box
          (replace new box :start2 (pane-out-start pane) :end2 (pane-out-end pane)))
        (setf box new
              (pane-outbox pane) new
              (pane-out-start pane) 0
              (pane-out-end pane) have)))
    (replace box octets :start1 (pane-out-end pane) :start2 start :end2 end)
    (incf (pane-out-end pane) more)))

(declaim (ftype (function (pane) t) pane-flush))
(defun pane-flush (pane)
  (when (pane-owing-p pane)
    (let ((n (handler-case (pty:pty-write-some (pane-fd pane) (pane-outbox pane)
                                               (pane-out-start pane) (pane-out-end pane))
               (error () (- (pane-out-end pane) (pane-out-start pane))))))
      (incf (pane-out-start pane) n)
      (unless (pane-owing-p pane)
        (setf (pane-out-start pane) 0
              (pane-out-end pane) 0)))))

(declaim (ftype (function (pane string) t) pane-write))
(defun pane-write (pane said)
  (when (and (pane-running pane) (pane-started pane))
    (on-pane pane (lambda () (pane-send pane said)) :wait nil)))

(defun pane-send (pane said)
  (ignore-errors
   (let* ((octets (sb-ext:string-to-octets said :external-format :latin-1))
          (end (length octets))
          (sent (if (pane-owing-p pane)
                    0
                    (pty:pty-write-some (pane-fd pane) octets 0 end))))
     (setf (pane-written-at pane) (monotonic-ns)
           (pane-typed-at pane) (now-ms))
     (when (< sent end)
       (pane-owe pane octets sent end)))))

(defun pane-resize (pane rows cols)
  (on-pane pane (lambda ()
                  (term:term-resize (pane-term pane) cols rows)
                  (ignore-errors (pty:pty-set-size (pane-fd pane) rows cols))
                  (setf (pane-scrolled pane) (min (pane-scrolled pane) (pane-history pane)))
                  (when (pane-thread pane) (pane-show pane))))
  pane)

(defun pane-close (pane)
  (pane-stop pane)
  (setf (pane-running pane) nil)
  (when (pane-started pane)
    (ignore-errors (pty:pty-close (pane-fd pane)))
    (ignore-errors (pty:pty-reap (pane-pid pane)))))

;;; What kind of program a pane holds, said the way a person would: the agent
;;; it was recognised as, or else whatever has the terminal now.

(defun program-name (line)
  "The program LINE runs: the first word, without where it lives, and without
the dash a login shell is started with."
  (let* ((said (string-trim " " (or line "")))
         (word (subseq said 0 (or (position #\Space said) (length said))))
         (base (subseq word (1+ (or (position #\/ word :from-end t) -1)))))
    (string-left-trim "-" base)))

(defun pane-kind (pane)
  "What the pane holds: a recognised agent's name, \"shell\" for a shell with
nothing in front of it, and otherwise the program in the foreground."
  (let ((agent (pane-agent pane)))
    (if (agent:agent-reader agent)
        (agent:agent-kind agent)
        (let ((name (program-name (or (first (pane-programs pane)) (pane-command pane)))))
          (cond ((zerop (length name)) "shell")
                ((member name +shells+ :test #'string=) "shell")
                (t name))))))

;;; Who typed into a pane, newest first. Keys somebody typed are counted, not
;;; kept: what is typed at a shell is theirs. What an agent verb sent is kept,
;;; since it was said on the record by one program to another.

(defparameter +key-run-gap+ 2000
  "Keys from one source this close together, in milliseconds, are one entry.")

(defun pane-push-log (pane now actor verb summary &optional (outcome t))
  "Put in PANE's log that WHO did VERB at NOW, and the time of day it was. For
:keys SUMMARY is how many bytes; a run of them from the same place is one
entry that grows."
  (on-pane pane (lambda () (pane-note-log pane now actor verb summary outcome))))

(defun pane-note-log (pane now actor verb summary outcome)
  (let ((newest (first (pane-log pane))))
    (if (and newest (eq verb :keys) (eq (third newest) :keys)
             (equal (second newest) actor)
             (<= (- now (first newest)) +key-run-gap+))
        (setf (pane-log pane) (cons (list now actor verb (+ (fourth newest) summary) (fifth newest)
                                          (get-universal-time))
                                    (rest (pane-log pane)))
              (pane-touched pane) now)
        (progn
          (push (list now actor verb summary outcome (get-universal-time)) (pane-log pane))
          (setf (pane-touched pane) now)
          (pane-push-event pane now
                      (case verb
                        (:keys :typed) (:answer :answered) (:prompt :prompted)
                        (:say :said) (:signal :signalled) (t verb))
                      actor
                      (cond ((eq verb :keys) nil)
                            ((eq outcome :refused) (format nil "~A (refused)" summary))
                            (t (and summary (princ-to-string summary)))))
          (when (> (incf (pane-log-count pane)) +log-length+)
            (setf (pane-log pane) (subseq (pane-log pane) 0 (floor +log-length+ 2))
                  (pane-log-count pane) (floor +log-length+ 2)))))
    (first (pane-log pane))))

(defun summarize-text (text &optional (most 60))
  (let ((one-line (substitute #\Space #\Newline (substitute #\Space #\Return text))))
    (if (> (length one-line) most)
        (concatenate 'string (subseq one-line 0 (1- most)) "…")
        one-line)))

(defun default-shell ()
  (or (sb-ext:posix-getenv "SHELL") "/bin/sh"))
