;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What libatty calls a key, and what a mode calls one.

(defparameter +key-names+
  '((:up . "Up") (:down . "Down") (:left . "Left") (:right . "Right")
    (:home . "Home") (:end . "End") (:insert . "Insert") (:delete . "Delete")
    (:page-up . "PageUp") (:page-down . "PageDown")
    (:enter . "RET") (:tab . "TAB") (:backspace . "DEL") (:escape . "Escape")))

(defun key-named (what)
  (or (cdr (assoc what +key-names+))
      (string-capitalize (symbol-name what))))

(defun event-key (event)
  "A key as the emulator reads it, as a key as a mode knows it."
  (etypecase event
    (character
     (let ((code (char-code event)))
       (cond ((= code 27) (atty/mode:make-key "Escape"))
             ((= code 13) (atty/mode:make-key "RET"))
             ((= code 9) (atty/mode:make-key "TAB"))
             ((= code 127) (atty/mode:make-key "DEL"))
             ((= code 32) (atty/mode:make-key "SPC"))
             ((< code 32) (atty/mode:make-key (string (code-char (+ 96 code)))
                                            :ctrl t))
             (t (atty/mode:make-key (string event))))))
    (cons
     (let ((mods (rest event)))
       (atty/mode:make-key (key-named (first event))
                         :ctrl (and (member :ctrl mods) t)
                         :meta (and (member :meta mods) t)
                         :shift (and (member :shift mods) t))))))

(defvar *mouse-position* nil
  "Where the click a command is running for landed, as (X . Y). Read the way
*CLIENT* is, rather than passed: a mouse binding takes no arguments either.")

(defvar *mouse-event* nil
  "The whole of what the mouse did, as the terminal said it: which button, and
what was held down with it. Beside *MOUSE-AT*, for the commands that pass a
click on rather than act on where it was.")

(defun mouse-event-key (event)
  "A decoded mouse EVENT as a key a mode can bind: a button going down, the
same with -up when it comes back, with -drag while it moves held down, and the
wheel either way. Nil for what has no name, which is the pointer moving with
nothing held. Unlike KEY-OF, EVENT's tail is a plist, not a list of the
modifiers that are down, so it is read with GETF rather than MEMBER."
  (let* ((e (rest event))
         (wheel (getf e :wheel))
         (button (case (getf e :button) (:left 1) (:middle 2) (:right 3)))
         (sym (cond
                (wheel (format nil "wheel-~(~A~)" wheel))
                ((null button) nil)
                (t (format nil "mouse-~D~A" button
                           (cond ((getf e :drag) "-drag")
                                 ((getf e :release) "-up")
                                 (t "")))))))
    (when sym
      (atty/mode:make-key sym :ctrl (getf e :ctrl) :meta (getf e :meta)
                             :shift (getf e :shift)))))

(defparameter +max-partial-sequence+ 64
  "How much of an unfinished sequence to keep for the next read. More than this
is not somebody typing a key.")

(defun pending-input (watcher said)
  "SAID with whatever was left half-said at the end of the last read in front of
it."
  (if (plusp (length (watcher-partial watcher)))
      (prog1 (concatenate 'string (watcher-partial watcher) said)
        (setf (watcher-partial watcher) ""))
      said))

(defun hold-input (watcher said at n)
  (setf (watcher-partial watcher)
        (if (> (- n at) +max-partial-sequence+) "" (subseq said at n))))

(defun chord-event (watcher event)
  "EVENT, a key or a click as the terminal sent it, to the mode WATCHER is in.
A click is a key a mode can bind, and says where it landed as *MOUSE-POSITION*."
  (if (and (consp event) (eq :mouse (first event)))
      (let ((key (mouse-event-key event)))
        (when key
          (let ((*mouse-position* (cons (getf (rest event) :x) (getf (rest event) :y)))
                (*mouse-event* (rest event)))
            (cond ((watcher-menu watcher)
                   (when (string= "mouse-1" (atty/mode:spelled key))
                     (let ((*client* watcher)) (handle-menu-click watcher))))
                  ((and (watcher-partial-chord watcher) (getf (rest event) :release)))
                  (t (press-chord watcher key))))))
      (press-chord watcher (event-key event))))

(defun handle-key (watcher said)
  "Bytes, as keys, to the mode whatever is on top put WATCHER in. A sequence
that is no key a mode knows is passed over, and one the rest of has not
arrived yet is kept until it has."
  (let* ((said (pending-input watcher said))
         (at 0)
         (n (length said)))
    (loop :while (< at n)
          :do (multiple-value-bind (event took)
                  (tty:escape-sequence-to-key-event said at n nil)
                (when (zerop took) (hold-input watcher said at n) (return))
                (incf at took)
                (when event (chord-event watcher event))))))

(defparameter +menu-delay+ 300
  "How long a prefix has to hang, in milliseconds, before the menu is drawn.")

(declaim (ftype (function (watcher) boolean) menu-due-p))
(defun menu-due-p (watcher)
  (and (watcher-partial-chord watcher)
       (watcher-pending-since watcher)
       (>= (- (now-ms) (watcher-pending-since watcher)) +menu-delay+)))

(defun menu-later (watcher)
  (let ((session (watcher-session watcher)))
    (when (and session (session-server session))
      (schedule-task (session-server session) +menu-delay+
                     (lambda () (draw-again watcher))))))

(defun press-chord (watcher key)
  "Give KEY to the mode WATCHER is in. Answers whether the chord wants more."
  (let* ((*client* watcher)
         (*rows* (list nil))
         (over (first (watcher-overlays watcher)))
         (before (watcher-partial-chord watcher))
         (atty/mode:*pending* before)
         (atty/mode:*unbound* (lambda (chord) (overlay-unbound-key over chord watcher))))
    (flet ((begun-inside () (not (eq before (watcher-partial-chord watcher)))))
      (prog1 (let ((how (atty/mode:press (atty/mode:spelled key)
                                         (atty/mode:mode-named (watcher-mode watcher)))))
               (cond ((eq how :pending)
                      (unless (watcher-pending-since watcher)
                        (setf (watcher-pending-since watcher) (now-ms))
                        (menu-later watcher)))
                     ((begun-inside))
                     (t
                      (setf (watcher-pending-since watcher) nil)
                      (when (watcher-menu watcher)
                        (setf (watcher-menu watcher) nil))))
               (eq how :pending))
        (unless (begun-inside)
          (setf (watcher-partial-chord watcher) atty/mode:*pending*))
        (setf (watcher-behind watcher) t)))))

(defun type-into-pane (watcher text)
  "TEXT typed into the pane with the focus of WATCHER's session."
  (let* ((session (watcher-session watcher))
         (pane (and session (session-focus session))))
    (when pane
      (let ((now (now-ms))
            (actor (actor-of watcher)))
        (setf (watcher-typed-at watcher) now
              (watcher-keyed-at watcher) (monotonic-ns))
        (when (watcher-following watcher) (stop-following (session-server session) watcher))
        (when (and (pane-running pane) (pane-started pane))
          (setf (pane-typed-at pane) now))
        (on-pane pane (lambda ()
                        (pane-note-log pane now actor :keys (length text) t)
                        (pane-scroll-to pane 0)
                        (when (and (pane-running pane) (pane-started pane))
                          (pane-send pane text)))
                 :wait nil)))))

(defun keys-into (watcher said)
  (let* ((said (pending-input watcher said))
         (at 0)
         (n (length said)))
    (loop :while (< at n)
          :do (multiple-value-bind (event took)
                  (tty:escape-sequence-to-key-event said at n nil)
                (when (zerop took) (hold-input watcher said at n) (return))
                (incf at took)
                (when (and event (not (and (consp event) (eq :mouse (first event)))))
                  (setf (watcher-keys-read watcher)
                        (append (watcher-keys-read watcher) (list (event-key event)))))))))

(defun read-key (&optional (watcher *client*))
  (unless (and watcher (eq (watcher-thread watcher) sb-thread:*current-thread*))
    (error "only a client's own commands can wait for its keys"))
  (let ((w (tty:make-waiting 2)))
    (setf (watcher-reading watcher) t)
    (unwind-protect
         (loop
           (let ((key (pop (watcher-keys-read watcher))))
             (when key
               (if (string= "C-g" (atty/mode:spelled key))
                   (error 'quit)
                   (return key))))
           (unless (wire-open (watcher-wire watcher))
             (error 'quit))
           (watcher-turn *server* watcher w))
      (setf (watcher-reading watcher) nil)
      (tty:free-waiting w))))

(defun handle-input (watcher said)
  "Pass what was typed to the pane, byte for byte, until the one byte that says
a chord is starting. Once a chord has started, or while something on top takes
the keys, bytes are read as keys and the pane does not see them. A mouse report
this build has a name for goes to the mode; any other is passed on."
  (when (watcher-reading watcher)
    (return-from handle-input (keys-into watcher said)))
  (when (and (watcher-overlays watcher)
             (not (overlay-passes-keys-p (first (watcher-overlays watcher)))))
    (return-from handle-input (handle-key watcher said)))
  (let* ((said (pending-input watcher said))
         (out (make-array (length said) :element-type 'character :fill-pointer 0))
         (at 0)
         (n (length said)))
    (flet ((send ()
             (when (plusp (fill-pointer out))
               (type-into-pane watcher (coerce out 'simple-string))
               (setf (fill-pointer out) 0))))
      (loop :while (< at n)
            :do (if (watcher-partial-chord watcher)
                    (multiple-value-bind (event took)
                        (tty:escape-sequence-to-key-event said at n nil)
                      (when (zerop took) (hold-input watcher said at n) (return))
                      (incf at took)
                      (when event (chord-event watcher event))
                      (when (watcher-overlays watcher)
                        (send)
                        (return (handle-key watcher (subseq said at n)))))
                    (let ((ch (char said at)))
                      (cond
                        ((char= ch +prefix+)
                         (incf at)
                         (send) (press-chord watcher (event-key ch)))
                        ((char= ch #\Escape)
                         (multiple-value-bind (event took)
                             (tty:escape-sequence-to-key-event said at n nil)
                           (let ((key (and (plusp took) (consp event)
                                           (eq (first event) :mouse)
                                           (mouse-event-key event))))
                             (cond
                               (key
                                (send)
                                (let ((*mouse-position* (cons (getf (rest event) :x)
                                                              (getf (rest event) :y)))
                                      (*mouse-event* (rest event)))
                                  (press-chord watcher key))
                                (incf at took))
                               ((and (watcher-overlays watcher) (<= took 1))
                                (send)
                                (press-chord watcher (event-key ch))
                                (incf at))
                               (t (vector-push ch out) (incf at))))))
                        (t (incf at) (vector-push ch out))))))
      (send))))
