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

(defun client-pending-input (client said)
  "SAID with whatever was left half-said at the end of the last read in front of
it."
  (if (plusp (length (client-partial client)))
      (prog1 (concatenate 'string (client-partial client) said)
        (setf (client-partial client) ""))
      said))

(defun client-hold-input (client said at n)
  (setf (client-partial client)
        (if (> (- n at) +max-partial-sequence+) "" (subseq said at n))))

(defun client-chord-event (client event)
  "EVENT, a key or a click as the terminal sent it, to the mode this client is
in. A click is a key a mode can bind, and says where it landed as *MOUSE-AT*."
  (if (and (consp event) (eq :mouse (first event)))
      (let ((key (mouse-event-key event)))
        (when key
          (let ((*mouse-position* (cons (getf (rest event) :x) (getf (rest event) :y)))
                (*mouse-event* (rest event)))
            (if (client-menu client)
                ;; the menu is up over a half chord: a press is the menu's,
                ;; not another key on the chord
                (when (string= "mouse-1" (atty/mode:spelled key))
                  (let ((*client* client)) (handle-menu-click client)))
                (client-chord client key)))))
      (client-chord client (event-key event))))

(defun client-handle-key (client said)
  "Bytes, as keys, to the mode whatever is on top put the client in.

A sequence the terminal sent that is no key a mode knows, a mouse report say, is
passed over rather than made into one, and one the rest of has not arrived yet is
kept until it has."
  (let* ((said (client-pending-input client said))
         (at 0)
         (n (length said)))
    (loop :while (< at n)
          :do (multiple-value-bind (event took)
                  (tty:escape-sequence-to-key-event said at n nil)
                (when (zerop took) (client-hold-input client said at n) (return))
                (incf at took)
                (when event (client-chord-event client event))))))

(defun client-chord (client key)
  "Give KEY to the mode this client is in. Answers whether the chord wants more
keys.

Which mode, and half a chord, are both this client's rather than the image's:
two of them attached in one process would otherwise be in each other's modes and
finishing each other's chords."
  (let* ((*client* client)
         (over (first (client-overlays client)))
         (atty/mode:*pending* (client-partial-chord client))
         (atty/mode:*unbound* (lambda (chord) (overlay-unbound-key over chord client))))
    (prog1 (let ((how (atty/mode:press (atty/mode:spelled key)
                                       (atty/mode:mode-named (client-mode client)))))
             (if (eq how :pending)
                 (unless (client-pending-since client)
                   (setf (client-pending-since client) (client-ms)))
                 (progn
                   (setf (client-pending-since client) nil)
                   (when (client-menu client)
                     (setf (client-menu client) nil (client-dirty client) t))))
             (eq how :pending))
      (setf (client-partial-chord client) atty/mode:*pending*)
      (when over (setf (client-dirty client) t)))))

(defun client-handle-input (client said)
  "Pass what was typed through, byte for byte, until the one byte that says a
chord is starting.

The bytes are not decoded into keys and encoded again: a terminal sends more
than any table of keys knows, such as mouse reports, pasted text and whatever
encoding it was built with, and what the pane reads should be what the terminal
sent. Once a chord has started they are read as keys, because that is what a
mode is written in, and the pane does not see them at all.

A mouse report this build has no name for is forwarded the same way. One it
does have a name for goes to the mode instead and is not passed on."
  (when (and (client-overlays client)
             (not (overlay-passes-keys-p (first (client-overlays client)))))
    (return-from client-handle-input (client-handle-key client said)))
  (let* ((said (client-pending-input client said))
         (out (make-array (length said) :element-type 'character :fill-pointer 0))
         (at 0)
         (n (length said)))
    (flet ((send ()
             (when (plusp (fill-pointer out))
               (wire-send (client-wire client)
                          (list :keys (coerce out 'simple-string)))
               (setf (fill-pointer out) 0))))
      (loop :while (< at n)
            :do (if (client-partial-chord client)
                    (multiple-value-bind (event took)
                        (tty:escape-sequence-to-key-event said at n nil)
                      (when (zerop took) (client-hold-input client said at n) (return))
                      (incf at took)
                      (when event (client-chord-event client event))
                      (when (client-overlays client) (return)))
                    (let ((ch (char said at)))
                      (cond
                        ((char= ch +prefix+)
                         (incf at)
                         (send) (client-chord client (event-key ch)))
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
                                  (client-chord client key))
                                (incf at took))
                               ;; Escape on its own, with something on top that
                               ;; lets keys through: the mode's, so it can close
                               ;; what is on top; a sequence is still the pane's
                               ((and (client-overlays client) (<= took 1))
                                (send)
                                (client-chord client (event-key ch))
                                (incf at))
                               (t (vector-push ch out) (incf at))))))
                        (t (incf at) (vector-push ch out))))))
      (send))))
