;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defparameter +was-known+
  '(:attach :keys :resize :knock :bar :detach :stop)
  "What every server has always known how to do.

A server that says nothing about what it can do is one from before it said, and
these are what it had then. A client assuming more of it would send keys into
silence.")

(defparameter +prefix+ (code-char 2)
              "What says the next byte is for the multiplexer rather than for the pane.")

(defstruct (client (:constructor %make-client))
           (wire nil)
           (socket nil)
           (fd 0 :type fixnum)
           (to 1 :type fixnum)
           (screen nil)
           (from nil)
           (shown nil)
           (over nil)
           (dirty t :type boolean)
           (takes t)
           (rows 24 :type fixnum)
           (cols 80 :type fixnum)
           (waiting nil)
           (chord-so-far nil :type list)
           (partial "" :type string)
           (greeted nil :type boolean)
           (knows +was-known+)
           (mode 'pane-mode)
           (going t :type boolean)
           (why nil))

(defun connect-to (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    (values (make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket)
            socket)))

(declaim (ftype function ask ask-a-name))

(defun tty-name (fd)
  "What the terminal on FD is called, such as /dev/ttys004, or nil. It is how a
server tells one person's keys from another's in a pane's log."
  (and (tty:a-terminal-p fd)
       (ignore-errors
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "ttyname" (function sb-alien:c-string sb-alien:int))
         fd))))

(defun terminal-size (fd)
  "How big the terminal on FD is. One that says it has no rows or no columns,
as a terminal made by a program that never set one does, is taken to be the
size a terminal is when nobody said: a pane one column wide is no use to
anybody."
  (multiple-value-bind (rows cols)
      (if (tty:a-terminal-p fd) (tty:host-size fd) (values 24 80))
    (if (or (zerop rows) (zerop cols))
        (values 24 80)
        (values rows cols))))

(defun make-client (path &key name open (fd tty:+stdin+) (to tty:+stdout+)
                              (takes (tty:takes-of)))
  "A client on the server at PATH, joined to the session called NAME, or the
first one when NAME is nil. OPEN is (command directory): the session is made
with them when it is not there."
  (multiple-value-bind (rows cols) (terminal-size fd)
                       (multiple-value-bind (wire socket) (connect-to path)
                                            (let ((client (%make-client :wire wire :socket socket :fd fd :to to
                                                                        :takes takes :rows rows :cols cols
                                                                        :screen (tty:make-screen :width cols :height rows)
                                                                        :waiting (tty:make-waiting 4))))
                                              ;; the name goes first and on its
                                              ;; own. A server that does not know
                                              ;; about names passes it over and
                                              ;; the attach it does know is the
                                              ;; shape it has always been
                                              (wire-send wire (list :who (tty-name fd)))
                                              (if open
                                                  (wire-send wire (list* :open name
                                                                         (append open
                                                                                 (list rows cols takes))))
                                                  (progn
                                                    (when name
                                                      (wire-send wire (list :want name)))
                                                    (wire-send wire (list :attach rows cols takes))))
                                              (wire-flush wire)
                                              client))))

(defun client-close (client)
  "Shut it, once. The wire owns the socket, so closing the wire is the close;
doing it again here would shut a descriptor number that by then belongs to
whoever opened the next one."
  (setf (client-going client) nil)
  (tty:free-waiting (client-waiting client))
  (wire-close (client-wire client)))

(defun host-say (client said)
  "Write to the terminal the client is sitting in. What the screen holds is
characters, not bytes, so this is the one place that says how they are spelt.

A terminal that will not take what it is given has gone, which is a reason to
stop rather than a fault: the panes are still running and somebody can attach to
them again."
  (handler-case (pty:pty-write-string (client-to client) said :utf-8)
    (error () (done-with client :terminal-gone) 0)))

(defgeneric draw-over (thing screen)
  (:documentation "Draw THING onto SCREEN, over whatever the session put there.
This is the seam: anything that can write cells can be put on top, and nothing
else needs to know about it."))

(defgeneric mode-of (thing)
  (:documentation "Which mode a client is in while THING is on top.")
  (:method (thing) (declare (ignore thing)) 'pane-mode))

(defun client-fit (client rows cols)
  (setf (client-screen client) (tty:make-screen :width cols :height rows)
        (client-from client) (tty:make-screen :width cols :height rows)
        (client-shown client) (tty:make-screen :width cols :height rows)
        (client-dirty client) t))

(defun client-show (client)
  "Put the session, and then whatever is drawn on top of it, onto the terminal.

The client composes for itself rather than writing out the runs the server sent,
because what is on top is this one person's: a prompt somebody opened here is
not something everybody attached should be shown."
  (let ((work (client-screen client)))
    (when (and work (client-from client) (client-shown client))
      (tty:screen-copy work (client-from client))
      (dolist (it (reverse (client-over client)))
        (draw-over it work))
      (let ((runs (tty:screen-diff (client-shown client) work)))
        (host-say client
                  (with-output-to-string (s)
                    (tty:encode-frame work runs s :takes (client-takes client)))))
      (setf (client-dirty client) nil))))

(defun client-in-mode (client)
  "The mode whatever is on top asks for, or the pane's when nothing is."
  (setf (client-mode client) (mode-of (first (client-over client)))
        (client-chord-so-far client) nil))

(defun client-over-put (client it)
  (push it (client-over client))
  (client-in-mode client)
  (setf (client-dirty client) t)
  it)

(defun client-over-drop (client it)
  (setf (client-over client) (remove it (client-over client)))
  (client-in-mode client)
  (setf (client-dirty client) t))

(defun done-with (client why)
  "Stop, for the first reason there was. What came after it is what stopping
looks like, not why it happened."
  (setf (client-going client) nil)
  (unless (client-why client)
    (setf (client-why client) why)))

(defun client-heard (client form)
  (case (first form)
        (:hello
         (destructuring-bind (name rows cols &optional knows) (rest form)
                             (declare (ignore name))
                             (setf (client-greeted client) t
                                   (client-knows client) (or knows +was-known+))
                             (client-fit client rows cols)
                             (host-say client tty:+blanked+)))
        (:frame
         (destructuring-bind (said faces) (rest form)
                             (said-into-screen (client-from client) said faces)
                             (setf (client-dirty client) t)))
        (:cursor
         (destructuring-bind (y x visible style) (rest form)
                             (let ((screen (client-from client)))
                               (setf (tty:screen-cursor-y screen) y
                                     (tty:screen-cursor-x screen) x
                                     (tty:screen-cursor-visible screen) (and visible t)
                                     (tty:screen-cursor-style screen) style))
                             (setf (client-dirty client) t)))
        (:these
         (ask client "session"
              (mapcar (lambda (row)
                        (destructuring-bind (name rows cols panes watching
                                             &optional (blocked 0))
                            row
                          (format nil "~A  ~Dx~D  ~D pane~:P  ~D watching~A"
                                  name cols rows panes watching
                                  (if (plusp blocked)
                                      (format nil "  ▲ ~D blocked" blocked)
                                      ""))))
                      (second form))
              :kind #\@
              :chose (lambda (said c)
                       (wire-send (client-wire c)
                                  (list :go (subseq said 0 (position #\Space said)))))))
        (:bell (host-say client (string (code-char 7))))
        (:do (run-command (second form) client))
        (:name-it
         (destructuring-bind (session id label title) (rest form)
           (ask-a-name client session id label title)))
        (:bye (done-with client (second form)))
        (t nil)))

(defun client-redraw (client)
  (when (client-shown client)
    (setf (client-shown client)
          (tty:make-screen :width (tty:screen-width (client-shown client))
                       :height (tty:screen-height (client-shown client)))))
  (setf (client-dirty client) t)
  (host-say client tty:+blanked+)
  (wire-send (client-wire client)
             (list :resize (client-rows client) (client-cols client))))

(defparameter +half-said+ 64
  "How much of an unfinished sequence to keep for the next read. More than this
is not somebody typing a key.")

(defun client-holding (client said)
  "SAID with whatever was left half-said at the end of the last read in front of
it."
  (if (plusp (length (client-partial client)))
      (prog1 (concatenate 'string (client-partial client) said)
        (setf (client-partial client) ""))
      said))

(defun client-hold (client said at n)
  (setf (client-partial client)
        (if (> (- n at) +half-said+) "" (subseq said at n))))

(defun client-pressed (client said)
  "Bytes, as keys, to the mode whatever is on top put the client in.

A sequence the terminal sent that is no key a mode knows, a mouse report say, is
passed over rather than made into one, and one the rest of has not arrived yet is
kept until it has."
  (let* ((said (client-holding client said))
         (at 0)
         (n (length said)))
    (loop :while (< at n)
          :do (multiple-value-bind (event took)
                  (tty:escape-sequence-to-key-event said at n nil)
                (when (zerop took) (client-hold client said at n) (return))
                (incf at took)
                (when event (client-chord client (key-of event)))))))

(defgeneric unbound (thing chord client)
  (:documentation "What to do with a key the mode has no binding for. A prompt
puts it in what has been typed; most things ignore it.")
  (:method (thing chord client) (declare (ignore thing chord client)) nil))

(defun client-chord (client key)
  "Give KEY to the mode this client is in. Answers whether the chord wants more
keys.

Which mode, and half a chord, are both this client's rather than the image's:
two of them attached in one process would otherwise be in each other's modes and
finishing each other's chords."
  (let* ((*client* client)
         (over (first (client-over client)))
         (atty/mode:*pending* (client-chord-so-far client))
         (atty/mode:*unbound* (lambda (chord) (unbound over chord client))))
    (prog1 (eq :pending (atty/mode:press (atty/mode:spelled key)
                                       (atty/mode:mode-named (client-mode client))))
      (setf (client-chord-so-far client) atty/mode:*pending*)
      (when over (setf (client-dirty client) t)))))

(defun client-typed (client said)
  "Pass what was typed through, byte for byte, until the one byte that says a
chord is starting.

The bytes are not decoded into keys and encoded again: a terminal sends more
than any table of keys knows, such as mouse reports, pasted text and whatever
encoding it was built with, and what the pane reads should be what the terminal
sent. Once a chord has started they are read as keys, because that is what a
mode is written in, and the pane does not see them at all.

A mouse report this build has no name for is forwarded the same way. One it
does have a name for goes to the mode instead and is not passed on."
  (when (client-over client)
    (return-from client-typed (client-pressed client said)))
  (let* ((said (client-holding client said))
         (out (make-array (length said) :element-type 'character :fill-pointer 0))
         (at 0)
         (n (length said)))
    (flet ((send ()
             (when (plusp (fill-pointer out))
               (wire-send (client-wire client)
                          (list :keys (coerce out 'simple-string)))
               (setf (fill-pointer out) 0))))
      (loop :while (< at n)
            :do (if (client-chord-so-far client)
                    (multiple-value-bind (event took)
                        (tty:escape-sequence-to-key-event said at n nil)
                      (when (zerop took) (client-hold client said at n) (return))
                      (incf at took)
                      (when event (client-chord client (key-of event)))
                      (when (client-over client) (return)))
                    (let ((ch (char said at)))
                      (cond
                        ((char= ch +prefix+)
                         (incf at)
                         (send) (client-chord client (key-of ch)))
                        ((char= ch #\Escape)
                         (multiple-value-bind (event took)
                             (tty:escape-sequence-to-key-event said at n nil)
                           (let ((key (and (plusp took) (consp event)
                                           (eq (first event) :mouse)
                                           (mouse-key-of event))))
                             (if key
                                 (progn
                                   (send)
                                   (let ((*mouse-at* (cons (getf (rest event) :x)
                                                            (getf (rest event) :y))))
                                     (client-chord client key))
                                   (incf at took))
                                 (progn (vector-push ch out) (incf at))))))
                        (t (incf at) (vector-push ch out))))))
      (send))))

(defun client-resized (client)
  (setf tty:*resized* nil)
  (when (tty:a-terminal-p (client-fd client))
    (multiple-value-bind (rows cols) (terminal-size (client-fd client))
                         (unless (and (= rows (client-rows client)) (= cols (client-cols client)))
                           (setf (client-rows client) rows
                                 (client-cols client) cols)
                           (wire-send (client-wire client) (list :resize rows cols))))))

(defun client-step (client &optional (patience 100))
  "One turn of the loop. What the server said is taken in before what was typed:
a bye says why everything is stopping, and a terminal that fell over at the same
moment would otherwise answer that question first, and answer it wrongly."
  (let* ((w (tty:waiting-clear (client-waiting client)))
         (keys (tty:waiting-add w (client-fd client)))
         (wire (client-wire client))
         (sock (tty:waiting-add w (wire-fd wire)
                            (logior sb-unix:pollin
                                    (if (plusp (wire-pending wire))
                                        sb-unix:pollout
                                      0)))))
    (tty:wait-on w patience)
    (when tty:*resized* (client-resized client))
    (when (tty:writable-p (tty:waiting-back w sock))
      (wire-flush wire))
    (when (tty:readable-p (tty:waiting-back w sock))
      (if (null (wire-fill wire))
          (done-with client :server-gone)
        (loop for form = (wire-take wire)
              while form
              do (client-heard client form))))
    (when (tty:readable-p (tty:waiting-back w keys))
      (let ((said (pty:pty-read-string (client-fd client) 8192)))
        (if (null said)
            (done-with client :input-gone)
          (client-typed client said))))
    (when (client-dirty client) (client-show client))
    (wire-flush wire)
    client))

(defparameter +patience+ 5
  "How many seconds to wait for the server to say what we are looking at.

A server that takes the connection and then says nothing leaves a terminal in
raw mode showing nothing at all, which says neither that anything is wrong nor
which end of it is wrong.")

(defun answered-p (client since)
  (or (client-greeted client)
      (< (- (get-internal-real-time) since)
         (* +patience+ internal-time-units-per-second))
      (progn (done-with client :no-answer) nil)))

(defun attach (path &key name open (fd tty:+stdin+) (to tty:+stdout+)
                         (takes (tty:takes-of)))
  (let ((client (make-client path :name name :open open :fd fd :to to :takes takes))
        (since (get-internal-real-time)))
    (unwind-protect
        (tty:with-host (fd :to to)
                   (loop while (and (client-going client)
                                    (not tty:*asked-to-stop*)
                                    (answered-p client since)
                                    (wire-open (client-wire client)))
                         do (client-step client))
                   (when tty:*asked-to-stop* (done-with client :asked-to-stop))
                   (client-why client))
      (ignore-errors (wire-send (client-wire client) '(:detach))
                     (wire-flush (client-wire client)))
      (client-close client))))
