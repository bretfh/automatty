;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

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
           (going t :type boolean)
           (why nil))

(defun connect-to (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    (values (make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket)
            socket)))

(defun make-client (path &key (fd +stdin+) (to +stdout+) (takes (takes-of)))
  (multiple-value-bind (rows cols)
                       (if (a-terminal-p fd) (host-size fd) (values 24 80))
                       (multiple-value-bind (wire socket) (connect-to path)
                                            (let ((client (%make-client :wire wire :socket socket :fd fd :to to
                                                                        :takes takes :rows rows :cols cols
                                                                        :screen (make-screen :width cols :height rows)
                                                                        :waiting (make-waiting 4))))
                                              (wire-send wire (list :attach rows cols takes))
                                              (wire-flush wire)
                                              client))))

(defun client-close (client)
  "Shut it, once. The wire owns the socket, so closing the wire is the close --
doing it again here would shut a descriptor number that by then belongs to
whoever opened the next one."
  (setf (client-going client) nil)
  (free-waiting (client-waiting client))
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

(defgeneric press (thing key client)
  (:documentation "Give KEY to THING. What is on top gets it, and the pane does
not see it at all."))

(defun client-fit (client rows cols)
  (setf (client-screen client) (make-screen :width cols :height rows)
        (client-from client) (make-screen :width cols :height rows)
        (client-shown client) (make-screen :width cols :height rows)
        (client-dirty client) t))

(defun client-show (client)
  "Put the session, and then whatever is drawn on top of it, onto the terminal.

The client composes for itself rather than writing out the runs the server sent,
because what is on top is this one person's: a prompt somebody opened here is
not something everybody attached should be shown."
  (let ((work (client-screen client)))
    (when (and work (client-from client) (client-shown client))
      (screen-copy work (client-from client))
      (dolist (it (reverse (client-over client)))
        (draw-over it work))
      (let ((runs (screen-diff (client-shown client) work)))
        (when runs
          (host-say client
                    (with-output-to-string (s)
                      (encode-runs work runs s :takes (client-takes client)))))
        (host-say client
                  (with-output-to-string (s) (encode-cursor work s))))
      (setf (client-dirty client) nil))))

(defun client-over-put (client it)
  (push it (client-over client))
  (setf (client-dirty client) t)
  it)

(defun client-over-drop (client it)
  (setf (client-over client) (remove it (client-over client))
        (client-dirty client) t))

(defun done-with (client why)
  "Stop, for the first reason there was. What came after it is what stopping
looks like, not why it happened."
  (setf (client-going client) nil)
  (unless (client-why client)
    (setf (client-why client) why)))

(defun client-heard (client form)
  (case (first form)
        (:hello
         (destructuring-bind (name rows cols) (rest form)
                             (declare (ignore name))
                             (client-fit client rows cols)
                             (host-say client +blanked+)))
        (:frame
         (destructuring-bind (said faces) (rest form)
                             (said-into-screen (client-from client) said faces)
                             (setf (client-dirty client) t)))
        (:cursor
         (destructuring-bind (y x visible style) (rest form)
                             (declare (ignore style))
                             (let ((screen (client-from client)))
                               (setf (screen-cursor-y screen) y
                                     (screen-cursor-x screen) x
                                     (screen-cursor-visible screen) (and visible t)))
                             (setf (client-dirty client) t)))
        (:bell (host-say client (string (code-char 7))))
        (:bye (done-with client (second form)))
        (t nil)))

(defun client-redraw (client)
  (when (client-shown client)
    (setf (client-shown client)
          (make-screen :width (screen-width (client-shown client))
                       :height (screen-height (client-shown client)))))
  (setf (client-dirty client) t)
  (host-say client +blanked+)
  (wire-send (client-wire client)
             (list :resize (client-rows client) (client-cols client))))

(defun client-pressed (client said)
  "Bytes, as keys, to whatever is on top."
  (let ((at 0)
        (n (length said)))
    (loop :while (< at n)
          :do (multiple-value-bind (key took)
                  (vt:escape-sequence-to-key-event said at n nil)
                (when (zerop took) (return))
                (incf at took)
                (let ((it (first (client-over client))))
                  (if it
                      (press it key client)
                      (return)))))))

(defun client-chord (client key)
  "Give KEY to the mode. Answers whether the chord wants more keys.

Half a chord is this client's, not the image's: two of them attached in one
process would otherwise be finishing each other's."
  (let ((*client* client)
        (vt/mode:*pending* (client-chord-so-far client)))
    (prog1 (eq :pending (vt/mode:press (vt/mode:spelled key)
                                       (vt/mode:mode-named 'pane-mode)))
      (setf (client-chord-so-far client) vt/mode:*pending*))))

(defun client-typed (client said)
  "Pass what was typed through, byte for byte, until the one byte that says a
chord is starting.

The bytes are not decoded into keys and encoded again: a terminal sends more
than any table of keys knows -- mouse reports, pasted text, whatever encoding it
was built with -- and what the pane reads should be what the terminal sent. Once
a chord has started they are read as keys, because that is what a mode is
written in, and the pane does not see them at all."
  (when (client-over client)
    (return-from client-typed (client-pressed client said)))
  (let ((out (make-array (length said) :element-type 'character :fill-pointer 0))
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
                        (vt:escape-sequence-to-key-event said at n nil)
                      (when (zerop took) (return))
                      (incf at took)
                      (client-chord client (key-of event))
                      (when (client-over client) (return)))
                    (let ((ch (char said at)))
                      (incf at)
                      (if (char= ch +prefix+)
                          (progn (send) (client-chord client (key-of ch)))
                          (vector-push ch out)))))
      (send))))

(defun client-resized (client)
  (setf *resized* nil)
  (when (a-terminal-p (client-fd client))
    (multiple-value-bind (rows cols) (host-size (client-fd client))
                         (unless (and (= rows (client-rows client)) (= cols (client-cols client)))
                           (setf (client-rows client) rows
                                 (client-cols client) cols)
                           (wire-send (client-wire client) (list :resize rows cols))))))

(defun client-step (client &optional (patience 100))
  "One turn of the loop. What the server said is taken in before what was typed:
a bye says why everything is stopping, and a terminal that fell over at the same
moment would otherwise answer that question first, and answer it wrongly."
  (let* ((w (waiting-clear (client-waiting client)))
         (keys (waiting-add w (client-fd client)))
         (wire (client-wire client))
         (sock (waiting-add w (wire-fd wire)
                            (logior sb-unix:pollin
                                    (if (plusp (wire-pending wire))
                                        sb-unix:pollout
                                      0)))))
    (wait-on w patience)
    (when *resized* (client-resized client))
    (when (writable-p (waiting-back w sock))
      (wire-flush wire))
    (when (readable-p (waiting-back w sock))
      (if (null (wire-fill wire))
          (done-with client :server-gone)
        (loop for form = (wire-take wire)
              while form
              do (client-heard client form))))
    (when (readable-p (waiting-back w keys))
      (let ((said (pty:pty-read-string (client-fd client) 8192)))
        (if (null said)
            (done-with client :input-gone)
          (client-typed client said))))
    (when (client-dirty client) (client-show client))
    (wire-flush wire)
    client))

(defun attach (path &key (fd +stdin+) (to +stdout+) (takes (takes-of)))
  (let ((client (make-client path :fd fd :to to :takes takes)))
    (unwind-protect
        (with-host (fd :to to)
                   (loop while (and (client-going client)
                                    (not *asked-to-stop*)
                                    (wire-open (client-wire client)))
                         do (client-step client))
                   (when *asked-to-stop* (done-with client :asked-to-stop))
                   (client-why client))
      (ignore-errors (wire-send (client-wire client) '(:detach))
                     (wire-flush (client-wire client)))
      (client-close client))))
