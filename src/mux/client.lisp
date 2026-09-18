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
  (takes t)
  (rows 24 :type fixnum)
  (cols 80 :type fixnum)
  (waiting nil)
  (waiting-for-command nil :type boolean)
  (going t :type boolean)
  (why nil))

(defun connect-to (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    (values (make-wire (sb-bsd-sockets:socket-file-descriptor socket)) socket)))

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
  (setf (client-going client) nil)
  (free-waiting (client-waiting client))
  (wire-close (client-wire client))
  (when (and (client-socket client)
             (sb-bsd-sockets:socket-open-p (client-socket client)))
    (ignore-errors (sb-bsd-sockets:socket-close (client-socket client)))))

(defun client-draw (client said faces)
  (let* ((screen (client-screen client))
         (runs (said-into-screen screen said faces)))
    (with-output-to-string (s)
      (encode-runs screen runs s :takes (client-takes client)))))

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
       (setf (client-screen client) (make-screen :width cols :height rows))
       (pty:pty-write-string (client-to client)
                             (format nil "~C[2J" #\Escape))))
    (:frame
     (destructuring-bind (said faces) (rest form)
       (pty:pty-write-string (client-to client) (client-draw client said faces))))
    (:cursor
     (destructuring-bind (y x visible style) (rest form)
       (declare (ignore style))
       (let ((screen (client-screen client)))
         (setf (screen-cursor-y screen) y
               (screen-cursor-x screen) x
               (screen-cursor-visible screen) (and visible t)))
       (pty:pty-write-string
        (client-to client)
        (with-output-to-string (s) (encode-cursor (client-screen client) s)))))
    (:bell (pty:pty-write-string (client-to client) (string (code-char 7))))
    (:bye (done-with client (second form)))
    (t nil)))

(defun client-redraw (client)
  (setf (client-screen client)
        (make-screen :width (screen-width (client-screen client))
                     :height (screen-height (client-screen client))))
  (pty:pty-write-string (client-to client) (format nil "~C[2J" #\Escape))
  (wire-send (client-wire client)
             (list :resize (client-rows client) (client-cols client))))

(defun client-typed (client said)
  "Pass what was typed through, byte for byte, except the one byte that says the
next one is a command.

The bytes are not decoded into keys and encoded again: a terminal sends more
than any table of keys knows -- mouse reports, pasted text, whatever encoding it
was built with -- and what the pane reads should be what the terminal sent."
  (let ((out (make-array (length said) :element-type 'character
                                       :fill-pointer 0)))
    (loop for ch across said
          do (cond
               ((client-waiting-for-command client)
                (setf (client-waiting-for-command client) nil)
                (cond
                  ((char= ch +prefix+) (vector-push ch out))
                  ((char-equal ch #\d) (done-with client :detached))
                  ((char-equal ch #\r) (client-redraw client))
                  (t nil)))
               ((char= ch +prefix+)
                (setf (client-waiting-for-command client) t))
               (t (vector-push ch out))))
    (when (plusp (length out))
      (wire-send (client-wire client) (list :keys (coerce out 'simple-string))))))

(defun client-resized (client)
  (setf *resized* nil)
  (when (a-terminal-p (client-fd client))
    (multiple-value-bind (rows cols) (host-size (client-fd client))
      (unless (and (= rows (client-rows client)) (= cols (client-cols client)))
        (setf (client-rows client) rows
              (client-cols client) cols)
        (wire-send (client-wire client) (list :resize rows cols))))))

(defun client-step (client &optional (patience 100))
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
    (when (readable-p (waiting-back w keys))
      (let ((said (pty:pty-read-string (client-fd client) 8192)))
        (if (null said)
            (done-with client :input-gone)
            (client-typed client said))))
    (when (writable-p (waiting-back w sock))
      (wire-flush wire))
    (when (readable-p (waiting-back w sock))
      (if (null (wire-fill wire))
          (done-with client :server-gone)
          (loop for form = (wire-take wire)
                while form
                do (client-heard client form))))
    (wire-flush wire)
    client))

(defun attach (path &key (fd +stdin+) (to +stdout+) (takes (takes-of)))
  (let ((client (make-client path :fd fd :to to :takes takes)))
    (unwind-protect
         (with-host (to)
           (loop while (and (client-going client) (wire-open (client-wire client)))
                 do (client-step client))
           (client-why client))
      (ignore-errors (wire-send (client-wire client) '(:detach))
                     (wire-flush (client-wire client)))
      (client-close client))))
