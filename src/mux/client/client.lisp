;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defstruct (client (:constructor %make-client))
           (wire nil)
           (socket nil)
           (fd 0 :type fixnum)
           (to 1 :type fixnum)
           (from nil)
           (shown nil)
           (dirty t :type boolean)
           (takes t)
           (rows 24 :type fixnum)
           (cols 80 :type fixnum)
           (waiting nil)
           (greeted nil :type boolean)
           (running t :type boolean)
           (exit-reason nil))

(defun connect-to (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    (values (make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket)
            socket)))

(defun tty-name (fd)
  "What the terminal on FD is called, such as /dev/ttys004, or nil. It is how a
server tells one person's keys from another's in a pane's log."
  (and (tty:terminal-p fd)
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
      (if (tty:terminal-p fd) (tty:host-size fd) (values 24 80))
    (if (or (zerop rows) (zerop cols))
        (values 24 80)
        (values rows cols))))

(defun make-client (path &key name open (fd tty:+stdin+) (to tty:+stdout+)
                              (takes (tty:takes-of)))
  "A client on the server at PATH, joined to the session called NAME, or the
first one when NAME is nil. OPEN is (command directory [label]): the session is
made with them when it is not there, its first pane called LABEL."
  (multiple-value-bind (rows cols) (terminal-size fd)
                       (multiple-value-bind (wire socket) (connect-to path)
                                            (let ((client (%make-client :wire wire :socket socket :fd fd :to to
                                                                        :takes takes :rows rows :cols cols
                                                                        :waiting (tty:make-waiting 4))))
                                              ;; the name goes first and on its
                                              ;; own. A server that does not know
                                              ;; about names passes it over and
                                              ;; the attach it does know is the
                                              ;; shape it has always been
                                              (wire-send wire (list :who (tty-name fd)))
                                              (if open
                                                  (destructuring-bind (command directory &optional label)
                                                      open
                                                    (wire-send wire (list :open name command directory
                                                                          rows cols takes label)))
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
  (setf (client-running client) nil)
  (tty:free-waiting (client-waiting client))
  (wire-close (client-wire client)))

(defun host-write (client said)
  "Write to the terminal the client is sitting in. What the screen holds is
characters, not bytes, so this is the one place that says how they are spelt.

A terminal that will not take what it is given has gone, which is a reason to
stop rather than a fault: the panes are still running and somebody can attach to
them again."
  (handler-case (pty:pty-write-string (client-to client) said :utf-8)
    (error () (client-stop client :terminal-gone) 0)))

(defun client-fit (client rows cols)
  (setf (client-from client) (tty:make-screen :width cols :height rows)
        (client-shown client) (tty:make-screen :width cols :height rows)
        (client-dirty client) t))

(defun client-draw (client)
  "Put what the server sent onto the terminal: only what differs from what is
there."
  (let ((from (client-from client))
        (shown (client-shown client)))
    (when (and from shown)
      (let ((runs (tty:screen-diff shown from)))
        (host-write client
                    (with-output-to-string (s)
                      (tty:encode-frame from runs s :takes (client-takes client)))))
      (setf (client-dirty client) nil))))

(defun client-stop (client why)
  "Stop, for the first reason there was. What came after it is what stopping
looks like, not why it happened."
  (setf (client-running client) nil)
  (unless (client-exit-reason client)
    (setf (client-exit-reason client) why)))

(defun base64 (text)
  "TEXT as base64, the way OSC 52 wants it."
  (let* ((bytes (sb-ext:string-to-octets text :external-format :utf-8))
         (table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (out (make-string-output-stream)))
    (loop :for i :from 0 :below (length bytes) :by 3
          :do (let* ((n (min 3 (- (length bytes) i)))
                     (b0 (aref bytes i))
                     (b1 (if (> n 1) (aref bytes (+ i 1)) 0))
                     (b2 (if (> n 2) (aref bytes (+ i 2)) 0))
                     (word (logior (ash b0 16) (ash b1 8) b2)))
                (write-char (char table (ldb (byte 6 18) word)) out)
                (write-char (char table (ldb (byte 6 12) word)) out)
                (write-char (if (> n 1) (char table (ldb (byte 6 6) word)) #\=) out)
                (write-char (if (> n 2) (char table (ldb (byte 6 0) word)) #\=) out)))
    (get-output-stream-string out)))

(defun client-resized (client)
  (setf tty:*resized* nil)
  (when (tty:terminal-p (client-fd client))
    (multiple-value-bind (rows cols) (terminal-size (client-fd client))
                         (unless (and (= rows (client-rows client)) (= cols (client-cols client)))
                           (setf (client-rows client) rows
                                 (client-cols client) cols)
                           (wire-send (client-wire client) (list :resize rows cols))))))

(defun client-step (client &optional (patience (if (client-greeted client) -1 100)))
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
                                      0))))
         (woken (tty:waiting-add w (or (tty:wake-fd) -1))))
    (tty:wait-on w patience)
    (when (tty:readable-p (tty:waiting-back w woken))
      (tty:drain-wake))
    (when tty:*resized* (client-resized client))
    (when (tty:writable-p (tty:waiting-back w sock))
      (wire-flush wire))
    (when (tty:readable-p (tty:waiting-back w sock))
      (if (null (wire-receive wire))
          (client-stop client :server-gone)
        (loop for form = (wire-read-message wire)
              while form
              do (handle-server-message client form))))
    (when (tty:readable-p (tty:waiting-back w keys))
      (let ((said (pty:pty-read-string (client-fd client) 8192)))
        (if (null said)
            (client-stop client :input-gone)
            (wire-send wire (list :keys said)))))
    (when (client-dirty client) (client-draw client))
    (wire-flush wire)
    client))

(defparameter +hello-timeout+ 5
  "How many seconds to wait for the server to say what we are looking at.

A server that takes the connection and then says nothing leaves a terminal in
raw mode showing nothing at all, which says neither that anything is wrong nor
which end of it is wrong.")

(defun client-greeted-p (client since)
  (or (client-greeted client)
      (< (- (get-internal-real-time) since)
         (* +hello-timeout+ internal-time-units-per-second))
      (progn (client-stop client :no-answer) nil)))

(defun attach (path &key name open (fd tty:+stdin+) (to tty:+stdout+)
                         (takes (tty:takes-of)))
  (let ((client (make-client path :name name :open open :fd fd :to to :takes takes))
        (since (get-internal-real-time)))
    (unwind-protect
        (tty:with-host (fd :to to)
                   (loop while (and (client-running client)
                                    (not tty:*asked-to-stop*)
                                    (client-greeted-p client since)
                                    (wire-open (client-wire client)))
                         do (client-step client))
                   (when tty:*asked-to-stop* (client-stop client :asked-to-stop))
                   (client-exit-reason client))
      (ignore-errors (wire-send (client-wire client) '(:detach))
                     (wire-flush (client-wire client)))
      (client-close client))))
