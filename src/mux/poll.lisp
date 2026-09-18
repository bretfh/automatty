;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

(defstruct (waiting (:constructor %make-waiting))
  (fds nil)
  (room 0 :type fixnum)
  (count 0 :type fixnum))

(defun make-waiting (&optional (room 8))
  (%make-waiting :fds (sb-alien:make-alien (sb-alien:struct sb-unix:pollfd) room)
                 :room room))

(defun free-waiting (w)
  (when (waiting-fds w)
    (sb-alien:free-alien (waiting-fds w))
    (setf (waiting-fds w) nil
          (waiting-room w) 0
          (waiting-count w) 0)))

(defun waiting-clear (w)
  (setf (waiting-count w) 0)
  w)

(defun waiting-grow (w room)
  (let ((old (waiting-fds w))
        (new (sb-alien:make-alien (sb-alien:struct sb-unix:pollfd) room)))
    (dotimes (i (waiting-count w))
      (let ((from (sb-alien:deref old i))
            (to (sb-alien:deref new i)))
        (setf (sb-alien:slot to 'sb-unix:fd) (sb-alien:slot from 'sb-unix:fd)
              (sb-alien:slot to 'sb-unix:events) (sb-alien:slot from 'sb-unix:events)
              (sb-alien:slot to 'sb-unix:revents) (sb-alien:slot from 'sb-unix:revents))))
    (when old (sb-alien:free-alien old))
    (setf (waiting-fds w) new
          (waiting-room w) room))
  w)

(defun waiting-add (w fd &optional (events sb-unix:pollin))
  (when (>= (waiting-count w) (waiting-room w))
    (waiting-grow w (max 8 (* 2 (waiting-room w)))))
  (let ((slot (sb-alien:deref (waiting-fds w) (waiting-count w))))
    (setf (sb-alien:slot slot 'sb-unix:fd) fd
          (sb-alien:slot slot 'sb-unix:events) events
          (sb-alien:slot slot 'sb-unix:revents) 0))
  (prog1 (waiting-count w)
    (incf (waiting-count w))))

(defun waiting-back (w n)
  (sb-alien:slot (sb-alien:deref (waiting-fds w) n) 'sb-unix:revents))

(defun readable-p (back)
  (plusp (logand back (logior sb-unix:pollin sb-unix:pollhup sb-unix:pollerr))))

(defun writable-p (back)
  (plusp (logand back sb-unix:pollout)))

(defun gone-p (back)
  (plusp (logand back (logior sb-unix:pollhup sb-unix:pollerr sb-unix:pollnval))))

(defun wait-on (w milliseconds)
  "How many of W's descriptors came back with something, or nil when a signal
arrived first.

A signal reaching this process interrupts the wait and is not a fault: it is
how the terminal says it was resized, and the loop above has to see it."
  (when (zerop (waiting-count w))
    (return-from wait-on 0))
  (multiple-value-bind (n errno)
      (sb-unix:unix-poll (sb-alien:addr (sb-alien:deref (waiting-fds w) 0))
                         (waiting-count w)
                         milliseconds)
    (cond
      (n n)
      ((eql errno sb-unix:eintr) nil)
      (t (error "poll: ~A" (sb-int:strerror errno))))))
