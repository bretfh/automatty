;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/tty)

;;; poll, in the shape src/pty/pty.lisp already uses: libc, declared here, with
;;; the one number that is not the same on every unix in a table of its own.
;;; struct pollfd is int, short, short on linux, on darwin and on every bsd, and
;;; POLLIN through POLLNVAL are the same values on all of them, which is why
;;; sb-unix names them everywhere and the rest of the tree goes on using those.

(sb-alien:define-alien-type nil
  (sb-alien:struct pollfd
                   (fd sb-alien:int)
                   (events sb-alien:short)
                   (revents sb-alien:short)))

#-(or linux darwin freebsd openbsd netbsd)
(error "atty/tty has no nfds_t for this system. It is poll's second argument
in poll.h, and it is the only thing here that is not the same on every unix.")

(sb-alien:define-alien-type nfds-t
  #+linux sb-alien:unsigned-long
  #+(or darwin freebsd openbsd netbsd) sb-alien:unsigned-int)

(sb-alien:define-alien-routine ("poll" %poll) sb-alien:int
  (fds (* (sb-alien:struct pollfd)))
  (count nfds-t)
  (milliseconds sb-alien:int))

(defstruct (waiting (:constructor %make-waiting))
  (fds nil)
  (room 0 :type fixnum)
  (count 0 :type fixnum))

(defun make-waiting (&optional (room 8))
  (%make-waiting :fds (sb-alien:make-alien (sb-alien:struct pollfd) room)
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
        (new (sb-alien:make-alien (sb-alien:struct pollfd) room)))
    (dotimes (i (waiting-count w))
      (let ((from (sb-alien:deref old i))
            (to (sb-alien:deref new i)))
        (setf (sb-alien:slot to 'fd) (sb-alien:slot from 'fd)
              (sb-alien:slot to 'events) (sb-alien:slot from 'events)
              (sb-alien:slot to 'revents) (sb-alien:slot from 'revents))))
    (when old (sb-alien:free-alien old))
    (setf (waiting-fds w) new
          (waiting-room w) room))
  w)

(defun waiting-add (w fd &optional (events sb-unix:pollin))
  (when (>= (waiting-count w) (waiting-room w))
    (waiting-grow w (max 8 (* 2 (waiting-room w)))))
  (let ((slot (sb-alien:deref (waiting-fds w) (waiting-count w))))
    (setf (sb-alien:slot slot 'fd) fd
          (sb-alien:slot slot 'events) events
          (sb-alien:slot slot 'revents) 0))
  (prog1 (waiting-count w)
    (incf (waiting-count w))))

(defun waiting-back (w n)
  (sb-alien:slot (sb-alien:deref (waiting-fds w) n) 'revents))

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
  (let ((n (%poll (sb-alien:addr (sb-alien:deref (waiting-fds w) 0))
                  (waiting-count w)
                  milliseconds)))
    (if (minusp n)
        (let ((errno (sb-alien:get-errno)))
          (unless (eql errno sb-unix:eintr)
            (error "poll: ~A" (sb-int:strerror errno))))
        n)))
