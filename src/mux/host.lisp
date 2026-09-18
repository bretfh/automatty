;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

;;; What a system calls a thing, in the shape src/pty/pty.lisp already uses:
;;; one table, frozen ABI, and a system nobody has checked stops at load rather
;;; than running on a guess. Everything else the host terminal needs -- every
;;; termios flag -- sb-posix groveled on the machine it was built on.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bsd-ioctl-read (group number length)
    (logior #x40000000
            (ash (logand length #x1fff) 16)
            (ash (char-code group) 8)
            number)))

(defconstant +tiocgwinsz+
  #+linux #x5413
  #+(or darwin freebsd openbsd netbsd) (bsd-ioctl-read #\t 104 8)
  #-(or linux darwin freebsd openbsd netbsd)
  (error "vt/mux has no TIOCGWINSZ for this system."))

(defconstant +stdin+ 0)
(defconstant +stdout+ 1)

(defun a-terminal-p (fd)
  (plusp (sb-unix:unix-isatty fd)))

(defun host-size (fd)
  "How many rows and columns the terminal on FD has."
  (sb-alien:with-alien ((size (sb-alien:array sb-alien:unsigned-short 4)))
    (let ((rc (sb-unix:unix-ioctl fd +tiocgwinsz+ (sb-alien:alien-sap size))))
      (when (or (null rc) (and (realp rc) (minusp rc)))
        (error "TIOCGWINSZ: ~A" (sb-int:strerror (sb-alien:get-errno)))))
    (values (sb-alien:deref size 0) (sb-alien:deref size 1))))

(defun host-raw (fd)
  "Put the terminal on FD into raw mode and answer what it was, for putting back.

Every byte the user types has to reach the pane unchanged: no line editing, no
echo, no signal characters, no translation in either direction."
  (let ((was (sb-posix:tcgetattr fd))
        (now (sb-posix:tcgetattr fd)))
    (setf (sb-posix:termios-iflag now)
          (logandc2 (sb-posix:termios-iflag now)
                    (logior sb-posix:ignbrk sb-posix:brkint sb-posix:parmrk
                            sb-posix:istrip sb-posix:inlcr sb-posix:igncr
                            sb-posix:icrnl sb-posix:ixon)))
    (setf (sb-posix:termios-oflag now)
          (logandc2 (sb-posix:termios-oflag now) sb-posix:opost))
    (setf (sb-posix:termios-lflag now)
          (logandc2 (sb-posix:termios-lflag now)
                    (logior sb-posix:echo sb-posix:echonl sb-posix:icanon
                            sb-posix:isig sb-posix:iexten)))
    (setf (sb-posix:termios-cflag now)
          (logior (logandc2 (sb-posix:termios-cflag now)
                            (logior sb-posix:csize sb-posix:parenb))
                  sb-posix:cs8))
    (let ((cc (sb-posix:termios-cc now)))
      (setf (aref cc sb-posix:vmin) 1
            (aref cc sb-posix:vtime) 0)
      (setf (sb-posix:termios-cc now) cc))
    (sb-posix:tcsetattr fd sb-posix:tcsaflush now)
    was))

(defun host-put-back (fd was)
  (when was
    (sb-posix:tcsetattr fd sb-posix:tcsaflush was)))

(defvar *resized* nil
  "Set by the SIGWINCH handler and read by the loop. A handler does this and
nothing else: it runs between any two instructions there are.")

(defun hear-resizes ()
  (setf *resized* t)
  (sb-sys:enable-interrupt sb-unix:sigwinch
                           (lambda (signal info context)
                             (declare (ignore signal info context))
                             (setf *resized* t))))

(defun stop-hearing-resizes ()
  (sb-sys:enable-interrupt sb-unix:sigwinch :default))

(defparameter +took-over+
  (format nil "~C[?1049h~C[?7l~C[?25l~C[2J" #\Escape #\Escape #\Escape #\Escape)
  "Alternate screen, no autowrap, no cursor, and a clean one to draw on.

Autowrap stays off for the whole session: writing the bottom right cell of a
terminal that has it on scrolls the screen out from under everything.")

(defparameter +gave-back+
  (format nil "~C[0m~C[?25h~C[?7h~C[?1049l" #\Escape #\Escape #\Escape #\Escape))

(defmacro with-host ((fd &key (raw t)) &body body)
  (let ((was (gensym "WAS")) (f (gensym "FD")))
    `(let* ((,f ,fd)
            (,was (when ,raw (host-raw ,f))))
       (unwind-protect
            (progn (pty:pty-write-string ,f +took-over+)
                   (hear-resizes)
                   ,@body)
         (stop-hearing-resizes)
         (ignore-errors (pty:pty-write-string ,f +gave-back+))
         (host-put-back ,f ,was)))))
