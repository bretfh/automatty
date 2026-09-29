;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/tty)

;;; What a system calls a thing, in the shape src/pty/pty.lisp already uses:
;;; one table, frozen ABI, and a system nobody has checked stops at load rather
;;; than running on a guess. Everything else the host terminal needs, which is
;;; every termios flag, sb-posix groveled on the machine it was built on.

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
  (error "atty has no TIOCGWINSZ for this system."))

(defconstant +stdin+ 0)
(defconstant +stdout+ 1)

(defun terminal-p (fd)
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
  (open-wake)
  (sb-sys:enable-interrupt sb-unix:sigwinch
                           (lambda (signal info context)
                             (declare (ignore signal info context))
                             (setf *resized* t)
                             (wake))))

(defun stop-hearing-resizes ()
  (sb-sys:enable-interrupt sb-unix:sigwinch :default))

(defparameter +blanked+
  (format nil "~C[0m~C[2J~C[H" #\Escape #\Escape #\Escape)
  "Nothing on the screen, and nothing left in force.

The reset has to come first. An erase paints in whatever colour was last set, so
clearing while the bar's background is in force fills the terminal with it, and
then every cell that is genuinely blank matches what we think is there and is
never drawn over.")

(defparameter +took-over+
  (format nil "~C[?1049h~C[?7l~C[?25l~C[?1002h~C[?1006h~A"
          #\Escape #\Escape #\Escape #\Escape #\Escape +blanked+)
  "Alternate screen, no autowrap, no cursor, mouse reports on, a button held
and moved among them so a scrollbar can be dragged, and a clean one
to draw on.

Autowrap stays off for the whole session: writing the bottom right cell of a
terminal that has it on scrolls the screen out from under everything.")

(defparameter +gave-back+
  (format nil "~C[0m~C[?25h~C[?7h~C[?2004l~C[?1006l~C[?1002l~C[?1049l"
          #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape #\Escape)
  "Bracketed paste is turned off here too, whether or not it was ever turned on
for this attach: a pane's own request only lasts as long as it has the focus,
but if it were somehow left on, the next thing to run in this real terminal
must not inherit it.")

(defvar *asked-to-stop* nil)

(defvar *wake* nil)
(defvar *wake-octet* (make-array 1 :element-type '(unsigned-byte 8) :initial-element 1))

(declaim (ftype (function () (or null fixnum)) wake-fd))
(defun wake-fd ()
  "The descriptor that is readable once a signal has been heard, a request to
stop or a resize, so a wait with nothing else to wake it still ends."
  (car *wake*))

(declaim (ftype (function () t) drain-wake))
(defun drain-wake ()
  (when *wake*
    (let ((octets (make-array 64 :element-type '(unsigned-byte 8))))
      (sb-sys:with-pinned-objects (octets)
        (loop :while (let ((n (sb-unix:unix-read (car *wake*) (sb-sys:vector-sap octets) 64)))
                       (and n (plusp n))))))))

(declaim (ftype (function () t) wake))
(defun wake ()
  (when *wake*
    (sb-sys:with-pinned-objects (*wake-octet*)
      (sb-unix:unix-write (cdr *wake*) *wake-octet* 0 1))))

(declaim (ftype (function () cons) open-wake))
(defun open-wake ()
  (or *wake*
      (multiple-value-bind (in out) (sb-posix:pipe)
        (dolist (fd (list in out))
          (pty:nonblocking (pty:close-on-exec fd)))
        (setf *wake* (cons in out)))))

(defun hear-the-end (&optional interrupt-too)
  "From here on a term or a hangup is a request to stop, heard as *ASKED-TO-STOP*
rather than the end: whoever is looping checks it and leaves properly, woken by
WAKE-FD. With INTERRUPT-TOO an interrupt is one as well, for a server in the
foreground."
  (setf *asked-to-stop* nil)
  (open-wake)
  (dolist (signal (list* sb-unix:sigterm sb-unix:sighup
                         (and interrupt-too (list sb-unix:sigint))))
    (sb-sys:enable-interrupt signal
                             (lambda (signal info context)
                               (declare (ignore signal info context))
                               (setf *asked-to-stop* t)
                               (wake)))))

(defun stop-hearing-the-end ()
  (dolist (signal (list sb-unix:sigterm sb-unix:sighup))
    (sb-sys:enable-interrupt signal :default)))

(defmacro with-host ((fd &key (to fd) (raw t)) &body body)
  "Take the terminal on FD over for the body, and give it back however the body
leaves.

Putting it back is the one thing that must always happen: a terminal left in raw
mode with no cursor is the worst thing this program can do to somebody."
  (let ((was (gensym "WAS")) (f (gensym "FD")) (w (gensym "TO")))
    `(let* ((,f ,fd)
            (,w ,to)
            (,was (when ,raw (host-raw ,f))))
       (unwind-protect
            (progn (pty:pty-write-string ,w +took-over+)
                   (hear-resizes)
                   (hear-the-end)
                   ,@body)
         (stop-hearing-the-end)
         (stop-hearing-resizes)
         (ignore-errors (pty:pty-write-string ,w +gave-back+))
         (host-put-back ,f ,was)))))
