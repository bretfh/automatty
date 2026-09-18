;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/pty)

;;; What a system calls a thing. These are the only per-system numbers here, and
;;; every one of them is frozen ABI -- TIOCSWINSZ has not moved since the 1980s.
;;; A system nobody has checked stops at load with the name of what it is
;;; missing, rather than running and silently handing back a pipe that looks
;;; like a terminal.

(defconstant +o-rdwr+ 2)

;; the low errnos are the same number on every unix there is; sb-unix happens
;; to name some of them and not this one
(defconstant +esrch+ 3)

(defconstant +o-noctty+
  #+linux #o400
  #+darwin #x20000
  #+(or freebsd openbsd netbsd) #x8000
  #-(or linux darwin freebsd openbsd netbsd)
  (error "vt/pty has no O_NOCTTY for this system."))

(defconstant +posix-spawn-setsid+
  #+linux #x80
  #+darwin #x400
  #-(or linux darwin)
  (error "vt/pty has no checked POSIX_SPAWN_SETSID for this system. It is in
spawn.h; the pty suite says whether the one you put here is right, because it
asserts the child comes up with a controlling terminal."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bsd-ioctl-write (group number length)
    "What a BSD calls an ioctl that writes LENGTH bytes. The number is a bit
layout rather than a list, so the whole family follows from the one rule."
    (logior #x80000000
            (ash (logand length #x1fff) 16)
            (ash (char-code group) 8)
            number)))

(defconstant +tiocswinsz+
  #+linux #x5414
  #+(or darwin freebsd openbsd netbsd) (bsd-ioctl-write #\t 103 8)
  #-(or linux darwin freebsd openbsd netbsd)
  (error "vt/pty has no TIOCSWINSZ for this system."))

;;; libc. Nothing here is compiled; it is all already on the machine.

(sb-alien:define-alien-routine ("posix_openpt" %openpt) sb-alien:int
  (flags sb-alien:int))

(sb-alien:define-alien-routine ("grantpt" %grantpt) sb-alien:int
  (fd sb-alien:int))

(sb-alien:define-alien-routine ("unlockpt" %unlockpt) sb-alien:int
  (fd sb-alien:int))

(sb-alien:define-alien-routine ("ptsname" %ptsname) sb-alien:c-string
  (fd sb-alien:int))

(sb-alien:define-alien-routine ("posix_spawn_file_actions_init" %actions-init)
    sb-alien:int (actions sb-alien:system-area-pointer))

(sb-alien:define-alien-routine ("posix_spawn_file_actions_destroy" %actions-destroy)
    sb-alien:int (actions sb-alien:system-area-pointer))

(sb-alien:define-alien-routine ("posix_spawn_file_actions_addopen" %actions-addopen)
    sb-alien:int
  (actions sb-alien:system-area-pointer) (fd sb-alien:int)
  (path sb-alien:c-string) (flags sb-alien:int) (mode sb-alien:int))

(sb-alien:define-alien-routine ("posix_spawn_file_actions_adddup2" %actions-adddup2)
    sb-alien:int
  (actions sb-alien:system-area-pointer) (fd sb-alien:int) (to sb-alien:int))

(sb-alien:define-alien-routine ("posix_spawnattr_init" %attr-init) sb-alien:int
  (attr sb-alien:system-area-pointer))

(sb-alien:define-alien-routine ("posix_spawnattr_destroy" %attr-destroy) sb-alien:int
  (attr sb-alien:system-area-pointer))

(sb-alien:define-alien-routine ("posix_spawnattr_setflags" %attr-setflags) sb-alien:int
  (attr sb-alien:system-area-pointer) (flags sb-alien:short))

(sb-alien:define-alien-routine ("posix_spawn" %spawn) sb-alien:int
  (pid sb-alien:system-area-pointer) (path sb-alien:c-string)
  (actions sb-alien:system-area-pointer) (attr sb-alien:system-area-pointer)
  (argv sb-alien:system-area-pointer) (envp sb-alien:system-area-pointer))

(sb-alien:define-alien-routine ("kill" %kill) sb-alien:int
  (pid sb-alien:int) (signal sb-alien:int))

(sb-alien:define-alien-routine ("waitpid" %waitpid) sb-alien:int
  (pid sb-alien:int) (status sb-alien:system-area-pointer) (flags sb-alien:int))

;;; posix_spawn_file_actions_t and posix_spawnattr_t are opaque, and every
;;; system makes them a different size. Nothing here needs to know which: the
;;; init call fills whatever it owns inside room that is more than enough.
(defconstant +opaque+ 1024)

(defun check (rc what)
  "The posix_spawn calls answer an errno rather than setting one."
  (unless (zerop rc)
    (error "~A: ~A" what (sb-int:strerror rc)))
  rc)

(defun check-errno (value what)
  "The rest set errno and answer -1, or nil through sb-unix."
  (when (or (null value) (and (realp value) (minusp value)))
    (error "~A: ~A" what (sb-int:strerror (sb-alien:get-errno))))
  value)

(defun c-strings (strings)
  (let* ((n (length strings))
         (array (sb-alien:make-alien (* sb-alien:char) (1+ n))))
    (loop :for string :in strings
          :for i :from 0
          :do (setf (sb-alien:deref array i) (sb-alien:make-alien-string string)))
    (setf (sb-alien:deref array n)
          (sb-alien:sap-alien (sb-sys:int-sap 0) (* sb-alien:char)))
    array))

(defun free-c-strings (array n)
  (dotimes (i n)
    (sb-alien:free-alien (sb-alien:deref array i)))
  (sb-alien:free-alien array))

(defvar *ptsname-lock* (sb-thread:make-mutex :name "ptsname")
  "ptsname answers out of one buffer it keeps, so only one caller may be in it.")

(defun open-pty ()
  "A pseudo-terminal. Answers (values master-fd slave-path)."
  (let ((master (check-errno (%openpt (logior +o-rdwr+ +o-noctty+))
                             "posix_openpt")))
    (handler-bind ((error (lambda (e) (declare (ignore e))
                            (sb-unix:unix-close master))))
      (check-errno (%grantpt master) "grantpt")
      (check-errno (%unlockpt master) "unlockpt")
      (let ((slave (sb-thread:with-mutex (*ptsname-lock*)
                     (%ptsname master))))
        (unless slave
          (error "ptsname: ~A" (sb-int:strerror (sb-alien:get-errno))))
        (values master slave)))))

(defun spawn-pty-process (command &key (rows 24) (cols 80) (shell "/bin/sh"))
  "Run COMMAND under a shell on a pseudo-terminal of its own. Answers
 (values master-fd pid).

The child is made a session leader by the spawn itself, and then opens the
slave by name rather than inheriting it already open: opening a terminal is how
a session leader with none takes one as its controlling terminal. That is what
makes ^C a signal and a resize a SIGWINCH, and it is why no code has to run in
the child between the fork and the exec -- which is the only thing posix_spawn
cannot do, and the reason this used to want a helper program written in C."
  (multiple-value-bind (master slave) (open-pty)
    (sb-alien:with-alien ((actions (sb-alien:array sb-alien:char #.+opaque+))
                          (attr (sb-alien:array sb-alien:char #.+opaque+))
                          (pid sb-alien:int))
      (let ((actions-sap (sb-alien:alien-sap actions))
            (attr-sap (sb-alien:alien-sap attr))
            (arguments (list (file-namestring shell) "-c" command))
            (environment (list* "TERM=xterm-256color" "COLORTERM=truecolor"
                                (sb-ext:posix-environ))))
        (check (%actions-init actions-sap) "posix_spawn_file_actions_init")
        (check (%attr-init attr-sap) "posix_spawnattr_init")
        (unwind-protect
             (let ((argv (c-strings arguments))
                   (envp (c-strings environment)))
               (unwind-protect
                    (progn
                      (check (%actions-addopen actions-sap 0 slave +o-rdwr+ 0)
                             "addopen of the slave")
                      (check (%actions-adddup2 actions-sap 0 1) "adddup2 to stdout")
                      (check (%actions-adddup2 actions-sap 0 2) "adddup2 to stderr")
                      (check (%attr-setflags attr-sap +posix-spawn-setsid+)
                             "setflags SETSID")
                      (let ((rc (%spawn (sb-alien:alien-sap (sb-alien:addr pid))
                                        shell actions-sap attr-sap
                                        (sb-alien:alien-sap argv)
                                        (sb-alien:alien-sap envp))))
                        (unless (zerop rc)
                          (sb-unix:unix-close master)
                          (error "could not start ~S: ~A"
                                 command (sb-int:strerror rc)))
                        (pty-set-size master rows cols)
                        (values master pid)))
                 (free-c-strings argv (length arguments))
                 (free-c-strings envp (length environment))))
          (%actions-destroy actions-sap)
          (%attr-destroy attr-sap))))))

(defun pty-set-size (fd rows cols)
  (sb-alien:with-alien ((size (sb-alien:array sb-alien:unsigned-short 4)))
    (setf (sb-alien:deref size 0) rows
          (sb-alien:deref size 1) cols
          (sb-alien:deref size 2) 0
          (sb-alien:deref size 3) 0)
    (check-errno (sb-unix:unix-ioctl fd +tiocswinsz+ (sb-alien:alien-sap size))
                 "TIOCSWINSZ")))

(defun pty-wait (fd milliseconds)
  "Whether FD has something to read within MILLISECONDS.

A blocking read cannot be interrupted: closing the descriptor under it does not
wake it, and killing the thread inside a foreign call wedges the image at the
next GC. So a reader waits with a timeout and looks at its own flag between
waits."
  (and (sb-unix:unix-simple-poll fd :input milliseconds) t))

(defvar *read-buffer* nil)
(defvar *read-buffer-thread* nil)

(defun read-buffer (size)
  "A buffer to read into, kept rather than made again every read.

Whose it is matters: two threads reading two terminals would otherwise read into
the same array at the same time, and each would answer some of the other's
bytes. A thread that finds the buffer is not its own takes a fresh one, and the
thread that had it goes on holding what it already has."
  (let ((buffer *read-buffer*))
    (if (and buffer (>= (length buffer) size)
             (eq *read-buffer-thread* sb-thread:*current-thread*))
        buffer
        (setf *read-buffer-thread* sb-thread:*current-thread*
              *read-buffer* (make-array size :element-type '(unsigned-byte 8))))))

(defun pty-read-string (fd size)
  "Read up to SIZE bytes from FD as a string. A byte is a character, so a
character split across two reads is not made nonsense of; what the bytes mean is
for whoever knows the encoding.

Answers nil when the program is done -- an end of file, or the EIO a master is
given once the last slave is closed -- and an empty string when a signal
interrupted the read, which is not the program being done and must not be read
as it. Anything else is a fault and is signalled."
  (let ((octets (read-buffer size)))
    (sb-sys:with-pinned-objects (octets)
      (multiple-value-bind (n errno)
          (sb-unix:unix-read fd (sb-sys:vector-sap octets) size)
        (cond ((and n (plusp n))
               (sb-ext:octets-to-string octets :external-format :latin-1 :end n))
              (n nil)
              ((or (eql errno sb-unix:eintr) (eql errno sb-unix:eagain)) "")
              ((eql errno sb-unix:eio) nil)
              (t (error "reading the terminal: ~A" (sb-int:strerror errno))))))))

(defun pty-write-string (fd string)
  "Say STRING to the program. Answers how many bytes of it were taken. A write
can be a short one, so it is finished rather than assumed."
  (let* ((octets (sb-ext:string-to-octets string :external-format :utf-8))
         (len (length octets))
         (sent 0))
    (sb-sys:with-pinned-objects (octets)
      (loop :while (< sent len)
            :do (multiple-value-bind (n errno)
                    (sb-unix:unix-write fd (sb-sys:vector-sap octets) sent
                                        (- len sent))
                  (cond ((and n (plusp n)) (incf sent n))
                        ((or (eql errno sb-unix:eintr) (eql errno sb-unix:eagain)))
                        (t (error "writing to the terminal: ~A"
                                  (sb-int:strerror errno)))))))
    sent))

(defun pty-close (fd)
  (sb-unix:unix-close fd))

(defun pty-kill (pid &optional (signal 15))
  "Answers nil when there was no such process to signal, which is not a fault:
it is the usual answer about something already gone."
  (let ((rc (%kill pid signal)))
    (cond ((zerop rc) t)
          ((eql (sb-alien:get-errno) +esrch+) nil)
          (t (error "kill: ~A" (sb-int:strerror (sb-alien:get-errno)))))))

(defun pty-reap (pid)
  "Signal PID and wait for it, so it is not left a zombie. Answers its status."
  (when (and pid (plusp pid))
    (pty-kill pid)
    (sb-alien:with-alien ((status sb-alien:int))
      (let ((got (%waitpid pid (sb-alien:alien-sap (sb-alien:addr status)) 0)))
        (when (plusp got) status)))))
