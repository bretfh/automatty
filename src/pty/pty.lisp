;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/pty)

;;; What a system calls a thing. These are the only per-system numbers here, and
;;; every one of them is frozen ABI. TIOCSWINSZ has not moved since the 1980s.
;;; A system nobody has checked stops at load with the name of what it is
;;; missing, rather than running and silently handing back a pipe that looks
;;; like a terminal.

(defconstant +o-rdwr+ 2)

;; the low errnos are the same number on every unix there is; sb-unix happens
;; to name some of them and not this one
(defconstant +esrch+ 3)

;; WNOHANG is 1 on linux, on darwin and on every bsd; SIGHUP is 1 and SIGKILL 9
;; everywhere a terminal has ever run.
(defconstant +wnohang+ 1)
(defconstant +sighup+ 1)
(defconstant +sigkill+ 9)

(defconstant +o-noctty+
  #+linux #o400
  #+darwin #x20000
  #+(or freebsd openbsd netbsd) #x8000
  #-(or linux darwin freebsd openbsd netbsd)
  (error "atty/pty has no O_NOCTTY for this system."))

(defconstant +posix-spawn-setsid+
  #+linux #x80
  #+darwin #x400
  #-(or linux darwin)
  (error "atty/pty has no checked POSIX_SPAWN_SETSID for this system. It is in
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
  (error "atty/pty has no TIOCSWINSZ for this system."))

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

(sb-alien:define-alien-routine ("execv" %execv) sb-alien:int
  (path sb-alien:c-string)
  (argv (* (* sb-alien:char))))

(defun become (program arguments)
  "Replace this process with PROGRAM run with ARGUMENTS: the same pid, the
same terminal, the same open files, and none of what this process was. How a
client becomes a newer build of itself. Answers only when it could not, with
why."
  (let ((argv (c-strings (cons (file-namestring program) arguments))))
    (%execv program argv)
    (let ((why (sb-int:strerror (sb-alien:get-errno))))
      (free-c-strings argv (1+ (length arguments)))
      (error "could not become ~A: ~A" program why))))

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

(defun can-change-directory-p ()
  "Whether this libc can have posix_spawn change directory for the child. It
is a late addition everywhere (macOS 10.15, glibc 2.29), so it is looked for
rather than assumed."
  (and (sb-sys:find-foreign-symbol-address "posix_spawn_file_actions_addchdir_np") t))

(defun sh-quoted (said)
  (with-output-to-string (out)
    (write-char #\' out)
    (loop :for c :across said
          :do (if (char= c #\')
                  (write-string "'\\''" out)
                  (write-char c out)))
    (write-char #\' out)))

(defun spawn-pty-process (command &key (rows 24) (cols 80) (shell "/bin/sh") environment
                                       directory)
  "Run COMMAND under a shell on a pseudo-terminal of its own, in DIRECTORY when
one is given. Answers (values master-fd pid).

The child is made a session leader by the spawn itself, and then opens the
slave by name rather than inheriting it already open: opening a terminal is how
a session leader with none takes one as its controlling terminal. That is what
makes ^C a signal and a resize a SIGWINCH, and it is why no code has to run in
the child between the fork and the exec, which is the only thing posix_spawn
cannot do and the reason this used to want a helper program written in C."
  (multiple-value-bind (master slave) (open-pty)
    (sb-alien:with-alien ((actions (sb-alien:array sb-alien:char #.+opaque+))
                          (attr (sb-alien:array sb-alien:char #.+opaque+))
                          (pid sb-alien:int))
      (let ((actions-sap (sb-alien:alien-sap actions))
            (attr-sap (sb-alien:alien-sap attr))
            (chdir (and directory (can-change-directory-p)))
            (arguments (list (file-namestring shell) "-c"
                             ;; without the spawn's own chdir, the shell does it:
                             ;; a directory that is gone is said and nothing runs
                             (if (and directory (not (can-change-directory-p)))
                                 (format nil "cd ~A || exit 1~%~A"
                                         (sh-quoted directory) command)
                                 command)))
            ;; what is said here is what the program sees: a name given twice
            ;; is read either way round by different shells, so what this
            ;; process inherited under the same name goes. ATTY_PANE from an
            ;; atty this one runs inside must not reach a pane of this one.
            (environment (let ((given (list* "TERM=xterm-256color" "COLORTERM=truecolor"
                                             environment)))
                           (append given
                                   (remove-if (lambda (entry)
                                                (let ((name (subseq entry 0 (position #\= entry))))
                                                  (member name given
                                                          :test (lambda (name g)
                                                                  (and (> (length g) (length name))
                                                                       (char= (char g (length name)) #\=)
                                                                       (string= name g :end2 (length name)))))))
                                              (sb-ext:posix-environ))))))
        (check (%actions-init actions-sap) "posix_spawn_file_actions_init")
        (check (%attr-init attr-sap) "posix_spawnattr_init")
        (unwind-protect
             (let ((argv (c-strings arguments))
                   (envp (c-strings environment)))
               (unwind-protect
                    (progn
                      (when chdir
                        (check (sb-alien:alien-funcall
                                (sb-alien:extern-alien "posix_spawn_file_actions_addchdir_np"
                                                       (function sb-alien:int
                                                                 sb-alien:system-area-pointer
                                                                 sb-alien:c-string))
                                actions-sap directory)
                               "addchdir of the directory"))
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

(defun spawn-in-its-own-session (program arguments &key input output
                                                       (environment
                                                        (sb-ext:posix-environ)))
  "Run PROGRAM in a session of its own. Answers its pid.

INPUT is a path it reads, OUTPUT a path it appends to; both go to /dev/null when
they are not given, and what it writes to standard error goes where its output
does.

A child started any other way keeps the session, the foreground group and the
controlling terminal of whatever started it, and the hangup that closes that
terminal reaches it. This one is out of reach of all three before it has run a
single instruction of its own: the spawn does it, so there is nothing to run
between the fork and the exec."
  (sb-alien:with-alien ((actions (sb-alien:array sb-alien:char #.+opaque+))
                        (attr (sb-alien:array sb-alien:char #.+opaque+))
                        (pid sb-alien:int))
    (let ((actions-sap (sb-alien:alien-sap actions))
          (attr-sap (sb-alien:alien-sap attr))
          (arguments (cons (file-namestring program) arguments)))
      (check (%actions-init actions-sap) "posix_spawn_file_actions_init")
      (check (%attr-init attr-sap) "posix_spawnattr_init")
      (unwind-protect
           (let ((argv (c-strings arguments))
                 (envp (c-strings environment)))
             (unwind-protect
                  (progn
                    (check (%actions-addopen actions-sap 0
                                             (or input "/dev/null")
                                             sb-unix:o_rdonly 0)
                           "addopen of the input")
                    (check (%actions-addopen actions-sap 1
                                             (or output "/dev/null")
                                             (logior sb-unix:o_wronly
                                                     sb-unix:o_creat
                                                     sb-unix:o_append)
                                             #o600)
                           "addopen of the output")
                    (check (%actions-adddup2 actions-sap 1 2) "adddup2 to stderr")
                    (check (%attr-setflags attr-sap +posix-spawn-setsid+)
                           "setflags SETSID")
                    (let ((rc (%spawn (sb-alien:alien-sap (sb-alien:addr pid))
                                      (namestring program) actions-sap attr-sap
                                      (sb-alien:alien-sap argv)
                                      (sb-alien:alien-sap envp))))
                      (unless (zerop rc)
                        (error "could not start ~S: ~A"
                               program (sb-int:strerror rc)))
                      pid))
               (free-c-strings argv (length arguments))
               (free-c-strings envp (length environment))))
        (%actions-destroy actions-sap)
        (%attr-destroy attr-sap)))))

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

Answers nil when the program is done, which is an end of file or the EIO a
master is given once the last slave is closed. Answers an empty string when a
signal interrupted the read, which is not the program being done and must not be
read as it. Anything else is a fault and is signalled."
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

(defun pty-write-string (fd string &optional (external-format :latin-1))
  "Say STRING to the program. Answers how many bytes of it were taken. A write
can be a short one, so it is finished rather than assumed.

One byte a character by default, which is what pty-read-string answers, so what
was read from one terminal can be written to another and be the same bytes. What
the bytes mean is for whoever knows the encoding, which is what decode-utf-8 is
for, and a caller holding real characters rather than bytes says so."
  (let* ((octets (sb-ext:string-to-octets string :external-format external-format))
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

(defun pty-reap (pid &optional (patience 2))
  "Tell PID its terminal is gone and wait for it, so it is not left a zombie.
Answers its status, or nil when it would not go.

It never waits without end. A shell ignores the polite ask, which is what makes
it a shell, so whoever is tidying up would wait on it forever; and the thing
tidying up is usually a server on its way out, still holding the socket
everybody else is trying to reach. So: the hangup a terminal going away sends,
then PATIENCE seconds, then the one nothing ignores, then give up and let init
have it."
  (when (and pid (plusp pid))
    (pty-kill pid +sighup+)
    (sb-alien:with-alien ((status sb-alien:int))
      (let ((sap (sb-alien:alien-sap (sb-alien:addr status)))
            (deadline (+ (get-internal-real-time)
                         (* patience internal-time-units-per-second)))
            (asked-harder nil))
        (loop
          (let ((got (%waitpid pid sap +wnohang+)))
            (cond
              ((plusp got) (return status))
              ((minusp got) (return nil))
              ((> (get-internal-real-time) deadline)
               (when asked-harder (return nil))
               (setf asked-harder t
                     deadline (+ (get-internal-real-time)
                                 (* patience internal-time-units-per-second)))
               (pty-kill pid +sigkill+))
              (t (sleep 0.005)))))))))

(sb-alien:define-alien-routine ("tcgetpgrp" %tcgetpgrp) sb-alien:int
  (fd sb-alien:int))

(defun pty-foreground (fd)
  (let ((group (%tcgetpgrp fd)))
    (and (plusp group) group)))

(defconstant +arguments-room+ 65536)

(defun nul-separated (octets start end count)
  (loop :with at := start
        :for n :from 0
        :for stop := (and (< n count) (< at end)
                          (or (position 0 octets :start at :end end) end))
        :while stop
        :collect (sb-ext:octets-to-string octets :start at :end stop
                                                 :external-format :utf-8)
        :do (setf at (1+ stop))))

#+darwin
(progn
  (defconstant +proc-pgrp-only+ 2)
  (defconstant +ctl-kern+ 1)
  (defconstant +kern-procargs2+ 49)

  (sb-alien:define-alien-routine ("proc_listpids" %proc-listpids) sb-alien:int
    (type sb-alien:unsigned-int) (info sb-alien:unsigned-int)
    (buffer sb-alien:system-area-pointer) (size sb-alien:int))

  (sb-alien:define-alien-routine ("sysctl" %sysctl) sb-alien:int
    (name sb-alien:system-area-pointer) (count sb-alien:unsigned-int)
    (old sb-alien:system-area-pointer) (size sb-alien:system-area-pointer)
    (new sb-alien:system-area-pointer) (new-size sb-alien:unsigned-long))

  (sb-alien:define-alien-routine ("proc_pidpath" %proc-pidpath) sb-alien:int
    (pid sb-alien:int) (buffer sb-alien:system-area-pointer) (size sb-alien:unsigned-int))

  (defun process-path (pid)
    (let ((room (make-array 4096 :element-type '(unsigned-byte 8))))
      (sb-sys:with-pinned-objects (room)
        (let ((n (%proc-pidpath pid (sb-sys:vector-sap room) (length room))))
          (and (plusp n) (sb-ext:octets-to-string room :end n :external-format :utf-8))))))

  (defun group-members (group)
    (let ((pids (make-array 256 :element-type '(signed-byte 32))))
      (sb-sys:with-pinned-objects (pids)
        (let ((bytes (%proc-listpids +proc-pgrp-only+ group (sb-sys:vector-sap pids)
                                     (* 4 (length pids)))))
          (loop :for i :below (max 0 (floor bytes 4))
                :for pid := (aref pids i)
                :when (plusp pid) :collect pid)))))

  (defun command-line (pid)
    (let ((name (make-array 3 :element-type '(signed-byte 32)
                              :initial-contents (list +ctl-kern+ +kern-procargs2+ pid)))
          (size (make-array 1 :element-type '(unsigned-byte 64)
                              :initial-element +arguments-room+))
          (room (make-array +arguments-room+ :element-type '(unsigned-byte 8))))
      (sb-sys:with-pinned-objects (name size room)
        (when (zerop (%sysctl (sb-sys:vector-sap name) 3 (sb-sys:vector-sap room)
                              (sb-sys:vector-sap size) (sb-sys:int-sap 0) 0))
          (let* ((end (min (aref size 0) (length room)))
                 (count (sb-sys:signed-sap-ref-32 (sb-sys:vector-sap room) 0))
                 (path-end (position 0 room :start 4 :end end))
                 (start (and path-end
                             (position 0 room :start path-end :end end :test-not #'eql))))
            (and start (nul-separated room start end count))))))))

#+linux
(progn
  (defun words-of (line)
    (loop :with at := 0
          :for start := (position #\Space line :start at :test-not #'char=)
          :while start
          :collect (let ((stop (or (position #\Space line :start start) (length line))))
                     (prog1 (subseq line start stop) (setf at stop)))))

  (defun group-of (pid)
    (let* ((line (ignore-errors
                  (with-open-file (in (format nil "/proc/~D/stat" pid))
                    (read-line in nil))))
           (close (and line (position #\) line :from-end t)))
           (fields (and close (words-of (subseq line (1+ close))))))
      (and (>= (length fields) 3) (parse-integer (third fields) :junk-allowed t))))

  (defun group-members (group)
    (loop :for path :in (directory "/proc/*/" :resolve-symlinks nil)
          :for pid := (parse-integer (first (last (pathname-directory path)))
                                     :junk-allowed t)
          :when (and pid (eql group (group-of pid))) :collect pid))

  (defun process-path (pid)
    (ignore-errors (namestring (truename (format nil "/proc/~D/exe" pid)))))

  (defun command-line (pid)
    (let ((octets (ignore-errors
                   (with-open-file (in (format nil "/proc/~D/cmdline" pid)
                                       :element-type '(unsigned-byte 8))
                     (let ((room (make-array +arguments-room+
                                             :element-type '(unsigned-byte 8))))
                       (subseq room 0 (read-sequence room in)))))))
      (and octets (nul-separated octets 0 (length octets) most-positive-fixnum)))))

#-(or darwin linux)
(progn
  (defun group-members (group)
    (declare (ignore group))
    nil)

  (defun command-line (pid)
    (declare (ignore pid))
    nil)

  (defun process-path (pid)
    (declare (ignore pid))
    nil))

(defun group-command-lines (group)
  (loop :for pid :in (and group (group-members group))
        :for words := (command-line pid)
        :when words :collect (format nil "~{~A~^ ~}" words)))

(defun group-processes (group)
  (loop :for pid :in (and group (group-members group))
        :for words := (command-line pid)
        :when words :collect (list :pid pid
                                   :line (format nil "~{~A~^ ~}" words)
                                   :path (process-path pid))))
