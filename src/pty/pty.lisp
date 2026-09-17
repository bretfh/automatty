;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/pty)

(defvar *helper* nil)

(cffi:define-foreign-library libvt-pty
  (t (:default "libvt-pty")))

(defvar *pty-loaded* nil)

(defun lib-dirs ()
  (remove nil
          (list (uiop:getenv "VT_LIB")
                (uiop:getenv "GUIX_ENVIRONMENT")
                (ignore-errors
                 (namestring (asdf:system-source-directory :vt/pty))))))

(defun attend ()
  (dolist (root (lib-dirs))
    (let ((dir (pathname (format nil "~a/lib/" (string-right-trim "/" root)))))
      (pushnew dir cffi:*foreign-library-directories* :test #'equal))))

(defun helper-path ()
  (or *helper*
      (loop :for root :in (lib-dirs)
            :for path := (format nil "~a/lib/vt-pty-helper"
                                 (string-right-trim "/" root))
            :when (probe-file path) :return path)))

(defun pty-library-p ()
  (unless *pty-loaded*
    (attend)
    (handler-case
        (progn (cffi:load-foreign-library 'libvt-pty)
               (setf *pty-loaded* t))
      (error () nil)))
  *pty-loaded*)

(cffi:defcfun ("vt_pty_spawn" %pty-spawn) :int
  (command :string) (helper :string) (rows :int) (cols :int) (pid :pointer))

(cffi:defcfun ("vt_pty_set_size" pty-set-size) :void
  (fd :int) (rows :int) (cols :int))

(defun spawn-pty-process (command &key (rows 24) (cols 80))
  "Run COMMAND under /bin/sh in a new pty. Returns (values master-fd pid)."
  (unless (pty-library-p)
    (error "libvt-pty is not loaded: build it with make libs"))
  (cffi:with-foreign-object (pid :int)
    (let ((master (%pty-spawn command (or (helper-path) "") rows cols pid)))
      (values master (cffi:mem-ref pid :int)))))

(defparameter +pollin+ 1)

(defun pty-wait (fd milliseconds)
  "Whether FD has something to read within MILLISECONDS.

A blocking read cannot be interrupted: closing the descriptor under it does not
wake it, and killing the thread inside a foreign call wedges the image at the
next GC. So a reader waits with a timeout and looks at its own flag between
waits."
  (cffi:with-foreign-object (pfd :char 8)   ; struct pollfd
    (setf (cffi:mem-ref pfd :int 0) fd
          (cffi:mem-ref pfd :short 4) +pollin+
          (cffi:mem-ref pfd :short 6) 0)
    (plusp (cffi:foreign-funcall "poll" :pointer pfd :unsigned-long 1
                                        :int milliseconds :int))))

(defun pty-read-string (fd size)
  "Read up to SIZE bytes from FD as a string, or nil at EOF/error."
  (cffi:with-foreign-object (buf :unsigned-char size)
    (let ((n (cffi:foreign-funcall "read" :int fd :pointer buf :long size :long)))
      (when (plusp n)
        (cffi:foreign-string-to-lisp buf :count n :encoding :latin-1)))))

(defun pty-write-string (fd string)
  (cffi:with-foreign-string ((s len) string :encoding :utf-8)
    (cffi:foreign-funcall "write" :int fd :pointer s :long len :long)))

(defun pty-close (fd)
  (cffi:foreign-funcall "close" :int fd :int))

(defun pty-kill (pid &optional (signal 15))
  (cffi:foreign-funcall "kill" :int pid :int signal :int))

(defun pty-reap (pid)
  "Signal PID and wait for it, so it is not left a zombie. Answers its status."
  (when (and pid (plusp pid))
    (pty-kill pid)
    (cffi:with-foreign-object (status :int)
      (let ((got (cffi:foreign-funcall "waitpid" :int pid :pointer status
                                                 :int 0 :int)))
        (when (plusp got) (cffi:mem-ref status :int))))))
