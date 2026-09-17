;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:vt/pty
  (:use #:cl)
  (:export
   #:*helper*
   #:pty-library-p
   #:spawn-pty-process
   #:pty-set-size
   #:pty-wait
   #:pty-read-string
   #:pty-write-string
   #:pty-close
   #:pty-kill
   #:pty-reap))
