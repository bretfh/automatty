;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:atty/pty
  (:use #:cl)
  (:export
   #:open-pty
   #:spawn-pty-process
   #:spawn-in-its-own-session
   #:pty-set-size
   #:pty-wait
   #:pty-read-string
   #:pty-write-string
   #:pty-close
   #:pty-kill
   #:pty-reap))
