;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:atty/pty
  (:use #:cl)
  (:export
   #:open-pty
   #:spawn-pty-process
   #:spawn-in-its-own-session
   #:become
   #:pty-set-size
   #:pty-wait
   #:pty-read-string #:pty-read-into #:close-on-exec #:keep-on-exec
   #:pty-write-string
   #:pty-write-some
   #:nonblocking
   #:pty-close
   #:pty-kill
   #:pty-reap
   #:pty-foreground
   #:group-command-lines
   #:group-processes
   #:process-path
   #:process-directory))
