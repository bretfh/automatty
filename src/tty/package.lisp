;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:vt/tty
  (:use #:cl)
  (:local-nicknames (#:pty #:vt/pty))
  (:export
   #:make-waiting
   #:free-waiting
   #:waiting-clear
   #:waiting-add
   #:waiting-back
   #:waiting-count
   #:readable-p
   #:writable-p
   #:gone-p
   #:wait-on

   #:+tiocgwinsz+
   #:+stdin+
   #:+stdout+
   #:a-terminal-p
   #:host-size
   #:host-raw
   #:host-put-back
   #:*resized*
   #:hear-resizes
   #:stop-hearing-resizes
   #:*asked-to-stop*
   #:hear-the-end
   #:stop-hearing-the-end
   #:+blanked+
   #:with-host

   #:screen
   #:screen-p
   #:make-screen
   #:screen-width
   #:screen-height
   #:screen-grid
   #:screen-row
   #:screen-cursor-x
   #:screen-cursor-y
   #:screen-cursor-visible
   #:screen-cursor-style
   #:screen-resize
   #:screen-copy
   #:screen-diff

   #:run
   #:make-run
   #:run-row
   #:run-start
   #:run-end

   #:*gap*
   #:takes-of
   #:taken
   #:as-taken

   #:escape-sequence-to-key-event
   #:encode-runs
   #:encode-cursor
   #:write-cursor-shape
   #:encode-frame))
