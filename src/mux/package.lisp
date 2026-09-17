;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:vt/mux
  (:use #:cl)
  (:export
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
   #:screen-clear
   #:screen-blit
   #:screen-diff
   #:screen-dump-to-string

   #:run-row
   #:run-start
   #:run-end
   #:run-cells

   #:*gap*
   #:encode-runs
   #:encode-cursor
   #:encode-frame
   #:takes-of))
