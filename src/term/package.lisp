;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:vt
  (:use #:cl)
  (:export
   #:cell
   #:make-cell
   #:cell-char
   #:cell-face

   #:face-attrs
   #:make-face-attrs
   #:copy-face-attrs
   #:face-attrs-equal
   #:face-default-p
   #:face-fg
   #:face-bg
   #:face-bold
   #:face-faint
   #:face-italic
   #:face-underline
   #:face-underline-color
   #:face-blink
   #:face-inverse
   #:face-conceal
   #:face-crossed

   #:term
   #:term-p
   #:make-term
   #:term-width
   #:term-height
   #:term-grid
   #:term-grid-row
   #:term-cursor-x
   #:term-cursor-y
   #:term-cursor-visible
   #:term-cursor-style
   #:term-title
   #:term-cwd
   #:term-bracketed-paste
   #:term-insert-mode
   #:term-keypad-mode
   #:term-auto-margin
   #:term-in-alt-screen
   #:term-scroll-top
   #:term-scroll-bottom
   #:term-input-fn
   #:term-bell-fn
   #:term-title-fn
   #:term-cwd-fn

   #:term-process-output
   #:term-write
   #:term-resize
   #:term-reset

   #:term-cursor-up
   #:term-cursor-down
   #:term-cursor-left
   #:term-cursor-right
   #:term-cursor-horizontal-abs
   #:term-cursor-vertical-abs
   #:term-goto
   #:term-save-cursor
   #:term-restore-cursor
   #:term-erase-in-line
   #:term-erase-in-display
   #:term-erase-char
   #:term-insert-char
   #:term-delete-char
   #:term-insert-line
   #:term-delete-line
   #:term-scroll-up
   #:term-scroll-down
   #:term-horizontal-tab
   #:term-horizontal-backtab
   #:term-index
   #:term-reverse-index
   #:term-line-feed
   #:term-carriage-return
   #:term-set-scroll-region
   #:term-enter-alt-screen
   #:term-exit-alt-screen

   #:term-max-scrollback
   #:term-scrollback-size
   #:term-scrollback-row
   #:term-scrollback-row-string

   #:term-render-line
   #:term-render-ansi-line
   #:write-sgr
   #:write-number
   #:rgb-to-color-index
   #:term-dump-row-string
   #:term-dump-to-string
   #:face-attrs-to-plist
   #:color-index-to-rgb
   #:char-display-width

   #:key-event-to-escape-sequence))
