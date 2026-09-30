;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(require :asdf)
(asdf:load-system :atty)
;; the build says which commit it is; make passes what git describe said
(setf atty:*version* (or (sb-ext:posix-getenv "ATTY_VERSION") "unknown"))
(sb-ext:save-lisp-and-die
 (or (sb-ext:posix-getenv "ATTY_OUT") "atty")
 :executable t
 :save-runtime-options t
 :toplevel (lambda ()
             (sb-ext:disable-debugger)
             ;; the heap is sized for scrollback, and the collector would take a
             ;; twentieth of it as its nursery: a collection that holds that much
             ;; garbage before it runs is that much memory and that long a pause
             (setf (sb-ext:bytes-consed-between-gcs) (* 64 1024 1024))
             (atty:main)
             (sb-ext:quit :unix-status 0)))
