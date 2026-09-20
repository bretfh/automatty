;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(require :asdf)
(asdf:load-system :atty)
(sb-ext:save-lisp-and-die
 (or (sb-ext:posix-getenv "ATTY_OUT") "atty")
 :executable t
 :save-runtime-options t
 :toplevel (lambda ()
             (sb-ext:disable-debugger)
             (atty:main)
             (sb-ext:quit :unix-status 0)))
