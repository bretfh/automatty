;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(require :asdf)
(asdf:load-system :vt/mux)
(sb-ext:save-lisp-and-die
 (or (sb-ext:posix-getenv "VT_MUX_OUT") "vt-mux")
 :executable t
 :save-runtime-options t
 :toplevel (lambda ()
             (sb-ext:disable-debugger)
             (vt/mux:main)
             (sb-ext:quit :unix-status 0)))
