;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
(require :asdf)
(asdf:load-system :vtx)
(sb-ext:save-lisp-and-die
 (or (sb-ext:posix-getenv "VTX_OUT") "vtx")
 :executable t
 :save-runtime-options t
 :toplevel (lambda ()
             (sb-ext:disable-debugger)
             (vtx:main)
             (sb-ext:quit :unix-status 0)))
