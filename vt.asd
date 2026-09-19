(asdf:defsystem #:vt
                :description "A library for an extended vt set for emulation"
                :author "Bret Horne"
                :license "GPL"
                :version "0.0.1"
                :depends-on ()
                :serial t
                :pathname "src/term/"
                :components ((:file "package")
                             (:file "types")
                             (:file "decode")
                             (:file "color")
                             (:file "sgr")
                             (:file "ops")
                             (:file "write")
                             (:file "parser")
                             (:file "render")
                             (:file "input")
                             (:file "seq/sgr")
                             (:file "seq/cursor")
                             (:file "seq/edit")
                             (:file "seq/screen")
                             (:file "seq/modes")
                             (:file "seq/report")
                             (:file "seq/osc"))
                :in-order-to ((asdf:test-op (asdf:test-op #:vt/test/term))))

(asdf:defsystem #:vt/pty
                :description "Provides a pseudo terminal. SBCL only"
                :depends-on ()
                :serial t
                :pathname "src/pty/"
                :components ((:file "package")
                             (:file "pty" :if-feature :sbcl)
                             (:file "elsewhere" :if-feature (:not :sbcl))))

(asdf:defsystem #:vt/tty
                :description "Controlling the host terminal"
                :depends-on (#:vt #:vt/pty (:require #:sb-posix))
                :serial t
                :pathname "src/tty/"
                :components ((:file "package")
                             (:file "poll")
                             (:file "host")
                             (:file "keys")
                             (:file "screen")
                             (:file "encode")))

(asdf:defsystem #:vt/mode
                :description "A collection of hierarchal keymaps that bind input to behavior"
                :depends-on ()
                :serial t
                :pathname "src/mode/"
                :components ((:file "key")
                             (:file "mode")))

(asdf:defsystem #:vt/ui
                :description "Components for the things you can see and interact with, and their stlying"
                :depends-on ()
                :serial t
                :pathname "src/ui/"
                :components ((:file "widget")
                             (:file "theme")
                             (:file "style")
                             (:file "layout")))

(asdf:defsystem #:vt/cells
                :description "A terminal as something the widgets can be drawn on"
                :depends-on (#:vt #:vt/ui)
                :serial t
                :pathname "src/cells/"
                :components ((:file "cells")))

(asdf:defsystem #:vt/mux
                :description "A server client relationship to hold a list of fd for a pty to blit bits to a client"
                :depends-on (#:vt #:vt/pty #:vt/tty #:vt/mode #:vt/ui #:vt/cells
                                  (:require #:sb-posix)
                                  (:require #:sb-bsd-sockets))
                :serial t
                :pathname "src/mux/"
                :components ((:file "package")
                             (:file "wire")
                             (:file "pane")
                             (:file "view")
                             (:file "match")
                             (:file "server")
                             (:file "bar")
                             (:file "command")
                             (:file "client")
                             (:file "note")
                             (:file "prompt")
                             (:file "keys")
                             (:file "main")))


(asdf:defsystem #:vt/all
                :description "Everything here"
                :depends-on (#:vt #:vt/pty #:vt/tty #:vt/mode #:vt/ui #:vt/cells
                                  #:vt/mux))

(asdf:defsystem #:vt/test
                :description "Vt lib's test suite"
                :depends-on (#:vt/all #:vt/test/term #:fiveam)
                :serial t
                :pathname "tests/"
                :components ((:file "suite")
                             (:file "keys")
                             (:file "pty")
                             (:file "host")
                             (:file "screen")
                             (:file "encode")
                             (:file "wire")
                             (:file "mux")
                             (:file "cells")
                             (:file "view")
                             (:file "prompt"))
                :perform (asdf:test-op (o c)
                                       (uiop:symbol-call :vt/test :run-them)))

(asdf:defsystem #:vt/test/term
                :description "Terminals test suite"
                :depends-on (#:vt #:fiveam)
                :serial t
                :pathname "tests/term/"
                :components ((:file "suite")
                             (:file "write")
                             (:file "ops")
                             (:file "sgr")
                             (:file "parser")
                             (:file "render")
                             (:file "input")
                             (:file "decode"))
                :perform (asdf:test-op (o c)
                                       (uiop:symbol-call :vt/test/term :run-them)))


