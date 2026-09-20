(asdf:defsystem #:vtx/pty
                :description "A pseudo terminal. SBCL only"
                :depends-on ()
                :serial t
                :pathname "src/pty/"
                :components ((:file "package")
                             (:file "pty" :if-feature :sbcl)
                             (:file "elsewhere" :if-feature (:not :sbcl))))

(asdf:defsystem #:vtx/tty
                :description "Controlling the host terminal"
                :depends-on (#:libvtx #:vtx/pty (:require #:sb-posix))
                :serial t
                :pathname "src/tty/"
                :components ((:file "package")
                             (:file "poll")
                             (:file "host")
                             (:file "keys")
                             (:file "screen")
                             (:file "encode")))

(asdf:defsystem #:vtx/mode
                :description "Hierarchal keymaps that bind input to behavior"
                :depends-on ()
                :serial t
                :pathname "src/mode/"
                :components ((:file "key")
                             (:file "mode")))

(asdf:defsystem #:vtx/ui
                :description "Widgets, and their styling and layout"
                :depends-on ()
                :serial t
                :pathname "src/ui/"
                :components ((:file "widget")
                             (:file "theme")
                             (:file "style")
                             (:file "layout")))

(asdf:defsystem #:vtx/cells
                :description "A terminal as something widgets are drawn on"
                :depends-on (#:libvtx #:vtx/ui)
                :serial t
                :pathname "src/cells/"
                :components ((:file "cells")))

(asdf:defsystem #:vtx/agent
                :description "What the program in a pane is doing"
                :depends-on (#:libvtx)
                :serial t
                :pathname "src/agent/"
                :components ((:file "package")
                             (:file "agent")
                             (:file "claude-code")))

(asdf:defsystem #:vtx
                :description "A terminal emulator and multiplexer"
                :author "Bret Horne"
                :license "GPL-3.0-or-later"
                :version "0.0.1"
                :depends-on (#:libvtx #:vtx/pty #:vtx/tty #:vtx/mode #:vtx/ui #:vtx/cells
                                  #:vtx/agent
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

(asdf:defsystem #:vtx/all
                :description "Everything here"
                :depends-on (#:libvtx #:vtx/pty #:vtx/tty #:vtx/mode #:vtx/ui #:vtx/cells
                                  #:vtx/agent #:vtx))

(asdf:defsystem #:vtx/test
                :description "Vtx's test suite"
                :depends-on (#:vtx/all #:libvtx/test #:fiveam)
                :serial t
                :pathname "tests/"
                :components ((:file "suite")
                             (:file "keys")
                             (:file "pty")
                             (:file "agent")
                             (:file "host")
                             (:file "screen")
                             (:file "encode")
                             (:file "wire")
                             (:file "mux")
                             (:file "cells")
                             (:file "view")
                             (:file "prompt"))
                :perform (asdf:test-op (o c)
                                       (uiop:symbol-call :vtx/test :run-them)))
