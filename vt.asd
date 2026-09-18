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
                             (:file "input"))
                :in-order-to ((asdf:test-op (asdf:test-op #:vt/test))))

(asdf:defsystem #:vt/pty
                :description "A program on a pseudo-terminal of its own"
                :depends-on ()
                :serial t
                :pathname "src/pty/"
                :components ((:file "package")
                             (:file "pty")))

(asdf:defsystem #:vt/graph
                :description "A value, what was worked out from it, and what
hears that it moved"
                :depends-on ()
                :serial t
                :pathname "src/graph/"
                :components ((:file "graph")
                             (:file "watch")))

(asdf:defsystem #:vt/mode
                :description "What a key is called, what it does, and which mode
says so"
                :depends-on (#:vt/graph)
                :serial t
                :pathname "src/mode/"
                :components ((:file "key")
                             (:file "mode")))

(asdf:defsystem #:vt/ui
                :description "Widgets, what they measure to, and the face each
one wears"
                :depends-on (#:vt/graph)
                :serial t
                :pathname "src/ui/"
                :components ((:file "widget")
                             (:file "theme")
                             (:file "style")
                             (:file "layout")
                             (:file "surface")))

(asdf:defsystem #:vt/cells
                :description "A terminal as something the widgets can be drawn
on"
                :depends-on (#:vt #:vt/ui)
                :serial t
                :pathname "src/cells/"
                :components ((:file "cells")))

(asdf:defsystem #:vt/mux
                :description "Many terminals inside one: a server that holds
them, and a client that shows one"
                :depends-on (#:vt #:vt/pty #:vt/mode #:vt/ui #:vt/cells
                                  (:require #:sb-posix)
                                  (:require #:sb-bsd-sockets))
                :serial t
                :pathname "src/mux/"
                :components ((:file "package")
                             (:file "poll")
                             (:file "host")
                             (:file "screen")
                             (:file "encode")
                             (:file "wire")
                             (:file "pane")
                             (:file "match")
                             (:file "server")
                             (:file "bar")
                             (:file "command")
                             (:file "client")
                             (:file "note")
                             (:file "prompt")
                             (:file "main")))

(asdf:defsystem #:vt/all
                :description "Every system cl-vt ships"
                :depends-on (#:vt #:vt/pty #:vt/graph #:vt/mode #:vt/ui #:vt/cells #:vt/mux))

(asdf:defsystem #:vt/test
                :depends-on (#:vt/all #:fiveam)
                :serial t
                :pathname "tests/"
                :components ((:file "suite")
                             (:file "write")
                             (:file "ops")
                             (:file "sgr")
                             (:file "parser")
                             (:file "render")
                             (:file "input")
                             (:file "decode")
                             (:file "pty")
                             (:file "host")
                             (:file "screen")
                             (:file "encode")
                             (:file "wire")
                             (:file "mux")
                             (:file "cells")
                             (:file "prompt"))
                :perform (asdf:test-op (o c)
                                       (uiop:symbol-call :vt/test :run-them)))
