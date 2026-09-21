(asdf:defsystem #:atty/pty
                :description "A pseudo terminal. SBCL only"
                :depends-on ()
                :serial t
                :pathname "src/pty/"
                :components ((:file "package")
                             (:file "pty" :if-feature :sbcl)
                             (:file "elsewhere" :if-feature (:not :sbcl))))

(asdf:defsystem #:atty/tty
                :description "Controlling the host terminal"
                :depends-on (#:libatty #:atty/pty (:require #:sb-posix))
                :serial t
                :pathname "src/tty/"
                :components ((:file "package")
                             (:file "poll")
                             (:file "host")
                             (:file "keys")
                             (:file "screen")
                             (:file "encode")))

(asdf:defsystem #:atty/mode
                :description "Hierarchal keymaps that bind input to behavior"
                :depends-on ()
                :serial t
                :pathname "src/mode/"
                :components ((:file "key")
                             (:file "mode")))

(asdf:defsystem #:atty/ui
                :description "Widgets, and their styling and layout"
                :depends-on ()
                :serial t
                :pathname "src/ui/"
                :components ((:file "widget")
                             (:file "theme")
                             (:file "style")
                             (:file "layout")))

(asdf:defsystem #:atty/cells
                :description "A terminal as something widgets are drawn on"
                :depends-on (#:libatty #:atty/ui)
                :serial t
                :pathname "src/cells/"
                :components ((:file "cells")))

(asdf:defsystem #:atty/agent
                :description "What the program in a pane is doing"
                :depends-on (#:libatty)
                :serial t
                :pathname "src/agent/"
                :components ((:file "package")
                             (:file "agent")
                             (:file "claude-code")))

(asdf:defsystem #:atty
                :description "A terminal emulator and multiplexer"
                :author "Bret Horne"
                :license "GPL-3.0-or-later"
                :version "0.0.1"
                :depends-on (#:libatty #:atty/pty #:atty/tty #:atty/mode #:atty/ui #:atty/cells
                                  #:atty/agent
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
                             (:file "panes")
                             (:file "bar")
                             (:file "frames")
                             (:file "command")
                             (:file "client")
                             (:file "note")
                             (:file "prompt")
                             (:file "queue")
                             (:file "keys")
                             (:file "main")))

(asdf:defsystem #:atty/all
                :description "Everything here"
                :depends-on (#:libatty #:atty/pty #:atty/tty #:atty/mode #:atty/ui #:atty/cells
                                  #:atty/agent #:atty))

(asdf:defsystem #:atty/test
                :description "Atty's test suite"
                :depends-on (#:atty/all #:libatty/test #:fiveam)
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
                                       (uiop:symbol-call :atty/test :run-them)))
