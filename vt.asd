(asdf:defsystem #:vt
                :description "A terminal: what a program wrote, and the screen
that means"
                :author "Bret Horne"
                :license "GPL"
                :version "0.0.1"
                :depends-on ()
                :serial t
                :pathname "src/term/"
                :components ((:file "package")
                             (:file "types")
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

(asdf:defsystem #:vt/all
                :description "Every system cl-vt ships"
                :depends-on (#:vt #:vt/pty))

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
                             (:file "pty"))
                :perform (asdf:test-op (o c)
                           (uiop:symbol-call :vt/test :run-them)))
