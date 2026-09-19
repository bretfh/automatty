(asdf:defsystem #:libvtx
                :description "An extended VT100 terminal emulator"
                :author "Bret Horne"
                :license "GPL-3.0-or-later"
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
                :in-order-to ((asdf:test-op (asdf:test-op #:libvtx/test))))

(asdf:defsystem #:libvtx/test
                :description "Libvtx's test suite"
                :depends-on (#:libvtx #:fiveam)
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
                                       (uiop:symbol-call :libvtx/test :run-them)))
