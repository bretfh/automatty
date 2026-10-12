(asdf:defsystem #:automatty
                :description "The language a config is written in"
                :author "Bret Horne"
                :license "GPL-3.0-or-later"
                :version "0.0.1"
                :depends-on (#:atty)
                :serial t
                :pathname "src/automatty/"
                :components ((:file "package")
                             (:file "tree")))
