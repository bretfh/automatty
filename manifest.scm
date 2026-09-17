(use-modules (guix profiles) (gnu packages))

;; cl-vt itself needs an sbcl and nothing else. The rest is the test suite.
(specifications->manifest
 (list "sbcl"
       "sbcl-fiveam"
       "sbcl-alexandria"
       "sbcl-net.didierverna.asdf-flv"
       "sbcl-trivial-backtrace"))
