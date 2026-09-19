(use-modules (guix profiles) (gnu packages))

(specifications->manifest
 (list "sbcl"
       "sbcl-fiveam"
       "sbcl-alexandria"
       "sbcl-net.didierverna.asdf-flv"
       "sbcl-trivial-backtrace"))
