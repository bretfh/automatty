(use-modules (guix profiles) (gnu packages))

(specifications->manifest
 (list "sbcl"
       "gcc-toolchain"
       "sbcl-cffi"
       "sbcl-fiveam"
       "sbcl-alexandria"
       "sbcl-babel"
       "sbcl-trivial-features"
       "sbcl-trivial-garbage"
       "sbcl-net.didierverna.asdf-flv"
       "sbcl-trivial-backtrace"))
