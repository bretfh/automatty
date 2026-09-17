(define-module (vt packages vt)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (cl-vt))

(define (vt-source-select? file stat)
  (not (member (basename file) '(".git" ".cache" "systems" "ocicl"))))

(define-public cl-vt
  (package
    (name "cl-vt")
    (version "0.0.1")
    (source (local-file "../../.." "cl-vt-source"
                        #:recursive? #t
                        #:select? vt-source-select?))
    (build-system asdf-build-system/sbcl)
    ;; no inputs, no C, no phases: the pty is libc through sb-alien, and libc is
    ;; already under every sbcl there is
    (arguments (list #:asd-systems ''("vt" "vt/pty")
                     #:tests? #f))
    (home-page "https://github.com/bretfh/cl-vt")
    (synopsis "A terminal emulator in Common Lisp")
    (description "cl-vt reads what a program writes on a terminal and keeps the
screen that means: a grid of cells, each with the face it was written in, a
cursor, a scroll region, an alternate screen and a scrollback.  It parses the
control sequences a program sends, answers the ones that expect an answer, and
turns a key event back into the bytes a program reads.  vt/pty puts a program on
a pseudo-terminal of its own, with a session and a controlling terminal, so that
an interrupt is a signal and a resize is a SIGWINCH.")
    (license license:gpl3+)))
