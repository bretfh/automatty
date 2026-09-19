(define-module (vtx packages libvtx)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:export (libvtx))

(define (libvtx-source-select? file stat)
  (not (member (basename file) '(".git" ".cache" "systems" "ocicl"))))

(define-public libvtx
  (package
    (name "libvtx")
    (version "0.0.1")
    (source (local-file "../../.." "vtx-source"
                        #:recursive? #t
                        #:select? libvtx-source-select?))
    (build-system asdf-build-system/sbcl)
    ;; no inputs, no C, no phases: this is bytes in, a grid of cells out
    (arguments (list #:asd-systems ''("libvtx")
                     #:tests? #f))
    (home-page "https://github.com/bretfhorne/vtx")
    (synopsis "A VT100-and-extensions terminal emulator")
    (description "libvtx reads what a program writes on a terminal and keeps the
screen that means: a grid of cells, each with the face it was written in, a
cursor, a scroll region, an alternate screen and a scrollback.  It parses the
control sequences a program sends, answers the ones that expect an answer, and
turns a key event back into the bytes a program reads.  It opens no pty, reads
no keyboard, and draws nothing: what uses it decides all of that.  vtx, in
this same repository, is such a thing.")
    (license license:gpl3+)))
