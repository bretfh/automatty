(define-module (vt packages vt)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system asdf)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:export (cl-vt))

(define (S name) (specification->package name))

(define (vt-source-select? file stat)
  (not (member (basename file) '(".git" ".cache" "lib" "systems" "ocicl"))))

(define-public cl-vt
  (package
    (name "cl-vt")
    (version "0.0.1")
    (source (local-file "../../.." "cl-vt-source"
                        #:recursive? #t
                        #:select? vt-source-select?))
    (build-system asdf-build-system/sbcl)
    (arguments
     (list
      #:asd-systems ''("vt" "vt/pty")
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'build-owned-c
            (lambda _
              (let* ((lib (string-append #$output "/lib"))
                     (libexec (string-append #$output "/libexec"))
                     (helper (string-append libexec "/vt-pty-helper")))
                (mkdir-p lib)
                (mkdir-p libexec)
                ;; the helper execs a shell, and the one on PATH is not here
                (substitute* "c/pty-helper.c"
                  (("\"/bin/sh\"")
                   (string-append "\"" #$(file-append bash-minimal "/bin/sh")
                                  "\"")))
                (invoke "gcc" "-O2" "c/pty-helper.c" "-o" helper)
                (invoke "gcc" "-shared" "-fPIC"
                        (string-append "-DVT_PTY_HELPER=\"" helper "\"")
                        "c/pty.c" "-lutil"
                        "-o" (string-append lib "/libvt-pty.so"))
                ;; the library is named through cffi's :default, which appends
                ;; the suffix itself, so what goes in is extensionless
                (substitute* "src/pty/pty.lisp"
                  (("\"libvt-pty\"")
                   (string-append "\"" lib "/libvt-pty\""))
                  (("\\(defvar \\*helper\\* nil\\)")
                   (string-append "(defvar *helper* \"" helper "\")")))))))))
    (inputs (list (S "sbcl-cffi")))
    (home-page "https://github.com/bretfh/cl-vt")
    (synopsis "A terminal emulator in Common Lisp")
    (description "cl-vt reads what a program writes on a terminal and keeps the
screen that means: a grid of cells, each with the face it was written in, a
cursor, a scroll region, an alternate screen and a scrollback.  It parses the
control sequences a program sends, answers the ones that expect an answer, and
turns a key event back into the bytes a program reads.  vt/pty puts a program on
a pseudo-terminal of its own to be read from.")
    (license license:gpl3+)))
