(define-module (atty packages atty)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages lisp-xyz)
  #:export (atty))

(define (atty-source-select? file stat)
  (not (member (basename file) '(".git" ".cache" "systems" "ocicl"))))

(define-public atty
  (package
    (name "atty")
    (version "0.0.1")
    (source (local-file "../../.." "atty-source"
                        #:recursive? #t
                        #:select? atty-source-select?))
    (build-system gnu-build-system)
    ;; sbcl is a build-time tool, not a runtime dependency: save-lisp-and-die
    ;; embeds the runtime it needs into the executable it writes
    (native-inputs (list sbcl sbcl-cl-ppcre))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (setenv "CL_SOURCE_REGISTRY"
                      (string-append (getcwd) "//:"
                                     #$(this-package-native-input "sbcl-cl-ppcre")
                                     "/share/common-lisp//"))
              (setenv "ATTY_OUT" "atty")
              (invoke "sbcl" "--non-interactive" "--load" "build.lisp")))
          (delete 'check)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (mkdir-p bin)
                (copy-file "atty" (string-append bin "/atty"))
                (chmod (string-append bin "/atty") #o755)))))))
    (home-page "https://github.com/bretfhorne/atty")
    (synopsis "A terminal emulator and multiplexer")
    (description "atty opens a pty, spawns a shell, reads your keyboard and
draws to a real screen, holding several sessions and several panes at once
behind one server, so a client can attach, detach, and come back.  It is
built on libatty, the terminal protocol engine underneath it, also packaged
in this channel.")
    (license license:gpl3+)))
