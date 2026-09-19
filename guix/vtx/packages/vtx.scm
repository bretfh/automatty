(define-module (vtx packages vtx)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages lisp)
  #:export (vtx))

(define (vtx-source-select? file stat)
  (not (member (basename file) '(".git" ".cache" "systems" "ocicl"))))

(define-public vtx
  (package
    (name "vtx")
    (version "0.0.1")
    (source (local-file "../../.." "vtx-source"
                        #:recursive? #t
                        #:select? vtx-source-select?))
    (build-system gnu-build-system)
    ;; sbcl is a build-time tool, not a runtime dependency: save-lisp-and-die
    ;; embeds the runtime it needs into the executable it writes
    (native-inputs (list sbcl))
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda _
              (setenv "CL_SOURCE_REGISTRY" (string-append (getcwd) "//"))
              (setenv "VTX_OUT" "vtx")
              (invoke "sbcl" "--non-interactive" "--load" "build.lisp")))
          (delete 'check)
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
                (mkdir-p bin)
                (copy-file "vtx" (string-append bin "/vtx"))
                (chmod (string-append bin "/vtx") #o755)))))))
    (home-page "https://github.com/bretfhorne/vtx")
    (synopsis "A terminal emulator and multiplexer")
    (description "vtx opens a pty, spawns a shell, reads your keyboard and
draws to a real screen, holding several sessions and several panes at once
behind one server, so a client can attach, detach, and come back.  It is
built on libvtx, the terminal protocol engine underneath it, also packaged
in this channel.")
    (license license:gpl3+)))
