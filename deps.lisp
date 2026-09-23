;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;; The lisp systems atty needs, fetched into ./ocicl from what ocicl.csv
;;;; pins. ocicl.csv is ocicl's lockfile, and ocicl is what changes it: `ocicl
;;;; install cl-ppcre' or `ocicl latest'. Fetching what it pins takes no ocicl:
;;;; each line names an image on ghcr.io by digest, the image has one layer, and
;;;; the layer is a tarball of the system's directory. curl and tar are enough,
;;;; and they are on every machine this builds on, which the ocicl binary is not:
;;;; it wants a newer glibc than the linux a release is built on has.
;;;;
;;;;   sbcl --non-interactive --load deps.lisp

(defun split (string char)
  (loop :with start := 0
        :for at := (position char string :start start)
        :collect (string-trim " " (subseq string start at))
        :while at
        :do (setf start (1+ at))))

(defun run (program &rest arguments)
  "PROGRAM's standard output as a string, or an error."
  (let* ((out (make-string-output-stream))
         (process (sb-ext:run-program program arguments :search t :output out :error nil)))
    (unless (eql 0 (sb-ext:process-exit-code process))
      (error "~A ~{~A~^ ~} failed" program arguments))
    (get-output-stream-string out)))

(defun json-string-after (key json)
  "The string value after KEY in JSON, without reading json: what one token
answers, and what one manifest says of its layer, is one string each."
  (let ((at (search (format nil "\"~A\"" key) json)))
    (when at
      (let* ((open (position #\" json :start (+ at (length key) 3)))
             (close (and open (position #\" json :start (1+ open)))))
        (and open close (subseq json (1+ open) close))))))

(defun fetch-system (repository digest into)
  "The one layer of the image REPOSITORY@DIGEST on ghcr.io, unpacked under INTO."
  (let* ((token (json-string-after
                 "token" (run "curl" "-fsSL"
                              (format nil "https://ghcr.io/token?scope=repository:~A:pull&service=ghcr.io"
                                      repository))))
         (manifest (run "curl" "-fsSL" "-H" (format nil "Authorization: Bearer ~A" token)
                        "-H" "Accept: application/vnd.oci.image.manifest.v1+json"
                        (format nil "https://ghcr.io/v2/~A/manifests/~A" repository digest)))
         (layer (or (json-string-after "digest" (subseq manifest (search "\"layers\"" manifest)))
                    (error "~A@~A has no layer" repository digest)))
         (tarball (format nil "~A~A.tar.gz" into (subseq layer (1+ (position #\: layer))))))
    (run "curl" "-fsSL" "-H" (format nil "Authorization: Bearer ~A" token)
         "-o" tarball (format nil "https://ghcr.io/v2/~A/blobs/~A" repository layer))
    (unwind-protect (run "tar" "xzf" tarball "-C" into)
      (delete-file tarball))))

(defun deps (&key (csv "ocicl.csv") (into "ocicl/"))
  (ensure-directories-exist into)
  (let ((wanted nil))
    (with-open-file (in csv)
      (loop :for line := (read-line in nil)
            :while line
            :unless (zerop (length (string-trim " " line)))
              :do (destructuring-bind (name reference asd) (split line #\,)
                    (let* ((at (position #\@ reference))
                           (repository (subseq reference (length "ghcr.io/") at))
                           (digest (subseq reference (1+ at)))
                           (dir (subseq asd 0 (position #\/ asd))))
                      (pushnew dir wanted :test #'string=)
                      (if (probe-file (format nil "~A~A/" into dir))
                          (format t "~&  ~A is here~%" dir)
                          (progn
                            (format t "~&  ~A: fetching ~A~%" name dir)
                            (fetch-system repository digest into)))))))
    ;; what ocicl.csv no longer pins is not a dependency, and asdf must not find it
    (dolist (there (directory (format nil "~A*/" into)))
      (let ((dir (car (last (pathname-directory there)))))
        (unless (member dir wanted :test #'string=)
          (format t "~&  ~A is no longer pinned; removing it~%" dir)
          (sb-ext:delete-directory there :recursive t))))))

(deps)
