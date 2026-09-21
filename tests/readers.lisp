(in-package #:atty/test)

(def-suite readers :in all)
(in-suite readers)

(test every-recorded-version-reads-the-way-it-was-recorded
  (dolist (dir (mux:corpus-dirs (asdf:system-relative-pathname :atty "readers/")))
    (multiple-value-bind (expect octets) (agent:load-corpus dir)
      (let* ((said (nthcdr 2 expect))
             (reader (agent:reader-for-version (getf said :agent) (getf said :version))))
        (is-true reader "~A ~A was recorded and has no reader of its own"
                 (getf said :agent) (getf said :version))
        (when reader
          (let ((verdict (agent:replay reader expect octets)))
            (is-true (agent:verdict-passed-p verdict) "~A ~A: ~S"
                     (getf said :agent) (getf said :version) verdict)))))))
