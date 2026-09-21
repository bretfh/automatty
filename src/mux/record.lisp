;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defparameter +record-rows+ 50)
(defparameter +record-cols+ 160)
(defparameter +record-quiet+ 3000)
(defparameter +record-optional-quiet+ 1500)
(defparameter +record-settle+ 600)
(defparameter +record-patience+ 180)
(defparameter +record-gap+ 400)

(defstruct recording fd pid term decoder bytes (heard 0))

(defun recording-ms ()
  (floor (get-internal-real-time) (floor internal-time-units-per-second 1000)))

(defun recording-drain (r &optional (ms 50))
  (when (pty:pty-wait (recording-fd r) ms)
    (let ((said (pty:pty-read-string (recording-fd r) 262144)))
      (when (and said (plusp (length said)))
        (loop :for ch :across said :do (vector-push-extend (char-code ch) (recording-bytes r)))
        (setf (recording-heard r) (recording-ms))
        (term:term-process-output (recording-term r)
                                  (term:decode-utf-8 (recording-decoder r) said))))))

(defun recording-quiet (r)
  (- (recording-ms) (recording-heard r)))

(defun recording-wait (r ms)
  (let ((until (+ (recording-ms) ms)))
    (loop :while (< (recording-ms) until) :do (recording-drain r 20))))

(defun recording-send (r keys)
  (loop :for (chunk . more) :on keys
        :do (pty:pty-write-string (recording-fd r) chunk :utf-8)
            (when more (recording-wait r +record-gap+))))

(defun recording-version (r reader)
  (loop :repeat 200
        :for version := (agent:version-from
                         reader
                         (mapcar (lambda (p) (getf p :path))
                                 (pty:group-processes (pty:pty-foreground (recording-fd r)))))
        :when version :return version
        :do (recording-drain r 50)))

(defun await-step (r reader want optional)
  (let ((deadline (+ (recording-ms) (* 1000 +record-patience+)))
        (since nil))
    (loop
      (recording-drain r)
      (let ((seen (agent:observe reader (recording-term r))))
        (setf since (and (eq want (getf seen :screen)) (or since (recording-ms))))
        (cond ((and since (>= (- (recording-ms) since) +record-settle+))
               (return (values :observed seen)))
              ((and optional (not (eq want (getf seen :screen)))
                    (>= (recording-quiet r) +record-optional-quiet+))
               (return (values :skipped seen)))
              ((and (not optional) (>= (recording-quiet r) +record-quiet+))
               (return (values :settled seen)))
              ((> (recording-ms) deadline)
               (return (values :timeout seen))))))))

(defun record-agent (name &key (into "readers/") (say *standard-output*))
  (let* ((known (or (agent:reader-named name) (error "there is no reader called ~A" name)))
         (home (format nil "/tmp/atty-record/~(~A~)/" name))
         (launch (or (agent::reader-launch known) (list (first (agent::reader-programs known))))))
    (ensure-directories-exist home)
    (multiple-value-bind (fd pid)
        (pty:spawn-pty-process (format nil "cd '~A' && exec~{ '~A'~}" home launch)
                               :rows +record-rows+ :cols +record-cols+)
      (let ((r (make-recording :fd fd :pid pid
                               :decoder (term:make-decoder)
                               :bytes (make-array 0 :element-type '(unsigned-byte 8)
                                                    :adjustable t :fill-pointer 0))))
        (setf (recording-term r)
              (term:make-term :width +record-cols+ :height +record-rows+
                              :input-fn (lambda (term said)
                                          (declare (ignore term))
                                          (ignore-errors (pty:pty-write-string fd said)))))
        (unwind-protect
             (let* ((version (or (recording-version r known)
                                 (error "~A is running but its version could not be read" (first launch))))
                    (reader (agent:nearest-reader name version))
                    (dir (merge-pathnames (format nil "~(~A~)/~A/" name version)
                                          (uiop:ensure-directory-pathname into)))
                    (before nil)
                    (steps nil))
               (format say "~&~A ~A, read with ~A ~{~A~^ ~}~%" name version
                       (agent:reader-name reader) (agent::reader-versions reader))
               (dolist (step (agent::reader-scenario reader))
                 (let* ((want (getf step :screen))
                        (do (getf step :do))
                        (keys (and do (agent:step-keys reader before do))))
                   (when keys (recording-send r keys))
                   (multiple-value-bind (cut seen)
                       (await-step r reader want (getf step :optional))
                     (push (append (list :screen want :cut cut)
                                   (unless (eq cut :skipped)
                                     (list :at (fill-pointer (recording-bytes r)) :saw seen))
                                   (when keys (list :via do :sent keys)))
                           steps)
                     (format say "  ~(~12A~) ~(~A~)~@[, saw ~(~A~)~]~%" want cut
                             (and (not (eq want (getf seen :screen))) (getf seen :screen)))
                     (when seen (setf before seen))
                     (when (eq cut :timeout) (return)))))
               (let ((expect (list :corpus 1 :agent (string-downcase name) :version version
                                   :width +record-cols+ :height +record-rows+
                                   :alt-screen (and (term:term-in-alt-screen (recording-term r)) t)
                                   :launch launch
                                   :steps (nreverse steps))))
                 (agent:save-corpus dir expect (coerce (recording-bytes r)
                                                       '(simple-array (unsigned-byte 8) (*))))
                 (format say "~&wrote ~A~%" (namestring dir))
                 (values dir expect)))
          (ignore-errors (pty:pty-close fd))
          (ignore-errors (pty:pty-reap pid)))))))

(defun extend-reader (dir expect)
  (let* ((said (nthcdr 2 expect))
         (name (getf said :agent))
         (version (getf said :version))
         (path (merge-pathnames "reader.lisp" dir))
         (exact (agent:reader-for-version name version))
         (nearest (agent:nearest-reader name version)))
    (when (and (null exact) nearest (not (probe-file path)))
      (let ((form (list (intern (string-upcase name) :keyword)
                        :from (first (agent::reader-versions nearest))
                        :versions (list version))))
        (with-open-file (out path :direction :output :external-format :utf-8)
          (with-standard-io-syntax
            (let ((*print-readably* nil) (*print-case* :downcase))
              (prin1 form out)
              (terpri out))))
        (agent:register-reader form)
        path))))

(defun corpus-dirs (root)
  (loop :for expect :in (directory (merge-pathnames "*/*/expect.lisp"
                                                    (uiop:ensure-directory-pathname root)))
        :collect (uiop:pathname-directory-pathname expect)))

(defun verify-corpus (dir &key (say *standard-output*))
  (multiple-value-bind (expect octets) (agent:load-corpus dir)
    (let* ((name (getf (nthcdr 2 expect) :agent))
           (version (getf (nthcdr 2 expect) :version))
           (exact (agent:reader-for-version name version))
           (reader (or exact (agent:nearest-reader name version))))
      (cond
        ((null reader)
         (format say "~&~A ~A: no reader~%" name version)
         nil)
        (t
         (let ((verdict (agent:replay reader expect octets)))
           (format say "~&~A ~A, read with ~A ~{~A~^ ~}~:[ (not its own)~;~]~%"
                   name version (agent:reader-name reader) (agent::reader-versions reader) exact)
           (dolist (row verdict)
             (format say "  ~(~12A~) ~(~A~)~{ ~S~}~%" (first row) (second row) (cddr row)))
           (agent:verdict-passed-p verdict)))))))
