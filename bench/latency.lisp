(load (merge-pathnames "corpus.lisp" *load-truename*))
(in-package #:term/bench)

;; get-internal-real-time steps by 4 ms on this kernel, which is a hundred times
;; coarser than the thing being measured. Everything here is nanoseconds off the
;; monotonic clock.
(declaim (inline nanos))
(defun nanos ()
  ;; the internal real time is monotonic on every platform sbcl runs on, and
  ;; sb-unix names its clocks differently on each: this asks nothing of them
  (* (get-internal-real-time)
     #.(floor 1000000000 internal-time-units-per-second)))

(defparameter +read+ 4096)
(defparameter +passes+ 3)
(defparameter +nurseries+ '(2 8 32 161))

(defvar *gc-pauses* nil)

(defun watch-gc ()
  (setf *gc-pauses* (make-array 64 :adjustable t :fill-pointer 0))
  (let ((last sb-ext:*gc-run-time*))
    (setf sb-ext:*after-gc-hooks*
          (list (lambda ()
                  (vector-push-extend (- sb-ext:*gc-run-time* last) *gc-pauses*)
                  (setf last sb-ext:*gc-run-time*))))))

(defun at (sorted fraction)
  (if (zerop (length sorted))
      0
      (aref sorted (min (1- (length sorted)) (floor (* fraction (length sorted)))))))

(defun reads-of (term said buffer times)
  (let ((len (length said)))
    (setf (fill-pointer times) 0)
    (do ((i 0 (+ i +read+)))
        ((>= i len) times)
      (let* ((end (min len (+ i +read+)))
             (n (- end i))
             (chunk (if (= n +read+) buffer (subseq buffer 0 n))))
        (replace buffer said :start2 i :end2 end)
        (let ((then (nanos)))
          (term:term-process-output term chunk)
          (vector-push (- (nanos) then) times))))))

(defun sweep (said)
  (let ((buffer (make-string +read+))
        (room (ceiling (length said) +read+)))
    (dolist (mb +nurseries+)
      (setf (sb-ext:bytes-consed-between-gcs) (* mb 1024 1024))
      (sb-ext:gc :full t)
      (let ((all (make-array (* +passes+ (1+ room)) :element-type 'fixnum
                                                    :fill-pointer 0))
            (times (make-array (1+ room) :element-type 'fixnum :fill-pointer 0))
            (wall 0))
        ;; one terminal for every pass. A terminal is a long-lived thing, and
        ;; making a fresh one per pass measures allocating a scrollback, which
        ;; is not what a terminal spends its life doing.
        (let ((term (term:make-term :width 80 :height 24)))
          (reads-of term said buffer times)
          (watch-gc)
          (let ((then (nanos)))
            (dotimes (i +passes+)
              (reads-of term said buffer times)
              (loop :for tk :across times :do (vector-push tk all)))
            (setf wall (- (nanos) then))))
        (setf sb-ext:*after-gc-hooks* nil)
        (let ((reads (sort (copy-seq all) #'<))
              (pauses (sort (copy-seq *gc-pauses*) #'<)))
          (format t "~&  ~7A ~6,1F ~8,1F ~8,1F ~8,1F ~9,1F ~7D ~9,1F ~9,1F~%"
                  (format nil "~D MB" mb)
                  (/ (* +passes+ (length said)) (/ wall 1d9) 1024d0 1024d0)
                  (/ (at reads 0.5) 1000d0) (/ (at reads 0.99) 1000d0)
                  (/ (at reads 0.999) 1000d0) (/ (at reads 1.0) 1000d0)
                  (length pauses)
                  (/ (at pauses 0.5) 1000d0) (/ (at pauses 1.0) 1000d0)))))))

(defun live-bytes (thunk)
  (sb-ext:gc :full t)
  (let* ((before (sb-kernel:dynamic-usage))
         (kept (funcall thunk))
         (ignore (sb-ext:gc :full t))
         (after (sb-kernel:dynamic-usage)))
    (declare (ignore ignore))
    (values (- after before) kept)))

(defun a-full-term (lines)
  (let ((term (term:make-term :width 80 :height 24 :max-scrollback lines))
        (line (format nil "~A~C" (make-string 79 :initial-element #\x) #\Newline)))
    (dotimes (i (+ lines 24) term)
      (term:term-process-output term line))))

(defun run ()
  (make-corpora)
  (format t "~&~%One 4 KB read, in microseconds, against the size of the nursery.~%")
  (format t "~&Same work each row; only (bytes-consed-between-gcs) moves.~%")
  (dolist (corpus (corpora))
    (destructuring-bind (name . path) corpus
      (format t "~&~%~A~%" name)
      (format t "~&  ~7A ~6A ~8A ~8A ~8A ~9A ~7A ~9A ~9A~%"
              "nursery" "MB/s" "p50" "p99" "p99.9" "worst" "gcs" "gc p50" "gc worst")
      (sweep (slurp path))))
  (setf (sb-ext:bytes-consed-between-gcs) (* 161 1024 1024))
  (format t "~&~%What a terminal weighs~%")
  (multiple-value-bind (bytes kept)
      (live-bytes (lambda () (loop :repeat 100
                                   :collect (term:make-term :width 80 :height 24))))
    (format t "~&  ~18A ~8,1F KB~%" "80x24, no history" (/ bytes 100 1024d0))
    (length kept))
  (dolist (lines '(2000 10000))
    (multiple-value-bind (bytes kept) (live-bytes (lambda () (a-full-term lines)))
      (format t "~&  ~18A ~8,2F MB   ~,1F bytes a cell~%"
              (format nil "+ ~D lines" lines)
              (/ bytes 1024d0 1024d0)
              (/ bytes (* 80 (+ lines 24))))
      (term:term-width kept)))
  (format t "~&~%")
  (finish-output))

(run)
