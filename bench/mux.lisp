(load (merge-pathnames "corpus.lisp" *load-truename*))
(in-package #:vt/bench)

;; What a frame costs before a socket exists. Everything the multiplexer does
;; between a pane writing and a terminal reading is here -- blit, diff, encode
;; -- and if the three of them together do not fit inside a frame there is no
;; arrangement of servers and clients that rescues it.
(declaim (inline nanos))
(defun nanos ()
  (multiple-value-bind (sec nsec) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* sec 1000000000) nsec)))

(defparameter +sizes+ '((80 . 24) (120 . 40) (200 . 50) (300 . 80)))
(defparameter +gaps+ '(1 4 8 16))
(defparameter +frames+ 400)

(defun at (sorted fraction)
  (if (zerop (length sorted))
      0
      (aref sorted (min (1- (length sorted)) (floor (* fraction (length sorted)))))))

(defun said-in (path bytes)
  (with-open-file (in path :external-format :latin-1)
    (let ((s (make-string bytes)))
      (subseq s 0 (read-sequence s in)))))

;; A pane is fed a slice of the corpus per frame, which is what a program
;; writing between two frames looks like from the server's side.
(defun slices (said n)
  (let* ((each (max 1 (floor (length said) n)))
         (out (make-array n)))
    (dotimes (i n out)
      (setf (aref out i)
            (subseq said (* i each) (min (length said) (* (1+ i) each)))))))

(defun frames (name said cols rows gap)
  (let* ((pane (vt:make-term :width cols :height rows))
         (screen (mux:make-screen :width cols :height rows))
         (was (mux:make-screen :width cols :height rows))
         (out (make-string-output-stream))
         (feed (slices said +frames+))
         (blit 0) (diff 0) (enc 0)
         (bytes 0) (cells 0) (runs 0)
         (whole (make-array +frames+ :element-type 'fixnum :fill-pointer 0))
         (weight 0))
    (sb-ext:gc :full t)
    (dotimes (i +frames+)
      (vt:term-process-output pane (aref feed i))
      ;; only the frame is weighed. The pane parsing what the program wrote is
      ;; measured by make latency and is not what this is asking about.
      (let ((mark (sb-ext:get-bytes-consed))
            (then (nanos)))
        (mux:screen-blit screen pane)
        (let ((mid (nanos)))
          (incf blit (- mid then))
          (let ((these (mux:screen-diff was screen gap)))
            (let ((after (nanos)))
              (incf diff (- after mid))
              (incf runs (length these))
              (dolist (r these)
                (incf cells (- (mux:run-end r) (mux:run-start r))))
              (mux:encode-frame screen these out)
              (let ((done (nanos)))
                (incf enc (- done after))
                (vector-push (- done then) whole)
                (incf weight (- (sb-ext:get-bytes-consed) mark)))))))
      (incf bytes (length (get-output-stream-string out))))
    (let ((sorted (sort (copy-seq whole) #'<)))
      (format t "~&  ~7A ~8A ~4D ~8,1F ~8,1F ~8,1F ~9,1F ~9,1F ~8D ~8D ~9D~%"
              name (format nil "~Dx~D" cols rows) gap
              (/ blit +frames+ 1000d0)
              (/ diff +frames+ 1000d0)
              (/ enc +frames+ 1000d0)
              (/ (at sorted 0.99) 1000d0)
              (/ (aref sorted (1- (length sorted))) 1000d0)
              (floor bytes +frames+)
              (floor cells +frames+)
              (floor weight +frames+)))))

(defun run-it ()
  (make-corpora)
  (format t "~&~%cl-vt frame cost -- blit, diff and encode, ~D frames each~%~%"
          +frames+)
  (format t "~&  ~7A ~8A ~4A ~8A ~8A ~8A ~9A ~9A ~8A ~8A ~9A~%"
          "corpus" "size" "gap" "blit us" "diff us" "enc us" "p99 us" "max us"
          "bytes/f" "cells/f" "consed/f")
  (dolist (name +corpora+)
    (let ((said (said-in (corpus-path name) (* 8 1024 1024))))
      (dolist (size +sizes+)
        (frames name said (car size) (cdr size) 4))))
  (format t "~%")
  (let ((said (said-in (corpus-path "redraw") (* 8 1024 1024))))
    (dolist (gap +gaps+)
      (frames "redraw" said 80 24 gap)))
  (format t "~%  A frame at 60 a second has 16666 us. Whole-frame p99 is the~%")
  (format t "  column that has to fit in it, and it is the only one that does.~%~%"))

(run-it)
