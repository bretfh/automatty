(load (merge-pathnames "corpus.lisp" *load-truename*))
(in-package #:vt/bench)

;; What a frame costs before a socket exists. Everything the multiplexer does
;; between a pane writing and a terminal reading is here: blit, diff, encode.
;; If the three of them together do not fit inside a frame there is no
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
         (screen (tty:make-screen :width cols :height rows))
         (was (tty:make-screen :width cols :height rows))
         (m (cells:make-cells (tty:screen-grid screen) cols rows))
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
        (cells:blit m pane 0 0 cols rows)
        (let ((mid (nanos)))
          (incf blit (- mid then))
          (let ((these (tty:screen-diff was screen gap)))
            (let ((after (nanos)))
              (incf diff (- after mid))
              (incf runs (length these))
              (dolist (r these)
                (incf cells (- (tty:run-end r) (tty:run-start r))))
              (tty:encode-frame screen these out)
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
  (format t "~&~%atty frame cost: blit, diff and encode, ~D frames each~%~%"
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


;;; The whole loop, over a real socket: a pane writing, a server composing and
;;; diffing, a wire, and a client encoding for a terminal that is a vt:term,
;;; so the bench checks what it measures as it measures it.

(defparameter +intervals+ '(0 4 8 16 33))

(defun a-path ()
  (format nil "~Aatty-bench-~D-~D" (uiop:temporary-directory)
          (sb-posix:getpid) (random 100000)))

(defstruct rig path thread client host in-write to-read)

(defun start-rig (command &key (rows 24) (cols 80) (interval 8))
  (let ((path (a-path)))
    (let ((thread (sb-thread:make-thread
                   (lambda () (mux:serve path command :rows rows :cols cols
                                         :interval interval))
                   :name "a bench server")))
      (loop repeat 500 until (probe-file path) do (sleep 0.01))
      (multiple-value-bind (in-read in-write) (sb-posix:pipe)
        (multiple-value-bind (to-read to-write) (sb-posix:pipe)
          (let ((client (mux:make-client path :fd in-read :to to-write)))
            (make-rig :path path :thread thread :client client
                      :host (vt:make-term :width cols :height rows)
                      :in-write in-write :to-read to-read)))))))

(defun rig-draw (rig)
  "Read what the client wrote for its terminal, give it to the term that is
standing in for one, and answer how many bytes that was."
  (let ((got 0))
    (loop while (pty:pty-wait (rig-to-read rig) 0)
          do (let ((said (pty:pty-read-string (rig-to-read rig) 65536)))
               (unless said (return))
               (when (zerop (length said)) (return))
               (incf got (length said))
               (vt:term-process-output (rig-host rig) said)))
    got))

(defun stop-rig (rig)
  (ignore-errors (mux:client-close (rig-client rig)))
  (ignore-errors (sb-posix:close (rig-in-write rig)))
  (ignore-errors (sb-posix:close (rig-to-read rig)))
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (sb-bsd-sockets:socket-connect socket (rig-path rig))
        (let ((wire (mux:make-wire (sb-bsd-sockets:socket-file-descriptor socket)
                                   socket)))
          (mux:wire-send wire '(:stop))
          (mux:wire-flush wire)
          (sleep 0.05)
          (mux:wire-close wire)))
    (error () nil))
  (sb-thread:join-thread (rig-thread rig) :timeout 5 :default nil)
  (ignore-errors (delete-file (rig-path rig))))

(defun loop-through (name interval)
  (let* ((rig (start-rig (format nil "cat ~A" (namestring (corpus-path name)))
                         :interval interval))
         (wire (mux::client-wire (rig-client rig)))
         (host 0) (frames 0)
         (deadline (+ (nanos) (* 6 1000000000)))
         (then (nanos)))
    (unwind-protect
         (progn
           (loop while (and (< (nanos) deadline)
                            (mux:client-going (rig-client rig)))
                 do (mux:client-step (rig-client rig) 2)
                    (let ((got (rig-draw rig)))
                      (when (plusp got) (incf host got) (incf frames))))
           (let ((wall (/ (- (nanos) then) 1d9)))
             (format t "~&  ~7A ~8D ~7,1F ~9D ~9D ~7,2F ~10,1F~%"
                     name interval (/ frames wall)
                     (if (plusp frames) (floor host frames) 0)
                     (if (plusp frames) (floor (mux:wire-in-bytes wire) frames) 0)
                     (if (plusp host)
                         (/ (mux:wire-in-bytes wire) host)
                         0d0)
                     (/ host wall 1024d0))))
      (stop-rig rig))))

(defun keystrokes (interval &key (n 120))
  ;; cat, with the line discipline echoing as it always does: one character
  ;; typed appears without waiting for a line, which is the path a shell gives
  ;; you and the one a person feels.
  (let* ((rig (start-rig "cat" :interval interval))
         (times (make-array n :element-type 'fixnum :fill-pointer 0)))
    (unwind-protect
         (progn
           (loop repeat 40 do (mux:client-step (rig-client rig) 2) (rig-draw rig))
           (dotimes (i n)
             (let ((ch (code-char (+ 33 (mod i 60))))
                   (then (nanos))
                   (saw nil))
               (pty:pty-write-string (rig-in-write rig) (string ch))
               (loop until (or saw (> (- (nanos) then) 200000000))
                     do (mux:client-step (rig-client rig) 1)
                        (rig-draw rig)
                        (when (find ch (vt:term-dump-to-string (rig-host rig)))
                          (setf saw t)))
               (when saw (vector-push (- (nanos) then) times))
               (vt:term-reset (rig-host rig))
               (pty:pty-write-string (rig-in-write rig) (string #\Return))
               (loop repeat 20 do (mux:client-step (rig-client rig) 1) (rig-draw rig))))
           (let ((sorted (sort (copy-seq times) #'<)))
             (format t "~&  ~8D ~7D ~10,2F ~10,2F ~10,2F~%"
                     interval (length sorted)
                     (/ (at sorted 0.5) 1000d0)
                     (/ (at sorted 0.99) 1000d0)
                     (if (plusp (length sorted))
                         (/ (aref sorted (1- (length sorted))) 1000d0)
                         0d0))))
      (stop-rig rig))))

(defun run-the-loop ()
  (format t "~&~%the whole loop: a pane, a server, a socket and a client~%~%")
  (format t "~&  ~7A ~8A ~7A ~9A ~9A ~7A ~10A~%"
          "corpus" "interval" "fps" "host B/f" "sock B/f" "ratio" "host KB/s")
  (dolist (name +corpora+)
    (loop-through name 8))
  (dolist (interval '(0 16 33))
    (loop-through "redraw" interval))
  (format t "~%  The ratio is what the socket carries over what the terminal reads.~%")
  (format t "  Near one says s-expressions cost about what the escapes they turn~%")
  (format t "  into cost, and the wire does not need to be binary.~%")
  (format t "~&~%what a keystroke costs: typed, to the frame that shows it~%~%")
  (format t "~&  ~8A ~7A ~10A ~10A ~10A~%" "interval" "kept" "p50 us" "p99 us" "max us")
  (dolist (interval +intervals+)
    (keystrokes interval))
  (format t "~%"))

(run-it)
(run-the-loop)
