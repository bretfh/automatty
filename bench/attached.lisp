(load (merge-pathnames "corpus.lisp" *load-truename*))
(in-package #:term/bench)

;; Both multiplexers doing the whole job: a program writing hard, a grid kept,
;; a frame worked out and escapes put on a terminal somebody is reading. The
;; terminal is one of ours, so the same harness drains both and the same
;; emulator reads back what each drew, which measures them and checks them
;; against each other at the same time.

(declaim (inline nanos))
(defun nanos ()
  (multiple-value-bind (sec nsec) (sb-unix:clock-gettime sb-unix:clock-monotonic)
    (+ (* sec 1000000000) nsec)))

(defparameter +ticks+ 100)
(defparameter +rows+ 48)
(defparameter +cols+ 132)
(defparameter +ready+ "BENCH-IS-READY")
(defparameter +began+ "BENCH-IS-GOING")
(defparameter +ended+ "BENCH-IS-DONE")

#-darwin
(progn
(defun stat-fields (pid)
  (ignore-errors
   (with-open-file (in (format nil "/proc/~D/stat" pid) :if-does-not-exist nil)
     (when in
       (let* ((line (read-line in nil))
              (after (position #\) line :from-end t)))
         (when after
           (let ((out nil) (at (+ after 2)))
             (loop while (< at (length line))
                   do (let ((space (or (position #\Space line :start at)
                                       (length line))))
                        (push (subseq line at space) out)
                        (setf at (1+ space))))
             (coerce (nreverse out) 'vector))))))))

(defun ppid-of (pid)
  (let ((f (stat-fields pid)))
    (when (and f (> (length f) 1)) (parse-integer (aref f 1) :junk-allowed t))))

(defun cpu-of (pid)
  (let ((f (stat-fields pid)))
    (if (and f (> (length f) 12))
        (/ (+ (or (parse-integer (aref f 11) :junk-allowed t) 0)
              (or (parse-integer (aref f 12) :junk-allowed t) 0))
           (float +ticks+ 1d0))
        0d0)))

(defun rss-of (pid)
  (or (ignore-errors
       (with-open-file (in (format nil "/proc/~D/status" pid) :if-does-not-exist nil)
         (when in
           (loop for line = (read-line in nil)
                 while line
                 when (and (> (length line) 6) (string= "VmRSS:" line :end2 6))
                   return (parse-integer line :start 6 :junk-allowed t)))))
      0))

(defun every-pid ()
  (loop for path in (directory "/proc/*/")
        for name = (car (last (pathname-directory path)))
        for pid = (and (stringp name) (parse-integer name :junk-allowed t))
        when pid collect pid)))

#+darwin
(progn
  (sb-alien:define-alien-routine ("proc_listpids" %listpids) sb-alien:int
    (type sb-alien:unsigned-int) (info sb-alien:unsigned-int)
    (buffer sb-alien:system-area-pointer) (size sb-alien:int))

  (sb-alien:define-alien-routine ("proc_pidinfo" %pidinfo) sb-alien:int
    (pid sb-alien:int) (flavor sb-alien:int) (arg sb-alien:unsigned-long)
    (buffer sb-alien:system-area-pointer) (size sb-alien:int))

  (sb-alien:define-alien-routine ("mach_timebase_info" %timebase) sb-alien:int
    (info sb-alien:system-area-pointer))

  (defparameter +nanos-a-tick+
    (let ((info (make-array 2 :element-type '(unsigned-byte 32))))
      (sb-sys:with-pinned-objects (info)
        (%timebase (sb-sys:vector-sap info)))
      (/ (aref info 0) (aref info 1))))

  (defun pid-info (pid flavor offset)
    (let ((buf (make-array 256 :element-type '(unsigned-byte 8))))
      (sb-sys:with-pinned-objects (buf)
        (and (plusp (%pidinfo pid flavor 0 (sb-sys:vector-sap buf) (length buf)))
             (sb-sys:sap-ref-64 (sb-sys:vector-sap buf) offset)))))

  (defun ppid-of (pid)
    (let ((buf (make-array 256 :element-type '(unsigned-byte 8))))
      (sb-sys:with-pinned-objects (buf)
        (and (plusp (%pidinfo pid 3 0 (sb-sys:vector-sap buf) (length buf)))
             (sb-sys:sap-ref-32 (sb-sys:vector-sap buf) 16)))))

  (defun cpu-of (pid)
    (let ((user (pid-info pid 4 16))
          (system (pid-info pid 4 24)))
      (if (and user system)
          (/ (* (+ user system) +nanos-a-tick+) 1d9)
          0d0)))

  (defun rss-of (pid)
    (let ((bytes (pid-info pid 4 8)))
      (if bytes (floor bytes 1024) 0)))

  (defun every-pid ()
    (let ((pids (make-array 16384 :element-type '(signed-byte 32))))
      (sb-sys:with-pinned-objects (pids)
        (let ((bytes (%listpids 1 0 (sb-sys:vector-sap pids) (* 4 (length pids)))))
          (loop for i below (max 0 (floor bytes 4))
                for pid = (aref pids i)
                when (plusp pid) collect pid))))))


(defun pids-under (root &optional also)
  "ROOT, everything descended from it, and whatever else ALSO names. A tmux server
puts itself outside the tree that started it, so it has to be asked for."
  (let* ((all (every-pid))
         (parents (make-hash-table))
         (kept (make-hash-table)))
    (dolist (pid all)
      (let ((up (ppid-of pid)))
        (when up (push pid (gethash up parents)))))
    (labels ((walk (pid)
               (unless (gethash pid kept)
                 (setf (gethash pid kept) t)
                 (mapc #'walk (gethash pid parents)))))
      (walk root)
      (dolist (pid (and also (funcall also))) (when pid (walk pid))))
    (loop for pid being the hash-keys of kept collect pid)))

(defun spent (pids) (reduce #'+ pids :key #'cpu-of :initial-value 0d0))
(defun weighs (pids) (reduce #'+ pids :key #'rss-of :initial-value 0))

(defun script (name text)
  (let ((path (merge-pathnames (format nil "~A.sh" name) (corpus-dir))))
    (with-open-file (out path :direction :output :if-exists :supersede)
      (write-string text out))
    (namestring path)))

(defparameter +look+ 50000000)

(defun go-path ()
  (namestring (merge-pathnames "go" (corpus-dir))))

(defun watch-one (command &key also (seconds 150))
  "Run COMMAND on a terminal of ours and read what it drew. Answers what it cost
between the two markers: seconds, bytes it wrote, cpu seconds, and peak rss.

Reading is the only thing this does in a hurry. Looking at the screen and
walking /proc are done on a clock, because doing either per read leaves the
terminal unread and the program blocked on writing to it, which measures the
harness rather than what it was pointed at."
  (ignore-errors (delete-file (go-path)))
  (multiple-value-bind (fd pid) (pty:spawn-pty-process command :rows +rows+
                                                               :cols +cols+)
    (let ((term (term:make-term :width +cols+ :height +rows+))
          (decoder (term:make-decoder))
          (deadline (+ (nanos) (* seconds 1000000000)))
          (look 0)
          (weigh 0)
          (pids nil)
          (bytes 0) (began nil) (then 0) (cpu 0d0) (rss 0))
      (let ((answer
              (unwind-protect
                   (loop
             (let ((now (nanos)))
               (when (> now deadline) (return))
               (when (pty:pty-wait fd 5)
                 (let ((said (pty:pty-read-string fd 262144)))
                   (unless said (return))
                   (incf bytes (length said))
                   (term:term-process-output term (term:decode-utf-8 decoder said))))
               (when (> now look)
                 (setf look (+ now +look+))
                 (let ((screen (term:term-dump-to-string term)))
                   (cond
                     ;; the pane waits until the terminal it is drawing on is
                     ;; actually up: a client that takes seconds to start would
                     ;; otherwise attach to a program that had already finished.
                     ;; Letting it go is also the moment to start the clock:
                     ;; a marker it printed on its way past would have scrolled
                     ;; off the screen before anybody looked.
                     ((not began)
                      (when (search +ready+ screen)
                        (with-open-file (out (go-path) :direction :output
                                                       :if-exists :supersede)
                          (write-line "go" out))
                        ;; who to watch is settled once. Walking /proc is a
                        ;; glob and six hundred files, and doing it while the
                        ;; program is writing measures the harness.
                        (setf pids (pids-under pid also)
                              began t
                              then (nanos)
                              cpu (spent pids)
                              rss (weighs pids)
                              weigh (+ (nanos) 1000000000)
                              bytes 0)))
                     ((search +ended+ screen)
                      (return (list (/ (- (nanos) then) 1d9)
                                    bytes
                                    (- (spent pids) cpu)
                                    (max rss (weighs pids))
                                    term)))
                     ((> now weigh)
                      (setf weigh (+ now 1000000000)
                            rss (max rss (weighs pids)))))))))
                (ignore-errors (pty:pty-close fd))
                (ignore-errors (pty:pty-reap pid 1)))))
        (if answer
            (values-list answer)
            (values nil bytes 0d0 rss term))))))

(defparameter +megabytes-in+ 8)
(defparameter +rounds+ 5)

(defun middle (xs)
  (let ((sorted (sort (copy-seq xs) #'<)))
    (aref sorted (floor (length sorted) 2))))

(defun slice-path (corpus)
  (merge-pathnames (format nil "~A-slice.txt" corpus) (corpus-dir)))

(defun slice (corpus)
  "The first few megabytes of a corpus, ending on a line, so a run finishes
while somebody waits and the last thing it wrote is a whole thing. A cut through
the middle of an escape sequence leaves the terminal waiting for the rest of it,
and swallows whatever is said next."
  (let ((path (slice-path corpus))
        (want (* +megabytes-in+ 1024 1024)))
    (unless (probe-file path)
      (with-open-file (in (corpus-path corpus) :element-type '(unsigned-byte 8))
        (with-open-file (out path :direction :output :if-exists :supersede
                                  :element-type '(unsigned-byte 8))
          (let ((buf (make-array 65536 :element-type '(unsigned-byte 8)))
                (left want)
                (kept 0))
            (loop while (plusp left)
                  for n = (read-sequence buf in :end (min 65536 left))
                  while (plusp n)
                  do (let ((end (if (> left n)
                                    n
                                    (let ((nl (position 10 buf :end n :from-end t)))
                                      (if nl (1+ nl) n)))))
                       (write-sequence buf out :end end)
                       (incf kept end)
                       (decf left n)))
            (setf want kept)))))
    (values (namestring path)
            (with-open-file (s path :element-type '(unsigned-byte 8))
              (file-length s)))))

(defun pane-script (corpus)
  (script (format nil "pane-~A" corpus)
          (format nil "printf '~A\\n'~%while [ ! -f ~A ]; do sleep 0.05; done~%~
printf '~A\\n'~%cat ~A~%printf '\\n~A\\n'~%sleep 3~%"
                  +ready+ (go-path) +began+ (slice corpus) +ended+)))

(defparameter +atty-bin+
  (or (uiop:getenv "ATTY_BIN")
      (namestring (make-pathname :name "atty" :type nil
                                 :directory (butlast (pathname-directory *load-truename*))
                                 :defaults *load-truename*)))
  "What a user actually runs is the saved executable, not an SBCL loading ASDF
fresh every time: the two pay entirely different costs to get to the same
running program, and only one of them is what this is measuring against tmux.")

(defvar *ours-run* 0)

(defun ours (corpus)
  (unless (probe-file +atty-bin+)
    (error "no atty binary at ~A: run make atty first" +atty-bin+))
  (let* ((pane (pane-script corpus))
         (name (format nil "bench-~D-~D" (sb-posix:getpid) (incf *ours-run*)))
         ;; a server of its own, as tmux -L gives tmux one: a run measured
         ;; inside the user's own server would be measuring their sessions too
         (run (script "ours"
                      (format nil "exec ~A -L ~A run bench 'sh ~A'~%" +atty-bin+ name pane))))
    (multiple-value-prog1 (watch-one (format nil "sh ~A" run))
      (ignore-errors (delete-file (mux:socket-path name)))
      (ignore-errors (delete-file (mux:log-path name))))))

(defun tmux-there-p ()
  (ignore-errors
   (let ((out (with-output-to-string (s)
                (sb-ext:run-program "sh" (list "-c" "tmux -V") :search t
                                                               :output s
                                                               :error nil))))
     (and (plusp (length out)) (string-trim '(#\Newline) out)))))

(defun tmux-server-pid (socket)
  (ignore-errors
   (parse-integer
    (string-trim '(#\Newline #\Space)
                 (with-output-to-string (s)
                   (sb-ext:run-program
                    "sh" (list "-c" (format nil "tmux -L ~A display-message -p '#{pid}'"
                                            socket))
                    :search t :output s :error nil)))
    :junk-allowed t)))

(defun theirs (corpus)
  (let* ((pane (pane-script corpus))
         (socket (format nil "atty-bench-~D" (sb-posix:getpid)))
         (found nil)
         (run (script "theirs"
                      (format nil "exec tmux -f /dev/null -L ~A new-session -x ~D -y ~D 'sh ~A'~%"
                              socket +cols+ +rows+ pane))))
    (unwind-protect
         (watch-one (format nil "sh ~A" run)
                    :also (lambda ()
                            (list (or found
                                      (setf found (tmux-server-pid socket))))))
      (ignore-errors
       (sb-ext:run-program "sh" (list "-c" (format nil "tmux -L ~A kill-server" socket))
                           :search t :output nil :error nil)))))

(defun rounds-of (what corpus in)
  "WHAT, that many times, answered as the middle of what each round said.

One round is not a number: tmux decides how often to repaint from how the
timing fell, so the same work twice is not the same bytes twice. The middle of
five, with the ends printed beside it, says both what it does and how much it
wanders."
  (let ((speeds nil) (outs nil) (cpus nil) (rss 0) (lost 0))
    (dotimes (i +rounds+)
      (multiple-value-bind (secs bytes cpu weight)
          (funcall what corpus)
        (if secs
            (progn (push (/ in secs 1024d0 1024d0) speeds)
                   (push (/ bytes (float in 1d0)) outs)
                   (push (/ cpu (/ in 1024d0 1024d0)) cpus)
                   (setf rss (max rss weight)))
            (incf lost))))
    (if speeds
        (values (middle (coerce speeds 'vector))
                (reduce #'min speeds) (reduce #'max speeds)
                (middle (coerce outs 'vector))
                (middle (coerce cpus 'vector))
                (/ rss 1024d0)
                lost)
        (values nil nil nil nil nil 0 lost))))

(defun run-it ()
  (make-corpora)
  (let ((tmux (tmux-there-p)))
    (format t "~&~%attached, both drawing: ~Dx~D, ~D MB in, ~D rounds each~%~%"
            +cols+ +rows+ +megabytes-in+ +rounds+)
    (when tmux (format t "~&  ~A~%~%" tmux))
    (format t "~&  ~7A ~7A ~8A ~17A ~8A ~9A ~8A~%"
            "corpus" "what" "MB/s" "low .. high" "out/in" "cpu s/MB" "rss MB")
    (dolist (corpus +corpora+)
      (multiple-value-bind (path in) (slice corpus)
        (declare (ignore path))
        (flet ((row (what fn)
                 (multiple-value-bind (mid low high out cpu rss lost)
                     (rounds-of fn corpus in)
                   (if mid
                       (format t "~&  ~7A ~7A ~8,1F ~7,1F .. ~7,1F ~8,3F ~9,2F ~8,1F~@[  (~D lost)~]~%"
                               corpus what mid low high out cpu rss
                               (and (plusp lost) lost))
                       (format t "~&  ~7A ~7A ~A~%" corpus what "no round finished")))))
          (row "atty" #'ours)
          (if tmux
              (row "tmux" #'theirs)
              (format t "~&  ~7A ~7A ~A~%" corpus "tmux" "not here")))))
    (format t "~%  out/in is what the terminal had to read for every byte the~%")
    (format t "  program wrote. cpu is the whole tree: both ends of each.~%~%")))

(run-it)
