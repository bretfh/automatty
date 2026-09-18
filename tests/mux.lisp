(in-package #:vt/test)

(def-suite mux :in all)
(in-suite mux)

(defvar *paths* 0)

(defun a-socket-path ()
  (format nil "~Acl-vt-~D-~D" (uiop:temporary-directory)
          (sb-posix:getpid) (incf *paths*)))

(defun until (seconds fn)
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :until (funcall fn)
          :do (when (> (get-internal-real-time) deadline) (return nil))
             (sleep 0.005)
          :finally (return t))))

(defun stop-server (path)
  (handler-case
      (let ((wire (mux:make-wire
                   (let ((socket (make-instance 'sb-bsd-sockets:local-socket
                                                :type :stream)))
                     (sb-bsd-sockets:socket-connect socket path)
                     (sb-bsd-sockets:socket-file-descriptor socket)))))
        (mux:wire-send wire '(:stop))
        (mux:wire-flush wire)
        (sleep 0.05)
        (mux:wire-close wire))
    (error () nil)))

(defmacro with-server ((path &key (command "cat") (rows 10) (cols 40)) &body body)
  (let ((thread (gensym "THREAD")))
    `(let ((,path (a-socket-path)))
       (let ((,thread (sb-thread:make-thread
                       (lambda () (mux:serve ,path ,command :rows ,rows :cols ,cols
                                             :interval 0))
                       :name "a test server")))
         (unwind-protect
              (progn (until 5 (lambda () (probe-file ,path)))
                     ,@body)
           (stop-server ,path)
           ;; a server left running goes on holding descriptors, and the next
           ;; test opens a pty onto the numbers it is about to close
           (when (eq :gave-up (sb-thread:join-thread ,thread :timeout 5
                                                             :default :gave-up))
             (error "a test server would not stop"))
           (ignore-errors (delete-file ,path)))))))

(defstruct seer client host master slave (decoder (vt:make-decoder)))

(defun a-seer (path &key (rows 10) (cols 40))
  (multiple-value-bind (master slave-path) (pty:open-pty)
    (let ((slave (sb-posix:open slave-path sb-posix:o-rdwr)))
      (pty:pty-set-size master rows cols)
      (mux:host-raw slave)
      (make-seer :client (mux:make-client path :fd slave :to slave)
                 :host (a-term :width cols :height rows)
                 :master master
                 :slave slave))))

(defun seer-close (seer)
  (ignore-errors (mux:client-close (seer-client seer)))
  (ignore-errors (sb-posix:close (seer-slave seer)))
  (ignore-errors (pty:pty-close (seer-master seer))))

(defun pump (seer &key (seconds 2) want)
  "Run the client and read what it drew, until WANT is on its screen."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop
      (mux:client-step (seer-client seer) 5)
      (when (pty:pty-wait (seer-master seer) 0)
        (let ((said (pty:pty-read-string (seer-master seer) 65536)))
          (when said
            (vt:term-process-output
             (seer-host seer)
             (vt:decode-utf-8 (seer-decoder seer) said)))))
      (when (and want (search want (vt:term-dump-to-string (seer-host seer))))
        (return t))
      (when (> (get-internal-real-time) deadline)
        (return (null want))))))

(defun type-at (seer said)
  (pty:pty-write-string (seer-master seer) said))

(defun seen (seer)
  (vt:term-dump-to-string (seer-host seer)))

(defmacro with-seer ((seer path &rest args) &body body)
  `(let ((,seer (a-seer ,path ,@args)))
     (unwind-protect (progn ,@body) (seer-close ,seer))))

(test what-a-program-drew-is-what-the-client-shows
  (with-server (path :command "printf 'hello-from-the-pane\\n'; sleep 30")
    (with-seer (seer path)
      (is-true (pump seer :want "hello-from-the-pane")))))

(test what-is-typed-at-the-client-the-program-reads
  (with-server (path :command "cat")
    (with-seer (seer path)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "knock-knock~C" #\Return))
      (is-true (pump seer :want "knock-knock")))))

(test the-prefix-byte-does-not-reach-the-program
  (with-server (path :command "cat")
    (with-seer (seer path)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "a~Cqb~C" mux:+prefix+ #\Return))
      (is-true (pump seer :want "ab"))
      (is (null (search "q" (seen seer)))
          "the byte after the prefix was passed through"))))

(test the-prefix-twice-is-the-prefix-once
  (with-server (path :command "cat -v")
    (with-seer (seer path)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~C~C~C" mux:+prefix+ mux:+prefix+ #\Return))
      (is-true (pump seer :want "^B")))))

(test a-client-that-comes-back-is-shown-the-whole-screen
  (with-server (path :command "printf 'still-here\\n'; sleep 30")
    (with-seer (first-seer path)
      (is-true (pump first-seer :want "still-here")))
    (with-seer (again path)
      (is-true (pump again :want "still-here")))))

(test two-clients-see-the-same-screen
  (with-server (path :command "printf 'both-of-them\\n'; sleep 30")
    (with-seer (one path)
      (with-seer (two path)
        (is-true (pump one :want "both-of-them"))
        (is-true (pump two :want "both-of-them"))))))

(test a-client-sees-what-the-pane-holds-cell-for-cell
  (with-server (path :command "printf '\\033[31mred\\033[0m \\033[1mbold\\033[0m\\n'; sleep 30")
    (with-seer (seer path)
      (is-true (pump seer :want "red"))
      (pump seer :seconds 1/4)
      (let ((host (seer-host seer)))
        (is (eql 1 (vt:face-fg (face-at host 0 0))) "the red did not come across")
        (is (vt:face-bold (face-at host 4 0)) "the bold did not come across")))))

(test a-resize-reaches-the-program-and-the-screen
  (with-server (path :command "trap 'stty size' WINCH; stty size; sleep 1; sleep 60" :rows 10 :cols 40)
    (with-seer (seer path :rows 10 :cols 40)
      (is-true (pump seer :want "10 40"))
      (pty:pty-set-size (seer-master seer) 20 60)
      (vt:term-resize (seer-host seer) 60 20)
      (multiple-value-bind (rows cols) (mux:host-size (seer-slave seer))
        (is (eql 20 rows))
        (is (eql 60 cols)))
      (mux:client-resized (seer-client seer))
      (is-true (pump seer :want "20 60")))))

(test a-pane-whose-program-is-done-says-bye
  (with-server (path :command "printf 'and-out\\n'; sleep 1")
    (with-seer (seer path)
      (pump seer :want "and-out")
      (pump seer :seconds 2)
      (is (null (mux:client-going (seer-client seer))))
      (is (eql :done (mux:client-why (seer-client seer)))))))

(test a-screenful-of-redraws-arrives-as-the-same-screen
  (with-server (path :command "for i in 1 2 3 4 5 6 7 8; do printf '\\033[2J\\033[H'; printf 'frame-%d\\n' $i; done; printf 'done-them\\n'; sleep 30"
                     :rows 10 :cols 40)
    (with-seer (seer path)
      (is-true (pump seer :want "done-them"))
      (pump seer :seconds 1/4)
      (is (search "frame-8" (seen seer))
          "the last frame drawn is not the one on the screen: ~S" (seen seer)))))

(test a-bell-does-not-take-the-server-down
  (with-server (path :command "printf 'before\\a'; sleep 1; printf 'after\\n'; sleep 30")
    (with-seer (seer path)
      (is-true (pump seer :want "before"))
      (is-true (pump seer :want "after")
               "the server stopped when the pane rang the bell"))))

(test a-title-does-not-take-the-server-down
  (with-server (path :command "printf '\\033]0;a new title\\007here\\n'; sleep 30"
                     :rows 10 :cols 40)
    (with-seer (seer path)
      (is-true (pump seer :want "here")))))

(test what-a-program-drew-in-utf-8-is-drawn-in-utf-8
  (with-server (path :command "printf '\\342\\224\\234\\342\\224\\200\\342\\224\\200 leaf\\n'; sleep 30"
                     :rows 10 :cols 40)
    (with-seer (seer path)
      (is-true (pump seer :want "leaf"))
      (pump seer :seconds 1/4)
      (is (equal "├── leaf" (row (seer-host seer) 0))
          "came out as ~S" (row (seer-host seer) 0)))))

(test a-wide-character-takes-two-columns-through-the-whole-loop
  (with-server (path :command "printf '\\346\\274\\242\\345\\255\\227x\\n'; sleep 30"
                     :rows 10 :cols 40)
    (with-seer (seer path)
      (is-true (pump seer :want "x"))
      (pump seer :seconds 1/4)
      (let ((host (seer-host seer)))
        (is (eql (code-char #x6F22) (at host 0 0)))
        (is (eql (code-char #x5B57) (at host 2 0))
            "the second wide character did not start at column 2")
        (is (eql #\x (at host 4 0))
            "the wide characters did not take two columns each: ~S"
            (row host 0))))))
