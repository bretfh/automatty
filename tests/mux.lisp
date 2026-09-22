(in-package #:atty/test)

(def-suite mux :in all)
(in-suite mux)

(defvar *paths* 0)

(defun a-socket-path ()
  (format nil "~Aatty-~D-~D" (uiop:temporary-directory)
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
      ;; the wire is given the socket, not just its number. A socket left
      ;; unclosed shuts its descriptor when it is finalised, and by then that
      ;; number belongs to whoever opened the next one.
      (let* ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
             (wire (progn (sb-bsd-sockets:socket-connect socket path)
                          (mux:make-wire
                           (sb-bsd-sockets:socket-file-descriptor socket)
                           socket))))
        (mux:wire-send wire '(:stop))
        (mux:wire-flush wire)
        (sleep 0.05)
        (mux:wire-close wire))
    (error () nil)))

(defmacro with-server ((path &key (command "cat") (rows 10) (cols 40)) &body body)
  (let ((thread (gensym "THREAD")) (broke (gensym "BROKE")))
    `(let ((,path (a-socket-path))
           ;; a fault in the loop is stepped over rather than fatal, so a test
           ;; would go on passing over one. This is where the server says what
           ;; broke, and a test that leaves anything here has found something
           (,broke (make-string-output-stream)))
       (let ((,thread (sb-thread:make-thread
                       (lambda ()
                         (let ((*error-output* ,broke))
                           (mux:serve ,path ,command :rows ,rows :cols ,cols
                                      :interval 0 :persist nil)))
                       :name "a test server")))
         (unwind-protect
             (progn (until 5 (lambda () (probe-file ,path)))
                    ,@body)
           (stop-server ,path)
           ;; a server left running goes on holding descriptors, and the next
           ;; test opens a pty onto the numbers it is about to close
           (when (eq :gave-up (sb-thread:join-thread ,thread :timeout 15
                                                     :default :gave-up))
             (error "a test server would not stop"))
           (ignore-errors (delete-file ,path))
           (let ((said (get-output-stream-string ,broke)))
             (is (equal "" said) "the server said something broke:~%~A" said)))))))

(defstruct seer client host master slave (decoder (term:make-decoder))
  (heard nil) (lock (sb-thread:make-mutex :name "a seer")) (going t) (reader nil))

(defun seer-listen (seer)
  (loop :while (seer-going seer)
        :do (when (pty:pty-wait (seer-master seer) 50)
              (let ((said (ignore-errors (pty:pty-read-string (seer-master seer) 65536))))
                (when (and said (plusp (length said)))
                  (sb-thread:with-mutex ((seer-lock seer))
                    (push said (seer-heard seer))))))))

(defun seer-take (seer)
  (sb-thread:with-mutex ((seer-lock seer))
    (prog1 (reverse (seer-heard seer))
      (setf (seer-heard seer) nil))))

(defun a-seer (path &key (rows 10) (cols 40))
  (multiple-value-bind (master slave-path) (pty:open-pty)
                       (let ((slave (sb-posix:open slave-path sb-posix:o-rdwr)))
                         (pty:pty-set-size master rows cols)
                         (tty:host-raw slave)
                         (let ((seer (make-seer :client (mux:make-client path :fd slave :to slave)
                                                :host (a-term :width cols :height rows)
                                                :master master
                                                :slave slave)))
                           (setf (seer-reader seer)
                                 (sb-thread:make-thread #'seer-listen :arguments (list seer)
                                                                      :name "a seer reading"))
                           seer))))

(defun seer-close (seer)
  (ignore-errors (mux:client-close (seer-client seer)))
  (setf (seer-going seer) nil)
  (when (seer-reader seer)
    (ignore-errors (sb-thread:join-thread (seer-reader seer) :timeout 2)))
  (ignore-errors (sb-posix:close (seer-slave seer)))
  (ignore-errors (pty:pty-close (seer-master seer))))

(defun pump (seer &key (seconds 8) want until)
  "Run the client and read what it drew, until WANT is on its screen.

SECONDS is there to stop a hang, not to measure anything: a test that finds what
it wanted returns at once, so the only runs that feel the deadline are the ones
already failing."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop
     (mux:client-step (seer-client seer) 5)
     (dolist (said (seer-take seer))
       (term:term-process-output
        (seer-host seer)
        (term:decode-utf-8 (seer-decoder seer) said)))
     (when (and want (search want (term:term-dump-to-string (seer-host seer))))
       (return t))
     (when (and until (funcall until))
       (return t))
     (when (> (get-internal-real-time) deadline)
       (return (and (null want) (null until)))))))

(defun type-at (seer said)
  (pty:pty-write-string (seer-master seer) said))

(defun seen (seer)
  (term:term-dump-to-string (seer-host seer)))

(defun seen-below-bar (seer)
  (let ((all (seen seer)))
    (subseq all (min (length all) (1+ (or (position #\Newline all) (length all)))))))

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
                                (is (eql 1 (term:face-fg (face-at host 0 1))) "the red did not come across")
                                (is (term:face-bold (face-at host 4 1)) "the bold did not come across")))))

(test a-resize-reaches-the-program-and-the-screen
      (with-server (path :command "trap 'stty size' WINCH; stty size; sleep 1; sleep 60" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (is-true (pump seer :want "9 39")
                                       "the program was not given the rows the bar left it")
                              (pty:pty-set-size (seer-master seer) 20 60)
                              (term:term-resize (seer-host seer) 60 20)
                              (multiple-value-bind (rows cols) (tty:host-size (seer-slave seer))
                                                   (is (eql 20 rows))
                                                   (is (eql 60 cols)))
                              (mux:client-resized (seer-client seer))
                              (is-true (pump seer :want "19 59")))))

(test a-pane-whose-program-is-done-says-bye
      (with-server (path :command "printf 'and-out\\n'; sleep 1")
                   (with-seer (seer path)
                              (pump seer :want "and-out")
                              ;; wait for it to stop, rather than for a length of time and a hope
                              (is-true (pump seer :until (lambda ()
                                                           (not (mux:client-going (seer-client seer)))))
                                       "it never stopped")
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
                                       "the server stopped when the pane rang the bell: ~S" (seen seer)))))

(test the-bar-says-what-a-known-agent-is-doing
      (with-server (path :command "printf '\\033]0;\\342\\234\\263 Claude Code\\007'; printf '\\342\\224\\200\\342\\224\\200\\342\\224\\200\\342\\224\\200\\n\\342\\235\\257 \\n\\342\\224\\200\\342\\224\\200\\342\\224\\200\\342\\224\\200\\n'; sleep 1.5; printf '\\342\\217\\265\\342\\217\\265 auto mode on \\302\\267 esc to interrupt\\n'; sleep 30"
                         :rows 10 :cols 60)
                   (with-seer (seer path :rows 10 :cols 60)
                              (is-true (pump seer :want "idle") "the bar never said idle: ~S" (seen seer))
                              (is-true (pump seer :want "working") "the bar never said working: ~S" (seen seer)))))

(test a-program-is-told-which-pane-it-is-in-and-where-its-server-is
      (with-server (path :command "printf 'pane=%s socket=%s\\n' \"$ATTY_PANE\" \"$ATTY_SOCKET\"; sleep 30"
                         :rows 10 :cols 120)
                   (with-seer (seer path :rows 10 :cols 120)
                              (is-true (pump seer :want "pane=0:") "~S" (seen seer))
                              (is (search (format nil "socket=~A" path) (seen seer))
                                  "the program was not told where its server is: ~S" (seen seer)))))

(defun step-until (server test &optional (seconds 5))
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :until (funcall test)
          :do (mux:server-step server :interval 0)
          (when (> (get-internal-real-time) deadline) (return nil))
          :finally (return t))))

(defmacro with-a-server-here ((server path) &body body)
  "A server stepped by hand, so a test can look at what it holds between steps."
  `(let ((,path (a-socket-path)))
     (unwind-protect
         (let ((,server (mux:make-server ,path)))
           (unwind-protect (progn ,@body)
             (mux:server-close ,server)))
       (ignore-errors (delete-file ,path)))))

(defun heard-back (wire)
  (when (mux:wire-fill wire)
    (loop :for form := (mux:wire-take wire) :while form :collect form)))


(test what-every-pane-is-doing-can-be-asked-and-a-hook-can-say-it
      (with-a-server-here (server path)
                          (let* ((session (mux:add-session server "cat" :rows 6 :cols 20))
                                 (pane (first (mux:session-panes session)))
                                 (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
                                 (heard nil))
                            (sb-bsd-sockets:socket-connect socket path)
                            (let ((wire (mux:make-wire (sb-bsd-sockets:socket-file-descriptor socket)
                                                       socket)))
                              (mux:wire-send wire '(:agents))
                              (mux:wire-flush wire)
                              (is-true (step-until server (lambda ()
                                                            (setf heard (append heard (heard-back wire)))
                                                            (find :agents heard :key #'first)))
                                       "the server never said what the panes are doing")
                              (let ((rows (second (find :agents heard :key #'first))))
                                (is (eql 1 (length rows)) "the rows came back as ~S" rows)
                                (is (equal (list "0" (mux:pane-id pane) "cat") (subseq (first rows) 0 3))
                                    "the rows came back as ~S" rows)
                                (is (member (fourth (first rows)) '(:unknown :working :idle))
                                    "a pane nobody has spoken to is ~S" (fourth (first rows))))
                              (setf heard nil)
                              (mux:wire-send wire (list :go "0"))
                              (mux:wire-send wire (list :agent-signal "0" (mux:pane-id pane) :blocked))
                              (mux:wire-flush wire)
                              (is-true (step-until server (lambda ()
                                                            (setf heard (append heard (heard-back wire)))
                                                            (find :agent heard :key #'first)))
                                       "nobody was told the pane's state changed")
                              (is (equal (list :agent "0" (mux:pane-id pane) "cat" :blocked
                                               '(:signalled :blocked))
                                         (subseq (find :agent heard :key #'first) 0 6)))
                              (is (eq :blocked (agent:agent-state (mux:pane-agent pane))))
                              (mux:wire-close wire)))))

(test a-pane-can-be-read-typed-at-prompted-and-explained-by-its-number
      (with-a-server-here (server path)
                          (let* ((session (mux:add-session server "cat" :rows 6 :cols 20))
                                 (pane (first (mux:session-panes session)))
                                 (id (mux:pane-id pane))
                                 (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
                                 (heard nil))
                            (sb-bsd-sockets:socket-connect socket path)
                            (let ((wire (mux:make-wire (sb-bsd-sockets:socket-file-descriptor socket)
                                                       socket)))
                              (flet ((hear (tag)
                                       (setf heard nil)
                                       (step-until server (lambda ()
                                                            (setf heard (append heard (heard-back wire)))
                                                            (find tag heard :key #'first)))
                                       (find tag heard :key #'first)))
                                (mux:wire-send wire (list :agent-keys "0" id (format nil "hello~C" #\Return)))
                                (mux:wire-flush wire)
                                (is-true (step-until server (lambda ()
                                                              (search "hello" (term:term-dump-to-string (mux:pane-term pane)))))
                                         "the keys did not reach the pane")
                                (mux:wire-send wire (list :agent-read "0" id 3))
                                (mux:wire-flush wire)
                                (let ((lines (fourth (hear :agent-lines))))
                                  (is (member "hello" lines :test #'string=) "read gave ~S" lines))
                                (mux:wire-send wire (list :agent-explain "0" id))
                                (mux:wire-flush wire)
                                (let ((said (hear :agent-explained)))
                                  (is (member (fourth said) '(:working :idle :unknown)) "~S" said)
                                  (is (null (sixth said)) "a shell has rules: ~S" said))
                                (mux:wire-send wire (list :agent-signal "0" id :blocked))
                                (mux:wire-flush wire)
                                (is-true (step-until server (lambda ()
                                                              (eq :blocked (agent:agent-state (mux:pane-agent pane))))))
                                (mux:wire-send wire (list :agent-prompt "0" id "more"))
                                (mux:wire-flush wire)
                                (is (equal (list :agent-prompted "0" id :blocked) (subseq (hear :agent-prompted) 0 4))
                                    "a blocked pane took a prompt")
                                (mux:wire-send wire (list :agent-signal "0" id :idle))
                                (mux:wire-send wire (list :agent-prompt "0" id "more"))
                                (mux:wire-flush wire)
                                (is-true (step-until server (lambda ()
                                                              (eq :idle (agent:agent-state (mux:pane-agent pane))))))
                                (is (equal (list :agent-prompted "0" id t) (subseq (hear :agent-prompted) 0 4)))
                                (is-true (step-until server (lambda ()
                                                              (search "more" (term:term-dump-to-string (mux:pane-term pane)))))
                                         "the prompt did not reach the pane")
                                (mux:wire-close wire))))))

(test a-prompt-of-more-than-one-line-is-pasted-and-entered-a-moment-later
      (with-server (path :command "printf '\\033[?2004h'; stty -echo; cat -v" :rows 10 :cols 60)
                   (with-seer (seer path :rows 10 :cols 60)
                              (pump seer :seconds 1/2)
                              (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
                                (sb-bsd-sockets:socket-connect socket path)
                                (let ((wire (mux:make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket))
                                      (heard nil)
                                      (then (get-internal-real-time)))
                                  (mux:wire-send wire '(:agents))
                                  (mux:wire-flush wire)
                                  (is-true (until 5 (lambda ()
                                                      (setf heard (append heard (heard-back wire)))
                                                      (find :agents heard :key #'first)))
                                           "the server never said what it holds")
                                  (mux:wire-send wire (list :agent-prompt "0"
                                                            (second (first (second (find :agents heard :key #'first))))
                                                            (format nil "hello~%there")))
                                  (mux:wire-flush wire)
                                  (is-true (pump seer :want "there^[[201~")
                                           "more than one line was not pasted as a paste: ~S" (seen seer))
                                  (is (>= (- (get-internal-real-time) then)
                                          (* 1/4 internal-time-units-per-second))
                                      "enter came in the same breath as the text")
                                  (mux:wire-close wire))))))

(test a-program-nobody-knows-has-a-chip-but-is-not-said-to-be-doing-anything
      ;; working and idle for a shell would only say whether its screen moved
      (with-server (path :command "printf 'just-a-shell\\n'; sleep 30" :rows 10 :cols 80)
                   (with-seer (seer path :rows 10 :cols 80)
                              (is-true (pump seer :want "just-a-shell"))
                              (type-at seer (format nil "~C3" mux:+prefix+))
                              (is-true (pump seer :until (lambda () (<= 2 (count-of "printf 'just" (seen seer)))))
                                       "a second pane got no chip of its own: ~S" (seen seer))
                              (pump seer :seconds 1)
                              (is (null (search "idle" (seen seer)))
                                  "a program nobody recognised was said to be idle: ~S" (seen seer))
                              (is (null (search "working" (seen seer)))
                                  "a program nobody recognised was said to be working: ~S" (seen seer)))))

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
                              (is (eql 0 (search "├── leaf " (row (seer-host seer) 1)))
                                  "came out as ~S" (row (seer-host seer) 1)))))

(test a-wide-character-takes-two-columns-through-the-whole-loop
      (with-server (path :command "printf '\\346\\274\\242\\345\\255\\227x\\n'; sleep 30"
                         :rows 10 :cols 40)
                   (with-seer (seer path)
                              (is-true (pump seer :want "x"))
                              (pump seer :seconds 1/4)
                              (let ((host (seer-host seer)))
                                (is (eql (code-char #x6F22) (at host 0 1)))
                                (is (eql (code-char #x5B57) (at host 2 1))
                                    "the second wide character did not start at column 2")
                                (is (eql #\x (at host 4 1))
                                    "the wide characters did not take two columns each: ~S"
                                    (row host 1))))))

(test something-listening-that-never-answers-is-not-a-server
      (let* ((path (a-socket-path))
             (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (unwind-protect
            (progn
              (sb-bsd-sockets:socket-bind socket path)
              (sb-bsd-sockets:socket-listen socket 4)
              (is (probe-file path))
              (is (null (mux::answering-p path 1/2))
                  "a socket nobody is reading was taken for a running server"))
          (ignore-errors (sb-bsd-sockets:socket-close socket))
          (ignore-errors (delete-file path)))))

(test a-server-that-is-running-answers-a-knock
      (with-server (path :command "sleep 30")
                   (is-true (mux::answering-p path 3))))

(test a-server-that-has-gone-leaves-no-name-behind
      (let ((left nil))
        (with-server (path :command "sleep 30")
                     (setf left path)
                     (is (probe-file path)))
        (is (null (probe-file left))
            "the socket outlived the server that made it")))

(test the-program-is-given-the-rows-the-bar-left-it
      (with-server (path :command "stty size; sleep 30" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (is-true (pump seer :want "9 39")
                                       "the program was told the whole terminal, bar and all: ~S"
                                       (seen seer)))))

(test turning-the-bar-off-reaches-the-server-not-just-the-client
      (with-server (path :command "while :; do stty size; sleep 0.3; done"
                         :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (is-true (pump seer :want "9 39") "the bar was not taking a row")
                              (type-at seer (format nil "~Ct" mux:+prefix+))
                              (is-true (pump seer :want "10 39")
                                       "the bar did not come off: ~S" (seen seer))
                              (type-at seer (format nil "~Ct" mux:+prefix+))
                              (is-true (pump seer :want "9 39")
                                       "the bar did not come back: ~S" (seen seer)))))

(test with-no-bar-the-program-has-the-whole-terminal
      ;; the server runs in a thread of its own, and a special bound here would not
      ;; reach it
      (let ((was mux:*bar*))
        (unwind-protect
            (progn (setf mux:*bar* nil)
                   (with-server (path :command "stty size; sleep 30" :rows 10 :cols 40)
                                (with-seer (seer path :rows 10 :cols 40)
                                           (is-true (pump seer :want "10 39") "~S" (seen seer)))))
          (setf mux:*bar* was))))

(test the-prompt-opens-on-the-prefix-and-runs-what-was-chosen
      (with-server (path :command "printf 'the-pane\\n'; sleep 30" :rows 12 :cols 50)
                   (with-seer (seer path :rows 12 :cols 50)
                              (is-true (pump seer :want "the-pane"))
                              (type-at seer (format nil "~C:" mux:+prefix+))
                              (is-true (pump seer :want "run") "the prompt did not open: ~S" (seen seer))
                              (is-true (pump seer :want "bar off"))
                              (type-at seer "bar")
                              (is-true (pump seer :want "bar o") "typing did not narrow it: ~S" (seen seer))
                              (type-at seer (string (code-char 27)))
                              (pump seer :seconds 1/2)
                              (is (search "the-pane" (seen seer))
                                  "the pane did not come back after the prompt was dropped")
                              (is-true (mux:client-going (seer-client seer))))))

(test what-is-typed-at-a-prompt-does-not-reach-the-program
      (with-server (path :command "cat" :rows 12 :cols 50)
                   (with-seer (seer path :rows 12 :cols 50)
                              (pump seer :seconds 1/2)
                              (type-at seer (format nil "~C:" mux:+prefix+))
                              (is-true (pump seer :want "run"))
                              (type-at seer "redr")
                              (pump seer :seconds 1/2)
                              (type-at seer (string (code-char 27)))
                              (pump seer :seconds 1/2)
                              (type-at seer (format nil "after~C" #\Return))
                              (is-true (pump seer :want "after"))
                              (is (null (search "redr" (seen seer)))
                                  "what was typed at the prompt was echoed by the program: ~S" (seen seer)))))

(test a-prompt-is-drawn-for-whoever-opened-it-and-nobody-else
      (with-server (path :command "printf 'shared\\n'; sleep 30" :rows 12 :cols 50)
                   (with-seer (mine path :rows 12 :cols 50)
                              (with-seer (theirs path :rows 12 :cols 50)
                                         (is-true (pump mine :want "shared"))
                                         (is-true (pump theirs :want "shared"))
                                         (type-at mine (format nil "~C:" mux:+prefix+))
                                         (is-true (pump mine :want "bar off"))
                                         (pump theirs :seconds 1/2)
                                         (is (null (search "detach" (seen theirs)))
                                             "the other client was shown a prompt it did not open: ~S" (seen theirs))))))

(test the-bar-names-a-program-rather-than-spelling-out-where-it-lives
      (is (equal "bash" (mux::shortened "/gnu/store/abc-bash-5.2.37/bin/bash")))
      (is (equal "sh -c 'a thing'" (mux::shortened "/bin/sh -c 'a thing'")))
      (is (equal "vim" (mux::shortened "vim")))
      (is (equal "" (mux::shortened nil))))

(test a-resize-does-not-leave-the-screen-painted-in-the-bars-colour
      (with-server (path :command "printf 'before\\n'; sleep 30" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (is-true (pump seer :want "before"))
                              (pty:pty-set-size (seer-master seer) 14 50)
                              (term:term-resize (seer-host seer) 50 14)
                              (mux:client-resized (seer-client seer))
                              (is-true (pump seer :want "before") "the pane did not come back after a resize")
                              (pump seer :seconds 3)
                              (let ((host (seer-host seer)))
                                (is (term:face-default-p (face-at host 20 6))
                                    "an empty cell came back wearing ~S, so the clear was done in
whatever colour was last in force"
                                    (term:face-plist (face-at host 20 6)))))))

(test a-bar-that-has-stood-long-enough-puts-a-watcher-behind
      (let ((session (mux::%make-session))
            (w (mux::%make-watcher :here t)))
        (setf (mux::session-watchers session) (list w)
              (mux::session-clocked session) 0
              (mux::watcher-behind w) nil)
        (is-true (mux::session-clock session mux::+bar-gap+)
                 "the bar stood a whole gap and nobody was put behind")
        (is-true (mux::watcher-behind w))
        (setf (mux::watcher-behind w) nil)
        (is (null (mux::session-clock session (1+ mux::+bar-gap+)))
            "the bar was drawn again the moment after")
        (is-false (mux::watcher-behind w))
        (is-true (mux::session-clock session (* 2 mux::+bar-gap+)))
        (is-true (mux::watcher-behind w))))

(defun where-said (seer said)
  "Which column SAID starts at on the seer's screen, or nil."
  (loop :for y :below (term:term-height (seer-host seer))
        :for found := (search said (term:term-dump-row-string (seer-host seer) y))
        :when found :do (return found)))

(test a-split-gives-the-new-pane-half-the-terminal-and-the-cursor
      (with-server (path :command "cat" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (type-at seer "on-the-first")
                              (is-true (pump seer :want "on-the-first"))
                              (type-at seer (format nil "~C3" mux:+prefix+))
                              (is-true (pump seer :until (lambda () (search "│" (seen seer))))
                                       "no rule came up between the two panes: ~S" (seen seer))
                              (type-at seer "on-the-second")
                              (is-true (pump seer :want "on-the-second")
                                       "what was typed reached nobody: ~S" (seen seer))
                              (is (eql 1 (count-of "on-the-second" (seen seer)))
                                  "what was typed reached both panes: ~S" (seen seer))
                              (is (>= (or (where-said seer "on-the-second") 0) 20)
                                  "what was typed went to the pane on the left, not the new one: ~S"
                                  (seen seer))
                              (is (< (or (where-said seer "on-the-first") 40) 20)
                                  "the first pane moved: ~S" (seen seer)))))

(test a-click-moves-the-focus-to-the-pane-under-it
      (with-server (path :command "cat" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (type-at seer "the-first")
                              (is-true (pump seer :want "the-first"))
                              (type-at seer (format nil "~C3" mux:+prefix+))
                              (is-true (pump seer :until (lambda () (search "│" (seen seer))))
                                       "no rule came up between the two panes: ~S" (seen seer))
                              (type-at seer (format nil "~C[<0;3;3M~C[<0;3;3m" #\Escape #\Escape))
                              (pump seer :seconds 1/2)
                              (type-at seer "-again")
                              (is-true (pump seer :want "the-first-again")
                                       "the click did not move the focus to the pane under it: ~S"
                                       (seen seer))
                              (is (< (or (where-said seer "the-first-again") 100) 20)
                                  "what was typed after the click went to the pane on the right,
not the one clicked on: ~S" (seen seer))
                              (is (null (search "[<" (seen seer)))
                                  "the click left raw escape bytes in a pane: ~S" (seen seer)))))

(test a-click-that-lands-on-the-already-focused-pane-does-nothing-odd
      (with-server (path :command "cat" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (type-at seer "before-the-click")
                              (is-true (pump seer :want "before-the-click"))
                              (type-at seer (format nil "~C[<0;3;3M~C[<0;3;3m" #\Escape #\Escape))
                              (pump seer :seconds 1/2)
                              (type-at seer "-after")
                              (is-true (pump seer :want "before-the-click-after")
                                       "a click on the pane already focused broke typing: ~S" (seen seer))
                              (is (null (search "[<" (seen seer)))
                                  "the click left raw escape bytes in a pane: ~S" (seen seer)))))

(test closing-the-last-pane-ends-the-session
      (with-server (path :command "cat" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (is-true (pump seer :until (lambda () (mux:client-screen (seer-client seer)))))
                              (type-at seer (format nil "~C0" mux:+prefix+))
                              (is-true (pump seer :until (lambda ()
                                                           (not (mux:client-going (seer-client seer)))))
                                       "the client was not told the session was over")
                              (is (eq :done (mux:client-why (seer-client seer)))
                                  "the session ended for the wrong reason: ~S"
                                  (mux:client-why (seer-client seer))))))

(test two-sessions-in-one-server-do-not-see-each-other
      (with-server (path :command "cat" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (type-at seer "in-the-first")
                              (is-true (pump seer :want "in-the-first"))
                              (type-at seer (format nil "~CC" mux:+prefix+))
                              (is-true (pump seer :until (lambda ()
                                                           (null (search "in-the-first" (seen seer)))))
                                       "the new session was shown what the first one holds: ~S" (seen seer))
                              (type-at seer "in-the-second")
                              (is-true (pump seer :want "in-the-second"))
                              (is (null (search "in-the-first" (seen seer)))
                                  "the two sessions are sharing a screen: ~S" (seen seer))
                              (type-at seer (format nil "~Cb" mux:+prefix+))
                              (is-true (pump seer :want "◆ here") "no chooser came up: ~S" (seen seer))
                              (is-true (pump seer :until (lambda () (search "1 pane" (seen seer))))
                                       "the chooser does not say what is in them: ~S" (seen seer))
                              ;; it opens on the session this is; the first is above it
                              (type-at seer (format nil "~C[A" #\Escape))
                              (pump seer :seconds 1/4)
                              (type-at seer (string #\Return))
                              (is-true (pump seer :want "in-the-first")
                                       "choosing the first session did not go back to it: ~S" (seen seer))
                              (is (null (search "in-the-second" (seen seer)))
                                  "what the second session holds came along: ~S" (seen seer)))))

(test a-watcher-whose-wire-was-shut-here-is-let-go
      (with-a-server-here (server path)
                          (let ((session (mux:add-session server "cat" :rows 6 :cols 20))
                                (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
                            (sb-bsd-sockets:socket-connect socket path)
                            (let ((wire (mux:make-wire (sb-bsd-sockets:socket-file-descriptor socket)
                                                       socket)))
                              (mux:wire-send wire (list :attach 6 20 t))
                              (mux:wire-flush wire)
                              (is-true (step-until server (lambda () (mux:session-watchers session)))
                                       "the watcher never joined the session")
                              (let ((watcher (first (mux:session-watchers session))))
                                ;; this is what a write that came apart leaves behind
                                (mux:wire-close (mux:watcher-wire watcher))
                                (is-true (step-until server
                                                     (lambda () (null (mux:session-watchers session))))
                                         "a watcher with a shut wire stayed on the session"))
                              (mux:wire-close wire)))))

(test a-message-the-server-cannot-make-sense-of-is-not-the-end-of-it
      ;; a client newer than the server it reached sends shapes that server has
      ;; never seen. One of them must not take down the sessions everybody else is
      ;; looking at
      (with-a-server-here (server path)
                          (let ((session (mux:add-session server "cat" :rows 6 :cols 20))
                                (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
                                (said (make-string-output-stream)))
                            (sb-bsd-sockets:socket-connect socket path)
                            (let ((wire (mux:make-wire (sb-bsd-sockets:socket-file-descriptor socket)
                                                       socket))
                                  (*error-output* said))
                              (mux:wire-send wire '(:attach 6))
                              (mux:wire-flush wire)
                              (step-until server (lambda () nil) 1)
                              (is-true (mux:server-going server)
                                       "a message it could not parse stopped the server")
                              (is (member session (mux:server-sessions server))
                                  "a message it could not parse took the session with it")
                              (is-true (search "ATTACH" (get-output-stream-string said))
                                       "it passed over the message without saying so")
                              (mux:wire-send wire '(:attach 6 20 t))
                              (mux:wire-flush wire)
                              (is-true (step-until server (lambda () (mux:session-watchers session)))
                                       "it would not take a message it does know afterwards")
                              (mux:wire-close wire)))))

(test a-server-that-does-not-know-the-name-message-still-attaches
      ;; a server older than the client that reached it: it has never heard of
      ;; :want, and what it does with one must be nothing at all
      (with-server (path :command "printf 'still-here\\n'; sleep 30" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (mux:wire-send (mux:client-wire (seer-client seer))
                                             '(:a-message-from-a-later-build 1 2 3))
                              (is-true (pump seer :want "still-here")
                                       "one message the server did not know took the screen with it: ~S"
                                       (seen seer)))))

(test only-this-pane-leaves-the-one-with-the-cursor
      (with-server (path :command "cat" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (type-at seer "in-the-first")
                              (is-true (pump seer :want "in-the-first"))
                              (type-at seer (format nil "~C3" mux:+prefix+))
                              (is-true (pump seer :until (lambda () (search "┏" (seen seer))))
                                       "nothing split: ~S" (seen seer))
                              (type-at seer "in-the-second")
                              (is-true (pump seer :want "in-the-second"))
                              (type-at seer (format nil "~C1" mux:+prefix+))
                              ;; below the bar, which has a rule of its own after the session
                              (is-true (pump seer :until (lambda () (null (search "┏" (seen-below-bar seer)))))
                                       "the frame is still there, so both panes are: ~S" (seen seer))
                              (is-true (pump seer :until (lambda () (null (search "in-the-first"
                                                                                  (seen seer)))))
                                       "the pane without the cursor was kept: ~S" (seen seer))
                              (type-at seer "still-typing")
                              (is-true (pump seer :want "still-typing")
                                       "the pane that was kept stopped taking keys: ~S" (seen seer)))))

(test an-answer-is-an-action-the-reader-knows-and-it-is-confirmed-on-the-screen
  (let ((agent:*readers* nil))
    (agent:register-reader
     '(probe :programs ("probe") :title "^probe$" :versions ("1.0")
             :screens ((:asking :widget :choice :question "(?i)proceed\\?" :means :blocked
                        :choose :digits
                        :actions (:approve (:option "^Yes$") :deny (:option "^No$")))
                       (:idle :widget :prompt-input :means :idle))))
    (with-a-server-here (server path)
      (let* ((session (mux:add-session
                       server
                       "printf '\\033]0;probe\\007────────\\n Proceed?\\n > 1. Yes\\n   2. No\\n'; exec cat"
                       :rows 8 :cols 30))
             (pane (first (mux:session-panes session)))
             (id (mux:pane-id pane))
             (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
             (heard nil))
        (sb-bsd-sockets:socket-connect socket path)
        (let ((wire (mux:make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket)))
          (flet ((ask (form tag)
                   (setf heard nil)
                   (mux:wire-send wire form)
                   (mux:wire-flush wire)
                   (step-until server (lambda ()
                                        (setf heard (append heard (heard-back wire)))
                                        (find tag heard :key #'first)))
                   (find tag heard :key #'first)))
            (is-true (step-until server (lambda () (eq :blocked (agent:agent-state (mux:pane-agent pane)))))
                     "the pane never read as blocked")
            (let ((it (fourth (ask (list :agent-observe "0" id) :agent-observed))))
              (is (equal "probe" (getf it :kind)))
              (is (equal '("Yes" "No") (getf (getf it :observation) :options)))
              (is (subsetp '(:approve :deny :choose) (getf it :offered))))
            (let ((said (ask (list :agent-act "0" id :submit "hello") :agent-acted)))
              (is (eq :not-offered (fifth said)) "~S" said)
              (is (member :approve (sixth said))))
            (let ((said (ask (list :agent-act "0" id :approve nil) :agent-acted)))
              (is (eq :done (fifth said)) "~S" said))
            (mux:wire-close wire)))))))

(test readers-loaded-into-a-running-server-are-taken-up-by-its-panes
  (let ((agent:*readers* nil))
    (with-a-server-here (server path)
      (let* ((session (mux:add-session server "printf '\\033]0;latecomer\\007'; exec cat" :rows 6 :cols 20))
             (pane (first (mux:session-panes session)))
             (socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
             (heard nil))
        (is-true (step-until server (lambda () (equal "latecomer" (mux::pane-named pane)))))
        (is (equal "agent" (agent:agent-kind (mux:pane-agent pane))))
        (sb-bsd-sockets:socket-connect socket path)
        (let ((wire (mux:make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket)))
          (mux:wire-send wire (list :readers-load
                                    (list "(latecomer :programs (\"latecomer\") :title \"^latecomer$\" :versions (\"1\") :screens ((:idle :widget :prompt-input :means :idle)))"
                                          "(nonsense")))
          (mux:wire-flush wire)
          (step-until server (lambda ()
                               (setf heard (append heard (heard-back wire)))
                               (find :readers-loaded heard :key #'first)))
          (let ((said (find :readers-loaded heard :key #'first)))
            (is (equal '("latecomer 1") (second said)))
            (is (= 1 (length (third said)))))
          (is (equal "latecomer" (agent:agent-kind (mux:pane-agent pane))))
          (mux:wire-close wire))))))

;;; One server holds every session.

(defun a-wire-to (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    (mux:make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket)))

(defun say-to (wire &rest forms)
  (dolist (form forms) (mux:wire-send wire form))
  (mux:wire-flush wire))

(defun heard-from (server wire tag)
  "Step SERVER until WIRE has been told something tagged TAG, and answer it."
  (let ((heard nil))
    (step-until server (lambda ()
                         (setf heard (append heard (heard-back wire)))
                         (find tag heard :key #'first)))
    (find tag heard :key #'first)))

(defun session-names (server)
  (mapcar #'mux:session-name (mux:server-sessions server)))

(test opening-a-session-makes-it-and-opening-it-again-joins-it
  (with-a-server-here (server path)
    (let ((one (a-wire-to path))
          (two (a-wire-to path)))
      (say-to one (list :open "work" "cat" nil 6 20 t))
      (say-to two (list :open "work" "cat" nil 6 20 t))
      (is-true (step-until server (lambda ()
                                    (let ((work (mux:session-named server "work")))
                                      (and work
                                           (= 2 (length (mux:session-watchers work))))))))
      (is (equal '("work") (session-names server))
          "two clients opening one name made ~S" (session-names server))
      (say-to one (list :open "play" "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:session-named server "play"))))
      (is (equal '("work" "play") (session-names server)))
      (mux:wire-close one)
      (mux:wire-close two))))

(test a-session-opened-with-no-name-is-given-one
  (with-a-server-here (server path)
    (let ((wire (a-wire-to path)))
      (say-to wire (list :open nil "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:server-sessions server))))
      (is (equal '("0") (session-names server)))
      (mux:wire-close wire))))

(test a-new-session-starts-where-it-was-asked-to
  (let ((dir (string-right-trim "/" (namestring (truename (uiop:temporary-directory))))))
    (with-a-server-here (server path)
      (let ((wire (a-wire-to path)))
        (say-to wire (list :open "here" "pwd -P; sleep 30" dir 6 60 t))
        (is-true (step-until server (lambda ()
                                      (let ((s (mux:session-named server "here")))
                                        (and s (search dir (term:term-dump-to-string
                                                            (mux:pane-term
                                                             (mux:session-focus s)))))))))
        (mux:wire-close wire)))))

(test stopping-one-session-leaves-the-others-and-moves-whoever-watched-it
  (with-a-server-here (server path)
    (let ((watching (a-wire-to path))
          (asking (a-wire-to path)))
      (say-to watching (list :open "keep" "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:session-named server "keep"))))
      (say-to watching (list :open "drop" "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:session-named server "drop"))))
      (say-to asking (list :kill-session "drop"))
      (is (equal (list :killed "drop" t) (heard-from server asking :killed)))
      (is (equal '("keep") (session-names server)))
      (is-true (mux:server-going server) "stopping one session stopped the server")
      (is-true (step-until server (lambda ()
                                    (mux:session-watchers (mux:session-named server "keep"))))
               "whoever watched the stopped session was not moved to the one left")
      (say-to asking (list :kill-session "nothing-by-this-name"))
      (is (equal (list :killed "nothing-by-this-name" nil) (heard-from server asking :killed)))
      (mux:wire-close watching)
      (mux:wire-close asking))))

(test the-sessions-say-how-many-of-their-panes-need-you
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (wire (a-wire-to path)))
      ;; a pane that has just started is still drawing, and anything it is said
      ;; to be doing is taken back the moment it draws; so it is let settle first
      (let ((agent (mux:pane-agent (mux:session-focus session))))
        (step-until server (lambda () (eq :idle (agent:agent-state agent))))
        (agent:agent-hear agent :blocked)
        (is-true (step-until server (lambda () (eq :blocked (agent:agent-state agent))))))
      (say-to wire '(:sessions))
      (let ((row (first (second (heard-from server wire :these)))))
        (is (equal "work" (first row)))
        (is (eql 1 (sixth row)) "the row was ~S" row))
      (mux:wire-close wire))))

(test a-server-says-it-holds-every-session-when-knocked-on
  (with-server (path :command "sleep 30")
    (is (member :one-server (mux:knocked path 3)))))

(test a-server-with-nothing-yet-waits-for-its-first-session-and-no-longer
  (with-a-server-here (server path)
    (let ((now (mux::server-born server)))
      (is-true (mux::server-wanted-p server now)
               "a server just started with no session gave up at once")
      (is (null (mux::server-wanted-p server (+ now mux::+first-session-patience+)))
          "a server nobody reached went on holding its socket")
      (mux:add-session server "cat" :name "work" :rows 6 :cols 20)
      (is-true (mux::server-wanted-p server (+ now (* 2 mux::+first-session-patience+))))
      (mux::end-the-session server (first (mux:server-sessions server)) :done)
      (is (null (mux::server-wanted-p server now))
          "a server whose last session ended went on"))))

(test a-session-name-with-a-directory-in-it-is-not-a-socket-somewhere-else
  (let ((mux:*server-name* "../../elsewhere"))
    (is (equal "elsewhere" (file-namestring (mux:socket-path))))
    (is (search (namestring (mux:mux-dir)) (mux:socket-path)))))

;;; Who typed into a pane, and what it is called.

(test keys-typed-at-a-pane-are-counted-by-who-typed-them-and-not-kept
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (pane (mux:session-focus session))
           (wire (a-wire-to path)))
      (say-to wire (list :who "/dev/ttys042") (list :want "work") (list :attach 6 20 t))
      (is-true (step-until server (lambda () (mux:session-watchers session))))
      (say-to wire (list :keys "abc"))
      (is-true (step-until server (lambda () (mux::pane-log pane))))
      (let ((entry (first (mux::pane-log pane))))
        (is (eq :keys (third entry)))
        (is (eql 3 (fourth entry)))
        (is (eq :client (first (second entry))))
        (is (equal "/dev/ttys042" (third (second entry))) "~S" entry))
      (say-to wire (list :keys "de"))
      (is-true (step-until server (lambda () (eql 5 (fourth (first (mux::pane-log pane)))))))
      (is (eql 1 (length (mux::pane-log pane)))
          "keys typed in one breath became ~D entries" (length (mux::pane-log pane)))
      (is (null (search "abc" (format nil "~S" (mux::pane-log pane))))
          "what was typed was kept")
      (mux:wire-close wire))))

(test a-pane-keeps-a-pulse-of-its-output-and-its-worst-state
  (let ((pane (mux:make-pane "cat")))
    (is (= 16 (length (mux::pane-pulse pane))))
    ;; some output, quietly
    (setf (mux::pane-output pane) 100)
    (mux::pane-pulse-roll pane 1000 :unknown)
    (is (equal '(100 . :unknown) (car (last (mux::pane-pulse pane)))))
    (is (eql 0 (mux::pane-output pane)) "what was read was not folded in")
    ;; more, and it asked: asking outranks
    (setf (mux::pane-output pane) 5)
    (mux::pane-pulse-roll pane 2000 :blocked)
    (is (equal '(105 . :blocked) (car (last (mux::pane-pulse pane)))))
    (mux::pane-pulse-roll pane 3000 :idle)
    (is (eq :blocked (cdr (car (last (mux::pane-pulse pane))))) "a better state took over the cell")
    ;; two cells later the old cell has moved two along and the new one is empty
    (mux::pane-pulse-roll pane (+ 3000 (* 2 mux::+pulse-every+)) :working)
    (is (= 16 (length (mux::pane-pulse pane))))
    (is (equal '(105 . :blocked) (nth 13 (mux::pane-pulse pane))))
    (is (equal '(0 . :working) (nth 15 (mux::pane-pulse pane))))
    ;; an age away, everything is fresh
    (mux::pane-pulse-roll pane (* 100 mux::+pulse-every+) nil)
    (is (every (lambda (c) (zerop (car c))) (mux::pane-pulse pane)))))

(test what-happens-in-a-pane-is-an-event-the-server-lists-newest-first
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (pane (mux:session-focus session))
           (wire (a-wire-to path)))
      (say-to wire (list :who "/dev/ttys042") (list :want "work") (list :attach 6 20 t))
      (is-true (step-until server (lambda () (mux:session-watchers session))))
      (say-to wire (list :keys "abc"))
      (is-true (step-until server (lambda () (find :typed (mux::pane-events pane) :key #'third))))
      (let ((typed (find :typed (mux::pane-events pane) :key #'third)))
        (is (equal "/dev/ttys042" (third (fourth typed))) "~S" typed))
      (say-to wire (list :keys "de"))
      (is-true (step-until server (lambda () (eql 5 (fourth (first (mux::pane-log pane)))))))
      (is (eql 1 (count :typed (mux::pane-events pane) :key #'third)) "a run of keys is one event")
      ;; something read as a state is an event too, with what it asks
      (mux::pane-noted pane (mux::now-ms) :asks nil "Bash command")
      (say-to wire (list :events 10))
      (let ((events (second (heard-from server wire :events))))
        (is (eq :asks (third (first events))) "newest first: ~S" events)
        (is (equal "work" (fourth (first events))))
        (is (eql (mux:pane-id pane) (fifth (first events))))
        (is (eql 1 (sixth (first events))) "the window is not said: ~S" (first events))
        (is (equal "Bash command" (eighth (first events))))
        (is (find :typed events :key #'third))
        (is (every (lambda (e) (integerp (first e))) events) "ages are not numbers: ~S" events))
      ;; and the pulses go out for every pane, sixteen cells each
      (say-to wire (list :pulses))
      (let ((pulses (second (heard-from server wire :pulses))))
        (is (= 1 (length pulses)))
        (destructuring-bind (name id cells) (first pulses)
          (is (equal "work" name))
          (is (eql (mux:pane-id pane) id))
          (is (= 16 (length cells)))))
      (mux:wire-close wire))))

(test a-program-starting-and-finishing-in-the-foreground-is-noted
  (let ((pane (mux:make-pane "sh")))
    (setf (mux::pane-programs pane) (list "-zsh"))
    (mux::pane-noted pane 1 :quiet nil)
    (is (= 1 (length (mux::pane-events pane))))
    ;; the events a foreground change makes, without a pty: the same test the notice runs
    (let ((was (mux::program-name (first (mux::pane-programs pane))))
          (is (mux::program-name "python3 -m pytest")))
      (is (equal "zsh" was))
      (is (equal "python3" is)))))

(test a-session-is-renamed-by-whoever-asks-and-everybody-attached-hears
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (other (mux:add-session server "cat" :name "play" :rows 6 :cols 20))
           (wire (a-wire-to path)))
      (declare (ignore other))
      (say-to wire (list :want "work") (list :attach 6 20 t))
      (is-true (step-until server (lambda () (mux:session-watchers session))))
      (say-to wire (list :name-session "work" " todo "))
      (is (equal (list :session-renamed "work" "todo") (heard-from server wire :session-renamed)))
      (is (equal "todo" (mux:session-name session)))
      (say-to wire (list :name-session "todo" "play"))
      (is (equal (list :session-named "todo" "play" :taken) (heard-from server wire :session-named)))
      (is (equal "todo" (mux:session-name session)) "a taken name was given anyway")
      (say-to wire (list :name-session "todo" "  "))
      (is (equal (list :session-named "todo" "" :empty) (heard-from server wire :session-named)))
      (say-to wire (list :name-session "nobody" "x"))
      (is (equal (list :session-named "nobody" "x" :gone) (heard-from server wire :session-named)))
      (mux:wire-close wire))))

(test a-client-hearing-a-session-renamed-keeps-everything-under-the-new-name
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "todo")))
    (setf (mux::client-over client) (list b)
          (mux::client-session client) "todo")
    (board-screen client b)
    (setf (gethash "todo" (mux::board-slid b)) 4)
    (mux::client-heard client (list :session-renamed "todo" "done"))
    (is (equal "done" (mux::client-session client)))
    (is (equal "done" (first (mux::board-cursor b))))
    (is (eql 4 (gethash "done" (mux::board-slid b))))
    (is (null (gethash "todo" (mux::board-slid b))))
    (is-true (gethash "done" (mux::client-layouts client)) "the layouts were not moved")
    (is-true (gethash (cons "done" 2) (mux::client-panes client)) "the pane rows were not moved")
    (is (equal "done" (getf (mux::pane-row-of client "done" 2) :session)) "a row still says the old name")
    (let ((all (board-screen client b)))
      (is (search "▶ done" all) "the board does not show the new name: ~A" all)
      (is (null (search "todo" all)) "the old name is still on the board: ~A" all))
    ;; r on the board asks for the cursor's session's name on the line at the foot
    (let ((mux::*client* client)) (mux::switchboard-rename))
    (is (typep (first (mux::client-over client)) 'mux::entry))
    (is (equal "session done" (mux::entry-of-what (first (mux::client-over client)))))
    (is (equal "done" (mux::entry-of-text (first (mux::client-over client)))))))

(test the-session-is-named-from-the-keys-and-the-bar-says-the-new-name
  (with-server (path :command "cat" :rows 10 :cols 100)
    (with-seer (seer path :rows 10 :cols 100)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~C$" mux:+prefix+))
      (is-true (pump seer :want " name  session ") "no line at the foot to name the session: ~S" (seen seer))
      (type-at seer (format nil "~C~C~Ctodo~C" #\Rubout #\Rubout #\Rubout #\Return))
      (is-true (pump seer :want " todo │") "the bar does not say the new name: ~S" (seen seer)))))

(test a-prompt-says-which-pane-sent-it-and-a-refused-one-says-so
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (pane (mux:session-focus session))
           (id (mux:pane-id pane))
           (wire (a-wire-to path)))
      (say-to wire (list :agent-prompt "work" id "run the suite" "todo:4"))
      (heard-from server wire :agent-prompted)
      (let ((entry (first (mux::pane-log pane))))
        (is (equal '(:pane "todo:4") (second entry)))
        (is (eq :prompt (third entry)))
        (is (equal "run the suite" (fourth entry)))
        (is (eq t (fifth entry))))
      (let ((agent (mux:pane-agent pane)))
        (step-until server (lambda () (eq :idle (agent:agent-state agent))))
        (agent:agent-hear agent :blocked)
        (step-until server (lambda () (eq :blocked (agent:agent-state agent)))))
      (say-to wire (list :agent-prompt "work" id "STATUS?"))
      (is (eq :blocked (fourth (heard-from server wire :agent-prompted))))
      (let ((entry (first (mux::pane-log pane))))
        (is (equal '(:cli) (second entry)) "~S" entry)
        (is (eq :refused (fifth entry))))
      (mux:wire-close wire))))

(test a-pane-log-is-kept-to-a-length
  (let ((pane (mux:make-pane "cat")))
    (dotimes (i (* 3 mux::+log-length+))
      (mux::pane-logged pane (* 10000 i) '(:cli) :say "x"))
    (is (<= (length (mux::pane-log pane)) mux::+log-length+))))

(test a-pane-is-named-by-whoever-asks-and-named-nothing-by-an-empty-name
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (pane (mux:session-focus session))
           (id (mux:pane-id pane))
           (wire (a-wire-to path)))
      (say-to wire (list :name-pane "work" id "  impl "))
      (is (equal (list :named "work" id t) (heard-from server wire :named)))
      (is (equal "impl" (mux::pane-label pane)))
      (is (equal "impl" (mux::pane-says pane)))
      (say-to wire (list :name-pane "work" id ""))
      (heard-from server wire :named)
      (is (null (mux::pane-label pane)))
      (say-to wire (list :name-pane "work" 9999 "nobody"))
      (is (equal (list :named "work" 9999 nil) (heard-from server wire :named)))
      (mux:wire-close wire))))

(test naming-the-focused-pane-is-asked-of-the-server
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (pane (mux:session-focus session))
           (wire (a-wire-to path)))
      (setf (mux::pane-label pane) "arch")
      (say-to wire (list :want "work") (list :attach 6 20 t) '(:naming))
      (is (equal (list :name-it "work" (mux:pane-id pane) "arch" nil "work:1.1")
                 (heard-from server wire :name-it)))
      (say-to wire '(:naming-window))
      (is (equal (list :name-window-of "work" 1 nil) (heard-from server wire :name-window-of)))
      (mux:wire-close wire))))

;;; Prompts: sessions and windows, commands with their keys, a click chooses.

(test the-window-prompt-lists-every-session-and-window-asking-first-and-goes-there
  (let ((choices (mux::window-choices
                  '(("todo" 24 80 3 1 1 ((1 "agents" 2 1 t) (2 nil 1 0 nil)))
                    ("lib" 24 80 1 0 0 ((1 nil 1 0 t))))
                  "todo")))
    (is (eql 3 (length choices)))
    (is (equal '("todo" 1) (list (getf (first choices) :session) (getf (first choices) :window)))
        "the window with a question is not first: ~S" choices)
    (is (search "▲ 1" (mux::window-choice-line (first choices))))
    (is (search "◆ here" (mux::window-choice-line (first choices))))
    (is (null (search "◆ here" (mux::window-choice-line (third choices))))
        "another session is marked as where this client is")
    (is (search "lib" (mux::window-choice-line (third choices)))))
  (with-server (path :command "cat" :rows 12 :cols 80)
    (with-seer (seer path :rows 12 :cols 80)
      (type-at seer "in-the-first")
      (is-true (pump seer :want "in-the-first"))
      (type-at seer (format nil "~Cc" mux:+prefix+))
      (is-true (pump seer :until (lambda () (null (search "in-the-first" (seen seer))))))
      (type-at seer (format nil "~C'" mux:+prefix+))
      (is-true (pump seer :want "windows") "the window prompt did not open: ~S" (seen seer))
      (is-true (pump seer :want "› 2 ") "window 2 is not listed: ~S" (seen seer))
      ;; the rows are buttons: a click on window 1's row goes there
      (multiple-value-bind (x y) (where-on seer "› 1 " :from 1)
        (is-true x "no row for window 1: ~S" (seen seer))
        (when x (click-at seer x y)))
      (is-true (pump seer :want "in-the-first") "clicking the row did not go there: ~S" (seen seer)))))

(test the-command-prompt-shows-each-commands-key-and-what-it-does
  (is (search "C-b 3" (mux::command-line "split right")))
  (is (search "beside" (mux::command-line "split right")))
  (is (string= "PANES" (symbol-name (mux::command-group "split right"))))
  (let ((rows (mux::keys-help-rows)))
    (is (string= "PANES" (symbol-name (third (first rows)))) "the help does not start with the panes: ~S" (first rows))
    (is (find "needs you" rows :key #'first :test #'equal))))

;;; What a client is told about every pane, not just the ones it is looking at.

(defun settled (server pane)
  (let ((agent (mux:pane-agent pane)))
    (step-until server (lambda () (member (agent:agent-state agent) '(:idle :blocked))))))

(defun blocked (server pane)
  (settled server pane)
  (agent:agent-hear (mux:pane-agent pane) :blocked)
  (step-until server (lambda () (eq :blocked (agent:agent-state (mux:pane-agent pane))))))

(test every-pane-in-every-session-can-be-asked-for-as-a-plist
  (with-a-server-here (server path)
    (let* ((one (mux:add-session server "cat" :name "one" :rows 6 :cols 20))
           (two (mux:add-session server "cat" :name "two" :rows 6 :cols 20))
           (wire (a-wire-to path)))
      (setf (mux::pane-label (mux:session-focus two)) "impl")
      (settled server (mux:session-focus one))
      (say-to wire '(:panes))
      (let ((rows (second (heard-from server wire :panes))))
        (is (equal '("one" "two") (mapcar (lambda (r) (getf r :session)) rows)))
        (is (equal "impl" (getf (second rows) :label)))
        (is (equal "cat" (getf (first rows) :kind)))
        (is (eq t (getf (first rows) :focus)))
        (is (integerp (getf (first rows) :for)) "~S" (first rows)))
      (mux:wire-close wire))))

(test a-client-watching-the-panes-is-told-what-changed-and-what-went
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (first-pane (mux:session-focus session))
           (wire (a-wire-to path))
           (heard nil))
      (flet ((listen-for (test)
               (step-until server (lambda ()
                                    (setf heard (append heard (heard-back wire)))
                                    (find-if test heard)))))
        (say-to wire '(:watch-panes t))
        (is-true (listen-for (lambda (f) (eq :pane (first f))))
                 "watching told nothing about the pane there already")
        (setf heard nil)
        (setf (mux::pane-label first-pane) "arch")
        (is-true (listen-for (lambda (f) (and (eq :pane (first f))
                                              (equal "arch" (getf (rest f) :label)))))
                 "a new name was not told")
        (let ((new (mux:split-the-session session :across)))
          (setf heard nil)
          (is-true (listen-for (lambda (f) (and (eq :pane (first f))
                                                (eql (mux:pane-id new) (getf (rest f) :id)))))
                   "a new pane was not told")
          (setf heard nil)
          (mux:close-the-pane session new)
          (is-true (listen-for (lambda (f) (equal (list :pane-gone "work" (mux:pane-id new)) f)))
                   "a pane that went was not told: ~S" heard))
        (setf heard nil)
        (step-until server (lambda () (setf heard (append heard (heard-back wire)))) 1/2)
        (is (null (remove :pane heard :key #'first :test-not #'eq))
            "a pane that had not changed was told again: ~S" heard))
      (mux:wire-close wire))))

(test a-panes-screen-comes-as-cells-with-its-faces
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "printf 'one\\n\\033[31mtwo\\033[0m\\n'; sleep 30"
                                     :name "work" :rows 6 :cols 20))
           (pane (mux:session-focus session))
           (wire (a-wire-to path)))
      (step-until server (lambda () (search "two" (term:term-dump-to-string (mux:pane-term pane)))))
      (say-to wire (list :pane-screen "work" (mux:pane-id pane) 2))
      (destructuring-bind (width said faces) (cdddr (heard-from server wire :pane-screen))
        ;; a column of the twenty is the scrollbar's
        (is (eql 19 width))
        (let ((screen (tty:make-screen :width width :height 2)))
          (mux:said-into-screen screen said faces)
          (is (equal "one" (shown screen 0)))
          (is (equal "two" (shown screen 1)))
          (is (eql 1 (term:face-fg (term:row-face (tty:screen-row screen 1) 0)))
              "the colour did not come with it")))
      (mux:wire-close wire))))

(test a-client-watching-screens-is-sent-a-pane-when-it-moves
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 30))
           (pane (mux:session-focus session))
           (wire (a-wire-to path))
           (heard nil))
      (say-to wire '(:watch-screens 3))
      (step-until server (lambda ()
                           (setf heard (append heard (heard-back wire)))
                           (find :pane-screen heard :key #'first)))
      (is-true (find :pane-screen heard :key #'first) "watching sent no screen at all")
      (setf heard nil)
      (mux:pane-say pane (format nil "moved-it~C" #\Return))
      (is-true (step-until server
                           (lambda ()
                             (setf heard (append heard (heard-back wire)))
                             (some (lambda (f)
                                     (and (eq :pane-screen (first f))
                                          (search "moved-it" (format nil "~S" f))))
                                   heard)))
               "the pane moved and its screen was not sent")
      (mux:wire-close wire))))

(test an-answer-is-typed-only-into-a-pane-that-is-asking
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 30))
           (pane (mux:session-focus session))
           (id (mux:pane-id pane))
           (wire (a-wire-to path)))
      (settled server pane)
      (say-to wire (list :answer "work" id 1))
      (is (equal (list :answered "work" id 1 :not-blocked) (heard-from server wire :answered)))
      (is (eq :refused (fifth (first (mux::pane-log pane)))))
      (blocked server pane)
      (say-to wire (list :answer "work" id 1 "todo:4"))
      (is (equal (list :answered "work" id 1 t) (heard-from server wire :answered)))
      (let ((entry (first (mux::pane-log pane))))
        (is (eq :answer (third entry)))
        (is (equal '(:pane "todo:4") (second entry))))
      (is-true (step-until server (lambda () (search "1" (term:term-dump-to-string
                                                          (mux:pane-term pane)))))
               "the answer was not typed")
      (say-to wire (list :answer "work" 9999 1))
      (is (equal (list :answered "work" 9999 1 :gone) (heard-from server wire :answered)))
      (mux:wire-close wire))))

(test focusing-a-pane-in-another-session-takes-the-client-there
  (with-a-server-here (server path)
    (let* ((one (mux:add-session server "cat" :name "one" :rows 6 :cols 20))
           (two (mux:add-session server "cat" :name "two" :rows 6 :cols 20))
           (other (mux:split-the-session two :across))
           (wire (a-wire-to path)))
      (setf (mux:session-focus two) (first (mux:session-panes two)))
      (say-to wire (list :want "one") (list :attach 6 20 t))
      (is-true (step-until server (lambda () (mux:session-watchers one))))
      (say-to wire (list :focus-pane "two" (mux:pane-id other)))
      (is (equal (list :focused "two" (mux:pane-id other) t) (heard-from server wire :focused)))
      (is (null (mux:session-watchers one)) "the client stayed on the first session")
      (is (eql 1 (length (mux:session-watchers two))))
      (is (eq other (mux:session-focus two)))
      (mux:wire-close wire))))

(test a-panes-history-and-log-come-with-how-long-ago
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (pane (mux:session-focus session))
           (id (mux:pane-id pane))
           (wire (a-wire-to path)))
      (settled server pane)
      (say-to wire (list :agent-keys "work" id "x" "todo:4"))
      (step-until server (lambda () (mux::pane-log pane)))
      (say-to wire (list :pane-history "work" id))
      (let ((history (fourth (heard-from server wire :pane-history))))
        (is (plusp (length history)) "no history")
        (is (every (lambda (h) (and (integerp (first h)) (>= (first h) 0)
                                    (keywordp (second h))))
                   history)
            "~S" history))
      (say-to wire (list :pane-log "work" id 5))
      (let ((entry (first (fourth (heard-from server wire :pane-log)))))
        (is (integerp (first entry)))
        (is (equal '(:pane "todo:4") (second entry)))
        (is (eq :say (third entry)))
        ;; the time of day it was goes with it, kept by the server, so the
        ;; drawer shows the same second every time it asks
        (is (typep (sixth entry) '(integer 0)) "~S" entry)
        (is (<= (abs (- (get-universal-time) (sixth entry))) 5) "~S" entry))
      (mux:wire-close wire))))

;;; The frames and the bar, through a real client.

(defun a-dialog-script ()
  "A script that sets the title a coding agent sets, draws a permission dialog
under a rule, and then echoes whatever it is answered."
  (let ((path (format nil "~Aatty-dialog-~D.sh" (uiop:temporary-directory) (sb-posix:getpid))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (format out "printf '\\033]0;✳ Claude Code\\007'~%")
      (dolist (line '("────────────────────────────────────"
                      " Bash command" ""
                      "   python3 -m pytest -q" ""
                      " Do you want to proceed?"
                      " ❯ 1. Yes"
                      "   2. No, and tell Claude what to do differently (esc)" ""
                      " Esc to cancel · Tab to amend"))
        (format out "printf '%s\\n' '~A'~%" line))
      (format out "stty -echo -icanon min 1; a=$(head -c 1); printf '\\033[2J\\033[H'; echo answered-with-$a; sleep 30~%"))
    path))

(defun where-on (seer said &key (from 0))
  "Column and row, from 0, where SAID first is on the seer's screen, looking
from row FROM down."
  (loop :for y :from from :below (term:term-height (seer-host seer))
        :for x := (search said (term:term-dump-row-string (seer-host seer) y))
        :when x :do (return (values x y))))

(defun click-at (seer x y)
  (type-at seer (format nil "~C[<0;~D;~DM~C[<0;~D;~Dm" #\Escape (1+ x) (1+ y)
                        #\Escape (1+ x) (1+ y))))

(test a-split-frame-says-which-pane-what-it-is-called-and-what-it-is-doing
  (with-server (path :command "sleep 30" :rows 12 :cols 80)
    (with-seer (seer path :rows 12 :cols 80)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~C3" mux:+prefix+))
      (is-true (pump seer :until (lambda () (search "┏" (seen seer))))
               "no double frame round the focused pane: ~S" (seen seer))
      (type-at seer (format nil "~C." mux:+prefix+))
      (is-true (pump seer :want "name ") "the name prompt did not open: ~S" (seen seer))
      (type-at seer (format nil "impl~C" #\Return))
      (is-true (pump seer :want " impl sleep")
               "the frame does not carry the new name and the program: ~S" (seen seer))
      (is (null (search "idle" (seen seer)))
          "a frame said what a program nobody knows is doing: ~S" (seen seer)))))

(test a-blocked-pane-is-answered-by-clicking-an-answer-in-its-border
  (let ((script (a-dialog-script)))
    (unwind-protect
         (with-server (path :command "/bin/sh" :rows 16 :cols 90)
           (with-seer (seer path :rows 16 :cols 90)
             (pump seer :seconds 1/2)
             (type-at seer (format nil "~C3" mux:+prefix+))
             (pump seer :until (lambda () (search "┏" (seen seer))))
             (pump seer :seconds 1/2)
             (type-at seer (format nil "sh ~A~C" script #\Return))
             (is-true (pump seer :want "▲ asks") "the question never reached the border: ~S"
                      (seen seer))
             (is-true (pump seer :want " 1 Yes") "the answers are not in the border: ~S" (seen seer))
             (multiple-value-bind (x y) (where-on seer " 1 Yes")
               (click-at seer (+ x 1) y))
             (is-true (pump seer :want "answered-with-1")
                      "clicking the answer did not type it: ~S" (seen seer))))
      (ignore-errors (delete-file script)))))

(test zooming-a-pane-gives-it-the-session-and-zooming-again-gives-it-back
  (with-server (path :command "cat" :rows 12 :cols 80)
    (with-seer (seer path :rows 12 :cols 80)
      (type-at seer "in-the-first")
      (is-true (pump seer :want "in-the-first"))
      (type-at seer (format nil "~C3" mux:+prefix+))
      (pump seer :until (lambda () (search "┏" (seen seer))))
      (type-at seer "in-the-second")
      (is-true (pump seer :want "in-the-second"))
      (type-at seer (format nil "~Cz" mux:+prefix+))
      (is-true (pump seer :until (lambda () (null (search "in-the-first" (seen seer)))))
               "the other pane still shows while one is zoomed: ~S" (seen seer))
      (is (search "in-the-second" (seen seer)))
      (type-at seer (format nil "~Cz" mux:+prefix+))
      (is-true (pump seer :want "in-the-first") "zooming again did not give it back: ~S"
               (seen seer)))))

(test going-to-the-blocked-pane-goes-to-it-in-whatever-session-it-is
  (with-a-server-here (server path)
    (let* ((here (mux:add-session server "cat" :name "here" :rows 6 :cols 30))
           (there (mux:add-session server "cat" :name "there" :rows 6 :cols 30))
           (wire (a-wire-to path)))
      (blocked server (mux:session-focus there))
      (say-to wire (list :want "here") (list :attach 6 30 t))
      (is-true (step-until server (lambda () (mux:session-watchers here))))
      (say-to wire '(:go-to-blocked))
      (is (equal (list :focused "there" (mux:pane-id (mux:session-focus there)) t)
                 (heard-from server wire :focused)))
      (is (mux:session-watchers there) "the client was not taken to the blocked pane")
      (agent:agent-hear (mux:pane-agent (mux:session-focus there)) :idle)
      (step-until server (lambda () (eq :idle (agent:agent-state
                                               (mux:pane-agent (mux:session-focus there))))))
      (say-to wire '(:go-to-blocked))
      (is (equal '(:say "nothing needs you") (heard-from server wire :say)))
      (mux:wire-close wire))))

;;; Needs you.

(defun a-told-client (&rest rows)
  "A client that has been told about ROWS, without a server behind it."
  (let ((client (mux::%make-client)))
    (dolist (row rows client)
      (setf (gethash (cons (getf row :session) (getf row :id)) (mux::client-panes client))
            (list* :heard-at (mux::ms-here)
                   ;; a row made up here is a known agent unless it says not
                   (if (member :known row) row (list* :known t row)))))))

(test the-queue-holds-what-is-asking-oldest-first-and-can-be-narrowed
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :says "impl" :kind "claude-code" :state :blocked
                        :for 42000 :asks '(:subject "Bash command" :options ((1 "Yes"))))
                  (list :session "lib" :id 1 :says "core" :kind "claude-code" :state :blocked
                        :for 8000 :asks '(:subject "Fetch" :options ((1 "Yes"))))
                  (list :session "todo" :id 1 :says "arch" :kind "claude-code" :state :working
                        :for 41000)))
         (q (mux::%make-queue)))
    (is (equal '("todo:2" "lib:1") (mapcar #'mux::row-address (mux::queue-rows q client)))
        "the queue was not what is blocked, oldest first")
    (setf (mux::queue-query q) "fetch")
    (is (equal '("lib:1") (mapcar #'mux::row-address (mux::queue-rows q client))))))

(test the-queue-draws-each-question-with-its-answers-and-the-one-picked-beside-it
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :says "impl" :kind "claude-code" :state :blocked
                        :for 42000 :asks '(:subject "Bash command"
                                           :detail ("python3 -m pytest -q")
                                           :options ((1 "Yes") (2 "No"))))))
         (q (mux::%make-queue))
         (screen (tty:make-screen :width 100 :height 20)))
    (let ((mux::*drawing-for* client))
      (mux:draw-over q screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 20 :collect (shown screen y)))))
      (is (search "needs you" all) "~A" all)
      (is (search "todo › impl · as it is now" all) "~A" all)
      (is (search "Bash command" all))
      (is (search "python3 -m pytest -q" all))
      (is (search " 1 Yes" all))
      (is (search "1 waiting · oldest 42s" all) "~A" all))))

(test the-queue-answers-a-pane-in-another-session-without-going-there
  (let ((script (a-dialog-script)))
    (unwind-protect
         (with-server (path :command "/bin/sh" :rows 20 :cols 100)
           (with-seer (seer path :rows 20 :cols 100)
             (pump seer :seconds 1/2)
             (type-at seer (format nil "sh ~A~C" script #\Return))
             (is-true (pump seer :want "needs you") "the bar never said anything needs you")
             ;; somewhere else to be while it is answered
             (type-at seer (format nil "~CC" mux:+prefix+))
             (is-true (pump seer :until (lambda () (null (search "Bash command" (seen seer))))))
             (type-at seer (format nil "~CN" mux:+prefix+))
             (is-true (pump seer :want "1 waiting") "the queue did not open: ~S" (seen seer))
             (is-true (pump seer :want "python3 -m pytest -q"))
             (type-at seer "1")
             (is-true (pump seer :want "nothing needs you")
                      "answering did not take it off the queue: ~S" (seen seer))
             (is-true (pump seer :want "✓")
                      "the answer is not among those answered lately: ~S" (seen seer))
             (is-true (pump seer :want "◆ here") "~S" (seen seer))
             (type-at seer (string (code-char 27)))
             (pump seer :seconds 1/2)
             (type-at seer (format nil "~Cb" mux:+prefix+))
             (is-true (pump seer :want "◆ here"))
             (type-at seer (format nil "~C[A" #\Escape))
             (pump seer :seconds 1/4)
             (type-at seer (string #\Return))
             (is-true (pump seer :want "answered-with-1")
                      "the pane was not answered: ~S" (seen seer))))
      (ignore-errors (delete-file script)))))

;;; The switchboard.

(defun board-client ()
  (let ((client (a-told-client
                 (list :session "todo" :order 0 :at 0 :id 1 :says "arch" :kind "claude-code"
                       :state :working :for 41000 :doing "Recording clarification 9…"
                       :driven-by "todo:4")
                 (list :session "todo" :order 0 :at 1 :id 2 :says "impl" :kind "claude-code"
                       :state :blocked :for 42000
                       :asks '(:subject "Bash command" :detail ("python3 -m pytest -q")
                               :options ((1 "Yes") (2 "No"))))
                 (list :session "todo" :order 0 :at 2 :id 4 :says "ctl" :kind "atty"
                       :state :working :for 2000 :drives (list "todo:1"))
                 (list :session "lib" :order 1 :at 0 :id 7 :says "tests" :kind "make"
                       :state :idle :for 3000
                       :history '((3000 :idle) (60000 :working))))))
    (setf (mux::client-session client) "todo")
    client))

(defun board-screen (client b &key (cols 150) (rows 30))
  "B drawn for CLIENT on a screen COLS by ROWS, as one string."
  (let ((screen (tty:make-screen :width cols :height rows)))
    (setf (mux::client-rows client) rows (mux::client-cols client) cols)
    (let ((mux::*drawing-for* client)) (mux:draw-over b screen))
    (values (format nil "~{~A~%~}" (loop :for y :below rows :collect (shown screen y))) screen)))

(defun a-buffer-wire ()
  "A wire that keeps what is sent to it: the write end of a pipe nobody reads."
  (multiple-value-bind (r w) (sb-posix:pipe)
    (declare (ignore r))
    (mux:make-wire w)))

(defun sent-on (wire)
  (sb-ext:octets-to-string (subseq (mux::wire-out wire) 0 (mux::wire-end wire))
                           :external-format :utf-8))

(defun five-windows (client)
  "The todo session laid out as five windows, the first split."
  (setf (mux::client-id client) 7
        (mux::client-clients client) '((7 "/dev/ttys042" 40 150 "todo" 1 1000 nil)
                                       (9 "/dev/ttys051" 30 100 "lib" 1 5000 200))
        (gethash "todo" (mux::client-layouts client))
        '((1 "agents" (:across 1 (:down 2 4)) 2 t) (2 "notes" 1 1 nil) (3 "ctl" 4 4 nil)
          (4 "more" 2 2 nil) (5 "last" 1 1 nil)))
  client)

(test the-switchboard-is-sessions-and-their-windows-with-their-panes-inside
  (let* ((client (board-client))
         (b (mux::%make-board :starting "todo")))
    (is (equal '("todo" "lib") (mux::board-sessions b client)) "the session asking is not first")
    (setf (mux::board-sort b) :name)
    (is (equal '("lib" "todo") (mux::board-sessions b client)) "sorting by name did nothing")
    (setf (mux::board-sort b) :asking)
    ;; with no layouts said yet, the panes of a window sit side by side
    (let ((windows (mux::windows-of client "todo")))
      (is (eql 1 (length windows)) "~S" windows)
      (is (equal '(:across 1 2 4) (third (first windows))) "~S" windows))
    ;; the cursor starts on the first pane of the session this began on
    (is (equal '("todo" 1 1) (mux::board-place b client)))
    (setf (mux::board-query b) "tests")
    (is (equal '("lib") (mux::board-sessions b client)))
    (setf (mux::board-query b) "")
    ;; layouts the server said win over the guess
    (setf (gethash "todo" (mux::client-layouts client))
          '((1 "agents" (:down 1 (:across 2 4)) 2 t) (2 "notes" 7 7 nil)))
    (is (eql 2 (length (mux::windows-of client "todo"))))
    (is (equal '(1 2 4) (mux::panes-in-tree '(:down 1 (:across 2 4)))))))

(test the-switchboard-draws-a-side-pane-and-a-lane-of-cards-for-every-session
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "todo"))
         (all (board-screen client b)))
    (is (search "switchboard" all) "~A" all)
    (is (search "default · 2 sessions" all) "~A" all)
    (is (search "SESSIONS" all))
    (is (search "CLIENTS" all))
    (is (search "◆ here" all) "this terminal is not among the clients: ~A" all)
    (is (search "⌨ ttys051" all) "the other terminal is not: ~A" all)
    (is (search "▶ todo" all) "the cursor's session is not marked in the tree: ~A" all)
    (is (search "├ 1 agents" all) "the cursor's session is not unfolded to its windows: ~A" all)
    (is (null (search "└ 1 tests" all)) "the other session is not folded: ~A" all)
    (is (search "20m ago" all) "the sparklines have no ruler: ~A" all)
    (is (search "ACTIVITY LOG" all) "~A" all)
    (is (search "SERVERS" all) "~A" all)
    (is (search "↵ go" all) "the cursor's card does not say go: ~A" all)
    (is (search "← → window" all) "the cursor's lane foot has no keys: ~A" all)
    (is (search "all 1 window" all) "the lane that fits does not say so: ~A" all)
    (is (search " 1 agents " all) "the window's card is not titled: ~A" all)
    (is (search "+ window" all) "a lane has no way to a new window: ~A" all)
    (is (search " 1 Yes " all) "the asking pane's answers are not on its card: ~A" all)
    (is (search "default › todo › 1 agents › 1 arch" all) "the status has no path: ~A" all)
    (is (search "Esc close" all) "the footer has no keys: ~A" all)
    (is (equal '("todo" 1 1) (mux::board-cursor b)))))

(test the-switchboard-moves-across-windows-and-down-sessions-and-zooms
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "todo")))
    (setf (mux::client-rows client) 40 (mux::client-cols client) 150)
    (mux::board-place b client)
    (let ((mux::*client* client))
      (setf (mux::client-over client) (list b))
      (mux::switchboard-right)
      (is (equal '("todo" 2 1) (mux::board-cursor b)) "right did not go to window 2: ~S" (mux::board-cursor b))
      (mux::switchboard-left)
      (is (equal '("todo" 1 1) (mux::board-cursor b)))
      (mux::switchboard-next-pane)
      (is (equal '("todo" 1 2) (mux::board-cursor b)) "TAB did not go to the next pane: ~S" (mux::board-cursor b))
      (mux::switchboard-last-window)
      (is (equal '("todo" 5 1) (mux::board-cursor b)) "End did not go to the last window: ~S" (mux::board-cursor b))
      (mux::switchboard-down)
      (is (equal "lib" (first (mux::board-cursor b))) "down did not go to the next session: ~S" (mux::board-cursor b))
      (is (eq :cards (mux::board-zoom b)))
      (mux::switchboard-zoom-in)
      (is (eq :one (mux::board-zoom b)))
      (mux::switchboard-zoom-out)
      (mux::switchboard-zoom-out)
      (is (eq :overview (mux::board-zoom b)))
      (mux::switchboard-zoom-in)
      (is (eq :cards (mux::board-zoom b))))))

(test a-lane-slides-by-the-cell-to-keep-the-cursor-in-sight-and-says-so-with-a-rail
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "todo")))
    (setf (mux::client-over client) (list b))
    (multiple-value-bind (all screen) (board-screen client b :cols 120 :rows 30)
      (declare (ignore all))
      ;; two windows fit past the side pane; the fifth is off to the right
      (let ((titles (shown screen 3)))
        (is (search " 1 agents " titles) "~S" titles)
        (is (null (search " 5 last " titles)) "the fifth window fits when it should not: ~S" titles))
      (is (find #\━ (shown screen 11)) "no rail under a lane wider than the view: ~S" (shown screen 11))
      (is (eql 0 (gethash "todo" (mux::board-slid b))))
      ;; under lib, which fits, the rail is a thin line with no ends
      (is (find #\─ (shown screen 21)) "~S" (shown screen 21))
      (is (null (find #\‹ (shown screen 21))) "~S" (shown screen 21))
      (is (null (find #\━ (shown screen 21))) "~S" (shown screen 21)))
    (let ((mux::*client* client))
      (mux::switchboard-last-window))
    (multiple-value-bind (all screen) (board-screen client b :cols 120 :rows 30)
      (declare (ignore all))
      (let ((slid (gethash "todo" (mux::board-slid b)))
            (titles (shown screen 3)))
        (is (plusp slid) "the lane did not slide for the cursor")
        (is (< 0 (mod slid 36) 36) "the lane snapped to a whole card rather than sliding by cells: ~D" slid)
        (is (search "5 last" titles) "the cursor's card is not in sight: ~S" titles)))))

(test a-scroll-by-hand-is-kept-until-the-cursor-moves
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "todo")))
    (setf (mux::client-over client) (list b))
    (board-screen client b :cols 120 :rows 30)
    (let ((mux::*client* client))
      ;; the lane slides on the wheel, and the next drawing does not slide it back
      (mux::switchboard-wheel-right)
      (is (eql 4 (gethash "todo" (mux::board-slid b))))
      ;; a sideways wheel over another lane slides that one
      (let ((mux::*mouse-at* (cons 60 14)))
        (mux::switchboard-wheel-right))
      (is (eql 4 (gethash "lib" (mux::board-slid b))) "the wheel over lib slid ~S" (mux::board-slid b))
      (is (eql 4 (gethash "todo" (mux::board-slid b))))
      (remhash "lib" (mux::board-slid b))
      (board-screen client b :cols 120 :rows 30)
      (is (eql 4 (gethash "todo" (mux::board-slid b))) "the drawing slid the lane back to the cursor")
      ;; the rail's right arrow slides it further, its left arrow back
      (multiple-value-bind (all screen) (board-screen client b :cols 120 :rows 30)
        (declare (ignore all))
        (let* ((line 11)
               (at (position #\‹ (shown screen line)))
               (to (position #\› (shown screen line))))
          (is-true at "no left end on the lane's foot: ~S" (shown screen line))
          (let ((mux::*mouse-at* (cons at line))) (mux::switchboard-click))
          (is (eql 0 (gethash "todo" (mux::board-slid b))) "the left arrow did not slide the lane back")
          (board-screen client b :cols 120 :rows 30)
          (let ((mux::*mouse-at* (cons to line))) (mux::switchboard-click))
          (is (eql 4 (gethash "todo" (mux::board-slid b))) "the right arrow did not slide the lane")
          ;; a press on the thumb, anywhere along it, does not move it
          (multiple-value-bind (all screen) (board-screen client b :cols 120 :rows 30)
            (declare (ignore all))
            (let ((last-thumb (position #\━ (shown screen line) :from-end t :end 118)))
              (let ((mux::*mouse-at* (cons last-thumb line))) (mux::switchboard-click))
              (is (eql 4 (gethash "todo" (mux::board-slid b))) "pressing the thumb's right end moved it")
              (is-true (mux::board-held b) "the thumb was not taken hold of")))))
      ;; moving the cursor brings the view back to it
      (mux::switchboard-last-window)
      (board-screen client b :cols 120 :rows 30)
      (is (> (gethash "todo" (mux::board-slid b)) 4)))))

(test the-switchboard-makes-a-window-in-the-lane-and-answers-and-hides-its-side-pane
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "todo")))
    (setf (mux::client-over client) (list b)
          (mux::client-wire client) (a-buffer-wire)
          (mux::client-knows client) (append '(:new-window :answer) mux::+was-known+))
    (let ((mux::*client* client))
      (mux::switchboard-next-pane)
      (is (equal '("todo" 1 2) (mux::board-cursor b)))
      ;; a digit answers the cursor's pane; c asks for a window in its lane
      (mux::unbound b "1" client)
      (mux::switchboard-new-window)
      (let ((sent (sent-on (mux::client-wire client))))
        (is (search "(:ANSWER \"todo\" 2 1)" sent) "~S" sent)
        (is (search "(:NEW-WINDOW \"todo\")" sent) "~S" sent))
      (is (search "SESSIONS" (board-screen client b)))
      (mux::switchboard-side)
      (is (null (search "SESSIONS" (board-screen client b))) "the side pane did not hide")
      (mux::switchboard-side)
      (is (search "SESSIONS" (board-screen client b))))))

(test the-switchboard-folds-quiet-lanes-and-the-overview-is-a-row-of-chips
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "todo")))
    (setf (mux::client-over client) (list b))
    (let ((mux::*client* client))
      (mux::switchboard-fold-quiet)
      (let ((all (board-screen client b)))
        (is (search "lib " all))
        (is (null (search " 1 tests " all)) "the quiet lane's card is still drawn: ~A" all))
      (mux::switchboard-fold-quiet)
      (mux::switchboard-zoom-out)
      (let ((all (board-screen client b)))
        (is (search " 1 agents " all) "~A" all)
        (is (search " 5 last " all) "the overview does not show every window: ~A" all)
        (is (null (search " 1 Yes " all)) "the overview draws cards")))))

(test the-side-pane-shows-the-pulse-and-the-activity-log-and-the-log-is-clickable
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "todo"))
         (clock (get-universal-time)))
    ;; pane 2 of todo, in window 1, wrote a lot just now while asking
    (setf (gethash (cons "todo" 2) (mux::client-pulses client))
          (append (make-list 15 :initial-element (cons 0 nil)) (list (cons 400 :blocked))))
    (setf (mux::client-events client)
          (list (list 0 clock :answered "todo" 2 1 (list :client 7 "/dev/ttys014") "1 Yes")
                (list 5000 clock :asks "lib" 7 1 nil "make")))
    (setf (mux::client-over client) (list b))
    (multiple-value-bind (all screen) (board-screen client b)
      (let ((todo (find-if (lambda (l) (search "▶ todo ◆" l)) (loop :for y :below 30 :collect (shown screen y))))
            (agents (find-if (lambda (l) (search "├ 1 agents" l)) (loop :for y :below 30 :collect (shown screen y))))
            (server (find-if (lambda (l) (search "◉ default" l)) (loop :for y :below 30 :collect (shown screen y)))))
        (is (and todo (find #\█ todo)) "the session's sparkline does not show the burst: ~S" todo)
        (is (and agents (find #\█ agents)) "the window's sparkline does not: ~S" agents)
        (is (and server (find #\█ server)) "the server's sparkline does not: ~S" server)
        (let ((notes (find-if (lambda (l) (search "├ 2 notes" l)) (loop :for y :below 30 :collect (shown screen y)))))
          (is (and notes (null (find #\█ notes))) "a quiet window's sparkline is not flat: ~S" notes)))
      (is (search "2 of 2" all) "the log does not count: ~A" all)
      (let ((answered (loop :for y :below 30 :when (search "answered 1 Yes" (shown screen y)) :return y))
            (asks (loop :for y :below 30 :when (search "asks: make" (shown screen y)) :return y)))
        (is-true answered "the answer is not in the log: ~A" all)
        (is-true asks "the question is not in the log: ~A" all)
        (is (and answered asks (< answered asks)) "the log is not newest first")
        (is (search "◆" (shown screen answered)) "the answer does not say who: ~S" (shown screen answered))
        ;; a click on the question's line puts the cursor on that window
        (let ((mux::*client* client)
              (mux::*mouse-at* (cons 10 asks)))
          (mux::switchboard-click))
        (is (equal '("lib" 1 7) (mux::board-cursor b)) "the click did not move the cursor: ~S" (mux::board-cursor b))))))

(test n-goes-to-the-oldest-question-and-tab-folds-the-lane-and-comma-names-the-window
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "lib")))
    (setf (mux::client-over client) (list b))
    (board-screen client b)
    (is (equal "lib" (first (mux::board-cursor b))))
    (let ((mux::*client* client))
      (mux::switchboard-next-asking)
      (is (equal '("todo" 1 2) (mux::board-cursor b)) "n did not go to the asking pane: ~S" (mux::board-cursor b))
      ;; TAB folds the cursor's lane to its header and foot
      (mux::switchboard-fold))
    (let ((all (board-screen client b)))
      (is (search "folded · 5 windows" all) "the lane is not folded: ~A" all)
      (is (null (search " 1 agents · 3 panes" all)) "the folded lane still shows its cards: ~A" all)
      (is (search "⇥ unfold" all) "~A" all))
    (let ((mux::*client* client))
      (mux::switchboard-fold)
      (is (search " 1 agents · 3 panes" (board-screen client b)) "unfolding did not bring the cards back")
      ;; , names the window on the line at the foot
      (mux::switchboard-name))
    (is (typep (first (mux::client-over client)) 'mux::entry))
    (is (equal "window todo › 1" (mux::entry-of-what (first (mux::client-over client)))))
    (is (equal "agents" (mux::entry-of-text (first (mux::client-over client)))))
    (let ((mux::*client* client)) (mux::entry-undo))
    ;; e opens the drawer on the cursor's pane
    (setf (mux::client-wire client) (a-buffer-wire))
    (let ((mux::*client* client)) (mux::switchboard-explain))
    (is (typep (first (mux::client-over client)) 'mux::drawer))
    (is (equal '("todo" . 2) (mux::drawer-target (first (mux::client-over client)))))))

(test a-narrow-terminal-has-no-side-pane-and-the-lane-header-carries-the-sparkline
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "todo")))
    (setf (mux::client-over client) (list b))
    (setf (gethash (cons "todo" 2) (mux::client-pulses client))
          (append (make-list 15 :initial-element (cons 0 nil)) (list (cons 400 :blocked))))
    (multiple-value-bind (all screen) (board-screen client b :cols 100 :rows 30)
      (is (null (search "SESSIONS" all)) "the side pane is up at 100 columns: ~A" all)
      (let ((header (find-if (lambda (l) (search "▶ todo" l)) (loop :for y :below 30 :collect (shown screen y)))))
        (is (and header (find #\█ header)) "the lane header has no sparkline without the side pane: ~S" header)
        (is (search "+ window" header) "~S" header)))))

(test a-click-on-the-switchboard-puts-the-cursor-there-and-a-second-click-goes-there
  (let* ((client (five-windows (board-client)))
         (b (mux::%make-board :starting "todo")))
    (setf (mux::client-over client) (list b))
    (multiple-value-bind (all screen) (board-screen client b)
      (declare (ignore all))
      (let* ((line (loop :for y :below 30 :when (search " 2 notes " (shown screen y)) :return y))
             (col (and line (+ 2 (search " 2 notes " (shown screen line))))))
        (is-true line "no card for window 2")
        (when line
          (let ((mux::*client* client)
                (mux::*mouse-at* (cons col line)))
            (mux::switchboard-click)
            (is (equal '("todo" 2 1) (mux::board-cursor b)) "the click did not move the cursor: ~S" (mux::board-cursor b))))))
    ;; the tree shares the cursor: its row for window 2 is lit, and a click on it goes
    (multiple-value-bind (all screen) (board-screen client b)
      (declare (ignore all))
      (let* ((line (loop :for y :below 30 :when (search "├ 2 notes" (shown screen y)) :return y)))
        (is-true line "the tree does not list window 2")
        (when line
          (setf (mux::client-knows client) (append '(:focus-pane) mux::+was-known+)
                (mux::client-wire client) (a-buffer-wire))
          (let ((mux::*client* client)
                (mux::*mouse-at* (cons 6 line)))
            (mux::switchboard-click))
          (is (null (mux::client-over client)) "a click on the cursor's own row did not go there")
          (is (search "(:FOCUS-PANE \"todo\" 1)" (sent-on (mux::client-wire client)))))))))

(test a-prompt-for-a-busy-pane-waits-until-it-is-idle
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 30))
           (pane (mux:session-focus session))
           (agent (mux:pane-agent pane))
           (wire (a-wire-to path)))
      (settled server pane)
      (agent:agent-hear agent :working)
      (step-until server (lambda () (eq :working (agent:agent-state agent))))
      (say-to wire (list :prompt-when-idle "work" (mux:pane-id pane) "later-please"))
      (is (eq :queued (fourth (heard-from server wire :agent-prompted))))
      (is (equal "later-please" (first (mux::pane-queued pane))))
      (is (null (search "later-please" (term:term-dump-to-string (mux:pane-term pane))))
          "it was typed while the pane was busy")
      (agent:agent-hear agent :idle)
      (is-true (step-until server (lambda ()
                                    (search "later-please"
                                            (term:term-dump-to-string (mux:pane-term pane)))))
               "it was not sent when the pane went idle")
      (is (null (mux::pane-queued pane)))
      (mux:wire-close wire))))

(test the-switchboard-prompts-panes-in-two-sessions-at-once
  (with-server (path :command "cat" :rows 20 :cols 90)
    (with-seer (seer path :rows 20 :cols 90)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~CC" mux:+prefix+))
      (pump seer :seconds 1)
      (type-at seer (format nil "~Cw" mux:+prefix+))
      (is-true (pump seer :want "switchboard") "the switchboard did not open: ~S" (seen seer))
      (is-true (pump seer :want "1 cat") "the board has no panes yet: ~S" (seen seer))
      (type-at seer " ")
      (pump seer :seconds 1/4)
      ;; the board opened on this session, the second; the first is up
      (type-at seer (format nil "~C[A" #\Escape))
      (pump seer :seconds 1/4)
      (type-at seer " ")
      (pump seer :seconds 1/4)
      (type-at seer "p")
      (is-true (pump seer :want "prompt 2") "the composer is not for the two picked: ~S" (seen seer))
      (type-at seer "hello-both")
      (pump seer :seconds 1/4)
      (type-at seer (string #\Return))
      (pump seer :seconds 1)
      (let* ((rows (second (find :panes (mux::asked path '((:panes))
                                                    :done (lambda (f) (eq :panes (first f))))
                                 :key #'first)))
             (read (lambda (row)
                     (fourth (find :agent-lines
                                   (mux::asked path (list (list :agent-read (getf row :session)
                                                                (getf row :id) 5))
                                               :done (lambda (f) (eq :agent-lines (first f))))
                                   :key #'first)))))
        (is (eql 2 (length rows)))
        (dolist (row rows)
          (is (some (lambda (l) (search "hello-both" l)) (funcall read row))
              "~A:~D was not prompted" (getf row :session) (getf row :id)))))))

;;; Why it thinks so, and who typed.

(test the-drawer-says-what-decided-the-state-and-who-typed-there
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :says "impl" :kind "claude-code" :state :blocked
                        :for 42000 :focus t)))
         (key (cons "todo" 2))
         (now (mux::ms-here))
         (d (mux::%make-drawer :key key :asked now))
         (screen (tty:make-screen :width 150 :height 36)))
    (setf (mux::client-session client) "todo"
          (gethash key (mux::client-about client))
          (list :agent-explained
                (list now :blocked :blocked
                      '((:transcript :footer :skip nil nil)
                        (:permission :choice :blocked t
                         (:screen :permission :means :blocked :widget :choice
                          :question "Do you want to proceed?" :options ("Yes" "No") :selected 0))
                        (:question :choice :blocked nil
                         (:screen :question :means :blocked :widget :choice
                          :question "Which?" :options ("a" "b") :selected 0))))
                :pane-about (list now '(:kind "claude-code" :programs ("claude --resume")
                                        :group 48213 :command "sh -c \"exec claude\""))
                :pane-history (list now '((42000 :blocked) (60000 :working)))
                :pane-log (list now '((50000 (:pane "todo:4") :prompt "STATUS?" :refused)
                                      (80000 (:pane "todo:4") :prompt "run the suite" t)
                                      (90000 (:client 7 "/dev/ttys004") :keys 14 t)))))
    (let ((mux::*drawing-for* client))
      (mux:draw-over d screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 36 :collect (shown screen y)))))
      (is (search "why is impl asking?" all) "~A" all)
      (is (search "in front: claude --resume  group 48213" all))
      (is (search "* blocked permission" all)
          "the winning rule is not marked: ~A" all)
      (is (search "│ Do you want to proceed?" all) "the winning rule's text is not under it")
      (is (search "+ " all) "another rule that matched is not marked")
      (is (search "WHO TYPED HERE" all))
      (is (search "todo:4" all))
      (is (search "refused" all))
      (is (search "keys 14 bytes" all))
      (is (search "20m" all)))))

(test the-drawer-follows-the-focus-and-typing-still-reaches-the-pane
  (with-server (path :command "cat" :rows 24 :cols 150)
    (with-seer (seer path :rows 24 :cols 150)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~Ce" mux:+prefix+))
      (is-true (pump seer :want "what is cat?") "the drawer did not open: ~S" (seen seer))
      (is-true (pump seer :want "not read: no reader knows this program"))
      (type-at seer "typed-past-it")
      (is-true (pump seer :want "typed-past-it")
               "what was typed with the drawer open did not reach the pane: ~S" (seen seer))
      (is-true (pump seer :want "bytes") "the drawer does not say who typed: ~S" (seen seer))
      (type-at seer (format nil "~Ce" mux:+prefix+))
      (is-true (pump seer :until (lambda () (null (search "what is" (seen seer)))))
               "the key that opened it did not close it"))))

;;; The command line does what the UI does.

(test waiting-after-a-prompt-is-done-only-once-the-pane-took-it-up
  ;; history is newest first, (age state); prompted is how long ago the prompt was
  (is (null (mux::done-since-prompt-p '(:idle 5000 ((60000 :idle))) '(:idle)))
      "an idle from before the prompt was taken for it being done")
  (is (null (mux::done-since-prompt-p '(:idle 5000 ((4000 :idle) (60000 :working))) '(:idle)))
      "the idle the paste itself made was taken for it being done")
  (is-true (mux::done-since-prompt-p '(:idle 5000 ((1000 :idle) (4000 :working) (60000 :idle)))
                                     '(:idle)))
  (is-true (mux::done-since-prompt-p '(:blocked 5000 ((1000 :blocked) (60000 :idle)))
                                     '(:blocked :idle))
           "asking something after the prompt is a change it made")
  (is (null (mux::done-since-prompt-p '(:idle nil ((1000 :idle))) '(:idle))))
  ;; what the field test found: the server marks a prompted pane working at the
  ;; very moment of the prompt, so that entry and the prompt are equally old
  (is-true (mux::done-since-prompt-p
            '(:idle 316791 ((310951 :idle) (316791 :working) (333441 :idle)))
            '(:blocked :idle))
           "working from the moment of the prompt was not taken for taking it up"))

(test the-command-line-reads-its-flags-and-words-apart
  (let ((args '("work" "--name" "impl" "--cwd" "/tmp" "--json")))
    (is (equal "impl" (mux::option args "--name")))
    (is-true (mux::flag-p args "--json"))
    (is (equal '("work") (mux::words args "--name" "--cwd")))))

(test what-goes-out-as-json-is-json
  (is (equal "[{\"address\":\"todo:2\",\"for_ms\":42000,\"state\":\"blocked\",\"asks\":null,\"ok\":true}]"
             (with-output-to-string (s)
               (mux::json (list (list :address "todo:2" :for-ms 42000 :state :blocked :asks nil :ok t))
                          s))))
  (is (equal "\"a \\\"q\\\"\\nb\"" (with-output-to-string (s) (mux::json (format nil "a \"q\"~%b") s)))))

(test reading-plainly-leaves-out-what-is-drawn-faint
  (let ((term (a-term :width 60 :height 3)))
    (say term (format nil "❯ ~C[2madd clarification 8~C[0m~C~Ctyped" #\Escape #\Escape #\Return #\Newline))
    (let ((lines (mux::plain-lines term 3)))
      (is (equal "❯" (first lines)) "the faint suggestion was read as typed: ~S" lines)
      (is (equal "typed" (second lines))))))

(test a-pane-is-spawned-into-a-session-or-makes-the-session
  (with-a-server-here (server path)
    (let ((wire (a-wire-to path)))
      (say-to wire (list :spawn "work" "cat" nil "impl"))
      (let ((id (third (heard-from server wire :spawned))))
        (is (integerp id))
        (let ((session (mux:session-named server "work")))
          (is-true session "spawning into no session did not make one")
          (is (equal "impl" (mux::pane-label (mux:session-focus session))))))
      (say-to wire (list :spawn "work" "cat" nil "test"))
      (let* ((id (third (heard-from server wire :spawned)))
             (session (mux:session-named server "work")))
        (is (eql 2 (length (mux:session-panes session))))
        (is (equal "test" (mux::pane-label (find id (mux:session-panes session)
                                                 :key #'mux:pane-id))))
        (is (equal "impl" (mux::pane-label (mux:session-focus session)))
            "spawning moved the focus off the pane somebody was in"))
      (mux:wire-close wire))))

(test since-a-prompt-says-when-it-was-and-what-came-after
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 30))
           (pane (mux:session-focus session))
           (wire (a-wire-to path)))
      (settled server pane)
      (say-to wire (list :since-prompt "work" (mux:pane-id pane)))
      (destructuring-bind (state prompted history) (fourth (heard-from server wire :since-prompt))
        (is (eq :idle state))
        (is (null prompted) "a pane never prompted said it was")
        (is (consp history)))
      (say-to wire (list :agent-prompt "work" (mux:pane-id pane) "go"))
      (heard-from server wire :agent-prompted)
      (say-to wire (list :since-prompt "work" (mux:pane-id pane)))
      (is (integerp (second (fourth (heard-from server wire :since-prompt)))))
      (mux:wire-close wire))))

(test a-prompt-of-one-line-is-typed-not-pasted
  ;; the field test: Claude Code declined to act on a request that arrived as
  ;; a bracketed paste, taking it for text pasted in rather than asked for
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server
                                     (format nil "printf '\\033[?2004h'; stty -echo; cat -v")
                                     :name "work" :rows 6 :cols 60))
           (pane (mux:session-focus session))
           (wire (a-wire-to path)))
      (step-until server (lambda () (term:term-bracketed-paste (mux:pane-term pane))))
      (say-to wire (list :agent-prompt "work" (mux:pane-id pane) "hello there"))
      (heard-from server wire :agent-prompted)
      (is-true (step-until server (lambda () (search "hello there"
                                                     (term:term-dump-to-string (mux:pane-term pane))))))
      (is (null (search "200~" (term:term-dump-to-string (mux:pane-term pane))))
          "one line was pasted: ~S" (term:term-dump-to-string (mux:pane-term pane)))
      (mux:wire-close wire))))

;;; The mouse in a pane, and reading one back.

(defun dumped (pane)
  (term:term-dump-to-string (mux:pane-term pane)))

(defmacro with-a-session ((session pane server command &key (rows 12) (cols 30)) &body body)
  "One session of one pane running COMMAND on a server stepped by hand, laid
out: the bar on the first row, the pane under it, its scrollbar down the last
column."
  (let ((path (gensym "PATH")))
    `(with-a-server-here (,server ,path)
       (let* ((,session (mux:add-session ,server ,command :name "work" :rows ,rows :cols ,cols))
              (,pane (mux:session-focus ,session)))
         ,@body))))

(test a-key-names-a-button-going-down-moving-and-coming-up
  (flet ((named (&rest event) (atty/mode:spelled (mux::mouse-key-of (cons :mouse event)))))
    (is (equal "mouse-1" (named :button :left)))
    (is (equal "mouse-1-up" (named :button :left :release t)))
    (is (equal "mouse-1-drag" (named :button :left :drag t)))
    (is (equal "mouse-3-drag" (named :button :right :drag t)))
    (is (equal "wheel-up" (named :wheel :up)))
    (is (equal "S-wheel-down" (named :wheel :down :shift t)))
    (is (null (mux::mouse-key-of '(:mouse :button nil :drag t)))
        "a pointer moving with nothing held has no name")))

(test the-wheel-and-the-scroll-keys-are-bound-with-nothing-set-up
  (flet ((bound (chord mode) (atty/mode:lookup-key chord (atty/mode:mode-named mode))))
    (is (eq #'mux::natural-scroll-up (bound "wheel-up" 'mux:pane-mode)))
    (is (eq #'mux::natural-scroll-down (bound "wheel-down" 'mux:pane-mode)))
    (is (eq #'mux::scroll-up-a-little (bound "S-wheel-up" 'mux:pane-mode)))
    (is (eq #'mux::mouse-dragged (bound "mouse-1-drag" 'mux:pane-mode)))
    (is (eq #'mux::scroll-mode (bound "C-b [" 'mux:pane-mode)))
    (is (eq #'mux::scroll-page-up (bound "PageUp" 'mux::scroll-mode)))
    (is (eq #'mux::natural-scroll-up (bound "wheel-up" 'mux::scroll-mode))
        "the mode for reading back lost what the pane's mode does with the wheel")))

(test the-wheel-over-a-shell-reads-its-pane-back-and-typing-is-back-to-live
  (with-a-session (session pane server "seq 1 60; sleep 30")
    (is-true (step-until server (lambda () (search "60" (dumped pane)))))
    (mux::session-compose session)
    (mux::wheel-at session :up 4 4 nil)
    (is (eql 3 (mux::pane-scrolled pane)))
    (mux::wheel-at session :down 4 4 nil)
    (mux::wheel-at session :down 4 4 nil)
    (is (eql 0 (mux::pane-scrolled pane)) "it went past live")
    (mux::scroll-the-pane pane :page-up)
    (is (eql 10 (mux::pane-scrolled pane)) "a page is the pane less a line to keep your place by")
    (mux::scroll-the-pane pane :top)
    (is (eql (mux::pane-history pane) (mux::pane-scrolled pane)))
    (mux::heard-about-a-session session (mux::%make-watcher) (list :keys "x"))
    (is (eql 0 (mux::pane-scrolled pane)) "typing did not bring it back to live")))

(test the-wheel-over-a-program-that-asked-for-the-mouse-is-that-programs
  (with-a-session (session pane server
                           "printf '\\033[?1000h\\033[?1006h'; seq 1 60; cat -v")
    (is-true (step-until server (lambda () (and (search "60" (dumped pane))
                                                (term:term-mouse-mode (mux:pane-term pane))))))
    (mux::session-compose session)
    (mux::wheel-at session :up 4 3 nil)
    (is (eql 0 (mux::pane-scrolled pane)) "the pane was read back under a program that wanted the wheel")
    (is-true (step-until server (lambda () (search "[<64;5;3M" (dumped pane))))
             "it was not told, or not told where in its own pane: ~S" (dumped pane))
    (mux::wheel-at session :up 4 3 '(:shift))
    (is (eql 3 (mux::pane-scrolled pane)) "with shift held it is the pane that is read back")
    (mux::wheel-at session :up 29 3 nil)
    (is (eql 6 (mux::pane-scrolled pane)) "the scrollbar is nobody's but the multiplexer's")))

(test the-wheel-over-a-program-with-the-whole-screen-is-the-arrow-keys
  (with-a-session (session pane server "printf '\\033[?1049h'; cat -v")
    (is-true (step-until server (lambda () (term:term-in-alt-screen (mux:pane-term pane)))))
    (mux::session-compose session)
    (mux::wheel-at session :down 4 3 nil)
    (is-true (step-until server (lambda () (search "^[[B^[[B^[[B" (dumped pane))))
             "~S" (dumped pane))))

(test a-click-is-passed-on-to-a-program-that-asked-and-so-is-letting-go
  (with-a-session (session pane server "printf '\\033[?1002h\\033[?1006h'; cat -v" :cols 60)
    (is-true (step-until server (lambda () (term:term-mouse-mode (mux:pane-term pane)))))
    (mux::session-compose session)
    (let ((watcher (mux::%make-watcher)))
      (mux::pointer-at session watcher :press :left 4 3 nil)
      (mux::pointer-at session watcher :drag :left 6 4 nil)
      (mux::pointer-at session watcher :release :left 6 4 nil))
    (is-true (step-until server (lambda () (search "[<0;7;4m" (dumped pane)))) "~S" (dumped pane))
    (is (search "[<0;5;3M" (dumped pane)))
    (is (search "[<32;7;4M" (dumped pane)))))

(test an-arrow-of-the-scrollbar-held-goes-on-until-it-is-let-go
  (with-a-session (session pane server "seq 1 200; sleep 30")
    (is-true (step-until server (lambda () (search "200" (dumped pane)))))
    (mux::session-compose session)
    (let ((watcher (mux::%make-watcher)))
      (mux::pointer-at session watcher :press :left 29 1 nil)
      (is (eql 1 (mux::pane-scrolled pane)) "a click on the arrow is a line")
      (is-true (step-until server (lambda () (> (mux::pane-scrolled pane) 3)) 3)
               "held, it did not go on")
      (mux::pointer-at session watcher :release :left 29 1 nil)
      (let ((stopped (mux::pane-scrolled pane)))
        (step-until server (lambda () nil) 1/4)
        (is (eql stopped (mux::pane-scrolled pane)) "let go, it did not stop")))))

(test the-track-pages-and-the-thumb-drags
  (with-a-session (session pane server "seq 1 200; sleep 30")
    (is-true (step-until server (lambda () (search "200" (dumped pane)))))
    (mux::session-compose session)
    (let ((watcher (mux::%make-watcher))
          (bar (mux::bar-of session pane)))
      (is (eq :above (mux::scrollbar-part bar 3)))
      (mux::pointer-at session watcher :press :left 29 3 nil)
      (mux::pointer-at session watcher :release :left 29 3 nil)
      (is (eql 10 (mux::pane-scrolled pane)) "a click on the track above the thumb is a page back")
      (mux::pane-scroll-to pane 0)
      (let ((thumb (loop :for y :from 1 :below 12
                         :when (eq :thumb (mux::scrollbar-part bar y)) :do (return y))))
        (mux::pointer-at session watcher :press :left 29 thumb nil)
        (mux::pointer-at session watcher :drag :left 29 2 nil)
        (is (eql (mux::pane-history pane) (mux::pane-scrolled pane))
            "dragged to the head of the track it is as far back as there is")
        (mux::pointer-at session watcher :drag :left 29 thumb nil)
        (is (eql 0 (mux::pane-scrolled pane)) "and dragged back to where it was, live")
        (mux::pointer-at session watcher :release :left 29 thumb nil)))))

(test the-chip-is-back-to-live
  (with-a-session (session pane server "seq 1 200; sleep 30")
    (is-true (step-until server (lambda () (search "200" (dumped pane)))))
    (mux::pane-scroll-to pane 40)
    (let* ((screen (mux::session-compose session))
           (at (search "↓ 40 to live" (shown screen 11))))
      (is-true at "~S" (shown screen 11))
      (mux::pointer-at session (mux::%make-watcher) :press :left at 11 nil)
      (is (eql 0 (mux::pane-scrolled pane))))))

(test a-session-can-give-the-column-back
  (with-a-session (session pane server "sleep 30")
    (mux::session-compose session)
    (is (eql 29 (term:term-width (mux:pane-term pane))))
    (mux::heard-about-a-session session (mux::%make-watcher) (list :scrollbars :toggle))
    (mux::session-compose session)
    (is (eql 30 (term:term-width (mux:pane-term pane))))))

(test a-wheel-at-the-terminal-reads-the-pane-back-on-the-screen-and-a-key-returns
  ;; below the bar, since the bar says what the command was and that has a 60 in it
  (with-server (path :command "seq 1 60; sleep 30" :rows 10 :cols 40)
    (with-seer (seer path :rows 10 :cols 40)
      (is-true (pump seer :until (lambda () (search "60" (seen-below-bar seer)))))
      (type-at seer (format nil "~C[<64;5;5M" #\Escape))
      (is-true (pump seer :want "3 to live") "~S" (seen seer))
      (is (search "57" (seen-below-bar seer)))
      (is (null (search "60" (seen-below-bar seer)))
          "the foot of it is still showing: ~S" (seen seer))
      (is (null (search "[<" (seen seer))) "the wheel left raw bytes in the pane")
      (type-at seer "x")
      (is-true (pump seer :until (lambda () (search "60" (seen-below-bar seer))))
               "a key did not bring it back to live: ~S" (seen seer))
      (is (null (search "to live" (seen seer)))))))

;;; Windows: a session shows one of several layouts at a time.

(defun heard-from-wire (wire tag &optional (seconds 5))
  "Wait until WIRE, to a server running on its own thread, has been told
something tagged TAG, and answer it."
  (let ((heard nil))
    (until seconds (lambda ()
                     (setf heard (append heard (heard-back wire)))
                     (find tag heard :key #'first)))
    (find tag heard :key #'first)))

(test a-session-starts-with-one-window-and-a-new-one-is-shown-after-it
  (with-a-session (session pane server "cat")
    (is (eql 1 (length (mux:session-windows session))))
    (is (eql 1 (mux:window-number session (mux:session-window session))))
    (is (equal "work:1.1" (mux:pane-address-of session pane)))
    (let ((second (mux:add-window session "cat")))
      (is (eql 2 (length (mux:session-windows session))))
      (is (eq second (mux:session-window session)) "the new window is not the one shown")
      (is (eql 2 (mux:window-number session second)))
      (is (not (eq pane (mux:session-focus session))) "the focus stayed in the first window")
      (is (equal "work:2.1" (mux:pane-address-of session (mux:session-focus session))))
      ;; every pane in every window, the ones on screen first
      (is (eql 2 (length (mux:session-panes session))))
      (is (eql 1 (length (mux:window-panes second))))
      (mux:step-window session 1)
      (is (eq pane (mux:session-focus session)) "stepping round did not come back to the first")
      (mux:step-window session -1)
      (is (eq second (mux:session-window session)))
      (mux:go-to-window session 1)
      (is (eq pane (mux:session-focus session))))))

(test a-split-is-in-the-window-shown-and-numbers-count-within-it
  (with-a-session (session pane server "cat")
    (let* ((second (mux:add-window session "cat"))
           (beside (mux::split-the-session session :across)))
      (is (eql 2 (length (mux:window-panes second))))
      (is (eql 1 (length (mux:window-panes (first (mux:session-windows session))))))
      (is (eql 2 (mux:pane-number session beside)))
      (is (equal "work:2.2" (mux:pane-address-of session beside)))
      (is (eql 1 (mux:pane-number session pane)))
      ;; focusing a pane in another window shows that window
      (mux::focus-on session pane)
      (is (eq (first (mux:session-windows session)) (mux:session-window session))
          "focusing a pane in another window did not show it")
      (is (eq pane (mux:session-focus session))))))

(test closing-a-windows-last-pane-closes-the-window-and-the-last-window-ends-the-session
  (with-a-session (session pane server "cat")
    (let ((second (mux:add-window session "cat")))
      (mux::close-the-pane session (mux:session-focus session))
      (is (eql 1 (length (mux:session-windows session))) "an empty window was kept")
      (is (not (member second (mux:session-windows session))))
      (is (eq pane (mux:session-focus session)) "the window before was not shown")
      (mux::close-the-pane session pane)
      (is (eql 1 (length (mux:session-windows session))) "the last window went")
      (is (null (mux:session-panes session)))
      (is (null (mux:session-layout session)) "an empty last window is a session that is over"))))

(test windows-are-made-shown-closed-and-named-over-the-wire
  (with-server (path :command "cat" :rows 10 :cols 40)
    (with-seer (seer path :rows 10 :cols 40)
      (type-at seer "in-the-first")
      (is-true (pump seer :want "in-the-first"))
      (type-at seer (format nil "~Cc" mux:+prefix+))
      (is-true (pump seer :until (lambda () (null (search "in-the-first" (seen seer)))))
               "a new window still shows the first: ~S" (seen seer))
      (type-at seer "in-the-second")
      (is-true (pump seer :want "in-the-second"))
      (type-at seer (format nil "~Cp" mux:+prefix+))
      (is-true (pump seer :until (lambda () (and (search "in-the-first" (seen seer))
                                                 (null (search "in-the-second" (seen seer))))))
               "previous window did not go back: ~S" (seen seer))
      (type-at seer (format nil "~Cn" mux:+prefix+))
      (is-true (pump seer :want "in-the-second") "next window did not go on: ~S" (seen seer))
      (let ((wire (a-wire-to path)))
        (say-to wire (list :sessions))
        (let ((these (heard-from-wire wire :these)))
          (is (equal '((1 nil 1 0 nil) (2 nil 1 0 t)) (seventh (first (second these))))
              "the session did not say its windows: ~S" these))
        (say-to wire (list :name-window "0" 2 "tests") (list :layouts "0"))
        (let ((layouts (heard-from-wire wire :layouts)))
          (is (equal "tests" (second (second (third layouts)))) "~S" layouts)
          (is (integerp (third (first (third layouts)))) "a lone pane is its id: ~S" layouts))
        (say-to wire (list :close-window "0" 2))
        (is-true (pump seer :want "in-the-first") "closing the window did not show the other")
        (mux:wire-close wire)))))

;;; Addresses, and the clients attached.

(test an-address-is-read-either-way-and-said-by-window-and-pane
  (multiple-value-bind (session id window n) (mux::pane-address "todo:1.2")
    (is (equal "todo" session)) (is (null id)) (is (eql 1 window)) (is (eql 2 n)))
  (multiple-value-bind (session id window n) (mux::pane-address "todo:4")
    (is (equal "todo" session)) (is (eql 4 id)) (is (null window)) (is (null n)))
  (signals error (mux::pane-address "todo"))
  (signals error (mux::pane-address "todo:1."))
  (is (equal "todo:1.2" (mux::row-address '(:session "todo" :id 9 :window 1 :at 1))))
  (is (equal "todo:9" (mux::row-address '(:session "todo" :id 9)))
      "a row from a server without windows is still said by its id")
  (is (equal "todo › 1 agents › impl"
             (mux::row-path '(:session "todo" :id 9 :window 1 :window-name "agents" :label "impl")))))

(test whoever-is-attached-is-listed-and-pushed-to-whoever-watches-the-panes
  (with-a-server-here (server path)
    (let ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
          (looking (a-wire-to path))
          (asking (a-wire-to path)))
      (declare (ignorable session))
      (say-to asking '(:watch-panes t))
      (is-true (step-until server (lambda () (some #'mux::watcher-watch-panes (mux::every-watcher server)))))
      (say-to looking '(:who "/dev/ttys042") '(:attach 6 20 t))
      (let ((told (heard-from server asking :clients)))
        ;; the push that came with the attaching: the one naming the tty
        (until 5 (lambda ()
                   (or (find "/dev/ttys042" (second told) :key #'second :test #'equal)
                       (progn (say-to asking '(:clients))
                              (setf told (heard-from server asking :clients))
                              nil))))
        (is-true told "a client joining was not pushed to the watcher")
        (let ((row (find "/dev/ttys042" (second told) :key #'second :test #'equal)))
          (is-true row "the joined client is not among them: ~S" told)
          (when row
            (is (equal "work" (fifth row)))
            (is (eql 1 (sixth row)) "it is not said to be looking at window 1: ~S" row)
            (is (integerp (seventh row)))
            (is (null (eighth row)) "it has not typed, so has no idle time: ~S" row))))
      (say-to looking '(:keys "hi"))
      (is-true (step-until server (lambda () (some (lambda (w) (plusp (mux::watcher-typed-at w)))
                                                   (mux::every-watcher server)))))
      (say-to asking '(:clients))
      (let* ((said (heard-from server asking :clients))
             (row (find "/dev/ttys042" (second said) :key #'second :test #'equal)))
        (is (integerp (eighth row)) "typing was not noted: ~S" row)
        (say-to asking (list :detach-client (first row))))
      ;; the far end going is a read of nothing
      (is-true (step-until server (lambda () (null (mux:wire-fill looking))))
               "detaching a client did not close its wire")
      (mux:wire-close asking))))

;;; The bar: windows, not panes.

(test the-bar-lists-the-windows-and-a-click-on-one-shows-it
  (with-server (path :command "cat" :rows 10 :cols 100)
    (with-seer (seer path :rows 10 :cols 100)
      (type-at seer "in-the-first")
      (is-true (pump seer :want "in-the-first"))
      (type-at seer (format nil "~Cc" mux:+prefix+))
      (is-true (pump seer :until (lambda () (null (search "in-the-first" (seen seer))))))
      (type-at seer "in-the-second")
      (is-true (pump seer :want "in-the-second"))
      (is (search " 2 cat" (seen seer)) "the bar does not list window 2: ~S" (seen seer))
      (is (null (search "100x" (seen seer))) "the bar still says the size")
      ;; the chip for window 1 is a button: a click on it shows that window
      (multiple-value-bind (x y) (where-on seer " 1 cat")
        (is-true x "no chip for window 1: ~S" (seen seer))
        (when x (click-at seer (+ x 2) y)))
      (is-true (pump seer :until (lambda () (and (search "in-the-first" (seen seer))
                                                 (null (search "in-the-second" (seen seer))))))
               "clicking window 1's chip did not show it: ~S" (seen seer))
      ;; + is a button too: a third window
      (multiple-value-bind (x y) (where-on seer " + ")
        (is-true x "no + on the bar: ~S" (seen seer))
        (when x (click-at seer (1+ x) y)))
      (is-true (pump seer :want " 3 cat") "clicking + made no window: ~S" (seen seer)))))

(test the-bar-is-where-what-else-who-and-the-one-key-to-learn
  (with-a-session (session pane server "cat" :rows 8 :cols 120)
    (declare (ignore pane))
    (let* ((screen (tty:make-screen :width 120 :height 1))
           (tree (mux::default-bar session)))
      (laid tree screen)
      (let ((line (shown screen 0)))
        (is (eql 0 (search " λ " line)) "~S" line)
        (is (search (format nil " ~A │" (mux:session-name session)) line) "~S" line)
        (is (search " 1 cat" line) "the window is not on the bar: ~S" line)
        (is (search "run a command" line) "the field is not on the bar: ~S" line)
        (is (null (search "need" line)) "nothing needs you, yet the bar says so: ~S" line)
        (is (search " C-b menu" line) "the one key to learn is not on the bar: ~S" line)
        (is (search " │ " line :from-end t) "the rule before the time is gone: ~S" line)
        (let ((menu (mux::button-at tree 0 (search "C-b menu" line))))
          (is (equal "show the menu" (and menu (mux::bar-button-runs menu)))))))))

(test the-menu-rises-after-the-prefix-hangs-and-a-click-on-it-runs-the-key
  (with-server (path :command "cat" :rows 24 :cols 100)
    (with-seer (seer path :rows 24 :cols 100)
      (pump seer :seconds 1/2)
      (type-at seer (string mux:+prefix+))
      (is-true (pump seer :want "Esc cancels" :seconds 2) "no menu after the prefix hung: ~S" (seen seer))
      (is (search "then one key" (seen seer)) "~S" (seen seer))
      (is (search "WINDOWS" (seen seer)) "~S" (seen seer))
      (is (search "c     another window" (seen seer)) "the new window key is not listed: ~S" (seen seer))
      ;; the second key does what it always did, and the menu goes with it
      (type-at seer "c")
      (is-true (pump seer :want " 2 ") "no second window: ~S" (seen seer))
      (is-true (pump seer :until (lambda () (null (search "then one key" (seen seer)))))
               "the menu stayed up after the chord: ~S" (seen seer))
      ;; a click on an entry runs it too
      (type-at seer (string mux:+prefix+))
      (is-true (pump seer :want "Esc cancels" :seconds 2))
      (multiple-value-bind (x y) (where-on seer "another window")
        (click-at seer x y))
      (is-true (pump seer :want " 3 ") "the click on the entry made no window: ~S" (seen seer))
      (is (null (search "then one key" (seen seer))) "the menu stayed up after the click"))))

(test the-bar-says-which-pane-is-zoomed-and-who-is-reading-back
  (with-server (path :command "cat" :rows 10 :cols 100)
    (with-seer (seer path :rows 10 :cols 100)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~C3" mux:+prefix+))
      (is-true (pump seer :until (lambda () (search "┏" (seen seer)))))
      (type-at seer (format nil "~Cz" mux:+prefix+))
      (is-true (pump seer :want "⤢ cat zoomed") "the bar does not name the zoomed pane: ~S" (seen seer))
      (type-at seer (format nil "~Cz" mux:+prefix+))
      (is-true (pump seer :until (lambda () (null (search "zoomed" (seen seer))))))
      (type-at seer (format nil "~C[" mux:+prefix+))
      (is-true (pump seer :want "reading back") "the bar does not say somebody is reading back: ~S" (seen seer))
      (type-at seer "q")
      (is-true (pump seer :until (lambda () (null (search "reading back" (seen seer)))))
               "leaving did not take the chip off the bar: ~S" (seen seer)))))

(test a-window-with-a-pane-asking-is-a-yellow-chip-with-a-count
  (with-a-session (session pane server "cat" :rows 8 :cols 100)
      (let ((agent (mux:pane-agent pane)))
        (step-until server (lambda () (member (agent:agent-state agent) '(:idle :working))))
        (agent:agent-hear agent :blocked)
        (is-true (step-until server (lambda () (eq :blocked (agent:agent-state agent)))))
        (is (eq :blocked (mux::window-worst (mux:session-window session))))
        (let ((said (with-output-to-string (out)
                      (let ((row (mux::window-chip session (mux:session-window session))))
                        (labels ((walk (w)
                                   (when (typep w 'atty/ui:label) (write-string (atty/ui::text w) out))
                                   (dolist (part (atty/ui:parts w)) (walk part))))
                          (walk row))))))
          (is (search "▲1" said) "the chip does not count what is asking: ~S" said)))))

;;; Finding in a pane, and copying lines out of it.

(defun screen-text (screen)
  "A composed screen's characters, row by row."
  (format nil "~{~A~%~}"
          (loop :for y :below (tty:screen-height screen)
                :collect (let ((row (aref (tty:screen-grid screen) y)))
                           (coerce (loop :for x :below (tty:screen-width screen)
                                         :collect (term:row-char row x))
                                   'string)))))

(test finding-in-a-pane-scrolls-to-the-hit-and-counts-them-and-lines-can-be-copied
  (with-a-session (session pane server "cat" :rows 6 :cols 70)
    (let ((wire (a-wire-to (mux::server-path server))))
      (say-to wire (list :want "work") (list :attach 6 70 t))
      (heard-from server wire :hello)
      ;; forty numbered lines, so most are behind the screen
      (say-to wire (list :keys (format nil "~{line-~D~%~}" (loop :for i :from 1 :to 40 :collect i))))
      (is-true (step-until server (lambda () (> (mux::pane-history pane) 30))))
      (say-to wire (list :find nil nil "line-1" :here))
      (let ((found (heard-from server wire :found)))
        (is (eql 11 (fourth found)) "line-1, line-10..line-19: ~S" found)
        (is (eql 1 (fifth found)) "a new find starts from the newest hit: ~S" found)
        (is (plusp (mux::pane-scrolled pane)) "the pane did not scroll to the hit"))
      (say-to wire (list :find nil nil nil :next))
      (is (eql 2 (fifth (heard-from server wire :found))))
      (say-to wire (list :find nil nil nil :back))
      (is (eql 1 (fifth (heard-from server wire :found))))
      ;; the hit is lit where the frame was drawn
      (mux::session-compose session)
      (let* ((screen (mux:session-screen session))
             (lit (loop :for y :below (tty:screen-height screen)
                        :thereis (loop :for x :below (tty:screen-width screen)
                                       :for f := (term:row-face (aref (tty:screen-grid screen) y) x)
                                       :thereis (and f (term:face-bold f) (term:face-bg f))))))
        (is-true lit "no hit is painted on the screen"))
      (is (search "⌕ find" (screen-text (mux:session-screen session)))
          "the frame has no find button while read back")
      (is (search "/ line-1" (screen-text (mux:session-screen session)))
          "the frame does not say what was found")
      ;; nothing found is said too
      (say-to wire (list :find nil nil "no-such-thing" :here))
      (is (eql 0 (fourth (heard-from server wire :found))))
      ;; copying: mark the top row, scroll, copy from the mark to here
      (say-to wire (list :find nil nil "line-20" :here))
      (heard-from server wire :found)
      (say-to wire (list :select :start))
      (say-to wire (list :scroll 3))
      (say-to wire (list :select :copy))
      (let ((copied (second (heard-from server wire :copied))))
        (is (search "line-20" copied) "the marked line is not in what was copied: ~S" copied)
        (is (< 6 (1+ (count #\Newline copied))) "the copy is only one screen: ~S" copied))
      (say-to wire (list :find nil nil nil :clear))
      (is-true (step-until server (lambda () (null (mux::pane-find pane)))))
      (mux:wire-close wire))))

(test base64-is-what-osc-52-wants
  (is (equal "aGVsbG8=" (mux::base64 "hello")))
  (is (equal "aGk=" (mux::base64 "hi")))
  (is (equal "YWJj" (mux::base64 "abc"))))

;;; The queue and the drawer, as the canvas has them.

(test the-queue-says-where-a-question-is-and-who-is-looking-at-it-and-who-answered
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :window 1 :window-name "agents" :at 1 :says "impl"
                        :kind "claude-code" :state :blocked :for 42000
                        :asks '(:subject "Bash command" :detail ("python3 -m pytest -q")
                                :options ((1 "Yes") (2 "No"))))))
         (q (mux::%make-queue))
         (screen (tty:make-screen :width 120 :height 20)))
    (setf (mux::client-id client) 7
          (mux::client-clients client) '((7 "/dev/ttys042" 40 120 "lib" 1 1000 nil)
                                         (9 "/dev/ttys051" 30 100 "todo" 1 5000 200))
          (mux::client-lately client) '(("todo" 2 4000 (:client 7 "/dev/ttys042") :answer "1 Yes" t)
                                        ("todo" 2 9000 (:pane "todo:1.4") :prompt "run it" :refused)))
    (let ((mux::*drawing-for* client))
      (mux:draw-over q screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 20 :collect (shown screen y)))))
      (is (search "todo › 1 agents › impl" all) "~A" all)
      (is (search "⌨ ttys051 looking" all) "~A" all)
      (is (null (search "ttys042 is looking" all)) "the client itself is not somebody else")
      (is (search "◆ here" all) "~A" all)
      (is (search "⌁ todo:1.4" all) "~A" all)
      ;; the answers are buttons where they are drawn
      (is-true (mux::queue-laid q))
      (let ((found nil))
        (labels ((walk (w)
                   (when (and (typep w 'mux::bar-button)
                              (equal '(:answer "todo" 2 1) (mux::bar-button-runs w)))
                     (setf found t))
                   (dolist (part (atty/ui:parts w)) (walk part))))
          (walk (mux::queue-laid q)))
        (is-true found "answer 1 is not a button")))))

(test the-queue-has-a-toolbar-that-sorts-narrows-and-hides-what-was-answered
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :says "impl" :kind "claude-code" :state :blocked
                        :for 42000 :asks '(:subject "Bash command" :options ((1 "Yes"))))
                  (list :session "lib" :id 1 :says "core" :kind "claude-code" :state :blocked
                        :for 8000 :asks '(:subject "Fetch" :options ((1 "Yes"))))))
         (q (mux::%make-queue))
         (screen (tty:make-screen :width 120 :height 20)))
    (setf (mux::client-session client) "lib"
          (mux::client-over client) (list q)
          (mux::client-lately client) '(("todo" 2 4000 (:client 7 "/dev/ttys042") :answer "1 Yes" t)))
    (is (equal '("todo:2" "lib:1") (mapcar #'mux::row-address (mux::queue-rows q client))))
    (let ((mux::*client* client))
      (mux::queue-sort)
      (is (equal '("lib:1" "todo:2") (mapcar #'mux::row-address (mux::queue-rows q client)))
          "sorting by name did nothing")
      (mux::queue-every-session)
      (is (equal '("lib:1") (mapcar #'mux::row-address (mux::queue-rows q client)))
          "narrowing to this session did nothing")
      (mux::queue-every-session))
    (let ((mux::*drawing-for* client)) (mux:draw-over q screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 20 :collect (shown screen y)))))
      (is (search "sort ▾  by name" all) "~A" all)
      (is (search "✓  every session" all) "~A" all)
      (is (search "ANSWERED LATELY" all) "~A" all)
      (is (search "↵ go" all) "the actions are not buttons on the toolbar: ~A" all)
      ;; the toggle is a button: a click on it hides what was answered lately
      (let* ((line (loop :for y :below 20 :when (search "answered lately" (shown screen y)) :return y))
             (col (search "answered lately" (shown screen line))))
        (let ((mux::*client* client)
              (mux::*mouse-at* (cons col line)))
          (mux::queue-click))
        (is (null (mux::queue-showing-lately q)) "the click on the toggle did nothing")))
    (let ((mux::*drawing-for* client)) (mux:draw-over q screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 20 :collect (shown screen y)))))
      (is (null (search "ANSWERED LATELY" all)) "~A" all))))

(test the-drawer-is-in-sections-and-answers-from-its-foot
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :window 1 :window-name "agents" :at 1 :says "impl"
                        :label "impl" :kind "claude-code" :state :blocked :for 42000 :focus t
                        :asks '(:subject "Bash command" :options ((1 "Yes") (2 "No"))))))
         (d (mux::%make-drawer))
         (key (cons "todo" 2))
         (now (mux::ms-here))
         (screen (tty:make-screen :width 150 :height 30)))
    (setf (mux::client-session client) "todo"
          (gethash key (mux::client-about client))
          (list :pane-about (list now '(:kind "claude-code" :version "2.1.278" :reader "claude-code"
                                        :pid 48213 :size (81 19) :directory "/tmp/x"
                                        :programs ("claude") :command "claude"))
                :agent-explained (list now :blocked :blocked nil)
                :pane-history (list now '((42000 :blocked)))
                :pane-log (list now nil)))
    (let ((mux::*drawing-for* client))
      (mux:draw-over d screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 30 :collect (shown screen y)))))
      (is (search "WHAT IT IS" all) "~A" all)
      (is (search "claude-code 2.1.278 · read by the claude" all) "~A" all)
      (is (search "pid 48213 · 81×19 · /tmp/x" all) "~A" all)
      (is (search "STATE" all))
      (is (search "HOW IT WAS READ" all))
      (is (search "WHO TYPED HERE" all))
      (is (search " 1 Yes" all) "no answers at the foot: ~A" all)
      (is (search "answer from here" all))
      (is (search " z zoom " all) "the drawer has no actions: ~A" all)
      (is (search "▾ STATE" all) "~A" all))
    ;; a click on a section's heading folds it
    (let ((line (loop :for yy :below 30 :when (search "▾ STATE" (shown screen yy)) :return yy)))
      (is-true line)
      (when line
        (mux::clicked-over d line (+ 3 (search "▾ STATE" (shown screen line))) client)
        (is (equal '(:state) (mux::drawer-folded d)))
        (let ((mux::*drawing-for* client)) (mux:draw-over d screen))
        (let ((all (format nil "~{~A~%~}" (loop :for y :below 30 :collect (shown screen y)))))
          (is (search "▸ STATE" all) "~A" all)
          (is (null (search "for 42s" all)) "the folded section is still drawn: ~A" all))))
    ;; a click on an answer is that answer, taken by the drawer itself
    (let ((sent nil))
      (setf (mux::client-wire client) nil)
      (multiple-value-bind (x y)
          (loop :for yy :below 30
                :for xx := (search " 1 Yes" (shown screen yy))
                :when xx :do (return (values xx yy)))
        (is-true x)
        (when x
          ;; no wire to send on, so telling the server comes apart; that it was
          ;; taken and tried is the point
          (setf sent (handler-case (mux::clicked-over d y (+ x 2) client)
                       (error () :tried)))
          (is-true sent "the click on the answer was not taken: ~S" sent))))))

;;; Spawning into a window.

(test a-pane-is-spawned-into-a-new-window-or-a-given-one
  (with-a-server-here (server path)
    (let ((wire (a-wire-to path)))
      (say-to wire (list :spawn "work" "cat" nil "first"))
      (heard-from server wire :spawned)
      (say-to wire (list :spawn "work" "cat" nil "second" :new))
      (let ((said (heard-from server wire :spawned))
            (session (mux:session-named server "work")))
        (is (equal "work:2.1" (fourth said)) "a new window's pane is not window 2's first: ~S" said)
        (is (eql 2 (length (mux:session-windows session))))
        (is (eql 1 (mux:window-number session (mux:session-window session)))
            "a window spawned into from outside took the session's view with it"))
      (say-to wire (list :spawn "work" "cat" nil "third" 1))
      (let ((said (heard-from server wire :spawned))
            (session (mux:session-named server "work")))
        (is (equal "work:1.2" (fourth said)) "a pane put in window 1 is not its second: ~S" said)
        (is (eql 1 (mux:window-number session (mux:session-window session)))
            "putting a pane in a window showed that window"))
      (mux:wire-close wire))))

;;; Everything on top closes on Escape and says nothing on the bar; a note is
;;; a band at the foot, and a yes or no is asked as one too.

(test escape-closes-the-drawer-and-the-bar-says-nothing-about-it
  (with-server (path :command "cat" :rows 12 :cols 100)
    (with-seer (seer path :rows 12 :cols 100)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~Ce" mux:+prefix+))
      (is-true (pump seer :want "what is cat?") "the drawer did not open: ~S" (seen seer))
      (is (null (search "· esc" (seen seer))) "the bar still carries a chip: ~S" (seen seer))
      (type-at seer (string (code-char 27)))
      (is-true (pump seer :until (lambda () (null (search "what is cat?" (seen seer)))))
               "Escape did not close the drawer: ~S" (seen seer)))))

(test a-note-is-one-band-at-the-foot-and-a-click-on-it-closes-it
  (let* ((client (mux::%make-client :id 7 :rows 10 :cols 60))
         (n (mux:make-note "init" (list "did not load" "line 4") :face :warning))
         (screen (tty:make-screen :width 60 :height 10)))
    (setf (mux::client-over client) (list n))
    (let ((mux::*drawing-for* client)) (mux:draw-over n screen))
    (is (equal "" (shown screen 7)) "the note took more rows than its lines")
    (is (search " init  did not load" (shown screen 8)) "~S" (shown screen 8))
    (is (search "any key" (shown screen 8)))
    (is (search "   line 4" (shown screen 9)))
    (let ((col (search "any key" (shown screen 8))))
      (is-true (mux::clicked-over n 8 col client) "the way out is not a button")
      (is (null (mux::client-over client)) "the click did not close it"))))

(test a-yes-or-no-is-a-band-answered-by-key-or-by-click
  (let* ((client (mux::%make-client :id 7 :rows 10 :cols 60))
         (said nil)
         (screen (tty:make-screen :width 60 :height 10)))
    (mux::confirm client "close it?" :yes (lambda (c) (declare (ignore c)) (setf said :yes)))
    (is (eq 'mux::confirm-mode (mux::client-mode client)))
    (let ((mux::*drawing-for* client)) (mux:draw-over (first (mux::client-over client)) screen))
    (is (search " ?  close it?" (shown screen 9)) "~S" (shown screen 9))
    (is (search " y yes " (shown screen 9)))
    (is (search " n no" (shown screen 9)))
    ;; n by key
    (let ((mux::*client* client)) (mux::confirm-no))
    (is (null (mux::client-over client)))
    (is (null said))
    ;; yes by click
    (mux::confirm client "close it?" :yes (lambda (c) (declare (ignore c)) (setf said :yes)))
    (let ((mux::*drawing-for* client)) (mux:draw-over (first (mux::client-over client)) screen))
    (let ((mux::*client* client)
          (mux::*mouse-at* (cons (1+ (search "y yes" (shown screen 9))) 9)))
      (mux::confirm-click))
    (is (eq :yes said) "the click on yes did not say yes")
    (is (null (mux::client-over client)))))

(test a-name-is-typed-on-the-line-at-the-foot-and-kept-or-undone
  (let* ((client (mux::%make-client :id 7 :rows 10 :cols 80))
         (kept nil) (swapped nil)
         (screen (tty:make-screen :width 80 :height 10)))
    (mux::entry client "name" "window todo › 2" "impl"
                :keep (lambda (text c) (declare (ignore c)) (setf kept text))
                :swap (lambda (c) (declare (ignore c)) (setf swapped t))
                :swap-says "pane instead")
    (is (eq 'mux::entry-mode (mux::client-mode client)))
    (let ((mux::*drawing-for* client)) (mux:draw-over (first (mux::client-over client)) screen))
    (is (search " name  window todo › 2  " (shown screen 9)) "~S" (shown screen 9))
    (is (search " impl_" (shown screen 9)) "~S" (shown screen 9))
    (is (search "↵ keep" (shown screen 9)))
    (is (search "Esc undo" (shown screen 9)))
    (is (search "⇥ pane instead" (shown screen 9)))
    ;; typing adds to it, DEL takes off it, RET keeps it
    (let ((mux::*client* client))
      (mux::unbound (first (mux::client-over client)) "-" client)
      (mux::unbound (first (mux::client-over client)) "t" client)
      (mux::entry-rub-out)
      (mux::entry-keep))
    (is (equal "impl-" kept))
    (is (null (mux::client-over client)))
    ;; Escape undoes
    (mux::entry client "name" "window todo › 2" "impl" :keep (lambda (text c) (declare (ignore c)) (setf kept text)))
    (let ((mux::*client* client)) (mux::entry-undo))
    (is (equal "impl-" kept) "undo kept something")
    (is (null (mux::client-over client)))
    ;; TAB swaps
    (mux::entry client "name" "window todo › 2" "impl" :swap (lambda (c) (declare (ignore c)) (setf swapped :yes)))
    (let ((mux::*client* client)) (mux::entry-swap))
    (is (eq :yes swapped))))

(test closing-a-window-asks-first-and-closes-on-yes
  (with-server (path :command "cat" :rows 12 :cols 100)
    (with-seer (seer path :rows 12 :cols 100)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~Cc" mux:+prefix+))
      (is-true (pump seer :want " 2 ") "no second window: ~S" (seen seer))
      (type-at seer (format nil "~C&" mux:+prefix+))
      (is-true (pump seer :want "close this window and every program in it?") "~S" (seen seer))
      (type-at seer "n")
      (is-true (pump seer :until (lambda () (null (search "close this window" (seen seer))))))
      (is-true (pump seer :want " 2 ") "no said no but the window went: ~S" (seen seer))
      (type-at seer (format nil "~C&" mux:+prefix+))
      (is-true (pump seer :want "close this window"))
      (type-at seer "y")
      (is-true (pump seer :until (lambda () (null (search " 2 " (seen seer)))))
               "yes did not close the window: ~S" (seen seer)))))

(test a-question-mark-on-the-queue-lists-its-keys
  (with-server (path :command "cat" :rows 14 :cols 100)
    (with-seer (seer path :rows 14 :cols 100)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~CN" mux:+prefix+))
      (is-true (pump seer :want "needs you") "~S" (seen seer))
      (type-at seer "?")
      (is-true (pump seer :want "keys · queue-mode") "~S" (seen seer))
      (is-true (pump seer :want "queue go") "the queue's own keys are not listed: ~S" (seen seer))
      (type-at seer (string (code-char 27)))
      (pump seer :seconds 1/4))))

;;; The palette: one prompt with four kinds.

(test the-palette-switches-kinds-by-prefix-and-by-tab
  (with-server (path :command "cat" :rows 16 :cols 100)
    (with-seer (seer path :rows 16 :cols 100)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~C:" mux:+prefix+))
      (is-true (pump seer :want " : ") "the palette did not open: ~S" (seen seer))
      (is-true (pump seer :want "bar off"))
      ;; another kind's prefix, typed first, is that kind
      (type-at seer "#")
      (is-true (pump seer :want "this terminal") "# did not open the clients: ~S" (seen seer))
      ;; TAB is the next kind round
      (type-at seer (string #\Tab))
      (is-true (pump seer :want "hit, the pane follows") "TAB did not open find: ~S" (seen seer))
      (type-at seer (string #\Tab))
      (is-true (pump seer :want "bar off") "TAB did not come round to the commands: ~S" (seen seer))
      ;; a click on a tab opens it too
      (multiple-value-bind (x y) (where-on seer "clients")
        (is-true x)
        (when x (click-at seer x y)))
      (is-true (pump seer :want "this terminal") "clicking the tab did not open the clients: ~S" (seen seer))
      (type-at seer (string (code-char 27)))
      (pump seer :seconds 1/4))))

(test the-windows-kind-previews-the-panes-of-the-window-chosen
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :window 1 :at 0 :label "impl" :kind "claude-code"
                        :state :blocked :for 42000 :asks '(:subject "Bash command" :options ((1 "Yes"))))
                  (list :session "todo" :id 4 :window 1 :at 1 :says "ctl" :kind "shell" :known nil
                        :doing "atty agent wait")))
         (tree (mux::window-preview '(:session "todo" :window 1 :label "agents") client))
         (screen (painted tree 70 4))
         (all (format nil "~{~A~%~}" (loop :for y :below 4 :collect (shown screen y)))))
    (is (search "todo › 1 agents" all) "~A" all)
    (is (search "▲  1 impl claude-code  Bash command" all) "~A" all)
    (is (search "·  2 ctl shell  atty agent wait" all) "~A" all)))

(test find-lists-its-hits-as-you-type-and-the-pane-follows-the-one-chosen
  (with-server (path :command "cat" :rows 14 :cols 90)
    (with-seer (seer path :rows 14 :cols 90)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~{line-~D~%~}" (loop :for i :from 1 :to 40 :collect i)))
      (is-true (pump seer :want "line-40"))
      (type-at seer (format nil "~C/" mux:+prefix+))
      (is-true (pump seer :want "hit, the pane follows") "find did not open: ~S" (seen seer))
      (type-at seer "line-1")
      (is-true (pump seer :want "of 22" :seconds 3) "the hits are not counted: ~S" (seen seer))
      (is-true (pump seer :want "line-19") "the hits are not listed: ~S" (seen seer))
      (is-true (pump seer :want "↓") "the pane did not scroll to the newest hit: ~S" (seen seer))
      ;; up is the next older hit: the pane follows
      (type-at seer (format nil "~C[A" #\Escape))
      (is-true (pump seer :want "2 of 22" :seconds 3) "the pane did not follow the choice: ~S" (seen seer))
      ;; C-RET copies the chosen hit's line
      (type-at seer (format nil "~C[13;5u" #\Escape))
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~C/" mux:+prefix+))
      (is-true (pump seer :want "line-1") "the last find is not offered again: ~S" (seen seer))
      (type-at seer (string (code-char 27)))
      (pump seer :seconds 1/4)
      (type-at seer "q")
      (pump seer :seconds 1/4))))

;;; Following: one terminal goes where another goes.

(test a-terminal-that-follows-another-goes-where-it-goes-until-it-types
  (with-a-server-here (server path)
    (mux:add-session server "cat" :name "one" :rows 6 :cols 40)
    (mux:add-session server "cat" :name "two" :rows 6 :cols 40)
    (let ((lead (a-wire-to path))
          (tail (a-wire-to path)))
      (say-to lead (list :who "/dev/ttys001") (list :want "one") (list :attach 6 40 t))
      (heard-from server lead :hello)
      (say-to tail (list :who "/dev/ttys002") (list :want "two") (list :attach 6 40 t))
      (heard-from server tail :hello)
      (let* ((leader (find "/dev/ttys001" (mux::every-watcher server) :key #'mux::watcher-tty :test #'equal))
             (follower (find "/dev/ttys002" (mux::every-watcher server) :key #'mux::watcher-tty :test #'equal)))
        (is-true (and leader follower))
        (say-to tail (list :follow (mux::watcher-id leader)))
        (is-true (step-until server (lambda () (eq (mux::watcher-session follower) (mux::watcher-session leader))))
                 "following did not bring the follower to the leader's session")
        (is (equal "one" (mux:session-name (mux::watcher-session follower))))
        ;; the leader moves; the follower comes along
        (say-to lead (list :go "two"))
        (is-true (step-until server (lambda () (equal "two" (mux:session-name (mux::watcher-session follower)))))
                 "the follower did not follow the leader to the other session")
        ;; the clients say who follows whom
        (say-to tail (list :clients))
        (let* ((rows (second (heard-from server tail :clients)))
               (mine (find (mux::watcher-id follower) rows :key #'first)))
          (is (eql (mux::watcher-id leader) (ninth mine)) "~S" rows))
        ;; a key of the follower's own ends it
        (say-to tail (list :keys "x"))
        (is-true (step-until server (lambda () (null (mux::watcher-following follower)))))
        (say-to lead (list :go "one"))
        (step-until server (lambda () (equal "one" (mux:session-name (mux::watcher-session leader)))))
        (is (equal "two" (mux:session-name (mux::watcher-session follower)))
            "the follower still followed after typing")
        ;; following again, and the leader detaching, ends it too
        (say-to tail (list :follow (mux::watcher-id leader)))
        (is-true (step-until server (lambda () (equal "one" (mux:session-name (mux::watcher-session follower))))))
        (say-to lead (list :detach))
        (is-true (step-until server (lambda () (null (mux::watcher-following follower))))
                 "the leader went and the follower still follows"))
      (mux:wire-close tail))))
