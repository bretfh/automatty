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
                                      :interval 0)))
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
                              (is-true (pump seer :want "9 40")
                                       "the program was not given the rows the bar left it")
                              (pty:pty-set-size (seer-master seer) 20 60)
                              (term:term-resize (seer-host seer) 60 20)
                              (multiple-value-bind (rows cols) (tty:host-size (seer-slave seer))
                                                   (is (eql 20 rows))
                                                   (is (eql 60 cols)))
                              (mux:client-resized (seer-client seer))
                              (is-true (pump seer :want "19 60")))))

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
                                (is (equal (list "0" (mux:pane-id pane) "cat") (butlast (first rows)))
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
                              (is (equal (list :agent "0" (mux:pane-id pane) "cat" :blocked)
                                         (find :agent heard :key #'first)))
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
                                (is (equal (list :agent-prompted "0" id :blocked) (hear :agent-prompted))
                                    "a blocked pane took a prompt")
                                (mux:wire-send wire (list :agent-signal "0" id :idle))
                                (mux:wire-send wire (list :agent-prompt "0" id "more"))
                                (mux:wire-flush wire)
                                (is-true (step-until server (lambda ()
                                                              (eq :idle (agent:agent-state (mux:pane-agent pane))))))
                                (is (equal (list :agent-prompted "0" id t) (hear :agent-prompted)))
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

(test the-bar-has-a-chip-for-every-pane-saying-what-it-is-doing
      (with-server (path :command "printf 'just-a-shell\\n'; sleep 30" :rows 10 :cols 80)
                   (with-seer (seer path :rows 10 :cols 80)
                              (is-true (pump seer :want "just-a-shell"))
                              (is-true (pump seer :want "○ idle")
                                       "the bar did not say the quiet pane is idle: ~S" (seen seer))
                              (type-at seer (format nil "~C3" mux:+prefix+))
                              (is-true (pump seer :until (lambda () (<= 2 (count-of "○ idle" (seen seer)))))
                                       "a second pane got no chip of its own: ~S" (seen seer)))))

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
                              (is (equal "├── leaf" (row (seer-host seer) 1))
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
                              (is-true (pump seer :want "9 40")
                                       "the program was told the whole terminal, bar and all: ~S"
                                       (seen seer)))))

(test turning-the-bar-off-reaches-the-server-not-just-the-client
      (with-server (path :command "while :; do stty size; sleep 0.3; done"
                         :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (is-true (pump seer :want "9 40") "the bar was not taking a row")
                              (type-at seer (format nil "~Ct" mux:+prefix+))
                              (is-true (pump seer :want "10 40")
                                       "the bar did not come off: ~S" (seen seer))
                              (type-at seer (format nil "~Ct" mux:+prefix+))
                              (is-true (pump seer :want "9 40")
                                       "the bar did not come back: ~S" (seen seer)))))

(test with-no-bar-the-program-has-the-whole-terminal
      ;; the server runs in a thread of its own, and a special bound here would not
      ;; reach it
      (let ((was mux:*bar*))
        (unwind-protect
            (progn (setf mux:*bar* nil)
                   (with-server (path :command "stty size; sleep 30" :rows 10 :cols 40)
                                (with-seer (seer path :rows 10 :cols 40)
                                           (is-true (pump seer :want "10 40") "~S" (seen seer)))))
          (setf mux:*bar* was))))

(test the-prompt-opens-on-the-prefix-and-runs-what-was-chosen
      (with-server (path :command "printf 'the-pane\\n'; sleep 30" :rows 12 :cols 50)
                   (with-seer (seer path :rows 12 :cols 50)
                              (is-true (pump seer :want "the-pane"))
                              (type-at seer (format nil "~C:" mux:+prefix+))
                              (is-true (pump seer :want "run") "the prompt did not open: ~S" (seen seer))
                              (is-true (pump seer :want "detach"))
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
                                         (is-true (pump mine :want "detach"))
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
                              (type-at seer "on-the-first")
                              (is-true (pump seer :want "on-the-first"))
                              (type-at seer (format nil "~C3" mux:+prefix+))
                              (is-true (pump seer :until (lambda () (search "│" (seen seer))))
                                       "no rule came up between the two panes: ~S" (seen seer))
                              (type-at seer (format nil "~C[<0;3;3M~C[<0;3;3m" #\Escape #\Escape))
                              (pump seer :seconds 1/2)
                              (type-at seer "-again")
                              (is-true (pump seer :want "on-the-first-again")
                                       "the click did not move the focus to the pane under it: ~S"
                                       (seen seer))
                              (is (< (or (where-said seer "on-the-first-again") 100) 20)
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
                              (type-at seer (format nil "~Cc" mux:+prefix+))
                              (is-true (pump seer :until (lambda ()
                                                           (null (search "in-the-first" (seen seer)))))
                                       "the new session was shown what the first one holds: ~S" (seen seer))
                              (type-at seer "in-the-second")
                              (is-true (pump seer :want "in-the-second"))
                              (is (null (search "in-the-first" (seen seer)))
                                  "the two sessions are sharing a screen: ~S" (seen seer))
                              (type-at seer (format nil "~Cb" mux:+prefix+))
                              (is-true (pump seer :want "● here") "no chooser came up: ~S" (seen seer))
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
                              (is-true (pump seer :until (lambda () (search "│" (seen seer))))
                                       "nothing split: ~S" (seen seer))
                              (type-at seer "in-the-second")
                              (is-true (pump seer :want "in-the-second"))
                              (type-at seer (format nil "~C1" mux:+prefix+))
                              (is-true (pump seer :until (lambda () (null (search "│" (seen seer)))))
                                       "the rule is still there, so both panes are: ~S" (seen seer))
                              (is-true (pump seer :until (lambda () (null (search "in-the-first"
                                                                                  (seen seer)))))
                                       "the pane without the cursor was kept: ~S" (seen seer))
                              (type-at seer "still-typing")
                              (is-true (pump seer :want "still-typing")
                                       "the pane that was kept stopped taking keys: ~S" (seen seer)))))

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
      (is (equal (list :name-it "work" (mux:pane-id pane) "arch" nil)
                 (heard-from server wire :name-it)))
      (mux:wire-close wire))))

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
        (is (eql 20 width))
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
        (is (eq :say (third entry))))
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

(defun where-on (seer said)
  "Column and row, from 0, where SAID first is on the seer's screen."
  (loop :for y :below (term:term-height (seer-host seer))
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
      (is-true (pump seer :until (lambda () (search "╔" (seen seer))))
               "no double frame round the focused pane: ~S" (seen seer))
      (type-at seer (format nil "~C," mux:+prefix+))
      (is-true (pump seer :want "name ") "the name prompt did not open: ~S" (seen seer))
      (type-at seer (format nil "impl~C" #\Return))
      (is-true (pump seer :want " impl sleep")
               "the frame does not carry the new name and the program: ~S" (seen seer))
      (is-true (pump seer :want "○ idle")))))

(test a-blocked-pane-is-answered-by-clicking-an-answer-in-its-border
  (let ((script (a-dialog-script)))
    (unwind-protect
         (with-server (path :command "/bin/sh" :rows 16 :cols 90)
           (with-seer (seer path :rows 16 :cols 90)
             (pump seer :seconds 1/2)
             (type-at seer (format nil "~C3" mux:+prefix+))
             (pump seer :until (lambda () (search "╔" (seen seer))))
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
      (pump seer :until (lambda () (search "╔" (seen seer))))
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
            (list* :heard-at (mux::ms-here) row)))))

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
      (is (search "todo:2" all))
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
             (type-at seer (format nil "~Cc" mux:+prefix+))
             (is-true (pump seer :until (lambda () (null (search "Bash command" (seen seer))))))
             (type-at seer (format nil "~Cn" mux:+prefix+))
             (is-true (pump seer :want "1 waiting") "the queue did not open: ~S" (seen seer))
             (is-true (pump seer :want "python3 -m pytest -q"))
             (type-at seer "1")
             (is-true (pump seer :want "nothing needs you")
                      "answering did not take it off the queue: ~S" (seen seer))
             (is-true (pump seer :want "✓")
                      "the answer is not among those answered lately: ~S" (seen seer))
             (is-true (pump seer :want "by you") "~S" (seen seer))
             (type-at seer (string (code-char 27)))
             (pump seer :seconds 1/2)
             (type-at seer (format nil "~Cb" mux:+prefix+))
             (is-true (pump seer :want "● here"))
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

(test the-switchboard-is-a-band-a-session-and-needs-you-first-within-one
  (let* ((client (board-client))
         (b (mux::%make-board)))
    (let ((bands (mux::board-bands b client)))
      (is (equal '("todo" "lib") (mapcar #'car bands)) "the bands were not in the server's order")
      (is (equal '(2 1 4) (mapcar (lambda (r) (getf r :id)) (cdr (first bands))))
          "within a band the one asking did not come first, then the longest working"))
    (setf (mux::board-sort b) 1)
    (is (equal '(1 2 4) (mapcar (lambda (r) (getf r :id)) (cdr (first (mux::board-bands b client)))))
        "laid out was not the order the session has them in")
    (setf (mux::board-query b) "tests")
    (is (equal '("lib") (mapcar #'car (mux::board-bands b client))))))

(test the-switchboard-draws-what-each-is-doing-and-who-drives-it
  (let* ((client (board-client))
         (b (mux::%make-board :starting "todo"))
         (screen (tty:make-screen :width 140 :height 30)))
    (let ((mux::*drawing-for* client))
      (mux:draw-over b screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 30 :collect (shown screen y)))))
      (is (search " todo" all))
      (is (search "● here" all) "~A" all)
      (is (search "go there" all) "the other session does not say it can be gone to")
      (is (search "Recording clarification 9…" all) "what the agent is doing is not on its card")
      (is (search "▲ Bash command" all))
      (is (search " 1 Yes" all) "the answers are not on the asking card")
      (is (search "⌁ driven by todo:4" all))
      (is (search "drives todo:1" all))
      (is (search "+ new pane in lib" all))
      (is (search "needs you first" all)))
    (is (equal '("todo" . 2) (mux::board-cursor b))
        "the cursor did not start on the first card of the session this is")))

(test a-cards-strip-says-what-the-pane-was-over-the-last-while
  (is (eq :idle (mux::state-at '((3000 :idle) (60000 :working)) 0)))
  (is (eq :working (mux::state-at '((3000 :idle) (60000 :working)) 30000)))
  (is (null (mux::state-at '((3000 :idle) (60000 :working)) 90000))
      "a time before anything was known was said to be something"))

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
      (type-at seer (format nil "~Cc" mux:+prefix+))
      (pump seer :seconds 1)
      (type-at seer (format nil "~Cw" mux:+prefix+))
      (is-true (pump seer :want "+ new pane in 1") "the switchboard did not open: ~S" (seen seer))
      (type-at seer " ")
      (pump seer :seconds 1/4)
      (type-at seer (format nil "~C[B" #\Escape))
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
                      '(("live-prompt-box" 950 :prompt-box :idle nil "")
                        ("bash-permission-prompt" 850 :whole :blocked t
                         "Bash command
Do you want to proceed?
❯ 1. Yes")
                        ("generic-permission-prompt" 840 :after-last-rule :blocked t "x")))
                :pane-about (list now '(:kind "claude-code" :programs ("claude --resume")
                                        :group 48213 :command "sh -c \"exec claude\""))
                :pane-history (list now '((42000 :blocked) (60000 :working)))
                :pane-log (list now '((50000 (:pane "todo:4") :prompt "STATUS?" :refused)
                                      (80000 (:pane "todo:4") :prompt "run the suite" t)
                                      (90000 (:client 7 "/dev/ttys004") :keys 14 t)))))
    (let ((mux::*drawing-for* client))
      (mux:draw-over d screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 36 :collect (shown screen y)))))
      (is (search "why is todo:2 blocked?" all) "~A" all)
      (is (search "foreground claude --resume  group 48213" all))
      (is (search "*  850 blocked bash-permission-prompt" all)
          "the winning rule is not marked: ~A" all)
      (is (search "│ Do you want to proceed?" all) "the winning rule's text is not under it")
      (is (search "+ " all) "another rule that matched is not marked")
      (is (search "who typed here" all))
      (is (search "todo:4" all))
      (is (search "refused" all))
      (is (search "keys 14 bytes" all))
      (is (search "last 20 minutes" all)))))

(test the-drawer-follows-the-focus-and-typing-still-reaches-the-pane
  (with-server (path :command "cat" :rows 24 :cols 150)
    (with-seer (seer path :rows 24 :cols 150)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~Ce" mux:+prefix+))
      (is-true (pump seer :want "why is 0:") "the drawer did not open: ~S" (seen seer))
      (is-true (pump seer :want "no rules know this program"))
      (type-at seer "typed-past-it")
      (is-true (pump seer :want "typed-past-it")
               "what was typed with the drawer open did not reach the pane: ~S" (seen seer))
      (is-true (pump seer :want "bytes") "the drawer does not say who typed: ~S" (seen seer))
      (type-at seer (format nil "~Ce" mux:+prefix+))
      (is-true (pump seer :until (lambda () (null (search "why is" (seen seer)))))
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
