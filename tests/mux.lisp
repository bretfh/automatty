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

(defstruct seer client host master slave (decoder (term:make-decoder)))

(defun a-seer (path &key (rows 10) (cols 40))
  (multiple-value-bind (master slave-path) (pty:open-pty)
                       (let ((slave (sb-posix:open slave-path sb-posix:o-rdwr)))
                         (pty:pty-set-size master rows cols)
                         (tty:host-raw slave)
                         (make-seer :client (mux:make-client path :fd slave :to slave)
                                    :host (a-term :width cols :height rows)
                                    :master master
                                    :slave slave))))

(defun seer-close (seer)
  (ignore-errors (mux:client-close (seer-client seer)))
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
     (when (pty:pty-wait (seer-master seer) 0)
       (let ((said (pty:pty-read-string (seer-master seer) 65536)))
         (when said
           (term:term-process-output
            (seer-host seer)
            (term:decode-utf-8 (seer-decoder seer) said)))))
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
                                (is (equal (list "0" (mux:pane-id pane) "agent") (butlast (first rows)))
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
                              (is (equal (list :agent "0" (mux:pane-id pane) "agent" :blocked)
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

(test a-prompt-is-pasted-and-entered-a-moment-later
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
                                                            "hello there"))
                                  (mux:wire-flush wire)
                                  (is-true (pump seer :want "^[[200~hello there^[[201~")
                                           "the text was not pasted as a paste, or enter never came: ~S" (seen seer))
                                  (is (>= (- (get-internal-real-time) then)
                                          (* 1/4 internal-time-units-per-second))
                                      "enter came in the same breath as the text")
                                  (mux:wire-close wire))))))

(test a-shell-is-not-said-to-be-doing-anything
      (with-server (path :command "printf 'just-a-shell\\n'; sleep 30" :rows 10 :cols 40)
                   (with-seer (seer path :rows 10 :cols 40)
                              (is-true (pump seer :want "just-a-shell"))
                              (pump seer :seconds 1)
                              (is (null (search "idle" (seen seer)))
                                  "the bar reports on a program nobody recognised: ~S" (seen seer)))))

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
                              (is-true (pump seer :want "session") "no chooser came up: ~S" (seen seer))
                              (is-true (pump seer :until (lambda () (search "1 pane" (seen seer))))
                                       "the chooser does not say what is in them: ~S" (seen seer))
                              (type-at seer (string #\Return))
                              (is-true (pump seer :want "in-the-first")
                                       "choosing the first session did not go back to it: ~S" (seen seer))
                              (is (null (search "in-the-second" (seen seer)))
                                  "what the second session holds came along: ~S" (seen seer)))))

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
