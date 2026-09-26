;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/test)

(def-suite server-loop :in all)
(in-suite server-loop)

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
                                                           (not (mux:client-running (seer-client seer)))))
                                       "it never stopped")
                              (is (null (mux:client-running (seer-client seer))))
                              (is (eql :done (mux:client-exit-reason (seer-client seer)))))))

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
                              ;; sixty columns is a narrow bar: the glyph after the window's number says it
                              (is-true (pump seer :want "1○") "the bar never showed idle: ~S" (seen seer))
                              (is-true (pump seer :want "1◐") "the bar never showed working: ~S" (seen seer)))))

(test a-program-is-told-which-pane-it-is-in-and-where-its-server-is
      (with-server (path :command "printf 'pane=%s socket=%s\\n' \"$ATTY_PANE\" \"$ATTY_SOCKET\"; sleep 30"
                         :rows 10 :cols 120)
                   (with-seer (seer path :rows 10 :cols 120)
                              (is-true (pump seer :want "pane=0:") "~S" (seen seer))
                              (is (search (format nil "socket=~A" path) (seen seer))
                                  "the program was not told where its server is: ~S" (seen seer)))))

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
              (is (null (mux::server-alive-p path 1/2))
                  "a socket nobody is reading was taken for a running server"))
          (ignore-errors (sb-bsd-sockets:socket-close socket))
          (ignore-errors (delete-file path)))))

(test a-server-that-is-running-answers-a-knock
      (with-server (path :command "sleep 30")
                   (is-true (mux::server-alive-p path 3))))

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
                              (is-true (mux:client-running (seer-client seer))))))

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
      (is (equal "bash" (mux::program-display-name "/gnu/store/abc-bash-5.2.37/bin/bash")))
      (is (equal "sh -c 'a thing'" (mux::program-display-name "/bin/sh -c 'a thing'")))
      (is (equal "vim" (mux::program-display-name "vim")))
      (is (equal "" (mux::program-display-name nil))))

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
            (w (mux::%make-watcher :interactive t)))
        (setf (mux::session-watchers session) (list w)
              (mux::session-clocked session) 0
              (mux::watcher-behind w) nil)
        (is-true (mux::session-tick session mux::+bar-refresh-interval+)
                 "the bar stood a whole gap and nobody was put behind")
        (is-true (mux::watcher-behind w))
        (setf (mux::watcher-behind w) nil)
        (is (null (mux::session-tick session (1+ mux::+bar-refresh-interval+)))
            "the bar was drawn again the moment after")
        (is-false (mux::watcher-behind w))
        (is-true (mux::session-tick session (* 2 mux::+bar-refresh-interval+)))
        (is-true (mux::watcher-behind w))))

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
                                                           (not (mux:client-running (seer-client seer)))))
                                       "the client was not told the session was over")
                              (is (eq :done (mux:client-exit-reason (seer-client seer)))
                                  "the session ended for the wrong reason: ~S"
                                  (mux:client-exit-reason (seer-client seer))))))

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
                              (is-true (mux:server-running server)
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

(test past-the-scrollback-budget-a-pane-nobody-is-looking-at-gives-up-its-oldest-rows
      (with-a-server-here (server path)
                          (let* ((one (mux:add-session server "seq 1 3000; sleep 30" :name "one" :rows 6 :cols 20))
                                 (two (mux:add-session server "seq 1 3000; sleep 30" :name "two" :rows 6 :cols 20))
                                 (terms (mapcar (lambda (s) (mux:pane-term (first (mux:session-panes s))))
                                                (list one two))))
                            (is-true (step-until server (lambda ()
                                                          (every (lambda (term) (search "3000" (term:term-dump-to-string term)))
                                                                 terms))))
                            (let ((mux:+scrollback-budget+ 2000))
                              (is (<= (mux::trim-scrollback server) 2000))
                              (dolist (term terms)
                                (is (<= (term:term-scrollback-size term) 1000))
                                (is (search "3000" (term:term-dump-to-string term))))))))

(test a-session-whose-program-cannot-start-is-said-to-whoever-asked-and-not-kept
      (with-a-server-here (server path)
                          (let ((wire (a-wire-to path)) (heard nil))
                            (say-to wire (list :open "nowhere" "sh" "/no/such/directory" 10 40 nil))
                            (is-true (step-until server (lambda ()
                                                          (setf heard (append heard (heard-back wire)))
                                                          (find :bye heard :key #'first))))
                            (let ((bye (find :bye heard :key #'first)))
                              (is (search "could not start" (princ-to-string (second bye))) "said ~S" bye))
                            (is (null (mux::session-named server "nowhere")))
                            (mux:wire-close wire))))

(test a-window-whose-program-cannot-start-goes-and-whoever-watches-is-told
      (with-a-server-here (server path)
                          (let* ((session (mux:add-session server "cat" :name "here" :rows 10 :cols 40))
                                 (wire (a-wire-to path))
                                 (heard nil))
                            (say-to wire (list :want "here") (list :attach 10 40 nil))
                            (step-until server (lambda () (mux:session-watchers session)))
                            (mux::session-add-window session "sh" "/no/such/directory" nil)
                            (is-true (step-until server (lambda ()
                                                          (setf heard (append heard (heard-back wire)))
                                                          (find-if (lambda (f) (and (eq (first f) :say)
                                                                                    (search "could not start" (second f))))
                                                                   heard))))
                            (is (= 1 (length (mux:session-panes session))))
                            (mux:wire-close wire))))
