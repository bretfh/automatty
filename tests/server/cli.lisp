;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/test)

(def-suite server-cli :in all)
(in-suite server-cli)

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
    (is (equal "impl" (mux::option-value args "--name")))
    (is-true (mux::flag-p args "--json"))
    (is (equal '("work") (mux::positional-args args "--name" "--cwd")))))

(test what-goes-out-as-json-is-json
  (is (equal "[{\"address\":\"todo:2\",\"for_ms\":42000,\"state\":\"blocked\",\"asks\":null,\"ok\":true}]"
             (with-output-to-string (s)
               (mux::to-json (list (list :address "todo:2" :for-ms 42000 :state :blocked :asks nil :ok t))
                          s))))
  (is (equal "\"a \\\"q\\\"\\nb\"" (with-output-to-string (s) (mux::to-json (format nil "a \"q\"~%b") s)))))

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

(test bracketed-paste-follows-the-focused-pane-to-the-host
  ;; +took-over+ never turns ?2004h on by itself: only a pane's own request
  ;; reaches the real terminal, only while that pane has the focus. A turns
  ;; it on right away and off again once given a line, on its own output, so
  ;; nothing here depends on the pty's local echo of what is typed in.
  (with-a-server-here (server path)
    (let* ((session (mux:add-session
                     server "printf '\\033[?2004h'; read x; printf '\\033[?2004l'; cat"
                     :name "work" :rows 6 :cols 30))
           (a (mux:session-focus session))
           (wire (a-wire-to path))
           (heard (heard-tags server wire)))
      (mux:session-add-window session "cat" nil nil)
      (is-true (step-until server (lambda () (term:term-bracketed-paste (mux:pane-term a)))))
      (say-to wire (list :want "work") (list :attach 6 30 t))
      (funcall heard :hello)
      (is (equal (list :bracketed-paste t) (funcall heard :bracketed-paste))
          "A already wants it when the client attaches: told right away")
      ;; focus leaves A for B, which never asked
      (mux:session-select-window session 2)
      (is (equal (list :bracketed-paste nil) (funcall heard :bracketed-paste)))
      ;; focus comes back to A, still on
      (mux:session-select-window session 1)
      (is (equal (list :bracketed-paste t) (funcall heard :bracketed-paste)))
      ;; A turns it back off without the focus moving
      (say-to wire (list :keys (format nil "go~%")))
      (is-true (step-until server (lambda () (not (term:term-bracketed-paste (mux:pane-term a))))))
      (is (equal (list :bracketed-paste nil) (funcall heard :bracketed-paste)))
      (mux:wire-close wire))))

(test a-pane-that-never-asks-for-bracketed-paste-leaves-the-host-alone
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 30))
           (pane (mux:session-focus session))
           (wire (a-wire-to path)))
      (say-to wire (list :want "work") (list :attach 6 30 t))
      (heard-from server wire :hello)
      (say-to wire (list :keys (format nil "hello~%")))
      (is-true (step-until server (lambda () (search "hello" (term:term-dump-to-string
                                                              (mux:pane-term pane))))))
      (is (null (find :bracketed-paste (heard-back wire) :key #'first))
          "a plain pane never asked for it; the host should never hear of it")
      (mux:wire-close wire))))
