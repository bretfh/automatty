(in-package #:atty/test)

(def-suite agent :in all)
(in-suite agent)

(defun claude-screen (&rest lines)
  (let ((term (a-term :width 80 :height 12)))
    (say term (osc "0;✳ Claude Code"))
    (dolist (line lines term)
      (say term (format nil "~A~C~C" line #\Return #\Newline)))))

(defun over-again (term &rest lines)
  (say term (csi "2J") (csi "H"))
  (dolist (line lines term)
    (say term (format nil "~A~C~C" line #\Return #\Newline))))

(test a-program-nobody-knows-is-working-while-it-prints-and-idle-once-quiet
  (let ((a (agent:make-agent "" "cat"))
        (term (a-term :width 40 :height 5)))
    (is (eq 'agent:agent (type-of a)))
    (is-true (agent:agent-look a term 0 t))
    (is (eq :working (agent:agent-state a)))
    (is-false (agent:agent-look a term 100 nil))
    (is-false (agent:agent-look a term 400 nil))
    (is (eq :working (agent:agent-state a)) "it went idle before the hold was up")
    (is-true (agent:agent-look a term (+ 100 agent:+hold+) nil))
    (is (eq :idle (agent:agent-state a)))))

(test a-program-that-was-never-working-is-idle-at-once
  (let ((a (agent:make-agent "" "cat"))
        (term (a-term :width 40 :height 5)))
    (is-true (agent:agent-look a term 0 nil))
    (is (eq :idle (agent:agent-state a)))))

(test claude-code-is-known-by-its-title-or-its-command
  (is (eq 'agent:claude-code (type-of (agent:make-agent "✳ Claude Code" "bash"))))
  (is (eq 'agent:claude-code (type-of (agent:make-agent "" "claude --resume x"))))
  (is (eq 'agent:claude-code (type-of (agent:make-agent "" "/usr/bin/claude"))))
  (is (eq 'agent:agent (type-of (agent:make-agent "vim" "claudette")))))

(test a-known-agent-is-unknown-until-its-screen-says-something
  (let ((a (agent:make-agent "" "bash"))
        (term (claude-screen " ▐▛███▛█   Claude Code v2.1.258" "  ▝▝ ▝▝    ~/git/cl/atty")))
    (agent:agent-look a term 0 t)
    (is (eq :working (agent:agent-state a)))
    (agent:agent-become a "✳ Claude Code" "bash")
    (is (eq 'agent:claude-code (type-of a)))
    (is (eq :unknown (agent:agent-state a)) "it kept the shell's state")
    (agent:agent-look a term 10 t)
    (agent:agent-look a term 900 nil)
    (is (eq :unknown (agent:agent-state a)) "a banner with no prompt box was taken for a state")
    (say term (format nil "─~C~C❯ ~C~C─~C~C" #\Return #\Newline #\Return #\Newline #\Return #\Newline))
    (is-true (agent:agent-look a term 1000 t))
    (is (eq :idle (agent:agent-state a)))))

(test claude-code-at-its-prompt-box-is-idle-at-once
  (let ((a (agent:make-agent "✳ Claude Code" ""))
        (term (claude-screen "─" "❯ " "─")))
    (say term (osc "0;◐ Claude Code"))
    (agent:agent-look a term 0 t)
    (is (eq :working (agent:agent-state a)))
    (say term (osc "0;✳ Claude Code"))
    (is-true (agent:agent-look a term 10 t))
    (is (eq :idle (agent:agent-state a)) "the prompt box was held like a plain quiet")))

(test claude-code-thinking-is-working
  (let ((a (agent:make-agent "✳ Claude Code" ""))
        (term (claude-screen "✽ Contemplating… (14s · ↓ 950 tokens)"
                             "─" "❯" "─"
                             "  ⏵⏵ auto mode on (shift+tab to cycle) · esc to interrupt")))
    (agent:agent-look a term 0 t)
    (is (eq :working (agent:agent-state a)))))

(test claude-code-asking-permission-is-blocked
  (let ((a (agent:make-agent "✳ Claude Code" ""))
        (term (claude-screen "  Bash(rm -rf build)"
                             "  Do you want to proceed?"
                             "  ❯ 1. Yes"
                             "    2. Yes, and don't ask again"
                             "    3. No"
                             "  Esc to cancel")))
    (agent:agent-look a term 0 t)
    (is (eq :blocked (agent:agent-state a)))
    (multiple-value-bind (seen rows) (agent:agent-explain a term)
      (is (eq :blocked seen))
      (let ((hits (mapcar #'first (remove-if-not #'fifth rows))))
        (is (member "bash-permission-prompt" hits :test #'string=) "~S" hits)
        (is (not (member "live-prompt-box" hits :test #'string=)) "~S" hits)))))

(test claude-code-asking-for-trust-is-blocked
  (let ((a (agent:make-agent "✳ Claude Code" ""))
        (term (claude-screen "Quick safety check: Is this a project you trust?"
                             "❯ No, exit"
                             "  Yes, I trust this folder"
                             "Enter to confirm · Esc to cancel")))
    (agent:agent-look a term 0 t)
    (is (eq :blocked (agent:agent-state a)))))

(test the-transcript-viewer-says-nothing-about-state
  (let ((a (agent:make-agent "✳ Claude Code" ""))
        (term (claude-screen "─" "❯ " "─")))
    (agent:agent-look a term 0 t)
    (is (eq :idle (agent:agent-state a)))
    (over-again term "some old turn" "showing detailed transcript · ctrl+o to toggle")
    (is-false (agent:agent-look a term 5 t))
    (is (eq :idle (agent:agent-state a)))))

(test what-a-hook-says-is-published-at-once-and-stands-until-the-screen-moves
  (let ((a (agent:make-agent "" "cat"))
        (term (a-term :width 40 :height 5)))
    (agent:agent-hear a :blocked)
    (is-true (agent:agent-look a term 0 nil))
    (is (eq :blocked (agent:agent-state a)))
    (agent:agent-look a term 5000 nil)
    (is (eq :blocked (agent:agent-state a))
        "the screen stood still and the hook's word was dropped")
    (is-true (agent:agent-look a term 6000 t))
    (is (null (agent:agent-heard a)))
    (is (eq :working (agent:agent-state a)))))

(test what-the-screen-shows-beats-what-a-hook-says
  (let ((a (agent:make-agent "✳ Claude Code" ""))
        (term (claude-screen "─" "❯ " "─")))
    (agent:agent-hear a :blocked)
    (agent:agent-look a term 0 t)
    (is (eq :idle (agent:agent-state a)) "a hook's word overrode a prompt box on screen")))

(test a-hook-fills-in-where-a-known-agents-screen-says-nothing
  (let ((a (agent:make-agent "✳ Claude Code" ""))
        (term (claude-screen "nothing any rule knows")))
    (agent:agent-look a term 0 t)
    (is (eq :unknown (agent:agent-state a)))
    (agent:agent-hear a :working)
    (agent:agent-look a term 10 nil)
    (is (eq :working (agent:agent-state a)))))

(test regions-are-cut-where-claude-draws-its-rules
  (let ((term (a-term :width 20 :height 8)))
    (say term (format nil "top~C~C────~C~C❯ hello~C~C────~C~Cfoot~C~C"
                      #\Return #\Newline #\Return #\Newline #\Return #\Newline
                      #\Return #\Newline #\Return #\Newline))
    (is (equal "❯ hello" (agent:region-text term :prompt-box)))
    (is (equal "foot" (agent:region-text term :after-last-rule)))
    (is (equal "top" (agent:region-text term :last-above-prompt-box)))
    (is (equal (format nil "❯ hello~%────~%foot") (agent:region-text term '(:bottom 3))))
    (is (equal (format nil "top~%────~%❯ hello~%────~%foot") (agent:region-text term :whole)))))

(defclass counting-agent (agent:agent) ((looks :initform 0 :accessor looks)))

(defmethod agent:agent-rules ((a counting-agent))
  (incf (looks a))
  (agent:agent-rules (make-instance 'agent:claude-code)))

(test a-screen-nothing-has-touched-is-not-read-again-whatever-it-says
  (let ((a (make-instance 'counting-agent))
        (term (claude-screen "  Bash(rm -rf build)" "  Do you want to proceed?"
                             "  ❯ 1. Yes" "    2. No" "  Esc to cancel")))
    (agent:agent-look a term 0 t)
    (is (eq :blocked (agent:agent-state a)))
    (let ((n (looks a)))
      (agent:agent-look a term 100 nil)
      (agent:agent-look a term 200 nil)
      (is (= n (looks a)) "the rules were run over a screen nothing had touched")
      (is (eq :blocked (agent:agent-state a)))
      (agent:agent-look a term 300 t)
      (is (= (1+ n) (looks a))))))

(test the-last-lines-reach-back-into-what-scrolled-off
  (let ((term (a-term :width 20 :height 3)))
    (say term (format nil "a~C~Cb~C~Cc~C~Cd~C~Ce~C~C"
                      #\Return #\Newline #\Return #\Newline #\Return #\Newline
                      #\Return #\Newline #\Return #\Newline))
    (is (equal '("d" "e") (agent:last-lines term 2)))
    (is (equal '("b" "c" "d" "e") (agent:last-lines term 4)))
    (is (equal '("a" "b" "c" "d" "e") (agent:last-lines term 40)))))

(test a-program-is-known-by-what-is-running-when-what-was-started-was-a-shell
  (is (eq 'agent:agent (type-of (agent:make-agent "" "cd /x && exec claude"))))
  (is (eq 'agent:claude-code
          (type-of (agent:make-agent "" "cd /x && exec claude"
                                     '("claude --permission-mode acceptEdits")))))
  (is (eq 'agent:claude-code
          (type-of (agent:make-agent "" "sh" '("sh -c cd /x && claude" "claude")))))
  (is (eq 'agent:agent (type-of (agent:make-agent "" "sh" '("sh" "claudette"))))))

(test a-title-that-stops-naming-it-does-not-demote-what-is-still-running
  (let ((a (agent:make-agent "" "cd /x && exec claude" '("claude"))))
    (agent:agent-become a "⠂ Reading SPEC.md" "cd /x && exec claude" '("claude"))
    (is (eq 'agent:claude-code (type-of a)))
    (agent:agent-become a "zsh" "cd /x && exec claude" '("-zsh"))
    (is (eq 'agent:agent (type-of a)) "it outlived the program it was named for")))

(test a-prompted-agent-is-working-until-its-turn-has-begun-and-ended
  (let ((a (agent:make-agent "✳ Claude Code" ""))
        (term (claude-screen "─" "❯ " "─")))
    (agent:agent-look a term 0 t)
    (is (eq :idle (agent:agent-state a)))
    (agent:agent-prompted a 100)
    (is-true (agent:agent-look a term 200 nil))
    (is (eq :working (agent:agent-state a)) "the prompt was taken for the turn being over")
    (agent:agent-look a term 5000 t)
    (is (eq :working (agent:agent-state a)) "idle before the turn began")
    (over-again term "✽ Contemplating… (1s · ↓ 10 tokens)" "─" "❯" "─"
                "  ⏵⏵ auto mode on (shift+tab to cycle) · esc to interrupt")
    (agent:agent-look a term 6000 t)
    (is (eq :working (agent:agent-state a)))
    (is (null (agent:agent-prompted-at a)) "the turn began and it was still held")
    (over-again term "─" "❯ " "─")
    (is-true (agent:agent-look a term 9000 t))
    (is (eq :idle (agent:agent-state a)))))

(test a-prompt-that-never-starts-a-turn-is-let-go-after-a-while
  (let ((a (agent:make-agent "✳ Claude Code" ""))
        (term (claude-screen "─" "❯ " "─")))
    (agent:agent-look a term 0 t)
    (agent:agent-prompted a 0)
    (agent:agent-look a term 10 nil)
    (is (eq :working (agent:agent-state a)))
    (is-true (agent:agent-look a term (1+ agent:+turn-patience+) nil))
    (is (eq :idle (agent:agent-state a)))))

(test a-prompt-to-a-program-nobody-knows-holds-nothing
  (let ((a (agent:make-agent "" "cat")))
    (agent:agent-prompted a 0)
    (is (null (agent:agent-prompted-at a)))))

(test a-dialog-on-the-screen-is-blocking-whatever-the-pane-is-taken-for
  (is-true (agent:screen-blocked-p
            (claude-screen "  Bash(rm -rf build)"
                           "  Do you want to proceed?"
                           "  ❯ 1. Yes"
                           "    2. Yes, and don't ask again"
                           "    3. No"
                           "  Esc to cancel")))
  (is-true (agent:screen-blocked-p
            (claude-screen "Quick safety check: Is this a project you trust?"
                           "❯ No, exit"
                           "  Yes, I trust this folder"
                           "Enter to confirm · Esc to cancel")))
  (is-false (agent:screen-blocked-p (claude-screen "─" "❯ " "─"))))

;;; What the server knows about a pane: how long, what it asks, what it is.

(test an-agent-knows-since-when-it-has-been-what-it-is
  (let ((a (agent:make-agent "" "cat"))
        (term (a-term :width 40 :height 5)))
    (is (null (agent:agent-since a)) "it knew a time before it had been anything")
    (agent:agent-look a term 100 t)
    (is (eql 100 (agent:agent-since a)))
    (agent:agent-look a term 300 t)
    (is (eql 100 (agent:agent-since a)) "working again moved when it began working")
    (agent:agent-look a term (+ 400 agent:+hold+) nil)
    (agent:agent-look a term (+ 1200 agent:+hold+) nil)
    (is (eq :idle (agent:agent-state a)))
    (is (eql 2 (length (agent:agent-history a))) "~S" (agent:agent-history a))
    (is (eq :idle (second (first (agent:agent-history a)))))
    (is (eql (agent:agent-since a) (first (first (agent:agent-history a)))))
    (is (eql 500 (agent:agent-for a (+ (agent:agent-since a) 500))))))

(test an-agents-history-is-kept-to-a-length
  (let ((a (agent:make-agent "" "cat"))
        (term (a-term :width 40 :height 5)))
    (dotimes (i (* 3 agent:+history-length+))
      (agent:agent-hear a (if (evenp i) :blocked :working))
      (agent:agent-look a term (* 10 i) nil))
    (is (<= (length (agent:agent-history a)) agent:+history-length+))))

(test a-numbered-option-is-read-with-its-number-and-whether-it-is-pointed-at
  (is (equal '(1 "Yes" t) (agent:option-of " ❯ 1. Yes")))
  (is (equal '(3 "No, and tell Claude what to do differently (esc)" nil)
             (agent:option-of "   3. No, and tell Claude what to do differently (esc)")))
  (is (null (agent:option-of " Do you want to proceed?")))
  (is (null (agent:option-of "  118 + ### Clarification 9")))
  (is (null (agent:option-of "  3.14 is pi"))))

(defparameter +bash-dialog+
  '(" Bash command"
    ""
    "   python3.13 -m pytest tests/ -q"
    "   Run the test suite after the store change"
    ""
    " Do you want to proceed?"
    " ❯ 1. Yes"
    "   2. Yes, and don't ask again for python3.13 -m pytest commands in ~/x"
    "   3. No, and tell Claude what to do differently (esc)"
    ""
    " Esc to cancel · Tab to amend · ctrl+e to explain"))

(test what-a-bash-permission-dialog-asks-is-read-off-it
  (let ((asks (agent:asks-of-lines +bash-dialog+)))
    (is (equal "Bash command" (getf asks :subject)))
    (is (equal '("python3.13 -m pytest tests/ -q" "Run the test suite after the store change")
               (getf asks :detail)))
    (is (equal "Do you want to proceed?" (getf asks :question)))
    (is (equal '(1 2 3) (mapcar #'first (getf asks :options))))
    (is (equal "Yes" (second (first (getf asks :options)))))
    (is (eql 1 (getf asks :chosen)))))

(test what-a-fetch-a-trust-and-an-mcp-dialog-ask-is-read-the-same-way
  (let ((fetch (agent:asks-of-lines
                '(" Fetch" "" "   Claude wants to fetch content from docs.python.org" ""
                  " Do you want to allow Claude to fetch this content?"
                  " ❯ 1. Yes" "   2. Yes, and don't ask again for docs.python.org"
                  "   3. No, and tell Claude what to do differently (esc)")))
        (trust (agent:asks-of-lines
                '(" Accessing workspace:" "" " /Users/you/git/new-thing" ""
                  " Do you trust the files in this folder?" ""
                  " ❯ 1. Yes, proceed" "   2. No, exit" ""
                  " Enter to confirm · Esc to cancel")))
        (mcp (agent:asks-of-lines
              '(" MCP server \"docs\" requests your input" "" " Which index should be searched?"
                " ❯ 1. Accept" "   2. Decline" "   3. Cancel"))))
    (is (equal "Fetch" (getf fetch :subject)))
    (is (eql 3 (length (getf fetch :options))))
    (is (equal "Do you trust the files in this folder?" (getf trust :question)))
    (is (equal '((1 "Yes, proceed") (2 "No, exit")) (getf trust :options)))
    (is (equal "Which index should be searched?" (getf mcp :question)))
    (is (equal "Accept" (second (first (getf mcp :options)))))))

(test a-tip-above-the-command-is-not-what-is-asked-about
  ;; as Claude Code 2.1 draws a bash dialog when auto mode is not on
  (let ((asks (agent:asks-of-lines
               '(" Bash command" ""
                 "   Tip: auto mode handles these prompts for you — choose \"switch to auto mode\""
                 "   below" ""
                 "   python3 -c 'print(6*7)'"
                 "   Run Python to print 6 times 7" ""
                 " This command requires approval" ""
                 " Do you want to proceed?"
                 " ❯ 1. Yes"
                 "   2. Yes, and don't ask again for: python3 *"
                 "   3. Yes, and switch to auto mode"
                 "   4. No"))))
    (is (equal "python3 -c 'print(6*7)'" (first (getf asks :detail)))
        "the tip was taken for the command: ~S" (getf asks :detail))
    (is (eql 4 (length (getf asks :options))))))

(test lines-with-no-question-ask-nothing
  (is (null (agent:asks-of-lines '("❯ " "  ? for shortcuts"))))
  (is (null (agent:asks-of-lines '(" 1. first step" " 2. second step"))))
  (is (null (agent:asks-of-lines nil))))

(test a-blocked-claude-code-says-what-it-asks-and-an-idle-one-says-nothing
  (let* ((a (agent:make-agent "✳ Claude Code" ""))
         (term (apply #'claude-screen "⏺ Bash(python3.13 -m pytest tests/ -q)"
                      "────────────────────────────────" +bash-dialog+)))
    (agent:agent-look a term 0 t)
    (is (eq :blocked (agent:agent-state a)))
    (let ((asks (agent:agent-asks a term)))
      (is (equal "Bash command" (getf asks :subject)) "~S" asks)
      (is (eql 3 (length (getf asks :options)))))
    (over-again term "─" "❯ " "─")
    (agent:agent-look a term 100 t)
    (is (eq :idle (agent:agent-state a)))
    (is (null (agent:agent-asks a term)))))

(test a-pane-is-called-by-what-it-holds
  (let ((pane (mux:make-pane "/bin/zsh")))
    (is (equal "shell" (mux::pane-kind pane)))
    (setf (mux::pane-programs pane) '("-zsh"))
    (is (equal "shell" (mux::pane-kind pane)) "a login shell was not a shell")
    (setf (mux::pane-programs pane) '("make FOREIGN=1 test"))
    (is (equal "make" (mux::pane-kind pane)))
    (setf (mux::pane-programs pane) '("/opt/homebrew/bin/atty agent wait todo:2 idle"))
    (is (equal "atty" (mux::pane-kind pane)))
    (agent:agent-become (mux:pane-agent pane) "✳ Claude Code" "")
    (is (equal "claude-code" (mux::pane-kind pane)))))
