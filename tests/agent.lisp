(in-package #:vtx/test)

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
        (term (claude-screen " ▐▛███▛█   Claude Code v2.1.258" "  ▝▝ ▝▝    ~/git/cl/cl-vt")))
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
