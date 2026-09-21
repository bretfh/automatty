(in-package #:atty/test)

(def-suite agent :in all)
(in-suite agent)

(defparameter +probe+
  '(probe :programs ("probe")
          :version (:exec-path "probe/([0-9.]+)/")
          :versions ("1.0")
          :scenario ((:idle) (:asking :do (:submit "go")))
          :screens ((:asking :widget :choice :question "(?i)proceed\\?" :means :blocked
                     :choose :digits
                     :actions (:approve (:option "^Yes$") :deny (:option "^No$")))
                    (:working :widget :spinner :means :working)
                    (:idle :widget :prompt-input :means :idle))))

(defmacro with-probe (&body body)
  `(let ((agent:*readers* nil)
         (agent::*unrecognized-written* agent::+unrecognized-kept+))
     (agent:register-reader +probe+)
     ,@body))

(defun screen-of (lines &key (width 60) (height 12))
  (let ((term (a-term :width width :height height)))
    (dolist (line lines term)
      (say term (format nil "~A~C~C" line #\Return #\Newline)))))

(defun redrawn (term lines)
  (say term (csi "2J") (csi "H"))
  (dolist (line lines term)
    (say term (format nil "~A~C~C" line #\Return #\Newline))))

(defparameter +probe-idle+ '("probe 1.0" "" "────────────" "> " "────────────" "  ? for help"))
(defparameter +probe-working+ '("* thinking… (3s)" "────────────" "> " "────────────"))
(defparameter +probe-asking+ '("────────────" " Proceed?" " > 1. Yes" "   2. No" " esc to cancel"))

(defun probe-agent ()
  (agent:make-agent :programs '("probe") :paths '("/opt/probe/1.0/bin/probe")))

(test a-program-nobody-knows-is-working-while-it-prints-and-idle-once-quiet
  (with-probe
    (let ((a (agent:make-agent :command "cat"))
          (term (a-term :width 40 :height 5)))
      (is (equal "agent" (agent:agent-kind a)))
      (is-true (agent:agent-look a term 0 t))
      (is (eq :working (agent:agent-state a)))
      (is-false (agent:agent-look a term 100 nil))
      (is-false (agent:agent-look a term 400 nil))
      (is (eq :working (agent:agent-state a)) "it went idle before the hold was up")
      (is-true (agent:agent-look a term (+ 100 agent:+hold+) nil))
      (is (eq :idle (agent:agent-state a))))))

(test a-program-that-was-never-working-is-idle-at-once
  (with-probe
    (let ((a (agent:make-agent :command "cat")))
      (is-true (agent:agent-look a (a-term :width 40 :height 5) 0 nil))
      (is (eq :idle (agent:agent-state a))))))

(test what-a-hook-says-is-published-at-once-and-stands-until-the-screen-moves
  (with-probe
    (let ((a (agent:make-agent :command "cat"))
          (term (a-term :width 40 :height 5)))
      (agent:agent-hear a :blocked)
      (is-true (agent:agent-look a term 0 nil))
      (is (eq :blocked (agent:agent-state a)))
      (agent:agent-look a term 5000 nil)
      (is (eq :blocked (agent:agent-state a)) "the screen stood still and the hook's word was dropped")
      (is-true (agent:agent-look a term 6000 t))
      (is (null (agent:agent-heard a)))
      (is (eq :working (agent:agent-state a))))))

(test what-the-screen-shows-beats-what-a-hook-says
  (with-probe
    (let ((a (probe-agent))
          (term (screen-of +probe-idle+)))
      (agent:agent-hear a :blocked)
      (agent:agent-look a term 0 t)
      (is (eq :idle (agent:agent-state a)) "a hook's word overrode a prompt box on screen"))))

(test a-hook-fills-in-where-a-known-agents-screen-says-nothing
  (with-probe
    (let ((a (probe-agent))
          (term (screen-of '("nothing the reader knows"))))
      (agent:agent-look a term 0 t)
      (is (eq :unknown (agent:agent-state a)))
      (agent:agent-hear a :working)
      (agent:agent-look a term 10 nil)
      (is (eq :working (agent:agent-state a))))))

(test a-screen-nothing-has-touched-is-not-read-again-whatever-it-says
  (with-probe
    (let ((a (probe-agent))
          (term (screen-of +probe-asking+)))
      (agent:agent-look a term 0 t)
      (is (eq :blocked (agent:agent-state a)))
      (redrawn term +probe-idle+)
      (agent:agent-look a term 100 nil)
      (is (eq :blocked (agent:agent-state a)) "a screen nothing had touched was read again")
      (agent:agent-look a term 200 t)
      (is (eq :idle (agent:agent-state a))))))

(test the-last-lines-reach-back-into-what-scrolled-off
  (let ((term (a-term :width 20 :height 3)))
    (say term (format nil "a~C~Cb~C~Cc~C~Cd~C~Ce~C~C"
                      #\Return #\Newline #\Return #\Newline #\Return #\Newline
                      #\Return #\Newline #\Return #\Newline))
    (is (equal '("d" "e") (agent:last-lines term 2)))
    (is (equal '("b" "c" "d" "e") (agent:last-lines term 4)))
    (is (equal '("a" "b" "c" "d" "e") (agent:last-lines term 40)))))

(test a-prompt-box-is-what-sits-between-the-last-two-rules
  (let ((term (screen-of '("top" "────" "❯ hello" "────" "foot"))))
    (is (equal "hello" (getf (agent:prompt-input term (agent:screen-lines term)) :text))))
  (let ((term (screen-of '("top" "────" "just text" "────" "foot"))))
    (is (null (agent:prompt-input term (agent:screen-lines term))))))

(test a-program-is-known-by-what-is-running-not-by-what-started-it
  (with-probe
    (is (equal "agent" (agent:agent-kind (agent:make-agent :command "cd /x && exec probe"))))
    (is (equal "probe" (agent:agent-kind (agent:make-agent :command "cd /x && exec probe"
                                                           :programs '("probe --fast")))))
    (is (equal "probe" (agent:agent-kind (agent:make-agent :command "sh"
                                                           :programs '("sh -c cd /x && probe" "probe")))))
    (is (equal "agent" (agent:agent-kind (agent:make-agent :programs '("prober")))))))

(test a-title-change-does-not-lose-what-is-still-running
  (with-probe
    (let ((a (agent:make-agent :programs '("probe"))))
      (agent:agent-become a :title "busy with something" :programs '("probe"))
      (is (equal "probe" (agent:agent-kind a)))
      (agent:agent-become a :title "zsh" :programs '("-zsh"))
      (is (equal "agent" (agent:agent-kind a)) "it outlived the program it was named for"))))

(test the-reader-for-the-exact-version-is-verified-and-any-other-is-the-nearest
  (with-probe
    (agent:register-reader '(probe :from "1.0" :versions ("2.0")))
    (let ((exact (agent:make-agent :programs '("probe") :paths '("/opt/probe/2.0/bin/probe")))
          (between (agent:make-agent :programs '("probe") :paths '("/opt/probe/1.5/bin/probe")))
          (older (agent:make-agent :programs '("probe") :paths '("/opt/probe/0.9/bin/probe"))))
      (is-true (agent:agent-verified exact))
      (is (equal "2.0" (agent:agent-version exact)))
      (is-false (agent:agent-verified between))
      (is (equal '("1.0") (agent::reader-versions (agent::agent-reader between))))
      (is-false (agent:agent-verified older))
      (is (equal '("1.0") (agent::reader-versions (agent::agent-reader older)))))))

(test a-reader-that-inherits-replaces-only-what-it-redefines
  (with-probe
    (let ((child (agent:register-reader
                  '(probe :from "1.0" :versions ("1.1")
                          :screens ((:working :widget :footer :match "busy" :means :working))))))
      (is (equal '(:asking :working :idle)
                 (mapcar #'agent::entry-id (agent::reader-screens child))))
      (is (eq :footer (agent::entry-widget (second (agent::reader-screens child)))))
      (is (equal '("probe") (agent::reader-programs child))))))

(test blocked-says-what-it-is-asking-and-what-it-offers
  (with-probe
    (let ((a (probe-agent))
          (term (screen-of +probe-asking+)))
      (agent:agent-look a term 0 t)
      (is (eq :blocked (agent:agent-state a)))
      (let ((reason (agent:agent-reason a)))
        (is (equal "Proceed?" (getf reason :question)))
        (is (equal '("Yes" "No") (getf reason :options)))
        (is (eql 0 (getf reason :selected)))))))

(test answers-are-keys-the-reader-knows-for-that-screen
  (with-probe
    (let* ((reader (agent:reader-named "probe"))
           (asking (agent:observe reader (screen-of +probe-asking+)))
           (idle (agent:observe reader (screen-of +probe-idle+)))
           (working (agent:observe reader (screen-of +probe-working+))))
      (is (equal '("1") (agent:action-keys reader asking :approve)))
      (is (equal '("2") (agent:action-keys reader asking :deny)))
      (is (equal '("2") (agent:action-keys reader asking :choose 2)))
      (is (null (agent:action-keys reader asking :choose 3)))
      (is (null (agent:action-keys reader asking :submit "hi")))
      (is (equal (list "hi" (string #\Return)) (agent:action-keys reader idle :submit "hi")))
      (is (null (agent:action-keys reader idle :approve)))
      (is (equal (list (string #\Escape)) (agent:action-keys reader working :interrupt))))))

(test a-choice-without-digits-is-reached-by-moving-to-it
  (with-probe
    (let* ((reader (agent:register-reader
                    '(probe :from "1.0" :versions ("1.2")
                            :screens ((:asking :widget :choice :question "(?i)proceed\\?"
                                       :means :blocked
                                       :actions (:approve (:option "^Yes$") :deny (:option "^No$")))))))
           (asking (agent:observe reader (screen-of +probe-asking+))))
      (is (equal (list (format nil "~C[B~C" #\Escape #\Return)) (agent:action-keys reader asking :deny)))
      (is (equal (list (string #\Return)) (agent:action-keys reader asking :approve))))))

(test a-prompt-is-working-until-its-turn-has-begun-and-ended
  (with-probe
    (let ((a (probe-agent))
          (term (screen-of +probe-idle+)))
      (agent:agent-look a term 0 t)
      (is (eq :idle (agent:agent-state a)))
      (agent:agent-prompted a 100)
      (is (equal '((:turn 1 :began)) (agent:agent-take-events a)))
      (agent:agent-look a term 200 nil)
      (is (eq :working (agent:agent-state a)) "the prompt was taken for the turn being over")
      (redrawn term +probe-working+)
      (agent:agent-look a term 2000 t)
      (is (eq :working (agent:agent-state a)))
      (is (null (agent:agent-prompted-at a)) "the turn began and it was still held")
      (redrawn term +probe-idle+)
      (agent:agent-look a term 3000 t)
      (is (eq :idle (agent:agent-state a)))
      (is (null (agent:agent-take-events a)) "it ended the turn before idle had held")
      (agent:agent-look a term (+ 3000 agent:+hold+) nil)
      (is (equal '((:turn 1 :ended)) (agent:agent-take-events a))))))

(test a-turn-somebody-starts-by-typing-is-a-turn-too
  (with-probe
    (let ((a (probe-agent))
          (term (screen-of +probe-idle+)))
      (agent:agent-look a term 0 t)
      (redrawn term +probe-working+)
      (agent:agent-look a term 100 t)
      (is (equal '((:turn 1 :began)) (agent:agent-take-events a)))
      (is (eql 1 (agent:agent-turn a))))))

(test a-prompt-that-never-starts-a-turn-is-let-go-after-a-while
  (with-probe
    (let ((a (probe-agent))
          (term (screen-of +probe-idle+)))
      (agent:agent-look a term 0 t)
      (agent:agent-prompted a 0)
      (agent:agent-look a term 10 nil)
      (is (eq :working (agent:agent-state a)))
      (agent:agent-look a term (1+ agent:+turn-patience+) nil)
      (is (eq :idle (agent:agent-state a))))))

(test a-screen-the-reader-cannot-read-is-unknown-once-it-is-still
  (with-probe
    (let ((a (probe-agent))
          (term (screen-of '("something the reader has never seen"))))
      (agent:agent-look a term 0 t)
      (is (eq :unknown (agent:agent-state a)))
      (is (null (agent:agent-reason a)) "it gave up before the screen had settled")
      (agent:agent-look a term (+ 100 agent::+still+) nil)
      (is (eq :unknown (agent:agent-state a)))
      (is (eq :unrecognized (first (agent:agent-reason a))))
      (is (equal "1.0" (getf (rest (agent:agent-reason a)) :version))))))

(test a-dialog-is-blocking-whatever-the-pane-is-taken-for
  (with-probe
    (is-true (agent:screen-blocked-p (screen-of +probe-asking+)))
    (is-false (agent:screen-blocked-p (screen-of +probe-idle+)))))

(test a-reader-read-from-text-is-data-and-nothing-in-it-runs
  (with-probe
    (is (typep (with-input-from-string
                   (s "(other :programs (\"other\") :versions (\"1\") :screens ((:idle :widget :prompt-input :means :idle)))")
                 (agent:register-reader (agent:read-reader s)))
               'agent:reader))
    (signals reader-error
      (with-input-from-string (s "(other :programs #.(error \"ran\") :screens ())")
        (agent:read-reader s)))))

(test a-reader-that-is-not-one-says-why
  (with-probe
    (dolist (form '((other :programs ("p") :screens ((:x :widget :telepathy :means :idle)))
                    (other :programs ("p") :screens ((:x :widget :spinner :means :sleepy)))
                    (other :programs ("p") :screens ((:x :widget :footer :means :idle)))
                    (other :programs ("p") :screens ((:x :widget :choice :question "(" :means :blocked)))
                    (other :programs "p" :screens ((:x :widget :spinner :means :working)))
                    (other :programs ("p") :screens ((:x :widget :choice :means :blocked
                                                       :actions (:approve (:run "rm")))))
                    (other :programs ("p") :colour :red :screens ((:x :widget :spinner :means :working)))
                    (other :programs ("p") :scenario ((:x :do (:dance))) :screens ((:x :widget :spinner :means :working)))
                    (other :from "9.9" :versions ("10.0"))
                    (other :programs ("p"))))
      (signals agent:bad-reader (agent:register-reader form)))
    (is (null (agent:reader-named "other")))))

(test keys-in-a-scenario-are-named-the-way-people-write-them
  (is (equal (string (code-char 15)) (agent:press-keys "C-o")))
  (is (equal (format nil "~C[B~C" #\Escape #\Return) (agent:press-keys "Down RET")))
  (is (equal "x" (agent:press-keys "x"))))

(test a-snapshot-gives-back-the-screen-it-was-taken-of
  (let ((term (a-term :width 30 :height 4)))
    (say term (osc "0;a title") (csi "7m") "chosen" (csi "0m") " plain"
         (format nil "~C~C" #\Return #\Newline) (csi "38;2;10;20;30m") "coloured")
    (let* ((text (with-output-to-string (s) (agent:write-snapshot (agent:snapshot term) s)))
           (back (agent:snapshot-term (with-input-from-string (s text) (agent:read-snapshot s)))))
      (is (equal (term:term-dump-to-string term) (term:term-dump-to-string back)))
      (is (equal "a title" (term:term-title back)))
      (is-true (term:face-inverse (term:row-face (term:term-grid-row back 0) 0)))
      (is (equal '(10 20 30) (term:face-fg (term:row-face (term:term-grid-row back 1) 0)))))))

(test reader-texts-load-in-version-order-and-the-bad-ones-are-refused
  (with-probe
    (multiple-value-bind (loaded refused)
        (agent:register-reader-texts
         (list "(other :from \"1.0\" :versions (\"1.1\"))"
               "(other :programs (\"other\") :versions (\"1.0\") :screens ((:idle :widget :prompt-input :means :idle)))"
               "(broken :programs #.(error \"ran\") :screens ())"
               "(broken :programs (\"b\") :screens ((:x :widget :telepathy :means :idle)))"
               "(unfinished :programs ("))
      (is (equal '("other 1.0" "other 1.1") loaded))
      (is (= 3 (length refused)) "~S" refused)
      (is (null (agent:reader-named "broken"))))))

(test a-choice-inside-a-box-is-read-inside-the-box
  (let* ((term (screen-of (list "a banner line above everything"
                                "and a question? that is not the one asked"
                                " ╭──────────────────────────────╮"
                                " │                              │"
                                " │ Keep going with this?        │"
                                " │                              │"
                                " │ ● 1. Keep going              │"
                                " │   2. Stop here               │"
                                " │                              │"
                                " ╰──────────────────────────────╯")))
         (it (agent:choice term (agent:screen-lines term))))
    (is (equal "Keep going with this?" (getf it :question)))
    (is (equal '("Keep going" "Stop here") (getf it :options)))
    (is (eql 0 (getf it :selected)))
    (is (null (getf it :subject)) "~S" (getf it :subject))
    (is (null (getf it :hint)) "~S" (getf it :hint))))
