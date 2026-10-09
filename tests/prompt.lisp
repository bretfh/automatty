(in-package #:atty/test)

(def-suite prompt :in all)
(in-suite prompt)

(test what-was-typed-narrows-what-is-offered
  (let ((p (mux:make-prompt "run" '("detach" "redraw" "rename" "bar off"))))
    (is (equal '("detach" "redraw" "rename" "bar off") (mux:prompt-showing p)))
    (setf (mux:prompt-query p) "re")
    (is (equal '("redraw" "rename") (mux:prompt-showing p)))
    (setf (mux:prompt-query p) "bo")
    (is (equal '("bar off") (mux:prompt-showing p))
        "letters at the start of two words did not find the one that has them")
    (setf (mux:prompt-query p) "zzz")
    (is (null (mux:prompt-showing p)))))

(test what-starts-a-word-beats-what-is-buried-in-one
  (is (> (mux:score "dt" "detach") (mux:score "dt" "wandered-into"))
      "a match across word starts did not win")
  (is (null (mux:score "qqq" "detach")))
  (is (> (mux:score "det" "detach") (mux:score "det" "a-detached-thing"))
      "a shorter answer that matched as well did not win"))

(defun pressing (p &rest chords)
  "Put P up on a watcher and press the chords at it, as the loop would."
  (let ((watcher (mux::%make-watcher)))
    (mux:push-overlay watcher p)
    (dolist (chord chords p)
      (let ((mux:*client* watcher)
            (atty/mode:*pending* nil)
            (atty/mode:*unbound* (lambda (c) (mux:overlay-unbound-key p c watcher))))
        (atty/mode:press chord (atty/mode:mode-named (mux:watcher-mode watcher)))))))

(test moving-through-what-is-offered-stops-at-the-ends
  (let ((p (mux:make-prompt "run" '("one" "two" "three"))))
    (pressing p "Down" "Down")
    (is (eql 2 (mux:prompt-index p)))
    (pressing p "Down" "Down")
    (is (eql 2 (mux:prompt-index p)) "it ran off the bottom")
    (pressing p "Up" "Up" "Up" "Up")
    (is (eql 0 (mux:prompt-index p)) "it ran off the top")))

(test typing-puts-the-choice-back-at-the-top
  (let ((p (mux:make-prompt "run" '("detach" "redraw" "rename"))))
    (pressing p "Down")
    (is (eql 1 (mux:prompt-index p)))
    (pressing p "r")
    (is (eql 0 (mux:prompt-index p)) "the choice stayed where it was")
    (is (equal "r" (mux:prompt-query p)))
    (pressing p "e" "DEL")
    (is (equal "r" (mux:prompt-query p)))))

(test the-prompt-draws-itself-under-the-bar
  (let ((screen (tty:make-screen :width 30 :height 10))
        (p (mux:make-prompt "run" '("detach" "redraw" "rename")))
        (mux:*client* (mux::%make-watcher :view (mux:make-view :rows 10 :cols 30))))
    (setf (mux:prompt-query p) "re")
    (mux:draw-overlay p screen)
    (let ((rows (loop :for y :below 10
                      :collect (string-right-trim " " (shown screen y)))))
      (is (find-if (lambda (r) (search "run" r)) rows) "no title: ~S" rows)
      (is (find-if (lambda (r) (search "▶ redraw" r)) rows)
          "the chosen one is not marked: ~S" rows)
      (is (find-if (lambda (r) (search "rename" r)) rows))
      (is (null (find-if (lambda (r) (search "detach" r)) rows))
          "something that does not match was drawn"))))

(test the-prompt-covers-only-what-it-needs
  (let ((screen (tty:make-screen :width 30 :height 10))
        (p (mux:make-prompt "run" '("one")))
        (mux:*client* (mux::%make-watcher :view (mux:make-view :rows 10 :cols 30))))
    (dotimes (x 30)
      (setf (term:row-char (tty:screen-row screen 0) x) #\x))
    (mux:draw-overlay p screen)
    (is (equal (make-string 30 :initial-element #\x) (shown screen 0))
        "the prompt drew over the top of the screen")))

(defun funcall-safely (f)
  (handler-case (funcall f) (error () :broke)))

(test a-command-can-be-named-and-run
  (let ((ran nil))
    (mux:defcommand a-test-command () (setf ran t))
    (unwind-protect
         (progn
           (is (member "a test command" (mux:command-names) :test #'equal)
               "the dashes in the name did not become spaces")
           (mux:run-command "a test command" nil)
           (is-true ran))
      (remhash "a test command" mux:*commands*))))

(test what-is-on-top-says-which-mode-the-client-is-in
  (let ((watcher (mux::%make-watcher))
        (p (mux:make-prompt "run" '("one" "two"))))
    (is (eq 'mux:pane-mode (mux:watcher-mode watcher)))
    (mux:push-overlay watcher p)
    (is (eq 'mux::prompt-mode (mux:watcher-mode watcher))
        "the prompt did not put the watcher in its own mode")
    (mux:push-overlay watcher (mux:make-note "hm" '("a line")))
    (is (eq 'mux::note-mode (mux:watcher-mode watcher)))
    (mux:pop-overlay watcher (first (mux:watcher-overlays watcher)))
    (is (eq 'mux::prompt-mode (mux:watcher-mode watcher))
        "dropping the note did not go back to the prompt underneath")
    (mux:pop-overlay watcher p)
    (is (eq 'mux:pane-mode (mux:watcher-mode watcher)))))

(test a-pane-chord-does-not-fire-while-a-prompt-is-up
  (let ((pane (atty/mode:mode-named 'mux:pane-mode))
        (prompt (atty/mode:mode-named 'mux::prompt-mode)))
    (is (atty/mode:lookup-key "C-b d" pane))
    (is (null (atty/mode:lookup-key "C-b d" prompt))
        "the prompt can still detach, so a pane binding is reaching it")
    (is (atty/mode:lookup-key "RET" prompt))
    (is (null (atty/mode:lookup-key "RET" pane))
        "return is bound in the pane, where it should reach the program")))

(test a-key-nobody-bound-is-what-was-typed
  (let ((p (mux:make-prompt "run" '("detach" "redraw"))))
    (pressing p "r" "e" "d")
    (is (equal "red" (mux:prompt-query p))
        "letters did not reach the query through the unbound hook")
    (pressing p "SPC")
    (is (equal "red " (mux:prompt-query p)) "space did not insert")
    (pressing p "C-b")
    (is (equal "red " (mux:prompt-query p))
        "a key with a modifier inserted itself")))

(test a-sequence-that-is-no-key-is-passed-over
  (let ((watcher (mux::%make-watcher))
        (p (mux:make-prompt "run" '("one" "two" "three"))))
    (mux:push-overlay watcher p)
    (mux::handle-key watcher (csi "<0;12;34M"))
    (is (equal "" (mux:prompt-query p))
        "a mouse report was read as something typed")
    (is (eql 0 (mux:prompt-index p)))
    (mux::handle-key watcher (csi "B"))
    (is (eql 1 (mux:prompt-index p))
        "the Down after it did not arrive")))

(test a-sequence-split-across-two-reads-is-still-one-key
  (let ((watcher (mux::%make-watcher))
        (p (mux:make-prompt "run" '("one" "two" "three"))))
    (mux:push-overlay watcher p)
    (mux::handle-key watcher (esc "["))
    (is (equal "" (mux:prompt-query p)) "half a sequence was read as text")
    (is (eql 0 (mux:prompt-index p)))
    (mux::handle-key watcher "B")
    (is (eql 1 (mux:prompt-index p))
        "the two halves did not make one Down")))

(test moving-in-the-prompt-asks-for-the-screen-again
  (let ((watcher (mux::%make-watcher))
        (p (mux:make-prompt "run" '("one" "two" "three"))))
    (mux:push-overlay watcher p)
    (setf (mux:watcher-behind watcher) nil)
    (mux::handle-key watcher (csi "B"))
    (is (eql 1 (mux:prompt-index p)))
    (is-true (mux:watcher-behind watcher)
             "the prompt moved and nothing was redrawn")))

(test what-only-works-inside-a-prompt-is-not-offered-by-one
  (is (member "detach" (mux:command-names) :test #'equal))
  (is (null (member "prompt next" (mux:command-names) :test #'equal))
      "the palette offers a command that closes the palette before it runs")
  (is (member "prompt next" (mux:command-names nil) :test #'equal)
      "it is not a command at all any more")
  (is-true (gethash "prompt next" mux:*commands*)
           "it can no longer be bound to a key"))
