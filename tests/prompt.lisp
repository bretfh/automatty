(in-package #:vt/test)

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

(defun pressing (p &rest keys)
  (dolist (key keys p)
    (mux:press p key (mux::%make-client))))

(test moving-through-what-is-offered-stops-at-the-ends
  (let ((p (mux:make-prompt "run" '("one" "two" "three"))))
    (pressing p '(:down) '(:down))
    (is (eql 2 (mux:prompt-index p)))
    (pressing p '(:down) '(:down))
    (is (eql 2 (mux:prompt-index p)) "it ran off the bottom")
    (pressing p '(:up) '(:up) '(:up) '(:up))
    (is (eql 0 (mux:prompt-index p)) "it ran off the top")))

(test typing-puts-the-choice-back-at-the-top
  (let ((p (mux:make-prompt "run" '("detach" "redraw" "rename"))))
    (pressing p '(:down))
    (is (eql 1 (mux:prompt-index p)))
    (pressing p #\r)
    (is (eql 0 (mux:prompt-index p)) "the choice stayed where it was")
    (is (equal "r" (mux:prompt-query p)))
    (pressing p #\e '(:backspace))
    (is (equal "r" (mux:prompt-query p)))))

(test the-prompt-draws-itself-at-the-foot-and-says-what-was-typed
  (let ((screen (mux:make-screen :width 30 :height 10))
        (p (mux:make-prompt "run" '("detach" "redraw" "rename"))))
    (setf (mux:prompt-query p) "re")
    (mux:draw-over p screen)
    (let ((rows (loop :for y :below 10
                      :collect (string-right-trim " " (shown screen y)))))
      (is (find-if (lambda (r) (search "run" r)) rows) "no title: ~S" rows)
      (is (find-if (lambda (r) (search "re" r)) rows) "what was typed is not shown")
      (is (find-if (lambda (r) (search "> redraw" r)) rows)
          "the chosen one is not marked: ~S" rows)
      (is (find-if (lambda (r) (search "rename" r)) rows))
      (is (null (find-if (lambda (r) (search "detach" r)) rows))
          "something that does not match was drawn"))
    (is (>= (mux:screen-cursor-y screen) 7)
        "the prompt did not take the foot of the screen")))

(test the-prompt-covers-only-what-it-needs
  (let ((screen (mux:make-screen :width 30 :height 10))
        (p (mux:make-prompt "run" '("one"))))
    (dotimes (x 30)
      (setf (vt:cell-char (aref (mux:screen-row screen 0) x)) #\x))
    (mux:draw-over p screen)
    (is (equal (make-string 30 :initial-element #\x) (shown screen 0))
        "the prompt drew over the top of the screen")))

(test a-command-can-be-named-and-run
  (let ((ran nil))
    (mux:defcommand "a test command" (client)
      (declare (ignore client))
      (setf ran t))
    (is (member "a test command" (mux:command-names) :test #'equal))
    (mux:run-command "a test command" nil)
    (is-true ran)
    (remhash "a test command" mux:*commands*)))

(test a-key-is-bound-to-a-name-and-the-name-to-a-thing-to-do
  (is (equal "detach" (mux:bound #\d)))
  (is (equal "run a command" (mux:bound #\:)))
  (is (null (mux:bound #\Q)))
  (let ((was (mux:bound #\z)))
    (unwind-protect
         (progn (mux:bind #\z "redraw")
                (is (equal "redraw" (mux:bound #\z))))
      (if was (mux:bind #\z was) (remhash #\z mux:*keys*)))))

(test a-command-that-comes-apart-is-shown-rather-than-fatal
  (let ((client (mux::%make-client)))
    (mux:defcommand "a command that breaks" (client)
      (declare (ignore client))
      (error "on purpose"))
    (unwind-protect
         (progn
           (finishes (mux:run-command "a command that breaks" client))
           (let ((over (first (mux:client-over client))))
             (is (typep over 'mux:note) "nothing was put up to say what happened")
             (is (search "came apart" (mux::note-title over)))
             (is (find-if (lambda (l) (search "on purpose" l)) (mux::note-lines over))
                 "the note does not say what the trouble was")
             (is (> (length (mux::note-lines over)) 2)
                 "no backtrace came with it")))
      (remhash "a command that breaks" mux:*commands*))))

(test a-note-draws-at-the-foot-and-any-key-puts-it-away
  (let ((screen (mux:make-screen :width 30 :height 10))
        (client (mux::%make-client))
        (note (mux:make-note "trouble" '("the first line" "the second line"))))
    (mux:draw-over note screen)
    (let ((rows (loop :for y :below 10
                      :collect (string-right-trim " " (shown screen y)))))
      (is (find-if (lambda (r) (search "trouble" r)) rows))
      (is (find-if (lambda (r) (search "the second line" r)) rows)))
    (is (null (mux:screen-cursor-visible screen))
        "a note left the cursor showing in the middle of itself")
    (mux:client-over-put client note)
    (mux:press note #\x client)
    (is (null (mux:client-over client)) "a key did not put the note away")))

(test a-long-line-in-a-note-is-cut-rather-than-wrapped
  (let ((screen (mux:make-screen :width 20 :height 6))
        (note (mux:make-note "long" (list (make-string 200 :initial-element #\z)))))
    (mux:draw-over note screen)
    (dotimes (y 6)
      (is (<= (length (string-right-trim " " (shown screen y))) 20)
          "row ~D ran past the edge" y))))
