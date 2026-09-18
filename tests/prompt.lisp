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

(defun funcall-safely (f)
  (handler-case (funcall f) (error () :broke)))

(test a-command-can-be-named-and-run
  (let ((ran nil))
    (mux:defcommand a-test-command (setf ran t))
    (unwind-protect
         (progn
           (is (member "a test command" (mux:command-names) :test #'equal)
               "the dashes in the name did not become spaces")
           (mux:run-command "a test command" nil)
           (is-true ran))
      (remhash "a test command" mux:*commands*))))
