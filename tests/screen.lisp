(in-package #:vt/test)

(def-suite screen :in all)
(in-suite screen)

(defun put (screen x y char &optional face)
  (let ((row (tty:screen-row screen y)))
    (setf (vt:row-char row x) char
          (vt:row-face row x) face)
    screen))

(test two-screens-the-same-differ-nowhere
  (let ((was (a-screen :width 8 :height 2))
        (now (a-screen :width 8 :height 2)))
    (is (null (tty:screen-diff was now)))))

(test one-cell-that-moved-is-one-run-of-one
  (let ((was (a-screen :width 8 :height 2))
        (now (a-screen :width 8 :height 2)))
    (put now 3 1 #\x)
    (is (equal '((1 3 4)) (laid-out (tty:screen-diff was now))))))

(test a-row-that-all-moved-is-one-run
  (let ((was (a-screen :width 8 :height 1))
        (now (a-screen :width 8 :height 1)))
    (dotimes (x 8) (put now x 0 #\x))
    (is (equal '((0 0 8)) (laid-out (tty:screen-diff was now))))))

(test two-changes-near-each-other-are-one-run-and-far-apart-are-two
  (let ((was (a-screen :width 40 :height 1))
        (now (a-screen :width 40 :height 1)))
    (put now 0 0 #\a) (put now 3 0 #\b)
    (is (equal '((0 0 4)) (laid-out (tty:screen-diff was now)))))
  (let ((was (a-screen :width 40 :height 1))
        (now (a-screen :width 40 :height 1)))
    (put now 0 0 #\a) (put now 30 0 #\b)
    (is (equal '((0 0 1) (0 30 31)) (laid-out (tty:screen-diff was now))))))

(test the-gap-says-how-far-apart-is-far
  (let ((was (a-screen :width 40 :height 1))
        (now (a-screen :width 40 :height 1)))
    (put now 0 0 #\a) (put now 3 0 #\b)
    (is (equal '((0 0 1) (0 3 4)) (laid-out (tty:screen-diff was now 1))))))

(test a-diff-leaves-nothing-to-say-the-second-time
  (let ((was (a-screen :width 8 :height 2))
        (now (a-screen :width 8 :height 2)))
    (put now 3 1 #\x)
    (is (= 1 (length (tty:screen-diff was now))))
    (is (null (tty:screen-diff was now)))))

(test a-run-takes-in-the-left-half-of-a-wide-character
  (let ((was (a-screen :width 8 :height 1))
        (now (a-screen :width 8 :height 1))
        (term (a-term :width 8 :height 1)))
    (say term (format nil "  ~C" (code-char #x6F22)))
    (blit now term)
    (tty:screen-diff was now)
    (put now 3 0 #\x)
    (is (equal '((0 2 4)) (laid-out (tty:screen-diff was now)))
        "the run starts at the character, not at its right half")))

(test a-screen-taken-from-a-term-is-the-term
  (let ((term (a-term :width 8 :height 2))
        (screen (a-screen :width 8 :height 2)))
    (say term (csi "31m") "hi" (csi "0m") (esc "[2;1H") "there")
    (blit screen term)
    (dotimes (y 2)
      (dotimes (x 8)
        (let ((row (vt:term-grid-row term y)))
          (is (char= (char-at screen x y) (vt:row-char row x)))
          (is (vt:face-equal (face-on screen x y) (vt:row-face row x))))))))

(test a-screen-does-not-share-its-cells-with-the-term-it-came-from
  (let ((term (a-term :width 8 :height 1))
        (screen (a-screen :width 8 :height 1)))
    (say term "first")
    (blit screen term)
    (say term (esc "[1;1H") "later")
    (is (equal "first" (shown screen 0))
        "the pane wrote on itself and the screen followed it")))

(test a-pane-smaller-than-the-screen-is-blank-where-it-is-not
  (let ((term (a-term :width 3 :height 1))
        (screen (a-screen :width 8 :height 2)))
    (say term "abc")
    (blit screen term :top 1 :left 2)
    (is (equal "" (shown screen 0)))
    (is (equal "  abc" (shown screen 1)))))

(test a-pane-larger-than-the-screen-is-clipped-not-wrapped
  (let ((term (a-term :width 20 :height 4))
        (screen (a-screen :width 6 :height 2)))
    (say term "abcdefghij" (esc "[4;1H") "zzz")
    (blit screen term)
    (is (equal "abcdef" (shown screen 0)))
    (is (equal "" (shown screen 1)))))
