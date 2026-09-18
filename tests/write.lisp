(in-package #:vt/test)

(def-suite write :in all)
(in-suite write)

(test what-was-written-is-where-the-cursor-was
  (let ((term (a-term :width 20 :height 3)))
    (say term "hello")
    (is (equal "hello" (row term 0)))
    (is (equal '(5 0) (cursor term)))))

(test a-line-that-runs-out-of-width-carries-on-below
  (let ((term (a-term :width 5 :height 3)))
    (say term "abcdefgh")
    (is (equal "abcde" (row term 0)))
    (is (equal "fgh" (row term 1)))
    (is (equal '(3 1) (cursor term)))))

(test with-no-auto-margin-the-last-column-is-written-over
  (let ((term (a-term :width 5 :height 3)))
    (say term (csi "?7l") "abcdefgh")
    (is (equal "abcdh" (row term 0)))
    (is (equal "" (row term 1)))))

(test a-wide-character-takes-two-columns
  (let ((term (a-term :width 10 :height 2)))
    (say term "a漢b")
    (is (eql #\a (at term 0 0)))
    (is (eql #\漢 (at term 1 0)))
    (is (eql #\Space (at term 2 0)))
    (is (eql #\b (at term 3 0)))
    (is (equal '(4 0) (cursor term)))))

(test a-wide-character-will-not-be-split-across-the-margin
  (let ((term (a-term :width 4 :height 3)))
    (say term "abc漢")
    (is (eql #\Space (at term 3 0)))
    (is (eql #\漢 (at term 0 1)))
    (is (equal '(2 1) (cursor term)))))

(test a-combining-mark-takes-no-column
  (let ((term (a-term :width 10 :height 2)))
    (say term (format nil "e~Ax" (code-char #x301)))
    (is (eql #\e (at term 0 0)))
    (is (eql #\x (at term 1 0)))))

(test insert-mode-pushes-what-is-there-rightwards
  (let ((term (a-term :width 10 :height 2)))
    (say term "abcd" (csi "H") (csi "4h") "XY")
    (is (equal "XYabcd" (row term 0)))))

(test the-line-drawing-charset-is-a-charset-not-a-font
  (let ((term (a-term :width 10 :height 2)))
    (say term (esc "(0") "qx" (esc "(B") "q")
    (is (eql #\─ (at term 0 0)))
    (is (eql #\│ (at term 1 0)))
    (is (eql #\q (at term 2 0)))))

(test shift-out-draws-from-g1
  (let ((term (a-term :width 10 :height 2)))
    (say term (esc ")0") (format nil "~C" #\So) "q"
         (format nil "~C" #\Si) "q")
    (is (eql #\─ (at term 0 0)))
    (is (eql #\q (at term 1 0)))))

(test a-tab-lands-on-the-next-stop
  (let ((term (a-term :width 40 :height 2)))
    (say term (format nil "a~Cb~Cc" #\Tab #\Tab))
    (is (eql #\a (at term 0 0)))
    (is (eql #\b (at term 8 0)))
    (is (eql #\c (at term 16 0)))))

(test a-return-with-no-line-feed-writes-over-what-was-said
  (let ((term (a-term :width 10 :height 2)))
    (say term "abcdef" (format nil "~C" #\Return) "XY")
    (is (equal "XYcdef" (row term 0)))))

(test output-split-anywhere-is-the-same-output
  (let ((whole (a-term :width 20 :height 3))
        (piecemeal (a-term :width 20 :height 3))
        (said (format nil "one~C[31mtwo~C[0m~C[2;3Hthree"
                      #\Escape #\Escape #\Escape)))
    (say whole said)
    (loop :for i :below (length said)
          :do (say piecemeal (string (char said i))))
    (is (equal (rows whole) (rows piecemeal)))
    (is (equal (cursor whole) (cursor piecemeal)))))

(test the-cursor-waits-on-the-last-column-rather-than-past-it
  (let ((term (a-term :width 8 :height 2)))
    (say term "abcdefgh")
    (is (equal '(7 0) (cursor term)) "the cursor went past the last column")
    (is (vt:term-wrap-pending term))
    (is (equal "abcdefgh" (row term 0)))
    (say term "i")
    (is (equal '(1 1) (cursor term)) "the next character did not wrap")
    (is (equal "i" (row term 1)))
    (is (null (vt:term-wrap-pending term)))))

(test a-backspace-at-the-right-margin-moves-one-column
  (let ((term (a-term :width 8 :height 2)))
    (say term "abcdefgh")
    (vt:term-cursor-left term 1)
    (is (equal '(6 0) (cursor term))
        "backspacing from the last column landed on the column it was already on")
    (say term "X")
    (is (equal "abcdefXh" (row term 0)))))

(test erasing-from-the-right-margin-erases-the-last-column
  (let ((term (a-term :width 8 :height 2)))
    (say term "abcdefgh" (csi "K"))
    (is (equal "abcdefg" (row term 0))
        "the last column survived an erase that should have taken it")))

(test a-wide-character-that-does-not-fit-waits-rather-than-splitting
  (let ((term (a-term :width 4 :height 2)))
    (say term "abc" (format nil "~C" (code-char #x6F22)))
    (is (equal "abc" (row term 0)))
    (is (eql (code-char #x6F22) (at term 0 1)))
    (is (equal '(2 1) (cursor term)))))

(test nothing-wraps-when-the-margin-is-off
  (let ((term (a-term :width 4 :height 2)))
    (say term (csi "?7l") "abcd")
    (is (equal '(3 0) (cursor term)))
    (say term "XY")
    (is (equal "abcY" (row term 0)) "it wrapped with the margin off")
    (is (equal "" (row term 1)))))
