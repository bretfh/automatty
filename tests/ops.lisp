(in-package #:vt/test)

(def-suite ops :in all)
(in-suite ops)

(test a-cursor-goes-where-it-was-sent-and-no-further
  (let ((term (a-term :width 10 :height 5)))
    (say term (csi "3;4H"))
    (is (equal '(3 2) (cursor term)))
    (say term (csi "99;99H"))
    (is (equal '(9 4) (cursor term)))
    (say term (csi "H"))
    (is (equal '(0 0) (cursor term)))))

(test the-moves-are-clamped-at-every-edge
  (let ((term (a-term :width 10 :height 5)))
    (say term (csi "5A") (csi "5D"))
    (is (equal '(0 0) (cursor term)))
    (say term (csi "99B") (csi "99C"))
    (is (equal '(9 4) (cursor term)))))

(test a-saved-cursor-comes-back-with-its-colour
  (let ((term (a-term :width 10 :height 5)))
    (say term (csi "2;3H") (csi "31m") (esc "7")
         (csi "5;5H") (csi "0m") (esc "8") "x")
    (is (equal '(3 1) (cursor term)))
    (is (eql 1 (vt:face-fg (face-at term 2 1))))))

(test erasing-a-line-erases-the-half-it-was-asked-for
  (let ((term (a-term :width 8 :height 2)))
    (say term "abcdefgh" (csi "1;4H") (csi "K"))
    (is (equal "abc" (row term 0)))
    (say term (csi "2;1H") "abcdefgh" (csi "2;4H") (csi "1K"))
    (is (equal "    efgh" (vt:term-dump-row-string term 1)))))

(test erasing-the-display-erases-from-here-on
  (let ((term (a-term :width 4 :height 3)))
    (say term "aaaabbbbcccc" (csi "2;3H") (csi "J"))
    (is (equal '("aaaa" "bb" "") (rows term)))))

(test erasing-all-of-it-puts-the-cursor-home
  (let ((term (a-term :width 4 :height 2)))
    (say term "aaaabbbb" (csi "2J"))
    (is (equal '("" "") (rows term)))
    (is (equal '(0 0) (cursor term)))))

(test what-is-erased-keeps-the-background-that-was-in-force
  (let ((term (a-term :width 4 :height 2)))
    (say term (csi "41m") (csi "K"))
    (is (eql 1 (vt:face-bg (face-at term 2 0))))))

(test characters-are-deleted-and-inserted-in-place
  (let ((term (a-term :width 8 :height 2)))
    (say term "abcdefgh" (csi "1;3H") (csi "2P"))
    (is (equal "abefgh" (row term 0)))
    (say term (csi "1;3H") (csi "2@"))
    (is (equal "ab  efgh" (vt:term-dump-row-string term 0)))))

(test erase-char-leaves-a-hole-where-it-stood
  (let ((term (a-term :width 8 :height 2)))
    (say term "abcdefgh" (csi "1;3H") (csi "3X"))
    (is (equal "ab   fgh" (vt:term-dump-row-string term 0)))))

(test lines-are-inserted-and-deleted-under-the-cursor
  (let ((term (a-term :width 4 :height 4)))
    (say term "aaaabbbbcccc" (csi "2;1H") (csi "L"))
    (is (equal '("aaaa" "" "bbbb" "cccc") (rows term)))
    (say term (csi "2;1H") (csi "M"))
    (is (equal '("aaaa" "bbbb" "cccc" "") (rows term)))))

(test the-scroll-region-is-the-only-part-that-moves
  (let ((term (a-term :width 4 :height 4)))
    (say term "aaaabbbbccccdddd" (csi "2;3r") (csi "3;1H")
         (format nil "~C" #\Newline))
    (is (equal '("aaaa" "cccc" "" "dddd") (rows term)))))

(test a-reverse-index-at-the-top-scrolls-the-other-way
  (let ((term (a-term :width 4 :height 3)))
    (say term "aaaabbbbcccc" (csi "H") (esc "M"))
    (is (equal '("" "aaaa" "bbbb") (rows term)))))

(test what-scrolled-off-the-top-is-in-the-scrollback
  (let ((term (a-term :width 4 :height 2)))
    (say term "aaaa" (format nil "~C" #\Newline)
         "bbbb" (format nil "~C" #\Newline)
         "cccc")
    (is (equal '("bbbb" "cccc") (rows term)))
    (is (= 1 (vt:term-scrollback-size term)))
    (is (equal "aaaa" (vt:term-scrollback-row-string term 0)))))

(test the-scrollback-forgets-its-oldest-line-when-it-is-full
  (let ((term (a-term :width 4 :height 1 :max-scrollback 2)))
    (dolist (said '("aaaa" "bbbb" "cccc" "dddd"))
      (say term said (format nil "~C" #\Newline)))
    (is (= 2 (vt:term-scrollback-size term)))
    (is (equal "cccc" (vt:term-scrollback-row-string term 0)))
    (is (equal "dddd" (vt:term-scrollback-row-string term 1)))
    (is (null (vt:term-scrollback-row term 2)))))

(test the-alt-screen-is-another-screen-and-the-first-one-is-kept
  (let ((term (a-term :width 4 :height 2)))
    (say term "main" (csi "?1049h"))
    (is (vt:term-in-alt-screen term))
    (is (equal '("" "") (rows term)))
    (say term "alt" (csi "?1049l"))
    (is (not (vt:term-in-alt-screen term)))
    (is (equal "main" (row term 0)))))

(test the-alt-screen-scrolls-into-no-scrollback
  (let ((term (a-term :width 4 :height 2)))
    (say term (csi "?1049h") "aaaa" (format nil "~C" #\Newline)
         "bbbb" (format nil "~C" #\Newline) "cccc")
    (is (zerop (vt:term-scrollback-size term)))))

(test a-resize-keeps-what-still-fits
  (let ((term (a-term :width 8 :height 3)))
    (say term "abcdefgh" (csi "2;1H") "second")
    (vt:term-resize term 4 2)
    (is (equal '("abcd" "seco") (rows term)))
    (is (= 4 (vt:term-width term)))
    (is (= 2 (vt:term-height term)))
    (is (equal '(3 1) (cursor term)))))

(test a-reset-is-a-terminal-as-it-was-made
  (let ((term (a-term :width 8 :height 3)))
    (say term (csi "31m") (csi "?25l") (csi "2;4r") "text" (esc "c"))
    (is (equal '("" "" "") (rows term)))
    (is (equal '(0 0) (cursor term)))
    (is (vt:term-cursor-visible term))
    (is (= 2 (vt:term-scroll-bottom term)))
    (say term "x")
    (is (null (vt:face-fg (face-at term 0 0))))))

(test a-shorter-screen-keeps-what-the-cursor-is-on
  (let ((term (a-term :width 10 :height 6)))
    (dotimes (i 6)
      (say term (format nil "line~D" i))
      (when (< i 5) (say term (format nil "~C~C" #\Return #\Newline))))
    (is (equal '(5 5) (cursor term)))
    (vt:term-resize term 10 3)
    (is (equal '(5 2) (cursor term)) "the cursor came with its line")
    (is (equal '("line3" "line4" "line5") (rows term)))
    (is (equal "line0" (string-right-trim " " (vt:term-scrollback-row-string term 0)))
        "the top went to the scrollback, not away")))

(test a-shorter-screen-that-the-cursor-still-fits-on-loses-nothing
  (let ((term (a-term :width 10 :height 6)))
    (say term "top")
    (vt:term-resize term 10 3)
    (is (equal '(3 0) (cursor term)))
    (is (equal "top" (row term 0)))
    (is (zerop (vt:term-scrollback-size term)))))

(test a-taller-screen-keeps-what-was-there-where-it-was
  (let ((term (a-term :width 10 :height 3)))
    (say term "one" (format nil "~C~C" #\Return #\Newline) "two")
    (vt:term-resize term 10 8)
    (is (equal "one" (row term 0)))
    (is (equal "two" (row term 1)))
    (is (equal '(3 1) (cursor term)))))

(test the-alternate-screen-is-not-scrolled-into-the-scrollback
  (let ((term (a-term :width 10 :height 6)))
    (say term "main-line")
    (say term (csi "?1049h"))
    (dotimes (i 6)
      (say term (format nil "alt~D" i))
      (when (< i 5) (say term (format nil "~C~C" #\Return #\Newline))))
    (vt:term-resize term 10 3)
    (is (zerop (vt:term-scrollback-size term)) "the alt screen went to scrollback")
    (say term (csi "?1049l"))
    (is (equal "main-line" (row term 0)) "the main screen did not come back")))
