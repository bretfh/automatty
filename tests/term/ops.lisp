(in-package #:libatty/test)

(def-suite ops :in emulator)
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
    (is (eql 1 (term:face-fg (face-at term 2 1))))))

(test erasing-a-line-erases-the-half-it-was-asked-for
  (let ((term (a-term :width 8 :height 2)))
    (say term "abcdefgh" (csi "1;4H") (csi "K"))
    (is (equal "abc" (row term 0)))
    (say term (csi "2;1H") "abcdefgh" (csi "2;4H") (csi "1K"))
    (is (equal "    efgh" (term:term-dump-row-string term 1)))))

(test erasing-the-display-erases-from-here-on
  (let ((term (a-term :width 4 :height 3)))
    (say term "aaaabbbbcccc" (csi "2;3H") (csi "J"))
    (is (equal '("aaaa" "bb" "") (rows term)))))

(test erasing-all-of-it-leaves-the-cursor-where-it-was
  (let ((term (a-term :width 4 :height 2)))
    (say term "aaaabbbb")
    (let ((before (cursor term)))
      (say term (csi "2J"))
      (is (equal '("" "") (rows term)))
      (is (equal before (cursor term)) "ED moved the cursor; it never does"))))

(test what-is-erased-keeps-the-background-that-was-in-force
  (let ((term (a-term :width 4 :height 2)))
    (say term (csi "41m") (csi "K"))
    (is (eql 1 (term:face-bg (face-at term 2 0))))))

(test what-is-erased-carries-only-the-background
  (let ((term (a-term :width 4 :height 2)))
    (say term (csi "1;4;44m") (csi "K"))
    (let ((f (face-at term 2 0)))
      (is (eql 4 (term:face-bg f)))
      (is (null (term:face-bold f)) "BCE carried bold along with the background")
      (is (null (term:face-underline f))
          "BCE carried underline along with the background"))))

(test characters-are-deleted-and-inserted-in-place
  (let ((term (a-term :width 8 :height 2)))
    (say term "abcdefgh" (csi "1;3H") (csi "2P"))
    (is (equal "abefgh" (row term 0)))
    (say term (csi "1;3H") (csi "2@"))
    (is (equal "ab  efgh" (term:term-dump-row-string term 0)))))

(test erase-char-leaves-a-hole-where-it-stood
  (let ((term (a-term :width 8 :height 2)))
    (say term "abcdefgh" (csi "1;3H") (csi "3X"))
    (is (equal "ab   fgh" (term:term-dump-row-string term 0)))))

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
    (say term "aaaa" (format nil "~C~C" #\Return #\Newline)
         "bbbb" (format nil "~C~C" #\Return #\Newline)
         "cccc")
    (is (equal '("bbbb" "cccc") (rows term)))
    (is (= 1 (term:term-scrollback-size term)))
    (is (equal "aaaa" (term:term-scrollback-row-string term 0)))))

(test the-scrollback-forgets-its-oldest-line-when-it-is-full
  (let ((term (a-term :width 4 :height 1 :max-scrollback 2)))
    (dolist (said '("aaaa" "bbbb" "cccc" "dddd"))
      (say term said (format nil "~C~C" #\Return #\Newline)))
    (is (= 2 (term:term-scrollback-size term)))
    (is (equal "cccc" (term:term-scrollback-row-string term 0)))
    (is (equal "dddd" (term:term-scrollback-row-string term 1)))
    (is (null (term:term-scrollback-row term 2)))))

(test the-alt-screen-is-another-screen-and-the-first-one-is-kept
  (let ((term (a-term :width 4 :height 2)))
    (say term "main" (csi "?1049h"))
    (is (term:term-in-alt-screen term))
    (is (equal '("" "") (rows term)))
    (say term "alt" (csi "?1049l"))
    (is (not (term:term-in-alt-screen term)))
    (is (equal "main" (row term 0)))))

(test the-alt-screen-scrolls-into-no-scrollback
  (let ((term (a-term :width 4 :height 2)))
    (say term (csi "?1049h") "aaaa" (format nil "~C" #\Newline)
         "bbbb" (format nil "~C" #\Newline) "cccc")
    (is (zerop (term:term-scrollback-size term)))))

(test a-resize-keeps-what-still-fits
  (let ((term (a-term :width 8 :height 3)))
    (say term "abcdefgh" (csi "2;1H") "second")
    (term:term-resize term 4 2)
    (is (equal '("abcd" "seco") (rows term)))
    (is (= 4 (term:term-width term)))
    (is (= 2 (term:term-height term)))
    (is (equal '(3 1) (cursor term)))))

(test a-reset-is-a-terminal-as-it-was-made
  (let ((term (a-term :width 8 :height 3)))
    (say term (csi "31m") (csi "?25l") (csi "2;4r") "text" (esc "c"))
    (is (equal '("" "" "") (rows term)))
    (is (equal '(0 0) (cursor term)))
    (is (term:term-cursor-visible term))
    (is (= 2 (term:term-scroll-bottom term)))
    (say term "x")
    (is (null (term:face-fg (face-at term 0 0))))))

(test a-shorter-screen-keeps-what-the-cursor-is-on
  (let ((term (a-term :width 10 :height 6)))
    (dotimes (i 6)
      (say term (format nil "line~D" i))
      (when (< i 5) (say term (format nil "~C~C" #\Return #\Newline))))
    (is (equal '(5 5) (cursor term)))
    (term:term-resize term 10 3)
    (is (equal '(5 2) (cursor term)) "the cursor came with its line")
    (is (equal '("line3" "line4" "line5") (rows term)))
    (is (equal "line0" (string-right-trim " " (term:term-scrollback-row-string term 0)))
        "the top went to the scrollback, not away")))

(test a-shorter-screen-that-the-cursor-still-fits-on-loses-nothing
  (let ((term (a-term :width 10 :height 6)))
    (say term "top")
    (term:term-resize term 10 3)
    (is (equal '(3 0) (cursor term)))
    (is (equal "top" (row term 0)))
    (is (zerop (term:term-scrollback-size term)))))

(test a-taller-screen-keeps-what-was-there-where-it-was
  (let ((term (a-term :width 10 :height 3)))
    (say term "one" (format nil "~C~C" #\Return #\Newline) "two")
    (term:term-resize term 10 8)
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
    (term:term-resize term 10 3)
    (is (zerop (term:term-scrollback-size term)) "the alt screen went to scrollback")
    (say term (csi "?1049l"))
    (is (equal "main-line" (row term 0)) "the main screen did not come back")))

(test deleting-a-line-does-not-put-it-behind-the-screen
  (let ((term (a-term :width 8 :height 3)))
    (say term "one" (csi "2;1H") "two" (csi "3;1H") "three")
    (say term (csi "H") (csi "M"))
    (is (equal "two" (row term 0)) "the line under it did not come up")
    (is (zerop (term:term-scrollback-size term))
        "a line deleted out of the screen was put in the scrollback")))

(test a-cursor-saved-on-a-bigger-screen-comes-back-inside-this-one
  (let ((term (a-term :width 80 :height 40)))
    (say term (csi "?1049h") (csi "40;70H"))
    (term:term-resize term 20 10)
    (say term (csi "?1049l"))
    (is (equal '(0 0) (cursor term))
        "restored to ~S on a 20x10 screen" (cursor term))
    (say term "x")
    (is (equal "x" (row term 0)) "writing after the restore did not land")))

(test the-alt-screen-keeps-its-own-saved-cursor
  (let ((term (a-term :width 20 :height 10)))
    (say term (csi "3;5H") (csi "?1049h"))
    ;; a save the program does while it is over there
    (say term (csi "8;12H") (esc "7") (csi "1;1H") (esc "8"))
    (is (equal '(11 7) (cursor term))
        "the save made inside the alt screen did not come back: ~S"
        (cursor term))
    (say term (csi "?1049l"))
    (is (equal '(4 2) (cursor term))
        "leaving the alt screen did not restore where it was entered: ~S"
        (cursor term))))

(test a-screen-can-be-driven-by-hand-without-any-escapes
  ;; the operations are the vocabulary a sequence is written in, so they are
  ;; part of the surface: somebody adding one reaches for these
  (let ((term (a-term :width 12 :height 4)))
    (term:term-goto term 1 1)
    (term:term-write term "one")
    (term:term-goto term 2 1)
    (term:term-write term "two")
    (term:term-goto term 3 1)
    (term:term-write term "three")
    (is (equal '("one" "two" "three" "") (rows term)))

    (term:term-goto term 2 1)
    (term:term-delete-line term 1)
    (is (equal '("one" "three" "" "") (rows term))
        "delete-line: ~S" (rows term))

    (term:term-goto term 1 2)
    (term:term-insert-char term 2)
    (is (equal "o  ne" (row term 0)) "insert-char: ~S" (row term 0))

    (term:term-goto term 1 1)
    (term:term-erase-in-line term 0)
    (is (equal "" (row term 0)) "erase-in-line: ~S" (row term 0))

    (term:term-set-scroll-region term 1 2)
    (is (eql 0 (term:term-scroll-top term)))
    (is (eql 1 (term:term-scroll-bottom term)))
    (term:term-scroll-up term 1)
    (is (equal '("three" "" "" "") (rows term))
        "scroll-up inside the region: ~S" (rows term))

    (term:term-save-cursor term)
    (term:term-goto term 4 4)
    (term:term-restore-cursor term)
    (is (equal '(0 0) (cursor term)) "save and restore: ~S" (cursor term))

    (term:term-enter-alt-screen term)
    (term:term-write term "over")
    (is (equal "over" (row term 0)))
    (is-true (term:term-in-alt-screen term))
    (term:term-exit-alt-screen term)
    (is (equal "three" (row term 0))
        "the main screen did not come back: ~S" (row term 0))))

(test origin-mode-addresses-inside-the-region
  (let ((term (a-term :width 10 :height 10)))
    (say term (csi "3;7r") (csi "?6h") (csi "1;1H"))
    (is (equal '(0 2) (cursor term))
        "row 1 of the region is row 3 of the screen: ~S" (cursor term))
    (say term (csi "?6l") (csi "1;1H"))
    (is (equal '(0 0) (cursor term)) "origin mode off addresses the screen")))

(test origin-mode-cannot-leave-the-region
  (let ((term (a-term :width 10 :height 10)))
    (say term (csi "3;7r") (csi "?6h") (csi "20;1H"))
    (is (equal '(0 6) (cursor term))
        "row 20 clamped to the bottom of the region: ~S" (cursor term))))

(test a-bare-linefeed-does-not-return-to-column-one
  (let ((term (a-term :width 10 :height 10)))
    (say term (csi "5;5H") "ab" (string #\Newline) "c")
    (is (equal '(7 5) (cursor term))
        "LF moved the column when LNM was off: ~S" (cursor term))))

(test lnm-makes-a-bare-linefeed-return-too
  (let ((term (a-term :width 10 :height 10)))
    (say term (csi "20h") (csi "5;5H") "ab" (string #\Newline) "c")
    (is (equal '(1 5) (cursor term)))))

(test a-tab-stop-is-set-and-cleared-where-the-cursor-is
  (let ((term (a-term :width 40 :height 1)))
    (say term (csi "4G") (esc "H") (csi "1G") (csi "I"))
    (is (equal '(3 0) (cursor term)) "the custom stop at column 4 was used")
    (say term (csi "0g") (csi "1G") (csi "I"))
    (is (equal '(8 0) (cursor term))
        "the cleared stop was skipped, landing on the next default one")))

(test tbc-3-clears-every-stop
  (let ((term (a-term :width 40 :height 1)))
    (say term (csi "3g") (csi "I"))
    (is (equal '(39 0) (cursor term))
        "with no stops left, tab goes to the right margin")))

(test backtab-goes-to-the-stop-before-the-cursor
  (let ((term (a-term :width 40 :height 1)))
    (say term (csi "20G") (csi "Z"))
    (is (equal '(16 0) (cursor term)))))

(test decstr-puts-the-pen-back-without-touching-the-screen
  (let ((term (a-term :width 10 :height 4)))
    (say term (csi "31m") (csi "4h") (csi "2;5r") "text" (csi "!p"))
    (is (not (term:term-insert-mode term)))
    (is (eql 0 (term:term-scroll-top term)))
    (is (eql 3 (term:term-scroll-bottom term)))
    (is (equal '(0 0) (cursor term)))
    (is (equal "text" (row term 0)) "the screen itself was not touched")
    (say term "x")
    (is (term:face-default-p (face-at term 0 0)) "the pen itself was reset")))

(test decaln-fills-the-screen-with-e
  (let ((term (a-term :width 4 :height 2)))
    (say term (csi "31m") (esc "#8"))
    (is (equal '("EEEE" "EEEE") (rows term)))
    (is (term:face-default-p (face-at term 0 0))
        "the alignment pattern is in the default face")))
