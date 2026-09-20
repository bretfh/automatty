(in-package #:libatty/test)

(def-suite sgr :in emulator)
(in-suite sgr)

(defun face-said (said)
  (let ((term (a-term :width 4 :height 1)))
    (say term said "x")
    (face-at term 0 0)))

(test the-eight-colours-are-the-first-eight-of-the-palette
  (is (eql 1 (term:face-fg (face-said (csi "31m")))))
  (is (eql 4 (term:face-bg (face-said (csi "44m")))))
  (is (eql 11 (term:face-fg (face-said (csi "93m")))))
  (is (eql 10 (term:face-bg (face-said (csi "102m"))))))

(test a-colour-of-the-two-hundred-and-fifty-six-is-an-index
  (is (eql 208 (term:face-fg (face-said (csi "38;5;208m")))))
  (is (eql 17 (term:face-bg (face-said (csi "48;5;17m")))))
  (is (null (term:face-fg (face-said (csi "38;5;999m"))))))

(test a-true-colour-is-the-three-of-them
  (is (equal '(10 20 30) (term:face-fg (face-said (csi "38;2;10;20;30m")))))
  (is (equal '(1 2 3) (term:face-bg (face-said (csi "48;2;1;2;3m"))))))

(test a-colon-says-the-same-colour-as-a-semicolon
  (is (equal '(10 20 30) (term:face-fg (face-said (csi "38:2:10:20:30m")))))
  (is (equal '(10 20 30) (term:face-fg (face-said (csi "38:2::10:20:30m")))))
  (is (eql 208 (term:face-fg (face-said (csi "38:5:208m"))))))

(test a-colon-after-four-is-which-underline-it-is
  (is (eql :single (term:face-underline (face-said (csi "4m")))))
  (is (eql :double (term:face-underline (face-said (csi "4:2m")))))
  (is (eql :curly (term:face-underline (face-said (csi "4:3m")))))
  (is (eql :dashed (term:face-underline (face-said (csi "4:5m")))))
  (is (null (term:face-underline (face-said (csi "4:0m"))))))

(test a-curly-underline-is-not-an-italic
  (let ((face (face-said (csi "4:3m"))))
    (is (eql :curly (term:face-underline face)))
    (is (null (term:face-italic face)))))

(test an-underline-has-a-colour-of-its-own
  (is (eql 5 (term:face-underline-color (face-said (csi "4;58;5;5m")))))
  (is (equal '(9 9 9) (term:face-underline-color (face-said (csi "58:2:9:9:9m")))))
  (is (null (term:face-underline-color (face-said (csi "58;5;5m~C[59m" #\Escape))))))

(test the-attributes-go-on-and-come-off
  (let ((face (face-said (csi "1;3;4;5;7;8;9m"))))
    (is (term:face-bold face))
    (is (term:face-italic face))
    (is (eql :single (term:face-underline face)))
    (is (eql :slow (term:face-blink face)))
    (is (term:face-inverse face))
    (is (term:face-conceal face))
    (is (term:face-crossed face)))
  (let ((face (face-said (csi "1;3;4;7;9m~C[22;23;24;27;29m" #\Escape))))
    (is (null (term:face-bold face)))
    (is (null (term:face-italic face)))
    (is (null (term:face-underline face)))
    (is (null (term:face-inverse face)))
    (is (null (term:face-crossed face)))))

(test bold-and-faint-turn-each-other-off
  (is (term:face-faint (face-said (csi "1;2m"))))
  (is (null (term:face-bold (face-said (csi "1;2m")))))
  (is (term:face-bold (face-said (csi "2;1m")))))

(test an-sgr-with-nothing-in-it-is-a-reset
  (let ((face (face-said (csi "31;1m~C[m" #\Escape))))
    (is (null (term:face-fg face)))
    (is (null (term:face-bold face)))))

(test the-default-colours-come-back-without-the-rest-going
  (let ((face (face-said (csi "1;31;44m~C[39;49m" #\Escape))))
    (is (null (term:face-fg face)))
    (is (null (term:face-bg face)))
    (is (term:face-bold face))))

(test one-face-is-shared-by-every-cell-that-wears-it
  (let ((term (a-term :width 8 :height 1)))
    (say term (csi "31m") "abc" (csi "32m") "d" (csi "31m") "e")
    (is (eq (face-at term 0 0) (face-at term 2 0)))
    (is (eq (face-at term 0 0) (face-at term 4 0)))
    (is (not (eq (face-at term 0 0) (face-at term 3 0))))))

(test a-palette-index-answers-its-three-numbers
  (is (equalp #(0 0 0) (term:color-index-to-rgb 16)))
  (is (equalp #(255 255 255) (term:color-index-to-rgb 231)))
  (is (equalp #(8 8 8) (term:color-index-to-rgb 232)))
  (is (null (term:color-index-to-rgb 256)))
  (is (null (term:color-index-to-rgb nil))))
