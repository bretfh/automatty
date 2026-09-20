(in-package #:libatty/test)

(def-suite render :in emulator)
(in-suite render)

(test a-rendered-line-is-the-characters-and-where-the-face-turns
  (let ((term (a-term :width 8 :height 1)))
    (say term (csi "31m") "ab" (csi "0m") "cd")
    (multiple-value-bind (chars changes) (vt:term-render-line term 0)
      (is (equal "abcd    " chars))
      (is (equal '(0 2) (mapcar #'first changes))
          "nothing turns where a reset face meets a cell never written to")
      (is (eql 1 (getf (second (first changes)) :fg))))))

(test a-line-of-one-face-turns-once
  (let ((term (a-term :width 4 :height 1)))
    (say term (csi "1m") "abcd")
    (multiple-value-bind (chars changes) (vt:term-render-line term 0)
      (is (equal "abcd" chars))
      (is (= 1 (length changes)))
      (is (eq t (getf (second (first changes)) :bold))))))

(test an-untouched-line-turns-nowhere
  (let ((term (a-term :width 4 :height 2)))
    (multiple-value-bind (chars changes) (vt:term-render-line term 1)
      (is (equal "    " chars))
      (is (null changes)))))

(test inverse-is-the-two-colours-the-other-way-round
  (let ((plist (vt:face-plist
                (let ((term (a-term :width 2 :height 1)))
                  (say term (csi "31;44;7m") "x")
                  (face-at term 0 0)))))
    (is (eql 4 (getf plist :fg)))
    (is (eql 1 (getf plist :bg)))
    (is (eq t (getf plist :inverse)))))

(test a-concealed-cell-is-its-own-background
  (let ((plist (vt:face-plist
                (let ((term (a-term :width 2 :height 1)))
                  (say term (csi "31;44;8m") "x")
                  (face-at term 0 0)))))
    (is (eql 4 (getf plist :fg)))
    (is (eql 4 (getf plist :bg)))))

(test what-was-rendered-back-to-ansi-is-read-back-the-same
  (let ((term (a-term :width 20 :height 1))
        (again (a-term :width 20 :height 1)))
    (say term (csi "31;44;1m") "red" (csi "0m") " plain "
         (csi "38;5;208m") "amber" (csi "0m"))
    (say again (vt:term-render-ansi-line term 0))
    (is (equal (vt:term-dump-row-string term 0)
               (vt:term-dump-row-string again 0)))
    (dotimes (x 20)
      (is (vt:face-equal (face-at term x 0) (face-at again x 0))
          "column ~D came back differently" x))))

(test a-true-colour-line-is-read-back-the-same
  (let ((term (a-term :width 8 :height 1))
        (again (a-term :width 8 :height 1)))
    (say term (csi "38;2;10;20;30;48;2;200;100;50m") "hi")
    (say again (vt:term-render-ansi-line term 0))
    (is (equal '(10 20 30) (vt:face-fg (face-at again 0 0))))
    (is (equal '(200 100 50) (vt:face-bg (face-at again 0 0))))))

(test the-whole-screen-dumps-as-one-line-per-row
  (let ((term (a-term :width 3 :height 3)))
    (say term "abc" "def")
    (is (equal (format nil "abc~%def~%   ") (vt:term-dump-to-string term)))))

(test a-character-knows-how-many-columns-it-is
  (is (= 1 (vt:char-display-width #\a)))
  (is (= 2 (vt:char-display-width #\漢)))
  (is (= 0 (vt:char-display-width (code-char #x301))))
  (is (= 0 (vt:char-display-width #\Nul))))

(test a-presentation-selector-takes-no-column-of-its-own
  (is (= 0 (vt:char-display-width (code-char #xFE0E))) "VS15")
  (is (= 0 (vt:char-display-width (code-char #xFE0F))) "VS16")
  (is (= 0 (vt:char-display-width (code-char #xFEFF))) "zero width no-break space")
  (is (= 0 (vt:char-display-width (code-char #x2060))) "word joiner"))

(test yi-and-the-pictographs-below-1f300-are-wide
  (is (= 2 (vt:char-display-width (code-char #xA000))) "a Yi syllable")
  (is (= 2 (vt:char-display-width (code-char #x1F000))) "a mahjong tile"))

(test cjk-extension-planes-are-wide
  (is (= 2 (vt:char-display-width (code-char #x20000))) "extension B")
  (is (= 2 (vt:char-display-width (code-char #x2FA1D))) "the compatibility supplement"))

(test an-out-of-range-colour-resets-rather-than-picking-white
  (let ((s (make-string-output-stream)))
    (vt:write-sgr (vt:make-face :fg 9999) s)
    (is (search ";39" (get-output-stream-string s))
        "an unwritable fg reset it instead of choosing 30-something"))
  (let ((s (make-string-output-stream)))
    (vt:write-sgr (vt:make-face :bg 9999) s)
    (is (search ";49" (get-output-stream-string s))))
  (let ((s (make-string-output-stream)))
    (vt:write-sgr (vt:make-face :underline-color 9999) s)
    (is (search ";59" (get-output-stream-string s))
        "an unwritable underline colour fell back to 39, resetting fg instead")))

(defun round-trips (said)
  "Say SAID to a terminal, write that line back out, and read it into another.
Answers the two faces at column 0, which a complete encoder makes equal."
  (let ((term (a-term :width 8 :height 1))
        (again (a-term :width 8 :height 1)))
    (say term said "x")
    (say again (with-output-to-string (s)
                 (vt:write-sgr (face-at term 0 0) s)
                 (write-char #\x s)))
    (values (face-at term 0 0) (face-at again 0 0))))

(test inverse-is-said-not-worked-out
  (multiple-value-bind (was now) (round-trips (csi "31;44;7m"))
    (is (eql 1 (vt:face-fg now)) "the fg that was written is the fg read back")
    (is (eql 4 (vt:face-bg now)))
    (is (vt:face-inverse now))
    (is (vt:face-equal was now))))

(test a-concealed-face-comes-back-concealed
  (multiple-value-bind (was now) (round-trips (csi "31;44;8m"))
    (is (vt:face-conceal now))
    (is (eql 1 (vt:face-fg now)))
    (is (vt:face-equal was now))))

(test the-underline-style-survives-the-round-trip
  (dolist (style '((2 . :double) (3 . :curly) (4 . :dotted) (5 . :dashed)))
    (multiple-value-bind (was now) (round-trips (csi "4:~Dm" (car style)))
      (is (eql (cdr style) (vt:face-underline now))
          "4:~D came back as ~S" (car style) (vt:face-underline now))
      (is (vt:face-equal was now)))))

(test blink-and-the-underline-colour-survive-the-round-trip
  (multiple-value-bind (was now) (round-trips (csi "5;4;58;5;9m"))
    (is (eql :slow (vt:face-blink now)))
    (is (eql 9 (vt:face-underline-color now)))
    (is (vt:face-equal was now)))
  (multiple-value-bind (was now) (round-trips (csi "6m"))
    (is (eql :fast (vt:face-blink now)))
    (is (vt:face-equal was now))))

(test the-nearest-of-the-256-to-an-rgb-colour
  (is (eql 231 (vt:rgb-to-color-index 255 255 255)) "white is the top of the cube")
  (is (eql 16 (vt:rgb-to-color-index 0 0 0)) "black is the bottom of it")
  (is (eql 244 (vt:rgb-to-color-index 128 128 128)) "grey lands on the ramp")
  (is (eql 196 (vt:rgb-to-color-index 255 0 0)) "red lands on the cube"))

(test a-face-that-drops-an-attribute-drops-it
  (let ((term (a-term :width 4 :height 1))
        (again (a-term :width 4 :height 1)))
    (say term (csi "1;31m") "a" (csi "22m") "b")
    (say again (vt:term-render-ansi-line term 0))
    (is (vt:face-bold (face-at again 0 0)) "the first cell kept its bold")
    (is (null (vt:face-bold (face-at again 1 0)))
        "the second dropped it rather than catching it from the first")
    (is (eql 1 (vt:face-fg (face-at again 1 0))))))

(test a-wide-character-does-not-move-the-columns-after-it
  (let ((term (a-term :width 8 :height 1))
        (again (a-term :width 8 :height 1)))
    (say term (format nil "a~Cb" (code-char #x6F22)))
    (say again (vt:term-render-ansi-line term 0))
    (is (equal (vt:term-dump-row-string term 0)
               (vt:term-dump-row-string again 0))
        "~S came back as ~S" (vt:term-dump-row-string term 0)
        (vt:term-dump-row-string again 0))))
