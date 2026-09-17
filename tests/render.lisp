(in-package #:vt/test)

(def-suite render :in all)
(in-suite render)

(test a-rendered-line-is-the-characters-and-where-the-face-turns
  (let ((term (a-term :width 8 :height 1)))
    (say term (csi "31m") "ab" (csi "0m") "cd")
    (multiple-value-bind (chars changes) (vt:term-render-line term 0)
      (is (equal "abcd    " chars))
      (is (equal '(0 2 4) (mapcar #'first changes)))
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
  (let ((plist (vt:face-attrs-to-plist
                (let ((term (a-term :width 2 :height 1)))
                  (say term (csi "31;44;7m") "x")
                  (face-at term 0 0)))))
    (is (eql 4 (getf plist :fg)))
    (is (eql 1 (getf plist :bg)))
    (is (eq t (getf plist :inverse)))))

(test a-concealed-cell-is-its-own-background
  (let ((plist (vt:face-attrs-to-plist
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
      (is (equal (vt:face-attrs-to-plist (face-at term x 0))
                 (vt:face-attrs-to-plist (face-at again x 0)))
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
