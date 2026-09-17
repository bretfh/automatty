(in-package #:vt/test)

(def-suite parser :in all)
(in-suite parser)

(defun heard (term)
  (let ((said nil))
    (setf (vt:term-input-fn term)
          (lambda (term string) (declare (ignore term)) (push string said)))
    (lambda () (apply #'concatenate 'string (nreverse said)))))

(test a-title-is-said-once-and-kept
  (let* ((term (a-term :width 10 :height 2))
         (told nil))
    (setf (vt:term-title-fn term)
          (lambda (term title) (declare (ignore term)) (push title told)))
    (say term (osc "0;a window"))
    (is (equal "a window" (vt:term-title term)))
    (is (equal '("a window") told))))

(test a-title-with-a-backslash-in-it-is-the-whole-title
  (let ((term (a-term :width 10 :height 2)))
    (say term (osc "2;C:\\Users\\bfh"))
    (is (equal "C:\\Users\\bfh" (vt:term-title term)))))

(test a-string-terminator-ends-a-title-as-a-bell-does
  (let ((term (a-term :width 10 :height 2)))
    (say term (format nil "~C]0;by st~C\\" #\Escape #\Escape))
    (is (equal "by st" (vt:term-title term)))
    (say term "after")
    (is (equal "after" (row term 0)))))

(test a-working-directory-is-told-as-it-is-given
  (let ((term (a-term :width 10 :height 2)))
    (say term (osc "7;file://host/home/bfh"))
    (is (equal "file://host/home/bfh" (vt:term-cwd term)))))

(test a-title-split-across-what-arrived-is-still-one-title
  (let ((term (a-term :width 10 :height 2)))
    (say term (format nil "~C]0;half" #\Escape) " and half"
         (format nil "~C" #\Bel))
    (is (equal "half and half" (vt:term-title term)))))

(test a-device-status-report-is-answered-where-the-cursor-is
  (let* ((term (a-term :width 10 :height 5))
         (said (heard term)))
    (say term (csi "3;4H") (csi "6n"))
    (is (equal (format nil "~C[3;4R" #\Escape) (funcall said)))))

(test a-terminal-says-what-it-is-when-it-is-asked
  (let* ((term (a-term :width 10 :height 2))
         (said (heard term)))
    (say term (csi "c"))
    (is (equal (format nil "~C[?12;4c" #\Escape) (funcall said)))))

(test a-bell-is-rung-and-nothing-is-written
  (let* ((term (a-term :width 10 :height 2))
         (rung 0))
    (setf (vt:term-bell-fn term) (lambda (term) (declare (ignore term)) (incf rung)))
    (say term (format nil "a~Cb" #\Bel))
    (is (= 1 rung))
    (is (equal "ab" (row term 0)))))

(test a-mode-is-set-and-unset-by-the-same-number
  (let ((term (a-term :width 10 :height 2)))
    (say term (csi "?25l"))
    (is (not (vt:term-cursor-visible term)))
    (say term (csi "?25h"))
    (is (vt:term-cursor-visible term))
    (say term (csi "?2004h"))
    (is (vt:term-bracketed-paste term))
    (say term (csi "?2004l"))
    (is (not (vt:term-bracketed-paste term)))))

(test the-cursor-is-the-shape-that-was-asked-for
  (let ((term (a-term :width 10 :height 2)))
    (say term (csi "2 q"))
    (is (eql :block (vt:term-cursor-style term)))
    (say term (csi "4 q"))
    (is (eql :underline (vt:term-cursor-style term)))
    (say term (csi "?12h"))
    (is (eql :blinking-underline (vt:term-cursor-style term)))
    (say term (csi "6 q"))
    (is (eql :bar (vt:term-cursor-style term)))))

(test a-device-control-string-is-passed-over
  (let ((term (a-term :width 10 :height 2)))
    (say term (format nil "~CP1$r0m~C\\here" #\Escape #\Escape))
    (is (equal "here" (row term 0)))))

(test a-repeat-says-the-last-character-again
  (let ((term (a-term :width 10 :height 2)))
    (say term "a" (csi "4b"))
    (is (equal "aaaaa" (row term 0)))))

(test a-nonsense-sequence-is-dropped-and-the-rest-is-read
  (let ((term (a-term :width 20 :height 2)))
    (say term (csi "99;99;99!x") "after")
    (is (equal "after" (row term 0)))))
