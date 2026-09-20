(in-package #:libatty/test)

(def-suite parser :in emulator)
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
    (is (equal (format nil "~C[?12c" #\Escape) (funcall said))
        "it claims nothing it cannot draw: no sixel")))

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

(test mouse-tracking-modes-are-tracked-not-reported
  (let ((term (a-term :width 10 :height 2)))
    (say term (csi "?1000h"))
    (is (eq :normal (vt:term-mouse-mode term)))
    (say term (csi "?1002h"))
    (is (eq :button-event (vt:term-mouse-mode term)))
    (say term (csi "?1003h"))
    (is (eq :any-event (vt:term-mouse-mode term)))
    (say term (csi "?1003l"))
    (is (null (vt:term-mouse-mode term)))
    (say term (csi "?1006h"))
    (is (vt:term-mouse-sgr term))
    (say term (csi "?1006l"))
    (is (not (vt:term-mouse-sgr term)))))

(test legacy-alt-screen-is-the-same-as-1049
  (let ((term (a-term :width 4 :height 2)))
    (say term "main" (csi "?47h"))
    (is (vt:term-in-alt-screen term))
    (say term (csi "?47l"))
    (is (not (vt:term-in-alt-screen term)))
    (is (equal "main" (row term 0)))))

(test screen-column-modes-are-just-tracked
  (let ((term (a-term :width 10 :height 2)))
    (say term (csi "?5h"))
    (is (vt:term-reverse-video term))
    (say term (csi "?45h"))
    (is (vt:term-reverse-wraparound term))
    (say term (csi "?2026h"))
    (is (vt:term-synchronized-output term))))

(test deccolm-resizes-to-132-columns-and-clears
  (let ((term (a-term :width 80 :height 4)))
    (say term "hello" (csi "?3h"))
    (is (= 132 (vt:term-width term)))
    (is (equal "" (row term 0)) "DECCOLM clears the screen")
    (is (equal '(0 0) (cursor term)))
    (say term (csi "?3l"))
    (is (= 80 (vt:term-width term)))))

(test keypad-application-mode-is-esc-equals-not-decckm
  (let ((term (a-term :width 10 :height 2)))
    (say term (esc "="))
    (is (vt:term-keypad-application-mode term))
    (is (not (vt:term-keypad-mode term))
        "ESC= is DECKPAM, a different mode from DECCKM")
    (say term (esc ">"))
    (is (not (vt:term-keypad-application-mode term)))))

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

(test an-apc-payload-never-reaches-the-grid
  (let ((term (a-term :width 20 :height 2)))
    (say term (format nil "~C_Gf=100,a=T;AAAA~C\\after" #\Escape #\Escape))
    (is (equal "after" (row term 0))
        "a kitty graphics payload was written as text: ~S" (row term 0))))

(test sos-and-pm-are-also-swallowed-whole
  (dolist (opener '(#\X #\^))
    (let ((term (a-term :width 20 :height 2)))
      (say term (format nil "~C~Cignore me~C\\ok" #\Escape opener #\Escape))
      (is (equal "ok" (row term 0)) "~C leaked into the grid: ~S" opener (row term 0)))))

(test the-8-bit-csi-is-the-same-as-esc-bracket
  (let ((term (a-term :width 10 :height 5)))
    (say term (format nil "~C3;4H" (code-char 155)) "x")
    (is (eql #\x (at term 3 2)) "the 8-bit CSI was written to the grid as text")))

(test a-repeat-says-the-last-character-again
  (let ((term (a-term :width 10 :height 2)))
    (say term "a" (csi "4b"))
    (is (equal "aaaaa" (row term 0)))))

(test a-nonsense-sequence-is-dropped-and-the-rest-is-read
  (let ((term (a-term :width 20 :height 2)))
    (say term (csi "99;99;99!x") "after")
    (is (equal "after" (row term 0)))))

(test a-device-control-string-split-across-two-reads-still-ends
  (let ((term (a-term :width 20 :height 2)))
    (say term (esc "P1$r0m") (string #\Escape))
    (say term "\\hello")
    (is (equal "hello" (row term 0))
        "everything after the split terminator was swallowed: ~S" (row term 0))))

(test a-device-control-string-one-character-at-a-time
  (let ((term (a-term :width 20 :height 2))
        (said (format nil "~CP1$r0m~C\\done" #\Escape #\Escape)))
    (loop :for ch :across said :do (say term (string ch)))
    (is (equal "done" (row term 0)) "~S" (row term 0))))

(test an-escape-inside-a-control-sequence-abandons-it
  (let ((term (a-term :width 20 :height 3)))
    (say term (csi "3") (csi "2J") "after")
    (is (equal "after" (row term 0))
        "the abandoned sequence was read as text: ~S" (row term 0))))

(test cancel-and-substitute-abandon-a-control-sequence
  (dolist (code (list 24 26))
    (let ((term (a-term :width 20 :height 2)))
      (say term (csi "31") (string (code-char code)) "plain")
      (is (equal "plain" (row term 0))
          "~D did not abandon it: ~S" code (row term 0))
      (is (vt:face-default-p (face-at term 0 0))
          "the abandoned SGR was applied anyway"))))

(test what-a-program-can-make-us-hold-is-bounded
  ;; a terminal's input is untrusted: one escape sequence must not be able to
  ;; ask for an arbitrary amount of work or memory
  (let ((term (a-term :width 10 :height 4)))
    (say term "x" (csi "999999999b"))
    ;; one x, then a screenful of them: 41 characters on 10x4, so the last one
    ;; wraps off the bottom and takes the rest up with it
    (is (equal "xxxxxxxxxx" (row term 0))
        "REP was not held to what a screen is: ~S" (row term 0))
    (is (equal "x" (row term 3))
        "REP did not stop at a screenful: ~S" (row term 3)))
  (let ((term (a-term :width 20 :height 2)))
    (say term (csi (format nil "~Am" (make-string 10000 :initial-element #\9)))
         "after")
    (is (equal "after" (row term 0))
        "a runaway sequence was not abandoned: ~S" (row term 0))
    (is-true (vt:face-default-p (face-at term 0 0))
             "the abandoned sequence was applied anyway"))
  (let ((term (a-term :width 20 :height 2)))
    (say term (csi (format nil "~{~A~^;~}m" (make-list 500 :initial-element 1)))
         "after")
    (is (equal "after" (row term 0))
        "a sequence with 500 parameters was not abandoned: ~S" (row term 0)))
  (is (eql vt::+biggest-param+ (vt::held-param (1+ vt::+biggest-param+)))
      "a parameter past the ceiling is handed on whole")
  (is (eql 7 (vt::held-param 7)) "an ordinary parameter was changed")
  (is (null (vt::held-param nil)) "a parameter nobody gave became a number")
  (let ((term (a-term :width 10 :height 2)))
    (say term (format nil "~C]0;~A" #\Escape (make-string 200000
                                                          :initial-element #\z)))
    (is (<= (length (vt::term-osc-buf term)) vt::+biggest-string+)
        "an unterminated osc payload grew past its bound: ~D"
        (length (vt::term-osc-buf term)))))

;;; What somebody who did not write this library can do to it: a term of their
;;; own, and methods that add a sequence, change one, or take the place of a
;;; callback slot.

(defstruct (a-borrowed-term (:include vt:term))
  (rang 0 :type fixnum)
  (marks nil :type list))

(defmethod vt:handle-csi ((term a-borrowed-term) (final (eql #\|)) format params)
  "A sequence this library has never heard of."
  (declare (ignore format))
  (push (or (first params) 0) (a-borrowed-term-marks term)))

(defmethod vt:handle-csi ((term a-borrowed-term) (final (eql #\J)) format params)
  "A sequence it has, made to mean something else."
  (declare (ignore format params))
  (vt:term-write term "kept"))

(defmethod vt:handle-esc ((term a-borrowed-term) (final (eql #\7)))
  "And one of its escapes."
  (push :saved (a-borrowed-term-marks term)))

(test a-term-of-your-own-can-add-a-sequence-and-change-one
  (let ((term (vt:init-term (make-a-borrowed-term) :width 12 :height 2)))
    (say term (csi "42|"))
    (is (equal '(42) (a-borrowed-term-marks term))
        "a sequence nothing defined did not reach the method")

    (say term "keepme" (csi "2J"))
    (is (equal "keepmekept" (row term 0))
        "erase still erased, so the method did not take: ~S" (row term 0))

    (say term (esc "7"))
    (is (equal '(:saved 42) (a-borrowed-term-marks term)))

    (is (equal '(0 0) (cursor (a-term :width 4 :height 1)))
        "a plain term was changed by somebody else's methods")))

(test a-plain-term-is-untouched-by-them
  (let ((term (a-term :width 12 :height 2)))
    (say term "keepme" (csi "2J"))
    (is (equal "" (row term 0))
        "the library's own erase stopped erasing: ~S" (row term 0))))

(defmethod vt:term-rang ((term a-borrowed-term))
  (incf (a-borrowed-term-rang term)))

(defmethod vt:term-titled ((term a-borrowed-term) title)
  (push (cons :titled title) (a-borrowed-term-marks term)))

(defmethod vt:term-linked ((term a-borrowed-term) uri params)
  (push (list :linked uri params) (a-borrowed-term-marks term)))

(defmethod vt:term-copied ((term a-borrowed-term) selection data)
  (push (list :copied selection data) (a-borrowed-term-marks term)))

(test a-hyperlink-is-handed-over-not-drawn
  (let ((term (vt:init-term (make-a-borrowed-term) :width 10 :height 2)))
    (say term (osc "8;;https://example.com"))
    (say term "click" (osc "8;;"))
    (is (equal '(:linked nil "") (first (a-borrowed-term-marks term))))
    (is (equal '(:linked "https://example.com" "")
               (second (a-borrowed-term-marks term))))
    (is (equal "click" (row term 0)) "the link text was still written")))

(test a-clipboard-write-is-handed-over
  (let ((term (vt:init-term (make-a-borrowed-term) :width 10 :height 2)))
    (say term (osc "52;c;aGVsbG8="))
    (is (equal '(:copied "c" "aGVsbG8=") (first (a-borrowed-term-marks term))))))

(test a-method-takes-the-place-of-a-callback-slot
  (let ((term (vt:init-term (make-a-borrowed-term) :width 12 :height 2)))
    (say term (string (code-char 7)) (string (code-char 7)))
    (is (eql 2 (a-borrowed-term-rang term))
        "the bell did not reach the method")
    (say term (osc "0;a name"))
    (is (equal '(:titled . "a name") (first (a-borrowed-term-marks term)))
        "the title did not reach the method")
    (is (equal "a name" (vt:term-title term))
        "the term stopped keeping the title itself")))

(test a-slot-still-works-for-somebody-who-wants-a-lambda
  (let* ((rang 0)
         (titles nil)
         (term (a-term :width 8 :height 1
                       :bell-fn (lambda (term) (declare (ignore term))
                                  (incf rang))
                       :title-fn (lambda (term title) (declare (ignore term))
                                   (push title titles)))))
    (say term (string (code-char 7)) (osc "2;still here"))
    (is (eql 1 rang))
    (is (equal '("still here") titles))))
