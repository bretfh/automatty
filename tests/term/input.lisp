(in-package #:libatty/test)

(def-suite input :in emulator)
(in-suite input)

(defun typed (term key)
  (term:key-event-to-escape-sequence term key))

(test a-character-is-itself
  (let ((term (a-term)))
    (is (equal "a" (typed term #\a)))
    (is (equal (string #\Return) (typed term '(:enter))))
    (is (equal (string #\Tab) (typed term '(:tab))))
    (is (equal (string #\Escape) (typed term '(:escape))))))

(test an-arrow-is-a-sequence-and-the-keypad-changes-which
  (let ((term (a-term)))
    (is (equal (format nil "~C[A" #\Escape) (typed term '(:up))))
    (is (equal (format nil "~C[D" #\Escape) (typed term '(:left))))
    (say term (csi "?1h"))
    (is (equal (format nil "~COA" #\Escape) (typed term '(:up))))
    (say term (csi "?1l"))
    (is (equal (format nil "~C[A" #\Escape) (typed term '(:up))))))

(test a-modifier-is-a-number-in-the-middle
  (let ((term (a-term)))
    (is (equal (format nil "~C[1;2A" #\Escape) (typed term '(:up :shift))))
    (is (equal (format nil "~C[1;5C" #\Escape) (typed term '(:right :ctrl))))
    (is (equal (format nil "~C[1;3D" #\Escape) (typed term '(:left :meta))))
    (is (equal (format nil "~C[1;8B" #\Escape)
               (typed term '(:down :shift :meta :ctrl))))))

(test home-and-end-are-arrows-by-another-name
  (let ((term (a-term)))
    (is (equal (format nil "~C[H" #\Escape) (typed term '(:home))))
    (is (equal (format nil "~C[F" #\Escape) (typed term '(:end))))))

(test the-keys-above-the-arrows-are-tilde-keys
  (let ((term (a-term)))
    (is (equal (format nil "~C[3~~" #\Escape) (typed term '(:delete))))
    (is (equal (format nil "~C[5~~" #\Escape) (typed term '(:page-up))))
    (is (equal (format nil "~C[6;5~~" #\Escape)
               (typed term '(:page-down :ctrl))))))

(test a-function-key-is-one-of-twelve
  (let ((term (a-term)))
    (is (equal (format nil "~COP" #\Escape) (typed term '(:f1))))
    (is (equal (format nil "~C[15~~" #\Escape) (typed term '(:f5))))
    (is (equal (format nil "~C[24~~" #\Escape) (typed term '(:f12))))
    (is (equal (format nil "~C[1;2P" #\Escape) (typed term '(:f1 :shift))))
    (is (equal (format nil "~C[15;5~~" #\Escape) (typed term '(:f5 :ctrl))))
    (is (null (typed term '(:f13))))))

(test backspace-is-rubout-until-a-modifier-says-otherwise
  (let ((term (a-term)))
    (is (equal (string #\Rubout) (typed term '(:backspace))))
    (is (equal (string #\Backspace) (typed term '(:backspace :ctrl))))
    (is (equal (format nil "~C~C" #\Escape #\Rubout)
               (typed term '(:backspace :meta))))))

(test shift-tab-goes-the-other-way
  (is (equal (format nil "~C[Z" #\Escape)
             (typed (a-term) '(:tab :shift)))))

(test a-key-that-is-not-one-is-nothing
  (let ((term (a-term)))
    (is (null (typed term '(:nonesuch))))
    (is (null (typed term "a string")))))

(test what-is-typed-is-what-the-program-reads
  (let* ((term (a-term :width 20 :height 2))
         (said nil))
    (setf (term:term-input-fn term)
          (lambda (term string) (declare (ignore term)) (push string said)))
    (dolist (key (list #\h #\i '(:enter)))
      (funcall (term:term-input-fn term) term (typed term key)))
    (is (equal (list (string #\Return) "i" "h") said))))

(test a-program-that-did-not-ask-for-the-mouse-is-told-nothing
  (let ((term (a-term)))
    (is (null (term:mouse-report term :press 2 3 :button :left)))
    (is (null (term:mouse-report term :wheel 2 3 :wheel :up)))))

(test the-mouse-is-said-the-way-the-program-asked
  (let ((term (a-term)))
    (say term (csi "?1000h"))
    (is (equal (format nil "~C[M~C~C~C" #\Escape #\Space #\# #\$)
               (term:mouse-report term :press 2 3 :button :left))
        "with no encoding asked for it is three bytes, each what it is and 32")
    (is (equal (format nil "~C[M~C~C~C" #\Escape #\# #\# #\$)
               (term:mouse-report term :release 2 3 :button :left))
        "the old report cannot say which button let go")
    (say term (csi "?1006h"))
    (is (equal (format nil "~C[<0;3;4M" #\Escape)
               (term:mouse-report term :press 2 3 :button :left)))
    (is (equal (format nil "~C[<2;3;4m" #\Escape)
               (term:mouse-report term :release 2 3 :button :right)))
    (is (equal (format nil "~C[<64;3;4M" #\Escape)
               (term:mouse-report term :wheel 2 3 :wheel :up)))
    (is (equal (format nil "~C[<69;3;4M" #\Escape)
               (term:mouse-report term :wheel 2 3 :wheel :down :shift t)))))

(test a-drag-is-only-said-to-a-program-that-asked-for-drags
  (let ((term (a-term)))
    (say term (csi "?1000h") (csi "?1006h"))
    (is (null (term:mouse-report term :drag 2 3 :button :left)))
    (say term (csi "?1002h"))
    (is (equal (format nil "~C[<32;3;4M" #\Escape)
               (term:mouse-report term :drag 2 3 :button :left)))))

(test the-old-encoding-says-nothing-rather-than-the-wrong-place
  (let ((term (a-term)))
    (say term (csi "?1000h"))
    (is (null (term:mouse-report term :press 200 3 :button :left)))
    (say term (csi "?1015h"))
    (is (equal (format nil "~C[32;201;4M" #\Escape)
               (term:mouse-report term :press 200 3 :button :left)))))
