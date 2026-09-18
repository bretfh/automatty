(in-package #:vt/test)

(def-suite input :in all)
(in-suite input)

(defun typed (term key)
  (vt:key-event-to-escape-sequence term key))

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
    (setf (vt:term-input-fn term)
          (lambda (term string) (declare (ignore term)) (push string said)))
    (dolist (key (list #\h #\i '(:enter)))
      (funcall (vt:term-input-fn term) term (typed term key)))
    (is (equal (list (string #\Return) "i" "h") said))))

(defun a-key-again (term event)
  (let ((said (vt:key-event-to-escape-sequence term event)))
    (when said
      (multiple-value-bind (back took)
          (vt:escape-sequence-to-key-event said 0 (length said) nil)
        (values back took (length said) said)))))

(defun same-key-p (a b)
  (and (eq (first a) (first b))
       (null (set-exclusive-or (rest a) (rest b)))))

(defparameter +mods+ '(() (:shift) (:ctrl) (:meta) (:shift :ctrl)
                       (:ctrl :meta) (:shift :meta :ctrl)))

(test what-was-typed-and-read-back-is-the-same-key
  (let ((term (a-term)))
    (dolist (keypad '(nil t))
      (when keypad (say term (csi "?1h")))
      (dolist (key '(:up :down :left :right :home :end
                     :insert :delete :page-up :page-down
                     :f1 :f2 :f3 :f4 :f5 :f6 :f7 :f8 :f9 :f10 :f11 :f12))
        (dolist (mods +mods+)
          (let ((event (list* key mods)))
            (multiple-value-bind (back took length said) (a-key-again term event)
              (is (same-key-p event back)
                  "~S went out as ~S and came back as ~S" event said back)
              (is (eql took length)
                  "~S came back after ~D of its ~D characters" said took length))))))))

(test a-backspace-comes-back-with-the-modifiers-it-was-sent-with
  (let ((term (a-term)))
    (dolist (mods '(() (:ctrl) (:meta) (:ctrl :meta)))
      (let ((event (list* :backspace mods)))
        (multiple-value-bind (back took length said) (a-key-again term event)
          (is (same-key-p event back)
              "~S went out as ~S and came back as ~S" event said back)
          (is (eql took length)))))))

(test a-modifier-the-old-encoding-has-no-room-for-is-dropped
  (let ((term (a-term)))
    (dolist (event '((:tab :ctrl) (:enter :shift) (:escape :meta)
                     (:backspace :shift)))
      (let ((said (vt:key-event-to-escape-sequence term event)))
        (is (eql 1 (length said))
            "~S was written as ~S, which has room for a modifier" event said)
        (is (same-key-p (list (first event))
                        (vt:escape-sequence-to-key-event said 0 1 nil))
            "~S came back as something other than a bare ~S" said (first event))))
    (multiple-value-bind (back) (a-key-again term '(:tab :shift))
      (is (same-key-p '(:tab :shift) back)
          "shift-tab has its own sequence and should keep its shift"))))

(test a-character-typed-is-the-character-read-back
  (dolist (ch '(#\a #\Z #\5 #\Space #\~ #\ä))
    (multiple-value-bind (back took)
        (vt:escape-sequence-to-key-event (string ch) 0 1 nil)
      (is (eql ch back))
      (is (eql 1 took)))))

(test half-a-sequence-asks-for-the-rest-of-it
  (let ((whole (format nil "~C[1;5A" #\Escape)))
    (dotimes (n (length whole))
      (multiple-value-bind (back took)
          (vt:escape-sequence-to-key-event whole 0 n)
        (is (null back) "~D characters of ~S already said a key" n whole)
        (is (eql 0 took))))
    (multiple-value-bind (back took)
        (vt:escape-sequence-to-key-event whole)
      (is (same-key-p '(:up :ctrl) back))
      (is (eql 6 took)))))

(test a-lone-escape-is-the-escape-key-only-when-nothing-is-coming
  (let ((said (string #\Escape)))
    (is (null (vt:escape-sequence-to-key-event said 0 1 t)))
    (is (equal '(:escape) (vt:escape-sequence-to-key-event said 0 1 nil)))))

(test one-read-of-many-keys-is-many-keys
  (let ((said (format nil "hi~C[A~C[3~~~C" #\Escape #\Escape #\Return))
        (keys nil)
        (at 0))
    (loop
      (multiple-value-bind (key took)
          (vt:escape-sequence-to-key-event said at (length said) nil)
        (when (zerop took) (return))
        (push key keys)
        (incf at took)
        (when (>= at (length said)) (return))))
    (is (equal '(#\h #\i (:up) (:delete) (:enter)) (nreverse keys)))))
