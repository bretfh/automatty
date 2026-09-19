(in-package #:vt/test)

(def-suite keys :in all)
(in-suite keys)

;;; What a terminal sent, read back as the key somebody pressed. The other
;;; direction is vt's and is tested in tests/term/input.lisp; a round trip
;;; needs both, so it lives on this side.

(defun a-key-again (term event)
  (let ((said (vt:key-event-to-escape-sequence term event)))
    (when said
      (multiple-value-bind (back took)
          (tty:escape-sequence-to-key-event said 0 (length said) nil)
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
                        (tty:escape-sequence-to-key-event said 0 1 nil))
            "~S came back as something other than a bare ~S" said (first event))))
    (multiple-value-bind (back) (a-key-again term '(:tab :shift))
      (is (same-key-p '(:tab :shift) back)
          "shift-tab has its own sequence and should keep its shift"))))

(test a-character-typed-is-the-character-read-back
  (dolist (ch '(#\a #\Z #\5 #\Space #\~ #\ä))
    (multiple-value-bind (back took)
        (tty:escape-sequence-to-key-event (string ch) 0 1 nil)
      (is (eql ch back))
      (is (eql 1 took)))))

(test half-a-sequence-asks-for-the-rest-of-it
  (let ((whole (format nil "~C[1;5A" #\Escape)))
    (dotimes (n (length whole))
      (multiple-value-bind (back took)
          (tty:escape-sequence-to-key-event whole 0 n)
        (is (null back) "~D characters of ~S already said a key" n whole)
        (is (eql 0 took))))
    (multiple-value-bind (back took)
        (tty:escape-sequence-to-key-event whole)
      (is (same-key-p '(:up :ctrl) back))
      (is (eql 6 took)))))

(test a-lone-escape-is-the-escape-key-only-when-nothing-is-coming
  (let ((said (string #\Escape)))
    (is (null (tty:escape-sequence-to-key-event said 0 1 t)))
    (is (equal '(:escape) (tty:escape-sequence-to-key-event said 0 1 nil)))))

(test one-read-of-many-keys-is-many-keys
  (let ((said (format nil "hi~C[A~C[3~~~C" #\Escape #\Escape #\Return))
        (keys nil)
        (at 0))
    (loop
      (multiple-value-bind (key took)
          (tty:escape-sequence-to-key-event said at (length said) nil)
        (when (zerop took) (return))
        (push key keys)
        (incf at took)
        (when (>= at (length said)) (return))))
    (is (equal '(#\h #\i (:up) (:delete) (:enter)) (nreverse keys)))))
