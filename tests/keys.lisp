(in-package #:atty/test)

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

(test a-click-decodes-with-the-cell-it-landed-on
  (let ((said (format nil "~C[<0;10;5M" #\Escape)))
    (multiple-value-bind (event took)
        (tty:escape-sequence-to-key-event said 0 (length said) nil)
      (is (eql (length said) took))
      (is (equal '(:mouse :x 9 :y 4 :button :left :wheel nil :drag nil
                   :release nil :shift nil :meta nil :ctrl nil)
                 event)))))

(test a-release-says-so-and-a-drag-is-not-a-click
  (let ((up (format nil "~C[<0;10;5m" #\Escape))
        (drag (format nil "~C[<32;1;1M" #\Escape)))
    (is (getf (rest (tty:escape-sequence-to-key-event up)) :release))
    (is (getf (rest (tty:escape-sequence-to-key-event drag)) :drag))))

(test a-wheel-is-told-apart-from-a-button-and-keeps-its-direction
  (let ((wup (format nil "~C[<64;1;1M" #\Escape))
        (wdown (format nil "~C[<65;1;1M" #\Escape)))
    (is (eq :up (getf (rest (tty:escape-sequence-to-key-event wup)) :wheel)))
    (is (eq :down (getf (rest (tty:escape-sequence-to-key-event wdown)) :wheel)))
    (is (null (getf (rest (tty:escape-sequence-to-key-event wup)) :button)))))

(test the-legacy-x10-report-decodes-too
  (let ((said (format nil "~C[M~C~C~C" #\Escape (code-char (+ 32 0))
                      (code-char (+ 32 6)) (code-char (+ 32 3)))))
    (multiple-value-bind (event took)
        (tty:escape-sequence-to-key-event said 0 (length said) nil)
      (is (eql 6 took))
      (is (eq :left (getf (rest event) :button)))
      (is (eql 5 (getf (rest event) :x)))
      (is (eql 2 (getf (rest event) :y))))))

(test a-clicks-modifiers-come-back-the-way-a-keys-do
  (let ((shifted (format nil "~C[<4;1;1M" #\Escape)))
    (is (getf (rest (tty:escape-sequence-to-key-event shifted)) :shift))
    (is (not (getf (rest (tty:escape-sequence-to-key-event shifted)) :ctrl)))))

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
