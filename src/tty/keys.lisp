;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/tty)

;;; What a terminal sent, as the key somebody pressed. This is the host's half:
;;; an abstract terminal never receives these bytes, it only ever produces them
;;; (term:key-event-to-escape-sequence is the other direction and lives there).

(defun modifier-mods (code)
  (let ((bits (1- (max 1 (or code 1))))
        (mods nil))
    (when (logbitp 2 bits) (push :ctrl mods))
    (when (logbitp 1 bits) (push :meta mods))
    (when (logbitp 0 bits) (push :shift mods))
    mods))

(defparameter +tilde-keys+
  '((2 . :insert) (3 . :delete) (5 . :page-up) (6 . :page-down)
    (15 . :f5) (17 . :f6) (18 . :f7) (19 . :f8)
    (20 . :f9) (21 . :f10) (23 . :f11) (24 . :f12)))

(defun arrow-of (ch)
  (case ch (#\A :up) (#\B :down) (#\C :right) (#\D :left)
        (#\H :home) (#\F :end)))

(defun plain-key (ch)
  (case ch
    (#\Return '(:enter))
    (#\Tab '(:tab))
    (#\Rubout '(:backspace))
    (#\Backspace '(:backspace :ctrl))
    (t ch)))

(defun read-csi-params (said start end)
  "The numbers between ESC [ and the byte that ends it. Answers the numbers, the
final character and where it was, or nil when there is not yet an end to it."
  (let ((params nil)
        (n nil)
        (i start))
    (loop
      (when (>= i end) (return nil))
      (let ((ch (char said i)))
        (cond
          ((digit-char-p ch) (setf n (+ (* 10 (or n 0)) (digit-char-p ch))))
          ((char= ch #\;) (push n params) (setf n nil))
          ((char< ch #\@) nil)
          (t (push n params)
             (return (values (nreverse params) ch i)))))
      (incf i))))

(defun mouse-event (cb x y release)
  (list :mouse :x x :y y
        :button (unless (logbitp 6 cb) (case (logand cb 3) (0 :left) (1 :middle) (2 :right)))
        :wheel (when (logbitp 6 cb) (case (logand cb 3) (0 :up) (1 :down) (2 :left) (t :right)))
        :drag (logbitp 5 cb)
        :release release
        :shift (logbitp 2 cb) :meta (logbitp 3 cb) :ctrl (logbitp 4 cb)))

(defun sgr-mouse-event (params final took)
  (if (= (length params) 3)
      (destructuring-bind (cb cx cy) params
        (values (mouse-event (or cb 0) (1- (or cx 1)) (1- (or cy 1)) (char= final #\m))
                took))
      (values nil took)))

(defun x10-mouse-event (said start end)
  "ESC [ M Cb Cx Cy, the older report a terminal that never learned SGR still
sends: three raw bytes, each the real value plus 32, right after the M."
  (if (< (+ start 3) end)
      (let ((cb (- (char-code (char said (1+ start))) 32))
            (cx (- (char-code (char said (+ start 2))) 32))
            (cy (- (char-code (char said (+ start 3))) 32)))
        (values (mouse-event cb (1- cx) (1- cy) (= 3 (logand cb 3)))
                6))
      (values nil 0)))

(defun csi-key (said start end)
  (when (and (< start end) (char= (char said start) #\M))
    (return-from csi-key (x10-mouse-event said start end)))
  (multiple-value-bind (params final at) (read-csi-params said start end)
    (unless final (return-from csi-key (values nil 0)))
    (let* ((took (- (1+ at) (- start 2)))
           (arrow (arrow-of final)))
      (cond
        (arrow (values (list* arrow (modifier-mods (second params))) took))
        ((char= final #\Z) (values '(:tab :shift) took))
        ((find final "PQRS")
         (values (list* (aref #(:f1 :f2 :f3 :f4) (position final "PQRS"))
                        (modifier-mods (second params)))
                 took))
        ((char= final #\~)
         (let ((key (cdr (assoc (or (first params) 0) +tilde-keys+))))
           (if key
               (values (list* key (modifier-mods (second params))) took)
               (values nil took))))
        ((find final "Mm") (sgr-mouse-event params final took))
        (t (values nil took))))))

(defun ss3-key (said start end)
  (when (>= start end) (return-from ss3-key (values nil 0)))
  (let* ((ch (char said start))
         (arrow (arrow-of ch)))
    (cond
      (arrow (values (list arrow) 3))
      ((find ch "PQRS")
       (values (list (aref #(:f1 :f2 :f3 :f4) (position ch "PQRS"))) 3))
      (t (values nil 3)))))

(defun escape-sequence-to-key-event (said &optional (start 0) (end (length said))
                                               (more t))
  "The key at START of SAID, and how many characters it took.

Answers (values nil 0) when what is there is the beginning of something longer
and the rest has not arrived. MORE says whether more is coming: a lone escape is
either the escape key or the start of a sequence, and only a clock can tell
which, so whoever has one says so."
  (when (>= start end)
    (return-from escape-sequence-to-key-event (values nil 0)))
  (let ((ch (char said start)))
    (cond
      ((char/= ch #\Escape) (values (plain-key ch) 1))
      ((>= (1+ start) end)
       (if more (values nil 0) (values '(:escape) 1)))
      (t
       (let ((next (char said (1+ start))))
         (case next
           (#\[ (csi-key said (+ start 2) end))
           (#\O (ss3-key said (+ start 2) end))
           (#\Rubout (values '(:backspace :meta) 2))
           (#\Backspace (values '(:backspace :ctrl :meta) 2))
           (t (values '(:escape) 1))))))))
