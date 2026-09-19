;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

(defconstant +biggest-param+ 65535
  "The largest number a control sequence may carry.

Every parameter a terminal defines is small, and the ones that count something
are held against the screen again by whoever reads them. A ceiling is also what
lets the accumulating be fixnum arithmetic: without one the digits a program
sends are a bignum in the making, and every digit is a generic add.")

(defparameter +most-params+ 32
  "How many parameters one control sequence may carry.

Nothing a terminal defines uses more than a handful, and a terminal's input is
untrusted: without a ceiling a million semicolons is a million conses from one
escape sequence.")

(defparameter +biggest-string+ 65536
  "How much of an OSC payload is kept. A title is short and a clipboard is not,
but neither is unbounded, and nothing says a program has to send a terminator.")

(defconstant +cancel+ 24
  "CAN. A program says the control sequence it was writing is to be abandoned.")

(defconstant +substitute+ 26
  "SUB. CAN, and the terminal may show something in its place.")

(deftype param () `(integer 0 ,+biggest-param+))

(declaim (inline held-param))
(defun held-param (n)
  (if (and n (> n +biggest-param+)) +biggest-param+ n))

(declaim (inline held-digit))
(defun held-digit (had digit)
  "HAD with DIGIT put on the end, up to the ceiling and no further."
  (declare (type (or null param) had) (type (integer 0 9) digit))
  (let ((n (if had had 0)))
    (declare (type param n))
    (if (>= n +biggest-param+)
        n
        (min +biggest-param+ (+ (* n 10) digit)))))

(defun abandon-the-sequence (term)
  "The sequence is over and nothing is done about it."
  (setf (term-csi-params term) nil
        (term-csi-format term) nil
        (term-parser-state term) nil))

(declaim (inline abandons-a-sequence-p))
(defun abandons-a-sequence-p (ch)
  (let ((code (char-code ch)))
    (or (= code 27) (= code +cancel+) (= code +substitute+))))

(defun term-process-output (term string)
  (if (typep string '(simple-array character (*)))
      (%term-process-output term string)
      (%term-process-output term (coerce string '(simple-array character (*))))))

(defun %term-process-output (term string)
  (declare (type (simple-array character (*)) string))
  (let ((len (length string))
        (index 0))
    (loop while (< index len) do
      (let ((state (term-parser-state term)))
        (cond
          ((null state)
           (let ((span-start index))
             (loop while (and (< index len)
                              (let ((ch (char string index)))
                                (not (or (char<= #\Nul ch #\Us)
                                         (char= ch #\Rubout)
                                         (char= ch #\Escape)))))
                   do (incf index))
             (when (> index span-start)
               (%term-write term string span-start index))
             (when (< index len)
               (let ((ch (char string index)))
                 (incf index)
                 (case ch
                   (#\Bel (term-rang term))
                   (#\Backspace (term-cursor-left term 1))
                   (#\Tab (term-horizontal-tab term 1))
                   (#\Newline (term-line-feed term))
                   (#\Vt (term-index term))
                   (#\Page (term-index term))
                   (#\Return
                    (unless (and (< index len)
                                 (char= (char string index) #\Newline))
                      (term-carriage-return term)))
                   (#\So (setf (term-active-charset term) :g1))
                   (#\Si (setf (term-active-charset term) :g0))
                   (#\Escape
                    (setf (term-parser-state term) :read-esc)))))))

          ((eq state :read-esc)
           (let ((ch (char string index)))
             (incf index)
             (setf (term-parser-state term) nil)
             ;; only the ones that say what to read next are the state
             ;; machine's. What an escape sequence means is a method.
             (case ch
               (#\( (setf (term-parser-state term) '(:read-charset :g0)))
               (#\) (setf (term-parser-state term) '(:read-charset :g1)))
               (#\* (setf (term-parser-state term) '(:read-charset :g2)))
               (#\+ (setf (term-parser-state term) '(:read-charset :g3)))
               (#\[ (setf (term-parser-state term) :read-csi-format))
               (#\] (setf (term-parser-state term) :read-osc
                          (fill-pointer (term-osc-buf term)) 0))
               (#\P (setf (term-parser-state term) :read-dcs))
               (t (handle-esc term ch)))))

          ((and (listp state) (eq (car state) :read-charset))
           (let ((ch (char string index))
                 (slot (cadr state)))
             (incf index)
             (setf (term-parser-state term) nil)
             (let ((charset (case ch
                              (#\0 :dec-line-drawing)
                              (#\B :us-ascii)
                              (t :us-ascii))))
               (case slot
                 (:g0 (setf (term-g0 term) charset))
                 (:g1 (setf (term-g1 term) charset))
                 (:g2 (setf (term-g2 term) charset))
                 (:g3 (setf (term-g3 term) charset))))))

          ((eq state :read-csi-format)
           (let ((ch (char string index)))
             (flet ((begin (fmt)
                      (setf (term-csi-format term) fmt
                            (term-parser-state term) :read-csi-params
                            (term-csi-params term) (list nil)
                            (term-csi-length term) 1)))
               (case ch
                 (#\? (begin #\?) (incf index))
                 (#\> (begin #\>) (incf index))
                 (#\= (begin #\=) (incf index))
                 (t (begin nil))))))

          ((eq state :read-csi-params)
           (let ((ch (char string index))
                 (params (term-csi-params term)))
             (cond
               ((and (char<= #\0 ch) (char<= ch #\9))
                ;; a parameter at the ceiling stops growing. One compare a
                ;; digit, and no bignum however many a program sends.
                (let ((digit (- (char-code ch) (char-code #\0))))
                  (if (consp (car params))
                      (setf (caar params) (held-digit (caar params) digit))
                      (setf (car params) (held-digit (car params) digit))))
                (incf index))
               ((char= ch #\;)
                ;; the count is kept rather than measured: asking a list how
                ;; long it is once a separator is work that grows with what a
                ;; program sends
                (when (< (term-csi-length term) +most-params+)
                  (incf (term-csi-length term))
                  (push nil (term-csi-params term)))
                (incf index))
               ((char= ch #\:)
                (when (< (term-csi-length term) +most-params+)
                  (incf (term-csi-length term))
                  (if (consp (car params))
                      (push nil (car (term-csi-params term)))
                      (push (list nil) (term-csi-params term))))
                (incf index))
               (t
                (setf (term-parser-state term) :read-csi-function)))))

          ((eq state :read-csi-function)
           (let ((ch (char string index)))
             (cond
               ;; a control sequence is abandoned when one of these turns up
               ;; inside it, never finished. Swallowing them instead means what
               ;; follows a half-written sequence is read as text and lands on
               ;; the screen: ESC[3 ESC[2J would print "2J".
               ((abandons-a-sequence-p ch)
                (abandon-the-sequence term)
                (incf index)
                (when (char= ch #\Escape)
                  (setf (term-parser-state term) :read-esc)))
               (t
                (incf index)
                (when (and (char>= ch #\@) (char<= ch #\~))
                  (setf (term-parser-state term) nil)
                  (let ((params (nreverse (term-csi-params term)))
                        (fmt (term-csi-format term)))
                    ;; held once a sequence rather than once a digit: the
                    ;; accumulating stops at the ceiling, and this is what makes
                    ;; the ceiling exact for whoever reads the parameter
                    (do ((p params (cdr p)))
                        ((null p))
                      (if (consp (car p))
                          (setf (car p) (mapcar #'held-param (nreverse (car p))))
                          (setf (car p) (held-param (car p)))))
                    (handle-csi term ch fmt params)))))))

          ((eq state :read-osc)
           (let ((end-pos (position-if
                           (lambda (c) (or (char= c #\Bel)
                                           (char= c #\Escape)))
                           string :start index))
                 (buf (term-osc-buf term)))
             (if end-pos
                 (progn
                   (loop for i from index below end-pos
                         while (< (fill-pointer buf) +biggest-string+)
                         do (vector-push-extend (char string i) buf))
                   (setf index (1+ end-pos))
                   (if (char= (char string end-pos) #\Bel)
                       (progn (dispatch-osc term buf)
                              (setf (term-parser-state term) nil))
                       (setf (term-parser-state term) :read-osc-esc)))
                 (progn
                   (loop for i from index below len
                         while (< (fill-pointer buf) +biggest-string+)
                         do (vector-push-extend (char string i) buf))
                   (setf index len)))))

          ((eq state :read-osc-esc)
           (let ((ch (char string index)))
             (dispatch-osc term (term-osc-buf term))
             (if (char= ch #\\)
                 (progn (setf (term-parser-state term) nil)
                        (incf index))
                 (setf (term-parser-state term) :read-esc))))

          ;; the same two steps the osc states take, and for the same reason:
          ;; the terminator is two characters and they do not have to arrive in
          ;; one read. Looking for both at once means an escape at the end of a
          ;; buffer is never the terminator, and everything after it is thrown
          ;; away until some later one turns up.
          ((eq state :read-dcs)
           (let ((at (position #\Escape string :start index :end len)))
             (if at
                 (setf index (1+ at)
                       (term-parser-state term) :read-dcs-esc)
                 (setf index len))))

          ((eq state :read-dcs-esc)
           (let ((ch (char string index)))
             (if (char= ch #\\)
                 (progn (setf (term-parser-state term) nil)
                        (incf index))
                 (setf (term-parser-state term) :read-esc))))

          (t
           (setf (term-parser-state term) nil)))))))
