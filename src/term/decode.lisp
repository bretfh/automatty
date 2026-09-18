;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

;;; What a byte means. A terminal reads whatever the kernel had ready, which is
;;; not whole characters, so the half of one left at the end of a read is kept
;;; here until the rest of it arrives.

(defstruct (decoder (:constructor make-decoder ()))
  (tail nil :type (or null simple-string)))

(defparameter +not-a-character+ (code-char #xFFFD))

(declaim (inline utf-8-length))
(defun utf-8-length (byte)
  (declare (type (unsigned-byte 8) byte))
  (cond ((< byte #x80) 1)
        ;; #xC0 and #xC1 can only begin an overlong, and anything past #xF4
        ;; can only begin a character there is no room for
        ((< byte #xC2) 0)
        ((< byte #xE0) 2)
        ((< byte #xF0) 3)
        ((< byte #xF5) 4)
        (t 0)))

(defun plain-p (said)
  "Whether every byte in SAID is one a character all of its own."
  (declare (type simple-string said))
  (loop for i of-type fixnum from 0 below (length said)
        always (< (char-code (schar said i)) #x80)))

(defun decode-utf-8 (decoder said)
  "SAID, whose characters are the bytes a program wrote, as the characters those
bytes mean.

Anything that is not utf-8 comes back as the character that says so -- one per
sequence rather than one per byte, so a character cut short does not swallow the
one after it -- and a terminal goes on showing what it got rather than stopping.

Bytes that are already characters are answered as they came, the same string and
not a copy of it, because that is nearly everything a program ever writes."
  (let ((said (if (typep said 'simple-string) said (coerce said 'simple-string))))
    (if (and (null (decoder-tail decoder)) (plain-p said))
        said
        (%decode-utf-8 decoder said))))

(defun %decode-utf-8 (decoder said)
  (let* ((tail (decoder-tail decoder))
         (bytes (if tail (concatenate 'simple-string tail said) said))
         (n (length bytes))
         (out (make-string n))
         (at 0)
         (i 0))
    (declare (type fixnum n i at) (type simple-string out))
    (setf (decoder-tail decoder) nil)
    (loop while (< i n)
          do (let* ((b (char-code (char bytes i)))
                    (took (utf-8-length b)))
               (declare (type fixnum b took))
               (cond
                 ((= took 1) (setf (schar out at) (char bytes i)) (incf at) (incf i))
                 ((zerop took) (setf (schar out at) +not-a-character+) (incf at) (incf i))
                 ((> (+ i took) n)
                  (setf (decoder-tail decoder)
                        (coerce (subseq bytes i n) 'simple-string)
                        i n))
                 (t
                  ;; as much of the sequence as is really there is one thing
                  ;; that went wrong, not one per byte of it
                  (let ((code (logand b (case took (2 #x1F) (3 #x0F) (t #x07))))
                        (k 1))
                    (declare (type fixnum code k))
                    (loop while (< k took)
                          do (let ((c (char-code (char bytes (+ i k)))))
                               (declare (type fixnum c))
                               (unless (= #x80 (logand c #xC0)) (return))
                               (setf code (logior (ash code 6) (logand c #x3F)))
                               (incf k)))
                    (if (and (= k took)
                             (not (<= #xD800 code #xDFFF))
                             (>= code (case took (2 #x80) (3 #x800) (t #x10000))))
                        (progn (setf (schar out at) (code-char code)) (incf at) (incf i took))
                        (progn (setf (schar out at) +not-a-character+) (incf at) (incf i k))))))))
    (if (= at n) out (subseq out 0 at))))
