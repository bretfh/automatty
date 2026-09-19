;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx)

;;; One message is a decimal byte count, a newline, and that many bytes of one
;;; s-expression in utf-8. The count is there because a poll loop may not block:
;;; reading a form straight off the descriptor would stall every pane in the
;;; server on one message that arrived in two pieces.

(defparameter +scratch+ 65536)
(defparameter +biggest+ (* 16 1024 1024))

(deftype bytes () '(simple-array (unsigned-byte 8) (*)))

(defstruct (wire (:constructor %make-wire))
  (fd -1 :type fixnum)
  (in (make-array 4096 :element-type '(unsigned-byte 8)) :type bytes)
  (have 0 :type fixnum)
  (read 0 :type fixnum)
  (out (make-array 4096 :element-type '(unsigned-byte 8)) :type bytes)
  (end 0 :type fixnum)
  (sent 0 :type fixnum)
  (scratch (make-array +scratch+ :element-type '(unsigned-byte 8)) :type bytes)
  (owner nil)
  (in-bytes 0 :type fixnum)
  (open t :type boolean))

(defun grown (vec need)
  (declare (type bytes vec) (type fixnum need))
  (if (>= (length vec) need)
      vec
      (replace (make-array (max need (* 2 (length vec)))
                           :element-type '(unsigned-byte 8))
               vec)))

(defun make-wire (fd &optional owner)
  (let ((flags (sb-posix:fcntl fd sb-posix:f-getfl)))
    (sb-posix:fcntl fd sb-posix:f-setfl (logior flags sb-posix:o-nonblock)))
  (%make-wire :fd fd :owner owner))

(defun wire-close (wire)
  "Shut the descriptor, through whoever owns it. A socket closed twice is a
descriptor number closed twice, and by the second time it may belong to
somebody else."
  (when (wire-open wire)
    (setf (wire-open wire) nil)
    (let ((owner (wire-owner wire)))
      (if owner
          (ignore-errors (sb-bsd-sockets:socket-close owner))
          (ignore-errors (sb-unix:unix-close (wire-fd wire)))))))

(defun face-said (face)
  (when face
    (list (vt:face-fg face) (vt:face-bg face)
          (vt:face-bold face) (vt:face-faint face) (vt:face-italic face)
          (vt:face-underline face) (vt:face-underline-color face)
          (vt:face-blink face)
          (vt:face-inverse face) (vt:face-conceal face) (vt:face-crossed face))))

(defun said-face (said)
  (when said
    (destructuring-bind (fg bg bold faint italic underline under-color blink
                         inverse conceal crossed)
        said
      (vt:make-face :fg fg :bg bg
                          :bold (and bold t) :faint (and faint t)
                          :italic (and italic t)
                          :underline underline :underline-color under-color
                          :blink blink
                          :inverse (and inverse t) :conceal (and conceal t)
                          :crossed (and crossed t)))))

(defun runs-said (screen runs)
  "RUNS of SCREEN as what goes on the wire: each run a row, a column and the
spans of one face in it, and the faces themselves said once for the frame.

Cells go across, never escape sequences. Only the client knows what its own
terminal can show, and a session whose escapes were settled by whoever attached
first could not be picked up from a different terminal."
  (let ((faces (make-hash-table :test 'eq))
        (table nil)
        (n 0)
        (out nil))
    (labels ((number-of (face)
               (or (gethash face faces)
                   (progn (push (face-said face) table)
                          (setf (gethash face faces) n)
                          (incf n)
                          (1- n)))))
      (dolist (run runs)
        (let ((row (tty:screen-row screen (tty:run-row run)))
              (spans nil)
              (text (make-array 16 :element-type 'character :adjustable t
                                   :fill-pointer 0))
              (face :none))
          (loop for x from (tty:run-start run) below (tty:run-end run)
                do (let ((now (vt:row-face row x)))
                     (unless (or (eq face :none) (eq face now))
                       (push (cons (number-of face) (coerce text 'simple-string))
                             spans)
                       (setf (fill-pointer text) 0))
                     (setf face now)
                     (vector-push-extend (vt:row-char row x) text)))
          (unless (eq face :none)
            (push (cons (number-of face) (coerce text 'simple-string)) spans))
          (push (list (tty:run-row run) (tty:run-start run) (nreverse spans)) out)))
      (values (nreverse out) (coerce (nreverse table) 'simple-vector)))))

(defun said-into-screen (screen said faces)
  "Put what came off the wire into SCREEN, and answer the runs it covered."
  (let ((seen (map 'simple-vector #'said-face faces)))
    (loop for (y start spans) in said
          collect (let ((row (tty:screen-row screen y))
                        (x start))
                    (dolist (span spans)
                      (let* ((face (svref seen (car span)))
                             (text (cdr span))
                             (n (length text)))
                        (replace (vt:row-chars row) text :start1 x)
                        (fill (vt:row-faces row) face :start x :end (+ x n))
                        (incf x n)))
                    (tty:make-run y start x)))))

(defun wire-send (wire form)
  (let* ((text (with-standard-io-syntax
                 (let ((*package* (find-package '#:vtx)))
                   (prin1-to-string form))))
         (bytes (sb-ext:string-to-octets text :external-format :utf-8))
         (head (sb-ext:string-to-octets (format nil "~D~C" (length bytes) #\Newline)
                                        :external-format :latin-1))
         (need (+ (wire-end wire) (length head) (length bytes))))
    (setf (wire-out wire) (grown (wire-out wire) need))
    (replace (wire-out wire) head :start1 (wire-end wire))
    (incf (wire-end wire) (length head))
    (replace (wire-out wire) bytes :start1 (wire-end wire))
    (incf (wire-end wire) (length bytes))
    wire))

(defun wire-pending (wire)
  (- (wire-end wire) (wire-sent wire)))

(defun wire-flush (wire)
  "Write what is waiting, as much of it as the descriptor took. Answers whether
it all got out."
  (let ((out (wire-out wire)))
    (loop while (< (wire-sent wire) (wire-end wire))
          do (multiple-value-bind (n errno)
                 (sb-sys:with-pinned-objects (out)
                   (sb-unix:unix-write (wire-fd wire) (sb-sys:vector-sap out)
                                       (wire-sent wire)
                                       (- (wire-end wire) (wire-sent wire))))
               (cond
                 ((and n (plusp n)) (incf (wire-sent wire) n))
                 ((or (eql errno sb-unix:eagain) (eql errno sb-unix:ewouldblock)
                      (eql errno sb-unix:eintr))
                  (return))
                 (t (wire-close wire) (return)))))
    (when (= (wire-sent wire) (wire-end wire))
      (setf (wire-end wire) 0
            (wire-sent wire) 0))
    (zerop (wire-pending wire))))

(defun wire-fill (wire)
  "Read what is there without waiting. Answers nil when the other end is gone."
  (let ((buf (wire-scratch wire))
        (got 0))
    (loop
      (multiple-value-bind (n errno)
          (sb-sys:with-pinned-objects (buf)
            (sb-unix:unix-read (wire-fd wire) (sb-sys:vector-sap buf)
                               (length buf)))
        (cond
          ((and n (plusp n))
           (setf (wire-in wire) (grown (wire-in wire) (+ (wire-have wire) n)))
           (replace (wire-in wire) buf :start1 (wire-have wire) :end2 n)
           (incf (wire-have wire) n)
           (incf (wire-in-bytes wire) n)
           (incf got n)
           (when (< n (length buf)) (return got)))
          ((eql n 0) (return nil))
          ((eql errno sb-unix:eintr))
          ((or (eql errno sb-unix:eagain) (eql errno sb-unix:ewouldblock))
           (return got))
          (t (return nil)))))))

(defvar *reading-in* nil)
(defvar *reads* 0)

(defparameter +most-names+ 4096
  "How many names a peer may make up before the package it makes them in is
thrown away and started again.")

(defun too-many-names-p (package)
  (> (loop :for s :being :the :present-symbols :of package :count s)
     +most-names+))

(defun reading-package ()
  "The package a message is read in.

Not VT/MUX: a message names symbols, and reading them where the program's own
names live lets whoever is on the other end put anything it likes there. A
package of its own, thrown away and started again once it fills, so a peer
naming something new every message cannot grow this image without end."
  (when (or (null *reading-in*)
            (and (zerop (mod (incf *reads*) 1024))
                 (too-many-names-p *reading-in*)))
    (when *reading-in* (ignore-errors (delete-package *reading-in*)))
    (setf *reading-in*
          ;; common-lisp so that T and NIL read as themselves; nothing else,
          ;; so everything a peer makes up is this package's own and goes with
          ;; it when it is thrown away
          (make-package (symbol-name (gensym "VT/WIRE")) :use '(#:common-lisp))))
  *reading-in*)

(defun wire-take (wire)
  "The next whole message, or nil when what has come in is not yet one."
  (let* ((in (wire-in wire))
         (have (wire-have wire))
         (at (wire-read wire)))
    (let ((eol (loop for i from at below have
                     when (= (aref in i) 10) return i)))
      (unless eol (return-from wire-take nil))
      (let ((size 0))
        (loop for i from at below eol
              for b = (aref in i)
              do (unless (<= 48 b 57)
                   (error "a message said its length was not a number"))
                 (setf size (+ (* 10 size) (- b 48))))
        (when (> size +biggest+)
          (error "a message said it was ~D bytes" size))
        (let ((from (1+ eol)))
          (when (< (- have from) size)
            (return-from wire-take nil))
          (let ((text (sb-ext:octets-to-string in :external-format :utf-8
                                                  :start from :end (+ from size))))
            (setf (wire-read wire) (+ from size))
            (if (= (wire-read wire) have)
                (setf (wire-have wire) 0
                      (wire-read wire) 0)
                (when (> (wire-read wire) +scratch+)
                  (replace in in :start2 (wire-read wire) :end2 have)
                  (setf (wire-have wire) (- have (wire-read wire))
                        (wire-read wire) 0)))
            (with-standard-io-syntax
              (let ((*read-eval* nil)
                    (*package* (reading-package)))
                (read-from-string text)))))))))
