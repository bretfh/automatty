;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

;;; Modes a program turns on and off. A mode is a number and whether it came
;;; with the DEC marker, so mode 4 and mode ?4 are two different modes and get
;;; two methods.

(defmethod handle-csi ((term term) (final (eql #\h)) format params)
  (dolist (p params)
    (when p (handle-mode term p (eql format #\?) t))))

(defmethod handle-csi ((term term) (final (eql #\l)) format params)
  (dolist (p params)
    (when p (handle-mode term p (eql format #\?) nil))))

(defmethod handle-mode ((term term) (number (eql 4)) (privatep null) set)
  (setf (term-insert-mode term) set))

(defmethod handle-mode ((term term) (number (eql 1)) (privatep (eql t)) set)
  (setf (term-keypad-mode term) set))

(defmethod handle-mode ((term term) (number (eql 7)) (privatep (eql t)) set)
  (setf (term-auto-margin term) set))

(defmethod handle-mode ((term term) (number (eql 3)) (privatep (eql t)) set)
  ;; DECCOLM. Switching columns is defined to clear the screen and home the
  ;; cursor along with it, and ED itself no longer does the homing.
  (term-resize term (if set 132 80) (term-height term))
  (term-erase-in-display term 2)
  (term-goto term 1 1))

(defmethod handle-mode ((term term) (number (eql 5)) (privatep (eql t)) set)
  (setf (term-reverse-video term) set))

(defmethod handle-mode ((term term) (number (eql 6)) (privatep (eql t)) set)
  (setf (term-origin-mode term) set)
  (term-goto term 1 1))

(defmethod handle-mode ((term term) (number (eql 20)) (privatep null) set)
  (setf (term-newline-mode term) set))

(defmethod handle-mode ((term term) (number (eql 45)) (privatep (eql t)) set)
  (setf (term-reverse-wraparound term) set))

(defmethod handle-mode ((term term) (number (eql 2026)) (privatep (eql t)) set)
  (setf (term-synchronized-output term) set))

(defmethod handle-mode ((term term) (number (eql 47)) (privatep (eql t)) set)
  (if set (term-enter-alt-screen term) (term-exit-alt-screen term)))

(defmethod handle-mode ((term term) (number (eql 1000)) (privatep (eql t)) set)
  (setf (term-mouse-mode term) (and set :normal)))

(defmethod handle-mode ((term term) (number (eql 1002)) (privatep (eql t)) set)
  (setf (term-mouse-mode term) (and set :button-event)))

(defmethod handle-mode ((term term) (number (eql 1003)) (privatep (eql t)) set)
  (setf (term-mouse-mode term) (and set :any-event)))

(defmethod handle-mode ((term term) (number (eql 1005)) (privatep (eql t)) set)
  (setf (term-mouse-utf8 term) set))

(defmethod handle-mode ((term term) (number (eql 1006)) (privatep (eql t)) set)
  (setf (term-mouse-sgr term) set))

(defmethod handle-mode ((term term) (number (eql 1015)) (privatep (eql t)) set)
  (setf (term-mouse-urxvt term) set))

(defmethod handle-mode ((term term) (number (eql 1016)) (privatep (eql t)) set)
  (setf (term-mouse-sgr-pixels term) set))

(defparameter +blinking-cursors+
  '((:block . :blinking-block)
    (:underline . :blinking-underline)
    (:bar . :blinking-bar)))

(defmethod handle-mode ((term term) (number (eql 12)) (privatep (eql t)) set)
  (let ((had (term-cursor-style term)))
    (setf (term-cursor-style term)
          (if set
              (or (cdr (assoc had +blinking-cursors+)) had)
              (or (car (rassoc had +blinking-cursors+)) had)))))

(defmethod handle-mode ((term term) (number (eql 25)) (privatep (eql t)) set)
  (setf (term-cursor-visible term) set))

(defmethod handle-mode ((term term) (number (eql 1047)) (privatep (eql t)) set)
  (if set (term-enter-alt-screen term) (term-exit-alt-screen term)))

(defmethod handle-mode ((term term) (number (eql 1048)) (privatep (eql t)) set)
  (if set (term-save-cursor term) (term-restore-cursor term)))

(defmethod handle-mode ((term term) (number (eql 1049)) (privatep (eql t)) set)
  (if set
      (progn (term-save-cursor term) (term-enter-alt-screen term))
      (progn (term-exit-alt-screen term) (term-restore-cursor term))))

(defmethod handle-mode ((term term) (number (eql 2004)) (privatep (eql t)) set)
  (setf (term-bracketed-paste term) set))

(defmethod handle-esc ((term term) (final (eql #\=)))
  (setf (term-keypad-application-mode term) t))

(defmethod handle-esc ((term term) (final (eql #\>)))
  (setf (term-keypad-application-mode term) nil))

(defmethod handle-csi ((term term) (final (eql #\p)) format params)
  (declare (ignore format params))
  (term-soft-reset term))
