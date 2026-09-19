;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +face-slots+
    '((fg :color) (bg :color)
      (bold :flag) (faint :flag) (italic :flag) (underline :said)
      (inverse :flag) (conceal :flag) (crossed :flag)
      (underline-color :color) (blink :said))
    "What a face carries, and how two of them are told apart.

A colour is an index or three numbers, a flag is a boolean the struct says is
one, and the rest are whatever the escape said. Everything mechanical about a
face is written out from this: making another, clearing one, telling two apart.
So a twelfth attribute is one line here rather than five edits in four files
with nothing to say any of them was missed.

The order is the order two faces are compared in, and it is not arbitrary. Every
SGR change looks its face up in a cache by comparing it against what is there, so
the attributes that differ most often go first and the ones almost nothing sets
go last, where the comparison usually never reaches them.")

  (defun face-reader (name) (intern (format nil "FACE-~A" name) '#:vt))

  (defun face-initarg (name) (intern (symbol-name name) '#:keyword)))

(declaim (inline color-equal))
(defun color-equal (a b)
  "Whether two colours are the same one. A colour is nil, an index, or three
numbers, and the first two are what nearly every comparison is between."
  (or (eql a b)
      (and (consp a) (consp b)
           (eql (first a) (first b))
           (eql (second a) (second b))
           (eql (third a) (third b)))))

(macrolet
    ((a-face ()
       (flet ((same (name how)
                (if (eq how :color)
                    `(color-equal (,(face-reader name) a) (,(face-reader name) b))
                    `(eq (,(face-reader name) a) (,(face-reader name) b)))))
         `(progn
            (defstruct (face (:copier nil) (:conc-name face-))
              ,@(loop :for (name how) :in +face-slots+
                      :collect (if (eq how :flag)
                                   `(,name nil :type boolean)
                                   `(,name nil))))

            (defun copy-face (src)
              (make-face
               ,@(loop :for (name nil) :in +face-slots+
                       :append (list (face-initarg name)
                                     `(,(face-reader name) src)))))

            (defun face-into (into from)
              "Make INTO carry what FROM carries, without making another face."
              (setf ,@(loop :for (name nil) :in +face-slots+
                            :append `((,(face-reader name) into)
                                      (,(face-reader name) from))))
              into)

            (defun reset-face (face)
              (setf ,@(loop :for (name nil) :in +face-slots+
                            :append `((,(face-reader name) face) nil))))

            (defun face-default-p (f)
              "Whether F is the face a cell nothing has written to already has. A
cleared cell holds nil and a cell written under a fresh SGR 0 holds a face of
all defaults; they draw the same, so nothing that compares faces may call them
different."
              (or (null f)
                  (and ,@(loop :for (name nil) :in +face-slots+
                               :collect `(null (,(face-reader name) f))))))

            (defun face-equal (a b)
              (cond
                ((and (null a) (null b)) t)
                ((or (null a) (null b)) (and (face-default-p a) (face-default-p b)))
                (t (and ,@(loop :for (name how) :in +face-slots+
                                :collect (same name how))))))))))
  (a-face))

(defconstant +faces-kept+ 16
  "How many faces a term keeps shared. A power of two, so walking the ring is a
mask rather than a division.")

(defstruct (cell (:constructor make-cell (&optional char face)))
  (char #\Space :type character)
  (face nil :type (or null face)))

(defun make-osc-buf ()
  (make-array 64 :element-type 'character :adjustable t :fill-pointer 0))

(defun make-row (width)
  (let ((row (make-array width :initial-element nil)))
    (dotimes (x width row)
      (setf (aref row x) (make-cell)))))

(defun make-grid (width height)
  (let ((grid (make-array height :initial-element nil)))
    (dotimes (y height grid)
      (setf (aref grid y) (make-row width)))))

(defstruct (term (:constructor %make-term))
  (width 80 :type fixnum)
  (height 24 :type fixnum)
  (grid nil :type (or null simple-vector))
  (main-grid nil :type (or null simple-vector))
  (cursor-x 0 :type fixnum)
  (cursor-y 0 :type fixnum)
  (saved-cursor-x 0 :type fixnum)
  (saved-cursor-y 0 :type fixnum)
  (saved-attrs nil :type (or null face))
  ;; the alt screen keeps its own, the way a real terminal does: 1049 saves on
  ;; the main screen before it leaves and restores it after it comes back, and
  ;; an ESC7 the program does while it is over there must not land on top of
  ;; that
  (alt-saved-cursor-x 0 :type fixnum)
  (alt-saved-cursor-y 0 :type fixnum)
  (alt-saved-attrs nil :type (or null face))
  (attrs nil :type (or null face))
  (scroll-top 0 :type fixnum)
  (scroll-bottom 23 :type fixnum)
  (parser-state nil)
  (csi-params nil :type list)
  (csi-format nil)
  (osc-buf (make-osc-buf) :type string)
  ;; how many parameters the control sequence being read has so far, kept
  ;; rather than measured
  (csi-length 0 :type fixnum)
  (auto-margin t :type boolean)
  ;; the last column is written and the next character belongs on the next
  ;; line, but the cursor has not gone there and will not unless one arrives.
  ;; It sits on the last column, which is where a terminal shows it and where
  ;; anything that moves or erases from it must start.
  (wrap-pending nil :type boolean)
  (insert-mode nil :type boolean)
  (keypad-mode nil :type boolean)
  (bracketed-paste nil :type boolean)
  (cursor-visible t :type boolean)
  (cursor-style :block)
  (g0 :us-ascii)
  (g1 :us-ascii)
  (g2 :us-ascii)
  (g3 :us-ascii)
  (active-charset :g0)
  (scrollback nil :type (or null vector))
  (scrollback-size 0 :type fixnum)
  (scrollback-head 0 :type fixnum)
  (max-scrollback 10000 :type fixnum)
  (input-fn nil)
  (bell-fn nil)
  (title-fn nil)
  (cwd-fn nil)
  (title "" :type string)
  (cwd "" :type string)
  (last-char #\Space :type character)
  (in-alt-screen nil :type boolean)
  (face-cache (make-array +faces-kept+ :initial-element nil) :type simple-vector)
  (face-keys (make-array +faces-kept+ :element-type 'fixnum :initial-element 0)
             :type (simple-array fixnum (*)))
  (face-cache-pos 0 :type fixnum)
  (face-now nil))

(declaim (inline color-key face-key))

(defun color-key (c)
  (cond ((null c) 0)
        ((integerp c) (1+ c))
        ((consp c) (+ 300 (ash (the (integer 0 255) (first c)) 16)
                      (ash (the (integer 0 255) (second c)) 8)
                      (the (integer 0 255) (third c))))
        (t 0)))

(defun face-key (f)
  "A fixnum two equal faces share. Different keys are different faces; the same
key still has to be checked. A miss in the cache is then a run of fixnum
compares rather than a run of eleven-field ones."
  (declare (type face f))
  (let ((flags (logior (if (face-bold f) 1 0)
                       (if (face-faint f) 2 0)
                       (if (face-italic f) 4 0)
                       (if (face-inverse f) 8 0)
                       (if (face-conceal f) 16 0)
                       (if (face-crossed f) 32 0)
                       (if (face-blink f) 64 0)
                       (if (face-underline f) 128 0))))
    (declare (type fixnum flags))
    (logand most-positive-fixnum
            (+ flags
               (* 257 (the fixnum (color-key (face-fg f))))
               (* 65537 (the fixnum (color-key (face-bg f))))))))

(defun intern-face (term)
  "A shared face equal to TERM's current attrs. A small ring cache keeps
the working set of faces shared, so SGR-heavy output stops copying a struct per
color change, and the answer stands until something says an attribute moved."
  (or (term-face-now term)
      (setf (term-face-now term) (%intern-face term))))

(defun %intern-face (term)
  ;; searched newest first. A program works in a handful of faces at a time and
  ;; comes back to the one it just used, so from the other end the answer is
  ;; usually the first or second thing looked at rather than the last.
  (let* ((cur (term-attrs term))
         (cache (term-face-cache term))
         (keys (term-face-keys term))
         (pos (term-face-cache-pos term))
         (key (face-key cur)))
    (declare (type simple-vector cache)
             (type (simple-array fixnum (*)) keys)
             (type fixnum pos key))
    (or (loop :for i :of-type fixnum :from 1 :to +faces-kept+
              :for at :of-type fixnum := (logand (- pos i) (1- +faces-kept+))
              :when (and (= key (aref keys at))
                         (svref cache at)
                         (face-equal (svref cache at) cur))
                :return (svref cache at))
        (let ((new (copy-face cur)))
          (setf (svref cache pos) new
                (aref keys pos) key
                (term-face-cache-pos term) (logand (1+ pos)
                                                   (1- +faces-kept+)))
          new))))

(defun init-term (term &key (width 80) (height 24)
                            input-fn bell-fn title-fn cwd-fn
                            (max-scrollback 10000))
  "Fill TERM in and answer it.

A term of your own is a struct that includes this one, and a struct's
constructor is its own: make it with yours, then hand it here. MAKE-TERM is
this and a plain term."
  (setf (term-width term) width
        (term-height term) height
        (term-grid term) (make-grid width height)
        (term-attrs term) (make-face)
        (term-saved-attrs term) (make-face)
        (term-alt-saved-attrs term) (make-face)
        (term-scroll-top term) 0
        (term-scroll-bottom term) (1- height)
        (term-input-fn term) input-fn
        (term-bell-fn term) bell-fn
        (term-title-fn term) title-fn
        (term-cwd-fn term) cwd-fn
        (term-max-scrollback term) max-scrollback
        (term-scrollback term) (when (plusp max-scrollback)
                                 (make-array 64 :initial-element nil)))
  term)

(defun make-term (&rest args)
  (apply #'init-term (%make-term) args))

(defun term-grid-row (term y)
  (the simple-vector (aref (the simple-vector (term-grid term)) y)))

(defun clear-row (row &optional face)
  (declare (type simple-vector row))
  (loop :for x :of-type fixnum :from 0 :below (length row)
        :do (let ((c (svref row x)))
              (setf (cell-char c) #\Space
                    (cell-face c) face))))

(defun clear-grid (grid &optional face)
  (declare (type simple-vector grid))
  (dotimes (y (length grid))
    (clear-row (the simple-vector (aref grid y)) face)))


;;; What a program said, as something to do about it. Each is a generic
;;; function and each sequence is one method: (defstruct (my-term (:include
;;; vt:term))) and a method of your own adds one, or changes what one already
;;; means, without touching this library.
;;;
;;; A method of your own on a term of your own beats the one here, because its
;;; first argument is the more specific. Something that wants every sequence
;;; rather than one wants :before or :around.

(defgeneric handle-csi (term final format params)
  (:documentation "A control sequence ending in FINAL. FORMAT is the private
marker it carried, one of #\\? #\\> #\\= or nil, and PARAMS its numbers.")
  (:method ((term term) final format params)
    (declare (ignore final format params))
    nil))

(defgeneric handle-esc (term final)
  (:documentation "An escape sequence of one character, such as ESC 7.")
  (:method ((term term) final)
    (declare (ignore final))
    nil))

(defgeneric handle-osc (term code said)
  (:documentation "An operating system command: its number, and the rest of
what it carried.")
  (:method ((term term) code said)
    (declare (ignore code said))
    nil))

(defgeneric handle-mode (term number privatep set)
  (:documentation "A mode set or reset. PRIVATEP is whether it came with the
DEC marker, so mode 4 and mode ?4 are two different modes.")
  (:method ((term term) number privatep set)
    (declare (ignore number privatep set))
    nil))
