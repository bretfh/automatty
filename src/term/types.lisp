;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:term)

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

  (defun face-reader (name) (intern (format nil "FACE-~A" name) '#:term))

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

(defstruct (row (:constructor %make-row (chars faces)))
  "A line of the screen: the characters, and the face each one wears.

Two arrays rather than a struct per cell. A cell object costs its header, its
layout and the word the row spends pointing at it -- forty bytes to hold a
character and a pointer -- and every read of one is a chase. Side by side they
are twelve, and clearing a line is two fills."
  (chars (make-string 0) :type (simple-array character (*)))
  (faces #() :type simple-vector))

(declaim (inline row-width row-char (setf row-char) row-face (setf row-face)))

(defun row-width (row)
  (declare (type row row))
  (length (row-chars row)))

(defun row-char (row x)
  (declare (type row row) (type fixnum x))
  (schar (row-chars row) x))

(defun (setf row-char) (ch row x)
  (declare (type row row) (type fixnum x) (type character ch))
  (setf (schar (row-chars row) x) ch))

(defun row-face (row x)
  (declare (type row row) (type fixnum x))
  (svref (row-faces row) x))

(defun (setf row-face) (f row x)
  (declare (type row row) (type fixnum x))
  (setf (svref (row-faces row) x) f))

(defstruct (line (:constructor %make-line (width chars runs)))
  (width 0 :type fixnum)
  (end 0 :type fixnum)
  (count 0 :type fixnum)
  (chars "" :type simple-string)
  (runs (make-array 0 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (*))))

(defconstant +most-faces+ #xFFFF)

(defun make-osc-buf ()
  (make-array 64 :element-type 'character :adjustable t :fill-pointer 0))

(defun make-row (width)
  (%make-row (make-string width :initial-element #\Space)
             (make-array width :initial-element nil)))

(defun make-grid (width height)
  (let ((grid (make-array height :initial-element nil)))
    (dotimes (y height grid)
      (setf (aref grid y) (make-row width)))))

(defun make-tab-stops (width)
  "A stop every eighth column, which is what a terminal starts with."
  (let ((v (make-array width :element-type 'bit :initial-element 0)))
    (loop :for x :from 8 :below width :by 8 :do (setf (aref v x) 1))
    v))

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
  (keypad-application-mode nil :type boolean)
  (bracketed-paste nil :type boolean)
  (cursor-visible t :type boolean)
  (cursor-style :block)
  (origin-mode nil :type boolean)
  (newline-mode nil :type boolean)
  (reverse-video nil :type boolean)
  (reverse-wraparound nil :type boolean)
  (synchronized-output nil :type boolean)
  (mouse-mode nil :type (or null (member :normal :button-event :any-event)))
  (mouse-utf8 nil :type boolean)
  (mouse-sgr nil :type boolean)
  (mouse-urxvt nil :type boolean)
  (mouse-sgr-pixels nil :type boolean)
  (tab-stops nil :type (or null simple-bit-vector))
  (g0 :us-ascii)
  (g1 :us-ascii)
  (g2 :us-ascii)
  (g3 :us-ascii)
  (active-charset :g0)
  (scrollback nil :type (or null vector))
  (scrollback-size 0 :type fixnum)
  (scrollback-head 0 :type fixnum)
  (scrollback-pushed 0 :type fixnum)
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
  (face-now nil)
  (face-table (make-array 64 :adjustable t :fill-pointer 1 :initial-element nil) :type vector)
  (face-ids (make-hash-table) :type hash-table)
  (face-last nil)
  (face-last-id 0 :type fixnum)
  (runs (make-array 0 :element-type '(unsigned-byte 32)) :type (simple-array (unsigned-byte 32) (*))))

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
        (let ((new (or (car (assoc cur (gethash key (term-face-ids term)) :test #'face-equal))
                       (copy-face cur))))
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
        (term-tab-stops term) (make-tab-stops width)
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
  (the row (svref (the simple-vector (term-grid term)) y)))

(defun term-tab-stop-p (term x)
  (= 1 (aref (the simple-bit-vector (term-tab-stops term)) x)))

(declaim (inline blank-span move-span))

(defun blank-span (row from to &optional face)
  "Columns FROM below TO of ROW, blank and wearing FACE."
  (declare (type row row) (type fixnum from to))
  (fill (row-chars row) #\Space :start from :end to)
  (fill (row-faces row) face :start from :end to))

(defun move-span (row from to count)
  "COUNT columns of ROW starting at FROM, put to start at TO. The two may
overlap; REPLACE answers as though the source were taken first."
  (declare (type row row) (type fixnum from to count))
  (replace (row-chars row) (row-chars row)
           :start1 to :start2 from :end2 (+ from count))
  (replace (row-faces row) (row-faces row)
           :start1 to :start2 from :end2 (+ from count)))

(defun clear-row (row &optional face)
  (declare (type row row))
  (fill (row-chars row) #\Space)
  (fill (row-faces row) face))

(defun clear-grid (grid &optional face)
  (declare (type simple-vector grid))
  (dotimes (y (length grid))
    (clear-row (svref grid y) face)))


;;; What a program said, as something to do about it. Each is a generic
;;; function and each sequence is one method: (defstruct (my-term (:include
;;; term:term))) and a method of your own adds one, or changes what one already
;;; means, without touching this library.
;;;
;;; A method of your own on a term of your own beats the one here, because its
;;; first argument is the more specific. Something that wants every sequence
;;; rather than one wants :before or :around.

(defgeneric term-rang (term)
  (:documentation "The program rang the bell.")
  (:method ((term term))
    (let ((told (term-bell-fn term)))
      (when told (funcall told term)))))

(defgeneric term-titled (term title)
  (:documentation "The program said what it would like to be called.")
  (:method ((term term) title)
    (let ((told (term-title-fn term)))
      (when told (funcall told term title)))))

(defgeneric term-moved (term cwd)
  (:documentation "The program said which directory it is working in.")
  (:method ((term term) cwd)
    (let ((told (term-cwd-fn term)))
      (when told (funcall told term cwd)))))

(defgeneric term-answers (term said)
  (:documentation "The terminal is answering a question the program asked, and
SAID is what the program should read back.")
  (:method ((term term) said)
    (let ((told (term-input-fn term)))
      (when told (funcall told term said)))))

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

(defgeneric handle-hash (term final)
  (:documentation "A DEC screen sequence of the form ESC # FINAL, such as the
alignment test.")
  (:method ((term term) final)
    (declare (ignore final))
    nil))

(defgeneric term-linked (term uri params)
  (:documentation "OSC 8: the program is about to write a hyperlink, or has
finished one when URI is nil. What a hyperlink means to a display is not this
library's business; it only hands the event over.")
  (:method ((term term) uri params)
    (declare (ignore uri params))
    nil))

(defgeneric term-copied (term selection data)
  (:documentation "OSC 52: the program asked the terminal to put DATA, base64
encoded, on SELECTION.")
  (:method ((term term) selection data)
    (declare (ignore selection data))
    nil))
