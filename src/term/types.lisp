;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(declaim (optimize (speed 3) (safety 1)))

(defstruct (face-attrs (:copier nil) (:conc-name face-))
  (fg nil)
  (bg nil)
  (bold nil :type boolean)
  (faint nil :type boolean)
  (italic nil :type boolean)
  (underline nil)
  (underline-color nil)
  (blink nil)
  (inverse nil :type boolean)
  (conceal nil :type boolean)
  (crossed nil :type boolean))

(defun copy-face-attrs (src)
  (make-face-attrs :fg (face-fg src)
                   :bg (face-bg src)
                   :bold (face-bold src)
                   :faint (face-faint src)
                   :italic (face-italic src)
                   :underline (face-underline src)
                   :underline-color (face-underline-color src)
                   :blink (face-blink src)
                   :inverse (face-inverse src)
                   :conceal (face-conceal src)
                   :crossed (face-crossed src)))

(defstruct (cell (:constructor make-cell (&optional char face)))
  (char #\Space :type character)
  (face nil :type (or null face-attrs)))

(declaim (inline color-equal))
(defun color-equal (a b)
  "Whether two colours are the same one. A colour is nil, an index, or three
numbers, and the first two are what nearly every comparison is between."
  (or (eql a b)
      (and (consp a) (consp b)
           (eql (first a) (first b))
           (eql (second a) (second b))
           (eql (third a) (third b)))))

(defun face-default-p (f)
  "Whether F is the face a cell nothing has written to already has. A cleared
cell holds nil and a cell written under a fresh SGR 0 holds a face of all
defaults; they draw the same, so nothing that compares faces may call them
different."
  (or (null f)
      (and (null (face-fg f)) (null (face-bg f))
           (not (face-bold f)) (not (face-faint f)) (not (face-italic f))
           (null (face-underline f)) (null (face-underline-color f))
           (null (face-blink f))
           (not (face-inverse f)) (not (face-conceal f))
           (not (face-crossed f)))))

(defun face-attrs-equal (a b)
  (cond
    ((and (null a) (null b)) t)
    ((or (null a) (null b)) (and (face-default-p a) (face-default-p b)))
    (t (and (color-equal (face-fg a) (face-fg b))
            (color-equal (face-bg a) (face-bg b))
            (eq (face-bold a) (face-bold b))
            (eq (face-faint a) (face-faint b))
            (eq (face-italic a) (face-italic b))
            (eq (face-underline a) (face-underline b))
            (eq (face-inverse a) (face-inverse b))
            (eq (face-conceal a) (face-conceal b))
            (eq (face-crossed a) (face-crossed b))
            (color-equal (face-underline-color a) (face-underline-color b))
            (eq (face-blink a) (face-blink b))))))

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
  (alt-grid nil :type (or null simple-vector))
  (main-grid nil :type (or null simple-vector))
  (cursor-x 0 :type fixnum)
  (cursor-y 0 :type fixnum)
  (saved-cursor-x 0 :type fixnum)
  (saved-cursor-y 0 :type fixnum)
  (saved-attrs nil :type (or null face-attrs))
  (attrs nil :type (or null face-attrs))
  (scroll-top 0 :type fixnum)
  (scroll-bottom 23 :type fixnum)
  (parser-state nil)
  (csi-params nil :type list)
  (csi-format nil)
  (osc-buf (make-osc-buf) :type string)
  (auto-margin t :type boolean)
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
  (face-cache (make-array 16 :initial-element nil) :type simple-vector)
  (face-cache-pos 0 :type fixnum)
  (face-now nil))

(defun intern-face (term)
  "A shared face-attrs equal to TERM's current attrs. A small ring cache keeps
the working set of faces shared, so SGR-heavy output stops copying a struct per
color change, and the answer stands until something says an attribute moved."
  (or (term-face-now term)
      (setf (term-face-now term) (%intern-face term))))

(defun %intern-face (term)
  (let ((cur (term-attrs term))
        (cache (term-face-cache term)))
    (or (loop for i from 0 below (length cache)
              for f = (svref cache i)
              when (and f (face-attrs-equal f cur)) return f)
        (let ((new (copy-face-attrs cur))
              (pos (term-face-cache-pos term)))
          (setf (svref cache pos) new
                (term-face-cache-pos term) (mod (1+ pos) (length cache)))
          new))))

(defun make-term (&key (width 80) (height 24)
                       input-fn bell-fn title-fn cwd-fn
                       (max-scrollback 10000))
  (%make-term :width width
              :height height
              :grid (make-grid width height)
              :attrs (make-face-attrs)
              :saved-attrs (make-face-attrs)
              :scroll-top 0
              :scroll-bottom (1- height)
              :input-fn input-fn
              :bell-fn bell-fn
              :title-fn title-fn
              :cwd-fn cwd-fn
              :max-scrollback max-scrollback
              :scrollback (when (plusp max-scrollback)
                            (make-array 64 :initial-element nil))))

(defun term-grid-row (term y)
  (the simple-vector (aref (the simple-vector (term-grid term)) y)))

(defun grid-cell (term x y)
  (aref (aref (term-grid term) y) x))

(defun (setf grid-cell) (cell term x y)
  (setf (aref (aref (term-grid term) y) x) cell))

(defun clear-row (row &optional face)
  (declare (type simple-vector row))
  (dotimes (x (length row))
    (let ((c (aref row x)))
      (setf (cell-char c) #\Space
            (cell-face c) face))))

(defun clear-grid (grid &optional face)
  (declare (type simple-vector grid))
  (dotimes (y (length grid))
    (clear-row (the simple-vector (aref grid y)) face)))

