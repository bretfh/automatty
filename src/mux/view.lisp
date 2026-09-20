;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx)

;;; A pane is a widget like any other. What that buys is the layout pass: a
;;; split is a row or a column of these, the bar is the last child of the
;;; column they sit in, and the arithmetic that hands each of them its rows and
;;; columns is the one src/ui/layout.lisp already does for everything else.

(defclass pane-view (vtx/ui:widget)
  ((pane :initarg :pane :reader view-pane)))

(defun pane-view (pane &rest props)
  (apply #'make-instance 'pane-view :pane pane :expand 1 props))

(defmethod vtx/ui:measure ((w pane-view) m aw ah)
  "Nothing of its own, and it grows. A program has no size it wants: it is given
one and told what it is, so what it asks for is the room left over."
  (declare (ignore m aw ah))
  (values 0 0))

(defmethod vtx/ui:paint ((w pane-view) (m vtx/cells:cells))
  (vtx/cells:blit m (pane-term (view-pane w))
                 (vtx/ui:left w) (vtx/ui:top w)
                 (vtx/ui:width w) (vtx/ui:height w)))

(defmethod vtx/ui:under ((w pane-view) line col)
  "A pane-view covers whatever it was laid out to, same test the click-through
widgets already use, just answering with itself rather than an action to run."
  (when (and (<= (vtx/ui:top w) line) (< line (vtx/ui:bottom w))
             (<= (vtx/ui:left w) col) (< col (vtx/ui:right w)))
    w))

;;; A pane sits inside a frame of its own, all four sides, lit one colour when
;;; it has the focus and another when it does not.

(defun pane-frame (pane focusp)
  (vtx/ui:framed (pane-view pane)
                 :face (if focusp :border-active :border-inactive)))

(defun views-in (tree)
  "Every pane-view in TREE, in the order they were put there."
  (let ((out nil))
    (labels ((walk (w)
               (when (typep w 'pane-view) (push w out))
               (dolist (part (vtx/ui:parts w)) (walk part))))
      (walk tree))
    (nreverse out)))

(defun view-of (tree pane)
  (find pane (views-in tree) :key #'view-pane))

;;; How the panes in a session are arranged. A split is a way and the things
;;; laid out that way, each of which is a pane or another split, so any
;;; arrangement is a tree and any tree becomes a row or column of widgets.

(defstruct (split (:constructor make-split (way parts)))
  (way :across)
  (parts nil :type list))

(defun panes-in (it)
  "Every pane under IT, left to right and top to bottom. A layout with nothing
left in it holds no panes, which is not the same as holding one that is nothing."
  (cond ((null it) nil)
        ((split-p it)
         (loop :for part :in (split-parts it) :append (panes-in part)))
        (t (list it))))

(defun put-beside (it pane way new)
  "IT with NEW put beside PANE, the way given."
  (cond ((eq it pane) (make-split way (list pane new)))
        ((split-p it)
         (setf (split-parts it)
               (mapcar (lambda (p) (put-beside p pane way new)) (split-parts it)))
         it)
        (t it)))

(defun without-pane (it pane)
  "IT with PANE taken out, or nothing when that leaves nothing. A split down to
one part is that part: nobody wants a border around a single pane."
  (cond ((eq it pane) nil)
        ((split-p it)
         (let ((kept (remove nil (mapcar (lambda (p) (without-pane p pane))
                                         (split-parts it)))))
           (cond ((null kept) nil)
                 ((null (rest kept)) (first kept))
                 (t (setf (split-parts it) kept) it))))
        (t it)))

(defun layout-tree (it &optional focus)
  "IT as widgets: a pane alone is a view with nothing around it, since there is
nothing for a border to tell it apart from. A split is a row or a column of
panes each in a frame of its own, the one with the focus lit."
  (if (split-p it) (framed-tree it focus) (pane-view it)))

(defun framed-tree (it focus)
  (if (split-p it)
      (apply (if (eq (split-way it) :across) #'vtx/ui:row #'vtx/ui:column)
             :align :stretch :spacing 0 :expand 1
             (mapcar (lambda (part) (framed-tree part focus)) (split-parts it)))
      (pane-frame it (eql it focus))))
