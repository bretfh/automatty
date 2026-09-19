;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx)

;;; The bar is what the session looks like rather than what any one person is
;;; doing, so the server composes it and it crosses the wire as cells like
;;; everything else. Everyone attached sees the same one.

(defun bar-face (role)
  (vtx/ui:unhex (vtx/ui:color role)))

(defun clock-says ()
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (format nil "~2,'0D:~2,'0D" hour minute)))

(defun shortened (command)
  "What to call a program on a bar. A shell out of the store is a path nobody
reads to the end, and its name is the last thing in it."
  (let* ((said (string-trim " " (or command "")))
         (space (position #\Space said))
         (first-word (subseq said 0 (or space (length said))))
         (slash (position #\/ first-word :from-end t))
         (name (if slash (subseq first-word (1+ slash)) first-word)))
    (if space
        (concatenate 'string name (subseq said space))
        name)))

(defun pane-says (pane)
  (or (and (pane-named pane) (plusp (length (pane-named pane))) (pane-named pane))
      (shortened (pane-command pane))
      ""))

(defun default-bar (session)
  "What the bar shows. Rebind *BAR* to a function of the session answering
another tree, and it is another bar."
  (vtx/ui:row
   :spacing 1
   :background-color (bar-face :bg-alt)
   (vtx/ui:label (format nil " ~A " (session-name session)) :face :accent)
   (vtx/ui:label (pane-says (session-focus session)))
   (vtx/ui:gap)
   (vtx/ui:label (if (pane-running (session-focus session)) "" "done") :face :warning)
   (vtx/ui:label (let ((term (session-pane-term session)))
                  (format nil "~Dx~D" (vt:term-width term) (vt:term-height term))))
   (vtx/ui:label (clock-says))
   (vtx/ui:label " ")))

(defvar *bar* #'default-bar)

(defun session-bar (session)
  "The bar for SESSION, or nothing when it is turned off. It is the last child
of the column the panes are in, so how many rows it takes is whatever it
measures to rather than a number somebody has to keep in step."
  (when (and (session-barp session) *bar*)
    (funcall *bar* session)))
