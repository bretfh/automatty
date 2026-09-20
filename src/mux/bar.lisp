;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx)

;;; The bar is what the session looks like rather than what any one person is
;;; doing, so the server composes it and it crosses the wire as cells like
;;; everything else. Everyone attached sees the same one.

(declaim (special +prompt-toggles+))

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

;;; A button on the bar is a widget like any other, except a click on it does
;;; not run anything itself: the bar is composed once and shared by everyone
;;; watching, so what a click on it means is the server's to say, and RUNS
;;; names a command for whichever watcher clicked it to be told to run.

(defclass bar-button (vtx/ui:widget)
  ((runs :initarg :runs :reader bar-button-runs)))

(defun bar-button (runs part)
  (make-instance 'bar-button :runs runs :parts (list part)))

(defmethod vtx/ui:measure ((w bar-button) m aw ah)
  (let ((part (first (vtx/ui:parts w))))
    (if part (vtx/ui:measure part m aw ah) (values 0 1))))

(defmethod vtx/ui:lay ((w bar-button) m x y width height)
  (call-next-method)
  (let ((part (first (vtx/ui:parts w))))
    (when part (vtx/ui:lay part m x y width height))))

(defmethod vtx/ui:under ((w bar-button) line col)
  (when (and (<= (vtx/ui:top w) line) (< line (vtx/ui:bottom w))
             (<= (vtx/ui:left w) col) (< col (vtx/ui:right w)))
    w))

(defun search-segment (session)
  "The bar's own way in: one field, styled like a search bar and a shade
deeper than the bar it sits on, and one button naming what it currently opens.
Clicking the button cycles it through +PROMPT-TOGGLES+; clicking the field
runs whichever of them is current."
  (let* ((kind (nth (mod (session-search-kind session) (length +prompt-toggles+))
                    +prompt-toggles+))
         (prefix (car kind)) (runs (cdr kind)))
    (vtx/ui:row
     :spacing 0 :background-color (bar-face :bg-dim)
     (bar-button :cycle-search-kind (vtx/ui:label (format nil " ~C " prefix) :face :brand))
     (bar-button runs (vtx/ui:label (format nil " ~A " runs))))))

(defun default-bar (session)
  "What the bar shows. Rebind *BAR* to a function of the session answering
another tree, and it is another bar."
  (vtx/ui:row
   :spacing 1
   :background-color (bar-face :blue)
   (vtx/ui:label " λ " :face :brand)
   (vtx/ui:label (format nil " ~A " (session-name session)) :face :accent)
   (vtx/ui:label (pane-says (session-focus session)))
   (vtx/ui:gap)
   (search-segment session)
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
