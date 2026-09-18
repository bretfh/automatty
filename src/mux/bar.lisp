;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

;;; The bar is what the session looks like rather than what any one person is
;;; doing, so the server composes it and it crosses the wire as cells like
;;; everything else. Everyone attached sees the same one.

(defun bar-face (role)
  (vt/ui:unhex (vt/ui:color role)))

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
  (vt/ui:row
   :spacing 1
   :background-color (bar-face :bg-alt)
   (vt/ui:label (format nil " ~A " (session-name session)) :face :accent)
   (vt/ui:label (pane-says (session-pane session)))
   (vt/ui:gap)
   (vt/ui:label (if (pane-running (session-pane session)) "" "done") :face :warning)
   (vt/ui:label (format nil "~Dx~D" (session-cols session)
                        (pane-rows session)))
   (vt/ui:label (clock-says))
   (vt/ui:label " ")))

(defvar *bar* #'default-bar)



(defun draw-bar (session screen)
  (let ((rows *bar-rows*))
    (when (and (plusp rows) *bar* (> (screen-height screen) rows))
      (let* ((tree (funcall *bar* session))
             (top (- (screen-height screen) rows))
             (m (vt/cells:make-cells (screen-grid screen)
                                     (screen-width screen)
                                     (screen-height screen))))
        (vt/cells:fill-rect m 0 top (screen-width screen) rows
                            (vt:make-face-attrs :bg (bar-face :bg-alt)))
        (vt/cells:draw tree (screen-grid screen)
                       (screen-width screen) (screen-height screen)
                       :top top)))))
