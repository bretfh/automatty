;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

;;; Something to read: what broke, and where. Any key puts it away.

(defstruct (note (:constructor %make-note))
  (title "" :type string)
  (lines nil :type list)
  (face :warning))

(defun make-note (title lines &key (face :warning))
  (%make-note :title title
              :lines (remove "" (mapcar (lambda (l) (string-right-trim '(#\Return) l))
                                        lines)
                             :test #'equal)
              :face face))

(defun note-tree (n cols most)
  (vt/ui:column
   :background-color (bar-face :bg-dim)
   :min-width cols
   (vt/ui:row :background-color (bar-face :bg-alt)
              (vt/ui:label (format nil " ~A " (note-title n)) :face (note-face n))
              (vt/ui:gap)
              (vt/ui:label " any key "))
   (apply #'vt/ui:column
          (loop :for line :in (subseq (note-lines n)
                                      0 (min most (length (note-lines n))))
                :collect (vt/ui:label (if (> (length line) (- cols 2))
                                          (subseq line 0 (- cols 2))
                                          line))))))

(defmethod draw-over ((n note) screen)
  (let* ((cols (screen-width screen))
         (rows (screen-height screen))
         (m (vt/cells:make-cells (screen-grid screen) cols rows))
         (tree (note-tree n cols (max 1 (- rows 3))))
         (high (nth-value 1 (vt/ui:with-pass
                              (vt/ui:restyle tree)
                              (vt/ui:measure tree m cols rows))))
         (top (max 0 (- rows high))))
    (vt/cells:fill-rect m 0 top cols (- rows top)
                        (vt:make-face-attrs :bg (bar-face :bg-dim)))
    (vt/cells:draw tree (screen-grid screen) cols rows :top top)
    (setf (screen-cursor-visible screen) nil)))

(vt/mode:define-mode note-mode ())

(defmethod mode-of ((n note)) 'note-mode)

(defmethod unbound ((n note) chord client)
  "Anything at all puts it away."
  (declare (ignore chord))
  (client-over-drop client n)
  t)

(defun lines-of (text)
  (vt/ui:split-string text :separator (list #\Newline)))

(defun show-note (client title text &key (face :warning))
  (client-over-put client (make-note title (lines-of text) :face face)))

(defun show-broke (client what e)
  "What went wrong, and where it went wrong."
  (show-note client
             (format nil "~A came apart" what)
             (format nil "~A~%~A" e
                     (with-output-to-string (s)
                       (ignore-errors
                        (sb-debug:print-backtrace :stream s :count 20))))))
