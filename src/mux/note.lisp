;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx)

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
  (vtx/ui:column
   :background-color (bar-face :bg-dim)
   :min-width cols
   (vtx/ui:row :background-color (bar-face :bg-alt)
              (vtx/ui:label (format nil " ~A " (note-title n)) :face (note-face n))
              (vtx/ui:gap)
              (vtx/ui:label " any key "))
   (apply #'vtx/ui:column
          (loop :for line :in (subseq (note-lines n)
                                      0 (min most (length (note-lines n))))
                :collect (vtx/ui:label (if (> (length line) (- cols 2))
                                          (subseq line 0 (- cols 2))
                                          line))))))

(defmethod draw-over ((n note) screen)
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (m (vtx/cells:make-cells (tty:screen-grid screen) cols rows))
         (tree (note-tree n cols (max 1 (- rows 3))))
         (high (nth-value 1 (vtx/ui:with-pass
                              (vtx/ui:restyle tree)
                              (vtx/ui:measure tree m cols rows))))
         (top (max 0 (- rows high))))
    (vtx/cells:fill-rect m 0 top cols (- rows top)
                        (vt:make-face :bg (bar-face :bg-dim)))
    (vtx/cells:draw tree (tty:screen-grid screen) cols rows :top top)
    (setf (tty:screen-cursor-visible screen) nil)))

(vtx/mode:define-mode note-mode ())

(defmethod mode-of ((n note)) 'note-mode)

(defmethod unbound ((n note) chord client)
  "Anything at all puts it away."
  (declare (ignore chord))
  (client-over-drop client n)
  t)

(defun lines-of (text)
  (vtx/ui:split-string text :separator (list #\Newline)))

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
