;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

;;; Reading one thing from a list of things. It is drawn over the session rather
;;; than composed into it, because it belongs to whoever opened it: somebody
;;; else attached to the same session is still looking at their shell.

(defstruct (prompt (:constructor %make-prompt))
  (title "" :type string)
  (query "" :type string)
  (items nil :type list)
  (text #'identity)
  (index 0 :type fixnum)
  (most 8 :type fixnum)
  (chose nil)
  (dropped nil))

(defun make-prompt (title items &key (text #'identity) chose dropped (most 8))
  (%make-prompt :title title :items items :text text
                :chose chose :dropped dropped :most most))

(defun prompt-showing (p)
  (matches (prompt-query p) (prompt-items p) (prompt-text p)))

(defun prompt-chosen (p)
  (nth (prompt-index p) (prompt-showing p)))

(defun prompt-tree (p cols)
  (let* ((showing (prompt-showing p))
         (room (min (prompt-most p) (length showing)))
         (from (max 0 (min (- (length showing) room)
                           (- (prompt-index p) (floor room 2)))))
         (rows (loop :for item :in (subseq showing from (+ from room))
                     :for i :from from
                     :collect (vt/ui:choice
                               :chosen (= i (prompt-index p))
                               :face (if (= i (prompt-index p)) :accent :default)
                               (vt/ui:label (princ-to-string
                                             (funcall (prompt-text p) item)))))))
    (vt/ui:column
     :background-color (bar-face :bg-dim)
     :min-width cols
     (vt/ui:row :background-color (bar-face :bg-alt)
                (vt/ui:label (format nil " ~A " (prompt-title p)) :face :accent)
                (vt/ui:label (prompt-query p))
                (vt/ui:label "_")
                (vt/ui:gap))
     (if rows
         (apply #'vt/ui:column rows)
         (vt/ui:label " nothing matches that")))))

(defmethod draw-over ((p prompt) screen)
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (m (vt/cells:make-cells (tty:screen-grid screen) cols rows))
         (tree (prompt-tree p cols))
         (high (nth-value 1 (vt/ui:with-pass
                              (vt/ui:restyle tree)
                              (vt/ui:measure tree m cols rows))))
         (top (max 0 (- rows high))))
    (vt/cells:fill-rect m 0 top cols (- rows top)
                        (vt:make-face :bg (bar-face :bg-dim)))
    (vt/cells:draw tree (tty:screen-grid screen) cols rows :top top)
    (setf (tty:screen-cursor-y screen) top
          (tty:screen-cursor-x screen) (min (1- cols)
                                        (+ 2 (length (prompt-title p))
                                           (length (prompt-query p))))
          (tty:screen-cursor-visible screen) t)))

(defun prompt-close (p client)
  "Take it away. Nothing else needs doing: what it was covering is still in the
screen it was drawn over, and the diff puts it back."
  (client-over-drop client p))

(vt/mode:define-mode prompt-mode ())

(defmethod mode-of ((p prompt)) 'prompt-mode)

(defun the-prompt ()
  (let ((it (first (client-over *client*))))
    (when (typep it 'prompt) it)))

(defun prompt-moved (p to)
  (let ((most (max 0 (1- (length (prompt-showing p))))))
    (setf (prompt-index p) (max 0 (min most to)))))

(defmethod unbound ((p prompt) chord client)
  "A key nothing is bound to, if it is one that stands for a character, is what
was typed."
  (let ((said (vt/mode:self-inserting chord)))
    (when said
      (setf (prompt-query p) (concatenate 'string (prompt-query p) said)
            (prompt-index p) 0
            (client-dirty client) t)
      t)))

(defcommand (prompt-next :unlisted)
  (let ((p (the-prompt))) (when p (prompt-moved p (1+ (prompt-index p))))))

(defcommand (prompt-previous :unlisted)
  (let ((p (the-prompt))) (when p (prompt-moved p (1- (prompt-index p))))))

(defcommand (prompt-page-down :unlisted)
  (let ((p (the-prompt)))
    (when p (prompt-moved p (+ (prompt-index p) (prompt-most p))))))

(defcommand (prompt-page-up :unlisted)
  (let ((p (the-prompt)))
    (when p (prompt-moved p (- (prompt-index p) (prompt-most p))))))

(defcommand (prompt-first :unlisted)
  (let ((p (the-prompt))) (when p (prompt-moved p 0))))

(defcommand (prompt-last :unlisted)
  (let ((p (the-prompt)))
    (when p (prompt-moved p (length (prompt-showing p))))))

(defcommand (prompt-rub-out :unlisted)
  (let ((p (the-prompt)))
    (when p
      (let ((q (prompt-query p)))
        (setf (prompt-query p) (subseq q 0 (max 0 (1- (length q))))
              (prompt-index p) 0)))))

(defcommand (prompt-clear :unlisted)
  (let ((p (the-prompt)))
    (when p (setf (prompt-query p) "" (prompt-index p) 0))))

(defcommand (prompt-accept :unlisted)
  (let ((p (the-prompt)))
    (when p
      (let ((it (prompt-chosen p)))
        (prompt-close p *client*)
        (when (and it (prompt-chose p)) (funcall (prompt-chose p) it *client*))))))

(defcommand (prompt-cancel :unlisted)
  (let ((p (the-prompt)))
    (when p
      (prompt-close p *client*)
      (when (prompt-dropped p) (funcall (prompt-dropped p) *client*)))))

(vt/mode:define-key 'prompt-mode "Down"     #'prompt-next)
(vt/mode:define-key 'prompt-mode "C-n"      #'prompt-next)
(vt/mode:define-key 'prompt-mode "Up"       #'prompt-previous)
(vt/mode:define-key 'prompt-mode "C-p"      #'prompt-previous)
(vt/mode:define-key 'prompt-mode "PageDown" #'prompt-page-down)
(vt/mode:define-key 'prompt-mode "PageUp"   #'prompt-page-up)
(vt/mode:define-key 'prompt-mode "Home"     #'prompt-first)
(vt/mode:define-key 'prompt-mode "End"      #'prompt-last)
(vt/mode:define-key 'prompt-mode "DEL"      #'prompt-rub-out)
(vt/mode:define-key 'prompt-mode "C-u"      #'prompt-clear)
(vt/mode:define-key 'prompt-mode "RET"      #'prompt-accept)
(vt/mode:define-key 'prompt-mode "Escape"   #'prompt-cancel)
(vt/mode:define-key 'prompt-mode "C-g"      #'prompt-cancel)

(defun ask (client title items &key (text #'identity) chose dropped)
  "Put a prompt over whatever CLIENT is showing."
  (client-over-put client (make-prompt title items :text text
                                                   :chose chose
                                                   :dropped dropped)))

(defun ask-a-command (client)
  (ask client "run" (command-names)
       :chose (lambda (name client) (run-command name client))))
