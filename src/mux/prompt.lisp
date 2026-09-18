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
  (let* ((cols (screen-width screen))
         (rows (screen-height screen))
         (m (vt/cells:make-cells (screen-grid screen) cols rows))
         (tree (prompt-tree p cols))
         (high (nth-value 1 (vt/ui:with-pass
                              (vt/ui:restyle tree)
                              (vt/ui:measure tree m cols rows))))
         (top (max 0 (- rows high))))
    (vt/cells:fill-rect m 0 top cols (- rows top)
                        (vt:make-face-attrs :bg (bar-face :bg-dim)))
    (vt/cells:draw tree (screen-grid screen) cols rows :top top)
    (setf (screen-cursor-y screen) top
          (screen-cursor-x screen) (min (1- cols)
                                        (+ 2 (length (prompt-title p))
                                           (length (prompt-query p))))
          (screen-cursor-visible screen) t)))

(defun prompt-close (p client)
  "Take it away. Nothing else needs doing: what it was covering is still in the
screen it was drawn over, and the diff puts it back."
  (client-over-drop client p))

(defmethod press ((p prompt) key client)
  (let ((showing (prompt-showing p)))
    (flet ((moved (to)
             (setf (prompt-index p)
                   (max 0 (min (max 0 (1- (length showing))) to)))))
      (cond
        ((characterp key)
         (setf (prompt-query p) (concatenate 'string (prompt-query p) (string key))
               (prompt-index p) 0))
        ((not (consp key)) nil)
        (t (case (first key)
             (:backspace
              (let ((q (prompt-query p)))
                (setf (prompt-query p) (subseq q 0 (max 0 (1- (length q))))
                      (prompt-index p) 0)))
             (:up (moved (1- (prompt-index p))))
             (:down (moved (1+ (prompt-index p))))
             (:page-up (moved (- (prompt-index p) (prompt-most p))))
             (:page-down (moved (+ (prompt-index p) (prompt-most p))))
             (:home (moved 0))
             (:end (moved (1- (length showing))))
             (:escape
              (prompt-close p client)
              (when (prompt-dropped p) (funcall (prompt-dropped p) client)))
             (:enter
              (let ((it (prompt-chosen p)))
                (prompt-close p client)
                (when (and it (prompt-chose p))
                  (funcall (prompt-chose p) it client))))
             (t nil))))))
  (setf (client-dirty client) t))

(defun ask (client title items &key (text #'identity) chose dropped)
  "Put a prompt over whatever CLIENT is showing."
  (client-over-put client (make-prompt title items :text text
                                                   :chose chose
                                                   :dropped dropped)))

(defun ask-a-command (client)
  (ask client "run" (command-names)
       :chose (lambda (name client) (run-command name client))))

(defcommand detach
  (done-with *client* :detached))

(defcommand redraw
  (client-redraw *client*))

(defun tell-the-server (form)
  (wire-send (client-wire *client*) form))

(defcommand bar-off
  (tell-the-server (list :bar 0)))

(defcommand bar-on
  (tell-the-server (list :bar 1)))

(defcommand run-a-command
  (ask-a-command *client*))

(defcommand send-the-prefix
  (wire-send (client-wire *client*) (list :keys (string +prefix+))))

(defcommand what-the-keys-do
  (show-note *client* "keys"
             (format nil "~{~A~%~}"
                     (loop :for (chord . nil) :in (vt/mode:keys-in-force
                                                   (vt/mode:mode-named 'pane-mode))
                           :collect (format nil "  ~A" chord)))
             :face :accent))

;;; What the keys mean. A mode holds them, so another mode may be defined on
;;; top of this one and change or add to what is here without touching it.

(vt/mode:define-key 'pane-mode "C-b d" #'detach)
(vt/mode:define-key 'pane-mode "C-b r" #'redraw)
(vt/mode:define-key 'pane-mode "C-b :" #'run-a-command)
(vt/mode:define-key 'pane-mode "C-b ?" #'what-the-keys-do)
(vt/mode:define-key 'pane-mode "C-b C-b" #'send-the-prefix)
(vt/mode:define-key 'pane-mode "C-b b o" #'bar-off)
(vt/mode:define-key 'pane-mode "C-b b b" #'bar-on)
