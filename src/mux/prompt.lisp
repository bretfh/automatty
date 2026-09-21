;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

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
  (dropped nil)
  (kind nil)
  (free nil))

(defun make-prompt (title items &key (text #'identity) chose dropped (most 8) kind
                                     free (query ""))
  "A prompt titled TITLE offering ITEMS. A FREE one takes what was typed rather
than one of its items: they are only lines saying what to type, and are not
narrowed by it."
  (%make-prompt :title title :items items :text text :query query
                :chose chose :dropped dropped :most most :kind kind :free free))

(defun prompt-showing (p)
  (if (prompt-free p)
      (prompt-items p)
      (matches (prompt-query p) (prompt-items p) (prompt-text p))))

(defun prompt-chosen (p)
  (nth (prompt-index p) (prompt-showing p)))

(defun prompt-tree (p cols)
  (let* ((showing (prompt-showing p))
         (room (min (prompt-most p) (length showing)))
         (from (max 0 (min (- (length showing) room)
                           (- (prompt-index p) (floor room 2)))))
         (rows (loop :for item :in (subseq showing from (+ from room))
                     :for i :from from
                     :collect (let ((said (princ-to-string (funcall (prompt-text p) item))))
                                (if (prompt-free p)
                                    (atty/ui:label (format nil "  ~A" said))
                                    (atty/ui:choice
                                     :chosen (= i (prompt-index p))
                                     :face (if (= i (prompt-index p)) :accent :default)
                                     (atty/ui:label said)))))))
    (atty/ui:framed
     (atty/ui:column
      :align :stretch
      :background-color (bar-face :bg-dim)
      :min-width (max 0 (- cols 2))
      (atty/ui:row :background-color (bar-face :blue)
                 (atty/ui:label (format nil " ~A " (prompt-title p)) :face :accent)
                 (atty/ui:label (prompt-query p))
                 (atty/ui:label "_")
                 (atty/ui:gap))
      (if rows
          (apply #'atty/ui:column rows)
          (atty/ui:label " nothing matches that")))
     :face :border-active)))

(defmethod draw-over ((p prompt) screen)
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows))
         (tree (prompt-tree p cols))
         (high (nth-value 1 (atty/ui:with-pass
                              (atty/ui:restyle tree)
                              (atty/ui:measure tree m cols rows))))
         (top (min 1 (max 0 (1- rows))))
         (bottom (min rows (+ top high))))
    (atty/cells:draw tree (tty:screen-grid screen) cols bottom :top top)
    (setf (tty:screen-cursor-y screen) (min (1- rows) (1+ top))
          (tty:screen-cursor-x screen) (min (1- cols)
                                        (+ 3 (length (prompt-title p))
                                           (length (prompt-query p))))
          (tty:screen-cursor-visible screen) t)))

(defun prompt-close (p client)
  "Take it away. Nothing else needs doing: what it was covering is still in the
screen it was drawn over, and the diff puts it back."
  (client-over-drop client p))

(atty/mode:define-mode prompt-mode ())

(defmethod mode-of ((p prompt)) 'prompt-mode)

(defun the-prompt ()
  (let ((it (first (client-over *client*))))
    (when (typep it 'prompt) it)))

(defun prompt-moved (p to)
  (let ((most (max 0 (1- (length (prompt-showing p))))))
    (setf (prompt-index p) (max 0 (min most to)))))

(defparameter +prompt-toggles+
  '((#\: . "run a command") (#\@ . "choose a session"))
  "The prefix that opens each kind of prompt, and the command that opens it.
Typing one as the first character of an empty query, in a prompt of a
different kind, switches to it instead of being searched for.")

(defmethod unbound ((p prompt) chord client)
  "A key nothing is bound to, if it is one that stands for a character, is what
was typed -- unless it is another prompt's prefix, typed at the very start,
which switches to that prompt instead."
  (let ((said (atty/mode:self-inserting chord)))
    (when said
      (let ((runs (and (not (prompt-free p))
                       (= 1 (length said)) (zerop (length (prompt-query p)))
                       (not (eql (char said 0) (prompt-kind p)))
                       (cdr (assoc (char said 0) +prompt-toggles+)))))
        (if runs
            (progn (prompt-close p client) (run-command runs client))
            (setf (prompt-query p) (concatenate 'string (prompt-query p) said)
                  (prompt-index p) 0
                  (client-dirty client) t)))
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
      (let ((it (if (prompt-free p) (prompt-query p) (prompt-chosen p))))
        (prompt-close p *client*)
        (when (and it (prompt-chose p)) (funcall (prompt-chose p) it *client*))))))

(defcommand (prompt-cancel :unlisted)
  (let ((p (the-prompt)))
    (when p
      (prompt-close p *client*)
      (when (prompt-dropped p) (funcall (prompt-dropped p) *client*)))))

(atty/mode:define-key 'prompt-mode "Down"     #'prompt-next)
(atty/mode:define-key 'prompt-mode "C-n"      #'prompt-next)
(atty/mode:define-key 'prompt-mode "Up"       #'prompt-previous)
(atty/mode:define-key 'prompt-mode "C-p"      #'prompt-previous)
(atty/mode:define-key 'prompt-mode "PageDown" #'prompt-page-down)
(atty/mode:define-key 'prompt-mode "PageUp"   #'prompt-page-up)
(atty/mode:define-key 'prompt-mode "Home"     #'prompt-first)
(atty/mode:define-key 'prompt-mode "End"      #'prompt-last)
(atty/mode:define-key 'prompt-mode "DEL"      #'prompt-rub-out)
(atty/mode:define-key 'prompt-mode "C-u"      #'prompt-clear)
(atty/mode:define-key 'prompt-mode "RET"      #'prompt-accept)
(atty/mode:define-key 'prompt-mode "Escape"   #'prompt-cancel)
(atty/mode:define-key 'prompt-mode "C-g"      #'prompt-cancel)

(defun ask (client title items &key (text #'identity) chose dropped kind free (query ""))
  "Put a prompt over whatever CLIENT is showing."
  (client-over-put client (make-prompt title items :text text
                                                   :chose chose
                                                   :dropped dropped
                                                   :kind kind
                                                   :free free
                                                   :query query)))

(defun ask-a-name (client session id label title)
  "Ask what to call pane ID of SESSION, starting from what it is called now."
  (ask client (format nil "name ~A:~D" session id)
       (list (if label
                 (format nil "now  ~A" label)
                 (format nil "now  no name; shown as its title~@[, ~A~]"
                         (and title (plusp (length title)) title)))
             "RET sets it   empty RET clears it   C-g cancels")
       :free t
       :query (or label "")
       :chose (lambda (typed c)
                (let ((*client* c))
                  (tell-the-server (list :name-pane session id typed))))))

(defun ask-a-command (client)
  (ask client "run" (command-names) :kind #\:
       :chose (lambda (name client) (run-command name client))))
