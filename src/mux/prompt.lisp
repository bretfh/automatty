;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(declaim (ftype function hints key-for keep-told stop-told rows-of button-at))

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
  (free nil)
  (alt nil)
  (into nil)
  (foot nil)
  (laid nil))

(defun make-prompt (title items &key (text #'identity) chose dropped (most 8) kind
                                     free (query "") alt into foot)
  "A prompt titled TITLE offering ITEMS. A FREE one takes what was typed rather
than one of its items: they are only lines saying what to type, and are not
narrowed by it. ALT is what C-RET does with the choice, INTO what TAB does, and
FOOT a line under the items saying what the keys do."
  (%make-prompt :title title :items items :text text :query query
                :chose chose :dropped dropped :most most :kind kind :free free
                :alt alt :into into :foot foot))

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
                                    ;; a row is a button: a click on it is that choice
                                    (bar-button (list :pick i)
                                                (atty/ui:choice
                                                 :chosen (= i (prompt-index p))
                                                 :face (if (= i (prompt-index p)) :accent :default)
                                                 (atty/ui:label said))))))))
    (atty/ui:framed
     (apply #'atty/ui:column
      :align :stretch
      :background-color (bar-face :bg-dim)
      :min-width (max 0 (- cols 2))
      (append
       (list (atty/ui:row :background-color (bar-face :blue)
                          (atty/ui:label (format nil " ~A " (prompt-title p)) :face :accent)
                          (atty/ui:label (prompt-query p))
                          (atty/ui:label "_")
                          (atty/ui:gap)
                          (atty/ui:label (if (prompt-free p)
                                             ""
                                             (format nil "~D of ~D " (length showing) (length (prompt-items p))))
                                         :face :quiet))
             (if rows
                 (apply #'atty/ui:column rows)
                 (atty/ui:label " nothing matches that")))
       (and (prompt-foot p) (list (prompt-foot p)))))
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
    (setf (prompt-laid p) tree)
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
  '((#\: . "run a command") (#\@ . "choose a window"))
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

(defcommand (prompt-accept-otherwise :unlisted)
  "C-RET: the other thing a prompt does with the choice, when it has one."
  (let ((p (the-prompt)))
    (when (and p (prompt-alt p))
      (let ((it (if (prompt-free p) (prompt-query p) (prompt-chosen p))))
        (prompt-close p *client*)
        (when it (funcall (prompt-alt p) it *client*))))))

(defcommand (prompt-go-into :unlisted)
  "TAB: go into the choice, or over to the other prompt, when the prompt has
somewhere to go."
  (let ((p (the-prompt)))
    (when (and p (prompt-into p))
      (let ((it (if (prompt-free p) (prompt-query p) (prompt-chosen p))))
        (prompt-close p *client*)
        (funcall (prompt-into p) it *client*)))))

(defcommand (prompt-click :unlisted)
  "A click on a row chooses it; on the field, nothing."
  (let* ((p (the-prompt))
         (hit (and p (prompt-laid p) *mouse-at*
                   (button-at (prompt-laid p) (cdr *mouse-at*) (car *mouse-at*)))))
    (when (and hit (consp (bar-button-runs hit)) (eq :pick (first (bar-button-runs hit))))
      (setf (prompt-index p) (second (bar-button-runs hit)))
      (prompt-accept))))

(defcommand (prompt-nothing :unlisted) nil)

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
(atty/mode:define-key 'prompt-mode "C-RET"    #'prompt-accept-otherwise)
(atty/mode:define-key 'prompt-mode "TAB"      #'prompt-go-into)
(atty/mode:define-key 'prompt-mode "mouse-1"  #'prompt-click)
(atty/mode:define-key 'prompt-mode "mouse-1-up" #'prompt-nothing)
(atty/mode:define-key 'prompt-mode "wheel-up"   #'prompt-previous)
(atty/mode:define-key 'prompt-mode "wheel-down" #'prompt-next)

(defun ask (client title items &key (text #'identity) chose dropped kind free (query "")
                                      alt into foot)
  "Put a prompt over whatever CLIENT is showing."
  (client-over-put client (make-prompt title items :text text
                                                   :chose chose
                                                   :dropped dropped
                                                   :kind kind
                                                   :free free
                                                   :query query
                                                   :alt alt :into into :foot foot)))

(defun ask-a-name (client session id label title &key address)
  "Ask what to call pane ID of SESSION, starting from what it is called now.
TAB goes over to naming the window it is in."
  (ask client (format nil "name pane ~A" (or address (format nil "~A:~D" session id)))
       (list (if label
                 (format nil "now  ~A" label)
                 (format nil "now  no name; shown as its title~@[, ~A~]"
                         (and title (plusp (length title)) title)))
             "RET sets it   empty RET clears it   TAB names the window instead   C-g cancels")
       :free t
       :query (or label "")
       :chose (lambda (typed c)
                (let ((*client* c))
                  (tell-the-server (list :name-pane session id typed))))
       :into (lambda (typed c)
               (declare (ignore typed))
               (let ((*client* c)) (tell-the-server (list :naming-window))))))

(defun ask-a-window-name (client session n label)
  "Ask what to call window N of SESSION. TAB goes over to naming the pane."
  (ask client (format nil "name window ~A › ~D" session n)
       (list (if label
                 (format nil "now  ~A" label)
                 (format nil "now  no name; shown as ~D, its number" n))
             "RET sets it   empty RET clears it   TAB names the pane instead   C-g cancels")
       :free t
       :query (or label "")
       :chose (lambda (typed c)
                (let ((*client* c))
                  (tell-the-server (list :name-window session n typed))))
       :into (lambda (typed c)
               (declare (ignore typed))
               (let ((*client* c)) (tell-the-server (list :naming))))))

(defun command-line (name)
  "NAME as the prompt offers it: the name, its key when it has one, and a line
on what it does."
  (let ((key (key-for (intern (string-upcase (substitute #\- #\Space name)) :atty))))
    (format nil "~24A ~10A ~@[~A~]" name (or key "") (command-doc name))))

(defun ask-a-command (client)
  (ask client "run" (command-names) :kind #\:
       :text #'command-line
       :foot (hints 'prompt-mode 'prompt-accept "run" 'prompt-cancel "close")
       :chose (lambda (name client) (run-command name client))))

;;; Sessions and windows, as one list: @ on the bar, or C-b '.

(defun window-choices (rows here)
  "Every session › window the server said, the ones asking first, from what
:these says: (name rows cols panes watching blocked windows). HERE is the
session this client is on: the window it shows is marked."
  (let ((out nil))
    (dolist (row rows)
      (destructuring-bind (name rows cols panes watching &optional (blocked 0) windows) row
        (declare (ignore rows cols panes watching blocked))
        (if windows
            (loop :for (n label npanes asking shownp) :in windows
                  :do (push (list :session name :window n :label label :panes npanes
                                  :asking asking :here (and shownp (equal name here)))
                            out))
            (push (list :session name :window nil :label nil :panes 0 :asking 0
                        :here (equal name here))
                  out))))
    (stable-sort (nreverse out) #'> :key (lambda (c) (getf c :asking)))))

(defun window-choice-line (c)
  "One line: the mark for where this client is, the session and window, what
is asking, how many panes."
  (let ((here (if (getf c :here) "● here" "      "))
        (session (getf c :session))
        (window (if (getf c :window)
                    (format nil "› ~D ~A" (getf c :window) (or (getf c :label) ""))
                    ""))
        (asking (if (plusp (getf c :asking)) (format nil "▲ ~D" (getf c :asking)) ""))
        (panes (format nil "~D pane~:P" (getf c :panes))))
    (format nil "~A ~8A ~10A ~@[~A ~]~A" here session window
            (and (plusp (length asking)) asking) panes)))

(defun ask-a-window (client rows)
  (ask client "sessions and windows" (window-choices rows (client-session client)) :kind #\@
       :text #'window-choice-line
       :foot (hints 'prompt-mode 'prompt-accept "go there" 'prompt-accept-otherwise "new window there"
                    'prompt-go-into "its panes" 'prompt-cancel "close")
       :chose (lambda (c client)
                (wire-send (client-wire client) (list :go (getf c :session)))
                (when (getf c :window)
                  (wire-send (client-wire client) (list :go-window (getf c :session) (getf c :window)))))
       :alt (lambda (c client)
              (wire-send (client-wire client) (list :go (getf c :session)))
              (wire-send (client-wire client) (list :new-window (getf c :session))))
       :into (lambda (c client)
               (let ((*client* client))
                 (ask-panes-of client (getf c :session) (getf c :window))))))

(defun ask-panes-of (client session window)
  "The panes of one window, to go to one."
  (let ((panes (sort (remove-if-not (lambda (r) (and (equal session (getf r :session))
                                                     (eql window (getf r :window))))
                                    (rows-of client))
                     #'< :key (lambda (r) (or (getf r :at) 0)))))
    (keep-told client)
    (ask client (format nil "panes of ~A › ~D" session window) panes
         :text (lambda (r) (format nil "~D  ~12A ~10A ~@[~(~A~)~]"
                                   (1+ (or (getf r :at) 0)) (or (getf r :says) "")
                                   (or (getf r :kind) "") (and (getf r :known) (getf r :state))))
         :chose (lambda (r client)
                  (stop-told client)
                  (wire-send (client-wire client) (list :focus-pane (getf r :session) (getf r :id))))
         :dropped (lambda (client) (stop-told client)))))
