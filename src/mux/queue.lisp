;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(declaim (ftype function explain-this-pane short-tty))

;;; Needs you: every pane in every session that is asking something, oldest
;;; first, with what it asks and the answers it takes, and the screen of the one
;;; picked beside it. It is drawn over the session like a prompt, because it is
;;; one person's: somebody else attached is still looking at their panes.
;;;
;;; An answer is given from here without going there. Going there is one key,
;;; and so is reading the whole of it, or giving it new work instead.

;;; A screen the server sent, drawn as it came, its last rows at the bottom of
;;; whatever room it was given.

(defclass screen-view (atty/ui:widget)
  ((screen :initarg :screen :reader view-screen)))

(defun screen-view (screen &rest props)
  (apply #'make-instance 'screen-view :screen screen :expand 1 props))

(defmethod atty/ui:measure ((w screen-view) m aw ah)
  (declare (ignore m aw ah))
  (values 0 0))

(defmethod atty/ui:paint ((w screen-view) (m atty/cells:cells))
  (let* ((screen (view-screen w))
         (grid (atty/cells:cells-grid m))
         (rows (min (atty/ui:height w) (tty:screen-height screen)))
         (from (- (tty:screen-height screen) rows))
         (into (+ (atty/ui:top w) (- (atty/ui:height w) rows)))
         (cols (min (atty/ui:width w) (tty:screen-width screen)
                    (max 0 (- (atty/cells:cells-cols m) (atty/ui:left w))))))
    (dotimes (i rows)
      (let ((y (+ into i)))
        (when (and (<= 0 y) (< y (atty/cells:cells-rows m)) (plusp cols))
          (let ((row (svref grid y))
                (said (tty:screen-row screen (+ from i))))
            (replace (term:row-chars row) (term:row-chars said)
                     :start1 (atty/ui:left w) :end2 cols)
            (replace (term:row-faces row) (term:row-faces said)
                     :start1 (atty/ui:left w) :end2 cols)))))))

;;; What the client has been told about the panes, read for the queue.

(defun row-for (row now)
  "How long ROW's pane has been what it is, now: what the server said, and the
time since it said it."
  (let ((for (getf row :for)))
    (and for (+ for (max 0 (- now (getf row :heard-at)))))))

(defun row-address (row)
  "A pane's address as a person says it: session:window.pane, or session:id
from a server that has no windows."
  (if (and (getf row :window) (getf row :at))
      (format nil "~A:~D.~D" (getf row :session) (getf row :window) (1+ (getf row :at)))
      (format nil "~A:~D" (getf row :session) (getf row :id))))

(defun row-path (row)
  "Where a pane is, as the queue and the drawer say it: session › window › pane."
  (let ((window (getf row :window))
        (wname (getf row :window-name))
        (pane (or (getf row :label) (getf row :says) (getf row :id))))
    (if window
        (format nil "~A › ~D~@[ ~A~] › ~A" (getf row :session) window
                (and wname (plusp (length wname)) wname) pane)
        (format nil "~A › ~A" (getf row :session) pane))))

(defun row-text (row)
  "What a filter is matched against: everything a person might type to mean it."
  (format nil "~A ~A ~A ~(~A~) ~@[~A~]" (row-address row) (getf row :says)
          (getf row :kind) (getf row :state)
          (getf (getf row :asks) :subject)))

(defun rows-of (client)
  (loop :for row :being :the :hash-values :of (client-panes client) :collect row))

(defun said-by (client who)
  "WHO from a log, as this client would call it, and what kind of thing it is:
client you, client ttys051, pane ctl, the command line."
  (case (first who)
    (:client (if (eql (second who) (client-id client))
                 "client you"
                 (format nil "client ~A" (short-tty (third who) (second who)))))
    (:pane (format nil "pane ~A" (second who)))
    (t "the command line")))

(defun key-in (mode command)
  "The chord COMMAND is bound to in MODE, or its name when nothing is: a hint
names whatever somebody has set up, not what this file set up."
  (let ((does (and (fboundp command) (symbol-function command))))
    (or (car (find does (atty/mode:keys-in-force (atty/mode:mode-named mode)) :key #'cdr))
        (string-downcase (substitute #\Space #\- (symbol-name command))))))

(defun hints (mode &rest pairs)
  "A line of what the keys in MODE do, each key lit and what it does dim: PAIRS
is a command, then what it does. A string in place of the command is said as it
is, for keys no one command is bound to, such as the digits."
  (apply #'atty/ui:row :spacing 0
         (loop :for (command does) :on pairs :by #'cddr
               :append (list (atty/ui:label (format nil " ~A" (if (stringp command)
                                                                  command
                                                                  (key-in mode command)))
                                            :face :key-hint)
                             (atty/ui:label (format nil " ~A  " does) :face :quiet)))))

;;; The queue.

(defstruct (queue (:constructor %make-queue))
  (index 0 :type fixnum)
  (query "" :type string)
  (filtering nil)
  (laid nil))

(defun queue-rows (q client)
  "Every pane that is asking something, the one waiting longest first, and only
those the filter matches."
  (let* ((now (ms-here))
         (blocked (remove-if-not (lambda (r) (and (getf r :known) (eq :blocked (getf r :state))))
                                 (rows-of client)))
         (sorted (sort blocked #'> :key (lambda (r) (or (row-for r now) 0)))))
    (if (plusp (length (queue-query q)))
        (matches (queue-query q) sorted #'row-text)
        sorted)))

(defun queue-chosen (q client)
  (nth (queue-index q) (queue-rows q client)))

(defun option-labels (row options selected)
  "The answers as buttons: a click on one is that answer to ROW's pane."
  (loop :for (n text) :in options
        :append (list (bar-button (list :answer (getf row :session) (getf row :id) n)
                                  (atty/ui:row :spacing 0
                                               (atty/ui:label (format nil " ~D" n) :face :key-number)
                                               (atty/ui:label (format nil " ~A " text)
                                                              :face (if selected :key :default))))
                      (atty/ui:label " "))))

(defun queue-item (client row now i selected)
  "One question: where it is, how long it has waited, who else is looking at
it, what it asks, and its answers. The whole of it is a button that picks it."
  (let* ((asks (getf row :asks))
         (looking (clients-looking-at client (getf row :session) (getf row :window))))
    (bar-button (list :pick i)
     (apply #'atty/ui:column :align :stretch
           (append
            (when selected (list :background-color (bar-face :bg-active)))
            (list
             (atty/ui:row :spacing 0
                          (atty/ui:label (if selected " ▶ " "   ") :face :state-blocked)
                          (atty/ui:label (row-path row) :face :strong)
                          (atty/ui:label (format nil "  ▲ ~A   " (duration (row-for row now)))
                                         :face :state-blocked)
                          (atty/ui:label (or (getf asks :subject) "asking something") :face :strong)
                          (atty/ui:label (if looking
                                             (format nil "   ⌨ client ~{~A~^, ~} is looking at it"
                                                     (mapcar (lambda (c) (short-tty (second c) (first c))) looking))
                                             "")
                                         :face :driven))
             (atty/ui:label (format nil "     ~A" (or (first (getf asks :detail))
                                                     (getf asks :question) "")))
             (apply #'atty/ui:row :spacing 0 (atty/ui:label "    ")
                    (option-labels row (getf asks :options) selected))
             (atty/ui:label "")))))))

(defun lately-line (client entry)
  (destructuring-bind (session id age who verb summary outcome &optional clock) entry
    (declare (ignore clock))
    (atty/ui:row :spacing 0
                 (atty/ui:label (if (eq outcome t) "   ✓ " "   ✗ ")
                                :face (if (eq outcome t) :state-idle :error))
                 (atty/ui:label (format nil "~A  " (or (let ((row (gethash (cons session id) (client-panes client))))
                                                             (and row (row-path row)))
                                                           (format nil "~A:~D" session id)))
                                :face :quiet)
                 (atty/ui:label (format nil "~(~A~) ~A" verb (shortened-to (or summary "") 30)))
                 (atty/ui:label (format nil "  by ~A~:[~;, refused~]  ~A ago"
                                        (said-by client who) (not (eq outcome t))
                                        (duration age))
                                :face :quiet))))

(defun queue-preview (client row)
  (let ((screen (and row (gethash (cons (getf row :session) (getf row :id))
                                  (client-screens client)))))
    (atty/ui:column :align :stretch :expand 1
                    (atty/ui:label (if row
                                       (format nil " ~A · as it is now" (row-address row))
                                       "")
                                   :face :quiet)
                    (if screen
                        (screen-view screen)
                        (atty/ui:label (if row " waiting for its screen" "") :face :quiet)))))

(defun queue-tree (q client cols)
  (let* ((now (ms-here))
         (rows (queue-rows q client))
         (chosen (nth (queue-index q) rows))
         (oldest (and rows (row-for (first rows) now))))
    (atty/ui:framed
     (atty/ui:column
      :align :stretch
      :background-color (bar-face :bg-dim)
      :min-width (max 0 (- cols 2))
      :expand 1
      (atty/ui:row :spacing 2 :background-color (bar-face :bg-alt)
                   (atty/ui:label (format nil " ~D waiting~@[ · oldest ~A~] · every session"
                                          (length rows) (and oldest (duration oldest)))
                                  :face :quiet)
                   (atty/ui:gap)
                   (atty/ui:label (cond ((queue-filtering q)
                                         (format nil "› ~A_ " (queue-query q)))
                                        ((plusp (length (queue-query q)))
                                         (format nil "› ~A " (queue-query q)))
                                        (t (format nil "~A filter " (key-in 'queue-mode 'queue-filter))))
                                  :face (if (queue-filtering q) :default :quiet)))
      (atty/ui:row
       :align :stretch :expand 1 :spacing 1
       (apply #'atty/ui:column :align :stretch :expand 3
              (append
               (if rows
                   (loop :for row :in rows
                         :for i :from 0
                         :collect (queue-item client row now i (= i (queue-index q))))
                   (list (atty/ui:label "   nothing needs you" :face :quiet)
                         (atty/ui:label "")))
               (when (client-lately client)
                 (cons (atty/ui:label " answered lately" :face :quiet)
                       (mapcar (lambda (e) (lately-line client e)) (client-lately client))))
               (list (atty/ui:gap :expand 1))))
       (queue-preview client chosen))
      (atty/ui:row :background-color (bar-face :bg-alt)
       (hints 'queue-mode "↑↓" "choose" "1-9" "answer in place"
             'queue-go "go there" 'queue-explain "why it thinks so" 'queue-read "read"
             'queue-prompt "prompt instead" 'queue-close "close")
       (atty/ui:gap)))
     :face :state-blocked
     :titles (list :tl (atty/ui:row :spacing 0
                                   (atty/ui:label " needs you " :face :chip-blocked)
                                   (atty/ui:label (format nil " ~A " (key-in 'pane-mode 'needs-you))
                                                  :face :quiet))))))

(defmethod draw-over ((q queue) screen)
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (top (min 1 (max 0 (1- rows))))
         (bottom (max top (1- rows))))
    (setf (queue-index q) (max 0 (min (queue-index q)
                                      (1- (length (queue-rows q *drawing-for*))))))
    (let ((tree (queue-tree q *drawing-for* cols)))
      (atty/cells:draw tree (tty:screen-grid screen) cols bottom :top top)
      (setf (queue-laid q) tree))
    (setf (tty:screen-cursor-visible screen) nil)))

(atty/mode:define-mode queue-mode ())

;;; Typing a filter is its own mode, so every letter is the filter's while it is
;;; being typed rather than the command it would be otherwise.
(atty/mode:define-mode queue-filter-mode ())

(defmethod mode-of ((q queue))
  (if (queue-filtering q) 'queue-filter-mode 'queue-mode))

(defmethod ticks-p ((q queue)) t)

(defun the-queue ()
  (let ((it (first (client-over *client*))))
    (when (typep it 'queue) it)))

(defmethod unbound ((q queue) chord client)
  "A digit answers the one picked with that number. While filtering, what is
typed is the filter."
  (let ((said (atty/mode:self-inserting chord)))
    (when said
      (cond
        ((queue-filtering q)
         (setf (queue-query q) (concatenate 'string (queue-query q) said)
               (queue-index q) 0
               (client-dirty client) t))
        ((and (= 1 (length said)) (digit-char-p (char said 0))
              (plusp (digit-char-p (char said 0))))
         (let ((*client* client))
           (queue-answer-with (digit-char-p (char said 0))))))
      t)))

(defun queue-answer-with (n)
  (let* ((q (the-queue))
         (row (and q (queue-chosen q *client*))))
    (when row
      (tell-the-server (list :answer (getf row :session) (getf row :id) n)))))

(defun queue-close-it (q client)
  (client-over-drop client q)
  (stop-told client))

(defcommand (queue-next :unlisted)
  (let ((q (the-queue)))
    (when q (setf (queue-index q) (1+ (queue-index q))))))

(defcommand (queue-previous :unlisted)
  (let ((q (the-queue)))
    (when q (setf (queue-index q) (max 0 (1- (queue-index q)))))))

(defcommand (queue-go :unlisted)
  (let* ((q (the-queue))
         (row (and q (queue-chosen q *client*))))
    (when q
      (queue-close-it q *client*)
      (when row
        (tell-the-server (list :focus-pane (getf row :session) (getf row :id)))))))

(defcommand (queue-read :unlisted)
  (let* ((q (the-queue))
         (row (and q (queue-chosen q *client*))))
    (when row
      (tell-the-server (list :pane-read (getf row :session) (getf row :id))))))

(defcommand (queue-prompt :unlisted)
  (let* ((q (the-queue))
         (row (and q (queue-chosen q *client*))))
    (when row
      (let ((session (getf row :session))
            (id (getf row :id)))
        (ask *client* (format nil "prompt ~A:~D" session id)
             (list "it is asking something; a prompt is refused until it is answered")
             :free t
             :chose (lambda (typed c)
                      (let ((*client* c))
                        (when (plusp (length typed))
                          (tell-the-server (list :agent-prompt session id typed))))))))))

(defcommand (queue-filter :unlisted)
  (let ((q (the-queue)))
    (when q
      (setf (queue-filtering q) t)
      (client-in-mode *client*))))

(defcommand (queue-rub-out :unlisted)
  (let ((q (the-queue)))
    (when (and q (queue-filtering q))
      (let ((s (queue-query q)))
        (setf (queue-query q) (subseq s 0 (max 0 (1- (length s)))))))))

(defcommand (queue-explain :unlisted)
  "go to the one picked and open the drawer on it"
  (queue-go)
  (explain-this-pane))

(defcommand (queue-click :unlisted)
  "a click on a question picks it; on an answer, answers"
  (let* ((q (the-queue))
         (hit (and q (queue-laid q) *mouse-at*
                   (button-at (queue-laid q) (cdr *mouse-at*) (car *mouse-at*)))))
    (when hit
      (destructuring-bind (what &rest it) (bar-button-runs hit)
        (case what
          (:pick (setf (queue-index q) (first it) (client-dirty *client*) t))
          (:answer (tell-the-server (list* :answer it))))))))

(defcommand (queue-nothing :unlisted) nil)

(defcommand (queue-close :unlisted)
  (let ((q (the-queue)))
    (when q (queue-close-it q *client*))))

(defcommand (queue-done-filtering :unlisted)
  (let ((q (the-queue)))
    (when q
      (setf (queue-filtering q) nil)
      (client-in-mode *client*))))

(defcommand (queue-drop-filter :unlisted)
  (let ((q (the-queue)))
    (when q
      (setf (queue-filtering q) nil
            (queue-query q) "")
      (client-in-mode *client*))))

(atty/mode:define-key 'queue-mode "Down"   #'queue-next)
(atty/mode:define-key 'queue-mode "C-n"    #'queue-next)
(atty/mode:define-key 'queue-mode "Up"     #'queue-previous)
(atty/mode:define-key 'queue-mode "C-p"    #'queue-previous)
(atty/mode:define-key 'queue-mode "RET"    #'queue-go)
(atty/mode:define-key 'queue-mode "r"      #'queue-read)
(atty/mode:define-key 'queue-mode "p"      #'queue-prompt)
(atty/mode:define-key 'queue-mode "/"      #'queue-filter)
(atty/mode:define-key 'queue-mode "Escape" #'queue-close)
(atty/mode:define-key 'queue-mode "C-g"    #'queue-close)
(atty/mode:define-key 'queue-mode "e"      #'queue-explain)
(atty/mode:define-key 'queue-mode "mouse-1" #'queue-click)
(atty/mode:define-key 'queue-mode "mouse-1-up" #'queue-nothing)
(atty/mode:define-key 'queue-mode "wheel-up"   #'queue-previous)
(atty/mode:define-key 'queue-mode "wheel-down" #'queue-next)

(atty/mode:define-key 'queue-filter-mode "RET"    #'queue-done-filtering)
(atty/mode:define-key 'queue-filter-mode "DEL"    #'queue-rub-out)
(atty/mode:define-key 'queue-filter-mode "Escape" #'queue-drop-filter)
(atty/mode:define-key 'queue-filter-mode "C-g"    #'queue-drop-filter)

(defcommand (needs-you :group agents)
  "every question anywhere, oldest first; a digit answers it"
  (let ((q (%make-queue)))
    (keep-told *client*)
    (client-over-put *client* q)))
