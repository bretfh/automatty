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
  "WHO from a log, as this client would say it: ◆ here, ⌨ ttys051, ⌁ todo:1.2,
$ cli."
  (who-text who client))

(defun key-in (mode command)
  "The chord COMMAND is bound to in MODE, or its name when nothing is: a hint
names whatever somebody has set up, not what this file set up."
  (let* ((does (and (fboundp command) (symbol-function command)))
         (chord (car (find does (atty/mode:keys-in-force (atty/mode:mode-named mode)) :key #'cdr))))
    (cond ((null chord) (string-downcase (substitute #\Space #\- (symbol-name command))))
          ((string= chord "RET") "↵")
          ((string= chord "TAB") "⇥")
          (t chord))))

;;; The queue.

(defstruct (queue (:constructor %make-queue))
  (index 0 :type fixnum)
  (query "" :type string)
  (filtering nil)
  (order :oldest)                       ; :oldest or :name
  (every t)                             ; every session, or this one
  (showing-lately t)                    ; whether what was answered lately is shown
  (laid nil))

(defun queue-rows (q client)
  "Every pane that is asking something, the one waiting longest first or by
name as the sort says, in every session or this one, and only those the
filter matches."
  (let* ((now (ms-here))
         (blocked (remove-if-not (lambda (r) (and (getf r :known) (eq :blocked (getf r :state))
                                                  (or (queue-every q)
                                                      (equal (getf r :session) (client-session client)))))
                                 (rows-of client)))
         (sorted (if (eq :name (queue-order q))
                     (sort blocked #'string< :key #'row-path)
                     (sort blocked #'> :key (lambda (r) (or (row-for r now) 0))))))
    (if (plusp (length (queue-query q)))
        (matches (queue-query q) sorted #'row-text)
        sorted)))

(defun queue-chosen (q client)
  (nth (queue-index q) (queue-rows q client)))

(defun option-labels (row options selected)
  "The answers as key caps: a click on one is that answer to ROW's pane."
  (declare (ignore selected))
  (loop :for (n text) :in options
        :append (list (keycap n text :runs (list :answer (getf row :session) (getf row :id) n))
                      (atty/ui:label " "))))

(defun queue-item (client row now i selected)
  "One question as three rows with a gutter: where it is, how long it has
waited, who else is looking at it and what it asks; the detail; the answers.
The whole of it is a button that picks it."
  (let* ((asks (getf row :asks))
         (looking (clients-looking-at client (getf row :session) (getf row :window))))
    (bar-button (list :pick i)
     (atty/ui:column :align :stretch
      (gutter-row (if selected "▶" "")
                  (atty/ui:row :spacing 0
                               (atty/ui:label (row-path row) :face :strong)
                               (atty/ui:label (format nil "  ~A" (or (getf row :kind) "")) :face :quiet)
                               (atty/ui:label (format nil "   ▲ ~A" (duration (row-for row now)))
                                              :face :state-blocked-strong)
                               (atty/ui:label (format nil "   ~A" (or (getf asks :subject) "asking something")) :face :strong)
                               (atty/ui:label (if looking
                                                  (format nil "   ⌨ ~{~A~^, ~} looking"
                                                          (mapcar (lambda (c) (short-tty (second c) (first c))) looking))
                                                  "")
                                              :face :client))
                  :selected selected)
      (gutter-row "" (atty/ui:label (format nil "  ~A" (or (first (getf asks :detail)) (getf asks :question) "")))
                  :selected selected)
      (gutter-row "" (apply #'atty/ui:row :spacing 0 (atty/ui:label " ")
                            (option-labels row (getf asks :options) selected))
                  :selected selected)
      (atty/ui:label "")))))

(defun lately-line (client entry)
  (destructuring-bind (session id age who verb summary outcome &optional clock) entry
    (declare (ignore clock))
    (gutter-row (atty/ui:label (if (eq outcome t) "✓ " "✗ ") :face (if (eq outcome t) :state-idle :error))
                (atty/ui:row :spacing 0
                             (atty/ui:label (format nil "~24A" (or (let ((row (gethash (cons session id) (client-panes client))))
                                                                       (and row (row-path row)))
                                                                     (format nil "~A:~D" session id)))
                                            :face :quiet)
                             (atty/ui:label (format nil "~(~A~) ~A" verb (shortened-to (or summary "") 30)))
                             (atty/ui:label (if (eq outcome t) "   " "   refused   ") :face :error)
                             (who who client :pad 18)
                             (atty/ui:label (format nil "  ~A ago" (duration age)) :face :quiet)))))

(defun queue-preview (client row)
  (let ((screen (and row (gethash (cons (getf row :session) (getf row :id))
                                  (client-screens client)))))
    (atty/ui:column :align :stretch :expand 1
                    (band (list (atty/ui:label (if row (format nil " ~A" (row-path row)) "") :face :quiet)
                                (atty/ui:label (if row " · as it is now" "") :face :quiet))
                          nil)
                    (if screen
                        (screen-view screen)
                        (atty/ui:label (if row " waiting for its screen" "") :face :quiet)))))

(defparameter +question-rows+ 4 "How many rows one question takes on the list.")

(defun queue-window (q client rows)
  "Which questions fit: the first shown and how many, the chosen one among them."
  (let* ((lately (if (and (queue-showing-lately q) (client-lately client))
                     (1+ (length (client-lately client)))
                     0))
         (room (max +question-rows+ (- (client-rows client) 6 lately)))
         (most (max 1 (floor room +question-rows+)))
         (from (max 0 (min (- (length rows) most) (- (queue-index q) (floor most 2))))))
    (values from most)))

(defun queue-tree (q client)
  (let* ((now (ms-here))
         (rows (queue-rows q client))
         (chosen (nth (queue-index q) rows))
         (oldest (and rows (row-for (first rows) now))))
    (overlay-tree
     :title "needs you"
     :path (atty/ui:label (format nil "~D waiting~@[ · oldest ~A~]" (length rows) (and oldest (duration oldest)))
                          :face :quiet)
     :toolbar (toolbar (selector "sort" (if (eq :name (queue-order q)) "by name" "oldest first") :runs '(:sort) :key "s")
                       (toggle "every session" (queue-every q) :runs '(:toggle :every) :key "a")
                       (toggle "answered lately" (queue-showing-lately q) :runs '(:toggle :lately) :key "l")
                       (field "/" (queue-query q) :cursor (queue-filtering q) :runs '(:filter) :width 24)
                       :right
                       (keycap "↵" "go" :runs "queue go")
                       (keycap "e" "why" :runs "queue explain")
                       (keycap "r" "read" :runs "queue read")
                       (keycap "p" "prompt" :runs "queue prompt"))
     :body (multiple-value-bind (from most) (queue-window q client rows)
             (atty/ui:row
              :align :stretch :expand 1 :spacing 0
              (apply #'atty/ui:column :align :stretch :expand 3
                     (append
                      (list (atty/ui:label ""))
                      (if rows
                          (loop :for row :in (subseq rows from (min (length rows) (+ from most)))
                                :for i :from from
                                :collect (queue-item client row now i (= i (queue-index q))))
                          (list (atty/ui:label "   nothing needs you" :face :quiet)
                                (atty/ui:label "")))
                      (when (and (queue-showing-lately q) (client-lately client))
                        (cons (section "answered lately")
                              (mapcar (lambda (e) (lately-line client e)) (client-lately client))))
                      (list (atty/ui:gap :expand 1))))
              (rail from most (max 1 (length rows)))
              (atty/ui:rule :upright t :face :card)
              (queue-preview client chosen)))
     :hints (hints 'queue-mode "↑↓" "choose" "1-9" "answer in place"
                   'queue-go "go there" 'queue-explain "why it thinks so" 'queue-read "read"
                   'queue-prompt "prompt instead" 'queue-filter "filter"))))

(defmethod draw-over ((q queue) screen)
  (let ((client *drawing-for*))
    (setf (queue-index q) (max 0 (min (queue-index q)
                                      (1- (length (queue-rows q client))))))
    (setf (queue-laid q) (draw-overlay (queue-tree q client) client screen))))

(defmethod laid-tree ((q queue)) (queue-laid q))

(atty/mode:define-mode queue-mode ())

;;; Typing a filter is its own mode, so every letter is the filter's while it is
;;; being typed rather than the command it would be otherwise.
(atty/mode:define-mode queue-filter-mode ())

(defmethod mode-of ((q queue))
  (if (queue-filtering q) 'queue-filter-mode 'queue-mode))

(defmethod ticks-p ((q queue)) t)
(defmethod over-name ((q queue)) "needs you")
(defmethod close-over ((q queue) client) (queue-close-it q client))

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

(defcommand (queue-sort :unlisted)
  "the questions oldest first, or by where they are"
  (let ((q (the-queue)))
    (when q (setf (queue-order q) (if (eq :name (queue-order q)) :oldest :name)))))

(defcommand (queue-every-session :unlisted)
  "every session's questions, or this one's"
  (let ((q (the-queue)))
    (when q (setf (queue-every q) (not (queue-every q)) (queue-index q) 0))))

(defcommand (queue-lately :unlisted)
  "what was answered lately shown under the questions, or not"
  (let ((q (the-queue)))
    (when q (setf (queue-showing-lately q) (not (queue-showing-lately q))))))

(defcommand (queue-click :unlisted)
  "a click on a question picks it; on an answer, answers; on a control, does
what it says"
  (let* ((q (the-queue))
         (hit (and q (queue-laid q) *mouse-at*
                   (button-at (queue-laid q) (cdr *mouse-at*) (car *mouse-at*)))))
    (when hit
      (let ((runs (bar-button-runs hit)))
        (case (and (consp runs) (first runs))
          (:pick (setf (queue-index q) (second runs) (client-dirty *client*) t))
          (:answer (tell-the-server runs))
          (:sort (queue-sort))
          (:toggle (ecase (second runs) (:every (queue-every-session)) (:lately (queue-lately))))
          (:filter (queue-filter))
          (t (generic-click q runs *client*)))))))

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
(atty/mode:define-key 'queue-mode "s"      #'queue-sort)
(atty/mode:define-key 'queue-mode "a"      #'queue-every-session)
(atty/mode:define-key 'queue-mode "l"      #'queue-lately)
(atty/mode:define-key 'queue-mode "?"      "keys of this mode")
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
