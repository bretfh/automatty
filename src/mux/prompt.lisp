;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(declaim (ftype function hints key-for keep-told stop-told rows-of button-at scroll-mode
                tell-the-server-if-it-knows))

;;; Reading one thing from a list of things. It is drawn over the session rather
;;; than composed into it, because it belongs to whoever opened it: somebody
;;; else attached to the same session is still looking at their shell.
;;;
;;; The palette is four of these with tabs: the commands, the sessions and
;;; windows, the clients, and find. Typing another kind's prefix at the start
;;; switches to it, so does TAB, and the bar's field opens whichever is
;;; current there.

(defstruct (prompt (:constructor %make-prompt))
  (title "" :type string)
  (query "" :type string)
  (items nil :type list)
  (items-fn nil)                        ; the items, worked out afresh each time, when they move
  (text #'identity)                     ; an item as a string, or as a widget
  (index 0 :type fixnum)
  (most 8 :type fixnum)
  (chose nil)
  (dropped nil)
  (kind nil)                            ; the prefix that opens this kind, when it is one of the palette's
  (free nil)                            ; what was typed is the answer, and the items are only lines
  (narrow t)                            ; whether what is typed narrows the items
  (alt nil)
  (into nil)
  (foot nil)
  (third nil)                           ; what C-f does with the choice, when there is a third thing
  (preview nil)                         ; a function of the chosen item and the client answering a tree
  (on-move nil)                         ; called with the chosen item when the choice moves
  (live nil)                            ; called with what is typed as it is typed
  (controls nil)                        ; a function of the prompt answering toolbar parts
  (laid nil))

(defun make-prompt (title items &key (text #'identity) chose dropped (most 8) kind
                                     free (narrow t) (query "") alt into foot items-fn preview moved live controls third)
  "A prompt titled TITLE offering ITEMS. A FREE one takes what was typed rather
than one of its items: they are only lines saying what to type, and are not
narrowed by it. ALT is what C-RET does with the choice, INTO what TAB does, and
FOOT a line under the items saying what the keys do."
  (%make-prompt :title title :items items :text text :query query
                :chose chose :dropped dropped :most most :kind kind :free free :narrow narrow
                :alt alt :into into :foot foot :items-fn items-fn :preview preview
                :on-move moved :live live :controls controls :third third))

(defun prompt-all (p)
  (if (prompt-items-fn p)
      (funcall (prompt-items-fn p) (or *drawing-for* *client*))
      (prompt-items p)))

(defun prompt-showing (p)
  (if (or (prompt-free p) (not (prompt-narrow p)))
      (prompt-all p)
      (matches (prompt-query p) (prompt-all p) (prompt-text p))))

(defun prompt-chosen (p)
  (nth (prompt-index p) (prompt-showing p)))

(defparameter +palette+
  '((#\: "commands" "run a command")
    (#\@ "windows" "choose a window")
    (#\# "clients" "clients")
    (#\/ "find" "find in pane"))
  "The palette's kinds: the prefix that opens each, what its tab says, and
the command that opens it.")

(defparameter +prompt-toggles+
  (mapcar (lambda (kind) (cons (first kind) (third kind))) +palette+)
  "The prefix that opens each kind of prompt, and the command that opens it.
Typing one as the first character of an empty query, in a prompt of a
different kind, switches to it instead of being searched for.")

(defun prompt-tabs (p)
  "The palette's tabs, the current one lit; a click on another opens it."
  (apply #'atty/ui:row :spacing 0
         (loop :for (prefix name runs) :in +palette+
               :for active := (eql prefix (prompt-kind p))
               :collect (bar-button runs
                                    (atty/ui:row :spacing 0
                                                 (atty/ui:label (format nil " ~C " prefix)
                                                                :face (if active :brand :quiet))
                                                 (atty/ui:label (format nil " ~A " name)
                                                                :face (if active :tab-active :quiet))
                                                 (atty/ui:label " "))))))

(defun prompt-row (p item i said)
  "One item as a row: a button that picks it, marked and lit where the choice is."
  (let ((chosen (= i (prompt-index p))))
    (if (prompt-free p)
        (atty/ui:label (format nil "   ~A" said))
        (gutter-row (if chosen "▶" "")
                    (if (stringp said) (atty/ui:label said) said)
                    :selected chosen :runs (list :pick i)))))

(defun prompt-tree (p client cols)
  (let* ((showing (prompt-showing p))
         (all (prompt-all p))
         (room (min (prompt-most p) (length showing)))
         (from (max 0 (min (- (length showing) room)
                           (- (prompt-index p) (floor room 2)))))
         (rows (loop :for item :in (subseq showing from (+ from room))
                     :for i :from from
                     :collect (prompt-row p item i (funcall (prompt-text p) item))))
         (chosen (prompt-chosen p))
         (preview (and (prompt-preview p) chosen (funcall (prompt-preview p) chosen client)))
         (count (if (prompt-free p) "" (format nil "~D of ~D" (length showing) (length all)))))
    (apply #'atty/ui:column
           :align :stretch
           :background-color (bar-face :bg-dim)
           :min-width cols
           (append
            (list (if (prompt-kind p)
                      (band (list (prompt-tabs p))
                            (list (atty/ui:label (format nil " ~A " count) :face :quiet)
                                  (close-button) (atty/ui:label " ")))
                      (header-band (prompt-title p)
                                   :right (atty/ui:label (format nil " ~A " count) :face :quiet)))
                  (apply #'toolbar
                         (field (if (prompt-kind p) (string (prompt-kind p)) "›") (prompt-query p))
                         (append (when (prompt-controls p) (list :right))
                                 (and (prompt-controls p) (funcall (prompt-controls p) p))))
                  (atty/ui:row
                   :align :stretch :spacing 0
                   (apply #'atty/ui:column :align :stretch :expand 2
                          (or rows (list (atty/ui:label "   nothing matches that" :face :quiet))))
                   (rail from (max 1 room) (max 1 (length showing)))
                   (if preview
                       (atty/ui:row :align :stretch :spacing 0 :expand 1
                                    (atty/ui:rule :upright t :face :card)
                                    (atty/ui:column :align :stretch :expand 1 preview))
                       (atty/ui:label ""))))
            (and (prompt-foot p)
                 (list (footer-band (prompt-foot p)
                                    :right (list (hint "Esc" "close" :runs :close)))))))))

(defmethod draw-over ((p prompt) screen)
  (let* ((client *drawing-for*)
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows))
         (tree (prompt-tree p client cols))
         (high (nth-value 1 (atty/ui:with-pass
                              (atty/ui:restyle tree)
                              (atty/ui:measure tree m cols rows))))
         (top (if (or (null client) (client-barp client)) (min 1 (max 0 (1- rows))) 0))
         (bottom (min rows (+ top high))))
    (atty/cells:draw tree (tty:screen-grid screen) cols bottom :top top)
    (setf (prompt-laid p) tree)
    (setf (tty:screen-cursor-y screen) (min (1- rows) (1+ top))
          (tty:screen-cursor-x screen) (min (1- cols) (+ 4 (length (prompt-query p))))
          (tty:screen-cursor-visible screen) t)))

(defmethod laid-tree ((p prompt)) (prompt-laid p))

(defun prompt-close (p client)
  "Take it away. Nothing else needs doing: what it was covering is still in the
screen it was drawn over, and the diff puts it back."
  (client-over-drop client p))

(atty/mode:define-mode prompt-mode ())

(defmethod mode-of ((p prompt)) 'prompt-mode)
(defmethod over-name ((p prompt)) (prompt-title p))
(defmethod close-over ((p prompt) client)
  (prompt-close p client)
  (when (prompt-dropped p) (funcall (prompt-dropped p) client)))

(defun the-prompt ()
  (let ((it (first (client-over *client*))))
    (when (typep it 'prompt) it)))

(defun prompt-moved (p to)
  (let ((most (max 0 (1- (length (prompt-showing p))))))
    (setf (prompt-index p) (max 0 (min most to)))
    (when (prompt-on-move p)
      (let ((it (prompt-chosen p)))
        (when it (funcall (prompt-on-move p) it *client*))))))

(defun prompt-typed (p client)
  "What was typed changed: the choice starts over, and a live prompt is told."
  (setf (prompt-index p) 0
        (client-dirty client) t)
  (when (prompt-live p)
    (let ((*client* client)) (funcall (prompt-live p) (prompt-query p) client))))

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
            (progn (close-over p client) (run-command runs client))
            (progn (setf (prompt-query p) (concatenate 'string (prompt-query p) said))
                   (prompt-typed p client))))
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
        (setf (prompt-query p) (subseq q 0 (max 0 (1- (length q)))))
        (prompt-typed p *client*)))))

(defcommand (prompt-clear :unlisted)
  (let ((p (the-prompt)))
    (when p (setf (prompt-query p) "") (prompt-typed p *client*))))

(defcommand (prompt-accept :unlisted)
  (let ((p (the-prompt)))
    (when p
      (let ((it (if (prompt-free p) (prompt-query p) (or (prompt-chosen p) (prompt-query p)))))
        (prompt-close p *client*)
        (when (and it (prompt-chose p)) (funcall (prompt-chose p) it *client*))))))

(defcommand (prompt-cancel :unlisted)
  (let ((p (the-prompt)))
    (when p (close-over p *client*))))

(defcommand (prompt-accept-otherwise :unlisted)
  "C-RET: the other thing a prompt does with the choice, when it has one."
  (let ((p (the-prompt)))
    (when (and p (prompt-alt p))
      (let ((it (if (prompt-free p) (prompt-query p) (prompt-chosen p))))
        (prompt-close p *client*)
        (when it (funcall (prompt-alt p) it *client*))))))

(defcommand (prompt-accept-thirdly :unlisted)
  "C-f: the third thing a prompt does with the choice, when it has one."
  (let ((p (the-prompt)))
    (when (and p (prompt-third p))
      (let ((it (prompt-chosen p)))
        (prompt-close p *client*)
        (when it (funcall (prompt-third p) it *client*))))))

(defun next-kind (p)
  "The palette kind after P's, round the end."
  (let ((at (position (prompt-kind p) +palette+ :key #'first)))
    (and at (nth (mod (1+ at) (length +palette+)) +palette+))))

(defcommand (prompt-go-into :unlisted)
  "TAB: go into the choice when the prompt has somewhere to go, else over to
the palette's next kind."
  (let ((p (the-prompt)))
    (cond
      ((and p (prompt-into p))
       (let ((it (if (prompt-free p) (prompt-query p) (prompt-chosen p))))
         (prompt-close p *client*)
         (funcall (prompt-into p) it *client*)))
      ((and p (next-kind p))
       (let ((kind (next-kind p)))
         (close-over p *client*)
         (run-command (third kind) *client*))))))

(defcommand (prompt-click :unlisted)
  "A click on a row chooses it; on a tab, opens that kind; on a control, does
what it says; on the field, nothing."
  (let* ((p (the-prompt))
         (hit (and p (prompt-laid p) *mouse-at*
                   (button-at (prompt-laid p) (cdr *mouse-at*) (car *mouse-at*))))
         (runs (and hit (bar-button-runs hit))))
    (when hit
      (cond
        ((and (consp runs) (eq :pick (first runs)))
         (setf (prompt-index p) (second runs))
         (prompt-accept))
        ((stringp runs)
         (close-over p *client*)
         (run-command runs *client*))
        (t (generic-click p runs *client*))))))

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
(atty/mode:define-key 'prompt-mode "C-f"      #'prompt-accept-thirdly)
(atty/mode:define-key 'prompt-mode "TAB"      #'prompt-go-into)
(atty/mode:define-key 'prompt-mode "mouse-1"  #'prompt-click)
(atty/mode:define-key 'prompt-mode "mouse-1-up" #'prompt-nothing)
(atty/mode:define-key 'prompt-mode "wheel-up"   #'prompt-previous)
(atty/mode:define-key 'prompt-mode "wheel-down" #'prompt-next)

(defun ask (client title items &key (text #'identity) chose dropped kind free (narrow t) (query "")
                                      alt into foot items-fn preview moved live controls third)
  "Put a prompt over whatever CLIENT is showing."
  (client-over-put client (make-prompt title items :text text
                                                   :chose chose
                                                   :dropped dropped
                                                   :kind kind
                                                   :free free :narrow narrow
                                                   :query query
                                                   :alt alt :into into :foot foot
                                                   :items-fn items-fn :preview preview
                                                   :moved moved :live live :controls controls
                                                   :third third)))

(defun ask-a-name (client session id label title &key address)
  "Ask what to call pane ID of SESSION, on the line at the foot, starting
from what it is called now. TAB goes over to naming the window it is in."
  (entry client "name"
         (format nil "pane ~A~@[ · ~A~]" (or address (format nil "~A:~D" session id))
                 (and (null label) title (plusp (length title)) title))
         (or label "")
         :keep (lambda (typed c)
                 (let ((*client* c))
                   (tell-the-server (list :name-pane session id typed))))
         :swap (lambda (c)
                 (let ((*client* c)) (tell-the-server (list :naming-window))))
         :swap-says "window instead"))

(defun ask-a-window-name (client session n label)
  "Ask what to call window N of SESSION. TAB goes over to naming the pane."
  (entry client "name"
         (format nil "window ~A › ~D" session n)
         (or label "")
         :keep (lambda (typed c)
                 (let ((*client* c))
                   (tell-the-server (list :name-window session n typed))))
         :swap (lambda (c)
                 (let ((*client* c)) (tell-the-server (list :naming))))
         :swap-says "pane instead"))

;;; The palette's kinds.

(defun command-line (name)
  "NAME as the prompt offers it: the name, its key when it has one, and a line
on what it does."
  (let ((key (key-for (intern (string-upcase (substitute #\- #\Space name)) :atty))))
    (format nil "~24A ~10A ~@[~A~]" name (or key "") (command-doc name))))

(defun ask-a-command (client)
  (ask client "commands" (command-names) :kind #\:
       :text #'command-line
       :foot (hints 'prompt-mode 'prompt-accept "run" 'prompt-go-into "next kind")
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
  (let ((here (if (getf c :here) "◆ here" "      "))
        (session (getf c :session))
        (window (if (getf c :window)
                    (format nil "› ~D ~A" (getf c :window) (or (getf c :label) ""))
                    ""))
        (asking (if (plusp (getf c :asking)) (format nil "▲ ~D" (getf c :asking)) ""))
        (panes (format nil "~D pane~:P" (getf c :panes))))
    (format nil "~A ~8A ~10A ~@[~A ~]~A" here session window
            (and (plusp (length asking)) asking) panes)))

(defun window-preview (c client)
  "The panes of the window C names, each a line: its state, name, kind, and
what it is doing."
  (let ((panes (sort (remove-if-not (lambda (r) (and (equal (getf c :session) (getf r :session))
                                                     (eql (getf c :window) (getf r :window))))
                                    (rows-of client))
                     #'< :key (lambda (r) (or (getf r :at) 0)))))
    (apply #'atty/ui:column :align :stretch
           (band (list (atty/ui:label (format nil " ~A › ~D~@[ ~A~]" (getf c :session) (getf c :window) (getf c :label))
                                      :face :quiet))
                 nil)
           (if panes
               (loop :for r :in panes
                     :collect (let ((state (and (getf r :known) (getf r :state))))
                                (atty/ui:row :spacing 0
                                             (atty/ui:label (format nil " ~A " (state-glyph state))
                                                            :face (if state (number-face state) :number-unknown))
                                             (atty/ui:label (format nil " ~D ~A" (1+ (or (getf r :at) 0)) (or (getf r :label) (getf r :says) ""))
                                                            :face :strong)
                                             (atty/ui:label (format nil " ~A" (or (getf r :kind) "")) :face :quiet)
                                             (atty/ui:label (format nil "  ~A" (shortened-to (or (getf (getf r :asks) :subject) (getf r :doing) "") 40))
                                                            :face (if (eq state :blocked) :state-blocked-strong :quiet)))))
               (list (atty/ui:label "  asking what is there…" :face :quiet))))))

(defun ask-a-window (client rows)
  (keep-told client)
  (ask client "windows" (window-choices rows (client-session client)) :kind #\@
       :text #'window-choice-line
       :preview #'window-preview
       :foot (hints 'prompt-mode 'prompt-accept "go there" 'prompt-accept-otherwise "new window there"
                    'prompt-go-into "its panes")
       :chose (lambda (c client)
                (stop-told client)
                (wire-send (client-wire client) (list :go (getf c :session)))
                (when (getf c :window)
                  (wire-send (client-wire client) (list :go-window (getf c :session) (getf c :window)))))
       :alt (lambda (c client)
              (stop-told client)
              (wire-send (client-wire client) (list :go (getf c :session)))
              (wire-send (client-wire client) (list :new-window (getf c :session))))
       :into (lambda (c client)
               (stop-told client)
               (let ((*client* client))
                 (ask-panes-of client (getf c :session) (getf c :window))))
       :dropped (lambda (client) (stop-told client))))

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

;;; The clients: every terminal attached, and what each looks at.

(defun client-line (c client)
  "One attached terminal as a row: who, how big, what it looks at, how long."
  (if (null c)
      (atty/ui:label "nobody is attached yet; asking…" :face :quiet)
      (atty/ui:row :spacing 0
                   (who-client c client :pad 12)
                   (atty/ui:label (format nil " ~Dx~D  " (fourth c) (third c)) :face :quiet)
                   (path '(:quiet "default") (or (fifth c) "nothing") (and (sixth c) (format nil "~D" (sixth c))))
                   (atty/ui:label (format nil "   attached ~A~@[   typed ~A ago~]"
                                          (duration (seventh c)) (and (eighth c) (duration (eighth c))))
                                  :face :quiet)
                   (atty/ui:label (cond ((here-p c client) "   this terminal")
                                        ((and (ninth c) (eql (ninth c) (client-id client))) "   following this terminal")
                                        ((ninth c) (format nil "   following ~A" (let ((led (find (ninth c) (client-clients client) :key #'first)))
                                                                                   (if led (short-tty (second led) (first led)) (ninth c)))))
                                        (t ""))
                                  :face :client))))

(defun ask-the-clients (client)
  (keep-told client)
  (ask client "clients" nil :kind #\#
       :items-fn (lambda (client) (or (client-clients client) (list nil)))
       :narrow nil
       :text (lambda (c) (client-line c client))
       :foot (hints 'prompt-mode 'prompt-accept "go to what it sees" 'prompt-accept-thirdly "follow it"
                    'prompt-accept-otherwise "detach it")
       :chose (lambda (c client)
                (stop-told client)
                (when (and c (fifth c))
                  (wire-send (client-wire client) (list :go (fifth c)))
                  (when (sixth c)
                    (wire-send (client-wire client) (list :go-window (fifth c) (sixth c))))))
       :third (lambda (c client)
                (stop-told client)
                (when (and c (not (here-p c client)))
                  (let ((*client* client)) (tell-the-server-if-it-knows (list :follow (first c))))))
       :alt (lambda (c client)
              (stop-told client)
              (when c (wire-send (client-wire client) (list :detach-client (first c)))))
       :dropped (lambda (client) (stop-told client))))

;;; Find: the hits in the pane's history as rows, the pane scrolled to the
;;; one chosen as the choice moves.

(defun hit-line (hit)
  "A hit as a row: its line number dim, the row's text with the find lit."
  (destructuring-bind (row start end text &optional current) hit
    (let ((text (or text "")))
      (atty/ui:row :spacing 0
                   (atty/ui:label (format nil " ~6:D " (1+ row)) :face :quiet)
                   (atty/ui:label (subseq text 0 (min start (length text))))
                   (atty/ui:label (subseq text (min start (length text)) (min end (length text)))
                                  :face (if current :find-hit-current :find-hit))
                   (atty/ui:label (subseq text (min end (length text))))))))

(defun found-hits (client)
  "The hits the server said, the current one marked."
  (let* ((found (client-found client))
         (hits (getf found :hits))
         (current (getf found :current)))
    (loop :for hit :in hits
          :for i :from 0
          :collect (append hit (list (eql i current))))))

(defun ask-a-find (client)
  "Find in the pane with the focus: the pane is read back, and what is typed
is found as it is typed."
  (let ((*client* client)) (scroll-mode))
  (ask client "find" nil :kind #\/
       :items-fn #'found-hits
       :narrow nil
       :query (or (client-find-text client) "")
       :text #'hit-line
       :foot (hints 'prompt-mode "↑↓" "hit, the pane follows" 'prompt-accept "keep it and leave"
                    'prompt-accept-otherwise "copy its line")
       :live (lambda (typed client)
               (let ((*client* client))
                 (setf (client-find-text client) typed)
                 (tell-the-server-if-it-knows (list :find nil nil typed :here))))
       :moved (lambda (hit client)
                (let ((*client* client))
                  (tell-the-server-if-it-knows (list :find nil nil nil (list :row (first hit))))))
       :chose (lambda (it client) (declare (ignore it client)))
       :alt (lambda (hit client)
              (let ((*client* client))
                (when (consp hit)
                  (tell-the-server-if-it-knows (list :select (list :line (first hit)))))))
       :dropped (lambda (client)
                  (let ((*client* client))
                    (tell-the-server-if-it-knows (list :find nil nil nil :clear))))))

(defun open-the-palette (client kind)
  "Open the palette on KIND: :commands, :windows, :clients or :find."
  (ecase kind
    (:commands (ask-a-command client))
    (:windows (let ((*client* client)) (tell-the-server (list :sessions))))
    (:clients (ask-the-clients client))
    (:find (ask-a-find client))))
