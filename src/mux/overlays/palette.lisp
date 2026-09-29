;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun prompt-pane-name (watcher session id label title &key address)
  "Ask what to call pane ID of SESSION, on the line at the foot, starting
from what it is called now. TAB goes over to naming the window it is in."
  (entry watcher "name"
         (format nil "pane ~A~@[ · ~A~]" (or address (format nil "~A:~D" session id))
                 (and (null label) title (plusp (length title)) title))
         (or label "")
         :keep (lambda (typed w) (name-pane (watcher-server w) session id typed))
         :swap (lambda (w) (run-command "rename window" w))
         :swap-says "window instead"))

(declaim (ftype (function (watcher string integer (or null string)) t) prompt-window-name))
(defun prompt-window-name (watcher session n label)
  "Ask what to call window N of SESSION. TAB goes over to naming the pane."
  (entry watcher "name"
         (format nil "window ~A › ~D" session n)
         (or label "")
         :keep (lambda (typed w)
                 (let* ((it (session-named (watcher-server w) session))
                        (window (and it (session-nth-window it n))))
                   (when window (session-rename-window it window typed))))
         :swap (lambda (w) (run-command "rename pane" w))
         :swap-says "pane instead"))

(defun command-item-line (name)
  "NAME as the prompt offers it: the name, its key when it has one, and a line
on what it does, dim, cut where the row ends."
  (let ((key (command-key (intern (string-upcase (substitute #\- #\Space name)) :atty))))
    (squeezed (atty/ui:row :spacing 0
                           (atty/ui:label (format nil "~24A " name))
                           (atty/ui:label (format nil "~10A " (or key "")) :face :key-hint)
                           (atty/ui:label (or (command-doc name) "") :face :quiet)))))

(defun prompt-command (watcher)
  (open-prompt watcher "commands" (command-names) :kind #\:
       :text #'command-item-line
       :foot (hints 'prompt-mode 'prompt-accept "run" 'prompt-descend "next kind")
       :chose (lambda (name watcher) (run-command name watcher))))

(defun window-choices (rows here)
  "Every session › window the server said, the ones asking first, from what
:these says: (name rows cols panes watching blocked windows). HERE is the
session this watcher is on: the window it shows is marked."
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
  "One line: the mark for where this watcher is, the session and window, what
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

(defun window-preview (c watcher)
  "The panes of the window C names, each a line: its state, name, kind, and
what it is doing."
  (let ((panes (sort (remove-if-not (lambda (r) (and (equal (getf c :session) (getf r :session))
                                                     (eql (getf c :window) (getf r :window))))
                                    (pane-rows-of watcher))
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
                                             (atty/ui:label (format nil "  ~A" (truncate-string (or (getf (getf r :asks) :subject) (getf r :doing) "") 40))
                                                            :face (if (eq state :blocked) :state-blocked-strong :quiet)))))
               (list (atty/ui:label "  asking what is there…" :face :quiet))))))

(defun prompt-window (watcher rows)
  (open-prompt watcher "windows" (window-choices rows (watcher-session-name watcher)) :kind #\@
       :text #'window-choice-line
       :preview #'window-preview
       :foot (hints 'prompt-mode 'prompt-accept "go there" 'prompt-accept-alternate "new window there"
                    'prompt-descend "its panes")
       :chose (lambda (c watcher) (go-to watcher (getf c :session) (getf c :window)))
       :alt (lambda (c watcher)
              (let ((session (go-to watcher (getf c :session))))
                (when session (session-add-window session))))
       :into (lambda (c watcher) (prompt-panes watcher (getf c :session) (getf c :window)))))

(defun prompt-panes (watcher session window)
  "The panes of one window, to go to one."
  (let ((panes (sort (remove-if-not (lambda (r) (and (equal session (getf r :session))
                                                     (eql window (getf r :window))))
                                    (pane-rows-of watcher))
                     #'< :key (lambda (r) (or (getf r :at) 0)))))
    (open-prompt watcher (format nil "panes of ~A › ~D" session window) panes
         :text (lambda (r) (format nil "~D  ~12A ~10A ~@[~(~A~)~]"
                                   (1+ (or (getf r :at) 0)) (or (getf r :says) "")
                                   (or (getf r :kind) "") (and (getf r :known) (getf r :state))))
         :chose (lambda (r watcher)
                  (focus-pane (watcher-server watcher) watcher (getf r :session) (getf r :id))))))

(defun client-line (c watcher)
  "One attached terminal as a row: who, how big, what it looks at, how long."
  (if (null c)
      (atty/ui:label "nobody is attached yet; asking…" :face :quiet)
      (atty/ui:row :spacing 0
                   (client-label c watcher :pad 12)
                   (atty/ui:label (format nil " ~Dx~D  " (fourth c) (third c)) :face :quiet)
                   (path '(:quiet "default") (or (fifth c) "nothing") (and (sixth c) (format nil "~D" (sixth c))))
                   (atty/ui:label (format nil "   attached ~A~@[   typed ~A ago~]"
                                          (format-duration (seventh c)) (and (eighth c) (format-duration (eighth c))))
                                  :face :quiet)
                   (atty/ui:label (cond ((this-client-p c watcher) "   this terminal")
                                        ((and (ninth c) (eql (ninth c) (watcher-id watcher))) "   following this terminal")
                                        ((ninth c) (format nil "   following ~A" (let ((led (find (ninth c) (clients-of watcher) :key #'first)))
                                                                                   (if led (format-tty (second led) (first led)) (ninth c)))))
                                        (t ""))
                                  :face :client))))

(defun prompt-clients (watcher)
  (open-prompt watcher "clients" nil :kind #\#
       :items-fn (lambda (watcher) (or (clients-of watcher) (list nil)))
       :narrow nil
       :text (lambda (c) (client-line c watcher))
       :foot (hints 'prompt-mode 'prompt-accept "go to what it sees" 'prompt-accept-third "follow it"
                    'prompt-accept-alternate "detach it")
       :chose (lambda (c watcher)
                (when (and c (fifth c))
                  (go-to watcher (fifth c) (sixth c))))
       :third (lambda (c watcher)
                (when (and c (not (this-client-p c watcher)))
                  (follow (watcher-server watcher) watcher (first c))))
       :alt (lambda (c watcher)
              (let* ((server (watcher-server watcher))
                     (it (and c (find (first c) (all-watchers server) :key #'watcher-id))))
                (when (and it (watcher-interactive it))
                  (drop-watcher server it :detached)
                  (send-client-list server))))))

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

(declaim (ftype (function (watcher integer list (or null integer)) t) found-in-pane))
(defun found-in-pane (watcher n hits current)
  "WATCHER's find found N, HITS near the one gone to, CURRENT among them."
  (setf (watcher-found watcher) (list :n n :hits hits :current current)
        (watcher-behind watcher) t)
  (let ((top (first (watcher-overlays watcher))))
    (if (and (typep top 'prompt) (eql #\/ (prompt-kind top)))
        (setf (prompt-index top) (or current 0))
        (when (zerop n) (show-note watcher "find" "nothing has that in it" :face :warning)))))

(defun found-hits (watcher)
  "The hits found, the current one marked."
  (let* ((found (watcher-found watcher))
         (hits (getf found :hits))
         (current (getf found :current)))
    (loop :for hit :in hits
          :for i :from 0
          :collect (append hit (list (eql i current))))))

(defun prompt-search (watcher)
  "Find in the pane with the focus: the pane is read back, and what is typed
is found as it is typed."
  (let ((*client* watcher)) (scroll-mode))
  (open-prompt watcher "find" nil :kind #\/
       :items-fn #'found-hits
       :narrow nil
       :query (or (watcher-find-text watcher) "")
       :text #'hit-line
       :foot (hints 'prompt-mode "↑↓" "hit, the pane follows" 'prompt-accept "keep it and leave"
                    'prompt-accept-alternate "copy its line")
       :live (lambda (typed watcher)
               (setf (watcher-find-text watcher) typed)
               (search-pane (watcher-server watcher) watcher nil nil typed :here))
       :moved (lambda (hit watcher)
                (search-pane (watcher-server watcher) watcher nil nil nil (list :row (first hit))))
       :chose (lambda (it watcher) (declare (ignore it watcher)))
       :alt (lambda (hit watcher)
              (when (consp hit)
                (select-pane-rows watcher (list :line (first hit)))))
       :dropped (lambda (watcher)
                  (search-pane (watcher-server watcher) watcher nil nil nil :clear))))

(defun open-palette (watcher kind)
  "Open the palette on KIND: :commands, :windows, :clients or :find."
  (ecase kind
    (:commands (prompt-command watcher))
    (:windows (prompt-window watcher (session-list (watcher-server watcher))))
    (:clients (prompt-clients watcher))
    (:find (prompt-search watcher))))
