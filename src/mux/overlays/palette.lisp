;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun prompt-pane-name (client session id label title &key address)
  "Ask what to call pane ID of SESSION, on the line at the foot, starting
from what it is called now. TAB goes over to naming the window it is in."
  (entry client "name"
         (format nil "pane ~A~@[ · ~A~]" (or address (format nil "~A:~D" session id))
                 (and (null label) title (plusp (length title)) title))
         (or label "")
         :keep (lambda (typed c)
                 (let ((*client* c))
                   (send-to-server (list :name-pane session id typed))))
         :swap (lambda (c)
                 (let ((*client* c)) (send-to-server (list :naming-window))))
         :swap-says "window instead"))

(defun prompt-window-name (client session n label)
  "Ask what to call window N of SESSION. TAB goes over to naming the pane."
  (entry client "name"
         (format nil "window ~A › ~D" session n)
         (or label "")
         :keep (lambda (typed c)
                 (let ((*client* c))
                   (send-to-server (list :name-window session n typed))))
         :swap (lambda (c)
                 (let ((*client* c)) (send-to-server (list :naming))))
         :swap-says "pane instead"))

(defun command-item-line (name)
  "NAME as the prompt offers it: the name, its key when it has one, and a line
on what it does."
  (let ((key (command-key (intern (string-upcase (substitute #\- #\Space name)) :atty))))
    (format nil "~24A ~10A ~@[~A~]" name (or key "") (command-doc name))))

(defun prompt-command (client)
  (open-prompt client "commands" (command-names) :kind #\:
       :text #'command-item-line
       :foot (hints 'prompt-mode 'prompt-accept "run" 'prompt-descend "next kind")
       :chose (lambda (name client) (run-command name client))))

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
                                    (client-pane-rows client))
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

(defun prompt-window (client rows)
  (subscribe-panes client)
  (open-prompt client "windows" (window-choices rows (client-session client)) :kind #\@
       :text #'window-choice-line
       :preview #'window-preview
       :foot (hints 'prompt-mode 'prompt-accept "go there" 'prompt-accept-alternate "new window there"
                    'prompt-descend "its panes")
       :chose (lambda (c client)
                (unsubscribe-panes client)
                (wire-send (client-wire client) (list :go (getf c :session)))
                (when (getf c :window)
                  (wire-send (client-wire client) (list :go-window (getf c :session) (getf c :window)))))
       :alt (lambda (c client)
              (unsubscribe-panes client)
              (wire-send (client-wire client) (list :go (getf c :session)))
              (wire-send (client-wire client) (list :new-window (getf c :session))))
       :into (lambda (c client)
               (unsubscribe-panes client)
               (let ((*client* client))
                 (prompt-panes client (getf c :session) (getf c :window))))
       :dropped (lambda (client) (unsubscribe-panes client))))

(defun prompt-panes (client session window)
  "The panes of one window, to go to one."
  (let ((panes (sort (remove-if-not (lambda (r) (and (equal session (getf r :session))
                                                     (eql window (getf r :window))))
                                    (client-pane-rows client))
                     #'< :key (lambda (r) (or (getf r :at) 0)))))
    (subscribe-panes client)
    (open-prompt client (format nil "panes of ~A › ~D" session window) panes
         :text (lambda (r) (format nil "~D  ~12A ~10A ~@[~(~A~)~]"
                                   (1+ (or (getf r :at) 0)) (or (getf r :says) "")
                                   (or (getf r :kind) "") (and (getf r :known) (getf r :state))))
         :chose (lambda (r client)
                  (unsubscribe-panes client)
                  (wire-send (client-wire client) (list :focus-pane (getf r :session) (getf r :id))))
         :dropped (lambda (client) (unsubscribe-panes client)))))

(defun client-line (c client)
  "One attached terminal as a row: who, how big, what it looks at, how long."
  (if (null c)
      (atty/ui:label "nobody is attached yet; asking…" :face :quiet)
      (atty/ui:row :spacing 0
                   (client-label c client :pad 12)
                   (atty/ui:label (format nil " ~Dx~D  " (fourth c) (third c)) :face :quiet)
                   (path '(:quiet "default") (or (fifth c) "nothing") (and (sixth c) (format nil "~D" (sixth c))))
                   (atty/ui:label (format nil "   attached ~A~@[   typed ~A ago~]"
                                          (format-duration (seventh c)) (and (eighth c) (format-duration (eighth c))))
                                  :face :quiet)
                   (atty/ui:label (cond ((this-client-p c client) "   this terminal")
                                        ((and (ninth c) (eql (ninth c) (client-id client))) "   following this terminal")
                                        ((ninth c) (format nil "   following ~A" (let ((led (find (ninth c) (client-clients client) :key #'first)))
                                                                                   (if led (format-tty (second led) (first led)) (ninth c)))))
                                        (t ""))
                                  :face :client))))

(defun prompt-clients (client)
  (subscribe-panes client)
  (open-prompt client "clients" nil :kind #\#
       :items-fn (lambda (client) (or (client-clients client) (list nil)))
       :narrow nil
       :text (lambda (c) (client-line c client))
       :foot (hints 'prompt-mode 'prompt-accept "go to what it sees" 'prompt-accept-third "follow it"
                    'prompt-accept-alternate "detach it")
       :chose (lambda (c client)
                (unsubscribe-panes client)
                (when (and c (fifth c))
                  (wire-send (client-wire client) (list :go (fifth c)))
                  (when (sixth c)
                    (wire-send (client-wire client) (list :go-window (fifth c) (sixth c))))))
       :third (lambda (c client)
                (unsubscribe-panes client)
                (when (and c (not (this-client-p c client)))
                  (let ((*client* client)) (send-if-supported (list :follow (first c))))))
       :alt (lambda (c client)
              (unsubscribe-panes client)
              (when c (wire-send (client-wire client) (list :detach-client (first c)))))
       :dropped (lambda (client) (unsubscribe-panes client))))

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

(defun prompt-search (client)
  "Find in the pane with the focus: the pane is read back, and what is typed
is found as it is typed."
  (let ((*client* client)) (scroll-mode))
  (open-prompt client "find" nil :kind #\/
       :items-fn #'found-hits
       :narrow nil
       :query (or (client-find-text client) "")
       :text #'hit-line
       :foot (hints 'prompt-mode "↑↓" "hit, the pane follows" 'prompt-accept "keep it and leave"
                    'prompt-accept-alternate "copy its line")
       :live (lambda (typed client)
               (let ((*client* client))
                 (setf (client-find-text client) typed)
                 (send-if-supported (list :find nil nil typed :here))))
       :moved (lambda (hit client)
                (let ((*client* client))
                  (send-if-supported (list :find nil nil nil (list :row (first hit))))))
       :chose (lambda (it client) (declare (ignore it client)))
       :alt (lambda (hit client)
              (let ((*client* client))
                (when (consp hit)
                  (send-if-supported (list :select (list :line (first hit)))))))
       :dropped (lambda (client)
                  (let ((*client* client))
                    (send-if-supported (list :find nil nil nil :clear))))))

(defun open-palette (client kind)
  "Open the palette on KIND: :commands, :windows, :clients or :find."
  (ecase kind
    (:commands (prompt-command client))
    (:windows (let ((*client* client)) (send-to-server (list :sessions))))
    (:clients (prompt-clients client))
    (:find (prompt-search client))))
