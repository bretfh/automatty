;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defparameter +command-groups+ '(panes windows sessions agents scrolling asking)
  "What the keys act on, in the order the help lists them.")

(defun keys-help-rows ()
  "Every offered command with its key, grouped by what it acts on."
  (let ((rows (loop :for name :in (command-names)
                    :collect (list name (command-key (intern (string-upcase (substitute #\- #\Space name)) :atty))
                                   (or (command-group name) 'other)))))
    (stable-sort rows #'< :key (lambda (r) (or (position (third r) +command-groups+) (length +command-groups+))))))

(defun command-name-of (does)
  "The name DOES is registered under, or nil for a function that is no command."
  (loop :for name :being :the :hash-keys :of *commands* :using (hash-value fn)
        :when (eq fn does) :return name))

(defun mode-keys-rows (mode)
  "Every key in force in MODE that runs a command, as (name key group)."
  (stable-sort
   (loop :for (chord . does) :in (atty/mode:keys-in-force (atty/mode:mode-named mode))
         :for name := (command-name-of does)
         :when name :collect (list name chord (or (command-group name) 'other)))
   #'string< :key #'second))

(defparameter +menu-delay+ 300
  "How long a prefix has to hang, in milliseconds, before the menu is drawn.")

(defparameter +menu-column+ 30 "How wide a column of the menu is.")

(defun menu-due-p (client)
  (and (client-partial-chord client)
       (client-pending-since client)
       (>= (- (client-ms) (client-pending-since client)) +menu-delay+)))

(defun close-menu (client)
  "Put the menu away, and the half chord it was for."
  (setf (client-partial-chord client) nil
        (client-pending-since client) nil
        (client-menu client) nil
        (client-dirty client) t))

(defun handle-menu-click (client)
  "A press while the menu is up: an entry under it runs, and either way the
menu goes away with the half chord it was for."
  (let ((hit (button-at (client-menu client) (cdr *mouse-position*) (car *mouse-position*))))
    (close-menu client)
    (when hit (run-command (bar-button-runs hit) client))))

(defun group-title (group)
  (case group
    (panes "panes") (windows "windows") (sessions "sessions")
    (agents "agents") (scrolling "reading") (asking "look around")
    (t "more")))

(defun menu-groups (client)
  "What can follow the chord CLIENT has half typed, as (group (key name) ...)
in the order the groups are listed."
  (let* ((prefix (let ((atty/mode:*pending* (client-partial-chord client))) (atty/mode:pending)))
         (start (concatenate 'string prefix " "))
         (rows (loop :for (name chord group) :in (mode-keys-rows (client-mode client))
                     :when (and (> (length chord) (length start))
                                (string= start chord :end2 (length start)))
                       :collect (list group (subseq chord (length start)) name)))
         (groups (remove-duplicates (mapcar #'first rows))))
    (loop :for group :in (stable-sort groups #'<
                                      :key (lambda (g) (or (position g +command-groups+) (length +command-groups+))))
          :collect (cons group (loop :for (g key name) :in rows
                                     :when (eq g group) :collect (list key name))))))

(defun menu-entry (key name width)
  (bar-button name
              (atty/ui:row :spacing 0
                           (atty/ui:label (format nil "  ~5A " key) :face :state-blocked-strong)
                           (atty/ui:label (truncate-string (or (command-doc name) name) (max 1 (- width 10)))))))

(defun menu-tree (client cols)
  "The menu: columns of groups, each its title and the keys under it, in a
rounded box that says what it is for and how to put it away."
  (let* ((width +menu-column+)
         (across (max 1 (min 4 (floor (- cols 4) width))))
         (columns (make-array across :initial-element nil))
         (heights (make-array across :initial-element 0)))
    ;; each group goes into the column with the least in it so far
    (dolist (group (menu-groups client))
      (let ((at (position (reduce #'min heights) heights)))
        (setf (aref columns at)
              (append (aref columns at)
                      (list (atty/ui:label (format nil "  ~:@(~A~)" (group-title (first group))) :face :quiet))
                      (loop :for (key name) :in (rest group) :collect (menu-entry key name width))
                      (list (atty/ui:label ""))))
        (incf (aref heights at) (+ 2 (length (rest group))))))
    (atty/ui:framed
     (apply #'atty/ui:row :spacing 0 :align :stretch
            (loop :for column :across columns
                  :collect (apply #'atty/ui:column :align :stretch :min-width width
                                  (cons (atty/ui:label "") column))))
     :line :rounded :face :card-cursor
     :titles (list :tl (atty/ui:row :spacing 0
                                    (atty/ui:label (format nil " ~A " (prefix-string)) :face :key)
                                    (atty/ui:label " then one key" :face :strong))
                   :tr (and (client-session client)
                            (atty/ui:label (format nil " ~A " (client-session client)) :face :quiet))
                   :br (atty/ui:row :spacing 0
                                    (atty/ui:label "Esc" :face :key-hint)
                                    (atty/ui:label " cancels · no timeout " :face :quiet))))))

(defun draw-menu (client screen)
  "Draw the menu over the foot of SCREEN and keep the tree for clicks."
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows))
         (tree (menu-tree client cols))
         (high (nth-value 1 (atty/ui:with-pass
                              (atty/ui:restyle tree)
                              (atty/ui:measure tree m cols rows))))
         (top (max 0 (- rows (min high rows)))))
    (atty/cells:fill-rect m 0 top cols (- rows top) (term:make-face :bg (bar-face :bg)))
    (atty/cells:draw tree (tty:screen-grid screen) cols (+ top (min high rows)) :top top)
    (setf (client-menu client) tree
          (tty:screen-cursor-visible screen) nil)))

(defcommand (show-menu :group asking)
  "the menu of every key that follows the prefix, as though it had been pressed"
  (client-chord *client* (event-key +prefix+))
  (when (client-partial-chord *client*)
    (setf (client-pending-since *client*) (- (client-ms) +menu-delay+)
          (client-dirty *client*) t)))

(defcommand (describe-mode :unlisted)
  "the keys of whatever is on top, or of the session when nothing is"
  (let ((mode (client-mode *client*)))
    (if (eq mode 'pane-mode)
        (describe-bindings)
        (open-prompt *client* (format nil "keys · ~(~A~)" mode) (mode-keys-rows mode)
             :text (lambda (r) (format nil "~12A ~24A ~@[~A~]" (second r) (first r) (command-doc (first r))))
             :foot (hints 'prompt-mode 'prompt-accept "run" 'prompt-cancel "close")
             :chose (lambda (r client) (run-command (first r) client))))))

(defcommand (describe-bindings :group asking)
  "every command, its key and what it acts on; RET runs one"
  (open-prompt *client* "keys" (keys-help-rows)
       :text (lambda (r) (format nil "~(~9A~) ~24A ~10A ~@[~A~]"
                                 (third r) (first r) (or (second r) "") (command-doc (first r))))
       :foot (hints 'prompt-mode 'prompt-accept "run" 'prompt-cancel "close")
       :chose (lambda (r client) (run-command (first r) client))))
