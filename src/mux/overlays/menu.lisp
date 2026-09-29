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

(defun close-menu (watcher)
  "Put the menu away, and the half chord it was for."
  (setf (watcher-partial-chord watcher) nil
        (watcher-pending-since watcher) nil
        (watcher-menu watcher) nil
        (watcher-menu-full watcher) nil
        (watcher-behind watcher) t))

(declaim (ftype (function (watcher) t) handle-menu-click))
(defun handle-menu-click (watcher)
  "A press while the menu is up: an entry under it runs, and either way the
menu goes away with the half chord it was for."
  (let ((hit (button-at (watcher-menu watcher) (cdr *mouse-position*) (car *mouse-position*))))
    (close-menu watcher)
    (when (and hit (stringp (bar-button-runs hit)))
      (run-command (bar-button-runs hit) watcher))))

(defun group-title (group)
  (case group
        (panes "panes") (windows "windows") (sessions "sessions")
        (agents "agents") (scrolling "reading") (asking "look around")
        (t "more")))

(defun menu-groups (watcher)
  "What can follow the chord WATCHER has half typed, as (group (key name) ...)
in the order the groups are listed."
  (let* ((prefix (let ((atty/mode:*pending* (watcher-partial-chord watcher))) (atty/mode:pending)))
         (start (concatenate 'string prefix " "))
         (rows (loop :for (name chord group) :in (mode-keys-rows (watcher-mode watcher))
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
                           (atty/ui:label (format nil " ~3A" key) :face :state-blocked-strong)
                           (atty/ui:label (truncate-string name (max 1 (- width 4)))))))

(defparameter +menu-width+ 36 "How wide the menu down from the corner is.")
(defparameter +keys-width+ 44 "How wide the sheet of keys after the prefix is.")

(defun menu-title ()
  (atty/ui:row :spacing 0
               (atty/ui:label (format nil " ~A " (prefix-string)) :face :key)
               (atty/ui:label " then one key" :face :strong)))

(defun menu-tree (watcher rows)
  "The menu: every key after the prefix, in its groups, one column down from
the corner."
  (let* ((width +menu-width+)
         (body (loop :for group :in (menu-groups watcher)
                     :append (list* (section (group-title (first group)))
                                    (loop :for (key name) :in (rest group)
                                          :collect (menu-entry key name (- width 2))))))
         (height (max 4 (min rows (+ 4 (length body))))))
    (values (sheet :menu (menu-title) (subseq body 0 (min (length body) (- height 4)))
                   :hints (hints 'pane-mode "Esc" "cancels, no timeout")
                   :width width :height height)
            width height)))

(defun keys-tree (watcher rows)
  "The keys after the prefix, two to a row, as many as there is room for; the
way to every key in the foot when they do not all fit."
  (let* ((width +keys-width+)
         (half (floor (- width 2) 2))
         (entries (loop :for group :in (menu-groups watcher)
                        :append (loop :for (key name) :in (rest group)
                                      :collect (menu-entry key name half))))
         (room (max 1 (- rows 4)))
         (pairs (loop :for (a b) :on entries :by #'cddr
                      :collect (atty/ui:row :spacing 0
                                            (fixed a half 1)
                                            (if b (fixed b half 1) (atty/ui:label "")))))
         (all (<= (length pairs) room))
         (shown (subseq pairs 0 (min room (length pairs))))
         (height (+ 4 (length shown))))
    (values (sheet :keys (menu-title) shown
                   :hints (atty/ui:row :spacing 0
                                       (if all (atty/ui:label "") (hint "λ" "every key" :runs "show menu"))
                                       (hint "Esc" "cancel"))
                   :width width :height height)
            width height)))

(declaim (ftype (function (watcher tty:screen) t) draw-menu))
(defun draw-menu (watcher screen)
  "Draw the menu down from the corner, or the keys after the prefix at the
foot of the panes, and keep the tree for clicks."
  (let* ((rows (tty:screen-height screen))
         (room (sheet-room watcher (if (watcher-menu-full watcher) :menu :keys) rows)))
    (multiple-value-bind (tree width height)
        (if (watcher-menu-full watcher) (menu-tree watcher room) (keys-tree watcher room))
      (draw-sheet tree watcher (if (watcher-menu-full watcher) :menu :keys) screen width height)
      (setf (watcher-menu watcher) tree
            (tty:screen-cursor-visible screen) nil))))

(defcommand (show-menu :group asking) ()
            "the menu of every key that follows the prefix, as though it had been pressed"
            (press-chord *client* (event-key +prefix+))
            (when (watcher-partial-chord *client*)
              (setf (watcher-pending-since *client*) (- (now-ms) +menu-delay+)
                    (watcher-menu-full *client*) t
                    (watcher-behind *client*) t)))

(defcommand (describe-mode :unlisted) ()
            "the keys of whatever is on top, or of the session when nothing is"
            (let ((mode (watcher-mode *client*)))
              (if (eq mode 'pane-mode)
                  (describe-bindings)
                (open-prompt *client* (format nil "keys · ~(~A~)" mode) (mode-keys-rows mode)
                             :text (lambda (r) (format nil "~12A ~24A ~@[~A~]" (second r) (first r) (command-doc (first r))))
                             :foot (hints 'prompt-mode 'prompt-accept "run" 'prompt-cancel "close")
                             :chose (lambda (r watcher) (run-command (first r) watcher))))))

(defcommand (describe-bindings :group asking) ()
            "every command, its key and what it acts on; RET runs one"
            (open-prompt *client* "keys" (keys-help-rows)
                         :text (lambda (r) (format nil "~(~9A~) ~24A ~10A ~@[~A~]"
                                                   (third r) (first r) (or (second r) "") (command-doc (first r))))
                         :foot (hints 'prompt-mode 'prompt-accept "run" 'prompt-cancel "close")
                         :chose (lambda (r watcher) (run-command (first r) watcher))))
