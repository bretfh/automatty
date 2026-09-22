;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What the keys do, and which keys do it.

(defcommand (detach :group sessions)
  "leave the session running and give the terminal back"
  (done-with *client* :detached))

(defcommand (redraw :group asking)
  "draw the whole screen again"
  (client-redraw *client*))

(defun tell-the-server (form)
  "Say FORM to the server holding this session, or say why not.

The server is usually older than the client that reached it: it has been running
since whenever, and this was started a moment ago. A key it has never heard of
that quietly does nothing looks exactly like a key that is broken, so it says so
instead."
  (if (member (first form) (client-knows *client*))
      (wire-send (client-wire *client*) form)
      (show-note *client* "not here"
                 (format nil "The server holding this session cannot ~(~A~).~%~%~
                              It has been running since before that was added.~%~
                              What is in it goes on running; a session started~%~
                              now knows the whole of it."
                         (first form))
                 :face :warning)))

(defcommand (bar-off :group asking)
  "take the bar off, for everybody on the session"
  (tell-the-server (list :bar nil)))

(defcommand (bar-on :group asking)
  "put the bar back"
  (tell-the-server (list :bar t)))

(defcommand (toggle-the-bar :group asking)
  "the bar off or on"
  (tell-the-server (list :bar :toggle)))

(defcommand (run-a-command :group asking)
  "run any command by name; the palette's first tab"
  (open-the-palette *client* :commands))

(defcommand (split-right :group panes)
  "another pane beside this one"
  (tell-the-server (list :split :across)))

(defcommand (split-below :group panes)
  "another pane under this one"
  (tell-the-server (list :split :down)))

(defcommand (next-pane :group panes)
  "the focus to the next pane in this window"
  (tell-the-server (list :focus)))

(defcommand (close-pane :group panes)
  "close this pane and let its program go"
  (tell-the-server (list :close)))

(defcommand (only-this-pane :group panes)
  "close every other pane in this window"
  (tell-the-server (list :only)))

(defcommand (new-session :group sessions)
  "another session, started where this pane is"
  (tell-the-server (list :new)))

;;; Windows. Each names the session it is on, since the server takes the same
;;; forms from a client on another session and from the command line.

(defcommand (new-window :group windows)
  "another window in this session, after this one"
  (tell-the-server (list :new-window (client-session *client*))))

(defcommand (next-window :group windows)
  "show the next window"
  (tell-the-server (list :next-window (client-session *client*))))

(defcommand (previous-window :group windows)
  "show the window before"
  (tell-the-server (list :previous-window (client-session *client*))))

(defcommand (close-window :group windows)
  "close this window and every program in it, after a yes"
  (confirm *client* "close this window and every program in it?"
           :yes (lambda (c)
                  (let ((*client* c))
                    (tell-the-server (list :close-window (client-session c)))))))

(defcommand (choose-a-session :group sessions)
  "every session and its windows, to go to one; the same list as @"
  (tell-the-server (list :sessions)))

(defcommand (clients :group sessions)
  "who is attached to this server, and what each is looking at; RET goes there, C-RET detaches one"
  (open-the-palette *client* :clients))

(defcommand (choose-a-window :group windows)
  "every session › window, the ones asking first; RET goes, C-RET makes one, TAB its panes"
  (tell-the-server (list :sessions)))

(defcommand (name-this-window :group windows)
  "what to call this window; TAB names the pane instead"
  (tell-the-server (list :naming-window)))

(defcommand (send-the-prefix :group asking)
  "send the prefix itself to the pane"
  (tell-the-server (list :keys (string +prefix+))))

(defcommand (name-this-pane :group panes)
  "what to call this pane; TAB names the window instead"
  (tell-the-server (list :naming)))

(defun ask-a-session-name (client session)
  "What to call SESSION, on the line at the foot, starting from its name."
  (entry client "name" (format nil "session ~A" session) session
         :keep (lambda (typed c)
                 (let ((*client* c)) (tell-the-server (list :name-session session typed))))))

(defcommand (name-this-session :group sessions)
  "what to call this session"
  (when (client-session *client*)
    (ask-a-session-name *client* (client-session *client*))))

(defcommand (go-to-the-blocked :group agents)
  "go to whatever has been asking longest, in any session"
  (tell-the-server (list :go-to-blocked)))

(defcommand (zoom-this-pane :group panes)
  "this pane has the whole window, or gives it back"
  (tell-the-server (list :zoom)))

(defun server-knows-p (what)
  (member what (client-knows *client*)))

(defun tell-the-server-if-it-knows (form)
  "Say FORM to the server if it has heard of it, and nothing at all if not. For
what a mouse does: a wheel that does nothing on a server from before it did
anything is what that server always did, and a note for every notch is worse."
  (when (server-knows-p (first form))
    (wire-send (client-wire *client*) form)))

(defcommand (mouse-clicked :unlisted)
  (tell-the-server (list :mouse-at (car *mouse-at*) (cdr *mouse-at*))))

;;; The mouse, passed on. Which button and where is the server's to make sense
;;; of: it knows what was drawn there, and whether the program under it asked
;;; to be told.

(defun mouse-held ()
  "The modifiers held with what the mouse just did."
  (loop :for mod :in '(:shift :meta :ctrl)
        :when (getf *mouse-event* mod) :collect mod))

(defun pointer-did (what)
  (let ((button (or (getf *mouse-event* :button) :left)))
    (cond ((and (eq what :press) (eq button :left)
                (some (lambda (over) (clicked-over over (cdr *mouse-at*) (car *mouse-at*) *client*))
                      (client-over *client*)))
           ;; something drawn over the session took the click: the drawer's answers, say
           nil)
          ((server-knows-p :pointer)
           (wire-send (client-wire *client*)
                      (list :pointer what button (car *mouse-at*) (cdr *mouse-at*)
                            (mouse-held))))
          ;; a server from before buttons were passed on still knows a click
          ((and (eq what :press) (eq button :left)) (mouse-clicked)))))

(defcommand (mouse-pressed :unlisted) (pointer-did :press))
(defcommand (mouse-dragged :unlisted) (pointer-did :drag))
(defcommand (mouse-released :unlisted) (pointer-did :release))

;;; Reading a pane back. Every one of these is about the pane under the pointer
;;; when a mouse asked for it and the pane with the focus when a key did.

(defun scroll-by (amount)
  (tell-the-server-if-it-knows
   (list :scroll amount (car *mouse-at*) (cdr *mouse-at*))))

(defun wheel-went (way)
  (tell-the-server-if-it-knows
   (list :wheel way (car *mouse-at*) (cdr *mouse-at*) (mouse-held))))

(defcommand natural-scroll-up (wheel-went :up))
(defcommand natural-scroll-down (wheel-went :down))
(defcommand natural-scroll-left (wheel-went :left))
(defcommand natural-scroll-right (wheel-went :right))

(defcommand scroll-up (scroll-by 1))
(defcommand scroll-down (scroll-by -1))
(defcommand scroll-up-a-little (scroll-by 3))
(defcommand scroll-down-a-little (scroll-by -3))
(defcommand scroll-page-up (scroll-by :page-up))
(defcommand scroll-page-down (scroll-by :page-down))
(defcommand scroll-half-page-up (scroll-by :half-up))
(defcommand scroll-half-page-down (scroll-by :half-down))
(defcommand scroll-to-top (scroll-by :top))
(defcommand scroll-to-bottom (scroll-by :bottom))

(defcommand (toggle-scrollbars :group reading)
  "the scrollbar column off or on, for the programs"
  (tell-the-server (list :scrollbars :toggle)))

;;; Scroll mode: the keys that read back, with no prefix in front of them, for
;;; as long as it is on. Nothing typed reaches the pane until it is left, and
;;; leaving it is back to live.

(defstruct (reading (:constructor %make-reading)))

(atty/mode:define-mode scroll-mode (pane-mode))

(defmethod mode-of ((r reading)) 'scroll-mode)

(defmethod draw-over ((r reading) screen)
  "Only what says it is on and how to get out: what is being read is the pane."
  (let ((cols (tty:screen-width screen))
        (rows (tty:screen-height screen))
        (leave (car (find #'leave-scroll-mode
                          (atty/mode:keys-in-force (atty/mode:mode-named 'scroll-mode))
                          :key #'cdr))))
    (atty/cells:draw (atty/ui:label (format nil " reading back~@[ · ~A leaves~] " leave)
                                    :face :chip-scrolled)
                     (tty:screen-grid screen) cols rows :left 0 :top (max 0 (1- rows)))
    (setf (tty:screen-cursor-visible screen) nil)))

(defun reading-now ()
  (find-if (lambda (it) (typep it 'reading)) (client-over *client*)))

(defcommand (scroll-mode :group reading)
  "read this pane back from the keys; q leaves"
  (unless (reading-now)
    (client-over-put *client* (%make-reading))
    (tell-the-server-if-it-knows (list :reading t))))

(defcommand (leave-scroll-mode :unlisted)
  (let ((r (reading-now)))
    (when r (client-over-drop *client* r)))
  (tell-the-server-if-it-knows (list :reading nil))
  (tell-the-server-if-it-knows (list :find nil nil nil :clear))
  (scroll-to-bottom))

(defcommand (find-in-pane :group reading)
  "find in this pane's history, the hits listed as you type; n and N move between them after"
  (open-the-palette *client* :find))

(defcommand (find-next :group reading)
  "the next older hit of the last find"
  (tell-the-server-if-it-knows (list :find nil nil nil :next)))

(defcommand (find-back :group reading)
  "the next newer hit of the last find"
  (tell-the-server-if-it-knows (list :find nil nil nil :back)))

(defcommand (select-from-here :group reading)
  "mark the top line shown; y copies from it to wherever you scroll"
  (tell-the-server-if-it-knows (list :select :start)))

(defcommand (copy-lines :group reading)
  "copy the lines marked, or the screen, to the clipboard"
  (tell-the-server-if-it-knows (list :select :copy)))

(defcommand (scroll-mode-page-up :group reading)
  "the same, a page back to begin with"
  (scroll-mode)
  (scroll-page-up))

(defparameter +groups+ '(panes windows sessions agents reading asking)
  "What the keys act on, in the order the help lists them.")

(defun keys-help-rows ()
  "Every offered command with its key, grouped by what it acts on."
  (let ((rows (loop :for name :in (command-names)
                    :collect (list name (key-for (intern (string-upcase (substitute #\- #\Space name)) :atty))
                                   (or (command-group name) 'other)))))
    (stable-sort rows #'< :key (lambda (r) (or (position (third r) +groups+) (length +groups+))))))

(defun command-named-by (does)
  "The name DOES is registered under, or nil for a function that is no command."
  (loop :for name :being :the :hash-keys :of *commands* :using (hash-value fn)
        :when (eq fn does) :return name))

(defun mode-keys-rows (mode)
  "Every key in force in MODE that runs a command, as (name key group)."
  (stable-sort
   (loop :for (chord . does) :in (atty/mode:keys-in-force (atty/mode:mode-named mode))
         :for name := (command-named-by does)
         :when name :collect (list name chord (or (command-group name) 'other)))
   #'string< :key #'second))

;;; The one key to learn. Press the prefix and wait, and a menu rises from
;;; the foot with every key that can follow it, grouped by what it acts on.
;;; It draws only after a moment, so a chord typed straight through never
;;; sees it, and it waits as long as anyone needs.

(defparameter +menu-after+ 300
  "How long a prefix has to hang, in milliseconds, before the menu is drawn.")

(defparameter +menu-column+ 30 "How wide a column of the menu is.")

(defun menu-due-p (client)
  (and (client-chord-so-far client)
       (client-pending-since client)
       (>= (- (ms-here) (client-pending-since client)) +menu-after+)))

(defun menu-closed (client)
  "Put the menu away, and the half chord it was for."
  (setf (client-chord-so-far client) nil
        (client-pending-since client) nil
        (client-menu client) nil
        (client-dirty client) t))

(defun menu-clicked (client)
  "A press while the menu is up: an entry under it runs, and either way the
menu goes away with the half chord it was for."
  (let ((hit (button-at (client-menu client) (cdr *mouse-at*) (car *mouse-at*))))
    (menu-closed client)
    (when hit (run-command (bar-button-runs hit) client))))

(defun group-title (group)
  (case group
    (panes "panes") (windows "windows") (sessions "sessions")
    (agents "agents") (reading "reading") (asking "look around")
    (t "more")))

(defun menu-groups (client)
  "What can follow the chord CLIENT has half typed, as (group (key name) ...)
in the order the groups are listed."
  (let* ((prefix (let ((atty/mode:*pending* (client-chord-so-far client))) (atty/mode:pending)))
         (start (concatenate 'string prefix " "))
         (rows (loop :for (name chord group) :in (mode-keys-rows (client-mode client))
                     :when (and (> (length chord) (length start))
                                (string= start chord :end2 (length start)))
                       :collect (list group (subseq chord (length start)) name)))
         (groups (remove-duplicates (mapcar #'first rows))))
    (loop :for group :in (stable-sort groups #'<
                                      :key (lambda (g) (or (position g +groups+) (length +groups+))))
          :collect (cons group (loop :for (g key name) :in rows
                                     :when (eq g group) :collect (list key name))))))

(defun menu-entry (key name width)
  (bar-button name
              (atty/ui:row :spacing 0
                           (atty/ui:label (format nil "  ~5A " key) :face :state-blocked-strong)
                           (atty/ui:label (shortened-to (or (command-doc name) name) (max 1 (- width 10)))))))

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
                                    (atty/ui:label (format nil " ~A " (prefix-spelled)) :face :key)
                                    (atty/ui:label " then one key" :face :strong))
                   :tr (and (client-session client)
                            (atty/ui:label (format nil " ~A " (client-session client)) :face :quiet))
                   :br (atty/ui:row :spacing 0
                                    (atty/ui:label "Esc" :face :key-hint)
                                    (atty/ui:label " cancels · no timeout " :face :quiet))))))

(defun draw-the-menu (client screen)
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

(defcommand (show-the-menu :group asking)
  "the menu of every key that follows the prefix, as though it had been pressed"
  (client-chord *client* (key-of +prefix+))
  (when (client-chord-so-far *client*)
    (setf (client-pending-since *client*) (- (ms-here) +menu-after+)
          (client-dirty *client*) t)))

(defcommand (keys-of-this-mode :unlisted)
  "the keys of whatever is on top, or of the session when nothing is"
  (let ((mode (client-mode *client*)))
    (if (eq mode 'pane-mode)
        (what-the-keys-do)
        (ask *client* (format nil "keys · ~(~A~)" mode) (mode-keys-rows mode)
             :text (lambda (r) (format nil "~12A ~24A ~@[~A~]" (second r) (first r) (command-doc (first r))))
             :foot (hints 'prompt-mode 'prompt-accept "run" 'prompt-cancel "close")
             :chose (lambda (r client) (run-command (first r) client))))))

(defcommand (what-the-keys-do :group asking)
  "every command, its key and what it acts on; RET runs one"
  (ask *client* "keys" (keys-help-rows)
       :text (lambda (r) (format nil "~(~9A~) ~24A ~10A ~@[~A~]"
                                 (third r) (first r) (or (second r) "") (command-doc (first r))))
       :foot (hints 'prompt-mode 'prompt-accept "run" 'prompt-cancel "close")
       :chose (lambda (r client) (run-command (first r) client))))

;;; A mode holds these, so another mode may be defined on top of this one and
;;; change or add to what is here without touching any of it.


;;; Panes are windows and the keys for them are the ones an editor uses for
;;; windows: 2 splits below, 3 splits beside, 0 closes this one, 1 leaves only
;;; this one, o goes to the next.



(declaim (ftype function load-user-init init-loaded-note))

(defcommand (reload-init :group asking)
  "read the init file again, here and in the server"
  ;; here and in the server both, since the file is read in both
  (load-user-init)
  (destructuring-bind (text face) (init-loaded-note)
    (show-note *client* "init" text :face face))
  (tell-the-server (list :reload-init)))

;;; Everything behind the prefix, as what follows it. They are bound from
;;; whatever the prefix is, so setting it in an init file moves every one of
;;; them; :prefix is the prefix itself, which sends one to the pane.

(defparameter +prefixed-keys+
  '(
    ("d" . detach)
    ("r" . redraw)
    (":" . run-a-command)
    ("?" . what-the-keys-do)
    (:prefix . send-the-prefix)
    ("t" . toggle-the-bar)
    ("2" . split-below)
    ("3" . split-right)
    ("0" . close-pane)
    ("1" . only-this-pane)
    ("o" . next-pane)
    ("," . name-this-window)
    ("a" . go-to-the-blocked)
    ("z" . zoom-this-pane)
    ("N" . needs-you)
    ("w" . switchboard)
    ("e" . explain-this-pane)
    ("C" . new-session)
    ("c" . new-window)
    ("n" . next-window)
    ("p" . previous-window)
    ("&" . close-window)
    ("b" . choose-a-session)
    ("'" . choose-a-window)
    ("D" . clients)
    ("." . name-this-pane)
    ("$" . name-this-session)
    ("[" . scroll-mode)
    ("/" . find-in-pane)
    ("PageUp" . scroll-mode-page-up)
    ("R" . reload-init)))

(defun prefix-spelled (&optional (prefix +prefix+))
  (atty/mode:spelled (key-of prefix)))

(defun bind-prefixed-keys (&optional (prefix +prefix+) was)
  "Bind everything in +PREFIXED-KEYS+ behind PREFIX, first unbinding it from
behind WAS when the prefix has moved."
  (flet ((chord (prefix tail)
           (format nil "~A ~A" (prefix-spelled prefix)
                   (if (eq tail :prefix) (prefix-spelled prefix) tail))))
    (when (and was (not (eql was prefix)))
      (loop :for (tail . nil) :in +prefixed-keys+
            :do (atty/mode:undefine-key 'pane-mode (chord was tail))))
    (loop :for (tail . command) :in +prefixed-keys+
          :do (atty/mode:define-key 'pane-mode (chord prefix tail) (symbol-function command)))
    prefix))

(bind-prefixed-keys)

(let ((was +prefix+))
  (after-setting '+prefix+ (lambda (prefix)
                             (bind-prefixed-keys prefix was)
                             (setf was prefix))))

;;; A click is looked up the same as any other key, unprefixed: a mouse's
;;; buttons are always the multiplexer's, the way a keyboard's letters are
;;; always the pane's until C-b says otherwise.

(dolist (button '("mouse-1" "mouse-2" "mouse-3"))
  (atty/mode:define-key 'pane-mode button #'mouse-pressed)
  (atty/mode:define-key 'pane-mode (format nil "~A-drag" button) #'mouse-dragged)
  (atty/mode:define-key 'pane-mode (format nil "~A-up" button) #'mouse-released))

;;; The wheel is whoever's is under it. With shift or meta held it reads the
;;; pane back whatever is running there, both because which of them a terminal
;;; keeps for itself depends on the terminal.

(atty/mode:define-key 'pane-mode "wheel-up" #'natural-scroll-up)
(atty/mode:define-key 'pane-mode "wheel-down" #'natural-scroll-down)
(atty/mode:define-key 'pane-mode "wheel-left" #'natural-scroll-left)
(atty/mode:define-key 'pane-mode "wheel-right" #'natural-scroll-right)
(atty/mode:define-key 'pane-mode "S-wheel-up" #'scroll-up-a-little)
(atty/mode:define-key 'pane-mode "S-wheel-down" #'scroll-down-a-little)
(atty/mode:define-key 'pane-mode "M-wheel-up" #'scroll-up-a-little)
(atty/mode:define-key 'pane-mode "M-wheel-down" #'scroll-down-a-little)


(loop :for (chord does) :in `(("Up" ,#'scroll-up) ("k" ,#'scroll-up)
                              ("Down" ,#'scroll-down) ("j" ,#'scroll-down)
                              ("PageUp" ,#'scroll-page-up) ("PageDown" ,#'scroll-page-down)
                              ("SPC" ,#'scroll-page-down)
                              ("C-u" ,#'scroll-half-page-up) ("C-d" ,#'scroll-half-page-down)
                              ("g" ,#'scroll-to-top) ("Home" ,#'scroll-to-top)
                              ("G" ,#'scroll-to-bottom) ("End" ,#'scroll-to-bottom)
                              ("q" ,#'leave-scroll-mode) ("Escape" ,#'leave-scroll-mode)
                              ("RET" ,#'leave-scroll-mode)
                              ("/" ,#'find-in-pane) ("n" ,#'find-next) ("N" ,#'find-back)
                              ("v" ,#'select-from-here) ("y" ,#'copy-lines))
      :do (atty/mode:define-key 'scroll-mode chord does))
