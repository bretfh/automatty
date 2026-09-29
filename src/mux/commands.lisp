;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What the keys do, and which keys do it.

(defun here () (watcher-session *client*))

(defun here-server () (or (watcher-server *client*) *server*))

(defcommand (detach :group sessions) ()
  "leave the session running and give the terminal back"
  (drop-watcher (here-server) *client* :detached))

(defcommand (redraw :group asking) ()
  "draw the whole screen again"
  (let ((session (here)))
    (when session
      (setf (watcher-shadow *client*) (tty:make-screen :width (session-cols session)
                                                        :height (session-rows session))
            (watcher-told *client*) nil
            (watcher-behind *client*) t)
      (send-hello *client* session))))

(defun set-bar (session state)
  "The bar is the session's: whoever asks, everybody on it sees the change."
  (setf (session-bar-p session) (if (eq state :toggle)
                                    (not (session-bar-p session))
                                    (and state t)))
  (session-reset-shadows session))

(defcommand (bar-off :group asking) ()
  "take the bar off, for everybody on the session"
  (set-bar (here) nil))

(defcommand (bar-on :group asking) ()
  "put the bar back"
  (set-bar (here) t))

(defcommand (toggle-bar :group asking) ()
  "the bar off or on"
  (set-bar (here) :toggle))

(defcommand (commands :group asking) ()
  "run any command by name; the palette's first tab"
  (open-palette *client* :commands))

(defcommand (split-right :group panes) ()
  "another pane beside this one"
  (session-split (here) :across))

(defcommand (split-below :group panes) ()
  "another pane under this one"
  (session-split (here) :down))

(defcommand (next-pane :group panes) ()
  "the focus to the next pane in this window"
  (session-focus-next (here)))

(defcommand (close-pane :group panes) ()
  "close this pane and let its program go"
  (session-close-pane (here) (session-focus (here))))

(defcommand (delete-other-panes :group panes) ()
  "close every other pane in this window"
  (session-delete-other-panes (here) (session-focus (here))))

(defun open-session (server watcher &optional name command directory)
  "A new session, joined by WATCHER, started where its pane is."
  (let ((session (watcher-session watcher)))
    (join-session server watcher
                  (add-session server (or command (server-command server))
                               :name (or name (unused-session-name server))
                               :rows (watcher-rows watcher)
                               :cols (watcher-cols watcher)
                               :directory (or directory
                                              (and session
                                                   (pane-directory (session-focus session))))))))

(defcommand (new-session :group sessions) ()
  "another session, started where this pane is"
  (open-session (here-server) *client*))

(defcommand (new-window :group windows) ()
  "another window in this session, after this one"
  (session-add-window (here)))

(defcommand (next-window :group windows) ()
  "show the next window"
  (session-cycle-window (here) 1))

(defcommand (previous-window :group windows) ()
  "show the window before"
  (session-cycle-window (here) -1))

(defcommand (close-window :group windows) ()
  "close this window and every program in it, after a yes"
  (confirm *client* "close this window and every program in it?"
           :yes (lambda (w)
                  (let ((session (watcher-session w)))
                    (session-close-window session (session-window session))))))

(defun session-list (server)
  "Every session as the window chooser lists them: (name rows cols panes
watching blocked windows)."
  (mapcar (lambda (s)
            (list (session-name s) (session-rows s) (session-cols s)
                  (length (session-panes s))
                  (count-if #'watcher-interactive (session-watchers s))
                  (count :blocked (session-panes s)
                         :key (lambda (p) (agent:agent-state (pane-agent p))))
                  (encode-windows s)))
          (server-sessions server)))

(defcommand (switch-session :group sessions) ()
  "every session and its windows, to go to one; the same list as @"
  (prompt-window *client* (session-list (here-server))))

(defcommand (clients :group sessions) ()
  "who is attached to this server, and what each is looking at; RET goes there, C-RET detaches one"
  (open-palette *client* :clients))

(defcommand (switch-window :group windows) ()
  "every session › window, the ones asking first; RET goes, C-RET makes one, TAB its panes"
  (prompt-window *client* (session-list (here-server))))

(defcommand (rename-window :group windows) ()
  "what to call this window; TAB names the pane instead"
  (let ((session (here)))
    (prompt-window-name *client* (session-name session)
                        (window-number session (session-window session))
                        (window-label (session-window session)))))

(defcommand (send-prefix :group asking) ()
  "send the prefix itself to the pane"
  (type-into-pane *client* (string +prefix+)))

(defcommand (rename-pane :group panes) ()
  "what to call this pane; TAB names the window instead"
  (let* ((session (here))
         (pane (session-focus session)))
    (prompt-pane-name *client* (session-name session) (pane-id pane)
                      (pane-label pane) (pane-named pane)
                      :address (pane-address-of session pane))))

(declaim (ftype (function (watcher string) t) prompt-session-name))
(defun prompt-session-name (watcher session)
  "What to call SESSION, on the line at the foot, starting from its name."
  (entry watcher "name" (format nil "session ~A" session) session
         :keep (lambda (typed w)
                 (let* ((server (watcher-server w))
                        (it (session-named server session)))
                   (if it
                       (session-rename server w it typed)
                       (show-note w "name" (format nil "no session is called ~A any more" session)))))))

(defcommand (rename-session :group sessions) (&optional name new)
  "what to call this session; given a session and a name, that is what it is called"
  (if new
      (let* ((server (here-server))
             (it (session-named server name)))
        (unless it (error "nothing is called ~A" name))
        (when (eq t (session-rename server *client* it new))
          (format t "~&~A is now ~A~%" name (string-trim " " new))))
      (when (watcher-session-name *client*)
        (prompt-session-name *client* (watcher-session-name *client*)))))

(defcommand (goto-blocked-pane :group agents) ()
  "go to whatever has been asking longest, in any session"
  (focus-oldest-blocked (here-server) *client*))

(defcommand (zoom-pane :group panes) ()
  "this pane has the whole window, or gives it back"
  (session-zoom-pane (here-server) *client*))

(defun mouse-modifiers ()
  "The modifiers held with what the mouse just did."
  (loop :for mod :in '(:shift :meta :ctrl)
        :when (getf *mouse-event* mod) :collect mod))

(defun pointer (what)
  (let ((button (or (getf *mouse-event* :button) :left)))
    (unless (and (eq what :press) (eq button :left)
                 (some (lambda (over) (overlay-clicked over (cdr *mouse-position*) (car *mouse-position*) *client*))
                       (watcher-overlays *client*)))
      (handle-pointer (here) *client* what button (car *mouse-position*) (cdr *mouse-position*)
                      (mouse-modifiers)))))

(defcommand (mouse-pressed :unlisted) () (pointer :press))
(defcommand (mouse-dragged :unlisted) () (pointer :drag))
(defcommand (mouse-released :unlisted) () (pointer :release))

(defun scroll-by (amount)
  (let* ((session (here))
         (x (car *mouse-position*))
         (y (cdr *mouse-position*)))
    (scroll-pane (or (and x y (pane-at session x y)) (session-focus session)) amount)))

(defun wheel (way)
  (handle-wheel (here) way (car *mouse-position*) (cdr *mouse-position*) (mouse-modifiers)))

(defcommand natural-scroll-up () (wheel :up))
(defcommand natural-scroll-down () (wheel :down))
(defcommand natural-scroll-left () (wheel :left))
(defcommand natural-scroll-right () (wheel :right))

(defcommand scroll-up () (scroll-by 1))
(defcommand scroll-down () (scroll-by -1))
(defcommand scroll-up-line () (scroll-by 3))
(defcommand scroll-down-line () (scroll-by -3))
(defcommand scroll-page-up () (scroll-by :page-up))
(defcommand scroll-page-down () (scroll-by :page-down))
(defcommand scroll-half-page-up () (scroll-by :half-up))
(defcommand scroll-half-page-down () (scroll-by :half-down))
(defcommand scroll-to-top () (scroll-by :top))
(defcommand scroll-to-bottom () (scroll-by :bottom))

(defcommand (toggle-scrollbars :group scrolling) ()
  "the scrollbar column off or on, for the programs"
  (let ((session (here)))
    (setf (session-scrollbars-p session) (not (session-scrollbars-p session)))
    (dolist (w (session-watchers session)) (setf (watcher-behind w) t))))

(defcommand (reload-init :group asking) ()
  "have the server read the init file again"
  (load-user-init)
  (destructuring-bind (text face) (init-load-note)
    (show-note *client* "atty" text :face face))
  (dolist (w (all-watchers (here-server))) (setf (watcher-behind w) t)))
