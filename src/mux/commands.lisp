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
  (when (here)
    (setf (watcher-shadow *client*) (tty:make-screen :width (watcher-cols *client*)
                                                      :height (watcher-rows *client*))
          (watcher-told *client*) nil
          (watcher-behind *client*) t)
    (send-hello *client*)))

(defun set-bar (watcher state)
  (setf (watcher-bar-p watcher) (if (eq state :toggle)
                                    (not (watcher-bar-p watcher))
                                    (and state t))))

(defcommand (bar-off :group asking) ()
  "take the bar off"
  (set-bar *client* nil))

(defcommand (bar-on :group asking) ()
  "put the bar back"
  (set-bar *client* t))

(defcommand (toggle-bar :group asking) ()
  "the bar off or on"
  (set-bar *client* :toggle))

(defun set-rail (watcher state)
  (setf (watcher-rail-p watcher) (if (eq state :toggle)
                                     (not (watcher-rail-p watcher))
                                     (and state t))))

(defcommand (rail-off :group asking) ()
  "take the rail of sessions off"
  (set-rail *client* nil))

(defcommand (rail-on :group asking) ()
  "put the rail of sessions back"
  (set-rail *client* t))

(defcommand (toggle-rail :group asking) ()
  "the rail of sessions off or on"
  (set-rail *client* :toggle))

(defcommand (commands :group asking) ()
  "run any command by name; the palette's first tab"
  (open-palette *client* :commands))

(defcommand (split-right :group panes) ()
  "another pane beside this one"
  (watcher-split *client* :across))

(defcommand (split-below :group panes) ()
  "another pane under this one"
  (watcher-split *client* :down))

(defcommand (next-pane :group panes) ()
  "the focus to the next pane in this window"
  (watcher-focus-next *client*))

(defcommand (close-pane :group panes) ()
  "close this pane and let its program go"
  (session-close-pane (here) (watcher-focus *client*)))

(defcommand (delete-other-panes :group panes) ()
  "close every other pane in this window"
  (session-delete-other-panes (here) (watcher-focus *client*)))

(defun open-session (server watcher &optional name command directory)
  "A new session, joined by WATCHER, started where its pane is."
  (let ((focus (watcher-focus watcher)))
    (join-session server watcher
                  (add-session server (or command (server-command server))
                               :name (or name (unused-session-name server))
                               :rows (watcher-rows watcher)
                               :cols (watcher-cols watcher)
                               :directory (or directory (and focus (pane-directory focus)))))))

(defcommand (new-session :group sessions) ()
  "another session, started where this pane is"
  (open-session (here-server) *client*))

(defcommand (new-window :group windows) ()
  "another window in this session, after this one"
  (watcher-add-window *client*))

(defcommand (next-window :group windows) ()
  "show the next window"
  (watcher-cycle-window *client* 1))

(defcommand (previous-window :group windows) ()
  "show the window before"
  (watcher-cycle-window *client* -1))

(defcommand (close-window :group windows) ()
  "close this window and every program in it, after a yes"
  (let ((session (here))
        (window (watcher-window *client*)))
    (confirm *client* "close this window and every program in it?"
             :yes (lambda (w)
                    (declare (ignore w))
                    (let ((now (window-now session window)))
                      (when now (session-close-window session now)))))))

(defun session-list (server &optional watcher)
  "Every session as the window chooser lists them: (name panes watching blocked
windows), the window shown being WATCHER's."
  (mapcar (lambda (s)
            (list (session-name s)
                  (length (session-panes s))
                  (count-if #'watcher-interactive (session-watchers s))
                  (count :blocked (session-panes s)
                         :key (lambda (p) (agent:agent-state (pane-agent p))))
                  (encode-windows s watcher)))
          (server-sessions server)))

(defcommand (switch-session :group sessions) ()
  "every session and its windows, to go to one; the same list as @"
  (prompt-window *client* (session-list (here-server) *client*)))

(defcommand (clients :group sessions) ()
  "who is attached to this server, and what each is looking at; RET goes there, C-RET detaches one"
  (open-palette *client* :clients))

(defcommand (switch-window :group windows) ()
  "every session › window, the ones asking first; RET goes, C-RET makes one, TAB its panes"
  (prompt-window *client* (session-list (here-server) *client*)))

(defcommand (rename-window :group windows) ()
  "what to call this window; TAB names the pane instead"
  (let ((session (here))
        (window (watcher-window *client*)))
    (prompt-window-name *client* session window)))

(defcommand (send-prefix :group asking) ()
  "send the prefix itself to the pane"
  (type-into-pane *client* (string +prefix+)))

(defcommand (rename-pane :group panes) ()
  "what to call this pane; TAB names the window instead"
  (let* ((session (here))
         (pane (watcher-focus *client*)))
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
      (handle-pointer *client* what button (car *mouse-position*) (cdr *mouse-position*)
                      (mouse-modifiers)))))

(defcommand (mouse-pressed :unlisted) () (pointer :press))
(defcommand (mouse-dragged :unlisted) () (pointer :drag))
(defcommand (mouse-released :unlisted) () (pointer :release))

(defun scroll-by (amount)
  (let ((x (car *mouse-position*))
        (y (cdr *mouse-position*)))
    (scroll-pane *client* (or (and x y (pane-at *client* x y)) (watcher-focus *client*)) amount)))

(defun wheel (way)
  (handle-wheel *client* way (car *mouse-position*) (cdr *mouse-position*) (mouse-modifiers)))

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
  (setf (watcher-scrollbars-p *client*) (not (watcher-scrollbars-p *client*))))

(defcommand (reload-init :group asking) ()
  "have the server read the init file again"
  (on-server (here-server) #'load-user-init)
  (destructuring-bind (text face) (init-load-note)
    (show-note *client* "atty" text :face face))
  (dolist (w (all-watchers (here-server))) (draw-again w)))
