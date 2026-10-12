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

(defun made-options (args)
  (if (keywordp (first args))
      (destructuring-bind (&key command directory name) args
        (values command (declared-directory *caller-directory* directory) name))
      (let* ((dashes (member "--" args :test #'string=))
             (before (ldiff args dashes))
             (words (or (rest dashes) (positional-args before "--name" "--directory" "--cwd"))))
        (values (and words (format nil "~{~A~^ ~}" words))
                (declared-directory *caller-directory*
                                    (or (option-value before "--directory") (option-value before "--cwd")))
                (option-value before "--name")))))

(defun made-pane (session pane)
  (when (and *command-line* pane)
    (format t "~&~A~%" (pane-address-of session pane)))
  pane)

(defcommand (split-right :group panes) (&rest options)
  "another pane beside this one: [--name <name>] [--directory <dir>] [<command>]"
  (multiple-value-bind (command directory name) (made-options options)
    (made-pane (here) (watcher-split *client* :across command directory name))))

(defcommand (split-below :group panes) (&rest options)
  "another pane under this one: [--name <name>] [--directory <dir>] [<command>]"
  (multiple-value-bind (command directory name) (made-options options)
    (made-pane (here) (watcher-split *client* :down command directory name))))

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
  (let* ((focus (watcher-focus watcher))
         (session (add-session server (or command (server-command server))
                               :name (or name (unused-session-name server))
                               :rows (watcher-rows watcher)
                               :cols (watcher-cols watcher)
                               :directory (or directory (and focus (pane-directory focus))))))
    (join-session server watcher session)
    session))

(defcommand (new-session :group sessions) (&rest options)
  "another session, started where this pane is: [--name <name>] [--directory <dir>] [<command>]"
  (multiple-value-bind (command directory name) (made-options options)
    (let ((session (open-session (here-server) *client* name command directory)))
      (made-pane session (first (session-panes session)))
      session)))

(defcommand (new-window :group windows) (&rest options)
  "another window in this session, after this one: [--name <name>] [--directory <dir>] [<command>]"
  (multiple-value-bind (command directory name) (made-options options)
    (made-pane (here) (first (window-panes (watcher-add-window *client* command directory name))))))

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

(defcommand (rename-pane :group panes) (&optional address name)
  "what to call a pane: this one, asked for; <pane> <name>; or from inside a pane <name>"
  (cond ((and address name)
         (multiple-value-bind (pane session) (address-pane address)
           (name-pane (here-server) (session-name session) (pane-id pane) name)
           (format t "~&~A is ~A~%" (pane-address-of session pane)
                   (if (plusp (length (string-trim " " name))) (string-trim " " name) "unnamed"))))
        (address
         (rename-pane (or *caller* (refuse 1 "which pane? atty rename-pane <pane> <name>")) address))
        (t
         (let* ((session (here))
                (pane (watcher-focus *client*)))
           (prompt-pane-name *client* (session-name session) (pane-id pane)
                             (pane-name pane) (pane-title pane)
                             :address (pane-address-of session pane))))))

(defcommand (send-keys :group panes) (address &rest keys)
  "keys into a pane: RET, TAB, ESC, SPC, Up, Down, Left, Right and C-x by name, anything else as it is"
  (let ((pane (address-pane address))
        (text (format nil "~{~A~}" (mapcar (lambda (k) (if (find #\Space k) k (agent:press-keys k))) keys))))
    (pane-push-log pane (now-ms) (actor-of *client* *caller*) :keys (length text))
    (pane-write pane text)))

(defcommand (read-pane :group panes) (address &rest args)
  "what is on a pane's screen: <pane> [<lines>] [--plain]"
  (let* ((pane (address-pane address))
         (words (positional-args args))
         (n (or (and (first words) (parse-integer (first words) :junk-allowed t)) 24)))
    (dolist (line (if (flag-p args "--plain")
                      (with-term (term pane) (plain-lines term n))
                      (with-term (term pane) (agent:last-lines term n))))
      (format t "~&~A~%" line))))

(defcommand (read-log :group panes) (address &optional n)
  "who typed into a pane, newest first"
  (let* ((pane (address-pane address))
         (n (or (and n (parse-integer (princ-to-string n) :junk-allowed t)) 20))
         (now (now-ms))
         (entries (mapcar (lambda (e) (encode-log-entry e now))
                          (subseq (pane-log pane) 0 (min n (length (pane-log pane)))))))
    (if entries
        (loop :for (nil actor verb summary outcome clock) :in entries
              :do (format t "~&~A  ~12A ~(~7A~) ~A~:[~;  refused~]~%"
                          (format-clock-hms clock) (caller-actor actor) verb
                          (if (eq verb :keys) (format nil "~D bytes" summary) (or summary ""))
                          (eq outcome :refused)))
        (format t "~&nobody has typed into ~A~%" address))))

(declaim (ftype (function (watcher string) t) prompt-session-name))
(defun prompt-session-name (watcher session)
  "What to call SESSION, on the line at the foot, starting from its name."
  (entry watcher "name" (format nil "session ~A" session) session
         :keep (lambda (typed w)
                 (let* ((server (watcher-server w))
                        (it (session-named server session)))
                   (if it
                       (let ((outcome (session-rename server w it typed)))
                         (unless (eq outcome t)
                           (show-note w "name" (rename-refused outcome typed))))
                       (show-note w "name" (format nil "no session is called ~A any more" session)))))))

(defun rename-refused (outcome new)
  (if (eq outcome :empty)
      "a session needs a name"
      (format nil "there is already a session called ~A" (string-trim " " new))))

(defcommand (rename-session :group sessions) (&optional name new)
  "what to call this session; given a session and a name, that is what it is called"
  (if new
      (let* ((server (here-server))
             (it (session-named server name)))
        (unless it (error "nothing is called ~A" name))
        (let ((outcome (session-rename server *client* it new)))
          (unless (eq outcome t) (refuse 1 (rename-refused outcome new)))
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
  (let ((server (here-server)))
    (on-server server (lambda () (load-user-init) (make-declared server))))
  (destructuring-bind (text face) (init-load-note)
    (show-note *client* "atty" text :face face))
  (dolist (w (all-watchers (here-server))) (draw-again w)))
