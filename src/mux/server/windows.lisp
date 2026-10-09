;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun session-add-window (session pane &optional after)
  "PANE in a window of its own in SESSION, after the window AFTER, or last.
Nobody is shown it: whoever asked for it shows it themselves."
  (on-server (session-server session)
             (lambda ()
               (let ((window (%make-window :layout pane)))
                 (change-session session
                                 (lambda (s)
                                   (let* ((windows (state-windows s))
                                          (at (if after
                                                  (or (position (window-id after) windows :key #'window-id)
                                                      (1- (length windows)))
                                                  (1- (length windows)))))
                                     (setf (state-windows s)
                                           (append (subseq windows 0 (1+ at))
                                                   (list window)
                                                   (subseq windows (1+ at)))))))
                 (session-start-pane session pane)
                 (run-hook 'pane-started session pane)
                 window))))

(defun session-nth-window (session n)
  "Window N of SESSION, counting from 1."
  (and (integerp n) (nth (1- n) (session-windows session))))

(defun session-remove-window (session window)
  "WINDOW is gone from SESSION. The last window stays, empty, which is a session
that is over."
  (on-server (session-server session)
             (lambda ()
               (let ((left (remove (window-id window) (session-windows session) :key #'window-id)))
                 (when left
                   (setf (session-windows session) left))
                 left))))

(defun session-close-window (session window)
  "Let every program in WINDOW go, and take the window out."
  (dolist (pane (window-panes window))
    (session-close-pane session pane)))

(defun session-rename (server watcher session new)
  "Call SESSION NEW, when NEW is a name and no other session has it: whatever
anybody has on top follows it, and WATCHER is told whether it was done.
Answers t, or :empty or :taken."
  (let* ((old (session-name session))
         (new (string-trim " " (or new "")))
         (outcome (on-server server
                             (lambda ()
                               (let ((taken (and (plusp (length new)) (session-named server new))))
                                 (cond ((zerop (length new)) :empty)
                                       ((and taken (not (eq taken session))) :taken)
                                       (t (setf (session-name session) new) t)))))))
    (when (eq outcome t)
      (dolist (w (all-watchers server))
        (on-watcher w (let ((w w))
                        (lambda ()
                          (dolist (it (watcher-overlays w)) (overlay-session-renamed it old new))
                          (draw-again w))))))
    (send-message watcher (list :session-named old new outcome))
    outcome))

(defun session-rename-window (session window label)
  (on-server (session-server session)
             (lambda ()
               (change-window session window
                              (lambda (w) (setf (window-label w) (and label (plusp (length label)) label)))))))

(defun window-display-name (session window)
  "What to call WINDOW: its name, else its number."
  (or (window-label window) (princ-to-string (window-number session window))))

(defun encode-layout (it)
  "A layout as it goes out: a pane's id, or a split as its way and its parts."
  (cond ((null it) nil)
        ((split-p it) (cons (split-way it) (mapcar #'encode-layout (split-parts it))))
        (t (pane-id it))))

(defun encode-windows (session &optional watcher)
  "SESSION's windows as a client is told them: number, name, how many panes, how
many of them are asking, and whether it is the one WATCHER shows."
  (let ((shown (and watcher (eq session (watcher-session watcher)) (watcher-window watcher))))
    (loop :for w :in (session-windows session)
          :for n :from 1
          :collect (list n (window-label w) (length (window-panes w))
                         (count :blocked (window-panes w)
                                :key (lambda (p) (agent:agent-state (pane-agent p))))
                         (same-window-p w shown)))))

(defun session-split (session pane way new)
  "NEW beside PANE, the way given, in PANE's window, and started. Answers NEW,
or nil when PANE is in no window any more."
  (on-server (session-server session)
             (lambda ()
               (let ((window (window-of session pane)))
                 (when window
                   (change-window session window
                                  (lambda (w) (setf (window-layout w) (layout-insert (window-layout w) pane way new))))
                   (session-start-pane session new)
                   (run-hook 'pane-started session new)
                   new)))))

(defun find-pane (server name id)
  (let ((session (session-named server name)))
    (and session (find id (session-panes session) :key #'pane-id))))

(defun session-close-pane (session pane)
  "Take PANE out of its window and let its program go. A window left with
nothing in it goes too, unless it is the last: an empty last window is a
session that is over."
  (on-server (session-server session)
             (lambda ()
               (let ((window (window-of session pane)))
                 (when window
                   (change-window session window
                                  (lambda (w) (setf (window-layout w) (layout-remove (window-layout w) pane))))
                   (dolist (w (session-watchers session))
                     (on-watcher w (let ((w w))
                                     (lambda ()
                                       (when (eq pane (second (watcher-held w)))
                                         (setf (watcher-held w) nil))))))
                   (pane-close pane)
                   (run-hook 'pane-ended session pane)
                   (let ((now (window-now session window)))
                     (when (and now (null (window-panes now)) (rest (session-windows session)))
                       (session-remove-window session now))))
                 (session-panes session)))))

(defun session-delete-other-panes (session pane)
  "Every other pane in PANE's window goes, and PANE has the whole of it."
  (let ((window (window-of session pane)))
    (when window
      (dolist (other (remove pane (window-panes window)))
        (session-close-pane session other))
      (window-panes (window-now session window)))))

(defun new-pane-beside (watcher command directory)
  (let ((focus (watcher-focus watcher))
        (session (watcher-session watcher)))
    (make-pane (or command (and focus (pane-command focus)) (server-command (session-server session)))
               :directory (or directory (and focus (pane-directory focus))))))

(defun add-window-in (session)
  (let* ((like (first (session-panes session)))
         (pane (make-pane (if like (pane-command like) (server-command (session-server session)))
                          :rows (if like (pane-height like) 24) :cols (if like (pane-width like) 80)
                          :directory (and like (pane-directory like)))))
    (session-add-window session pane)))

(defun watcher-add-window (watcher &optional command directory)
  (let* ((session (watcher-session watcher))
         (pane (new-pane-beside watcher command directory)))
    (multiple-value-bind (rows cols) (laid-size session (watcher-view watcher) pane)
      (pane-resize pane rows cols))
    (let ((window (session-add-window session pane (watcher-window watcher))))
      (setf (watcher-window watcher) window))))

(defun watcher-split (watcher way)
  (let* ((session (watcher-session watcher))
         (focus (watcher-focus watcher))
         (new (new-pane-beside watcher nil nil)))
    (multiple-value-bind (rows cols)
        (laid-size session (watcher-view watcher) new
                   (layout-insert (watcher-layout watcher) focus way new))
      (pane-resize new rows cols))
    (when (session-split session focus way new)
      (setf (watcher-focus watcher) new))))

(defun watcher-select-window (watcher n)
  (let ((window (session-nth-window (watcher-session watcher) n)))
    (when window (setf (watcher-window watcher) window))
    window))

(defun watcher-cycle-window (watcher by)
  "The window BY after the one WATCHER shows, round the end."
  (let* ((windows (session-windows (watcher-session watcher)))
         (at (position (window-id (watcher-window watcher)) windows :key #'window-id)))
    (when (rest windows)
      (setf (watcher-window watcher) (nth (mod (+ at by) (length windows)) windows)))
    (watcher-window watcher)))

(defun watcher-focus-next (watcher)
  (let* ((panes (window-panes (watcher-window watcher)))
         (at (position (watcher-focus watcher) panes)))
    (when panes
      (setf (watcher-focus watcher) (nth (mod (1+ (or at -1)) (length panes)) panes)))
    (watcher-focus watcher)))
