;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun session-show-window (session window)
  "WINDOW is the one SESSION shows. Everybody watching is redrawn, and the
panes are fitted to the room when it is next composed."
  (unless (same-window-p window (session-window session))
    (setf (session-window session) window)
    (dolist (pane (window-panes window)) (setf (pane-dirty pane) t))))

(defun session-add-window (session &optional command directory (show t))
  "Another window in SESSION, after the current one, with one pane running
COMMAND, or what the focus runs, where the focus is. It is the one shown,
unless SHOW says not: a program spawning into a window of its own should not
take whoever is attached away from what they were looking at."
  (let* ((focus (session-focus session))
         (pane (make-pane (or command (and focus (pane-command focus))
                              (server-command (session-server session)))
                          :rows (if focus (pane-height focus) (session-rows session))
                          :cols (if focus (pane-width focus) (session-cols session))
                          :directory (or directory (and focus (pane-directory focus)))))
         (window (%make-window :layout pane :focus pane))
         (current (session-window session)))
    (change-session session
                    (lambda (s)
                      (let* ((windows (state-windows s))
                             (at (and current (position (window-id current) windows
                                                        :key #'window-id))))
                        (setf (state-windows s)
                              (append (subseq windows 0 (1+ (or at -1)))
                                      (list window)
                                      (subseq windows (1+ (or at -1))))))))
    (when show
      (session-show-window session window)
      (session-compose session))
    (pane-start pane :environment (pane-environment session pane))
    (run-hook 'pane-started session pane)
    window))

(defun session-nth-window (session n)
  "Window N of SESSION, counting from 1."
  (and (integerp n) (nth (1- n) (session-windows session))))

(defun session-select-window (session n)
  (let ((window (session-nth-window session n)))
    (when window (session-show-window session window))
    window))

(defun session-cycle-window (session by)
  "The window BY after the current one, round the end."
  (let* ((windows (session-windows session))
         (at (position (window-id (session-window session)) windows :key #'window-id)))
    (when (rest windows)
      (session-show-window session (nth (mod (+ at by) (length windows)) windows)))
    (session-window session)))

(defun session-remove-window (session window)
  "WINDOW is gone from SESSION; if it was shown, the one before it is, or the
one after. The last window stays, empty, which is a session that is over."
  (let* ((windows (session-windows session))
         (at (position (window-id window) windows :key #'window-id))
         (left (remove (window-id window) windows :key #'window-id)))
    (when (and at left)
      (let ((shown (same-window-p window (session-window session))))
        (change-session session (lambda (s)
                                  (setf (state-windows s) left)
                                  (unless shown
                                    (setf (state-window s)
                                          (find (window-id (state-window s)) left
                                                :key #'window-id)))))
        (when shown
          (session-show-window session (nth (min at (1- (length left))) left)))))
    left))

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
         (taken (and (plusp (length new)) (session-named server new))))
    (let ((outcome
            (cond
              ((zerop (length new))
               (show-note watcher "name" "a session needs a name")
               :empty)
              ((and taken (not (eq taken session)))
               (show-note watcher "name" (format nil "there is already a session called ~A" new))
               :taken)
              (t
               (setf (session-name session) new)
               (dolist (w (all-watchers server))
                 (dolist (it (watcher-overlays w)) (overlay-session-renamed it old new))
                 (setf (watcher-behind w) t))
               t))))
      (send-message watcher (list :session-named old new outcome))
      outcome)))

(defun session-rename-window (session window label)
  (change-window session window
                 (lambda (w) (setf (window-label w) (and label (plusp (length label)) label)))))

(defun window-display-name (session window)
  "What to call WINDOW: its name, else its number."
  (or (window-label window) (princ-to-string (window-number session window))))

(defun encode-layout (it)
  "A layout as it goes out: a pane's id, or a split as its way and its parts."
  (cond ((null it) nil)
        ((split-p it) (cons (split-way it) (mapcar #'encode-layout (split-parts it))))
        (t (pane-id it))))

(defun encode-windows (session)
  "SESSION's windows as a client is told them: number, name, how many panes, how
many of them are asking, and whether it is the one shown."
  (loop :for w :in (session-windows session)
        :for n :from 1
        :collect (list n (window-label w) (length (window-panes w))
                       (count :blocked (window-panes w)
                              :key (lambda (p) (agent:agent-state (pane-agent p))))
                       (same-window-p w (session-window session)))))

(defun session-split (session way)
  "Another pane beside the one that has the cursor, running what that one runs."
  (let* ((focus (session-focus session))
         (new (make-pane (pane-command focus)
                         :rows (pane-height focus)
                         :cols (pane-width focus)
                         :directory (pane-directory focus))))
    (change-window session (session-window session)
                   (lambda (w)
                     (setf (window-layout w) (layout-insert (window-layout w) focus way new)
                           (window-focus w) new)))
    (session-compose session)
    (pane-start new :environment (pane-environment session new))
    (run-hook 'pane-started session new)
    new))

(defun find-pane (server name id)
  (let ((session (session-named server name)))
    (and session (find id (session-panes session) :key #'pane-id))))

(defun session-close-pane (session pane)
  "Take PANE out of its window and let its program go. A window left with
nothing in it goes too, unless it is the last: an empty last window is a
session that is over."
  (let ((window (window-of session pane)))
    (unless window (return-from session-close-pane (session-panes session)))
    (change-window session window
                   (lambda (w)
                     (setf (window-layout w) (layout-remove (window-layout w) pane))
                     (when (eq pane (window-zoomed w))
                       (setf (window-zoomed w) nil))
                     (when (eq pane (window-focus w))
                       (setf (window-focus w) (first (window-panes w))))))
    (when (eq pane (second (session-held session)))
      (setf (session-held session) nil))
    (pane-close pane)
    (run-hook 'pane-ended session pane)
    (let ((now (window-now session window)))
      (when (and now (null (window-panes now)) (rest (session-windows session)))
        (session-remove-window session now))
      (session-panes session))))

(defun session-delete-other-panes (session pane)
  "Every other pane in PANE's window goes, and PANE has the whole of it."
  (let ((window (window-of session pane)))
    (dolist (other (remove pane (window-panes window)))
      (session-close-pane session other))
    (window-panes (window-now session window))))

(defun session-focus-pane (session pane)
  "PANE has the focus. A zoom was of the pane that had it, and goes with it, the
way it does in every multiplexer: the one just chosen is to be seen in its place."
  (let ((window (window-of session pane)))
    (when window (session-show-window session window)))
  (unless (eq pane (session-focus session))
    (change-window session (session-window session)
                   (lambda (w)
                     (setf (window-focus w) pane)
                     (unless (eq pane (window-zoomed w))
                       (setf (window-zoomed w) nil))))))

(defun session-focus-next (session)
  (let* ((panes (window-panes (session-window session)))
         (at (position (session-focus session) panes)))
    (when panes
      (session-focus-pane session (nth (mod (1+ (or at -1)) (length panes)) panes)))
    (session-focus session)))
