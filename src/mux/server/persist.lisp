;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun restore-panes (server dir tree now)
  "Every pane the saved TREE names, by id, from its own file; one whose file
is missing or unreadable comes back empty, and the server's notes say so."
  (let ((panes (make-hash-table)))
    (dolist (id (tree-pane-ids tree) panes)
      (multiple-value-bind (form status) (read-state-file (pane-file dir id) :atty-pane)
        (let ((rows (saved-session-size tree id)))
          (case status
            (:ok
             (multiple-value-bind (pane note) (decode-pane form now)
               (setf (gethash id panes) pane)
               (when note (push (list note :warning) (server-notes server)))))
            (t
             (setf (gethash id panes)
                   (make-empty-pane id (first rows) (second rows)
                                    (if (eq status :missing) "not on disk" "unreadable")))
             (push (list (format nil "pane ~D came back empty: its file ~A" id
                                 (if (eq status :missing) "was not there" "could not be read"))
                         :warning)
                   (server-notes server)))))))))

(defun restore-state (server &optional (dir (server-state-dir server)))
  "Bring back what the server called by DIR's name held when it was last
saved. Answers the sessions, and puts what could not be as it was in the
server's notes for whoever attaches first."
  (flet ((note (control &rest args)
           (push (list (apply #'format nil control args) :warning) (server-notes server))
           nil))
    (multiple-value-bind (tree status) (read-state-file (tree-file dir) :atty-state)
      (case status
        (:missing nil)
        (:truncated (note "~A could not be read; nothing was brought back" (tree-file dir)))
        (:wrong-version (note "~A was written by another build and was moved aside" (tree-file dir)))
        (t
         (let* ((now (now-ms))
                (panes (restore-panes server dir tree now))
                (sessions (loop :for form :in (getf (nthcdr 2 tree) :sessions)
                                :for session := (decode-session server form panes)
                                :when session :collect session)))
           (setf *panes-made* (max *panes-made* (or (getf (nthcdr 2 tree) :panes-made) 0)
                                  (reduce #'max (tree-pane-ids tree) :initial-value 0)))
           (dolist (session sessions)
             (session-compose session)
             (dolist (pane (session-panes session))
               (pane-start pane :environment (pane-environment session pane))))
           (setf (server-sessions server) (append (server-sessions server) sessions)
                 (server-had-sessions server) (or (server-had-sessions server) (and sessions t))
                 (server-tree-saved server) (encode-tree server))
           (dolist (session sessions)
             (dolist (pane (session-panes session))
               (run-hook 'pane-started session pane)))
           (run-hook 'server-restored server sessions)
           sessions))))))

(defun saved-session-size (tree id)
  "The size of the session the pane ID is in, as (rows cols), for a pane that
comes back with no file of its own."
  (dolist (session (getf (nthcdr 2 tree) :sessions) (list 24 80))
    (dolist (window (getf session :windows))
      (when (member id (let ((ids nil))
                         (labels ((walk (said)
                                    (cond ((integerp said) (push said ids))
                                          ((consp said) (mapc #'walk (rest said))))))
                           (walk (getf window :layout)))
                         ids))
        (return-from saved-session-size
          (list (getf session :rows) (getf session :cols)))))))

(defun save-tree (server &optional (dir (server-state-dir server)) force)
  "Write the tree if it is not what was last written, or when FORCE, and drop
the files of panes that are in no session any more. Answers whether it was
written."
  (let ((tree (encode-tree server)))
    (when (or force (not (equal tree (server-tree-saved server))))
      (run-hook 'before-save server)
      (when (write-form-atomically (tree-file dir)
                                   (list* :atty-state +state-version+
                                          :saved (get-universal-time)
                                          (cddr tree)))
        (setf (server-tree-saved server) tree)
        (let ((live (loop :for session :in (server-sessions server)
                          :append (mapcar #'pane-id (session-panes session)))))
          (loop :for (id . path) :in (pane-files dir)
                :unless (member id live) :do (ignore-errors (delete-file path))))
        t))))

(defun save-pane (server pane now &optional (dir (server-state-dir server)))
  (when (write-form-atomically (pane-file dir (pane-id pane)) (encode-pane pane now))
    (setf (pane-saved-at pane) now)
    t))

(defun pane-changed-at (pane)
  (max (pane-moved-at pane) (pane-touched pane)))

(defun save-due-p (pane now)
  "Whether PANE has changed since it was saved and has either been quiet for
+SAVE-QUIET-AFTER+ or gone unsaved for +SAVE-AT-MOST-EVERY+."
  (let ((changed (pane-changed-at pane)))
    (and (> changed (pane-saved-at pane))
         (or (>= (- now changed) +save-quiet-after+)
             (>= (- now (pane-saved-at pane)) +save-at-most-every+)))))

(defun save-due (server now &optional (dir (server-state-dir server)))
  "One turn of saving: the tree if it changed, and the one pane longest owed
a save, so no turn of the loop stalls on more than one pane's rows."
  (save-tree server dir)
  (let ((due (loop :for session :in (server-sessions server)
                   :append (remove-if-not (lambda (p) (save-due-p p now))
                                          (session-panes session)))))
    (when due
      (save-pane server (reduce (lambda (a b) (if (<= (pane-saved-at a) (pane-saved-at b)) a b))
                                due)
                 now dir))))

(defun save-all (server &optional (dir (server-state-dir server)))
  "The tree and every pane, now. The tree is written whether or not it changed,
so when it says it was saved is when everything was."
  (let ((now (now-ms)))
    (save-tree server dir t)
    (dolist (session (server-sessions server))
      (dolist (pane (session-panes session))
        (save-pane server pane now dir)))))

(defparameter +save-tick+ 1000
  "Milliseconds between looks at what is owed a save.")

(defun schedule-saves (server)
  "Look at what is owed a save every +SAVE-TICK+ for as long as the server
runs, stepping over a save that comes apart: a disk that is full is not a
reason to lose the panes."
  (labels ((tick ()
             (when (and (server-saving server) (server-running server))
               (handler-case (save-due server (now-ms))
                 (error (e) (report-error e)))
               (schedule-task server +save-tick+ #'tick))))
    (schedule-task server +save-tick+ #'tick)))

(defun pane-touch (pane)
  "PANE changed in something other than its screen: its name, its log."
  (setf (pane-touched pane) (now-ms)))

(defun move-state-aside (&optional (name (server-name)))
  "Put the state of the server called NAME out of the way, so it starts with
nothing. Answers where it went, or nil when there was none."
  (let* ((dir (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "atty/~A/" (server-file-name name "a server")) (state-home))))
         (aside (merge-pathnames
                 (format nil "atty/~A.fresh-~A/" (server-file-name name "a server")
                         (multiple-value-bind (s m h day month year) (get-decoded-time)
                           (format nil "~D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D" year month day h m s)))
                 (state-home))))
    (when (probe-file dir)
      (sb-posix:rename (string-right-trim "/" (namestring dir))
                       (string-right-trim "/" (namestring aside)))
      aside)))

(defun delete-saved-session (session-name &optional (name (server-name)))
  "Take the session called SESSION-NAME out of what was saved for the server
called NAME, its panes' files with it. Answers whether it was there."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "atty/~A/" (server-file-name name "a server")) (state-home)))))
    (multiple-value-bind (tree status) (read-state-file (tree-file dir) :atty-state)
      (when (eq status :ok)
        (let* ((sessions (getf (nthcdr 2 tree) :sessions))
               (gone (find session-name sessions :key (lambda (s) (getf s :name)) :test #'equal)))
          (when gone
            (let ((ids nil))
              (labels ((walk (said)
                         (cond ((integerp said) (pushnew said ids))
                               ((consp said) (mapc #'walk (rest said))))))
                (dolist (w (getf gone :windows)) (walk (getf w :layout))))
              (dolist (id ids) (ignore-errors (delete-file (pane-file dir id)))))
            (let ((left (remove gone sessions)))
              (if left
                  (write-form-atomically (tree-file dir)
                                         (list* :atty-state +state-version+
                                                :saved (get-universal-time)
                                                (let ((rest (copy-list (cddr tree))))
                                                  (setf (getf rest :sessions) left)
                                                  (remf rest :saved)
                                                  rest)))
                  (ignore-errors (delete-file (tree-file dir)))))
            t))))))

(defun saved-sessions (&optional (name (server-name)))
  "What was saved for the server called NAME: (name windows panes saved-at)
rows, or nothing."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "atty/~A/" (server-file-name name "a server")) (state-home)))))
    (multiple-value-bind (tree status) (read-state-file (tree-file dir) :atty-state)
      (when (eq status :ok)
        (loop :for session :in (getf (nthcdr 2 tree) :sessions)
              :collect (list (getf session :name)
                             (length (getf session :windows))
                             (length (let ((ids nil))
                                       (labels ((walk (said)
                                                  (cond ((integerp said) (pushnew said ids))
                                                        ((consp said) (mapc #'walk (rest said))))))
                                         (dolist (w (getf session :windows))
                                           (walk (getf w :layout))))
                                       ids))
                             (getf (nthcdr 2 tree) :saved)))))))
