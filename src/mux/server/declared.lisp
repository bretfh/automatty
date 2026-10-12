;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defvar *declared* (make-hash-table :test 'equal :synchronized t))

(defun declare-server (form)
  (let ((name (server-file-name (second form) "a server")))
    (setf (gethash name *declared*) form)
    name))

(defun declared-servers ()
  (sb-ext:with-locked-hash-table (*declared*)
    (loop :for name :being :the :hash-keys :of *declared* :collect name)))

(defun declared-for (server)
  (gethash (file-namestring (server-path server)) *declared*))

(defun expand-home (path)
  (if (and (plusp (length path))
           (char= #\~ (char path 0))
           (or (= 1 (length path)) (char= #\/ (char path 1))))
      (concatenate 'string
                   (string-right-trim "/" (namestring (user-homedir-pathname)))
                   (subseq path 1))
      path))

(defun declared-directory (base given)
  (if (or (null given) (zerop (length given)))
      base
      (let ((path (expand-home given)))
        (if (or (null base) (char= #\/ (char path 0)))
            path
            (concatenate 'string (string-right-trim "/" base) "/" path)))))

(defun env-name (entry)
  (subseq entry 0 (or (position #\= entry) (length entry))))

(defun declared-env (base given)
  (remove-duplicates (append base given) :key #'env-name :test #'string=))

(defun declared-pane (server form directory env)
  (destructuring-bind (command &key name ((:directory here)) ((:env more))) (rest form)
    (let ((pane (make-pane (or command (server-command server) (default-shell))
                           :directory (declared-directory directory here))))
      (setf (pane-name pane) name
            (pane-env pane) (declared-env env more))
      pane)))

(defun declared-layout (server form directory env)
  (ecase (first form)
    (:pane (declared-pane server form directory env))
    ((:columns :rows)
     (let ((parts (mapcar (lambda (part) (declared-layout server part directory env))
                          (getf (rest form) :parts))))
       (if (rest parts)
           (make-split (if (eq (first form) :columns) :across :down) parts)
           (first parts))))))

(defun declared-window (server form directory env)
  (destructuring-bind (&key ((:directory here)) ((:env more)) layout) (cddr form)
    (%make-window :name (second form)
                  :layout (declared-layout server layout
                                           (declared-directory directory here)
                                           (declared-env env more)))))

(defun start-declared (session window)
  (dolist (pane (window-panes window))
    (multiple-value-bind (rows cols)
        (laid-size session (make-view) pane (window-layout window))
      (pane-resize pane rows cols))
    (session-start-pane session pane)
    (run-hook 'pane-started session pane))
  (run-hook 'window-made session window))

(defun make-declared-session (server form directory env)
  (destructuring-bind (&key ((:directory here)) ((:env more)) windows) (cddr form)
    (let* ((directory (declared-directory directory here))
           (env (declared-env env more))
           (made (mapcar (lambda (w) (declared-window server w directory env)) windows))
           (session (%make-session :name (second form) :socket (server-path server)
                                   :windows made :server server)))
      (sb-ext:atomic-update (server-sessions server) (lambda (all) (append all (list session))))
      (setf (server-had-sessions server) t)
      (dolist (window made) (start-declared session window))
      (run-hook 'session-made session)
      session)))

(defun make-missing-windows (server session form directory env)
  (destructuring-bind (&key ((:directory here)) ((:env more)) windows) (cddr form)
    (let ((directory (declared-directory directory here))
          (env (declared-env env more)))
      (loop :for w :in windows
            :unless (find (second w) (session-windows session) :key #'window-name :test #'equal)
              :collect (let ((window (declared-window server w directory env)))
                         (change-session session
                                         (lambda (s)
                                           (setf (state-windows s)
                                                 (append (state-windows s) (list window)))))
                         (start-declared session window)
                         window)))))

(defun make-declared (server)
  (let ((form (declared-for server)))
    (when form
      (on-server server
                 (lambda ()
                   (destructuring-bind (&key ((:directory here)) env sessions) (cddr form)
                     (let ((directory (declared-directory nil here)))
                       (loop :for s :in sessions
                             :for there := (session-named server (second s))
                             :if there
                               :append (make-missing-windows server there s directory env)
                             :else
                               :collect (make-declared-session server s directory env)))))))))
