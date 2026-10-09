;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun watcher-server (watcher)
  (let ((session (watcher-session watcher)))
    (and session (session-server session))))

(defun watcher-session-name (watcher)
  (let ((session (watcher-session watcher)))
    (and session (session-name session))))



(defun row-key (row) (cons (getf row :session) (getf row :id)))

(defun rows-and-table (watcher)
  (flet ((make ()
           (let* ((server (watcher-server watcher))
                  (now (now-ms))
                  (rows (and server (mapcar (lambda (r) (list* :heard-at now r))
                                            (pane-rows server now watcher))))
                  (table (make-hash-table :test 'equal)))
             (dolist (r rows) (setf (gethash (row-key r) table) r))
             (cons rows table))))
    (if *rows*
        (or (car *rows*) (setf (car *rows*) (make)))
        (make))))

(defun pane-rows-of (watcher)
  "A row for every pane on WATCHER's server, as PANE-ROW makes them."
  (car (rows-and-table watcher)))

(defun pane-row-of (watcher session id)
  (gethash (cons session id) (cdr (rows-and-table watcher))))

(defun session-rows-of (watcher session)
  (remove-if-not (lambda (r) (equal session (getf r :session))) (pane-rows-of watcher)))

(defun row-name (row)
  "What to call a pane on the board: what somebody called it, else its
program. Not its title: a shell's title is a path nobody wants forty of."
  (or (getf row :label) (getf row :command) (getf row :says) ""))

(defun row-pane (watcher row)
  (let ((server (watcher-server watcher)))
    (and server row (find-pane server (getf row :session) (getf row :id)))))

(defun clients-of (watcher)
  (let ((server (watcher-server watcher)))
    (and server (encode-clients server (now-ms)))))

(defun recent-of (watcher &optional (n 5))
  (let ((server (watcher-server watcher)))
    (and server (recent-answers server n (now-ms)))))

(defun events-of (watcher &optional (n 64))
  (let ((server (watcher-server watcher)))
    (and server (encode-events server n (now-ms)))))

(defun other-clients-at (watcher session window)
  "The attached terminals looking at WINDOW of SESSION, other than WATCHER."
  (remove-if-not (lambda (c) (and (equal session (fifth c)) (eql window (sixth c))
                                  (not (eql (first c) (watcher-id watcher)))))
                 (clients-of watcher)))

(defun windows-of (watcher session)
  "SESSION's windows as (n label tree focus-id shownp), the focus and what is
shown being WATCHER's own."
  (let* ((server (watcher-server watcher))
         (it (and server (session-named server session)))
         (mine (and it (eq it (watcher-session watcher))))
         (view (watcher-view watcher)))
    (and it
         (loop :for w :in (session-windows it)
               :for n :from 1
               :collect (let ((focus (if mine (view-focus-in it view w) (first (window-panes w)))))
                          (list n (window-label w) (encode-layout (window-layout w))
                                (and focus (pane-id focus))
                                (and mine (same-window-p w (watcher-window watcher)))))))))

(defun panes-in-tree (tree)
  (cond ((null tree) nil)
        ((consp tree) (loop :for part :in (rest tree) :append (panes-in-tree part)))
        (t (list tree))))

(defun pane-cells (watcher session id)
  (let* ((server (watcher-server watcher))
         (pane (and server (find-pane server session id))))
    (if pane
        (mapcar (lambda (cell) (cons (car cell) (cdr cell))) (last (pane-pulse-now pane (now-ms)) +spark-cells+))
        (loop :repeat +spark-cells+ :collect (cons 0 nil)))))

(defun window-cells (watcher session window)
  (merge-cells (mapcar (lambda (id) (pane-cells watcher session id)) (panes-in-tree (third window)))))

(defun session-cells (watcher session)
  (merge-cells (mapcar (lambda (w) (window-cells watcher session w)) (windows-of watcher session))))

(defun server-cells (watcher sessions)
  (merge-cells (mapcar (lambda (s) (session-cells watcher s)) sessions)))

(defparameter +pane-screen-rows+ 16)

(defun pane-screen-of (watcher key)
  "The last rows of the pane KEY names that have anything on them."
  (let* ((server (watcher-server watcher))
         (pane (and server (find-pane server (car key) (cdr key)))))
    (and pane (let ((shown (pane-shown-now pane)))
                (if shown
                    (screen-last-rows (shown-screen shown) +pane-screen-rows+)
                    (pane-last-rows pane +pane-screen-rows+))))))
