;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(declaim (ftype function confirm open-drawer prompt-window-name prompt-session-name))

(declaim (special +drawer-poll-interval+))

(defparameter +side-width+ 34 "How wide the side pane is.")

(defparameter +tree-width+ 22 "How wide the side pane is folded to its tree, in the one-session zoom.")

(defparameter +side-from+ 110 "A terminal narrower than this has no room for the side pane.")

(defparameter +card-width+ 35)

(defparameter +card-height+ 8)

(defparameter +big-card-width+ 72)

(defstruct (board (:constructor %make-board))
  (cursor nil)                          ; (session window-n pane-id)
  (offsets (make-hash-table :test 'equal)) ; session -> how far its lane is slid, in cells
  (scroll-y 0 :type fixnum)                   ; how far the lanes are offsets up
  (zoom :cards)                         ; :overview, :cards or :one
  (side t)                              ; whether the side pane is shown
  (folded nil :type list)               ; sessions folded in the tree
  (picked nil :type list)               ; (session . id) keys picked for a prompt
  (query "" :type string)
  (filtering nil)
  (composing nil)
  (sort :asking)                        ; :asking, :name or :order
  (fold-quiet nil)                      ; whether lanes with nothing doing are folded
  (live t)                              ; whether windows show their panes' last rows
  (starting nil)
  (following t)                         ; whether the view is to slide to the cursor when next drawn
  (held nil)                            ; a rail's thumb taken hold of: (:rail-h session rail grabbed) or (:rail-v rail grabbed)
  (area-top 0 :type fixnum)             ; the screen row the lanes start on, as last drawn
  (log-scroll-y 0 :type fixnum)               ; how far the activity log is scrolled
  (requested-at 0 :type integer)
  (regions nil :type list))             ; where things were drawn, for clicks

(defun state-rank (row)
  (or (position (row-state row) +state-rank+) (length +state-rank+)))

(defun row-state (row)
  "What ROW's pane is doing, when that means anything: nil for a program
nobody knows how to read, whose screen moving or not is all there is."
  (and (getf row :known) (getf row :state)))

(defun board-rows (b client)
  (let ((rows (client-pane-rows client)))
    (if (plusp (length (board-query b)))
        (matches (board-query b) rows
                 (lambda (r) (format nil "~A ~@[~A~] ~@[~A~]" (row-text r) (getf r :doing)
                                     (getf r :window-name))))
        rows)))

(defun session-counts (client session)
  "How many panes of SESSION are asking, working, idle, and not read."
  (let ((rows (client-session-rows client session)))
    (list (count :blocked rows :key #'row-state)
          (count :working rows :key #'row-state)
          (count :idle rows :key #'row-state)
          (count nil rows :key #'row-state))))

(defun board-sessions (b client)
  "Every session's name: in the server's order, or asking first, or by name,
as the sort says; narrowed to the ones with a pane the filter matches."
  (let* ((rows (board-rows b client))
         (named (mapcar #'car
                        (sort (remove-duplicates (mapcar (lambda (r) (cons (getf r :session) (or (getf r :order) 0)))
                                                         rows)
                                                 :test #'equal :key #'car)
                              #'< :key #'cdr))))
    (case (board-sort b)
      (:name (sort (copy-list named) #'string<))
      (:asking (stable-sort (copy-list named) #'>
                            :key (lambda (s) (first (session-counts client s)))))
      (t named))))

(defun pane-row-of (client session id)
  (gethash (cons session id) (client-panes client)))

(defun window-rows (client session window)
  "The pane rows of window WINDOW of SESSION, in layout order."
  (remove nil (mapcar (lambda (id) (pane-row-of client session id)) (panes-in-tree (third window)))))

(defun window-worst-row (client session window)
  (first (sort (copy-list (window-rows client session window)) #'< :key #'state-rank)))

(defun window-name (client session window)
  "What to call WINDOW: its label, else its first pane's name."
  (or (second window)
      (let ((rows (window-rows client session window))) (and rows (row-name (first rows))))
      ""))

(defun clients-on (client session &optional n)
  "The attached terminals looking at SESSION, or at its window N, this one included."
  (remove-if-not (lambda (c) (and (equal session (fifth c)) (or (null n) (eql n (sixth c)))))
                 (client-clients client)))

(defun quiet-p (client session)
  "Whether nothing in SESSION is asking or working."
  (let ((counts (session-counts client session)))
    (and (zerop (first counts)) (zerop (second counts)))))

(defun board-place (b client)
  "The cursor as (session window-n pane-id), put somewhere real when it is on
nothing: where this client is looking, else the first pane of the first
window of the first session."
  (let ((cursor (board-cursor b)))
    (or (and cursor
             (pane-row-of client (first cursor) (third cursor))
             cursor)
        (let* ((sessions (board-sessions b client))
               (session (or (find (board-starting b) sessions :test #'equal) (first sessions)))
               (mine (find (client-id client) (client-clients client) :key #'first))
               (focused (find-if (lambda (r) (and (equal session (getf r :session)) (getf r :focus)))
                                 (client-pane-rows client)))
               (n (or (and mine (equal (fifth mine) session) (sixth mine))
                      (and focused (getf focused :window))
                      1))
               (window (and session (or (find n (windows-of client session) :key #'first)
                                        (first (windows-of client session)))))
               (id (and window (or (and focused (member (getf focused :id) (panes-in-tree (third window)))
                                        (getf focused :id))
                                   (first (panes-in-tree (third window)))))))
          (and id (setf (board-cursor b) (list session (first window) id)))))))

(defun board-row (b client)
  "The pane row under the cursor."
  (let ((place (board-place b client)))
    (and place (pane-row-of client (first place) (third place)))))

(defun cursor-key (b client)
  (let ((row (board-row b client))) (and row (row-key row))))

(defun board-targets (b client)
  "What a prompt from the composer goes to: what is picked, or the pane under
the cursor when nothing is."
  (let ((rows (board-rows b client)))
    (or (remove-if-not (lambda (r) (member (row-key r) (board-picked b) :test #'equal)) rows)
        (let ((it (board-row b client))) (and it (list it))))))
