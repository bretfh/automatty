;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defparameter +base-message-types+
  '(:attach :keys :resize :knock :bar :detach :stop)
  "What every server has always known how to do.

A server that says nothing about what it can do is one from before it said, and
these are what it had then. A client assuming more of it would send keys into
silence.")

(defstruct (client (:constructor %make-client))
           (wire nil)
           (socket nil)
           (fd 0 :type fixnum)
           (to 1 :type fixnum)
           (screen nil)
           (from nil)
           (shown nil)
           (overlays nil)
           (dirty t :type boolean)
           (takes t)
           (rows 24 :type fixnum)
           (cols 80 :type fixnum)
           (waiting nil)
           (partial-chord nil :type list)
           (partial "" :type string)
           (greeted nil :type boolean)
           (message-types +base-message-types+)
           (mode 'pane-mode)
           (running t :type boolean)
           (exit-reason nil)
           ;; what the server has told this client pane-info every pane, while
           ;; something on top has asked to be kept told
           (id nil)
           (panes (make-hash-table :test 'equal))
           (find-text nil)
           (found nil)
           (screens (make-hash-table :test 'equal))
           (recent nil)
           (clients nil)
           (pulses (make-hash-table :test 'equal))
           (events nil)
           (events-at 0 :type integer)
           (barp t)
           (layouts (make-hash-table :test 'equal))
           (watching 0 :type fixnum)
           (ticked 0 :type integer)
           (session nil)
           (pane-info (make-hash-table :test 'equal))
           ;; half a chord, and the menu of what can follow it once it has
           ;; been pending long enough to want one
           (pending-since nil)
           (menu nil)
           (warned-old-server nil))

(defun connect-to (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    (values (make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket)
            socket)))

(declaim (ftype function open-prompt prompt-pane-name menu-due-p draw-menu handle-menu-click
                        apply-config))

(defun client-ms ()
  (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun subscribe-panes (client)
  "Something on top wants to be told about every pane. Asked for once however
many want it, and dropped when the last of them goes."
  (when (= 1 (incf (client-watching client)))
    (wire-send (client-wire client) '(:watch-panes t))
    (wire-send (client-wire client) '(:watch-screens 16)))
  (wire-send (client-wire client) '(:lately 5))
  (wire-send (client-wire client) '(:clients)))

(defun other-clients-at (client session window)
  "The attached terminals looking at WINDOW of SESSION, other than this one:
what a question's line says when somebody else already has it in front of them."
  (remove-if-not (lambda (c) (and (equal session (fifth c)) (eql window (sixth c))
                                  (not (eql (first c) (client-id client)))))
                 (client-clients client)))

(defun unsubscribe-panes (client)
  (when (zerop (setf (client-watching client) (max 0 (1- (client-watching client)))))
    (wire-send (client-wire client) '(:watch-panes nil))
    (wire-send (client-wire client) '(:watch-screens nil))
    (clrhash (client-panes client))
    (clrhash (client-screens client))))

(defun tty-name (fd)
  "What the terminal on FD is called, such as /dev/ttys004, or nil. It is how a
server tells one person's keys from another's in a pane's log."
  (and (tty:a-terminal-p fd)
       (ignore-errors
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "ttyname" (function sb-alien:c-string sb-alien:int))
         fd))))

(defun terminal-size (fd)
  "How big the terminal on FD is. One that says it has no rows or no columns,
as a terminal made by a program that never set one does, is taken to be the
size a terminal is when nobody said: a pane one column wide is no use to
anybody."
  (multiple-value-bind (rows cols)
      (if (tty:a-terminal-p fd) (tty:host-size fd) (values 24 80))
    (if (or (zerop rows) (zerop cols))
        (values 24 80)
        (values rows cols))))

(defun make-client (path &key name open (fd tty:+stdin+) (to tty:+stdout+)
                              (takes (tty:takes-of)))
  "A client on the server at PATH, joined to the session called NAME, or the
first one when NAME is nil. OPEN is (command directory [label]): the session is
made with them when it is not there, its first pane called LABEL."
  (multiple-value-bind (rows cols) (terminal-size fd)
                       (multiple-value-bind (wire socket) (connect-to path)
                                            (let ((client (%make-client :wire wire :socket socket :fd fd :to to
                                                                        :takes takes :rows rows :cols cols
                                                                        :screen (tty:make-screen :width cols :height rows)
                                                                        :waiting (tty:make-waiting 4))))
                                              ;; the name goes first and on its
                                              ;; own. A server that does not know
                                              ;; about names passes it over and
                                              ;; the attach it does know is the
                                              ;; shape it has always been
                                              (wire-send wire (list :who (tty-name fd)))
                                              (if open
                                                  (destructuring-bind (command directory &optional label)
                                                      open
                                                    (wire-send wire (list :open name command directory
                                                                          rows cols takes label)))
                                                  (progn
                                                    (when name
                                                      (wire-send wire (list :want name)))
                                                    (wire-send wire (list :attach rows cols takes))))
                                              (wire-flush wire)
                                              client))))

(defun client-close (client)
  "Shut it, once. The wire owns the socket, so closing the wire is the close;
doing it again here would shut a descriptor number that by then belongs to
whoever opened the next one."
  (setf (client-running client) nil)
  (tty:free-waiting (client-waiting client))
  (wire-close (client-wire client)))

(defun host-write (client said)
  "Write to the terminal the client is sitting in. What the screen holds is
characters, not bytes, so this is the one place that says how they are spelt.

A terminal that will not take what it is given has gone, which is a reason to
stop rather than a fault: the panes are still running and somebody can attach to
them again."
  (handler-case (pty:pty-write-string (client-to client) said :utf-8)
    (error () (client-stop client :terminal-gone) 0)))

(defvar *version-noted* nil
  "Whether this client has said that the server is another build: once.")

(defun client-fit (client rows cols)
  (setf (client-screen client) (tty:make-screen :width cols :height rows)
        (client-from client) (tty:make-screen :width cols :height rows)
        (client-shown client) (tty:make-screen :width cols :height rows)
        (client-dirty client) t))

(defun client-draw (client)
  "Put the session, and then whatever is drawn on top of it, onto the terminal.

The client composes for itself rather than writing out the runs the server sent,
because what is on top is this one person's: a prompt somebody opened here is
not something everybody attached should be shown."
  (let ((work (client-screen client)))
    (when (and work (client-from client) (client-shown client))
      (tty:screen-copy work (client-from client))
      (let ((*overlay-client* client))
        (dolist (it (reverse (client-overlays client)))
          (draw-overlay it work))
        (if (menu-due-p client)
            (draw-menu client work)
            (setf (client-menu client) nil)))
      (let ((runs (tty:screen-diff (client-shown client) work)))
        (host-write client
                  (with-output-to-string (s)
                    (tty:encode-frame work runs s :takes (client-takes client)))))
      (setf (client-dirty client) nil))))

(defun current-client-mode (client)
  "The mode whatever is on top asks for, or the pane's when nothing is."
  (setf (client-mode client) (mode-of (first (client-overlays client)))
        (client-partial-chord client) nil
        (client-pending-since client) nil
        (client-menu client) nil))

(defun client-push-overlay (client it)
  (push it (client-overlays client))
  (current-client-mode client)
  (setf (client-dirty client) t)
  it)

(defun client-pop-overlay (client it)
  (setf (client-overlays client) (remove it (client-overlays client)))
  (current-client-mode client)
  (setf (client-dirty client) t))

(defun client-stop (client why)
  "Stop, for the first reason there was. What came after it is what stopping
looks like, not why it happened."
  (setf (client-running client) nil)
  (unless (client-exit-reason client)
    (setf (client-exit-reason client) why)))

(defun rehash-session (table old new)
  "Every key (OLD . id) in TABLE moved to (NEW . id)."
  (let ((moved nil))
    (maphash (lambda (k v) (when (equal (car k) old) (push (cons k v) moved))) table)
    (loop :for (k . v) :in moved
          :do (remhash k table)
              (setf (gethash (cons new (cdr k)) table) v))))

(defun client-session-renamed (client old new)
  "The server called the session OLD NEW: everything this client keeps by
the old name is kept by the new one, and whatever is on top is told."
  (when (equal (client-session client) old)
    (setf (client-session client) new))
  (dolist (table (list (client-panes client) (client-screens client)
                       (client-pulses client) (client-pane-info client)))
    (rehash-session table old new))
  (dolist (row (loop :for v :being :the :hash-values :of (client-panes client) :collect v))
    (when (equal (getf row :session) old)
      (setf (getf row :session) new)))
  (let ((layout (gethash old (client-layouts client))))
    (when layout
      (remhash old (client-layouts client))
      (setf (gethash new (client-layouts client)) layout)))
  (setf (client-events client)
        (mapcar (lambda (e) (if (equal (fourth e) old) (append (subseq e 0 3) (list new) (nthcdr 4 e)) e))
                (client-events client)))
  (setf (client-clients client)
        (mapcar (lambda (c) (if (equal (fifth c) old) (append (subseq c 0 4) (list new) (nthcdr 5 c)) c))
                (client-clients client))
        (client-recent client)
        (mapcar (lambda (e) (if (equal (first e) old) (cons new (rest e)) e)) (client-recent client)))
  (dolist (it (client-overlays client)) (overlay-session-renamed it old new))
  (setf (client-dirty client) t))

(defun base64 (text)
  "TEXT as base64, the way OSC 52 wants it."
  (let* ((bytes (sb-ext:string-to-octets text :external-format :utf-8))
         (table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (out (make-string-output-stream)))
    (loop :for i :from 0 :below (length bytes) :by 3
          :do (let* ((n (min 3 (- (length bytes) i)))
                     (b0 (aref bytes i))
                     (b1 (if (> n 1) (aref bytes (+ i 1)) 0))
                     (b2 (if (> n 2) (aref bytes (+ i 2)) 0))
                     (word (logior (ash b0 16) (ash b1 8) b2)))
                (write-char (char table (ldb (byte 6 18) word)) out)
                (write-char (char table (ldb (byte 6 12) word)) out)
                (write-char (if (> n 1) (char table (ldb (byte 6 6) word)) #\=) out)
                (write-char (if (> n 2) (char table (ldb (byte 6 0) word)) #\=) out)))
    (get-output-stream-string out)))

(defun client-redraw (client)
  (when (client-shown client)
    (setf (client-shown client)
          (tty:make-screen :width (tty:screen-width (client-shown client))
                       :height (tty:screen-height (client-shown client)))))
  (setf (client-dirty client) t)
  (host-write client tty:+blanked+)
  (wire-send (client-wire client)
             (list :resize (client-rows client) (client-cols client))))

(defun client-resized (client)
  (setf tty:*resized* nil)
  (when (tty:a-terminal-p (client-fd client))
    (multiple-value-bind (rows cols) (terminal-size (client-fd client))
                         (unless (and (= rows (client-rows client)) (= cols (client-cols client)))
                           (setf (client-rows client) rows
                                 (client-cols client) cols)
                           (wire-send (client-wire client) (list :resize rows cols))))))

(defun client-step (client &optional (patience 100))
  "One turn of the loop. What the server said is taken in before what was typed:
a bye says why everything is stopping, and a terminal that fell over at the same
moment would otherwise answer that question first, and answer it wrongly."
  (let* ((w (tty:waiting-clear (client-waiting client)))
         (keys (tty:waiting-add w (client-fd client)))
         (wire (client-wire client))
         (sock (tty:waiting-add w (wire-fd wire)
                            (logior sb-unix:pollin
                                    (if (plusp (wire-pending wire))
                                        sb-unix:pollout
                                      0)))))
    (tty:wait-on w (if (client-partial-chord client) (min patience 50) patience))
    (when tty:*resized* (client-resized client))
    (when (tty:writable-p (tty:waiting-back w sock))
      (wire-flush wire))
    (when (tty:readable-p (tty:waiting-back w sock))
      (if (null (wire-receive wire))
          (client-stop client :server-gone)
        (loop for form = (wire-read-message wire)
              while form
              do (handle-server-message client form))))
    (when (tty:readable-p (tty:waiting-back w keys))
      (let ((said (pty:pty-read-string (client-fd client) 8192)))
        (if (null said)
            (client-stop client :input-gone)
          (client-handle-input client said))))
    (when (and (some #'overlay-ticks-p (client-overlays client))
               (>= (- (client-ms) (client-ticked client)) 1000))
      (setf (client-ticked client) (client-ms)
            (client-dirty client) t))
    (when (and (menu-due-p client) (null (client-menu client)))
      (setf (client-dirty client) t))
    (when (client-dirty client) (client-draw client))
    (wire-flush wire)
    client))

(defparameter +hello-timeout+ 5
  "How many seconds to wait for the server to say what we are looking at.

A server that takes the connection and then says nothing leaves a terminal in
raw mode showing nothing at all, which says neither that anything is wrong nor
which end of it is wrong.")

(defun client-greeted-p (client since)
  (or (client-greeted client)
      (< (- (get-internal-real-time) since)
         (* +hello-timeout+ internal-time-units-per-second))
      (progn (client-stop client :no-answer) nil)))

(defun attach (path &key name open (fd tty:+stdin+) (to tty:+stdout+)
                         (takes (tty:takes-of)))
  (let ((client (make-client path :name name :open open :fd fd :to to :takes takes))
        (since (get-internal-real-time)))
    (unwind-protect
        (tty:with-host (fd :to to)
                   (loop while (and (client-running client)
                                    (not tty:*asked-to-stop*)
                                    (client-greeted-p client since)
                                    (wire-open (client-wire client)))
                         do (client-step client))
                   (when tty:*asked-to-stop* (client-stop client :asked-to-stop))
                   (client-exit-reason client))
      (ignore-errors (wire-send (client-wire client) '(:detach))
                     (wire-flush (client-wire client)))
      (client-close client))))

(defun client-pane-rows (client)
  (loop :for row :being :the :hash-values :of (client-panes client) :collect row))

(defun row-key (row) (cons (getf row :session) (getf row :id)))

(defun row-name (row)
  "What to call a pane on the board: what somebody called it, else its
program. Not its title: a shell's title is a path nobody wants forty of."
  (or (getf row :label) (getf row :command) (getf row :says) ""))

(defun client-session-rows (client session)
  (remove-if-not (lambda (r) (equal session (getf r :session))) (client-pane-rows client)))

(defun windows-of (client session)
  "SESSION's windows as (n label tree focus-id shownp), from the layouts the
server said; from the pane rows alone, side by side, when it has not yet."
  (or (gethash session (client-layouts client))
      (let* ((rows (client-session-rows client session))
             (ns (sort (remove-duplicates (mapcar (lambda (r) (or (getf r :window) 1)) rows)) #'<)))
        (loop :for n :in ns
              :collect (let ((in (sort (remove-if-not (lambda (r) (eql n (or (getf r :window) 1))) rows)
                                       #'< :key (lambda (r) (or (getf r :at) 0)))))
                         (list n (getf (first in) :window-name)
                               (if (rest in) (cons :across (mapcar (lambda (r) (getf r :id)) in))
                                   (getf (first in) :id))
                               (getf (find-if (lambda (r) (getf r :focus)) in) :id)
                               t))))))

(defun panes-in-tree (tree)
  (cond ((null tree) nil)
        ((consp tree) (loop :for part :in (rest tree) :append (panes-in-tree part)))
        (t (list tree))))

(defun pane-cells (client session id)
  (or (gethash (cons session id) (client-pulses client))
      (loop :repeat +spark-cells+ :collect (cons 0 nil))))

(defun window-cells (client session window)
  (merge-cells (mapcar (lambda (id) (pane-cells client session id)) (panes-in-tree (third window)))))

(defun session-cells (client session)
  (merge-cells (mapcar (lambda (w) (window-cells client session w)) (windows-of client session))))

(defun server-cells (client sessions)
  (merge-cells (mapcar (lambda (s) (session-cells client s)) sessions)))
