;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun socket-directory ()
  "Where the sockets live, made if it is not there and shut to everybody else.

A socket in it is a way to type into somebody's shell, so who may open the
directory is the whole of the access control there is: the sockets themselves
carry no idea of who is asking."
  (let* ((run (sb-ext:posix-getenv "XDG_RUNTIME_DIR"))
         (dir (ensure-directories-exist
               (pathname (if (and run (plusp (length run)))
                             (format nil "~A/atty/" (string-right-trim "/" run))
                             (format nil "/tmp/atty-~D/" (sb-posix:getuid)))))))
    (ignore-errors (sb-posix:chmod (namestring dir) #o700))
    dir))

(defvar *fresh-start* nil
  "Whether --fresh was said: the server starts with nothing, what it had on
disk put aside.")

(defvar *server-name* nil
  "Which server, when -L said. One server holds every session, the way tmux's
does; a second one is for somebody who asks for it by name.")

(defun server-file-name (said what)
  "SAID as a file name in the socket directory. A name with a directory in it
would put the socket somewhere nobody is looking for it and under permissions
nobody set."
  (let ((name (file-namestring (princ-to-string said))))
    (when (zerop (length name))
      (error "~S is not a name ~A can have" said what))
    name))

(defun server-name ()
  (let ((env (sb-ext:posix-getenv "ATTY_SERVER")))
    (or *server-name* (and env (plusp (length env)) env) "default")))

(defun socket-path (&optional (name (server-name)))
  "Where the socket for the server called NAME is."
  (namestring (merge-pathnames (server-file-name name "a server") (socket-directory))))

(defun server-socket-path ()
  "The server this invocation talks to: the one -L or ATTY_SERVER names, else
the one the pane it is running in belongs to, else the user's own."
  (let ((inside (sb-ext:posix-getenv "ATTY_SOCKET")))
    (if (and (null *server-name*)
             (null (sb-ext:posix-getenv "ATTY_SERVER"))
             inside (plusp (length inside)))
        inside
        (socket-path))))

(defun log-path (&optional (name (server-name)))
  (namestring (merge-pathnames (format nil "~A.log" (server-file-name name "a server"))
                               (socket-directory))))

(defun probe-socket (path &optional (patience 3))
  "What a server at PATH says when knocked on, or nil when nothing answers.

Something listening is not the same as a server: one wedged on its way out
still holds its socket, so a connection to it succeeds and then nothing ever
comes back, and a client that took that for a living server would sit at a
screen that never arrives."
  (and (probe-file path)
       (handler-case
           (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
             (sb-bsd-sockets:socket-connect socket path)
             (let ((wire (make-wire (sb-bsd-sockets:socket-file-descriptor socket)
                                    socket))
                   (deadline (+ (get-internal-real-time)
                                (* patience internal-time-units-per-second)))
                   (here nil))
               (unwind-protect
                    (progn
                      (wire-send wire '(:knock))
                      (wire-flush wire)
                      (loop until here
                            do (when (> (get-internal-real-time) deadline) (return))
                               (when (pty:pty-wait (wire-fd wire) 100)
                                 (unless (wire-receive wire) (return))
                                 (loop for form = (wire-read-message wire)
                                       while form
                                       do (when (and (consp form)
                                                     (eq :here (first form)))
                                            (setf here form))))))
                 (wire-close wire))
               here))
         (error () nil))))

(defun server-alive-p (path &optional (patience 3))
  (and (probe-socket path patience) t))

(defun other-servers ()
  "Every other socket here that answers, as (name path legacyp). A legacy one is
a server from before one server held every session: it holds the one session its
name is, and says so by not saying it is the other kind."
  (loop :for path :in (mapcar #'namestring (directory (merge-pathnames "*" (socket-directory))))
        :for name := (file-namestring path)
        ;; by name, not by path: the directory listing answers the path with
        ;; every link resolved (/private/tmp on a mac), which is not how it
        ;; was spelt when it was made
        :for here := (and (not (search ".log" name))
                          (string/= name (file-namestring (server-socket-path)))
                          (probe-socket path 1))
        :when here
          :collect (list name path (not (member :one-server here)))))

(defun request (path forms &key (patience 3) (done (constantly t)))
  "Say FORMS to the server at PATH and gather what it says back until DONE
likes a form or PATIENCE seconds pass. What was said is all sent before the
patience starts: a long prompt is more than a socket takes in one write, and
cutting it off halfway would leave the server holding half a message."
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (sb-bsd-sockets:socket-connect socket path)
        (let ((wire (make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket))
              (handle-message nil))
          (unwind-protect
               (progn
                 (dolist (form forms) (wire-send wire form))
                 (let ((sending (+ (get-internal-real-time)
                                   (* 10 internal-time-units-per-second))))
                   (loop :until (or (wire-flush wire)
                                    (not (wire-open wire))
                                    (> (get-internal-real-time) sending))
                         :do (sb-unix:unix-simple-poll (wire-fd wire) :output 100)))
                 (let ((deadline (+ (get-internal-real-time)
                                    (* patience internal-time-units-per-second))))
                   (loop
                     (when (> (get-internal-real-time) deadline) (return))
                     (when (pty:pty-wait (wire-fd wire) 100)
                       (unless (wire-receive wire) (return))
                       (loop for form = (wire-read-message wire)
                             while form
                             do (push form handle-message)
                                (when (funcall done form)
                                  (return-from request (nreverse handle-message))))))))
            (wire-close wire))
          (nreverse handle-message)))
    (error () nil)))

(defun server-agent-rows ()
  "Every pane in every session of this server, each row led by the server's
path."
  (let* ((path (server-socket-path))
         (said (request path '((:agents))
                      :done (lambda (form) (eq :agents (first form))))))
    (mapcar (lambda (row) (cons path row))
            (second (find :agents said :key #'first)))))

(defun caller-pane ()
  "The pane this is running in, as ATTY_PANE says, or nil outside one. It goes
last in what an agent verb sends, so a server from before it is not bothered by
it, and the pane that was acted on can say who by."
  (let ((pane (sb-ext:posix-getenv "ATTY_PANE")))
    (and pane (plusp (length pane)) pane)))

(defvar *successor* nil
  "Where the server said its next one would be, when it said it was
restarting: what a client becomes to match it.")
