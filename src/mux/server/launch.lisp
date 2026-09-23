;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun spawn-server (path &optional (program (executable-path)))
  "Another of this program, or of PROGRAM, told to serve at PATH, out of
reach of this terminal. Answers its pid."
  (let ((me program))
    (if me
        (pty:spawn-in-its-own-session me (list "-L" (server-name) "serve")
                                      :output (log-path))
        (let ((form (format nil "(atty:serve ~S ~S :name nil)" path (default-shell))))
          (pty:spawn-in-its-own-session
           (namestring sb-ext:*runtime-pathname*)
           (list "--no-userinit" "--disable-debugger"
                 "--eval" "(require :asdf)"
                 "--eval" "(asdf:load-system :atty)"
                 "--eval" form
                 "--quit")
           :output (log-path))))))

(defun start-server ()
  "Start this user's server, holding nothing yet: the client that started it
opens the first session. Answers its path."
  (let ((path (socket-path)))
    (spawn-server path)
    ;; a server with a lot to bring back from disk takes a while before it
    ;; takes its name; a minute is longer than any restore should be
    (loop repeat 6000
          until (server-alive-p path)
          do (sleep 0.01))
    path))

(defun start-fresh-server ()
  "Start this user's server with nothing in it: what it had on disk is put
aside first. It cannot be done to a server that is running."
  (let ((path (server-socket-path)))
    (when (server-alive-p path)
      (error "a server is running; atty kill-server or atty restart-server first"))
    (ignore-errors (delete-file path))
    (let ((aside (move-state-aside)))
      (when aside (format t "~&what was saved is in ~A~%" aside)))
    (start-server)))

(defun restart-server ()
  "Stop this user's server, keeping everything, and start it again from
whatever atty is now: how a new build takes over. Whoever is attached is told,
waits, and comes back. Answers how many sessions came back."
  (let ((path (server-socket-path)))
    (unless (server-alive-p path)
      (error "no server is running"))
    (let* ((said (request path (list (list :restart (executable-path)))
                        :done (lambda (f) (eq :restarting (first f)))))
           (by-itself (second (find :restarting said :key #'first))))
      (loop repeat 3000
            while (probe-file path)
            do (sleep 0.01)
            finally (when (probe-file path)
                      (error "the server did not stop; ~A says why" (log-path))))
      ;; a server that is a program starts its successor itself, and it is
      ;; this atty when this is one; one in a lisp cannot, and this does
      (if by-itself
          (loop repeat 6000 until (server-alive-p path) do (sleep 0.01))
          (start-server))
      (unless (server-alive-p path)
        (error "no server came back. ~A says why." (log-path)))
      (length (request-sessions)))))

(defun server-back-p (path)
  "Wait for the server at PATH to go and come back, as a restart does: first
until its name is gone, then until it answers again. Answers whether it did."
  (loop repeat 3000 while (probe-file path) do (sleep 0.01))
  (loop repeat 6000
        until (server-alive-p path 1)
        do (sleep 0.01)
        finally (return (server-alive-p path 1))))

(defvar *self-inode* nil
  "The inode this program's file was when this process began: another file
under the same name since is a newer build. The inode and not the date: a
file put there by tar has the date it had in the tarball.")

(defun inode-of (path)
  (ignore-errors (sb-posix:stat-ino (sb-posix:stat path))))

(defun exec-successor ()
  "After a restart whose server is another build than this, become that
build with the same arguments and the same terminal, so the client matches
the server it is coming back to. Answers only when there is nothing to
become: the same file, unchanged."
  (let ((next *successor*)
        (me (executable-path)))
    (when (and next me (executable-p next)
               (or (not (equal (ignore-errors (truename next))
                               (ignore-errors (truename me))))
                   (not (eql (inode-of me) *self-inode*))))
      (pty:become next (rest sb-ext:*posix-argv*)))))

(defun ensure-server ()
  "This user's server, started when it is not running."
  (let ((path (server-socket-path)))
    (unless (server-alive-p path)
      (ignore-errors (delete-file path))
      (start-server))
    (unless (server-alive-p path)
      (error "no server came up. ~A says why." (log-path)))
    path))

(defun stop-server (path)
  "Ask the server at PATH to go. Answers whether it went.

The programs in it go with it, which is what stopping it means, so it is never
something anything else does on your behalf."
  (and (server-alive-p path)
       (progn (request path '((:stop)) :patience 0)
              (loop repeat 300
                    while (server-alive-p path 1)
                    do (sleep 0.01))
              (not (server-alive-p path 1)))))

(defun serve-foreground (args)
  "atty serve [<name> [<command> [<rows> <cols>]]]: this user's server, in the
foreground. With a name it holds that session from the start."
  (let ((path (socket-path)))
    (when (server-alive-p path 1)
      (error "a server is already running at ~A; atty run <name> <command> adds ~
              a session to it" path))
    (load-user-init)
    (multiple-value-bind (rows cols)
        (terminal-size tty:+stdin+)
      (let ((said-rows (and (third args) (parse-integer (third args) :junk-allowed t)))
            (said-cols (and (fourth args) (parse-integer (fourth args) :junk-allowed t))))
        (let ((server (serve path (or (second args) (default-shell))
                             :name (first args)
                             :rows (or said-rows rows)
                             :cols (or said-cols cols)
                             :fresh *fresh-start*)))
          ;; asked to restart: the name is gone and the state is on disk, and
          ;; the next server is this program again, started by the one leaving
          ;; so that nobody has to stay around to do it
          (when (server-successor server)
            (spawn-server path (server-successor server))))))))

(defun kill-server ()
  "atty kill-server: stop every session, saying what was kept."
  (if (stop-server (server-socket-path))
      (let ((saved (saved-sessions)))
        (if saved
            (format t "~&stopped every session; ~D saved, atty brings ~:[them~;it~] back~%"
                    (length saved) (= 1 (length saved)))
            (format t "~&stopped every session~%")))
      (error "no server is running")))
