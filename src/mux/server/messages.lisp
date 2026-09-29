;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defvar *message-handlers* '()
  "Every message a client can send, in definition order: (TYPE SESSION-REQUIRED-P HANDLER).")

(defmacro define-message-handler (type lambda-list &body body)
  "Handle a message of TYPE from a client. LAMBDA-LIST destructures the message
after its type; SERVER, WATCHER and SESSION are bound in BODY. TYPE written as
(TYPE :session) is only handled once the watcher has joined a session, and
SESSION is non-nil in it."
  (destructuring-bind (type &rest options) (if (listp type) type (list type))
    `(setf *message-handlers*
           (append (remove ,type *message-handlers* :key #'first)
                   (list (list ,type ,(and (member :session options) t)
                               (lambda (server watcher session args)
                                 (declare (ignorable server watcher session))
                                 (destructuring-bind ,lambda-list args ,@body))))))))

(defun handle-message (server watcher form)
  "Dispatch FORM from WATCHER to its handler. Answers nil for a type nothing
here handles, or one that needs a session the watcher has not joined."
  (let ((entry (assoc (first form) *message-handlers*))
        (session (watcher-session watcher)))
    (when (and entry (or (not (second entry)) session))
      (let ((*server* server))
        (funcall (third entry) server watcher session (rest form)))
      t)))

(define-message-handler :want (name)
  (setf (watcher-wanted-session watcher) name))

(define-message-handler :attach (rows cols takes)
  (watcher-resize watcher rows cols takes)
  (let ((want (session-named server (watcher-wanted-session watcher))))
    (if want
        (join-session server watcher want)
        (drop-watcher server watcher :no-such-session))))

(define-message-handler :open (name command directory rows cols takes &optional label)
  ;; join NAME, making it first when it is not there. One message, so the
  ;; asking and the making are one step here: two clients opening the same
  ;; new name get one session, not two, and not an error
  (watcher-resize watcher rows cols takes)
  (let ((session (or (and name (session-named server name))
                     (handler-case
                         (let ((made (add-session server (or command (server-command server))
                                                  :name (or name (unused-session-name server))
                                                  :rows (watcher-rows watcher)
                                                  :cols (watcher-cols watcher)
                                                  :directory directory)))
                           (when label
                             (setf (pane-label (session-focus made)) label))
                           made)
                       (error (e)
                         (drop-watcher server watcher (princ-to-string e))
                         nil)))))
    (when session
      (join-session server watcher session))))

(define-message-handler :since-prompt (name id)
  (let ((pane (find-pane server name id))
        (now (now-ms)))
    (send-message watcher
          (list :since-prompt name id
                (and pane
                     (let ((prompted (find :prompt (pane-log pane) :key #'third)))
                       (list (agent:agent-state (pane-agent pane))
                             (and prompted (- now (first prompted)))
                             (encode-history (pane-agent pane) now))))))))

(define-message-handler :go (name)
  (let ((want (session-named server name)))
    (when want (join-session server watcher want))))

(define-message-handler :sessions ()
  (send-message watcher
        (list :these
              (mapcar (lambda (s)
                        (list (session-name s) (session-rows s)
                              (session-cols s)
                              (length (session-panes s))
                              (count-if #'watcher-interactive (session-watchers s))
                              (count :blocked (session-panes s)
                                     :key (lambda (p) (agent:agent-state
                                                       (pane-agent p))))
                              (encode-windows s)))
                      (server-sessions server)))))

(define-message-handler :restart (&optional successor)
  ;; everybody attached is told it is a restart, so they wait for the
  ;; server to be back rather than taking it for gone
  ;; a server that is a program starts its successor itself on the way
  ;; out, so whoever asked can go away, or be interrupted, without the
  ;; server being lost between the old one and the new; one loaded into
  ;; a lisp cannot, and the asker starts it. The successor is the atty
  ;; that asked when it says which it is: under guix or nix this
  ;; program's own path is the old store item, and what is on PATH is
  ;; the new one
  (let ((asked successor))
    (setf (server-successor server)
          (or (and (executable-p asked) asked) (executable-path))))
  (dolist (session (server-sessions server))
    (dolist (w (session-watchers session))
      (send-message w (list :bye :restarting (server-successor server)))))
  (send-message watcher (list :restarting (server-successor server)))
  (setf (server-running server) nil))

(define-message-handler :knock ()
  (send-message watcher (list :here (server-path server) :one-server *version*)))

(define-message-handler :settings ()
  (send-message watcher (list :settings (encode-settings))))

(define-message-handler :agents ()
  (send-message watcher (list :agents (agent-rows server session))))

(define-message-handler :who (tty)
  (setf (watcher-tty watcher) (and (stringp tty) tty))
  ;; a tty said after attaching is a change to what is listed; one
  ;; said before is told with the attaching
  (when (watcher-interactive watcher) (send-client-list server)))

(define-message-handler :agent-prompt (name id text &optional caller-pane)
  (let ((pane (find-pane server name id)))
    (let ((said (if pane
                      (prompt-pane server pane text (actor-of watcher caller-pane))
                      :gone)))
      (send-message watcher (list :agent-prompted name id said
                          (and (eq said t) (agent:agent-reader (pane-agent pane))
                               (agent:agent-turn (pane-agent pane))))))))

(define-message-handler :agent-act (name id action &optional argument)
  (act-on-pane server watcher name id action argument))

(define-message-handler :readers-load (texts)
  (multiple-value-bind (loaded refused) (agent:register-reader-texts texts)
    (dolist (session (server-sessions server))
      (dolist (pane (session-panes session))
        (agent:agent-become (pane-agent pane) :title (pane-named pane)
                                              :command (pane-command pane)
                                              :programs (pane-programs pane)
                                              :paths (pane-paths pane))))
    (send-message watcher (list :readers-loaded loaded refused))))

(define-message-handler :panes ()
  (send-message watcher (list :panes (pane-rows server (now-ms)))))

(defun pane-by-address (server address)
  "The pane ADDRESS names, as ATTY_PANE says it, and its session."
  (loop :for session :in (server-sessions server)
        :for pane := (find address (session-panes session)
                           :key (lambda (p) (pane-address-of session p)) :test #'equal)
        :when pane :return (values pane session)))

(defvar *caller* nil
  "The pane a command run from a command line was run in, as ATTY_PANE says.")

(defvar *caller-directory* nil
  "Where the command line a command was run from was.")

(define-message-handler :run (name arguments &optional here directory)
  (let ((does (gethash name *commands*))
        (out (make-string-output-stream))
        (status t))
    (unless (watcher-session watcher)
      (setf (watcher-session watcher)
            (or (nth-value 1 (and here (pane-by-address server here)))
                (first (server-sessions server)))))
    (if (null does)
        (send-message watcher (list :ran nil :no-command))
        (progn
          (let ((*standard-output* out)
                (*client* watcher)
                (*caller* here)
                (*caller-directory* directory))
            (handler-case (apply does arguments)
              (refused (e)
                (setf status (refused-code e))
                (when (plusp (length (refused-text e)))
                  (format out "~&atty: ~A~%" (refused-text e))))
              (error (e)
                (setf status 1)
                (format out "~&atty: ~A~%" e))))
          (send-message watcher (list :ran (get-output-stream-string out) status))))))

(define-message-handler :detach ()
  (drop-watcher server watcher))

(define-message-handler :stop ()
  (setf (server-running server) nil))

(define-message-handler (:keys :session) (text)
  (handle-input watcher text))

(define-message-handler (:resize :session) (rows cols)
  (setf (watcher-rows watcher) (max 1 (min +max-pane-size+ rows))
        (watcher-cols watcher) (max 1 (min +max-pane-size+ cols)))
  (session-fit session)
  (send-client-list (session-server session)))

(defun read-messages (server watcher)
  "Read what the client said and act on it.

A message this server cannot make sense of is passed over.
A client is not always the same build as the server it reached: it is the one
that was just started, and the server has been running since whenever. One
message it does not know must not take down the sessions everybody else is
looking at."
  (let ((wire (watcher-wire watcher)))
    (if (null (wire-receive wire))
        (drop-watcher server watcher)
        (loop for form = (handler-case (wire-read-message wire)
                           (error () (drop-watcher server watcher) nil))
              while form
              do (handler-case (handle-message server watcher form)
                   (error (e)
                     (format *error-output* "~&atty: ~S: ~A~%"
                             (and (consp form) (first form)) e)
                     (finish-output *error-output*)))
              while (wire-open wire)))))

(defun accept-watcher (server)
  "Somebody has opened the socket. Which session they want is something they
have yet to say, so until they do they are the server's rather than any
session's."
  (let ((socket (handler-case (sb-bsd-sockets:socket-accept (server-socket server))
                  (error () nil))))
    (when socket
      (let ((watcher (%make-watcher
                      :socket socket
                      :wire (make-wire (sb-bsd-sockets:socket-file-descriptor
                                        socket)
                                       socket))))
        (push watcher (server-pending-watchers server))
        watcher))))
