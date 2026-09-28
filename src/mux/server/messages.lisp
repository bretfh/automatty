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

(defun message-types ()
  "Every message type this server handles, in definition order."
  (mapcar #'first *message-handlers*))

(defun handle-message (server watcher form)
  "Dispatch FORM from WATCHER to its handler. Answers nil for a type nothing
here handles, or one that needs a session the watcher has not joined."
  (let ((entry (assoc (first form) *message-handlers*))
        (session (watcher-session watcher)))
    (when (and entry (or (not (second entry)) session))
      (funcall (third entry) server watcher session (rest form))
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

(define-message-handler :spawn (name command directory label &optional window)
  (multiple-value-bind (pane why)
      (handler-case (spawn-pane server name command directory label window)
        (error (e) (values nil (princ-to-string e))))
    (let* ((why (or why (and pane (pane-failed pane))))
           (pane (and (not why) pane))
           (session (and pane (session-named server name))))
      (send-message watcher (list :spawned name (and pane (pane-id pane))
                                  (and pane (pane-address-of session pane))
                                  why)))))

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

(define-message-handler :new (&optional name command directory)
  (join-session server watcher
                (add-session server (or command (server-command server))
                             :name (or name (unused-session-name server))
                             :rows (watcher-rows watcher)
                             :cols (watcher-cols watcher)
                             :directory (or directory
                                            (and session
                                                 (pane-directory
                                                  (session-focus session)))))))

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

(define-message-handler :kill-session (name)
  (let ((it (and name (session-named server name))))
    (send-message watcher (list :killed name (and it t)))
    (when it
      (end-session server it :stopped)
      (when (server-saving server) (save-tree server)))))

(define-message-handler :save ()
  (when (server-saving server) (save-all server))
  (send-message watcher (list :saved (and (server-saving server) t))))

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

(define-message-handler :reload-init ()
  (load-user-init)
  (send-message watcher (list* :say (init-load-note)))
  (dolist (w (all-watchers server))
    (when (wire-open (watcher-wire w)) (send-config w))))

(define-message-handler :command (name)
  (run-init-command watcher name))

(define-message-handler :settings ()
  (send-message watcher (list :settings (encode-settings))))

(define-message-handler :agents ()
  (send-message watcher (list :agents (agent-rows server session))))

(define-message-handler :who (tty)
  (setf (watcher-tty watcher) (and (stringp tty) tty))
  ;; a tty said after attaching is a change to what is listed; one
  ;; said before is told with the attaching
  (when (watcher-interactive watcher) (send-client-list server)))

(define-message-handler :clients ()
  (send-message watcher (list :clients (encode-clients server (now-ms)))))

(define-message-handler :detach-client (id)
  (let ((it (find id (all-watchers server) :key #'watcher-id)))
    (when (and it (watcher-interactive it))
      (drop-watcher server it :detached)
      (send-client-list server))))

(define-message-handler :follow (id)
  (follow server watcher id))

(define-message-handler :agent-signal (name id state &optional caller-pane)
  (let ((pane (find-pane server name id)))
    (when (and pane (member state '(:working :blocked :idle)))
      (pane-push-log pane (now-ms) (actor-of watcher caller-pane) :signal state)
      (agent:agent-hear (pane-agent pane) state)
      (session-observe (session-named server name) (monotonic-ns)))))

(define-message-handler :name-pane (name id label)
  (let ((pane (find-pane server name id)))
    (when pane
      (setf (pane-label pane) (and (stringp label)
                                   (plusp (length (string-trim " " label)))
                                   (string-trim " " label))
            (pane-touched pane) (now-ms))
      (dolist (w (session-watchers (session-named server name)))
        (setf (watcher-behind w) t)))
    (send-message watcher (list :named name id (and pane t)))))

(define-message-handler :naming ()
  ;; the client does not know which pane has the focus, only the server
  ;; does; so a rename is asked for here and the prompt is the client's
  (let ((pane (and session (session-focus session))))
    (when pane
      (send-message watcher (list :name-it (session-name session) (pane-id pane)
                          (pane-label pane) (pane-named pane)
                          (pane-address-of session pane))))))

(define-message-handler :name-session (name new)
  (let ((it (session-named server name)))
    (if it
        (session-rename server watcher it new)
        (send-message watcher (list :session-named name new :gone)))))

(define-message-handler :naming-window ()
  (when session
    (send-message watcher (list :name-window-of (session-name session)
                        (window-number session (session-window session))
                        (window-label (session-window session))))))

(define-message-handler :agent-read (name id n &optional plain)
  (let ((pane (find-pane server name id)))
    (send-message watcher (list :agent-lines name id
                        (and pane (if plain
                                      (plain-lines (pane-term pane) n)
                                      (agent:last-lines (pane-term pane) n)))))))

(define-message-handler :agent-snapshot (name id)
  (let ((pane (find-pane server name id)))
    (send-message watcher (list :agent-snapshotted name id
                        (and pane (agent:snapshot (pane-term pane)))))))

(define-message-handler :agent-keys (name id text &optional caller-pane)
  (let ((pane (find-pane server name id)))
    (when pane
      (pane-push-log pane (now-ms) (actor-of watcher caller-pane) :say (summarize-text text))
      (pane-write pane text))))

(define-message-handler :agent-prompt (name id text &optional caller-pane)
  (let ((pane (find-pane server name id)))
    (let ((said (if pane
                      (prompt-pane server pane text (actor-of watcher caller-pane))
                      :gone)))
      (send-message watcher (list :agent-prompted name id said
                          (and (eq said t) (agent:agent-reader (pane-agent pane))
                               (agent:agent-turn (pane-agent pane))))))))

(define-message-handler :prompt-when-idle (name id text &optional caller-pane)
  (let ((pane (find-pane server name id)))
    (send-message watcher (list :agent-prompted name id
                        (if pane
                            (prompt-when-idle server pane text (actor-of watcher caller-pane))
                            :gone)))))

(define-message-handler :agent-observe (name id)
  (let* ((pane (find-pane server name id))
         (agent (and pane (pane-agent pane)))
         (reader (and agent (agent:agent-reader agent)))
         (seen (and reader (agent:observe reader (pane-term pane)))))
    (send-message watcher (list :agent-observed name id
                        (and agent (list :kind (agent:agent-kind agent)
                                         :version (agent:agent-version agent)
                                         :verified (agent:agent-verified agent)
                                         :state (agent:agent-state agent)
                                         :offered (and seen (agent:offered reader seen))
                                         :observation seen))))))

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

(define-message-handler :agent-trace (name id)
  (let ((pane (find-pane server name id)))
    (send-message watcher (list :agent-traced name id
                        (and pane (reverse (agent:agent-trace (pane-agent pane))))))))

(define-message-handler :agent-explain (name id)
  (let ((pane (find-pane server name id)))
    (if pane
        (multiple-value-bind (seen rows)
            (agent:agent-explain (pane-agent pane) (pane-term pane))
          (send-message watcher (list :agent-explained name id
                              (agent:agent-state (pane-agent pane)) seen rows)))
        (send-message watcher (list :agent-explained name id :gone nil nil)))))

(define-message-handler :panes ()
  (send-message watcher (list :panes (pane-rows server (now-ms)))))

(define-message-handler :watch-panes (wanted)
  (setf (watcher-watch-panes watcher) (and wanted t))
  (clrhash (watcher-rows-sent watcher))
  (when wanted (send-pane-rows server watcher (now-ms))))

(define-message-handler :watch-screens (n)
  (let ((n n))
    (setf (watcher-screen-rows watcher)
          (and (integerp n) (plusp n) (min n +max-pane-size+)))
    (clrhash (watcher-screens-sent watcher))))

(define-message-handler :pane-screen (name id n)
  (let ((pane (find-pane server name id)))
    (send-message watcher (list* :pane-screen name id
                         (and pane (encode-pane-screen
                                    pane (max 1 (min n +max-pane-size+))))))))

(define-message-handler :pane-history (name id)
  (let ((pane (find-pane server name id)))
    (send-message watcher (list :pane-history name id
                        (and pane (encode-history (pane-agent pane) (now-ms)))))))

(define-message-handler :pane-log (name id n)
  (let ((pane (find-pane server name id))
        (now (now-ms)))
    (send-message watcher (list :pane-log name id
                        (and pane
                             (mapcar (lambda (e) (encode-log-entry e now))
                                     (subseq (pane-log pane)
                                             0 (min (max 0 n)
                                                    (length (pane-log pane))))))))))

(define-message-handler :answer (name id n &optional caller-pane)
  (answer-pane server watcher name id n caller-pane))

(define-message-handler :focus-pane (name id)
  (focus-pane server watcher name id))

(define-message-handler :go-to-blocked ()
  (focus-oldest-blocked server watcher))

(define-message-handler :lately (&optional n)
  (send-message watcher (list :lately (recent-answers server (or n 5) (now-ms)))))

(define-message-handler :pulses ()
  (send-message watcher (list :pulses (encode-pulses server))))

(define-message-handler :events (&optional n)
  (send-message watcher (list :events (encode-events server (or n 64) (now-ms)))))

(define-message-handler :pane-about (name id)
  (let ((pane (find-pane server name id)))
    (send-message watcher (list :pane-about name id (and pane (pane-info pane))))))

(define-message-handler :close-pane (name id)
  (let* ((session (session-named server name))
         (pane (and session (find id (session-panes session) :key #'pane-id))))
    (when pane (session-close-pane session pane))))

(define-message-handler :split-in (name &optional (way :across))
  (let ((session (session-named server name)))
    (when session (session-split session way))))

(define-message-handler :new-window (name &optional command directory)
  (let ((session (session-named server name)))
    (when session
      (let ((w (session-add-window session command directory)))
        (send-message watcher (list :window name (window-number session w)))))))

(define-message-handler :go-window (name n)
  (let ((session (session-named server name)))
    (when session
      (unless (eq session (watcher-session watcher))
        (join-session server watcher session))
      (session-select-window session n))))

(define-message-handler :next-window (name)
  (let ((session (session-named server name)))
    (when session (session-cycle-window session 1))))

(define-message-handler :previous-window (name)
  (let ((session (session-named server name)))
    (when session (session-cycle-window session -1))))

(define-message-handler :close-window (name &optional n)
  (let* ((session (session-named server name))
         (window (and session (if n (session-nth-window session n) (session-window session)))))
    (when window (session-close-window session window))))

(define-message-handler :name-window (name n label)
  (let* ((session (session-named server name))
         (window (and session (if n (session-nth-window session n) (session-window session)))))
    (when window (session-rename-window session window label))))

(define-message-handler :layouts (name)
  (let ((session (session-named server name)))
    (send-message watcher
          (list :layouts name
                (and session
                     (loop :for w :in (session-windows session)
                           :for n :from 1
                           :collect (list n (window-label w) (encode-layout (window-layout w))
                                          (and (window-focus w) (pane-id (window-focus w)))
                                          (eq w (session-window session)))))))))

(define-message-handler :zoom (&optional name id)
  (session-zoom-pane server watcher name id))

(define-message-handler :find (name id query way)
  (search-pane server watcher name id query way))

(define-message-handler :select (what)
  (select-pane-rows server watcher what))

(define-message-handler :pane-read (name id)
  (read-pane server watcher name id))

(define-message-handler :detach ()
  (drop-watcher server watcher))

(define-message-handler :stop ()
  (setf (server-running server) nil))

(define-message-handler (:keys :session) (text)
  (let ((pane (session-focus session))
        (said text))
    (setf (watcher-typed-at watcher) (now-ms))
    (when (watcher-following watcher) (stop-following (session-server session) watcher))
    (pane-push-log pane (now-ms) (actor-of watcher) :keys (length said))
    ;; somebody typing wants to see what they are typing at
    (pane-scroll-to pane 0)
    (pane-write pane said)))

(define-message-handler (:resize :session) (rows cols)
  (setf (watcher-rows watcher) (max 1 (min +max-pane-size+ rows))
        (watcher-cols watcher) (max 1 (min +max-pane-size+ cols)))
  (session-fit session)
  (send-client-list (session-server session)))

(define-message-handler (:bar :session) (state)
  ;; the bar is the session's, not the client's: whoever asked, everybody
  ;; attached is looking at the same one
  (setf (session-bar-p session) (if (eq state :toggle)
                                   (not (session-bar-p session))
                                   (and state t)))
  (session-reset-shadows session)
  (dolist (w (session-watchers session))
    (send-message w (list :barp (session-bar-p session)))))

(define-message-handler (:split :session) (&optional way)
  (session-split session (or way :across)))

(define-message-handler (:focus :session) ()
  (session-focus-next session))

(define-message-handler (:close :session) ()
  (session-close-pane session (session-focus session)))

(define-message-handler (:only :session) ()
  (session-delete-other-panes session (session-focus session)))

(define-message-handler (:mouse-at :session) (x y)
  (handle-click session watcher x y))

(define-message-handler (:pointer :session) (what button x y &optional mods)
  (handle-pointer session watcher what button x y mods))

(define-message-handler (:wheel :session) (way &optional x y mods)
  (handle-wheel session way x y mods))

(define-message-handler (:scroll :session) (amount &optional x y)
  (scroll-pane (or (and x y (pane-at session x y)) (session-focus session))
                   amount))

(define-message-handler (:reading :session) (readingp)
  ;; a client reading a pane back says so, so the bar can say it for everybody
  (setf (session-readers session)
        (if readingp
            (adjoin watcher (session-readers session))
            (remove watcher (session-readers session))))
  (dolist (w (session-watchers session)) (setf (watcher-behind w) t)))

(define-message-handler (:scrollbars :session) (state)
  (setf (session-scrollbars-p session) (if (eq state :toggle)
                                          (not (session-scrollbars-p session))
                                          (and state t)))
  (dolist (w (session-watchers session)) (setf (watcher-behind w) t)))


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
