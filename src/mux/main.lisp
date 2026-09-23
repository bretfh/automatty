;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun mux-dir ()
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

(defvar *fresh* nil
  "Whether --fresh was said: the server starts with nothing, what it had on
disk put aside.")

(defvar *server-name* nil
  "Which server, when -L said. One server holds every session, the way tmux's
does; a second one is for somebody who asks for it by name.")

(defun a-name (said what)
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
  (namestring (merge-pathnames (a-name name "a server") (mux-dir))))

(defun where-the-server-is ()
  "The server this invocation talks to: the one -L or ATTY_SERVER names, else
the one the pane it is running in belongs to, else the user's own."
  (let ((inside (sb-ext:posix-getenv "ATTY_SOCKET")))
    (if (and (null *server-name*)
             (null (sb-ext:posix-getenv "ATTY_SERVER"))
             inside (plusp (length inside)))
        inside
        (socket-path))))

(defun log-path (&optional (name (server-name)))
  (namestring (merge-pathnames (format nil "~A.log" (a-name name "a server"))
                               (mux-dir))))

(defun knocked (path &optional (patience 3))
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
                                 (unless (wire-fill wire) (return))
                                 (loop for form = (wire-take wire)
                                       while form
                                       do (when (and (consp form)
                                                     (eq :here (first form)))
                                            (setf here form))))))
                 (wire-close wire))
               here))
         (error () nil))))

(defun answering-p (path &optional (patience 3))
  (and (knocked path patience) t))

(defun other-servers ()
  "Every other socket here that answers, as (name path legacyp). A legacy one is
a server from before one server held every session: it holds the one session its
name is, and says so by not saying it is the other kind."
  (loop :for path :in (mapcar #'namestring (directory (merge-pathnames "*" (mux-dir))))
        :for name := (file-namestring path)
        ;; by name, not by path: the directory listing answers the path with
        ;; every link resolved (/private/tmp on a mac), which is not how it
        ;; was spelt when it was made
        :for here := (and (not (search ".log" name))
                          (string/= name (file-namestring (where-the-server-is)))
                          (knocked path 1))
        :when here
          :collect (list name path (not (member :one-server here)))))

(defun asked (path forms &key (patience 3) (done (constantly t)))
  "Say FORMS to the server at PATH and gather what it says back until DONE
likes a form or PATIENCE seconds pass. What was said is all sent before the
patience starts: a long prompt is more than a socket takes in one write, and
cutting it off halfway would leave the server holding half a message."
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (sb-bsd-sockets:socket-connect socket path)
        (let ((wire (make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket))
              (heard nil))
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
                       (unless (wire-fill wire) (return))
                       (loop for form = (wire-take wire)
                             while form
                             do (push form heard)
                                (when (funcall done form)
                                  (return-from asked (nreverse heard))))))))
            (wire-close wire))
          (nreverse heard)))
    (error () nil)))

(defun agents-here ()
  "Every pane in every session of this server, each row led by the server's
path."
  (let* ((path (where-the-server-is))
         (said (asked path '((:agents))
                      :done (lambda (form) (eq :agents (first form))))))
    (mapcar (lambda (row) (cons path row))
            (second (find :agents said :key #'first)))))

(defun pane-address (said)
  "SAID taken apart: the session, and either the window and the pane's number
in it (todo:1.2) or, from before there were windows, the pane's id (todo:4).
Answers (values session id window n): id when it was an id, window and n when
it was those."
  (let* ((colon (position #\: said :from-end t))
         (rest (and colon (subseq said (1+ colon))))
         (dot (and rest (position #\. rest))))
    (unless (and colon (plusp (length rest))
                 (parse-integer rest :junk-allowed t :end (or dot (length rest)))
                 (or (null dot) (parse-integer rest :start (1+ dot) :junk-allowed t)))
      (error "~S is not a pane: a pane is <session>:<window>.<pane>, as ATTY_PANE says" said))
    (if dot
        (values (subseq said 0 colon) nil
                (parse-integer rest :end dot) (parse-integer rest :start (1+ dot)))
        (values (subseq said 0 colon) (parse-integer rest) nil nil))))


(defun a-state (said)
  (let ((state (intern (string-upcase said) :keyword)))
    (unless (member state '(:working :blocked :idle))
      (error "~A is not something a program can be doing: working, blocked or idle" said))
    state))

(defun agent-state-in (form id)
  (case (first form)
    (:agent-turn (and (eql (third form) id) (eq :ended (fifth form)) :idle))
    (:agent (and (eql (third form) id) (fifth form)))
    (:agents (let ((row (find id (second form) :key #'second)))
               (and row (fourth row))))))

(defun pane-found (address)
  "The pane ADDRESS names: its server's path, its session and its id, and its
kind. Either form of address finds it."
  (unless address (error "which pane? <session>:<window>.<pane>, as atty agent list says"))
  (multiple-value-bind (session id window n) (pane-address address)
    (let ((row (find-if (lambda (r)
                          (and (equal session (getf r :session))
                               (if id
                                   (eql id (getf r :id))
                                   (and (eql window (getf r :window))
                                        (eql (1- n) (getf r :at))))))
                        (the-panes))))
      (unless row (error "there is no pane called ~A running" address))
      (values (where-the-server-is) session (getf row :id) (getf row :kind)))))

(defun caller ()
  "The pane this is running in, as ATTY_PANE says, or nil outside one. It goes
last in what an agent verb sends, so a server from before it is not bothered by
it, and the pane that was acted on can say who by."
  (let ((pane (sb-ext:posix-getenv "ATTY_PANE")))
    (and pane (plusp (length pane)) pane)))

(defun unescaped (said)
  (with-output-to-string (out)
    (loop :with i := 0
          :while (< i (length said))
          :do (let ((c (char said i)))
                (if (and (char= c #\\) (< (1+ i) (length said)))
                    (let ((next (char said (1+ i))))
                      (write-char (case next
                                    (#\r #\Return) (#\n #\Newline) (#\t #\Tab)
                                    (#\e (code-char 27)) (t next))
                                  out)
                      (incf i 2))
                    (progn (write-char c out) (incf i)))))))

;;; The agent verbs: everything the panes' UI does, from a command line, so a
;;; program in a pane can do for another what a person does at the keyboard.

(defun flag-p (args name)
  (and (member name args :test #'string=) t))

(defun option (args name)
  "The word after NAME in ARGS, or nil."
  (second (member name args :test #'string=)))

(defun words (args &rest options)
  "ARGS without its flags, and without each of OPTIONS and the word after it."
  (loop :with skip := nil
        :for a :in args
        :if skip :do (setf skip nil)
        :else :if (member a options :test #'string=) :do (setf skip t)
        :else :unless (and (> (length a) 2) (string= "--" a :end2 2)) :collect a))

(defun the-panes ()
  "Every pane of this server as the plist it tells a client."
  (second (find :panes (asked (where-the-server-is) '((:panes))
                              :done (lambda (f) (eq :panes (first f))))
                :key #'first)))

(defun json (thing out)
  "THING as JSON: a plist is an object, a list an array, a keyword a string."
  (cond ((null thing) (write-string "null" out))
        ((eq thing t) (write-string "true" out))
        ((numberp thing) (format out "~D" thing))
        ((keywordp thing) (json (string-downcase (symbol-name thing)) out))
        ((stringp thing)
         (write-char #\" out)
         (loop :for c :across thing
               :do (case c
                     (#\" (write-string "\\\"" out))
                     (#\\ (write-string "\\\\" out))
                     (#\Newline (write-string "\\n" out))
                     (#\Return (write-string "\\r" out))
                     (#\Tab (write-string "\\t" out))
                     (t (if (< (char-code c) 32)
                            (format out "\\u~4,'0X" (char-code c))
                            (write-char c out)))))
         (write-char #\" out))
        ((and (consp thing) (keywordp (first thing)) (evenp (length thing)))
         (write-char #\{ out)
         (loop :for (key value) :on thing :by #'cddr
               :for first := t :then nil
               :do (unless first (write-char #\, out))
                   (json (substitute #\_ #\- (string-downcase (symbol-name key))) out)
                   (write-char #\: out)
                   (json value out))
         (write-char #\} out))
        ((consp thing)
         (write-char #\[ out)
         (loop :for it :in thing
               :for first := t :then nil
               :do (unless first (write-char #\, out))
                   (json it out))
         (write-char #\] out))
        (t (json (princ-to-string thing) out))))

(defun list-agents (args)
  "atty agent list [--blocked] [--session <name>] [--json]"
  (let* ((rows (the-panes))
         (rows (if (flag-p args "--blocked")
                   (remove-if-not (lambda (r) (eq :blocked (getf r :state))) rows)
                   rows))
         (rows (let ((only (option args "--session")))
                 (if only (remove-if-not (lambda (r) (equal only (getf r :session))) rows) rows))))
    (cond
      ((and (flag-p args "--json") (null rows))
       ;; an empty array, not null: a script iterating it wants nothing to do,
       ;; not something to check for first
       (format t "[]~%"))
      ((flag-p args "--json")
       (json (mapcar (lambda (r)
                       (list :address (row-address r)
                             :session (getf r :session) :id (getf r :id)
                             :window (getf r :window) :window-name (getf r :window-name)
                             :name (getf r :label) :says (getf r :says) :kind (getf r :kind)
                             :state (getf r :state) :for-ms (getf r :for)
                             :asks (let ((asks (getf r :asks)))
                                     (and asks
                                          (list :subject (getf asks :subject)
                                                :question (getf asks :question)
                                                :detail (getf asks :detail)
                                                :options (mapcar (lambda (o)
                                                                   (list :n (first o)
                                                                         :text (second o)))
                                                                 (getf asks :options)))))
                             :doing (getf r :doing)
                             :driven-by (getf r :driven-by)
                             :queued (getf r :queued)))
                     rows)
             *standard-output*)
       (terpri))
      ((null rows) (format t "~&nothing is running~%"))
      (t
       (let* ((addresses (mapcar #'row-address rows))
              (names (mapcar (lambda (r) (or (getf r :label) "-")) rows))
              (wa (reduce #'max addresses :key #'length))
              (wn (reduce #'max names :key #'length))
              (wk (reduce #'max rows :key (lambda (r) (length (getf r :kind))))))
         (loop :for r :in rows
               :for address :in addresses
               :for name :in names
               :do (format t "~&~vA  ~vA  ~vA  ~(~7A~)  ~4A~@[  ~A~]~%"
                           wa address wn name wk (getf r :kind)
                           (if (getf r :known) (getf r :state) "-")
                           (if (getf r :known) (duration (getf r :for)) "")
                           (getf (getf r :asks) :subject))))))))

(defun spawn-agent (args)
  "atty agent spawn <session> [--name <name>] [--cwd <dir>] [--window <n> | --new-window] -- <command>"
  (let* ((dashes (member "--" args :test #'string=))
         (before (ldiff args dashes))
         (session (first (words before "--name" "--cwd" "--window")))
         (command (format nil "~{~A~^ ~}" (or (rest dashes) (rest (words before "--name" "--cwd" "--window")))))
         (window (cond ((flag-p before "--new-window") :new)
                       ((option before "--window")
                        (or (parse-integer (option before "--window") :junk-allowed t)
                            (error "--window wants a number, as atty agent list says them"))))))
    (unless session
      (error "atty agent spawn <session> [--name <name>] [--cwd <dir>] [--window <n> | --new-window] -- <command>"))
    (let* ((path (the-server))
           (said (asked path (list (list :spawn session
                                         (if (plusp (length command)) command (a-shell))
                                         (or (option before "--cwd") (cwd))
                                         (option before "--name")
                                         window))
                        :done (lambda (f) (eq :spawned (first f)))))
           (spawned (find :spawned said :key #'first))
           (id (third spawned)))
      (unless id (error "no pane was made in ~A" session))
      (format t "~&~A~%" (or (fourth spawned) (format nil "~A:~D" session id))))))

(defun name-agent (args)
  "atty agent name <pane> <name>, or from inside a pane atty agent name <name>"
  (multiple-value-bind (address label)
      (if (third args)
          (values (second args) (third args))
          (values (or (caller) (error "atty agent name <session>:<pane> <name>")) (second args)))
    (multiple-value-bind (path session id) (pane-found address)
      (asked path (list (list :name-pane session id (or label "")))
             :done (lambda (f) (eq :named (first f))))
      (format t "~&~A:~D is ~A~%" session id (if (plusp (length (or label ""))) label "unnamed")))))

(defun answer-agent (args)
  "atty agent answer <pane> <n>: type answer N to what the pane is asking. Exits
3, having typed nothing, when it is not asking or has no such answer."
  (let ((n (and (third args) (parse-integer (third args) :junk-allowed t))))
    (unless n (error "atty agent answer <session>:<pane> <n>"))
    (multiple-value-bind (path session id) (pane-found (second args))
      (let ((outcome (fifth (find :answered
                                  (asked path (list (list :answer session id n (caller)))
                                         :done (lambda (f) (eq :answered (first f))))
                                  :key #'first))))
        (case outcome
          ((t) (format t "~&answered ~D~%" n))
          (:not-blocked
           (format *error-output* "~&atty: ~A is not blocked; nothing was typed.~%" (second args))
           (sb-ext:quit :unix-status 3))
          (:no-such-option
           (format *error-output* "~&atty: ~A has no answer ~D; nothing was typed.~%"
                   (second args) n)
           (sb-ext:quit :unix-status 3))
          (t (error "~A is gone" (second args))))))))

(defun who-said-here (who)
  (case (first who)
    (:pane (second who))
    (:client (or (third who) (format nil "client ~D" (second who))))
    (:atty "atty")
    (t "the command line")))

(defun log-of-agent (args)
  "atty agent log <pane> [<n>]: who typed into it, newest first."
  (multiple-value-bind (path session id) (pane-found (second args))
    (let* ((n (or (and (third args) (parse-integer (third args) :junk-allowed t)) 20))
           (entries (fourth (find :pane-log
                                  (asked path (list (list :pane-log session id n))
                                         :done (lambda (f) (eq :pane-log (first f))))
                                  :key #'first))))
      (if entries
          (loop :for (nil who verb summary outcome clock) :in entries
                :do (format t "~&~A  ~12A ~(~7A~) ~A~:[~;  refused~]~%"
                            (wall-clock clock) (who-said-here who) verb
                            (if (eq verb :keys) (format nil "~D bytes" summary) (or summary ""))
                            (eq outcome :refused)))
          (format t "~&nobody has typed into ~A~%" (second args))))))

(defun done-since-prompt-p (said wanted)
  "Whether what :SINCE-PROMPT SAID shows the pane in a WANTED state it came to
after the last prompt: not an idle left over from before it, nor the idle the
paste itself makes before the program has started on it."
  (destructuring-bind (state prompted history) said
    (let ((since (first (first history))))
      (and prompted since
           (member state wanted)
           (< since prompted)
           (or (not (eq state :idle))
               ;; at or after the prompt: the server holds a pane it has just
               ;; prompted as working from that moment, so the idle the paste
               ;; makes is never published, and that entry has the prompt's time
               (some (lambda (h) (and (<= (first h) prompted) (> (first h) since)
                                      (member (second h) '(:working :blocked))))
                     history))))))

(defun wait-after-prompt (path session id wanted)
  (loop :with deadline := (+ (get-internal-real-time) (* 3600 internal-time-units-per-second))
        :for said := (fourth (find :since-prompt
                                   (asked path (list (list :since-prompt session id))
                                          :done (lambda (f) (eq :since-prompt (first f))))
                                   :key #'first))
        :do (cond ((null said) (error "~A:~D is gone" session id))
                  ((null (second said)) (error "nothing was ever prompted into ~A:~D" session id))
                  ((done-since-prompt-p said wanted)
                   (format t "~&~(~A~)~%" (first said))
                   (return))
                  ((> (get-internal-real-time) deadline) (sb-ext:quit :unix-status 1)))
            (sleep 0.3)))

(defun agent-verbs (args)
  (let ((what (first args)))
    (cond
      ((or (null what) (string= what "list")) (list-agents (rest args)))
      ((string= what "spawn") (spawn-agent (rest args)))
      ((string= what "name") (name-agent args))
      ((string= what "answer") (answer-agent args))
      ((string= what "log") (log-of-agent args))
      ((string= what "read")
       (let ((words (words args)))
         (multiple-value-bind (path session id) (pane-found (second words))
           (let* ((n (or (and (third words) (parse-integer (third words) :junk-allowed t)) 24))
                  (said (asked path (list (list :agent-read session id n
                                                (flag-p args "--plain")))
                               :done (lambda (f) (eq :agent-lines (first f)))))
                  (lines (fourth (find :agent-lines said :key #'first))))
             (dolist (line lines) (format t "~&~A~%" line))))))
      ((string= what "explain")
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((said (asked path (list (list :agent-explain session id))
                             :done (lambda (f) (eq :agent-explained (first f)))))
                (form (find :agent-explained said :key #'first)))
           (unless form (error "~A did not say" (second args)))
           (destructuring-bind (published seen rows) (cdddr form)
             (format t "~&~(~A~); the screen says ~(~A~)~%" published seen)
             (dolist (row rows)
               (destructuring-bind (id widget means won found) row
                 (format t "~&~A ~(~12A~) ~(~13A~) ~(~A~)~%" (if won "*" " ") id widget means)
                 (when won
                   (loop :for (key value) :on (cddr (cddr (cddr found))) :by #'cddr
                         :when value
                           :do (format t "~&        ~(~A~) ~S~%" key value)))))))))
      ((string= what "trace")
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((said (asked path (list (list :agent-trace session id))
                             :done (lambda (f) (eq :agent-traced (first f)))))
                (rows (fourth (find :agent-traced said :key #'first)))
                (first-ms (or (first (first rows)) 0)))
           (dolist (row rows)
             (destructuring-bind (ms moved said state) row
               (format t "~&~8D ~A ~(~A~) ~(~A~)~%"
                       (- ms first-ms) (if moved "+" " ")
                       (if (consp said) (format nil "~A:~A" (first said) (second said)) said)
                       state))))))
      ((string= what "snapshot")
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((said (asked path (list (list :agent-snapshot session id))
                             :done (lambda (f) (eq :agent-snapshotted (first f)))))
                (snapshot (fourth (find :agent-snapshotted said :key #'first))))
           (unless snapshot (error "~A did not say" (second args)))
           (if (third args)
               (with-open-file (out (third args) :direction :output :if-exists :supersede
                                                 :external-format :utf-8)
                 (agent:write-snapshot snapshot out))
               (agent:write-snapshot snapshot *standard-output*)))))
      ((string= what "say")
       (unless (third args) (error "atty agent say <session>:<pane> <keys>"))
       (multiple-value-bind (path session id) (pane-found (second args))
         (asked path (list (list :agent-keys session id (unescaped (third args)) (caller)))
                :patience 0)))
      ((string= what "prompt")
       (unless (third args)
         (error "atty agent prompt <session>:<pane> <text> [--until <state>...]"))
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((wanted (mapcar #'a-state
                                (rest (member "--until" (cdddr args) :test #'string=))))
                (turn nil)
                (said (asked path (list (list :go session)
                                        (list :agent-prompt session id (third args) (caller)))
                             :patience (if wanted 3600 3)
                             :done (lambda (f)
                                     (case (first f)
                                       (:agent-prompted
                                        (setf turn (fifth f))
                                        (or (null wanted) (not (eq t (fourth f)))))
                                       (:agent-turn (and turn (eql (third f) id)
                                                         (eql (fourth f) turn)
                                                         (eq :ended (fifth f))
                                                         (member :idle wanted)))
                                       (:agent (and (eql (third f) id)
                                                    (member (fifth f) wanted)
                                                    (or (null turn) (not (eq :idle (fifth f))))))))))
                (answer (fourth (find :agent-prompted said :key #'first))))
           (cond
             ((eq answer :blocked)
              (format *error-output*
                      "~&atty: ~A is blocked: it wants an answer, not a prompt.~%~
                       atty agent read it, then atty agent answer it.~%"
                      (second args))
              (sb-ext:quit :unix-status 2))
             ((not (eq answer t)) (error "~A is gone" (second args)))
             (wanted
              (let ((state (agent-state-in (car (last said)) id)))
                (if (member state wanted)
                    (format t "~&~(~A~)~%" state)
                    (sb-ext:quit :unix-status 1))))))))
      ((string= what "signal")
       (let ((socket (sb-ext:posix-getenv "ATTY_SOCKET"))
             (pane (sb-ext:posix-getenv "ATTY_PANE")))
         (unless (and (second args) socket pane (plusp (length socket)) (plusp (length pane)))
           (error "atty agent signal <working|blocked|idle> is said from inside a pane"))
         (multiple-value-bind (session id) (pane-address pane)
           (asked socket (list (list :agent-signal session id (a-state (second args)) (caller)))
                  :patience 0))))
      ((and (string= what "wait") (member "--turn" (cddr args) :test #'string=))
       (multiple-value-bind (session id) (pane-address (second args))
         (let ((row (find-if (lambda (r) (and (string= session (second r)) (eql id (third r))))
                             (agents-here))))
           (unless row (error "there is no pane called ~A running" (second args)))
           (let ((said (asked (first row) (list (list :go session) '(:agents))
                              :patience 3600
                              :done (lambda (form)
                                      (and (eq :agent-turn (first form)) (eql (third form) id)
                                           (eq :ended (fifth form)))))))
             (if (find :agent-turn said :key #'first)
                 (format t "~&turn ~D ended~%" (fourth (car (last said))))
                 (sb-ext:quit :unix-status 1))))))
      ((string= what "act")
       (unless (third args)
         (error "atty agent act <session>:<pane> approve|approve-always|deny|choose <n>|submit <text>|interrupt"))
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((action (find (third args) '(:approve :approve-always :deny :choose :submit :interrupt)
                              :test #'string-equal))
                (argument (case action
                            (:choose (and (fourth args) (parse-integer (fourth args) :junk-allowed t)))
                            (:submit (fourth args)))))
           (unless action
             (error "~A is not something atty can do: approve, approve-always, deny, choose, submit or interrupt"
                    (third args)))
           (let* ((said (asked path (list (list :agent-act session id action argument))
                               :patience 15
                               :done (lambda (f) (eq :agent-acted (first f)))))
                  (form (find :agent-acted said :key #'first)))
             (unless form (error "~A did not say" (second args)))
             (destructuring-bind (happened &optional detail screen) (nthcdr 4 form)
               (case happened
                 (:done (format t "~&done~@[, now ~(~A~)~]~%" detail))
                 (:not-offered
                  (format *error-output* "~&atty: ~A is on ~(~A~), which offers ~:[nothing~;~:*~{~(~A~)~^, ~}~]~%"
                          (second args) screen detail)
                  (sb-ext:quit :unix-status 2))
                 (:failed
                  (format *error-output* "~&atty: the keys went to ~A and its screen did not change~%"
                          (second args))
                  (sb-ext:quit :unix-status 1))
                 (:no-reader (error "~A is running nothing atty has a reader for" (second args)))
                 (t (error "~A is gone" (second args)))))))))
      ((string= what "observe")
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((said (asked path (list (list :agent-observe session id))
                             :done (lambda (f) (eq :agent-observed (first f)))))
                (it (fourth (find :agent-observed said :key #'first)))
                (seen (getf it :observation)))
           (unless it (error "~A did not say" (second args)))
           (format t "~&~A~@[ ~A~]~:[, with no reader of its own for this version~;~]  ~(~A~)~%"
                   (getf it :kind) (getf it :version)
                   (or (getf it :verified) (string= "agent" (getf it :kind)))
                   (getf it :state))
           (cond
             ((string= "agent" (getf it :kind)))
             ((null seen) (format t "~&the screen is nothing its reader knows~%"))
             (t
              (format t "~&screen  ~(~A~), which means ~(~A~)~%" (getf seen :screen) (getf seen :means))
              (when (getf seen :question) (format t "~&asks    ~A~%" (getf seen :question)))
              (dolist (line (getf seen :subject)) (format t "~&about   ~A~%" line))
              (loop :for option :in (getf seen :options)
                    :for n :from 1
                    :do (format t "~&  ~:[ ~;>~] ~D. ~A~%" (eql (1- n) (getf seen :selected)) n option))
              (when (eq :prompt-input (getf seen :widget))
                (format t "~&typed   ~S~:[~; (a suggestion, not typed)~]~%"
                        (getf seen :text) (getf seen :ghost)))
              (when (getf seen :label)
                (format t "~&doing   ~A~@[ for ~Ds~]~%" (getf seen :label) (getf seen :seconds)))
              (when (getf it :offered)
                (format t "~&can     ~{~(~A~)~^, ~}~%" (getf it :offered))))))))
      ((string= what "wait")
       (let ((words (words args)))
         (unless (second words) (error "atty agent wait <session>:<pane> [<state>...] [--after-prompt]"))
         (multiple-value-bind (path session id) (pane-found (second words))
           (let ((wanted (or (mapcar #'a-state (cddr words)) '(:blocked :idle))))
             (if (flag-p args "--after-prompt")
                 (wait-after-prompt path session id wanted)
                 (let* ((said (asked path (list (list :go session) '(:agents))
                                     :patience 3600
                                     :done (lambda (form)
                                             (member (agent-state-in form id) wanted))))
                        (state (and said (agent-state-in (car (last said)) id))))
                   (if (member state wanted)
                       (format t "~&~(~A~)~%" state)
                       (sb-ext:quit :unix-status 1))))))))
      (t (error "atty agent list, spawn, name, wait, read, prompt, answer, say, log, ~
                 explain, trace or signal; not ~A" what)))))

(defun reason-said (state reason)
  (cond ((and (eq state :blocked) (getf reason :question)) (getf reason :question))
        ((eq state :blocked) (and (getf reason :screen) (string-downcase (getf reason :screen))))
        ((eq :unrecognized (first reason))
         (format nil "unrecognized ~A~@[ ~A~]" (getf (rest reason) :program)
                 (getf (rest reason) :version)))))

(defun readers-verbs (args)
  (let ((what (first args)))
    (cond
      ((or (null what) (string= what "list"))
       (dolist (reader agent:*readers*)
         (format t "~&~A~{ ~A~}~%" (agent:reader-name reader) (agent::reader-versions reader))))
      ((string= what "index")
       (let ((dir (or (second args) "readers/")))
         (format t "~&~{~A~%~}" (agent:write-index dir))))
      ((string= what "update") (update-readers))
      ((string= what "verify")
       (let ((dirs (corpus-dirs (or (second args) "readers/"))))
         (unless dirs (error "no corpora under ~A" (or (second args) "readers/")))
         (unless (every #'identity (mapcar #'verify-corpus dirs))
           (sb-ext:quit :unix-status 1))))
      (t (error "atty readers list, index, update or verify; not ~A" what)))))

(defun catalog-url ()
  (let ((said (sb-ext:posix-getenv "ATTY_READERS_URL")))
    (string-right-trim "/" (if (and said (plusp (length said)))
                               said
                               "https://raw.githubusercontent.com/bretfh/automatty/main/readers"))))

(defun fetched (url)
  (let* ((out (make-string-output-stream))
         (process (sb-ext:run-program "curl" (list "-fsSL" "--max-time" "20" url)
                                      :search t :output out :error nil)))
    (unless (eql 0 (sb-ext:process-exit-code process))
      (error "could not fetch ~A" url))
    (get-output-stream-string out)))

(defun update-readers ()
  (let* ((base (catalog-url))
         (index (with-standard-io-syntax
                  (let ((*read-eval* nil))
                    (read-from-string (fetched (format nil "~A/index.sexp" base))))))
         (entries (getf (nthcdr 2 index) :readers))
         (texts (mapcar (lambda (entry) (fetched (format nil "~A/~A/reader.lisp" base entry)))
                        entries)))
    (multiple-value-bind (loaded refused)
        (let ((agent:*readers* nil)) (agent:register-reader-texts texts))
      (dolist (why refused) (format *error-output* "~&atty: refused ~A~%" why))
      (format t "~&~D from ~A~{~%  ~A~}~%" (length loaded) base loaded)
      (dolist (path (remove-duplicates
                     (remove-if-not #'answering-p
                                    (cons (where-the-server-is)
                                          (mapcar #'second (other-servers))))
                     :test #'string=))
        (let* ((said (asked path (list (list :readers-load texts))
                            :done (lambda (f) (eq :readers-loaded (first f)))))
               (answer (find :readers-loaded said :key #'first)))
          (format t "~&~A ~:[did not answer~;loaded ~:*~D~]~%"
                  (file-namestring path) (and answer (length (second answer)))))))))

(defun spawn-a-server (path)
  "Another of this program, told to serve at PATH, out of reach of this
terminal. Answers its pid."
  (let ((me (self)))
    (if me
        (pty:spawn-in-its-own-session me (list "-L" (server-name) "serve")
                                      :output (log-path))
        (let ((form (format nil "(atty:serve ~S ~S :name nil)" path (a-shell))))
          (pty:spawn-in-its-own-session
           (namestring sb-ext:*runtime-pathname*)
           (list "--no-userinit" "--disable-debugger"
                 "--eval" "(require :asdf)"
                 "--eval" "(asdf:load-system :atty)"
                 "--eval" form
                 "--quit")
           :output (log-path))))))

(defun start-a-server ()
  "Start this user's server, holding nothing yet: the client that started it
opens the first session. Answers its path."
  (let ((path (socket-path)))
    (spawn-a-server path)
    ;; a server with a lot to bring back from disk takes a while before it
    ;; takes its name; a minute is longer than any restore should be
    (loop repeat 6000
          until (answering-p path)
          do (sleep 0.01))
    path))

(defun a-fresh-server ()
  "Start this user's server with nothing in it: what it had on disk is put
aside first. It cannot be done to a server that is running."
  (let ((path (where-the-server-is)))
    (when (answering-p path)
      (error "a server is running; atty kill-server or atty restart-server first"))
    (ignore-errors (delete-file path))
    (let ((aside (move-state-aside)))
      (when aside (format t "~&what was saved is in ~A~%" aside)))
    (start-a-server)))

(defun restart-the-server ()
  "Stop this user's server, keeping everything, and start it again from
whatever atty is now: how a new build takes over. Whoever is attached is told,
waits, and comes back. Answers how many sessions came back."
  (let ((path (where-the-server-is)))
    (unless (answering-p path)
      (error "no server is running"))
    (let* ((said (asked path '((:restart)) :done (lambda (f) (eq :restarting (first f)))))
           (by-itself (second (find :restarting said :key #'first))))
      (loop repeat 3000
            while (probe-file path)
            do (sleep 0.01)
            finally (when (probe-file path)
                      (error "the server did not stop; ~A says why" (log-path))))
      ;; a server that is a program starts its successor itself; one in a
      ;; lisp cannot, and this does
      (if by-itself
          (loop repeat 6000 until (answering-p path) do (sleep 0.01))
          (start-a-server))
      (unless (answering-p path)
        (error "no server came back. ~A says why." (log-path)))
      (length (the-sessions)))))

(defun server-back-p (path)
  "Wait for the server at PATH to go and come back, as a restart does: first
until its name is gone, then until it answers again. Answers whether it did."
  (loop repeat 3000 while (probe-file path) do (sleep 0.01))
  (loop repeat 6000
        until (answering-p path 1)
        do (sleep 0.01)
        finally (return (answering-p path 1))))

(defun attach-through-restarts (path &rest args)
  "ATTACH, and when the server says it is restarting, wait for it and attach
again to the same session, so a restart is a pause rather than an end."
  (loop
    (let ((why (apply #'attach path args)))
      (unless (eq why :restarting) (return why))
      (format t "~&the server is restarting; waiting for it…~%")
      (unless (server-back-p path) (return :no-server-back)))))

(defun the-server ()
  "This user's server, started when it is not running."
  (let ((path (where-the-server-is)))
    (unless (answering-p path)
      (ignore-errors (delete-file path))
      (start-a-server))
    (unless (answering-p path)
      (error "no server came up. ~A says why." (log-path)))
    path))

(defun say-why (name why)
  (case why
    (:detached (format t "~&detached from ~A~%" name))
    (:done nil)
    (:restarting nil)
    (:no-server-back
     (format *error-output* "~&atty: the server did not come back. ~A says why.~%" (log-path)))
    (:asked-to-stop nil)
    (:stopped (format t "~&~A was stopped~%" name))
    (:no-answer
     (format *error-output*
             "~&atty: the server took the connection and then said nothing.~%~
              It is most likely older than this client: it has been running~%~
              since whenever, and what it made of what we sent it is in~%~
              ~A. The programs in it are still running.~%"
             (log-path)))
    (:no-such-session
     (format *error-output* "~&atty: there is no session called ~A.~%" name))
    (:server-gone
     (format *error-output* "~&atty: the server stopped. ~A says why.~%" (log-path)))
    (t (when why (format t "~&~A~%" why))))
  why)

(defun a-shell ()
  (or (sb-ext:posix-getenv "SHELL") "/bin/sh"))

(defun stop-a-server (path)
  "Ask the server at PATH to go. Answers whether it went.

The programs in it go with it, which is what stopping it means, so it is never
something anything else does on your behalf."
  (and (answering-p path)
       (progn (asked path '((:stop)) :patience 0)
              (loop repeat 300
                    while (answering-p path 1)
                    do (sleep 0.01))
              (not (answering-p path 1)))))

(defun stop-a-session (name)
  "Stop the session called NAME and the programs in it. A server from before
one server held them all is stopped whole, since that is all it holds."
  (let* ((path (where-the-server-is))
         (said (and (answering-p path)
                    (asked path (list (list :kill-session name))
                           :done (lambda (f) (eq :killed (first f))))))
         (killed (third (find :killed said :key #'first))))
    (or killed
        (let ((legacy (find name (other-servers) :key #'first :test #'string=)))
          (and legacy (third legacy) (stop-a-server (second legacy)))))))

(defun the-sessions ()
  "What this server holds, as (name rows cols panes attached blocked) rows."
  (let ((path (where-the-server-is)))
    (and (answering-p path)
         (second (find :these (asked path '((:sessions))
                                     :done (lambda (f) (eq :these (first f))))
                       :key #'first)))))

(defun the-clients ()
  "Who is attached to this server, as (id tty rows cols session window
since-ms idle-ms) rows."
  (let ((path (where-the-server-is)))
    (and (answering-p path)
         (second (find :clients (asked path '((:clients))
                                       :done (lambda (f) (eq :clients (first f))))
                       :key #'first)))))

(defun client-name (row)
  "What to call an attached terminal: its tty without the /dev/, else its id."
  (let ((tty (second row)))
    (if (and (stringp tty) (plusp (length tty)))
        (if (and (> (length tty) 5) (string= "/dev/" tty :end2 5)) (subseq tty 5) tty)
        (format nil "client ~D" (first row)))))

(defun list-clients ()
  "atty clients: every terminal attached, where it is looking, and for how long."
  (let ((rows (the-clients)))
    (if (null rows)
        (format t "~&nobody is attached~%")
        (dolist (row rows)
          (destructuring-bind (id tty rows cols session window since idle &optional following) row
            (declare (ignore id tty))
            (format t "~&~-10A ~Dx~D  ~A~@[ › ~D~]  attached ~A~@[  idle ~A~]~@[  follows ~A~]~%"
                    (client-name row) cols rows (or session "no session") window
                    (duration since) (and idle (duration idle))
                    (and following (let ((led (find following rows :key #'first)))
                                     (if led (client-name led) following)))))))))

(defun event-line (kind who text)
  "What an event says, in a few words, and who did it when somebody did."
  (format nil "~A~@[ by ~A~]" (event-says kind text) (and who (who-said-here who))))

(defun list-events (args)
  "atty events [<n>]: what happened lately across the server, newest first."
  (let* ((path (where-the-server-is))
         (n (or (and (second args) (parse-integer (second args) :junk-allowed t)) 20))
         (events (and (answering-p path)
                      (second (find :events (asked path (list (list :events n))
                                                   :done (lambda (f) (eq :events (first f))))
                                    :key #'first)))))
    (if events
        (loop :for (nil clock kind session id window who text) :in events
              :do (format t "~&~A  ~A ~A:~@[~D.~]~D  ~A~%"
                          (wall-clock clock) (event-glyph kind) session window id
                          (event-line kind who text)))
        (format t "~&nothing has happened yet~%"))))

(defun rename-a-session (args)
  "atty rename <old> <new>: call a session something else."
  (destructuring-bind (&optional old new) (rest args)
    (unless (and old new) (error "atty rename <old> <new>: which session, and what to call it?"))
    (let* ((path (where-the-server-is))
           (said (and (answering-p path)
                      (find :session-named (asked path (list (list :name-session old new))
                                                  :done (lambda (f) (eq :session-named (first f))))
                            :key #'first))))
      (case (fourth said)
        ((t) (format t "~&~A is now ~A~%" old new))
        (:taken (error "there is already a session called ~A" new))
        (:empty (error "a session needs a name"))
        (:gone (error "nothing is called ~A" old))
        (t (error "no server answered"))))))

(defun list-sessions ()
  (let ((rows (the-sessions))
        (clients (the-clients))
        (others (other-servers)))
    (dolist (row rows)
      (destructuring-bind (name rows cols panes attached &optional (blocked 0) windows) row
        (declare (ignore rows cols attached))
        (let ((here (remove name clients :key #'fifth :test-not #'equal)))
          (format t "~&~12A ~D window~:P  ~D pane~:P~A~@[  ~{~A~^, ~}~]~%"
                  name (max 1 (length windows)) panes
                  (if (plusp blocked)
                      (format nil "  ▲ ~D need~A you" blocked (if (= blocked 1) "s" ""))
                      "")
                  (mapcar #'client-name here)))))
    (dolist (other others)
      (destructuring-bind (name path legacyp) other
        (declare (ignore path))
        (if legacyp
            (format t "~&~12A a server from before one held them all; atty stop ~A~%"
                    name name)
            (format t "~&~12A another server; atty -L ~A list~%" name name))))
    (unless (or rows others)
      (let ((saved (saved-sessions)))
        (if saved
            (loop :for (name windows panes at) :in saved
                  :do (format t "~&~12A ~D window~:P  ~D pane~:P  saved ~A; not running, atty ~A brings it back~%"
                              name windows panes (day-and-time at) name))
            (format t "~&nothing is running~%"))))))

(defun cwd ()
  (ignore-errors (sb-posix:getcwd)))

(defun run (&key (name "0") (command (a-shell)) label)
  "Join NAME in this user's server, making the server and the session when
they are not there, its first pane called LABEL."
  (say-why name (attach-through-restarts (if *fresh* (a-fresh-server) (the-server))
                                         :name name :open (list command (cwd) label))))

(defun usage (s)
  (format s "~&atty: many terminals inside one~%~%")
  (format s "  atty                 a shell in a session called 0, made if it is not there~%")
  (format s "  atty <name>          the same, under another name~%")
  (format s "  atty run <name> <command> [--name <n>]   the same, running COMMAND~%")
  (format s "  atty attach [<name>] join a session already running~%")
  (format s "  atty list            the sessions, their windows and panes, how many need you, who is attached~%")
  (format s "  atty clients         every terminal attached, and what each is looking at~%")
  (format s "  atty events [n]      what happened lately across the server, newest first~%")
  (format s "  atty rename <old> <new>  call a session something else~%")
  (format s "  atty stop <name>     stop a session, and the programs in it~%")
  (format s "  atty kill-server     stop every session~%")
  (format s "  atty restart-server  stop it and start it again from this build, keeping everything~%")
  (format s "  atty start [--fresh] the server, without attaching; --fresh puts what it had on disk aside~%")
  (format s "  atty save            what every session holds, to disk, now~%")
  (format s "  atty serve           the server itself, in the foreground~%")
  (format s "  atty -L <server> ... any of these, against another server than your own~%")
  (format s "  atty help init       what ~~/.config/atty/init.lisp can say, and every setting~%")
  (format s "  atty agent list [--blocked] [--session <s>] [--json]   what every pane is doing~%")
  (format s "  atty agent spawn <session> [--name <n>] [--cwd <d>] [--window <n> | --new-window] -- <command>   a new pane~%")
  (format s "  atty agent name <session>:<pane> <name>    what to call it~%")
  (format s "  atty agent wait <session>:<pane> [<state>...] [--after-prompt]   until it is~%")
  (format s "                       blocked or idle; after a prompt, only once it took it up~%")
  (format s "  atty agent read <session>:<pane> [<lines>] [--plain]   what is on its screen;~%")
  (format s "                       --plain leaves out what is only suggested, drawn faint~%")
  (format s "  atty agent prompt <session>:<pane> <text> [--until <state>...]   new work~%")
  (format s "  atty agent answer <session>:<pane> <n>   answer n to what it asks; exit 3 if~%")
  (format s "                       it is asking nothing~%")
  (format s "  atty agent say <session>:<pane> <keys>   raw keys, \\r for enter~%")
  (format s "  atty agent log <session>:<pane> [<n>]    who typed into it~%")
  (format s "  atty agent explain <session>:<pane>      which rule says what it is doing~%")
  (format s "  atty agent trace <session>:<pane>        every look at it: ms, moved, said, state~%")
  (format s "  atty agent snapshot <session>:<pane> [<file>]   its screen, cells and faces, as data~%")
  (format s "  atty agent observe <session>:<pane>      what its screen shows, and what can be done~%")
  (format s "  atty agent act <session>:<pane> <action> [<n>|<text>]   approve, approve-always, deny,~%")
  (format s "                         choose <n>, submit <text> or interrupt, confirmed on its screen~%")
  (format s "  atty agent wait <session>:<pane> --turn  until its turn ends~%")
  (format s "  atty record <agent> [<dir>]  drive the installed agent through its scenario, keep what it drew~%")
  (format s "  atty readers list            which readers there are, for which versions~%")
  (format s "  atty readers index [<dir>]   write the catalog's index of reader files~%")
  (format s "  atty readers update          fetch the catalog and load it into every running server~%")
  (format s "  atty readers verify [<dir>]  replay every recorded version against its reader~%")
  (format s "  atty agent signal <state>   from inside a pane: what its program is doing~%~%")
  (format s "  ~C-b N what needs you, ~C-b w the switchboard, ~C-b a the one blocked longest,~%"
          #\^ #\^ #\^)
  (format s "  ~C-b e why a pane is what it is, ~C-b z zoom, ~C-b , name a pane.~%"
          #\^ #\^ #\^)
  (format s "  ~C-b d detaches, ~C-b r redraws, ~C-b ~C-b types a ~C-b,~%"
          #\^ #\^ #\^ #\^ #\^)
  (format s "  ~C-b : runs a command by name and ~C-b ? says what every key does.~%"
          #\^ #\^)
  (format s "  Panes are windows: ~C-b 2 splits below, ~C-b 3 beside, ~C-b 0 closes~%"
          #\^ #\^ #\^)
  (format s "  one, ~C-b 1 leaves only this one, ~C-b o goes to the next.~%"
          #\^ #\^)
  (format s "  ~C-b c starts another session, ~C-b b chooses one, ~C-b t the bar.~%"
          #\^ #\^ #\^))

(defun serve-here (args)
  "atty serve [<name> [<command> [<rows> <cols>]]]: this user's server, in the
foreground. With a name it holds that session from the start."
  (let ((path (socket-path)))
    (when (answering-p path 1)
      (error "a server is already running at ~A; atty run <name> <command> adds ~
              a session to it" path))
    (multiple-value-bind (rows cols)
        (terminal-size tty:+stdin+)
      (let ((said-rows (and (third args) (parse-integer (third args) :junk-allowed t)))
            (said-cols (and (fourth args) (parse-integer (fourth args) :junk-allowed t))))
        (let ((server (serve path (or (second args) (a-shell))
                             :name (first args)
                             :rows (or said-rows rows)
                             :cols (or said-cols cols)
                             :fresh *fresh*)))
          ;; asked to restart: the name is gone and the state is on disk, and
          ;; the next server is this program again, started by the one leaving
          ;; so that nobody has to stay around to do it
          (when (server-restarting server)
            (spawn-a-server path)))))))

(defun main (&optional (args (rest sb-ext:*posix-argv*)))
  (handler-case
      (let ((*server-name* *server-name*)
            (*fresh* (and (member "--fresh" args :test #'string=) t))
            (args (remove "--fresh" args :test #'string=)))
        (load-user-init)
        (loop :while (and (first args) (string= (first args) "-L"))
              :do (unless (second args) (error "-L wants the name of a server"))
                  (setf *server-name* (a-name (second args) "a server")
                        args (cddr args)))
        (let ((what (first args)))
          (cond
            ((null what) (run))
            ((string= what "list") (list-sessions))
            ((string= what "clients") (list-clients))
            ((string= what "events") (list-events args))
            ((string= what "rename") (rename-a-session args))
            ((string= what "stop")
             (let ((name (or (second args) (error "atty stop <name>: which session?"))))
               (cond ((stop-a-session name)
                      (format t "~&stopped ~A~%" name))
                     ;; not running, but kept on disk: stopping it is forgetting it
                     ((forget-saved-session name)
                      (format t "~&~A was not running; what was saved of it is gone~%" name))
                     (t (error "nothing called ~A is running or saved" name)))))
            ((string= what "start")
             (if *fresh* (a-fresh-server) (the-server))
             (format t "~&~A~%" (if (the-sessions)
                                    (format nil "~D session~:P" (length (the-sessions)))
                                    "the server is up, holding nothing yet")))
            ((string= what "restart-server")
             (format t "~&the server is back with ~D session~:P~%" (restart-the-server)))
            ((string= what "save")
             (let ((path (where-the-server-is)))
               (unless (answering-p path) (error "no server is running"))
               (asked path '((:save)) :done (lambda (f) (eq :saved (first f))))
               (format t "~&saved~%")))
            ((string= what "kill-server")
             (if (stop-a-server (where-the-server-is))
                 (let ((saved (saved-sessions)))
                   (if saved
                       (format t "~&stopped every session; ~D saved, atty brings ~:[them~;it~] back~%"
                               (length saved) (= 1 (length saved)))
                       (format t "~&stopped every session~%")))
                 (error "no server is running")))
            ((string= what "attach")
             (let ((path (where-the-server-is)))
               (unless (answering-p path)
                 (error "nothing is running"))
               (say-why (or (second args) "the first session")
                        (attach-through-restarts path :name (second args)))))
            ((string= what "serve") (serve-here (rest args)))
            ((string= what "run")
             (let ((words (words (rest args) "--name")))
               (run :name (or (first words) "0")
                    :command (or (second words) (a-shell))
                    :label (option args "--name"))))
            ((string= what "agent") (agent-verbs (rest args)))
            ((string= what "record")
             (unless (second args) (error "atty record <agent> [<dir>]"))
             (multiple-value-bind (dir expect)
                 (record-agent (second args) :into (or (third args) "readers/"))
               (unless (verify-corpus dir) (sb-ext:quit :unix-status 1))
               (let ((made (extend-reader dir expect)))
                 (when made
                   (format t "~&it reads the way the nearest reader says, so ~A joins it: ~A~%"
                           (getf (nthcdr 2 expect) :version) (namestring made))))))
            ((string= what "readers") (readers-verbs (rest args)))
            ((or (string= what "-h") (string= what "--help")) (usage *standard-output*))
            ((string= what "help")
             (if (equal (second args) "init")
                 (init-help *standard-output*)
                 (usage *standard-output*)))
            (t (run :name what)))))
    (stream-error ()
      (sb-ext:quit :unix-status 0 :recklessly-p t))
    ;; ^C while this waits on something, a server restarting say, is somebody
    ;; leaving, not a fault to print a backtrace for
    (sb-sys:interactive-interrupt ()
      (sb-ext:quit :unix-status 130 :recklessly-p t))
    (error (e)
      (format *error-output* "~&atty: ~A~%" e)
      (sb-ext:quit :unix-status 1))))
