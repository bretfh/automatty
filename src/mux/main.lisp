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

(defun socket-path (name)
  "Where the socket for NAME is. NAME names one session, not a path: a name with
a directory in it would put the socket somewhere nobody is looking for it and
under permissions nobody set."
  (let ((name (file-namestring (princ-to-string name))))
    (when (zerop (length name))
      (error "~S is not a name a session can have" name))
    (namestring (merge-pathnames name (mux-dir)))))

(defun log-path (name)
  (namestring (merge-pathnames (format nil "~A.log" name) (mux-dir))))

(defun answering-p (path &optional (patience 3))
  "Whether a server at PATH answers, which is not the same as whether something
is listening there.

A server wedged on its way out still holds its socket, so a connection to it
succeeds and then nothing ever comes back, and a client that took that for a
living server would sit at a screen that never arrives. So it is knocked on."
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
                                            (setf here t))))))
                 (wire-close wire))
               here))
         (error () nil))))

(defun sessions-here ()
  (remove-if-not #'answering-p
                 (mapcar #'namestring (directory (merge-pathnames "*" (mux-dir))))))

(defun asked (path forms &key (patience 3) (done (constantly t)))
  (handler-case
      (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
        (sb-bsd-sockets:socket-connect socket path)
        (let ((wire (make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket))
              (deadline (+ (get-internal-real-time)
                           (* patience internal-time-units-per-second)))
              (heard nil))
          (unwind-protect
               (progn
                 (dolist (form forms) (wire-send wire form))
                 (wire-flush wire)
                 (loop
                   (when (> (get-internal-real-time) deadline) (return))
                   (when (pty:pty-wait (wire-fd wire) 100)
                     (unless (wire-fill wire) (return))
                     (loop for form = (wire-take wire)
                           while form
                           do (push form heard)
                              (when (funcall done form) (return-from asked (nreverse heard)))))))
            (wire-close wire))
          (nreverse heard)))
    (error () nil)))

(defun agents-here ()
  (loop :for path :in (sessions-here)
        :append (let ((said (asked path '((:agents))
                                   :done (lambda (form) (eq :agents (first form))))))
                  (mapcar (lambda (row) (cons path row))
                          (second (find :agents said :key #'first))))))

(defun pane-address (said)
  (let ((colon (position #\: said :from-end t)))
    (unless (and colon (parse-integer said :start (1+ colon) :junk-allowed t))
      (error "~S is not a pane: a pane is <session>:<number>, as ATTY_PANE says" said))
    (values (subseq said 0 colon) (parse-integer said :start (1+ colon)))))

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
  (unless address (error "which pane? <session>:<number>, as atty agent list says"))
  (multiple-value-bind (session id) (pane-address address)
    (let ((row (find-if (lambda (r) (and (string= session (second r)) (eql id (third r))))
                        (agents-here))))
      (unless row (error "there is no pane called ~A running" address))
      (values (first row) session id (fifth row)))))

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

(defun agent-verbs (args)
  (let ((what (first args)))
    (cond
      ((string= what "read")
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((n (or (and (third args) (parse-integer (third args) :junk-allowed t)) 24))
                (said (asked path (list (list :agent-read session id n))
                             :done (lambda (f) (eq :agent-lines (first f)))))
                (lines (fourth (find :agent-lines said :key #'first))))
           (dolist (line lines) (format t "~&~A~%" line)))))
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
         (asked path (list (list :agent-keys session id (unescaped (third args))))
                :patience 0)))
      ((string= what "prompt")
       (unless (third args)
         (error "atty agent prompt <session>:<pane> <text> [--until <state>...]"))
       (multiple-value-bind (path session id) (pane-found (second args))
         (let* ((wanted (mapcar #'a-state
                                (rest (member "--until" (cdddr args) :test #'string=))))
                (turn nil)
                (said (asked path (list (list :go session)
                                        (list :agent-prompt session id (third args)))
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
                       atty agent read it, then atty agent say what it is waiting for.~%"
                      (second args))
              (sb-ext:quit :unix-status 2))
             ((not (eq answer t)) (error "~A is gone" (second args)))
             (wanted
              (let ((state (agent-state-in (car (last said)) id)))
                (if (member state wanted)
                    (format t "~&~(~A~)~%" state)
                    (sb-ext:quit :unix-status 1))))))))
      ((or (null what) (string= what "list"))
       (let ((rows (agents-here)))
         (if rows
             (dolist (row rows)
               (destructuring-bind (path session id kind state &optional reason turn) row
                 (declare (ignore path turn))
                 (format t "~&~A:~D  ~A  ~(~A~)~@[  ~A~]~%" session id kind state
                         (reason-said state reason))))
             (format t "~&nothing is running~%"))))
      ((string= what "signal")
       (let ((socket (sb-ext:posix-getenv "ATTY_SOCKET"))
             (pane (sb-ext:posix-getenv "ATTY_PANE")))
         (unless (and (second args) socket pane (plusp (length socket)) (plusp (length pane)))
           (error "atty agent signal <working|blocked|idle> is said from inside a pane"))
         (multiple-value-bind (session id) (pane-address pane)
           (asked socket (list (list :agent-signal session id (a-state (second args))))
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
       (unless (second args) (error "atty agent wait <session>:<pane> [<state>...|--turn]"))
       (multiple-value-bind (session id) (pane-address (second args))
         (let* ((wanted (or (mapcar #'a-state (cddr args)) '(:blocked :idle)))
                (row (find-if (lambda (r) (and (string= session (second r)) (eql id (third r))))
                              (agents-here))))
           (unless row (error "there is no pane called ~A running" (second args)))
           (let* ((said (asked (first row) (list (list :go session) '(:agents))
                               :patience 3600
                               :done (lambda (form)
                                       (member (agent-state-in form id) wanted))))
                  (state (and said (agent-state-in (car (last said)) id))))
             (if (member state wanted)
                 (format t "~&~(~A~)~%" state)
                 (sb-ext:quit :unix-status 1))))))
      (t (error "atty agent list, wait, read, prompt, say, explain, trace, snapshot or signal; not ~A" what)))))

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
      (dolist (path (sessions-here))
        (let* ((said (asked path (list (list :readers-load texts))
                            :done (lambda (f) (eq :readers-loaded (first f)))))
               (answer (find :readers-loaded said :key #'first)))
          (format t "~&~A ~:[did not answer~;loaded ~:*~D~]~%"
                  (pathname-name path) (and answer (length (second answer)))))))))

(defun self ()
  "This program, when it is a program.

An executable core is its own runtime, so the server a client starts is another
of this told to serve rather than an sbcl told what to load. Out of a repl it is
neither, and there is nothing to run."
  (let ((runtime (and sb-ext:*runtime-pathname*
                      (namestring sb-ext:*runtime-pathname*)))
        (core (and sb-ext:*core-pathname* (namestring sb-ext:*core-pathname*))))
    (when (and runtime core (string= runtime core)) runtime)))

(defun start-a-server (name command &key rows cols)
  (let ((me (self)))
    (if me
        (pty:spawn-in-its-own-session
         me
         (list "serve" name command (princ-to-string rows) (princ-to-string cols))
         :output (log-path name))
        (let ((form (format nil "(atty:serve ~S ~S :name ~S :rows ~D :cols ~D)"
                            (socket-path name) command name rows cols)))
          (pty:spawn-in-its-own-session
           (namestring sb-ext:*runtime-pathname*)
           (list "--no-userinit" "--disable-debugger"
                 "--eval" "(require :asdf)"
                 "--eval" "(asdf:load-system :atty)"
                 "--eval" form
                 "--quit")
           :output (log-path name)))))
  (loop repeat 400
        until (answering-p (socket-path name))
        do (sleep 0.01))
  (socket-path name))

(defun say-why (name why)
  (case why
    (:detached (format t "~&detached from ~A~%" name))
    (:done nil)
    (:asked-to-stop nil)
    (:no-answer
     (format *error-output*
             "~&atty: the server for ~A took the connection and then said~%~
              nothing. It is most likely older than this client: it has been~%~
              running since whenever, and what it made of what we sent it is in~%~
              ~A. The programs in it are still running.~%"
             name (log-path name)))
    (:no-such-session
     (format *error-output* "~&atty: there is no session called ~A there.~%" name))
    (:server-gone
     (format *error-output* "~&atty: the server for ~A stopped. ~A says why.~%"
             name (log-path name)))
    (t (when why (format t "~&~A~%" why))))
  why)

(defun a-shell ()
  (or (sb-ext:posix-getenv "SHELL") "/bin/sh"))

(defun stop-a-server (name)
  "Ask the server for NAME to go. Answers whether there was one to ask.

The programs in it go with it, which is what stopping it means, so it is never
something anything else does on your behalf."
  (let ((path (socket-path name)))
    (and (answering-p path)
         (handler-case
             (let ((socket (make-instance 'sb-bsd-sockets:local-socket
                                          :type :stream)))
               (sb-bsd-sockets:socket-connect socket path)
               (let ((wire (make-wire (sb-bsd-sockets:socket-file-descriptor socket)
                                      socket)))
                 (unwind-protect
                      (progn (wire-send wire '(:stop)) (wire-flush wire))
                   (wire-close wire)))
               (loop repeat 300
                     while (answering-p path 1)
                     do (sleep 0.01))
               (not (answering-p path 1)))
           (error () nil)))))

(defun run (&key (name "0") (command (a-shell)))
  (let ((path (socket-path name)))
    (multiple-value-bind (rows cols)
        (if (tty:a-terminal-p tty:+stdin+) (tty:host-size tty:+stdin+) (values 24 80))
      (unless (answering-p path)
        (ignore-errors (delete-file path))
        (start-a-server name command :rows rows :cols cols))
      (unless (answering-p path)
        (error "no server came up. ~A says why." (log-path name)))
      (say-why name (attach path :name name)))))

(defun usage (s)
  (format s "~&atty: many terminals inside one~%~%")
  (format s "  atty                 a shell in a session called 0, made if it is not there~%")
  (format s "  atty <name>          the same, under another name~%")
  (format s "  atty run <name> <command>~%")
  (format s "  atty attach <name>   join a session already running~%")
  (format s "  atty serve <name> <command>   the server itself, in the foreground~%")
  (format s "  atty list            what is running~%")
  (format s "  atty stop <name>     stop a session, and the programs in it~%")
  (format s "  atty agent list      what the program in each pane is doing~%")
  (format s "  atty agent wait <session>:<pane> [<state>...]   until it is blocked or idle~%")
  (format s "  atty agent read <session>:<pane> [<lines>]      what is on its screen~%")
  (format s "  atty agent prompt <session>:<pane> <text> [--until <state>...]   new work~%")
  (format s "  atty agent say <session>:<pane> <keys>   an answer: raw keys, \\r for enter~%")
  (format s "  atty agent explain <session>:<pane>      which rule says what it is doing~%")
  (format s "  atty agent trace <session>:<pane>        every look at it: ms, moved, said, state~%")
  (format s "  atty agent snapshot <session>:<pane> [<file>]   its screen, cells and faces, as data~%")
  (format s "  atty agent observe <session>:<pane>      what its screen shows, and what can be done~%")
  (format s "  atty agent act <session>:<pane> <action> [<n>|<text>]   approve, approve-always, deny,~%")
  (format s "                         choose <n>, submit <text> or interrupt, confirmed on its screen~%")
  (format s "  atty agent wait <session>:<pane> --turn  until its turn ends~%")
  (format s "  atty agent signal <state>   from inside a pane: what its program is doing~%")
  (format s "  atty record <agent> [<dir>]  drive the installed agent through its scenario, keep what it drew~%")
  (format s "  atty readers list            which readers there are, for which versions~%")
  (format s "  atty readers index [<dir>]   write the catalog's index of reader files~%")
  (format s "  atty readers update          fetch the catalog and load it into every running server~%")
  (format s "  atty readers verify [<dir>]  replay every recorded version against its reader~%~%")
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

(defun main (&optional (args (rest sb-ext:*posix-argv*)))
  (handler-case
      (let ((what (first args)))
        (cond
          ((null what) (run))
          ((string= what "list")
           (let ((running (sessions-here)))
             (if running
                 (dolist (path running) (format t "~&~A~%" (pathname-name path)))
                 (format t "~&nothing is running~%"))))
          ((string= what "stop")
           (let ((name (or (second args) "0")))
             (if (stop-a-server name)
                 (format t "~&stopped ~A~%" name)
                 (format *error-output* "~&atty: nothing called ~A is running~%"
                         name))))
          ((string= what "attach")
           (let ((path (socket-path (or (second args) "0"))))
             (unless (answering-p path)
               (error "no session called ~A is running" (or (second args) "0")))
             (say-why (or (second args) "0")
                      (attach path :name (or (second args) "0")))))
          ((string= what "serve")
           (multiple-value-bind (rows cols)
               (if (tty:a-terminal-p tty:+stdin+)
                   (tty:host-size tty:+stdin+)
                   (values 24 80))
             (let ((said-rows (and (fourth args)
                                   (parse-integer (fourth args) :junk-allowed t)))
                   (said-cols (and (fifth args)
                                   (parse-integer (fifth args) :junk-allowed t))))
               (serve (socket-path (or (second args) "0"))
                      (or (third args) (a-shell))
                      :name (or (second args) "0")
                      :rows (or said-rows rows)
                      :cols (or said-cols cols)))))
          ((string= what "run")
           (run :name (or (second args) "0")
                :command (or (third args) (a-shell))))
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
          (t (run :name what))))
    (stream-error ()
      (sb-ext:quit :unix-status 0 :recklessly-p t))
    (error (e)
      (format *error-output* "~&atty: ~A~%" e)
      (sb-ext:quit :unix-status 1))))
