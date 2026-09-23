;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun parse-pane-address (said)
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

(defun parse-state (said)
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

(defun resolve-pane (address)
  "The pane ADDRESS names: its server's path, its session and its id, and its
kind. Either form of address finds it."
  (unless address (error "which pane? <session>:<window>.<pane>, as atty agent list says"))
  (multiple-value-bind (session id window n) (parse-pane-address address)
    (let ((row (find-if (lambda (r)
                          (and (equal session (getf r :session))
                               (if id
                                   (eql id (getf r :id))
                                   (and (eql window (getf r :window))
                                        (eql (1- n) (getf r :at))))))
                        (request-pane-rows))))
      (unless row (error "there is no pane called ~A running" address))
      (values (server-socket-path) session (getf row :id) (getf row :kind)))))

(defun unescape (said)
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

(defun option-value (args name)
  "The word after NAME in ARGS, or nil."
  (second (member name args :test #'string=)))

(defun positional-args (args &rest options)
  "ARGS without its flags, and without each of OPTIONS and the word after it."
  (loop :with skip := nil
        :for a :in args
        :if skip :do (setf skip nil)
        :else :if (member a options :test #'string=) :do (setf skip t)
        :else :unless (and (> (length a) 2) (string= "--" a :end2 2)) :collect a))

(defun request-pane-rows ()
  "Every pane of this server as the plist it tells a client."
  (second (find :panes (request (server-socket-path) '((:panes))
                              :done (lambda (f) (eq :panes (first f))))
                :key #'first)))

(defun to-json (thing out)
  "THING as JSON: a plist is an object, a list an array, a keyword a string."
  (cond ((null thing) (write-string "null" out))
        ((eq thing t) (write-string "true" out))
        ((numberp thing) (format out "~D" thing))
        ((keywordp thing) (to-json (string-downcase (symbol-name thing)) out))
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
                   (to-json (substitute #\_ #\- (string-downcase (symbol-name key))) out)
                   (write-char #\: out)
                   (to-json value out))
         (write-char #\} out))
        ((consp thing)
         (write-char #\[ out)
         (loop :for it :in thing
               :for first := t :then nil
               :do (unless first (write-char #\, out))
                   (to-json it out))
         (write-char #\] out))
        (t (to-json (princ-to-string thing) out))))

(defun list-agents (args)
  "atty agent list [--blocked] [--session <name>] [--json]"
  (let* ((rows (request-pane-rows))
         (rows (if (flag-p args "--blocked")
                   (remove-if-not (lambda (r) (eq :blocked (getf r :state))) rows)
                   rows))
         (rows (let ((only (option-value args "--session")))
                 (if only (remove-if-not (lambda (r) (equal only (getf r :session))) rows) rows))))
    (cond
      ((and (flag-p args "--json") (null rows))
       ;; an empty array, not null: a script iterating it wants nothing to do,
       ;; not something to check for first
       (format t "[]~%"))
      ((flag-p args "--json")
       (to-json (mapcar (lambda (r)
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
                           (if (getf r :known) (format-duration (getf r :for)) "")
                           (getf (getf r :asks) :subject))))))))

(defun spawn-agent (args)
  "atty agent spawn <session> [--name <name>] [--cwd <dir>] [--window <n> | --new-window] -- <command>"
  (let* ((dashes (member "--" args :test #'string=))
         (before (ldiff args dashes))
         (session (first (positional-args before "--name" "--cwd" "--window")))
         (command (format nil "~{~A~^ ~}" (or (rest dashes) (rest (positional-args before "--name" "--cwd" "--window")))))
         (window (cond ((flag-p before "--new-window") :new)
                       ((option-value before "--window")
                        (or (parse-integer (option-value before "--window") :junk-allowed t)
                            (error "--window wants a number, as atty agent list says them"))))))
    (unless session
      (error "atty agent spawn <session> [--name <name>] [--cwd <dir>] [--window <n> | --new-window] -- <command>"))
    (let* ((path (ensure-server))
           (said (request path (list (list :spawn session
                                         (if (plusp (length command)) command (default-shell))
                                         (or (option-value before "--cwd") (cwd))
                                         (option-value before "--name")
                                         window))
                        :done (lambda (f) (eq :spawned (first f)))))
           (spawned (find :spawned said :key #'first))
           (id (third spawned)))
      (unless id (error "no pane was made in ~A" session))
      (format t "~&~A~%" (or (fourth spawned) (format nil "~A:~D" session id))))))

(defun rename-agent (args)
  "atty agent name <pane> <name>, or from inside a pane atty agent name <name>"
  (multiple-value-bind (address label)
      (if (third args)
          (values (second args) (third args))
          (values (or (caller-pane) (error "atty agent name <session>:<pane> <name>")) (second args)))
    (multiple-value-bind (path session id) (resolve-pane address)
      (request path (list (list :name-pane session id (or label "")))
             :done (lambda (f) (eq :named (first f))))
      (format t "~&~A:~D is ~A~%" session id (if (plusp (length (or label ""))) label "unnamed")))))

(defun answer-agent (args)
  "atty agent answer <pane> <n>: type answer N to what the pane is asking. Exits
3, having typed nothing, when it is not asking or has no such answer."
  (let ((n (and (third args) (parse-integer (third args) :junk-allowed t))))
    (unless n (error "atty agent answer <session>:<pane> <n>"))
    (multiple-value-bind (path session id) (resolve-pane (second args))
      (let ((outcome (fifth (find :answered
                                  (request path (list (list :answer session id n (caller-pane)))
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

(defun caller-actor (actor)
  (case (first actor)
    (:pane (second actor))
    (:client (or (third actor) (format nil "client ~D" (second actor))))
    (:atty "atty")
    (t "the command line")))

(defun agent-log (args)
  "atty agent log <pane> [<n>]: who typed into it, newest first."
  (multiple-value-bind (path session id) (resolve-pane (second args))
    (let* ((n (or (and (third args) (parse-integer (third args) :junk-allowed t)) 20))
           (entries (fourth (find :pane-log
                                  (request path (list (list :pane-log session id n))
                                         :done (lambda (f) (eq :pane-log (first f))))
                                  :key #'first))))
      (if entries
          (loop :for (nil actor verb summary outcome clock) :in entries
                :do (format t "~&~A  ~12A ~(~7A~) ~A~:[~;  refused~]~%"
                            (format-clock-hms clock) (caller-actor actor) verb
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
                                   (request path (list (list :since-prompt session id))
                                          :done (lambda (f) (eq :since-prompt (first f))))
                                   :key #'first))
        :do (cond ((null said) (error "~A:~D is gone" session id))
                  ((null (second said)) (error "nothing was ever prompted into ~A:~D" session id))
                  ((done-since-prompt-p said wanted)
                   (format t "~&~(~A~)~%" (first said))
                   (return))
                  ((> (get-internal-real-time) deadline) (sb-ext:quit :unix-status 1)))
            (sleep 0.3)))

(defun read-agent (args)
  (let ((words (positional-args args)))
    (multiple-value-bind (path session id) (resolve-pane (second words))
      (let* ((n (or (and (third words) (parse-integer (third words) :junk-allowed t)) 24))
             (said (request path (list (list :agent-read session id n
                                           (flag-p args "--plain")))
                          :done (lambda (f) (eq :agent-lines (first f)))))
             (lines (fourth (find :agent-lines said :key #'first))))
        (dolist (line lines) (format t "~&~A~%" line))))))

(defun explain-agent (args)
  (multiple-value-bind (path session id) (resolve-pane (second args))
    (let* ((said (request path (list (list :agent-explain session id))
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

(defun trace-agent (args)
  (multiple-value-bind (path session id) (resolve-pane (second args))
    (let* ((said (request path (list (list :agent-trace session id))
                        :done (lambda (f) (eq :agent-traced (first f)))))
           (rows (fourth (find :agent-traced said :key #'first)))
           (first-ms (or (first (first rows)) 0)))
      (dolist (row rows)
        (destructuring-bind (ms moved said state) row
          (format t "~&~8D ~A ~(~A~) ~(~A~)~%"
                  (- ms first-ms) (if moved "+" " ")
                  (if (consp said) (format nil "~A:~A" (first said) (second said)) said)
                  state))))))

(defun snapshot-agent (args)
  (multiple-value-bind (path session id) (resolve-pane (second args))
    (let* ((said (request path (list (list :agent-snapshot session id))
                        :done (lambda (f) (eq :agent-snapshotted (first f)))))
           (snapshot (fourth (find :agent-snapshotted said :key #'first))))
      (unless snapshot (error "~A did not say" (second args)))
      (if (third args)
          (with-open-file (out (third args) :direction :output :if-exists :supersede
                                            :external-format :utf-8)
            (agent:write-snapshot snapshot out))
          (agent:write-snapshot snapshot *standard-output*)))))

(defun send-keys-to-agent (args)
  (unless (third args) (error "atty agent say <session>:<pane> <keys>"))
  (multiple-value-bind (path session id) (resolve-pane (second args))
    (request path (list (list :agent-keys session id (unescape (third args)) (caller-pane)))
           :patience 0)))

(defun prompt-agent (args)
  (unless (third args)
    (error "atty agent prompt <session>:<pane> <text> [--until <state>...]"))
  (multiple-value-bind (path session id) (resolve-pane (second args))
    (let* ((wanted (mapcar #'parse-state
                           (rest (member "--until" (cdddr args) :test #'string=))))
           (turn nil)
           (said (request path (list (list :go session)
                                   (list :agent-prompt session id (third args) (caller-pane)))
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

(defun signal-agent (args)
  (let ((socket (sb-ext:posix-getenv "ATTY_SOCKET"))
        (pane (sb-ext:posix-getenv "ATTY_PANE")))
    (unless (and (second args) socket pane (plusp (length socket)) (plusp (length pane)))
      (error "atty agent signal <working|blocked|idle> is said from inside a pane"))
    ;; the address is session:window.pane; the pane's id is looked up
    (multiple-value-bind (path session id) (resolve-pane pane)
      (declare (ignore path))
      (request socket (list (list :agent-signal session id (parse-state (second args)) (caller-pane)))
             :patience 0))))

(defun wait-for-turn (args)
  (multiple-value-bind (path session id) (resolve-pane (second args))
    (declare (ignore path))
    (let ((row (find-if (lambda (r) (and (string= session (second r)) (eql id (third r))))
                        (server-agent-rows))))
      (unless row (error "there is no pane called ~A running" (second args)))
      (let ((said (request (first row) (list (list :go session) '(:agents))
                         :patience 3600
                         :done (lambda (form)
                                 (and (eq :agent-turn (first form)) (eql (third form) id)
                                      (eq :ended (fifth form)))))))
        (if (find :agent-turn said :key #'first)
            (format t "~&turn ~D ended~%" (fourth (car (last said))))
            (sb-ext:quit :unix-status 1))))))

(defun act-on-agent (args)
  (unless (third args)
    (error "atty agent act <session>:<pane> approve|approve-always|deny|choose <n>|submit <text>|interrupt"))
  (multiple-value-bind (path session id) (resolve-pane (second args))
    (let* ((action (find (third args) '(:approve :approve-always :deny :choose :submit :interrupt)
                         :test #'string-equal))
           (argument (case action
                       (:choose (and (fourth args) (parse-integer (fourth args) :junk-allowed t)))
                       (:submit (fourth args)))))
      (unless action
        (error "~A is not something atty can do: approve, approve-always, deny, choose, submit or interrupt"
               (third args)))
      (let* ((said (request path (list (list :agent-act session id action argument))
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

(defun observe-agent (args)
  (multiple-value-bind (path session id) (resolve-pane (second args))
    (let* ((said (request path (list (list :agent-observe session id))
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

(defun wait-for-agent (args)
  (let ((words (positional-args args)))
    (unless (second words) (error "atty agent wait <session>:<pane> [<state>...] [--after-prompt]"))
    (multiple-value-bind (path session id) (resolve-pane (second words))
      (let ((wanted (or (mapcar #'parse-state (cddr words)) '(:blocked :idle))))
        (if (flag-p args "--after-prompt")
            (wait-after-prompt path session id wanted)
            (let* ((said (request path (list (list :go session) '(:agents))
                                :patience 3600
                                :done (lambda (form)
                                        (member (agent-state-in form id) wanted))))
                   (state (and said (agent-state-in (car (last said)) id))))
              (if (member state wanted)
                  (format t "~&~(~A~)~%" state)
                  (sb-ext:quit :unix-status 1))))))))

(defun agent-command (args)
  (let ((what (first args)))
    (cond
      ((or (null what) (string= what "list")) (list-agents (rest args)))
      ((string= what "spawn") (spawn-agent (rest args)))
      ((string= what "name") (rename-agent args))
      ((string= what "answer") (answer-agent args))
      ((string= what "log") (agent-log args))
      ((string= what "read") (read-agent args))
      ((string= what "explain") (explain-agent args))
      ((string= what "trace") (trace-agent args))
      ((string= what "snapshot") (snapshot-agent args))
      ((string= what "say") (send-keys-to-agent args))
      ((string= what "prompt") (prompt-agent args))
      ((string= what "signal") (signal-agent args))
      ((and (string= what "wait") (member "--turn" (cddr args) :test #'string=)) (wait-for-turn args))
      ((string= what "act") (act-on-agent args))
      ((string= what "observe") (observe-agent args))
      ((string= what "wait") (wait-for-agent args))
      (t (error "atty agent list, spawn, name, wait, read, prompt, answer, say, log, ~
                 explain, trace or signal; not ~A" what)))))

(defun format-reason (state reason)
  (cond ((and (eq state :blocked) (getf reason :question)) (getf reason :question))
        ((eq state :blocked) (and (getf reason :screen) (string-downcase (getf reason :screen))))
        ((eq :unrecognized (first reason))
         (format nil "unrecognized ~A~@[ ~A~]" (getf (rest reason) :program)
                 (getf (rest reason) :version)))))

(defun cwd ()
  (ignore-errors (sb-posix:getcwd)))

(defun attach-through-restarts (path &rest args)
  "ATTACH, and when the server says it is restarting, wait for it and attach
again to the same session, so a restart is a pause rather than an end. When
the server that comes back is another build, this becomes it first."
  (loop
    (let ((why (apply #'attach path args)))
      (unless (eq why :restarting) (return why))
      (format t "~&the server is restarting; waiting for it…~%")
      (unless (server-back-p path) (return :no-server-back))
      (exec-successor))))

(defun report-exit-reason (name why)
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

(defun attach-or-create (&key (name "0") (command (default-shell)) label)
  "Join NAME in this user's server, making the server and the session when
they are not there, its first pane called LABEL."
  (report-exit-reason name (attach-through-restarts (if *fresh-start* (start-fresh-server) (ensure-server))
                                         :name name :open (list command (cwd) label))))
