;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun address-part (text)
  (if (every #'digit-char-p text) (parse-integer text) text))

(defun parse-pane-address (said)
  "SAID taken apart: the session, and either the window and the pane in it
(todo:1.2, todo:agents.impl) or the pane alone (todo:4, todo:impl, impl).
Answers (values session id window n), a number for a number and a name for a
name; session is nil when SAID names a pane alone."
  (let* ((colon (position #\: said :from-end t))
         (rest (if colon (subseq said (1+ colon)) said))
         (dot (position #\. rest)))
    (when (or (zerop (length rest))
              (and dot (or (zerop dot) (= (1+ dot) (length rest)))))
      (error "~S is not a pane: a pane is <session>:<window>.<pane>, by number or by name" said))
    (let ((session (and colon (subseq said 0 colon))))
      (if dot
          (values session nil
                  (address-part (subseq rest 0 dot)) (address-part (subseq rest (1+ dot))))
          (values session (address-part rest) nil nil)))))

(defun address-fits-p (session id window n session-name window-number window-name pane-number pane-name pane-id)
  (and (or (null session) (equal session session-name))
       (if id
           (if (integerp id) (and session (eql id pane-id)) (equal id pane-name))
           (and (if (integerp window) (eql window window-number) (equal window window-name))
                (if (integerp n) (eql n pane-number) (equal n pane-name))))))

(defun one-fitting (said found here-p address-of)
  (let ((found (or (and (rest found) (remove-if-not here-p found)) found)))
    (when (rest found)
      (error "~A names ~D panes: ~{~A~^, ~}" said (length found) (mapcar address-of found)))
    (first found)))

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

(defun request-pane-rows ()
  "Every pane of this server as the plist it tells a client."
  (second (find :panes (request (server-socket-path) '((:panes))
                              :done (lambda (f) (eq :panes (first f))))
                :key #'first)))

(defun resolve-pane (address)
  "The pane ADDRESS names: its server's path, its session and its id, and its
kind. Either form of address finds it."
  (unless address (error "which pane? <session>:<window>.<pane>, as atty agent list says"))
  (multiple-value-bind (session id window n) (parse-pane-address address)
    (let* ((here (let ((pane (sb-ext:posix-getenv "ATTY_PANE")))
                   (and pane (plusp (length pane)) (subseq pane 0 (position #\: pane :from-end t)))))
           (row (one-fitting address
                             (remove-if-not (lambda (r)
                                              (address-fits-p session id window n
                                                              (getf r :session) (getf r :window) (getf r :window-name)
                                                              (and (getf r :at) (1+ (getf r :at))) (getf r :name) (getf r :id)))
                                            (request-pane-rows))
                             (lambda (r) (equal here (getf r :session)))
                             #'row-address)))
      (unless row (error "there is no pane called ~A running" address))
      (values (server-socket-path) (getf row :session) (getf row :id) (getf row :kind)))))

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
  (let ((what (or (first args) "list")))
    (cond
      ((string= what "prompt") (prompt-agent args))
      ((and (string= what "wait") (member "--turn" (cddr args) :test #'string=)) (wait-for-turn args))
      ((string= what "wait") (wait-for-agent args))
      ((string= what "act") (act-on-agent args))
      ((and (string= what "snapshot") (third args))
       (with-open-file (*standard-output* (third args) :direction :output :if-exists :supersede
                                                       :external-format :utf-8)
         (ran (run-in-server "agent snapshot" (list (second args))))))
      ((string= what "spawn")
       (ensure-server)
       (ran (run-in-server "agent spawn" (rest args))))
      ((member what '("list" "name" "answer" "log" "read" "explain" "trace" "snapshot" "say"
                      "signal" "observe")
               :test #'string=)
       (ran (run-in-server (format nil "agent ~A" what) (rest args))))
      (t (error "atty agent list, spawn, wait, prompt, answer, explain, trace, snapshot, ~
                 act, observe or signal; not ~A" what)))))

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

(defun attach-or-create (&key (name "0") (command (default-shell)) pane-name)
  "Join NAME in this user's server, making the server and the session when
they are not there, its first pane called LABEL."
  (report-exit-reason name (attach-through-restarts (if *fresh-start* (start-fresh-server) (ensure-server))
                                         :name name :open (list command (cwd) pane-name))))
