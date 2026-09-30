;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; The agent commands: what the panes' UI does, run by name from a command
;;; line, so a program in a pane can do for another what a person does at the
;;; keyboard. Each says what it has to say, and that is what is printed.

(defun pane-at-address (server address)
  "The pane ADDRESS names, session:window.pane or session:id, and its session."
  (multiple-value-bind (name id window n) (parse-pane-address address)
    (let ((session (find name (server-sessions server) :key #'session-name :test #'equal)))
      (when session
        (let ((pane (if id
                        (find id (session-panes session) :key #'pane-id)
                        (let ((w (session-nth-window session window)))
                          (and w (nth (1- n) (window-panes w)))))))
          (and pane (values pane session)))))))

(defun address-pane (address)
  (unless address (error "which pane? <session>:<window>.<pane>, as atty agent list says"))
  (multiple-value-bind (pane session) (pane-at-address (here-server) address)
    (unless pane (error "there is no pane called ~A running" address))
    (values pane session)))

(defcommand (agent-list :unlisted) (&rest args)
  "every pane, what it is and what it is doing: [--blocked] [--session <name>] [--json]"
  (let* ((rows (pane-rows (here-server) (now-ms)))
         (rows (if (flag-p args "--blocked")
                   (remove-if-not (lambda (r) (eq :blocked (getf r :state))) rows)
                   rows))
         (rows (let ((only (option-value args "--session")))
                 (if only (remove-if-not (lambda (r) (equal only (getf r :session))) rows) rows))))
    (cond
      ((and (flag-p args "--json") (null rows))
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

(defcommand (agent-spawn :unlisted) (&rest args)
  "a new pane: <session> [--name <name>] [--cwd <dir>] [--window <n> | --new-window] -- <command>"
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
    (let* ((server (here-server))
           (pane (spawn-pane server session
                             (if (plusp (length command)) command (default-shell))
                             (or (option-value before "--cwd") *caller-directory*)
                             (option-value before "--name")
                             window)))
      (when (pane-failed pane) (error "~A" (pane-failed pane)))
      (format t "~&~A~%" (pane-address-of (session-named server session) pane)))))

(defcommand (agent-name :unlisted) (&rest args)
  "what to call a pane: <pane> <name>, or from inside a pane <name>"
  (multiple-value-bind (address label)
      (if (second args)
          (values (first args) (second args))
          (values (or *caller* (error "atty agent name <session>:<pane> <name>")) (first args)))
    (multiple-value-bind (pane session) (address-pane address)
      (name-pane (here-server) (session-name session) (pane-id pane) (or label ""))
      (format t "~&~A:~D is ~A~%" (session-name session) (pane-id pane)
              (if (plusp (length (or label ""))) label "unnamed")))))

(defcommand (agent-answer :unlisted) (address n)
  "answer N to what the pane is asking; exits 3, having typed nothing, when it is not asking"
  (let ((n (or (parse-integer (princ-to-string n) :junk-allowed t)
               (error "atty agent answer <session>:<pane> <n>"))))
    (multiple-value-bind (pane session) (address-pane address)
      (case (answer-pane (here-server) *client* (session-name session) (pane-id pane) n *caller*)
        ((t) (format t "~&answered ~D~%" n))
        (t (refuse 3 "nothing was typed"))))))

(defun caller-actor (actor)
  (case (first actor)
    (:pane (second actor))
    (:client (or (third actor) (format nil "client ~D" (second actor))))
    (:atty "atty")
    (t "the command line")))

(defcommand (agent-log :unlisted) (address &optional n)
  "who typed into the pane, newest first"
  (let* ((pane (address-pane address))
         (n (or (and n (parse-integer (princ-to-string n) :junk-allowed t)) 20))
         (now (now-ms))
         (entries (mapcar (lambda (e) (encode-log-entry e now))
                          (subseq (pane-log pane) 0 (min n (length (pane-log pane)))))))
    (if entries
        (loop :for (nil actor verb summary outcome clock) :in entries
              :do (format t "~&~A  ~12A ~(~7A~) ~A~:[~;  refused~]~%"
                          (format-clock-hms clock) (caller-actor actor) verb
                          (if (eq verb :keys) (format nil "~D bytes" summary) (or summary ""))
                          (eq outcome :refused)))
        (format t "~&nobody has typed into ~A~%" address))))

(defcommand (agent-read :unlisted) (&rest args)
  "what is on the pane's screen: <pane> [<lines>] [--plain]"
  (let* ((words (positional-args args))
         (pane (address-pane (first words)))
         (n (or (and (second words) (parse-integer (second words) :junk-allowed t)) 24)))
    (dolist (line (if (flag-p args "--plain")
                      (with-term (term pane) (plain-lines term n))
                      (with-term (term pane) (agent:last-lines term n))))
      (format t "~&~A~%" line))))

(defcommand (agent-explain :unlisted) (address)
  "which rule says what the pane is doing"
  (let ((pane (address-pane address)))
    (multiple-value-bind (seen rows) (with-term (term pane) (agent:agent-explain (pane-agent pane) term))
      (format t "~&~(~A~); the screen says ~(~A~)~%" (agent:agent-state (pane-agent pane)) seen)
      (dolist (row rows)
        (destructuring-bind (id widget means won found) row
          (format t "~&~A ~(~12A~) ~(~13A~) ~(~A~)~%" (if won "*" " ") id widget means)
          (when won
            (loop :for (key value) :on (cddr (cddr (cddr found))) :by #'cddr
                  :when value
                    :do (format t "~&        ~(~A~) ~S~%" key value))))))))

(defcommand (agent-trace :unlisted) (address)
  "every look at the pane: ms, moved, what it said, state"
  (let* ((pane (address-pane address))
         (rows (reverse (agent:agent-trace (pane-agent pane))))
         (first-ms (or (first (first rows)) 0)))
    (dolist (row rows)
      (destructuring-bind (ms moved said state) row
        (format t "~&~8D ~A ~(~A~) ~(~A~)~%"
                (- ms first-ms) (if moved "+" " ")
                (if (consp said) (format nil "~A:~A" (first said) (second said)) said)
                state)))))

(defcommand (agent-snapshot :unlisted) (address)
  "the pane's screen, cells and faces, as data"
  (agent:write-snapshot (with-term (term (address-pane address)) (agent:snapshot term)) *standard-output*))

(defcommand (agent-say :unlisted) (address keys)
  "raw keys into the pane, \\r for enter"
  (multiple-value-bind (pane) (address-pane address)
    (let ((text (unescape keys)))
      (pane-push-log pane (now-ms) (actor-of *client* *caller*) :say (summarize-text text))
      (pane-write pane text))))

(defcommand (agent-signal :unlisted) (state)
  "from inside a pane: what its program is doing, working, blocked or idle"
  (unless *caller* (error "atty agent signal <working|blocked|idle> is said from inside a pane"))
  (multiple-value-bind (pane session) (address-pane *caller*)
    (pane-push-log pane (now-ms) (actor-of *client* *caller*) :signal (parse-state state))
    (pane-hear pane (parse-state state))
    (let ((ms (now-ms)))
      (session-observe session (monotonic-ns)
                       (and (on-pane pane (lambda () (look-at-agent pane ms))) (list pane))))))

(defcommand (agent-observe :unlisted) (address)
  "what the pane's screen shows, and what can be done"
  (let* ((pane (address-pane address))
         (agent (pane-agent pane))
         (reader (agent:agent-reader agent))
         (seen (and reader (with-term (term pane) (agent:observe reader term))))
         (kind (agent:agent-kind agent)))
    (format t "~&~A~@[ ~A~]~:[, with no reader of its own for this version~;~]  ~(~A~)~%"
            kind (agent:agent-version agent)
            (or (agent:agent-verified agent) (string= "agent" kind))
            (agent:agent-state agent))
    (cond
      ((string= "agent" kind))
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
       (let ((offered (agent:offered reader seen)))
         (when offered
           (format t "~&can     ~{~(~A~)~^, ~}~%" offered)))))))
