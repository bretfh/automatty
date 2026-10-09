;;;; An init.lisp to start from. Copy it to ~/.config/atty/init.lisp, keep
;;;; what you like, and ^-b R loads it again. It is Lisp the server itself
;;;; runs, so whatever atty can do, it can do.

(configure :wheel-rows 5
           :gui-font-size 14)

(define-key 'pane-mode "C-b |" "split right")
(define-key 'pane-mode "C-b -" "split below")
(define-key 'pane-mode "M-o" "next pane")

;;; Every command has a name, and a name is something to run. These are the
;;; same commands `atty agent ...` runs from a shell.

(defun spawn (session dir name command &rest more)
  (run-command "agent spawn" *client*
               (append (list session "--name" name "--cwd" dir) more (list "--" command))))

(defun say (address keys)
  (run-command "agent say" *client* (list address keys)))

;;; A whole project at once: a session named for it, an editor and a shell side
;;; by side, and claude in a window of its own, each pane called what it is.
;;;
;;;   atty project site ~/code/site
;;;   atty site

(defcommand project (name dir)
  "a session called NAME for the project in DIR: an editor, a shell, and claude"
  (spawn name dir "edit" "${EDITOR:-vi} .")
  (spawn name dir "shell" "$SHELL")
  (spawn name dir "claude" "claude" "--new-window"))

;;; From any pane, the tests again in the project's shell.

(defparameter *tests-pane* "site:1.2")

(defcommand rerun-tests ()
  "make test again, in the project's shell"
  (say *tests-pane* "make test\\r"))

(define-key 'pane-mode "C-b m" "rerun tests")

;;; Servers are cheap. This one is for work, apart from everything else: it
;;; stops, restarts and is saved on its own. `atty work` makes it with a
;;; standup session in it, and `atty -L work` goes there.

(defparameter *atty* sb-ext:*runtime-pathname*)

(defun atty (&rest args)
  (with-output-to-string (out)
    (sb-ext:run-program *atty* args :output out :error out)))

(defcommand work ()
  "a server called work, with notes and a shell in a session called standup"
  (let ((home (namestring (user-homedir-pathname))))
    (atty "-L" "work" "start")
    (atty "-L" "work" "agent" "spawn" "standup" "--name" "notes" "--cwd" home
          "--" "${EDITOR:-vi} standup.md")
    (atty "-L" "work" "agent" "spawn" "standup" "--name" "inbox" "--cwd" home
          "--" "$SHELL")))

;;; Whatever is asking, and an answer for all of it at once: the first choice,
;;; which for claude is yes. Run it when you mean it.

(defun asking ()
  (let ((said (with-output-to-string (*standard-output*)
                (run-command "agent list" *client* (list "--blocked")))))
    (loop :for line :in (uiop:split-string said :separator '(#\Newline))
          :for address := (subseq line 0 (or (position #\Space line) (length line)))
          :when (find #\: address) :collect address)))

(defcommand yes-to-all ()
  "answer every pane that is asking with its first choice"
  (dolist (address (asking))
    (run-command "agent answer" *client* (list address "1"))))

(define-key 'pane-mode "C-b Y" "yes to all")

;;; Coming back, a note says when something is waiting on you, and nothing
;;; when nothing is.

(add-hook 'client-attached
          (lambda (client)
            (let ((n (length (asking))))
              (when (plusp n)
                (show-note client "waiting on you"
                           (format nil "~D pane~:P asking; ^-b a goes to the first" n)
                           :face :accent)))))
