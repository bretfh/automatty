;;;; An init.lisp to start from. Copy it to ~/.config/atty/init.lisp, make the
;;;; paths yours and keep what you like; ^-b R loads it again.

(configure :wheel-rows 5)

(define-key 'pane-mode "C-b |" "split right")
(define-key 'pane-mode "C-b -" "split below")

;;; Your servers and what is in them. Each server makes whatever of its own is
;;; missing, by name, when it starts and when this file is loaded again, and
;;; leaves what is there alone. atty -L ops starts the second.

(defserver default
  :directory "~/code/"
  (session "site" :directory "site/"
    (window "edit"
      (columns (pane "zsh" :name "repl")
               (pane "claude" :name "claude")))
    (window "test"
      (rows (pane "zsh" :name "suite")
            (columns (pane "tail -f log/development.log" :name "logs")
                     (pane "psql site_development" :name "db")))))
  (session "notes"
    (window "today"
      (pane "${EDITOR:-vi} ~/notes/today.md" :name "today"))))

(defserver ops
  :directory "~/infra/"
  :env '("AWS_PROFILE=dev")
  (session "k8s"
    (window "watch"
      (columns (pane "k9s" :name "k9s")
               (rows (pane "zsh" :name "kubectl")
                     (pane "zsh" :name "tf" :directory "terraform/")))))
  (session "prod" :env '("AWS_PROFILE=prod")
    (window "careful" (pane "zsh" :name "prod"))))

;;; atty worktree feat/x: a worktree for the branch, and a window in it with
;;; claude beside a shell.

(defcommand worktree (branch)
  "a worktree for BRANCH, and a window in it"
  (uiop:run-program (list "git" "-C" (namestring (merge-pathnames "code/site/" (user-homedir-pathname)))
                          "worktree" "add" (format nil "../wt/~A" branch) branch))
  (new-window :name branch :directory (format nil "~~/code/wt/~A/" branch) :command "zsh")
  (split-right :name "claude" :command "claude"))

;;; ^-b k stops whatever the suite is running, from any pane.

(defcommand stop-suite ()
  "C-c into the suite"
  (send-keys "site:test.suite" "C-c"))

(define-key 'pane-mode "C-b k" "stop suite")
