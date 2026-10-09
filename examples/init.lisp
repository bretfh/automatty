;;;; An init.lisp to start from. Copy it to ~/.config/atty/init.lisp, make the
;;;; paths yours and keep what you like; ^-b R loads it again.

(configure :wheel-rows 5)

(define-key 'pane-mode "C-b |" "split right")
(define-key 'pane-mode "C-b -" "split below")

;;; Your servers and everything in them. After a restart each server makes
;;; whatever of its tree is missing, by name, and leaves what came back alone;
;;; starting one starts the rest.

(defserver default
  :directory "~/code/"
  (session "site"
    :directory "site/"
    (window "edit"
      (columns (pane "zsh" :label "repl")
               (pane "claude" :label "claude" :size 1/3)))
    (window "test"
      (rows (pane "zsh" :label "suite" :then "make test" :hold t)
            (columns :size 12
                     (pane "zsh" :label "logs" :then "tail -f log/development.log")
                     (pane "zsh" :label "db" :then "psql site_development")))))
  (session "notes"
    (window "today"
      (pane "${EDITOR:-vi} ~/notes/today.md" :label "today"))))

(defserver ops
  :directory "~/infra/"
  :env '(("AWS_PROFILE" . "dev"))
  (session "k8s"
    (window "watch"
      (columns (pane "k9s" :label "k9s")
               (rows :size 1/3
                     (pane "zsh" :label "kubectl")
                     (pane "zsh" :label "tf" :directory "terraform/")))))
  (session "prod"
    :env '(("AWS_PROFILE" . "prod"))
    (window "careful" (pane "zsh" :label "prod"))))

;;; A command that says what it takes: `: worktree feat/x` makes it at once,
;;; `: worktree` asks which branch and offers yours.

(defcommand worktree ((branch :string "branch"
                              :from (run-program "git" "-C" "~/code/site" "branch" "--format=%(refname:short)")))
  "a worktree for BRANCH, and a window in it"
  (run-program "git" "-C" "~/code/site" "worktree" "add" (format nil "~~/code/wt/~A" branch) branch)
  (window branch
    (columns (pane "zsh" :label branch :directory (format nil "~~/code/wt/~A/" branch))
             (pane "claude" :size 1/3 :directory (format nil "~~/code/wt/~A/" branch)))))

;;; ^-b k stops whatever the suite is running.

(defcommand stop-suite ()
  "C-c into the suite"
  (send-keys (pane-labelled "suite") "C-c"))

(define-key 'pane-mode "C-b k" "stop suite")
