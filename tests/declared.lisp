(in-package #:atty/test)

(def-suite declared :in all)
(in-suite declared)

(test a-tree-is-plain-data
  (let ((w (automatty:window "edit" :directory "src/"
             (automatty:columns (automatty:pane "zsh" :name "repl")
                                (automatty:pane "claude")))))
    (is (eq :window (first w)))
    (is (equal "edit" (second w)))
    (is (equal "src/" (getf (cddr w) :directory)))
    (let ((layout (getf (cddr w) :layout)))
      (is (eq :columns (first layout)))
      (is (equal '("zsh" "claude") (mapcar #'second (getf (rest layout) :parts)))))
    (is (equal (automatty:pane nil) (getf (cddr (automatty:window "bare")) :layout)))))

(test a-tree-refuses-what-it-cannot-hold
  (signals error (automatty:pane "zsh" :label "x"))
  (signals error (automatty:pane "zsh" :then "make"))
  (signals error (automatty:columns :size 1/3 (automatty:pane "a")))
  (signals error (automatty:window "w" (automatty:pane "a") (automatty:pane "b")))
  (signals error (automatty:session "s"))
  (signals error (automatty:session "s" (automatty:window "w") (automatty:window "w")))
  (signals error (automatty:columns))
  (signals error (automatty:pane "zsh" :env '(("A" . "b"))))
  (signals error (automatty:window "" (automatty:pane "a"))))

(test directories-and-env-are-inherited
  (is (equal "/a/b/c/" (mux::declared-directory "/a/b/" "c/")))
  (is (equal "/x" (mux::declared-directory "/a/" "/x")))
  (is (equal "/a/" (mux::declared-directory "/a/" nil)))
  (is (equal (format nil "~A/code" (string-right-trim "/" (namestring (user-homedir-pathname))))
             (mux::declared-directory nil "~/code")))
  (is (equal '("A=1" "B=3" "C=4") (mux::declared-env '("A=1" "B=2") '("B=3" "C=4")))))

(defun declared-here (path &rest sessions)
  (mux:declare-server (automatty::server-form (file-namestring path) sessions)))

(defun window-names (session)
  (mapcar #'mux:window-name (mux:session-windows session)))

(defun screen-has (server pane text)
  (step-until server
              (lambda ()
                (search text (mux::with-term (term pane) (term:term-dump-to-string term))))))

(test a-declared-server-makes-its-tree
  (with-state-home (dir)
    (let ((path (socket-path)))
      (declared-here path
                     (automatty:session "work" :env '("WHERE=work")
                       (automatty:window "edit"
                         (automatty:columns (automatty:pane "sleep 30" :name "left")
                                            (automatty:rows (automatty:pane "sleep 30" :name "top")
                                                            (automatty:pane "sh" :name "shell"
                                                                            :directory "/tmp"
                                                                            :env '("WHAT=shell")))))
                       (automatty:window "logs" (automatty:pane "sleep 30" :name "logs"))))
      (let ((server (mux:make-server path)))
        (unwind-protect
             (progn
               (mux:make-declared server)
               (let ((work (mux:session-named server "work")))
                 (is-true work)
                 (is (equal '("edit" "logs") (window-names work)))
                 (is (equal '("left" "top" "shell") (mapcar #'mux::pane-name
                                                            (mux:window-panes (first (mux:session-windows work))))))
                 (is (every #'mux:pane-started (mux:session-panes work)))
                 (let ((shell (find "shell" (mux:session-panes work) :key #'mux::pane-name :test #'equal)))
                   (is (equal "/tmp" (mux::pane-directory shell)))
                   (is (equal '("WHERE=work" "WHAT=shell") (mux::pane-env shell)))
                   (mux:pane-write shell (format nil "echo $WHERE-$WHAT; pwd~C" #\Return))
                   (is-true (screen-has server shell "work-shell"))
                   (is-true (screen-has server shell "/tmp")))
                 (mux:make-declared server)
                 (is (eql 1 (length (mux:server-sessions server))) "a second make made the session again")
                 (is (equal '("edit" "logs") (window-names work)) "a second make made a window again")))
          (mux:server-close server)
          (remhash (file-namestring path) mux:*declared*)
          (ignore-errors (delete-file path)))))))

(test a-window-that-is-there-is-left-alone-and-a-missing-one-made-again
  (with-state-home (dir)
    (let ((path (socket-path)))
      (declared-here path
                     (automatty:session "work"
                       (automatty:window "edit"
                         (automatty:columns (automatty:pane "sleep 30" :name "a")
                                            (automatty:pane "sleep 30" :name "b")))
                       (automatty:window "logs" (automatty:pane "sleep 30" :name "logs"))))
      (let ((server (mux:make-server path)))
        (unwind-protect
             (let ((work (first (mux:make-declared server))))
               (mux:session-close-pane work (second (mux:window-panes (first (mux:session-windows work)))))
               (mux:session-close-window work (second (mux:session-windows work)))
               (is (equal '("edit") (window-names work)))
               (mux:save-all server))
          (mux:server-close server)))
      (ignore-errors (delete-file path))
      (let ((server (mux:make-server path)))
        (unwind-protect
             (let ((work (first (mux:restore-state server))))
               (mux:make-declared server)
               (is (eql 1 (length (mux:server-sessions server))))
               (is (equal '("edit" "logs") (window-names work)))
               (is (equal '("a") (mapcar #'mux::pane-name
                                         (mux:window-panes (first (mux:session-windows work)))))
                   "the window that came back was changed"))
          (mux:server-close server)
          (remhash (file-namestring path) mux:*declared*)
          (ignore-errors (delete-file path)))))))

(test a-load-without-a-defserver-forgets-it
  (with-init-home ("(defserver gone (session \"s\" (window \"w\" (pane \"sleep 1\"))))")
    (is-true (mux:load-user-init))
    (is (null mux:*init-error*) "~A" mux:*init-error*)
    (is-true (gethash "gone" mux:*declared*))
    (write-init "(configure :wheel-rows 3)")
    (is-true (mux:load-user-init))
    (is (null (gethash "gone" mux:*declared*)))))

(test a-pane-is-found-by-its-names-and-told-keys-and-read
  (let ((path (socket-path)))
    (declared-here path
                   (automatty:session "work"
                     (automatty:window "edit" (automatty:pane "sh" :name "shell"))
                     (automatty:window "logs"
                       (automatty:columns (automatty:pane "sleep 30" :name "tail")
                                          (automatty:pane "sleep 30" :name "shell")))))
    (let ((server (mux:make-server path)))
      (unwind-protect
           (let* ((work (first (mux:make-declared server)))
                  (shell (first (mux:session-panes work)))
                  (wire (wire-to path)))
             (is (eq shell (mux::pane-at-address server "work:edit.shell")))
             (is (eq shell (mux::pane-at-address server "work:1.1")))
             (is (eq (third (mux:session-panes work)) (mux::pane-at-address server "work:logs.shell")))
             (is (eq (second (mux:session-panes work)) (mux::pane-at-address server "tail")))
             (signals error (mux::pane-at-address server "work:shell"))
             (is (null (mux::pane-at-address server "work:nothing")))
             (multiple-value-bind (said status)
                 (run-by-name server wire "send keys" '("work:edit.shell" "echo sent-by-name" "RET"))
               (is (eq t status) "~S" said))
             (is-true (screen-has server shell "sent-by-name"))
             (is (search "sent-by-name" (run-by-name server wire "read pane" '("work:edit.shell" "--plain"))))
             (is (search "keys" (run-by-name server wire "read log" '("work:edit.shell"))))
             (run-by-name server wire "rename pane" '("work:logs.tail" "follow"))
             (is (equal "follow" (mux::pane-name (second (mux:session-panes work)))))
             (multiple-value-bind (said status) (run-by-name server wire "send keys" '("work:shell" "x"))
               (is (eql 1 status))
               (is (search "names 2 panes" said) "~S" said))
             (mux:wire-close wire))
        (mux:server-close server)
        (remhash (file-namestring path) mux:*declared*)
        (ignore-errors (delete-file path))))))

(test the-commands-that-make-take-a-command-a-directory-and-a-name
  (with-stepped-server (server path)
    (let* ((session (mux:add-session server "sleep 30" :name "work" :rows 12 :cols 60))
           (wire (wire-to path)))
      (multiple-value-bind (said status)
          (run-by-name server wire "split right" '("--name" "tail" "--directory" "/tmp" "--" "sleep" "31"))
        (is (eq t status) "~S" said)
        (is (search "work:1.2" said) "~S" said))
      (let ((made (second (mux:session-panes session))))
        (is (equal "tail" (mux::pane-name made)))
        (is (equal "/tmp" (mux::pane-directory made)))
        (is (equal "sleep 31" (mux:pane-command made))))
      (run-by-name server wire "new window" '("--name" "logs" "sleep 32"))
      (is (equal "logs" (mux:window-name (second (mux:session-windows session)))))
      (is (equal "sleep 32" (mux:pane-command (first (mux:window-panes (second (mux:session-windows session)))))))
      (run-by-name server wire "new session" '("--name" "other" "sleep 33"))
      (is-true (mux:session-named server "other"))
      (mux:wire-close wire))))

(test a-window-made-says-so
  (let ((path (socket-path))
        (made nil))
    (flet ((note (session window) (push (list (mux:session-name session) (mux:window-name window)) made)))
      (declared-here path
                     (automatty:session "work"
                       (automatty:window "edit" (automatty:pane "sleep 30"))
                       (automatty:window "logs" (automatty:pane "sleep 30"))))
      (mux:add-hook 'mux::window-made #'note)
      (let ((server (mux:make-server path)))
        (unwind-protect
             (progn
               (mux:make-declared server)
               (is (equal '(("work" "edit") ("work" "logs")) (reverse made))))
          (mux:remove-hook 'mux::window-made #'note)
          (mux:server-close server)
          (remhash (file-namestring path) mux:*declared*)
          (ignore-errors (delete-file path)))))))

(test every-command-offered-is-a-function-of-the-language
  (dolist (name (mux::command-names))
    (let ((symbol (find-symbol (string-upcase (substitute #\- #\Space name)) '#:automatty/user)))
      (is-true symbol "~A is not in automatty/user" name)
      (is (eq :external (nth-value 1 (find-symbol (symbol-name symbol) '#:automatty))))
      (is (fboundp symbol))))
  (is (null (find-symbol "PRESS" '#:automatty/user)) "the keymap's insides reach a config"))

(test the-example-init-file-loads
  (unwind-protect
       (progn
         (is-true (mux:load-user-init (namestring (asdf:system-relative-pathname :atty "examples/init.lisp"))))
         (is (null mux:*init-error*) "~A" mux:*init-error*)
         (is-true (gethash "default" mux:*declared*))
         (is-true (gethash "ops" mux:*declared*))
         (is-true (gethash "worktree" mux:*commands*))
         (is-true (gethash "stop suite" mux:*commands*)))
    (init-forgotten)))
