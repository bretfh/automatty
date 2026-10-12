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
