(in-package #:atty/test)

(def-suite init :in all)
(in-suite init)

(defmacro with-init-file ((file text) &body body)
  `(let ((,file (format nil "~Aatty-init-~D-~D.lisp" (uiop:temporary-directory)
                        (sb-posix:getpid) (random 1000000))))
     (unwind-protect
          (progn
            (with-open-file (s ,file :direction :output :if-exists :supersede)
              (write-string ,text s))
            ,@body)
       (ignore-errors (delete-file ,file))
       (init-forgotten))))

(defun init-forgotten ()
  (mux::undo-changes mux::*init-changes*)
  (setf mux::*init-changes* nil
        mux:*init-error* nil))

(defmacro with-init-home ((text) &body body)
  (let ((home (gensym "HOME")) (was (gensym "WAS")))
    `(let ((,home (format nil "~Aatty-home-~D-~D/" (uiop:temporary-directory)
                          (sb-posix:getpid) (random 1000000)))
           (,was (sb-posix:getenv "XDG_CONFIG_HOME")))
       (unwind-protect
            (progn
              (ensure-directories-exist (format nil "~Aatty/" ,home))
              (with-open-file (s (format nil "~Aatty/init.lisp" ,home)
                                 :direction :output :if-exists :supersede)
                (write-string ,text s))
              (sb-posix:setenv "XDG_CONFIG_HOME" ,home 1)
              ,@body)
         (if ,was (sb-posix:setenv "XDG_CONFIG_HOME" ,was 1) (sb-posix:unsetenv "XDG_CONFIG_HOME"))
         (uiop:delete-directory-tree (uiop:ensure-directory-pathname ,home)
                                     :validate t :if-does-not-exist :ignore)
         (init-forgotten)))))

(defun write-init (text)
  (with-open-file (s (mux:user-init-file) :direction :output :if-exists :supersede)
    (write-string text s)))

(test an-init-file-binds-a-key-to-a-command-by-name-and-sets-a-setting
  (with-init-file (file "(define-key 'scroll-mode \"C-wheel-up\" \"scroll to top\")
                            (configure :wheel-rows 7)")
    (with-setting-kept (:wheel-rows)
      (unwind-protect
           (progn
             (is-true (mux:load-user-init file))
             (is (null mux:*init-error*))
             (is (eql 7 mux:+wheel-rows+))
             (let ((does (atty/mode:lookup-key "C-wheel-up" (atty/mode:mode-named 'mux::scroll-mode))))
               (is (functionp does) "the key was not bound")))
        (atty/mode:undefine-key 'mux::scroll-mode "C-wheel-up")))))

(test a-command-of-ones-own-can-be-defined-and-run-by-name
  (with-init-file (file "(defcommand wave () (setf (symbol-value 'atty/user::*waved*) t))")
    (is-true (mux:load-user-init file))
    (is-true (gethash "wave" mux:*commands*))
    (mux:run-command "wave" nil)
    (is-true (symbol-value (find-symbol "*WAVED*" '#:atty/user)))
    (remhash "wave" mux:*commands*)))

(test a-broken-init-file-is-said-kept-and-passed-over
  (with-init-file (file "(this-is-not-anything)")
    (let ((said (with-output-to-string (*error-output*)
                  (is (null (mux:load-user-init file))))))
      (is (search "did not load" said))
      (is (search "did not load" mux:*init-error*)))
    (is (null (mux:load-user-init "/tmp/atty-there-is-no-such-file.lisp")))
    (is (null mux:*init-error*) "a missing file is not a problem")))

(test what-the-server-has-to-say-goes-to-the-first-client-only
  (with-stepped-server (server path)
    (let ((session (mux:add-session server "sleep 30" :name "work" :rows 6 :cols 30)))
      (push (list "in the server, init.lisp did not load: no" :warning) (mux::server-notes server))
      (let ((first-in (wire-to path))
            (second-in (wire-to path)))
        (say-to first-in (list :open "work" nil nil 6 30 t))
        (heard-from server first-in :hello)
        (say-to second-in (list :open "work" nil nil 6 30 t))
        (heard-from server second-in :hello)
        (destructuring-bind (one two) (reverse (mux:session-watchers session))
          (is (equal '("in the server, init.lisp did not load: no")
                     (mux::note-lines (first (mux:watcher-overlays one)))))
          (is (null (mux:watcher-overlays two))
              "the second client was told what the first already was"))
        (mux:wire-close first-in)
        (mux:wire-close second-in)))))

(test the-server-reloads-the-init-file-when-asked-and-says-how-it-went
  (with-stepped-server (server path)
    (let ((session (mux:add-session server "sleep 30" :name "work" :rows 6 :cols 30))
          (wire (wire-to path)))
      (say-to wire (list :open "work" nil nil 6 30 t))
      (heard-from server wire :hello)
      (let ((w (first (mux:session-watchers session))))
        (mux:run-command "reload init" w)
        (let ((note (first (mux:watcher-overlays w))))
          (is (typep note 'mux:note))
          (is (member (mux::note-face note) '(:accent :warning)))))
      (mux:wire-close wire))))

(test help-init-lists-every-setting
  (let ((said (with-output-to-string (s) (mux:init-help s))))
    (dolist (row (mux:settings))
      (is (search (format nil ":~(~A~)" (first row)) said)
          "help init does not mention ~S" (first row)))
    (is (search "pane-started" said))))

(test a-load-undoes-what-the-last-one-did
  (with-init-home ("(define-key 'scroll-mode \"C-wheel-up\" \"scroll to top\")
                       (defcommand wave () (show-note *client* \"hi\" \"there\"))
                       (configure :wheel-rows 7)")
    (let ((rows mux:+wheel-rows+))
      (is-true (mux:load-user-init))
      (is (eql 7 mux:+wheel-rows+))
      (is-true (gethash "wave" mux:*commands*))
      (is (equal "scroll to top"
                 (atty/mode:handler-name (atty/mode:lookup-key "C-wheel-up" 'mux::scroll-mode))))
      (write-init "(configure :hold-every 60)")
      (is-true (mux:load-user-init))
      (is (eql rows mux:+wheel-rows+) "a setting the file no longer sets was left as it set it")
      (is (null (gethash "wave" mux:*commands*)) "a command the file no longer defines was left")
      (is (null (atty/mode:lookup-key "C-wheel-up" 'mux::scroll-mode)) "a key the file no longer binds was left")
      (is (eql 60 mux::+hold-every+)))))

(test a-command-from-the-init-file-runs-for-whoever-asked
  (with-init-home ("(defcommand wave () (show-note *client* \"hi\" \"there\" :face :accent))")
    (mux:load-user-init)
    (with-stepped-server (server path)
      (let ((session (mux:add-session server "sleep 30" :name "work" :rows 6 :cols 30))
            (wire (wire-to path)))
        (say-to wire (list :open "work" nil nil 6 30 t))
        (heard-from server wire :hello)
        (let ((w (first (mux:session-watchers session))))
          (mux:run-command "wave" w)
          (let ((note (first (mux:watcher-overlays w))))
            (is (equal "hi" (mux::note-title note)))
            (is (equal '("there") (mux::note-lines note)))))
        (mux:wire-close wire)))))

(test the-server-says-its-settings-as-they-are-now
  (with-init-home ("(configure :wheel-rows 8)")
    (mux:load-user-init)
    (with-stepped-server (server path)
      (let ((wire (wire-to path)))
        (say-to wire (list :settings))
        (let ((row (find :wheel-rows (second (heard-from server wire :settings)) :key #'first)))
          (is (equal "8" (second row)))
          (is (equal "3" (third row))))
        (mux:wire-close wire)))))
