(in-package #:atty/test)

(def-suite init :in all)
(in-suite init)

(defmacro with-an-init-file ((file text) &body body)
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
  (mux::undo-changes mux::*config-changes*)
  (setf mux::*init-changes* nil
        mux::*config-changes* nil
        mux::*init-generation* 0
        mux:*init-error* nil))

(defmacro with-an-init-home ((text) &body body)
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
  (with-an-init-file (file "(define-key 'scroll-mode \"C-wheel-up\" \"scroll to top\")
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
  (with-an-init-file (file "(defcommand wave (setf (symbol-value 'atty/user::*waved*) t))")
    (is-true (mux:load-user-init file))
    (is-true (gethash "wave" mux:*commands*))
    (mux:run-command "wave" nil)
    (is-true (symbol-value (find-symbol "*WAVED*" '#:atty/user)))
    (remhash "wave" mux:*commands*)))

(test a-broken-init-file-is-said-kept-and-passed-over
  (with-an-init-file (file "(this-is-not-anything)")
    (let ((said (with-output-to-string (*error-output*)
                  (is (null (mux:load-user-init file))))))
      (is (search "did not load" said))
      (is (search "did not load" mux:*init-error*)))
    (is (null (mux:load-user-init "/tmp/atty-there-is-no-such-file.lisp")))
    (is (null mux:*init-error*) "a missing file is not a problem")))

(test what-the-server-has-to-say-goes-to-the-first-client-only
  (with-a-server-here (server path)
    (mux:add-session server "sleep 30" :name "work" :rows 6 :cols 30)
    (push (list "in the server, init.lisp did not load: no" :warning) (mux::server-notes server))
    (let ((first-in (a-wire-to path))
          (second-in (a-wire-to path)))
      (say-to first-in (list :open "work" nil nil 6 30 t))
      (let ((heard (heard-from server first-in :say)))
        (is (equal '(:say "in the server, init.lisp did not load: no" :warning) heard)))
      (say-to second-in (list :open "work" nil nil 6 30 t))
      (heard-from server second-in :hello)
      (step-until server (lambda () nil) 1/4)
      (is (null (find :say (heard-back second-in) :key #'first))
          "the second client was told what the first already was")
      (mux:wire-close first-in)
      (mux:wire-close second-in))))

(test the-server-reloads-the-init-file-when-asked-and-says-how-it-went
  (with-a-server-here (server path)
    (mux:add-session server "sleep 30" :name "work" :rows 6 :cols 30)
    (let ((wire (a-wire-to path)))
      (say-to wire (list :open "work" nil nil 6 30 t))
      (heard-from server wire :hello)
      (say-to wire (list :reload-init))
      (let ((heard (heard-from server wire :say)))
        (is (stringp (second heard)))
        (is (member (third heard) '(:accent :warning))))
      (mux:wire-close wire))))

(test help-init-lists-every-setting
  (let ((said (with-output-to-string (s) (mux:init-help s))))
    (dolist (row (mux:settings))
      (is (search (format nil ":~(~A~)" (first row)) said)
          "help init does not mention ~S" (first row)))
    (is (search "pane-started" said))))

(test a-load-undoes-what-the-last-one-did
  (with-an-init-home ("(define-key 'scroll-mode \"C-wheel-up\" \"scroll to top\")
                       (defcommand wave (show-note *client* \"hi\" \"there\"))
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

(test the-config-says-the-inits-keys-by-name-and-its-commands
  (with-an-init-home ("(define-key 'scroll-mode \"C-wheel-up\" \"scroll to top\")
                       (undefine-key 'scroll-mode \"q\")
                       (define-key 'scroll-mode \"C-wheel-down\" (lambda () nil))
                       (defcommand wave \"waves\" (show-note *client* \"hi\" \"there\"))")
    (mux:load-user-init)
    (destructuring-bind (tag settings commands keys) (mux::encode-config)
      (is (eq :config tag))
      (is (find :wheel-rows settings :key #'first))
      (is (null (find :restore-command settings :key #'first)) "~S" (find :restore-command settings :key #'first))
      (is (equal '("wave" "waves" nil nil) (find "wave" commands :key #'first :test #'equal)))
      (is (find '("SCROLL-MODE" "C-wheel-up" "scroll to top") keys :test #'equal))
      (is (find '("SCROLL-MODE" "q" nil) keys :test #'equal))
      (let ((bare (find "C-wheel-down" keys :key #'second :test #'equal)))
        (is (stringp (third bare)) "a key bound to a function has no name: ~S" bare)
        (is (find (third bare) commands :key #'first :test #'equal))
        (is-true (fourth (find (third bare) commands :key #'first :test #'equal)))))))

(test a-client-takes-a-config-and-puts-it-back-on-the-next
  (let ((rows mux:+wheel-rows+))
   (unwind-protect
       (progn
         (mux::apply-config '((:wheel-rows 9))
                            '(("wave" "waves" nil nil))
                            '(("SCROLL-MODE" "C-wheel-up" "wave") ("NO-SUCH-MODE" "x" "wave")))
         (is (eql 9 mux:+wheel-rows+))
         (is (functionp (gethash "wave" mux:*commands*)))
         (is (equal "waves" (mux::command-doc "wave")))
         (is (equal "wave" (atty/mode:handler-name (atty/mode:lookup-key "C-wheel-up" 'mux::scroll-mode))))
         (mux::apply-config nil nil nil)
         (is (eql rows mux:+wheel-rows+))
         (is (null (gethash "wave" mux:*commands*)))
         (is (null (atty/mode:lookup-key "C-wheel-up" 'mux::scroll-mode))))
    (init-forgotten))))

(test the-server-tells-a-client-the-config-when-it-attaches-and-after-a-reload
  (with-an-init-home ("(defcommand wave (show-note *client* \"hi\" \"there\"))")
    (mux:load-user-init)
    (with-a-server-here (server path)
      (mux:add-session server "sleep 30" :name "work" :rows 6 :cols 30)
      (let ((wire (a-wire-to path)))
        (say-to wire (list :open "work" nil nil 6 30 t))
        (let ((config (heard-from server wire :config)))
          (is (find "wave" (third config) :key #'first :test #'equal)))
        (write-init "(defcommand shrug (show-note *client* \"eh\" \"well\"))")
        (say-to wire (list :reload-init))
        (let ((config (heard-from server wire :config)))
          (is (find "shrug" (third config) :key #'first :test #'equal))
          (is (null (find "wave" (third config) :key #'first :test #'equal))))
        (mux:wire-close wire)))))

(test a-command-from-the-init-file-runs-in-the-server-and-its-note-comes-back
  (with-an-init-home ("(defcommand wave (show-note *client* \"hi\" \"there\" :face :accent))")
    (mux:load-user-init)
    (with-a-server-here (server path)
      (mux:add-session server "sleep 30" :name "work" :rows 6 :cols 30)
      (let ((wire (a-wire-to path)))
        (say-to wire (list :open "work" nil nil 6 30 t))
        (heard-from server wire :hello)
        (say-to wire (list :command "wave"))
        (is (equal '(:note "hi" "there" :accent) (heard-from server wire :note)))
        (say-to wire (list :command "detach"))
        (is (equal "no command" (second (heard-from server wire :note))))
        (mux:wire-close wire)))))

(test the-server-says-its-settings-as-they-are-now
  (with-an-init-home ("(configure :wheel-rows 8)")
    (mux:load-user-init)
    (with-a-server-here (server path)
      (let ((wire (a-wire-to path)))
        (say-to wire (list :settings))
        (let ((row (find :wheel-rows (second (heard-from server wire :settings)) :key #'first)))
          (is (equal "8" (second row)))
          (is (equal "3" (third row))))
        (mux:wire-close wire)))))
