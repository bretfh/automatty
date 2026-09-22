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
       (setf mux:*init-problem* nil))))

(test an-init-file-binds-a-key-to-a-command-by-name-and-sets-a-setting
  (with-an-init-file (file "(define-key 'scroll-mode \"C-wheel-up\" \"scroll to top\")
                            (configure :wheel-rows 7)")
    (with-setting-kept (:wheel-rows)
      (unwind-protect
           (progn
             (is-true (mux:load-user-init file))
             (is (null mux:*init-problem*))
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
      (is (search "did not load" mux:*init-problem*)))
    (is (null (mux:load-user-init "/tmp/atty-there-is-no-such-file.lisp")))
    (is (null mux:*init-problem*) "a missing file is not a problem")))

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
