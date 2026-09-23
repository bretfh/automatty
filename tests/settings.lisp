(in-package #:atty/test)

(def-suite settings :in all)
(in-suite settings)

(defmacro with-setting-kept ((key) &body body)
  "BODY, with the setting called KEY put back afterwards however it was left."
  (let ((was (gensym "WAS")))
    `(let ((,was (mux:setting ,key)))
       (unwind-protect (progn ,@body)
         (setf (mux:setting ,key) ,was)))))

(test a-setting-is-set-by-name-and-read-back
  (with-setting-kept (:wheel-rows)
    (mux:configure :wheel-rows 5)
    (is (eql 5 mux:+wheel-rows+))
    (is (eql 5 (mux:setting :wheel-rows)))))

(test a-name-nothing-is-called-says-what-there-is
  (signals error (mux:configure :wheel-rowz 5))
  (let ((said (handler-case (progn (mux:configure :wheel-rowz 5) "")
                (error (e) (format nil "~A" e)))))
    (is (search "wheel-rows" said) "the error did not name the settings there are: ~A" said)))

(test a-value-of-the-wrong-kind-is-refused-and-nothing-changes
  (with-setting-kept (:wheel-rows)
    (signals error (mux:configure :wheel-rows "six"))
    (signals error (mux:configure :wheel-rows 0))
    (is (eql 3 mux:+wheel-rows+))))

(test the-prefix-is-a-character-or-a-chord-and-moves-every-key-behind-it
  (with-setting-kept (:prefix)
    (mux:configure :prefix "C-a")
    (is (eql (code-char 1) mux:+prefix+))
    (is (eq #'mux::detach (atty/mode:lookup-key "C-a d" (atty/mode:mode-named 'mux:pane-mode))))
    (is (null (atty/mode:lookup-key "C-b d" (atty/mode:mode-named 'mux:pane-mode)))
        "the old prefix still reaches detach")
    (is (eq #'mux::send-prefix
            (atty/mode:lookup-key "C-a C-a" (atty/mode:mode-named 'mux:pane-mode))))
    (mux:configure :prefix (code-char 2))
    (is (eq #'mux::detach (atty/mode:lookup-key "C-b d" (atty/mode:mode-named 'mux:pane-mode))))
    (signals error (mux:configure :prefix "C-M-x"))))

(test the-theme-is-checked-against-the-themes-there-are
  (let ((was (atty/ui:active)))
    (unwind-protect
         (progn
           (signals error (mux:configure :theme :nonesuch))
           (is (eq was (atty/ui:active)))
           (mux:configure :theme (first (atty/ui:themes)))
           (is (eq (first (atty/ui:themes)) (atty/ui:active))))
      (setf (atty/ui:active) was))))

(test every-setting-is-listed-with-its-line
  (let ((rows (mux:settings)))
    (is (every (lambda (row) (and (keywordp (first row)) (stringp (fourth row)))) rows))
    (is (member :prefix rows :key #'first))
    (is (member :theme rows :key #'first))))

(test a-new-session-takes-the-defaults-for-the-bar-and-the-scrollbars
  (with-setting-kept (:scrollbars-by-default)
    (with-setting-kept (:bar-by-default)
      (mux:configure :scrollbars-by-default nil :bar-by-default nil)
      (with-a-server-here (server path)
        (let ((session (mux:add-session server "sleep 30" :name "work" :rows 6 :cols 30)))
          (is (null (mux::session-scrollbars-p session)))
          (is (null (mux::session-bar-p session))))))))

(test a-hook-that-comes-apart-is-said-and-the-rest-still-run
  (let ((ran nil))
    (flet ((first-one (session) (push (list :one session) ran))
           (broken (session) (declare (ignore session)) (error "no"))
           (last-one (session) (push (list :three session) ran)))
      (unwind-protect
           (progn
             (mux:add-hook 'mux::session-made #'first-one)
             (mux:add-hook 'mux::session-made #'broken)
             (mux:add-hook 'mux::session-made #'last-one)
             (let ((said (with-output-to-string (*error-output*)
                           (is (eql 2 (mux:run-hook 'mux::session-made :the-session))))))
               (is (search "came apart" said) "nothing was said about the hook that broke: ~S" said))
             (is (equal '((:three :the-session) (:one :the-session)) ran)))
        (mux:remove-hook 'mux::session-made #'first-one)
        (mux:remove-hook 'mux::session-made #'broken)
        (mux:remove-hook 'mux::session-made #'last-one)))))

(test a-hook-runs-when-a-session-is-made-and-a-pane-started
  (let ((made nil) (started nil))
    (flet ((a-session (session) (push session made))
           (a-pane (session pane) (push (cons session pane) started)))
      (unwind-protect
           (progn
             (mux:add-hook 'mux::session-made #'a-session)
             (mux:add-hook 'mux::pane-started #'a-pane)
             (with-a-server-here (server path)
               (let ((session (mux:add-session server "sleep 30" :name "work" :rows 6 :cols 30)))
                 (is (equal (list session) made))
                 (is (eq (mux:session-focus session) (cdr (first started))))
                 (mux::session-split session :across)
                 (is (eql 2 (length started))))))
        (mux:remove-hook 'mux::session-made #'a-session)
        (mux:remove-hook 'mux::pane-started #'a-pane)))))
