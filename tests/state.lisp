(in-package #:atty/test)

(def-suite state :in all)
(in-suite state)

;;; Every test here keeps its state under a directory of its own, so nothing
;;; touches anybody's real ~/.local/state.

(defmacro with-state-home ((dir) &body body)
  `(let* ((mux:*state-home* (format nil "~Aatty-state-~D-~D/" (uiop:temporary-directory)
                                    (sb-posix:getpid) (random 1000000)))
          (,dir (uiop:ensure-directory-pathname mux:*state-home*)))
     (unwind-protect (progn ,@body)
       (ignore-errors (uiop:delete-directory-tree ,dir :validate t)))))

(defun coloured-pane (lines &key (rows 3) (cols 8) (command "sh"))
  "A pane that has had LINES numbered lines written to it, every other one red,
so most of them are behind the screen."
  (let ((pane (mux:make-pane command :rows rows :cols cols)))
    (term:term-process-output
     (mux:pane-term pane)
     (with-output-to-string (s)
       (dotimes (i lines)
         (unless (zerop i) (format s "~C~C" #\Return #\Newline))
         (if (oddp i)
             (format s "~C[31mline ~D~C[0m" #\Escape i #\Escape)
             (format s "line ~D" i)))))
    pane))

(defun rows-behind (pane)
  "Every scrollback row of PANE as (text . fg-of-first-cell), oldest first."
  (mux::with-term (term pane)
    (loop :for i :below (term:term-scrollback-size term)
          :for row := (term:term-scrollback-row term i)
          :collect (cons (string-right-trim " " (term:row-chars row))
                         (and (term:row-face row 0) (term:face-fg (term:row-face row 0)))))))

(test a-row-round-trips-with-its-faces-and-without-its-blank-tail
  (let* ((row (term:make-row 10))
         (red (term:make-face :fg 1 :bold t))
         (faces (make-hash-table :test 'equal))
         (table (make-array 4 :adjustable t :fill-pointer 1 :initial-element nil)))
    (replace (term:row-chars row) "ab cd")
    (setf (term:row-face row 3) red (term:row-face row 4) red)
    (let ((said (mux::encode-row row faces table)))
      (is (equal '(10 ((0 . "ab ") (1 . "cd"))) said) "~S" said)
      (let ((back (mux::decode-row said (map 'simple-vector #'mux:decode-face table))))
        (is (eql 10 (term:row-width back)))
        (is (equal "ab cd     " (term:row-chars back)))
        (is (null (term:row-face back 0)))
        (is (term:face-equal red (term:row-face back 4)))))))

(defun shown-row (pane y)
  (string-right-trim " " (mux::with-term (term pane) (term:term-dump-row-string term y))))

(test a-pane-comes-back-showing-what-it-showed-with-the-rule-under-it
  (with-state-home (dir)
    (let* ((pane (coloured-pane 12 :rows 3 :cols 8))
           (now 5000)
           (form (mux:encode-pane pane now)))
      (mux::pane-resize pane 3 12)
      (setf form (mux:encode-pane pane now))
      (let ((back (mux:decode-pane form now)))
        (is (eql (mux:pane-id pane) (mux:pane-id back)))
        (is (eql 12 (mux::with-term (term back) (term:term-width term))))
        (let ((rows (rows-behind back)))
          ;; nine were behind; the three shown and the rule are four lines on
          ;; a screen of three, so two more went behind on the way
          (is (eql 11 (length rows)) "~S" rows)
          (is (equal "line 0" (car (first rows))))
          (is (eql 1 (cdr (second rows))) "the red did not come back: ~S" (second rows))
          (is (null (cdr (first rows))))
          (is (equal "line 10" (car (nth 10 rows)))))
        (is (equal "line 11" (shown-row back 0)) "the last line shown is not still shown")
        (is (search "restored" (shown-row back 1)) "no rule under it: ~S" (shown-row back 1))
        (is (equal "" (shown-row back 2)) "the program's first line has nowhere clean to go")
        (is (eql 2 (mux::with-term (term back) (term:term-cursor-y term))))
        (is (eql 0 (mux::with-term (term back) (term:term-cursor-x term))))))))

(test a-pane-on-the-alt-screen-saves-its-main-screen-and-what-was-over-it
  (let ((pane (coloured-pane 5 :rows 3 :cols 8)))
    (mux::with-term (term pane) (term:term-process-output term (format nil "~C[?1049hfull scr" #\Escape)))
    (let* ((form (mux:encode-pane pane 100))
           (screen (getf (nthcdr 2 form) :screen))
           (over (getf (nthcdr 2 form) :over)))
      (is (getf (nthcdr 2 form) :alt-screen))
      (is (search "line 4" (format nil "~{~A~}" (mapcar #'cdr (second (first (last screen))))))
          "the main screen was not what was saved: ~S" screen)
      (is (null (search "full scr" (format nil "~S" screen))))
      (is (eql 1 (length over)) "~S" over)
      (is (search "full scr" (format nil "~S" over)) "what was in front was not saved: ~S" over)
      ;; and it comes back in front: the main screen, then the program's
      ;; screen, then the rule
      (let ((back (mux:decode-pane form 200)))
        (is (equal "line 4" (car (first (last (rows-behind back))))) "~S" (rows-behind back))
        (is (equal "full scr" (shown-row back 0)) "~S" (shown-row back 0))
        ;; the rule, as much of it as eight columns hold
        (is (search "resto" (shown-row back 1)) "~S" (shown-row back 1))))))

(test the-log-and-the-agents-history-are-saved-as-ages-and-come-back-as-moments
  (let* ((pane (coloured-pane 2))
         (now 10000))
    (mux::pane-push-log pane 9000 '(:client 1 "/dev/ttys1") :keys 4)
    (mux::pane-push-log pane 9500 '(:cli) :prompt "run it")
    (let* ((form (mux:encode-pane pane now))
           (later 50000)
           (back (mux:decode-pane form later)))
      (is (equal '(500 (:cli) :prompt "run it") (subseq (first (getf (nthcdr 2 form) :log)) 0 4)))
      (is (eql 49500 (first (first (mux::pane-log back)))))
      (is (eql 49000 (first (second (mux::pane-log back))))))))

(test a-claude-pane-comes-back-as-a-shell-that-picks-claude-up-unless-asked-otherwise
  (with-setting-kept (:restore-command)
    (let* ((pane (mux:make-pane "claude" :rows 3 :cols 100))
           (form (progn (setf (mux::pane-pending-prompt pane) '("go on" (:cli)))
                        (mux:encode-pane pane 100))))
      (mux:configure :restore-command :shell)
      (let ((back (mux:decode-pane form 200)))
        (is (mux::shell-command-p (mux:pane-command back)))
        (is (equal '("claude --continue" t) (mux::pane-typed back)))
        (is (equal '("go on" (:cli)) (mux::pane-pending-prompt back)))
        (is (null (mux::pane-log back)))
        (is (search "claude --continue started again" (shown-row back 0)) "~S" (shown-row back 0)))
      (mux:configure :restore-command :same)
      (let ((back (mux:decode-pane form 200)))
        (is (equal "claude" (mux:pane-command back)))
        (is (equal '("go on" (:cli)) (mux::pane-pending-prompt back)))
        (is (null (mux::pane-log back))))
      (mux:configure :restore-command (lambda (form) (declare (ignore form)) "sh -c true"))
      (is (equal "sh -c true" (mux:pane-command (mux:decode-pane form 200)))))))

(defun pane-in-front-of (words &key alt (directory "/tmp") (cols 60))
  "A shell pane saved with WORDS in front of it, on the alt screen when ALT."
  (let ((pane (coloured-pane 4 :rows 3 :cols cols)))
    (when alt
      (mux::with-term (term pane)
        (term:term-process-output term (format nil "~C[?1049hfull scr" #\Escape))))
    (setf (mux::pane-front pane) (list :words words :directory directory)
          (mux::pane-here pane) "/tmp")
    (mux:encode-pane pane 100)))

(test what-was-in-front-and-where-the-shell-was-are-saved
  (let ((form (pane-in-front-of '("vim" "my notes.org") :alt t)))
    (is (equal "/tmp" (getf (nthcdr 2 form) :here)))
    (is (equal '(:alt t :words ("vim" "my notes.org") :directory "/tmp") (getf (nthcdr 2 form) :front)))))

(test what-was-in-front-is-kept-through-a-restore-until-it-runs-again
  (with-setting-kept (:restore-programs)
    (mux:configure :restore-programs :full-screen)
    (let* ((once (mux:decode-pane (pane-in-front-of '("vim" "x") :alt t) 200))
           (form (mux:encode-pane once 300))
           (twice (mux:decode-pane form 400)))
      (is (equal '("vim" "x") (getf (getf (nthcdr 2 form) :front) :words)) "~S" (getf (nthcdr 2 form) :front))
      (is (equal '("vim x" t) (mux::pane-typed twice)))
      (setf (mux::pane-kept-front twice) nil)
      (is (null (getf (nthcdr 2 (mux:encode-pane twice 500)) :front))))))

(test a-pane-saved-before-fronts-were-brings-back-what-its-programs-say
  (with-setting-kept (:restore-programs)
    (mux:configure :restore-programs :full-screen)
    (let* ((form (mux:encode-pane (coloured-pane 4 :rows 3 :cols 60) 100))
           (old (list* (first form) (second form)
                       :programs '("claude --dangerously-skip-permissions")
                       (let ((rest (copy-list (nthcdr 2 form))))
                         (remf rest :front) (remf rest :here) (remf rest :programs) rest)))
           (back (mux:decode-pane old 200)))
      (is (equal '("claude --dangerously-skip-permissions --continue" t) (mux::pane-typed back)))
      (let ((none (list* (first form) (second form)
                         :programs '("/bin/zsh" "git status")
                         (let ((rest (copy-list (nthcdr 2 form)))) (remf rest :programs) rest))))
        (is (null (mux::pane-typed (mux:decode-pane none 200))))))))

(test a-full-screen-program-comes-back-in-front-and-starts-again
  (with-setting-kept (:restore-programs)
    (mux:configure :restore-programs :full-screen)
    (let ((back (mux:decode-pane (pane-in-front-of '("vim" "my notes.org") :alt t :cols 30) 200)))
      (flet ((cover-row (y) (string-right-trim " " (term:term-dump-row-string (mux::pane-cover back) y)))
             (say (text) (mux::with-term (term back) (term:term-process-output term text))))
        (is (equal '("vim 'my notes.org'" t) (mux::pane-typed back)))
        (is (equal "/tmp" (mux::pane-here back)))
        (is-false (mux::with-term (term back) (term:term-in-alt-screen term)))
        (is (equal "full scr" (cover-row 0)) "~S" (cover-row 0))
        (is (eq t (mux::pane-covered back)))
        (say (format nil "$ vim~C~C" #\Return #\Newline))
        (is (equal "full scr" (cover-row 0)))
        (is (search "full scr" (format nil "~S" (getf (nthcdr 2 (mux:encode-pane back 300)) :over)))
            "the picture in front was not saved while it was in front")
        (is (getf (nthcdr 2 (mux:encode-pane back 300)) :alt-screen))
        (say (format nil "~C[?1049h~C[2J" #\Escape #\Escape))
        (is-false (mux::pane-drew-over-p back) "a blank alt screen took the picture's place")
        (say "vim")
        (is-true (mux::pane-drew-over-p back))
        (say (format nil "~C[?1049l$ " #\Escape))
        (is (equal '("$ vim" "$") (list (shown-row back 1) (shown-row back 2)))
            "~S" (loop :for y :below 3 :collect (shown-row back y)))
        (is (search "restored" (shown-row back 0)) "~S" (shown-row back 0))))))

(test a-command-that-had-no-screen-of-its-own-is-typed-and-not-entered
  (with-setting-kept (:restore-programs)
    (mux:configure :restore-programs :full-screen)
    (let ((back (mux:decode-pane (pane-in-front-of '("make" "test")) 200)))
      (is (equal '("make test" nil) (mux::pane-typed back)))
      (is-false (mux::with-term (term back) (term:term-in-alt-screen term)))
      (is (null (mux::pane-covered back))))
    (mux:configure :restore-programs :all)
    (is (equal '("make test" t) (mux::pane-typed (mux:decode-pane (pane-in-front-of '("make" "test")) 200))))
    (mux:configure :restore-programs :typed)
    (is (equal '("vim x" nil)
               (mux::pane-typed (mux:decode-pane (pane-in-front-of '("vim" "x") :alt t) 200))))))

(test a-program-somewhere-else-is-gone-to-first
  (with-setting-kept (:restore-programs)
    (mux:configure :restore-programs :all)
    (let ((home (namestring (user-homedir-pathname))))
      (is (equal (list (format nil "cd ~A; ls" (mux::shell-quote home)) t)
                 (mux::pane-typed (mux:decode-pane (pane-in-front-of '("ls") :directory home) 200)))))))

(test a-resume-command-is-found-by-the-agent-it-was
  (is (equal "claude --continue" (mux::resume-command "node /usr/lib/cli.js" "claude")))
  (is (equal "claude --continue" (mux::resume-command "/opt/bin/claude --model x")))
  (is (null (mux::resume-command "vim"))))

(test claude-comes-back-to-its-own-session-with-the-flags-it-was-started-with
  (let ((home (format nil "~Aatty-claude-~D/" (uiop:temporary-directory) (random 1000000))))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "projects/-tmp-x/" home))
           (dolist (id '("abc-1" "abc-2"))
             (with-open-file (out (merge-pathnames (format nil "projects/-tmp-x/~A.jsonl" id) home)
                                  :direction :output)
               (write-line "{}" out)))
           (sb-posix:setenv "CLAUDE_CONFIG_DIR" home 1)
           (is (equal "claude --dangerously-skip-permissions --resume abc-1"
                      (mux::session-command '(:words ("claude" "--dangerously-skip-permissions" "--continue")
                                              :session "abc-1"))))
           (is (equal "claude --resume abc-2"
                      (mux::session-command '(:words ("claude" "--resume" "old" "-c") :session "abc-2"))))
           (is (equal "claude --dangerously-skip-permissions"
                      (mux::session-command '(:words ("claude" "--dangerously-skip-permissions")
                                              :session "never-said-anything")))))
      (sb-posix:unsetenv "CLAUDE_CONFIG_DIR")
      (ignore-errors (uiop:delete-directory-tree (pathname home) :validate t))))
  (is (equal "claude --continue" (mux::session-command '(:words ("claude")))))
  (is (null (mux::session-command '(:words ("vim" "x"))))))

(test claudes-session-is-read-from-what-it-says-about-itself
  (let ((home (format nil "~Aatty-claude-~D/" (uiop:temporary-directory) (random 1000000))))
    (unwind-protect
         (progn
           (ensure-directories-exist (merge-pathnames "sessions/" home))
           (with-open-file (out (merge-pathnames "sessions/4242.json" home) :direction :output)
             (write-string "{\"pid\":4242,\"sessionId\":\"s-77\",\"cwd\":\"/tmp\"}" out))
           (sb-posix:setenv "CLAUDE_CONFIG_DIR" home 1)
           (is (equal "s-77" (mux::claude-session 4242)))
           (is (equal "s-77" (getf (mux::front-with-session '(:words ("claude") :pid 4242)) :session)))
           (is (null (getf (mux::front-with-session '(:words ("vim") :pid 4242)) :session))))
      (sb-posix:unsetenv "CLAUDE_CONFIG_DIR")
      (ignore-errors (uiop:delete-directory-tree (pathname home) :validate t)))))

(test a-pane-is-saved-at-once-when-what-is-in-front-changes
  (let ((pane (mux:make-pane "sh")))
    (setf (mux::pane-saved-at pane) 1000
          (mux::pane-touched pane) 1500
          (mux::pane-moved-at pane) 1500)
    (is (not (mux::save-due-p pane 1600)))
    (setf (mux::pane-save-soon pane) t)
    (is (mux::save-due-p pane 1600))))

(test an-old-pane-file-comes-back-as-it-did
  (let* ((form (mux:encode-pane (coloured-pane 4 :rows 3 :cols 40) 100))
         (old (cons (first form) (cons (second form)
                                       (let ((rest (copy-list (nthcdr 2 form))))
                                         (remf rest :here) (remf rest :front) rest))))
         (back (mux:decode-pane old 200)))
    (is (null (mux::pane-typed back)))
    (is (search "restored" (shown-row back 1)) "~S" (shown-row back 1))))

(defun server-with-windows (server)
  "Two sessions; the first with three windows, the second one shown by a
terminal on it, splits in it, a zoom in the third. Answers the first session
and the terminal."
  (let* ((one (mux:add-session server "sleep 30" :name "work" :rows 17 :cols 40))
         (two (mux:add-session server "sleep 30" :name "other" :rows 17 :cols 40))
         (w (viewer one :rows 17 :cols 40 :tty "/dev/ttys001")))
    (declare (ignore two))
    (mux:watcher-add-window w)
    (mux:watcher-split w :across)
    (mux:watcher-split w :down)
    (mux:watcher-add-window w)
    (mux::session-rename-window one (mux:session-nth-window one 3) "third")
    (setf (mux:watcher-zoomed w) (mux:watcher-focus w))
    (mux:watcher-select-window w 2)
    (values one w)))

(test the-tree-round-trips-window-for-window
  (with-state-home (dir)
    (with-stepped-server (server path)
      (multiple-value-bind (one w) (server-with-windows server)
       (let* ((addresses (loop :for s :in (mux::server-sessions server)
                              :append (mapcar (lambda (p) (mux:pane-address-of s p))
                                              (mux:session-panes s))))
             (tree (mux:encode-tree server))
             (panes (make-hash-table)))
        (dolist (s (mux::server-sessions server))
          (dolist (p (mux:session-panes s)) (setf (gethash (mux:pane-id p) panes) p)))
        (let* ((forms (getf (nthcdr 2 tree) :sessions))
               (back (mapcar (lambda (form) (mux::decode-session server form panes)) forms)))
          (is (equal '("work" "other") (mapcar #'mux::session-name back)))
          (let* ((b (first back))
                 (seen (cdr (assoc "/dev/ttys001" (mux::session-seeds b) :test #'equal))))
            (is-true seen "what the terminal showed was not kept")
            (is (eql 3 (length (mux:session-windows b))))
            (is (eql 2 (mux:window-number b (mux::view-shown-window b seen))))
            (is (equal "third" (mux:window-label (mux:session-nth-window b 3))))
            (is (equal (mapcar (lambda (w) (mux::encode-layout (mux:window-layout w)))
                               (mux:session-windows one))
                       (mapcar (lambda (w) (mux::encode-layout (mux:window-layout w)))
                               (mux:session-windows b))))
            (is (eq (mux:watcher-focus w) (mux::view-focus-in b seen)))
            (is-true (mux::view-zoomed-in b seen (mux:session-nth-window b 3)))
            (is (eq (mux::view-zoomed-in one (mux:watcher-view w) (mux:session-nth-window one 3))
                    (mux::view-zoomed-in b seen (mux:session-nth-window b 3))))
            (is (equal addresses
                       (loop :for s :in back
                             :append (mapcar (lambda (p) (mux:pane-address-of s p))
                                             (mux:session-panes s))))))))))))

(test a-file-is-whole-or-not-there-and-a-strange-version-is-moved-aside
  (with-state-home (dir)
    (let ((path (merge-pathnames "one.sexp" dir)))
      (ensure-directories-exist path)
      (is-true (mux:write-form-atomically path '(:atty-state 1 :sessions nil)))
      (is (equal '(:ok (:atty-state 1 :sessions nil))
                 (multiple-value-bind (form status) (mux:read-state-file path :atty-state)
                   (list status form))))
      (is (null (directory (merge-pathnames "*.tmp-*" dir))) "a tmp file was left")
      ;; a write that cannot finish leaves the old file
      (is (null (let ((*error-output* (make-broadcast-stream)))
                  (mux:write-form-atomically (merge-pathnames "nowhere/deeper/x.sexp" dir) '(:x)))))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (write-string "(:atty-state 1 :sessions (" s))
      (is (eq :truncated (nth-value 1 (mux:read-state-file path :atty-state))))
      (with-open-file (s path :direction :output :if-exists :supersede)
        (write-string "(:atty-state 99 :sessions nil)" s))
      (is (eq :wrong-version (nth-value 1 (mux:read-state-file path :atty-state))))
      (is (null (probe-file path)))
      (is-true (probe-file (format nil "~A.unread-99" (namestring path))) "it was not moved aside")
      (is (eq :missing (nth-value 1 (mux:read-state-file path :atty-state)))))))

(test what-a-server-held-comes-back-on-another-and-its-panes-run-again
  (with-state-home (dir)
    (let ((path (socket-path)) ids)
      (let ((server (mux:make-server path)))
        (unwind-protect
             (multiple-value-bind (one w) (server-with-windows server)
               (mux::with-term (term (mux:watcher-focus w)) (term:term-process-output term
                                         (format nil "typed here~C~%and more" #\Return)))
               (setf ids (mapcar #'mux:pane-id (mux:session-panes one)))
               (mux:save-all server))
          (mux:server-close server)))
      (ignore-errors (delete-file path))
      (let ((server (mux:make-server path)))
        (unwind-protect
             (let ((sessions (mux:restore-state server)))
               (is (eql 2 (length sessions)))
               (is (equal '("work" "other") (mapcar #'mux::session-name sessions)))
               (let ((one (first sessions)))
                 (is (equal ids (mapcar #'mux:pane-id (mux:session-panes one))))
                 (is (>= mux::*panes-made* (reduce #'max ids)))
                 (let ((seen (cdr (assoc "/dev/ttys001" (mux::session-seeds one) :test #'equal))))
                   (is (eql 2 (mux:window-number one (mux::view-shown-window one seen)))))
                 (let* ((pane (mux::view-focus-in one (cdr (assoc "/dev/ttys001" (mux::session-seeds one)
                                                                  :test #'equal))))
                        (dump (mux::with-term (term pane) (term:term-dump-to-string term)))
                        (behind (mapcar #'car (rows-behind pane))))
                   ;; a pane three rows tall: the first line has gone behind
                   ;; by the time the rule is under the last
                   (is (find "typed here" behind :test #'string=) "~S ~S" behind dump)
                   (is (search "and more" dump) "~S" dump)
                   (is (search "restored" dump) "no rule: ~S" dump))
                 (is (every #'mux:pane-started (mux:session-panes one)) "the programs were not started")
                 (is (null (mux::server-notes server)) "~S" (mux::server-notes server))))
          (mux:server-close server)
          (ignore-errors (delete-file path)))))))

(test a-pane-whose-file-is-broken-comes-back-empty-and-the-rest-whole
  (with-state-home (dir)
    (let ((path (socket-path)) broken)
      (let ((server (mux:make-server path)))
        (unwind-protect
             (let ((one (server-with-windows server)))
               (setf broken (mux:pane-id (first (mux:session-panes one))))
               (mux:save-all server))
          (mux:server-close server)))
      (ignore-errors (delete-file path))
      (let ((file (mux::pane-file (mux::state-dir (file-namestring path)) broken)))
        (with-open-file (s file :direction :output :if-exists :supersede)
          (write-string "(:atty-pane 1 :id" s)))
      (let ((server (mux:make-server path)))
        (unwind-protect
             (let ((sessions (mux:restore-state server)))
               (is (eql 2 (length sessions)))
               (let ((pane (find broken (mux:session-panes (first sessions)) :key #'mux:pane-id)))
                 (is-true pane "the broken pane is gone from the tree")
                 (is (search "unreadable" (shown-row pane 0)) "~S" (shown-row pane 0)))
               (is (search "could not be read" (first (first (mux::server-notes server))))))
          (mux:server-close server)
          (ignore-errors (delete-file path)))))))

(test a-pane-is-owed-a-save-when-quiet-or-when-it-has-been-too-long
  (with-setting-kept (:save-quiet-after)
    (with-setting-kept (:save-at-most-every)
      (mux:configure :save-quiet-after 5000 :save-at-most-every 60000)
      (let ((pane (mux:make-pane "sleep 30")))
        (setf (mux::pane-saved-at pane) 1000 (mux::pane-moved-at pane) 2000)
        (is (null (mux::save-due-p pane 3000)) "still writing")
        (is-true (mux::save-due-p pane 7000) "quiet for five seconds")
        (setf (mux::pane-moved-at pane) 60500)
        (is-true (mux::save-due-p pane 61001) "a minute since the last save")
        (setf (mux::pane-saved-at pane) 61001)
        (is (null (mux::save-due-p pane 70000)) "nothing changed since")))))

(test the-tree-is-written-when-it-changes-and-closed-panes-files-go-with-it
  (with-state-home (dir)
    (with-stepped-server (server path)
      (let* ((one (server-with-windows server))
             (state (mux::state-dir (file-namestring path))))
        (is-true (mux:save-tree server))
        (is (null (mux:save-tree server)) "written again with nothing changed")
        (mux:save-all server)
        (let ((gone (first (mux:session-panes one))))
          (is-true (probe-file (mux::pane-file state (mux:pane-id gone))))
          (mux::session-close-pane one gone)
          (is-true (mux:save-tree server))
          (is (null (probe-file (mux::pane-file state (mux:pane-id gone))))
              "the closed pane's file is still there"))))))

(test what-was-saved-is-listed-when-nothing-is-running-and-can-be-moved-aside
  (with-state-home (dir)
    (with-stepped-server (server path)
      (server-with-windows server)
      (mux:save-all server)
      (let ((name (file-namestring path)))
        (let ((rows (mux:saved-sessions name)))
          (is (equal '("work" 3 5) (subseq (first rows) 0 3)) "~S" rows)
          (is (integerp (fourth (first rows)))))
        (is-true (mux:move-state-aside name))
        (is (null (mux:saved-sessions name)))
        (is-true (directory (merge-pathnames (format nil "atty/~A.fresh-*/" name) dir)))))))

;;; A server that persists: threaded, like WITH-SERVER, but keeping its state
;;; under a temporary home.

(defmacro with-persisting-server ((path dir &key (command "sleep 30")) &body body)
  (let ((thread (gensym "THREAD")) (broke (gensym "BROKE")))
    `(with-state-home (,dir)
       (let* ((,path (socket-path))
              (,broke (make-string-output-stream))
              ;; the binding is this thread's; the server's thread is given
              ;; the value, or it would write to the real state home
              (home mux:*state-home*)
              (,thread (sb-thread:make-thread
                        (lambda ()
                          (let ((*error-output* ,broke)
                                (mux:*state-home* home))
                            (mux:serve ,path ,command :rows 10 :cols 40 :interval 0)))
                        :name "a persisting test server")))
         (unwind-protect
              (progn (until 5 (lambda () (probe-file ,path)))
                     ,@body)
           (stop-server ,path)
           (when (eq :gave-up (sb-thread:join-thread ,thread :timeout 15 :default :gave-up))
             (error "a persisting test server would not stop"))
           (ignore-errors (delete-file ,path))
           (let ((said (get-output-stream-string ,broke)))
             (is (equal "" said) "the server said something broke:~%~A" said)))))))

(test a-serving-server-writes-its-tree-soon-and-everything-when-it-stops
  (with-persisting-server (path dir)
    (let ((state (mux::state-dir (file-namestring path))))
      (is-true (until 5 (lambda () (probe-file (mux::tree-file state))))
               "the tree was not written while the server ran")
      (let ((wire (wire-to path)))
        (say-to wire (list :open "0" nil nil 10 40 t))
        (until 5 (lambda () (find :hello (heard-back wire) :key #'first)))
        (say-to wire (list :run "save" nil))
        (is-true (until 5 (lambda () (find :ran (heard-back wire) :key #'first))))
        (is (eql 1 (length (mux::pane-files state))))
        (mux:wire-close wire))
      (stop-server path)
      (until 5 (lambda () (not (probe-file path))))
      (is (eql 1 (length (mux:saved-sessions (file-namestring path)))))
      (is (eql 1 (length (mux::pane-files state)))))))

(test a-restart-tells-whoever-is-attached-and-leaves-the-state-whole
  (with-persisting-server (path dir)
    (let ((wire (wire-to path))
          (state (mux::state-dir (file-namestring path))))
      (say-to wire (list :open "0" nil nil 10 40 t))
      (until 5 (lambda () (find :hello (heard-back wire) :key #'first)))
      (say-to wire (list :restart))
      ;; a server loaded into a lisp is not a program and cannot start itself
      ;; again; it says so, and whoever asked starts it
      (let ((heard nil))
        (is-true (until 5 (lambda ()
                            (setf heard (append heard (heard-back wire)))
                            (and (find '(:bye :restarting nil) heard :test #'equal)
                                 (find '(:restarting nil) heard :test #'equal))))
                 "heard ~S" heard))
      (is-true (until 5 (lambda () (not (probe-file path)))) "the server did not stop")
      ;; the name goes before the save, so the files follow the socket by a moment
      (is-true (until 5 (lambda () (and (eql 1 (length (mux:saved-sessions (file-namestring path))))
                                        (eql 1 (length (mux::pane-files state))))))
               "the state was not whole: ~S" (mux::pane-files state))
      (mux:wire-close wire))))

(test a-server-asked-to-stop-by-a-signal-saves-on-its-way-out
  (with-persisting-server (path dir)
    (let ((state (mux::state-dir (file-namestring path))))
      (until 5 (lambda () (probe-file (mux::tree-file state))))
      ;; what the handler for a term does, without sending one to the whole
      ;; process the tests run in
      (setf tty:*asked-to-stop* t)
      (is-true (until 5 (lambda () (not (probe-file path)))) "the server did not stop")
      ;; the name goes before the save, so the file follows the socket by a moment
      (is-true (until 5 (lambda () (eql 1 (length (mux::pane-files state)))))
               "the pane was not saved on the way out"))))

(test what-a-server-saved-a-new-server-brings-back-on-the-same-name
  (with-persisting-server (path dir)
    (let ((wire (wire-to path)))
      (say-to wire (list :open "0" nil nil 10 40 t))
      (until 5 (lambda () (find :hello (heard-back wire) :key #'first)))
      (say-to wire (list :keys (format nil "~Cc" mux:+prefix+)))
      (say-to wire (list :run "save" nil))
      (until 5 (lambda () (find :ran (heard-back wire) :key #'first)))
      (mux:wire-close wire))
    (stop-server path)
    (until 5 (lambda () (not (probe-file path))))
    ;; the same name again: a server started where the last one left off
    (let* ((home mux:*state-home*)
           (thread (sb-thread:make-thread
                   (lambda ()
                     (let ((mux:*state-home* home)
                           (*error-output* (make-broadcast-stream)))
                       (mux:serve path "sleep 30" :name "0" :rows 10 :cols 40 :interval 0)))
                   :name "a server come back")))
      (unwind-protect
           (progn
             (is-true (until 10 (lambda () (probe-file path))))
             (let ((wire (wire-to path)))
               (say-to wire (list :sessions))
               (let (heard)
                 (is-true (until 5 (lambda () (setf heard (find :these (heard-back wire) :key #'first)))))
                 (is (eql 2 (length (fifth (first (second heard)))))
                     "the two windows did not come back: ~S" heard))
               (mux:wire-close wire)))
        (stop-server path)
        (sb-thread:join-thread thread :timeout 15 :default :gave-up)
        (ignore-errors (delete-file path))))))

(test a-saved-session-that-is-not-running-can-be-forgotten-and-a-full-save-says-when
  (with-state-home (dir)
    (with-stepped-server (server path)
      (server-with-windows server)
      (let ((name (file-namestring path))
            (state (mux::state-dir (file-namestring path))))
        (mux:save-all server)
        (let ((at (fourth (first (mux:saved-sessions name)))))
          (sleep 1.1)
          (mux:save-all server)
          (is (> (fourth (first (mux:saved-sessions name))) at)
              "a full save with nothing changed did not say when it was"))
        (is (eql 6 (length (mux::pane-files state))))
        (is-true (mux:delete-saved-session "work" name))
        (is (equal '("other") (mapcar #'first (mux:saved-sessions name))))
        (is (eql 1 (length (mux::pane-files state))) "work's pane files are still there")
        (is (null (mux:delete-saved-session "work" name)))
        (is-true (mux:delete-saved-session "other" name))
        (is (null (mux:saved-sessions name)))))))
