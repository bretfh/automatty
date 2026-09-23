;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/test)

(def-suite overlay-prompt :in all)
(in-suite overlay-prompt)

(test the-window-prompt-lists-every-session-and-window-asking-first-and-goes-there
  (let ((choices (mux::window-choices
                  '(("todo" 24 80 3 1 1 ((1 "agents" 2 1 t) (2 nil 1 0 nil)))
                    ("lib" 24 80 1 0 0 ((1 nil 1 0 t))))
                  "todo")))
    (is (eql 3 (length choices)))
    (is (equal '("todo" 1) (list (getf (first choices) :session) (getf (first choices) :window)))
        "the window with a question is not first: ~S" choices)
    (is (search "▲ 1" (mux::window-choice-line (first choices))))
    (is (search "◆ here" (mux::window-choice-line (first choices))))
    (is (null (search "◆ here" (mux::window-choice-line (third choices))))
        "another session is marked as where this client is")
    (is (search "lib" (mux::window-choice-line (third choices)))))
  (with-server (path :command "cat" :rows 12 :cols 80)
    (with-seer (seer path :rows 12 :cols 80)
      (type-at seer "in-the-first")
      (is-true (pump seer :want "in-the-first"))
      (type-at seer (format nil "~Cc" mux:+prefix+))
      (is-true (pump seer :until (lambda () (null (search "in-the-first" (seen seer))))))
      (type-at seer (format nil "~C'" mux:+prefix+))
      (is-true (pump seer :want "windows") "the window prompt did not open: ~S" (seen seer))
      (is-true (pump seer :want "› 2 ") "window 2 is not listed: ~S" (seen seer))
      ;; the rows are buttons: a click on window 1's row goes there
      (multiple-value-bind (x y) (where-on seer "› 1 " :from 1)
        (is-true x "no row for window 1: ~S" (seen seer))
        (when x (click-at seer x y)))
      (is-true (pump seer :want "in-the-first") "clicking the row did not go there: ~S" (seen seer)))))

(test the-command-prompt-shows-each-commands-key-and-what-it-does
  (is (search "C-b 3" (mux::command-item-line "split right")))
  (is (search "beside" (mux::command-item-line "split right")))
  (is (string= "PANES" (symbol-name (mux::command-group "split right"))))
  (let ((rows (mux::keys-help-rows)))
    (is (string= "PANES" (symbol-name (third (first rows)))) "the help does not start with the panes: ~S" (first rows))
    (is (find "show queue" rows :key #'first :test #'equal))))

(test the-palette-switches-kinds-by-prefix-and-by-tab
  (with-server (path :command "cat" :rows 16 :cols 100)
    (with-seer (seer path :rows 16 :cols 100)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~C:" mux:+prefix+))
      (is-true (pump seer :want " : ") "the palette did not open: ~S" (seen seer))
      (is-true (pump seer :want "bar off"))
      ;; another kind's prefix, typed first, is that kind
      (type-at seer "#")
      (is-true (pump seer :want "this terminal") "# did not open the clients: ~S" (seen seer))
      ;; TAB is the next kind round
      (type-at seer (string #\Tab))
      (is-true (pump seer :want "hit, the pane follows") "TAB did not open find: ~S" (seen seer))
      (type-at seer (string #\Tab))
      (is-true (pump seer :want "bar off") "TAB did not come round to the commands: ~S" (seen seer))
      ;; a click on a tab opens it too
      (multiple-value-bind (x y) (where-on seer "clients")
        (is-true x)
        (when x (click-at seer x y)))
      (is-true (pump seer :want "this terminal") "clicking the tab did not open the clients: ~S" (seen seer))
      (type-at seer (string (code-char 27)))
      (pump seer :seconds 1/4))))

(test the-windows-kind-previews-the-panes-of-the-window-chosen
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :window 1 :at 0 :label "impl" :kind "claude-code"
                        :state :blocked :for 42000 :asks '(:subject "Bash command" :options ((1 "Yes"))))
                  (list :session "todo" :id 4 :window 1 :at 1 :says "ctl" :kind "shell" :known nil
                        :doing "atty agent wait")))
         (tree (mux::window-preview '(:session "todo" :window 1 :label "agents") client))
         (screen (painted tree 70 4))
         (all (format nil "~{~A~%~}" (loop :for y :below 4 :collect (shown screen y)))))
    (is (search "todo › 1 agents" all) "~A" all)
    (is (search "▲  1 impl claude-code  Bash command" all) "~A" all)
    (is (search "·  2 ctl shell  atty agent wait" all) "~A" all)))

(test find-lists-its-hits-as-you-type-and-the-pane-follows-the-one-chosen
  (with-server (path :command "cat" :rows 14 :cols 90)
    (with-seer (seer path :rows 14 :cols 90)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~{line-~D~%~}" (loop :for i :from 1 :to 40 :collect i)))
      (is-true (pump seer :want "line-40"))
      (type-at seer (format nil "~C/" mux:+prefix+))
      (is-true (pump seer :want "hit, the pane follows") "find did not open: ~S" (seen seer))
      (type-at seer "line-1")
      (is-true (pump seer :want "of 22" :seconds 3) "the hits are not counted: ~S" (seen seer))
      (is-true (pump seer :want "line-19") "the hits are not listed: ~S" (seen seer))
      (is-true (pump seer :want "↓") "the pane did not scroll to the newest hit: ~S" (seen seer))
      ;; up is the next older hit: the pane follows
      (type-at seer (format nil "~C[A" #\Escape))
      (is-true (pump seer :want "2 of 22" :seconds 3) "the pane did not follow the choice: ~S" (seen seer))
      ;; C-RET copies the chosen hit's line
      (type-at seer (format nil "~C[13;5u" #\Escape))
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~C/" mux:+prefix+))
      (is-true (pump seer :want "line-1") "the last find is not offered again: ~S" (seen seer))
      (type-at seer (string (code-char 27)))
      (pump seer :seconds 1/4)
      (type-at seer "q")
      (pump seer :seconds 1/4))))
