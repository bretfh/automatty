(in-package #:atty/test)

(def-suite chrome :in all)
(in-suite chrome)

(defun painted (tree cols rows)
  "TREE laid and painted on a screen COLS by ROWS; answers the screen."
  (let ((screen (tty:make-screen :width cols :height rows)))
    (cells:draw tree (tty:screen-grid screen) cols rows)
    screen))

(test a-header-band-has-its-title-at-the-left-and-the-way-out-at-the-right
  (let* ((tree (mux::header-band "needs you" :path (mux::path "todo" "2 impl")))
         (screen (painted tree 60 1))
         (line (shown screen 0)))
    (is (eql 1 (search "needs you" line)) "~S" line)
    (is (search "todo › 2 impl" line) "~S" line)
    (is (eql 57 (position #\✕ line)) "the close is not at the right edge: ~S" line)
    (is (eq :close (mux::bar-button-runs (mux::button-at tree 0 57)))
        "the ✕ is not a button that closes")))

(test a-key-cap-and-a-hint-are-buttons-that-run-what-they-say
  (let* ((tree (atty/ui:row :spacing 1
                            (mux::keycap "1" "Yes" :runs '(:answer "todo" 4 1))
                            (mux::hint "RET" "go" :runs "queue goto")))
         (screen (painted tree 40 1)))
    (is (search " 1 Yes " (shown screen 0)))
    (is (equal '(:answer "todo" 4 1) (mux::bar-button-runs (mux::button-at tree 0 2))))
    (is (equal "queue goto" (mux::bar-button-runs (mux::button-at tree 0 10))))))

(test hints-run-their-commands-when-clicked
  (let* ((tree (mux::hints 'atty::queue-mode 'atty::queue-goto "go there" "1-9" "answer"))
         (screen (painted tree 60 1)))
    (is (search "go there" (shown screen 0)))
    (is (equal "queue goto" (mux::bar-button-runs (mux::button-at tree 0 2))))
    (is (null (mux::bar-button-runs (mux::button-at tree 0 20)))
        "a key nothing is bound to runs nothing")))

(test the-thumb-of-a-rail-is-at-the-head-at-the-start-and-at-the-foot-at-the-end
  (is (equal '(0 2) (multiple-value-list (mux::rail-thumb 10 20 100 0))))
  (is (equal '(8 2) (multiple-value-list (mux::rail-thumb 10 20 100 80))))
  (is (equal '(4 2) (multiple-value-list (mux::rail-thumb 10 20 100 40))))
  (is (equal '(0 10) (multiple-value-list (mux::rail-thumb 10 20 20 0)))
      "a view that shows the whole has a thumb the whole track long"))

(test a-rail-lights-only-the-arrow-there-is-somewhere-to-go
  (let* ((r (mux::rail 0 8 40 :upright t))
         (screen (painted (atty/ui:row :align :stretch r) 1 10)))
    (is (char= #\▲ (char-at screen 0 0)))
    (is (char= #\▼ (char-at screen 0 9)))
    (is (not (equal (term:face-fg (face-on screen 0 0)) (term:face-fg (face-on screen 0 9))))
        "at the start the up arrow is dim and the down arrow, with somewhere to go, is lit")
    (is (equal (term:face-fg (face-on screen 0 9)) (term:face-fg (face-on screen 0 1)))
        "the lit end is as bright as the part in view")
    (is (char= #\┃ (char-at screen 0 1)) "the thumb is at the head")
    (is (eq :up (mux::rail-part r 0)))
    (is (eq :thumb (mux::rail-part r 1)))
    (is (eq :below (mux::rail-part r 5)))
    (is (eq :down (mux::rail-part r 9)))))

(test a-rail-lying-down-uses-the-sideways-arrows
  (let* ((r (mux::rail 20 10 40 :upright nil))
         (screen (painted (atty/ui:column :align :stretch r) 12 1))
         (line (shown screen 0)))
    (is (char= #\‹ (char line 0)) "~S" line)
    (is (char= #\› (char line 11)) "~S" line)
    (is (find #\━ line))
    (is (find #\─ line))
    (is (eq :up (mux::rail-part r 0)))
    (is (eq :down (mux::rail-part r 11)))
    (is (eql 0 (mux::rail-at r 1 0)) "the head of the track is the start")
    (is (eql 30 (mux::rail-at r 11 0)) "the foot of the track is as far as there is")))

(test a-rail-with-nothing-past-the-view-is-a-thin-line-and-answers-no-part
  (let* ((r (mux::rail 0 10 10))
         (screen (painted (atty/ui:row :align :stretch r) 1 6)))
    (is (equal '(#\│ #\│ #\│ #\│ #\│ #\│) (loop :for y :below 6 :collect (char-at screen 0 y))))
    (is (null (mux::rail-part r 3)))))

(test a-chosen-gutter-row-is-a-band-the-whole-way-across
  (let* ((tree (atty/ui:column :align :stretch
                               (mux::gutter-row "▶" "one" :selected t)
                               (mux::gutter-row "" "two")))
         (screen (painted tree 30 2)))
    (is (equal "▶ one" (shown screen 0)))
    (is (equal "  two" (shown screen 1)))
    (is (equal (term:face-bg (face-on screen 0 0)) (term:face-bg (face-on screen 29 0)))
        "the chosen row's ground does not reach the edge")
    (is (not (equal (term:face-bg (face-on screen 29 0)) (term:face-bg (face-on screen 29 1))))
        "the other row has no ground")))

(test who-is-said-by-glyph
  (let ((client (mux::%make-client :id 7)))
    (is (equal "◆ here" (mux::format-actor '(:client 7 "/dev/ttys042") client)))
    (is (equal "⌨ ttys051" (mux::format-actor '(:client 9 "/dev/ttys051") client)))
    (is (equal "⌨ client 9" (mux::format-actor '(:client 9 nil) client)))
    (is (equal "⌁ todo:1.2" (mux::format-actor '(:pane "todo:1.2") client)))
    (is (equal "$ cli" (mux::format-actor '(:cli) client)))
    (is (eq :here (mux::actor-face '(:client 7 "/dev/ttys042") client)))
    (is (eq :client (mux::actor-face '(:client 9 "/dev/ttys051") client)))))

(test a-toolbar-holds-its-controls-and-puts-the-rest-at-the-right
  (let* ((tree (mux::toolbar (mux::selector "sort" "oldest first" :runs :sort :key "s")
                             (mux::toggle "every session" t :runs '(:toggle :all) :key "a")
                             :right
                             (mux::keycap "↵" "go" :runs "queue goto")))
         (screen (painted tree 80 1))
         (line (shown screen 0)))
    (is (search "sort ▾  oldest first s" line) "~S" line)
    (is (search "✓  every session a" line) "~S" line)
    (is (> (search "↵ go" line) 60) "the go button is not at the right: ~S" line)
    (is (eq :sort (mux::bar-button-runs (mux::button-at tree 0 3))))))

(test a-card-is-heavy-where-the-cursor-is-and-rounded-elsewhere
  (let* ((plain (mux::card (atty/ui:label " 1 arch ") (atty/ui:label " ◐ ")
                           (list (atty/ui:label "hello"))
                           :width 20 :height 4))
         (cursor (mux::card (atty/ui:label " 2 impl ") nil (list (atty/ui:label "there"))
                            :cursor t :width 20 :height 4
                            :bl (mux::keycap "1" "Yes" :runs '(:option 1))))
         (tree (atty/ui:row :spacing 1 plain cursor))
         (screen (painted tree 41 4)))
    (is (search "╭─ 1 arch " (shown screen 0)))
    (is (search "┏━ 2 impl " (shown screen 0)))
    (is (search " 1 Yes " (shown screen 3)) "the answers are in the bottom border")
    (is (equal '(:option 1) (mux::bar-button-runs (mux::button-at tree 3 24))))))

(test a-sparkline-is-scaled-to-its-own-busiest-cell-and-coloured-by-state
  (let* ((cells (append (make-list 13 :initial-element (cons 0 nil))
                        (list (cons 10 :working) (cons 40 :blocked) (cons 0 :blocked))))
         (screen (painted (mux::spark cells) 16 1))
         (line (shown screen 0)))
    (is (= 16 (length line)) "~S" line)
    (is (char= #\█ (char line 14)) "the busiest cell is the tallest: ~S" line)
    (is (char= #\▃ (char line 13)) "a quarter of the busiest is a quarter tall: ~S" line)
    (is (char= #\▁ (char line 0)) "nothing is the lowest block: ~S" line)
    (is (not (equal (term:face-fg (face-on screen 0 0)) (term:face-fg (face-on screen 15 0))))
        "a cell that asked is lit even with no output")
    (is (not (equal (term:face-fg (face-on screen 13 0)) (term:face-fg (face-on screen 14 0))))
        "working and asking are different colours")))

(test rolling-up-adds-the-output-and-keeps-the-worst-state
  (let* ((a (append (make-list 14 :initial-element (cons 0 nil)) (list (cons 3 :working) (cons 1 :idle))))
         (b (append (make-list 14 :initial-element (cons 0 nil)) (list (cons 2 nil) (cons 0 :blocked))))
         (up (mux::merge-cells (list a b))))
    (is (= 16 (length up)))
    (is (equal '(5 . :working) (nth 14 up)))
    (is (equal '(1 . :blocked) (nth 15 up)) "asking outranks everything else")
    (is (equal '(0 . nil) (first up)))))

(test a-foot-is-a-rail-with-where-the-view-is-and-the-keys-on-it
  (let* ((r (mux::rail 0 3 5 :upright nil :expand 1))
         (screen (painted (mux::foot r "windows 1–3 of 5" (mux::hints 'mux::board-mode "⇥" "fold")) 60 1))
         (line (shown screen 0)))
    (is (search "‹━" line) "~S" line)
    (is (search "─› windows 1–3 of 5" line) "~S" line)
    (is (search "⇥ fold" line) "~S" line)
    (is (char= #\─ (char (string-right-trim " " line) (1- (length (string-right-trim " " line)))))
        "the line closes at the right: ~S" line)))

(test a-well-is-sunk-between-a-dark-edge-and-a-light-one
  (let* ((screen (painted (mux::well (list (atty/ui:label " 22:14 asks") (atty/ui:label " 22:13 typed"))) 20 4)))
    (is (equal (make-string 20 :initial-element #\▔) (shown screen 0)))
    (is (search "22:14 asks" (shown screen 1)))
    (is (equal (make-string 20 :initial-element #\▁) (shown screen 3)))))
