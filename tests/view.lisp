(in-package #:atty/test)

(def-suite view :in all)
(in-suite view)

(defun a-pane (said &key (rows 3) (cols 8))
  (let ((pane (mux:make-pane "true" :rows rows :cols cols)))
    (term:term-process-output (mux:pane-term pane) said)
    pane))

(defun laid (tree screen)
  (cells:draw tree (tty:screen-grid screen)
              (tty:screen-width screen) (tty:screen-height screen))
  screen)

(test two-panes-side-by-side-each-get-half-the-columns
  (let* ((left (a-pane "aaaa" :rows 3 :cols 10))
         (right (a-pane "bbbb" :rows 3 :cols 10))
         (a (mux:pane-view left))
         (b (mux:pane-view right))
         (screen (tty:make-screen :width 20 :height 3))
         (tree (atty/ui:row :align :stretch :spacing 0 a b)))
    (laid tree screen)
    (is (eql 0 (atty/ui:left a)))
    (is (eql 10 (atty/ui:width a)) "the left pane was not given half the columns")
    (is (eql 10 (atty/ui:left b)) "the right pane did not start at the halfway mark")
    (is (eql 10 (atty/ui:width b)))
    (is (eql 3 (atty/ui:height a)) "a stretched row did not give it every row")
    (is (equal "aaaa      bbbb" (string-right-trim " " (shown screen 0))))))

(test the-bar-takes-its-row-off-the-pane-under-it
  (let* ((pane (a-pane "hello" :rows 5 :cols 12))
         (v (mux:pane-view pane))
         (screen (tty:make-screen :width 12 :height 5))
         (tree (atty/ui:column :align :stretch v (atty/ui:label "bar"))))
    (laid tree screen)
    (is (eql 4 (atty/ui:height v))
        "the pane was not given the rows the bar left it")
    (is (eql 0 (atty/ui:top v)))
    (is (equal "hello" (string-right-trim " " (shown screen 0))))
    (is (equal "bar" (string-right-trim " " (shown screen 4)))
        "the bar is not at the foot: ~S" (shown screen 4))))

(test a-pane-is-drawn-where-it-was-put-and-clipped-to-it
  (let* ((pane (a-pane "0123456789" :rows 2 :cols 10))
         (v (mux:pane-view pane))
         (screen (tty:make-screen :width 10 :height 2))
         (tree (atty/ui:row :align :stretch :spacing 0 (atty/ui:gap) v)))
    (laid tree screen)
    (is (eql 5 (atty/ui:left v)) "the two gaps did not split the columns evenly")
    (is (equal "     01234" (shown screen 0))
        "what did not fit was drawn past the edge: ~S" (shown screen 0))))

(test every-pane-in-a-tree-is-found
  (let* ((one (mux:pane-view (a-pane "")))
         (two (mux:pane-view (a-pane "")))
         (tree (atty/ui:column (atty/ui:row one) two (atty/ui:label "bar"))))
    (is (equal (list one two) (mux:views-in tree)))))

(test a-split-lays-its-panes-out-with-a-rule-between-them
  (let* ((one (a-pane "aaa" :rows 3 :cols 20))
         (two (a-pane "bbb" :rows 3 :cols 20))
         (layout (mux:make-split :across (list one two)))
         (screen (tty:make-screen :width 21 :height 3))
         (tree (mux::layout-tree layout)))
    (laid tree screen)
    (is (equal (list one two) (mapcar #'mux:view-pane (mux:views-in tree))))
    (let ((views (mux:views-in tree)))
      (is (eql 11 (atty/ui:left (second views)))
          "the frame around each pane took no column"))))

(test a-pane-put-beside-another-and-taken-out-again
  (let ((one (a-pane "")) (two (a-pane "")) (three (a-pane "")))
    (let ((layout (mux:put-beside one one :across two)))
      (is (equal (list one two) (mux:panes-in layout)))
      (setf layout (mux:put-beside layout two :down three))
      (is (equal (list one two three) (mux:panes-in layout)))
      (setf layout (mux:without-pane layout two))
      (is (equal (list one three) (mux:panes-in layout)))
      (setf layout (mux:without-pane layout one))
      (is (equal (list three) (mux:panes-in layout))
          "a split down to one part did not become that part")
      (is (eq three layout))
      (is (null (mux:without-pane layout three))
          "taking the last pane out left something behind"))))

(test the-focused-pane-is-framed-double-and-every-frame-is-coloured-by-state
  (let* ((one (a-pane "aaa" :rows 3 :cols 20))
         (two (a-pane "bbb" :rows 3 :cols 20))
         (layout (mux:make-split :across (list one two)))
         (screen (tty:make-screen :width 44 :height 5))
         (tree (mux::layout-tree layout two)))
    (agent:agent-hear (mux:pane-agent two) :blocked)
    (agent:agent-look (mux:pane-agent two) (mux:pane-term two) 0 nil)
    (setf tree (mux::layout-tree layout two))
    (laid tree screen)
    (destructuring-bind (frame-one frame-two) (atty/ui:parts tree)
      (is (eq :single (atty/ui:line frame-one)))
      (is (eq :double (atty/ui:line frame-two)) "the focused pane was not framed double")
      (is (eq :state-blocked (atty/ui:face frame-two))
          "a blocked pane's frame was ~S" (atty/ui:face frame-two)))
    (is (search "╔" (shown screen 0)) "~S" (shown screen 0))
    (is (search "┌" (shown screen 0)))))

(test a-frame-with-no-session-given-has-no-titles
  (let* ((one (a-pane "aaa" :rows 3 :cols 20))
         (two (a-pane "bbb" :rows 3 :cols 20))
         (layout (mux:make-split :across (list one two)))
         (tree (mux::layout-tree layout)))
    (dolist (frame (atty/ui:parts tree))
      (is (null (atty/ui:titles frame)))
      (is (eq :single (atty/ui:line frame))))))

(test a-title-sits-in-the-border-and-is-cut-where-the-border-ends
  (let* ((screen (tty:make-screen :width 16 :height 4))
         (tree (atty/ui:framed (atty/ui:label "in")
                               :titles (list :tl (atty/ui:label " a-long-name-for-it ")
                                             :br (atty/ui:label " br ")))))
    (laid tree screen)
    (let ((top (shown screen 0)))
      (is (equal "┌─ a-long-name─┐" top) "the top border came out as ~S" top))
    (is (search " br ─┘" (shown screen 3)) "~S" (shown screen 3))))

(test a-title-on-the-right-gives-way-to-the-one-on-the-left
  (let* ((screen (tty:make-screen :width 20 :height 3))
         (tree (atty/ui:framed (atty/ui:label "x")
                               :titles (list :tl (atty/ui:label "LEFT-SIDE")
                                             :tr (atty/ui:label "RIGHT-SIDE")))))
    (laid tree screen)
    (let ((top (shown screen 0)))
      (is (search "LEFT-SIDE" top) "~S" top)
      (is (char= #\┐ (char top 19)) "the right title ran over the corner: ~S" top))))

(test the-answers-fit-by-shortening-the-longest-first
  (let ((fitted (mux::options-fitted '((1 "Yes") (2 "Yes, and don't ask again for this in ~/git/x")
                                       (3 "No"))
                                     30)))
    (is (equal "Yes" (first fitted)))
    (is (equal "No" (third fitted)) "the short last option was cut: ~S" fitted)
    (is (<= (+ (reduce #'+ fitted :key (lambda (s) (+ 4 (length s)))) 2) 30) "~S" fitted)
    (is (search "…" (second fitted)))))

(test a-time-is-said-the-way-a-person-says-it
  (is (equal "0s" (mux::duration 0)))
  (is (equal "41s" (mux::duration 41999)))
  (is (equal "3m" (mux::duration (* 3 60000))))
  (is (equal "2h" (mux::duration (* 2 3600000))))
  (is (equal "" (mux::duration nil))))

(test a-lone-pane-with-no-split-has-no-frame
  (let* ((pane (a-pane "solo"))
         (tree (mux::layout-tree pane pane)))
    (is (typep tree 'mux::pane-area)
        "a lone pane is itself and its scrollbar, with no frame round them")
    (is (equal (list pane) (mapcar #'mux::view-pane (mux:views-in tree))))))

(test a-layout-with-nothing-left-in-it-holds-no-panes
  (is (null (mux:panes-in nil))
      "an emptied layout answered a list holding nothing, which is not nothing"))

;;; Reading a pane back, and the scrollbar that says how far.

(defun a-pane-with-history (lines &key (rows 3) (cols 8))
  "A pane that has had LINES lines written to it, numbered from nought, so all
but the last ROWS of them are behind the screen."
  (a-pane (with-output-to-string (s)
            (dotimes (i lines)
              (unless (zerop i) (format s "~C~C" #\Return #\Newline))
              (format s "~D" i)))
          :rows rows :cols cols))

(test a-pane-scrolled-back-shows-the-rows-that-far-behind-the-screen
  (let* ((pane (a-pane-with-history 10))
         (v (mux:pane-view pane))
         (screen (tty:make-screen :width 8 :height 3)))
    (laid v screen)
    (is (equal '("7" "8" "9") (loop :for y :below 3 :collect (shown screen y))))
    (is-true (mux::pane-scroll-to pane 2))
    (laid v screen)
    (is (equal '("5" "6" "7") (loop :for y :below 3 :collect (shown screen y)))
        "two rows back is two rows of history and then the top of the screen")
    (mux::pane-scroll-to pane 1000)
    (is (eql 7 (mux::pane-scrolled pane)) "it went further back than there is")
    (laid v screen)
    (is (equal '("0" "1" "2") (loop :for y :below 3 :collect (shown screen y))))))

(test a-pane-being-read-back-holds-still-while-its-program-goes-on-writing
  (let* ((pane (a-pane-with-history 10))
         (v (mux:pane-view pane))
         (screen (tty:make-screen :width 8 :height 3)))
    (mux::pane-scroll-settle pane)
    (mux::pane-scroll-to pane 4)
    (term:term-process-output (mux:pane-term pane) (format nil "~C~%ten~C~%eleven" #\Return #\Return))
    (mux::pane-scroll-settle pane)
    (is (eql 6 (mux::pane-scrolled pane)) "two more rows went off the top")
    (laid v screen)
    (is (equal '("3" "4" "5") (loop :for y :below 3 :collect (shown screen y))))))

(test a-pane-at-the-foot-stays-at-the-foot
  (let ((pane (a-pane-with-history 10)))
    (mux::pane-scroll-settle pane)
    (term:term-process-output (mux:pane-term pane) (format nil "~C~%ten" #\Return))
    (mux::pane-scroll-settle pane)
    (is (eql 0 (mux::pane-scrolled pane)))))

(test a-program-with-the-whole-screen-has-nothing-behind-it
  (let ((pane (a-pane-with-history 10)))
    (mux::pane-scroll-to pane 4)
    (term:term-process-output (mux:pane-term pane) (format nil "~C[?1049h" #\Escape))
    (mux::pane-scroll-settle pane)
    (is (eql 0 (mux::pane-history pane)))
    (is (eql 0 (mux::pane-scrolled pane)))))

(test the-thumb-is-at-the-foot-when-live-and-at-the-head-when-all-the-way-back
  (is (equal '(8 2) (multiple-value-list (mux::scrollbar-thumb 10 20 80 0))))
  (is (equal '(0 2) (multiple-value-list (mux::scrollbar-thumb 10 20 80 80))))
  (is (equal '(4 2) (multiple-value-list (mux::scrollbar-thumb 10 20 80 40))))
  (is (equal '(9 1) (multiple-value-list (mux::scrollbar-thumb 10 20 10000 0)))
      "however much there is behind it there is a thumb to take hold of")
  (is (equal '(0 10) (multiple-value-list (mux::scrollbar-thumb 10 20 0 0)))))

(test a-pane-gives-its-last-column-to-its-scrollbar
  (let* ((pane (a-pane-with-history 30 :rows 8 :cols 12))
         (area (mux::pane-area pane))
         (screen (tty:make-screen :width 12 :height 8)))
    (laid area screen)
    (let ((v (first (mux:views-in area)))
          (bar (mux::area-bar area)))
      (is (eql 11 (atty/ui:width v)))
      (is (eql 11 (atty/ui:left bar)))
      (is (char= #\▲ (char-at screen 11 0)))
      (is (char= #\▼ (char-at screen 11 7)))
      (is (char= #\█ (char-at screen 11 6)) "live, the thumb is at the foot of the track")
      (is (char= #\░ (char-at screen 11 1)))
      (is (eq bar (atty/ui:under area 3 11)))
      (is (eq v (atty/ui:under area 3 10))))))

(test a-pane-too-narrow-for-one-keeps-every-column
  (let* ((pane (a-pane "abc" :rows 3 :cols 3))
         (area (mux::pane-area pane))
         (screen (tty:make-screen :width 3 :height 3)))
    (laid area screen)
    (is (eql 3 (atty/ui:width (first (mux:views-in area)))))
    (is (equal "abc" (shown screen 0)))))

(test a-session-that-wants-no-scrollbars-has-none
  (let* ((pane (a-pane "solo" :rows 3 :cols 8))
         (tree (let ((mux::*scrollbars* nil)) (mux::layout-tree pane pane)))
         (screen (tty:make-screen :width 8 :height 3)))
    (laid tree screen)
    (is (eql 8 (atty/ui:width (first (mux:views-in tree)))))))

(test each-part-of-a-scrollbar-is-where-it-is-drawn
  (let* ((pane (a-pane-with-history 38 :rows 8 :cols 12))
         (area (mux::pane-area pane))
         (bar (mux::area-bar area))
         (screen (tty:make-screen :width 12 :height 8)))
    (mux::pane-scroll-to pane 15)
    (laid area screen)
    ;; six cells of track, a thumb of one, half way up it
    (is (eq :up (mux::scrollbar-part bar 0)))
    (is (eq :down (mux::scrollbar-part bar 7)))
    (let ((thumb (position #\█ (loop :for y :below 8 :collect (char-at screen 11 y)))))
      (is-true thumb)
      (is (eq :thumb (mux::scrollbar-part bar thumb)))
      (is (eq :above (mux::scrollbar-part bar (1- thumb))))
      (is (eq :below (mux::scrollbar-part bar (1+ thumb)))))))

(test a-scrollbar-with-nothing-behind-the-pane-has-no-parts
  (let* ((pane (a-pane "abc" :rows 8 :cols 12))
         (area (mux::pane-area pane))
         (screen (tty:make-screen :width 12 :height 8)))
    (laid area screen)
    (is (null (mux::scrollbar-part (mux::area-bar area) 3)))
    (is (null (find #\█ (loop :for y :below 8 :collect (char-at screen 11 y)))))))

(test dragging-the-thumb-goes-from-live-at-the-foot-to-the-oldest-at-the-head
  (let* ((pane (a-pane-with-history 38 :rows 8 :cols 12))
         (area (mux::pane-area pane))
         (bar (mux::area-bar area))
         (screen (tty:make-screen :width 12 :height 8)))
    (laid area screen)
    (is (eql 30 (mux::scrollbar-back-at bar 1 0)) "the head of the track is as far back as there is")
    (is (eql 0 (mux::scrollbar-back-at bar 6 0)) "the foot of it is live")
    (is (eql 0 (mux::scrollbar-back-at bar 100 0)) "past the foot is still the foot")
    (is (< 0 (mux::scrollbar-back-at bar 3 0) 30))))

(test a-lone-pane-read-back-says-so-over-its-own-last-line
  (let* ((pane (a-pane-with-history 30 :rows 4 :cols 24))
         (screen (tty:make-screen :width 24 :height 4)))
    (mux::pane-scroll-to pane 5)
    (let ((tree (mux::layout-tree pane pane)))
      (laid tree screen)
      (is (search "↓ 5 to live" (shown screen 3)) "~S" (shown screen 3))
      (is (typep (atty/ui:under tree 3 (search "↓" (shown screen 3))) 'mux::live-chip)))))
