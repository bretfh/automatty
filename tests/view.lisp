(in-package #:vtx/test)

(def-suite view :in all)
(in-suite view)

(defun a-pane (said &key (rows 3) (cols 8))
  (let ((pane (mux:make-pane "true" :rows rows :cols cols)))
    (vt:term-process-output (mux:pane-term pane) said)
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
         (tree (vtx/ui:row :align :stretch :spacing 0 a b)))
    (laid tree screen)
    (is (eql 0 (vtx/ui:left a)))
    (is (eql 10 (vtx/ui:width a)) "the left pane was not given half the columns")
    (is (eql 10 (vtx/ui:left b)) "the right pane did not start at the halfway mark")
    (is (eql 10 (vtx/ui:width b)))
    (is (eql 3 (vtx/ui:height a)) "a stretched row did not give it every row")
    (is (equal "aaaa      bbbb" (string-right-trim " " (shown screen 0))))))

(test the-bar-takes-its-row-off-the-pane-under-it
  (let* ((pane (a-pane "hello" :rows 5 :cols 12))
         (v (mux:pane-view pane))
         (screen (tty:make-screen :width 12 :height 5))
         (tree (vtx/ui:column :align :stretch v (vtx/ui:label "bar"))))
    (laid tree screen)
    (is (eql 4 (vtx/ui:height v))
        "the pane was not given the rows the bar left it")
    (is (eql 0 (vtx/ui:top v)))
    (is (equal "hello" (string-right-trim " " (shown screen 0))))
    (is (equal "bar" (string-right-trim " " (shown screen 4)))
        "the bar is not at the foot: ~S" (shown screen 4))))

(test a-pane-is-drawn-where-it-was-put-and-clipped-to-it
  (let* ((pane (a-pane "0123456789" :rows 2 :cols 10))
         (v (mux:pane-view pane))
         (screen (tty:make-screen :width 10 :height 2))
         (tree (vtx/ui:row :align :stretch :spacing 0 (vtx/ui:gap) v)))
    (laid tree screen)
    (is (eql 5 (vtx/ui:left v)) "the two gaps did not split the columns evenly")
    (is (equal "     01234" (shown screen 0))
        "what did not fit was drawn past the edge: ~S" (shown screen 0))))

(test every-pane-in-a-tree-is-found
  (let* ((one (mux:pane-view (a-pane "")))
         (two (mux:pane-view (a-pane "")))
         (tree (vtx/ui:column (vtx/ui:row one) two (vtx/ui:label "bar"))))
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
      (is (eql 11 (vtx/ui:left (second views)))
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

(test the-rule-beside-the-focused-pane-is-lit
  (let* ((one (a-pane "aaa" :rows 3 :cols 20))
         (two (a-pane "bbb" :rows 3 :cols 20))
         (layout (mux:make-split :across (list one two)))
         (screen (tty:make-screen :width 21 :height 3))
         (tree (mux::layout-tree layout two)))
    (laid tree screen)
    (destructuring-bind (frame-one frame-two) (vtx/ui:parts tree)
      (is (eq :border-inactive (vtx/ui:face frame-one)))
      (is (eq :border-active (vtx/ui:face frame-two))
          "the frame around the focused pane was not lit"))))

(test a-rule-with-no-focus-given-is-unlit-as-before
  (let* ((one (a-pane "aaa" :rows 3 :cols 20))
         (two (a-pane "bbb" :rows 3 :cols 20))
         (layout (mux:make-split :across (list one two)))
         (tree (mux::layout-tree layout)))
    (dolist (frame (vtx/ui:parts tree))
      (is (eq :border-inactive (vtx/ui:face frame))))))

(test a-lone-pane-with-no-split-has-no-frame
  (let* ((pane (a-pane "solo"))
         (tree (mux::layout-tree pane pane)))
    (is (typep tree 'mux::pane-view))))

(test a-layout-with-nothing-left-in-it-holds-no-panes
  (is (null (mux:panes-in nil))
      "an emptied layout answered a list holding nothing, which is not nothing"))
