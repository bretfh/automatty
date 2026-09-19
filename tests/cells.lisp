(in-package #:vt/test)

(def-suite cells :in all)
(in-suite cells)

(defun a-canvas (&key (cols 40) (rows 4))
  (let ((screen (tty:make-screen :width cols :height rows)))
    (values screen (tty:screen-grid screen) cols rows)))

(defun drawn (tree &key (cols 40) (rows 4))
  (multiple-value-bind (screen grid c r) (a-canvas :cols cols :rows rows)
    (cells:draw tree grid c r)
    screen))

(defun line (screen y)
  (string-right-trim " " (shown screen y)))

(test a-string-is-as-wide-as-the-columns-it-takes
  (let ((m (cells:make-cells (tty:screen-grid (tty:make-screen)) 80 24)))
    (is (eql 5 (vt/ui:text-size m "hello" nil)))
    (is (eql 1 (nth-value 1 (vt/ui:text-size m "hello" nil))))
    (is (eql 4 (vt/ui:text-size m (format nil "~C~C" (code-char #x6F22)
                                          (code-char #x5B57))
                                nil))
        "two wide characters take four columns")))

(test a-label-lands-where-it-was-put
  (let ((screen (drawn (vt/ui:label "hello"))))
    (is (equal "hello" (line screen 0)))))

(test a-row-puts-its-parts-side-by-side-with-the-spacing-it-was-given
  (let ((screen (drawn (vt/ui:row :spacing 1
                                  (vt/ui:label "one")
                                  (vt/ui:label "two")
                                  (vt/ui:label "three")))))
    (is (equal "one two three" (line screen 0))))
  (let ((screen (drawn (vt/ui:row :spacing 3
                                  (vt/ui:label "one")
                                  (vt/ui:label "two")))))
    (is (equal "one   two" (line screen 0)))))

(test a-column-puts-its-parts-one-under-the-other
  (let ((screen (drawn (vt/ui:column (vt/ui:label "top")
                                     (vt/ui:label "middle")
                                     (vt/ui:label "bottom")))))
    (is (equal "top" (line screen 0)))
    (is (equal "middle" (line screen 1)))
    (is (equal "bottom" (line screen 2)))))

(test padding-is-counted-in-cells
  (let ((screen (drawn (vt/ui:row :padding '(2 . 1)
                                  (vt/ui:label "in")))))
    (is (equal "" (line screen 0)) "the top padding took no row")
    (is (equal "  in" (line screen 1)) "the left padding took no columns")))

(test a-gap-pushes-what-comes-after-it-to-the-end
  (let ((screen (drawn (vt/ui:row :spacing 0
                                  (vt/ui:label "left")
                                  (vt/ui:gap)
                                  (vt/ui:label "right"))
                       :cols 20)))
    (is (equal "left           right" (line screen 0)))))

(test a-rule-is-drawn-with-the-glyph-it-carries
  (let ((screen (drawn (vt/ui:row (vt/ui:rule :expand 1)) :cols 6)))
    (is (equal "──────" (line screen 0))))
  (let ((screen (drawn (vt/ui:column (vt/ui:rule :upright t :expand 1))
                       :cols 6 :rows 3)))
    (is (equal "│" (line screen 0)))
    (is (equal "│" (line screen 2)))))

(test a-centerbox-holds-its-three-parts-apart
  (let ((screen (drawn (vt/ui:centerbox :start (vt/ui:label "first")
                                        :center (vt/ui:label "middle")
                                        :end (vt/ui:label "last"))
                       :cols 20 :rows 5)))
    (is (equal "first" (line screen 0)))
    (is (equal "last" (line screen 4)))))

(test a-choice-shows-the-mark-it-is-wearing
  (let ((screen (drawn (vt/ui:choice :chosen t (vt/ui:label "picked")) :cols 20)))
    (is (equal "> picked" (line screen 0))))
  (let ((screen (drawn (vt/ui:choice (vt/ui:label "not picked")) :cols 20)))
    (is (equal "  not picked" (line screen 0))
        "one that was not picked did not line up under one that was")))

(test what-was-drawn-and-read-back-is-the-same-screen
  (let* ((screen (drawn (vt/ui:column
                         (vt/ui:row :spacing 1
                                    (vt/ui:label "name" :face :accent)
                                    (vt/ui:label "value"))
                         (vt/ui:rule :expand 1)
                         (vt/ui:label "below"))
                        :cols 24 :rows 4))
         (was (tty:make-screen :width 24 :height 4))
         (host (a-host screen)))
    (say host (with-output-to-string (s)
                (tty:encode-frame screen (tty:screen-diff was screen) s)))
    (is (null (difference screen host)) "~A" (difference screen host))))

(test a-face-from-the-theme-becomes-a-face-on-the-grid
  (let* ((screen (drawn (vt/ui:label "coloured" :face :accent)))
         (face (vt:cell-face (cell-at screen 0 0))))
    (is (consp (vt:face-fg face)) "the theme colour did not reach the cell")
    (is (= 3 (length (vt:face-fg face))) "a colour is three numbers")
    (is (every (lambda (n) (<= 0 n 255)) (vt:face-fg face)))))

(test a-wide-character-in-a-label-takes-the-columns-it-draws
  (let ((screen (drawn (vt/ui:row :spacing 1
                                  (vt/ui:label (format nil "~C~C"
                                                       (code-char #x6F22)
                                                       (code-char #x5B57)))
                                  (vt/ui:label "after")))))
    (is (eql #\a (at-screen screen 5 0))
        "the label after the wide characters did not start at column 5")))

(test the-attributes-a-face-carries-reach-the-grid
  (vt/ui:set-face :a-test-face '(:fg "#ff0000" :bold t :italic t
                                 :underline :curly :crossed t))
  (unwind-protect
       (let* ((screen (drawn (vt/ui:label "marked" :face :a-test-face)))
              (face (vt:cell-face (cell-at screen 0 0))))
         (is-true (vt:face-bold face))
         (is-true (vt:face-italic face))
         (is (eq :curly (vt:face-underline face))
             "the underline style was flattened: ~S" (vt:face-underline face))
         (is-true (vt:face-crossed face) "crossed never reached the cell"))
    (vt/ui:set-face :a-test-face nil)))

(test the-warning-face-is-one-the-theme-has
  (is-true (vt/ui:in-force :warning)
           "the bar asks for a warning face the theme does not define"))
