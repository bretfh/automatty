;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/test)

(def-suite overlay-drawer :in all)
(in-suite overlay-drawer)

(test the-drawer-says-what-decided-the-state-and-who-typed-there
  (with-world (client server (list (list :session "todo" :id 2 :says "impl" :kind "claude-code" :state :blocked
                                         :for 42000 :focus t)))
    (let* ((key (cons "todo" 2))
           (d (mux::%make-drawer))
           (row (mux::pane-row-of client "todo" 2))
           (context (list '(:blocked :blocked
                            ((:transcript :footer :skip nil nil)
                             (:permission :choice :blocked t
                              (:screen :permission :means :blocked :widget :choice
                               :question "Do you want to proceed?" :options ("Yes" "No") :selected 0))
                             (:question :choice :blocked nil
                              (:screen :question :means :blocked :widget :choice
                               :question "Which?" :options ("a" "b") :selected 0))))
                          '(:kind "claude-code" :programs ("claude --resume")
                            :group 48213 :command "sh -c \"exec claude\"")
                          '((50000 (:pane "todo:4") :prompt "STATUS?" :refused 0)
                            (80000 (:pane "todo:4") :prompt "run the suite" t 0)
                            (90000 (:client 7 "/dev/ttys004") :keys 14 t 0))))
           (screen (tty:make-screen :width 150 :height 36)))
      (atty/cells:draw (mux::drawer-tree d client key row 50 context) (tty:screen-grid screen) 150 36 :left 100 :top 1)
      (let ((all (format nil "~{~A~%~}" (loop :for y :below 36 :collect (shown screen y)))))
        (is (search "why is impl asking?" all) "~A" all)
        (is (search "in front: claude --resume  group 48213" all))
        (is (search "* blocked permission" all)
            "the winning rule is not marked: ~A" all)
        (is (search "│ Do you want to proceed?" all) "the winning rule's text is not under it")
        (is (search "+ " all) "another rule that matched is not marked")
        (is (search "WHO TYPED HERE" all))
        (is (search "todo:4" all))
        (is (search "refused" all))
        (is (search "keys 14 bytes" all))
        (is (search "20m" all))))))

(test the-drawer-follows-the-focus-and-typing-still-reaches-the-pane
  (with-server (path :command "cat" :rows 24 :cols 150)
    (with-seer (seer path :rows 24 :cols 150)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~Ce" mux:+prefix+))
      (is-true (pump seer :want "what is cat?") "the drawer did not open: ~S" (seen seer))
      (is-true (pump seer :want "not read: no reader knows this program"))
      (type-at seer "typed-past-it")
      (is-true (pump seer :want "typed-past-it")
               "what was typed with the drawer open did not reach the pane: ~S" (seen seer))
      (is-true (pump seer :want "bytes") "the drawer does not say who typed: ~S" (seen seer))
      (type-at seer (format nil "~Ce" mux:+prefix+))
      (is-true (pump seer :until (lambda () (null (search "what is" (seen seer)))))
               "the key that opened it did not close it"))))
