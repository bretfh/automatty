;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/test)

(def-suite client-frames :in all)
(in-suite client-frames)

(test a-split-frame-says-which-pane-what-it-is-called-and-what-it-is-doing
  (with-server (path :command "sleep 30" :rows 12 :cols 80)
    (with-seer (seer path :rows 12 :cols 80)
      (pump seer :seconds 1/2)
      (type-at seer (format nil "~C3" mux:+prefix+))
      (is-true (pump seer :until (lambda () (search "┏" (seen seer))))
               "no double frame round the focused pane: ~S" (seen seer))
      (type-at seer (format nil "~C." mux:+prefix+))
      (is-true (pump seer :want "name ") "the name prompt did not open: ~S" (seen seer))
      (type-at seer (format nil "impl~C" #\Return))
      (is-true (pump seer :want " impl sleep")
               "the frame does not carry the new name and the program: ~S" (seen seer))
      (is (null (search "idle" (seen seer)))
          "a frame said what a program nobody knows is doing: ~S" (seen seer)))))

(test a-blocked-pane-is-answered-by-clicking-an-answer-in-its-border
  (let ((script (a-dialog-script)))
    (unwind-protect
         (with-server (path :command "/bin/sh" :rows 16 :cols 90)
           (with-seer (seer path :rows 16 :cols 90)
             (pump seer :seconds 1/2)
             (type-at seer (format nil "~C3" mux:+prefix+))
             (pump seer :until (lambda () (search "┏" (seen seer))))
             (pump seer :seconds 1/2)
             (type-at seer (format nil "sh ~A~C" script #\Return))
             (is-true (pump seer :want "▲ asks") "the question never reached the border: ~S"
                      (seen seer))
             (is-true (pump seer :want " 1 Yes") "the answers are not in the border: ~S" (seen seer))
             (multiple-value-bind (x y) (where-on seer " 1 Yes")
               (click-at seer (+ x 1) y))
             (is-true (pump seer :want "answered-with-1")
                      "clicking the answer did not type it: ~S" (seen seer))))
      (ignore-errors (delete-file script)))))

(test zooming-a-pane-gives-it-the-session-and-zooming-again-gives-it-back
  (with-server (path :command "cat" :rows 12 :cols 80)
    (with-seer (seer path :rows 12 :cols 80)
      (type-at seer "in-the-first")
      (is-true (pump seer :want "in-the-first"))
      (type-at seer (format nil "~C3" mux:+prefix+))
      (pump seer :until (lambda () (search "┏" (seen seer))))
      (type-at seer "in-the-second")
      (is-true (pump seer :want "in-the-second"))
      (type-at seer (format nil "~Cz" mux:+prefix+))
      (is-true (pump seer :until (lambda () (null (search "in-the-first" (seen seer)))))
               "the other pane still shows while one is zoomed: ~S" (seen seer))
      (is (search "in-the-second" (seen seer)))
      (type-at seer (format nil "~Cz" mux:+prefix+))
      (is-true (pump seer :want "in-the-first") "zooming again did not give it back: ~S"
               (seen seer)))))

(test going-to-the-blocked-pane-goes-to-it-in-whatever-session-it-is
  (with-a-server-here (server path)
    (let* ((here (mux:add-session server "cat" :name "here" :rows 6 :cols 30))
           (there (mux:add-session server "cat" :name "there" :rows 6 :cols 30))
           (wire (a-wire-to path)))
      (blocked server (mux:session-focus there))
      (say-to wire (list :want "here") (list :attach 6 30 t))
      (is-true (step-until server (lambda () (mux:session-watchers here))))
      (say-to wire '(:go-to-blocked))
      (is (equal (list :focused "there" (mux:pane-id (mux:session-focus there)) t)
                 (heard-from server wire :focused)))
      (is (mux:session-watchers there) "the client was not taken to the blocked pane")
      (agent:agent-hear (mux:pane-agent (mux:session-focus there)) :idle)
      (step-until server (lambda () (eq :idle (agent:agent-state
                                               (mux:pane-agent (mux:session-focus there))))))
      (say-to wire '(:go-to-blocked))
      (is (equal '(:say "nothing needs you") (heard-from server wire :say)))
      (mux:wire-close wire))))
