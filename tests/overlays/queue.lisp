;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/test)

(def-suite overlay-queue :in all)
(in-suite overlay-queue)

(test the-queue-holds-what-is-asking-oldest-first-and-can-be-narrowed
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :says "impl" :kind "claude-code" :state :blocked
                        :for 42000 :asks '(:subject "Bash command" :options ((1 "Yes"))))
                  (list :session "lib" :id 1 :says "core" :kind "claude-code" :state :blocked
                        :for 8000 :asks '(:subject "Fetch" :options ((1 "Yes"))))
                  (list :session "todo" :id 1 :says "arch" :kind "claude-code" :state :working
                        :for 41000)))
         (q (mux::%make-queue)))
    (is (equal '("todo:2" "lib:1") (mapcar #'mux::row-address (mux::queue-rows q client)))
        "the queue was not what is blocked, oldest first")
    (setf (mux::queue-query q) "fetch")
    (is (equal '("lib:1") (mapcar #'mux::row-address (mux::queue-rows q client))))))

(test the-queue-draws-each-question-with-its-answers-and-the-one-picked-beside-it
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :says "impl" :kind "claude-code" :state :blocked
                        :for 42000 :asks '(:subject "Bash command"
                                           :detail ("python3 -m pytest -q")
                                           :options ((1 "Yes") (2 "No"))))))
         (q (mux::%make-queue))
         (screen (tty:make-screen :width 100 :height 20)))
    (let ((mux::*overlay-client* client))
      (mux:draw-overlay q screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 20 :collect (shown screen y)))))
      (is (search "needs you" all) "~A" all)
      (is (search "todo › impl · as it is now" all) "~A" all)
      (is (search "Bash command" all))
      (is (search "python3 -m pytest -q" all))
      (is (search " 1 Yes" all))
      (is (search "1 waiting · oldest 42s" all) "~A" all))))

(test the-queue-answers-a-pane-in-another-session-without-going-there
  (let ((script (a-dialog-script)))
    (unwind-protect
         (with-server (path :command "/bin/sh" :rows 20 :cols 100)
           (with-seer (seer path :rows 20 :cols 100)
             (pump seer :seconds 1/2)
             (type-at seer (format nil "sh ~A~C" script #\Return))
             (is-true (pump seer :want "▲ 1") "the bar never said anything needs you: ~S" (seen seer))
             ;; somewhere else to be while it is answered
             (type-at seer (format nil "~CC" mux:+prefix+))
             (is-true (pump seer :until (lambda () (null (search "Bash command" (seen seer))))))
             (type-at seer (format nil "~CN" mux:+prefix+))
             (is-true (pump seer :want "1 waiting") "the queue did not open: ~S" (seen seer))
             (is-true (pump seer :want "python3 -m pytest -q"))
             (type-at seer "1")
             (is-true (pump seer :want "nothing needs you")
                      "answering did not take it off the queue: ~S" (seen seer))
             (is-true (pump seer :want "✓")
                      "the answer is not among those answered lately: ~S" (seen seer))
             (is-true (pump seer :want "◆ here") "~S" (seen seer))
             (type-at seer (string (code-char 27)))
             (pump seer :seconds 1/2)
             (type-at seer (format nil "~Cb" mux:+prefix+))
             (is-true (pump seer :want "◆ here"))
             (type-at seer (format nil "~C[A" #\Escape))
             (pump seer :seconds 1/4)
             (type-at seer (string #\Return))
             (is-true (pump seer :want "answered-with-1")
                      "the pane was not answered: ~S" (seen seer))))
      (ignore-errors (delete-file script)))))

(test the-queue-says-where-a-question-is-and-who-is-looking-at-it-and-who-answered
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :window 1 :window-name "agents" :at 1 :says "impl"
                        :kind "claude-code" :state :blocked :for 42000
                        :asks '(:subject "Bash command" :detail ("python3 -m pytest -q")
                                :options ((1 "Yes") (2 "No"))))))
         (q (mux::%make-queue))
         (screen (tty:make-screen :width 120 :height 20)))
    (setf (mux::client-id client) 7
          (mux::client-clients client) '((7 "/dev/ttys042" 40 120 "lib" 1 1000 nil)
                                         (9 "/dev/ttys051" 30 100 "todo" 1 5000 200))
          (mux::client-recent client) '(("todo" 2 4000 (:client 7 "/dev/ttys042") :answer "1 Yes" t)
                                        ("todo" 2 9000 (:pane "todo:1.4") :prompt "run it" :refused)))
    (let ((mux::*overlay-client* client))
      (mux:draw-overlay q screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 20 :collect (shown screen y)))))
      (is (search "todo › 1 agents › impl" all) "~A" all)
      (is (search "⌨ ttys051 looking" all) "~A" all)
      (is (null (search "ttys042 is looking" all)) "the client itself is not somebody else")
      (is (search "◆ here" all) "~A" all)
      (is (search "⌁ todo:1.4" all) "~A" all)
      ;; the answers are buttons where they are drawn
      (is-true (mux::queue-laid q))
      (let ((found nil))
        (labels ((walk (w)
                   (when (and (typep w 'mux::bar-button)
                              (equal '(:answer "todo" 2 1) (mux::bar-button-runs w)))
                     (setf found t))
                   (dolist (part (atty/ui:parts w)) (walk part))))
          (walk (mux::queue-laid q)))
        (is-true found "answer 1 is not a button")))))

(test the-queue-has-a-toolbar-that-sorts-narrows-and-hides-what-was-answered
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :says "impl" :kind "claude-code" :state :blocked
                        :for 42000 :asks '(:subject "Bash command" :options ((1 "Yes"))))
                  (list :session "lib" :id 1 :says "core" :kind "claude-code" :state :blocked
                        :for 8000 :asks '(:subject "Fetch" :options ((1 "Yes"))))))
         (q (mux::%make-queue))
         (screen (tty:make-screen :width 120 :height 20)))
    (setf (mux::client-session client) "lib"
          (mux::client-overlays client) (list q)
          (mux::client-recent client) '(("todo" 2 4000 (:client 7 "/dev/ttys042") :answer "1 Yes" t)))
    (is (equal '("todo:2" "lib:1") (mapcar #'mux::row-address (mux::queue-rows q client))))
    (let ((mux::*client* client))
      (mux::queue-sort)
      (is (equal '("lib:1" "todo:2") (mapcar #'mux::row-address (mux::queue-rows q client)))
          "sorting by name did nothing")
      (mux::queue-all-sessions)
      (is (equal '("lib:1") (mapcar #'mux::row-address (mux::queue-rows q client)))
          "narrowing to this session did nothing")
      (mux::queue-all-sessions))
    (let ((mux::*overlay-client* client)) (mux:draw-overlay q screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 20 :collect (shown screen y)))))
      (is (search "sort ▾  by name" all) "~A" all)
      (is (search "✓  every session" all) "~A" all)
      (is (search "ANSWERED LATELY" all) "~A" all)
      (is (search "↵ go" all) "the actions are not buttons on the toolbar: ~A" all)
      ;; the toggle is a button: a click on it hides what was answered lately
      (let* ((line (loop :for y :below 20 :when (search "answered lately" (shown screen y)) :return y))
             (col (search "answered lately" (shown screen line))))
        (let ((mux::*client* client)
              (mux::*mouse-position* (cons col line)))
          (mux::queue-click))
        (is (null (mux::queue-showing-recent q)) "the click on the toggle did nothing")))
    (let ((mux::*overlay-client* client)) (mux:draw-overlay q screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 20 :collect (shown screen y)))))
      (is (null (search "ANSWERED LATELY" all)) "~A" all))))

(test the-drawer-is-in-sections-and-answers-from-its-foot
  (let* ((client (a-told-client
                  (list :session "todo" :id 2 :window 1 :window-name "agents" :at 1 :says "impl"
                        :label "impl" :kind "claude-code" :state :blocked :for 42000 :focus t
                        :asks '(:subject "Bash command" :options ((1 "Yes") (2 "No"))))))
         (d (mux::%make-drawer))
         (key (cons "todo" 2))
         (now (mux::client-ms))
         (screen (tty:make-screen :width 150 :height 30)))
    (setf (mux::client-session client) "todo"
          (gethash key (mux::client-pane-info client))
          (list :pane-about (list now '(:kind "claude-code" :version "2.1.278" :reader "claude-code"
                                        :pid 48213 :size (81 19) :directory "/tmp/x"
                                        :programs ("claude") :command "claude"))
                :agent-explained (list now :blocked :blocked nil)
                :pane-history (list now '((42000 :blocked)))
                :pane-log (list now nil)))
    (let ((mux::*overlay-client* client))
      (mux:draw-overlay d screen))
    (let ((all (format nil "~{~A~%~}" (loop :for y :below 30 :collect (shown screen y)))))
      (is (search "WHAT IT IS" all) "~A" all)
      (is (search "claude-code 2.1.278 · read by the claude" all) "~A" all)
      (is (search "pid 48213 · 81×19 · /tmp/x" all) "~A" all)
      (is (search "STATE" all))
      (is (search "HOW IT WAS READ" all))
      (is (search "WHO TYPED HERE" all))
      (is (search " 1 Yes" all) "no answers at the foot: ~A" all)
      (is (search "answer from here" all))
      (is (search " z zoom " all) "the drawer has no actions: ~A" all)
      (is (search "▾ STATE" all) "~A" all))
    ;; a click on a section's heading folds it
    (let ((line (loop :for yy :below 30 :when (search "▾ STATE" (shown screen yy)) :return yy)))
      (is-true line)
      (when line
        (mux::overlay-clicked d line (+ 3 (search "▾ STATE" (shown screen line))) client)
        (is (equal '(:state) (mux::drawer-folded d)))
        (let ((mux::*overlay-client* client)) (mux:draw-overlay d screen))
        (let ((all (format nil "~{~A~%~}" (loop :for y :below 30 :collect (shown screen y)))))
          (is (search "▸ STATE" all) "~A" all)
          (is (null (search "for 42s" all)) "the folded section is still drawn: ~A" all))))
    ;; a click on an answer is that answer, taken by the drawer itself
    (let ((sent nil))
      (setf (mux::client-wire client) nil)
      (multiple-value-bind (x y)
          (loop :for yy :below 30
                :for xx := (search " 1 Yes" (shown screen yy))
                :when xx :do (return (values xx yy)))
        (is-true x)
        (when x
          ;; no wire to send on, so telling the server comes apart; that it was
          ;; taken and tried is the point
          (setf sent (handler-case (mux::overlay-clicked d y (+ x 2) client)
                       (error () :tried)))
          (is-true sent "the click on the answer was not taken: ~S" sent))))))
