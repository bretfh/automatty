;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/test)

(def-suite server-watchers :in all)
(in-suite server-watchers)

(test an-address-is-read-either-way-and-said-by-window-and-pane
  (multiple-value-bind (session id window n) (mux::parse-pane-address "todo:1.2")
    (is (equal "todo" session)) (is (null id)) (is (eql 1 window)) (is (eql 2 n)))
  (multiple-value-bind (session id window n) (mux::parse-pane-address "todo:4")
    (is (equal "todo" session)) (is (eql 4 id)) (is (null window)) (is (null n)))
  (signals error (mux::parse-pane-address "todo"))
  (signals error (mux::parse-pane-address "todo:1."))
  (is (equal "todo:1.2" (mux::row-address '(:session "todo" :id 9 :window 1 :at 1))))
  (is (equal "todo:9" (mux::row-address '(:session "todo" :id 9)))
      "a row from a server without windows is still said by its id")
  (is (equal "todo › 1 agents › impl"
             (mux::row-path '(:session "todo" :id 9 :window 1 :window-name "agents" :label "impl")))))

(test whoever-is-attached-is-listed-and-can-be-let-go
  (with-stepped-server (server path)
    (let ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
          (looking (wire-to path)))
      (declare (ignorable session))
      (say-to looking '(:who "/dev/ttys042") '(:attach 6 20 t))
      (flet ((row () (find "/dev/ttys042" (mux::encode-clients server (mux::now-ms))
                           :key #'second :test #'equal)))
        (is-true (step-until server (lambda () (fifth (row)))) "the joined client is not listed")
        (let ((row (row)))
          (when row
            (is (equal "work" (fifth row)))
            (is (eql 1 (sixth row)) "it is not said to be looking at window 1: ~S" row)
            (is (integerp (seventh row)))
            (is (null (eighth row)) "it has not typed, so has no idle time: ~S" row)))
        (say-to looking '(:keys "hi"))
        (is-true (step-until server (lambda () (integerp (eighth (row))))) "typing was not noted")
        (mux::drop-watcher server (find (first (row)) (mux::all-watchers server) :key #'mux::watcher-id)
                           :detached))
      ;; the far end going is a read of nothing
      (is-true (step-until server (lambda () (null (mux:wire-receive looking))))
               "letting a client go did not close its wire"))))

(test a-terminal-that-follows-another-goes-where-it-goes-until-it-types
  (with-stepped-server (server path)
    (mux:add-session server "cat" :name "one" :rows 6 :cols 40)
    (mux:add-session server "cat" :name "two" :rows 6 :cols 40)
    (let ((lead (wire-to path))
          (tail (wire-to path)))
      (say-to lead (list :who "/dev/ttys001") (list :want "one") (list :attach 6 40 t))
      (heard-from server lead :hello)
      (say-to tail (list :who "/dev/ttys002") (list :want "two") (list :attach 6 40 t))
      (heard-from server tail :hello)
      (let* ((leader (find "/dev/ttys001" (mux::all-watchers server) :key #'mux::watcher-tty :test #'equal))
             (follower (find "/dev/ttys002" (mux::all-watchers server) :key #'mux::watcher-tty :test #'equal)))
        (is-true (and leader follower))
        (mux::follow server follower (mux::watcher-id leader))
        (is-true (step-until server (lambda () (eq (mux::watcher-session follower) (mux::watcher-session leader))))
                 "following did not bring the follower to the leader's session")
        (is (equal "one" (mux:session-name (mux::watcher-session follower))))
        ;; the leader moves; the follower comes along
        (mux::go-to leader "two")
        (is-true (step-until server (lambda () (equal "two" (mux:session-name (mux::watcher-session follower)))))
                 "the follower did not follow the leader to the other session")
        ;; the clients say who follows whom
        (let* ((rows (mux::encode-clients server (mux::now-ms)))
               (mine (find (mux::watcher-id follower) rows :key #'first)))
          (is (eql (mux::watcher-id leader) (ninth mine)) "~S" rows))
        ;; a key of the follower's own ends it
        (say-to tail (list :keys "x"))
        (is-true (step-until server (lambda () (null (mux::watcher-following follower)))))
        (mux::go-to leader "one")
        (step-until server (lambda () (equal "one" (mux:session-name (mux::watcher-session leader)))))
        (is (equal "two" (mux:session-name (mux::watcher-session follower)))
            "the follower still followed after typing")
        ;; following again, and the leader detaching, ends it too
        (mux::follow server follower (mux::watcher-id leader))
        (is-true (step-until server (lambda () (equal "one" (mux:session-name (mux::watcher-session follower))))))
        (say-to lead (list :detach))
        (is-true (step-until server (lambda () (null (mux::watcher-following follower))))
                 "the leader went and the follower still follows"))
      (mux:wire-close tail))))
