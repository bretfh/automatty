;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/test)

(def-suite server-sessions :in all)
(in-suite server-sessions)

(test opening-a-session-makes-it-and-opening-it-again-joins-it
  (with-stepped-server (server path)
    (let ((one (wire-to path))
          (two (wire-to path)))
      (say-to one (list :open "work" "cat" nil 6 20 t))
      (say-to two (list :open "work" "cat" nil 6 20 t))
      (is-true (step-until server (lambda ()
                                    (let ((work (mux:session-named server "work")))
                                      (and work
                                           (= 2 (length (mux:session-watchers work))))))))
      (is (equal '("work") (session-names server))
          "two clients opening one name made ~S" (session-names server))
      (say-to one (list :open "play" "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:session-named server "play"))))
      (is (equal '("work" "play") (session-names server)))
      (mux:wire-close one)
      (mux:wire-close two))))

(test a-session-opened-with-no-name-is-given-one
  (with-stepped-server (server path)
    (let ((wire (wire-to path)))
      (say-to wire (list :open nil "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:server-sessions server))))
      (is (equal '("0") (session-names server)))
      (mux:wire-close wire))))

(test a-new-session-starts-where-it-was-asked-to
  (let ((dir (string-right-trim "/" (namestring (truename (uiop:temporary-directory))))))
    (with-stepped-server (server path)
      (let ((wire (wire-to path)))
        (say-to wire (list :open "here" "pwd -P; sleep 30" dir 10 60 t))
        (is-true (step-until server (lambda ()
                                      (let ((s (mux:session-named server "here")))
                                        (and s (search dir (dumped (first (mux:session-panes s)))))))))
        (mux:wire-close wire)))))

(test stopping-one-session-leaves-the-others-and-moves-whoever-watched-it
  (with-stepped-server (server path)
    (let ((watching (wire-to path))
          (asking (wire-to path)))
      (say-to watching (list :open "keep" "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:session-named server "keep"))))
      (say-to watching (list :open "drop" "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:session-named server "drop"))))
      (is (eq t (nth-value 1 (run-by-name server asking "stop session" '("drop")))))
      (is (equal '("keep") (session-names server)))
      (is-true (mux:server-running server) "stopping one session stopped the server")
      (is-true (step-until server (lambda ()
                                    (mux:session-watchers (mux:session-named server "keep"))))
               "whoever watched the stopped session was not moved to the one left")
      (is (eql 1 (nth-value 1 (run-by-name server asking "stop session" '("nothing-by-this-name")))))
      (mux:wire-close watching)
      (mux:wire-close asking))))

(test the-sessions-say-how-many-of-their-panes-need-you
  (with-stepped-server (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (wire (wire-to path)))
      ;; a pane that has just started is still drawing, and anything it is said
      ;; to be doing is taken back the moment it draws; so it is let settle first
      (let ((agent (mux:pane-agent (first (mux:session-panes session)))))
        (step-until server (lambda () (eq :idle (agent:agent-state agent))))
        (mux::pane-hear (first (mux:session-panes session)) :blocked)
        (is-true (step-until server (lambda () (eq :blocked (agent:agent-state agent))))))
      (say-to wire '(:sessions))
      (let ((row (first (second (heard-from server wire :these)))))
        (is (equal "work" (first row)))
        (is (eql 1 (fourth row)) "the row was ~S" row))
      (mux:wire-close wire))))

(test a-server-says-it-holds-every-session-when-knocked-on
  (with-server (path :command "sleep 30")
    (is (member :one-server (mux:probe-socket path 3)))))

(test a-server-with-nothing-yet-waits-for-its-first-session-and-no-longer
  (with-stepped-server (server path)
    (let ((now (mux::server-born server)))
      (is-true (mux::server-needed-p server now)
               "a server just started with no session gave up at once")
      (is (null (mux::server-needed-p server (+ now mux::+first-session-timeout+)))
          "a server nobody reached went on holding its socket")
      (mux:add-session server "cat" :name "work" :rows 6 :cols 20)
      (is-true (mux::server-needed-p server (+ now (* 2 mux::+first-session-timeout+))))
      (mux::end-session server (first (mux:server-sessions server)) :done)
      (is (null (mux::server-needed-p server now))
          "a server whose last session ended went on"))))

(test a-session-name-with-a-directory-in-it-is-not-a-socket-somewhere-else
  (let ((mux:*server-name* "../../elsewhere"))
    (is (equal "elsewhere" (file-namestring (mux:socket-path))))
    (is (search (namestring (mux:socket-directory)) (mux:socket-path)))))

(test two-terminals-on-one-server-are-two-views-of-the-same-panes
  (with-server (path :command "cat" :rows 12 :cols 60)
    (with-seer (a path :rows 12 :cols 60)
      (with-seer (b path :rows 12 :cols 60)
        (type-at a "from-a")
        (is-true (pump a :want "from-a"))
        (is-true (pump b :want "from-a") "the other terminal does not show the pane they share")
        (type-at a (format nil "~Cc" mux:+prefix+))
        (is-true (pump a :until (lambda () (null (search "from-a" (seen a))))))
        (pump b :seconds 1/2)
        (is (search "from-a" (seen b)) "a window one terminal made was shown in the other: ~S" (seen b))
        (type-at a "in-two")
        (is-true (pump a :want "in-two"))
        (pump b :seconds 1/2)
        (is (null (search "in-two" (seen b))) "keys in one terminal reached the other's pane: ~S" (seen b))
        (type-at b "from-b")
        (is-true (pump b :want "from-b") "the other terminal's keys went nowhere: ~S" (seen b))
        (pump a :seconds 1/2)
        (is (null (search "from-b" (seen a))) "keys in one terminal went to the window the other shows")
        (type-at b (format nil "~Ct" mux:+prefix+))
        (is-true (pump b :until (lambda () (null (search " λ " (seen b))))) "the bar did not go: ~S" (seen b))
        (pump a :seconds 1/2)
        (is (search " λ " (seen a)) "the bar went in a terminal that did not ask: ~S" (seen a))
        (type-at a (format nil "~Cp" mux:+prefix+))
        (is-true (pump a :want "from-b") "the pane they share does not have what the other typed: ~S" (seen a))))))
