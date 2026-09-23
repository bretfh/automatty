;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/test)

(def-suite server-sessions :in all)
(in-suite server-sessions)

(test opening-a-session-makes-it-and-opening-it-again-joins-it
  (with-a-server-here (server path)
    (let ((one (a-wire-to path))
          (two (a-wire-to path)))
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
  (with-a-server-here (server path)
    (let ((wire (a-wire-to path)))
      (say-to wire (list :open nil "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:server-sessions server))))
      (is (equal '("0") (session-names server)))
      (mux:wire-close wire))))

(test a-new-session-starts-where-it-was-asked-to
  (let ((dir (string-right-trim "/" (namestring (truename (uiop:temporary-directory))))))
    (with-a-server-here (server path)
      (let ((wire (a-wire-to path)))
        (say-to wire (list :open "here" "pwd -P; sleep 30" dir 6 60 t))
        (is-true (step-until server (lambda ()
                                      (let ((s (mux:session-named server "here")))
                                        (and s (search dir (term:term-dump-to-string
                                                            (mux:pane-term
                                                             (mux:session-focus s)))))))))
        (mux:wire-close wire)))))

(test stopping-one-session-leaves-the-others-and-moves-whoever-watched-it
  (with-a-server-here (server path)
    (let ((watching (a-wire-to path))
          (asking (a-wire-to path)))
      (say-to watching (list :open "keep" "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:session-named server "keep"))))
      (say-to watching (list :open "drop" "cat" nil 6 20 t))
      (is-true (step-until server (lambda () (mux:session-named server "drop"))))
      (say-to asking (list :kill-session "drop"))
      (is (equal (list :killed "drop" t) (heard-from server asking :killed)))
      (is (equal '("keep") (session-names server)))
      (is-true (mux:server-running server) "stopping one session stopped the server")
      (is-true (step-until server (lambda ()
                                    (mux:session-watchers (mux:session-named server "keep"))))
               "whoever watched the stopped session was not moved to the one left")
      (say-to asking (list :kill-session "nothing-by-this-name"))
      (is (equal (list :killed "nothing-by-this-name" nil) (heard-from server asking :killed)))
      (mux:wire-close watching)
      (mux:wire-close asking))))

(test the-sessions-say-how-many-of-their-panes-need-you
  (with-a-server-here (server path)
    (let* ((session (mux:add-session server "cat" :name "work" :rows 6 :cols 20))
           (wire (a-wire-to path)))
      ;; a pane that has just started is still drawing, and anything it is said
      ;; to be doing is taken back the moment it draws; so it is let settle first
      (let ((agent (mux:pane-agent (mux:session-focus session))))
        (step-until server (lambda () (eq :idle (agent:agent-state agent))))
        (agent:agent-hear agent :blocked)
        (is-true (step-until server (lambda () (eq :blocked (agent:agent-state agent))))))
      (say-to wire '(:sessions))
      (let ((row (first (second (heard-from server wire :these)))))
        (is (equal "work" (first row)))
        (is (eql 1 (sixth row)) "the row was ~S" row))
      (mux:wire-close wire))))

(test a-server-says-it-holds-every-session-when-knocked-on
  (with-server (path :command "sleep 30")
    (is (member :one-server (mux:probe-socket path 3)))))

(test a-server-with-nothing-yet-waits-for-its-first-session-and-no-longer
  (with-a-server-here (server path)
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
