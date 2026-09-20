;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(defpackage #:atty/agent
  (:use #:cl)
  (:local-nicknames (#:term #:libatty))
  (:export
   #:agent
   #:make-agent
   #:agent-state
   #:agent-moved
   #:agent-heard
   #:agent-rules
   #:agent-signal
   #:agent-look
   #:agent-hear
   #:agent-become
   #:agent-explain
   #:agent-trace
   #:*agents*
   #:+hold+
   #:rule
   #:make-rule
   #:rule-id
   #:rule-state
   #:rule-priority
   #:rule-region
   #:rule-test
   #:region-text
   #:last-lines
   #:lines-of
   #:has
   #:any-line
   #:option-line-p
   #:claude-code))
