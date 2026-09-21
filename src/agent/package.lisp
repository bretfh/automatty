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
   #:agent-prompted
   #:agent-prompted-at
   #:agent-since
   #:agent-for
   #:agent-history
   #:+history-length+
   #:agent-asks
   #:agent-won
   #:agent-known-p
   #:agent-doing
   #:screen-lines
   #:asks-of-lines
   #:option-of
   #:screen-blocked-p
   #:+turn-patience+
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
