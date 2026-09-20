;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/agent)

(defclass claude-code (agent) ())

(defun claude-code-p (title command)
  (or (search "Claude Code" title)
      (let ((word (subseq command 0 (or (position #\Space command) (length command)))))
        (string= "claude" (subseq word (1+ (or (position #\/ word :from-end t) -1)))))))

(pushnew (cons 'claude-code #'claude-code-p) *agents* :key #'car)

(defun busy-glyph-p (char)
  (or (find char "◐◑◒◓")
      (<= #x2800 (char-code char) #x28FF)))

(defun spinner-line-p (line)
  (let ((said (string-trim " " line)))
    (and (> (length said) 2)
         (find (char said 0) "*·✢✳✶✻✽")
         (char= #\Space (char said 1))
         (search "…" said))))

(defun navigate-hint-p (text)
  (or (has text "tab/arrow keys to navigate")
      (has text "arrow keys to navigate")
      (has text "arrows to navigate")
      (has text "↑/↓ to navigate")
      (has text "↑↓ to navigate")))

(defparameter *claude-code-rules*
  (list
   (make-rule :id "osc-title-working" :state :working :priority 1100 :region :title
              :test (lambda (text)
                      (and (> (length text) 1)
                           (busy-glyph-p (char text 0))
                           (char= #\Space (char text 1)))))
   (make-rule :id "transcript-viewer" :state :skip :priority 1000 :region '(:bottom 3)
              :test (lambda (text)
                      (and (has text "showing detailed transcript")
                           (or (has text "ctrl+o" "to toggle")
                               (has text "ctrl+e" "show all")
                               (has text "ctrl+e" "collapse")
                               (has text "↑↓ scroll")
                               (has text "? for shortcuts")))))
   (make-rule :id "live-blocked-form" :state :blocked :priority 980 :region :after-last-rule
              :test (lambda (text)
                      (and (has text "esc to cancel")
                           (or (has text "enter to confirm")
                               (and (has text "enter to select") (navigate-hint-p text))))))
   (make-rule :id "dynamic-workflow-prompt" :state :blocked :priority 980 :region :whole
              :test (lambda (text) (has text "run a dynamic workflow?" "esc to cancel")))
   (make-rule :id "mcp-elicitation-prompt" :state :blocked :priority 980 :region :whole
              :test (lambda (text)
                      (and (has text "esc to cancel")
                           (any-line text (lambda (l) (and (starts l "mcp server")
                                                           (has l "requests your input"))))
                           (any-line text (lambda (l) (option-line-p l "accept" "decline"))))))
   (make-rule :id "btw-overlay" :state :working :priority 975 :region '(:bottom 5)
              :test (lambda (text)
                      (and (any-line text (lambda (l) (starts l "/btw")))
                           (any-line text (lambda (l)
                                            (let ((s (string-trim " " l)))
                                              (and (>= (length s) 12)
                                                   (string-equal "esc to close"
                                                                 s :start2 (- (length s) 12)))))))))
   (make-rule :id "live-turn-working" :state :working :priority 970 :region '(:bottom 12)
              :test (lambda (text)
                      (or (any-line text (lambda (l)
                                           (let ((s (string-trim " " l)))
                                             (and (plusp (length s))
                                                  (find (char s 0) "⏸⏵")
                                                  (has s "esc to interrupt")))))
                          (any-line text #'spinner-line-p))))
   (make-rule :id "background-agents-working" :state :working :priority 965
              :region :last-above-prompt-box
              :test (lambda (text)
                      (let ((said (string-trim " " text)))
                        (and (> (length said) 2)
                             (find (char said 0) "*·✢✶✻✽")
                             (has said "waiting for" "background agent" "to finish")))))
   (make-rule :id "live-prompt-box" :state :idle :priority 950 :region :prompt-box
              :test (lambda (text)
                      (and (any-line text (lambda (l) (starts l "❯")))
                           (not (has text "enter to select"))
                           (not (has text "esc to cancel"))
                           (not (navigate-hint-p text)))))
   (make-rule :id "model-picker-menu" :state :skip :priority 900 :region :whole
              :test (lambda (text)
                      (and (has text "select model" "enter to set as default" "esc to cancel")
                           (not (has text "do you want to proceed?"))
                           (not (has text "enter to select")))))
   (make-rule :id "bash-permission-prompt" :state :blocked :priority 850 :region :whole
              :test (lambda (text)
                      (and (has text "do you want to proceed?")
                           (or (has text "bash command") (has text "bash(")
                               (has text "contains expansion") (has text "tab to amend")
                               (has text "ctrl+e to explain"))
                           (any-line text (lambda (l)
                                            (option-line-p l "yes" "1. yes" "2. yes"
                                                           "2. no" "3. no"))))))
   (make-rule :id "generic-permission-prompt" :state :blocked :priority 840
              :region :after-last-rule
              :test (lambda (text)
                      (and (has text "do you want to proceed?" "esc to cancel")
                           (any-line text (lambda (l)
                                            (option-line-p l "1. yes" "2. yes" "2. no" "3. no"))))))
   (make-rule :id "legacy-no-prompt-blocker" :state :blocked :priority 300 :region :whole
              :test (lambda (text)
                      (and (or (and (has text "do you want to")
                                    (or (has text "yes") (has text "❯")))
                               (and (has text "would you like to")
                                    (or (has text "yes") (has text "❯")))
                               (has text "waiting for permission")
                               (has text "do you want to allow this connection?")
                               (has text "tab to amend")
                               (has text "ctrl+e to explain")
                               (has text "do you want to proceed?" "esc to cancel")
                               (has text "review your answers")
                               (has text "skip interview and plan immediately"))
                           (not (any-line text (lambda (l) (string= "❯" (string-trim " " l))))))))))

(defmethod agent-rules ((agent claude-code))
  *claude-code-rules*)
