;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/agent)

(defparameter +hold+ 700)
(defparameter +turn-patience+ 60000)
(defparameter +trace-length+ 4000)
(defparameter +history-length+ 512
  "How many changes of state an agent keeps. A trace is every look and is for
finding out why; this is only what it became and when, and is what a timeline of
the last while is drawn from.")

(defclass agent ()
  ((state :initform :unknown :accessor agent-state)
   (quiet-since :initform nil :accessor agent-quiet-since)
   (moved :initform nil :accessor agent-moved)
   (heard :initform nil :accessor agent-heard)
   (trace :initform nil :accessor agent-trace)
   (traced :initform 0 :accessor agent-traced)
   (prompted :initform nil :accessor agent-prompted-at)
   (since :initform nil :accessor agent-since)
   (history :initform nil :accessor agent-history)
   (historied :initform 0 :accessor agent-historied)))

(defstruct rule id state priority region test)

(defvar *agents* nil)

(defgeneric agent-rules (agent)
  (:method ((agent agent)) nil))

(defun recognized (title command &optional programs)
  (loop :for (class . test) :in *agents*
        :when (or (funcall test (or title "") (or command ""))
                  (some (lambda (line) (funcall test "" line)) programs))
          :return class))

(defun agent-become (agent title command &optional programs)
  (let ((class (or (recognized title command programs) 'agent)))
    (unless (eq class (type-of agent))
      (change-class agent class)
      (setf (agent-state agent) :unknown
            (agent-quiet-since agent) nil
            (agent-heard agent) nil))
    agent))

(defun make-agent (&optional title command programs)
  (agent-become (make-instance 'agent) title command programs))

(defun agent-prompted (agent now)
  (when (agent-rules agent)
    (setf (agent-prompted-at agent) now)))

(defun traced (agent now moved said state)
  (push (list now moved said state) (agent-trace agent))
  (when (> (incf (agent-traced agent)) +trace-length+)
    (setf (agent-trace agent) (subseq (agent-trace agent) 0 (floor +trace-length+ 2))
          (agent-traced agent) (floor +trace-length+ 2))))

(defun became (agent now state)
  "AGENT is STATE from NOW on: when it changed, and a line in its history."
  (setf (agent-since agent) now)
  (push (list now state) (agent-history agent))
  (when (> (incf (agent-historied agent)) +history-length+)
    (setf (agent-history agent) (subseq (agent-history agent) 0 (floor +history-length+ 2))
          (agent-historied agent) (floor +history-length+ 2))))

(defun agent-for (agent now)
  "How many milliseconds AGENT has been what it is, or nil when it has never
been anything."
  (and (agent-since agent) (max 0 (- now (agent-since agent)))))

(defun screen-lines (term)
  (let ((lines (loop :for y :below (term:term-height term)
                     :collect (string-right-trim " " (term:term-dump-row-string term y)))))
    (subseq lines 0 (1+ (or (position-if (lambda (l) (plusp (length l))) lines :from-end t)
                            -1)))))

(defun last-lines (term n)
  (let* ((screen (screen-lines term))
         (above (max 0 (- n (length screen))))
         (size (term:term-scrollback-size term)))
    (append (loop :for i :from (max 0 (- size above)) :below size
                  :collect (string-right-trim " " (term:term-scrollback-row-string term i)))
            (last screen (min n (length screen))))))

(defun rule-line-p (line)
  (let* ((said (string-trim " " line))
         (run (or (position #\─ said :test-not #'char=) (length said))))
    (and (plusp run)
         (or (= run (length said)) (>= run 3)))))

(defun bottom-non-empty (lines n)
  (let ((from (loop :with seen := 0
                    :for i :from (1- (length lines)) :downto 0
                    :when (plusp (length (string-trim " " (nth i lines))))
                      :do (incf seen)
                          (when (= seen n) (return i))
                    :finally (return (if (plusp seen) 0 (length lines))))))
    (nthcdr from lines)))

(defun after-last-rule (lines)
  (let ((at (position-if #'rule-line-p lines :from-end t)))
    (if at (nthcdr (1+ at) lines) lines)))

(defun prompt-box-top (lines)
  (loop :with seen := 0
        :for i :from (1- (length lines)) :downto 0
        :when (rule-line-p (nth i lines))
          :do (incf seen)
              (when (= seen 2) (return i))))

(defun prompt-box-body (lines)
  (let ((top (prompt-box-top lines)))
    (when top
      (let* ((rest (nthcdr (1+ top) lines))
             (end (position-if #'rule-line-p rest)))
        (subseq rest 0 end)))))

(defun above-prompt-box (lines)
  (let ((top (prompt-box-top lines)))
    (if top (subseq lines 0 top) lines)))

(defun region-lines (term lines region)
  (cond
    ((eq region :whole) lines)
    ((eq region :title) (list (term:term-title term)))
    ((eq region :after-last-rule) (after-last-rule lines))
    ((eq region :prompt-box) (prompt-box-body lines))
    ((eq region :above-prompt-box) (above-prompt-box lines))
    ((eq region :last-above-prompt-box)
     (let ((it (find-if (lambda (l) (plusp (length (string-trim " " l))))
                        (above-prompt-box lines) :from-end t)))
       (and it (list it))))
    ((and (consp region) (eq (first region) :bottom))
     (bottom-non-empty lines (second region)))
    (t nil)))

(defun region-text (term region)
  (format nil "~{~A~^~%~}" (region-lines term (screen-lines term) region)))

(defun has (text &rest wanted)
  (every (lambda (w) (search w text :test #'char-equal)) wanted))

(defun lines-of (text)
  (loop :with start := 0
        :for nl := (position #\Newline text :start start)
        :collect (subseq text start nl)
        :while nl :do (setf start (1+ nl))))

(defun any-line (text test)
  (some test (lines-of text)))

(defun starts (line &rest heads)
  (let ((said (string-trim " " line)))
    (some (lambda (head)
            (and (>= (length said) (length head))
                 (string-equal head said :end2 (length head))))
          heads)))

(defun option-line-p (line &rest heads)
  (let ((said (string-left-trim " ❯" (string-trim " " line))))
    (apply #'starts said heads)))

(defun judged (rules term)
  (let ((lines (screen-lines term))
        (texts nil)
        (rows nil)
        (won nil))
    (flet ((text-of (region)
             (let ((seen (assoc region texts :test #'equal)))
               (if seen
                   (cdr seen)
                   (let ((text (format nil "~{~A~^~%~}" (region-lines term lines region))))
                     (push (cons region text) texts)
                     text)))))
      (dolist (rule rules)
        (let* ((text (text-of (rule-region rule)))
               (hit (and (plusp (length text)) (funcall (rule-test rule) text) t)))
          (push (list rule hit text) rows)
          (when (and hit (or (null won) (> (rule-priority rule) (rule-priority won))))
            (setf won rule)))))
    (values won (nreverse rows))))

(defgeneric agent-signal (agent term)
  (:method-combination or))

(defmethod agent-signal or ((agent agent) term)
  (let* ((rules (agent-rules agent))
         (won (judged rules term)))
    (cond (won (rule-state won))
          ((agent-heard agent) (agent-heard agent))
          (rules :unknown)
          ((agent-moved agent) :busy)
          (t :quiet))))

(defun agent-hear (agent state)
  (setf (agent-heard agent) state))

(defun agent-look (agent term now moved)
  (setf (agent-moved agent) moved)
  (when moved (setf (agent-heard agent) nil))
  (let ((was (agent-state agent))
        (said nil))
    (flet ((publish (state)
             (let ((asked (agent-prompted-at agent)))
               (when asked
                 (cond ((member state '(:working :blocked))
                        (setf (agent-prompted-at agent) nil))
                       ((< (- now asked) +turn-patience+)
                        (setf state :working))
                       (t (setf (agent-prompted-at agent) nil)))))
             (unless (eq state (agent-state agent))
               (became agent now state))
             (setf (agent-quiet-since agent) nil
                   (agent-state agent) state)))
      (if (and (not moved) (null (agent-quiet-since agent)) (null (agent-heard agent))
               (null (agent-prompted-at agent))
               (member was '(:idle :blocked)))
          (setf said :settled)
          (let ((heard (agent-heard agent)))
            (setf said (agent-signal agent term)
                  (agent-heard agent) nil)
            (case said
              (:skip)
              (:busy (publish :working))
              (:quiet
               (if (eq was :working)
                   (let ((since (or (agent-quiet-since agent)
                                    (setf (agent-quiet-since agent) now))))
                     (when (>= (- now since) +hold+) (publish :idle)))
                   (publish :idle)))
              (t (publish said)))
            (when (and heard (eq said heard))
              (setf said (list :heard said))))))
    (traced agent now moved said (agent-state agent))
    (not (eq was (agent-state agent)))))

(defun agent-explain (agent term)
  (let ((rules (agent-rules agent)))
    (multiple-value-bind (won rows) (judged rules term)
      (values (cond (won (rule-state won))
                    (rules :unknown)
                    ((agent-moved agent) :busy)
                    (t :quiet))
              (mapcar (lambda (row)
                        (destructuring-bind (rule hit text) row
                          (list (rule-id rule) (rule-priority rule) (rule-region rule)
                                (rule-state rule) hit
                                (subseq text 0 (min 240 (length text))))))
                      rows)))))

(defgeneric agent-doing (agent term)
  (:documentation "One line of what AGENT is doing, in its own words where its
screen has any, for somebody glancing at it rather than reading it; nil when
its screen says nothing a line could.")
  (:method ((agent agent) term)
    (declare (ignore term))
    nil))

(defun agent-won (agent term)
  "The id of the rule that says what AGENT's screen is, or nil when none does."
  (let ((won (judged (agent-rules agent) term)))
    (and won (rule-id won))))

(defun screen-blocked-p (term)
  (loop :for (class . nil) :in *agents*
        :thereis (let ((won (judged (agent-rules (make-instance class)) term)))
                   (and won (eq :blocked (rule-state won))))))

;;; What a blocked program is asking. A dialog in a terminal is a subject, a few
;;; lines of what it is about, a question, and numbered options, one of them
;;; pointed at. Nothing here knows any one program's wording: a program whose
;;; dialogs are shaped like that is read by this, and a program whose are not
;;; says so with a method of its own.

(defun option-of (line)
  "LINE as (number text chosen) when it is a numbered option, such as
\"❯ 1. Yes\" or \"  2. No, and tell Claude what to do differently (esc)\"."
  (let* ((said (string-trim " " line))
         (chosen (and (plusp (length said)) (char= (char said 0) #\❯)))
         (said (string-left-trim " ❯>" said))
         (dot (position #\. said)))
    (when (and dot (plusp dot) (< dot 3)
               (every #'digit-char-p (subseq said 0 dot))
               (< (1+ dot) (length said))
               (char= #\Space (char said (1+ dot))))
      (list (parse-integer said :end dot)
            (string-trim " " (subseq said (1+ dot)))
            chosen))))

(defun paragraphs (lines)
  "LINES cut at the empty ones, the empty ones left out."
  (let ((out nil) (now nil))
    (dolist (line lines)
      (if (zerop (length line))
          (when now (push (nreverse now) out) (setf now nil))
          (push line now)))
    (when now (push (nreverse now) out))
    (nreverse out)))

(defun asks-of-lines (lines)
  "What LINES are asking, as a plist of :subject :detail :question :options and
:chosen, or nil when they hold no numbered options with a question above them.

LINES is what is under the last rule on the screen, which is where a dialog is
drawn: the subject is the first line of it, the question the nearest line above
the options that ends in a question mark, and the detail what is between."
  (let* ((lines (mapcar (lambda (l) (string-right-trim " " l)) lines))
         (first-option (position-if #'option-of lines))
         (question-at (and first-option
                           (position-if (lambda (l)
                                          (let ((s (string-trim " " l)))
                                            (and (plusp (length s))
                                                 (char= #\? (char s (1- (length s)))))))
                                        lines :end first-option :from-end t))))
    (when question-at
      (let* ((options (loop :for l :in (nthcdr first-option lines)
                            :for o := (option-of l)
                            :while (or o (zerop (length (string-trim " " l))))
                            :when o :collect o))
             (subject-at (position-if (lambda (l) (plusp (length (string-trim " " l))))
                                      lines :end question-at))
             (detail (and subject-at
                          (loop :for paragraph :in (paragraphs
                                                    (mapcar (lambda (l) (string-trim " " l))
                                                            (subseq lines (1+ subject-at) question-at)))
                                ;; a program's advice about itself is not what
                                ;; it is asking about
                                :unless (starts (first paragraph) "Tip:")
                                  :append paragraph))))
        (list :subject (string-trim " " (nth (or subject-at question-at) lines))
              :detail detail
              :question (string-trim " " (nth question-at lines))
              :options (mapcar (lambda (o) (list (first o) (second o))) options)
              :chosen (first (find-if #'third options)))))))

(defgeneric agent-asks (agent term)
  (:documentation "What AGENT is asking, when it is blocked on a question, as
ASKS-OF-LINES says it; nil when it is not blocked or asks nothing a person
could answer by number.")
  (:method ((agent agent) term)
    (and (eq :blocked (agent-state agent))
         (asks-of-lines (region-lines term (screen-lines term) :after-last-rule)))))
