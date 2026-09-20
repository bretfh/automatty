;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx/agent)

(defparameter +hold+ 700)
(defparameter +trace-length+ 4000)

(defclass agent ()
  ((state :initform :unknown :accessor agent-state)
   (quiet-since :initform nil :accessor agent-quiet-since)
   (moved :initform nil :accessor agent-moved)
   (heard :initform nil :accessor agent-heard)
   (trace :initform nil :accessor agent-trace)
   (traced :initform 0 :accessor agent-traced)))

(defstruct rule id state priority region test)

(defvar *agents* nil)

(defgeneric agent-rules (agent)
  (:method ((agent agent)) nil))

(defun recognized (title command)
  (loop :for (class . test) :in *agents*
        :when (funcall test (or title "") (or command "")) :return class))

(defun agent-become (agent title command)
  (let ((class (or (recognized title command) 'agent)))
    (unless (eq class (type-of agent))
      (change-class agent class)
      (setf (agent-state agent) :unknown
            (agent-quiet-since agent) nil
            (agent-heard agent) nil))
    agent))

(defun make-agent (&optional title command)
  (agent-become (make-instance 'agent) title command))

(defun traced (agent now moved said state)
  (push (list now moved said state) (agent-trace agent))
  (when (> (incf (agent-traced agent)) +trace-length+)
    (setf (agent-trace agent) (subseq (agent-trace agent) 0 (floor +trace-length+ 2))
          (agent-traced agent) (floor +trace-length+ 2))))

(defun screen-lines (term)
  (let ((lines (loop :for y :below (vt:term-height term)
                     :collect (string-right-trim " " (vt:term-dump-row-string term y)))))
    (subseq lines 0 (1+ (or (position-if (lambda (l) (plusp (length l))) lines :from-end t)
                            -1)))))

(defun last-lines (term n)
  (let* ((screen (screen-lines term))
         (above (max 0 (- n (length screen))))
         (size (vt:term-scrollback-size term)))
    (append (loop :for i :from (max 0 (- size above)) :below size
                  :collect (string-right-trim " " (vt:term-scrollback-row-string term i)))
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
    ((eq region :title) (list (vt:term-title term)))
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
             (setf (agent-quiet-since agent) nil
                   (agent-state agent) state)))
      (if (and (not moved) (null (agent-quiet-since agent)) (null (agent-heard agent))
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
