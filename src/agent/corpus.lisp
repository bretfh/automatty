;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/agent)

(defun step-keys (reader observation do)
  (if (eq :press (first do))
      (list (press-keys (second do)))
      (action-keys reader observation (first do) (second do))))

(defun write-expect (expect stream)
  (with-standard-io-syntax
    (let ((*print-readably* nil) (*print-right-margin* 100) (*print-pretty* t))
      (prin1 expect stream)
      (terpri stream))))

(defun read-expect (stream)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read stream))))

(defun corpus-paths (dir)
  (values (merge-pathnames "expect.lisp" dir) (merge-pathnames "corpus.bytes" dir)))

(defun load-corpus (dir)
  (multiple-value-bind (expect bytes) (corpus-paths dir)
    (values (with-open-file (in expect :external-format :utf-8) (read-expect in))
            (with-open-file (in bytes :element-type '(unsigned-byte 8))
              (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
                (read-sequence octets in)
                octets)))))

(defun save-corpus (dir expect octets)
  (ensure-directories-exist dir)
  (multiple-value-bind (expect-path bytes-path) (corpus-paths dir)
    (with-open-file (out expect-path :direction :output :if-exists :supersede
                                     :external-format :utf-8)
      (write-expect expect out))
    (with-open-file (out bytes-path :direction :output :if-exists :supersede
                                    :element-type '(unsigned-byte 8))
      (write-sequence octets out)))
  dir)

(defun feed (term decoder octets from to)
  (when (< from to)
    (term:term-process-output
     term (term:decode-utf-8 decoder (map 'string #'code-char (subseq octets from to))))))

(defun same-choice-p (seen saw)
  (and (equal (getf seen :question) (getf saw :question))
       (equal (getf seen :options) (getf saw :options))
       (eql (getf seen :selected) (getf saw :selected))))

(defun judge-step (reader step seen before)
  (let ((want (getf step :screen))
        (wrong nil))
    (unless (eq want (getf seen :screen))
      (push (list :screen want :saw (getf seen :screen)) wrong))
    (when (and (eq want (getf seen :screen)) (eq :choice (getf seen :widget))
               (eq want (getf (getf step :saw) :screen))
               (not (same-choice-p seen (getf step :saw))))
      (push (list :choice (list :question (getf seen :question) :options (getf seen :options)
                                :selected (getf seen :selected))
                  :recorded (list :question (getf (getf step :saw) :question)
                                  :options (getf (getf step :saw) :options)
                                  :selected (getf (getf step :saw) :selected)))
            wrong))
    (when (getf step :sent)
      (let ((keys (step-keys reader before (getf step :via))))
        (unless (equal keys (getf step :sent))
          (push (list :keys (getf step :via) :would-send keys :sent (getf step :sent)) wrong))))
    (cond ((eq :timeout (getf step :cut)) (list* :unreached (nreverse wrong)))
          (wrong (list* :fail (nreverse wrong)))
          (t (list :pass)))))

(defun replay (reader expect octets)
  (destructuring-bind (&key width height steps &allow-other-keys) (nthcdr 2 expect)
    (let ((term (term:make-term :width width :height height))
          (decoder (term:make-decoder))
          (at 0)
          (before nil))
      (loop :for step :in steps
            :collect (if (eq :skipped (getf step :cut))
                         (list (getf step :screen) :skipped)
                         (progn
                           (feed term decoder octets at (getf step :at))
                           (setf at (getf step :at))
                           (let ((seen (observe reader term)))
                             (prog1 (list* (getf step :screen) (judge-step reader step seen before))
                               (setf before seen)))))))))

(defun verdict-passed-p (verdict)
  (every (lambda (row) (member (second row) '(:pass :skipped))) verdict))
