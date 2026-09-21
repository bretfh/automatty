;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/agent)

(defstruct reader name programs title from-path versions from launch submit interrupt screens scenario form)

(defstruct entry id widget means match question lines choose actions input)

(defvar *readers* nil)

(defparameter +widgets+ '(:prompt-input :spinner :footer :title :choice :framed))
(defparameter +meanings+ '(:working :blocked :idle :skip))
(defparameter +down+ (format nil "~C[B" #\Escape))
(defparameter +up+ (format nil "~C[A" #\Escape))

(defparameter +key-names+
  `(("RET" . ,(string #\Return)) ("TAB" . ,(string #\Tab)) ("ESC" . ,(string #\Escape))
    ("SPC" . " ") ("Up" . ,+up+) ("Down" . ,+down+)
    ("Right" . ,(format nil "~C[C" #\Escape)) ("Left" . ,(format nil "~C[D" #\Escape))))

(defun key-named (name)
  (cond ((rest (assoc name +key-names+ :test #'string=)))
        ((and (= 3 (length name)) (string= "C-" name :end2 2))
         (string (code-char (logand (char-code (char-downcase (char name 2))) 31))))
        (t name)))

(defun press-keys (text)
  (format nil "~{~A~}"
          (mapcar #'key-named
                  (loop :with at := 0
                        :for space := (position #\Space text :start at)
                        :collect (subseq text at space)
                        :while space :do (setf at (1+ space))))))

(define-condition bad-reader (error)
  ((why :initarg :why :reader bad-reader-why))
  (:report (lambda (c s) (format s "not a reader: ~A" (bad-reader-why c)))))

(defun refuse (fmt &rest args)
  (error 'bad-reader :why (apply #'format nil fmt args)))

(defun regex-of (text what)
  (cond ((null text) nil)
        ((stringp text)
         (handler-case (ppcre:create-scanner text)
           (ppcre:ppcre-syntax-error (e) (refuse "~A ~S: ~A" what text e))))
        (t (refuse "~A must be a string, not ~S" what text))))

(defun one-of (value allowed what)
  (unless (member value allowed)
    (refuse "~A ~S is not one of ~S" what value allowed))
  value)

(defun strings-of (value what)
  (unless (and (listp value) (every #'stringp value))
    (refuse "~A must be a list of strings, not ~S" what value))
  value)

(defun action-of (name spec)
  (unless (keywordp name) (refuse "an action is named by a keyword, not ~S" name))
  (unless (and (consp spec) (= 2 (length spec)) (stringp (second spec)))
    (refuse "action ~S is (:option \"regex\"), (:press \"key names\") or (:keys \"keys\"), not ~S"
            name spec))
  (cons name (ecase (one-of (first spec) '(:option :keys :press) "action kind")
               (:option (list :option (regex-of (second spec) "option")))
               (:press (list :keys (press-keys (second spec))))
               (:keys (list :keys (second spec))))))

(defun entry-of (form)
  (unless (and (consp form) (keywordp (first form)) (evenp (length (rest form))))
    (refuse "a screen is (id :widget … :means …), not ~S" form))
  (handler-case
      (destructuring-bind (&key widget means match question lines choose actions input) (rest form)
        (one-of widget +widgets+ "widget")
        (when (and (member widget '(:footer :title)) (null match))
          (refuse "a ~(~A~) screen needs :match" widget))
        (unless (or (null lines) (and (integerp lines) (<= 1 lines 50)))
          (refuse ":lines ~S is not a count of lines" lines))
        (unless (and (listp actions) (evenp (length actions)))
          (refuse ":actions must be a plist, not ~S" actions))
        (make-entry :id (first form)
                    :widget widget
                    :means (one-of means +meanings+ "means")
                    :match (regex-of match "match")
                    :question (regex-of question "question")
                    :lines (or lines 3)
                    :choose (one-of (or choose :arrows) '(:arrows :digits) "choose")
                    :input (and input t)
                    :actions (loop :for (name spec) :on actions :by #'cddr
                                   :collect (action-of name spec))))
    (program-error (e) (refuse "~S: ~A" form e))))

(defparameter +step-actions+ '(:press :submit :choose :approve :approve-always :deny :interrupt))

(defun step-of (form)
  (unless (and (consp form) (keywordp (first form)) (evenp (length (rest form))))
    (refuse "a step is (screen :do (…) :optional t), not ~S" form))
  (handler-case
      (destructuring-bind (&key do optional) (rest form)
        (when do
          (unless (and (consp do) (member (first do) +step-actions+))
            (refuse "a step does one of ~S, not ~S" +step-actions+ do))
          (case (first do)
            ((:press :submit) (unless (and (= 2 (length do)) (stringp (second do)))
                                (refuse "~S takes one string" do)))
            (:choose (unless (and (= 2 (length do)) (integerp (second do)))
                       (refuse "~S takes an option number" do)))
            (t (unless (= 1 (length do)) (refuse "~S takes nothing" do)))))
        (list :screen (first form) :do do :optional (and optional t)))
    (program-error (e) (refuse "~S: ~A" form e))))

(defun inherited-screens (parent screens)
  (let ((own (mapcar #'entry-of screens)))
    (if (null parent)
        own
        (let ((ids (remove-duplicates (mapcar #'entry-id own)))
              (placed nil))
          (append (remove-if (lambda (entry) (find (entry-id entry) (reader-screens parent)
                                                   :key #'entry-id))
                             own)
                  (loop :for entry :in (reader-screens parent)
                        :append (cond ((not (member (entry-id entry) ids)) (list entry))
                                      ((member (entry-id entry) placed) nil)
                                      (t (push (entry-id entry) placed)
                                         (remove-if-not (lambda (o) (eq (entry-id o) (entry-id entry)))
                                                        own)))))))))

(defun reader-of (form)
  (unless (and (consp form) (symbolp (first form)) (first form) (evenp (length (rest form))))
    (refuse "a reader is (name :programs … :screens …), not ~S" form))
  (handler-case
      (destructuring-bind (&key programs title version versions from launch (submit :typed) interrupt
                             screens scenario)
          (rest form)
        (unless (or (null version)
                    (and (consp version) (eq :exec-path (first version)) (stringp (second version))))
          (refuse ":version is (:exec-path \"regex\"), not ~S" version))
        (unless (or (null interrupt) (stringp interrupt))
          (refuse ":interrupt is the keys that interrupt, not ~S" interrupt))
        (let ((parent (and from (or (and (stringp from) (reader-for-version (first form) from))
                                    (refuse ":from ~S names no reader of ~(~A~) there is" from (first form))))))
          (unless (or parent (and screens (listp screens)))
            (refuse "a reader needs :screens"))
          (unless (listp scenario)
            (refuse ":scenario is a list of steps, not ~S" scenario))
          (make-reader :name (string-downcase (symbol-name (first form)))
                       :programs (if parent (reader-programs parent) (strings-of programs "programs"))
                       :title (if (and parent (null title)) (reader-title parent) (regex-of title "title"))
                       :from-path (if (and parent (null version))
                                      (reader-from-path parent)
                                      (and version (regex-of (second version) "version")))
                       :versions (strings-of versions "versions")
                       :from from
                       :launch (if (and parent (null launch))
                                   (reader-launch parent)
                                   (strings-of launch "launch"))
                       :submit (one-of submit '(:typed :paste) "submit")
                       :interrupt (or interrupt (and parent (reader-interrupt parent)) (string #\Escape))
                       :screens (inherited-screens parent screens)
                       :scenario (if (and parent (null scenario))
                                     (reader-scenario parent)
                                     (mapcar #'step-of scenario))
                       :form form)))
    (program-error (e) (refuse "~A" e))))

(defun same-reader-p (a b)
  (and (string= (reader-name a) (reader-name b))
       (equal (reader-versions a) (reader-versions b))))

(defun register-reader (form)
  (let ((reader (reader-of form)))
    (setf *readers*
          (append (remove reader *readers* :test #'same-reader-p)
                  (list reader)))
    reader))

(defun reader-for-version (name version)
  (find-if (lambda (reader)
             (and (string-equal (reader-name reader) (string name))
                  (member version (reader-versions reader) :test #'string=)))
           *readers*))

(defun version-parts (version)
  (loop :with at := 0
        :for dot := (position #\. version :start at)
        :collect (or (parse-integer version :start at :end dot :junk-allowed t) 0)
        :while dot :do (setf at (1+ dot))))

(defun version< (a b)
  (loop :for x :in (version-parts a)
        :for y :in (version-parts b)
        :when (< x y) :return t
        :when (> x y) :return nil
        :finally (return (< (length (version-parts a)) (length (version-parts b))))))

(defun nearest-reader (name version)
  (let ((known (loop :for reader :in *readers*
                     :when (string-equal (reader-name reader) (string name))
                       :append (mapcar (lambda (v) (cons v reader)) (reader-versions reader)))))
    (or (rest (find version known :key #'first :test #'string=))
        (rest (first (last (sort (remove-if-not (lambda (v) (not (version< version v))) known
                                                :key #'first)
                                 #'version< :key #'first))))
        (rest (first (sort known #'version< :key #'first))))))

(defmacro defreader (name &body plist)
  `(register-reader '(,name ,@plist)))

(defun read-reader (stream)
  (with-standard-io-syntax
    (let ((*read-eval* nil)
          (*package* (find-package :keyword)))
      (read stream))))

(defun reader-named (name)
  (find name *readers* :key #'reader-name :test #'string-equal))

(defun program-name (line)
  (let ((word (subseq line 0 (or (position #\Space line) (length line)))))
    (subseq word (1+ (or (position #\/ word :from-end t) -1)))))

(defun reader-for (&key title command programs)
  (loop :for reader :in *readers*
        :when (or (and (reader-title reader) title (plusp (length title))
                       (ppcre:scan (reader-title reader) title))
                  (some (lambda (line)
                          (member (program-name line) (reader-programs reader) :test #'string=))
                        (remove nil (cons command programs))))
          :return reader))

(defun version-from (reader paths)
  (let ((scanner (reader-from-path reader)))
    (when scanner
      (loop :for path :in paths
            :when path
              :do (ppcre:register-groups-bind (version) (scanner path)
                    (return version))))))

(defun entry-sees (entry term lines)
  (let ((match (entry-match entry)))
    (ecase (entry-widget entry)
      (:prompt-input (prompt-input term lines))
      (:spinner (spinner lines))
      (:footer (footer lines :match match :count (entry-lines entry)))
      (:title (title term :match match))
      (:choice (let ((seen (choice term lines)))
                 (and seen
                      (or (null (entry-question entry))
                          (matches (entry-question entry) (getf seen :text)))
                      (or (null match) (matches match (or (getf seen :hint) "")))
                      seen)))
      (:framed (let ((seen (framed lines)))
                 (and seen
                      (or (null match)
                          (matches match (format nil "~{~A~^~%~}" (getf seen :lines))))
                      seen))))))

(defun observe (reader term)
  (let ((lines (screen-lines term)))
    (loop :for entry :in (reader-screens reader)
          :for seen := (entry-sees entry term lines)
          :when seen
            :return (list* :screen (entry-id entry) :means (entry-means entry) seen))))

(defun observe-explained (reader term)
  (let ((lines (screen-lines term))
        (won nil))
    (loop :for entry :in (reader-screens reader)
          :for seen := (entry-sees entry term lines)
          :collect (list (entry-id entry) (entry-widget entry) (entry-means entry)
                         (and seen (not won) (setf won t))
                         seen))))

(defun entry-named (reader id)
  (find id (reader-screens reader) :key #'entry-id))

(defun offered (reader observation)
  (let ((entry (entry-named reader (getf observation :screen))))
    (append (mapcar #'car (and entry (entry-actions entry)))
            (when (eq :choice (getf observation :widget)) '(:choose))
            (when (or (eq :prompt-input (getf observation :widget)) (and entry (entry-input entry)))
              '(:submit))
            (when (eq :working (getf observation :means)) '(:interrupt)))))

(defun choose-keys (entry observation index)
  (let ((options (getf observation :options))
        (from (or (getf observation :selected) 0)))
    (when (and index (< -1 index (length options)))
      (if (and entry (eq :digits (entry-choose entry)) (getf observation :numbered) (< index 9))
          (list (princ-to-string (1+ index)))
          (list (format nil "~{~A~}~C"
                        (make-list (abs (- index from)) :initial-element (if (> index from) +down+ +up+))
                        #\Return))))))

(defun action-keys (reader observation action &optional argument)
  (when (member action (offered reader observation))
    (let ((entry (entry-named reader (getf observation :screen))))
      (case action
        (:submit (and (stringp argument) (list argument (string #\Return))))
        (:interrupt (list (reader-interrupt reader)))
        (:choose (and (integerp argument) (choose-keys entry observation (1- argument))))
        (t (let ((spec (rest (assoc action (entry-actions entry)))))
             (ecase (first spec)
               (:keys (list (second spec)))
               (:option (choose-keys entry observation
                                     (position-if (lambda (o) (ppcre:scan (second spec) o))
                                                  (getf observation :options)))))))))))
