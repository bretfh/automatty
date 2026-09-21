;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/agent)

(defparameter +markers+ "❯›>▸▶►●➤→")

(defparameter +blank+ (list #\Space (code-char #xA0)))

(defparameter +braille+
  (format nil "~C-~C" (code-char #x2800) (code-char #x28FF)))

(defparameter +spinner-scanner+
  (ppcre:create-scanner
   (format nil "^\\s*[*·✢✳✶✻✽◐◑◒◓~A]\\s+(\\S[^…]*?)…(?:\\s*\\((?:(\\d+)m\\s*)?(\\d+)s)?"
           +braille+)))

(defparameter +glyph-spinner-scanner+
  (ppcre:create-scanner
   (format nil "^\\s*[◐◑◒◓~A]+\\s+([A-Za-z][^…]*?)\\s*(?:\\((?:(\\d+)m\\s*)?(\\d+)s|$)" +braille+)))

(defparameter +numbered-scanner+
  (ppcre:create-scanner "^(\\s*)(?:([❯›>▸▶►●➤→])\\s*)?(\\d+)\\.\\s+(\\S.*)$"))

(defparameter +marked-scanner+
  (ppcre:create-scanner "^(\\s*)([❯›>▸▶►●➤→])\\s+(\\S.*)$"))

(defun indent-of (line)
  (or (position #\Space line :test-not #'char=) (length line)))

(defun blank-p (line)
  (zerop (length (string-trim +blank+ line))))

(defun matches (regex text)
  (and regex text (ppcre:scan regex text) t))

(defun rule-rows (lines)
  (loop :for line :in lines
        :for y :from 0
        :when (rule-row-p line) :collect y))

(defun prompt-input (term lines)
  (let* ((rules (rule-rows lines))
         (bottom (first (last rules)))
         (top (first (last rules 2))))
    (when (and top bottom (< top bottom) (> (- bottom top) 1)
               (<= (count-if-not #'blank-p (nthcdr (1+ bottom) lines)) 3))
      (let* ((body (subseq lines (1+ top) bottom))
             (first-line (find-if-not #'blank-p body))
             (said (and first-line (string-left-trim +blank+ first-line))))
        (when (and said (find (char said 0) +markers+))
          (let* ((y (+ 1 top (position first-line body)))
                 (from (1+ (position (char said 0) first-line)))
                 (text (string-trim +blank+ (format nil "~{~A~^ ~}"
                                                (cons (subseq first-line from)
                                                      (mapcar (lambda (l) (string-trim +blank+ l))
                                                              (rest (member first-line body))))))))
            (list :widget :prompt-input
                  :text text
                  :ghost (and (plusp (length text)) (ghost-row-p term y from)))))))))

(defun ghost-row-p (term y from)
  (let ((row (term:term-grid-row term y)))
    (loop :with seen := nil
          :for x :from from :below (term:term-width term)
          :for ch := (term:row-char row x)
          :for face := (term:row-face row x)
          :unless (member ch +blank+)
            :do (setf seen t)
                (unless (and face (or (term:face-faint face) (grey-p (term:face-fg face))))
                  (return nil))
          :finally (return seen))))

(defun grey-p (color)
  (and (consp color)
       (= (first color) (second color) (third color))
       (< (first color) 160)))

(defun spinner (lines &key (count 12))
  (loop :for line :in (bottom-non-empty lines count)
        :do (dolist (scanner (list +spinner-scanner+ +glyph-spinner-scanner+))
              (ppcre:register-groups-bind (label minutes seconds) (scanner line)
                (return-from spinner
                  (list :widget :spinner
                        :label (string-trim +blank+ label)
                        :seconds (and seconds
                                      (+ (* 60 (if minutes (parse-integer minutes) 0))
                                         (parse-integer seconds)))))))))

(defun footer (lines &key match (count 3))
  (loop :for line :in (bottom-non-empty lines count)
        :when (matches match line)
          :return (list :widget :footer :line (string-trim +blank+ line))))

(defun title (term &key match)
  (let ((said (term:term-title term)))
    (when (matches match said)
      (list :widget :title :text said))))

(defun numbered-options (lines)
  (let ((found nil))
    (loop :for line :in lines
          :for y :from 0
          :do (ppcre:register-groups-bind (indent marker number label)
                  (+numbered-scanner+ (left-column line))
                (push (list :y y :number (parse-integer number) :marker (and marker t)
                            :label (string-trim +blank+ label) :column (length indent))
                      found)))
    (let ((run nil))
      (dolist (option found)
        (cond ((null run) (push option run))
              ((= (getf option :number) (1- (getf (first run) :number))) (push option run))
              ((= 1 (getf (first run) :number)) (return))
              (t (setf run (list option)))))
      (when (and (>= (length run) 2) (= 1 (getf (first run) :number)))
        run))))

(defun marked-options (lines)
  (loop :for line :in lines
        :for y :from 0
        :for found := (multiple-value-bind (start end starts) (ppcre:scan +marked-scanner+ line)
                        (declare (ignore end))
                        (and start
                             (list :y y :marker t
                                   :label (string-trim +blank+ (left-column (subseq line (aref starts 2))))
                                   :column (aref starts 2))))
        :with best := nil
        :when found
          :do (let ((group (list found)))
                (flet ((sibling (at)
                         (let ((other (and (<= 0 at (1- (length lines))) (nth at lines))))
                           (and other (not (blank-p other))
                                (= (indent-of other) (getf found :column))
                                (list :y at :marker nil
                                      :label (string-trim +blank+ (left-column other))
                                      :column (getf found :column))))))
                  (loop :for at :downfrom (1- y)
                        :for other := (sibling at)
                        :while other :do (push other group))
                  (loop :for at :from (1+ y)
                        :for other := (sibling at)
                        :while other :do (setf group (append group (list other)))))
                (when (>= (length group) 2)
                  (setf best group)))
        :finally (return best)))

(defun option-details (lines options)
  (loop :for (option next) :on options
        :collect (let ((from (1+ (getf option :y)))
                       (to (if next (getf next :y) (length lines))))
                   (loop :for y :from from :below to
                         :for line := (nth y lines)
                         :while (and (> (indent-of line) (getf option :column))
                                     (not (rule-row-p line)))
                         :collect (string-trim +blank+ (left-column line))))))

(defun border-p (line)
  (let ((said (string-trim +blank+ line)))
    (and (plusp (length said)) (find (char said 0) "╭╰"))))

(defun unframed (lines)
  (let ((frame (framed lines)))
    (if (null frame)
        (values lines nil)
        (values (loop :for line :in lines
                      :for y :from 0
                      :collect (let ((left (position #\│ line))
                                     (right (position #\│ line :from-end t)))
                                 (if (and (< (getf frame :top) y (getf frame :bottom))
                                          left right (< left right))
                                     (let ((copy (copy-seq line)))
                                       (setf (char copy left) #\Space
                                             (char copy right) #\Space)
                                       copy)
                                     line)))
                frame))))

(defun choice (term lines)
  (multiple-value-bind (lines frame) (unframed lines)
    (choice-in term lines frame)))

(defun choice-in (term lines frame)
  (let ((options (or (numbered-options lines) (marked-options lines))))
    (when options
      (let* ((first-y (getf (first options) :y))
             (last-y (getf (first (last options)) :y))
             (inside (and frame (< (getf frame :top) first-y (getf frame :bottom))))
             (start (if inside
                        (1+ (getf frame :top))
                        (let ((rule (position-if #'rule-row-p lines :end first-y :from-end t)))
                          (if rule (1+ rule) 0))))
             (above (loop :for y :from start :below first-y
                          :for line := (left-column (nth y lines))
                          :unless (blank-p line) :collect (string-trim +blank+ line)))
             (question (find-if (lambda (l) (find #\? l)) above :from-end t))
             (selected (or (position-if (lambda (o) (getf o :marker)) options)
                           (position-if (lambda (o) (row-inverse-p term (getf o :y)
                                                                   (getf o :column)))
                                        options)))
             (hint (find-if (lambda (l) (not (or (blank-p l) (border-p l))))
                            (nthcdr (1+ last-y) lines))))
        (list :widget :choice
              :question question
              :subject (if question (subseq above 0 (position question above :from-end t)) above)
              :text (format nil "~{~A~^~%~}" above)
              :options (mapcar (lambda (o) (getf o :label)) options)
              :details (option-details lines options)
              :selected selected
              :numbered (and (getf (first options) :number) t)
              :hint (and hint (string-trim +blank+ hint)))))))

(defun framed (lines)
  (let* ((top (position-if (lambda (l) (let ((s (string-trim +blank+ l)))
                                         (and (plusp (length s)) (char= (char s 0) #\╭))))
                           lines :from-end t))
         (bottom (and top (position-if (lambda (l) (let ((s (string-trim +blank+ l)))
                                                     (and (plusp (length s)) (char= (char s 0) #\╰))))
                                       lines :start top))))
    (when bottom
      (let ((left (position #\╭ (nth top lines))))
        (list :widget :framed :top top :bottom bottom :left left
              :lines (loop :for y :from (1+ top) :below bottom
                           :collect (string-trim " │" (nth y lines))))))))

(defun said (lines marker)
  (let ((line (find-if (lambda (l)
                         (let ((s (string-trim +blank+ l)))
                           (and (> (length s) (length marker))
                                (string= marker s :end2 (length marker)))))
                       (above-prompt-box lines) :from-end t)))
    (and line (string-trim (append +blank+ (coerce marker 'list)) line))))
