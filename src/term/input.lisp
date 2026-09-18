;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt)

(defvar *function-key-table*
  (map 'vector (lambda (tail) (format nil "~C~A" #\Escape tail))
       #("" "OP" "OQ" "OR" "OS" "[15~" "[17~" "[18~"
         "[19~" "[20~" "[21~" "[23~" "[24~")))

(defun modifier-code (shift meta ctrl)
  (1+ (logior (if shift 1 0)
              (if meta 2 0)
              (if ctrl 4 0))))

(defun arrow-key-sequence (term key &key shift meta ctrl)
  (let ((ch (case key
              (:up #\A) (:down #\B) (:right #\C) (:left #\D)
              (:home #\H) (:end #\F))))
    (when ch
      (if (or shift meta ctrl)
          (format nil "~C[1;~D~C" #\Escape (modifier-code shift meta ctrl) ch)
          (if (term-keypad-mode term)
              (format nil "~CO~C" #\Escape ch)
              (format nil "~C[~C" #\Escape ch))))))

(defun special-key-sequence (key &key shift meta ctrl)
  (let ((code (case key
                (:insert #\2) (:delete #\3)
                (:page-up #\5) (:page-down #\6))))
    (when code
      (if (or shift meta ctrl)
          (format nil "~C[~C;~D~~" #\Escape code (modifier-code shift meta ctrl))
          (format nil "~C[~C~~" #\Escape code)))))

(defun function-key-sequence (n &key shift meta ctrl)
  (when (<= 1 n 12)
    (if (or shift meta ctrl)
        (let* ((base-codes #(0 "P" "Q" "R" "S" "15" "17" "18"
                             "19" "20" "21" "23" "24"))
               (base (aref base-codes n))
               (mod-code (modifier-code shift meta ctrl)))
          (if (<= n 4)
              (format nil "~C[1;~D~A" #\Escape mod-code base)
              (format nil "~C[~A;~D~~" #\Escape base mod-code)))
        (aref *function-key-table* n))))

(defun key-event-to-escape-sequence (term key-event)
  (cond
    ((characterp key-event)
     (string key-event))

    ((and (listp key-event) (keywordp (car key-event)))
     (let ((key (car key-event))
           (mods (cdr key-event)))
       (let ((shift (member :shift mods))
             (meta (member :meta mods))
             (ctrl (member :ctrl mods)))
         (case key
           ((:up :down :left :right :home :end)
            (arrow-key-sequence term key :shift shift :meta meta :ctrl ctrl))
           ((:insert :delete :page-up :page-down)
            (special-key-sequence key :shift shift :meta meta :ctrl ctrl))
           (:backspace
            (cond
              ((and ctrl meta) (format nil "~C~C" #\Escape #\Backspace))
              (ctrl (string #\Backspace))
              (meta (format nil "~C~C" #\Escape #\Rubout))
              (t (string #\Rubout))))
           (:tab
            (if shift
                (format nil "~C[Z" #\Escape)
                (string #\Tab)))
           (:enter (string #\Return))
           (:escape (string #\Escape))
           (t
            (when (<= 1 (or (and (symbolp key)
                                  (let ((name (symbol-name key)))
                                    (when (and (> (length name) 1)
                                               (char= (char name 0) #\F))
                                      (parse-integer name :start 1
                                                     :junk-allowed t))))
                             0)
                      12)
              (function-key-sequence
               (parse-integer (symbol-name key) :start 1 :junk-allowed t)
               :shift shift :meta meta :ctrl ctrl)))))))

    ((integerp key-event)
     (let ((ch (code-char key-event)))
       (when ch (string ch))))

    (t nil)))

(defun modifier-mods (code)
  (let ((bits (1- (max 1 (or code 1))))
        (mods nil))
    (when (logbitp 2 bits) (push :ctrl mods))
    (when (logbitp 1 bits) (push :meta mods))
    (when (logbitp 0 bits) (push :shift mods))
    mods))

(defparameter +tilde-keys+
  '((2 . :insert) (3 . :delete) (5 . :page-up) (6 . :page-down)
    (15 . :f5) (17 . :f6) (18 . :f7) (19 . :f8)
    (20 . :f9) (21 . :f10) (23 . :f11) (24 . :f12)))

(defun arrow-of (ch)
  (case ch (#\A :up) (#\B :down) (#\C :right) (#\D :left)
        (#\H :home) (#\F :end)))

(defun plain-key (ch)
  (case ch
    (#\Return '(:enter))
    (#\Tab '(:tab))
    (#\Rubout '(:backspace))
    (#\Backspace '(:backspace :ctrl))
    (t ch)))

(defun read-csi-params (said start end)
  "The numbers between ESC [ and the byte that ends it. Answers the numbers, the
final character and where it was, or nil when there is not yet an end to it."
  (let ((params nil)
        (n nil)
        (i start))
    (loop
      (when (>= i end) (return nil))
      (let ((ch (char said i)))
        (cond
          ((digit-char-p ch) (setf n (+ (* 10 (or n 0)) (digit-char-p ch))))
          ((char= ch #\;) (push n params) (setf n nil))
          ((char< ch #\@) nil)
          (t (push n params)
             (return (values (nreverse params) ch i)))))
      (incf i))))

(defun csi-key (said start end)
  (multiple-value-bind (params final at) (read-csi-params said start end)
    (unless final (return-from csi-key (values nil 0)))
    (let* ((took (- (1+ at) (- start 2)))
           (arrow (arrow-of final)))
      (cond
        (arrow (values (list* arrow (modifier-mods (second params))) took))
        ((char= final #\Z) (values '(:tab :shift) took))
        ((find final "PQRS")
         (values (list* (aref #(:f1 :f2 :f3 :f4) (position final "PQRS"))
                        (modifier-mods (second params)))
                 took))
        ((char= final #\~)
         (let ((key (cdr (assoc (or (first params) 0) +tilde-keys+))))
           (if key
               (values (list* key (modifier-mods (second params))) took)
               (values nil took))))
        (t (values nil took))))))

(defun ss3-key (said start end)
  (when (>= start end) (return-from ss3-key (values nil 0)))
  (let* ((ch (char said start))
         (arrow (arrow-of ch)))
    (cond
      (arrow (values (list arrow) 3))
      ((find ch "PQRS")
       (values (list (aref #(:f1 :f2 :f3 :f4) (position ch "PQRS"))) 3))
      (t (values nil 3)))))

(defun escape-sequence-to-key-event (said &optional (start 0) (end (length said))
                                               (more t))
  "The key at START of SAID, and how many characters it took.

Answers (values nil 0) when what is there is the beginning of something longer
and the rest has not arrived. MORE says whether more is coming: a lone escape is
either the escape key or the start of a sequence, and only a clock can tell
which, so whoever has one says so."
  (when (>= start end)
    (return-from escape-sequence-to-key-event (values nil 0)))
  (let ((ch (char said start)))
    (cond
      ((char/= ch #\Escape) (values (plain-key ch) 1))
      ((>= (1+ start) end)
       (if more (values nil 0) (values '(:escape) 1)))
      (t
       (let ((next (char said (1+ start))))
         (case next
           (#\[ (csi-key said (+ start 2) end))
           (#\O (ss3-key said (+ start 2) end))
           (#\Rubout (values '(:backspace :meta) 2))
           (#\Backspace (values '(:backspace :ctrl :meta) 2))
           (t (values '(:escape) 1))))))))
