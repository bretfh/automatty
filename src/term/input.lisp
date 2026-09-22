;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:term)

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

;;; What a mouse did, said to a program the way it asked to be told. The
;;; program says which events it wants and in which encoding by setting modes;
;;; this reads those and answers the report, or nil when it asked for none.

(defun mouse-wanted-p (term kind)
  "Whether the program in TERM asked to be told about a mouse doing KIND:
:press, :release, :wheel or :drag."
  (case (term-mouse-mode term)
    ((nil) nil)
    (:normal (member kind '(:press :release :wheel)))
    (t t)))

(defun mouse-report (term kind x y &key button wheel shift meta ctrl)
  "The report for a mouse doing KIND at column X and line Y of TERM, both from
nought. BUTTON is :left, :middle or :right; WHEEL is :up, :down, :left or
:right. Nil when the
program did not ask for it, or asked in an encoding that cannot say where it
was."
  (when (mouse-wanted-p term kind)
    (let* ((cb (+ (if (eq kind :wheel)
                      (ecase wheel (:up 64) (:down 65) (:left 66) (:right 67))
                      (ecase button (:left 0) (:middle 1) (:right 2) ((nil) 3)))
                  (if (eq kind :drag) 32 0)
                  (if shift 4 0) (if meta 8 0) (if ctrl 16 0)))
           (cx (1+ x))
           (cy (1+ y)))
      (cond
        ((or (term-mouse-sgr term) (term-mouse-sgr-pixels term))
         (format nil "~C[<~D;~D;~D~C" #\Escape cb cx cy
                 (if (eq kind :release) #\m #\M)))
        (t
         ;; the older encodings have no way to say which button let go
         (let ((cb (if (eq kind :release) (+ 3 (logand cb (lognot 3))) cb)))
           (cond
             ((term-mouse-urxvt term)
              (format nil "~C[~D;~D;~DM" #\Escape (+ 32 cb) cx cy))
             ((or (term-mouse-utf8 term)
                  (and (<= (+ 32 cx) 127) (<= (+ 32 cy) 127)))
              (format nil "~C[M~C~C~C" #\Escape
                      (code-char (+ 32 cb)) (code-char (+ 32 cx))
                      (code-char (+ 32 cy)))))))))))
