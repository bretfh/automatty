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
