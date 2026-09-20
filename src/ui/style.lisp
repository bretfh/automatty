(in-package #:atty/ui)

(declaim (ftype function rules))

(defvar *properties* nil)

(defun property (key parser)
  (setf *properties*
        (append (remove key *properties* :key #'car) (list (cons key parser))))
  key)

(defun properties ()
  (sort (mapcar #'car *properties*) #'string< :key #'symbol-name))

(defun %words (s)
  (remove "" (split-string s :separator '(#\space #\tab)) :test #'string=))

(defun %segment (s)
  (cond ((string= s "*") (list :any t :classes nil :pseudo nil))
        ((find #\. s)
         (let* ((colon (position #\: s))
                (pseudo (and colon (subseq s (1+ colon))))
                (body (subseq s 0 (or colon (length s)))))
           (list :any nil
                 :classes (remove "" (split-string body :separator ".")
                                  :test #'string=)
                 :pseudo pseudo)))
        (t nil)))

(defun segments (s)
  (let ((segments (mapcar #'%segment (%words s))))
    (when (member nil segments)
      (error "~s is not a selector that can be matched: a class starts with a dot." s))
    segments))

(defun %group (text)
  (mapcar (lambda (g) (segments (string-trim " " g)))
          (split-string text :separator '(#\,))))

(defun %readable (text) (%group text) text)

(defun specificity (segments)
  (list (length segments)
        (reduce #'+ segments :key (lambda (s) (length (getf s :classes)))
                             :initial-value 0)))

(defun %beats (a b)
  (destructuring-bind (as ac) a
    (destructuring-bind (bs bc) b
      (or (> as bs) (and (= as bs) (> ac bc))))))

(defun %compiled (cascade)
  (loop :for (text props) :in cascade
        :for at :from 0
        :append (loop :for segments :in (%group text)
                      :collect (list segments props (specificity segments) at))))

(defun %fits (segment classes hover)
  (let ((pseudo (getf segment :pseudo)))
    (and (or (null pseudo) (and (string= pseudo "hover") hover))
         (or (getf segment :any)
             (subsetp (getf segment :classes) classes :test #'string=)))))

(defun %above (segments chain)
  (if (null segments)
      t
      (loop :for tail :on chain
            :when (%fits (first segments) (first tail) nil)
              :do (return (%above (rest segments) (rest tail)))
            :finally (return nil))))

(defun %matches (segments chain hover)
  (and segments chain
       (%fits (car (last segments)) (car (last chain)) hover)
       (%above (butlast segments) (butlast chain))))

(defun %props (chain hover)
  (let ((found (loop :for rule :in (rules)
                     :when (%matches (first rule) chain hover) :collect rule)))
    (let ((merged nil))
      (dolist (rule (stable-sort found (lambda (a b)
                                         (cond ((%beats (third a) (third b)) nil)
                                               ((%beats (third b) (third a)) t)
                                               (t (< (fourth a) (fourth b))))))
                    merged)
        (loop :for (k v) :on (second rule) :by #'cddr
              :do (setf (getf merged k) v))))))

(defun resolve (chain &key hover)
  (let ((props (%props chain hover))
        (out nil))
    (loop :for (key . parser) :in *properties*
          :for v := (funcall parser props)
          :when v :do (setf out (append out (list key v))))
    out))

(defun classes (it)
  (typecase it
    (null nil)
    (symbol (list (string-downcase (symbol-name it))))
    (string (remove "" (split-string it :separator '(#\space)) :test #'string=))
    (cons (mapcan #'classes it))))

(defun %lengths (v)
  (typecase v
    (real (list (round v)))
    (cons (mapcar #'round v))
    (string (loop :for tok :in (%words v)
                  :for n := (and (plusp (length tok))
                                 (digit-char-p (char tok 0))
                                 (not (search "rem" tok)) (not (find #\% tok))
                                 (parse-integer tok :junk-allowed t))
                  :when n :collect n))
    (t nil)))

(defun %rgba (s n)
  (let* ((open (position #\( s)) (close (position #\) s))
         (parts (split-string (subseq s (1+ open) close) :separator '(#\,))))
    (when (>= (length parts) n)
      (flet ((num (i)
               (let ((*read-eval* nil))
                 (read-from-string (string-trim " " (nth i parts))))))
        (list (num 0) (num 1) (num 2) (if (= n 4) (float (num 3)) 1.0))))))

(defun %color (s)
  (cond ((or (null s) (equal s "transparent") (equal s "none")) nil)
        ((and (stringp s) (plusp (length s)) (char= (char s 0) #\#))
         (let ((rgb (unhex s)))
           (when rgb (append rgb (list 1.0)))))
        ((and (stringp s) (eql 0 (search "rgba(" s))) (%rgba s 4))
        ((and (stringp s) (eql 0 (search "rgb(" s)))  (%rgba s 3))
        (t nil)))

(defun %box (v)
  (let ((l (%lengths v)))
    (case (length l)
      (0 nil)
      (1 (list (first l) (first l) (first l) (first l)))
      (2 (list (first l) (second l) (first l) (second l)))
      (3 (list (first l) (second l) (third l) (second l)))
      (t (subseq l 0 4)))))

(property :background-color (lambda (p) (%color (getf p :background-color))))
(property :color (lambda (p) (let ((c (%color (getf p :color))))
                               (and c (subseq c 0 3)))))
(property :font-weight (lambda (p) (equal (getf p :font-weight) "bold")))

(defun %family (v)
  (when (stringp v)
    (let ((first (string-trim " " (first (split-string v :separator '(#\,))))))
      (when (plusp (length first))
        (string-trim "\"'" first)))))

(property :font-family (lambda (p) (%family (getf p :font-family))))
(property :font-size (lambda (p) (first (%lengths (getf p :font-size)))))
(property :min-width (lambda (p) (first (%lengths (getf p :min-width)))))
(property :min-height (lambda (p) (first (%lengths (getf p :min-height)))))

(property :border-radius
          (lambda (p)
            (let ((v (getf p :border-radius)))
              (cond ((null v) nil)
                    ((realp v) (round v))
                    ((and (stringp v) (search "100%" v)) :round)
                    (t (first (%lengths v)))))))

(property :padding
          (lambda (p)
            (let ((l (%lengths (getf p :padding))))
              (case (length l)
                (0 nil)
                (1 (cons (first l) (first l)))
                (2 (cons (second l) (first l)))
                (3 (cons (second l) (round (+ (first l) (third l)) 2)))
                (t (cons (round (+ (second l) (fourth l)) 2)
                         (round (+ (first l) (third l)) 2)))))))

(property :margin
          (lambda (p)
            (let ((m (or (%box (getf p :margin)) (list 0 0 0 0))))
              (loop :for key :in '(:margin-top :margin-right :margin-bottom
                                   :margin-left)
                    :for i :from 0
                    :for v := (first (%lengths (getf p key)))
                    :when v :do (setf (nth i m) v))
              (unless (every #'zerop m) m))))

(property :background-image
          (lambda (p)
            (let ((v (getf p :background-image)))
              (when (and v (search "linear-gradient" v))
                (let* ((open (position #\( v))
                       (close (position #\) v :from-end t))
                       (stops (loop :for part :in (split-string
                                                   (subseq v (1+ open) close)
                                                   :separator '(#\,))
                                    :for c := (%color (string-trim " " part))
                                    :when c :collect (subseq c 0 3))))
                  (when (>= (length stops) 2) (subseq stops 0 2)))))))

(property :border
          (lambda (p)
            (when (equal (getf p :border-style) "solid")
              (let ((c (%color (getf p :border-color))))
                (list (or (first (%lengths (getf p :border-width))) 1)
                      (and c (subseq c 0 3)))))))

(property :box-shadow-inset
          (lambda (p)
            (let ((v (getf p :box-shadow)))
              (when (and v (search "inset" v))
                (let ((c (%color (car (last (%words v))))))
                  (when c (list (subseq c 0 3)
                                (or (first (%lengths v)) 3))))))))

(property :box-shadow
          (lambda (p)
            (let ((v (getf p :box-shadow)))
              (when (and v (not (search "inset" v)) (search "px" v))
                (let ((l (%lengths v))
                      (c (%color (car (last (%words v))))))
                  (when (and (>= (length l) 3) c)
                    (list (first l) (second l) (third l) c)))))))

(property :opacity
          (lambda (p)
            (let* ((v (getf p :opacity))
                   (n (if (realp v)
                          v
                          (and (stringp v)
                               (ignore-errors
                                (with-standard-io-syntax
                                  (let ((*read-eval* nil))
                                    (read-from-string v))))))))
              (when (realp n) (float (max 0 (min 1 n)) 1.0)))))

(defvar *rules* nil)

(defun %tone (role else)
  (or (%role (theme-palette (themed (active))) role) else))

(defun glass (role &optional (a (metric :opacity 0.4)))
  (let ((rgb (or (unhex (%tone role nil))
                 (error "the active theme has no colour ~s to make glass of." role))))
    (format nil "rgba(~d, ~d, ~d, ~a)" (first rgb) (second rgb) (third rgb) a)))

(defun radius () (format nil "~apx" (metric :radius 8)))

(defun mono () (format nil "~s, monospace" (metric :font "Maple Mono NF")))

(defgeneric selector (it)
  (:method ((it string)) it)
  (:method ((it symbol)) (format nil ".~(~a~)" (symbol-name it)))
  (:method ((it cons)) (format nil "~{.~(~a~)~}" (mapcar #'symbol-name it))))

(defun styles () *rules*)

(defun style (selector &rest properties)
  (put-rules (list (list selector properties)))
  (selector selector))

(defun hovered (&rest selectors)
  (format nil "~{~a:hover~^, ~}" selectors))

(defun put-rules (pairs)
  (let* ((had *rules*)
         (now (loop :for (sel props) :in pairs
                    :collect (list (%readable (selector sel)) props)))
         (named (mapcar #'first now))
         (kept (remove-if (lambda (rule) (member (first rule) named :test #'string=))
                          had)))
    (setf *rules* (append kept now))
    (forget-rules)))

(defun built-in ()
  (list
   (list "*" (list :border-width "0" :border-style "none" :box-shadow "none"
                   :background-color "transparent" :background-image "none"))
   (list ".window" (list :font-family (mono)
                         :font-size (format nil "~apx" (metric :font-px 15))
                         :color (%tone :fg "#ffffff")))

   (list ".confirm" (list :background-color (%tone :bg "#000000")
                          :color (%tone :fg "#ffffff")
                          :border-width "1px" :border-style "solid"
                          :border-color (%tone :border (%tone :fg "#ffffff"))
                          :border-radius (radius)
                          :padding "18px" :min-width "260px"))
   (list ".confirm-question" (list :color (%tone :fg "#ffffff") :font-weight "bold"
                                   :padding "0 0 6px 0"))
   (list ".confirm-yes" (list :background-color (%tone :red "#aa3333")
                              :color (%tone :accent-fg (%tone :fg "#ffffff"))
                              :border-radius (radius)
                              :padding "8px 18px" :min-width "80px"))
   (list ".confirm-no" (list :background-color (%tone :bg-active (%tone :bg "#222222"))
                             :color (%tone :fg "#ffffff")
                             :border-radius (radius)
                             :padding "8px 18px" :min-width "80px"))))

(defvar *sheet* nil
  "The built-in rules and whatever was put over them, compiled, and kept rather
than compiled again. PUT-RULES and (SETF ACTIVE) are the two things that change
it, and each forgets this.")

(defun forget-rules () (setf *sheet* nil))

(defun rules () (or *sheet* (setf *sheet* (%compiled (append (built-in) (styles))))))
