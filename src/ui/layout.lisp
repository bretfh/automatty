(in-package #:vt/ui)

(defvar *styles* nil)
(defvar *sizes* nil)

(defclass medium () ())

(defgeneric text-size (medium text font &optional bold family))

(defun line-height (m font) (nth-value 1 (text-size m "M" font)))

(defmacro with-pass (&body body)
  `(let ((*styles* (make-hash-table :test 'eq))
         (*sizes* (make-hash-table :test 'eq)))
     ,@body))

(defun style-of (w)
  (and *styles* (gethash w *styles*)))

(defun restyle (root &optional chain)
  (labels ((walk (w chain)
             (let* ((classes (classes (css-class-name w)))
                    (full (append chain (list classes))))
               (when *styles*
                 (setf (gethash w *styles*) (resolve full :hover (hoveredp w))))
               (dolist (part (parts w)) (walk part full)))))
    (walk root chain))
  root)

(defun %pad-x (w)
  (let ((p (padding w)) (s (getf (style-of w) :padding)))
    (cond ((consp p) (car p)) ((realp p) p) ((consp s) (car s)) (t 0))))

(defun %pad-y (w)
  (let ((p (padding w)) (s (getf (style-of w) :padding)))
    (cond ((consp p) (cdr p)) ((realp p) p) ((consp s) (cdr s)) (t 0))))

(defun %margin (w) (or (margin w) (getf (style-of w) :margin)))
(defun %margin-x (w) (let ((m (%margin w))) (if m (+ (fourth m) (second m)) 0)))
(defun %margin-y (w) (let ((m (%margin w))) (if m (+ (first m) (third m)) 0)))
(defun %min-w (w) (max (min-width w) (or (getf (style-of w) :min-width) 0)))
(defun %min-h (w) (max (min-height w) (or (getf (style-of w) :min-height) 0)))
(defun %font (w) (or (font-size w) (getf (style-of w) :font-size)))

(defun %attrs (w)
  (let ((f (face w)))
    (if f (attrs (in-force f)) 0)))

(defun boldp (w)
  (or (and (getf (style-of w) :font-weight) t)
      (logtest 1 (%attrs w))))

(defun italicp (w) (logtest 2 (%attrs w)))

(defun underlinep (w) (logtest 4 (%attrs w)))

(defun family-of (w) (getf (style-of w) :font-family))

(defgeneric measure (widget medium avail-w avail-h)
  (:method ((w widget) m aw ah) (declare (ignore m aw ah)) (values 0 1)))

(defmethod measure :around ((w widget) m aw ah)
  (let ((had (and *sizes* (gethash w *sizes*))))
    (if (and had (eql (first had) aw) (eql (second had) ah))
        (values (third had) (fourth had))
        (multiple-value-bind (cw ch)
            (call-next-method w m
                              (max 0 (- aw (* 2 (%pad-x w)) (%margin-x w)))
                              (max 0 (- ah (* 2 (%pad-y w)) (%margin-y w))))
          (let ((out-w (+ (max (%min-w w) (+ cw (* 2 (%pad-x w)))) (%margin-x w)))
                (out-h (+ (max (%min-h w) (+ ch (* 2 (%pad-y w)))) (%margin-y w))))
            (when *sizes* (setf (gethash w *sizes*) (list aw ah out-w out-h)))
            (values out-w out-h))))))

(defgeneric lay (widget medium x y w h)
  (:method ((w widget) m x y width height)
    (declare (ignore m))
    (setf (left w) x (top w) y
          (right w) (+ x width) (bottom w) (+ y (max 0 height)))))

(defmethod lay :around ((w widget) m x y width height)
  (let ((mg (%margin w)))
    (if mg
        (call-next-method w m (+ x (fourth mg)) (+ y (first mg))
                          (max 0 (- width (%margin-x w)))
                          (max 0 (- height (%margin-y w))))
        (call-next-method))))

(defun %inner (w x y width height)
  (values (+ x (%pad-x w)) (+ y (%pad-y w))
          (max 0 (- width (* 2 (%pad-x w))))
          (max 0 (- height (* 2 (%pad-y w))))))

(defgeneric paint (widget medium)
  (:method ((w widget) m) (dolist (part (parts w)) (paint part m))))

(defmethod measure ((w label) m aw ah)
  (declare (ignore aw ah))
  (text-size m (text w) (%font w) (boldp w) (family-of w)))

(defmethod measure ((w rule) m aw ah)
  (declare (ignore aw ah))
  (let ((thick (max 1 (nth-value 1 (text-size m "M" nil)))))
    (if (upright w) (values thick 1) (values 1 thick))))

(defmethod measure ((w gap) m aw ah) (declare (ignore m aw ah)) (values 0 0))

(defmethod measure ((w slider) m aw ah)
  (declare (ignore aw ah))
  (values (* 8 (track w)) (line-height m (%font w))))

(defmethod measure ((w calendar) m aw ah)
  (declare (ignore aw ah))
  (multiple-value-bind (cw ch) (text-size m "MM " (%font w))
    (values (* 7 cw) (* 8 ch))))

(defmethod measure ((w ring) m aw ah)
  (declare (ignore m aw ah))
  (let ((d (max (diameter w) (%min-w w) (%min-h w))))
    (values d d)))

(defmethod lay ((w ring) m x y width height)
  (call-next-method)
  (let ((part (first (parts w))))
    (when part
      (multiple-value-bind (ix iy iw ih) (%inner w x y width height)
        (multiple-value-bind (cw ch) (measure part m iw ih)
          (lay part m (+ ix (floor (- iw cw) 2)) (+ iy (floor (- ih ch) 2))
               cw ch))))))

(defun %stacked (w m aw ah verticalp)
  (let ((width 0) (high 0) (all (parts w)))
    (dolist (part all)
      (multiple-value-bind (cw ch) (measure part m aw ah)
        (if verticalp
            (progn (setf width (max width cw)) (incf high ch))
            (progn (incf width cw) (setf high (max high ch))))))
    (let ((gaps (* (spacing w) (max 0 (1- (length all))))))
      (if verticalp (values width (+ high gaps)) (values (+ width gaps) high)))))

(defmethod measure ((w column) m aw ah) (%stacked w m aw ah t))
(defmethod measure ((w row) m aw ah) (%stacked w m aw ah nil))

(defun %lay (w m x y width height verticalp)
  (let* ((all (parts w))
         (sizes (mapcar (lambda (p) (multiple-value-list (measure p m width height)))
                        all))
         (natural (+ (reduce #'+ sizes :initial-value 0
                                       :key (if verticalp #'second #'first))
                     (* (spacing w) (max 0 (1- (length all))))))
         (slack (max 0 (- (if verticalp height width) natural)))
         (weight (reduce #'+ all :initial-value 0 :key #'expand))
         (acc 0) (given 0)
         (at (if verticalp y x)))
    (loop :for part :in all
          :for (cw ch) :in sizes
          :for extra := (if (plusp weight)
                            (progn (incf acc (* slack (/ (expand part) weight)))
                                   (prog1 (- (round acc) given)
                                     (setf given (round acc))))
                            0)
          :do (if verticalp
                  (let ((fh (+ ch extra))
                        (cx (ecase (align w)
                              ((:start :stretch) x)
                              (:center (+ x (floor (- width cw) 2)))
                              (:end (+ x (- width cw))))))
                    (lay part m cx at (if (eq (align w) :stretch) width cw) fh)
                    (setf at (+ at fh (spacing w))))
                  (let ((fw (+ cw extra))
                        (cy (ecase (align w)
                              ((:start :stretch) y)
                              (:center (+ y (floor (- height ch) 2)))
                              (:end (+ y (- height ch))))))
                    (lay part m at cy fw (if (eq (align w) :stretch) height ch))
                    (setf at (+ at fw (spacing w))))))))

(defmethod lay ((w column) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (%lay w m x y width height t)))

(defmethod lay ((w row) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (%lay w m x y width height nil)))

(defmethod measure ((w stack) m aw ah)
  (let ((width 0) (high 0))
    (dolist (part (parts w) (values width high))
      (multiple-value-bind (cw ch) (measure part m aw ah)
        (setf width (max width cw) high (max high ch))))))

(defmethod lay ((w stack) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (dolist (part (parts w)) (lay part m x y width height))))

(defmethod measure ((w box) m aw ah)
  (declare (ignore aw))
  (let ((part (first (parts w))))
    (values (fixed-width w)
            (if part (nth-value 1 (measure part m (fixed-width w) ah)) 1))))

(defmethod lay ((w box) m x y width height)
  (declare (ignore width))
  (call-next-method w m x y (fixed-width w) height)
  (let ((part (first (parts w))))
    (when part
      (multiple-value-bind (cw ch) (measure part m (fixed-width w) height)
        (lay part m
             (ecase (align w)
               (:left x)
               (:right (+ x (- (fixed-width w) cw)))
               (:center (+ x (floor (- (fixed-width w) cw) 2))))
             y cw (max 1 ch))))))

(defmethod measure ((w center) m aw ah)
  (let ((part (first (parts w))))
    (if part (measure part m aw ah) (values 0 0))))

(defmethod lay ((w center) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (let ((part (first (parts w))))
      (when part
        (multiple-value-bind (cw ch) (measure part m width height)
          (lay part m (+ x (floor (- width cw) 2))
               (+ y (floor (- height ch) 2)) cw ch))))))

(defmethod measure ((w centerbox) m aw ah)
  (let ((width 0) (high 0))
    (dolist (part (parts w) (values width high))
      (multiple-value-bind (cw ch) (measure part m aw ah)
        (setf width (max width cw))
        (incf high ch)))))

(defmethod lay ((w centerbox) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (let* ((s (start w)) (c (middle w)) (e (end w))
           (sh (if s (nth-value 1 (measure s m width height)) 0))
           (eh (if e (nth-value 1 (measure e m width height)) 0)))
      (when s (lay s m x y width sh))
      (when e (lay e m x (+ y (- height eh)) width eh))
      (when c
        (let ((ch (nth-value 1 (measure c m width height)))
              (room (max 0 (- height sh eh))))
          (lay c m x (+ y sh (max 0 (floor (- room ch) 2))) width ch))))))

(defmethod measure ((w scroll) m aw ah)
  (declare (ignore ah))
  (let ((part (first (parts w))))
    (values (if part (nth-value 0 (measure part m aw 100000)) 0) (fixed-height w))))

(defmethod lay ((w scroll) m x y width height)
  (declare (ignore height))
  (call-next-method w m x y width (fixed-height w))
  (let ((part (first (parts w))))
    (when part
      (let ((ch (nth-value 1 (measure part m width 100000))))
        (lay part m x (- y (offset w)) width ch)))))

(defun mark-of (w) (if (chosen w) (before w) (after w)))

(defun %mark-width (w m)
  (nth-value 0 (text-size m (mark-of w) (%font w) (boldp w) (family-of w))))

(defmethod measure ((w choice) m aw ah)
  (let ((mark (%mark-width w m))
        (part (first (parts w))))
    (multiple-value-bind (cw ch)
        (if part (measure part m (max 0 (- aw mark)) ah) (values 0 1))
      (values (+ mark cw) ch))))

(defmethod lay ((w choice) m x y width height)
  (call-next-method)
  (let ((mark (%mark-width w m))
        (part (first (parts w))))
    (when part
      (lay part m (+ x mark) y (max 0 (- width mark)) height))))

(defmethod measure ((w action) m aw ah)
  (let ((part (first (parts w))))
    (if part (measure part m aw ah) (values 0 1))))

(defmethod lay ((w action) m x y width height)
  (call-next-method)
  (multiple-value-bind (x y width height) (%inner w x y width height)
    (let ((part (first (parts w))))
      (when part
        (multiple-value-bind (cw ch) (measure part m width height)
          (lay part m (+ x (floor (- width cw) 2))
               (+ y (floor (- height ch) 2)) cw ch))))))

(defun %covers (w line col)
  (and (<= (top w) line) (< line (bottom w))
       (<= (left w) col) (< col (right w))))

(defgeneric under (widget line col)
  (:method ((w widget) line col)
    (let ((found nil))
      (dolist (part (parts w) found)
        (let ((it (under part line col)))
          (when it (setf found it))))))
  (:method ((w action) line col)
    (when (%covers w line col) (or (call-next-method) w)))
  (:method ((w choice) line col)
    (when (%covers w line col) (or (call-next-method) w)))
  (:method ((w slider) line col) (when (%covers w line col) w))
  (:method ((w scroll) line col) (when (%covers w line col) (call-next-method))))

(defun value-at (w col)
  (let* ((n (max 1 (width w)))
         (rel (max 0 (min n (- col (left w)))))
         (span (- (high w) (low w))))
    (+ (low w) (round (* (/ rel n) span)))))

(defgeneric clicked (widget col)
  (:method ((w widget) col) (declare (ignore col)) nil)
  (:method ((w action) col) (declare (ignore col)) (on-click w))
  (:method ((w choice) col) (declare (ignore col)) (on-click w))
  (:method ((w slider) col)
    (let ((fn (on-change w)) (v (value-at w col)))
      (when fn (lambda () (funcall fn v))))))

(defun scrolling-under (root line col)
  (let ((found nil))
    (labels ((walk (w)
               (when (%covers w line col)
                 (when (typep w 'scroll) (setf found w))
                 (dolist (part (parts w)) (walk part)))))
      (walk root))
    found))

(defun clicked-at (root line col)
  (let ((hit (under root line col)))
    (and hit (clicked hit col))))
