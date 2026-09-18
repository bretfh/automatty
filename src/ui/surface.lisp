(in-package #:vt/ui)

(defvar *surfaces* nil)

(defstruct (anchor (:constructor %anchor (edges width height reserve margin
                                          keyboard output))
                   (:copier nil))
  edges width height reserve margin keyboard output)

(defmethod print-object ((a anchor) stream)
  (print-unreadable-object (a stream :type t)
    (format stream "~{~(~a~)~^ ~} ~dx~d" (anchor-edges a)
            (anchor-width a) (anchor-height a))))

(defclass surface ()
  ((name    :initarg :name    :reader name)
   (builds  :initarg :builds  :reader builds)
   (windowp :initarg :windowp :reader windowp :initform nil)
   (title   :initarg :title   :accessor title :initform nil)
   (left    :initarg :left    :accessor left    :initform nil)
   (top     :initarg :top     :accessor top     :initform nil)
   (right   :initarg :right   :accessor right   :initform nil)
   (bottom  :initarg :bottom  :accessor bottom  :initform nil)
   (width   :initarg :width   :accessor width   :initform nil)
   (height  :initarg :height  :accessor height  :initform nil)
   (reserve :initarg :reserve :accessor reserve :initform nil)
   (keyboard :initarg :keyboard :reader keyboardp :initform nil)
   (output  :initarg :output  :accessor output-of :initform nil)
   (shown   :reader shown)
   (size    :reader size)
   (tree    :reader tree)
   (where   :reader where)))

(defmethod print-object ((s surface) stream)
  (print-unreadable-object (s stream :type t)
    (format stream "~a~:[~; shown~]" (name s) (value (shown s)))))

(defun %anchor-of (s size)
  (let* ((l (left s)) (r (right s)) (u (top s)) (b (bottom s))
         (edges (append (when u '(:top)) (when l '(:left))
                        (when b '(:bottom)) (when r '(:right))))
         (across (and l r)) (down (and u b))
         (w (or (width s) (if across 0 (or (getf size :width) 0))))
         (h (or (height s) (if down 0 (or (getf size :height) 0))))
         (reserve (reserve s)))
    (%anchor edges w h
             (cond ((null reserve) 0)
                   ((eq reserve t) (cond ((and down (not across)) w)
                                         ((and across (not down)) h)
                                         (t 0)))
                   (t reserve))
             (list (or u 0) (or r 0) (or b 0) (or l 0))
             (keyboardp s)
             (output-of s))))

(defun %along (near far size off-near off-far room start)
  (cond ((and near far) (values (+ start off-near)
                                (max 0 (- room off-near off-far))))
        (near (values (+ start off-near) size))
        (far  (values (+ start (- room off-far size)) size))
        (t    (values (+ start (floor (- room size) 2)) size))))

(defun anchor-rect (a rect)
  (destructuring-bind (ox oy ow oh) rect
    (destructuring-bind (top right bottom left) (anchor-margin a)
      (let ((edges (anchor-edges a)))
        (multiple-value-bind (x width)
            (%along (member :left edges) (member :right edges)
                    (anchor-width a) left right ow ox)
          (multiple-value-bind (y height)
              (%along (member :top edges) (member :bottom edges)
                      (anchor-height a) top bottom oh oy)
            (values x y width height)))))))

(defmethod initialize-instance :after ((s surface) &key (visible nil))
  (setf (slot-value s 'shown) (g:make-cell (and visible t)))
  (setf (slot-value s 'size) (g:make-cell nil))
  (setf (slot-value s 'tree) (g:make-derived (lambda () (funcall (builds s)))))
  (setf (slot-value s 'where)
        (g:make-derived (lambda () (%anchor-of s (value (size s)))))))

(defun surfaces () (reverse *surfaces*))

(defun surface (name)
  (find (princ-to-string name) *surfaces* :key #'name :test #'string=))

(defgeneric on-declare (surface)
  (:method (surface) (declare (ignore surface)) nil))

(defgeneric on-forget (surface)
  (:method (surface) (declare (ignore surface)) nil))

(defun create-surface (name builds &rest options)
  (let* ((name (princ-to-string name))
         (had (surface name))
         (s (apply #'make-instance 'surface :name name :builds builds options)))
    (when had (forget-surface name))
    (push s *surfaces*)
    (on-declare s)
    s))

(defun forget-surface (name)
  (let ((s (surface (princ-to-string name))))
    (when s
      (on-forget s)
      (setf *surfaces* (remove s *surfaces*)))
    name))

(defmacro defsurface (name options &body body)
  `(create-surface ,(string-downcase (string name)) (lambda () ,@body) ,@options))

(defmacro defwindow (name options &body body)
  `(create-surface ,(string-downcase (string name)) (lambda () ,@body)
                   :windowp t :visible t ,@options))

(defun %surface (it)
  (or (if (typep it 'surface) it (surface it))
      (error "there is no surface called ~s" it)))

(defgeneric visiblep (it)
  (:method ((s surface)) (and (value (shown s)) t))
  (:method (it) (visiblep (%surface it))))

(defgeneric (setf visiblep) (v it)
  (:method (v (s surface)) (setf (value (shown s)) (and v t)) v)
  (:method (v it) (setf (visiblep (%surface it)) v)))

(defun show (it) (setf (visiblep it) t))

(defun hide (it) (setf (visiblep it) nil))

(defgeneric toggle (it)
  (:method ((s surface)) (setf (visiblep s) (not (visiblep s))))
  (:method ((it string)) (toggle (%surface it)))
  (:method ((it place)) (setf (value it) (not (value it))))
  (:method ((it symbol))
    (let ((p (place it)))
      (if p (setf (value p) (not (value p))) (toggle (%surface it))))))

(defvar *question* (g:make-cell nil))

(defun %answer (yes)
  (let ((had (value *question*)))
    (setf (value *question*) nil)
    (let ((s (surface "confirm")))
      (when s (setf (visiblep s) nil)))
    (when (and yes had)
      (handler-case (funcall (cdr had))
        (error (c) (g:note "~a: ~a" (car had) c))))
    t))

(defun %confirm-tree ()
  (let ((had (value *question*)))
    (column :class "confirm" :align :stretch :spacing 12
      (label (if had (car had) "") :class "confirm-question")
      (row :align :center :spacing 10
        (button :class "confirm-no" :on-click (lambda () (%answer nil))
                (center (label "No")))
        (button :class "confirm-yes" :on-click (lambda () (%answer t))
                (center (label "Yes")))))))

(defmethod confirm (question thunk)
  (let ((s (or (surface "confirm")
               (create-surface "confirm" #'%confirm-tree :top 8 :right 8))))
    (setf (value *question*) (cons (princ-to-string question) thunk))
    (setf (visiblep s) t)
    s))
