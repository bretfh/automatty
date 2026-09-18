(in-package #:vt/graph)

(defvar *watchers* nil)

(defclass watcher (place)
  ((subject :initarg :subject :reader   subject)
   (handler :initarg :handler :accessor handler)
   (name    :initarg :name    :reader   name       :initform nil)
   (when    :initarg :when    :reader   fire-when  :initform :on-change)
   (every   :initarg :every                          :initform nil)
   (previous :initform nil :accessor previous)
   (running :initform nil :accessor running)
   (again   :initform nil :accessor again)
   (tick    :initform nil :accessor tick-of)
   (stopped :initform nil :accessor stopped)))

(defmethod print-object ((w watcher) stream)
  (print-unreadable-object (w stream :type t)
    (format stream "~a~:[~; stopped~]" (or (name w) "?") (stopped w))))

(defun %fire (w)
  (let* ((n (subject w))
         (now (value n)))
    (when (or (eq (fire-when w) :always) (not (same now (previous w))))
      (setf (previous w) now)
      (handler-case (funcall (handler w) n now)
        (error (c) (when *broke* (funcall *broke* c w)))))))

(defun %fired (w)
  (loop
    (setf (again w) nil)
    (unless (stopped w) (%fire w))
    (unless (again w) (return)))
  (setf (running w) nil)
  (when (again w) (touch w)))

(defmethod touch ((w watcher))
  (cond ((stopped w) w)
        ((cas-p (slot-value w 'running) nil t)
         (later (lambda () (%fired w)))
         w)
        (t (setf (again w) t) w)))

(defun watch (subject handler &key name (when :on-change) every)
  (let ((w (make-instance 'watcher :subject subject :handler handler
                                   :name name :when when :every every)))
    (setf (previous w) (value subject))
    (depend w subject)
    (when every
      (setf (tick-of w) (repeat every (lambda () (touch subject)))))
    (sb-ext:atomic-update *watchers* (lambda (had) (cons w had)))
    w))

(defun unwatch (w)
  (when w
    (setf (stopped w) t)
    (undepend w (subject w))
    (when (tick-of w) (cancel (tick-of w)))
    (sb-ext:atomic-update *watchers* (lambda (had) (remove w had))))
  nil)
