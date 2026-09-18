(in-package #:vt/mode)

(defclass mode ()
  ((keys   :initform (make-hash-table :test 'equal) :reader mode-keys)
   (values :initform (make-hash-table :test 'eq)    :reader mode-values)))

(defvar *modes* (make-hash-table :test 'eq))
(defvar *current* (g:make-cell nil))
(defvar *pending* nil)

(defmacro define-mode (name parents &body options)
  `(progn
     (defclass ,name ,(or parents '(mode)) () ,@options)
     (mode-named ',name)
     ',name))

(defun mode-named (name)
  (etypecase name
    (symbol (or (gethash name *modes*)
                (setf (gethash name *modes*) (make-instance name))))
    (string (loop :for key :being :the :hash-keys :of *modes*
                  :when (string-equal name (symbol-name key))
                    :do (return (gethash key *modes*))))))

(defgeneric as-mode (it)
  (:method ((it mode)) it)
  (:method ((it symbol)) (mode-named it))
  (:method ((it string)) (mode-named it)))

(defun modes ()
  (sort (loop :for name :being :the :hash-keys :of *modes* :collect name)
        #'string< :key #'symbol-name))

(defun global-map () (mode-named 'mode))

(defun current-mode ()
  (or (g:value *current*) (global-map)))

(defun (setf current-mode) (it)
  (setf (g:value *current*) (and it (as-mode it)))
  it)

(defmacro with-mode (it &body body)
  `(let ((was (g:value *current*)))
     (unwind-protect (progn (setf (current-mode) ,it) ,@body)
       (setf (g:value *current*) was))))

(defun %chain (m)
  (loop :for c :in (sb-mop:class-precedence-list (class-of m))
        :for had := (gethash (class-name c) *modes*)
        :when had :collect had))

(defun %spelled (chord) (spelled (chord (princ-to-string chord))))

(defun define-key (it chord does)
  (let ((m (as-mode it)) (chord (%spelled chord)))
    (setf (gethash chord (mode-keys m)) (g:as-handler does))
    (g:touch *current*)
    chord))

(defun undefine-key (it chord)
  (let ((m (as-mode it)) (chord (%spelled chord)))
    (remhash chord (mode-keys m))
    (g:touch *current*)
    chord))

(defun global-set-key (chord does) (define-key (global-map) chord does))

(defun global-unset-key (chord) (undefine-key (global-map) chord))

(defun lookup-key (chord &optional (it (current-mode)))
  (let ((chord (%spelled chord)))
    (loop :for m :in (%chain (as-mode it))
          :for found := (gethash chord (mode-keys m))
          :when found :do (return (values found m)))))

(defun keys-in-force (&optional (it (current-mode)))
  (let ((out nil))
    (dolist (m (reverse (%chain (as-mode it))) out)
      (maphash (lambda (chord does)
                 (setf out (cons (cons chord does)
                                 (remove chord out :key #'car :test #'string=))))
               (mode-keys m)))))

(defvar *chords*
  (g:name-place 'chords-in-force
                (g:make-derived (lambda ()
                                  (g:depend-on *current*)
                                  (mapcar #'car (keys-in-force))))))

(defun chords-in-force () (g:value *chords*))

(defvar *unbound* nil)

(defvar *run* #'funcall)

(defun pending () (when *pending* (spelled *pending*)))

(defun prefixp (chord &optional (it (current-mode)))
  (let ((n (length chord)))
    (find-if (lambda (each)
               (let ((had (car each)))
                 (and (> (length had) n)
                      (string= chord had :end2 n)
                      (char= #\Space (char had n)))))
             (keys-in-force it))))

(defun press (spec &optional (it (current-mode)))
  (let* ((keys (append *pending* (chord (princ-to-string spec))))
         (chord (spelled keys))
         (does (lookup-key chord it)))
    (cond (does
           (setf *pending* nil)
           (handler-case (progn (funcall *run* does) :taken)
             (error (c)
               (g:note "~a: ~a" chord c)
               (ignore-errors
                (let ((*print-pretty* nil))
                  (sb-debug:print-backtrace :count 20 :stream *error-output*)))
               :taken)))
          ((prefixp chord it) (setf *pending* keys) :pending)
          (t (setf *pending* nil)
             (or (and *unbound* (funcall *unbound* chord)) :unbound)))))

(defun setq-mode (it name value)
  (setf (gethash name (mode-values (as-mode it))) value))

(defun setq-default (name value)
  (setq-mode (global-map) name value))

(defun mode-value (name &optional (it (current-mode)))
  (loop :for m :in (%chain (as-mode it))
        :do (multiple-value-bind (found had) (gethash name (mode-values m))
              (when had (return (values found m))))))

(defun (setf mode-value) (value name &optional (it (current-mode)))
  (setq-mode it name value))

(mode-named 'mode)
