;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/objc)

(defvar *symbols* (make-hash-table :test 'equal))
(defvar *selectors* (make-hash-table :test 'equal))
(defvar *classes* (make-hash-table :test 'equal))
(defvar *started* nil)

(defun forget ()
  (clrhash *symbols*)
  (clrhash *selectors*)
  (clrhash *classes*)
  (setf *started* nil))

(pushnew 'forget sb-ext:*save-hooks*)

(defun start ()
  (unless *started*
    (load-shared-object "/usr/lib/libobjc.dylib" :dont-save t)
    (load-shared-object "/System/Library/Frameworks/AppKit.framework/AppKit" :dont-save t)
    (setf *started* t)))

(defun none () (sb-sys:int-sap 0))

(defun none-p (sap) (zerop (sb-sys:sap-int sap)))

(defun sym (name)
  (or (gethash name *symbols*)
      (let ((sap (alien-funcall (extern-alien "dlsym" (function system-area-pointer system-area-pointer c-string))
                                (sb-sys:int-sap (ldb (byte 64 0) -2)) name)))
        (when (none-p sap)
          (error "~A is in no library loaded" name))
        (setf (gethash name *symbols*) sap))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun alien-type (type)
    (case type
      ((:id :pointer) 'system-area-pointer)
      (:void 'void)
      (:double 'double)
      (:float 'single-float)
      ((:bool :char) 'char)
      (:int 'int)
      (:long 'long)
      (:ulong 'unsigned-long)
      (:short 'short)
      (:ushort 'unsigned-short)
      (:string '(c-string :external-format :utf-8))
      (t type)))

  (defun split-typed (args)
    (values (loop :for (type) :on args :by #'cddr :collect (alien-type type))
            (loop :for (nil value) :on args :by #'cddr :collect value)))

  (defun fallback (ret)
    (case (alien-type ret)
      (void nil)
      (double 0d0)
      (system-area-pointer '(none))
      (t 0))))

(defmacro cfun (name ret &rest types)
  `(sap-alien (sym ,name) (function ,(alien-type ret) ,@(mapcar #'alien-type types))))

(defun class-named (name)
  (or (gethash name *classes*)
      (let ((c (alien-funcall (cfun "objc_lookUpClass" system-area-pointer c-string) name)))
        (when (none-p c)
          (error "there is no class ~A" name))
        (setf (gethash name *classes*) c))))

(defun selector (name)
  (or (gethash name *selectors*)
      (setf (gethash name *selectors*)
            (alien-funcall (cfun "sel_registerName" system-area-pointer c-string) name))))

(defmacro send (ret obj sel &rest args)
  (multiple-value-bind (types values) (split-typed args)
    `(alien-funcall (cfun "objc_msgSend" ,ret :id :id ,@types)
                    ,obj (selector ,sel) ,@values)))

(defmacro id (obj sel &rest args)
  `(send :id ,obj ,sel ,@args))

(defmacro send-super (ret obj super sel &rest args)
  (multiple-value-bind (types values) (split-typed args)
    (let ((place (gensym "SUPER")))
      `(with-alien ((,place (array system-area-pointer 2)))
         (setf (deref ,place 0) ,obj
               (deref ,place 1) ,super)
         (alien-funcall (cfun "objc_msgSendSuper" ,ret :id :id ,@types)
                        (alien-sap ,place) (selector ,sel) ,@values)))))

(defun ns (string)
  (id (class-named "NSString") "stringWithUTF8String:" (c-string :external-format :utf-8) string))

(defun lisp-string (nsstring)
  (if (none-p nsstring)
      ""
      (or (send (c-string :external-format :utf-8) nsstring "UTF8String") "")))

(defun extern-id (name)
  (sb-sys:sap-ref-sap (sym name) 0))

(defun boxed (obj key count)
  (let ((value (id obj "valueForKey:" system-area-pointer (ns key))))
    (with-alien ((into (array double 8)))
      (send void value "getValue:size:" system-area-pointer (alien-sap into) unsigned-long (* 8 count))
      (loop :for i :below count :collect (deref into i)))))

(defun retain (obj) (id obj "retain"))

(defun release (obj) (send void obj "release"))

(defmacro with-cocoa (&body body)
  `(sb-int:with-float-traps-masked (:invalid :divide-by-zero :overflow :underflow :inexact)
     ,@body))

(defmacro with-pool (&body body)
  (let ((pool (gensym "POOL")))
    `(let ((,pool (alien-funcall (cfun "objc_autoreleasePoolPush" system-area-pointer))))
       (unwind-protect (progn ,@body)
         (alien-funcall (cfun "objc_autoreleasePoolPop" void system-area-pointer) ,pool)))))

(defmacro defimp (name ret (&rest args) &body body)
  (let ((does (gensym (string name))))
   `(define-alien-callable ,name ,(alien-type ret)
       ,(loop :for (arg type) :in args :collect (list arg (alien-type type)))
     (with-cocoa
       (handler-case (flet ((,does ,(mapcar #'first args) ,@body))
                       (,does ,@(mapcar #'first args)))
         (error (e)
           (ignore-errors (format *error-output* "~&~(~A~): ~A~%" ',name e))
           ,(fallback ret)))))))

(defun imp (name)
  (alien-sap (alien-callable-function name)))

(defun make-class (name super methods)
  (let ((made (alien-funcall (cfun "objc_lookUpClass" system-area-pointer c-string) name)))
    (when (none-p made)
      (setf made (alien-funcall (cfun "objc_allocateClassPair" system-area-pointer
                                      system-area-pointer c-string unsigned-long)
                                (class-named super) name 0))
      (alien-funcall (cfun "objc_registerClassPair" void system-area-pointer) made))
    (loop :for (sel imp types) :in methods
          :do (alien-funcall (cfun "class_replaceMethod" system-area-pointer
                                   system-area-pointer system-area-pointer system-area-pointer c-string)
                             made (selector sel) (imp imp) types))
    (setf (gethash name *classes*) made)))
