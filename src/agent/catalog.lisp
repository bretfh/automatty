;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/agent)

(defun catalog-entries (dir)
  (sort (loop :for path :in (directory (merge-pathnames "*/*/reader.lisp"
                                                        (uiop:ensure-directory-pathname dir)))
              :collect (let ((parts (last (pathname-directory path) 2)))
                         (list (first parts) (second parts) path)))
        (lambda (a b)
          (or (string< (first a) (first b))
              (and (string= (first a) (first b)) (version< (second a) (second b)))))))

(defun form-order (form)
  (list (string-downcase (symbol-name (first form)))
        (let ((versions (getf (rest form) :versions)))
          (or (and (consp versions) (stringp (first versions)) (first versions)) ""))))

(defun register-reader-texts (texts)
  (let ((forms nil)
        (loaded nil)
        (refused nil))
    (dolist (text texts)
      (handler-case (push (with-input-from-string (in text) (read-reader in)) forms)
        (error (e) (push (format nil "~A" e) refused))))
    (dolist (form (sort forms (lambda (a b)
                                (destructuring-bind (an av) (form-order a)
                                  (destructuring-bind (bn bv) (form-order b)
                                    (or (string< an bn) (and (string= an bn) (version< av bv))))))))
      (handler-case (let ((reader (register-reader form)))
                      (push (format nil "~A~{ ~A~}" (reader-name reader) (reader-versions reader)) loaded))
        (bad-reader (e) (push (format nil "~A" e) refused))))
    (values (nreverse loaded) (nreverse refused))))

(defun load-catalog (dir)
  (register-reader-texts
   (loop :for (nil nil path) :in (catalog-entries dir)
         :collect (uiop:read-file-string path :external-format :utf-8))))

(defun write-index (dir)
  (let ((entries (loop :for (agent version) :in (catalog-entries dir)
                       :collect (format nil "~A/~A" agent version))))
    (with-open-file (out (merge-pathnames "index.sexp" (uiop:ensure-directory-pathname dir))
                         :direction :output :if-exists :supersede :external-format :utf-8)
      (with-standard-io-syntax
        (let ((*print-readably* nil) (*print-pretty* t) (*print-right-margin* 80))
          (prin1 (list :catalog 1 :readers entries) out)
          (terpri out))))
    entries))

(load-catalog (asdf:system-relative-pathname "atty" "readers/"))
