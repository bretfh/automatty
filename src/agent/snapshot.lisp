;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/agent)

(defparameter +face-keys+
  '((:fg . term:face-fg) (:bg . term:face-bg) (:bold . term:face-bold)
    (:faint . term:face-faint) (:italic . term:face-italic)
    (:underline . term:face-underline) (:inverse . term:face-inverse)
    (:conceal . term:face-conceal) (:crossed . term:face-crossed)
    (:underline-color . term:face-underline-color) (:blink . term:face-blink)))

(defun face-sexp (face)
  (and face
       (loop :for (key . reader) :in +face-keys+
             :for value := (funcall reader face)
             :when value :append (list key value))))

(defun row-runs (row width)
  (loop :with runs := nil
        :with start := 0
        :for x :from 1 :to width
        :do (when (or (= x width)
                      (not (term:face-equal (term:row-face row x) (term:row-face row start))))
              (let ((face (face-sexp (term:row-face row start))))
                (when face (push (list* start x face) runs)))
              (setf start x))
        :finally (return (nreverse runs))))

(defun snapshot (term)
  (let ((width (term:term-width term))
        (height (term:term-height term)))
    (list :snapshot 1
          :width width :height height
          :cursor (list (term:term-cursor-x term) (term:term-cursor-y term))
          :cursor-visible (and (term:term-cursor-visible term) t)
          :title (term:term-title term)
          :alt-screen (and (term:term-in-alt-screen term) t)
          :rows (loop :for y :below height
                      :for row := (term:term-grid-row term y)
                      :collect (list (copy-seq (term:row-chars row)) (row-runs row width))))))

(defun snapshot-term (snapshot)
  (destructuring-bind (&key width height cursor cursor-visible title alt-screen rows
                       &allow-other-keys)
      (nthcdr 2 snapshot)
    (let ((term (term:make-term :width width :height height)))
      (loop :for (chars runs) :in rows
            :for y :from 0
            :for row := (term:term-grid-row term y)
            :do (replace (term:row-chars row) chars)
                (loop :for (start end . face) :in runs
                      :for made := (apply #'term:make-face face)
                      :do (loop :for x :from start :below end
                                :do (setf (term:row-face row x) made))))
      (setf (term::term-cursor-x term) (first cursor)
            (term::term-cursor-y term) (second cursor)
            (term::term-cursor-visible term) cursor-visible
            (term::term-title term) (or title "")
            (term::term-in-alt-screen term) alt-screen)
      term)))

(defun write-snapshot (snapshot stream)
  (with-standard-io-syntax
    (let ((*print-readably* nil) (*print-right-margin* 200))
      (prin1 snapshot stream)
      (terpri stream))))

(defun read-snapshot (stream)
  (with-standard-io-syntax
    (let ((*read-eval* nil))
      (read stream))))

(defun load-snapshot (path)
  (with-open-file (in path :external-format :utf-8)
    (snapshot-term (read-snapshot in))))
