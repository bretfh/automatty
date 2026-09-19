(require :asdf)
(asdf:load-system :vt/all)

(defpackage #:vt/bench
  (:use #:cl)
  (:local-nicknames (#:pty #:vt/pty) (#:tty #:vt/tty) (#:mux #:vt/mux)
                    (#:cells #:vt/cells))
  (:export #:corpus-dir #:make-corpora #:corpora))
(in-package #:vt/bench)

(defun corpus-dir ()
  (let ((said (or (uiop:getenv "BENCH_DIR") "/tmp/cl-vt-bench")))
    (pathname (format nil "~A/" (string-right-trim "/" said)))))

(defparameter +corpora+ '("plain" "color" "redraw"))
(defparameter +megabytes+ 32)

(defun corpus-path (name)
  (merge-pathnames (format nil "~A.txt" name) (corpus-dir)))

(defparameter +words+
  #("terminal" "grid" "cursor" "scroll" "region" "alternate" "face" "cell"
    "escape" "sequence" "parser" "column" "margin" "charset" "palette"))

(defun word (n) (aref +words+ (mod n (length +words+))))

(defun write-plain (s)
  (loop :for n :from 0
        :do (format s "~D ~A ~A ~A ~A ~A ~A~%"
                    n (word n) (word (+ n 3)) (word (+ n 7))
                    (word (+ n 11)) (word (+ n 2)) (word (+ n 5)))
        :while (< (file-position s) (* +megabytes+ 1024 1024))))

(defun write-color (s)
  (let ((e #\Escape))
    (loop :for n :from 0
          :do (format s "~C[38;5;~Dm~D~C[0m ~C[1;32m~A~C[0m ~C[34m~A~C[0m ~A ~C[3;31m~A~C[0m~%"
                      e (mod n 256) n e
                      e (word n) e
                      e (word (+ n 3)) e
                      (word (+ n 7))
                      e (word (+ n 11)) e)
          :while (< (file-position s) (* +megabytes+ 1024 1024)))))

(defun write-redraw (s)
  (let ((e #\Escape))
    (loop :for frame :from 0
          :do (format s "~C[H~C[2J" e e)
              (dotimes (y 24)
                (format s "~C[~D;1H~C[7m~2,'0D~C[0m ~A ~C[36m~A~C[0m ~D"
                        e (1+ y) e y e (word (+ frame y)) e
                        (word (+ frame y 5)) e (* frame y)))
              (format s "~C[24;60H" e)
          :while (< (file-position s) (* +megabytes+ 1024 1024)))))

(defun make-corpora (&optional force)
  (ensure-directories-exist (corpus-dir))
  (dolist (name +corpora+ (corpora))
    (let ((path (corpus-path name)))
      (when (or force (not (probe-file path)))
        (with-open-file (s path :direction :output :if-exists :supersede
                                :external-format :latin-1)
          (funcall (ecase (intern (string-upcase name) :keyword)
                     (:plain #'write-plain)
                     (:color #'write-color)
                     (:redraw #'write-redraw))
                   s))))))

(defun corpora ()
  (mapcar (lambda (name) (cons name (corpus-path name))) +corpora+))

(defun slurp (path)
  (with-open-file (s path :external-format :latin-1)
    (let ((said (make-string (file-length s))))
      (subseq said 0 (read-sequence said s)))))
