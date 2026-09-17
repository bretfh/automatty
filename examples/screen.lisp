(require :asdf)
(asdf:load-system :vt/all)

(defpackage #:vt/example
  (:use #:cl)
  (:local-nicknames (#:pty #:vt/pty)))
(in-package #:vt/example)

(defun run (command &key (width 80) (height 24) (seconds 2))
  (multiple-value-bind (fd pid) (pty:spawn-pty-process command
                                                       :rows height :cols width)
    (let ((term (vt:make-term :width width :height height))
          (deadline (+ (get-internal-real-time)
                       (* seconds internal-time-units-per-second))))
      (unwind-protect
           (loop :while (< (get-internal-real-time) deadline)
                 :do (if (pty:pty-wait fd 100)
                         (let ((said (pty:pty-read-string fd 8192)))
                           (if said
                               (vt:term-process-output term said)
                               (return)))
                         (return)))
        (pty:pty-close fd)
        (pty:pty-reap pid))
      term)))

(let* ((command (or (uiop:getenv "CMD") "ls --color=always -la /"))
       (term (run command)))
  (format t "~&~A~%" (make-string 80 :initial-element #\-))
  (dotimes (y (vt:term-height term))
    (write-line (vt:term-render-ansi-line term y)))
  (format t "~A~%" (make-string 80 :initial-element #\-))
  (format t "~&cursor ~D,~D  title ~S~%"
          (vt:term-cursor-x term) (vt:term-cursor-y term)
          (vt:term-title term)))
