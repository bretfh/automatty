(require :asdf)
(asdf:load-system :atty/all)

(defpackage #:term/example
            (:use #:cl)
            (:local-nicknames (#:pty #:atty/pty)))
(in-package #:term/example)

(defun run (command &key (width 80) (height 24) (seconds 2))
  (multiple-value-bind (fd pid) (pty:spawn-pty-process command
                                                       :rows height :cols width)
                       (let ((term (term:make-term :width width :height height))
                             (decoder (term:make-decoder))
                             (deadline (+ (get-internal-real-time)
                                          (* seconds internal-time-units-per-second))))
                         (unwind-protect
                             (loop :while (< (get-internal-real-time) deadline)
                                   :do (if (pty:pty-wait fd 100)
                                           (let ((said (pty:pty-read-string fd 8192)))
                                             (if said
                                                 (term:term-process-output
                                                  term (term:decode-utf-8 decoder said))
                                               (return)))
                                         (return)))
                           (pty:pty-close fd)
                           (pty:pty-reap pid))
                         term)))

(let* ((command (or (uiop:getenv "CMD") "ls --color=always -la /"))
       (term (run command)))
  (format t "~&~A~%" (make-string 80 :initial-element #\-))
  (dotimes (y (term:term-height term))
    (write-line (term:term-render-ansi-line term y)))
  (format t "~A~%" (make-string 80 :initial-element #\-))
  (format t "~&cursor ~D,~D  title ~S~%"
          (term:term-cursor-x term) (term:term-cursor-y term)
          (term:term-title term)))
