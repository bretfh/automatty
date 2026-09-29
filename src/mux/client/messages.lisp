;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun handle-server-message (client form)
  (case (first form)
    (:hello
     (destructuring-bind (name rows cols &rest more) (rest form)
       (declare (ignore name more))
       (setf (client-greeted client) t)
       (client-fit client rows cols)
       (host-write client tty:+blanked+)))
    (:frame
     (destructuring-bind (said faces) (rest form)
       (decode-runs-into-screen (client-from client) said faces)
       (setf (client-dirty client) t)))
    (:cursor
     (destructuring-bind (y x visible style) (rest form)
       (let ((screen (client-from client)))
         (setf (tty:screen-cursor-y screen) y
               (tty:screen-cursor-x screen) x
               (tty:screen-cursor-visible screen) (and visible t)
               (tty:screen-cursor-style screen) style))
       (setf (client-dirty client) t)))
    (:bell (host-write client (string (code-char 7))))
    (:bracketed-paste
     (host-write client (format nil "~C[?2004~C" (code-char 27) (if (second form) #\h #\l))))
    (:copied
     (host-write client (format nil "~C]52;c;~A~C" (code-char 27) (base64 (second form)) (code-char 7))))
    (:bye (client-stop client (second form))
     (when (eq (second form) :restarting)
       (setf *successor* (third form))))
    (t nil)))
