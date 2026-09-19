;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx)

(defstruct (pane (:constructor %make-pane))
  (term nil)
  (fd -1 :type fixnum)
  (pid -1 :type fixnum)
  (running t :type boolean)
  (dirty t :type boolean)
  (rang nil :type boolean)
  (named nil)
  (command nil)
  (decoder (vt:make-decoder)))

(defun make-pane (command &key (rows 24) (cols 80))
  "A pane with a terminal that size and no program in it yet.

Starting it is a second step because the size it is started at is the size it is
told, once: a shell that asks stty for it on its first line must be told the
room the layout gave it rather than a guess it is corrected out of afterwards."
  (let ((pane (%make-pane :command command)))
    (setf (pane-term pane)
          (vt:make-term :width cols :height rows
                        :bell-fn (lambda (term)
                                   (declare (ignore term))
                                   (setf (pane-rang pane) t))
                        :title-fn (lambda (term title)
                                    (declare (ignore term))
                                    (setf (pane-named pane) title))
                        :input-fn (lambda (term said)
                                    (declare (ignore term))
                                    (pane-say pane said))))
    pane))

(defun pane-started (pane) (>= (pane-fd pane) 0))

(defun pane-start (pane)
  "Run the pane's program on a terminal of its own, the size the pane is now."
  (unless (pane-started pane)
    (let ((term (pane-term pane)))
      (multiple-value-bind (fd pid)
          (pty:spawn-pty-process (pane-command pane)
                                 :rows (vt:term-height term)
                                 :cols (vt:term-width term))
        (setf (pane-fd pane) fd
              (pane-pid pane) pid))))
  pane)

(defun pane-drain (pane &key (budget 16) (size 65536))
  "Read what the program wrote and give it to the term. Answers nil when the
program is done.

At most BUDGET reads a wakeup: the descriptor stays readable and the next poll
comes straight back, so one pane writing without pause cannot starve the rest."
  (dotimes (i budget t)
    (unless (pty:pty-wait (pane-fd pane) 0)
      (return t))
    (let ((said (pty:pty-read-string (pane-fd pane) size)))
      (cond
        ((null said) (setf (pane-running pane) nil) (return nil))
        ((zerop (length said)) (return t))
        (t (vt:term-process-output
            (pane-term pane)
            (vt:decode-utf-8 (pane-decoder pane) said))
           (setf (pane-dirty pane) t))))))

(declaim (ftype function pane-say))

(defun pane-say (pane said)
  (when (and (pane-running pane) (pane-started pane))
    (ignore-errors (pty:pty-write-string (pane-fd pane) said))))

(defun pane-resize (pane rows cols)
  (vt:term-resize (pane-term pane) cols rows)
  (ignore-errors (pty:pty-set-size (pane-fd pane) rows cols))
  (setf (pane-dirty pane) t)
  pane)

(defun pane-close (pane)
  (setf (pane-running pane) nil)
  (when (pane-started pane)
    (ignore-errors (pty:pty-close (pane-fd pane)))
    (ignore-errors (pty:pty-reap (pane-pid pane)))))
