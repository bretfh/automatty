;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; Panes are windows and the keys for them are the ones an editor uses for
;;; windows: 2 splits below, 3 splits beside, 0 closes this one, 1 leaves only
;;; this one, o goes to the next.

(defparameter +prefix-bindings+
  '(
    ("d" . detach)
    ("r" . redraw)
    (":" . commands)
    ("?" . describe-bindings)
    (:prefix . send-prefix)
    ("t" . toggle-bar)
    ("2" . split-below)
    ("3" . split-right)
    ("0" . close-pane)
    ("1" . delete-other-panes)
    ("o" . next-pane)
    ("," . rename-window)
    ("a" . goto-blocked-pane)
    ("z" . zoom-pane)
    ("N" . show-queue)
    ("w" . switchboard)
    ("e" . explain-pane)
    ("C" . new-session)
    ("c" . new-window)
    ("n" . next-window)
    ("p" . previous-window)
    ("&" . close-window)
    ("b" . switch-session)
    ("'" . switch-window)
    ("D" . clients)
    ("." . rename-pane)
    ("$" . rename-session)
    ("[" . scroll-mode)
    ("/" . find-in-pane)
    ("PageUp" . scroll-mode-page-up)
    ("R" . reload-init)))

(defun prefix-string (&optional (prefix +prefix+))
  (atty/mode:spelled (event-key prefix)))

(defun bind-prefix-keys (&optional (prefix +prefix+) was)
  "Bind everything in +PREFIXED-KEYS+ behind PREFIX, first unbinding it from
behind WAS when the prefix has moved."
  (flet ((chord (prefix tail)
           (format nil "~A ~A" (prefix-string prefix)
                   (if (eq tail :prefix) (prefix-string prefix) tail))))
    (when (and was (not (eql was prefix)))
      (loop :for (tail . nil) :in +prefix-bindings+
            :do (atty/mode:undefine-key 'pane-mode (chord was tail))))
    (loop :for (tail . command) :in +prefix-bindings+
          :do (atty/mode:define-key 'pane-mode (chord prefix tail) (symbol-function command)))
    prefix))

(bind-prefix-keys)

(let ((was +prefix+))
  (after-setting '+prefix+ (lambda (prefix)
                             (bind-prefix-keys prefix was)
                             (setf was prefix))))

(dolist (button '("mouse-1" "mouse-2" "mouse-3"))
  (atty/mode:define-key 'pane-mode button #'mouse-pressed)
  (atty/mode:define-key 'pane-mode (format nil "~A-drag" button) #'mouse-dragged)
  (atty/mode:define-key 'pane-mode (format nil "~A-up" button) #'mouse-released))

(atty/mode:define-key 'pane-mode "wheel-up" #'natural-scroll-up)

(atty/mode:define-key 'pane-mode "wheel-down" #'natural-scroll-down)

(atty/mode:define-key 'pane-mode "wheel-left" #'natural-scroll-left)

(atty/mode:define-key 'pane-mode "wheel-right" #'natural-scroll-right)

(atty/mode:define-key 'pane-mode "S-wheel-up" #'scroll-up-line)

(atty/mode:define-key 'pane-mode "S-wheel-down" #'scroll-down-line)

(atty/mode:define-key 'pane-mode "M-wheel-up" #'scroll-up-line)

(atty/mode:define-key 'pane-mode "M-wheel-down" #'scroll-down-line)
