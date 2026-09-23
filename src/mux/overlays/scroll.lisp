;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defstruct (scroll-view (:constructor %make-scroll-view)))

(atty/mode:define-mode scroll-mode (pane-mode))

(defmethod mode-of ((r scroll-view)) 'scroll-mode)

(defmethod draw-overlay ((r scroll-view) screen)
  "Only what says it is on and how to get out: what is being read is the pane."
  (let ((cols (tty:screen-width screen))
        (rows (tty:screen-height screen))
        (leave (car (find #'exit-scroll-mode
                          (atty/mode:keys-in-force (atty/mode:mode-named 'scroll-mode))
                          :key #'cdr))))
    (atty/cells:draw (atty/ui:label (format nil " reading back~@[ · ~A leaves~] " leave)
                                    :face :chip-scrolled)
                     (tty:screen-grid screen) cols rows :left 0 :top (max 0 (1- rows)))
    (setf (tty:screen-cursor-visible screen) nil)))

(defun current-scroll-view ()
  (find-if (lambda (it) (typep it 'scroll-view)) (client-overlays *client*)))

(defcommand (scroll-mode :group scrolling)
  "read this pane back from the keys; q leaves"
  (unless (current-scroll-view)
    (client-push-overlay *client* (%make-scroll-view))
    (send-if-supported (list :reading t))))

(defcommand (exit-scroll-mode :unlisted)
  (let ((r (current-scroll-view)))
    (when r (client-pop-overlay *client* r)))
  (send-if-supported (list :reading nil))
  (send-if-supported (list :find nil nil nil :clear))
  (scroll-to-bottom))

(defcommand (find-in-pane :group scrolling)
  "find in this pane's history, the hits listed as you type; n and N move between them after"
  (open-palette *client* :find))

(defcommand (find-next :group scrolling)
  "the next older hit of the last find"
  (send-if-supported (list :find nil nil nil :next)))

(defcommand (find-previous :group scrolling)
  "the next newer hit of the last find"
  (send-if-supported (list :find nil nil nil :back)))

(defcommand (start-selection :group scrolling)
  "mark the top line shown; y copies from it to wherever you scroll"
  (send-if-supported (list :select :start)))

(defcommand (copy-lines :group scrolling)
  "copy the lines marked, or the screen, to the clipboard"
  (send-if-supported (list :select :copy)))

(defcommand (scroll-mode-page-up :group scrolling)
  "the same, a page back to begin with"
  (scroll-mode)
  (scroll-page-up))

(loop :for (chord does) :in `(("Up" ,#'scroll-up) ("k" ,#'scroll-up)
                              ("Down" ,#'scroll-down) ("j" ,#'scroll-down)
                              ("PageUp" ,#'scroll-page-up) ("PageDown" ,#'scroll-page-down)
                              ("SPC" ,#'scroll-page-down)
                              ("C-u" ,#'scroll-half-page-up) ("C-d" ,#'scroll-half-page-down)
                              ("g" ,#'scroll-to-top) ("Home" ,#'scroll-to-top)
                              ("G" ,#'scroll-to-bottom) ("End" ,#'scroll-to-bottom)
                              ("q" ,#'exit-scroll-mode) ("Escape" ,#'exit-scroll-mode)
                              ("RET" ,#'exit-scroll-mode)
                              ("/" ,#'find-in-pane) ("n" ,#'find-next) ("N" ,#'find-previous)
                              ("v" ,#'start-selection) ("y" ,#'copy-lines))
      :do (atty/mode:define-key 'scroll-mode chord does))
