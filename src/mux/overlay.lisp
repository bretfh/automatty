;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defgeneric overlay-laid-tree (overlay)
  (:documentation "The widget tree OVERLAY was last laid out as, for clicks."))

(defgeneric draw-overlay (thing screen)
  (:documentation "Draw THING onto SCREEN, over whatever the session put there.
This is the seam: anything that can write cells can be put on top, and nothing
else needs to know about it."))

(defgeneric overlay-ticks-p (thing)
  (:documentation "Whether THING on top says how long something has been, and
so is drawn again every second whether anything was said or not.")
  (:method (thing) (declare (ignore thing)) nil))

(defgeneric overlay-passes-keys-p (thing)
  (:documentation "Whether what is typed goes on to the pane while THING is on
top, as it would with nothing there. Something beside the panes rather than in
front of them, that only says things, need not take the keyboard away.")
  (:method (thing) (declare (ignore thing)) nil))

(defgeneric mode-of (thing)
  (:documentation "Which mode a watcher is in while THING is on top.")
  (:method (thing) (declare (ignore thing)) 'pane-mode))

(defgeneric overlay-name (thing)
  (:documentation "What THING on top is called, for its header: nil for
something that need not be said.")
  (:method (thing) (declare (ignore thing)) nil))

(declaim (ftype (function (t watcher) t) close-overlay))
(defgeneric close-overlay (thing watcher)
  (:documentation "Take THING off the top of WATCHER, the way its own close key
would.")
  (:method (thing watcher) (pop-overlay watcher thing)))

(declaim (ftype (function (t string string) t) overlay-session-renamed))
(defgeneric overlay-session-renamed (thing old new)
  (:documentation "THING, drawn on top, hears that session OLD is now NEW.")
  (:method (thing old new) (declare (ignore thing old new)) nil))

(defgeneric overlay-unbound-key (thing chord watcher)
  (:documentation "What to do with a key the mode has no binding for. A prompt
puts it in what has been typed; most things ignore it.")
  (:method (thing chord watcher) (declare (ignore thing chord watcher)) nil))

(defun overlay-tree (&key title path right toolbar body status hints (close t) (keys t))
  "The whole of something on top, as one tree that fills its room."
  (apply #'atty/ui:column :align :stretch :expand 1
         (remove nil
                 (list (header-band title :path path :right right :close close)
                       toolbar
                       (atty/ui:column :align :stretch :expand 1 (or body (atty/ui:label "")))
                       status
                       (footer-band hints
                                    :right (list (and keys (hint "?" "keys" :runs "describe mode"))
                                                 (hint "Esc" "close" :runs :close)))))))

(defvar *rows* nil
  "While one frame is drawn or one key is handled, the pane rows worked out
for it, so they are worked out once.")

(declaim (ftype (function (session watcher) tty:screen) watcher-view))
(defun watcher-view (session watcher)
  "SESSION's screen with whatever WATCHER has on top drawn over it."
  (let ((screen (session-screen session)))
    (if (or (watcher-overlays watcher) (menu-due-p watcher))
        (let ((work (let ((had (watcher-screen watcher)))
                      (if (and had (= (tty:screen-width had) (tty:screen-width screen))
                               (= (tty:screen-height had) (tty:screen-height screen)))
                          had
                          (setf (watcher-screen watcher)
                                (tty:make-screen :width (tty:screen-width screen)
                                                 :height (tty:screen-height screen)))))))
          (tty:screen-copy work screen)
          (let ((*client* watcher)
                (*rows* (list nil)))
            (dolist (it (reverse (watcher-overlays watcher)))
              (draw-overlay it work))
            (if (menu-due-p watcher)
                (draw-menu watcher work)
                (setf (watcher-menu watcher) nil)))
          work)
        screen)))

(defun bar-shown-p (watcher)
  (let ((session (watcher-session watcher)))
    (or (null session) (session-bar-p session))))

(declaim (ftype (function (watcher t) t) push-overlay))
(defun push-overlay (watcher it)
  (push it (watcher-overlays watcher))
  (update-mode watcher)
  (setf (watcher-behind watcher) t)
  it)

(defun pop-overlay (watcher it)
  (setf (watcher-overlays watcher) (remove it (watcher-overlays watcher)))
  (update-mode watcher)
  (setf (watcher-behind watcher) t))

(defun update-mode (watcher)
  "The mode whatever is on top asks for, or the pane's when nothing is."
  (setf (watcher-mode watcher) (mode-of (first (watcher-overlays watcher)))
        (watcher-partial-chord watcher) nil
        (watcher-pending-since watcher) nil
        (watcher-menu watcher) nil))

(defun draw-overlay-tree (tree watcher screen &key top)
  "Draw TREE over the session, under the bar unless TOP says where, on its
own ground, and hide the cursor. Answers the laid tree."
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (top (or top (chrome-top watcher rows)))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows)))
    (atty/cells:fill-rect m 0 top cols (- rows top) (term:make-face :bg (bar-face :bg)))
    (atty/cells:draw tree (tty:screen-grid screen) cols rows :top top)
    (setf (tty:screen-cursor-visible screen) nil)
    tree))

(defgeneric overlay-clicked (thing line col watcher)
  (:documentation "A press of the mouse at LINE, COL while THING is drawn over
the session: true when THING took it, so it goes no further.")
  (:method (thing line col watcher)
    (let* ((tree (overlay-laid-tree thing))
           (hit (and tree (button-at tree line col))))
      (and hit (handle-button thing (bar-button-runs hit) watcher)))))

(defun handle-button (thing runs watcher)
  "What every button on every surface means: :close closes THING, a name runs
that command, a function is called. Answers whether RUNS was one of those;
anything else is THING's own to make sense of."
  (cond ((eq runs :close) (close-overlay thing watcher) t)
        ((stringp runs) (run-command runs watcher) t)
        ((functionp runs) (let ((*client* watcher)) (funcall runs)) t)
        (t nil)))
