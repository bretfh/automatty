;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defgeneric overlay-laid-tree (overlay)
  (:documentation "The widget tree OVERLAY was last laid out as, for clicks."))

(defgeneric draw-overlay (thing screen)
  (:documentation "Draw THING onto SCREEN, over whatever the session put there.
This is the seam: anything that can write cells can be put on top, and nothing
else needs to know about it."))

(defvar *overlay-client* nil
  "The client whatever is being drawn over its session belongs to. Something on
top that shows what the server said about the panes reads it from here.")

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
  (:documentation "Which mode a client is in while THING is on top.")
  (:method (thing) (declare (ignore thing)) 'pane-mode))

(defgeneric overlay-name (thing)
  (:documentation "What THING on top is called, for its header: nil for
something that need not be said.")
  (:method (thing) (declare (ignore thing)) nil))

(defgeneric close-overlay (thing client)
  (:documentation "Take THING off the top of CLIENT, the way its own close key
would.")
  (:method (thing client) (client-pop-overlay client thing)))

(defgeneric overlay-session-renamed (thing old new)
  (:documentation "THING, drawn on top, hears that session OLD is now NEW.")
  (:method (thing old new) (declare (ignore thing old new)) nil))

(defgeneric overlay-unbound-key (thing chord client)
  (:documentation "What to do with a key the mode has no binding for. A prompt
puts it in what has been typed; most things ignore it.")
  (:method (thing chord client) (declare (ignore thing chord client)) nil))

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

(defun draw-overlay-tree (tree client screen &key top)
  "Draw TREE over the session, under the bar unless TOP says where, on its
own ground, and hide the cursor. Answers the laid tree."
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (top (or top (if (client-barp client) (min 1 (max 0 (1- rows))) 0)))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows)))
    (atty/cells:fill-rect m 0 top cols (- rows top) (term:make-face :bg (bar-face :bg)))
    (atty/cells:draw tree (tty:screen-grid screen) cols rows :top top)
    (setf (tty:screen-cursor-visible screen) nil)
    tree))

(defgeneric overlay-clicked (thing line col client)
  (:documentation "A press of the mouse at LINE, COL while THING is drawn over
the session: true when THING took it, so it is not passed on to the server.")
  (:method (thing line col client)
    (let* ((tree (overlay-laid-tree thing))
           (hit (and tree (button-at tree line col))))
      (and hit (handle-button thing (bar-button-runs hit) client)))))

(defun handle-button (thing runs client)
  "What every button on every surface means: :close closes THING, a name
runs that command here, a form the server knows is said to it. Answers
whether RUNS was one of those; anything else is THING's own to make sense of."
  (cond ((eq runs :close) (close-overlay thing client) t)
        ((stringp runs) (run-command runs client) t)
        ((and (consp runs) (keywordp (first runs)) (member (first runs) (client-message-types client)))
         (wire-send (client-wire client) runs)
         t)
        (t nil)))
