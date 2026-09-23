;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun session-pane-term (session)
  (pane-term (session-focus session)))

(defun oldest-watcher (session)
  (let ((here (remove-if-not #'watcher-interactive (session-watchers session))))
    (if here (reduce #'min here :key #'watcher-sent) 0)))

(defun session-reset-shadows (session)
  "The session is a different size. Nobody watching knows what is on their own
screen any more, so every shadow goes and everybody is told the new size."
  (let ((rows (session-rows session))
        (cols (session-cols session)))
    (tty:screen-resize (session-screen session) cols rows)
    (dolist (pane (session-panes session)) (setf (pane-dirty pane) t))
    (dolist (w (session-watchers session))
      (setf (watcher-shadow w) (tty:make-screen :width cols :height rows)
            (watcher-told w) nil
            (watcher-behind w) t)
      (when (watcher-interactive w)
        (ignore-errors (send-hello w session))))))

(defun session-fit (session)
  "As big as the smallest watcher can show, so nobody is shown a screen with a
piece missing. What each pane gets out of that is the layout pass's business,
not this one's."
  (let ((rows (session-rows session))
        (cols (session-cols session))
        (here (remove-if-not #'watcher-interactive (session-watchers session))))
    (when here
      (setf rows (reduce #'min here :key #'watcher-rows)
            cols (reduce #'min here :key #'watcher-cols)))
    (setf rows (max 1 (min rows +max-pane-size+))
          cols (max 1 (min cols +max-pane-size+)))
    (unless (and (= rows (session-rows session))
                 (= cols (session-cols session))
                 (= rows (tty:screen-height (session-screen session)))
                 (= cols (tty:screen-width (session-screen session))))
      (setf (session-rows session) rows
            (session-cols session) cols)
      (session-reset-shadows session)
      t)))

(defun session-tick (session now)
  "Put everybody watching behind when the bar has stood for +BAR-GAP+. Answers
whether it did."
  (when (>= (- now (session-clocked session)) +bar-refresh-interval+)
    (setf (session-clocked session) now)
    (dolist (watcher (session-watchers session))
      (setf (watcher-behind watcher) t))
    t))

(defun session-tree (session)
  "What the session looks like: the bar, and the panes under it."
  (atty/ui:column
   :align :stretch
   (session-bar session)
   (layout-tree (session-layout session) (session-focus session)
                session (session-zoomed session))))

(defun fit-panes (tree)
  "Give each pane the room the layout gave its view."
  (dolist (v (views-in tree))
    (let ((pane (view-pane v))
          (rows (max 1 (atty/ui:height v)))
          (cols (max 1 (atty/ui:width v))))
      (unless (and (= rows (term:term-height (pane-term pane)))
                   (= cols (term:term-width (pane-term pane))))
        (pane-resize pane rows cols)))))

(defun place-cursor (session tree)
  "The cursor sits where the pane it belongs to says, moved to where that pane
was put."
  (let* ((screen (session-screen session))
         (v (view-of tree (session-focus session)))
         (term (session-pane-term session)))
    (setf (tty:screen-cursor-x screen)
          (min (+ (if v (atty/ui:left v) 0) (term:term-cursor-x term))
               (1- (tty:screen-width screen)))
          (tty:screen-cursor-y screen)
          (min (+ (if v (atty/ui:top v) 0) (term:term-cursor-y term))
               (1- (tty:screen-height screen)))
          ;; a pane being read back is not showing the line the cursor is on
          (tty:screen-cursor-visible screen)
          (and (zerop (pane-scrolled (session-focus session)))
               (term:term-cursor-visible term))
          (tty:screen-cursor-style screen) (term:term-cursor-style term))))

(defun session-compose (session)
  "Measure the session, lay it out, give each pane what it was given, and paint
it. One pass: the panes are resized between the laying and the painting, so what
is drawn is what they have just been told they are."
  (let* ((screen (session-screen session))
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (tree (let ((*scrollbars* (session-scrollbars-p session)))
                 (session-tree session)))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows)))
    (atty/ui:with-pass
      (atty/ui:restyle tree)
      (atty/ui:measure tree m cols rows)
      (atty/ui:lay tree m 0 0 cols rows)
      (setf (session-geometry session) tree)
      (fit-panes tree)
      (atty/ui:paint tree m))
    (place-cursor session tree)
    screen))

(declaim (ftype function send-message))

(defun watcher-frame (session watcher)
  (let* ((screen (session-screen session))
         (runs (tty:screen-diff (watcher-shadow watcher) screen)))
    (when runs
      (multiple-value-bind (said faces) (encode-runs screen runs)
        (send-message watcher (list :frame said faces))))
    (let ((now (list :cursor (tty:screen-cursor-y screen) (tty:screen-cursor-x screen)
                     (tty:screen-cursor-visible screen) (tty:screen-cursor-style screen))))
      (unless (equal now (watcher-told watcher))
        (send-message watcher now)
        (setf (watcher-told watcher) now)))
    (when (some #'pane-rang (session-panes session))
      (send-message watcher '(:bell)))
    runs))
