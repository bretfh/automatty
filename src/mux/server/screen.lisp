;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun session-reset-shadows (session)
  "The session is a different size. Nobody watching knows what is on their own
screen any more, so every shadow goes and everybody is told the new size."
  (let ((rows (session-rows session))
        (cols (session-cols session)))
    (dolist (w (session-watchers session))
      (on-watcher w (let ((w w))
                      (lambda ()
                        (setf (watcher-shadow w) (tty:make-screen :width cols :height rows)
                              (watcher-told w) nil)
                        (draw-again w)
                        (when (watcher-interactive w)
                          (ignore-errors (send-hello w session)))))))))

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
                 (= cols (session-cols session)))
      (setf (session-rows session) rows
            (session-cols session) cols)
      (session-reset-shadows session)
      t)))

(declaim (ftype (function (integer) (integer 1)) duration-turns))
(defun duration-turns (elapsed)
  (let ((unit (cond ((< elapsed 60000) 1000)
                    ((< elapsed 3600000) 60000)
                    (t 3600000))))
    (- unit (mod elapsed unit))))

(declaim (ftype (function () (integer 1)) minute-turns))
(defun minute-turns ()
  (multiple-value-bind (seconds microseconds) (sb-ext:get-time-of-day)
    (- 60000 (mod (+ (* seconds 1000) (floor microseconds 1000)) 60000))))

(declaim (ftype (function (session integer) (integer 1)) session-next-tick))
(defun session-next-tick (session now)
  (if (some #'watcher-overlays (session-watchers session))
      (floor +bar-refresh-interval+ 1000000)
      (let ((next (minute-turns)))
        (dolist (pane (session-panes session) next)
          (let* ((agent (pane-agent pane))
                 (for (agent:agent-for agent now)))
            (when (and for (agent:agent-known-p agent))
              (setf next (min next (duration-turns for)))))))))

(declaim (ftype (function (session) (or null integer)) session-tick-at))
(defun session-tick-at (session)
  (loop :for w :in (session-watchers session)
        :when (watcher-interactive w) :minimize (watcher-clocked w)))

(declaim (ftype (function (session integer) boolean) session-tick))
(defun session-tick (session now)
  "Put everybody watching behind once what the bar or an open overlay shows
could read differently: the minute turned, a time it shows went up, or a second
passed under an overlay. Answers whether it did."
  (let ((ticked nil))
    (dolist (watcher (session-watchers session) ticked)
      (when (watcher-tick watcher session now)
        (setf ticked t)))))

(declaim (ftype (function (watcher session integer) boolean) watcher-tick))
(defun watcher-tick (watcher session now)
  (when (watcher-interactive watcher)
    (when (watcher-overlays watcher)
      (setf (watcher-clocked watcher)
            (min (watcher-clocked watcher) (+ now +bar-refresh-interval+))))
    (when (>= now (watcher-clocked watcher))
      (setf (watcher-clocked watcher)
            (+ now (* 1000000 (session-next-tick session (floor now 1000000))))
            (watcher-behind watcher) t)
      t)))

(defun session-tree (session)
  "What the session looks like: the bar, the rail of sessions and the panes
under it."
  (let ((panes (layout-tree (session-layout session) (session-focus session)
                            session (session-zoomed session))))
    (if (rail-shown-p session)
        (atty/ui:column
         :align :stretch
         (session-bar session)
         (atty/ui:row :align :stretch :spacing 0 :expand 1
                      (session-rail session)
                      (atty/ui:column :align :stretch :expand 1 panes (session-field session))))
        (atty/ui:column
         :align :stretch
         (session-bar session)
         panes
         (atty/ui:row :align :stretch :spacing 0 :background-color (bar-face :ground)
                      (atty/ui:label (make-string +mode-chip-width+ :initial-element #\Space))
                      (atty/ui:column :align :stretch :expand 1 (session-field session)))))))

(defun fit-panes (tree)
  "Give each pane the room the layout gave its view."
  (dolist (v (views-in tree))
    (let ((pane (view-pane v))
          (rows (max 1 (atty/ui:height v)))
          (cols (max 1 (atty/ui:width v))))
      (unless (and (= rows (pane-height pane))
                   (= cols (pane-width pane)))
        (pane-resize pane rows cols)))))

(defun place-cursor (session screen tree)
  "The cursor sits where the pane it belongs to says, moved to where that pane
was put."
  (let* ((v (view-of tree (session-focus session))))
    (multiple-value-bind (x y visible style)
        (let* ((focus (session-focus session))
               (shown (pane-shown-now focus)))
          (if shown
              (let ((it (shown-screen shown)))
                (values (tty:screen-cursor-x it) (tty:screen-cursor-y it)
                        (tty:screen-cursor-visible it) (tty:screen-cursor-style it)))
              (let ((term (pane-term focus)))
                (values (term:term-cursor-x term) (term:term-cursor-y term)
                        (term:term-cursor-visible term) (term:term-cursor-style term)))))
      (setf (tty:screen-cursor-x screen)
            (min (+ (if v (atty/ui:left v) 0) x)
                 (1- (tty:screen-width screen)))
            (tty:screen-cursor-y screen)
            (min (+ (if v (atty/ui:top v) 0) y)
                 (1- (tty:screen-height screen)))
            ;; a pane being read back is not showing the line the cursor is on
            (tty:screen-cursor-visible screen)
            (and (zerop (pane-scrolled (session-focus session))) visible)
            (tty:screen-cursor-style screen) style))))

(declaim (ftype (function (session &optional (or null watcher)) tty:screen) session-compose))
(defun session-compose (session &optional watcher)
  "Measure the session, lay it out, give each pane what it was given, and paint
it. One pass: the panes are resized between the laying and the painting, so what
is drawn is what they have just been told they are."
  (let* ((cols (session-cols session))
         (rows (session-rows session))
         (screen (let ((had (and watcher (watcher-session-screen watcher))))
                   (if (and had (= cols (tty:screen-width had)) (= rows (tty:screen-height had)))
                       had
                       (tty:make-screen :width cols :height rows))))
         (tree (let ((*scrollbars* (session-scrollbars-p session)))
                 (session-tree session)))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows)))
    (atty/ui:with-pass
      (atty/ui:restyle tree)
      (atty/ui:measure tree m cols rows)
      (atty/ui:lay tree m 0 0 cols rows)
      (fit-panes tree)
      (atty/ui:paint tree m))
    (place-cursor session screen tree)
    (if watcher
        (setf (watcher-session-screen watcher) screen
              (watcher-geometry watcher) tree
              (watcher-composed-at watcher) (now-ms))
        (setf (session-screen session) screen
              (session-geometry session) tree))
    screen))

(declaim (ftype (function (session integer) boolean) session-plain-p))
(defun session-plain-p (session since)
  "Whether nothing drawn around the panes shown depends on what is in them:
none is read back, searched or selected in, none was titled or scrolled since
SINCE, and none is an agent, whose frame reads its screen."
  (every (lambda (pane)
           (and (zerop (pane-scrolled pane))
                (null (pane-find pane))
                (null (pane-selecting pane))
                (< (pane-titled-at pane) since)
                (< (pane-scrolled-at pane) since)
                (not (pane-known-p pane))))
         (window-panes (session-window session))))

(declaim (ftype (function (session watcher) (or null tty:screen)) session-repaint))
(defun session-repaint (session watcher)
  "Paint the panes again where the last compose put them, when they are all
that changed. Answers the screen, or nil when it takes a compose."
  (let* ((tree (watcher-geometry watcher))
         (screen (watcher-session-screen watcher))
         (cols (session-cols session))
         (rows (session-rows session)))
    (when (and tree screen
               (= cols (tty:screen-width screen) (atty/ui:width tree))
               (= rows (tty:screen-height screen) (atty/ui:height tree))
               (session-plain-p session (watcher-composed-at watcher)))
      (let ((m (atty/cells:make-cells (tty:screen-grid screen) cols rows)))
        (atty/ui:with-pass
          (labels ((walk (w)
                     (typecase w
                       (pane-area (atty/ui:paint (area-view w) m)
                                  (when (area-bar w) (atty/ui:paint (area-bar w) m)))
                       (pane-position (atty/ui:paint w m))
                       (t (dolist (part (atty/ui:parts w)) (walk part))))))
            (walk tree))))
      (place-cursor session screen tree)
      screen)))

(defun watcher-frame (session watcher)
  (let* ((screen (watcher-view session watcher))
         (runs (tty:screen-diff (watcher-shadow watcher) screen)))
    (when runs
      (multiple-value-bind (said faces) (encode-runs screen runs)
        (send-message watcher (list :frame said faces))))
    (let ((now (list :cursor (tty:screen-cursor-y screen) (tty:screen-cursor-x screen)
                     (tty:screen-cursor-visible screen) (tty:screen-cursor-style screen))))
      (unless (equal now (watcher-told watcher))
        (send-message watcher now)
        (setf (watcher-told watcher) now)))
    (let ((rung (loop :for pane :in (session-panes session) :sum (pane-rang pane))))
      (when (> rung (watcher-rung watcher))
        (send-message watcher '(:bell)))
      (setf (watcher-rung watcher) rung))
    runs))
