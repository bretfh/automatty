;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

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

(defun session-tree (session view &key (layout (window-layout (view-shown-window session view)))
                                        (focus (view-focus-in session view))
                                        (zoomed (view-zoomed-in session view)))
  "What the session looks like: the bar, the rail of sessions and the panes
under it."
  (let ((panes (layout-tree layout focus session view zoomed)))
    (if (rail-shown-p view)
        (atty/ui:column
         :align :stretch
         (session-bar session view)
         (atty/ui:row :align :stretch :spacing 0 :expand 1
                      (session-rail session view)
                      (atty/ui:column :align :stretch :expand 1 panes (session-field view))))
        (atty/ui:column
         :align :stretch
         (session-bar session view)
         panes
         (atty/ui:row :align :stretch :spacing 0 :background-color (bar-face :ground)
                      (atty/ui:label (make-string +mode-chip-width+ :initial-element #\Space))
                      (atty/ui:column :align :stretch :expand 1 (session-field view)))))))

(defun lay-tree (tree screen)
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows)))
    (atty/ui:restyle tree)
    (atty/ui:measure tree m cols rows)
    (atty/ui:lay tree m 0 0 cols rows)
    m))

(defun laid-size (session view pane &optional (layout pane))
  (let ((tree (session-tree session view :layout layout :focus pane :zoomed nil)))
    (atty/ui:with-pass
      (lay-tree tree (tty:make-screen :width (view-cols view) :height (view-rows view))))
    (let ((it (view-of tree pane)))
      (if it
          (values (max 1 (atty/ui:height it)) (max 1 (atty/ui:width it)))
          (values (view-rows view) (view-cols view))))))

(defun fit-panes (watcher tree)
  (let* ((session (watcher-session watcher))
         (fits (mapcar (lambda (v) (list (view-pane v) (max 1 (atty/ui:height v)) (max 1 (atty/ui:width v))))
                       (views-in tree)))
         (others (remove watcher (session-watchers session))))
    (unless (equal fits (watcher-fits watcher))
      (setf (watcher-fits watcher) fits)
      (dolist (it fits) (pane-poke (first it)))
      (dolist (w others) (when (watcher-interactive w) (draw-again w))))
    (loop :for (pane rows cols) :in fits
          :do (dolist (w others)
                (let ((there (and (watcher-interactive w) (assoc pane (watcher-fits w)))))
                  (when there
                    (setf rows (min rows (second there))
                          cols (min cols (third there))))))
              (unless (and (= rows (pane-height pane)) (= cols (pane-width pane)))
                (pane-resize pane rows cols)))))

(defun place-cursor (watcher screen tree)
  "The cursor sits where the pane it belongs to says, moved to where that pane
was put."
  (let* ((focus (watcher-focus watcher))
         (v (view-of tree focus)))
    (multiple-value-bind (x y visible style)
        (let ((shown (pane-shown-now focus)))
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
            (and (zerop (watcher-back watcher focus)) visible)
            (tty:screen-cursor-style screen) style))))

(defun forget-gone-looks (watcher session)
  (let* ((view (watcher-view watcher))
         (panes (session-panes session)))
    (setf (view-looks view)
          (remove-if-not (lambda (it) (member (car it) panes)) (view-looks view)))))

(declaim (ftype (function (session watcher) tty:screen) session-compose))
(defun session-compose (session watcher)
  "Measure what WATCHER sees of the session, lay it out, give each pane what it
was given, and paint it. One pass: the panes are resized between the laying and
the painting, so what is drawn is what they have just been told they are."
  (let* ((cols (watcher-cols watcher))
         (rows (watcher-rows watcher))
         (screen (let ((had (watcher-session-screen watcher)))
                   (if (and had (= cols (tty:screen-width had)) (= rows (tty:screen-height had)))
                       had
                       (tty:make-screen :width cols :height rows))))
         (tree (progn (forget-gone-looks watcher session)
                      (session-tree session (watcher-view watcher)))))
    (atty/ui:with-pass
      (let ((m (lay-tree tree screen)))
        (fit-panes watcher tree)
        (atty/ui:paint tree m)))
    (place-cursor watcher screen tree)
    (setf (watcher-session-screen watcher) screen
          (watcher-geometry watcher) tree
          (watcher-composed-at watcher) (now-ms))
    screen))

(declaim (ftype (function (watcher integer) boolean) session-plain-p))
(defun session-plain-p (watcher since)
  "Whether nothing drawn around the panes WATCHER shows depends on what is in
them: none is read back, searched or selected in, none was titled since SINCE,
and none is an agent, whose frame reads its screen."
  (let ((window (watcher-window watcher))
        (view (watcher-view watcher)))
    (and window
         (every (lambda (pane)
                  (let ((look (cdr (assoc pane (view-looks view)))))
                    (and (zerop (view-back view pane))
                         (not (and look (or (look-find look) (look-selecting look))))
                         (< (pane-titled-at pane) since)
                         (not (pane-known-p pane)))))
                (window-panes window)))))

(declaim (ftype (function (watcher) (or null tty:screen)) session-repaint))
(defun session-repaint (watcher)
  "Paint the panes again where the last compose put them, when they are all
that changed. Answers the screen, or nil when it takes a compose."
  (let* ((tree (watcher-geometry watcher))
         (screen (watcher-session-screen watcher))
         (cols (watcher-cols watcher))
         (rows (watcher-rows watcher)))
    (when (and tree screen
               (= cols (tty:screen-width screen) (atty/ui:width tree))
               (= rows (tty:screen-height screen) (atty/ui:height tree))
               (session-plain-p watcher (watcher-composed-at watcher)))
      (let ((m (atty/cells:make-cells (tty:screen-grid screen) cols rows)))
        (atty/ui:with-pass
          (labels ((walk (w)
                     (typecase w
                       (pane-area (atty/ui:paint (area-view w) m)
                                  (when (area-bar w) (atty/ui:paint (area-bar w) m)))
                       (pane-position (atty/ui:paint w m))
                       (t (dolist (part (atty/ui:parts w)) (walk part))))))
            (walk tree))))
      (place-cursor watcher screen tree)
      screen)))

(defun watcher-frame (session watcher)
  (let* ((screen (overlaid-screen watcher))
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
