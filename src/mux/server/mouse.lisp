;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun handle-click (session watcher x y)
  "Whatever was at (X, Y) the last time this session was drawn is told about
the click: the toggle on the search bar cycles which prompt it opens, another
bar button tells WATCHER, the one who clicked, to run what it is for, and a
pane becomes the focus. A geometry from before the first draw, or a click that
landed on a rule, does nothing."
  (let ((hit (and (session-geometry session)
                  (atty/ui:under (session-geometry session) y x))))
    (cond
      ((and (typep hit 'bar-button) (eq (bar-button-runs hit) :cycle-search-kind))
       (setf (session-field-kind session)
             (mod (1+ (session-field-kind session)) (length +palette-prefixes+)))
       (dolist (w (session-watchers session)) (setf (watcher-behind w) t)))
      ((typep hit 'bar-button)
       (let ((runs (bar-button-runs hit)))
         ;; a form is the server's to do, as though the one who clicked had
         ;; said it; a name is a command for them to run
         (if (consp runs)
             (handle-message (session-server session) watcher runs)
             (send-message watcher (list :do runs)))))
      ((and (typep hit 'pane-view) (not (eq (view-pane hit) (session-focus session))))
       (session-focus-pane session (view-pane hit))))))

(defun region-at (session x y)
  (and x y (session-geometry session)
       (atty/ui:under (session-geometry session) y x)))

(defun pane-at (session x y)
  "The pane whose view, scrollbar or chip is at (X, Y), and which of them it is."
  (let ((hit (region-at session x y)))
    (when (typep hit '(or pane-view scrollbar live-chip))
      (values (view-pane hit) hit))))

(defun pane-scrollbar (session pane)
  "PANE's scrollbar as the session was last laid out."
  (labels ((walk (w)
             (if (and (typep w 'scrollbar) (eq pane (view-pane w)))
                 w
                 (some #'walk (atty/ui:parts w)))))
    (and (session-geometry session) (walk (session-geometry session)))))

(defun scroll-pane (pane amount)
  "Read PANE back by AMOUNT: a number of rows, further back when it is more than
nought, or :page-up, :page-down, :half-up, :half-down, :top or :bottom."
  (when pane
    (let ((rows (term:term-height (pane-term pane))))
      (case amount
        (:top (pane-scroll-to pane (pane-history pane)))
        (:bottom (pane-scroll-to pane 0))
        (:page-up (pane-scroll-by pane (max 1 (1- rows))))
        (:page-down (pane-scroll-by pane (- (max 1 (1- rows)))))
        (:half-up (pane-scroll-by pane (max 1 (floor rows 2))))
        (:half-down (pane-scroll-by pane (- (max 1 (floor rows 2)))))
        (t (when (integerp amount) (pane-scroll-by pane amount)))))))

(defun send-mouse-to-program (session pane kind x y &rest keys)
  "Say to PANE's program that a mouse did KIND at (X, Y) of the session, where
it is in the pane's own rows and columns. Answers whether it wanted to know."
  (let* ((view (and (session-geometry session)
                    (view-of (session-geometry session) pane)))
         (term (pane-term pane))
         (said (and view
                    (zerop (pane-scrolled pane))
                    (apply #'term:mouse-report term kind
                           (max 0 (min (1- (term:term-width term))
                                       (- x (atty/ui:left view))))
                           (max 0 (min (1- (term:term-height term))
                                       (- y (atty/ui:top view))))
                           keys))))
    (when said
      (pane-write pane said)
      t)))

(defun handle-wheel (session way x y mods)
  "A notch of the wheel, WAY being :up or :down, over whatever is at (X, Y), or
over the pane with the focus when nobody said where.

A program that asked for the mouse is told. One that has the whole screen and
did not is sent the arrow keys, which is what it scrolls by. Anything else has
its pane read back. With shift held it is always the pane that is read back,
and so it is over the scrollbar, which is nobody's but the multiplexer's."
  (multiple-value-bind (pane hit) (pane-at session x y)
    (let* ((pane (or pane (session-focus session)))
           (term (and pane (pane-term pane)))
           (sideways (member way '(:left :right)))
           (rows (if (eq way :up) +wheel-rows+ (- +wheel-rows+))))
      (when pane
        (cond
          (sideways
           ;; nothing here scrolls sideways; a program that asked is told
           (when (typep hit 'pane-view)
             (send-mouse-to-program session pane :wheel x y :wheel way
                               :meta (and (member :meta mods) t)
                               :ctrl (and (member :ctrl mods) t))))
          ((or (member :shift mods) (typep hit 'scrollbar) (plusp (pane-scrolled pane)))
           (pane-scroll-by pane rows))
          ((and (typep hit 'pane-view)
                (send-mouse-to-program session pane :wheel x y :wheel way
                                  :meta (and (member :meta mods) t)
                                  :ctrl (and (member :ctrl mods) t))))
          ((term:term-in-alt-screen term)
           (let ((key (term:key-event-to-escape-sequence
                       term (list (if (eq way :up) :up :down)))))
             (dotimes (i +wheel-rows+) (pane-write pane key))))
          (t (pane-scroll-by pane rows)))))))

(defun hold-scrollbar (session pane part line)
  "An arrow or the track of PANE's scrollbar is being held at LINE: do what one
click of it does, and go on doing it until it is let go. The track stops when
the thumb has come to where the pointer is."
  (let ((held (list :bar pane part line)))
    (setf (session-held session) held)
    (labels ((once ()
               (let* ((bar (pane-scrollbar session pane))
                      (now (and bar (scrollbar-part bar (fourth held)))))
                 (ecase part
                   (:up (pane-scroll-by pane 1))
                   (:down (pane-scroll-by pane -1))
                   (:above (when (eq now :above) (scroll-pane pane :page-up)))
                   (:below (when (eq now :below) (scroll-pane pane :page-down))))))
             (again ()
               (when (eq held (session-held session))
                 (once)
                 (when (session-server session)
                   (schedule-task (session-server session) +hold-every+ #'again)))))
      (once)
      (when (session-server session)
        (schedule-task (session-server session) +hold-after+ #'again)))))

(defun handle-pointer (session watcher what button x y mods)
  "A button of the mouse went down, moved while down, or came up: WHAT is
:press, :drag or :release.

On the scrollbar it is the scrollbar's. On the chip it is back to live. Anywhere
else a press of the left button is what a click has always been, and whichever
button it was, a program that asked for the mouse is told, from the press to
the release, wherever the pointer went in between."
  (let ((held (session-held session)))
    (ecase what
      (:press
       (multiple-value-bind (pane hit) (pane-at session x y)
         (setf (session-held session) nil)
         (cond
           ((and (typep hit 'scrollbar) (eq button :left))
            (let ((part (scrollbar-part hit y)))
              (case part
                ((nil))
                (:thumb
                 (multiple-value-bind (from track) (scrollbar-track hit)
                   (let ((top (scrollbar-thumb track (term:term-height (pane-term pane))
                                               (pane-history pane) (pane-scrolled pane))))
                     (setf (session-held session)
                           (list :thumb pane (- y from top))))))
                (t (hold-scrollbar session pane part y)))))
           ((typep hit 'scrollbar))
           ((typep hit 'live-chip)
            (when (eq button :left) (pane-scroll-to pane 0)))
           (t
            (when (eq button :left) (handle-click session watcher x y))
            (when (and (typep hit 'pane-view)
                       (send-mouse-to-program session pane :press x y :button button
                                         :shift (and (member :shift mods) t)
                                         :meta (and (member :meta mods) t)
                                         :ctrl (and (member :ctrl mods) t)))
              (setf (session-held session) (list :pane pane button)))))))
      (:drag
       (case (first held)
         (:thumb
          (let* ((pane (second held))
                 (bar (pane-scrollbar session pane)))
            (when bar
              (pane-scroll-to pane (scrollbar-back-at bar y (third held))))))
         (:bar (setf (fourth held) y))
         (:pane
          (send-mouse-to-program session (second held) :drag x y :button (third held)))))
      (:release
       (when (eq (first held) :pane)
         (send-mouse-to-program session (second held) :release x y :button (third held)))
       (setf (session-held session) nil)))))
