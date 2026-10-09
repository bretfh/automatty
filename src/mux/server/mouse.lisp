;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun handle-click (watcher x y)
  "Whatever was at (X, Y) the last time this session was drawn is told about
the click: the toggle on the search bar cycles which prompt it opens, another
bar button tells WATCHER, the one who clicked, to run what it is for, and a
pane becomes the focus. A geometry from before the first draw, or a click that
landed on a rule, does nothing."
  (let ((hit (let ((tree (watcher-geometry watcher)))
               (and tree (atty/ui:under tree y x)))))
    (cond
      ((and (typep hit 'bar-button) (eq (bar-button-runs hit) :cycle-search-kind))
       (setf (watcher-field-kind watcher)
             (mod (1+ (watcher-field-kind watcher)) (length +palette-prefixes+))))
      ((typep hit 'bar-button)
       (let ((runs (bar-button-runs hit)))
         (if (functionp runs)
             (let ((*client* watcher)) (funcall runs))
             (run-command runs watcher))))
      ((and (typep hit 'pane-view) (not (eq (view-pane hit) (watcher-focus watcher))))
       (setf (watcher-focus watcher) (view-pane hit))))))

(defun region-at (watcher x y)
  (let ((tree (watcher-geometry watcher)))
    (and x y tree (atty/ui:under tree y x))))

(defun pane-at (watcher x y)
  "The pane whose view, scrollbar or chip is at (X, Y), and which of them it is."
  (let ((hit (region-at watcher x y)))
    (when (typep hit '(or pane-view scrollbar live-chip))
      (values (view-pane hit) hit))))

(defun pane-scrollbar (watcher pane)
  "PANE's scrollbar as WATCHER last laid it out."
  (labels ((walk (w)
             (if (and (typep w 'scrollbar) (eq pane (view-pane w)))
                 w
                 (some #'walk (atty/ui:parts w)))))
    (let ((tree (watcher-geometry watcher)))
      (and tree (walk tree)))))

(defun scroll-pane (watcher pane amount)
  "Read PANE back by AMOUNT for WATCHER: a number of rows, further back when it
is more than nought, or :page-up, :page-down, :half-up, :half-down, :top or
:bottom."
  (when pane
    (let ((rows (pane-height pane)))
      (case amount
        (:top (scroll-to watcher pane (pane-history pane)))
        (:bottom (scroll-to watcher pane 0))
        (:page-up (scroll-back-by watcher pane (max 1 (1- rows))))
        (:page-down (scroll-back-by watcher pane (- (max 1 (1- rows)))))
        (:half-up (scroll-back-by watcher pane (max 1 (floor rows 2))))
        (:half-down (scroll-back-by watcher pane (- (max 1 (floor rows 2)))))
        (t (when (integerp amount) (scroll-back-by watcher pane amount)))))))

(defun send-mouse-to-program (watcher pane kind x y &rest keys)
  "Say to PANE's program that a mouse did KIND at (X, Y) of what WATCHER shows,
where it is in the pane's own rows and columns. Answers whether it wanted to
know."
  (let* ((view (let ((tree (watcher-geometry watcher)))
                (and tree (view-of tree pane))))
         (said (and view
                    (zerop (watcher-back watcher pane))
                    (with-term (term pane)
                      (apply #'term:mouse-report term kind
                             (max 0 (min (1- (term:term-width term))
                                         (- x (atty/ui:left view))))
                             (max 0 (min (1- (term:term-height term))
                                         (- y (atty/ui:top view))))
                             keys)))))
    (when said
      (pane-write pane said)
      t)))

(defun handle-wheel (watcher way x y mods)
  "A notch of the wheel, WAY being :up or :down, over whatever is at (X, Y), or
over the pane with the focus when nobody said where.

A program that asked for the mouse is told. One that has the whole screen and
did not is sent the arrow keys, which is what it scrolls by. Anything else has
its pane read back. With shift held it is always the pane that is read back,
and so it is over the scrollbar, which is nobody's but the multiplexer's."
  (multiple-value-bind (pane hit) (pane-at watcher x y)
    (let* ((pane (or pane (watcher-focus watcher)))
           (sideways (member way '(:left :right)))
           (rows (if (eq way :up) +wheel-rows+ (- +wheel-rows+))))
      (when pane
        (cond
          (sideways
           ;; nothing here scrolls sideways; a program that asked is told
           (when (typep hit 'pane-view)
             (send-mouse-to-program watcher pane :wheel x y :wheel way
                               :meta (and (member :meta mods) t)
                               :ctrl (and (member :ctrl mods) t))))
          ((or (member :shift mods) (typep hit 'scrollbar) (plusp (watcher-back watcher pane)))
           (scroll-back-by watcher pane rows))
          ((and (typep hit 'pane-view)
                (send-mouse-to-program watcher pane :wheel x y :wheel way
                                  :meta (and (member :meta mods) t)
                                  :ctrl (and (member :ctrl mods) t))))
          ((with-term (term pane) (term:term-in-alt-screen term))
           (let ((key (with-term (term pane)
                        (term:key-event-to-escape-sequence
                         term (list (if (eq way :up) :up :down))))))
             (dotimes (i +wheel-rows+) (pane-write pane key))))
          (t (scroll-back-by watcher pane rows)))))))

(defun hold-scrollbar (watcher pane part line)
  "An arrow or the track of PANE's scrollbar is being held at LINE: do what one
click of it does, and go on doing it until it is let go. The track stops when
the thumb has come to where the pointer is."
  (let ((held (list :bar pane part line))
        (server (watcher-server watcher)))
    (setf (watcher-held watcher) held)
    (labels ((once ()
               (let* ((bar (pane-scrollbar watcher pane))
                      (now (and bar (scrollbar-part bar (fourth held)))))
                 (ecase part
                   (:up (scroll-back-by watcher pane 1))
                   (:down (scroll-back-by watcher pane -1))
                   (:above (when (eq now :above) (scroll-pane watcher pane :page-up)))
                   (:below (when (eq now :below) (scroll-pane watcher pane :page-down))))))
             (again ()
               (when (eq held (watcher-held watcher))
                 (once)
                 (later +hold-every+)))
             (later (ms)
               (when server
                 (schedule-task server ms (lambda () (on-watcher watcher #'again))))))
      (once)
      (later +hold-after+))))

(defun handle-pointer (watcher what button x y mods)
  "A button of the mouse went down, moved while down, or came up: WHAT is
:press, :drag or :release.

On the scrollbar it is the scrollbar's. On the chip it is back to live. Anywhere
else a press of the left button is what a click has always been, and whichever
button it was, a program that asked for the mouse is told, from the press to
the release, wherever the pointer went in between."
  (let ((held (watcher-held watcher)))
    (ecase what
      (:press
       (multiple-value-bind (pane hit) (pane-at watcher x y)
         (setf (watcher-held watcher) nil)
         (cond
           ((and (typep hit 'scrollbar) (eq button :left))
            (let ((part (scrollbar-part hit y)))
              (case part
                ((nil))
                (:thumb
                 (multiple-value-bind (from track) (scrollbar-track hit)
                   (let ((top (scrollbar-thumb track (pane-height pane)
                                               (pane-history pane) (watcher-back watcher pane))))
                     (setf (watcher-held watcher)
                           (list :thumb pane (- y from top))))))
                (t (hold-scrollbar watcher pane part y)))))
           ((typep hit 'scrollbar))
           ((typep hit 'live-chip)
            (when (eq button :left) (scroll-to watcher pane 0)))
           (t
            (when (eq button :left) (handle-click watcher x y))
            (when (and (typep hit 'pane-view)
                       (send-mouse-to-program watcher pane :press x y :button button
                                         :shift (and (member :shift mods) t)
                                         :meta (and (member :meta mods) t)
                                         :ctrl (and (member :ctrl mods) t)))
              (setf (watcher-held watcher) (list :pane pane button)))))))
      (:drag
       (case (first held)
         (:thumb
          (let* ((pane (second held))
                 (bar (pane-scrollbar watcher pane)))
            (when bar
              (scroll-to watcher pane (scrollbar-back-at bar y (third held))))))
         (:bar (setf (fourth held) y))
         (:pane
          (send-mouse-to-program watcher (second held) :drag x y :button (third held)))))
      (:release
       (when (eq (first held) :pane)
         (send-mouse-to-program watcher (second held) :release x y :button (third held)))
       (setf (watcher-held watcher) nil)))))
