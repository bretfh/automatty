;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(atty/mode:define-mode board-mode ())

(atty/mode:define-mode board-type-mode ()
  (:documentation "Typing a filter or a prompt: every letter is what is typed."))

(defmethod mode-of ((b board))
  (cond ((or (board-filtering b) (board-composing b)) 'board-type-mode)
        (t 'board-mode)))

(defun current-board ()
  (let ((it (first (client-overlays *client*))))
    (when (typep it 'board) it)))

(defmethod overlay-unbound-key ((b board) chord client)
  (let ((said (atty/mode:self-inserting chord)))
    (when said
      (cond
        ((board-composing b)
         (setf (board-composing b) (concatenate 'string (board-composing b) said)))
        ((board-filtering b)
         (setf (board-query b) (concatenate 'string (board-query b) said)))
        ((and (= 1 (length said)) (digit-char-p (char said 0))
              (plusp (digit-char-p (char said 0))))
         (let ((*client* client)
               (row (board-row b client)))
           (when (and row (getf row :asks))
             (send-to-server (list :answer (getf row :session) (getf row :id)
                                    (digit-char-p (char said 0))))))))
      (setf (client-dirty client) t)
      t)))

(defun board-goto (b client session n &optional id)
  "The cursor onto window N of SESSION, on pane ID or its first; the view
follows it when it is drawn."
  (let* ((windows (windows-of client session))
         (window (or (find n windows :key #'first) (first windows))))
    (when window
      (let ((id (or (and id (member id (panes-in-tree (third window))) id)
                    (first (panes-in-tree (third window))))))
        (setf (board-cursor b) (list session (first window) id))))
    (setf (board-following b) t
          (client-dirty client) t)))

(defcommand (switchboard-left :unlisted)
  (let* ((b (current-board)) (place (and b (board-place b *client*))))
    (when place (board-goto b *client* (first place) (max 1 (1- (second place)))))))

(defcommand (switchboard-right :unlisted)
  (let* ((b (current-board)) (place (and b (board-place b *client*))))
    (when place (board-goto b *client* (first place) (1+ (second place))))))

(defcommand (switchboard-first-window :unlisted)
  (let* ((b (current-board)) (place (and b (board-place b *client*))))
    (when place (board-goto b *client* (first place) 1))))

(defcommand (switchboard-last-window :unlisted)
  (let* ((b (current-board)) (place (and b (board-place b *client*))))
    (when place (board-goto b *client* (first place) (length (windows-of *client* (first place)))))))

(defun switchboard-session-by (by)
  (let* ((b (current-board)) (place (and b (board-place b *client*))))
    (when place
      (let* ((sessions (board-sessions b *client*))
             (at (position (first place) sessions :test #'equal))
             (to (max 0 (min (1- (length sessions)) (+ (or at 0) by)))))
        (board-goto b *client* (nth to sessions) (second place))))))

(defcommand (switchboard-up :unlisted) (switchboard-session-by -1))

(defcommand (switchboard-down :unlisted) (switchboard-session-by 1))

(defcommand (switchboard-next-pane :unlisted)
  (let* ((b (current-board)) (place (and b (board-place b *client*))))
    (when place
      (let* ((window (find (second place) (windows-of *client* (first place)) :key #'first))
             (ids (and window (panes-in-tree (third window))))
             (at (position (third place) ids)))
        (when ids
          (board-goto b *client* (first place) (second place)
                       (nth (mod (1+ (or at -1)) (length ids)) ids)))))))

(defcommand (switchboard-select :unlisted)
  "pick the cursor's pane, for a prompt to several at once"
  (let* ((b (current-board)) (key (and b (cursor-key b *client*))))
    (when key
      (setf (board-picked b)
            (if (member key (board-picked b) :test #'equal)
                (remove key (board-picked b) :test #'equal)
                (cons key (board-picked b)))))))

(defun close-board (b client)
  (client-pop-overlay client b)
  (unsubscribe-panes client))

(defcommand (switchboard-goto :unlisted)
  "go to the cursor's pane"
  (let* ((b (current-board)) (row (and b (board-row b *client*))))
    (when b
      (close-board b *client*)
      (when row (send-to-server (list :focus-pane (getf row :session) (getf row :id)))))))

(defun board-zoom-to (b client level)
  (setf (board-zoom b) level
        (board-scroll-y b) 0
        (board-following b) t
        (client-dirty client) t))

(defcommand (switchboard-zoom-in :unlisted)
  "closer: the sessions as rows of windows, then the windows themselves, then one session"
  (let ((b (current-board)))
    (when b (board-zoom-to b *client* (ecase (board-zoom b) (:overview :cards) ((:cards :one) :one))))))

(defcommand (switchboard-zoom-out :unlisted)
  "further: one session to every session's windows, then to a row of windows each"
  (let ((b (current-board)))
    (when b (board-zoom-to b *client* (ecase (board-zoom b) (:one :cards) ((:cards :overview) :overview))))))

(defcommand (switchboard-toggle-side :unlisted)
  "the side pane off or on"
  (let ((b (current-board)))
    (when b (setf (board-side b) (not (board-side b)) (client-dirty *client*) t))))

(defcommand (switchboard-sort :unlisted)
  "the sessions asking first, by name, or as made"
  (let ((b (current-board)))
    (when b (setf (board-sort b) (ecase (board-sort b) (:asking :name) (:name :order) (:order :asking))
                  (client-dirty *client*) t))))

(defcommand (switchboard-fold-quiet :unlisted)
  "lanes with nothing doing folded to their header, or not"
  (let ((b (current-board)))
    (when b (setf (board-fold-quiet b) (not (board-fold-quiet b)) (client-dirty *client*) t))))

(defcommand (switchboard-toggle-live :unlisted)
  "windows showing their panes' last rows, or what each says it is doing"
  (let ((b (current-board)))
    (when b (setf (board-live b) (not (board-live b)) (client-dirty *client*) t))))

(defcommand (switchboard-fold :unlisted)
  "fold the cursor's session to its header and foot, or unfold it"
  (let* ((b (current-board)) (place (and b (board-place b *client*))))
    (when place
      (setf (board-folded b) (if (member (first place) (board-folded b) :test #'equal)
                                 (remove (first place) (board-folded b) :test #'equal)
                                 (cons (first place) (board-folded b)))
            (client-dirty *client*) t))))

(defcommand (switchboard-new-window :unlisted)
  "another window in the cursor's session"
  (let* ((b (current-board)) (place (and b (board-place b *client*))))
    (when place
      (send-to-server (list :new-window (first place)))
      (setf (board-requested-at b) 0))))

(defcommand (switchboard-new-session :unlisted)
  "another session"
  (when (current-board) (send-to-server (list :new))))

(defcommand (switchboard-clients :unlisted)
  (let ((b (current-board)))
    (when b (close-board b *client*))
    (run-command "clients" *client*)))

(defcommand (switchboard-prompt :unlisted)
  "prompt the picked panes, or the cursor's, from here"
  (let ((b (current-board)))
    (when (and b (board-targets b *client*))
      (setf (board-composing b) "")
      (current-client-mode *client*))))

(defcommand (switchboard-read :unlisted)
  "read the cursor's pane whole"
  (let* ((b (current-board)) (row (and b (board-row b *client*))))
    (when row (send-to-server (list :pane-read (getf row :session) (getf row :id))))))

(defcommand (switchboard-name :unlisted)
  "name the cursor's window; TAB names its pane instead"
  (let* ((b (current-board)) (place (and b (board-place b *client*))))
    (when place
      (let ((window (find (second place) (windows-of *client* (first place)) :key #'first)))
        (prompt-window-name *client* (first place) (second place) (and window (second window)))))))

(defcommand (switchboard-rename :unlisted)
  "name the cursor's session"
  (let* ((b (current-board)) (place (and b (board-place b *client*))))
    (when place (prompt-session-name *client* (first place)))))

(defmethod overlay-session-renamed ((b board) old new)
  "The cursor, the lane slid, the folds and where it opened follow the name."
  (flet ((renamed (name) (if (equal name old) new name)))
    (when (board-cursor b) (setf (first (board-cursor b)) (renamed (first (board-cursor b)))))
    (setf (board-starting b) (renamed (board-starting b))
          (board-folded b) (mapcar #'renamed (board-folded b))
          (board-picked b) (mapcar (lambda (k) (cons (renamed (car k)) (cdr k))) (board-picked b)))
    (let ((slid (gethash old (board-offsets b))))
      (when slid
        (remhash old (board-offsets b))
        (setf (gethash new (board-offsets b)) slid)))))

(defcommand (switchboard-next-blocked :unlisted)
  "the cursor to whatever has been asking longest, in any session"
  (let* ((b (current-board))
         (asking (and b (remove-if-not (lambda (r) (getf r :asks)) (board-rows b *client*))))
         (oldest (first (sort (copy-list asking) #'> :key (lambda (r) (or (getf r :for) 0))))))
    (when oldest
      (board-goto b *client* (getf oldest :session) (getf oldest :window) (getf oldest :id)))))

(defcommand (switchboard-explain :unlisted)
  "why the cursor's pane is what it is, and who typed into it"
  (let* ((b (current-board)) (row (and b (board-row b *client*))))
    (when row (open-drawer *client* (row-key row)))))

(defcommand (switchboard-close-pane :unlisted)
  "close the cursor's pane, after a yes"
  (let* ((b (current-board)) (row (and b (board-row b *client*))))
    (when row
      (let ((session (getf row :session)) (id (getf row :id)))
        (confirm *client* (format nil "close ~A and what runs in it?" (row-path row))
                 :yes (lambda (c)
                        (let ((*client* c)) (send-to-server (list :close-pane session id)))))))))

(defcommand (switchboard-filter :unlisted)
  "narrow the board to what matches"
  (let ((b (current-board)))
    (when b
      (setf (board-filtering b) t)
      (current-client-mode *client*))))

(defcommand (switchboard-close :unlisted)
  (let ((b (current-board)))
    (when b (close-board b *client*))))

(defcommand (switchboard-typed :unlisted)
  ;; RET while typing: a prompt goes to its targets, a filter is kept
  (let ((b (current-board)))
    (when b
      (cond
        ((board-composing b)
         (let ((text (board-composing b)))
           (when (plusp (length text))
             (dolist (row (board-targets b *client*))
               (send-to-server (list :prompt-when-idle (getf row :session) (getf row :id) text))))
           (setf (board-composing b) nil
                 (board-picked b) nil)))
        (t (setf (board-filtering b) nil)))
      (current-client-mode *client*))))

(defcommand (switchboard-cancel-typing :unlisted)
  ;; Esc while typing: the prompt or the filter is dropped
  (let ((b (current-board)))
    (when b
      (if (board-composing b)
          (setf (board-composing b) nil)
          (setf (board-filtering b) nil (board-query b) ""))
      (current-client-mode *client*))))

(defcommand (switchboard-delete-backward :unlisted)
  (let ((b (current-board)))
    (when b
      (flet ((less (s) (subseq s 0 (max 0 (1- (length s))))))
        (if (board-composing b)
            (setf (board-composing b) (less (board-composing b)))
            (setf (board-query b) (less (board-query b))))))))

(defun rail-clicked (r pos &key (step 3))
  "Where a rail R is after a click at POS along it: an arrow steps, the track
pages, the thumb stays where it is to be dragged; never past either end."
  (let ((most (max 0 (- (rail-total-of r) (rail-extent-of r)))))
    (min most
         (case (rail-part r pos)
           (:up (max 0 (- (rail-at-of r) step)))
           (:down (+ (rail-at-of r) step))
           (:above (max 0 (- (rail-at-of r) (rail-extent-of r))))
           (:below (+ (rail-at-of r) (rail-extent-of r)))
           (t (rail-at-of r))))))

(defun board-hit (b col line)
  "What is under a click at COL, LINE: (:runs form) for a button, (:rail-h
session at) or (:rail-v at) for a rail, with the rail and how far along its
thumb was taken hold of when it was the thumb, else nil."
  (dolist (region (board-regions b))
    (case (first region)
      (:side nil)
      (:tree
       (let ((hit (button-at (second region) line col)))
         (when hit (return (list :runs (bar-button-runs hit))))))
      (:sheet
       (destructuring-bind (tree left top x0 y0 width height) (rest region)
         (when (and (<= left col) (< col (+ left width)) (<= top line) (< line (+ top height)))
           (let ((hit (button-at tree (+ y0 (- line top)) (+ x0 (- col left)))))
             (when hit (return (list :runs (bar-button-runs hit))))))))
      (:rail-h
       (destructuring-bind (r session left line-at width) (rest region)
         (when (and (= line line-at) (<= left col) (< col (+ left width)))
           (return (list :rail-h session (rail-clicked r col :step 4)
                         (and (eq :thumb (rail-part r col)) (list r (rail-grabbed r col))))))))
      (:rail-v
       (destructuring-bind (r col-at top height) (rest region)
         (when (and (= col col-at) (<= top line) (< line (+ top height)))
           (return (list :rail-v (rail-clicked r line)
                         (and (eq :thumb (rail-part r line)) (list r (rail-grabbed r line)))))))))))

(defun rail-grabbed (r pos)
  "How far along the thumb of R the cell at POS is."
  (multiple-value-bind (from track) (rail-track r)
    (let ((top (rail-thumb track (rail-extent-of r) (rail-total-of r) (rail-at-of r))))
      (max 0 (- pos from top)))))

(defun board-handle-button (b runs client)
  "What a button on the board means."
  (destructuring-bind (what &rest it) runs
    (case what
      (:cursor
       (destructuring-bind (session &optional n) it
         (let ((place (board-place b client)))
           (if (and place (equal session (first place)) (or (null n) (eql n (second place))))
               (let ((*client* client)) (switchboard-goto))
               (board-goto b client session (or n 1))))))
      (:option (let ((*client* client)) (send-to-server (list :answer (car (first it)) (cdr (first it)) (second it)))))
      (:go (close-board b client) (let ((*client* client)) (send-to-server (list :go (first it)))))
      (:zoom (board-zoom-to b client (first it)))
      (:side (setf (board-side b) (not (board-side b)) (client-dirty client) t))
      (:sort (let ((*client* client)) (switchboard-sort)))
      (:toggle (let ((*client* client))
                 (ecase (first it) (:fold-quiet (switchboard-fold-quiet)) (:live (switchboard-toggle-live)))))
      (:filter (let ((*client* client)) (switchboard-filter)))
      (:new-window (let ((*client* client))
                     (send-to-server runs)
                     (setf (board-requested-at b) 0)))
      (t (handle-button b runs client)))))

(defcommand (switchboard-click :unlisted)
  "a click on a card or a row puts the cursor there, and on the cursor goes
there; on an answer, answers; on a control, does what it says; on a rail,
slides"
  (let* ((b (current-board))
         (hit (and b *mouse-position* (board-hit b (car *mouse-position*) (cdr *mouse-position*)))))
    (setf (board-held b) nil)
    (when hit
      (ecase (first hit)
        (:runs (board-handle-button b (second hit) *client*))
        (:rail-h (setf (gethash (second hit) (board-offsets b)) (max 0 (third hit))
                       (board-held b) (and (fourth hit) (list* :rail-h (second hit) (fourth hit)))
                       (board-following b) nil (client-dirty *client*) t))
        (:rail-v (setf (board-scroll-y b) (max 0 (second hit))
                       (board-held b) (and (third hit) (cons :rail-v (third hit)))
                       (board-following b) nil (client-dirty *client*) t))))))

(defcommand (switchboard-drag :unlisted)
  "a thumb taken hold of goes where the pointer goes"
  (let* ((b (current-board)) (held (and b (board-held b))))
    (when (and held *mouse-position*)
      (ecase (first held)
        (:rail-h (destructuring-bind (session r grabbed) (rest held)
                   (setf (gethash session (board-offsets b)) (rail-at r (car *mouse-position*) grabbed))))
        (:rail-v (destructuring-bind (r grabbed) (rest held)
                   (setf (board-scroll-y b) (rail-at r (cdr *mouse-position*) grabbed)))))
      (setf (board-following b) nil (client-dirty *client*) t))))

(defcommand (switchboard-release :unlisted)
  (let ((b (current-board))) (when b (setf (board-held b) nil))))

(defcommand (switchboard-ignore :unlisted) nil)

(defun board-scroll (b client dy)
  (setf (board-scroll-y b) (max 0 (+ (board-scroll-y b) dy))
        (board-following b) nil
        (client-dirty client) t))

(defun lane-at-line (b client line)
  "The session whose lane is on screen row LINE, or nil."
  (let ((sessions (board-sessions b client)))
    (loop :for (s . top) :in (lane-tops b client sessions)
          :for y := (+ (board-area-top b) (- top (board-scroll-y b)))
          :when (and (<= y line) (< line (+ y (lane-height b client s))))
            :return s)))

(defun board-slide (b client dx)
  "Slide the lane under the pointer, or the cursor's when the pointer is on none."
  (let ((session (or (and *mouse-position* (lane-at-line b client (cdr *mouse-position*)))
                     (first (board-place b client)))))
    (when session
      (setf (gethash session (board-offsets b))
            (max 0 (+ (gethash session (board-offsets b) 0) dx))
            (board-following b) nil
            (client-dirty client) t))))

(defun pointer-on-side-p (b)
  "Whether the pointer is over the side pane."
  (let ((side (find :side (board-regions b) :key #'first)))
    (and side *mouse-position*
         (destructuring-bind (left top width height) (rest side)
           (and (<= left (car *mouse-position*)) (< (car *mouse-position*) (+ left width))
                (<= top (cdr *mouse-position*)) (< (cdr *mouse-position*) (+ top height)))))))

(defun board-wheel (b client dy)
  "The wheel over the side pane scrolls the activity log; anywhere else, the lanes."
  (if (pointer-on-side-p b)
      (setf (board-log-scroll-y b) (max 0 (+ (board-log-scroll-y b) dy))
            (client-dirty client) t)
      (board-scroll b client dy)))

(defcommand (switchboard-wheel-up :unlisted)
  (let ((b (current-board))) (when b (board-wheel b *client* -3))))

(defcommand (switchboard-wheel-down :unlisted)
  (let ((b (current-board))) (when b (board-wheel b *client* 3))))

(defcommand (switchboard-wheel-left :unlisted)
  (let ((b (current-board))) (when b (board-slide b *client* -4))))

(defcommand (switchboard-wheel-right :unlisted)
  (let ((b (current-board))) (when b (board-slide b *client* 4))))

(defcommand (switchboard-page-up :unlisted)
  (let ((b (current-board))) (when b (board-scroll b *client* (- (board-height *client*))))))

(defcommand (switchboard-page-down :unlisted)
  (let ((b (current-board))) (when b (board-scroll b *client* (board-height *client*)))))

(atty/mode:define-key 'board-mode "Left"   #'switchboard-left)

(atty/mode:define-key 'board-mode "Right"  #'switchboard-right)

(atty/mode:define-key 'board-mode "Up"     #'switchboard-up)

(atty/mode:define-key 'board-mode "Down"   #'switchboard-down)

(atty/mode:define-key 'board-mode "Home"   #'switchboard-first-window)

(atty/mode:define-key 'board-mode "End"    #'switchboard-last-window)

(atty/mode:define-key 'board-mode "TAB"    #'switchboard-fold)

(atty/mode:define-key 'board-mode "o"      #'switchboard-next-pane)

(atty/mode:define-key 'board-mode "SPC"    #'switchboard-select)

(atty/mode:define-key 'board-mode "RET"    #'switchboard-goto)

(atty/mode:define-key 'board-mode "+"      #'switchboard-zoom-in)

(atty/mode:define-key 'board-mode "="      #'switchboard-zoom-in)

(atty/mode:define-key 'board-mode "-"      #'switchboard-zoom-out)

(atty/mode:define-key 'board-mode "\\"     #'switchboard-toggle-side)

(atty/mode:define-key 'board-mode "c"      #'switchboard-new-window)

(atty/mode:define-key 'board-mode "C"      #'switchboard-new-session)

(atty/mode:define-key 'board-mode "p"      #'switchboard-prompt)

(atty/mode:define-key 'board-mode "n"      #'switchboard-next-blocked)

(atty/mode:define-key 'board-mode "r"      #'switchboard-rename)

(atty/mode:define-key 'board-mode ","      #'switchboard-name)

(atty/mode:define-key 'board-mode "e"      #'switchboard-explain)

(atty/mode:define-key 'board-mode "x"      #'switchboard-close-pane)

(atty/mode:define-key 'board-mode "s"      #'switchboard-sort)

(atty/mode:define-key 'board-mode "f"      #'switchboard-fold-quiet)

(atty/mode:define-key 'board-mode "l"      #'switchboard-toggle-live)

(atty/mode:define-key 'board-mode "/"      #'switchboard-filter)

(atty/mode:define-key 'board-mode "?"      "describe mode")

(atty/mode:define-key 'board-mode "Escape" #'switchboard-close)

(atty/mode:define-key 'board-mode "C-g"    #'switchboard-close)

(atty/mode:define-key 'board-mode "mouse-1" #'switchboard-click)

(atty/mode:define-key 'board-mode "mouse-1-up" #'switchboard-release)

(atty/mode:define-key 'board-mode "mouse-1-drag" #'switchboard-drag)

(atty/mode:define-key 'board-mode "wheel-left"  #'switchboard-wheel-left)

(atty/mode:define-key 'board-mode "wheel-right" #'switchboard-wheel-right)

(atty/mode:define-key 'board-mode "PageUp" #'switchboard-page-up)

(atty/mode:define-key 'board-mode "PageDown" #'switchboard-page-down)

(atty/mode:define-key 'board-mode "wheel-up"   #'switchboard-wheel-up)

(atty/mode:define-key 'board-mode "wheel-down" #'switchboard-wheel-down)

(atty/mode:define-key 'board-mode "S-wheel-up"   #'switchboard-wheel-left)

(atty/mode:define-key 'board-mode "S-wheel-down" #'switchboard-wheel-right)

(atty/mode:define-key 'board-type-mode "RET"    #'switchboard-typed)

(atty/mode:define-key 'board-type-mode "DEL"    #'switchboard-delete-backward)

(atty/mode:define-key 'board-type-mode "Escape" #'switchboard-cancel-typing)

(atty/mode:define-key 'board-type-mode "C-g"    #'switchboard-cancel-typing)

(defun open-board (client &key (session (client-session client)))
  "Put the switchboard over CLIENT's session, the cursor on where it is
looking once there are rows."
  (subscribe-panes client)
  (client-push-overlay client (%make-board :starting session)))

(defcommand (switchboard :group sessions)
  "the whole server: every session, window and pane, and who is looking"
  (open-board *client*))
