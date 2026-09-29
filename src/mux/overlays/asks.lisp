;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What one pane is asking, opened from where it is said to ask: its session
;;; on the rail, or what needs you on the bar. Its answers are keys; going
;;; there is RET, or the same click again.

(defstruct (asks (:constructor %make-asks) (:conc-name asks-of-))
           (session "" :type string)
           (pane 0)
           (laid nil))

(defun asks-pane (a)
  (let ((session (session-named (here-server) (asks-of-session a))))
    (and session
         (values (find (asks-of-pane a) (session-panes session) :key #'pane-id) session))))

(defun asks-tree (a width)
  (multiple-value-bind (pane session) (asks-pane a)
    (let* ((agent (and pane (pane-agent pane)))
           (asks (and agent (agent:agent-asks agent (pane-term pane))))
           (options (getf asks :options)))
      (sheet :asks
             (atty/ui:row :spacing 0
                          (atty/ui:label " ▲ " :face :state-blocked-strong)
                          (path (asks-of-session a)
                                (and pane (format nil "~D ~A" (or (pane-number session pane) (pane-id pane))
                                                  (pane-display-name pane))))
                          (atty/ui:label (format nil "  asking ~A"
                                                 (if agent (format-duration (agent:agent-for agent (now-ms))) ""))
                                         :face :state-blocked-strong))
             (list (atty/ui:row :spacing 0
                                (atty/ui:label (format nil " ~A" (or (getf asks :subject) "")) :face :state-blocked-strong)
                                (atty/ui:label (format nil "  ~A" (truncate-string (or (first (getf asks :detail))
                                                                                       (getf asks :question) "")
                                                                                   (max 1 (- width 20))))))
                   (atty/ui:label "")
                   (if options
                       (atty/ui:row :spacing 0 (atty/ui:label " ")
                                    (option-buttons session pane options (- width 4)))
                       (atty/ui:label " nothing to answer here now" :face :quiet)))
             :hints (atty/ui:row :spacing 0
                                 (hint (format nil "1-~D" (max 1 (length options))) "answer")
                                 (hint "↵" "go there" :runs :go)
                                 (hint "Esc" "close" :runs :close))
             :width width :height 7))))

(defparameter +asks-width+ 46 "How wide the sheet of what a pane asks is.")

(defmethod draw-overlay ((a asks) screen)
           (let* ((watcher *client*)
                  (width (max 1 (min (- (tty:screen-width screen) (chrome-left watcher) 4)
                                     +asks-width+)))
                  (tree (asks-tree a width)))
             (draw-sheet tree watcher :asks screen width 7)
             (setf (asks-of-laid a) tree
                   (tty:screen-cursor-visible screen) nil)))

(defmethod overlay-laid-tree ((a asks)) (asks-of-laid a))

(atty/mode:define-mode asks-mode ())

(defmethod mode-of ((a asks)) 'asks-mode)
(defmethod overlay-name ((a asks)) "asking")
(defmethod overlay-ticks-p ((a asks)) t)

(defun open-asks (watcher session pane)
  (push-overlay watcher (%make-asks :session (session-name session) :pane (pane-id pane))))

(defun asks-or-go (watcher session pane)
  "Open what PANE asks, or go there when it is what is open already."
  (let ((top (first (watcher-overlays watcher))))
    (if (and (asks-p top) (equal (asks-of-session top) (session-name session))
             (eql (asks-of-pane top) (pane-id pane)))
        (asks-go top watcher)
        (progn (when (asks-p top) (pop-overlay watcher top))
               (open-asks watcher session pane)))))

(defun asks-go (a watcher)
  (pop-overlay watcher a)
  (focus-pane (session-server (watcher-session watcher)) watcher (asks-of-session a) (asks-of-pane a)))

(defun current-asks ()
  (let ((it (first (watcher-overlays *client*))))
    (when (asks-p it) it)))

(defmethod overlay-unbound-key ((a asks) chord watcher)
           (let ((said (atty/mode:self-inserting chord)))
             (when (and said (= 1 (length said)) (digit-char-p (char said 0))
                        (plusp (digit-char-p (char said 0))))
               (when (eq t (answer-pane (session-server (watcher-session watcher)) watcher
                                        (asks-of-session a) (asks-of-pane a) (digit-char-p (char said 0))))
                 (pop-overlay watcher a))
               (setf (watcher-behind watcher) t)
               t)))

(defcommand (asks-accept :unlisted) ()
            (let ((a (current-asks))) (when a (asks-go a *client*))))

(defcommand (asks-close :unlisted) ()
            (let ((a (current-asks))) (when a (pop-overlay *client* a))))

(defcommand (asks-click :unlisted) ()
            (let* ((a (current-asks))
                   (hit (and a (asks-of-laid a) *mouse-position*
                             (button-at (asks-of-laid a) (cdr *mouse-position*) (car *mouse-position*)))))
              (cond ((and hit (eq :go (bar-button-runs hit))) (asks-go a *client*))
                    (hit (handle-button a (bar-button-runs hit) *client*))
                    (a (handle-click (watcher-session *client*) *client*
                                     (car *mouse-position*) (cdr *mouse-position*))))))

(defcommand (asks-ignore :unlisted) () nil)

(atty/mode:define-key 'asks-mode "RET"        #'asks-accept)
(atty/mode:define-key 'asks-mode "Escape"     #'asks-close)
(atty/mode:define-key 'asks-mode "C-g"        #'asks-close)
(atty/mode:define-key 'asks-mode "mouse-1"    #'asks-click)
(atty/mode:define-key 'asks-mode "mouse-1-up" #'asks-ignore)
