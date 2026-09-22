;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; Why it thinks so, and who typed: a drawer down the right of the screen about
;;; the pane with the focus. What decided its kind, what state it is in and
;;; since when, every rule that looked at its screen and which one won with the
;;; text it matched, how the last twenty minutes went, and who typed into it.
;;;
;;; It is beside the panes, not in front of them, so what is typed still goes to
;;; the pane; it follows the focus, and the key that opened it closes it.

(defparameter +drawer-most+ 60
  "The most columns the drawer takes; a third of the terminal when that is less.")

(defparameter +asked-every+ 1000
  "How often, in milliseconds, the drawer asks again about the pane it shows.")

(defstruct (drawer (:constructor %make-drawer))
  (key nil)
  (asked 0 :type integer))

(defun drawer-width (cols)
  (min +drawer-most+ (floor cols 3)))

(defun focused-row (client)
  "What the client has been told of the pane with the focus in its session."
  (find-if (lambda (r) (and (getf r :focus) (equal (getf r :session) (client-session client))))
           (rows-of client)))

(defun drawer-ask (client key)
  (let ((session (car key)) (id (cdr key)))
    (dolist (form (list (list :agent-explain session id)
                        (list :pane-history session id)
                        (list :pane-log session id 8)
                        (list :pane-about session id)))
      (tell-the-server form))))

(defun about (client key what)
  "What the server last said about KEY's pane as WHAT, and when it came."
  (let ((it (getf (gethash key (client-about client)) what)))
    (values (rest it) (first it))))

(defun wall-clock (clock)
  "The universal time CLOCK as the time of day, hh:mm:ss, or blank if there
is none."
  (if clock
      (multiple-value-bind (s m h) (decode-universal-time clock)
        (format nil "~2,'0D:~2,'0D:~2,'0D" h m s))
      "        "))

(defun seen-lines (seen)
  (remove nil
          (append (list (getf seen :question))
                  (loop :for option :in (getf seen :options)
                        :for n :from 1
                        :collect (format nil "~:[ ~;>~] ~D. ~A" (eql (1- n) (getf seen :selected)) n option))
                  (list (getf seen :label)
                        (getf seen :line)
                        (and (not (getf seen :options)) (getf seen :text))))))

(defun rule-lines (rows)
  (cons (atty/ui:label "  says    screen        widget" :face :quiet)
        (loop :for row :in rows
              :for (id widget means won seen) := row
              :append (cons (atty/ui:row :spacing 0
                                         (atty/ui:label (cond (won "* ") (seen "+ ") (t "  "))
                                                        :face (if won :state-blocked-strong :quiet))
                                         (atty/ui:label (format nil "~(~7A~) " means)
                                                        :face (if seen (state-face means) :quiet))
                                         (atty/ui:label (format nil "~(~13A~) " id)
                                                        :face (cond (won :strong) (seen :default) (t :quiet)))
                                         (atty/ui:label (format nil "~(~A~)" widget) :face :quiet))
                            (when won
                              (loop :for line :in (seen-lines seen)
                                    :repeat 3
                                    :collect (atty/ui:label (format nil "        │ ~A"
                                                                    (string-trim " " line)))))))))

(defun who-typed-lines (client log)
  (loop :for (nil who verb summary outcome clock) :in log
        :collect (atty/ui:row :spacing 0
                              (atty/ui:label (format nil "~A " (wall-clock clock))
                                             :face :quiet)
                              (atty/ui:label (format nil "~8A " (said-by client who))
                                             :face (case (first who)
                                                     (:pane :driven)
                                                     (:client :you)
                                                     (t :quiet)))
                              (atty/ui:label (format nil "~(~A~)" verb))
                              (atty/ui:label (cond ((eq verb :keys) (format nil " ~D bytes" summary))
                                                   (summary (format nil " ~A"
                                                                    (shortened-to (princ-to-string summary) 30)))
                                                   (t "")))
                              (atty/ui:label (if (eq outcome :refused) " refused" "")
                                             :face :error))))

(defun drawer-tree (client key row width)
  (let ((now (ms-here)))
    (multiple-value-bind (explained) (about client key :agent-explained)
      (multiple-value-bind (about) (about client key :pane-about)
        (multiple-value-bind (history history-at) (about client key :pane-history)
          (multiple-value-bind (log) (about client key :pane-log)
            (let* ((about (first about))
                   (state (and (getf row :known) (getf row :state)))
                   (rows (third explained))
                   (for (row-for row now)))
              (atty/ui:framed
               (apply #'atty/ui:column :align :stretch :expand 1
                      :background-color (bar-face :bg-dim)
                      :min-width (max 0 (- width 2))
                      (append
                       (list (atty/ui:row :spacing 0
                                          (atty/ui:label "kind    " :face :quiet)
                                          (atty/ui:label (or (getf about :kind) (getf row :kind) "")
                                                         :face :strong))
                             (atty/ui:label (format nil "        foreground ~A~@[  group ~A~]"
                                                    (or (first (getf about :programs)) "nothing yet")
                                                    (getf about :group)))
                             (atty/ui:label (format nil "        spawned as ~A"
                                                    (shortened-to (or (getf about :command) "") 40))
                                            :face :quiet)
                             (if (null state)
                                 (atty/ui:label "state   not read: no rules know this program"
                                                :face :quiet)
                             (atty/ui:row :spacing 0
                                          (atty/ui:label "state   " :face :quiet)
                                          (atty/ui:label (format nil "~A ~(~A~)" (state-glyph state) state)
                                                         :face (if (eq state :blocked)
                                                                   :state-blocked-strong
                                                                   (state-face state)))
                                          (atty/ui:label (format nil " for ~A" (duration for)))
                                          (atty/ui:label (if (getf row :since-clock)
                                                             (format nil " · since ~A"
                                                                     (wall-clock (getf row :since-clock)))
                                                             "")
                                                         :face :quiet)))
                             (atty/ui:label ""))
                       ;; the rules and the last while only for an agent that is
                       ;; read; for anything else they would be about nothing
                       (when state
                         (append
                          (and rows (rule-lines rows))
                          (list (atty/ui:label "")
                                (atty/ui:label "last 20 minutes" :face :quiet)
                                (strip (list :history (first history) :heard-at (or history-at now))
                                       now)
                                (atty/ui:row :spacing 0
                                             (atty/ui:label "■" :face :state-idle)
                                             (atty/ui:label " idle  " :face :quiet)
                                             (atty/ui:label "■" :face :state-working)
                                             (atty/ui:label " working  " :face :quiet)
                                             (atty/ui:label "■" :face :state-blocked)
                                             (atty/ui:label " blocked" :face :quiet))
                                (atty/ui:label ""))))
                       (list (atty/ui:label "who typed here" :face :quiet))
                       (or (who-typed-lines client (first log))
                           (list (atty/ui:label "  nobody yet" :face :quiet)))
                       (list (atty/ui:gap :expand 1))))
               :face :state-unknown
               :titles (list :tl (atty/ui:label (if state
                                                    (format nil " why is ~A:~D ~(~A~)? "
                                                            (car key) (cdr key) state)
                                                    (format nil " what is ~A:~D? "
                                                            (car key) (cdr key)))
                                                :face :strong)
                             :tr (atty/ui:label (format nil " ~A " (key-in 'pane-mode 'explain-this-pane))
                                                :face :quiet))))))))))

(defmethod draw-over ((d drawer) screen)
  (let* ((client *drawing-for*)
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (width (drawer-width cols))
         (row (focused-row client))
         (key (and row (row-key row))))
    (when key
      ;; asked again when the focus moves, and every so often about the same
      ;; pane, since the rules are read off a screen that goes on changing
      (when (or (not (equal key (drawer-key d)))
                (>= (- (ms-here) (drawer-asked d)) +asked-every+))
        (setf (drawer-key d) key
              (drawer-asked d) (ms-here))
        (let ((*client* client)) (drawer-ask client key)))
      (atty/cells:draw (drawer-tree client key row width) (tty:screen-grid screen)
                       cols rows :left (- cols width) :top (min 1 (max 0 (1- rows)))))))

(defmethod ticks-p ((d drawer)) t)
(defmethod passes-keys-p ((d drawer)) t)
(defmethod mode-of ((d drawer)) 'pane-mode)

(defcommand (explain-this-pane :group agents)
  "why this pane is what it is, and who typed into it"
  (let ((open (find-if (lambda (it) (typep it 'drawer)) (client-over *client*))))
    (if open
        (progn (client-over-drop *client* open)
               (stop-told *client*))
        (progn (keep-told *client*)
               (client-over-put *client* (%make-drawer))))))
