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
  (asked 0 :type integer)
  (laid nil))

(defun section (name)
  (atty/ui:label (format nil " ~A" (string-upcase name)) :face :quiet))

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
                              (atty/ui:label (format nil "~14A " (said-by client who))
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
                   (for (row-for row now))
                   (asks (and (eq state :blocked) (getf row :asks)))
                   (size (getf about :size)))
              (atty/ui:framed
               (apply #'atty/ui:column :align :stretch :expand 1
                      :background-color (bar-face :bg-dim)
                      :min-width (max 0 (- width 2))
                      (append
                       (list (atty/ui:row :spacing 0
                                          (atty/ui:label (format nil " ~A " (row-path row)) :face :strong)
                                          (atty/ui:label (format nil "> ~A" (shortened-to (or (getf about :command) "") 30))
                                                         :face :quiet))
                             (section "what it is")
                             (atty/ui:row :spacing 0
                                          (atty/ui:label "  ")
                                          (atty/ui:label (or (getf about :kind) (getf row :kind) "") :face :strong)
                                          (atty/ui:label (format nil "~@[ ~A~]" (getf about :version)))
                                          (atty/ui:label (if (getf about :reader)
                                                             (format nil " · read by the ~A reader" (getf about :reader))
                                                             " · no reader knows it")
                                                         :face :quiet))
                             (atty/ui:label (format nil "  ~@[pid ~D · ~]~@[~{~D×~D~} · ~]~A"
                                                    (getf about :pid) size
                                                    (or (getf about :directory) ""))
                                            :face :quiet)
                             (atty/ui:label (format nil "  in front: ~A~@[  group ~A~]"
                                                    (or (first (getf about :programs)) "nothing yet")
                                                    (getf about :group))
                                            :face :quiet)
                             (section "state")
                             (if (null state)
                                 (atty/ui:label "  not read: no reader knows this program" :face :quiet)
                             (atty/ui:row :spacing 0
                                          (atty/ui:label "  ")
                                          (atty/ui:label (format nil "~A ~(~A~)" (state-glyph state) state)
                                                         :face (if (eq state :blocked)
                                                                   :state-blocked-strong
                                                                   (state-face state)))
                                          (atty/ui:label (format nil " for ~A" (duration for)))
                                          (atty/ui:label (if (getf row :since-clock)
                                                             (format nil " · since ~A"
                                                                     (wall-clock (getf row :since-clock)))
                                                             "")
                                                         :face :quiet))))
                       ;; the last while and the reading only for an agent that
                       ;; is read; for anything else they would be about nothing
                       (when state
                         (append
                          (list (strip (list :history (first history) :heard-at (or history-at now))
                                       now)
                                (atty/ui:row :spacing 0
                                             (atty/ui:label "  ■" :face :state-idle)
                                             (atty/ui:label " idle  " :face :quiet)
                                             (atty/ui:label "■" :face :state-working)
                                             (atty/ui:label " working  " :face :quiet)
                                             (atty/ui:label "■" :face :state-blocked)
                                             (atty/ui:label " blocked  · the last 20 minutes" :face :quiet))
                                (section "how it was read · entries in the order tried"))
                          (or (and rows (rule-lines rows))
                              (list (atty/ui:label "  nothing read yet" :face :quiet)))))
                       (list (section "who typed here"))
                       (or (who-typed-lines client (first log))
                           (list (atty/ui:label "  nobody yet" :face :quiet)))
                       (list (atty/ui:gap :expand 1))
                       ;; asking something: the answers, at the foot, to click or key
                       (when asks
                         (list (apply #'atty/ui:row :spacing 0
                                      (atty/ui:label " ")
                                      (append (option-labels row (getf asks :options) nil)
                                              (list (atty/ui:label " answer from here" :face :quiet))))))))
               :face :state-unknown
               :titles (list :tl (atty/ui:label (if state
                                                    (format nil " why is ~A ~(~A~)? "
                                                            (or (getf row :label) (getf row :says) (cdr key)) state)
                                                    (format nil " what is ~A? "
                                                            (or (getf row :label) (getf row :says) (cdr key))))
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
      (let ((tree (drawer-tree client key row width)))
        (atty/cells:draw tree (tty:screen-grid screen)
                         cols rows :left (- cols width) :top (min 1 (max 0 (1- rows))))
        (setf (drawer-laid d) tree)))))

(defmethod clicked-over ((d drawer) line col client)
  "An answer at the drawer's foot, clicked."
  (let ((hit (and (drawer-laid d) (button-at (drawer-laid d) line col))))
    (when (and hit (consp (bar-button-runs hit)) (eq :answer (first (bar-button-runs hit))))
      (let ((*client* client))
        (tell-the-server (bar-button-runs hit)))
      t)))

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
