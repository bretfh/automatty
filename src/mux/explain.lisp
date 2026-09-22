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
  (target nil)                          ; (session . id) to open on, else the focus
  (key nil)
  (asked 0 :type integer)
  (folded nil :type list)
  (laid nil))

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
                        (list :pane-about session id)
                        (list :pulses)))
      (tell-the-server-if-it-knows form))))

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
                                                     (:client :here)
                                                     (t :quiet)))
                              (atty/ui:label (format nil "~(~A~)" verb))
                              (atty/ui:label (cond ((eq verb :keys) (format nil " ~D bytes" summary))
                                                   (summary (format nil " ~A"
                                                                    (shortened-to (princ-to-string summary) 30)))
                                                   (t "")))
                              (atty/ui:label (if (eq outcome :refused) " refused" "")
                                             :face :error))))

(defun section-head (d name what)
  "A section's heading, a button that folds it: ▾ open, ▸ folded."
  (bar-button (list :fold what)
              (atty/ui:label (format nil " ~A ~A" (if (member what (drawer-folded d)) "▸" "▾") (string-upcase name))
                             :face :quiet)))

(defun drawer-tree (d client key row width)
  (let ((now (ms-here)))
    (multiple-value-bind (explained) (about client key :agent-explained)
      (multiple-value-bind (about) (about client key :pane-about)
        (multiple-value-bind (history history-at) (about client key :pane-history)
          (declare (ignore history history-at))
          (multiple-value-bind (log) (about client key :pane-log)
            (let* ((about (first about))
                   (state (and (getf row :known) (getf row :state)))
                   (rows (third explained))
                   (for (row-for row now))
                   (asks (and (eq state :blocked) (getf row :asks)))
                   (size (getf about :size))
                   (name (or (getf row :label) (getf row :says) (cdr key)))
                   (session (car key)) (id (cdr key)))
              (flet ((open-p (what) (not (member what (drawer-folded d)))))
                (apply #'atty/ui:column :align :stretch :expand 1
                       :background-color (bar-face :bg-dim)
                       :min-width width
                       (append
                        (list (header-band (if state
                                               (format nil "why is ~A ~A?" name (if (eq state :blocked) "asking" (string-downcase state)))
                                               (format nil "what is ~A?" name))
                                           :right (atty/ui:label (format nil " ~A " (key-in 'pane-mode 'explain-this-pane)) :face :quiet))
                              (atty/ui:row :spacing 0 (atty/ui:label " ")
                                           (path '(:quiet "default") (getf row :session)
                                                 (and (getf row :window)
                                                      (format nil "~D~@[ ~A~]" (getf row :window)
                                                              (let ((w (getf row :window-name))) (and w (plusp (length w)) w))))
                                                 (format nil "~@[~D ~]~A" (and (getf row :at) (1+ (getf row :at))) name)
                                                 (list :quiet (format nil "› ~A" (shortened-to (or (getf about :command) "") 24)))))
                              (toolbar (keycap "z" "zoom" :runs "zoom this pane")
                                       (keycap "r" "read" :runs (list :pane-read session id))
                                       (keycap "n" "name" :runs "name this pane")
                                       (keycap "x" "close" :runs (list :confirm-close session id)))
                              (section-head d "what it is" :what))
                        (when (open-p :what)
                          (list (atty/ui:row :spacing 0
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
                                               :face :quiet)))
                        (list (section-head d "state" :state))
                        (when (open-p :state)
                          (list (if (null state)
                                    (atty/ui:label "  not read: no reader knows this program" :face :quiet)
                                    (atty/ui:row :spacing 0
                                                 (atty/ui:label "  ")
                                                 (atty/ui:label (format nil "~A ~A" (state-glyph state)
                                                                        (if (eq state :blocked) "asking" (string-downcase state)))
                                                                :face (if (eq state :blocked)
                                                                          :state-blocked-strong
                                                                          (state-face state)))
                                                 (atty/ui:label (format nil " for ~A" (duration for)))
                                                 (atty/ui:label (if (getf row :since-clock)
                                                                    (format nil " · since ~A"
                                                                            (wall-clock (getf row :since-clock)))
                                                                    "")
                                                                :face :quiet)))))
                        (when (and state (open-p :state))
                          (list (atty/ui:row :spacing 0 (atty/ui:label "  ")
                                             (spark (pane-cells client (car key) (cdr key)))
                                             (atty/ui:label "  how much it wrote, in the colour of what it was" :face :quiet))
                                (atty/ui:row :spacing 0 (atty/ui:label "  ")
                                             (atty/ui:label "20m ago      now" :face :quiet))))
                        ;; the reading only for an agent that is read; for
                        ;; anything else it would be about nothing
                        (when state
                          (cons (section-head d "how it was read · in the order tried" :read)
                                (when (open-p :read)
                                  (or (and rows (rule-lines rows))
                                      (list (atty/ui:label "  nothing read yet" :face :quiet))))))
                        (list (section-head d "who typed here" :who))
                        (when (open-p :who)
                          (or (who-typed-lines client (first log))
                              (list (atty/ui:label "  nobody yet" :face :quiet))))
                        (list (atty/ui:gap :expand 1))
                        ;; asking something: the answers, at the foot, to click or key
                        (list (band (list (atty/ui:label " ")
                                          (if asks
                                              (apply #'atty/ui:row :spacing 0
                                                     (append (option-labels row (getf asks :options) nil)
                                                             (list (atty/ui:label " answer from here" :face :quiet))))
                                              (atty/ui:label "")))
                                    (list (hint (key-in 'pane-mode 'explain-this-pane) "closes" :runs :close))))))))))))))

(defmethod draw-over ((d drawer) screen)
  (let* ((client *drawing-for*)
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (width (drawer-width cols))
         (row (or (and (drawer-target d)
                       (find (drawer-target d) (rows-of client) :key #'row-key :test #'equal))
                  (focused-row client)))
         (key (and row (row-key row))))
    (when key
      ;; asked again when the focus moves, and every so often about the same
      ;; pane, since the rules are read off a screen that goes on changing
      (when (or (not (equal key (drawer-key d)))
                (>= (- (ms-here) (drawer-asked d)) +asked-every+))
        (setf (drawer-key d) key
              (drawer-asked d) (ms-here))
        (let ((*client* client)) (drawer-ask client key)))
      (let ((tree (drawer-tree d client key row width)))
        (atty/cells:draw tree (tty:screen-grid screen)
                         cols rows :left (- cols width) :top (min 1 (max 0 (1- rows))))
        (setf (drawer-laid d) tree)))))

(defmethod laid-tree ((d drawer)) (drawer-laid d))

(defmethod clicked-over ((d drawer) line col client)
  "A button on the drawer: an answer at its foot, a section's fold, one of
its actions."
  (let* ((hit (and (drawer-laid d) (button-at (drawer-laid d) line col)))
         (runs (and hit (bar-button-runs hit))))
    (when hit
      (let ((*client* client))
        (case (and (consp runs) (first runs))
          (:answer (tell-the-server runs))
          (:fold (setf (drawer-folded d) (if (member (second runs) (drawer-folded d))
                                             (remove (second runs) (drawer-folded d))
                                             (cons (second runs) (drawer-folded d)))
                       (client-dirty client) t))
          (:confirm-close
           (destructuring-bind (session id) (rest runs)
             (confirm client (format nil "close ~A and what runs in it?" (row-path (focused-row client)))
                      :yes (lambda (c) (let ((*client* c)) (tell-the-server (list :close-pane session id)))))))
          (t (generic-click d runs client))))
      t)))

(defmethod ticks-p ((d drawer)) t)
(defmethod passes-keys-p ((d drawer)) t)
(defmethod over-name ((d drawer)) "why")

;;; Beside the panes, so typing goes on to the pane; but Escape, alone, and
;;; C-g close it, as they close everything else on top.

(atty/mode:define-mode drawer-mode (pane-mode))
(defmethod mode-of ((d drawer)) 'drawer-mode)

(defun close-the-drawer (client)
  (let ((open (find-if (lambda (it) (typep it 'drawer)) (client-over client))))
    (when open
      (client-over-drop client open)
      (stop-told client))))

(defmethod close-over ((d drawer) client) (close-the-drawer client))

(defcommand (close-drawer :unlisted)
  (close-the-drawer *client*))

(atty/mode:define-key 'drawer-mode "Escape" #'close-drawer)
(atty/mode:define-key 'drawer-mode "C-g" #'close-drawer)

(defun open-the-drawer (client &optional target)
  "The drawer over CLIENT's screen, on the pane TARGET names or the focus."
  (keep-told client)
  (client-over-put client (%make-drawer :target target)))

(defcommand (explain-this-pane :group agents)
  "why this pane is what it is, and who typed into it"
  (if (find-if (lambda (it) (typep it 'drawer)) (client-over *client*))
      (close-the-drawer *client*)
      (open-the-drawer *client*)))
