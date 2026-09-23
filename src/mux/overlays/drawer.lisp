;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; Why it thinks so, and who typed: a drawer down the right of the screen about
;;; the pane with the focus. What decided its kind, what state it is in and
;;; since when, every rule that looked at its screen and which one won with the
;;; text it matched, how the last twenty minutes went, and who typed into it.
;;;
;;; It is beside the panes, not in front of them, so what is typed still goes to
;;; the pane; it follows the focus, and the key that opened it closes it.

(defparameter +drawer-max-width+ 60
  "The most columns the drawer takes; a third of the terminal when that is less.")

(defparameter +drawer-poll-interval+ 1000
  "How often, in milliseconds, the drawer asks again about the pane it shows.")

(defstruct (drawer (:constructor %make-drawer))
  (target nil)                          ; (session . id) to open on, else the focus
  (key nil)
  (requested-at 0 :type integer)
  (folded nil :type list)
  (laid nil))

(defun drawer-width (cols)
  (min +drawer-max-width+ (floor cols 3)))

(defun focused-row (client)
  "What the client has been told of the pane with the focus in its session."
  (find-if (lambda (r) (and (getf r :focus) (equal (getf r :session) (client-session client))))
           (client-pane-rows client)))

(defun drawer-request (client key)
  (let ((session (car key)) (id (cdr key)))
    (dolist (form (list (list :agent-explain session id)
                        (list :pane-history session id)
                        (list :pane-log session id 8)
                        (list :pane-about session id)
                        (list :pulses)))
      (send-if-supported form))))

(defun drawer-data (client key what)
  "What the server last said about KEY's pane as WHAT, and when it came."
  (let ((it (getf (gethash key (client-pane-info client)) what)))
    (values (rest it) (first it))))

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

(defun log-lines (client log)
  (loop :for (nil actor verb summary outcome clock) :in log
        :collect (atty/ui:row :spacing 0
                              (atty/ui:label (format nil "~A " (format-clock-hms clock))
                                             :face :quiet)
                              (atty/ui:label (format nil "~14A " (log-actor-text client actor))
                                             :face (case (first actor)
                                                     (:pane :driven)
                                                     (:client :here)
                                                     (t :quiet)))
                              (atty/ui:label (format nil "~(~A~)" verb))
                              (atty/ui:label (cond ((eq verb :keys) (format nil " ~D bytes" summary))
                                                   (summary (format nil " ~A"
                                                                    (truncate-string (princ-to-string summary) 30)))
                                                   (t "")))
                              (atty/ui:label (if (eq outcome :refused) " refused" "")
                                             :face :error))))

(defun section-head (d name what)
  "A section's heading, a button that folds it: ▾ open, ▸ folded."
  (bar-button (list :fold what)
              (atty/ui:label (format nil " ~A ~A" (if (member what (drawer-folded d)) "▸" "▾") (string-upcase name))
                             :face :quiet)))

(defun drawer-context (client key)
  "What the server last said about KEY's pane: what explained its state, what
it is, and its log."
  (values (drawer-data client key :agent-explained)
          (first (drawer-data client key :pane-about))
          (first (drawer-data client key :pane-log))))

(defun identity-section (info row)
  "What the pane is: its kind and version and who reads it, its pid, size and
directory, and the program in front."
  (list (atty/ui:row :spacing 0
                     (atty/ui:label "  ")
                     (atty/ui:label (or (getf info :kind) (getf row :kind) "") :face :strong)
                     (atty/ui:label (format nil "~@[ ~A~]" (getf info :version)))
                     (atty/ui:label (if (getf info :reader)
                                        (format nil " · read by the ~A reader" (getf info :reader))
                                        " · no reader knows it")
                                    :face :quiet))
        (atty/ui:label (format nil "  ~@[pid ~D · ~]~@[~{~D×~D~} · ~]~A"
                               (getf info :pid) (getf info :size)
                               (or (getf info :directory) ""))
                       :face :quiet)
        (atty/ui:label (format nil "  in front: ~A~@[  group ~A~]"
                               (or (first (getf info :programs)) "nothing yet")
                               (getf info :group))
                       :face :quiet)))

(defun state-section (client key row state now)
  "What state the pane is in and for how long, and its sparkline when it is read."
  (append
   (list (if (null state)
             (atty/ui:label "  not read: no reader knows this program" :face :quiet)
             (atty/ui:row :spacing 0
                          (atty/ui:label "  ")
                          (atty/ui:label (format nil "~A ~A" (state-glyph state)
                                                 (if (eq state :blocked) "asking" (string-downcase state)))
                                         :face (if (eq state :blocked)
                                                   :state-blocked-strong
                                                   (state-face state)))
                          (atty/ui:label (format nil " for ~A" (format-duration (row-duration row now))))
                          (atty/ui:label (if (getf row :since-clock)
                                             (format nil " · since ~A"
                                                     (format-clock-hms (getf row :since-clock)))
                                             "")
                                         :face :quiet))))
   (when state
     (list (atty/ui:row :spacing 0 (atty/ui:label "  ")
                        (spark (pane-cells client (car key) (cdr key)))
                        (atty/ui:label "  how much it wrote, in the colour of what it was" :face :quiet))
           (atty/ui:row :spacing 0 (atty/ui:label "  ")
                        (atty/ui:label "20m ago      now" :face :quiet))))))

(defun answer-bar (row asks)
  "The foot: the answers to what the pane is asking, to click or key, and the
key that closes the drawer."
  (band (list (atty/ui:label " ")
              (if asks
                  (apply #'atty/ui:row :spacing 0
                         (append (option-labels row (getf asks :options) nil)
                                 (list (atty/ui:label " answer from here" :face :quiet))))
                  (atty/ui:label "")))
        (list (hint (key-hint 'pane-mode 'explain-pane) "closes" :runs :close))))

(defun drawer-tree (d client key row width)
  (multiple-value-bind (explained info log) (drawer-context client key)
    (let* ((now (client-ms))
           (state (and (getf row :known) (getf row :state)))
           (rows (third explained))
           (asks (and (eq state :blocked) (getf row :asks)))
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
                                   :right (atty/ui:label (format nil " ~A " (key-hint 'pane-mode 'explain-pane)) :face :quiet))
                      (atty/ui:row :spacing 0 (atty/ui:label " ")
                                   (path '(:quiet "default") (getf row :session)
                                         (and (getf row :window)
                                              (format nil "~D~@[ ~A~]" (getf row :window)
                                                      (let ((w (getf row :window-name))) (and w (plusp (length w)) w))))
                                         (format nil "~@[~D ~]~A" (and (getf row :at) (1+ (getf row :at))) name)
                                         (list :quiet (format nil "› ~A" (truncate-string (or (getf info :command) "") 24)))))
                      (toolbar (keycap "z" "zoom" :runs "zoom pane")
                               (keycap "r" "read" :runs (list :pane-read session id))
                               (keycap "n" "name" :runs "rename pane")
                               (keycap "x" "close" :runs (list :confirm-close session id)))
                      (section-head d "what it is" :what))
                (when (open-p :what) (identity-section info row))
                (list (section-head d "state" :state))
                (when (open-p :state) (state-section client key row state now))
                ;; the reading only for an agent that is read; for
                ;; anything else it would be about nothing
                (when state
                  (cons (section-head d "how it was read · in the order tried" :read)
                        (when (open-p :read)
                          (or (and rows (rule-lines rows))
                              (list (atty/ui:label "  nothing read yet" :face :quiet))))))
                (list (section-head d "who typed here" :who))
                (when (open-p :who)
                  (or (log-lines client log)
                      (list (atty/ui:label "  nobody yet" :face :quiet))))
                (list (atty/ui:gap :expand 1))
                (list (answer-bar row asks))))))))

(defmethod draw-overlay ((d drawer) screen)
  (let* ((client *overlay-client*)
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (width (drawer-width cols))
         (row (or (and (drawer-target d)
                       (find (drawer-target d) (client-pane-rows client) :key #'row-key :test #'equal))
                  (focused-row client)))
         (key (and row (row-key row))))
    (when key
      ;; asked again when the focus moves, and every so often about the same
      ;; pane, since the rules are read off a screen that goes on changing
      (when (or (not (equal key (drawer-key d)))
                (>= (- (client-ms) (drawer-requested-at d)) +drawer-poll-interval+))
        (setf (drawer-key d) key
              (drawer-requested-at d) (client-ms))
        (let ((*client* client)) (drawer-request client key)))
      (let ((tree (drawer-tree d client key row width)))
        (atty/cells:draw tree (tty:screen-grid screen)
                         cols rows :left (- cols width) :top (min 1 (max 0 (1- rows))))
        (setf (drawer-laid d) tree)))))

(defmethod overlay-laid-tree ((d drawer)) (drawer-laid d))

(defmethod overlay-clicked ((d drawer) line col client)
  "A button on the drawer: an answer at its foot, a section's fold, one of
its actions."
  (let* ((hit (and (drawer-laid d) (button-at (drawer-laid d) line col)))
         (runs (and hit (bar-button-runs hit))))
    (when hit
      (let ((*client* client))
        (case (and (consp runs) (first runs))
          (:answer (send-to-server runs))
          (:fold (setf (drawer-folded d) (if (member (second runs) (drawer-folded d))
                                             (remove (second runs) (drawer-folded d))
                                             (cons (second runs) (drawer-folded d)))
                       (client-dirty client) t))
          (:confirm-close
           (destructuring-bind (session id) (rest runs)
             (confirm client (format nil "close ~A and what runs in it?" (row-path (focused-row client)))
                      :yes (lambda (c) (let ((*client* c)) (send-to-server (list :close-pane session id)))))))
          (t (handle-button d runs client))))
      t)))

(defmethod overlay-ticks-p ((d drawer)) t)
(defmethod overlay-passes-keys-p ((d drawer)) t)
(defmethod overlay-name ((d drawer)) "why")

;;; Beside the panes, so typing goes on to the pane; but Escape, alone, and
;;; C-g close it, as they close everything else on top.

(atty/mode:define-mode drawer-mode (pane-mode))
(defmethod mode-of ((d drawer)) 'drawer-mode)

(defun drawer-close (client)
  (let ((open (find-if (lambda (it) (typep it 'drawer)) (client-overlays client))))
    (when open
      (client-pop-overlay client open)
      (unsubscribe-panes client))))

(defmethod close-overlay ((d drawer) client) (drawer-close client))

(defcommand (close-drawer :unlisted)
  (drawer-close *client*))

(atty/mode:define-key 'drawer-mode "Escape" #'close-drawer)
(atty/mode:define-key 'drawer-mode "C-g" #'close-drawer)

(defun open-drawer (client &optional target)
  "The drawer over CLIENT's screen, on the pane TARGET names or the focus."
  (subscribe-panes client)
  (client-push-overlay client (%make-drawer :target target)))

(defcommand (explain-pane :group agents)
  "why this pane is what it is, and who typed into it"
  (if (find-if (lambda (it) (typep it 'drawer)) (client-overlays *client*))
      (drawer-close *client*)
      (open-drawer *client*)))
