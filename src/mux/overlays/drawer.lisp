;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)


(defparameter +drawer-max-width+ 60
              "The most columns the drawer takes; a third of the terminal when that is less.")

(defstruct (drawer (:constructor %make-drawer))
           (target nil)                          ; (session . id) to open on, else the focus
           (folded nil :type list)
           (laid nil))

(defun drawer-width (cols)
  (min +drawer-max-width+ (floor cols 3)))

(defun focused-row (watcher)
  "The row of the pane with the focus in WATCHER's session."
  (find-if (lambda (r) (and (getf r :focus) (equal (getf r :session) (watcher-session-name watcher))))
           (pane-rows-of watcher)))

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

(defun log-lines (watcher log)
  (loop :for (nil actor verb summary outcome clock) :in log
        :collect (atty/ui:row :spacing 0
                              (atty/ui:label (format nil "~A " (format-clock-hms clock))
                                             :face :quiet)
                              (atty/ui:label (format nil "~14A " (log-actor-text watcher actor))
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

(defun drawer-context (watcher key)
  "What explained the state of KEY's pane, what it is, and its log."
  (let ((pane (and key (find-pane (watcher-server watcher) (car key) (cdr key))))
        (now (now-ms)))
    (if (null pane)
        (values nil nil nil)
        (multiple-value-bind (seen rows) (with-term (term pane) (agent:agent-explain (pane-agent pane) term))
          (values (list (agent:agent-state (pane-agent pane)) seen rows)
                  (pane-info pane)
                  (mapcar (lambda (e) (encode-log-entry e now))
                          (subseq (pane-log pane) 0 (min 8 (length (pane-log pane))))))))))

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

(defun typed-cells (log)
  "For each cell of a pulse, oldest first, whether anything in LOG typed then."
  (let ((typed (make-list +pulse-cells+ :initial-element nil)))
    (dolist (entry log typed)
      (let ((at (- +pulse-cells+ 1 (floor (first entry) +pulse-interval+))))
        (when (<= 0 at (1- +pulse-cells+))
          (setf (nth at typed) t))))))

(defun state-section (watcher key row state now log)
  "What state the pane is in and for how long, and when it is read, its last
twenty minutes: what it was in the top half of each cell, and whether anything
typed into it in the bottom half."
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
                        (timeline (pane-cells watcher (car key) (cdr key)) (typed-cells log))
                        (atty/ui:label "  what it was, over what typed into it" :face :quiet))
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

(defun drawer-tree (d watcher key row width
                    &optional (context (multiple-value-list (drawer-context watcher key))) (height 0))
  "The drawer for KEY's pane: CONTEXT is what explained its state, what it is,
and its log."
  (destructuring-bind (explained info log) context
                       (let* ((now (now-ms))
                              (state (and (getf row :known) (getf row :state)))
                              (rows (third explained))
                              (asks (and (eq state :blocked) (getf row :asks)))
                              (name (or (getf row :label) (getf row :says) (cdr key)))
                              (session (car key)) (id (cdr key)))
                         (flet ((open-p (what) (not (member what (drawer-folded d)))))
                               (sheet :drawer
                                      (atty/ui:label (format nil " ~A "
                                                             (if state
                                                                 (format nil "why is ~A ~A?" name (if (eq state :blocked) "asking" (string-downcase state)))
                                                                 (format nil "what is ~A?" name)))
                                                     :face :strong)
                                      (append
                                       (list (atty/ui:row :spacing 0 (atty/ui:label " ")
                                                          (path '(:quiet "default") (getf row :session)
                                                                (and (getf row :window)
                                                                     (format nil "~D~@[ ~A~]" (getf row :window)
                                                                             (let ((w (getf row :window-name))) (and w (plusp (length w)) w))))
                                                                (format nil "~@[~D ~]~A" (and (getf row :at) (1+ (getf row :at))) name)
                                                                (list :quiet (format nil "› ~A" (truncate-string (or (getf info :command) "") 24)))))
                                             (toolbar (keycap "z" "zoom" :runs (lambda () (session-zoom-pane (here-server) *client* session id)))
                                                      (keycap "r" "read" :runs (lambda () (read-pane (here-server) *client* session id)))
                                                      (keycap "n" "name" :runs (lambda ()
                                                                                 (let ((pane (find-pane (here-server) session id)))
                                                                                   (when pane
                                                                                     (prompt-pane-name *client* session id (pane-label pane)
                                                                                                       (pane-named pane))))))
                                                      (keycap "x" "close" :runs (list :confirm-close session id)))
                                             (section-head d "what it is" :what))
                                       (when (open-p :what) (identity-section info row))
                                       (list (section-head d "state" :state))
                                       (when (open-p :state) (state-section watcher key row state now log))
                                       ;; the reading only for an agent that is read; for
                                       ;; anything else it would be about nothing
                                       (when state
                                         (cons (section-head d "how it was read · in the order tried" :read)
                                               (when (open-p :read)
                                                 (or (and rows (rule-lines rows))
                                                     (list (atty/ui:label "  nothing read yet" :face :quiet))))))
                                       (list (section-head d "who typed here" :who))
                                       (when (open-p :who)
                                         (or (log-lines watcher log)
                                             (list (atty/ui:label "  nobody yet" :face :quiet))))
                                       (list (atty/ui:gap :expand 1))
                                       (list (answer-bar row asks)))
                                      :right (atty/ui:label (format nil " ~A " (key-hint 'pane-mode 'explain-pane)) :face :quiet)
                                      :width width :height height)))))

(defmethod draw-overlay ((d drawer) screen)
           (let* ((watcher *client*)
                  (cols (tty:screen-width screen))
                  (rows (tty:screen-height screen))
                  (width (drawer-width cols))
                  (row (or (and (drawer-target d)
                                (find (drawer-target d) (pane-rows-of watcher) :key #'row-key :test #'equal))
                           (focused-row watcher)))
                  (key (and row (row-key row))))
             (when key
               (let* ((height (max 1 (sheet-room watcher :drawer rows)))
                      (tree (drawer-tree d watcher key row width
                                         (multiple-value-list (drawer-context watcher key)) height)))
                 (draw-sheet tree watcher :drawer screen width height)
                 (setf (drawer-laid d) tree)))))

(defmethod overlay-laid-tree ((d drawer)) (drawer-laid d))

(defmethod overlay-clicked ((d drawer) line col watcher)
           "A button on the drawer: an answer at its foot, a section's fold, one of
its actions."
           (let* ((hit (and (drawer-laid d) (button-at (drawer-laid d) line col)))
                  (runs (and hit (bar-button-runs hit))))
             (when hit
               (let ((*client* watcher))
                 (case (and (consp runs) (first runs))
                       (:answer (destructuring-bind (session id n) (rest runs)
                                  (answer-pane (watcher-server watcher) watcher session id n)))
                       (:fold (setf (drawer-folded d) (if (member (second runs) (drawer-folded d))
                                                          (remove (second runs) (drawer-folded d))
                                                        (cons (second runs) (drawer-folded d)))
                                    (watcher-behind watcher) t))
                       (:confirm-close
                        (destructuring-bind (session id) (rest runs)
                                            (confirm watcher (format nil "close ~A and what runs in it?"
                                                             (let ((row (find (cons session id) (pane-rows-of watcher)
                                                                              :key #'row-key :test #'equal)))
                                                               (if row (row-path row) (format nil "~A:~D" session id))))
                                                     :yes (lambda (w) (close-pane-named (watcher-server w) session id)))))
                       (t (handle-button d runs watcher))))
               t)))

(defmethod overlay-ticks-p ((d drawer)) t)
(defmethod overlay-passes-keys-p ((d drawer)) t)
(defmethod overlay-name ((d drawer)) "why")

(atty/mode:define-mode drawer-mode (pane-mode))
(defmethod mode-of ((d drawer)) 'drawer-mode)

(defun drawer-close (watcher)
  (let ((open (find-if (lambda (it) (typep it 'drawer)) (watcher-overlays watcher))))
    (when open
      (pop-overlay watcher open))))

(defmethod close-overlay ((d drawer) watcher) (drawer-close watcher))

(defcommand (close-drawer :unlisted) ()
            (drawer-close *client*))

(atty/mode:define-key 'drawer-mode "Escape" #'close-drawer)
(atty/mode:define-key 'drawer-mode "C-g" #'close-drawer)

(declaim (ftype (function (watcher &optional t) t) open-drawer))
(defun open-drawer (watcher &optional target)
  "The drawer over WATCHER's screen, on the pane TARGET names or the focus."
  (push-overlay watcher (%make-drawer :target target)))

(declaim (ftype (function () t) explain-pane))
(defcommand (explain-pane :group agents) ()
            "why this pane is what it is, and who typed into it"
            (if (find-if (lambda (it) (typep it 'drawer)) (watcher-overlays *client*))
                (drawer-close *client*)
              (open-drawer *client*)))
