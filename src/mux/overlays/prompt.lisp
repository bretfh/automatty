;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(declaim (ftype function hints command-key subscribe-panes unsubscribe-panes button-at scroll-mode
                send-if-supported))

;;; Reading one thing from a list of things. It is drawn over the session rather
;;; than composed into it, because it belongs to whoever opened it: somebody
;;; else attached to the same session is still looking at their shell.
;;;
;;; The palette is four of these with tabs: the commands, the sessions and
;;; windows, the clients, and find. Typing another kind's prefix at the start
;;; switches to it, so does TAB, and the bar's field opens whichever is
;;; current there.

(defstruct (prompt (:constructor %make-prompt))
  (title "" :type string)
  (query "" :type string)
  (items nil :type list)
  (items-fn nil)                        ; the items, worked out afresh each time, when they move
  (text #'identity)                     ; an item as a string, or as a widget
  (index 0 :type fixnum)
  (most 8 :type fixnum)
  (chose nil)
  (dropped nil)
  (kind nil)                            ; the prefix that opens this kind, when it is one of the palette's
  (free nil)                            ; what was typed is the answer, and the items are only lines
  (narrow t)                            ; whether what is typed narrows the items
  (alt nil)
  (into nil)
  (foot nil)
  (third nil)                           ; what C-f does with the choice, when there is a third thing
  (preview nil)                         ; a function of the chosen item and the client answering a tree
  (on-move nil)                         ; called with the chosen item when the choice moves
  (live nil)                            ; called with what is typed as it is typed
  (controls nil)                        ; a function of the prompt answering toolbar parts
  (laid nil))

(defun make-prompt (title items &key (text #'identity) chose dropped (most 8) kind
                                     free (narrow t) (query "") alt into foot items-fn preview moved live controls third)
  "A prompt titled TITLE offering ITEMS. A FREE one takes what was typed rather
than one of its items: they are only lines saying what to type, and are not
narrowed by it. ALT is what C-RET does with the choice, INTO what TAB does, and
FOOT a line under the items saying what the keys do."
  (%make-prompt :title title :items items :text text :query query
                :chose chose :dropped dropped :most most :kind kind :free free :narrow narrow
                :alt alt :into into :foot foot :items-fn items-fn :preview preview
                :on-move moved :live live :controls controls :third third))

(defun prompt-all (p)
  (if (prompt-items-fn p)
      (funcall (prompt-items-fn p) (or *overlay-client* *client*))
      (prompt-items p)))

(defun prompt-showing (p)
  (if (or (prompt-free p) (not (prompt-narrow p)))
      (prompt-all p)
      (matches (prompt-query p) (prompt-all p) (prompt-text p))))

(defun prompt-chosen (p)
  (nth (prompt-index p) (prompt-showing p)))

(defun prompt-tabs (p)
  "The palette's tabs, the current one lit; a click on another opens it."
  (apply #'atty/ui:row :spacing 0
         (loop :for (prefix name runs) :in +palette-kinds+
               :for active := (eql prefix (prompt-kind p))
               :collect (bar-button runs
                                    (atty/ui:row :spacing 0
                                                 (atty/ui:label (format nil " ~C " prefix)
                                                                :face (if active :brand :quiet))
                                                 (atty/ui:label (format nil " ~A " name)
                                                                :face (if active :tab-active :quiet))
                                                 (atty/ui:label " "))))))

(defun prompt-row (p item i said)
  "One item as a row: a button that picks it, marked and lit where the choice is."
  (let ((chosen (= i (prompt-index p))))
    (if (prompt-free p)
        (atty/ui:label (format nil "   ~A" said))
        (gutter-row (if chosen "▶" "")
                    (if (stringp said) (atty/ui:label said) said)
                    :selected chosen :runs (list :pick i)))))

(defun prompt-tree (p client cols)
  (let* ((showing (prompt-showing p))
         (all (prompt-all p))
         (room (min (prompt-most p) (length showing)))
         (from (max 0 (min (- (length showing) room)
                           (- (prompt-index p) (floor room 2)))))
         (rows (loop :for item :in (subseq showing from (+ from room))
                     :for i :from from
                     :collect (prompt-row p item i (funcall (prompt-text p) item))))
         (chosen (prompt-chosen p))
         (preview (and (prompt-preview p) chosen (funcall (prompt-preview p) chosen client)))
         (count (if (prompt-free p) "" (format nil "~D of ~D" (length showing) (length all)))))
    (apply #'atty/ui:column
           :align :stretch
           :background-color (bar-face :bg-dim)
           :min-width cols
           (append
            (list (if (prompt-kind p)
                      (band (list (prompt-tabs p))
                            (list (atty/ui:label (format nil " ~A " count) :face :quiet)
                                  (close-button) (atty/ui:label " ")))
                      (header-band (prompt-title p)
                                   :right (atty/ui:label (format nil " ~A " count) :face :quiet)))
                  (apply #'toolbar
                         (field (if (prompt-kind p) (string (prompt-kind p)) "›") (prompt-query p))
                         (append (when (prompt-controls p) (list :right))
                                 (and (prompt-controls p) (funcall (prompt-controls p) p))))
                  (atty/ui:row
                   :align :stretch :spacing 0
                   (apply #'atty/ui:column :align :stretch :expand 2
                          (or rows (list (atty/ui:label "   nothing matches that" :face :quiet))))
                   (rail from (max 1 room) (max 1 (length showing)))
                   (if preview
                       (atty/ui:row :align :stretch :spacing 0 :expand 1
                                    (atty/ui:rule :upright t :face :card)
                                    (atty/ui:column :align :stretch :expand 1 preview))
                       (atty/ui:label ""))))
            (and (prompt-foot p)
                 (list (footer-band (prompt-foot p)
                                    :right (list (hint "Esc" "close" :runs :close)))))))))

(defmethod draw-overlay ((p prompt) screen)
  (let* ((client *overlay-client*)
         (cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows))
         (tree (prompt-tree p client cols))
         (high (nth-value 1 (atty/ui:with-pass
                              (atty/ui:restyle tree)
                              (atty/ui:measure tree m cols rows))))
         (top (if (or (null client) (client-barp client)) (min 1 (max 0 (1- rows))) 0))
         (bottom (min rows (+ top high))))
    (atty/cells:draw tree (tty:screen-grid screen) cols bottom :top top)
    (setf (prompt-laid p) tree)
    (setf (tty:screen-cursor-y screen) (min (1- rows) (1+ top))
          (tty:screen-cursor-x screen) (min (1- cols) (+ 4 (length (prompt-query p))))
          (tty:screen-cursor-visible screen) t)))

(defmethod overlay-laid-tree ((p prompt)) (prompt-laid p))

(defun prompt-close (p client)
  "Take it away. Nothing else needs doing: what it was covering is still in the
screen it was drawn over, and the diff puts it back."
  (client-pop-overlay client p))

(atty/mode:define-mode prompt-mode ())

(defmethod mode-of ((p prompt)) 'prompt-mode)
(defmethod overlay-name ((p prompt)) (prompt-title p))
(defmethod close-overlay ((p prompt) client)
  (prompt-close p client)
  (when (prompt-dropped p) (funcall (prompt-dropped p) client)))

(defun current-prompt ()
  (let ((it (first (client-overlays *client*))))
    (when (typep it 'prompt) it)))

(defun prompt-moved (p to)
  (let ((most (max 0 (1- (length (prompt-showing p))))))
    (setf (prompt-index p) (max 0 (min most to)))
    (when (prompt-on-move p)
      (let ((it (prompt-chosen p)))
        (when it (funcall (prompt-on-move p) it *client*))))))

(defun prompt-typed (p client)
  "What was typed changed: the choice starts over, and a live prompt is told."
  (setf (prompt-index p) 0
        (client-dirty client) t)
  (when (prompt-live p)
    (let ((*client* client)) (funcall (prompt-live p) (prompt-query p) client))))

(defmethod overlay-unbound-key ((p prompt) chord client)
  "A key nothing is bound to, if it is one that stands for a character, is what
was typed -- unless it is another prompt's prefix, typed at the very start,
which switches to that prompt instead."
  (let ((said (atty/mode:self-inserting chord)))
    (when said
      (let ((runs (and (not (prompt-free p))
                       (= 1 (length said)) (zerop (length (prompt-query p)))
                       (not (eql (char said 0) (prompt-kind p)))
                       (cdr (assoc (char said 0) +palette-prefixes+)))))
        (if runs
            (progn (close-overlay p client) (run-command runs client))
            (progn (setf (prompt-query p) (concatenate 'string (prompt-query p) said))
                   (prompt-typed p client))))
      t)))

(defcommand (prompt-next :unlisted)
  (let ((p (current-prompt))) (when p (prompt-moved p (1+ (prompt-index p))))))

(defcommand (prompt-previous :unlisted)
  (let ((p (current-prompt))) (when p (prompt-moved p (1- (prompt-index p))))))

(defcommand (prompt-page-down :unlisted)
  (let ((p (current-prompt)))
    (when p (prompt-moved p (+ (prompt-index p) (prompt-most p))))))

(defcommand (prompt-page-up :unlisted)
  (let ((p (current-prompt)))
    (when p (prompt-moved p (- (prompt-index p) (prompt-most p))))))

(defcommand (prompt-first :unlisted)
  (let ((p (current-prompt))) (when p (prompt-moved p 0))))

(defcommand (prompt-last :unlisted)
  (let ((p (current-prompt)))
    (when p (prompt-moved p (length (prompt-showing p))))))

(defcommand (prompt-delete-backward :unlisted)
  (let ((p (current-prompt)))
    (when p
      (let ((q (prompt-query p)))
        (setf (prompt-query p) (subseq q 0 (max 0 (1- (length q)))))
        (prompt-typed p *client*)))))

(defcommand (prompt-clear :unlisted)
  (let ((p (current-prompt)))
    (when p (setf (prompt-query p) "") (prompt-typed p *client*))))

(defcommand (prompt-accept :unlisted)
  (let ((p (current-prompt)))
    (when p
      (let ((it (if (prompt-free p) (prompt-query p) (or (prompt-chosen p) (prompt-query p)))))
        (prompt-close p *client*)
        (when (and it (prompt-chose p)) (funcall (prompt-chose p) it *client*))))))

(defcommand (prompt-cancel :unlisted)
  (let ((p (current-prompt)))
    (when p (close-overlay p *client*))))

(defcommand (prompt-accept-alternate :unlisted)
  "C-RET: the other thing a prompt does with the choice, when it has one."
  (let ((p (current-prompt)))
    (when (and p (prompt-alt p))
      (let ((it (if (prompt-free p) (prompt-query p) (prompt-chosen p))))
        (prompt-close p *client*)
        (when it (funcall (prompt-alt p) it *client*))))))

(defcommand (prompt-accept-third :unlisted)
  "C-f: the third thing a prompt does with the choice, when it has one."
  (let ((p (current-prompt)))
    (when (and p (prompt-third p))
      (let ((it (prompt-chosen p)))
        (prompt-close p *client*)
        (when it (funcall (prompt-third p) it *client*))))))

(defun next-kind (p)
  "The palette kind after P's, round the end."
  (let ((at (position (prompt-kind p) +palette-kinds+ :key #'first)))
    (and at (nth (mod (1+ at) (length +palette-kinds+)) +palette-kinds+))))

(defcommand (prompt-descend :unlisted)
  "TAB: go into the choice when the prompt has somewhere to go, else over to
the palette's next kind."
  (let ((p (current-prompt)))
    (cond
      ((and p (prompt-into p))
       (let ((it (if (prompt-free p) (prompt-query p) (prompt-chosen p))))
         (prompt-close p *client*)
         (funcall (prompt-into p) it *client*)))
      ((and p (next-kind p))
       (let ((kind (next-kind p)))
         (close-overlay p *client*)
         (run-command (third kind) *client*))))))

(defcommand (prompt-click :unlisted)
  "A click on a row chooses it; on a tab, opens that kind; on a control, does
what it says; on the field, nothing."
  (let* ((p (current-prompt))
         (hit (and p (prompt-laid p) *mouse-position*
                   (button-at (prompt-laid p) (cdr *mouse-position*) (car *mouse-position*))))
         (runs (and hit (bar-button-runs hit))))
    (when hit
      (cond
        ((and (consp runs) (eq :pick (first runs)))
         (setf (prompt-index p) (second runs))
         (prompt-accept))
        ((stringp runs)
         (close-overlay p *client*)
         (run-command runs *client*))
        (t (handle-button p runs *client*))))))

(defcommand (prompt-ignore :unlisted) nil)

(atty/mode:define-key 'prompt-mode "Down"     #'prompt-next)
(atty/mode:define-key 'prompt-mode "C-n"      #'prompt-next)
(atty/mode:define-key 'prompt-mode "Up"       #'prompt-previous)
(atty/mode:define-key 'prompt-mode "C-p"      #'prompt-previous)
(atty/mode:define-key 'prompt-mode "PageDown" #'prompt-page-down)
(atty/mode:define-key 'prompt-mode "PageUp"   #'prompt-page-up)
(atty/mode:define-key 'prompt-mode "Home"     #'prompt-first)
(atty/mode:define-key 'prompt-mode "End"      #'prompt-last)
(atty/mode:define-key 'prompt-mode "DEL"      #'prompt-delete-backward)
(atty/mode:define-key 'prompt-mode "C-u"      #'prompt-clear)
(atty/mode:define-key 'prompt-mode "RET"      #'prompt-accept)
(atty/mode:define-key 'prompt-mode "Escape"   #'prompt-cancel)
(atty/mode:define-key 'prompt-mode "C-g"      #'prompt-cancel)
(atty/mode:define-key 'prompt-mode "C-RET"    #'prompt-accept-alternate)
(atty/mode:define-key 'prompt-mode "C-f"      #'prompt-accept-third)
(atty/mode:define-key 'prompt-mode "TAB"      #'prompt-descend)
(atty/mode:define-key 'prompt-mode "mouse-1"  #'prompt-click)
(atty/mode:define-key 'prompt-mode "mouse-1-up" #'prompt-ignore)
(atty/mode:define-key 'prompt-mode "wheel-up"   #'prompt-previous)
(atty/mode:define-key 'prompt-mode "wheel-down" #'prompt-next)

(defun open-prompt (client title items &key (text #'identity) chose dropped kind free (narrow t) (query "")
                                      alt into foot items-fn preview moved live controls third)
  "Put a prompt over whatever CLIENT is showing."
  (client-push-overlay client (make-prompt title items :text text
                                                   :chose chose
                                                   :dropped dropped
                                                   :kind kind
                                                   :free free :narrow narrow
                                                   :query query
                                                   :alt alt :into into :foot foot
                                                   :items-fn items-fn :preview preview
                                                   :moved moved :live live :controls controls
                                                   :third third)))

;;; The palette's kinds.

;;; Sessions and windows, as one list: @ on the bar, or C-b '.

;;; The clients: every terminal attached, and what each looks at.

;;; Find: the hits in the pane's history as rows, the pane scrolled to the
;;; one chosen as the choice moves.
