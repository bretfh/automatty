;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; Something to read: what broke, and where. Any key puts it away.

(defstruct (note (:constructor %make-note))
  (title "" :type string)
  (lines nil :type list)
  (face :warning)
  (laid nil))

(defun make-note (title lines &key (face :warning))
  (%make-note :title title
              :lines (remove "" (mapcar (lambda (l) (string-right-trim '(#\Return) l))
                                        lines)
                             :test #'equal)
              :face face))

(defun note-chip-face (face)
  "The chip a note's title is on, by what kind of note it is."
  (case face (:warning :chip-blocked) (:error :brand) (t :number-working)))

(defun note-tree (n cols most)
  "A note is a band at the foot: its title as a chip, its first line beside
it, the way out at the right, and any more lines under it."
  (let ((lines (or (subseq (note-lines n) 0 (min most (length (note-lines n)))) (list ""))))
    (apply #'atty/ui:column :align :stretch :background-color (bar-face :bg-dim) :min-width cols
           (band (list (atty/ui:label (format nil " ~A " (note-title n)) :face (note-chip-face (note-face n)))
                       (atty/ui:label (format nil " ~A" (truncate-string (first lines) (max 1 (- cols 24))))))
                 (list (hint "any key" "closes" :runs :close)))
           (loop :for line :in (rest lines)
                 :collect (atty/ui:label (format nil "   ~A" (truncate-string line (max 1 (- cols 4)))))))))

(defmethod draw-overlay ((n note) screen)
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (m (atty/cells:make-cells (tty:screen-grid screen) cols rows))
         (tree (note-tree n cols (max 1 (- rows 3))))
         (high (nth-value 1 (atty/ui:with-pass
                              (atty/ui:restyle tree)
                              (atty/ui:measure tree m cols rows))))
         (top (max 0 (- rows high))))
    (atty/cells:draw tree (tty:screen-grid screen) cols rows :top top)
    (setf (note-laid n) tree
          (tty:screen-cursor-visible screen) nil)))

(defmethod overlay-laid-tree ((n note)) (note-laid n))

(atty/mode:define-mode note-mode ())

(defmethod mode-of ((n note)) 'note-mode)
(defmethod overlay-name ((n note)) (note-title n))

(defmethod overlay-unbound-key ((n note) chord client)
  "Anything at all puts it away."
  (declare (ignore chord))
  (client-pop-overlay client n)
  t)

(defun split-lines (text)
  (atty/ui:split-string text :separator (list #\Newline)))

(defgeneric show-note (client title text &key face))

(defmethod show-note ((client client) title text &key (face :warning))
  (client-push-overlay client (make-note title (split-lines text) :face face)))

(defun show-error (client what e)
  "What went wrong, and where it went wrong."
  (show-note client
             (format nil "~A came apart" what)
             (format nil "~A~%~A" e
                     (with-output-to-string (s)
                       (ignore-errors
                        (sb-debug:print-backtrace :stream s :count 20))))))

;;; A yes or no, asked the same way a note is said: a band at the foot with
;;; the question and its two answers as key caps, the yes in red since it is
;;; the one that cannot be taken back.

(defstruct (confirm (:constructor %make-confirm) (:conc-name confirm-of-))
  (question "" :type string)
  (yes nil)
  (yes-label "yes")
  (no-label "no")
  (laid nil))

(defun confirm (client question &key yes (yes-says "yes") (no-says "no"))
  "Ask CLIENT QUESTION; YES is called with the client when they say so."
  (client-push-overlay client (%make-confirm :question question :yes yes
                                         :yes-label yes-says :no-label no-says)))

(defun confirm-tree (c cols)
  (band (list (atty/ui:label " ? " :face :brand)
              (atty/ui:label (format nil " ~A" (truncate-string (confirm-of-question c) (max 1 (- cols 30))))))
        (list (keycap "y" (confirm-of-yes-label c) :runs :yes :face :brand)
              (atty/ui:label " ")
              (keycap "n" (confirm-of-no-label c) :runs :close)
              (atty/ui:label " "))))

(defmethod draw-overlay ((c confirm) screen)
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (tree (confirm-tree c cols)))
    (atty/cells:draw tree (tty:screen-grid screen) cols rows :top (max 0 (1- rows)))
    (setf (confirm-of-laid c) tree
          (tty:screen-cursor-visible screen) nil)))

(atty/mode:define-mode confirm-mode ())

(defmethod mode-of ((c confirm)) 'confirm-mode)
(defmethod overlay-name ((c confirm)) (confirm-of-question c))
(defmethod overlay-laid-tree ((c confirm)) (confirm-of-laid c))

(defun current-confirm ()
  (let ((it (first (client-overlays *client*))))
    (when (typep it 'confirm) it)))

(defun accept-confirm (c client)
  (client-pop-overlay client c)
  (when (confirm-of-yes c) (funcall (confirm-of-yes c) client)))

(defcommand (confirm-yes :unlisted)
  (let ((c (current-confirm))) (when c (accept-confirm c *client*))))

(defcommand (confirm-no :unlisted)
  (let ((c (current-confirm))) (when c (client-pop-overlay *client* c))))

(defcommand (confirm-click :unlisted)
  (let* ((c (current-confirm))
         (hit (and c (confirm-of-laid c) *mouse-position*
                   (button-at (confirm-of-laid c) (cdr *mouse-position*) (car *mouse-position*)))))
    (when hit
      (if (eq :yes (bar-button-runs hit))
          (accept-confirm c *client*)
          (handle-button c (bar-button-runs hit) *client*)))))

(defcommand (confirm-ignore :unlisted) nil)

(atty/mode:define-key 'confirm-mode "y"      #'confirm-yes)
(atty/mode:define-key 'confirm-mode "n"      #'confirm-no)
(atty/mode:define-key 'confirm-mode "Escape" #'confirm-no)
(atty/mode:define-key 'confirm-mode "C-g"    #'confirm-no)
(atty/mode:define-key 'confirm-mode "mouse-1" #'confirm-click)
(atty/mode:define-key 'confirm-mode "mouse-1-up" #'confirm-ignore)

;;; A word from whoever is here: a name, typed on the one line at the foot
;;; where the keys are, kept with RET and undone with Escape.

(defstruct (entry (:constructor %make-entry) (:conc-name entry-of-))
  (title "" :type string)
  (what "" :type string)
  (text "" :type string)
  (keep nil)
  (swap nil)
  (swap-label nil)
  (laid nil))

(defun entry (client title what text &key keep swap swap-says)
  "Ask CLIENT for a line of TEXT to start from, under TITLE, about WHAT. KEEP
is called with the text and the client on RET; SWAP with the client on TAB,
when there is one, and SWAP-SAYS says what TAB does."
  (client-push-overlay client (%make-entry :title title :what what :text text
                                       :keep keep :swap swap :swap-label swap-says)))

(defun entry-tree (e cols)
  (band (list (atty/ui:label (format nil " ~A " (entry-of-title e)) :face :number-working)
              (atty/ui:label (format nil " ~A  " (entry-of-what e)) :face :strong)
              (squeezed (field "" (entry-of-text e))))
        (append (when (entry-of-swap e)
                  (list (hint "⇥" (entry-of-swap-label e) :runs :swap)))
                (list (hint "↵" "keep" :runs :keep)
                      (hint "Esc" "undo" :runs :close)))))

(defmethod draw-overlay ((e entry) screen)
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (tree (entry-tree e cols)))
    (atty/cells:draw tree (tty:screen-grid screen) cols rows :top (max 0 (1- rows)))
    (setf (entry-of-laid e) tree
          (tty:screen-cursor-visible screen) nil)))

(atty/mode:define-mode entry-mode ())

(defmethod mode-of ((e entry)) 'entry-mode)
(defmethod overlay-name ((e entry)) (entry-of-title e))
(defmethod overlay-laid-tree ((e entry)) (entry-of-laid e))

(defmethod overlay-unbound-key ((e entry) chord client)
  (let ((said (atty/mode:self-inserting chord)))
    (when said
      (setf (entry-of-text e) (concatenate 'string (entry-of-text e) said)
            (client-dirty client) t)
      t)))

(defun current-entry ()
  (let ((it (first (client-overlays *client*))))
    (when (typep it 'entry) it)))

(defun accept-entry (e client)
  (client-pop-overlay client e)
  (when (entry-of-keep e) (funcall (entry-of-keep e) (entry-of-text e) client)))

(defun toggle-entry-kind (e client)
  (client-pop-overlay client e)
  (when (entry-of-swap e) (funcall (entry-of-swap e) client)))

(defcommand (entry-accept :unlisted)
  (let ((e (current-entry))) (when e (accept-entry e *client*))))

(defcommand (entry-toggle-kind :unlisted)
  (let ((e (current-entry))) (when e (toggle-entry-kind e *client*))))

(defcommand (entry-undo :unlisted)
  (let ((e (current-entry))) (when e (client-pop-overlay *client* e))))

(defcommand (entry-delete-backward :unlisted)
  (let ((e (current-entry)))
    (when (and e (plusp (length (entry-of-text e))))
      (setf (entry-of-text e) (subseq (entry-of-text e) 0 (1- (length (entry-of-text e))))
            (client-dirty *client*) t))))

(defcommand (entry-click :unlisted)
  (let* ((e (current-entry))
         (hit (and e (entry-of-laid e) *mouse-position*
                   (button-at (entry-of-laid e) (cdr *mouse-position*) (car *mouse-position*)))))
    (when hit
      (case (bar-button-runs hit)
        (:keep (accept-entry e *client*))
        (:swap (toggle-entry-kind e *client*))
        (t (handle-button e (bar-button-runs hit) *client*))))))

(defcommand (entry-ignore :unlisted) nil)

(atty/mode:define-key 'entry-mode "RET"    #'entry-accept)
(atty/mode:define-key 'entry-mode "TAB"    #'entry-toggle-kind)
(atty/mode:define-key 'entry-mode "DEL"    #'entry-delete-backward)
(atty/mode:define-key 'entry-mode "Escape" #'entry-undo)
(atty/mode:define-key 'entry-mode "C-g"    #'entry-undo)
(atty/mode:define-key 'entry-mode "mouse-1" #'entry-click)
(atty/mode:define-key 'entry-mode "mouse-1-up" #'entry-ignore)
