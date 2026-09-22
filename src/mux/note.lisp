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
                       (atty/ui:label (format nil " ~A" (shortened-to (first lines) (max 1 (- cols 24))))))
                 (list (hint "any key" "closes" :runs :close)))
           (loop :for line :in (rest lines)
                 :collect (atty/ui:label (format nil "   ~A" (shortened-to line (max 1 (- cols 4)))))))))

(defmethod draw-over ((n note) screen)
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

(defmethod laid-tree ((n note)) (note-laid n))

(atty/mode:define-mode note-mode ())

(defmethod mode-of ((n note)) 'note-mode)
(defmethod over-name ((n note)) (note-title n))

(defmethod unbound ((n note) chord client)
  "Anything at all puts it away."
  (declare (ignore chord))
  (client-over-drop client n)
  t)

(defun lines-of (text)
  (atty/ui:split-string text :separator (list #\Newline)))

(defun show-note (client title text &key (face :warning))
  (client-over-put client (make-note title (lines-of text) :face face)))

(defun show-broke (client what e)
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
  (yes-says "yes")
  (no-says "no")
  (laid nil))

(defun confirm (client question &key yes (yes-says "yes") (no-says "no"))
  "Ask CLIENT QUESTION; YES is called with the client when they say so."
  (client-over-put client (%make-confirm :question question :yes yes
                                         :yes-says yes-says :no-says no-says)))

(defun confirm-tree (c cols)
  (band (list (atty/ui:label " ? " :face :brand)
              (atty/ui:label (format nil " ~A" (shortened-to (confirm-of-question c) (max 1 (- cols 30))))))
        (list (keycap "y" (confirm-of-yes-says c) :runs :yes :face :brand)
              (atty/ui:label " ")
              (keycap "n" (confirm-of-no-says c) :runs :close)
              (atty/ui:label " "))))

(defmethod draw-over ((c confirm) screen)
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (tree (confirm-tree c cols)))
    (atty/cells:draw tree (tty:screen-grid screen) cols rows :top (max 0 (1- rows)))
    (setf (confirm-of-laid c) tree
          (tty:screen-cursor-visible screen) nil)))

(atty/mode:define-mode confirm-mode ())

(defmethod mode-of ((c confirm)) 'confirm-mode)
(defmethod over-name ((c confirm)) (confirm-of-question c))
(defmethod laid-tree ((c confirm)) (confirm-of-laid c))

(defun the-confirm ()
  (let ((it (first (client-over *client*))))
    (when (typep it 'confirm) it)))

(defun confirm-said-yes (c client)
  (client-over-drop client c)
  (when (confirm-of-yes c) (funcall (confirm-of-yes c) client)))

(defcommand (confirm-yes :unlisted)
  (let ((c (the-confirm))) (when c (confirm-said-yes c *client*))))

(defcommand (confirm-no :unlisted)
  (let ((c (the-confirm))) (when c (client-over-drop *client* c))))

(defcommand (confirm-click :unlisted)
  (let* ((c (the-confirm))
         (hit (and c (confirm-of-laid c) *mouse-at*
                   (button-at (confirm-of-laid c) (cdr *mouse-at*) (car *mouse-at*)))))
    (when hit
      (if (eq :yes (bar-button-runs hit))
          (confirm-said-yes c *client*)
          (generic-click c (bar-button-runs hit) *client*)))))

(defcommand (confirm-nothing :unlisted) nil)

(atty/mode:define-key 'confirm-mode "y"      #'confirm-yes)
(atty/mode:define-key 'confirm-mode "n"      #'confirm-no)
(atty/mode:define-key 'confirm-mode "Escape" #'confirm-no)
(atty/mode:define-key 'confirm-mode "C-g"    #'confirm-no)
(atty/mode:define-key 'confirm-mode "mouse-1" #'confirm-click)
(atty/mode:define-key 'confirm-mode "mouse-1-up" #'confirm-nothing)

;;; A word from whoever is here: a name, typed on the one line at the foot
;;; where the keys are, kept with RET and undone with Escape.

(defstruct (entry (:constructor %make-entry) (:conc-name entry-of-))
  (title "" :type string)
  (what "" :type string)
  (text "" :type string)
  (keep nil)
  (swap nil)
  (swap-says nil)
  (laid nil))

(defun entry (client title what text &key keep swap swap-says)
  "Ask CLIENT for a line of TEXT to start from, under TITLE, about WHAT. KEEP
is called with the text and the client on RET; SWAP with the client on TAB,
when there is one, and SWAP-SAYS says what TAB does."
  (client-over-put client (%make-entry :title title :what what :text text
                                       :keep keep :swap swap :swap-says swap-says)))

(defun entry-tree (e cols)
  (band (list (atty/ui:label (format nil " ~A " (entry-of-title e)) :face :number-working)
              (atty/ui:label (format nil " ~A  " (entry-of-what e)) :face :strong)
              (squeezed (field "" (entry-of-text e))))
        (append (when (entry-of-swap e)
                  (list (hint "⇥" (entry-of-swap-says e) :runs :swap)))
                (list (hint "↵" "keep" :runs :keep)
                      (hint "Esc" "undo" :runs :close)))))

(defmethod draw-over ((e entry) screen)
  (let* ((cols (tty:screen-width screen))
         (rows (tty:screen-height screen))
         (tree (entry-tree e cols)))
    (atty/cells:draw tree (tty:screen-grid screen) cols rows :top (max 0 (1- rows)))
    (setf (entry-of-laid e) tree
          (tty:screen-cursor-visible screen) nil)))

(atty/mode:define-mode entry-mode ())

(defmethod mode-of ((e entry)) 'entry-mode)
(defmethod over-name ((e entry)) (entry-of-title e))
(defmethod laid-tree ((e entry)) (entry-of-laid e))

(defmethod unbound ((e entry) chord client)
  (let ((said (atty/mode:self-inserting chord)))
    (when said
      (setf (entry-of-text e) (concatenate 'string (entry-of-text e) said)
            (client-dirty client) t)
      t)))

(defun the-entry ()
  (let ((it (first (client-over *client*))))
    (when (typep it 'entry) it)))

(defun entry-kept (e client)
  (client-over-drop client e)
  (when (entry-of-keep e) (funcall (entry-of-keep e) (entry-of-text e) client)))

(defun entry-swapped (e client)
  (client-over-drop client e)
  (when (entry-of-swap e) (funcall (entry-of-swap e) client)))

(defcommand (entry-keep :unlisted)
  (let ((e (the-entry))) (when e (entry-kept e *client*))))

(defcommand (entry-swap :unlisted)
  (let ((e (the-entry))) (when e (entry-swapped e *client*))))

(defcommand (entry-undo :unlisted)
  (let ((e (the-entry))) (when e (client-over-drop *client* e))))

(defcommand (entry-rub-out :unlisted)
  (let ((e (the-entry)))
    (when (and e (plusp (length (entry-of-text e))))
      (setf (entry-of-text e) (subseq (entry-of-text e) 0 (1- (length (entry-of-text e))))
            (client-dirty *client*) t))))

(defcommand (entry-click :unlisted)
  (let* ((e (the-entry))
         (hit (and e (entry-of-laid e) *mouse-at*
                   (button-at (entry-of-laid e) (cdr *mouse-at*) (car *mouse-at*)))))
    (when hit
      (case (bar-button-runs hit)
        (:keep (entry-kept e *client*))
        (:swap (entry-swapped e *client*))
        (t (generic-click e (bar-button-runs hit) *client*))))))

(defcommand (entry-nothing :unlisted) nil)

(atty/mode:define-key 'entry-mode "RET"    #'entry-keep)
(atty/mode:define-key 'entry-mode "TAB"    #'entry-swap)
(atty/mode:define-key 'entry-mode "DEL"    #'entry-rub-out)
(atty/mode:define-key 'entry-mode "Escape" #'entry-undo)
(atty/mode:define-key 'entry-mode "C-g"    #'entry-undo)
(atty/mode:define-key 'entry-mode "mouse-1" #'entry-click)
(atty/mode:define-key 'entry-mode "mouse-1-up" #'entry-nothing)
