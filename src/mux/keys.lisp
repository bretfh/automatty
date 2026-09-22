;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What the keys do, and which keys do it.

(defcommand detach
  (done-with *client* :detached))

(defcommand redraw
  (client-redraw *client*))

(defun tell-the-server (form)
  "Say FORM to the server holding this session, or say why not.

The server is usually older than the client that reached it: it has been running
since whenever, and this was started a moment ago. A key it has never heard of
that quietly does nothing looks exactly like a key that is broken, so it says so
instead."
  (if (member (first form) (client-knows *client*))
      (wire-send (client-wire *client*) form)
      (show-note *client* "not here"
                 (format nil "The server holding this session cannot ~(~A~).~%~%~
                              It has been running since before that was added.~%~
                              What is in it goes on running; a session started~%~
                              now knows the whole of it."
                         (first form))
                 :face :warning)))

(defcommand bar-off
  (tell-the-server (list :bar nil)))

(defcommand bar-on
  (tell-the-server (list :bar t)))

(defcommand toggle-the-bar
  (tell-the-server (list :bar :toggle)))

(defcommand run-a-command
  (ask-a-command *client*))

(defcommand split-right
  (tell-the-server (list :split :across)))

(defcommand split-below
  (tell-the-server (list :split :down)))

(defcommand next-pane
  (tell-the-server (list :focus)))

(defcommand close-pane
  (tell-the-server (list :close)))

(defcommand only-this-pane
  (tell-the-server (list :only)))

(defcommand new-session
  (tell-the-server (list :new)))

;;; Windows. Each names the session it is on, since the server takes the same
;;; forms from a client on another session and from the command line.

(defcommand new-window
  (tell-the-server (list :new-window (client-session *client*))))

(defcommand next-window
  (tell-the-server (list :next-window (client-session *client*))))

(defcommand previous-window
  (tell-the-server (list :previous-window (client-session *client*))))

(defcommand close-window
  (ask *client* "close this window and every program in it?" (list "y  yes" "n  no")
       :free t
       :chose (lambda (typed c)
                (when (and (plusp (length typed)) (char-equal #\y (char typed 0)))
                  (let ((*client* c))
                    (tell-the-server (list :close-window (client-session c))))))))

(defcommand choose-a-session
  ;; the switchboard with nothing picked: its bands are the sessions, and the
  ;; cursor starts on the one this is
  (open-the-board *client* :session (client-session *client*)))

(defcommand send-the-prefix
  (tell-the-server (list :keys (string +prefix+))))

(defcommand name-this-pane
  (tell-the-server (list :naming)))

(defcommand go-to-the-blocked
  (tell-the-server (list :go-to-blocked)))

(defcommand zoom-this-pane
  (tell-the-server (list :zoom)))

(defun server-knows-p (what)
  (member what (client-knows *client*)))

(defun tell-the-server-if-it-knows (form)
  "Say FORM to the server if it has heard of it, and nothing at all if not. For
what a mouse does: a wheel that does nothing on a server from before it did
anything is what that server always did, and a note for every notch is worse."
  (when (server-knows-p (first form))
    (wire-send (client-wire *client*) form)))

(defcommand (mouse-clicked :unlisted)
  (tell-the-server (list :mouse-at (car *mouse-at*) (cdr *mouse-at*))))

;;; The mouse, passed on. Which button and where is the server's to make sense
;;; of: it knows what was drawn there, and whether the program under it asked
;;; to be told.

(defun mouse-held ()
  "The modifiers held with what the mouse just did."
  (loop :for mod :in '(:shift :meta :ctrl)
        :when (getf *mouse-event* mod) :collect mod))

(defun pointer-did (what)
  (let ((button (or (getf *mouse-event* :button) :left)))
    (cond ((server-knows-p :pointer)
           (wire-send (client-wire *client*)
                      (list :pointer what button (car *mouse-at*) (cdr *mouse-at*)
                            (mouse-held))))
          ;; a server from before buttons were passed on still knows a click
          ((and (eq what :press) (eq button :left)) (mouse-clicked)))))

(defcommand (mouse-pressed :unlisted) (pointer-did :press))
(defcommand (mouse-dragged :unlisted) (pointer-did :drag))
(defcommand (mouse-released :unlisted) (pointer-did :release))

;;; Reading a pane back. Every one of these is about the pane under the pointer
;;; when a mouse asked for it and the pane with the focus when a key did.

(defun scroll-by (amount)
  (tell-the-server-if-it-knows
   (list :scroll amount (car *mouse-at*) (cdr *mouse-at*))))

(defun wheel-went (way)
  (tell-the-server-if-it-knows
   (list :wheel way (car *mouse-at*) (cdr *mouse-at*) (mouse-held))))

(defcommand natural-scroll-up (wheel-went :up))
(defcommand natural-scroll-down (wheel-went :down))

(defcommand scroll-up (scroll-by 1))
(defcommand scroll-down (scroll-by -1))
(defcommand scroll-up-a-little (scroll-by 3))
(defcommand scroll-down-a-little (scroll-by -3))
(defcommand scroll-page-up (scroll-by :page-up))
(defcommand scroll-page-down (scroll-by :page-down))
(defcommand scroll-half-page-up (scroll-by :half-up))
(defcommand scroll-half-page-down (scroll-by :half-down))
(defcommand scroll-to-top (scroll-by :top))
(defcommand scroll-to-bottom (scroll-by :bottom))

(defcommand toggle-scrollbars
  (tell-the-server (list :scrollbars :toggle)))

;;; Scroll mode: the keys that read back, with no prefix in front of them, for
;;; as long as it is on. Nothing typed reaches the pane until it is left, and
;;; leaving it is back to live.

(defstruct (reading (:constructor %make-reading)))

(atty/mode:define-mode scroll-mode (pane-mode))

(defmethod mode-of ((r reading)) 'scroll-mode)

(defmethod draw-over ((r reading) screen)
  "Only what says it is on and how to get out: what is being read is the pane."
  (let ((cols (tty:screen-width screen))
        (rows (tty:screen-height screen))
        (leave (car (find #'leave-scroll-mode
                          (atty/mode:keys-in-force (atty/mode:mode-named 'scroll-mode))
                          :key #'cdr))))
    (atty/cells:draw (atty/ui:label (format nil " reading back~@[ · ~A leaves~] " leave)
                                    :face :chip-scrolled)
                     (tty:screen-grid screen) cols rows :left 0 :top (max 0 (1- rows)))
    (setf (tty:screen-cursor-visible screen) nil)))

(defun reading-now ()
  (find-if (lambda (it) (typep it 'reading)) (client-over *client*)))

(defcommand scroll-mode
  (unless (reading-now)
    (client-over-put *client* (%make-reading))))

(defcommand (leave-scroll-mode :unlisted)
  (let ((r (reading-now)))
    (when r (client-over-drop *client* r)))
  (scroll-to-bottom))

(defcommand scroll-mode-page-up
  (scroll-mode)
  (scroll-page-up))

(defcommand what-the-keys-do
  (show-note *client* "keys"
             (format nil "~{~A~%~}"
                     (loop :for (chord . nil) :in (atty/mode:keys-in-force
                                                   (atty/mode:mode-named 'pane-mode))
                           :collect (format nil "  ~A" chord)))
             :face :accent))

;;; A mode holds these, so another mode may be defined on top of this one and
;;; change or add to what is here without touching any of it.


;;; Panes are windows and the keys for them are the ones an editor uses for
;;; windows: 2 splits below, 3 splits beside, 0 closes this one, 1 leaves only
;;; this one, o goes to the next.



(declaim (ftype function load-user-init init-loaded-note))

(defcommand reload-init
  ;; here and in the server both, since the file is read in both
  (load-user-init)
  (destructuring-bind (text face) (init-loaded-note)
    (show-note *client* "init" text :face face))
  (tell-the-server (list :reload-init)))

;;; Everything behind the prefix, as what follows it. They are bound from
;;; whatever the prefix is, so setting it in an init file moves every one of
;;; them; :prefix is the prefix itself, which sends one to the pane.

(defparameter +prefixed-keys+
  '(
    ("d" . detach)
    ("r" . redraw)
    (":" . run-a-command)
    ("?" . what-the-keys-do)
    (:prefix . send-the-prefix)
    ("t" . toggle-the-bar)
    ("2" . split-below)
    ("3" . split-right)
    ("0" . close-pane)
    ("1" . only-this-pane)
    ("o" . next-pane)
    ("," . name-this-pane)
    ("a" . go-to-the-blocked)
    ("z" . zoom-this-pane)
    ("N" . needs-you)
    ("w" . switchboard)
    ("e" . explain-this-pane)
    ("C" . new-session)
    ("c" . new-window)
    ("n" . next-window)
    ("p" . previous-window)
    ("&" . close-window)
    ("b" . choose-a-session)
    ("[" . scroll-mode)
    ("PageUp" . scroll-mode-page-up)
    ("R" . reload-init)))

(defun prefix-spelled (&optional (prefix +prefix+))
  (atty/mode:spelled (key-of prefix)))

(defun bind-prefixed-keys (&optional (prefix +prefix+) was)
  "Bind everything in +PREFIXED-KEYS+ behind PREFIX, first unbinding it from
behind WAS when the prefix has moved."
  (flet ((chord (prefix tail)
           (format nil "~A ~A" (prefix-spelled prefix)
                   (if (eq tail :prefix) (prefix-spelled prefix) tail))))
    (when (and was (not (eql was prefix)))
      (loop :for (tail . nil) :in +prefixed-keys+
            :do (atty/mode:undefine-key 'pane-mode (chord was tail))))
    (loop :for (tail . command) :in +prefixed-keys+
          :do (atty/mode:define-key 'pane-mode (chord prefix tail) (symbol-function command)))
    prefix))

(bind-prefixed-keys)

(let ((was +prefix+))
  (after-setting '+prefix+ (lambda (prefix)
                             (bind-prefixed-keys prefix was)
                             (setf was prefix))))

;;; A click is looked up the same as any other key, unprefixed: a mouse's
;;; buttons are always the multiplexer's, the way a keyboard's letters are
;;; always the pane's until C-b says otherwise.

(dolist (button '("mouse-1" "mouse-2" "mouse-3"))
  (atty/mode:define-key 'pane-mode button #'mouse-pressed)
  (atty/mode:define-key 'pane-mode (format nil "~A-drag" button) #'mouse-dragged)
  (atty/mode:define-key 'pane-mode (format nil "~A-up" button) #'mouse-released))

;;; The wheel is whoever's is under it. With shift or meta held it reads the
;;; pane back whatever is running there, both because which of them a terminal
;;; keeps for itself depends on the terminal.

(atty/mode:define-key 'pane-mode "wheel-up" #'natural-scroll-up)
(atty/mode:define-key 'pane-mode "wheel-down" #'natural-scroll-down)
(atty/mode:define-key 'pane-mode "S-wheel-up" #'scroll-up-a-little)
(atty/mode:define-key 'pane-mode "S-wheel-down" #'scroll-down-a-little)
(atty/mode:define-key 'pane-mode "M-wheel-up" #'scroll-up-a-little)
(atty/mode:define-key 'pane-mode "M-wheel-down" #'scroll-down-a-little)


(loop :for (chord does) :in `(("Up" ,#'scroll-up) ("k" ,#'scroll-up)
                              ("Down" ,#'scroll-down) ("j" ,#'scroll-down)
                              ("PageUp" ,#'scroll-page-up) ("PageDown" ,#'scroll-page-down)
                              ("SPC" ,#'scroll-page-down)
                              ("C-u" ,#'scroll-half-page-up) ("C-d" ,#'scroll-half-page-down)
                              ("g" ,#'scroll-to-top) ("Home" ,#'scroll-to-top)
                              ("G" ,#'scroll-to-bottom) ("End" ,#'scroll-to-bottom)
                              ("q" ,#'leave-scroll-mode) ("Escape" ,#'leave-scroll-mode)
                              ("RET" ,#'leave-scroll-mode))
      :do (atty/mode:define-key 'scroll-mode chord does))
