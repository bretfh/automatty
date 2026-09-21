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

(atty/mode:define-key 'pane-mode "C-b d" #'detach)
(atty/mode:define-key 'pane-mode "C-b r" #'redraw)
(atty/mode:define-key 'pane-mode "C-b :" #'run-a-command)
(atty/mode:define-key 'pane-mode "C-b ?" #'what-the-keys-do)
(atty/mode:define-key 'pane-mode "C-b C-b" #'send-the-prefix)
(atty/mode:define-key 'pane-mode "C-b t" #'toggle-the-bar)

;;; Panes are windows and the keys for them are the ones an editor uses for
;;; windows: 2 splits below, 3 splits beside, 0 closes this one, 1 leaves only
;;; this one, o goes to the next.

(atty/mode:define-key 'pane-mode "C-b 2" #'split-below)
(atty/mode:define-key 'pane-mode "C-b 3" #'split-right)
(atty/mode:define-key 'pane-mode "C-b 0" #'close-pane)
(atty/mode:define-key 'pane-mode "C-b 1" #'only-this-pane)
(atty/mode:define-key 'pane-mode "C-b o" #'next-pane)

(atty/mode:define-key 'pane-mode "C-b ," #'name-this-pane)
(atty/mode:define-key 'pane-mode "C-b a" #'go-to-the-blocked)
(atty/mode:define-key 'pane-mode "C-b z" #'zoom-this-pane)
(atty/mode:define-key 'pane-mode "C-b n" #'needs-you)
(atty/mode:define-key 'pane-mode "C-b w" #'switchboard)
(atty/mode:define-key 'pane-mode "C-b e" #'explain-this-pane)
(atty/mode:define-key 'pane-mode "C-b c" #'new-session)
(atty/mode:define-key 'pane-mode "C-b b" #'choose-a-session)

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

(atty/mode:define-key 'pane-mode "C-b [" #'scroll-mode)
(atty/mode:define-key 'pane-mode "C-b PageUp" #'scroll-mode-page-up)

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

;;; Everything above is a default. Whoever wants another key for a command, or
;;; another command for a key or a button, says so in a file of their own, read
;;; when atty starts: (atty/mode:define-key 'pane-mode "M-wheel-up" #'scroll-page-up)

(defun user-init-file ()
  (let ((config (sb-ext:posix-getenv "XDG_CONFIG_HOME")))
    (merge-pathnames "atty/init.lisp"
                     (if (and config (plusp (length config)))
                         (concatenate 'string (string-right-trim "/" config) "/")
                         (merge-pathnames ".config/" (user-homedir-pathname))))))

(defun load-user-init (&optional (file (user-init-file)))
  "Load FILE if there is one, in this package. One that does not load is said
and passed over: a slip in somebody's own keys is not a reason not to start."
  (when (probe-file file)
    (handler-case (let ((*package* (find-package '#:atty)))
                    (load file)
                    t)
      (error (e)
        (format *error-output* "~&atty: ~A did not load: ~A~%" file e)
        nil))))
