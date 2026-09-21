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
  (tell-the-server (list :sessions)))

(defcommand send-the-prefix
  (tell-the-server (list :keys (string +prefix+))))

(defcommand name-this-pane
  (tell-the-server (list :naming)))

(defcommand go-to-the-blocked
  (tell-the-server (list :go-to-blocked)))

(defcommand zoom-this-pane
  (tell-the-server (list :zoom)))

(defcommand (mouse-clicked :unlisted)
  (tell-the-server (list :mouse-at (car *mouse-at*) (cdr *mouse-at*))))

(defcommand (mouse-noticed :unlisted) nil)

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
(atty/mode:define-key 'pane-mode "C-b c" #'new-session)
(atty/mode:define-key 'pane-mode "C-b b" #'choose-a-session)

;;; A click is looked up the same as any other key, unprefixed: a mouse's
;;; buttons are always the multiplexer's, the way a keyboard's letters are
;;; always the pane's until C-b says otherwise.

(atty/mode:define-key 'pane-mode "mouse-1" #'mouse-clicked)
(atty/mode:define-key 'pane-mode "mouse-1-up" #'mouse-noticed)
(atty/mode:define-key 'pane-mode "mouse-2" #'mouse-noticed)
(atty/mode:define-key 'pane-mode "mouse-2-up" #'mouse-noticed)
(atty/mode:define-key 'pane-mode "mouse-3" #'mouse-noticed)
(atty/mode:define-key 'pane-mode "mouse-3-up" #'mouse-noticed)
(atty/mode:define-key 'pane-mode "wheel-up" #'mouse-noticed)
(atty/mode:define-key 'pane-mode "wheel-down" #'mouse-noticed)
