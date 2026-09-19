;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx)

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

(defcommand what-the-keys-do
  (show-note *client* "keys"
             (format nil "~{~A~%~}"
                     (loop :for (chord . nil) :in (vtx/mode:keys-in-force
                                                   (vtx/mode:mode-named 'pane-mode))
                           :collect (format nil "  ~A" chord)))
             :face :accent))

;;; A mode holds these, so another mode may be defined on top of this one and
;;; change or add to what is here without touching any of it.

(vtx/mode:define-key 'pane-mode "C-b d" #'detach)
(vtx/mode:define-key 'pane-mode "C-b r" #'redraw)
(vtx/mode:define-key 'pane-mode "C-b :" #'run-a-command)
(vtx/mode:define-key 'pane-mode "C-b ?" #'what-the-keys-do)
(vtx/mode:define-key 'pane-mode "C-b C-b" #'send-the-prefix)
(vtx/mode:define-key 'pane-mode "C-b t" #'toggle-the-bar)

;;; Panes are windows and the keys for them are the ones an editor uses for
;;; windows: 2 splits below, 3 splits beside, 0 closes this one, 1 leaves only
;;; this one, o goes to the next.

(vtx/mode:define-key 'pane-mode "C-b 2" #'split-below)
(vtx/mode:define-key 'pane-mode "C-b 3" #'split-right)
(vtx/mode:define-key 'pane-mode "C-b 0" #'close-pane)
(vtx/mode:define-key 'pane-mode "C-b 1" #'only-this-pane)
(vtx/mode:define-key 'pane-mode "C-b o" #'next-pane)

(vtx/mode:define-key 'pane-mode "C-b c" #'new-session)
(vtx/mode:define-key 'pane-mode "C-b b" #'choose-a-session)
