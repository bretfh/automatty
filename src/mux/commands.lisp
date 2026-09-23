;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What the keys do, and which keys do it.

(defcommand (detach :group sessions)
  "leave the session running and give the terminal back"
  (client-stop *client* :detached))

(defcommand (redraw :group asking)
  "draw the whole screen again"
  (client-redraw *client*))

(defgeneric message-for-command (client form))

(defun send-to-server (form)
  (message-for-command *client* form))

(defmethod message-for-command ((client client) form)
  "Say FORM to the server holding this session, or say why not.

The server is usually older than the client that reached it: it has been running
since whenever, and this was started a moment ago. A key it has never heard of
that quietly does nothing looks exactly like a key that is broken, so it says so
instead."
  (if (member (first form) (client-message-types client))
      (wire-send (client-wire client) form)
      (show-note client "not here"
                 (format nil "The server holding this session cannot ~(~A~).~%~%~
                              It has been running since before that was added.~%~
                              What is in it goes on running; a session started~%~
                              now knows the whole of it."
                         (first form))
                 :face :warning)))

(defcommand (bar-off :group asking)
  "take the bar off, for everybody on the session"
  (send-to-server (list :bar nil)))

(defcommand (bar-on :group asking)
  "put the bar back"
  (send-to-server (list :bar t)))

(defcommand (toggle-bar :group asking)
  "the bar off or on"
  (send-to-server (list :bar :toggle)))

(defcommand (commands :group asking)
  "run any command by name; the palette's first tab"
  (open-palette *client* :commands))

(defcommand (split-right :group panes)
  "another pane beside this one"
  (send-to-server (list :split :across)))

(defcommand (split-below :group panes)
  "another pane under this one"
  (send-to-server (list :split :down)))

(defcommand (next-pane :group panes)
  "the focus to the next pane in this window"
  (send-to-server (list :focus)))

(defcommand (close-pane :group panes)
  "close this pane and let its program go"
  (send-to-server (list :close)))

(defcommand (delete-other-panes :group panes)
  "close every other pane in this window"
  (send-to-server (list :only)))

(defcommand (new-session :group sessions)
  "another session, started where this pane is"
  (send-to-server (list :new)))

;;; Windows. Each names the session it is on, since the server takes the same
;;; forms from a client on another session and from the command line.

(defcommand (new-window :group windows)
  "another window in this session, after this one"
  (send-to-server (list :new-window (client-session *client*))))

(defcommand (next-window :group windows)
  "show the next window"
  (send-to-server (list :next-window (client-session *client*))))

(defcommand (previous-window :group windows)
  "show the window before"
  (send-to-server (list :previous-window (client-session *client*))))

(defcommand (close-window :group windows)
  "close this window and every program in it, after a yes"
  (confirm *client* "close this window and every program in it?"
           :yes (lambda (c)
                  (let ((*client* c))
                    (send-to-server (list :close-window (client-session c)))))))

(defcommand (switch-session :group sessions)
  "every session and its windows, to go to one; the same list as @"
  (send-to-server (list :sessions)))

(defcommand (clients :group sessions)
  "who is attached to this server, and what each is looking at; RET goes there, C-RET detaches one"
  (open-palette *client* :clients))

(defcommand (switch-window :group windows)
  "every session › window, the ones asking first; RET goes, C-RET makes one, TAB its panes"
  (send-to-server (list :sessions)))

(defcommand (rename-window :group windows)
  "what to call this window; TAB names the pane instead"
  (send-to-server (list :naming-window)))

(defcommand (send-prefix :group asking)
  "send the prefix itself to the pane"
  (send-to-server (list :keys (string +prefix+))))

(defcommand (rename-pane :group panes)
  "what to call this pane; TAB names the window instead"
  (send-to-server (list :naming)))

(defun prompt-session-name (client session)
  "What to call SESSION, on the line at the foot, starting from its name."
  (entry client "name" (format nil "session ~A" session) session
         :keep (lambda (typed c)
                 (let ((*client* c)) (send-to-server (list :name-session session typed))))))

(defcommand (rename-session :group sessions)
  "what to call this session"
  (when (client-session *client*)
    (prompt-session-name *client* (client-session *client*))))

(defcommand (goto-blocked-pane :group agents)
  "go to whatever has been asking longest, in any session"
  (send-to-server (list :go-to-blocked)))

(defcommand (zoom-pane :group panes)
  "this pane has the whole window, or gives it back"
  (send-to-server (list :zoom)))

(defun server-supports-p (what)
  (member what (client-message-types *client*)))

(defun send-if-supported (form)
  "Say FORM to the server if it has heard of it, and nothing at all if not. For
what a mouse does: a wheel that does nothing on a server from before it did
anything is what that server always did, and a note for every notch is worse."
  (when (server-supports-p (first form))
    (wire-send (client-wire *client*) form)))

(defcommand (mouse-clicked :unlisted)
  (send-to-server (list :mouse-at (car *mouse-position*) (cdr *mouse-position*))))

;;; The mouse, passed on. Which button and where is the server's to make sense
;;; of: it knows what was drawn there, and whether the program under it asked
;;; to be told.

(defun mouse-modifiers ()
  "The modifiers held with what the mouse just did."
  (loop :for mod :in '(:shift :meta :ctrl)
        :when (getf *mouse-event* mod) :collect mod))

(defun send-pointer-event (what)
  (let ((button (or (getf *mouse-event* :button) :left)))
    (cond ((and (eq what :press) (eq button :left)
                (some (lambda (over) (overlay-clicked over (cdr *mouse-position*) (car *mouse-position*) *client*))
                      (client-overlays *client*)))
           ;; something drawn over the session took the click: the drawer's answers, say
           nil)
          ((server-supports-p :pointer)
           (wire-send (client-wire *client*)
                      (list :pointer what button (car *mouse-position*) (cdr *mouse-position*)
                            (mouse-modifiers))))
          ;; a server from before buttons were passed on still knows a click
          ((and (eq what :press) (eq button :left)) (mouse-clicked)))))

(defcommand (mouse-pressed :unlisted) (send-pointer-event :press))
(defcommand (mouse-dragged :unlisted) (send-pointer-event :drag))
(defcommand (mouse-released :unlisted) (send-pointer-event :release))

;;; Reading a pane back. Every one of these is about the pane under the pointer
;;; when a mouse asked for it and the pane with the focus when a key did.

(defun scroll-by (amount)
  (send-if-supported
   (list :scroll amount (car *mouse-position*) (cdr *mouse-position*))))

(defun send-wheel (way)
  (send-if-supported
   (list :wheel way (car *mouse-position*) (cdr *mouse-position*) (mouse-modifiers))))

(defcommand natural-scroll-up (send-wheel :up))
(defcommand natural-scroll-down (send-wheel :down))
(defcommand natural-scroll-left (send-wheel :left))
(defcommand natural-scroll-right (send-wheel :right))

(defcommand scroll-up (scroll-by 1))
(defcommand scroll-down (scroll-by -1))
(defcommand scroll-up-line (scroll-by 3))
(defcommand scroll-down-line (scroll-by -3))
(defcommand scroll-page-up (scroll-by :page-up))
(defcommand scroll-page-down (scroll-by :page-down))
(defcommand scroll-half-page-up (scroll-by :half-up))
(defcommand scroll-half-page-down (scroll-by :half-down))
(defcommand scroll-to-top (scroll-by :top))
(defcommand scroll-to-bottom (scroll-by :bottom))

(defcommand (toggle-scrollbars :group scrolling)
  "the scrollbar column off or on, for the programs"
  (send-to-server (list :scrollbars :toggle)))

;;; Scroll mode: the keys that read back, with no prefix in front of them, for
;;; as long as it is on. Nothing typed reaches the pane until it is left, and
;;; leaving it is back to live.

;;; The one key to learn. Press the prefix and wait, and a menu rises from
;;; the foot with every key that can follow it, grouped by what it acts on.
;;; It draws only after a moment, so a chord typed straight through never
;;; sees it, and it waits as long as anyone needs.

;;; A mode holds these, so another mode may be defined on top of this one and
;;; change or add to what is here without touching any of it.

(defcommand (reload-init :group asking)
  "have the server read the init file again, and tell every client what it says"
  (send-to-server (list :reload-init)))

;;; Everything behind the prefix, as what follows it. They are bound from
;;; whatever the prefix is, so setting it in an init file moves every one of
;;; them; :prefix is the prefix itself, which sends one to the pane.

;;; A click is looked up the same as any other key, unprefixed: a mouse's
;;; buttons are always the multiplexer's, the way a keyboard's letters are
;;; always the pane's until C-b says otherwise.

;;; The wheel is whoever's is under it. With shift or meta held it reads the
;;; pane back whatever is running there, both because which of them a terminal
;;; keeps for itself depends on the terminal.
