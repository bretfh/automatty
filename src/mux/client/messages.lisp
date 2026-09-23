;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun handle-hello (client name rows cols &optional knows)
  (setf (client-greeted client) t
        (client-session client) name
        (client-message-types client) (or knows +base-message-types+))
  (client-fit client rows cols)
  (host-write client tty:+blanked+)
  (unless (or (member :command (client-message-types client)) (client-warned-old-server client))
    (setf (client-warned-old-server client) t)
    (show-note client "init"
               (format nil "The server holding this session is from before it read~%~
                                                       the init file for everybody, so nothing in it is in force.~%~
                                                       atty restart-server starts it again from this build.")
               :face :warning)))

(defun handle-pane-screen (client session id &optional width said faces)
  (when width
    (setf (gethash (cons session id) (client-screens client))
          (let ((screen (tty:make-screen :width width
                                         :height (1+ (reduce #'max said
                                                             :key #'first
                                                             :initial-value 0)))))
            (decode-runs-into-screen screen said faces)
            screen)
          (client-dirty client) t)))

(defun handle-answered (client session id n outcome)
  (unless (eq outcome t)
    (show-note client "not answered"
               (format nil "~A:~D was not answered ~D: ~(~A~)."
                       session id n
                       (case outcome
                         (:not-blocked "it is not asking anything now")
                         (:no-such-option "it has no such answer")
                         (:gone "it is gone")
                         (t outcome)))))
  (wire-send (client-wire client) '(:lately 5)))

(defun handle-agent-prompted (client session id outcome)
  (unless (member outcome '(t :queued))
    (show-note client "not prompted"
               (format nil "~A:~D ~A" session id
                       (if (eq outcome :blocked)
                           "is asking something; answer it, not a prompt."
                           "is gone.")))))

(defun handle-found (client session id n at row &optional hits current)
  ;; the frame says it; the palette's find lists the hits; nothing
  ;; found is worth a word only when nothing is listing them
  (declare (ignore session id at row))
  (setf (client-found client) (list :n n :hits hits :current current)
        (client-dirty client) t)
  (let ((top (first (client-overlays client))))
    (if (and (typep top 'prompt) (eql #\/ (prompt-kind top)))
        (setf (prompt-index top) (or current 0))
        (when (zerop n) (show-note client "find" "nothing has that in it" :face :warning)))))

(defun handle-server-message (client form)
  (case (first form)
        (:hello (apply #'handle-hello client (rest form)))
        (:config (destructuring-bind (settings commands keys) (rest form)
                   (apply-config settings commands keys)
                   (setf (client-dirty client) t)))
        (:note (destructuring-bind (title text &optional (face :accent)) (rest form)
                 (show-note client title text :face face)))
        (:frame
         (destructuring-bind (said faces) (rest form)
                             (decode-runs-into-screen (client-from client) said faces)
                             (setf (client-dirty client) t)))
        (:cursor
         (destructuring-bind (y x visible style) (rest form)
                             (let ((screen (client-from client)))
                               (setf (tty:screen-cursor-y screen) y
                                     (tty:screen-cursor-x screen) x
                                     (tty:screen-cursor-visible screen) (and visible t)
                                     (tty:screen-cursor-style screen) style))
                             (setf (client-dirty client) t)))
        (:these (prompt-window client (second form)))
        (:bell (host-write client (string (code-char 7))))
        (:bracketed-paste
         (host-write client (format nil "~C[?2004~C" (code-char 27) (if (second form) #\h #\l))))
        (:do (run-command (second form) client))
        (:say (show-note client "atty" (second form) :face (or (third form) :accent)))
        (:you (setf (client-id client) (second form)))
        (:pane
         (let ((row (rest form)))
           (setf (gethash (cons (getf row :session) (getf row :id)) (client-panes client))
                 (list* :heard-at (client-ms) row)
                 (client-dirty client) t)))
        (:pane-gone
         (remhash (cons (second form) (third form)) (client-panes client))
         (remhash (cons (second form) (third form)) (client-screens client))
         (setf (client-dirty client) t))
        (:pane-screen (apply #'handle-pane-screen client (rest form)))
        (:lately (setf (client-recent client) (second form)
                       (client-dirty client) t))
        (:pulses (loop :for (session id cells) :in (second form)
                       :do (setf (gethash (cons session id) (client-pulses client))
                                 (mapcar (lambda (c) (cons (first c) (second c))) cells)))
                 (setf (client-dirty client) t))
        (:events (setf (client-events client) (second form)
                       (client-events-at client) (client-ms)
                       (client-dirty client) t))
        (:layouts (setf (gethash (second form) (client-layouts client)) (third form)
                        (client-dirty client) t))
        (:barp (setf (client-barp client) (and (second form) t)
                     (client-dirty client) t))
        (:clients (setf (client-clients client) (second form)
                        (client-dirty client) t))
        ((:agent-explained :pane-history :pane-log :pane-about)
         ;; what the drawer asked about a pane, kept by the pane and by what
         ;; it was, with when it came so ages in it can be brought up to now
         (destructuring-bind (session id &rest said) (rest form)
           (let ((key (cons session id)))
             (setf (getf (gethash key (client-pane-info client)) (first form))
                   (list* (client-ms) said)
                   (client-dirty client) t))))
        (:answered (apply #'handle-answered client (rest form)))
        (:agent-prompted (apply #'handle-agent-prompted client (rest form)))
        (:found (apply #'handle-found client (rest form)))
        (:copied
         ;; to the terminal the client sits in, the way a program would ask it
         (host-write client (format nil "~C]52;c;~A~C" (code-char 27) (base64 (second form)) (code-char 7)))
         (show-note client "copied" (format nil "~D line~:P" (1+ (count #\Newline (second form))))
                    :face :accent))
        (:read-it
         (destructuring-bind (session id lines) (rest form)
           (show-note client (format nil "~A:~D" session id)
                      (format nil "~{~A~%~}"
                              ;; the end of it, which is what is being asked
                              (last lines (max 1 (- (client-rows client) 3))))
                      :face :accent)))
        (:session-renamed
         (destructuring-bind (old new) (rest form)
           (client-session-renamed client old new)))
        (:session-named
         (destructuring-bind (old new how) (rest form)
           (case how
             (:empty (show-note client "name" "a session needs a name"))
             (:taken (show-note client "name" (format nil "there is already a session called ~A" new)))
             (:gone (show-note client "name" (format nil "no session is called ~A any more" old))))))
        (:name-it
         (destructuring-bind (session id label title &optional address) (rest form)
           (prompt-pane-name client session id label title :address address)))
        (:name-window-of
         (destructuring-bind (session n label) (rest form)
           (prompt-window-name client session n label)))
        (:version
         (let ((theirs (second form)))
           (unless (or (equal theirs *version*) *version-noted*)
             (setf *version-noted* t)
             (show-note client "version"
                        (format nil "This atty is ~A; the server holding this session is ~A.~%~
                                     atty restart-server starts it again from this build."
                                *version* theirs)
                        :face :warning))))
        (:bye (client-stop client (second form))
              (when (eq (second form) :restarting)
                (setf *successor* (third form))))
        (t nil)))
