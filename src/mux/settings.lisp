;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What can be set from outside: each a variable with a default, a name it is
;;; asked for by, and a line saying what it is. The line is what `atty help
;;; init' prints, so what is settable and what the docs say are one list.

(defvar *init-problem* nil
  "Why the init file did not load, or nil when it did or there is none.")

(defvar *settings* nil
  "Every setting in the order it was defined: (key symbol default doc check).")

(defun setting-key-of (symbol)
  "+WHEEL-ROWS+ is asked for as :wheel-rows."
  (intern (string-trim "+*" (symbol-name symbol)) :keyword))

(defmacro defsetting (name default doc &key check)
  "A DEFPARAMETER that can be set by name with CONFIGURE. CHECK, given a value
somebody offered, answers the value to keep or signals why not."
  `(progn
     (defparameter ,name ,default ,doc)
     (let ((entry (list (setting-key-of ',name) ',name ,default ,doc ,check)))
       (setf *settings* (append (remove (first entry) *settings* :key #'first)
                                (list entry))))
     ',name))

(defun setting-entry (key)
  (or (find key *settings* :key #'first)
      (error "there is no setting called ~S; there are ~{~(~S~)~^, ~}"
             key (mapcar #'first *settings*))))

(defun setting (key)
  "What the setting called KEY is now."
  (symbol-value (second (setting-entry key))))

(defun (setf setting) (value key)
  (let* ((entry (setting-entry key))
         (check (fifth entry))
         (kept (if check (funcall check value) value)))
    (setf (symbol-value (second entry)) kept)
    (let ((after (getf (symbol-plist (second entry)) 'after-setting)))
      (when after (funcall after kept)))
    kept))

(defun configure (&rest plist)
  "(configure :wheel-rows 6 :prefix \"C-a\"): set each setting named to what
follows it. A name nothing is called is an error naming what there is, so a
slip in an init file says so rather than doing nothing."
  (loop :for (key value) :on plist :by #'cddr
        :do (setf (setting key) value))
  plist)

(defun settings ()
  "Every setting: (key value default doc)."
  (loop :for (key symbol default doc) :in *settings*
        :collect (list key (symbol-value symbol) default doc)))

(defun after-setting (symbol does)
  "Run DOES with the new value whenever SYMBOL is set through CONFIGURE."
  (setf (getf (symbol-plist symbol) 'after-setting) does))

(defun a-number (least)
  (lambda (value)
    (unless (and (integerp value) (>= value least))
      (error "~S is not a whole number of at least ~D" value least))
    value))

(defun a-flag (value) (and value t))

(defun a-prefix (value)
  "The prefix as a character: given as one, or as a chord like \"C-a\"."
  (etypecase value
    (character value)
    (string
     (let ((key (atty/mode:parse-key value)))
       (cond ((and (atty/mode:key-ctrl key) (not (atty/mode:key-meta key))
                   (not (atty/mode:key-super key))
                   (= 1 (length (atty/mode:key-sym key)))
                   (alpha-char-p (char (atty/mode:key-sym key) 0)))
              (code-char (- (char-code (char-downcase (char (atty/mode:key-sym key) 0))) 96)))
             ((and (not (atty/mode:key-ctrl key)) (= 1 (length (atty/mode:key-sym key))))
              (char (atty/mode:key-sym key) 0))
             (t (error "~S is not a key the prefix can be: one character, or C- and a letter"
                       value)))))))

;;; The settings themselves. The ones about a thing are next to it; these are
;;; the ones that are about atty as a whole.

(defsetting +prefix+ (code-char 2)
  "The key that says the next one is for atty rather than for the pane; C-b."
  :check #'a-prefix)

(defsetting +wheel-rows+ 3
  "How many rows one notch of the wheel scrolls."
  :check (a-number 1))

(defsetting +hold-after+ 350
  "Milliseconds an arrow or the track of a scrollbar is held before it repeats."
  :check (a-number 0))

(defsetting +hold-every+ 50
  "Milliseconds between repeats once a held arrow or track does."
  :check (a-number 1))

(defsetting +scrollbars-by-default+ t
  "Whether a new session's panes have scrollbars; toggle scrollbars changes one session."
  :check #'a-flag)

(defsetting +bar-by-default+ t
  "Whether a new session has the bar; C-b t changes one session."
  :check #'a-flag)

(defsetting +max-scrollback+ 10000
  "How many rows behind its screen a pane keeps."
  :check (a-number 0))

(defsetting +log-length+ 256
  "How many entries a pane's log of who typed into it keeps."
  :check (a-number 2))

(defsetting +restore-command+ :shell
  "What a pane brought back from disk runs: :shell restarts a shell and stands a
shell in for anything else; :same restarts whatever it ran; a function is given
the saved pane and answers a command."
  :check (lambda (value)
           (unless (or (member value '(:shell :same)) (functionp value))
             (error "~S is not :shell, :same or a function" value))
           value))

(defsetting +saved-scrollback+ 10000
  "How many rows of a pane's scrollback are saved to disk."
  :check (a-number 0))

(defsetting +save-quiet-after+ 5000
  "Milliseconds a pane has to be quiet before what it holds is saved."
  :check (a-number 0))

(defsetting +save-at-most-every+ 60000
  "Milliseconds between saves of a pane that never goes quiet."
  :check (a-number 1000))

;;; The theme is a setting that is not a variable of atty's: it is the ui's.

(defun a-theme (value)
  (let ((name (intern (string-upcase (string value)) :keyword)))
    (unless (member name (atty/ui:themes))
      (error "there is no theme called ~S; there are ~{~(~S~)~^, ~}" value (atty/ui:themes)))
    name))

(defsetting +theme+ :ef-dream
  "Which theme the frames, the bar and the overlays are coloured by."
  :check #'a-theme)

(after-setting '+theme+ (lambda (name) (setf (atty/ui:active) name)))

;;; Hooks: lists of functions run when something happens. One that comes
;;; apart is shown, or said to the log when nobody is looking, and the rest
;;; still run: a slip in somebody's init is not a reason for atty to stop.

(defvar *hooks* nil "Every hook: (name symbol doc).")

(declaim (ftype function show-broke))

(defmacro defhook (name doc)
  (let ((symbol (intern (format nil "*~A-HOOK*" (symbol-name name)))))
    `(progn
       (defvar ,symbol nil ,doc)
       (setf *hooks* (append (remove ',name *hooks* :key #'first)
                             (list (list ',name ',symbol ,doc))))
       ',symbol)))

(defun hook-symbol (name)
  (or (second (find name *hooks* :key #'first))
      (error "there is no hook called ~S; there are ~{~(~S~)~^, ~}"
             name (mapcar #'first *hooks*))))

(defun add-hook (name does)
  "Run DOES when NAME happens, after whatever was already there."
  (let ((symbol (hook-symbol name)))
    (unless (member does (symbol-value symbol))
      (setf (symbol-value symbol) (append (symbol-value symbol) (list does))))
    does))

(defun remove-hook (name does)
  (let ((symbol (hook-symbol name)))
    (setf (symbol-value symbol) (remove does (symbol-value symbol)))
    does))

(defun run-hook (name &rest args)
  "Run everything on the hook called NAME with ARGS. Answers how many ran."
  (let ((ran 0)
        (client (and (boundp '*client*) (symbol-value '*client*))))
    (dolist (does (symbol-value (hook-symbol name)) ran)
      (handler-case (progn (apply does args) (incf ran))
        (error (e)
          (if client
              (show-broke client (format nil "the ~(~A~) hook" name) e)
              (format *error-output* "~&atty: the ~(~A~) hook came apart: ~A~%" name e)))))))

(defhook session-made "A session was made: (session).")
(defhook pane-started "A pane's program was started: (session pane).")
(defhook pane-ended "A pane was closed: (session pane).")
(defhook client-attached "This client is attached and has been greeted: (client).")
(defhook before-save "The server is about to save its state to disk: (server).")
(defhook server-restored "The server brought its sessions back from disk: (server sessions).")
