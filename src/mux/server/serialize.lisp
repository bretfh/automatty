;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What the server holds, on disk: the tree of sessions, windows and panes in
;;; one file, and what each pane holds, screen and scrollback with their faces,
;;; in a file of its own. Written while the server runs and read when one
;;; starts, so a reboot, a crash or a newer build does not lose anybody's
;;; panes. The programs in them cannot be kept, only what they showed and where
;;; they were; those are started again.

(defparameter +state-version+ 1)

(defvar *state-home* nil
  "Where state is kept, when not $XDG_STATE_HOME or ~/.local/state. Bound by
the tests so they never touch anybody's real state.")

(defun state-home ()
  (let ((env (sb-ext:posix-getenv "XDG_STATE_HOME")))
    (uiop:ensure-directory-pathname
     (or *state-home*
         (and env (plusp (length env)) env)
         (merge-pathnames ".local/state/" (user-homedir-pathname))))))

(defun state-dir (&optional (name (server-name)))
  "Where the server called NAME keeps its state, made if it is not there and
shut to everybody else: a saved screen is somebody's shell history."
  (let ((dir (ensure-directories-exist
              (merge-pathnames (format nil "atty/~A/panes/" (server-file-name name "a server"))
                               (state-home)))))
    (ignore-errors (sb-posix:chmod (namestring (merge-pathnames "../" dir)) #o700))
    (uiop:ensure-directory-pathname (merge-pathnames "../" dir))))

(defun server-state-dir (server)
  (state-dir (file-namestring (server-path server))))

(defun tree-file (dir) (merge-pathnames "tree.sexp" dir))

(defun pane-file (dir id) (merge-pathnames (format nil "panes/~D.sexp" id) dir))

(defun pane-files (dir)
  "Every pane file in DIR with the id it is for."
  (loop :for path :in (directory (merge-pathnames "panes/*.sexp" dir))
        :for id := (parse-integer (pathname-name path) :junk-allowed t)
        :when id :collect (cons id path)))

;;; Files are written whole and renamed into place, so whatever is there is
;;; always a whole file: a crash halfway through a write leaves the last one.

(defun write-form-atomically (path form)
  "Write FORM to PATH through a file beside it, so PATH is never half written.
Answers whether it was written; what went wrong is said to the log."
  (let* ((path (namestring path))
         (tmp (format nil "~A.tmp-~D" path (sb-posix:getpid))))
    (handler-case
        (progn
          (with-open-file (out tmp :direction :output :if-exists :supersede
                                   :external-format :utf-8)
            (with-standard-io-syntax
              (let ((*print-readably* nil) (*print-pretty* nil))
                (prin1 form out)
                (terpri out)))
            (finish-output out)
            (sb-posix:fsync (sb-sys:fd-stream-fd out)))
          (ignore-errors (sb-posix:chmod tmp #o600))
          (sb-posix:rename tmp path)
          t)
      (error (e)
        (ignore-errors (delete-file tmp))
        (format *error-output* "~&atty: ~A was not written: ~A~%" path e)
        nil))))

(defun read-state-file (path tag)
  "What is in the state file at PATH, which should begin (TAG version ...).
Answers the form and one of :ok, :missing, :truncated or :wrong-version. A
version this build does not know is moved aside, never deleted: a newer build
may know it."
  (let ((path (namestring path)))
    (if (not (probe-file path))
        (values nil :missing)
        (let ((form (handler-case
                        (with-open-file (in path :external-format :utf-8)
                          (with-standard-io-syntax
                            (let ((*read-eval* nil)
                                  (*package* (message-package)))
                              (read in))))
                      (error () :truncated))))
          (cond
            ((eq form :truncated) (values nil :truncated))
            ((and (consp form) (eq (first form) tag) (eql (second form) +state-version+))
             (values form :ok))
            (t
             (ignore-errors
              (sb-posix:rename path (format nil "~A.unread-~A" path
                                            (if (consp form) (second form) "what"))))
             (values nil :wrong-version)))))))

;;; A row as data: its width and the runs of one face in it, with the faces
;;; said once per file. Rows in the scrollback are as wide as the pane was when
;;; they went off its screen, so each says its own width.

(defun face-index (face faces table)
  "Which entry of TABLE FACE is, put there if it is not yet. Nought is no face."
  (if (or (null face) (term:face-default-p face))
      0
      (let ((said (encode-face face)))
        (or (gethash said faces)
            (progn (vector-push-extend said table)
                   (setf (gethash said faces) (1- (fill-pointer table))))))))

(defun encode-row (row faces table)
  "ROW as (width spans), a span being (face-index . text). What is blank in no
face at the end is left off."
  (let* ((width (term:row-width row))
         (end (loop :for x :from (1- width) :downto 0
                    :unless (and (char= #\Space (term:row-char row x))
                                 (zerop (face-index (term:row-face row x) faces table)))
                      :return (1+ x)
                    :finally (return 0)))
         (spans nil)
         (from 0))
    (loop :for x :from 1 :to end
          :do (when (or (= x end)
                        (/= (face-index (term:row-face row x) faces table)
                            (face-index (term:row-face row from) faces table)))
                (push (cons (face-index (term:row-face row from) faces table)
                            (subseq (term:row-chars row) from x))
                      spans)
                (setf from x)))
    (list width (nreverse spans))))

(defun decode-row (said seen)
  "A fresh row from what ENCODE-ROW answered, SEEN being the faces as objects."
  (destructuring-bind (width spans) said
    (let ((row (term:make-row width))
          (x 0))
      (dolist (span spans row)
        (let ((face (svref seen (car span)))
              (text (cdr span)))
          (replace (term:row-chars row) text :start1 x)
          (when face
            (fill (term:row-faces row) face :start x :end (min width (+ x (length text)))))
          (incf x (length text)))))))

;;; A pane as data, and back.

(defun last-shown-row (term)
  "The last row of TERM's main screen with anything on it, or -1."
  (let ((grid (if (term:term-in-alt-screen term) (term:term-main-grid term) nil)))
    (loop :for y :from (1- (term:term-height term)) :downto 0
          :for row := (if grid (aref grid y) (term:term-grid-row term y))
          :unless (every (lambda (ch) (char= ch #\Space)) (term:row-chars row))
            :return y
          :finally (return -1))))

(defun main-row (term y)
  (if (term:term-in-alt-screen term)
      (aref (term:term-main-grid term) y)
      (term:term-grid-row term y)))

(defun last-row-on (term)
  "The last row of what TERM shows with anything on it, or -1."
  (loop :for y :from (1- (term:term-height term)) :downto 0
        :unless (every (lambda (ch) (char= ch #\Space))
                       (term:row-chars (term:term-grid-row term y)))
          :return y
        :finally (return -1)))

(defun relative-ages (entries now)
  "ENTRIES, each beginning with a moment on the monotonic clock, with that
moment said as how long ago it was: the clock means nothing to another process."
  (mapcar (lambda (entry) (cons (max 0 (- now (first entry))) (rest entry))) entries))

(defun moments (entries now)
  (mapcar (lambda (entry) (cons (- now (first entry)) (rest entry))) entries))

(defun encode-pane (pane now)
  "PANE as it goes to disk: what it ran and where, what it was called, its
log, and every row it holds, oldest first, with the faces said once."
  (let* ((term (pane-term pane))
         (agent (pane-agent pane))
         (faces (make-hash-table :test 'equal))
         (table (make-array 8 :adjustable t :fill-pointer 1 :initial-element nil))
         (kept (term:term-scrollback-size term))
         (from (max 0 (- kept +saved-scrollback+)))
         (behind (loop :for i :from from :below kept
                       :collect (encode-row (term:term-scrollback-row term i) faces table)))
         (shown (loop :for y :from 0 :to (last-shown-row term)
                      :collect (encode-row (main-row term y) faces table)))
         ;; a full-screen program's screen is what was in front of somebody
         ;; when the server stopped, and the main screen is what was under it
         (over (and (term:term-in-alt-screen term)
                    (loop :for y :from 0 :to (last-row-on term)
                          :collect (encode-row (term:term-grid-row term y) faces table)))))
    (list :atty-pane +state-version+
          :id (pane-id pane)
          :saved (get-universal-time)
          :command (pane-command pane)
          :directory (pane-directory pane)
          :label (pane-label pane)
          :named (pane-named pane)
          :rows (term:term-height term)
          :cols (term:term-width term)
          :alt-screen (and (term:term-in-alt-screen term) t)
          :programs (pane-programs pane)
          :title (term:term-title term)
          :queued (pane-pending-prompt pane)
          :told (and (agent::agent-told agent) (agent:agent-heard agent))
          :since-clock (agent:agent-since-clock agent)
          :states (relative-ages (agent:agent-history agent) now)
          :log (relative-ages (pane-log pane) now)
          :faces (coerce table 'simple-vector)
          :behind behind
          :screen shown
          :over over)))

(defun shell-command-p (command)
  (member (program-name command) +shells+ :test #'string=))

(defun restore-command (form)
  "What a pane brought back from FORM runs, by +RESTORE-COMMAND+."
  (let ((command (getf (nthcdr 2 form) :command))
        (policy +restore-command+))
    (cond ((eq policy :same) command)
          ((functionp policy) (or (funcall policy form) (default-shell)))
          ((and command (shell-command-p command)) command)
          (t (default-shell)))))

(defun resume-command (command)
  "What picks up the work of a pane that ran COMMAND, when something is known to."
  (cdr (assoc (program-name command) +resume-commands+ :test #'string=)))

(defun restore-rule-text (saved command directory same)
  "What the rule under a restored pane says: when, and when its program is not
what ran there, what did, where, and what brings it back."
  (format nil "restored ~A~@[ · was: ~A~]~@[ in ~A~]~@[ · ~A picks it up~]"
          (format-day-time (or saved (get-universal-time)))
          (and (not same) command)
          (and (not same) directory (abbreviate-directory directory))
          (and (not same) (resume-command command))))

(defun divider-row (width text)
  "A row of rule with TEXT set into it, drawn faint: what says where what was
brought back ends and what the program started since begins."
  (let ((row (term:make-row width))
        (face (term:make-face :faint t))
        (said (format nil " ~A " text)))
    (dotimes (x width)
      (setf (term:row-char row x) #\─
            (term:row-face row x) face))
    (loop :for ch :across said
          :for x :from 2 :below width
          :do (setf (term:row-char row x) ch))
    row))

(defun push-rows-to-scrollback (term rows)
  (dolist (row rows) (term:push-scrollback term row)))

(defun write-rows-to-term (term rows)
  "ROWS onto TERM's screen from the cursor down, one a line, the cursor left at
the start of the line after the last: as though the program had written them.
What goes off the top goes behind, the way it would have."
  (dolist (row rows)
    (let ((into (term:term-grid-row term (term:term-cursor-y term)))
          (n (min (term:row-width row) (term:term-width term))))
      (replace (term:row-chars into) (term:row-chars row) :end2 n)
      (replace (term:row-faces into) (term:row-faces row) :end2 n)
      (setf (term:term-cursor-x term) 0)
      (term:term-line-feed term))))

(defun decode-pane (form now)
  "A pane from what ENCODE-PANE wrote, with everything it held behind a screen
its program starts afresh on. Answers the pane and a note when anything about
it could not be as it was."
  (destructuring-bind (&key id saved command directory label named rows cols
                            programs title queued told since-clock states log
                            faces behind screen over &allow-other-keys)
      (nthcdr 2 form)
    (let* ((runs (restore-command form))
           (same (equal runs command))
           (directory (and directory (probe-file directory) directory))
           (note (and (getf (nthcdr 2 form) :directory) (null directory)
                      (format nil "pane ~D was in ~A, which is gone; it starts at home"
                              id (getf (nthcdr 2 form) :directory))))
           (pane (make-pane runs :id id :rows rows :cols cols :directory directory))
           (term (pane-term pane))
           (seen (map 'simple-vector #'decode-face faces)))
      ;; what was behind the screen goes behind it; what was on it goes on
      ;; it, a full-screen program's screen after that since it was what was
      ;; in front, with the rule under that and the program's first line
      ;; under the rule, so it looks the way it did with one line saying what
      ;; happened
      (push-rows-to-scrollback term (mapcar (lambda (said) (decode-row said seen)) behind))
      (write-rows-to-term term (append (mapcar (lambda (said) (decode-row said seen)) screen)
                              (mapcar (lambda (said) (decode-row said seen)) over)
                              (list (divider-row cols (restore-rule-text saved command
                                                                     (getf (nthcdr 2 form) :directory)
                                                                     same)))))
      (setf (pane-pushed-seen pane) (term:term-scrollback-pushed term)
            (pane-label pane) label
            (pane-named pane) named
            (pane-programs pane) programs
            (pane-pending-prompt pane) (and same queued)
            (pane-log pane) (moments log now)
            (pane-log-count pane) (length log))
      (let ((agent (pane-agent pane)))
        (when told (agent:agent-hear agent told))
        (setf (agent:agent-history agent) (moments states now)
              (agent::agent-historied agent) (length states)
              (agent:agent-since-clock agent) since-clock)
        (agent:agent-become agent :title (or title named) :command command
                                  :programs programs))
      (unless same
        (pane-push-log pane now '(:atty) :restored (format nil "was: ~A" command)))
      (values pane note))))

(defun make-empty-pane (id rows cols why)
  "A pane standing in for one whose file could not be read."
  (let ((pane (make-pane (default-shell) :id id :rows rows :cols cols)))
    (write-rows-to-term (pane-term pane)
               (list (divider-row cols (format nil "restored; what it held is ~A" why))))
    (setf (pane-pushed-seen pane) (term:term-scrollback-pushed (pane-term pane)))
    pane))

;;; The tree as data, and back.

(defun encode-window (window)
  (list :label (window-label window)
        :layout (encode-layout (window-layout window))
        :focus (and (window-focus window) (pane-id (window-focus window)))
        :zoomed (and (window-zoomed window) (pane-id (window-zoomed window)))))

(defun encode-session (session)
  (list :name (session-name session)
        :rows (session-rows session) :cols (session-cols session)
        :bar (session-bar-p session) :scrollbars (session-scrollbars-p session)
        :search-kind (session-field-kind session)
        :window (or (window-number session (session-window session)) 1)
        :windows (mapcar #'encode-window (session-windows session))))

(defun encode-tree (server)
  "The server's sessions, windows and panes as they go to disk: without when,
so what it was last written as can be compared to what it is now."
  (list :atty-state +state-version+
        :server (file-namestring (server-path server))
        :panes-made *panes-made*
        :sessions (mapcar #'encode-session (server-sessions server))))

(defun decode-layout (said panes)
  "A layout from what ENCODE-LAYOUT wrote, with PANES the panes by id. A pane
that is not there is left out, and a split left with one part is that part."
  (cond ((integerp said) (gethash said panes))
        ((consp said)
         (let ((parts (remove nil (mapcar (lambda (part) (decode-layout part panes))
                                          (rest said)))))
           (cond ((null parts) nil)
                 ((null (rest parts)) (first parts))
                 (t (make-split (first said) parts)))))
        (t nil)))

(defun decode-window (form panes)
  (destructuring-bind (&key label layout focus zoomed &allow-other-keys) form
    (let ((layout (decode-layout layout panes)))
      (when layout
        (let ((in (layout-panes layout)))
          (%make-window :label label :layout layout
                        :focus (or (find focus in :key #'pane-id) (first in))
                        :zoomed (find zoomed in :key #'pane-id)))))))

(defun decode-session (server form panes)
  (destructuring-bind (&key name rows cols bar scrollbars search-kind window windows
                       &allow-other-keys)
      form
    (let ((made (remove nil (mapcar (lambda (w) (decode-window w panes)) windows))))
      (when made
        (%make-session :name name :rows rows :cols cols
                       :socket (server-path server) :server server
                       :bar-p bar :scrollbars-p scrollbars
                       :field-kind (or search-kind 0)
                       :windows made
                       :window (or (and window (nth (1- window) made)) (first made))
                       :screen (tty:make-screen :width cols :height rows))))))

(defun tree-pane-ids (tree)
  "Every pane id the saved TREE names, in the order the layouts name them."
  (let ((ids nil))
    (labels ((walk (said)
               (cond ((integerp said) (pushnew said ids))
                     ((consp said) (mapc #'walk (rest said))))))
      (dolist (session (getf (nthcdr 2 tree) :sessions))
        (dolist (window (getf session :windows))
          (walk (getf window :layout)))))
    (nreverse ids)))

;;; Saving: the tree when it has changed, each pane when it has been quiet a
;;; moment or has gone unsaved long enough, and everything when the server
;;; stops.

;;; Starting afresh, and what was saved when nothing is running.
