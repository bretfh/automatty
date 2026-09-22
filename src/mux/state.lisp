;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

;;; What the server holds, on disk: the tree of sessions, windows and panes in
;;; one file, and what each pane holds, screen and scrollback with their faces,
;;; in a file of its own. Written while the server runs and read when one
;;; starts, so a reboot, a crash or a newer build does not lose anybody's
;;; panes. The programs in them cannot be kept, only what they showed and where
;;; they were; those are started again.

(declaim (ftype function a-shell server-name a-name))

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
              (merge-pathnames (format nil "atty/~A/panes/" (a-name name "a server"))
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
                                  (*package* (reading-package)))
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
      (let ((said (face-said face)))
        (or (gethash said faces)
            (progn (vector-push-extend said table)
                   (setf (gethash said faces) (1- (fill-pointer table))))))))

(defun row-said (row faces table)
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

(defun said-row (said seen)
  "A fresh row from what ROW-SAID answered, SEEN being the faces as objects."
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

(defun ages (entries now)
  "ENTRIES, each beginning with a moment on the monotonic clock, with that
moment said as how long ago it was: the clock means nothing to another process."
  (mapcar (lambda (entry) (cons (max 0 (- now (first entry))) (rest entry))) entries))

(defun moments (entries now)
  (mapcar (lambda (entry) (cons (- now (first entry)) (rest entry))) entries))

(defun pane-said (pane now)
  "PANE as it goes to disk: what it ran and where, what it was called, its
log, and every row it holds, oldest first, with the faces said once."
  (let* ((term (pane-term pane))
         (agent (pane-agent pane))
         (faces (make-hash-table :test 'equal))
         (table (make-array 8 :adjustable t :fill-pointer 1 :initial-element nil))
         (kept (term:term-scrollback-size term))
         (from (max 0 (- kept +saved-scrollback+)))
         (behind (loop :for i :from from :below kept
                       :collect (row-said (term:term-scrollback-row term i) faces table)))
         (shown (loop :for y :from 0 :to (last-shown-row term)
                      :collect (row-said (main-row term y) faces table))))
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
          :queued (pane-queued pane)
          :told (and (agent::agent-told agent) (agent:agent-heard agent))
          :since-clock (agent:agent-since-clock agent)
          :states (ages (agent:agent-history agent) now)
          :log (ages (pane-log pane) now)
          :faces (coerce table 'simple-vector)
          :behind behind
          :screen shown)))

(defun shell-command-p (command)
  (member (program-name command) +shells+ :test #'string=))

(defun command-to-restore (form)
  "What a pane brought back from FORM runs, by +RESTORE-COMMAND+."
  (let ((command (getf (nthcdr 2 form) :command))
        (policy +restore-command+))
    (cond ((eq policy :same) command)
          ((functionp policy) (or (funcall policy form) (a-shell)))
          ((and command (shell-command-p command)) command)
          (t (a-shell)))))

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

(defun day-and-time (universal-time)
  (multiple-value-bind (s m h day month) (decode-universal-time universal-time)
    (declare (ignore s))
    (format nil "~D ~A ~2,'0D:~2,'0D" day
            (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                              "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            h m)))

(defun push-rows (term rows)
  (dolist (row rows) (term:push-scrollback term row)))

(defun said-pane (form now)
  "A pane from what PANE-SAID wrote, with everything it held behind a screen
its program starts afresh on. Answers the pane and a note when anything about
it could not be as it was."
  (destructuring-bind (&key id saved command directory label named rows cols
                            programs title queued told since-clock states log
                            faces behind screen &allow-other-keys)
      (nthcdr 2 form)
    (let* ((runs (command-to-restore form))
           (same (equal runs command))
           (directory (and directory (probe-file directory) directory))
           (note (and (getf (nthcdr 2 form) :directory) (null directory)
                      (format nil "pane ~D was in ~A, which is gone; it starts at home"
                              id (getf (nthcdr 2 form) :directory))))
           (pane (make-pane runs :id id :rows rows :cols cols :directory directory))
           (term (pane-term pane))
           (seen (map 'simple-vector #'said-face faces)))
      (push-rows term (mapcar (lambda (said) (said-row said seen)) behind))
      (push-rows term (mapcar (lambda (said) (said-row said seen)) screen))
      (push-rows term (list (divider-row cols (format nil "restored ~A~@[ · was: ~A~]"
                                                      (day-and-time (or saved (get-universal-time)))
                                                      (and (not same) command)))))
      (setf (pane-pushed-seen pane) (term:term-scrollback-pushed term)
            (pane-label pane) label
            (pane-named pane) named
            (pane-programs pane) programs
            (pane-queued pane) (and same queued)
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
        (pane-logged pane now '(:atty) :restored (format nil "was: ~A" command)))
      (values pane note))))

(defun empty-pane-for (id rows cols why)
  "A pane standing in for one whose file could not be read."
  (let ((pane (make-pane (a-shell) :id id :rows rows :cols cols)))
    (term:push-scrollback (pane-term pane)
                          (divider-row cols (format nil "restored; what it held is ~A" why)))
    (setf (pane-pushed-seen pane) (term:term-scrollback-pushed (pane-term pane)))
    pane))

;;; The tree as data, and back.

(defun window-said (window)
  (list :label (window-label window)
        :layout (layout-said (window-layout window))
        :focus (and (window-focus window) (pane-id (window-focus window)))
        :zoomed (and (window-zoomed window) (pane-id (window-zoomed window)))))

(defun session-said (session)
  (list :name (session-name session)
        :rows (session-rows session) :cols (session-cols session)
        :bar (session-barp session) :scrollbars (session-scrollbarsp session)
        :search-kind (session-search-kind session)
        :window (or (window-number session (session-window session)) 1)
        :windows (mapcar #'window-said (session-windows session))))

(defun tree-said (server)
  "The server's sessions, windows and panes as they go to disk: without when,
so what it was last written as can be compared to what it is now."
  (list :atty-state +state-version+
        :server (file-namestring (server-path server))
        :panes-made *panes-made*
        :sessions (mapcar #'session-said (server-sessions server))))

(defun said-layout (said panes)
  "A layout from what LAYOUT-SAID wrote, with PANES the panes by id. A pane
that is not there is left out, and a split left with one part is that part."
  (cond ((integerp said) (gethash said panes))
        ((consp said)
         (let ((parts (remove nil (mapcar (lambda (part) (said-layout part panes))
                                          (rest said)))))
           (cond ((null parts) nil)
                 ((null (rest parts)) (first parts))
                 (t (make-split (first said) parts)))))
        (t nil)))

(defun said-window (form panes)
  (destructuring-bind (&key label layout focus zoomed &allow-other-keys) form
    (let ((layout (said-layout layout panes)))
      (when layout
        (let ((in (panes-in layout)))
          (%make-window :label label :layout layout
                        :focus (or (find focus in :key #'pane-id) (first in))
                        :zoomed (find zoomed in :key #'pane-id)))))))

(defun said-session (server form panes)
  (destructuring-bind (&key name rows cols bar scrollbars search-kind window windows
                       &allow-other-keys)
      form
    (let ((made (remove nil (mapcar (lambda (w) (said-window w panes)) windows))))
      (when made
        (%make-session :name name :rows rows :cols cols
                       :socket (server-path server) :server server
                       :barp bar :scrollbarsp scrollbars
                       :search-kind (or search-kind 0)
                       :windows made
                       :window (or (and window (nth (1- window) made)) (first made))
                       :screen (tty:make-screen :width cols :height rows))))))

(defun pane-ids-in (tree)
  "Every pane id the saved TREE names, in the order the layouts name them."
  (let ((ids nil))
    (labels ((walk (said)
               (cond ((integerp said) (pushnew said ids))
                     ((consp said) (mapc #'walk (rest said))))))
      (dolist (session (getf (nthcdr 2 tree) :sessions))
        (dolist (window (getf session :windows))
          (walk (getf window :layout)))))
    (nreverse ids)))

(defun restore-state (server &optional (dir (server-state-dir server)))
  "Bring back what the server called by DIR's name held when it was last
saved. Answers the sessions, and puts what could not be as it was in the
server's notes for whoever attaches first."
  (multiple-value-bind (tree status) (read-state-file (tree-file dir) :atty-state)
    (case status
      (:missing nil)
      (:truncated
       (push (list (format nil "~A could not be read; nothing was brought back"
                           (tree-file dir))
                   :warning)
             (server-notes server))
       nil)
      (:wrong-version
       (push (list (format nil "~A was written by another build and was moved aside"
                           (tree-file dir))
                   :warning)
             (server-notes server))
       nil)
      (t
       (let* ((now (now-ms))
              (panes (make-hash-table))
              (sessions nil))
         (dolist (id (pane-ids-in tree))
           (multiple-value-bind (form status) (read-state-file (pane-file dir id) :atty-pane)
             (let ((rows (session-rows-for tree id)))
               (case status
                 (:ok
                  (multiple-value-bind (pane note) (said-pane form now)
                    (setf (gethash id panes) pane)
                    (when note (push (list note :warning) (server-notes server)))))
                 (t
                  (setf (gethash id panes)
                        (empty-pane-for id (first rows) (second rows)
                                        (if (eq status :missing) "not on disk" "unreadable")))
                  (push (list (format nil "pane ~D came back empty: its file ~A" id
                                      (if (eq status :missing) "was not there" "could not be read"))
                              :warning)
                        (server-notes server)))))))
         (dolist (form (getf (nthcdr 2 tree) :sessions))
           (let ((session (said-session server form panes)))
             (when session (push session sessions))))
         (setf sessions (nreverse sessions))
         (setf *panes-made* (max *panes-made* (or (getf (nthcdr 2 tree) :panes-made) 0)
                                (reduce #'max (pane-ids-in tree) :initial-value 0)))
         (dolist (session sessions)
           (session-compose session)
           (dolist (pane (session-panes session))
             (pane-start pane :environment (pane-environment session pane))))
         (setf (server-sessions server) (append (server-sessions server) sessions)
               (server-had-sessions server) (or (server-had-sessions server) (and sessions t))
               (server-tree-saved server) (tree-said server))
         (dolist (session sessions)
           (dolist (pane (session-panes session))
             (run-hook 'pane-started session pane)))
         (run-hook 'server-restored server sessions)
         sessions)))))

(defun session-rows-for (tree id)
  "The size of the session the pane ID is in, as (rows cols), for a pane that
comes back with no file of its own."
  (dolist (session (getf (nthcdr 2 tree) :sessions) (list 24 80))
    (dolist (window (getf session :windows))
      (when (member id (let ((ids nil))
                         (labels ((walk (said)
                                    (cond ((integerp said) (push said ids))
                                          ((consp said) (mapc #'walk (rest said))))))
                           (walk (getf window :layout)))
                         ids))
        (return-from session-rows-for
          (list (getf session :rows) (getf session :cols)))))))

;;; Saving: the tree when it has changed, each pane when it has been quiet a
;;; moment or has gone unsaved long enough, and everything when the server
;;; stops.

(defun save-tree (server &optional (dir (server-state-dir server)))
  "Write the tree if it is not what was last written, and drop the files of
panes that are in no session any more. Answers whether it was written."
  (let ((tree (tree-said server)))
    (unless (equal tree (server-tree-saved server))
      (run-hook 'before-save server)
      (when (write-form-atomically (tree-file dir)
                                   (list* :atty-state +state-version+
                                          :saved (get-universal-time)
                                          (cddr tree)))
        (setf (server-tree-saved server) tree)
        (let ((live (loop :for session :in (server-sessions server)
                          :append (mapcar #'pane-id (session-panes session)))))
          (loop :for (id . path) :in (pane-files dir)
                :unless (member id live) :do (ignore-errors (delete-file path))))
        t))))

(defun save-pane (server pane now &optional (dir (server-state-dir server)))
  (when (write-form-atomically (pane-file dir (pane-id pane)) (pane-said pane now))
    (setf (pane-saved-at pane) now)
    t))

(defun pane-changed-at (pane)
  (max (pane-moved-at pane) (pane-touched pane)))

(defun save-due-p (pane now)
  "Whether PANE has changed since it was saved and has either been quiet for
+SAVE-QUIET-AFTER+ or gone unsaved for +SAVE-AT-MOST-EVERY+."
  (let ((changed (pane-changed-at pane)))
    (and (> changed (pane-saved-at pane))
         (or (>= (- now changed) +save-quiet-after+)
             (>= (- now (pane-saved-at pane)) +save-at-most-every+)))))

(defun save-what-is-due (server now &optional (dir (server-state-dir server)))
  "One turn of saving: the tree if it changed, and the one pane longest owed
a save, so no turn of the loop stalls on more than one pane's rows."
  (save-tree server dir)
  (let ((due (loop :for session :in (server-sessions server)
                   :append (remove-if-not (lambda (p) (save-due-p p now))
                                          (session-panes session)))))
    (when due
      (save-pane server (reduce (lambda (a b) (if (<= (pane-saved-at a) (pane-saved-at b)) a b))
                                due)
                 now dir))))

(defun save-everything (server &optional (dir (server-state-dir server)))
  "The tree and every pane, now."
  (let ((now (now-ms)))
    (save-tree server dir)
    (dolist (session (server-sessions server))
      (dolist (pane (session-panes session))
        (save-pane server pane now dir)))))

(defparameter +save-tick+ 1000
  "Milliseconds between looks at what is owed a save.")

(defun keep-saving (server)
  "Look at what is owed a save every +SAVE-TICK+ for as long as the server
runs, stepping over a save that comes apart: a disk that is full is not a
reason to lose the panes."
  (labels ((tick ()
             (when (and (server-saving server) (server-going server))
               (handler-case (save-what-is-due server (now-ms))
                 (error (e) (say-what-broke e)))
               (later server +save-tick+ #'tick))))
    (later server +save-tick+ #'tick)))

(defun pane-touch (pane)
  "PANE changed in something other than its screen: its name, its log."
  (setf (pane-touched pane) (now-ms)))

;;; Starting afresh, and what was saved when nothing is running.

(defun move-state-aside (&optional (name (server-name)))
  "Put the state of the server called NAME out of the way, so it starts with
nothing. Answers where it went, or nil when there was none."
  (let* ((dir (uiop:ensure-directory-pathname
               (merge-pathnames (format nil "atty/~A/" (a-name name "a server")) (state-home))))
         (aside (merge-pathnames
                 (format nil "atty/~A.fresh-~A/" (a-name name "a server")
                         (multiple-value-bind (s m h day month year) (get-decoded-time)
                           (format nil "~D~2,'0D~2,'0DT~2,'0D~2,'0D~2,'0D" year month day h m s)))
                 (state-home))))
    (when (probe-file dir)
      (sb-posix:rename (string-right-trim "/" (namestring dir))
                       (string-right-trim "/" (namestring aside)))
      aside)))

(defun saved-sessions (&optional (name (server-name)))
  "What was saved for the server called NAME: (name windows panes saved-at)
rows, or nothing."
  (let ((dir (uiop:ensure-directory-pathname
              (merge-pathnames (format nil "atty/~A/" (a-name name "a server")) (state-home)))))
    (multiple-value-bind (tree status) (read-state-file (tree-file dir) :atty-state)
      (when (eq status :ok)
        (loop :for session :in (getf (nthcdr 2 tree) :sessions)
              :collect (list (getf session :name)
                             (length (getf session :windows))
                             (length (let ((ids nil))
                                       (labels ((walk (said)
                                                  (cond ((integerp said) (pushnew said ids))
                                                        ((consp said) (mapc #'walk (rest said))))))
                                         (dolist (w (getf session :windows))
                                           (walk (getf w :layout))))
                                       ids))
                             (getf (nthcdr 2 tree) :saved)))))))
