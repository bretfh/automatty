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

(defun handoff-file (dir) (merge-pathnames "handoff.sexp" dir))

(defun pid-file (dir) (merge-pathnames "pid.sexp" dir))

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
  (with-term (term pane)
   (let* ((agent (pane-agent pane))
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
         (in-front (or (pane-cover pane) (and (term:term-in-alt-screen term) term)))
         (over (and in-front
                    (loop :for y :from 0 :to (last-row-on in-front)
                          :collect (encode-row (term:term-grid-row in-front y) faces table)))))
    (list :atty-pane +state-version+
          :id (pane-id pane)
          :saved (get-universal-time)
          :command (pane-command pane)
          :directory (pane-directory pane)
          :here (pane-here pane)
          :front (if (pane-front pane)
                     (front-with-session (list* :alt (and in-front t)
                                                (pane-front pane)))
                     (pane-kept-front pane))
          :label (pane-label pane)
          :named (pane-named pane)
          :rows (term:term-height term)
          :cols (term:term-width term)
          :alt-screen (and in-front t)
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
          :over over
          :modes (encode-modes (term:term-modes term))))))

(defun encode-modes (modes)
  (loop :for (key value) :on modes :by #'cddr
        :collect key
        :collect (if (typep value 'term:face) (list :face (encode-face value)) value)))

(defun decode-modes (modes)
  (loop :for (key value) :on modes :by #'cddr
        :collect key
        :collect (if (and (consp value) (eq :face (first value))) (decode-face (second value)) value)))

(defvar *adopted* (make-hash-table)
  "Pane ids to (fd pid): the programs a server that handed over left running.")

(defun fill-rows (term rows)
  (loop :for row :in rows
        :for y :from 0 :below (term:term-height term)
        :do (let ((into (term:term-grid-row term y))
                  (n (min (term:row-width row) (term:term-width term))))
              (replace (term:row-chars into) (term:row-chars row) :end2 n)
              (replace (term:row-faces into) (term:row-faces row) :end2 n))))

(defun adopt-pane (form now fd pid)
  "The pane FORM saved, around the program still running on FD as PID: its
screen and its terminal's modes as they were, and nothing started."
  (destructuring-bind (&key id command directory here label named rows cols programs
                            queued told since-clock states log faces behind screen over
                            alt-screen modes &allow-other-keys)
      (nthcdr 2 form)
    (let* ((pane (make-pane command :id id :rows rows :cols cols :directory directory))
           (term (pane-term pane))
           (seen (map 'simple-vector #'decode-face faces)))
      (flet ((rows-of (said) (mapcar (lambda (r) (decode-row r seen)) said)))
        (push-rows-to-scrollback term (rows-of behind))
        (fill-rows term (rows-of screen))
        (when alt-screen
          (term:term-enter-alt-screen term)
          (fill-rows term (rows-of over))))
      (when modes (setf (term:term-modes term) (decode-modes modes)))
      (setf (pane-fd pane) (pty:nonblocking fd)
            (pane-pid pane) pid
            (pane-here pane) here
            (pane-pushed-seen pane) (term:term-scrollback-pushed term)
            (pane-label pane) label
            (pane-named pane) named
            (pane-programs pane) programs
            (pane-pending-prompt pane) queued
            (pane-log pane) (moments log now)
            (pane-log-count pane) (length log))
      (let ((agent (pane-agent pane)))
        (when told (agent:agent-hear agent told))
        (setf (agent:agent-history agent) (moments states now)
              (agent::agent-historied agent) (length states)
              (agent:agent-since-clock agent) since-clock)
        (agent:agent-become agent :title named :command command :programs programs))
      pane)))

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

(defun resume-command (command &optional kind)
  "What picks up the work of a pane that ran COMMAND, or the agent KIND, when
something is known to."
  (cdr (or (assoc (program-name command) +resume-commands+ :test #'string=)
           (and kind (assoc kind +resume-commands+ :test #'string=)))))

(defun shell-quote (word)
  (if (and (plusp (length word))
           (every (lambda (c) (or (alphanumericp c) (find c "-_./=:,+@%^"))) word))
      word
      (format nil "'~A'" (with-output-to-string (s)
                           (loop :for c :across word
                                 :do (if (char= c #\') (write-string "'\\''" s) (write-char c s)))))))

(defun claude-home ()
  (let ((env (sb-ext:posix-getenv "CLAUDE_CONFIG_DIR")))
    (uiop:ensure-directory-pathname
     (if (and env (plusp (length env))) env (merge-pathnames ".claude/" (user-homedir-pathname))))))

(defun claude-session (pid)
  (let* ((text (ignore-errors
                (uiop:read-file-string (merge-pathnames (format nil "sessions/~D.json" pid)
                                                        (claude-home)))))
         (key "\"sessionId\":\"")
         (at (and text (search key text)))
         (from (and at (+ at (length key))))
         (to (and from (position #\" text :start from))))
    (and to (subseq text from to))))

(defun front-with-session (front)
  (let ((pid (getf front :pid)))
    (if (and pid (string= "claude" (program-name (first (getf front :words)))))
        (let ((session (claude-session pid)))
          (if session (list* :session session front) front))
        front)))

(defun without-resuming (words)
  (loop :with skip := nil
        :for word :in words
        :if skip :do (setf skip nil)
        :else :if (member word '("-r" "--resume") :test #'string=)
                :do (setf skip t)
        :else :unless (or (member word '("-c" "--continue") :test #'string=)
                          (and (> (length word) 9) (string= "--resume=" word :end2 9)))
                :collect word))

(defun claude-conversation-p (session)
  (and (directory (merge-pathnames (format nil "projects/*/~A.jsonl" session) (claude-home))) t))

(defun session-command (front)
  (destructuring-bind (&key words session &allow-other-keys) front
    (when (string= "claude" (program-name (first words)))
      (format nil "~{~A~^ ~}~A"
              (mapcar #'shell-quote (without-resuming words))
              (cond ((null session) " --continue")
                    ((claude-conversation-p session) (format nil " --resume ~A" (shell-quote session)))
                    (t ""))))))

(defun front-of (form)
  "What was in front of the shell in the pane FORM saved: its words and where
it ran. A pane that ran something else, saved before that was looked at, had
that in front."
  (destructuring-bind (&key command (front nil saw-front) programs &allow-other-keys) (nthcdr 2 form)
    (or front
        (and (not saw-front)
         (let ((line (find-if (lambda (line) (and (plusp (length (program-name line)))
                                                 (not (shell-command-p line))))
                             programs)))
          (and line (list :words (split-words line)))))
        (and command (not (shell-command-p command))
             (list :words (list command))))))

(defun split-words (line)
  (loop :with at := 0
        :for start := (position #\Space line :start at :test-not #'char=)
        :while start
        :collect (let ((stop (or (position #\Space line :start start) (length line))))
                   (prog1 (subseq line start stop) (setf at stop)))))

(defun retyped-command (form kind here)
  "What a restored shell is given to type, from what was in front of it in the
pane FORM saved, and whether it is entered. Nil when nothing was."
  (let ((front (front-of form))
        (policy +restore-programs+))
    (when front
      (if (functionp policy)
          (funcall policy form)
          (destructuring-bind (&key words directory &allow-other-keys) front
            (let* ((line (format nil "~{~A~^ ~}" words))
                   (resume (or (session-command front) (resume-command line kind)))
                   (text (format nil "~@[cd ~A; ~]~A"
                                 (and directory (not (equal directory here)) (probe-file directory)
                                      (shell-quote directory))
                                 (or resume (format nil "~{~A~^ ~}" (mapcar #'shell-quote words))))))
              (values text
                      (case policy
                        (:all t)
                        (:typed nil)
                        (t (or (and resume t)
                               (and (if (member :alt front)
                                        (getf front :alt)
                                        (getf (nthcdr 2 form) :alt-screen))
                                    t)))))))))))

(defun restore-rule-text (saved command directory same &optional typed entered)
  "What the rule under a restored pane says: when, and when its program is not
what ran there, what did, where, and what brings it back."
  (format nil "restored ~A~@[ · was: ~A~]~@[ in ~A~]~@[ · ~A picks it up~]~@[ · ~A~]"
          (format-day-time (or saved (get-universal-time)))
          (and (not same) (not typed) command)
          (and (not same) (not typed) directory (abbreviate-directory directory))
          (and (not same) (not typed) (resume-command command))
          (and typed (format nil (if entered "~A started again" "~A is typed, Enter runs it")
                             typed))))

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
  (let ((kept (gethash (getf (nthcdr 2 form) :id) *adopted*)))
    (when kept
      (remhash (getf (nthcdr 2 form) :id) *adopted*)
      (return-from decode-pane (adopt-pane form now (first kept) (second kept)))))
  (destructuring-bind (&key id saved command directory here label named rows cols
                            programs title queued told since-clock states log
                            faces behind screen over &allow-other-keys)
      (nthcdr 2 form)
    (let* ((runs (restore-command form))
           (same (equal runs command))
           (start (find-if (lambda (d) (and d (probe-file d))) (list here directory)))
           (note (and (or here directory) (null start)
                      (format nil "pane ~D was in ~A, which is gone; it starts at home"
                              id (or here directory))))
           (pane (make-pane runs :id id :rows rows :cols cols :directory start))
           (term (pane-term pane))
           (seen (map 'simple-vector #'decode-face faces))
           (agent (pane-agent pane)))
      (when told (agent:agent-hear agent told))
      (setf (agent:agent-history agent) (moments states now)
            (agent::agent-historied agent) (length states)
            (agent:agent-since-clock agent) since-clock)
      (agent:agent-become agent :title (or title named) :command command
                                :programs programs)
      (multiple-value-bind (typed entered)
          (and (shell-command-p runs)
               (retyped-command form (and (agent:agent-reader agent) (agent:agent-kind agent))
                                start))
        (flet ((rows-of (said) (mapcar (lambda (r) (decode-row r seen)) said))
               (rule () (divider-row cols (restore-rule-text saved command directory same
                                                             typed entered))))
          ;; what was behind the screen goes behind it; what was on it goes on
          ;; it with the rule under it. A full-screen program started again
          ;; has its screen put back in front, as it was, until it draws its
          ;; own; one that is not has it written under the rest, since it was
          ;; what was in front
          (push-rows-to-scrollback term (rows-of behind))
          (if (and typed entered over)
              (progn
                (write-rows-to-term term (append (rows-of screen) (list (rule))))
                (let ((cover (term:make-term :width cols :height rows :max-scrollback 0)))
                  (fill-rows cover (rows-of over))
                  (setf (term:term-cursor-visible cover) nil
                        (pane-cover pane) cover
                        (pane-covered pane) t)))
              (write-rows-to-term term (append (rows-of screen) (rows-of over) (list (rule))))))
        (setf (pane-pushed-seen pane) (term:term-scrollback-pushed term)
              (pane-here pane) start
              (pane-typed pane) (and typed (list typed entered))
              (pane-kept-front pane) (and typed (front-of form))
              (pane-label pane) label
              (pane-named pane) named
              (pane-programs pane) programs
              (pane-pending-prompt pane) (and (or same entered) queued)
              (pane-log pane) (moments log now)
              (pane-log-count pane) (length log))
        (unless (or same typed)
          (pane-push-log pane now '(:atty) :restored (format nil "was: ~A" command))))
      (values pane note))))

(defun make-empty-pane (id rows cols why)
  "A pane standing in for one whose file could not be read."
  (let ((pane (make-pane (default-shell) :id id :rows rows :cols cols)))
    (write-rows-to-term (pane-term pane)
               (list (divider-row cols (format nil "restored; what it held is ~A" why))))
    (setf (pane-pushed-seen pane) (with-term (term pane) (term:term-scrollback-pushed term)))
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
        :bar (session-bar-p session) :rail (session-rail-p session)
        :scrollbars (session-scrollbars-p session)
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
  (destructuring-bind (&key name rows cols bar (rail t) scrollbars search-kind window windows
                       &allow-other-keys)
      form
    (let ((made (remove nil (mapcar (lambda (w) (decode-window w panes)) windows))))
      (when made
        (%make-session :name name :rows rows :cols cols
                       :socket (server-path server) :server server
                       :bar-p bar :rail-p rail :scrollbars-p scrollbars
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
