;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defparameter +was-known+
  '(:attach :keys :resize :knock :bar :detach :stop)
  "What every server has always known how to do.

A server that says nothing about what it can do is one from before it said, and
these are what it had then. A client assuming more of it would send keys into
silence.")

(defstruct (client (:constructor %make-client))
           (wire nil)
           (socket nil)
           (fd 0 :type fixnum)
           (to 1 :type fixnum)
           (screen nil)
           (from nil)
           (shown nil)
           (over nil)
           (dirty t :type boolean)
           (takes t)
           (rows 24 :type fixnum)
           (cols 80 :type fixnum)
           (waiting nil)
           (chord-so-far nil :type list)
           (partial "" :type string)
           (greeted nil :type boolean)
           (knows +was-known+)
           (mode 'pane-mode)
           (going t :type boolean)
           (why nil)
           ;; what the server has told this client about every pane, while
           ;; something on top has asked to be kept told
           (id nil)
           (panes (make-hash-table :test 'equal))
           (find-text nil)
           (found nil)
           (screens (make-hash-table :test 'equal))
           (lately nil)
           (clients nil)
           (pulses (make-hash-table :test 'equal))
           (events nil)
           (events-at 0 :type integer)
           (barp t)
           (layouts (make-hash-table :test 'equal))
           (watching 0 :type fixnum)
           (ticked 0 :type integer)
           (session nil)
           (about (make-hash-table :test 'equal))
           ;; half a chord, and the menu of what can follow it once it has
           ;; been pending long enough to want one
           (pending-since nil)
           (menu nil))

(defun connect-to (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    (values (make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket)
            socket)))

(declaim (ftype function ask ask-a-name shortened-to bar-face menu-due-p draw-the-menu menu-clicked))

(defun ms-here ()
  (floor (* 1000 (get-internal-real-time)) internal-time-units-per-second))

(defun keep-told (client)
  "Something on top wants to be told about every pane. Asked for once however
many want it, and dropped when the last of them goes."
  (when (= 1 (incf (client-watching client)))
    (wire-send (client-wire client) '(:watch-panes t))
    (wire-send (client-wire client) '(:watch-screens 16)))
  (wire-send (client-wire client) '(:lately 5))
  (wire-send (client-wire client) '(:clients)))

(defun clients-looking-at (client session window)
  "The attached terminals looking at WINDOW of SESSION, other than this one:
what a question's line says when somebody else already has it in front of them."
  (remove-if-not (lambda (c) (and (equal session (fifth c)) (eql window (sixth c))
                                  (not (eql (first c) (client-id client)))))
                 (client-clients client)))

(defun stop-told (client)
  (when (zerop (setf (client-watching client) (max 0 (1- (client-watching client)))))
    (wire-send (client-wire client) '(:watch-panes nil))
    (wire-send (client-wire client) '(:watch-screens nil))
    (clrhash (client-panes client))
    (clrhash (client-screens client))))

(defun tty-name (fd)
  "What the terminal on FD is called, such as /dev/ttys004, or nil. It is how a
server tells one person's keys from another's in a pane's log."
  (and (tty:a-terminal-p fd)
       (ignore-errors
        (sb-alien:alien-funcall
         (sb-alien:extern-alien "ttyname" (function sb-alien:c-string sb-alien:int))
         fd))))

(defun terminal-size (fd)
  "How big the terminal on FD is. One that says it has no rows or no columns,
as a terminal made by a program that never set one does, is taken to be the
size a terminal is when nobody said: a pane one column wide is no use to
anybody."
  (multiple-value-bind (rows cols)
      (if (tty:a-terminal-p fd) (tty:host-size fd) (values 24 80))
    (if (or (zerop rows) (zerop cols))
        (values 24 80)
        (values rows cols))))

(defun make-client (path &key name open (fd tty:+stdin+) (to tty:+stdout+)
                              (takes (tty:takes-of)))
  "A client on the server at PATH, joined to the session called NAME, or the
first one when NAME is nil. OPEN is (command directory [label]): the session is
made with them when it is not there, its first pane called LABEL."
  (multiple-value-bind (rows cols) (terminal-size fd)
                       (multiple-value-bind (wire socket) (connect-to path)
                                            (let ((client (%make-client :wire wire :socket socket :fd fd :to to
                                                                        :takes takes :rows rows :cols cols
                                                                        :screen (tty:make-screen :width cols :height rows)
                                                                        :waiting (tty:make-waiting 4))))
                                              ;; the name goes first and on its
                                              ;; own. A server that does not know
                                              ;; about names passes it over and
                                              ;; the attach it does know is the
                                              ;; shape it has always been
                                              (wire-send wire (list :who (tty-name fd)))
                                              (if open
                                                  (destructuring-bind (command directory &optional label)
                                                      open
                                                    (wire-send wire (list :open name command directory
                                                                          rows cols takes label)))
                                                  (progn
                                                    (when name
                                                      (wire-send wire (list :want name)))
                                                    (wire-send wire (list :attach rows cols takes))))
                                              (wire-flush wire)
                                              client))))

(defun client-close (client)
  "Shut it, once. The wire owns the socket, so closing the wire is the close;
doing it again here would shut a descriptor number that by then belongs to
whoever opened the next one."
  (setf (client-going client) nil)
  (tty:free-waiting (client-waiting client))
  (wire-close (client-wire client)))

(defun host-say (client said)
  "Write to the terminal the client is sitting in. What the screen holds is
characters, not bytes, so this is the one place that says how they are spelt.

A terminal that will not take what it is given has gone, which is a reason to
stop rather than a fault: the panes are still running and somebody can attach to
them again."
  (handler-case (pty:pty-write-string (client-to client) said :utf-8)
    (error () (done-with client :terminal-gone) 0)))

(defgeneric draw-over (thing screen)
  (:documentation "Draw THING onto SCREEN, over whatever the session put there.
This is the seam: anything that can write cells can be put on top, and nothing
else needs to know about it."))

(defvar *successor* nil
  "Where the server said its next one would be, when it said it was
restarting: what a client becomes to match it.")

(defvar *version-said* nil
  "Whether this client has said that the server is another build: once.")

(defvar *drawing-for* nil
  "The client whatever is being drawn over its session belongs to. Something on
top that shows what the server said about the panes reads it from here.")

(defgeneric ticks-p (thing)
  (:documentation "Whether THING on top says how long something has been, and
so is drawn again every second whether anything was said or not.")
  (:method (thing) (declare (ignore thing)) nil))

(defgeneric passes-keys-p (thing)
  (:documentation "Whether what is typed goes on to the pane while THING is on
top, as it would with nothing there. Something beside the panes rather than in
front of them, that only says things, need not take the keyboard away.")
  (:method (thing) (declare (ignore thing)) nil))

(defgeneric mode-of (thing)
  (:documentation "Which mode a client is in while THING is on top.")
  (:method (thing) (declare (ignore thing)) 'pane-mode))

(defgeneric over-name (thing)
  (:documentation "What THING on top is called, for its header: nil for
something that need not be said.")
  (:method (thing) (declare (ignore thing)) nil))

(defgeneric close-over (thing client)
  (:documentation "Take THING off the top of CLIENT, the way its own close key
would.")
  (:method (thing client) (client-over-drop client thing)))

(defun client-fit (client rows cols)
  (setf (client-screen client) (tty:make-screen :width cols :height rows)
        (client-from client) (tty:make-screen :width cols :height rows)
        (client-shown client) (tty:make-screen :width cols :height rows)
        (client-dirty client) t))

(defun client-show (client)
  "Put the session, and then whatever is drawn on top of it, onto the terminal.

The client composes for itself rather than writing out the runs the server sent,
because what is on top is this one person's: a prompt somebody opened here is
not something everybody attached should be shown."
  (let ((work (client-screen client)))
    (when (and work (client-from client) (client-shown client))
      (tty:screen-copy work (client-from client))
      (let ((*drawing-for* client))
        (dolist (it (reverse (client-over client)))
          (draw-over it work))
        (if (menu-due-p client)
            (draw-the-menu client work)
            (setf (client-menu client) nil)))
      (let ((runs (tty:screen-diff (client-shown client) work)))
        (host-say client
                  (with-output-to-string (s)
                    (tty:encode-frame work runs s :takes (client-takes client)))))
      (setf (client-dirty client) nil))))

(defun client-in-mode (client)
  "The mode whatever is on top asks for, or the pane's when nothing is."
  (setf (client-mode client) (mode-of (first (client-over client)))
        (client-chord-so-far client) nil
        (client-pending-since client) nil
        (client-menu client) nil))

(defun client-over-put (client it)
  (push it (client-over client))
  (client-in-mode client)
  (setf (client-dirty client) t)
  it)

(defun client-over-drop (client it)
  (setf (client-over client) (remove it (client-over client)))
  (client-in-mode client)
  (setf (client-dirty client) t))

(defun done-with (client why)
  "Stop, for the first reason there was. What came after it is what stopping
looks like, not why it happened."
  (setf (client-going client) nil)
  (unless (client-why client)
    (setf (client-why client) why)))

(defun client-heard (client form)
  (case (first form)
        (:hello
         (destructuring-bind (name rows cols &optional knows) (rest form)
                             (setf (client-greeted client) t
                                   (client-session client) name
                                   (client-knows client) (or knows +was-known+))
                             (client-fit client rows cols)
                             (host-say client tty:+blanked+)
                             (when *init-problem*
                               (show-note client "init" *init-problem* :face :warning))
                             (let ((*client* client)) (run-hook 'client-attached client))))
        (:frame
         (destructuring-bind (said faces) (rest form)
                             (said-into-screen (client-from client) said faces)
                             (setf (client-dirty client) t)))
        (:cursor
         (destructuring-bind (y x visible style) (rest form)
                             (let ((screen (client-from client)))
                               (setf (tty:screen-cursor-y screen) y
                                     (tty:screen-cursor-x screen) x
                                     (tty:screen-cursor-visible screen) (and visible t)
                                     (tty:screen-cursor-style screen) style))
                             (setf (client-dirty client) t)))
        (:these (ask-a-window client (second form)))
        (:bell (host-say client (string (code-char 7))))
        (:do (run-command (second form) client))
        (:say (show-note client "atty" (second form) :face (or (third form) :accent)))
        (:you (setf (client-id client) (second form)))
        (:pane
         (let ((row (rest form)))
           (setf (gethash (cons (getf row :session) (getf row :id)) (client-panes client))
                 (list* :heard-at (ms-here) row)
                 (client-dirty client) t)))
        (:pane-gone
         (remhash (cons (second form) (third form)) (client-panes client))
         (remhash (cons (second form) (third form)) (client-screens client))
         (setf (client-dirty client) t))
        (:pane-screen
         (destructuring-bind (session id &optional width said faces) (rest form)
           (when width
             (setf (gethash (cons session id) (client-screens client))
                   (let ((screen (tty:make-screen :width width
                                                  :height (1+ (reduce #'max said
                                                                      :key #'first
                                                                      :initial-value 0)))))
                     (said-into-screen screen said faces)
                     screen)
                   (client-dirty client) t))))
        (:lately (setf (client-lately client) (second form)
                       (client-dirty client) t))
        (:pulses (loop :for (session id cells) :in (second form)
                       :do (setf (gethash (cons session id) (client-pulses client))
                                 (mapcar (lambda (c) (cons (first c) (second c))) cells)))
                 (setf (client-dirty client) t))
        (:events (setf (client-events client) (second form)
                       (client-events-at client) (ms-here)
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
             (setf (getf (gethash key (client-about client)) (first form))
                   (list* (ms-here) said)
                   (client-dirty client) t))))
        (:answered
         (destructuring-bind (session id n outcome) (rest form)
           (unless (eq outcome t)
             (show-note client "not answered"
                        (format nil "~A:~D was not answered ~D: ~(~A~)."
                                session id n
                                (case outcome
                                  (:not-blocked "it is not asking anything now")
                                  (:no-such-option "it has no such answer")
                                  (:gone "it is gone")
                                  (t outcome)))))
           (wire-send (client-wire client) '(:lately 5))))
        (:agent-prompted
         (destructuring-bind (session id outcome) (rest form)
           (unless (member outcome '(t :queued))
             (show-note client "not prompted"
                        (format nil "~A:~D ~A" session id
                                (if (eq outcome :blocked)
                                    "is asking something; answer it, not a prompt."
                                    "is gone."))))))
        (:found
         ;; the frame says it; the palette's find lists the hits; nothing
         ;; found is worth a word only when nothing is listing them
         (destructuring-bind (session id n at row &optional hits current) (rest form)
           (declare (ignore session id at row))
           (setf (client-found client) (list :n n :hits hits :current current)
                 (client-dirty client) t)
           (let ((top (first (client-over client))))
             (if (and (typep top 'prompt) (eql #\/ (prompt-kind top)))
                 (setf (prompt-index top) (or current 0))
                 (when (zerop n) (show-note client "find" "nothing has that in it" :face :warning))))))
        (:copied
         ;; to the terminal the client sits in, the way a program would ask it
         (host-say client (format nil "~C]52;c;~A~C" (code-char 27) (base64 (second form)) (code-char 7)))
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
           (session-renamed-here client old new)))
        (:session-named
         (destructuring-bind (old new how) (rest form)
           (case how
             (:empty (show-note client "name" "a session needs a name"))
             (:taken (show-note client "name" (format nil "there is already a session called ~A" new)))
             (:gone (show-note client "name" (format nil "no session is called ~A any more" old))))))
        (:name-it
         (destructuring-bind (session id label title &optional address) (rest form)
           (ask-a-name client session id label title :address address)))
        (:name-window-of
         (destructuring-bind (session n label) (rest form)
           (ask-a-window-name client session n label)))
        ;; a server from another build: it works, since neither end takes
        ;; the other apart by its exact shape, but it is said once, with what
        ;; brings the two together
        (:version
         (let ((theirs (second form)))
           (unless (or (equal theirs *version*) *version-said*)
             (setf *version-said* t)
             (show-note client "version"
                        (format nil "This atty is ~A; the server holding this session is ~A.~%~
                                     atty restart-server starts it again from this build."
                                *version* theirs)
                        :face :warning))))
        (:bye (done-with client (second form))
              (when (eq (second form) :restarting)
                (setf *successor* (third form))))
        (t nil)))

(defun rehash-session (table old new)
  "Every key (OLD . id) in TABLE moved to (NEW . id)."
  (let ((moved nil))
    (maphash (lambda (k v) (when (equal (car k) old) (push (cons k v) moved))) table)
    (loop :for (k . v) :in moved
          :do (remhash k table)
              (setf (gethash (cons new (cdr k)) table) v))))

(defgeneric session-renamed-over (thing old new)
  (:documentation "THING, drawn on top, hears that session OLD is now NEW.")
  (:method (thing old new) (declare (ignore thing old new)) nil))

(defun session-renamed-here (client old new)
  "The server called the session OLD NEW: everything this client keeps by
the old name is kept by the new one, and whatever is on top is told."
  (when (equal (client-session client) old)
    (setf (client-session client) new))
  (dolist (table (list (client-panes client) (client-screens client)
                       (client-pulses client) (client-about client)))
    (rehash-session table old new))
  (dolist (row (loop :for v :being :the :hash-values :of (client-panes client) :collect v))
    (when (equal (getf row :session) old)
      (setf (getf row :session) new)))
  (let ((layout (gethash old (client-layouts client))))
    (when layout
      (remhash old (client-layouts client))
      (setf (gethash new (client-layouts client)) layout)))
  (setf (client-events client)
        (mapcar (lambda (e) (if (equal (fourth e) old) (append (subseq e 0 3) (list new) (nthcdr 4 e)) e))
                (client-events client)))
  (setf (client-clients client)
        (mapcar (lambda (c) (if (equal (fifth c) old) (append (subseq c 0 4) (list new) (nthcdr 5 c)) c))
                (client-clients client))
        (client-lately client)
        (mapcar (lambda (e) (if (equal (first e) old) (cons new (rest e)) e)) (client-lately client)))
  (dolist (it (client-over client)) (session-renamed-over it old new))
  (setf (client-dirty client) t))

(defun base64 (text)
  "TEXT as base64, the way OSC 52 wants it."
  (let* ((bytes (sb-ext:string-to-octets text :external-format :utf-8))
         (table "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (out (make-string-output-stream)))
    (loop :for i :from 0 :below (length bytes) :by 3
          :do (let* ((n (min 3 (- (length bytes) i)))
                     (b0 (aref bytes i))
                     (b1 (if (> n 1) (aref bytes (+ i 1)) 0))
                     (b2 (if (> n 2) (aref bytes (+ i 2)) 0))
                     (word (logior (ash b0 16) (ash b1 8) b2)))
                (write-char (char table (ldb (byte 6 18) word)) out)
                (write-char (char table (ldb (byte 6 12) word)) out)
                (write-char (if (> n 1) (char table (ldb (byte 6 6) word)) #\=) out)
                (write-char (if (> n 2) (char table (ldb (byte 6 0) word)) #\=) out)))
    (get-output-stream-string out)))

(defun client-redraw (client)
  (when (client-shown client)
    (setf (client-shown client)
          (tty:make-screen :width (tty:screen-width (client-shown client))
                       :height (tty:screen-height (client-shown client)))))
  (setf (client-dirty client) t)
  (host-say client tty:+blanked+)
  (wire-send (client-wire client)
             (list :resize (client-rows client) (client-cols client))))

(defparameter +half-said+ 64
  "How much of an unfinished sequence to keep for the next read. More than this
is not somebody typing a key.")

(defun client-holding (client said)
  "SAID with whatever was left half-said at the end of the last read in front of
it."
  (if (plusp (length (client-partial client)))
      (prog1 (concatenate 'string (client-partial client) said)
        (setf (client-partial client) ""))
      said))

(defun client-hold (client said at n)
  (setf (client-partial client)
        (if (> (- n at) +half-said+) "" (subseq said at n))))

(defun client-chord-event (client event)
  "EVENT, a key or a click as the terminal sent it, to the mode this client is
in. A click is a key a mode can bind, and says where it landed as *MOUSE-AT*."
  (if (and (consp event) (eq :mouse (first event)))
      (let ((key (mouse-key-of event)))
        (when key
          (let ((*mouse-at* (cons (getf (rest event) :x) (getf (rest event) :y)))
                (*mouse-event* (rest event)))
            (if (client-menu client)
                ;; the menu is up over a half chord: a press is the menu's,
                ;; not another key on the chord
                (when (string= "mouse-1" (atty/mode:spelled key))
                  (let ((*client* client)) (menu-clicked client)))
                (client-chord client key)))))
      (client-chord client (key-of event))))

(defun client-pressed (client said)
  "Bytes, as keys, to the mode whatever is on top put the client in.

A sequence the terminal sent that is no key a mode knows, a mouse report say, is
passed over rather than made into one, and one the rest of has not arrived yet is
kept until it has."
  (let* ((said (client-holding client said))
         (at 0)
         (n (length said)))
    (loop :while (< at n)
          :do (multiple-value-bind (event took)
                  (tty:escape-sequence-to-key-event said at n nil)
                (when (zerop took) (client-hold client said at n) (return))
                (incf at took)
                (when event (client-chord-event client event))))))

(defgeneric unbound (thing chord client)
  (:documentation "What to do with a key the mode has no binding for. A prompt
puts it in what has been typed; most things ignore it.")
  (:method (thing chord client) (declare (ignore thing chord client)) nil))

(defun client-chord (client key)
  "Give KEY to the mode this client is in. Answers whether the chord wants more
keys.

Which mode, and half a chord, are both this client's rather than the image's:
two of them attached in one process would otherwise be in each other's modes and
finishing each other's chords."
  (let* ((*client* client)
         (over (first (client-over client)))
         (atty/mode:*pending* (client-chord-so-far client))
         (atty/mode:*unbound* (lambda (chord) (unbound over chord client))))
    (prog1 (let ((how (atty/mode:press (atty/mode:spelled key)
                                       (atty/mode:mode-named (client-mode client)))))
             (if (eq how :pending)
                 (unless (client-pending-since client)
                   (setf (client-pending-since client) (ms-here)))
                 (progn
                   (setf (client-pending-since client) nil)
                   (when (client-menu client)
                     (setf (client-menu client) nil (client-dirty client) t))))
             (eq how :pending))
      (setf (client-chord-so-far client) atty/mode:*pending*)
      (when over (setf (client-dirty client) t)))))

(defun client-typed (client said)
  "Pass what was typed through, byte for byte, until the one byte that says a
chord is starting.

The bytes are not decoded into keys and encoded again: a terminal sends more
than any table of keys knows, such as mouse reports, pasted text and whatever
encoding it was built with, and what the pane reads should be what the terminal
sent. Once a chord has started they are read as keys, because that is what a
mode is written in, and the pane does not see them at all.

A mouse report this build has no name for is forwarded the same way. One it
does have a name for goes to the mode instead and is not passed on."
  (when (and (client-over client)
             (not (passes-keys-p (first (client-over client)))))
    (return-from client-typed (client-pressed client said)))
  (let* ((said (client-holding client said))
         (out (make-array (length said) :element-type 'character :fill-pointer 0))
         (at 0)
         (n (length said)))
    (flet ((send ()
             (when (plusp (fill-pointer out))
               (wire-send (client-wire client)
                          (list :keys (coerce out 'simple-string)))
               (setf (fill-pointer out) 0))))
      (loop :while (< at n)
            :do (if (client-chord-so-far client)
                    (multiple-value-bind (event took)
                        (tty:escape-sequence-to-key-event said at n nil)
                      (when (zerop took) (client-hold client said at n) (return))
                      (incf at took)
                      (when event (client-chord-event client event))
                      (when (client-over client) (return)))
                    (let ((ch (char said at)))
                      (cond
                        ((char= ch +prefix+)
                         (incf at)
                         (send) (client-chord client (key-of ch)))
                        ((char= ch #\Escape)
                         (multiple-value-bind (event took)
                             (tty:escape-sequence-to-key-event said at n nil)
                           (let ((key (and (plusp took) (consp event)
                                           (eq (first event) :mouse)
                                           (mouse-key-of event))))
                             (cond
                               (key
                                (send)
                                (let ((*mouse-at* (cons (getf (rest event) :x)
                                                         (getf (rest event) :y)))
                                      (*mouse-event* (rest event)))
                                  (client-chord client key))
                                (incf at took))
                               ;; Escape on its own, with something on top that
                               ;; lets keys through: the mode's, so it can close
                               ;; what is on top; a sequence is still the pane's
                               ((and (client-over client) (<= took 1))
                                (send)
                                (client-chord client (key-of ch))
                                (incf at))
                               (t (vector-push ch out) (incf at))))))
                        (t (incf at) (vector-push ch out))))))
      (send))))

(defun client-resized (client)
  (setf tty:*resized* nil)
  (when (tty:a-terminal-p (client-fd client))
    (multiple-value-bind (rows cols) (terminal-size (client-fd client))
                         (unless (and (= rows (client-rows client)) (= cols (client-cols client)))
                           (setf (client-rows client) rows
                                 (client-cols client) cols)
                           (wire-send (client-wire client) (list :resize rows cols))))))

(defun client-step (client &optional (patience 100))
  "One turn of the loop. What the server said is taken in before what was typed:
a bye says why everything is stopping, and a terminal that fell over at the same
moment would otherwise answer that question first, and answer it wrongly."
  (let* ((w (tty:waiting-clear (client-waiting client)))
         (keys (tty:waiting-add w (client-fd client)))
         (wire (client-wire client))
         (sock (tty:waiting-add w (wire-fd wire)
                            (logior sb-unix:pollin
                                    (if (plusp (wire-pending wire))
                                        sb-unix:pollout
                                      0)))))
    (tty:wait-on w (if (client-chord-so-far client) (min patience 50) patience))
    (when tty:*resized* (client-resized client))
    (when (tty:writable-p (tty:waiting-back w sock))
      (wire-flush wire))
    (when (tty:readable-p (tty:waiting-back w sock))
      (if (null (wire-fill wire))
          (done-with client :server-gone)
        (loop for form = (wire-take wire)
              while form
              do (client-heard client form))))
    (when (tty:readable-p (tty:waiting-back w keys))
      (let ((said (pty:pty-read-string (client-fd client) 8192)))
        (if (null said)
            (done-with client :input-gone)
          (client-typed client said))))
    (when (and (some #'ticks-p (client-over client))
               (>= (- (ms-here) (client-ticked client)) 1000))
      (setf (client-ticked client) (ms-here)
            (client-dirty client) t))
    (when (and (menu-due-p client) (null (client-menu client)))
      (setf (client-dirty client) t))
    (when (client-dirty client) (client-show client))
    (wire-flush wire)
    client))

(defparameter +patience+ 5
  "How many seconds to wait for the server to say what we are looking at.

A server that takes the connection and then says nothing leaves a terminal in
raw mode showing nothing at all, which says neither that anything is wrong nor
which end of it is wrong.")

(defun answered-p (client since)
  (or (client-greeted client)
      (< (- (get-internal-real-time) since)
         (* +patience+ internal-time-units-per-second))
      (progn (done-with client :no-answer) nil)))

(defun attach (path &key name open (fd tty:+stdin+) (to tty:+stdout+)
                         (takes (tty:takes-of)))
  (let ((client (make-client path :name name :open open :fd fd :to to :takes takes))
        (since (get-internal-real-time)))
    (unwind-protect
        (tty:with-host (fd :to to)
                   (loop while (and (client-going client)
                                    (not tty:*asked-to-stop*)
                                    (answered-p client since)
                                    (wire-open (client-wire client)))
                         do (client-step client))
                   (when tty:*asked-to-stop* (done-with client :asked-to-stop))
                   (client-why client))
      (ignore-errors (wire-send (client-wire client) '(:detach))
                     (wire-flush (client-wire client)))
      (client-close client))))
