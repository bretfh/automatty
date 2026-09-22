;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defvar *panes-made* 0)

(defstruct (pane (:constructor %make-pane))
  (id 0 :type fixnum)
  (term nil)
  (fd -1 :type fixnum)
  (pid -1 :type fixnum)
  (running t :type boolean)
  (dirty t :type boolean)
  (rang nil :type boolean)
  (named nil)
  (label nil)
  (log nil)
  (log-count 0 :type fixnum)
  (moved-at 0 :type integer)
  (queued nil)
  (command nil)
  (directory nil)
  (agent nil)
  (group nil)
  (programs nil)
  (paths nil)
  (programs-at 0)
  (scrolled 0 :type fixnum)
  (find nil)
  (selecting nil)
  (pushed-seen 0 :type fixnum)
  (touched 0 :type integer)
  (saved-at 0 :type integer)
  (decoder (term:make-decoder)))

(defparameter +programs-every+ 1000)

(defun make-pane (command &key (rows 24) (cols 80) directory id)
  "A pane with a terminal that size and no program in it yet.

Starting it is a second step because the size it is started at is the size it is
told, once: a shell that asks stty for it on its first line must be told the
room the layout gave it rather than a guess it is corrected out of afterwards.

ID is for a pane brought back from disk, which keeps the number it had; one
made now takes the next."
  (let ((pane (%make-pane :id (or id (incf *panes-made*)) :command command
                          :directory directory
                          :agent (agent:make-agent :command command))))
    (setf (pane-term pane)
          (term:make-term :width cols :height rows :max-scrollback +max-scrollback+
                        :bell-fn (lambda (term)
                                   (declare (ignore term))
                                   (setf (pane-rang pane) t))
                        :title-fn (lambda (term title)
                                    (declare (ignore term))
                                    (setf (pane-named pane) title)
                                    (agent:agent-become (pane-agent pane) :title title
                                                                          :command command
                                                                          :programs (pane-programs pane)
                                                                          :paths (pane-paths pane)))
                        :input-fn (lambda (term said)
                                    (declare (ignore term))
                                    (pane-say pane said))))
    pane))

(defun pane-started (pane) (>= (pane-fd pane) 0))

(defun pane-notice-programs (pane now)
  (when (and (pane-started pane) (pane-running pane))
    (let ((group (pty:pty-foreground (pane-fd pane))))
      (when (or (not (eql group (pane-group pane)))
                (and (null (agent:agent-reader (pane-agent pane)))
                     (>= (- now (pane-programs-at pane)) +programs-every+)))
        (let ((running (and group (pty:group-processes group))))
          (setf (pane-group pane) group
                (pane-programs-at pane) now
                (pane-programs pane) (mapcar (lambda (p) (getf p :line)) running)
                (pane-paths pane) (mapcar (lambda (p) (getf p :path)) running)))
        (agent:agent-become (pane-agent pane) :title (pane-named pane)
                                              :command (pane-command pane)
                                              :programs (pane-programs pane)
                                              :paths (pane-paths pane))))))

(defun pane-start (pane &key environment)
  "Run the pane's program on a terminal of its own, the size the pane is now."
  (unless (pane-started pane)
    (let ((term (pane-term pane)))
      (multiple-value-bind (fd pid)
          (pty:spawn-pty-process (pane-command pane)
                                 :rows (term:term-height term)
                                 :cols (term:term-width term)
                                 :environment environment
                                 :directory (pane-directory pane))
        (setf (pane-fd pane) fd
              (pane-pid pane) pid))))
  pane)

(defun pane-drain (pane &key (budget 16) (size 65536))
  "Read what the program wrote and give it to the term. Answers nil when the
program is done.

At most BUDGET reads a wakeup: the descriptor stays readable and the next poll
comes straight back, so one pane writing without pause cannot starve the rest."
  (dotimes (i budget t)
    (unless (pty:pty-wait (pane-fd pane) 0)
      (return t))
    (let ((said (pty:pty-read-string (pane-fd pane) size)))
      (cond
        ((null said) (setf (pane-running pane) nil) (return nil))
        ((zerop (length said)) (return t))
        (t (term:term-process-output
            (pane-term pane)
            (term:decode-utf-8 (pane-decoder pane) said))
           (pane-scroll-settle pane)
           (setf (pane-dirty pane) t))))))

;;; How far back a pane is being read. Nought is the screen as the program has
;;; it now; anything more is that many rows up into what has scrolled off it.
;;; It is the pane's and not the client's, the way the focus and the zoom are:
;;; everybody attached is looking at the same pane.

(defun pane-history (pane)
  "How many rows there are behind PANE's screen to scroll back into. A program
that has the whole screen to itself has none: what it draws never scrolls off."
  (let ((term (pane-term pane)))
    (if (term:term-in-alt-screen term) 0 (term:term-scrollback-size term))))

(defun pane-scroll-to (pane back)
  "Show PANE from BACK rows behind its screen, or as near as there is. Answers
whether that moved it."
  (let ((back (max 0 (min (pane-history pane) back))))
    (unless (= back (pane-scrolled pane))
      (setf (pane-scrolled pane) back
            (pane-dirty pane) t)
      t)))

(defun pane-scroll-by (pane rows)
  "ROWS further back, or nearer when it is negative."
  (pane-scroll-to pane (+ (pane-scrolled pane) rows)))

(defun pane-scroll-settle (pane)
  "The program wrote something. A pane being read back stays on the rows it was
showing, which are now further back by however many went off the top; one whose
program has taken the whole screen is back at it."
  (let* ((term (pane-term pane))
         (pushed (term:term-scrollback-pushed term))
         (more (- pushed (pane-pushed-seen pane))))
    (setf (pane-pushed-seen pane) pushed)
    (when (plusp (pane-scrolled pane))
      (setf (pane-scrolled pane)
            (max 0 (min (pane-history pane)
                        (+ (pane-scrolled pane) (max 0 more))))))))

(declaim (ftype function pane-say))

(defun pane-say (pane said)
  (when (and (pane-running pane) (pane-started pane))
    (ignore-errors (pty:pty-write-string (pane-fd pane) said))))

(defun pane-resize (pane rows cols)
  (term:term-resize (pane-term pane) cols rows)
  (ignore-errors (pty:pty-set-size (pane-fd pane) rows cols))
  (setf (pane-scrolled pane) (min (pane-scrolled pane) (pane-history pane))
        (pane-dirty pane) t)
  pane)

(defun pane-close (pane)
  (setf (pane-running pane) nil)
  (when (pane-started pane)
    (ignore-errors (pty:pty-close (pane-fd pane)))
    (ignore-errors (pty:pty-reap (pane-pid pane)))))

;;; What kind of program a pane holds, said the way a person would: the agent
;;; it was recognised as, or else whatever has the terminal now.

(defparameter +shells+
  '("sh" "bash" "zsh" "fish" "dash" "ksh" "mksh" "oksh" "tcsh" "csh" "yash"
    "nu" "elvish" "xonsh")
  "Programs that are a shell, and so are called one rather than by name.")

(defun program-name (line)
  "The program LINE runs: the first word, without where it lives, and without
the dash a login shell is started with."
  (let* ((said (string-trim " " (or line "")))
         (word (subseq said 0 (or (position #\Space said) (length said))))
         (base (subseq word (1+ (or (position #\/ word :from-end t) -1)))))
    (string-left-trim "-" base)))

(defun pane-kind (pane)
  "What the pane holds: a recognised agent's name, \"shell\" for a shell with
nothing in front of it, and otherwise the program in the foreground."
  (let ((agent (pane-agent pane)))
    (if (agent:agent-reader agent)
        (agent:agent-kind agent)
        (let ((name (program-name (or (first (pane-programs pane)) (pane-command pane)))))
          (cond ((zerop (length name)) "shell")
                ((member name +shells+ :test #'string=) "shell")
                (t name))))))

;;; Who typed into a pane, newest first. Keys somebody typed are counted, not
;;; kept: what is typed at a shell is theirs. What an agent verb sent is kept,
;;; since it was said on the record by one program to another.

(defparameter +keys-run+ 2000
  "Keys from one source this close together, in milliseconds, are one entry.")

(defun pane-logged (pane now who verb summary &optional (outcome t))
  "Put in PANE's log that WHO did VERB at NOW, and the time of day it was. For
:keys SUMMARY is how many bytes; a run of them from the same place is one
entry that grows."
  (let ((newest (first (pane-log pane))))
    (if (and newest (eq verb :keys) (eq (third newest) :keys)
             (equal (second newest) who)
             (<= (- now (first newest)) +keys-run+))
        (setf (first newest) now
              (fourth newest) (+ (fourth newest) summary)
              (sixth newest) (get-universal-time))
        (progn
          (push (list now who verb summary outcome (get-universal-time)) (pane-log pane))
          (when (> (incf (pane-log-count pane)) +log-length+)
            (setf (pane-log pane) (subseq (pane-log pane) 0 (floor +log-length+ 2))
                  (pane-log-count pane) (floor +log-length+ 2)))))
    (first (pane-log pane))))

(defun summarised (text &optional (most 60))
  (let ((one-line (substitute #\Space #\Newline (substitute #\Space #\Return text))))
    (if (> (length one-line) most)
        (concatenate 'string (subseq one-line 0 (1- most)) "…")
        one-line)))
