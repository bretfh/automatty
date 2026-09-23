;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun format-duration (ms)
  "MS as a person reads a time that has passed: 41s, 3m, 2h."
  (cond ((null ms) "")
        ((< ms 60000) (format nil "~Ds" (floor ms 1000)))
        ((< ms 3600000) (format nil "~Dm" (floor ms 60000)))
        (t (format nil "~Dh" (floor ms 3600000)))))

(defun truncate-string (text most)
  (if (<= (length text) most)
      text
      (concatenate 'string (subseq text 0 (max 0 (1- most))) "…")))

(defun abbreviate-directory (directory)
  "DIRECTORY with the home directory said as ~."
  (let ((home (namestring (user-homedir-pathname)))
        (said (namestring directory)))
    (if (and (> (length said) (length home)) (string= home said :end2 (length home)))
        (concatenate 'string "~/" (subseq said (length home)))
        (string-right-trim "/" said))))

(defun format-day-time (universal-time)
  (multiple-value-bind (s m h day month) (decode-universal-time universal-time)
    (declare (ignore s))
    (format nil "~D ~A ~2,'0D:~2,'0D" day
            (nth (1- month) '("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                              "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
            h m)))

(defun format-clock-hms (clock)
  "The universal time CLOCK as the time of day, hh:mm:ss, or blank if there
is none."
  (if clock
      (multiple-value-bind (s m h) (decode-universal-time clock)
        (format nil "~2,'0D:~2,'0D:~2,'0D" h m s))
      "        "))

(defun format-actor (actor client &key short)
  "WHO from a pane's log as it is said: ◆ here for this terminal, ⌨ and its
tty for another, ⌁ and its path for a pane, $ for the command line. SHORT is
the glyph alone."
  (case (first actor)
    (:client (cond ((and client (eql (second actor) (client-id client))) (if short "◆" "◆ here"))
                   (short "⌨")
                   (t (format nil "⌨ ~A" (format-tty (third actor) (second actor))))))
    (:pane (if short "⌁" (format nil "⌁ ~A" (second actor))))
    (:atty (if short "↺" "atty"))
    (t (if short "$" "$ cli"))))

(defun actor-face (actor client)
  (case (first actor)
    (:client (if (and client (eql (second actor) (client-id client))) :here :client))
    (:pane :driven)
    (t :quiet)))

(defun actor-label (actor client &key (pad 0))
  "WHO as a label in its own colour, PAD wide at least."
  (atty/ui:label (format nil "~vA" pad (format-actor actor client)) :face (actor-face actor client)))

(defun client-label (row client &key (pad 0))
  "An attached terminal as the server lists it, (id tty …), as a label."
  (actor-label (list :client (first row) (second row)) client :pad pad))

(defun this-client-p (row client)
  (and client (eql (first row) (client-id client))))

(defun event-glyph (kind)
  (case kind
    (:asks "▲") (:working "◐") (:idle "○") (:quiet "·")
    (:answered "✓") (:typed "⌨") (:prompted "›") (:said "›") (:signalled "!")
    (:started "▶") (:finished "■") (:restored "↺")
    (t " ")))

(defun event-face (kind)
  (case kind
    (:asks :state-blocked-strong) (:working :state-working) (:idle :state-idle)
    (:answered :state-idle) (:started :state-working) (:finished :error)
    (t :quiet)))

(defun format-event (kind text)
  "What an event was, in a few words, without who did it."
  (case kind
    (:asks (format nil "asks~@[: ~A~]" text))
    (:working "working")
    (:idle "finished")
    (:quiet "quiet")
    (:typed "typed")
    (:answered (format nil "answered~@[ ~A~]" text))
    (:prompted (format nil "prompted~@[ ~A~]" text))
    (:said (format nil "was told~@[ ~A~]" text))
    (:signalled (format nil "signalled~@[ ~A~]" text))
    (:started (format nil "started~@[ ~A~]" text))
    (:finished (format nil "finished~@[ ~A~]" text))
    (:restored (format nil "restored~@[ ~A~]" text))
    (t (format nil "~(~A~)~@[ ~A~]" kind text))))

(defun format-clock-hm (clock)
  "The universal time CLOCK as hh:mm, or blank."
  (if clock
      (multiple-value-bind (s m h) (decode-universal-time clock)
        (declare (ignore s))
        (format nil "~2,'0D:~2,'0D" h m))
      "     "))

(defun format-current-time ()
  (multiple-value-bind (second minute hour) (decode-universal-time (get-universal-time))
    (declare (ignore second))
    (format nil "~2,'0D:~2,'0D" hour minute)))

(defun program-display-name (command)
  "What to call a program on a bar. A shell out of the store is a path nobody
reads to the end, and its name is the last thing in it."
  (let* ((said (string-trim " " (or command "")))
         (space (position #\Space said))
         (first-word (subseq said 0 (or space (length said))))
         (slash (position #\/ first-word :from-end t))
         (name (if slash (subseq first-word (1+ slash)) first-word)))
    (if space
        (concatenate 'string name (subseq said space))
        name)))

(defun format-tty (tty id)
  (cond ((and (stringp tty) (> (length tty) 5) (string= "/dev/" tty :end2 5)) (subseq tty 5))
        ((and (stringp tty) (plusp (length tty))) tty)
        (t (format nil "client ~D" id))))
