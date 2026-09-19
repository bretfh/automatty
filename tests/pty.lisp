(in-package #:vtx/test)

(def-suite pty :in all)
(in-suite pty)

(defmacro with-pty ((fd pid command &rest args) &body body)
  `(multiple-value-bind (,fd ,pid) (pty:spawn-pty-process ,command ,@args)
     (unwind-protect (progn ,@body)
       (ignore-errors (pty:pty-close ,fd))
       (ignore-errors (pty:pty-reap ,pid)))))

(defun soak (term fd &key (seconds 5))
  "Read until the program has nothing more to say, or until SECONDS is up. Not
until the first quiet moment: a program that is asleep has not finished."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :while (< (get-internal-real-time) deadline)
          :do (when (pty:pty-wait fd 100)
                (let ((said (pty:pty-read-string fd 8192)))
                  (if said
                      (vt:term-process-output term said)
                      (return)))))
    term))

(defun until-said (term fd wanted &key (seconds 5))
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :until (search wanted (vt:term-dump-to-string term))
          :do (when (> (get-internal-real-time) deadline) (return nil))
             (when (pty:pty-wait fd 100)
               (let ((said (pty:pty-read-string fd 8192)))
                 (if said
                     (vt:term-process-output term said)
                     (return (search wanted (vt:term-dump-to-string term))))))
          :finally (return t))))

(defun screen (term) (vt:term-dump-to-string term))

(test a-program-on-a-pty-says-what-it-printed
  (let ((term (a-term :width 40 :height 10)))
    (with-pty (fd pid "printf 'from-the-pty\\n'" :rows 10 :cols 40)
      (is (plusp fd))
      (is (plusp pid))
      (is-true (until-said term fd "from-the-pty")))))

(test what-a-program-coloured-is-a-face-on-the-grid
  (let ((term (a-term :width 40 :height 10)))
    (with-pty (fd pid "printf '\\033[31mred\\033[0m\\n'" :rows 10 :cols 40)
      (until-said term fd "red")
      (let ((x (search "red" (vt:term-dump-row-string term 0))))
        (is-true x)
        (when x
          (is (eql 1 (vt:face-fg (face-at term x 0)))))))))

(test a-program-is-told-how-big-its-terminal-is
  (let ((term (a-term :width 77 :height 11)))
    (with-pty (fd pid "stty size" :rows 11 :cols 77)
      (is-true (until-said term fd "11 77")))))

(test what-is-written-to-a-pty-the-program-reads
  (let ((term (a-term :width 40 :height 10)))
    (with-pty (fd pid "read line; printf 'heard %s\\n' \"$line\"")
      (pty:pty-write-string fd (format nil "spoken~C" #\Newline))
      (is-true (until-said term fd "heard spoken")))))

(test a-pty-that-is-done-with-is-reaped
  (multiple-value-bind (fd pid) (pty:spawn-pty-process "sleep 30")
    (is (plusp pid))
    (pty:pty-close fd)
    (is (integerp (pty:pty-reap pid)))
    (is (null (pty:pty-reap pid)))))

;;; What separates a terminal from a pipe that answers isatty. Each of these is
;;; also what says whether this system's numbers up in pty.lisp are the right
;;; ones: get POSIX_SPAWN_SETSID wrong and every one of them fails at once.

(test the-child-is-a-session-of-its-own-on-this-terminal
  (let ((term (a-term :width 80 :height 10)))
    (with-pty (fd pid "ps -o pid,sid,tty= -p $$" :rows 10 :cols 80)
      (soak term fd :seconds 3)
      (let ((said (screen term)))
        (is (search "pts" said) "the child is on a pty: ~S" said)
        (let* ((line (string-trim " " (vt:term-dump-row-string term 1)))
               (numbers (with-input-from-string (s line)
                          (list (read s nil) (read s nil)))))
          (is (eql (first numbers) (second numbers))
              "pid and session id are the same, so it leads its own session: ~S"
              line))))))

(test the-child-can-open-its-controlling-terminal
  (let ((term (a-term :width 40 :height 10)))
    (with-pty (fd pid "echo opened > /dev/tty 2>/dev/null || echo no-dev-tty")
      (soak term fd :seconds 3)
      (is (search "opened" (screen term)))
      (is (not (search "no-dev-tty" (screen term)))))))

(test a-resize-reaches-the-program-as-a-signal
  (let ((term (a-term :width 80 :height 24)))
    (with-pty (fd pid "trap 'echo GOT-WINCH; stty size' WINCH; sleep 1; echo done"
                   :rows 24 :cols 80)
      (sleep 0.3)
      (pty:pty-set-size fd 40 100)
      (vt:term-resize term 100 40)
      (soak term fd :seconds 4)
      (is (search "GOT-WINCH" (screen term)))
      (is (search "40 100" (screen term))))))

(test an-interrupt-character-becomes-a-signal
  (let ((term (a-term :width 80 :height 24)))
    (with-pty (fd pid "trap 'echo GOT-SIGINT' INT; sleep 1; echo done")
      (sleep 0.3)
      (pty:pty-write-string fd (string (code-char 3)))
      (soak term fd :seconds 4)
      (is (search "GOT-SIGINT" (screen term))))))

(test a-write-is-finished-however-many-goes-it-takes
  ;; -echo, or the line discipline says it back as well and every z is two
  (let ((term (a-term :width 80 :height 24))
        (said (make-string 1000 :initial-element #\z)))
    (with-pty (fd pid "stty -echo; cat")
      (sleep 0.3)
      (is (= 1000 (pty:pty-write-string fd said)))
      (pty:pty-write-string fd (format nil "~C" #\Newline))
      (soak term fd :seconds 3)
      (is (= 1000 (count #\z (screen term)))))))

(test what-is-typed-is-counted-in-bytes-not-characters
  (with-pty (fd pid "cat > /dev/null")
    (is (= 1 (pty:pty-write-string fd (string (code-char #xE9))))
        "a character is a byte unless the caller says otherwise")
    (is (= 2 (pty:pty-write-string fd (string (code-char #xE9)) :utf-8)))
    (is (= 3 (pty:pty-write-string fd (string (code-char #x3042)) :utf-8)))))

(test what-was-read-from-one-terminal-written-to-another-is-the-same-bytes
  (let ((term (a-term :width 40 :height 4)))
    (with-pty (fd pid "printf '\342\224\234\342\224\200\342\224\200 tree\n'")
      (until-said term fd "tree")
      (with-pty (again other "cat")
        (let ((said (vt:term-dump-row-string term 0)))
          (pty:pty-write-string again (string-right-trim " " said))
          (pty:pty-write-string again (string #\Return))
          (let ((back (a-term :width 40 :height 4)))
            (until-said back again "tree")
            (is (equal (string-right-trim " " said)
                       (row back 0))
                "the bytes came back different: ~S then ~S"
                (string-right-trim " " said) (row back 0))))))))

(test reading-a-terminal-that-is-gone-is-a-fault-not-an-end
  (multiple-value-bind (fd pid) (pty:spawn-pty-process "sleep 5")
    (pty:pty-close fd)
    (signals error (pty:pty-read-string fd 64))
    (pty:pty-reap pid)))

(test a-terminal-of-a-size-nobody-can-have-says-so
  (with-pty (fd pid "sleep 1")
    (signals error (pty:pty-set-size -1 24 80))))

(test a-program-that-ignores-being-asked-is-not-waited-on-forever
  (multiple-value-bind (fd pid)
      (pty:spawn-pty-process "trap '' TERM HUP; sleep 60")
    (unwind-protect
         (let ((then (get-internal-real-time)))
           (pty:pty-close fd)
           (pty:pty-reap pid 1/4)
           (let ((took (/ (- (get-internal-real-time) then)
                          internal-time-units-per-second)))
             (is (< took 5) "reaping took ~,1F seconds" took)))
      (ignore-errors (pty:pty-kill pid 9)))
    (is (null (pty:pty-reap pid 1/4)) "it is still there")))

(test reaping-a-program-that-has-already-gone-is-not-a-fault
  (multiple-value-bind (fd pid) (pty:spawn-pty-process "true")
    (pty:pty-close fd)
    (pty:pty-reap pid)
    (is (null (pty:pty-reap pid 1/4)))))
