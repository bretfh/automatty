(in-package #:vt/test)

(def-suite host :in all)
(in-suite host)

(defmacro with-slave ((master slave) &body body)
  (let ((path (gensym "PATH")))
    `(multiple-value-bind (,master ,path) (pty:open-pty)
                          (let ((,slave (sb-posix:open ,path sb-posix:o-rdwr)))
                            (unwind-protect (progn ,@body)
                              (ignore-errors (sb-posix:close ,slave))
                              (ignore-errors (pty:pty-close ,master)))))))

(test a-terminal-says-how-big-it-is
      (with-slave (master slave)
                  (pty:pty-set-size master 30 100)
                  (multiple-value-bind (rows cols) (tty:host-size slave)
                                       (is (eql 30 rows))
                                       (is (eql 100 cols)))
                  (pty:pty-set-size master 12 40)
                  (multiple-value-bind (rows cols) (tty:host-size slave)
                                       (is (eql 12 rows))
                                       (is (eql 40 cols)))))

(test a-pipe-is-not-a-terminal-and-a-pty-is
      (multiple-value-bind (in out) (sb-posix:pipe)
                           (unwind-protect (is (null (tty:a-terminal-p in)))
                             (sb-posix:close in)
                             (sb-posix:close out)))
      (with-slave (master slave)
                  (is (tty:a-terminal-p master))
                  (is (tty:a-terminal-p slave))))

(test a-terminal-in-raw-mode-echoes-nothing-and-waits-for-no-line
      (with-slave (master slave)
                  (is (plusp master))
                  (tty:host-raw slave)
                  (let ((now (sb-posix:tcgetattr slave)))
                    (is (zerop (logand (sb-posix:termios-lflag now) sb-posix:echo)))
                    (is (zerop (logand (sb-posix:termios-lflag now) sb-posix:icanon)))
                    (is (zerop (logand (sb-posix:termios-lflag now) sb-posix:isig)))
                    (is (zerop (logand (sb-posix:termios-oflag now) sb-posix:opost)))
                    (is (zerop (logand (sb-posix:termios-iflag now) sb-posix:icrnl)))
                    (is (plusp (logand (sb-posix:termios-cflag now) sb-posix:cs8)))
                    (is (eql 1 (aref (sb-posix:termios-cc now) sb-posix:vmin))))))

(test a-terminal-is-put-back-the-way-it-was
      (with-slave (master slave)
                  (is (plusp master))
                  (let* ((was (tty:host-raw slave))
                         (raw (sb-posix:tcgetattr slave)))
                    (is (/= (sb-posix:termios-lflag was) (sb-posix:termios-lflag raw))
                        "raw mode changed nothing, so the test proves nothing")
                    (tty:host-put-back slave was)
                    (let ((back (sb-posix:tcgetattr slave)))
                      (is (eql (sb-posix:termios-iflag was) (sb-posix:termios-iflag back)))
                      (is (eql (sb-posix:termios-oflag was) (sb-posix:termios-oflag back)))
                      (is (eql (sb-posix:termios-cflag was) (sb-posix:termios-cflag back)))
                      (is (eql (sb-posix:termios-lflag was) (sb-posix:termios-lflag back)))))))

(test what-has-something-to-read-says-so-and-what-has-not-does-not
      (multiple-value-bind (in out) (sb-posix:pipe)
                           (multiple-value-bind (quiet-in quiet-out) (sb-posix:pipe)
                                                (unwind-protect
                                                    (let ((w (tty:make-waiting 2)))
                                                      (unwind-protect
                                                          (let ((busy (tty:waiting-add w in))
                                                                (idle (tty:waiting-add w quiet-in)))
                                                            (is (eql 0 (tty:wait-on w 0)))
                                                            (pty:pty-write-string out "hi")
                                                            (is (eql 1 (tty:wait-on w 50)))
                                                            (is (tty:readable-p (tty:waiting-back w busy)))
                                                            (is (null (tty:readable-p (tty:waiting-back w idle)))))
                                                        (tty:free-waiting w)))
                                                  (sb-posix:close in) (sb-posix:close out)
                                                  (sb-posix:close quiet-in) (sb-posix:close quiet-out)))))

(test a-descriptor-whose-other-end-is-gone-says-so
      (multiple-value-bind (in out) (sb-posix:pipe)
                           (let ((w (tty:make-waiting 1)))
                             (unwind-protect
                                 (let ((n (tty:waiting-add w in)))
                                   (sb-posix:close out)
                                   (is (eql 1 (tty:wait-on w 50)))
                                   (is (tty:gone-p (tty:waiting-back w n))))
                               (tty:free-waiting w)
                               (ignore-errors (sb-posix:close in))))))

(test a-set-that-was-cleared-waits-on-nothing
      (let ((w (tty:make-waiting 4)))
        (unwind-protect
            (progn
              (tty:waiting-add w 0)
              (is (eql 1 (tty:waiting-count w)))
              (tty:waiting-clear w)
              (is (eql 0 (tty:waiting-count w)))
              (is (eql 0 (tty:wait-on w 0))))
          (tty:free-waiting w))))

(test a-set-grows-past-the-room-it-was-made-with
      (let ((w (tty:make-waiting 1)))
        (unwind-protect
            (multiple-value-bind (in out) (sb-posix:pipe)
                                 (unwind-protect
                                     (progn
                                       (dotimes (i 40) (tty:waiting-add w in))
                                       (is (eql 40 (tty:waiting-count w)))
                                       (pty:pty-write-string out "x")
                                       (is (eql 40 (tty:wait-on w 50)))
                                       (is (tty:readable-p (tty:waiting-back w 39))))
                                   (sb-posix:close in) (sb-posix:close out)))
          (tty:free-waiting w))))
