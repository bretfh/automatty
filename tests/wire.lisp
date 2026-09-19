(in-package #:vt/test)

(def-suite wire :in all)
(in-suite wire)

(defmacro with-wires ((sender reader) &body body)
  (let ((in (gensym "IN")) (out (gensym "OUT")))
    `(multiple-value-bind (,in ,out) (sb-posix:pipe)
       (let ((,sender (mux:make-wire ,out))
             (,reader (mux:make-wire ,in)))
         (unwind-protect (progn ,@body)
           (mux:wire-close ,sender)
           (mux:wire-close ,reader))))))

(defun message-bytes (form)
  (let* ((text (with-standard-io-syntax
                 (let ((*package* (find-package '#:vt/mux)))
                   (prin1-to-string form))))
         (bytes (sb-ext:string-to-octets text :external-format :utf-8)))
    (concatenate 'string (format nil "~D~C" (length bytes) #\Newline)
                 (map 'string #'code-char bytes))))

(defun message-bytes-of-text (text)
  (let ((bytes (sb-ext:string-to-octets text :external-format :utf-8)))
    (concatenate 'string (format nil "~D~C" (length bytes) #\Newline)
                 (map 'string #'code-char bytes))))

(defun taken (reader)
  (mux:wire-fill reader)
  (mux:wire-take reader))

(test a-message-written-and-read-back-is-the-same-message
  (with-wires (sender reader)
    (let ((said '(:attach 24 80 (:rgb :blink))))
      (mux:wire-send sender said)
      (is (mux:wire-flush sender))
      (is (equal said (taken reader)))
      (is (null (mux:wire-take reader))))))

(test two-messages-in-one-read-are-two-messages
  (with-wires (sender reader)
    (mux:wire-send sender '(:keys "abc"))
    (mux:wire-send sender '(:resize 40 120))
    (mux:wire-flush sender)
    (is (equal '(:keys "abc") (taken reader)))
    (is (equal '(:resize 40 120) (mux:wire-take reader)))
    (is (null (mux:wire-take reader)))))

(test a-message-delivered-a-byte-at-a-time-is-one-message
  (with-wires (sender reader)
    (let ((bytes (message-bytes '(:cursor 3 4 t :block))))
      (dotimes (i (1- (length bytes)))
        (pty:pty-write-string (mux:wire-fd sender) (string (char bytes i)))
        (mux:wire-fill reader)
        (is (null (mux:wire-take reader))
            "a message said itself before all of it had arrived"))
      (pty:pty-write-string (mux:wire-fd sender)
                            (string (char bytes (1- (length bytes)))))
      (is (equal '(:cursor 3 4 t :block) (taken reader))))))

(test a-message-may-not-evaluate-anything
  (with-wires (sender reader)
    (pty:pty-write-string (mux:wire-fd sender)
                          (message-bytes-of-text "(:keys #.(quit))"))
    (signals error (taken reader))))

(test a-message-that-lies-about-its-length-is-refused
  (with-wires (sender reader)
    (pty:pty-write-string (mux:wire-fd sender)
                          (format nil "nine~C(:keys \"x\")" #\Newline))
    (signals error (taken reader))))

(test a-frame-that-went-across-the-wire-is-the-same-screen
  (let* ((pane (a-term :width 20 :height 3))
         (screen (a-screen :width 20 :height 3))
         (was (a-screen :width 20 :height 3))
         (there (a-screen :width 20 :height 3)))
    (say pane (csi "1;31m") "red" (csi "0m") " plain "
         (csi "38;2;10;200;30m") "green" (csi "0m")
         (esc "[2;1H") (format nil "~C~C" (code-char #x6F22) (code-char #x5B57)))
    (blit screen pane)
    (multiple-value-bind (said faces)
        (mux:runs-said screen (tty:screen-diff was screen))
      (let ((form (with-standard-io-syntax
                    (let ((*package* (find-package '#:vt/mux)))
                      (read-from-string (prin1-to-string (list said faces)))))))
        (mux:said-into-screen there (first form) (second form))))
    (dotimes (y 3)
      (dotimes (x 20)
        (let ((a (cell-at screen x y))
              (b (cell-at there x y)))
          (is (char= (vt:cell-char a) (vt:cell-char b))
              "~D,~D is ~S here and ~S there" x y
              (vt:cell-char a) (vt:cell-char b))
          (is (vt:face-equal (vt:cell-face a) (vt:cell-face b))
              "~D,~D wears a different face there" x y))))))

(test what-came-off-the-wire-drawn-is-what-was-put-on-it
  (let* ((pane (a-term :width 20 :height 3))
         (screen (a-screen :width 20 :height 3))
         (was (a-screen :width 20 :height 3))
         (there (a-screen :width 20 :height 3))
         (host (a-host there)))
    (say pane (csi "4:3;58;5;9m") "curly" (csi "0m") (esc "[2;1H") (csi "7m") "flipped")
    (blit screen pane)
    (multiple-value-bind (said faces)
        (mux:runs-said screen (tty:screen-diff was screen))
      (let ((runs (mux:said-into-screen there said faces)))
        (say host (with-output-to-string (s)
                    (tty:encode-frame there runs s)))))
    (is (null (difference there host)) "~A" (difference there host))))

(test what-a-peer-names-is-not-named-in-this-image
  (let ((form (let ((*read-eval* nil)
                    (*package* (mux::reading-package)))
                (read-from-string "(:keys a-name-nobody-here-uses nil t)"))))
    (is (eq :keys (first form)))
    (is (null (find-symbol "A-NAME-NOBODY-HERE-USES" '#:vt/mux))
        "a name off the wire was interned where the program's own names live")
    (is (null (third form)) "nil off the wire did not read as nil")
    (is (eq t (fourth form)) "t off the wire did not read as t")))

(test the-package-a-peer-names-things-in-is-thrown-away-when-it-fills
  (let ((was (mux::reading-package)))
    (dotimes (i (1+ mux::+most-names+))
      (intern (format nil "MADE-UP-~D" i) was))
    (is-true (mux::too-many-names-p was))
    (let ((mux::*reads* 1023))
      (is (not (eq was (mux::reading-package)))
          "a package full of made-up names was kept"))))
