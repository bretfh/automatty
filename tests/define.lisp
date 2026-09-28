(in-package #:atty/test)

(def-suite define :in all)
(in-suite define)

(defun frame-widths (layout width)
  (let ((tree (mux::layout-tree layout)))
    (laid tree (tty:make-screen :width width :height 6))
    (mapcar (lambda (frame) (- (atty/ui:right frame) (atty/ui:left frame)))
            (atty/ui:parts tree))))

(test a-split-gives-its-parts-the-cells-and-shares-they-were-given
  (let ((one (a-pane "")) (two (a-pane "")) (three (a-pane "")))
    (is (equal '(30 10) (frame-widths (mux:make-split :across (list one two) '(nil 10)) 40)))
    (is (equal '(10 30) (frame-widths (mux:make-split :across (list one two) '(1/4 nil)) 40)))
    (is (equal '(6 6 8) (frame-widths (mux:make-split :across (list one two three) '(nil nil 30)) 20))
        "parts that do not fit were not squeezed to the room there is")))

(test a-split-keeps-its-sizes-through-disk-and-through-a-pane-going
  (let* ((one (a-pane "")) (two (a-pane "")) (three (a-pane ""))
         (layout (mux:make-split :across (list one (mux:make-split :down (list two three) '(5 nil)))
                                 '(nil 1/3)))
         (panes (make-hash-table)))
    (dolist (p (list one two three)) (setf (gethash (mux:pane-id p) panes) p))
    (let ((back (mux::decode-layout (mux::encode-layout layout) panes)))
      (is (equal '(nil 1/3) (mux::split-sizes back)))
      (is (equal '(5 nil) (mux::split-sizes (second (mux:split-parts back)))))
      (is (equal (list one two three) (mux:layout-panes back))))
    (let ((left (mux:layout-remove layout one)))
      (is (equal '(5 nil) (mux::split-sizes left))
          "the split left standing lost its sizes"))))

(defun defined (&rest sessions)
  (let ((mux::*defining* t))
    (list :server :name "here" :sessions sessions)))

(test filling-in-makes-what-is-missing-by-name-and-nothing-twice
  (with-a-server-here (server path)
    (let ((mux::*defining* t))
      (let ((form (defined (mux::session "s" (mux::window "a" "cat")
                                             (mux::window "b" (mux::columns "cat" "cat"))))))
        (mux::fill-in-server server form)
        (let* ((session (mux:session-named server "s"))
               (panes (mux:session-panes session)))
          (is (equal '("a" "b") (mapcar #'mux::window-label (mux::session-windows session))))
          (is (= 3 (length panes)))
          (mux::fill-in-server server form)
          (is (= 1 (length (mux::server-sessions server))))
          (is (equal panes (mux:session-panes session)) "filling in again made or remade panes")
          (mux::fill-in-server server (defined (mux::session "s" (mux::window "a" "true")
                                                             (mux::window "c" "cat"))))
          (is (equal '("a" "b" "c") (mapcar #'mux::window-label (mux::session-windows session))))
          (is (equal panes (subseq (mux:session-panes session) 0 3))
              "a window already there was changed"))))))

(defvar *ran-with* nil)

(mux:defcommand test-given ((word :string "word") (n :number "n"))
  (setf *ran-with* (list word n)))

(defmethod mux::ask-for-arguments ((client (eql :asking)) name given)
  (setf *ran-with* (list :asked name given)))

(test a-command-is-given-what-follows-its-name-and-asks-for-the-rest
  (unwind-protect
       (progn
         (multiple-value-bind (name arguments) (mux::split-command-line "test given \"two words\" 3")
           (is (equal "test given" name))
           (is (equal '("two words" "3") arguments))
           (mux:run-command name :asking arguments)
           (is (equal '("two words" 3) *ran-with*)))
         (mux:run-command "test given" :asking '("one"))
         (is (equal '(:asked "test given" ("one")) *ran-with*)))
    (remhash "test given" mux::*commands*)
    (remhash "test given" mux::*command-arguments*)))

(test only-one-of-two-servers-starting-together-starts-a-third
  (let ((name (format nil "claim-~D-~D" (sb-posix:getpid) (random 1000000))))
    (unwind-protect
         (progn (is-true (mux::claim-start name))
                (is-false (mux::claim-start name)))
      (ignore-errors (delete-file (mux::starting-file name))))))

(test norc-reads-no-init
  (let ((mux::*no-init* t))
    (is (null (mux::user-init-file)))
    (is-false (mux:load-user-init))
    (is (null mux::*definitions*))))
