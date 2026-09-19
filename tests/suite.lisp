(defpackage #:vtx/test
  (:use #:cl #:fiveam #:libvtx/test)
  (:local-nicknames (#:pty #:vtx/pty) (#:tty #:vtx/tty) (#:mux #:vtx)
                    (#:cells #:vtx/cells))
  (:shadow #:run-them)
  (:export #:run-them #:all))
(in-package #:vtx/test)

(def-suite all)

(defun run-them ()
  "Everything: the emulator's own suite, which vt runs on its own, and then
everything built on it."
  (let ((results (append (run 'libvtx/test::emulator) (run 'all))))
    (explain! results)
    (unless (results-status results)
      (uiop:quit 1))))

(defun a-screen (&rest args)
  (apply #'tty:make-screen args))

(defun blit (screen term &key (top 0) (left 0))
  (cells:blit (cells:make-cells (tty:screen-grid screen)
                                (tty:screen-width screen)
                                (tty:screen-height screen))
              term left top
              (- (tty:screen-width screen) left)
              (- (tty:screen-height screen) top))
  screen)

(defun count-of (needle said)
  (loop :with n := 0 :with at := 0
        :for found := (search needle said :start2 at)
        :while found
        :do (incf n) (setf at (+ found (length needle)))
        :finally (return n)))

(defun shown (screen y)
  (string-right-trim " " (subseq (vt:row-chars (tty:screen-row screen y))
                                 0 (tty:screen-width screen))))

(defun char-at (screen x y)
  (vt:row-char (tty:screen-row screen y) x))

(defun face-on (screen x y)
  (vt:row-face (tty:screen-row screen y) x))

(defun laid-out (runs)
  (mapcar (lambda (r) (list (tty:run-row r) (tty:run-start r) (tty:run-end r)))
          runs))

(defun at-screen (screen x y)
  (vt:row-char (tty:screen-row screen y) x))
