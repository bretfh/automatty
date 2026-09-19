(defpackage #:vt/test
  (:use #:cl #:fiveam #:vt/test/term)
  (:local-nicknames (#:pty #:vt/pty) (#:tty #:vt/tty) (#:mux #:vt/mux)
                    (#:cells #:vt/cells))
  (:shadow #:run-them)
  (:export #:run-them #:all))
(in-package #:vt/test)

(def-suite all)

(defun run-them ()
  "Everything: the emulator's own suite, which vt runs on its own, and then
everything built on it."
  (let ((results (append (run 'vt/test/term::emulator) (run 'all))))
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
  (let* ((row (tty:screen-row screen y))
         (s (make-string (tty:screen-width screen))))
    (dotimes (x (length s) (string-right-trim " " s))
      (setf (schar s x) (vt:cell-char (aref row x))))))

(defun cell-at (screen x y)
  (aref (tty:screen-row screen y) x))

(defun laid-out (runs)
  (mapcar (lambda (r) (list (tty:run-row r) (tty:run-start r) (tty:run-end r)))
          runs))

(defun at-screen (screen x y)
  (vt:cell-char (aref (tty:screen-row screen y) x)))
