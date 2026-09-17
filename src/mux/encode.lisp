;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/mux)

(declaim (optimize (speed 3) (safety 1)))

(defun takes-of (&optional (term (sb-ext:posix-getenv "TERM"))
                           (colorterm (sb-ext:posix-getenv "COLORTERM")))
  "What the terminal named by TERM and COLORTERM will accept, as WRITE-SGR
wants it. Nothing here is asked of terminfo: the four things a face can carry
past the old SGR set are each either advertised or assumed absent."
  (let ((term (or term ""))
        (takes (list :blink)))
    (when (or (equal colorterm "truecolor") (equal colorterm "24bit")
              (search "direct" term))
      (push :rgb takes))
    (when (or (search "kitty" term) (search "wezterm" term)
              (search "foot" term) (search "contour" term)
              (search "vte" term) (search "alacritty" term)
              (search "ghostty" term))
      (push :underline-style takes)
      (push :underline-color takes))
    takes))

(defun write-cup (y x s)
  (declare (type fixnum y x))
  (write-char #\Escape s)
  (write-char #\[ s)
  (vt:write-number (1+ y) s)
  (write-char #\; s)
  (vt:write-number (1+ x) s)
  (write-char #\H s))

(defun blank-tail (row from to)
  "The column at or after FROM where the row is nothing but default blanks the
rest of the way to TO, or nil when it never is."
  (declare (type simple-vector row) (type fixnum from to))
  (let ((at to))
    (declare (type fixnum at))
    (loop while (> at from)
          for cell = (svref row (1- at))
          while (and (char= #\Space (vt:cell-char cell))
                     (vt:face-default-p (vt:cell-face cell)))
          do (decf at))
    (when (< at to) at)))

(defun encode-runs (screen runs s &key (takes t) (erase 4))
  (declare (type fixnum erase))
  "Write RUNS of SCREEN to stream S as the bytes a terminal reads.

Answers the face the terminal is left holding, so a caller writing more may
carry on from it."
  (let ((face :none))
    (dolist (run runs face)
      (let* ((y (run-row run))
             (row (screen-row screen y))
             (end (run-end run))
             (tail (and (= end (screen-width screen))
                        (blank-tail row (run-start run) end)))
             (stop (if (and tail (>= (- end tail) erase)) tail end))
             (x (run-start run)))
        (declare (type fixnum x stop))
        (write-cup y (run-start run) s)
        (loop while (< x stop)
              do (let* ((cell (svref row x))
                        (ch (vt:cell-char cell))
                        (now (vt:cell-face cell)))
                   (unless (and (not (eq face :none))
                                (vt:face-attrs-equal face now))
                     (vt:write-sgr now s takes)
                     (setf face now))
                   (write-char (if (graphic-char-p ch) ch #\Space) s)
                   (incf x (if (and (= 2 (vt:char-display-width ch))
                                    (< (1+ x) stop))
                               2
                               1))))
        (when (< stop end)
          (unless (and (not (eq face :none)) (vt:face-default-p face))
            (vt:write-sgr nil s takes)
            (setf face nil))
          (write-char #\Escape s) (write-char #\[ s) (write-char #\K s))))))

(defun encode-cursor (screen s)
  (write-cup (screen-cursor-y screen) (screen-cursor-x screen) s)
  (write-char #\Escape s) (write-char #\[ s)
  (write-char #\? s) (write-char #\2 s) (write-char #\5 s)
  (write-char (if (screen-cursor-visible screen) #\h #\l) s))

(defun encode-frame (screen runs s &key (takes t))
  (encode-runs screen runs s :takes takes)
  (vt:write-sgr nil s takes)
  (encode-cursor screen s))
