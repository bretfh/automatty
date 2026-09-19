;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vtx/tty)

(declaim (optimize (speed 3) (safety 1)))

(defparameter +everything+ '(:rgb :underline-style :underline-color :blink)
  "Everything past the old SGR set that a face can carry.")

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
    ;; a terminal that takes all of it says so, rather than being asked four
    ;; questions about every face that goes out
    (if (= (length takes) (length +everything+)) t takes)))

(defun taken (what takes)
  (or (eq takes t) (and (member what takes) t)))

(defun flattened (colour)
  "An rgb triple as the nearest of the 256, and anything else as it was."
  (if (and (consp colour) (= 3 (length colour)))
      (vt:rgb-to-color-index (first colour) (second colour) (third colour))
      colour))

(defun as-taken (face takes)
  "FACE as a face this terminal can wear.

vt writes a face whole and has no opinion about terminals. Which of it this one
will accept is this side's business, so what it cannot take comes down to what
it can before the face ever reaches VT:WRITE-SGR. A terminal that takes
everything gets the face it was given and nothing is copied."
  (if (or (null face) (eq takes t))
      face
      (let ((rgb (taken :rgb takes))
            (style (taken :underline-style takes))
            (under (taken :underline-color takes))
            (blink (taken :blink takes)))
        (if (and rgb style under blink)
            face
            (let ((out (vt:copy-face face)))
              (unless rgb
                (setf (vt:face-fg out) (flattened (vt:face-fg out))
                      (vt:face-bg out) (flattened (vt:face-bg out))
                      (vt:face-underline-color out)
                      (flattened (vt:face-underline-color out))))
              (unless style
                (when (vt:face-underline out)
                  (setf (vt:face-underline out) :single)))
              (unless under (setf (vt:face-underline-color out) nil))
              (unless blink (setf (vt:face-blink out) nil))
              out)))))

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
  (declare (type fixnum from to))
  (let ((at to))
    (declare (type fixnum at))
    (loop while (> at from)
          while (and (char= #\Space (vt:row-char row (1- at)))
                     (vt:face-default-p (vt:row-face row (1- at))))
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
              do (let ((ch (vt:row-char row x))
                       (now (vt:row-face row x)))
                   (unless (and (not (eq face :none))
                                (vt:face-equal face now))
                     (vt:write-sgr (as-taken now takes) s)
                     (setf face now))
                   (write-char (if (graphic-char-p ch) ch #\Space) s)
                   (incf x (if (and (= 2 (vt:char-display-width ch))
                                    (< (1+ x) stop))
                               2
                             1))))
        (when (< stop end)
          (unless (and (not (eq face :none)) (vt:face-default-p face))
            (vt:write-sgr nil s)
            (setf face nil))
          (write-char #\Escape s) (write-char #\[ s) (write-char #\K s))))))

(defparameter +cursor-shapes+
  '((:blinking-block . 1) (:block . 2)
    (:blinking-underline . 3) (:underline . 4)
    (:blinking-bar . 5) (:bar . 6))
  "What DECSCUSR calls each shape a program can ask its cursor to be.")

(defun write-cursor-shape (style s)
  (let ((n (cdr (assoc style +cursor-shapes+))))
    (when n
      (write-char #\Escape s) (write-char #\[ s)
      (vt:write-number n s)
      (write-char #\Space s) (write-char #\q s))))

(defun encode-cursor (screen s)
  (write-cursor-shape (screen-cursor-style screen) s)
  (write-cup (screen-cursor-y screen) (screen-cursor-x screen) s)
  (write-char #\Escape s) (write-char #\[ s)
  (write-char #\? s) (write-char #\2 s) (write-char #\5 s)
  (write-char (if (screen-cursor-visible screen) #\h #\l) s))

(defun encode-frame (screen runs s &key (takes t))
  (encode-runs screen runs s :takes takes)
  (vt:write-sgr nil s)
  (encode-cursor screen s))
