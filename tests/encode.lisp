(in-package #:vt/test)

(def-suite encode :in all)
(in-suite encode)

(defun a-host (screen)
  (let ((term (a-term :width (mux:screen-width screen)
                      :height (mux:screen-height screen))))
    (say term (csi "?7l"))
    term))

(defun difference (screen term)
  (dotimes (y (mux:screen-height screen))
    (dotimes (x (mux:screen-width screen))
      (let ((a (cell-at screen x y))
            (b (aref (vt:term-grid-row term y) x)))
        (unless (and (char= (vt:cell-char a) (vt:cell-char b))
                     (vt:face-attrs-equal (vt:cell-face a) (vt:cell-face b)))
          (return-from difference
            (format nil "~D,~D is ~S ~S on the screen and ~S ~S on the host"
                    x y (vt:cell-char a) (vt:face-attrs-to-plist (vt:cell-face a))
                    (vt:cell-char b) (vt:face-attrs-to-plist (vt:cell-face b)))))))))

(defun sent (screen runs &key (takes t))
  (with-output-to-string (s)
    (mux:encode-frame screen runs s :takes takes)))

(test what-was-encoded-and-read-back-is-the-same-screen
  (let* ((pane (a-term :width 20 :height 3))
         (screen (a-screen :width 20 :height 3))
         (was (a-screen :width 20 :height 3))
         (host (a-host screen)))
    (say pane (csi "1;31m") "red" (csi "0m") " plain "
         (csi "38;5;208m") "amber" (csi "0m")
         (esc "[2;1H") (csi "4:3;58;5;9m") "curly" (csi "0m")
         (esc "[3;1H") (csi "7m") "inverse")
    (mux:screen-blit screen pane)
    (say host (sent screen (mux:screen-diff was screen)))
    (is (null (difference screen host)) "~A" (difference screen host))))

(test a-screen-changed-and-encoded-again-is-still-the-same-screen
  (let* ((w 40) (h 8)
         (pane (a-term :width w :height h))
         (screen (a-screen :width w :height h))
         (was (a-screen :width w :height h))
         (host (a-host screen))
         (state (sb-ext:seed-random-state 20260917))
         (wrong nil))
    (dotimes (round 200)
      (say pane (format nil "~C[~D;~DH" #\Escape
                        (1+ (random h state)) (1+ (random w state))))
      (case (random 6 state)
        (0 (say pane (csi "~Dm" (+ 30 (random 8 state)))))
        (1 (say pane (csi "0m")))
        (2 (say pane (csi "1;4m")))
        (3 (say pane (csi "K")))
        (4 (say pane (csi "38;2;~D;~D;~Dm" (random 256 state)
                          (random 256 state) (random 256 state))))
        (t nil))
      (say pane (make-string (1+ (random 12 state))
                             :initial-element (code-char (+ 33 (random 80 state)))))
      (mux:screen-blit screen pane)
      (say host (sent screen (mux:screen-diff was screen)))
      (let ((bad (difference screen host)))
        (when (and bad (not wrong))
          (setf wrong (format nil "round ~D: ~A" round bad)))))
    (is (null wrong) "~A" wrong)))

(test a-run-of-blanks-to-the-end-of-a-row-is-an-erase-not-spaces
  (let* ((pane (a-term :width 40 :height 1))
         (screen (a-screen :width 40 :height 1))
         (was (a-screen :width 40 :height 1))
         (host (a-host screen)))
    (say pane (csi "?7l") (make-string 40 :initial-element #\x))
    (mux:screen-blit screen pane)
    (mux:screen-diff was screen)
    (say pane (esc "[1;1H") (csi "2K") "yo")
    (mux:screen-blit screen pane)
    (let ((bytes (sent screen (mux:screen-diff was screen))))
      (is (search (format nil "~C[K" #\Escape) bytes)
          "the tail was written out as spaces: ~S" bytes)
      (is (< (length bytes) 40))
      (say host bytes))
    (is (null (difference screen host)) "~A" (difference screen host))))

(test a-wide-character-does-not-move-the-columns-after-it
  (let* ((pane (a-term :width 12 :height 1))
         (screen (a-screen :width 12 :height 1))
         (was (a-screen :width 12 :height 1))
         (host (a-host screen)))
    (say pane (format nil "a~C~Cb" (code-char #x6F22) (code-char #x5B57)))
    (mux:screen-blit screen pane)
    (say host (sent screen (mux:screen-diff was screen)))
    (is (null (difference screen host)) "~A" (difference screen host))))

(test nothing-is-written-past-the-last-column-of-the-last-row
  (let* ((pane (a-term :width 8 :height 2))
         (screen (a-screen :width 8 :height 2))
         (was (a-screen :width 8 :height 2))
         (host (a-host screen)))
    (say pane "top" (esc "[2;1H") "abcdefgh")
    (mux:screen-blit screen pane)
    (say host (sent screen (mux:screen-diff was screen)))
    (is (equal "top" (row host 0)) "the host scrolled")
    (is (null (difference screen host)) "~A" (difference screen host))))

(test the-cursor-ends-where-the-screen-says-it-is
  (let* ((screen (a-screen :width 20 :height 4))
         (was (a-screen :width 20 :height 4))
         (host (a-host screen)))
    (setf (mux:screen-cursor-x screen) 7
          (mux:screen-cursor-y screen) 2
          (mux:screen-cursor-visible screen) nil)
    (put screen 0 0 #\x)
    (say host (sent screen (mux:screen-diff was screen)))
    (is (equal '(7 2) (cursor host)))
    (is (null (vt:term-cursor-visible host)))))

(test a-terminal-that-cannot-do-true-colour-is-given-the-nearest-index
  (let* ((pane (a-term :width 8 :height 1))
         (screen (a-screen :width 8 :height 1))
         (was (a-screen :width 8 :height 1))
         (host (a-host screen)))
    (say pane (csi "38;2;255;255;255m") "x")
    (mux:screen-blit screen pane)
    (say host (sent screen (mux:screen-diff was screen) :takes '(:blink)))
    (is (eql 231 (vt:face-fg (face-at host 0 0))))))

(test what-a-terminal-takes-is-read-off-its-name
  (is (member :rgb (mux:takes-of "xterm-256color" "truecolor")))
  (is (not (member :rgb (mux:takes-of "xterm-256color" nil))))
  (is (member :underline-style (mux:takes-of "xterm-kitty" nil)))
  (is (not (member :underline-color (mux:takes-of "xterm-256color" nil)))))
