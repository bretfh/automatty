;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-ext:add-package-local-nickname '#:objc '#:atty/objc '#:atty))

(defstruct (gui (:constructor %make-gui))
  (wire nil)
  (socket nil)
  (screen (tty:make-screen))
  (rows 24 :type fixnum)
  (cols 80 :type fixnum)
  (window nil)
  (view nil)
  (fd-ref nil)
  (fonts (make-array 4 :initial-element nil))
  (cell-w 8d0 :type double-float)
  (cell-h 16d0 :type double-float)
  (width 0d0 :type double-float)
  (height 0d0 :type double-float)
  (colors (make-hash-table :test 'equal))
  (attrs (make-hash-table :test 'equal))
  (greeted nil)
  (bracketed nil)
  (pressed nil)
  (dragged-to nil)
  (scrolled 0d0 :type double-float)
  (font-size 13d0 :type double-float)
  (running t)
  (exit-reason nil))

(defvar *gui* nil)

(defun gui-stop (gui why)
  (unless (gui-exit-reason gui)
    (setf (gui-exit-reason gui) why))
  (when (gui-running gui)
    (setf (gui-running gui) nil)
    (let ((app (objc:id (objc:class-named "NSApplication") "sharedApplication")))
      (objc:send :void app "stop:" :id (objc:none))
      (objc:send :void app "postEvent:atStart:"
                 :id (objc:id (objc:class-named "NSEvent")
                              "otherEventWithType:location:modifierFlags:timestamp:windowNumber:context:subtype:data1:data2:"
                              :ulong 15 :double 0d0 :double 0d0 :ulong 0 :double 0d0
                              :long 0 :id (objc:none) :short 0 :long 0 :long 0)
                 :bool 1))))

(defun gui-send (gui form)
  (wire-send (gui-wire gui) form)
  (gui-flush gui))

(defun gui-type (gui text)
  (gui-send gui (list :keys (sb-ext:octets-to-string
                             (sb-ext:string-to-octets text :external-format :utf-8)
                             :external-format :latin-1))))

(defun gui-flush (gui)
  (let ((wire (gui-wire gui)))
    (when (wire-open wire)
      (wire-flush wire)
      (when (gui-fd-ref gui)
        (sb-alien:alien-funcall (objc:cfun "CFFileDescriptorEnableCallBacks" :void :id :ulong)
                                (gui-fd-ref gui)
                                (if (plusp (wire-pending wire)) 3 1))))))

(defun hex-rgb (hex)
  (or (atty/ui:unhex hex) (list 255 0 255)))

(defun palette-rgb (n)
  (cond ((< n 16) (hex-rgb (nth n +gui-palette+)))
        ((< n 232) (let ((n (- n 16)))
                     (mapcar (lambda (c) (if (zerop c) 0 (+ 55 (* 40 c))))
                             (list (floor n 36) (mod (floor n 6) 6) (mod n 6)))))
        (t (let ((v (+ 8 (* 10 (- n 232))))) (list v v v)))))

(defun color-rgb (color default)
  (cond ((null color) (hex-rgb (atty/ui:color default)))
        ((integerp color) (palette-rgb (min 255 (max 0 color))))
        ((and (listp color) (= 3 (length color))) color)
        (t (hex-rgb (atty/ui:color default)))))

(defun nscolor (gui rgb &optional (alpha 1d0))
  (let ((key (cons alpha rgb)))
    (or (gethash key (gui-colors gui))
        (setf (gethash key (gui-colors gui))
              (destructuring-bind (r g b) rgb
                (objc:retain
                 (objc:id (objc:class-named "NSColor") "colorWithSRGBRed:green:blue:alpha:"
                          :double (/ r 255d0) :double (/ g 255d0) :double (/ b 255d0)
                          :double alpha)))))))

(defun face-rgbs (face)
  (let ((fg (color-rgb (and face (term:face-fg face)) 'atty/ui:fg))
        (bg (color-rgb (and face (term:face-bg face)) 'atty/ui:bg)))
    (when (and face (term:face-inverse face))
      (rotatef fg bg))
    (when (and face (term:face-conceal face))
      (setf fg bg))
    (values fg bg)))

(defun make-fonts (gui)
  (let* ((size (gui-font-size gui))
         (named (and (plusp (length +gui-font+))
                     (objc:id (objc:class-named "NSFont") "fontWithName:size:"
                              :id (objc:ns +gui-font+) :double size)))
         (base (if (and named (not (objc:none-p named)))
                   named
                   (objc:id (objc:class-named "NSFont") "monospacedSystemFontOfSize:weight:"
                            :double size :double 0d0)))
         (manager (objc:id (objc:class-named "NSFontManager") "sharedFontManager")))
    (loop :for old :across (gui-fonts gui) :when old :do (objc:release old))
    (dotimes (i 4)
      (setf (aref (gui-fonts gui) i)
            (objc:retain
             (let ((font base))
               (when (logbitp 0 i)
                 (setf font (objc:id manager "convertFont:toHaveTrait:" :id font :ulong 2)))
               (when (logbitp 1 i)
                 (setf font (objc:id manager "convertFont:toHaveTrait:" :id font :ulong 1)))
               font))))
    (maphash (lambda (k v) (declare (ignore k)) (objc:release v)) (gui-attrs gui))
    (clrhash (gui-attrs gui))
    (let ((attrs (objc:id (objc:class-named "NSDictionary") "dictionaryWithObject:forKey:"
                          :id base :id (objc:extern-id "NSFontAttributeName"))))
      (setf (gui-cell-w gui) (objc:send :double (objc:ns "M") "sizeWithAttributes:" :id attrs)
            (gui-cell-h gui) (float (ceiling (+ (objc:send :double base "ascender")
                                                (- (objc:send :double base "descender"))
                                                (objc:send :double base "leading")))
                                    1d0)))))

(defun text-attrs (gui face fg alpha)
  (let* ((index (+ (if (and face (term:face-bold face)) 1 0)
                   (if (and face (term:face-italic face)) 2 0)))
         (underline (and face (term:face-underline face)))
         (crossed (and face (term:face-crossed face)))
         (key (list index fg alpha underline crossed)))
    (or (gethash key (gui-attrs gui))
        (setf (gethash key (gui-attrs gui))
              (let ((d (objc:id (objc:class-named "NSMutableDictionary") "dictionary"))
                    (number (objc:class-named "NSNumber")))
                (flet ((put (value key)
                         (objc:send :void d "setObject:forKey:" :id value :id (objc:extern-id key))))
                  (put (aref (gui-fonts gui) index) "NSFontAttributeName")
                  (put (nscolor gui fg alpha) "NSForegroundColorAttributeName")
                  (when underline
                    (put (objc:id number "numberWithLong:" :long (if (eq underline :double) 9 1))
                         "NSUnderlineStyleAttributeName"))
                  (when crossed
                    (put (objc:id number "numberWithLong:" :long 1) "NSStrikethroughStyleAttributeName")))
                (objc:retain d))))))

(defun fill-cells (x y w h)
  (sb-alien:with-alien ((rect (array double-float 4)))
    (setf (sb-alien:deref rect 0) x (sb-alien:deref rect 1) y
          (sb-alien:deref rect 2) w (sb-alien:deref rect 3) h)
    (sb-alien:alien-funcall (objc:cfun "NSRectFillList" :void :pointer :long)
                            (sb-alien:alien-sap rect) 1)))

(defun cell-width (ch)
  (if (= 2 (term:char-display-width ch)) 2 1))

(defun draw-row (gui y)
  (let* ((screen (gui-screen gui))
         (row (tty:screen-row screen y))
         (cols (tty:screen-width screen))
         (cw (gui-cell-w gui))
         (ch (gui-cell-h gui))
         (py (* y ch))
         (x 0))
    (loop :while (< x cols)
          :do (let* ((face (term:row-face row x))
                     (first-char (term:row-char row x))
                     (end (if (< (char-code first-char) 128)
                              (loop :for e :from (1+ x) :below cols
                                    :while (and (< (char-code (term:row-char row e)) 128)
                                                (let ((f (term:row-face row e)))
                                                  (or (eq f face) (and f face (term:face-equal f face)))))
                                    :finally (return e))
                              (min cols (+ x (cell-width first-char)))))
                     (text (subseq (term:row-chars row) x end)))
                (multiple-value-bind (fg bg) (face-rgbs face)
                  (unless (equal bg (hex-rgb (atty/ui:color 'atty/ui:bg)))
                    (objc:send :void (nscolor gui bg) "setFill")
                    (fill-cells (* x cw) py (* (- end x) cw) ch))
                  (unless (and (every (lambda (c) (char= c #\Space)) text)
                               (not (and face (or (term:face-underline face) (term:face-crossed face)))))
                    (objc:send :void (objc:ns (if (< (char-code first-char) 128) text (string first-char)))
                               "drawAtPoint:withAttributes:"
                               :double (* x cw) :double py
                               :id (text-attrs gui face fg (if (and face (term:face-faint face)) 0.6d0 1d0)))))
                (setf x end)))))

(defun draw-cursor (gui)
  (let* ((screen (gui-screen gui))
         (x (tty:screen-cursor-x screen))
         (y (tty:screen-cursor-y screen))
         (cw (gui-cell-w gui))
         (ch (gui-cell-h gui))
         (style (string (tty:screen-cursor-style screen))))
    (when (and (tty:screen-cursor-visible screen)
               (< -1 y (tty:screen-height screen)) (< -1 x (tty:screen-width screen)))
      (let* ((row (tty:screen-row screen y))
             (face (term:row-face row x))
             (c (term:row-char row x)))
        (multiple-value-bind (fg bg) (face-rgbs face)
          (objc:send :void (nscolor gui fg) "setFill")
          (cond ((search "BAR" style) (fill-cells (* x cw) (* y ch) 2d0 ch))
                ((search "UNDERLINE" style) (fill-cells (* x cw) (- (* (1+ y) ch) 2d0) cw 2d0))
                (t (fill-cells (* x cw) (* y ch) (* (cell-width c) cw) ch)
                   (unless (char= c #\Space)
                     (objc:send :void (objc:ns (string c)) "drawAtPoint:withAttributes:"
                                :double (* x cw) :double (* y ch)
                                :id (text-attrs gui face bg 1d0))))))))))

(defun gui-draw (gui)
  (let ((ch (gui-cell-h gui))
        (rows (tty:screen-height (gui-screen gui)))
        (top most-positive-fixnum)
        (bottom 0))
    (sb-alien:with-alien ((rects sb-alien:system-area-pointer)
                          (count sb-alien:long))
      (objc:send :void (gui-view gui) "getRectsBeingDrawn:count:"
                 :pointer (sb-alien:alien-sap (sb-alien:addr rects))
                 :pointer (sb-alien:alien-sap (sb-alien:addr count)))
      (objc:send :void (nscolor gui (hex-rgb (atty/ui:color 'atty/ui:bg))) "setFill")
      (sb-alien:alien-funcall (objc:cfun "NSRectFillList" :void :pointer :long) rects count)
      (dotimes (i count)
        (let ((y (sb-sys:sap-ref-double rects (+ 8 (* 32 i))))
              (h (sb-sys:sap-ref-double rects (+ 24 (* 32 i)))))
          (setf top (min top (floor y ch))
                bottom (max bottom (ceiling (+ y h) ch))))))
    (loop :for y :from (max 0 top) :below (min rows bottom)
          :do (draw-row gui y))
    (draw-cursor gui)))

(defun gui-redraw-rows (gui from to)
  (let ((view (gui-view gui)))
    (when view
      (objc:send :void view "setNeedsDisplayInRect:"
                 :double 0d0 :double (* from (gui-cell-h gui))
                 :double (gui-width gui) :double (* (- to from) (gui-cell-h gui))))))

(defun gui-redraw (gui)
  (gui-redraw-rows gui 0 (tty:screen-height (gui-screen gui))))

(defun gui-message (gui form)
  (case (first form)
    (:hello
     (destructuring-bind (name rows cols &rest more) (rest form)
       (declare (ignore name more))
       (setf (gui-greeted gui) t)
       (setf (gui-screen gui) (tty:make-screen :width cols :height rows))
       (gui-redraw gui)))
    (:frame
     (destructuring-bind (said faces) (rest form)
       (dolist (run (decode-runs-into-screen (gui-screen gui) said faces))
         (gui-redraw-rows gui (tty:run-row run) (1+ (tty:run-row run))))))
    (:cursor
     (destructuring-bind (y x visible style) (rest form)
       (let ((screen (gui-screen gui)))
         (gui-redraw-rows gui (tty:screen-cursor-y screen) (1+ (tty:screen-cursor-y screen)))
         (setf (tty:screen-cursor-y screen) y
               (tty:screen-cursor-x screen) x
               (tty:screen-cursor-visible screen) (and visible t)
               (tty:screen-cursor-style screen) style)
         (gui-redraw-rows gui y (1+ y)))))
    (:bell (sb-alien:alien-funcall (objc:cfun "NSBeep" :void)))
    (:bracketed-paste (setf (gui-bracketed gui) (and (second form) t)))
    (:copied
     (let ((board (objc:id (objc:class-named "NSPasteboard") "generalPasteboard")))
       (objc:send :long board "clearContents")
       (objc:send :bool board "setString:forType:"
                  :id (objc:ns (second form)) :id (objc:extern-id "NSPasteboardTypeString"))))
    (:bye
     (gui-stop gui (second form))
     (when (eq (second form) :restarting)
       (setf *successor* (third form))))
    (t nil)))

(defun gui-read (gui)
  (let ((wire (gui-wire gui)))
    (if (null (wire-receive wire))
        (gui-stop gui :server-gone)
        (loop :for form := (wire-read-message wire)
              :while form
              :do (gui-message gui form)))
    (gui-flush gui)))

(objc:defimp gui-readable :void ((ref :pointer) (flags :ulong) (info :pointer))
  (declare (ignore ref info))
  (when *gui*
    (objc:with-pool
      (if (logtest flags 1)
          (gui-read *gui*)
          (gui-flush *gui*)))))

(defun modifier-param (flags)
  (+ 1
     (if (logbitp 17 flags) 1 0)
     (if (logbitp 19 flags) 2 0)
     (if (logbitp 18 flags) 4 0)))

(defparameter +gui-keys+
  '((126 :csi #\A) (125 :csi #\B) (124 :csi #\C) (123 :csi #\D)
    (115 :csi #\H) (119 :csi #\F)
    (122 :ss3 #\P) (120 :ss3 #\Q) (99 :ss3 #\R) (118 :ss3 #\S)
    (114 :tilde 2) (117 :tilde 3) (116 :tilde 5) (121 :tilde 6)
    (96 :tilde 15) (97 :tilde 17) (98 :tilde 18) (100 :tilde 19)
    (101 :tilde 20) (109 :tilde 21) (103 :tilde 23) (111 :tilde 24)))

(defun special-key (code flags)
  (let ((esc (string #\Escape))
        (m (modifier-param flags))
        (meta (and (logbitp 19 flags) +gui-option-is-meta+)))
    (destructuring-bind (&optional kind what) (rest (assoc code +gui-keys+))
      (case kind
        (:csi (if (= m 1) (format nil "~A[~C" esc what) (format nil "~A[1;~D~C" esc m what)))
        (:ss3 (if (= m 1) (format nil "~AO~C" esc what) (format nil "~A[1;~D~C" esc m what)))
        (:tilde (if (= m 1) (format nil "~A[~D~~" esc what) (format nil "~A[~D;~D~~" esc what m)))
        (t (case code
             ((36 76) (if meta (format nil "~A~C" esc #\Return) (string #\Return)))
             (48 (if (logbitp 17 flags) (format nil "~A[Z" esc) (string #\Tab)))
             (51 (cond ((logbitp 18 flags) (string #\Backspace))
                       (meta (format nil "~A~C" esc #\Rubout))
                       (t (string #\Rubout))))
             (53 esc)))))))

(defun control-of (text)
  (if (= 1 (length text))
      (let ((c (char-upcase (char text 0))))
        (cond ((char= c #\Space) (string (code-char 0)))
              ((char= c #\/) (string (code-char 31)))
              ((char= c #\?) (string #\Rubout))
              ((char<= #\@ c #\_) (string (code-char (logand (char-code c) 31))))
              (t text)))
      text))

(defun gui-paste (gui)
  (let* ((board (objc:id (objc:class-named "NSPasteboard") "generalPasteboard"))
         (got (objc:id board "stringForType:" :id (objc:extern-id "NSPasteboardTypeString"))))
    (unless (objc:none-p got)
      (let ((text (substitute #\Return #\Newline (remove #\Return (objc:lisp-string got)))))
        (gui-type gui (if (gui-bracketed gui)
                          (format nil "~C[200~~~A~C[201~~" #\Escape text #\Escape)
                          text))))))

(defun gui-refont (gui size)
  (setf (gui-font-size gui) (max 6d0 (min 72d0 size)))
  (make-fonts gui)
  (gui-resized gui (gui-width gui) (gui-height gui))
  (gui-redraw gui))

(defun gui-command-key (gui chars)
  (cond ((member chars '("q" "w") :test #'string=) (gui-stop gui :detached))
        ((string= chars "v") (gui-paste gui))
        ((member chars '("=" "+") :test #'string=) (gui-refont gui (+ (gui-font-size gui) 1d0)))
        ((string= chars "-") (gui-refont gui (- (gui-font-size gui) 1d0)))
        ((string= chars "0") (gui-refont gui (float +gui-font-size+ 1d0)))))

(objc:defimp gui-key-down :void ((self :id) (cmd :pointer) (event :id))
  (declare (ignore self cmd))
  (let* ((gui *gui*)
         (flags (objc:send :ulong event "modifierFlags"))
         (code (objc:send :ushort event "keyCode"))
         (plain (objc:lisp-string (objc:id event "charactersIgnoringModifiers"))))
    (cond
      ((logbitp 20 flags) (gui-command-key gui plain))
      (t
       (let ((said (or (special-key code flags)
                       (let ((text (cond ((logbitp 18 flags) (control-of plain))
                                         ((and (logbitp 19 flags) +gui-option-is-meta+) plain)
                                         (t (objc:lisp-string (objc:id event "characters"))))))
                         (if (and (logbitp 19 flags) +gui-option-is-meta+ (plusp (length text)))
                             (format nil "~C~A" #\Escape text)
                             text)))))
         (when (plusp (length said))
           (gui-type gui said)))))))

(defun event-cell (gui event)
  (destructuring-bind (x y) (objc:boxed event "locationInWindow" 2)
    (values (max 0 (min (1- (gui-cols gui)) (floor x (gui-cell-w gui))))
            (max 0 (min (1- (gui-rows gui)) (floor (- (gui-height gui) y) (gui-cell-h gui)))))))

(defun mouse-mods (event)
  (let ((flags (objc:send :ulong event "modifierFlags")))
    (+ (if (logbitp 17 flags) 4 0) (if (logbitp 19 flags) 8 0) (if (logbitp 18 flags) 16 0))))

(defun gui-mouse (gui event button how)
  (multiple-value-bind (x y) (event-cell gui event)
    (let ((cell (cons x y)))
      (unless (and (eq how :drag) (equal cell (gui-dragged-to gui)))
        (setf (gui-dragged-to gui) cell)
        (gui-type gui (format nil "~C[<~D;~D;~D~C" #\Escape
                              (+ button (mouse-mods event) (if (eq how :drag) 32 0))
                              (1+ x) (1+ y) (if (eq how :up) #\m #\M)))))))

(defmacro define-mouse-imp (name button how)
  `(objc:defimp ,name :void ((self :id) (cmd :pointer) (event :id))
     (declare (ignore self cmd))
     (gui-mouse *gui* event ,button ,how)))

(define-mouse-imp gui-left-down 0 :down)
(define-mouse-imp gui-left-up 0 :up)
(define-mouse-imp gui-left-dragged 0 :drag)
(define-mouse-imp gui-right-down 2 :down)
(define-mouse-imp gui-right-up 2 :up)
(define-mouse-imp gui-right-dragged 2 :drag)
(define-mouse-imp gui-other-down 1 :down)
(define-mouse-imp gui-other-up 1 :up)
(define-mouse-imp gui-other-dragged 1 :drag)

(objc:defimp gui-scroll :void ((self :id) (cmd :pointer) (event :id))
  (declare (ignore self cmd))
  (let* ((gui *gui*)
         (dy (objc:send :double event "scrollingDeltaY"))
         (step (if (plusp (objc:send :bool event "hasPreciseScrollingDeltas"))
                   (* (gui-cell-h gui) +wheel-rows+)
                   1d0)))
    (incf (gui-scrolled gui) dy)
    (multiple-value-bind (x y) (event-cell gui event)
      (loop :while (>= (abs (gui-scrolled gui)) step)
            :do (let ((up (plusp (gui-scrolled gui))))
                  (decf (gui-scrolled gui) (if up step (- step)))
                  (gui-type gui (format nil "~C[<~D;~D;~DM" #\Escape
                                        (+ (if up 64 65) (mouse-mods event))
                                        (1+ x) (1+ y))))))))

(objc:defimp gui-draw-rect :void ((self :id) (cmd :pointer))
  (declare (ignore self cmd))
  (gui-draw *gui*))

(objc:defimp gui-yes :bool ((self :id) (cmd :pointer))
  (declare (ignore self cmd))
  1)

(defun gui-resized (gui width height)
  (setf (gui-width gui) width
        (gui-height gui) height)
  (let ((rows (max 1 (floor height (gui-cell-h gui))))
        (cols (max 1 (floor width (gui-cell-w gui)))))
    (unless (and (= rows (gui-rows gui)) (= cols (gui-cols gui)))
      (setf (gui-rows gui) rows
            (gui-cols gui) cols)
      (when (gui-greeted gui)
        (gui-send gui (list :resize rows cols))))))

(objc:defimp gui-set-frame-size :void ((self :id) (cmd :pointer) (width :double) (height :double))
  (declare (ignore cmd))
  (objc:send-super :void self (objc:class-named "NSView") "setFrameSize:"
                   :double width :double height)
  (when *gui*
    (gui-resized *gui* width height)))

(objc:defimp gui-window-closing :void ((self :id) (cmd :pointer) (note :id))
  (declare (ignore self cmd note))
  (when *gui* (gui-stop *gui* :detached)))

(defun gui-classes ()
  (values
   (objc:make-class "AttyView" "NSView"
                    '(("drawRect:" gui-draw-rect "v@:{CGRect={CGPoint=dd}{CGSize=dd}}")
                      ("isFlipped" gui-yes "c@:")
                      ("isOpaque" gui-yes "c@:")
                      ("acceptsFirstResponder" gui-yes "c@:")
                      ("acceptsFirstMouse:" gui-yes "c@:@")
                      ("keyDown:" gui-key-down "v@:@")
                      ("mouseDown:" gui-left-down "v@:@")
                      ("mouseUp:" gui-left-up "v@:@")
                      ("mouseDragged:" gui-left-dragged "v@:@")
                      ("rightMouseDown:" gui-right-down "v@:@")
                      ("rightMouseUp:" gui-right-up "v@:@")
                      ("rightMouseDragged:" gui-right-dragged "v@:@")
                      ("otherMouseDown:" gui-other-down "v@:@")
                      ("otherMouseUp:" gui-other-up "v@:@")
                      ("otherMouseDragged:" gui-other-dragged "v@:@")
                      ("scrollWheel:" gui-scroll "v@:@")
                      ("setFrameSize:" gui-set-frame-size "v@:{CGSize=dd}")))
   (objc:make-class "AttyWindowWatcher" "NSObject"
                    '(("windowWillClose:" gui-window-closing "v@:@")))))

(defun gui-listen (gui)
  (let* ((ref (sb-alien:alien-funcall (objc:cfun "CFFileDescriptorCreate" :id :pointer :int :bool :pointer :pointer)
                                      (objc:none) (wire-fd (gui-wire gui)) 0
                                      (objc:imp 'gui-readable) (objc:none)))
         (source (sb-alien:alien-funcall (objc:cfun "CFFileDescriptorCreateRunLoopSource" :id :pointer :id :long)
                                         (objc:none) ref 0)))
    (sb-alien:alien-funcall (objc:cfun "CFRunLoopAddSource" :void :id :id :id)
                            (sb-alien:alien-funcall (objc:cfun "CFRunLoopGetMain" :id))
                            source (objc:extern-id "kCFRunLoopCommonModes"))
    (setf (gui-fd-ref gui) ref)
    (gui-flush gui)))

(defun gui-unlisten (gui)
  (let ((ref (shiftf (gui-fd-ref gui) nil)))
    (when ref
      (sb-alien:alien-funcall (objc:cfun "CFFileDescriptorInvalidate" :void :id) ref)
      (sb-alien:alien-funcall (objc:cfun "CFRelease" :void :id) ref))))

(defun gui-open-window (gui title)
  (multiple-value-bind (view-class watcher-class) (gui-classes)
    (let* ((width (* (gui-cols gui) (gui-cell-w gui)))
           (height (* (gui-rows gui) (gui-cell-h gui)))
           (window (objc:id (objc:id (objc:class-named "NSWindow") "alloc") "init"))
           (view (objc:id (objc:id view-class "alloc") "init"))
           (watcher (objc:id (objc:id watcher-class "alloc") "init")))
      (objc:send :void window "setStyleMask:" :ulong 15)
      (objc:send :void window "setReleasedWhenClosed:" :bool 0)
      (objc:send :void window "setContentView:" :id view)
      (objc:send :void window "setContentSize:" :double width :double height)
      (objc:send :void window "setContentResizeIncrements:"
                 :double (gui-cell-w gui) :double (gui-cell-h gui))
      (objc:send :void window "setBackgroundColor:"
                 :id (nscolor gui (hex-rgb (atty/ui:color 'atty/ui:bg))))
      (objc:send :void window "setTitle:" :id (objc:ns title))
      (objc:send :void window "setDelegate:" :id watcher)
      (objc:send :void window "center")
      (objc:send :void window "makeFirstResponder:" :id view)
      (setf (gui-window gui) window
            (gui-view gui) view)
      (gui-resized gui width height)
      (objc:send :void window "makeKeyAndOrderFront:" :id (objc:none)))))

(defun gui (path &key name open (rows 30) (cols 100))
  (objc:start)
  (objc:with-cocoa
    (objc:with-pool
      (let* ((app (objc:id (objc:class-named "NSApplication") "sharedApplication"))
             (gui (setf *gui* (%make-gui :rows rows :cols cols :font-size (float +gui-font-size+ 1d0)))))
        (objc:send :bool app "setActivationPolicy:" :long 0)
        (make-fonts gui)
        (multiple-value-bind (wire socket) (connect-to path)
          (setf (gui-wire gui) wire
                (gui-socket gui) socket))
        (unwind-protect
             (progn
               (gui-open-window gui (format nil "atty ~A" (or name "")))
               (wire-send (gui-wire gui) (list :who nil))
               (if open
                   (destructuring-bind (command directory &optional label) open
                     (wire-send (gui-wire gui) (list :open name command directory
                                                     (gui-rows gui) (gui-cols gui) t label)))
                   (progn
                     (when name (wire-send (gui-wire gui) (list :want name)))
                     (wire-send (gui-wire gui) (list :attach (gui-rows gui) (gui-cols gui) t))))
               (gui-listen gui)
               (objc:send :void app "activateIgnoringOtherApps:" :bool 1)
               (objc:send :void app "run")
               (gui-exit-reason gui))
          (setf (gui-running gui) nil
                *gui* nil)
          (gui-unlisten gui)
          (ignore-errors (wire-send (gui-wire gui) '(:detach))
                         (wire-flush (gui-wire gui)))
          (wire-close (gui-wire gui))
          (when (gui-window gui)
            (objc:send :void (gui-window gui) "setDelegate:" :id (objc:none))
            (objc:send :void (gui-window gui) "close")))))))

(defun gui-through-restarts (path &rest args)
  (loop
    (let ((why (apply #'gui path args)))
      (unless (eq why :restarting) (return why))
      (unless (server-back-p path) (return :no-server-back))
      (exec-successor))))

(defun gui-or-create (&key (name "0") (command (default-shell)))
  (report-exit-reason name (gui-through-restarts (if *fresh-start* (start-fresh-server) (ensure-server))
                                                 :name name :open (list command (cwd) nil))))

(defun in-app-p ()
  (let ((me (executable-path)))
    (and me (search ".app/Contents/MacOS/" me) t)))

(defparameter +left-to-the-pane+ '("PWD" "OLDPWD" "SHLVL" "_" "TERM" "COLORTERM"))

(defun login-environment ()
  (let* ((process (sb-ext:run-program (default-shell)
                                      (list "-l" "-i" "-c" "printf '\\0atty\\0'; env -0")
                                      :input nil :output :stream :error nil :wait nil
                                      :external-format :utf-8))
         (out (with-output-to-string (s)
                (let ((buffer (make-string 4096)))
                  (loop :for n := (read-sequence buffer (sb-ext:process-output process))
                        :while (plusp n) :do (write-string buffer s :end n))))))
    (sb-ext:process-wait process)
    (sb-ext:process-close process)
    (let ((at (search (format nil "~Catty~C" #\Nul #\Nul) out)))
      (when at
        (loop :with start := (+ at 6)
              :for end := (position #\Nul out :start start)
              :for entry := (subseq out start (or end (length out)))
              :for eq := (position #\= entry)
              :when (and eq (plusp eq))
                :collect (cons (subseq entry 0 eq) (subseq entry (1+ eq)))
              :while end
              :do (setf start (1+ end)))))))

(defun take-login-environment ()
  (loop :for (name . value) :in (ignore-errors (login-environment))
        :unless (member name +left-to-the-pane+ :test #'string=)
          :do (sb-posix:setenv name value 1))
  (unless (sb-ext:posix-getenv "LANG")
    (objc:start)
    (let* ((id (objc:lisp-string (objc:id (objc:id (objc:class-named "NSLocale") "currentLocale")
                                          "localeIdentifier")))
           (base (subseq id 0 (position #\@ id))))
      (sb-posix:setenv "LANG" (format nil "~A.UTF-8" (if (plusp (length base)) base "en_US")) 1)))
  (let ((home (sb-ext:posix-getenv "HOME")))
    (when home
      (ignore-errors (sb-posix:chdir home))
      (setf *default-pathname-defaults* (pathname (format nil "~A/" (string-right-trim "/" home)))))))

(defun gui-from-app ()
  (take-login-environment)
  (gui-or-create))
