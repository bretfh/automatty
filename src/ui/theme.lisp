(in-package #:atty/ui)

(defvar *in-force* nil)
(defvar *themes* (make-hash-table :test 'eq))
(defparameter +theme+ :ef-dream)

(defclass face ()
  ((fg        :initarg :fg        :accessor fg        :initform nil)
   (bg        :initarg :bg        :accessor bg        :initform nil)
   (bold      :initarg :bold      :accessor bold      :initform nil)
   (italic    :initarg :italic    :accessor italic    :initform nil)
   (underline :initarg :underline :accessor underline :initform nil)
   (crossed   :initarg :crossed   :accessor crossed   :initform nil)))

(defstruct (theme (:constructor %theme (palette metrics faces)) (:copier nil))
  palette metrics faces)

(defvar *active* +theme+)
(defvar *worn* nil)

(declaim (ftype function forget-rules))

(defun %as-keyword (name)
  (etypecase name
    (keyword name)
    (symbol (intern (symbol-name name) :keyword))
    (string (intern (string-upcase name) :keyword))))

(defun themes ()
  (sort (loop :for key :being :the :hash-keys :of *themes* :collect key)
        #'string< :key #'symbol-name))

(defun themed (name)
  (or (gethash (%as-keyword name) *themes*)
      (error "no theme called ~s" name)))

(defun active () *active*)

(defun (setf active) (name)
  (setf *active* (%as-keyword name))
  (forget-faces)
  (forget-rules)
  *active*)

(defun %role (plist name)
  (loop :for (k v) :on plist :by #'cddr
        :when (string= k name) :return (values v t)))

(defgeneric hex (color palette)
  (:method ((color null) palette) (declare (ignore palette)) nil)
  (:method ((color string) palette) (declare (ignore palette)) color)
  (:method ((color symbol) palette)
    (or (%role palette color)
        (error "color ~s is not in the palette" color))))

(defun define-theme (name palette-plist metrics-plist specs)
  (let* ((palette (loop :for (role h) :on palette-plist :by #'cddr
                        :append (list (%as-keyword role) h)))
         (metrics (loop :for (key v) :on metrics-plist :by #'cddr
                        :append (list (%as-keyword key) v)))
         (faces (loop :for (fname . spec) :in specs
                      :append (destructuring-bind (&key fg bg bold italic underline
                                                        crossed)
                                  spec
                                (list fname (list :fg (hex fg palette) :bg (hex bg palette)
                                                  :bold bold :italic italic
                                                  :underline underline
                                                  :crossed crossed))))))
    (setf (gethash (%as-keyword name) *themes*) (%theme palette metrics faces))
    (%as-keyword name)))

(defun %as-face (m)
  (when (and (consp m) (keywordp (first m)))
    (make-instance 'face :fg (getf m :fg) :bg (getf m :bg)
                         :bold (getf m :bold) :italic (getf m :italic)
                         :underline (getf m :underline)
                         :crossed (getf m :crossed))))

(defun %dropped (plist key)
  (loop :for (k v) :on plist :by #'cddr
        :unless (eq k key) :append (list k v)))

(defun set-face (name spec)
  (setf *worn* (append (list (%as-keyword name) spec)
                       (%dropped *worn* (%as-keyword name))))
  (forget-faces)
  name)

(defun %made ()
  (let ((out (make-hash-table :test 'eq))
        (worn *worn*))
    (loop :for (key plist) :on (theme-faces (themed (active))) :by #'cddr
          :do (setf (gethash key out) (%as-face (or (getf worn key) plist))))
    (loop :for (key plist) :on worn :by #'cddr
          :do (setf (gethash key out) (%as-face plist)))
    out))

(defvar *faces* nil
  "What the theme and whatever is worn over it work out to, kept rather than
worked out again. SET-FACE and (SETF ACTIVE) are the two things that change it,
and each forgets this.")

(defun forget-faces () (setf *faces* nil))

(defun faces-in-force ()
  (or *in-force* *faces* (setf *faces* (%made))))

(defmacro with-faces (&body body)
  `(let ((*in-force* (faces-in-force))) ,@body))

(defun in-force (name)
  (gethash name (faces-in-force)))

(defun attrs (f)
  (if f
      (logior (if (bold f) 1 0) (if (italic f) 2 0)
              (if (underline f) 4 0) (if (crossed f) 8 0))
      0))

(defun unhex (h)
  (when (and (stringp h) (>= (length h) 7) (char= (char h 0) #\#))
    (list (parse-integer h :start 1 :end 3 :radix 16)
          (parse-integer h :start 3 :end 5 :radix 16)
          (parse-integer h :start 5 :end 7 :radix 16))))

(defgeneric ink (it)
  (:method ((it null)) (values 255 255 255 -1 -1 -1 0))
  (:method ((it symbol)) (ink (in-force it)))
  (:method ((it cons))
    (destructuring-bind (fg bg &optional (attr 0)) it
      (let ((f (or (unhex fg) (list 255 255 255)))
            (b (or (unhex bg) (list -1 -1 -1))))
        (values (first f) (second f) (third f)
                (first b) (second b) (third b) attr))))
  (:method ((it face))
    (let ((f (or (unhex (fg it)) (list 255 255 255)))
          (b (or (unhex (bg it)) (list -1 -1 -1))))
      (values (first f) (second f) (third f)
              (first b) (second b) (third b) (attrs it)))))

(defun color (role)
  (or (%role (theme-palette (themed (active))) role)
      (error "the active theme has no color ~s" role)))

(defun metric (key &optional default)
  (multiple-value-bind (v found) (%role (theme-metrics (themed (active))) key)
    (if found v default)))

(define-theme
  :ef-dream
  '(bg        "#232025"   bg-dim    "#322f34"   bg-alt "#3b393e"
    bg-active "#5b595e"   fg        "#efd5c5"   fg-dim "#8f8886"
    fg-alt    "#b0a0cf"   border    "#635850"   accent "#675072"
    accent-fg "#fedeff"   red       "#ff6f6f"   green  "#51b04f"
    yellow    "#c0b24f"   blue      "#57b0ff"   magenta "#ffaacf"
    cyan      "#6fb3c0"
    red-faint     "#f3a0a0" blue-faint    "#a0a0cf" yellow-faint  "#caa89f"
    yellow-cooler "#deb07a" magenta-faint "#e3b0c0" blue-warmer   "#80aadf"
    cyan-warmer   "#8fcfd0" cyan-faint    "#99bfcf" green-faint   "#a9c99f"
    cyan-cooler   "#65c5a8" red-cooler    "#e47980" magenta-cooler "#d0b0ff"
    green-cooler  "#3fc489"
    region "#544a50" shadow "#0a0a10")
  '(radius 8 border 2 opacity 0.4 font "Maple Mono NF" font-px 15)
  '((:default   :fg fg)
    (:window    :bg bg)
    (:accent    :fg blue)
    (:hover     :fg accent-fg :bg bg-active)
    (:error     :fg red)
    (:warning   :fg yellow)
    (:done      :fg fg-dim :crossed t)
    (:border-active   :fg red)
    (:border-inactive :fg blue)
    (:state-working :fg blue)
    (:state-blocked :fg yellow)
    (:state-idle    :fg green)
    (:state-unknown :fg border)
    (:chip-blocked  :fg bg :bg yellow :bold t)
    (:chip-focus    :bg bg-active)
    (:number-working :fg bg :bg blue :bold t)
    (:number-blocked :fg bg :bg yellow :bold t)
    (:number-idle    :fg bg :bg green :bold t)
    (:number-unknown :fg bg :bg border :bold t)
    (:key           :fg fg :bg bg-active)
    (:key-number    :fg yellow :bg bg-active :bold t)
    (:quiet         :fg fg-dim)
    (:driven        :fg magenta)
    (:brand      :fg bg :bg red :bold t)
    (:ws-active  :fg accent-fg :bg accent :bold t)
    (:ring-cpu   :fg red)
    (:ring-ram   :fg blue)
    (:ring-disk  :fg green)
    (:ring-temp  :fg yellow)
    (:ring-track :fg bg-active)))
