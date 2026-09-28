(defpackage #:atty/ui
  (:use #:cl)
  (:export

   #:widget #:parts #:label #:rule #:gap #:picture #:calendar #:slider #:ring
   #:column #:row #:stack #:box #:center #:centerbox #:scroll #:action #:choice
   #:framed #:clip #:line #:titles #:+frame-glyphs+
   #:icon #:button #:image #:rows #:glyph
   #:on-click #:on-change #:set-on-click #:set-on-change
   #:text #:value #:css-class-name #:hint #:hoveredp #:chosen #:expand
   #:background-color #:background-image #:border-radius #:font-size
   #:padding #:margin #:min-width #:min-height
   #:top #:left #:bottom #:right #:width #:height #:fraction
   #:rule-glyph #:upright #:spacing #:align #:offset #:fixed-width #:fixed-height
   #:before #:after #:mark-of #:low #:high #:track #:thickness #:diameter
   #:year #:month #:day #:path #:start #:middle #:end

   #:split-string

   #:medium #:text-size #:line-height #:measure #:lay #:paint
   #:boldp #:italicp #:underlinep #:family-of
   #:restyle #:style-of #:with-pass #:under #:clicked #:clicked-at #:value-at
   #:underline-of
   #:scrolling-under

   #:face #:fg #:bg #:bold #:italic #:underline #:crossed
   #:ink #:attrs #:in-force
   #:with-faces #:faces-in-force
   #:theme #:themes #:themed #:define-theme #:set-face
   #:active #:color #:metric #:unhex #:hex
   #:style #:styles #:put-rules #:selector #:rules #:resolve
   #:property #:properties #:classes #:glass #:mono #:radius #:hovered))
(in-package #:atty/ui)

(defun split-string (s &key separator)
  "S in pieces, cut wherever a character of SEPARATOR stands. The pieces between
two separators are kept even when empty, which is what the style reader counts
on to tell \".a.b\" from \"a.b\"."
  (let ((cuts (coerce (or separator '(#\Space)) 'list))
        (out nil)
        (from 0))
    (dotimes (i (length s))
      (when (member (char s i) cuts)
        (push (subseq s from i) out)
        (setf from (1+ i))))
    (push (subseq s from) out)
    (nreverse out)))

(defgeneric value (x)
  (:method (x) x)
  (:documentation "What a widget was given. Anything that is not a widget stands
for itself."))

(defclass widget ()
  ((parts   :initarg :parts   :accessor parts   :initform nil)
   (chosen  :initarg :chosen  :accessor chosen  :initform nil)
   (face    :initarg :face    :accessor face    :initform nil)
   (css-class-name :initarg :class :accessor css-class-name :initform nil)
   (hint    :initarg :hint    :accessor hint    :initform nil)
   (hovered :initform nil     :accessor hoveredp)
   (border-radius :initarg :border-radius :accessor border-radius :initform 0)
   (background-color :initarg :background-color :accessor background-color
                     :initform nil)
   (background-image :initarg :background-image :accessor background-image
                     :initform nil)
   (font-size :initarg :font-size :accessor font-size :initform nil)
   (padding :initarg :padding :accessor padding :initform nil)
   (margin  :initarg :margin  :accessor margin  :initform nil)
   (expand  :initarg :expand  :accessor expand  :initform 0)
   (min-width  :initarg :min-width  :accessor min-width  :initform 0)
   (min-height :initarg :min-height :accessor min-height :initform 0)
   (top     :initform 0 :accessor top)
   (left    :initform 0 :accessor left)
   (bottom  :initform 0 :accessor bottom)
   (right   :initform 0 :accessor right)))

(defclass label (widget)
  ((text :initarg :text :accessor text :initform "")))

(defclass rule (widget)
  ((glyph   :initarg :glyph   :accessor rule-glyph :initform (code-char #x2500))
   (upright :initarg :upright :accessor upright    :initform nil)))

(defclass gap (widget) ())

(defclass column (widget)
  ((spacing :initarg :spacing :accessor spacing :initform 0)
   (align   :initarg :align   :accessor align   :initform :start)))

(defclass row (widget)
  ((spacing :initarg :spacing :accessor spacing :initform 1)
   (align   :initarg :align   :accessor align   :initform :start)))

(defclass stack (widget) ())

(defclass box (widget)
  ((fixed-width :initarg :fixed-width :accessor fixed-width :initform 0)
   (align :initarg :align :accessor align :initform :left)))

(defclass center (widget) ())

(defclass framed (widget)
  ((line   :initarg :line   :accessor line   :initform :single)
   (titles :initarg :titles :accessor titles :initform nil)))

(defclass clip (widget) ()
  (:documentation "Its one part, cut off at its own right edge. What is laid in
less room than it wants overruns into whatever is beside it, which in a frame's
border is the corner."))

(defclass centerbox (widget)
  ((start   :initarg :start   :accessor start   :initform nil)
   (middle  :initarg :middle  :accessor middle  :initform nil)
   (end     :initarg :end     :accessor end     :initform nil)))

(defclass scroll (widget)
  ((offset :initarg :offset :accessor offset :initform 0)
   (fixed-height :initarg :fixed-height :accessor fixed-height :initform 10)))

(defclass action (widget)
  ((on-click :initarg :on-click :accessor on-click :initform nil)))

(defclass choice (widget)
  ((on-click :initarg :on-click :accessor on-click :initform nil)
   (before   :initarg :before   :accessor before   :initform "> ")
   (after    :initarg :after    :accessor after    :initform "  ")))

(defclass slider (widget)
  ((value   :initarg :value   :accessor value   :initform 0)
   (low     :initarg :low     :accessor low     :initform 0)
   (high    :initarg :high    :accessor high    :initform 100)
   (track   :initarg :track   :accessor track   :initform 16)
   (on-change :initarg :on-change :accessor on-change :initform nil)))

(defclass ring (widget)
  ((value     :initarg :value     :accessor value     :initform 0)
   (low       :initarg :low       :accessor low       :initform 0)
   (high      :initarg :high      :accessor high      :initform 100)
   (thickness :initarg :thickness :accessor thickness :initform 5)
   (diameter  :initarg :diameter  :accessor diameter  :initform 56)))

(defclass calendar (widget)
  ((year  :initarg :year  :accessor year  :initform 2000)
   (month :initarg :month :accessor month :initform 1)
   (day   :initarg :day   :accessor day   :initform 1)))

(defclass picture (widget)
  ((path :initarg :path :accessor path :initform "")))

(defmethod parts ((w centerbox))
  (remove nil (list (start w) (middle w) (end w))))

(defgeneric width (w)
  (:method ((w widget)) (- (right w) (left w))))

(defgeneric height (w)
  (:method ((w widget)) (- (bottom w) (top w))))

(defun fraction (w)
  (let* ((span (max 1 (- (high w) (low w))))
         (v (max 0 (min span (- (or (value w) 0) (low w))))))
    (/ v span)))

(defgeneric set-on-click (widget does)
  (:method ((w widget) does) (setf (on-click w) does) w))

(defgeneric set-on-change (widget does)
  (:method ((w widget) does) (setf (on-change w) does) w))

(defmethod initialize-instance :after ((w gap) &key)
  (when (zerop (expand w)) (setf (expand w) 1)))

(defmethod initialize-instance :after ((w rule) &key)
  (when (and (upright w) (char= (rule-glyph w) (code-char #x2500)))
    (setf (rule-glyph w) (code-char #x2502))))

(defmethod initialize-instance :after ((w slider) &key)
  (unless (numberp (value w)) (setf (value w) 0))
  (unless (numberp (low w))  (setf (low w) 0))
  (unless (numberp (high w)) (setf (high w) 100)))

(defmethod initialize-instance :after ((w ring) &key)
  (unless (numberp (value w)) (setf (value w) 0))
  (unless (numberp (low w))  (setf (low w) 0))
  (unless (numberp (high w)) (setf (high w) 100)))

(defun glyph (code)
  (if (integerp code) (string (code-char code)) (princ-to-string code)))

(defun %click (props) (getf props :on-click))

(defun %one (parts) (when (first parts) (list (first parts))))

(defun %without (props &rest keys)
  (loop :for (k v) :on props :by #'cddr
        :unless (member k keys) :append (list k v)))

(defun %split (args)
  (let ((props nil) (rest args))
    (loop :while (and rest (keywordp (car rest)) (cdr rest))
          :do (let ((key (pop rest)))
                (setf props (append props (list key (pop rest))))))
    (values props
            (loop :for c :in rest :when c :append (if (listp c) c (list c))))))

(defun %shown (it) (or it ""))

(defun label (text &rest props)
  (apply #'make-instance 'label
         :text (let ((it (%shown text)))
                 (if (stringp it) it (princ-to-string it)))
         props))

(defun icon (code &rest props)
  (let* ((g (glyph (%shown code)))
         (thunk (%click props)))
    (if thunk
        (apply #'make-instance 'action
               :on-click thunk
               :parts (list (make-instance 'label :text g
                                                  :class (getf props :glyph-class)
                                                  :face (getf props :face)
                                                  :font-size (getf props :font-size)))
               (%without props :face :font-size :on-click :confirm :glyph-class))
        (apply #'make-instance 'label :text g
               (%without props :on-click :confirm :glyph-class)))))

(defun button (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'action :on-click (%click props)
           :parts (%one parts)
           (%without props :on-click :confirm))))

(defun column (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'column :parts parts props)))

(defun row (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'row :parts parts props)))

(defun stack (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'stack :parts parts props)))

(defun box (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'box :parts (%one parts) props)))

(defun center (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'center :parts (%one parts) props)))

(defparameter +frame-glyphs+
  '(:single  (#\─ #\│ #\┌ #\┐ #\└ #\┘)
    :rounded (#\─ #\│ #\╭ #\╮ #\╰ #\╯)
    :heavy   (#\━ #\┃ #\┏ #\┓ #\┗ #\┛)
    :double  (#\═ #\║ #\╔ #\╗ #\╚ #\╝))
  "What each kind of line is drawn with: across, up, and the four corners.")

(defun clip (child &rest props)
  (apply #'make-instance 'clip :parts (list child) props))

(defun framed (child &rest props)
  "CHILD, boxed in on all four sides with the glyphs a border is drawn from,
each lit however PROPS' :FACE says. :LINE is :single, :rounded, :heavy or
:double. :TITLES is a
plist from :tl :tr :bl :br to a widget set into the border at that corner, one
cell in from it, and cut short when the border is too short for it."
  (let* ((face (getf props :face))
         (line (or (getf props :line) :single))
         (glyphs (or (getf +frame-glyphs+ line) (getf +frame-glyphs+ :single)))
         (titles (loop :for (corner it) :on (getf props :titles) :by #'cddr
                       :when it :append (list corner (clip it)))))
    (destructuring-bind (across up tl tr bl br) glyphs
      (apply #'make-instance 'framed :expand 1
             :line line
             :titles titles
             :parts (list* child
                           (rule :face face :glyph across) (rule :face face :glyph across)
                           (rule :upright t :face face :glyph up)
                           (rule :upright t :face face :glyph up)
                           (label (string tl) :face face) (label (string tr) :face face)
                           (label (string bl) :face face) (label (string br) :face face)
                           ;; last, so they are painted over the border they sit in
                           (loop :for (nil it) :on titles :by #'cddr :collect it))
             props))))

(defun centerbox (&key class hint expand start center end)
  (make-instance 'centerbox :class class :hint hint :expand (or expand 0)
                            :start start :middle center :end end))

(defun scroll (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'scroll :parts (%one parts) props)))

(defun gap (&rest props) (apply #'make-instance 'gap props))

(defun rule (&rest props) (apply #'make-instance 'rule props))

(defun slider (&rest args)
  (let ((subject (first args)))
    (if (keywordp subject)
        (apply #'make-instance 'slider args)
        (apply #'make-instance 'slider :value (or subject 0) (cl:rest args)))))

(defun ring (&rest args)
  (let ((subject (first args)))
    (if (keywordp subject)
        (multiple-value-bind (props parts) (%split args)
          (apply #'make-instance 'ring :parts (%one parts) props))
        (multiple-value-bind (props parts) (%split (cl:rest args))
          (apply #'make-instance 'ring
                 :parts (%one parts)
                 :value (or subject 0)
                 props)))))

(defun choice (&rest args)
  (multiple-value-bind (props parts) (%split args)
    (apply #'make-instance 'choice :parts (%one parts)
           :on-click (%click props)
           (%without props :on-click :confirm))))

(defun calendar (&rest props) (apply #'make-instance 'calendar props))

(defun image (where &rest props)
  (apply #'make-instance 'picture :path (princ-to-string (%shown where)) props))

(defun rows (items builder &rest props)
  (let ((all (if (listp items) items (list items))))
    (apply #'make-instance 'column
           :parts (loop :for item :in all
                        :for i :from 0
                        :collect (funcall builder item i))
           props)))
