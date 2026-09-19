(defpackage #:vtx/mode
            (:use #:cl)
            (:export
             #:key #:make-key #:parse-key #:chord #:spelled #:keysym-name
             #:key-sym #:key-ctrl #:key-meta #:key-shift #:key-super
             #:self-inserting
             #:mode #:define-mode #:modes #:mode-named #:current-mode #:with-mode
             #:global-map #:global-set-key #:global-unset-key
             #:define-key #:undefine-key #:lookup-key #:keys-in-force #:as-handler
             #:press #:pending #:prefixp #:*unbound* #:*run* #:*pending*
             #:mode-value #:setq-mode #:setq-default))
(in-package #:vtx/mode)

(defun split-spec (spec)
  "SPEC cut at the spaces between one key and the next."
  (let ((out nil) (from 0))
    (dotimes (i (length spec))
      (when (char= #\Space (char spec i))
        (push (subseq spec from i) out)
        (setf from (1+ i))))
    (push (subseq spec from) out)
    (nreverse out)))

(defvar *interned* (make-hash-table :test 'equal :synchronized t))

(defparameter +keysyms+
              '(("SPC" "space" "Space")
                ("RET" "Return" "Enter" "KP_Enter")
                ("TAB" "Tab" "ISO_Left_Tab")
                ("DEL" "BackSpace")
                ("Escape" "Escape" "Esc")
                ("PageUp" "Prior")
                ("PageDown" "Next")))

(defstruct (key (:constructor %key) (:copier nil))
           (sym "" :type string :read-only t)
           (ctrl nil :read-only t)
           (meta nil :read-only t)
           (shift nil :read-only t)
           (super nil :read-only t))

(defun %named (sym)
  (if (< (length sym) 2)
      sym
    (or (first (find sym +keysyms+
                     :test (lambda (sym row) (member sym row :test #'string-equal))))
        sym)))

(defun keysym-name (sym)
  (or (second (assoc sym +keysyms+ :test #'string=)) sym))

(defun make-key (sym &key ctrl meta shift super)
  (let* ((sym (%named sym))
         (id (list sym ctrl meta shift super)))
    (or (gethash id *interned*)
        (sb-ext:with-locked-hash-table (*interned*)
                                       (or (gethash id *interned*)
                                           (setf (gethash id *interned*)
                                                 (%key :sym sym :ctrl ctrl :meta meta :shift shift :super super)))))))

(defun parse-key (spec)
  (let ((ctrl nil) (meta nil) (shift nil) (super nil)
        (i 0) (n (length spec)))
    (loop :while (and (< (1+ i) n) (char= (char spec (1+ i)) #\-))
          :do (case (char spec i)
                    (#\C (setf ctrl t))
                    (#\M (setf meta t))
                    (#\S (setf shift t))
                    (#\s (setf super t))
                    (t (return)))
          (incf i 2))
    (make-key (subseq spec i) :ctrl ctrl :meta meta :shift shift :super super)))

(defun spelled (keys)
  (format nil "~{~a~^ ~}"
          (mapcar (lambda (k)
                    (with-output-to-string (s)
                                           (when (key-ctrl k) (write-string "C-" s))
                                           (when (key-meta k) (write-string "M-" s))
                                           (when (key-shift k) (write-string "S-" s))
                                           (when (key-super k) (write-string "s-" s))
                                           (write-string (key-sym k) s)))
                  (if (listp keys) keys (list keys)))))

(defun self-inserting (spec)
  (let ((keys (chord spec)))
    (when (= 1 (length keys))
      (let ((k (first keys)))
        (unless (or (key-ctrl k) (key-meta k) (key-super k))
          (let ((sym (key-sym k)))
            (cond ((string= sym "SPC") " ")
                  ((and (= 1 (length sym)) (graphic-char-p (char sym 0)))
                   (if (key-shift k) (string-upcase sym) sym)))))))))

(defun chord (spec)
  (let ((keys (remove "" (split-spec spec)
                      :test #'string=)))
    (if (and (null keys) (plusp (length spec)))
        (list (parse-key "SPC"))
      (mapcar #'parse-key keys))))
