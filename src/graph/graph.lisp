(defpackage #:vt/graph
  (:use #:cl)
  (:export
   #:place #:cell #:derived #:make-cell #:make-derived
   #:value #:stored #:reader #:writer #:touch #:same #:name-place #:as-handler
   #:depend-on #:depend #:undepend #:livep #:forget
   #:later #:repeat #:cancel #:stop-work
   #:watch #:unwatch #:watcher #:subject
   #:cas-p #:emptied #:note #:*to*
   #:*broke* #:*await-inline* #:*give-up-seconds* #:*retry-seconds*
   #:*slow-after-seconds* #:*workers* #:*most-workers*))
(in-package #:vt/graph)

(defvar *reading* nil)
(defvar *visiting* nil)
(defvar *visit* 0)
(defvar *revision* 0)
(defvar *broke* nil)
(defvar *await-inline* nil)
(defvar *retry-seconds* 5)
(defvar *give-up-seconds* 30)
(defvar *slow-after-seconds* 0.05)
(defparameter +unread+ '#:unread)

(defmacro cas-p (place old new &environment env)
  (multiple-value-bind (temps values old-var new-var cas-form read-form)
      (sb-ext:get-cas-expansion place env)
    (declare (ignore read-form))
    `(let* (,@(mapcar #'list temps values)
            (,old-var ,old)
            (,new-var ,new))
       (eq ,old-var ,cas-form))))

(defmacro emptied (place &environment env)
  (multiple-value-bind (temps values old new cas-form read-form)
      (sb-ext:get-cas-expansion place env)
    `(let* (,@(mapcar #'list temps values)
            (,new nil))
       (loop :for ,old := ,read-form
             :until (eq ,old ,cas-form)
             :finally (return ,old)))))

(defclass place ()
  ((dependents  :initform nil :accessor dependents)
   (depends-on  :initform nil :accessor depends-on)
   (mtime       :initform 0   :accessor mtime)
   (visited-at  :initform 0   :accessor visited-at)
   (verified-at :initform -1  :accessor verified-at)))

(defclass cell (place)
  ((stored :initarg :stored :accessor stored :initform nil)))

(defclass derived (place)
  ((reader    :initarg :reader :accessor reader :initform nil)
   (writer    :initarg :writer :accessor writer :initform nil)
   (live      :initarg :live   :reader   livep  :initform nil)
   (waits     :initform nil       :accessor waits-p)
   (cached    :initform +unread+  :accessor cached)
   (last-good :initform +unread+  :accessor last-good)
   (computing-thread :initform nil :accessor computing-thread)
   (claimed-at :initform nil :accessor claimed-at)
   (waiting    :initform nil :accessor waiting)
   (scheduled  :initform nil :accessor scheduled)))

(defun make-cell (&optional stored) (make-instance 'cell :stored stored))

(defun make-derived (reader &optional writer live)
  (make-instance 'derived :reader reader :writer writer :live live))

(defgeneric same (a b)
  (:method (a b) (equal a b))
  (:method ((a number) (b number)) (= a b))
  (:method ((a string) (b string)) (string= a b)))

(defvar *named* (make-hash-table :test 'eq :synchronized t))

(defgeneric place (x)
  (:method (x) (declare (ignore x)) nil)
  (:method ((x place)) x)

  (:method ((x symbol))
    (let ((it (gethash x *named*)))
      (if (functionp it) (funcall it) it))))

(defun name-place (name it) (setf (gethash name *named*) it) name)

(defgeneric value (x)
  (:method (x) (let ((p (place x))) (if p (value p) x)))
  (:method ((x cell)) (%noted x (mtime x) (stored x))))

(defgeneric (setf value) (v x)
  (:method (v x)
    (let ((p (place x)))
      (unless p (error "~s is not a place, and names none." x))
      (setf (value p) v))
    v)
  (:method (v (x cell)) (setf (stored x) v) (touch x) v)
  (:method (v (x derived))
    (let ((it (writer x)))
      (unless it (error "~s takes no writing." x))
      (funcall it v))
    v))

(defgeneric touch (x))

(defun depend (x on)
  (unless (eq x on)
    (sb-ext:atomic-update (slot-value on 'dependents)
                          (lambda (had) (if (member x had) had (cons x had))))))

(defun undepend (x on)
  (sb-ext:atomic-update (slot-value on 'dependents)
                        (lambda (had) (remove x had))))

(defun %noted (x at value)
  (when *reading* (push (cons x at) (cdr *reading*)))
  value)

(defun %moved-on (x)
  (sb-ext:atomic-update (slot-value x 'mtime) #'1+)
  (sb-ext:atomic-update *revision* #'1+)
  x)

(defun %seen (x epoch)
  (loop :for had := (visited-at x)
        :do (when (>= had epoch) (return nil))
            (when (cas-p (slot-value x 'visited-at) had epoch) (return t))))

(defmethod touch ((x place))
  (%moved-on x)
  (let ((epoch (or *visiting* (sb-ext:atomic-update *visit* #'1+))))
    (when (%seen x epoch)
      (let ((*visiting* epoch))
        (dolist (each (dependents x)) (touch each)))))
  x)

(defun as-handler (does)
  (typecase does
    (null nil)
    (function does)
    (cons (lambda () (eval does)))
    (t (let ((p (place does)))
         (if p
             (lambda () (setf (value p) t))
             (error "~s is not something a press can do." does))))))

(defvar *to* *error-output*)

(defun note (control &rest args)
  (when *to*
    (format *to* "~&~a~%" (apply #'format nil control args))
    (force-output *to*))
  nil)

(defvar *workers* 4)
(defvar *most-workers* 32)
(defvar *pool* nil)
(defvar *ticks* nil)
(defvar *clock* nil)
(defvar *winding* (sb-thread:make-mutex :name "vt/clock"))

(defstruct (pool (:copier nil))
  threads
  (queue nil)
  (lock (sb-thread:make-mutex :name "vt/work"))
  (gate (sb-thread:make-waitqueue))
  (busy 0 :type sb-ext:word)
  (stopping nil))

(defstruct (tick (:constructor %tick (every does)) (:copier nil))
  every does (due 0) (gone nil))

(defun %take (p)
  (sb-thread:with-mutex ((pool-lock p))
    (loop :until (or (pool-queue p) (pool-stopping p))
          :do (sb-thread:condition-wait (pool-gate p) (pool-lock p) :timeout 1))
    (pop (pool-queue p))))

(defun %works (p)
  (loop :until (pool-stopping p)
        :for thunk := (%take p)
        :when thunk
          :do (sb-ext:atomic-incf (pool-busy p))
              (unwind-protect
                   (handler-case (funcall thunk)
                     (error (c) (when *broke* (funcall *broke* c nil))))
                (sb-ext:atomic-decf (pool-busy p)))))

(defun %pool ()
  (or *pool*
      (let ((p (make-pool)))
        (setf (pool-threads p)
              (loop :repeat *workers*
                    :collect (sb-thread:make-thread
                              (lambda () (%works p)) :name "vt/work")))
        (setf *pool* p))))

(defun %grow (p)
  (when (and (> (+ (pool-busy p) (length (pool-queue p)))
                (length (pool-threads p)))
             (< (length (pool-threads p)) *most-workers*))
    (push (sb-thread:make-thread (lambda () (%works p)) :name "vt/work")
          (pool-threads p))))

(defun later (thunk)
  (let ((p (%pool)))
    (sb-thread:with-mutex ((pool-lock p))
      (setf (pool-queue p) (append (pool-queue p) (list thunk)))
      (%grow p)
      (sb-thread:condition-notify (pool-gate p)))
    thunk))

(defun %now () (/ (get-internal-real-time) internal-time-units-per-second))

(defun %ticking ()
  (loop
    (let ((due nil) (wait 1))
      (dolist (each *ticks*)
        (unless (tick-gone each)
          (when (<= (tick-due each) (%now))
            (setf (tick-due each) (+ (%now) (tick-every each)))
            (push each due))))
      (dolist (each due) (later (tick-does each)))
      (sb-ext:atomic-update *ticks* (lambda (had) (remove-if #'tick-gone had)))
      (sb-thread:with-mutex (*winding*)
        (unless *ticks* (setf *clock* nil) (return)))
      (dolist (each *ticks*)
        (unless (tick-gone each)
          (setf wait (min wait (max 0.001 (- (tick-due each) (%now)))))))
      (sleep wait))))

(defun %wind ()
  (sb-thread:with-mutex (*winding*)
    (unless *clock*
      (setf *clock* (sb-thread:make-thread #'%ticking :name "vt/clock")))))

(defun repeat (seconds does)
  (let ((it (%tick seconds does)))
    (setf (tick-due it) (+ (%now) seconds))
    (sb-ext:atomic-update *ticks* (lambda (had) (cons it had)))
    (%wind)
    it))

(defun cancel (it) (setf (tick-gone it) t) it)

(defun stop-work ()
  (let ((p *pool*))
    (dolist (each *ticks*) (cancel each))
    (when p
      (sb-thread:with-mutex ((pool-lock p))
        (setf (pool-stopping p) t)
        (sb-thread:condition-notify (pool-gate p)))
      (dolist (each (pool-threads p))
        (ignore-errors (sb-thread:join-thread each))))
    (setf *pool* nil)
    nil))

(defstruct (memo (:constructor memo (value from at)) (:copier nil) (:predicate memop))
  value from at)

(defun current-mtime (x)
  (let ((h (and (typep x 'derived) (cached x))))
    (if (memop h) (memo-at h) (mtime x))))

(defun currentp (x at)
  (and (eql at (current-mtime x)) (validp x)))

(defun %settled (each at)
  (or (currentp each at)
      (and (typep each 'derived)
           (not (livep each))
           (let ((*reading* nil))
             (value each)
             (eql at (current-mtime each))))))

(defun %whole (from)
  (loop :for (each . at) :in from :always (%settled each at)))

(defun validp (x)
  (cond ((not (typep x 'derived)) t)
        ((livep x) t)
        ((not (memop (cached x))) nil)
        ((eql (verified-at x) *revision*) t)
        (t (let ((h (cached x))
                 (w *revision*))
             (when (%whole (memo-from h))
               (when (eql w *revision*) (setf (verified-at x) w))
               t)))))

(defun %memo (n)
  (let ((h (cached n)))
    (when (and (memop h) (%whole (memo-from h))) h)))

(defun %waiter (n)
  (or (waiting n)
      (progn (cas-p (slot-value n 'waiting) nil
                    (cons (sb-thread:make-mutex :name "vt/working")
                          (sb-thread:make-waitqueue)))
             (waiting n))))

(defun %wait (n)
  (let ((it (%waiter n)))
    (sb-thread:with-mutex ((car it))
      (if (computing-thread n)
          (sb-thread:condition-wait (cdr it) (car it) :timeout *retry-seconds*)
          (sb-thread:condition-notify (cdr it))))))

(defun %done (n)
  (let ((it (%waiter n)))
    (sb-thread:with-mutex ((car it))
      (setf (computing-thread n) nil (claimed-at n) nil)
      (sb-thread:condition-notify (cdr it)))))

(defun %overdue (n)
  (let ((since (claimed-at n)))
    (and (computing-thread n) since
         (> (- (get-universal-time) since) *give-up-seconds*))))

(defun %timed (n began)
  (setf (waits-p n)
        (> (- (get-internal-real-time) began)
           (* *slow-after-seconds* internal-time-units-per-second)))
  n)

(defun %edges (n from)
  (let ((had (depends-on n))
        (now (remove-duplicates (mapcar #'car from))))
    (dolist (on had) (unless (member on now) (undepend n on)))
    (setf (depends-on n) now)
    (dolist (on now) (depend n on))))

(defun %broke (n condition)
  (when *broke* (funcall *broke* condition n))
  nil)

(defun %compute (n)
  (let ((it (reader n)))
    (and it (funcall it))))

(defun %mine (n)
  (let ((reading (cons :reading nil))
        (from nil)
        (began (get-internal-real-time)))
    (unwind-protect
         (block mine
           (handler-bind ((error (lambda (c)
                                   (%broke n c)
                                   (return-from mine (values nil :broke nil)))))
             (let ((v (let ((*reading* reading)) (%compute n))))
               (%timed n began)
               (setf from (cdr reading))
               (cond ((%whole from)
                      (let* ((had (last-good n))
                             (unmoved (and (memop had) (same v (memo-value had))))
                             (at (if unmoved (memo-at had) (1+ (current-mtime n)))))
                        (setf (cached n) (memo v from at)
                              (last-good n) (cached n)
                              (verified-at n) -1)
                        (values v t at)))
                     (t (values v nil nil))))))
      (%edges n (or from (cdr reading))))))

(defun %last-value (n)
  (let ((h (last-good n)))
    (when (memop h) (memo-value h))))

(defun %gave-up (n)
  (%broke n (make-condition 'simple-error
                            :format-control "~s could not be worked out within ~d second~:p"
                            :format-arguments (list n *give-up-seconds*)))
  (values (%last-value n) (current-mtime n)))

(defun %work-out (n)
  (let ((me sb-thread:*current-thread*)
        (due (+ (get-universal-time) *give-up-seconds*)))
    (loop
      (let ((h (%memo n)))
        (when h (return (values (memo-value h) (memo-at h)))))
      (when (eq (computing-thread n) me)
        (error "~s is worked out from itself." n))
      (when (> (get-universal-time) due) (return (%gave-up n)))
      (when (%overdue n) (return (values (%last-value n) (current-mtime n))))
      (if (and (null (computing-thread n))
               (cas-p (slot-value n 'computing-thread) nil me))
          (multiple-value-bind (v stands at)
              (unwind-protect (progn (setf (claimed-at n) (get-universal-time))
                                     (%mine n))
                (%done n))
            (cond ((eq stands :broke) (return (values (%last-value n) (current-mtime n))))
                  (stands (return (values v at)))))
          (%wait n)))))

(defun %landed (n)
  (sb-ext:atomic-update *revision* #'1+)
  (let ((epoch (sb-ext:atomic-update *visit* #'1+)))
    (let ((*visiting* epoch))
      (dolist (each (dependents n)) (touch each))))
  n)

(defun %hand-off (n)
  (when (cas-p (slot-value n 'scheduled) nil t)
    (later (lambda ()
             (unwind-protect (progn (%work-out n) (%landed n))
               (setf (scheduled n) nil)))))
  (let ((h (%memo n)))
    (if h
        (values (memo-value h) (memo-at h))
        (values (%last-value n) (current-mtime n)))))

(defmethod value ((n derived))
  (if (livep n)
      (let ((before (mtime n)))
        (%noted n before (%compute n)))
      (let ((h (%memo n)))
        (cond (h (%noted n (memo-at h) (memo-value h)))
              ((and (waits-p n) (not *await-inline*))
               (multiple-value-bind (v at) (%hand-off n) (%noted n at v)))
              (t (multiple-value-bind (v at) (%work-out n) (%noted n at v)))))))

(defun depend-on (x)
  (if (and (typep x 'derived) (not (livep x)))
      (progn (value x) x)
      (%noted x (current-mtime x) x)))

(defun forget (n)
  (when (typep n 'derived)
    (setf (cached n) +unread+)
    (setf (last-good n) +unread+))
  (touch n)
  n)
