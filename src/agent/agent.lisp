;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/agent)

(defparameter +hold+ 700)
(defparameter +turn-patience+ 60000)
(defparameter +still+ 2000)
(defparameter +trace-length+ 4000)
(defparameter +unrecognized-kept+ 50)
(defparameter +history-length+ 512)

(defvar *unrecognized-written* 0)

(defclass agent ()
  ((reader :initform nil :accessor agent-reader)
   (version :initform nil :accessor agent-version)
   (verified :initform nil :accessor agent-verified)
   (observation :initform nil :accessor agent-observation)
   (looked :initform nil :accessor agent-looked)
   (state :initform :unknown :accessor agent-state)
   (reason :initform nil :accessor agent-reason)
   (turn :initform 0 :accessor agent-turn)
   (in-turn :initform nil :accessor agent-in-turn)
   (idle-since :initform nil :accessor agent-idle-since)
   (quiet-since :initform nil :accessor agent-quiet-since)
   (still-since :initform nil :accessor agent-still-since)
   (moved :initform nil :accessor agent-moved)
   (heard :initform nil :accessor agent-heard)
   (prompted :initform nil :accessor agent-prompted-at)
   (kept :initform nil :accessor agent-kept)
   (events :initform nil :accessor agent-events)
   (trace :initform nil :accessor agent-trace)
   (traced :initform 0 :accessor agent-traced)
   (since :initform nil :accessor agent-since)
   (history :initform nil :accessor agent-history)
   (historied :initform 0 :accessor agent-historied)
   (told :initform nil :accessor agent-told)))

(defun agent-kind (agent)
  (if (agent-reader agent) (reader-name (agent-reader agent)) "agent"))

(defun agent-submits-typed-p (agent)
  (and (agent-reader agent) (eq :typed (reader-submit (agent-reader agent)))))

(defun agent-become (agent &key title command programs paths)
  (let* ((known (reader-for :title title :command command :programs programs))
         (version (and known (version-from known paths)))
         (exact (and version (reader-for-version (reader-name known) version)))
         (reader (or exact (and known version (nearest-reader (reader-name known) version)) known)))
    (unless (eq reader (agent-reader agent))
      (setf (agent-reader agent) reader
            (agent-observation agent) nil
            (agent-looked agent) nil
            (agent-state agent) :unknown
            (agent-reason agent) nil
            (agent-quiet-since agent) nil
            (agent-still-since agent) nil
            (agent-heard agent) nil))
    (setf (agent-version agent) version
          (agent-verified agent) (and exact t))
    agent))

(defun make-agent (&key title command programs paths)
  (agent-become (make-instance 'agent) :title title :command command
                                       :programs programs :paths paths))

(defun said-event (agent event)
  (setf (agent-events agent) (append (agent-events agent) (list event))))

(defun agent-take-events (agent)
  (prog1 (agent-events agent)
    (setf (agent-events agent) nil)))

(defun begin-turn (agent)
  (setf (agent-in-turn agent) t
        (agent-idle-since agent) nil)
  (said-event agent (list :turn (incf (agent-turn agent)) :began)))

(defun agent-prompted (agent now)
  (when (agent-reader agent)
    (setf (agent-prompted-at agent) now)
    (begin-turn agent)))

(defun agent-hear (agent state)
  (setf (agent-heard agent) state
        (agent-told agent) t))

(defun became (agent now state)
  (setf (agent-since agent) now)
  (push (list now state) (agent-history agent))
  (when (> (incf (agent-historied agent)) +history-length+)
    (setf (agent-history agent) (subseq (agent-history agent) 0 (floor +history-length+ 2))
          (agent-historied agent) (floor +history-length+ 2))))

(defun agent-for (agent now)
  (and (agent-since agent) (max 0 (- now (agent-since agent)))))

(defun agent-known-p (agent)
  (and (or (agent-reader agent) (agent-told agent)) t))

(defun traced (agent now moved said state)
  (push (list now moved said state) (agent-trace agent))
  (when (> (incf (agent-traced agent)) +trace-length+)
    (setf (agent-trace agent) (subseq (agent-trace agent) 0 (floor +trace-length+ 2))
          (agent-traced agent) (floor +trace-length+ 2))))

(defun unrecognized-dir ()
  (let ((state (uiop:getenv "XDG_STATE_HOME")))
    (uiop:ensure-directory-pathname
     (format nil "~A/atty/unrecognized/"
             (string-right-trim "/" (if (and state (plusp (length state)))
                                        state
                                        (format nil "~A.local/state"
                                                (namestring (user-homedir-pathname)))))))))

(defun keep-unrecognized (agent term)
  (let ((hash (sxhash (term:term-dump-to-string term))))
    (unless (or (member hash (agent-kept agent))
                (>= *unrecognized-written* +unrecognized-kept+))
      (push hash (agent-kept agent))
      (incf *unrecognized-written*)
      (ignore-errors
       (let ((path (merge-pathnames (format nil "~A-~A-~36R.sexp" (agent-kind agent)
                                            (or (agent-version agent) "unknown") hash)
                                    (unrecognized-dir))))
         (ensure-directories-exist path)
         (with-open-file (out path :direction :output :if-exists :supersede
                                   :external-format :utf-8)
           (write-snapshot (snapshot term) out)))))))

(defun held-by-prompt (agent state now)
  (let ((asked (agent-prompted-at agent)))
    (cond ((null asked) state)
          ((member state '(:working :blocked))
           (setf (agent-prompted-at agent) nil)
           state)
          ((< (- now asked) +turn-patience+) :working)
          (t (setf (agent-prompted-at agent) nil)
             state))))

(defun look-with-reader (agent term now moved)
  (when (or moved (not (agent-looked agent)))
    (setf (agent-observation agent) (observe (agent-reader agent) term)
          (agent-looked agent) t))
  (let ((seen (agent-observation agent)))
    (cond
      ((and seen (eq :skip (getf seen :means)))
       (values (agent-state agent) (agent-reason agent) :skip))
      (seen
       (values (held-by-prompt agent (getf seen :means) now) seen (getf seen :screen)))
      ((agent-heard agent)
       (values (agent-heard agent) (list :signalled (agent-heard agent)) :heard))
      ((>= (- now (agent-still-since agent)) +still+)
       (keep-unrecognized agent term)
       (values (held-by-prompt agent :unknown now)
               (list :unrecognized :program (agent-kind agent) :version (agent-version agent))
               :unrecognized))
      (t (values (agent-state agent) (agent-reason agent) :settling)))))

(defun look-without-reader (agent now moved)
  (cond
    ((agent-heard agent) (values (agent-heard agent) (list :signalled (agent-heard agent)) :heard))
    (moved (setf (agent-quiet-since agent) nil)
           (values :working nil :busy))
    ((eq :working (agent-state agent))
     (let ((since (or (agent-quiet-since agent) (setf (agent-quiet-since agent) now))))
       (if (>= (- now since) +hold+)
           (values :idle nil :quiet)
           (values :working nil :quiet))))
    (t (values :idle nil :quiet))))

(defun keep-turns (agent was now)
  (let ((state (agent-state agent)))
    (cond
      ((and (not (agent-in-turn agent)) (member state '(:working :blocked))
            (member was '(:idle :unknown)))
       (begin-turn agent))
      ((and (agent-in-turn agent) (eq state :idle))
       (let ((since (or (agent-idle-since agent) (setf (agent-idle-since agent) now))))
         (when (>= (- now since) +hold+)
           (setf (agent-in-turn agent) nil
                 (agent-idle-since agent) nil)
           (said-event agent (list :turn (agent-turn agent) :ended)))))
      ((not (eq state :idle))
       (setf (agent-idle-since agent) nil)))))

(defun agent-look (agent term now moved)
  (setf (agent-moved agent) moved)
  (when moved
    (setf (agent-heard agent) nil
          (agent-still-since agent) now))
  (unless (agent-still-since agent)
    (setf (agent-still-since agent) now))
  (let ((was (agent-state agent))
        (was-reason (agent-reason agent))
        (was-events (length (agent-events agent))))
    (multiple-value-bind (state reason said)
        (if (agent-reader agent)
            (look-with-reader agent term now moved)
            (look-without-reader agent now moved))
      (unless (and (eq state (agent-state agent)) (agent-since agent))
        (became agent now state))
      (setf (agent-state agent) state
            (agent-reason agent) reason)
      (keep-turns agent was now)
      (traced agent now moved said state))
    (or (not (eq was (agent-state agent)))
        (not (equal was-reason (agent-reason agent)))
        (> (length (agent-events agent)) was-events))))

(defun agent-explain (agent term)
  (let ((reader (agent-reader agent)))
    (if reader
        (values (getf (observe reader term) :screen)
                (observe-explained reader term))
        (values (if (agent-moved agent) :busy :quiet) nil))))

(defun screen-blocked-p (term)
  (loop :for reader :in *readers*
        :thereis (eq :blocked (getf (observe reader term) :means))))

(defun agent-won (agent term)
  (and (agent-reader agent) (getf (observe (agent-reader agent) term) :screen)))

(defun asks-of (seen)
  (when (and (eq :choice (getf seen :widget)) (getf seen :question))
    (let* ((question (getf seen :question))
           (before (loop :for paragraph :in (getf seen :paragraphs)
                         :until (member question paragraph :test #'string=)
                         :append (loop :for line :in paragraph
                                       :until (starts line "Tip:")
                                       :collect line))))
      (list :subject (or (first before) question)
            :detail (rest before)
            :question question
            :options (loop :for option :in (getf seen :options)
                           :for n :from 1
                           :collect (list n option))
            :chosen (and (getf seen :selected) (1+ (getf seen :selected)))))))

(defun agent-asks (agent term)
  (declare (ignore term))
  (and (eq :blocked (agent-state agent))
       (asks-of (agent-reason agent))))

(defun agent-doing (agent term)
  (let* ((lines (screen-lines term))
         (spinning (spinner lines))
         (reader (agent-reader agent)))
    (cond (spinning (getf spinning :label))
          ((and reader (reader-said reader)) (said lines (reader-said reader))))))
