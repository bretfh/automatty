;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty/test)
;;; A server in a thread, and a seer: a client on a terminal of its own that
;;; the tests type at and read. Everything the mux tests share.

(defvar *paths* 0)

(defun a-socket-path ()
  (format nil "~Aatty-~D-~D" (uiop:temporary-directory)
          (sb-posix:getpid) (incf *paths*)))

(defun until (seconds fn)
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :until (funcall fn)
          :do (when (> (get-internal-real-time) deadline) (return nil))
          (sleep 0.005)
          :finally (return t))))

(defun stop-server (path)
  (handler-case
      ;; the wire is given the socket, not just its number. A socket left
      ;; unclosed shuts its descriptor when it is finalised, and by then that
      ;; number belongs to whoever opened the next one.
      (let* ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream))
             (wire (progn (sb-bsd-sockets:socket-connect socket path)
                          (mux:make-wire
                           (sb-bsd-sockets:socket-file-descriptor socket)
                           socket))))
        (mux:wire-send wire '(:stop))
        (mux:wire-flush wire)
        (sleep 0.05)
        (mux:wire-close wire))
    (error () nil)))

(defmacro with-server ((path &key (command "cat") (rows 10) (cols 40)) &body body)
  (let ((thread (gensym "THREAD")) (broke (gensym "BROKE")))
    `(let ((,path (a-socket-path))
           ;; a fault in the loop is stepped over rather than fatal, so a test
           ;; would go on passing over one. This is where the server says what
           ;; broke, and a test that leaves anything here has found something
           (,broke (make-string-output-stream)))
       (let ((,thread (sb-thread:make-thread
                       (lambda ()
                         (let ((*error-output* ,broke))
                           (mux:serve ,path ,command :rows ,rows :cols ,cols
                                      :interval 0 :persist nil)))
                       :name "a test server")))
         (unwind-protect
             (progn (until 5 (lambda () (probe-file ,path)))
                    ,@body)
           (stop-server ,path)
           ;; a server left running goes on holding descriptors, and the next
           ;; test opens a pty onto the numbers it is about to close
           (when (eq :gave-up (sb-thread:join-thread ,thread :timeout 15
                                                     :default :gave-up))
             (error "a test server would not stop"))
           (ignore-errors (delete-file ,path))
           (let ((said (get-output-stream-string ,broke)))
             (is (equal "" said) "the server said something broke:~%~A" said)))))))

(defstruct seer client host master slave (decoder (term:make-decoder))
  (heard nil) (lock (sb-thread:make-mutex :name "a seer")) (going t) (reader nil))

(defun seer-listen (seer)
  (loop :while (seer-going seer)
        :do (when (pty:pty-wait (seer-master seer) 50)
              (let ((said (ignore-errors (pty:pty-read-string (seer-master seer) 65536))))
                (when (and said (plusp (length said)))
                  (sb-thread:with-mutex ((seer-lock seer))
                    (push said (seer-heard seer))))))))

(defun seer-take (seer)
  (sb-thread:with-mutex ((seer-lock seer))
    (prog1 (reverse (seer-heard seer))
      (setf (seer-heard seer) nil))))

(defun a-seer (path &key (rows 10) (cols 40))
  (multiple-value-bind (master slave-path) (pty:open-pty)
                       (let ((slave (sb-posix:open slave-path sb-posix:o-rdwr)))
                         (pty:pty-set-size master rows cols)
                         (tty:host-raw slave)
                         (let ((seer (make-seer :client (mux:make-client path :fd slave :to slave)
                                                :host (a-term :width cols :height rows)
                                                :master master
                                                :slave slave)))
                           (setf (seer-reader seer)
                                 (sb-thread:make-thread #'seer-listen :arguments (list seer)
                                                                      :name "a seer reading"))
                           seer))))

(defun seer-close (seer)
  (ignore-errors (mux:client-close (seer-client seer)))
  (setf (seer-going seer) nil)
  (when (seer-reader seer)
    (ignore-errors (sb-thread:join-thread (seer-reader seer) :timeout 2)))
  (ignore-errors (sb-posix:close (seer-slave seer)))
  (ignore-errors (pty:pty-close (seer-master seer))))

(defun pump (seer &key (seconds 8) want until)
  "Run the client and read what it drew, until WANT is on its screen.

SECONDS is there to stop a hang, not to measure anything: a test that finds what
it wanted returns at once, so the only runs that feel the deadline are the ones
already failing."
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop
     (mux:client-step (seer-client seer) 5)
     (dolist (said (seer-take seer))
       (term:term-process-output
        (seer-host seer)
        (term:decode-utf-8 (seer-decoder seer) said)))
     (when (and want (search want (term:term-dump-to-string (seer-host seer))))
       (return t))
     (when (and until (funcall until))
       (return t))
     (when (> (get-internal-real-time) deadline)
       (return (and (null want) (null until)))))))

(defun type-at (seer said)
  (pty:pty-write-string (seer-master seer) said))

(defun seen (seer)
  (term:term-dump-to-string (seer-host seer)))

(defun seen-below-bar (seer)
  (let ((all (seen seer)))
    (subseq all (min (length all) (1+ (or (position #\Newline all) (length all)))))))

(defmacro with-seer ((seer path &rest args) &body body)
  `(let ((,seer (a-seer ,path ,@args)))
     (unwind-protect (progn ,@body) (seer-close ,seer))))

(defun step-until (server test &optional (seconds 5))
  (let ((deadline (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
    (loop :until (funcall test)
          :do (mux:server-step server :interval 0)
          (when (> (get-internal-real-time) deadline) (return nil))
          :finally (return t))))

(defmacro with-a-server-here ((server path) &body body)
  "A server stepped by hand, so a test can look at what it holds between steps."
  `(let ((,path (a-socket-path)))
     (unwind-protect
         (let ((,server (mux:make-server ,path)))
           (unwind-protect (progn ,@body)
             (mux:server-close ,server)))
       (ignore-errors (delete-file ,path)))))

(defun heard-back (wire)
  (when (mux:wire-receive wire)
    (loop :for form := (mux:wire-read-message wire) :while form :collect form)))

(defun where-said (seer said)
  "Which column SAID starts at on the seer's screen, or nil."
  (loop :for y :below (term:term-height (seer-host seer))
        :for found := (search said (term:term-dump-row-string (seer-host seer) y))
        :when found :do (return found)))

(defun a-wire-to (path)
  (let ((socket (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-connect socket path)
    (mux:make-wire (sb-bsd-sockets:socket-file-descriptor socket) socket)))

(defun say-to (wire &rest forms)
  (dolist (form forms) (mux:wire-send wire form))
  (mux:wire-flush wire))

(defun heard-from (server wire tag)
  "Step SERVER until WIRE has been told something tagged TAG, and answer it."
  (let ((heard nil))
    (step-until server (lambda ()
                         (setf heard (append heard (heard-back wire)))
                         (find tag heard :key #'first)))
    (find tag heard :key #'first)))

(defun heard-tags (server wire)
  "A function of one tag, each call like HEARD-FROM, but sharing one
accumulator: a message that arrives in the same batch as an earlier one asked
for is kept rather than thrown away with it, for the next call to find."
  (let ((heard nil))
    (lambda (tag)
      (step-until server (lambda ()
                           (setf heard (append heard (heard-back wire)))
                           (find tag heard :key #'first)))
      (let ((hit (find tag heard :key #'first)))
        (setf heard (remove hit heard :count 1))
        hit))))

(defun session-names (server)
  (mapcar #'mux:session-name (mux:server-sessions server)))

(defun settled (server pane)
  (let ((agent (mux:pane-agent pane)))
    (step-until server (lambda () (member (agent:agent-state agent) '(:idle :blocked))))))

(defun blocked (server pane)
  (settled server pane)
  (agent:agent-hear (mux:pane-agent pane) :blocked)
  (step-until server (lambda () (eq :blocked (agent:agent-state (mux:pane-agent pane))))))

(defun a-dialog-script ()
  "A script that sets the title a coding agent sets, draws a permission dialog
under a rule, and then echoes whatever it is answered."
  (let ((path (format nil "~Aatty-dialog-~D.sh" (uiop:temporary-directory) (sb-posix:getpid))))
    (with-open-file (out path :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (format out "printf '\\033]0;✳ Claude Code\\007'~%")
      (dolist (line '("────────────────────────────────────"
                      " Bash command" ""
                      "   python3 -m pytest -q" ""
                      " Do you want to proceed?"
                      " ❯ 1. Yes"
                      "   2. No, and tell Claude what to do differently (esc)" ""
                      " Esc to cancel · Tab to amend"))
        (format out "printf '%s\\n' '~A'~%" line))
      (format out "stty -echo -icanon min 1; a=$(head -c 1); printf '\\033[2J\\033[H'; echo answered-with-$a; sleep 30~%"))
    path))

(defun where-on (seer said &key (from 0))
  "Column and row, from 0, where SAID first is on the seer's screen, looking
from row FROM down."
  (loop :for y :from from :below (term:term-height (seer-host seer))
        :for x := (search said (term:term-dump-row-string (seer-host seer) y))
        :when x :do (return (values x y))))

(defun click-at (seer x y)
  (type-at seer (format nil "~C[<0;~D;~DM~C[<0;~D;~Dm" #\Escape (1+ x) (1+ y)
                        #\Escape (1+ x) (1+ y))))

(defun a-told-client (&rest rows)
  "A client that has been told about ROWS, without a server behind it."
  (let ((client (mux::%make-client)))
    (dolist (row rows client)
      (setf (gethash (cons (getf row :session) (getf row :id)) (mux::client-panes client))
            (list* :heard-at (mux::client-ms)
                   ;; a row made up here is a known agent unless it says not
                   (if (member :known row) row (list* :known t row)))))))

(defun board-client ()
  (let ((client (a-told-client
                 (list :session "todo" :order 0 :at 0 :id 1 :says "arch" :kind "claude-code"
                       :state :working :for 41000 :doing "Recording clarification 9…"
                       :driven-by "todo:4")
                 (list :session "todo" :order 0 :at 1 :id 2 :says "impl" :kind "claude-code"
                       :state :blocked :for 42000
                       :asks '(:subject "Bash command" :detail ("python3 -m pytest -q")
                               :options ((1 "Yes") (2 "No"))))
                 (list :session "todo" :order 0 :at 2 :id 4 :says "ctl" :kind "atty"
                       :state :working :for 2000 :drives (list "todo:1"))
                 (list :session "lib" :order 1 :at 0 :id 7 :says "tests" :kind "make"
                       :state :idle :for 3000
                       :history '((3000 :idle) (60000 :working))))))
    (setf (mux::client-session client) "todo")
    client))

(defun board-screen (client b &key (cols 150) (rows 30))
  "B drawn for CLIENT on a screen COLS by ROWS, as one string."
  (let ((screen (tty:make-screen :width cols :height rows)))
    (setf (mux::client-rows client) rows (mux::client-cols client) cols)
    (let ((mux::*overlay-client* client)) (mux:draw-overlay b screen))
    (values (format nil "~{~A~%~}" (loop :for y :below rows :collect (shown screen y))) screen)))

(defun a-buffer-wire ()
  "A wire that keeps what is sent to it: the write end of a pipe nobody reads."
  (multiple-value-bind (r w) (sb-posix:pipe)
    (declare (ignore r))
    (mux:make-wire w)))

(defun sent-on (wire)
  (sb-ext:octets-to-string (subseq (mux::wire-out wire) 0 (mux::wire-end wire))
                           :external-format :utf-8))

(defun five-windows (client)
  "The todo session laid out as five windows, the first split."
  (setf (mux::client-id client) 7
        (mux::client-clients client) '((7 "/dev/ttys042" 40 150 "todo" 1 1000 nil)
                                       (9 "/dev/ttys051" 30 100 "lib" 1 5000 200))
        (gethash "todo" (mux::client-layouts client))
        '((1 "agents" (:across 1 (:down 2 4)) 2 t) (2 "notes" 1 1 nil) (3 "ctl" 4 4 nil)
          (4 "more" 2 2 nil) (5 "last" 1 1 nil)))
  client)

(defun dumped (pane)
  (term:term-dump-to-string (mux:pane-term pane)))

(defmacro with-a-session ((session pane server command &key (rows 12) (cols 30)) &body body)
  "One session of one pane running COMMAND on a server stepped by hand, laid
out: the bar on the first row, the pane under it, its scrollbar down the last
column."
  (let ((path (gensym "PATH")))
    `(with-a-server-here (,server ,path)
       (let* ((,session (mux:add-session ,server ,command :name "work" :rows ,rows :cols ,cols))
              (,pane (mux:session-focus ,session)))
         ,@body))))

(defun heard-from-wire (wire tag &optional (seconds 5))
  "Wait until WIRE, to a server running on its own thread, has been told
something tagged TAG, and answer it."
  (let ((heard nil))
    (until seconds (lambda ()
                     (setf heard (append heard (heard-back wire)))
                     (find tag heard :key #'first)))
    (find tag heard :key #'first)))

(defun screen-text (screen)
  "A composed screen's characters, row by row."
  (format nil "~{~A~%~}"
          (loop :for y :below (tty:screen-height screen)
                :collect (let ((row (aref (tty:screen-grid screen) y)))
                           (coerce (loop :for x :below (tty:screen-width screen)
                                         :collect (term:row-char row x))
                                   'string)))))
