;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:atty)

(defun fetch-url (url)
  (let* ((out (make-string-output-stream))
         (process (sb-ext:run-program "curl" (list "-fsSL" "--max-time" "20" url)
                                      :search t :output out :error nil)))
    (unless (eql 0 (sb-ext:process-exit-code process))
      (error "could not fetch ~A" url))
    (get-output-stream-string out)))

;;; Releases: where they are, which is the latest, and putting one here.

(defun releases-url ()
  "Where the releases are downloaded from; ATTY_RELEASES_URL says another
place, a file: one in a test."
  (let ((said (sb-ext:posix-getenv "ATTY_RELEASES_URL")))
    (string-right-trim "/" (if (and said (plusp (length said)))
                               said
                               "https://github.com/bretfh/automatty/releases"))))

(defun latest-release-url ()
  "What says which release is the latest: github's api, or <ATTY_RELEASES_URL>/latest."
  (if (sb-ext:posix-getenv "ATTY_RELEASES_URL")
      (format nil "~A/latest" (releases-url))
      "https://api.github.com/repos/bretfh/automatty/releases/latest"))

(defun parse-tag (json)
  "The tag_name in what github says of a release, without reading json."
  (let ((at (search "\"tag_name\"" json)))
    (when at
      (let* ((open (position #\" json :start (+ at (length "\"tag_name\"") 1)))
             (close (and open (position #\" json :start (1+ open)))))
        (and open close (subseq json (1+ open) close))))))

(defun latest-release ()
  "The tag of the latest release, or nil when nothing says."
  (parse-tag (fetch-url (latest-release-url))))

(defun release-asset (tag)
  "What the tarball for TAG on this platform is called."
  (format nil "atty-~A-~A.tar.gz" tag (platform-name)))

(defun fetch-to-file (url path)
  (let ((process (sb-ext:run-program "curl" (list "-fsSL" "--max-time" "600" "-o" (namestring path) url)
                                     :search t :output nil :error nil)))
    (unless (eql 0 (sb-ext:process-exit-code process))
      (error "could not fetch ~A" url))
    path))

(defun sha256-of (path)
  "The sha256 of the file at PATH, by whichever of sha256sum and shasum is
here: sbcl has none of its own, and one of the two is wherever curl is."
  (dolist (command (list (list "sha256sum" (namestring path))
                         (list "shasum" "-a" "256" (namestring path)))
                   (error "neither sha256sum nor shasum is here to check the download"))
    (let* ((out (make-string-output-stream))
           (process (ignore-errors
                     (sb-ext:run-program (first command) (rest command)
                                         :search t :output out :error nil))))
      (when (and process (eql 0 (sb-ext:process-exit-code process)))
        (let ((said (get-output-stream-string out)))
          (return (subseq said 0 (position #\Space said))))))))

(defun listed-sha256 (sums name)
  "The sum SHA256SUMS gives for NAME."
  (with-input-from-string (in sums)
    (loop :for line := (read-line in nil)
          :while line
          :do (let ((at (position #\Space line)))
                (when (and at (string= name (string-left-trim " *" (subseq line at))))
                  (return (subseq line 0 at)))))))

(defun file-owner (path)
  "Who put the program at PATH there, and so who replaces it: :atty when it
is this user's to write, else the package manager it came from. Answers the
owner and what they run to update it."
  (let* ((real (namestring (or (ignore-errors (truename path)) path)))
         (dir (directory-namestring real))
         (writable (handler-case (progn (sb-posix:access real sb-posix:w-ok)
                                        (sb-posix:access dir sb-posix:w-ok)
                                        t)
                     (error () nil))))
    (cond ((search "/gnu/store/" real)
           (values :guix "guix pull && guix upgrade atty"))
          ((search "/nix/store/" real)
           (values :nix "nix profile upgrade atty"))
          ((search "/Cellar/" real)
           (values :brew "brew upgrade atty"))
          ((and (not writable) (eql 0 (search "/usr/" real)))
           (values :distro "the package manager that put it there, apt, dnf or pacman, upgrades atty"))
          ((not writable)
           (values :somebody (format nil "~A is not yours to write; make FOREIGN=1 install PREFIX=$HOME/.local puts one where it is"
                                     real)))
          (t (values :atty "atty update")))))

(defun release-message (tag)
  "One line for whoever attaches next: a release is out, and what brings it here."
  (multiple-value-bind (owner how) (file-owner (or (executable-path) ""))
    (if (eq owner :atty)
        (format nil "atty ~A is out; atty update brings it here" tag)
        (format nil "atty ~A is out; ~A, then atty restart-server" tag how))))

(defun install-release (tag me)
  "Fetch the tarball for TAG, check it against SHA256SUMS, and put its atty
where ME is, in one rename, so ME is at every moment a whole program. The
work happens beside ME, on the same filesystem, and is gone afterwards."
  (let* ((asset (release-asset tag))
         (dir (format nil "~A.update-~D/" me (sb-posix:getpid))))
    (ensure-directories-exist dir)
    (unwind-protect
         (let ((tarball (fetch-to-file (format nil "~A/download/~A/~A" (releases-url) tag asset)
                                       (merge-pathnames asset dir)))
               (sums (fetch-url (format nil "~A/download/~A/SHA256SUMS" (releases-url) tag))))
           (let ((listed (listed-sha256 sums asset))
                 (got (sha256-of tarball)))
             (unless listed (error "SHA256SUMS for ~A does not list ~A" tag asset))
             (unless (string-equal listed got)
               (error "~A does not match its sum: it is ~A, SHA256SUMS says ~A" asset got listed)))
           (let ((process (sb-ext:run-program "tar" (list "xzf" (namestring tarball) "-C" dir)
                                              :search t :output nil :error nil)))
             (unless (eql 0 (sb-ext:process-exit-code process))
               (error "could not unpack ~A" asset)))
           (let ((new (merge-pathnames "atty" dir)))
             (unless (probe-file new) (error "~A holds no atty" asset))
             (sb-posix:chmod (namestring new) #o755)
             (sb-posix:rename (namestring new) (namestring me))))
      (ignore-errors (uiop:delete-directory-tree (pathname dir) :validate t)))))

(defun update-atty (&key check)
  "atty update: the latest release put where this program is, and the server
started again from it, keeping everything. With CHECK only says whether there
is one. When the program is a package manager's, says what to run instead."
  (let ((latest (or (latest-release) (error "nothing says what the latest release is"))))
    (cond ((equal latest *version*)
           (format t "~&atty ~A is the latest release~%" *version*))
          (check
           (format t "~&this is atty ~A; the latest release is ~A~%" *version* latest)
           (sb-ext:quit :unix-status 1))
          (t
           (let ((me (or (executable-path) (error "this atty is not a program to replace; it is in a lisp"))))
             (multiple-value-bind (owner how) (file-owner me)
               (unless (eq owner :atty)
                 (format t "~&atty ~A is out. ~A; then atty restart-server brings the server up to it~%"
                         latest how)
                 (sb-ext:quit :unix-status 2)))
             (install-release latest me)
             (format t "~&atty ~A → ~A" *version* latest)
             (if (server-alive-p (server-socket-path) 1)
                 (format t "; the server is back with ~D session~:P~%" (restart-server))
                 (format t "~%")))))))
