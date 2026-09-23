(in-package #:atty/test)

(def-suite release :in all)
(in-suite release)

(test the-server-says-which-build-it-is-when-greeting-and-when-knocked
  (with-a-server-here (server path)
    (mux:add-session server "sleep 30" :name "work" :rows 6 :cols 30)
    (let ((wire (a-wire-to path)))
      (say-to wire (list :open "work" nil nil 6 30 t))
      (heard-from server wire :hello)
      (let ((heard (heard-from server wire :version)))
        (is (equal (list :version mux:*version*) heard)))
      (mux:wire-close wire))
    (let ((wire (a-wire-to path)))
      (say-to wire (list :knock))
      (let ((heard (heard-from server wire :here)))
        (is (equal mux:*version* (fourth heard)) "~S" heard))
      (mux:wire-close wire))))

(test the-platform-is-said-the-way-uname-says-it
  (let ((said (mux::platform-name)))
    (is (member said '("darwin-arm64" "darwin-x86_64" "linux-x86_64" "linux-aarch64")
                :test #'string=)
        "~S" said)))

(test the-tag-is-read-out-of-what-github-says
  (is (equal "v0.1.0" (mux::tag-in "{\"url\": \"x\", \"tag_name\": \"v0.1.0\", \"name\": \"v0.1.0\"}")))
  (is (equal "v0.2.0" (mux::tag-in (format nil "{~%  \"tag_name\":\"v0.2.0\"~%}"))))
  (is (null (mux::tag-in "{\"message\": \"Not Found\"}"))))

(test the-sum-for-a-file-is-found-in-sha256sums-in-either-spelling
  (let ((sums (format nil "aaaa  atty-v1-linux-x86_64.tar.gz~%bbbb *atty-v1-darwin-arm64.tar.gz~%")))
    (is (equal "aaaa" (mux::sum-listed sums "atty-v1-linux-x86_64.tar.gz")))
    (is (equal "bbbb" (mux::sum-listed sums "atty-v1-darwin-arm64.tar.gz")))
    (is (null (mux::sum-listed sums "atty-v2-darwin-arm64.tar.gz")))))

(test who-owns-the-program-says-who-updates-it
  (is (eq :guix (mux::owner-of "/gnu/store/abc-atty-0.1.0/bin/atty")))
  (is (eq :nix (mux::owner-of "/nix/store/abc-atty-0.1.0/bin/atty")))
  (is (eq :brew (mux::owner-of "/opt/homebrew/Cellar/atty/0.1.0/bin/atty")))
  (is (eq :distro (mux::owner-of "/usr/bin/atty-there-is-no-such")))
  (is (eq :somebody (mux::owner-of "/atty-nobody-can-write-here")))
  (let ((mine (format nil "~Aatty-owned-~D" (uiop:temporary-directory) (sb-posix:getpid))))
    (with-open-file (s mine :direction :output :if-exists :supersede) (write-string "x" s))
    (unwind-protect
         (multiple-value-bind (owner how) (mux::owner-of mine)
           (is (eq :atty owner))
           (is (equal "atty update" how)))
      (delete-file mine))))

(defun a-fake-release (dir tag text)
  "A release at DIR, as ATTY_RELEASES_URL=file://DIR serves it: latest says
TAG, and the tarball for this platform holds an atty that is TEXT."
  (let* ((asset (mux::release-asset tag))
         (down (format nil "~Adownload/~A/" dir tag))
         (unpacked (format nil "~Aunpacked/" dir)))
    (ensure-directories-exist down)
    (ensure-directories-exist unpacked)
    (with-open-file (s (format nil "~Alatest" dir) :direction :output :if-exists :supersede)
      (format s "{\"tag_name\": ~S}" tag))
    (with-open-file (s (format nil "~Aatty" unpacked) :direction :output :if-exists :supersede)
      (write-string text s))
    (sb-ext:run-program "tar" (list "czf" (format nil "~A~A" down asset) "-C" unpacked "atty")
                        :search t :output nil)
    (with-open-file (s (format nil "~ASHA256SUMS" down) :direction :output :if-exists :supersede)
      (format s "~A  ~A~%" (mux::sha256-of (format nil "~A~A" down asset)) asset))
    down))

(test a-release-is-fetched-checked-and-put-in-place-in-one-move
  (let* ((dir (format nil "~Aatty-release-~D/" (uiop:temporary-directory) (sb-posix:getpid)))
         (me (format nil "~Ame" dir))
         (down (a-fake-release dir "v9.9.9" "the new one")))
    (with-open-file (s me :direction :output :if-exists :supersede) (write-string "the old one" s))
    (sb-posix:setenv "ATTY_RELEASES_URL" (format nil "file://~A" (string-right-trim "/" dir)) 1)
    (unwind-protect
         (progn
           (is (equal "v9.9.9" (mux::latest-release)))
           (mux::install-release "v9.9.9" me)
           (is (equal "the new one" (uiop:read-file-string me)))
           (is (null (directory (format nil "~Ame.update-*" dir))) "the workshop was left behind")
           ;; a tarball that is not what the sums say leaves the old one alone
           (with-open-file (s (format nil "~ASHA256SUMS" down) :direction :output :if-exists :supersede)
             (format s "0000  ~A~%" (mux::release-asset "v9.9.9")))
           (signals error (mux::install-release "v9.9.9" me))
           (is (equal "the new one" (uiop:read-file-string me)))
           (is (null (directory (format nil "~Ame.update-*" dir)))))
      (sb-posix:unsetenv "ATTY_RELEASES_URL")
      (uiop:delete-directory-tree (pathname dir) :validate t))))
