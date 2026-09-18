(in-package #:vt/test)

(def-suite decode :in all)
(in-suite decode)

(defun bytes-of (string)
  (map 'string #'code-char
       (sb-ext:string-to-octets string :external-format :utf-8)))

(defun decoded (&rest reads)
  (let ((d (vt:make-decoder)))
    (apply #'concatenate 'string
           (mapcar (lambda (said) (vt:decode-utf-8 d said)) reads))))

(test what-was-written-in-utf-8-is-read-back-as-what-it-says
  (dolist (said '("plain ascii" "héllo" "├── tree" "漢字" "a→b" "🭰 and 😀"))
    (is (equal said (decoded (bytes-of said)))
        "~S came back as ~S" said (decoded (bytes-of said)))))

(test a-character-cut-in-half-by-a-read-is-finished-by-the-next-one
  (let ((bytes (bytes-of "├──")))
    (dotimes (n (length bytes))
      (is (equal "├──" (decoded (subseq bytes 0 n) (subseq bytes n)))
          "cut after ~D bytes came back as ~S" n
          (decoded (subseq bytes 0 n) (subseq bytes n))))))

(test a-character-delivered-a-byte-at-a-time-is-one-character
  (let* ((bytes (bytes-of "漢字"))
         (reads (loop for i below (length bytes) collect (string (char bytes i)))))
    (is (equal "漢字" (apply #'decoded reads)))))

(test what-is-not-utf-8-is-one-character-that-says-so-and-does-not-swallow-the-next
  (let ((bad (format nil "a~Cb" (code-char #xFF))))
    (is (equal (format nil "a~Cb" (code-char #xFFFD)) (decoded bad))))
  (let ((cut (format nil "~C~Cx" (code-char #xE2) (code-char #x94))))
    (is (equal (format nil "~Cx" (code-char #xFFFD)) (decoded cut))
        "a truncated sequence ate the character after it"))
  (is (equal (format nil "~C" (code-char #xFFFD))
             (decoded (string (code-char #x80))))
      "a continuation byte with nothing to continue"))

(test an-overlong-sequence-is-refused
  (is (equal (format nil "~C~C" (code-char #xFFFD) (code-char #xFFFD))
             (decoded (map 'string #'code-char '(#xC0 #xAF))))
      "an overlong slash was taken as a slash"))

(test a-box-drawing-character-is-one-cell-not-three
  (let ((term (a-term :width 10 :height 1))
        (d (vt:make-decoder)))
    (say term (vt:decode-utf-8 d (bytes-of "├── a")))
    (is (equal "├── a" (row term 0)))
    (is (equal '(5 0) (cursor term)) "it took more columns than it draws")))
