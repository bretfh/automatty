(in-package #:libatty/test)

(def-suite decode :in emulator)
(in-suite decode)

(defun bytes-of (string)
  (map 'string #'code-char
       (sb-ext:string-to-octets string :external-format :utf-8)))

(defun decoded (&rest reads)
  (let ((d (term:make-decoder)))
    (apply #'concatenate 'string
           (mapcar (lambda (said) (term:decode-utf-8 d said)) reads))))

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
      (let ((bad (format nil "a~Cb" (code-char 255))))
        (is (equal (format nil "a~Cb" (code-char 65533)) (decoded bad))))
      (let ((cut (format nil "~C~Cx" (code-char 226) (code-char 148))))
        (is (equal (format nil "~Cx" (code-char 65533)) (decoded cut))
            "a truncated sequence ate the character after it"))
      (is (equal (format nil "~C" (code-char 65533))
                 (decoded (string (code-char 128))))
          "a continuation byte with nothing to continue"))

(test an-overlong-sequence-is-refused
      (is (equal (format nil "~C~C" (code-char 65533) (code-char 65533))
                 (decoded (map 'string #'code-char '(192 175))))
          "an overlong slash was taken as a slash"))

(test a-box-drawing-character-is-one-cell-not-three
      (let ((term (a-term :width 10 :height 1))
            (d (term:make-decoder)))
        (say term (term:decode-utf-8 d (bytes-of "├── a")))
        (is (equal "├── a" (row term 0)))
        (is (equal '(5 0) (cursor term)) "it took more columns than it draws")))
