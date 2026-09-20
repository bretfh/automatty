;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:term)

(defvar *color-palette* (make-array 256 :initial-element nil))

(let ((base16 #(#(24 24 24)
                #(172 66 66)
                #(144 169 89)
                #(244 191 117)
                #(106 159 181)
                #(170 117 159)
                #(117 181 170)
                #(216 216 216)
                #(107 107 107)
                #(197 85 85)
                #(170 196 116)
                #(254 202 136)
                #(130 184 200)
                #(194 140 184)
                #(147 211 195)
                #(248 248 248))))
  (dotimes (i 16)
    (setf (aref *color-palette* i) (aref base16 i))))

(let ((levels #(0 95 135 175 215 255)))
  (loop for i from 16 below 232
        for idx = (- i 16)
        for r = (aref levels (floor idx 36))
        for g = (aref levels (floor (mod idx 36) 6))
        for b = (aref levels (mod idx 6))
        do (setf (aref *color-palette* i) (vector r g b))))

(loop for i from 232 below 256
      for v = (+ 8 (* 10 (- i 232)))
      do (setf (aref *color-palette* i) (vector v v v)))

(defun color-index-to-rgb (index)
  (when (and index (<= 0 index 255))
    (aref *color-palette* index)))
