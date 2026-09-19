;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(in-package #:vt/pty)

(error "vt/pty is written against SBCL: it opens a pseudo-terminal and starts a
program on it through sb-alien, sb-unix and sb-thread, and there is no portable
way to do either. vt itself needs none of that and loads anywhere.")
