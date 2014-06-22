;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BIT-SMASHER; Base: 10 -*-
;;;; packages.lisp

(in-package :cl-user)

(defpackage #:bit-smasher
  (:nicknames #:bitsmash)
  (:use :cl :cl-user :ironclad)
  (:shadowing-import-from :cl-user #:null)
  (:export #:*bit-smasher-version*))

(in-package :bit-smasher)

(defvar *bit-smasher-version* #.bit-smasher-asd:*bit-smasher-version*)

;; EOF
