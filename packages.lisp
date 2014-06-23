;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BIT-SMASHER; Base: 10 -*-
;;;; packages.lisp

;;;; Copyright (c) 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage #:bit-smasher
  (:nicknames #:bitsmash)
  (:use :cl :cl-user :ironclad)
  (:shadowing-import-from :cl-user #:null)
  (:export #:*bit-smasher-version*
           #:hex->bits #:hex->octets #:hex->int
           #:octets->hex #:octets->bits #:octets->int
           #:int->hex #:int->octets #:int->bits
           #:bits->hex #:bits->octets #:bits->int
           #:bit-sum #:bit+
           #:bit-difference #:bit-
           #:bit-product #:bit*
           #:bit-quotient #:bit/
           #:bit-floor #:bit-ceiling
           #:<< #:lshift
           #:>> #:rshift
           #:wide-bit-length #:min-bit-length
           #:twos-complement-p))

(in-package :bit-smasher)

(defvar *bit-smasher-version* #.bit-smasher-asd:*bit-smasher-version*)

;; EOF
