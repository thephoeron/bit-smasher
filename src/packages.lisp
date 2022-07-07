;;;; src/packages.lisp

;;;; Copyright (c) 2014--2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage #:bit-smasher
  (:nicknames #:bitsmash)
  (:use :cl :cl-user)
  (:shadowing-import-from :cl-user #:null)
  (:export #:hex<- #:hex->bits #:hex->octets #:hex->int
           #:octets<- #:octets->hex #:octets->bits #:octets->int
           #:int<- #:int->hex #:int->octets #:int->bits
           #:bits<- #:bits->hex #:bits->octets #:bits->int
           #:bit-sum #:bit+
           #:bit-difference #:bit-
           #:bit-product #:bit*
           #:bit-quotient #:bit/
           #:bit-floor #:bit-ceiling
           #:<< #:lshift
           #:>> #:rshift
           #:byte-length #:wide-bit-length #:min-bit-length
           #:twos-complement-p))
