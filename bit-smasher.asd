;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BIT-SMASHER; Base: 10 -*-
;;;; bit-smasher.asd

(in-package :cl-user)

(defpackage bit-smasher-asd
  (:use :cl :asdf)
  (:export #:*bit-smasher-version*))

(in-package :bit-smasher-asd)

(defvar *bit-smasher-version* "1.0.0")

(defsystem #:bit-smasher
  :serial t
  :description ""
  :version #.*bit-smasher-version*
  :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "MIT"
  :depends-on (:cl-base58
               :cl-base64
               :ironclad)
  :components ((:file "packages")
               (:file "bit-smasher")))
;; EOF
