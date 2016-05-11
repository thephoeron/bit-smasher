;;;; bit-smasher.asd

;;;; Copyright (c) 2014--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage bit-smasher-asd
  (:use :cl :asdf)
  (:export #:*bit-smasher-version*))

(in-package :bit-smasher-asd)

(defparameter *bit-smasher-version* "1.0.2")

(defsystem #:bit-smasher
  :serial t
  :description "Utility library for handling bit vectors, bit vector arithmetic, and universal integer type conversions between bit-vectors, byte-vectors, octals, decimals, and hexadecimal notation."
  :version #.*bit-smasher-version*
  :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "MIT"
  :pathname "src/"
  :depends-on (:cl-base58
               :cl-base64
               :ironclad)
  :components ((:file "packages")
               (:file "core")
               (:file "from-ironclad")
               (:file "conversion")
               (:file "arithmetic")
               (:file "aliases")
               (:file "utils"))
  :in-order-to ((test-op (test-op :bit-smasher-test))))
