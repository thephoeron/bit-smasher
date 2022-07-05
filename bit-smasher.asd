;;;; bit-smasher.asd

;;;; Copyright (c) 2014--2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage bit-smasher/asdf
  (:use cl asdf uiop))

(in-package :bit-smasher/asdf)

(defsystem bit-smasher
  :description "Utility library for handling bit vectors, bit vector arithmetic, and universal integer type conversions between bit-vectors, byte-vectors, octals, decimals, and hexadecimal notation."
  :author "\"the Phoeron\" Colin J.E. Lupton"
  :mailto "thephoeron@protonmail.com"
  :homepage "https://thephoeron.common-lisp.dev/bit-smasher/"
  :source-control (:git "https://gitlab.common-lisp.net/thephoeron/bit-smasher/")
  :bug-tracker "https://gitlab.common-lisp.net/thephoeron/bit-smasher/-/issues/"
  :version (:from-file-form "VERSION")
  :license "MIT"
  :pathname "src/"
  :serial t
  :depends-on (cl-base58
               cl-base64)
  :components ((:file "packages")
               (:file "core")
               (:file "from-ironclad")
               (:file "conversion")
               (:file "arithmetic")
               (:file "aliases")
               (:file "utils"))
  :perform (load-op :after (op c)
             (provide :bit-smasher)
             (pushnew :bit-smasher *features*))
  :in-order-to ((test-op (test-op :bit-smasher-test))))
