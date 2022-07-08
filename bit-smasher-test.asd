;;;; file: bit-smasher-test.asd

;;;; Copyright (c) 2014--2022, "the Phoeron" Colin J.E. Lupton <thephoeron@protonmail.com>
;;;; See LICENSE for additional information.

(in-package :cl-user)

(defpackage bit-smasher/test-asdf
  (:use cl asdf uiop))

(in-package :bit-smasher/test-asdf)

(defsystem bit-smasher-test
  :description "The test code for BIT-SMASHER."
  :author "\"the Phoeron\" Colin J.E. Lupton"
  :mailto "thephoeron@protonmail.com"
  :homepage "https://thephoeron.common-lisp.dev/bit-smasher/"
  :source-control (:git "https://gitlab.common-lisp.net/thephoeron/bit-smasher/")
  :bug-tracker "https://gitlab.common-lisp.net/thephoeron/bit-smasher/-/issues/"
  :version (:read-file-form "VERSION")
  :license "MIT"
  :depends-on (bit-smasher
               prove)
  :serial t
  :components ((:module "t"
                :components
                ((:test-file "bit-smasher"))))
  :defsystem-depends-on (prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
