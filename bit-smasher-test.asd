;;;; file: bit-smasher-test.asd

(in-package :cl-user)

(defpackage bit-smasher-test-asd
  (:use :cl :asdf))

(in-package :bit-smasher-test-asd)

(defsystem #:bit-smasher-test
  :serial t
  :version #.bit-smasher-asd:*bit-smasher-version*
  :description "The test code for BIT-SMASHER."
  :author "\"the Phoeron\" Colin J.E. Lupton <sysop@thephoeron.com>"
  :license "MIT"
  :depends-on (#:bit-smasher
               #:prove)
  :components ((:module "t"
                :components
                ((:test-file "bit-smasher"))))
  :defsystem-depends-on (prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))

