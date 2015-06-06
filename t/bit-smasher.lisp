;;;; file: t/bit-smasher.lisp

(in-package :cl-user)

(defpackage bit-smasher-test
  (:use cl bit-smasher prove))

(in-package :bit-smasher-test)

;; NOTE: To run this test file, execute `(asdf:test-system :bit-smasher)' in your Lisp.

(plan 1)

(deftest sanity-check
  (is (+ 1 1)
      2
      "Sane Lisp system."))

(run-test-all)

;; EOF
