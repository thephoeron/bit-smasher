;;;; file: t/bit-smasher.lisp

(in-package :cl-user)

(defpackage bit-smasher-test
  (:use cl bit-smasher prove))

(in-package :bit-smasher-test)

;; NOTE: To run this test file, execute `(asdf:test-system :bit-smasher)' in your Lisp.

(plan 2)

(deftest sanity-check
  (pass "PROVE is loaded and ready to go.")
  (ok (= 1 1)
      "Numeric equality: (= 1 1) => T.")
  (is (+ 1 1)
      2
      "Addition: (+ 1 1) => 2.")
  (is (* 2 2)
      4
      "Multiplication: (* 2 2) => 4."))

;; Two's Complement Test

(deftest twos-complement
  (ok (twos-complement-p 0)
      "0 is Two's Complement.")
  (ok (not (twos-complement-p 1))
      "1 is not Two's Complement.")
  (ok (twos-complement-p 2)
      "2 is Two's Complement."))

(run-test-all)

;; EOF
