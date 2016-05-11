;;;; utils.lisp

;;;; Copyright (c) 2014--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :bit-smasher)

;; open coding allows SBCL to remove unsatisfiable branches; Also detects type mismatch in compile time
(declaim (inline byte-length wide-bit-length min-bit-length twos-complement-p))

(defun byte-length (n)
  "Return the number of bytes required to represent an integer, bit-vector, or hex-string value; or the actual length of an octet-vector, N."
  (etypecase n
    (integer
     (length (octets<- n)))
    (bit-vector
     (length (octets<- n)))
    ((or (vector (unsigned-byte 8))
         (simple-array (unsigned-byte 8) (*)))
     (length n))
    (string
     (length (octets<- n)))))

(defun wide-bit-length (n)
  "Return the maximum number of bits required to represent an integer, bit-vector, octet-vector, or hex-string value N."
  (etypecase n
    (integer
     (length (bits<- n)))
    (bit-vector
     (length (bits<- (int<- n))))
    ((or (vector (unsigned-byte 8))
         (simple-array (unsigned-byte 8) (*)))
     (length (bits<- n)))
    (string
     (length (bits<- n)))))

(defun min-bit-length (n)
  "Return the minimum number of bits required to represent an integer, bit-vector, octet-vector, or hex-string value N."
  (etypecase n
    (integer
     (integer-length n))
    (bit-vector
     (integer-length (int<- n)))
    ((or (vector (unsigned-byte 8))
         (simple-array (unsigned-byte 8) (*)))
     (integer-length (int<- n)))
    (string
     (integer-length (int<- n)))))

(defun twos-complement-p (n)
  "Test a bit-vector, octet-vector, hex-string, or non-negative integer to see if it obeys the two's complement rule."
  (= (mod (min-bit-length n) 2) 0))

;; EOF
