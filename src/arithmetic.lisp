;;;; arithmetic.lisp

;;;; Copyright (c) 2014--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :bit-smasher)

;; from comp.lang.lisp
(defun bit-vector-integer-value-and-place (bit-vector)
  "Returns the bits of BIT-VECTOR as an integer as the primary value, number of bits as the secondary value.
SLOW!! Consult Hackers-Delight"
  (let ((place -1))
    (values (reduce #'+ bit-vector
                    :key (lambda (digit) (ash digit (incf place)))
                    :from-end t)
            (incf place))))

(defun bit-sum (&rest rest)
  "Addition for bit-vectors.  Return result SUM forced to absolute ceiling value."
  (let* ((intlist (loop for i in rest collect (int<- i)))
         (sum (apply #'+ intlist)))
    (bits<- (abs (ceiling sum)))))

(defun bit-difference (&rest rest)
  "Subtraction for bit-vectors.  Return result DIFFERENCE forced to absolute ceiling value."
  (let* ((intlist (loop for i in rest collect (int<- i)))
         (difference (apply #'- intlist)))
    (bits<- (abs (ceiling difference)))))

(defun bit-product (&rest rest)
  "Multiplication for bit-vectors.  Return result PRODUCT forced to absolute ceiling value."
  (let* ((intlist (loop for i in rest collect (int<- i)))
         (prod (apply #'* intlist)))
    (bits<- (abs (ceiling prod)))))

(defun bit-quotient (&rest rest)
  "Division for bit-vectors.  Return results QUOTIENT and REMAINDER forced to absolute ceiling values."
  (let* ((intlist (loop for i in rest collect (int<- i))))
    (multiple-value-bind (quotient remainder)
        (floor (apply #'/ intlist))
      (values (bits<- (abs (ceiling quotient))) (bits<- (abs (ceiling remainder)))))))

(defun bit-floor (&rest rest)
  "Floor division for bit-vectors.  Return result FLOOR forced to absolute ceiling value."
  (let* ((intlist (loop for i in rest collect (int<- i)))
         (the-floor (floor (apply #'/ intlist))))
    (bits<- (abs (ceiling the-floor)))))

(defun bit-ceiling (&rest rest)
  "Ceiling division for bit-vectors.  Return result CEILING forced to absolute ceiling value."
  (let* ((intlist (loop for i in rest collect (int<- i)))
         (the-ceiling (ceiling (apply #'/ intlist))))
    (bits<- (abs (ceiling the-ceiling)))))

(defun lshift (n count)
  "Return a bit vector of N left-shifted by COUNT. N may be an integer, bit-vector, octet-vector, or hex-string."
  (etypecase n
    (integer
     (bits<- (ash n count)))
    (bit-vector
     (bits<- (ash (int<- n) count)))
    ((or (vector (unsigned-byte 8))
         (simple-array (unsigned-byte 8) (*)))
     (bits<- (ash (int<- n) count)))
    (string
     (bits<- (ash (int<- n) count)))))

(defun rshift (n count)
  "Return a bit vector of N right-shifted by COUNT. N may be an integer, bit-vector, octet-vector, or hex-string."
  (etypecase n
    (integer
     (bits<- (ash n (- count))))
    (bit-vector
     (bits<- (ash (int<- n) (- count))))
    ((or (vector (unsigned-byte 8))
         (simple-array (unsigned-byte 8) (*)))
     (bits<- (ash (int<- n) (- count))))
    (string
     (bits<- (ash (int<- n) (- count))))))

