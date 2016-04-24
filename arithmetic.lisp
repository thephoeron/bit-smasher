;;;; arithmetic.lisp

;;;; Copyright (c) 2014--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :bit-smasher)

(defun bit-sum (&rest rest)
  "Addition for bit-vectors.  Return result SUM forced to absolute ceiling value."
  (let* ((intlist (loop for i in rest collect (int<- i)))
         (sum (apply #'+ intlist)))
    (bits<- (abs (ceiling sum)))))

(defmacro bit+ (&rest rest)
  "Shorthand for BIT-SUM function."
  `(bit-sum ,@rest))

(defun bit-difference (&rest rest)
  "Subtraction for bit-vectors.  Return result DIFFERENCE forced to absolute ceiling value."
  (let* ((intlist (loop for i in rest collect (int<- i)))
         (difference (apply #'- intlist)))
    (bits<- (abs (ceiling difference)))))

(defmacro bit- (&rest rest)
  "Shorthand for BIT-DIFFERENCE function."
  `(bit-difference ,@rest))

(defun bit-product (&rest rest)
  "Multiplication for bit-vectors.  Return result PRODUCT forced to absolute ceiling value."
  (let* ((intlist (loop for i in rest collect (int<- i)))
         (prod (apply #'* intlist)))
    (bits<- (abs (ceiling prod)))))

(defmacro bit* (&rest rest)
  "Shorthand for BIT-PRODUCT function."
  `(bit-product ,@rest))

(defun bit-quotient (&rest rest)
  "Division for bit-vectors.  Return results QUOTIENT and REMAINDER forced to absolute ceiling values."
  (let* ((intlist (loop for i in rest collect (int<- i))))
    (multiple-value-bind (quotient remainder)
        (floor (apply #'/ intlist))
      (values (bits<- (abs (ceiling quotient))) (bits<- (abs (ceiling remainder)))))))

(defmacro bit/ (&rest rest)
  "Shorthand for BIT-QUOTIENT function."
  `(bit-quotient ,@rest))

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
  (cond ((typep n 'integer)
         (bits<- (ash n count)))
        ((typep n 'bit-vector)
         (bits<- (ash (int<- n) count)))
        ((or (typep n '(vector (unsigned-byte 8)))
             (typep n '(simple-array (unsigned-byte 8) (*))))
         (bits<- (ash (int<- n) count)))
        ((typep n 'string)
         (bits<- (ash (int<- n) count)))
        (t (error "Type of value N not recognized."))))

(defmacro << (n count)
  "Shorthand for bit-shift function LSHIFT."
  `(lshift ,n ,count))

(defun rshift (n count)
  "Return a bit vector of N right-shifted by COUNT. N may be an integer, bit-vector, octet-vector, or hex-string."
  (cond ((typep n 'integer)
         (bits<- (ash n (- count))))
        ((typep n 'bit-vector)
         (bits<- (ash (int<- n) (- count))))
        ((or (typep n '(vector (unsigned-byte 8)))
             (typep n '(simple-array (unsigned-byte 8) (*))))
         (bits<- (ash (int<- n) (- count))))
        ((typep n 'string)
         (bits<- (ash (int<- n) (- count))))
        (t (error "Type of value N not recognized."))))

(defmacro >> (n count)
  "Shorthand for bit-shift function RSHIFT."
  `(rshift ,n ,count))

;; EOF
