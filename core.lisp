;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BIT-SMASHER; Base: 10 -*-
;;;; core.lisp

;;;; Copyright (c) 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :bit-smasher)

(defvar *bit-map* '((#\0 . #*0000)
                    (#\1 . #*0001)
                    (#\2 . #*0010)
                    (#\3 . #*0011)
                    (#\4 . #*0100)
                    (#\5 . #*0101)
                    (#\6 . #*0110)
                    (#\7 . #*0111)
                    (#\8 . #*1000)
                    (#\9 . #*1001)
                    (#\A . #*1010)
                    (#\B . #*1011)
                    (#\C . #*1100)
                    (#\D . #*1101)
                    (#\E . #*1110)
                    (#\F . #*1111)))

(defun hex-to-bit-lookup (char)
  "Return the bit vector associated with a hex-value character CHAR from *bit-map*."
  (cdr (assoc char *bit-map* :test #'char-equal)))

;; from comp.lang.lisp
(defun bit-vector-integer-value-and-place (bit-vector)
  "Returns the bits of BIT-VECTOR as an integer as the primary value, number of bits as the secondary value."
  (let ((place -1))
    (values (reduce #'+ (reverse bit-vector) :key (lambda (digit) (ash digit (incf place))))
            (incf place))))

;; EOF
