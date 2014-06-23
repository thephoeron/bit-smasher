;;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: BIT-SMASHER; Base: 10 -*-
;;;; conversion.lisp

;;;; Copyright (c) 2014 "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :bit-smasher)

(defun hex->bits (x)
  "Return the bit-vector for hexadecimal string X."
  (let ((binlist (loop for c across x collect (hex-to-bit-lookup c))))
    (apply #'concatenate 'bit-vector binlist)))

(defun hex->octets (x)
  "Return the octet-vector for hexadecimal string X."
  (ironclad:hex-string-to-byte-array x))

(defun hex->int (x)
  "Return the integer value for hexadecimal string X."
  (octets->int (hex->octets x)))

(defun octets->hex (o)
  "Return the hexadecimal string for octet-vector O."
  (ironclad:byte-array-to-hex-string o))

(defun octets->int (o)
  "Return the integer value for octet-vector O."
  (ironclad:octets-to-integer o))

(defun octets->bits (o)
  "Return the bit-vector for octet-vector O."
  (hex->bits (octets->hex o)))

(defun int->octets (n)
  "Return the octet-vector for integer N."
  (ironclad:integer-to-octets n))

(defun int->hex (n)
  "Return the hexadecimal string for integer N."
  (octets->hex (int->octets n)))

(defun int->bits (n)
  "Return the bit-vector for integer N."
  (hex->bits (int->hex n)))

(defun bits->hex (data)
  "Return the hexadecimal string for bit-vector DATA."
  (format nil "~X" (bit-array-integer-value-and-place data)))

(defun bits->int (data)
  "Return the integer value for bit-vector DATA."
  (bit-array-integer-value-and-place data))

(defun bits->octets (data order)
  "Return the octet-vector for bit-vector DATA, significant to ORDER."
  (ironclad:integer-to-octets (bits->int data) :n-bits order))

;; EOF
