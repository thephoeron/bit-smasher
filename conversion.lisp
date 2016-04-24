;;;; conversion.lisp

;;;; Copyright (c) 2014--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :bit-smasher)

(defun hex->bits (x)
  "Return the bit-vector for hexadecimal string X."
  (let ((binlist (loop for c across x collect (hex-to-bit-lookup/unsafe c))))
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
  (format nil "~X" (bit-vector-integer-value-and-place data)))

(defun bits->int (data)
  "Return the integer value for bit-vector DATA."
  (bit-vector-integer-value-and-place data))

(defun bits->octets (data)
  "Return the octet-vector for bit-vector DATA, significant to ORDER."
  (ironclad:integer-to-octets (bits->int data)))

(defun hex<- (data)
  (cond ((and (typep data 'integer)
              (= data 0))
         "00")
        ((typep data 'integer)
         (int->hex data))
        ((typep data 'bit-vector)
         (bits->hex data))
        ((or (typep data '(vector (unsigned-byte 8)))
             (typep data '(simple-array (unsigned-byte 8) (*))))
         (octets->hex data))
        ((typep data 'string)
         data)
        (t (error "Type of value N not recognized."))))

(defun octets<- (data)
  (cond ((and (typep data 'integer)
              (= data 0))
         (hex->octets "00"))
        ((typep data 'integer)
         (int->octets data))
        ((typep data 'bit-vector)
         (bits->octets data))
        ((or (typep data '(vector (unsigned-byte 8)))
             (typep data '(simple-array (unsigned-byte 8) (*))))
         data)
        ((typep data 'string)
         (hex->octets data))
        (t (error "Type of value N not recognized."))))

(defun int<- (data)
  (cond ((typep data 'integer)
         data)
        ((typep data 'bit-vector)
         (bits->int data))
        ((or (typep data '(vector (unsigned-byte 8)))
             (typep data '(simple-array (unsigned-byte 8) (*))))
         (octets->int data))
        ((typep data 'string)
         (hex->int data))
        (t (error "Type of value N not recognized."))))

(defun bits<- (data)
  (cond ((and (typep data 'integer)
              (= data 0))
         (hex->bits "00"))
        ((typep data 'integer)
         (int->bits data))
        ((typep data 'bit-vector)
         data)
        ((or (typep data '(vector (unsigned-byte 8)))
             (typep data '(simple-array (unsigned-byte 8) (*))))
         (octets->bits data))
        ((typep data 'string)
         (hex->bits data))
        (t (error "Type of value N not recognized."))))

;; EOF
