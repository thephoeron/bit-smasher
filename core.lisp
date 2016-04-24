;;;; core.lisp

;;;; Copyright (c) 2014--2015, "the Phoeron" Colin J.E. Lupton <//thephoeron.com>
;;;; See LICENSE for additional information.

(in-package :bit-smasher)

(defvar *bit-map* #(#*0000
                    #*0001
                    #*0010
                    #*0011
                    #*0100
                    #*0101
                    #*0110
                    #*0111
                    #*1000
                    #*1001
                    #*1010
                    #*1011
                    #*1100
                    #*1101
                    #*1110
                    #*1111))

(defun hex-to-bit-lookup/unsafe (char)
  "Return the bit vector associated with a hex-value character CHAR from *bit-map*."
  (cond ((char<= #\0 char #\9)
         (aref *bit-map* (- (char-code char) #.(char-code #\0))))
        ((char<= #\a char #\f)
         (aref *bit-map* (+ 10 (- (char-code char) #.(char-code #\a)))))
        ((char<= #\A char #\F)
         (aref *bit-map* (+ 10 (- (char-code char) #.(char-code #\A)))))))

(defun hex-to-bit-lookup (char)
  "Return the bit vector associated with a hex-value character CHAR from *bit-map*."
  (copy-seq (hex-to-bit-lookup/unsafe char)))

;; from comp.lang.lisp
(defun bit-vector-integer-value-and-place (bit-vector)
  "Returns the bits of BIT-VECTOR as an integer as the primary value, number of bits as the secondary value."
  (let ((place -1))
    (values (reduce #'+ (reverse bit-vector) :key (lambda (digit) (ash digit (incf place))))
            (incf place))))

;; EOF
