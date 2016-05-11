(in-package :bit-smasher)

(declaim (inline hex-string-to-byte-array
                 byte-array-to-hex-string
                 octets-to-integer
                 integer-to-octets))

;; since these functions are inlined, optimization settings follows the one in the inlined context


(defun hex-string-to-byte-array (string &aux (start 0) (end (length string)))
  "Parses a substring of STRING delimited by START and END of
hexadecimal digits into a byte array."
  (declare (type string string))
  (let* ((length
          (ash (- end start) -1)
           #+nil (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8)) key))
    (loop for i from 0
          for j from start below end by 2
          do (setf (aref key i)
                   (+ (* (hexchar->int (char string j)) 16)
                      (hexchar->int (char string (1+ j)))))
          finally (return key))))

(defun byte-array-to-hex-string (vector)
  "Return a string containing the hexadecimal representation of the
subsequence of VECTOR between START and END.  ELEMENT-TYPE controls
the element-type of the returned string."
  (declare (type (vector (unsigned-byte 8)) vector))
  (let* ((length (length vector))
         (hexdigits #.(coerce "0123456789abcdef" 'simple-base-string)))
    (loop with string = (make-string (* length 2) :element-type 'base-char)
       for i from 0 below length
       for j from 0 by 2
       do (let ((byte (aref vector i)))
            (declare (optimize (safety 0)))
            (setf (aref string j)
                  (aref hexdigits (ldb (byte 4 4) byte))
                  (aref string (1+ j))
                  (aref hexdigits (ldb (byte 4 0) byte))))
       finally (return string))))

(defun octets-to-integer (octet-vec &aux (end (length octet-vec)))
  (declare (type (simple-array (unsigned-byte 8)) octet-vec))
  (do ((j 0 (1+ j))
       (sum 0))
      ((>= j end) sum)
    (setf sum (+ (aref octet-vec j) (ash sum 8)))))

(defun integer-to-octets (bignum &aux (n-bits (integer-length bignum)))
  (let* ((n-bytes (ceiling n-bits 8))
         (octet-vec (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8)) octet-vec))
    (loop for i from (1- n-bytes) downto 0
          for index from 0
          do (setf (aref octet-vec index) (ldb (byte 8 (* i 8)) bignum))
          finally (return octet-vec))))
