(in-package :bit-smasher)

(defun hex-string-to-byte-array (string &aux (start 0) (end (length string)))
  "Parses a substring of STRING delimited by START and END of
hexadecimal digits into a byte array."
  (declare (type string string))
  (declare (optimize (speed 3) (safety 1)))
  (let* ((length
          (ash (- end start) -1)
           #+nil (/ (- end start) 2))
         (key (make-array length :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) key))
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
  (declare (type (vector (unsigned-byte 8)) vector)
           (optimize (speed 3) (safety 1)))
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

(defun octets-to-integer (octet-vec &aux (start 0) (end (length octet-vec)) (big-endian t) n-bits)
  (declare (type (simple-array (unsigned-byte 8) (*)) octet-vec))
  (let ()
    (multiple-value-bind (complete-bytes extra-bits)
        (if n-bits
            (truncate n-bits 8)
            (values (- end start) 0))
      (declare (ignorable complete-bytes extra-bits))
      (if big-endian
          (do ((j start (1+ j))
               (sum 0))
              ((>= j end) sum)
            (setf sum (+ (aref octet-vec j) (ash sum 8))))
          (loop for i from (- end start 1) downto 0
                for j from (1- end) downto start
                sum (ash (aref octet-vec j) (* i 8)))))))

(defun integer-to-octets (bignum &aux
                                   (n-bits (integer-length bignum))
                                   (big-endian t))
  (let* ((n-bytes (ceiling n-bits 8))
         (octet-vec (make-array n-bytes :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) octet-vec))
    (if big-endian
        (loop for i from (1- n-bytes) downto 0
              for index from 0
              do (setf (aref octet-vec index) (ldb (byte 8 (* i 8)) bignum))
              finally (return octet-vec))
        (loop for i from 0 below n-bytes
              for byte from 0 by 8
              do (setf (aref octet-vec i) (ldb (byte 8 byte) bignum))
              finally (return octet-vec)))))
