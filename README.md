# BIT-SMASHER

Common Lisp library for handling bit vectors, bit vector arithmetic, and type conversions.

### Usage Notes and Limitations

This library was designed to complement the set of functions included in the Common Lisp specification for handling bit-vectors, by adding relevant lookup, conversion, arithmetic, measurement, and predicate functions.  For documentation and tutorials on the bit-vector functions included in the Common Lisp standard, please refer to:

* Common Lisp HyperSpec:
    * [accessor BIT, SBIT](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_bt_sb.htm)
    * [function BIT-AND, etc...](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_bt_and.htm)
    * [function BIT-VECTOR-P](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_bt_vec.htm)
* Successful Lisp: [Chapter 18](http://psg.com/~dlamkins/sl/chapter18.html)

BIT-SMASHER only handles the set of non-negative integers.  As such, arithmetic on bit-vectors may not always produce the results you expect---return values of all arithmetic functions are given as the absolute ceiling value in bit-vector. Manual conversion of negative integers, floats, fractions, or complex numbers will trigger an error.

### Examples

The conversion functions allow you to convert universally between bit-vectors, octet-vectors, hexadecimal strings, and non-negative integers.

```lisp
; universal type-casting style functions
(bits<- "F0") => #*11110000
(bits<- 240) => #*11110000
(int<- #*11110000) => 240

; manual conversions without type-checking
(hex->bits "F0") => #*11110000
(int->bits 10) => #*00001010
(octets->bits (int->octets 244)) => #*11110100
; etc., etc...
```

Bit-vectors are returned zero-padded to the next full byte.

```lisp
(bits<- 255) => #*11111111
(bits<- 256) => #*0000000100000000
```

Arithmetic on bit-vectors can be achieved through the functions `bit-sum`, `bit-difference`, `bit-product`, `bit-quotient`, `bit-floor`, `bit-ceiling`, `lshift`, and `rshift`.  There are also the shorthand macros, `bit+`, `bit-`, `bit*`, `bit/`, `<<`, and `>>`.  As stated above, the bit-vector arithmetic functions return the absolute ceiling value of the operation.  So,

```lisp
(bit- #*0000 #*0010) => #*00000010 ; +2, not -2
```

The measurement functions `wide-bit-length` and `min-bit-length` tell you the maximum and minimum number of bytes needed to store a value, respectively.  They operate on bit-vectors, octet-vectors, hexadecimal strings, and non-negative integers.

```lisp
(wide-bit-length 256) => 16
(min-bit-length 256) => 9
```

In addition to the built-in CL predicate function, `bit-vector-p`, BIT-SMASHER adds the predicate function `twos-complement-p`, when you need to test the minimum bit length for the two's complement rule.  This is required where padding bit-vectors, octet-vectors, or hex-strings with leading zeros up to a set word-length is expected.

```lisp
(twos-complement-p 256) => NIL
(twos-complement-p 255) => T
```

### License

This project is released under the MIT License.  Please see bit-smasher/LICENSE for more information.
