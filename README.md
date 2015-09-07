# BIT-SMASHER

[![Build Status](https://travis-ci.org/thephoeron/bit-smasher.svg?branch=master)](https://travis-ci.org/thephoeron/bit-smasher)
[![Coverage Status](https://coveralls.io/repos/thephoeron/bit-smasher/badge.svg?branch=master)](https://coveralls.io/r/thephoeron/bit-smasher?branch=master)
[![Quicklisp](http://quickdocs.org/badge/bit-smasher.svg)](http://quickdocs.org/bit-smasher/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)
[![Join the chat at https://gitter.im/thephoeron/bit-smasher](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/thephoeron/bit-smasher?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Common Lisp library for handling bit vectors, bit vector arithmetic, and type conversions.

Available in Quicklisp as of July 2014 release.

Documentation available at: http://thephoeron.viewdocs.io/bit-smasher/

### Supported Platforms

BIT-SMASHER is known to compile and pass all tests with 100% code coverage on the latest 64-bit versions of the following Lisp implementations:

- SBCL
- Clozure CL
- CLISP
- ECL
- ABCL
- Allegro CL

It *does not build* on:
- CMUCL

It *has not been tested* on:
- LispWorks
- Clasp
- or other available Common Lisp implementations

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

The measurement functions `wide-bit-length` and `min-bit-length` tell you the maximum and minimum number of bits needed to store a value, respectively.  They operate on bit-vectors, octet-vectors, hexadecimal strings, and non-negative integers.

```lisp
(wide-bit-length 256) => 16
(min-bit-length 256) => 9
```

There is also the measurement function `byte-length` that returns the total number of bytes required to store an integer, bit-vector, or hexadecimal value; or the actual length of byte vector or simple byte array.

```lisp
(byte-length "A0FF") => 2
(byte-length 65536) => 3
```

In addition to the built-in CL predicate function, `bit-vector-p`, BIT-SMASHER adds the predicate function `twos-complement-p`, when you need to test the minimum bit length for the two's complement rule.  This is required where padding bit-vectors, octet-vectors, or hex-strings with leading zeros up to a set word-length is expected.

```lisp
(twos-complement-p 256) => NIL
(twos-complement-p 255) => T
```

### License

Copyright &copy; 2014&ndash;2015, "the Phoeron" Colin J.E. Lupton. This project is released under the MIT License; please see `bit-smasher/LICENSE` for more information.
