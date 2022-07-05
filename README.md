# BIT-SMASHER

[![Quicklisp](http://quickdocs.org/badge/bit-smasher.svg)](http://quickdocs.org/bit-smasher/)
[![MIT License](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

Common Lisp library for handling bit vectors, bit vector arithmetic, and type conversions.

Available in Quicklisp as of July 2014 release.

Documentation available at: https://thephoeron.github.io/bit-smasher/

### Supported Platforms

The current release of BIT-SMASHER *compiles without warning* and passes all
tests on 64-bit versions of the following Lisp implementations:

- LispWorks 8.0.1
- SBCL 2.2.5
- Clozure CL 1.12.1
- CLISP 2.49.92
- ABCL 1.9.0
- Allegro CL 10.1

It *compiles with style-warnings* on:

- ECL 21.2.1

It *does not build* on:

- CLASP CL 1.0.0 (clasp-boehmprecise-1.0.0-316-gf3b9992a5)
- CMUCL 21d

### Usage Notes and Limitations

This library was designed to complement the set of functions included in the
Common Lisp specification for handling bit-vectors, by adding relevant lookup,
conversion, arithmetic, measurement, and predicate functions.  For documentation
and tutorials on the bit-vector functions included in the Common Lisp standard,
please refer to:

* Common Lisp HyperSpec:
    * [accessor BIT, SBIT](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_bt_sb.htm)
    * [function BIT-AND, etc...](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_bt_and.htm)
    * [function BIT-VECTOR-P](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_bt_vec.htm)
* Successful Lisp: [Chapter 18](http://psg.com/~dlamkins/sl/chapter18.html)

BIT-SMASHER only handles the set of non-negative integers.  As such, arithmetic
on bit-vectors may not always produce the results you expect&mdash;return values
of all arithmetic functions are given as the bit-vector of the absolute ceiling
value. Manual conversion of negative integers, floats, fractions, or complex
numbers will trigger an error.

### Examples

The conversion functions allow you to convert universally between bit-vectors,
octet-vectors, hexadecimal strings, and non-negative integers.

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

Arithmetic on bit-vectors can be achieved through the functions `bit-sum`,
`bit-difference`, `bit-product`, `bit-quotient`, `bit-floor`, `bit-ceiling`,
`lshift`, and `rshift`.  There are also the shorthand macros, `bit+`, `bit-`,
`bit*`, `bit/`, `<<`, and `>>`.  As stated above, the bit-vector arithmetic
functions return the absolute ceiling value of the operation.  So,

```lisp
(bit- #*0000 #*0010) => #*00000010 ; +2, not -2
```

The measurement functions `wide-bit-length` and `min-bit-length` tell you the
maximum and minimum number of bits needed to store a value, respectively.  They
operate on bit-vectors, octet-vectors, hexadecimal strings, and non-negative
integers.

```lisp
(wide-bit-length 256) => 16
(min-bit-length 256) => 9
```

There is also the measurement function `byte-length` that returns the total
number of bytes required to store an integer, bit-vector, or hexadecimal value;
or the actual length of byte vector or simple byte array.

```lisp
(byte-length "A0FF") => 2
(byte-length 65536) => 3
```

In addition to the built-in CL predicate function, `bit-vector-p`, BIT-SMASHER
adds the predicate function `twos-complement-p`, when you need to test the
minimum bit length for the two's complement rule. This is required where padding
bit-vectors, octet-vectors, or hex-strings with leading zeros up to a set
word-length is expected.

```lisp
(twos-complement-p 256) => NIL
(twos-complement-p 255) => T
```

### License

Copyright &copy; 2014&ndash;2022, "the Phoeron" Colin J.E. Lupton and the
Contributors. This project is released under the MIT License; please see
[bit-smasher/LICENSE](./LICENSE) for more information.
