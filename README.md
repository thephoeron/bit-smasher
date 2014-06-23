# BIT-SMASHER

Common Lisp library for handling bit vectors, bit vector arithmetic, and type conversions.

## Usage Notes and Limitations

This library was designed to complement the set of functions included in the Common Lisp specification for handling bit-vectors, by adding relevant lookup, conversion, arithmetic, measurement, and predicate functions.  For documentation and tutorials on the bit-vector functions included in the Common Lisp standard, please refer to:

* Common Lisp HyperSpec:
    * [accessor BIT, SBIT](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_bt_sb.htm)
    * [function BIT-AND, etc...](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_bt_and.htm)
    * [function BIT-VECTOR-P](http://www.lispworks.com/documentation/lw50/CLHS/Body/f_bt_vec.htm)
* Successful Lisp: [Chapter 18](http://psg.com/~dlamkins/sl/chapter18.html)

BIT-SMASHER only handles the set of non-negative integers.  As such, arithmetic on bit-vectors may not always produce the results you expect---return values of all arithmetic functions are given as the absolute ceiling value in bit-vector. Manual conversion of negative integers, floats, fractions, or complex numbers will trigger an error.

### License

This project is released under the MIT License.  Please see bit-smasher/LICENSE for more information.
