
(in-package :bit-smasher)

(setf (fdefinition 'bit+) (fdefinition 'bit-sum))

(setf (fdefinition 'bit-) (fdefinition 'bit-difference))

(setf (fdefinition 'bit*) (fdefinition 'bit-product))

(setf (fdefinition 'bit/) (fdefinition 'bit-quotient))

(setf (fdefinition '<<) (fdefinition 'lshift))

(setf (fdefinition '>>) (fdefinition 'rshift))
