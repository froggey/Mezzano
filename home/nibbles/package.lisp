(cl:defpackage :nibbles
  (:use :cl)
  ;; Basic types and constructors.
  (:export #:octet #:index
           #:octet-vector #:simple-octet-vector
           #:make-octet-vector)
  ;; Basic octet vector accessors.
  (:export #:ub16ref/le #:ub16ref/be #:sb16ref/le #:sb16ref/be
           #:ub32ref/le #:ub32ref/be #:sb32ref/le #:sb32ref/be
           #:ub64ref/le #:ub64ref/be #:sb64ref/le #:sb64ref/be)
  ;; Stream readers.
  (:export #:read-ub16/le #:read-ub16/be #:read-sb16/be #:read-sb16/le
           #:read-ub32/le #:read-ub32/be #:read-sb32/be #:read-sb32/le
           #:read-ub64/le #:read-ub64/be #:read-sb64/be #:read-sb64/le)
  ;; Stream readers for vectors.
  (:export #:read-ub16/le-sequence #:read-ub16/be-sequence
           #:read-sb16/le-sequence #:read-sb16/be-sequence
           #:read-ub32/le-sequence #:read-ub32/be-sequence
           #:read-sb32/le-sequence #:read-sb32/be-sequence
           #:read-ub64/le-sequence #:read-ub64/be-sequence
           #:read-sb64/le-sequence #:read-sb64/be-sequence)
  ;; Non-consing variants akin to READ-SEQUENCE.
  (:export #:read-ub16/le-into-sequence #:read-ub16/be-into-sequence
           #:read-sb16/le-into-sequence #:read-sb16/be-into-sequence
           #:read-ub32/le-into-sequence #:read-ub32/be-into-sequence
           #:read-sb32/le-into-sequence #:read-sb32/be-into-sequence
           #:read-ub64/le-into-sequence #:read-ub64/be-into-sequence
           #:read-sb64/le-into-sequence #:read-sb64/be-into-sequence)
  ;; Stream writers.
  (:export #:write-ub16/le #:write-ub16/be #:write-sb16/be #:write-sb16/le
           #:write-ub32/le #:write-ub32/be #:write-sb32/be #:write-sb32/le
           #:write-ub64/le #:write-ub64/be #:write-sb64/be #:write-sb64/le)
  ;; Stream writers for vectors.
  (:export #:write-ub16/le-sequence #:write-ub16/be-sequence
           #:write-sb16/le-sequence #:write-sb16/be-sequence
           #:write-ub32/le-sequence #:write-ub32/be-sequence
           #:write-sb32/le-sequence #:write-sb32/be-sequence
           #:write-ub64/le-sequence #:write-ub64/be-sequence
           #:write-sb64/le-sequence #:write-sb64/be-sequence)
  ;; The following floating-point functions are not supported on all platforms.
  ;; Floating-point octet vector accessors.
  (:export #:ieee-single-ref/be #:ieee-single-ref/le
           #:ieee-double-ref/be #:ieee-double-ref/le)
  ;; Floating-point stream readers.
  (:export #:read-ieee-single/be #:read-ieee-single/le
	   #:read-ieee-double/be #:read-ieee-double/le)
  ;; Stream readers for floating-point sequences.
  (:export #:read-ieee-single/be-sequence #:read-ieee-single/le-sequence
	   #:read-ieee-double/be-sequence #:read-ieee-double/le-sequence)
  ;; Non-consing variants akin to READ-SEQUENCE.
  (:export #:read-ieee-single/be-into-sequence #:read-ieee-single/le-into-sequence
	   #:read-ieee-double/be-into-sequence #:read-ieee-double/le-into-sequence)
  ;; Stream writers.
  (:export #:write-ieee-single/be #:write-ieee-single/le
	   #:write-ieee-double/be #:write-ieee-double/le)
  ;; Stream writers for sequences.
  (:export #:write-ieee-single/be-sequence #:write-ieee-single/le-sequence
	   #:write-ieee-double/be-sequence #:write-ieee-double/le-sequence))
