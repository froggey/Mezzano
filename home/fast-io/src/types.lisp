(in-package :fast-io)

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))

(deftype index () `(integer 0 ,array-total-size-limit))
