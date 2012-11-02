(defpackage #:system
  (:export #:lambda-name
           #:io-port/8
           #:io-port/16
           #:io-port/32
           #:fixnump
           #:char-bits))

(declaim (declaration system:lambda-name))

(defpackage #:system.internals
  (:nicknames #:sys.int)
  (:use #:cl #:system)
  (:shadow #:proclaim
           #:get-setf-expansion
           #:macroexpand
           #:macroexpand-1)
  (:export #:proclaim
           #:get-setf-expansion
           #:macroexpand
           #:macroexpand-1))
