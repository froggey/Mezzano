(defpackage #:system.compiler
  (:nicknames #:sys.c)
  (:shadow #:compile
           #:compiler-macro-function
           #:*macroexpand-hook*
           #:macroexpand
           #:macroexpand-1
           #:macro-function
           #:constantp)
  (:use #:cl))

(in-package #:system.compiler)

(defpackage #:system
  (:export #:lambda-name
           #:io-port/8
           #:io-port/16
           #:io-port/32
           #:fixnump
           #:char-bits
           #:symbol-mode))

(declaim (declaration system:lambda-name))

(defpackage #:system.internals
  (:nicknames #:sys.int)
  (:use #:cl #:system)
  (:shadow #:proclaim
           #:get-setf-expansion
           #:macroexpand
           #:macroexpand-1
           #:compiler-macro-function
           #:macro-function
           #:most-positive-fixnum
           #:most-negative-fixnum)
  (:export #:proclaim
           #:get-setf-expansion
           #:macroexpand
           #:macroexpand-1
           #:compiler-macro-function
           #:macro-function
           #:most-positive-fixnum
           #:most-negative-fixnum))

(defpackage #:system.clos)
