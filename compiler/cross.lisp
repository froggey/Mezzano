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
