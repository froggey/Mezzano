(defpackage #:cross-cl
  (:use #:cl)
  (:shadow #:proclaim
           #:get-setf-expansion
           #:macroexpand
           #:macroexpand-1
           #:compiler-macro-function
           #:macro-function
           #:most-positive-fixnum
           #:most-negative-fixnum
           #:lambda-list-keywords
           #:*features*
           #:compile
           #:*macroexpand-hook*
           #:constantp
           #:array-rank-limit
           #:array-dimension-limit
           #:array-total-size-limit
           #:char-code-limit)
  (:export . #.(let ((symbols '()))
                 (do-external-symbols (sym :cl symbols)
                   (push sym symbols)))))

(defpackage #:system.compiler
  (:nicknames #:sys.c)
  (:export #:compile
           #:compiler-macro-function
           #:*macroexpand-hook*
           #:macroexpand
           #:macroexpand-1
           #:macro-function
           #:constantp)
  (:use #:cross-cl))

(defpackage #:sys.newc
  (:use #:cross-cl))

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
  (:use #:cross-cl #:system)
  (:export #:allocate-std-instance
           #:std-instance-p
           #:std-instance-class
           #:std-instance-slots
           #:allocate-funcallable-std-instance
           #:funcallable-std-instance-p
           #:funcallable-std-instance-function
           #:funcallable-std-instance-class
           #:funcallable-std-instance-slots
           #:class-precedence-list
           #:funcallable-standard-object))

(defpackage #:system.closette
  (:nicknames #:sys.clos)
  (:import-from #:sys.int
                #:class-precedence-list
                #:funcallable-standard-object))
