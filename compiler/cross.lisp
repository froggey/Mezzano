(defpackage :cross-cl
  (:use :cl)
  (:shadow :proclaim
           :get-setf-expansion
           :macroexpand
           :macroexpand-1
           :compiler-macro-function
           :macro-function
           :most-positive-fixnum
           :most-negative-fixnum
           :lambda-list-keywords
           :*features*
           :compile
           :*macroexpand-hook*
           :constantp
           :array-rank-limit
           :array-dimension-limit
           :array-total-size-limit
           :char-code-limit
           :call-arguments-limit
           :lambda-parameters-limit
           :multiple-values-limit
           :most-negative-short-float
           :most-negative-single-float
           :most-negative-double-float
           :most-negative-long-float
           :most-positive-short-float
           :most-positive-single-float
           :most-positive-double-float
           :most-positive-long-float
           :boole-1
           :boole-2
           :boole-andc1
           :boole-andc2
           :boole-and
           :boole-c1
           :boole-c2
           :boole-clr
           :boole-eqv
           :boole-ior
           :boole-nand
           :boole-nor
           :boole-orc1
           :boole-orc2
           :boole-set
           :boole-xor
           :internal-time-units-per-second
           :pi)
  (:export . #.(let ((symbols '()))
                 (do-external-symbols (sym :cl symbols)
                   (push sym symbols)))))

(defpackage :system.compiler
  (:nicknames :sys.c)
  (:export :compile
           :compiler-macro-function
           :*macroexpand-hook*
           :macroexpand
           :macroexpand-1
           :macro-function
           :constantp)
  (:use :cross-cl))

(defpackage :sys.newc
  (:use :cross-cl))

(in-package :system.compiler)

(defpackage :system
  (:export :lambda-name
           :io-port/8
           :io-port/16
           :io-port/32
           :fixnump
           :char-bits
           :symbol-mode))

(declaim (declaration system:lambda-name))

(defpackage :system.internals
  (:nicknames :sys.int)
  (:use :cross-cl :system)
  (:export :allocate-std-instance
           :std-instance-p
           :std-instance-class
           :std-instance-slots
           :allocate-funcallable-std-instance
           :funcallable-std-instance-p
           :funcallable-std-instance-function
           :funcallable-std-instance-class
           :funcallable-std-instance-slots
           :class-precedence-list
           :funcallable-standard-object))

(defpackage :system.closette
  (:nicknames :sys.clos)
  (:import-from :sys.int
                :class-precedence-list
                :funcallable-standard-object))
