;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defpackage :system.closette
  (:nicknames :sys.clos :clos)
  (:use :cl)
  (:import-from :sys.int
		:allocate-std-instance
		:std-instance-p
		:std-instance-class
		:std-instance-slots
		:std-instance-layout
                :allocate-funcallable-std-instance
                :funcallable-std-instance-p
                :funcallable-std-instance-function
                :funcallable-std-instance-class
                :funcallable-std-instance-slots
                :funcallable-std-instance-layout))

(in-package #:system.closette)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defvar exports
        '(defclass defgeneric defmethod
          find-class class-of
          call-next-method next-method-p
          slot-value slot-boundp slot-exists-p slot-makunbound
          make-instance change-class
          initialize-instance reinitialize-instance shared-initialize
          update-instance-for-different-class
          update-instance-for-redefined-class
          print-object
          set-funcallable-instance-function

          standard-object funcallable-standard-object
          standard-class funcallable-standard-class
          standard-generic-function standard-method
          standard-slot-definition
          class-name

          class-direct-superclasses class-direct-slots
          class-precedence-list class-slots class-direct-subclasses
          class-direct-methods
          generic-function-name generic-function-lambda-list
          generic-function-methods generic-function-discriminating-function
          generic-function-method-class
          method-lambda-list method-qualifiers method-specializers
          method-generic-function method-function
          slot-definition-name slot-definition-initfunction
          slot-definition-initform slot-definition-initargs
          slot-definition-readers slot-definition-writers
          slot-definition-allocation
          ;;
          ;; Class-related metaobject protocol
          ;;
          compute-class-precedence-list compute-slots
          compute-effective-slot-definition
          finalize-inheritance allocate-instance
          slot-value-using-class slot-boundp-using-class
          slot-exists-p-using-class slot-makunbound-using-class
          ;;
          ;; Generic function related metaobject protocol
          ;;
          compute-discriminating-function
          compute-applicable-methods-using-classes method-more-specific-p
          compute-applicable-methods
          compute-effective-method-function compute-method-function
          apply-methods apply-method

          metaobject specializer class
          structure-class structure-object
          intern-eql-specializer eql-specializer eql-specializer-object

          slot-unbound

          with-slots with-accessors
          ))

(export exports)

)
