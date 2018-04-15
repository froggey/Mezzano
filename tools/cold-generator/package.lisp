;;;; Copyright (c) 2011-2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Cold generator packages.

(defpackage :cold-generator
  (:use :cl :iterate :nibbles)
  (:export #:make-image
           #:allocate
           #:word
           #:array-header
           #:make-value
           #:function-reference
           #:symbol-address
           #:cold-symbol-value
           #:compile-lap-function
           #:set-up-cross-compiler))

(defpackage :cold-generator.x86-64
  (:use :cl :cold-generator)
  (:export #:*undefined-function-thunk*
           #:*closure-trampoline*
           #:*funcallable-instance-trampoline*
           #:create-low-level-interrupt-support))

(defpackage :cold-generator.arm64
  (:use :cl :cold-generator)
  (:export #:*undefined-function-thunk*
           #:*closure-trampoline*
           #:*funcallable-instance-trampoline*))

(defpackage :build-pci-ids
  (:export :build-pci-ids)
  (:use :cl :iter))

(defpackage :build-unicode
  (:export :decode-glyph :generate-unifont-table
	   :read-unicode-data :generate-unicode-data-tables)
  (:use :cl))
