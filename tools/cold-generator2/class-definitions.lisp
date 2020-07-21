;;;; CLOS bootstrapping.
;;;;
;;;; This file defines the standard class hierarchy.
;;;; It must only contain DEFCLASS forms.
;;;;
;;;; It is read in the MEZZANO.CLOS package.

(defclass t () ()
  (:metaclass built-in-class))

(defclass standard-object (t) ())

(defclass funcallable-standard-object (standard-object function) ()
  (:metaclass funcallable-standard-class))

(defclass metaobject (standard-object) ())

(defclass generic-function (metaobject funcallable-standard-object) ()
  (:metaclass funcallable-standard-class))

(defclass standard-generic-function (generic-function)
  ((name :initarg :name)
   (lambda-list :initarg :lambda-list)
   (methods :initform ())
   (method-class :initarg :method-class)
   (classes-to-emf-table :initform nil)
   (relevant-arguments)
   (method-combination :initarg :method-combination)
   (argument-precedence-order :initarg :argument-precedence-order)
   (argument-reordering-table :initform nil)
   ;; ### AMOP says :DECLARATIONS, spec says :DECLARE?
   (declarations :initarg :declarations :initarg :declare :initform nil)
   (documentation :initform nil :initarg :documentation)
   (dependents :initform '())
   (source-location :initform nil :initarg :source-location)
   (tracep :initform nil))
  (:default-initargs
   :name nil
   :lambda-list '()
   ;; The cold-generator can't deal with a call to FIND-CLASS here, use
   ;; a direct reference instead.
   :method-class *the-class-standard-method*
   :method-combination nil
   :argument-precedence-order '())
  (:metaclass funcallable-standard-class))

(defclass method (metaobject) ())

(defclass standard-method (method)
  ((lambda-list :initarg :lambda-list)
   (qualifiers :initarg :qualifiers)
   (specializers :initarg :specializers)
   (generic-function :initform nil)
   (function :initarg :function)
   (fast-function :initarg :fast-function)
   (documentation :initform nil :initarg :documentation))
  (:default-initargs
   :qualifiers '()
    :specializers '()))

(defclass standard-accessor-method (standard-method)
  ((slot-definition :initarg :slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())
(defclass standard-writer-method (standard-accessor-method) ())

(defclass method-combination (metaobject) ())

(defclass slot-definition (metaobject) ())

(defclass direct-slot-definition (slot-definition) ())

(defclass effective-slot-definition (slot-definition) ())

(defclass standard-slot-definition (slot-definition)
  ((name :initform nil :initarg :name)
   (allocation :initform :instance :initarg :allocation)
   (type :initform 't :initarg :type)
   (initform :initform nil :initarg :initform)
   (initfunction :initform nil :initarg :initfunction)
   (initargs :initform '() :initarg :initargs)
   (documentation :initform nil :initarg :documentation)))

(defclass standard-direct-slot-definition (standard-slot-definition direct-slot-definition)
  ((readers :initform '() :initarg :readers)
   (writers :initform '() :initarg :writers)))

(defclass standard-effective-slot-definition (standard-slot-definition effective-slot-definition)
  ((location :initarg :location)
   (typecheck :initform nil :initarg :typecheck)))

(defclass specializer (metaobject)
  ((direct-methods :initform ())))

(defclass class (specializer)
  ((direct-subclasses :initform nil)
   (source-location :initform nil :initarg :source-location)))

(defclass forward-referenced-class (class)
  ((name :initarg :name)))

(defclass clos-class (class)
  ((name :initarg :name)
   (direct-superclasses :initarg :direct-superclasses)
   (direct-slots :initform ())
   (precedence-list)
   (effective-slots :initform ())
   (slot-storage-layout :initform ())
   (direct-default-initargs :initform ())
   (hash)
   (finalized-p :initform nil)
   (prototype)
   (default-initargs)
   (dependents :initform '())
   (sealed :initform nil)
   (allocation-area :initform nil)
   (constructor :initform nil)
   (documentation :initform nil :initarg :documentation))
  (:default-initargs :name nil))

(defclass built-in-class (clos-class) ())

(defclass instance-class (clos-class) ())

(defclass std-class (instance-class) ())
(defclass standard-class (std-class) ())
(defclass funcallable-standard-class (std-class) ())

(defclass structure-class (instance-class)
  ((parent)
   (has-standard-constructor))
  (:area :wired)
  (:sealed t))

(defclass structure-slot-definition (slot-definition)
  ((name :initform nil :initarg :name)
   (type :initform 't :initarg :type)
   (read-only :initform nil :initarg :read-only)
   (initform :initform nil :initarg :initform)
   (fixed-vector :initform nil :initarg :fixed-vector)
   (align :initform nil :initarg :align)
   (dcas-sibling :initform nil :initarg :dcas-sibling)
   (documentation :initform nil :initarg :documentation)))

(defclass structure-direct-slot-definition (structure-slot-definition direct-slot-definition)
  ())

(defclass structure-effective-slot-definition (structure-slot-definition effective-slot-definition)
  ((location :initarg :location)))

(defclass structure-object (t) () (:metaclass structure-class))

(defclass function (t) () (:metaclass built-in-class))
(defclass compiled-function (function) () (:metaclass built-in-class))
(defclass sys.int::closure (compiled-function) () (:metaclass built-in-class))
(defclass mezzano.delimited-continuations:delimited-continuation (function) () (:metaclass built-in-class))
(defclass symbol (t) () (:metaclass built-in-class))
(defclass character (t) () (:metaclass built-in-class))
(defclass sys.int::function-reference (t) () (:metaclass built-in-class))
(defclass mezzano.runtime::symbol-value-cell (t) () (:metaclass built-in-class))
(defclass sys.int::weak-pointer (t) () (:metaclass built-in-class))
(defclass sys.int::weak-pointer-vector (t) () (:metaclass built-in-class))
;; FIXME: This doesn't quite work with large bytes.
(defclass byte (t) () (:metaclass built-in-class))
(defclass sys.int::instance-header (t) () (:metaclass built-in-class))
(defclass sys.int::interrupt-frame (t) () (:metaclass built-in-class))

(defclass sequence (t) () (:metaclass built-in-class))
(defclass list (sequence) () (:metaclass built-in-class))
(defclass null (symbol list) () (:metaclass built-in-class))
(defclass cons (list) () (:metaclass built-in-class))

(defclass array (t) () (:metaclass built-in-class))
(defclass simple-array (array) () (:metaclass built-in-class))
(defclass vector (array sequence) () (:metaclass built-in-class))
(defclass simple-vector (vector simple-array) () (:metaclass built-in-class))
(defclass bit-vector (vector) () (:metaclass built-in-class))
(defclass simple-bit-vector (bit-vector simple-array) () (:metaclass built-in-class))
(defclass string (vector) () (:metaclass built-in-class))
(defclass simple-string (string simple-array) () (:metaclass built-in-class))
;; Classes for the +OBJECT-TAG-ARRAY-foo+ types.
(defclass sys.int::simple-array-fixnum (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-unsigned-byte-2 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-unsigned-byte-4 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-unsigned-byte-8 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-unsigned-byte-16 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-unsigned-byte-32 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-unsigned-byte-64 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-signed-byte-1 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-signed-byte-2 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-signed-byte-4 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-signed-byte-8 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-signed-byte-16 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-signed-byte-32 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-signed-byte-64 (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-short-float (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-single-float (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-double-float (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-complex-short-float (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-complex-single-float (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-complex-double-float (vector simple-array) () (:metaclass built-in-class))

(defclass number (t) () (:metaclass built-in-class))
(defclass real (number) () (:metaclass built-in-class))
(defclass rational (real) () (:metaclass built-in-class))
(defclass integer (rational) () (:metaclass built-in-class))
(defclass fixnum (integer) () (:metaclass built-in-class))
(defclass bignum (integer) () (:metaclass built-in-class))
(defclass ratio (rational) () (:metaclass built-in-class))
(defclass float (real) () (:metaclass built-in-class))
(defclass short-float (float) () (:metaclass built-in-class))
(defclass single-float (float) () (:metaclass built-in-class))
(defclass double-float (float) () (:metaclass built-in-class))
(defclass complex (number) () (:metaclass built-in-class))
(defclass sys.int::complex-rational (complex) () (:metaclass built-in-class))
(defclass sys.int::complex-short-float (complex) () (:metaclass built-in-class))
(defclass sys.int::complex-single-float (complex) () (:metaclass built-in-class))
(defclass sys.int::complex-double-float (complex) () (:metaclass built-in-class))

(defclass mezzano.simd:mmx-vector (t) () (:metaclass built-in-class))
(defclass mezzano.simd:sse-vector (t) () (:metaclass built-in-class))
