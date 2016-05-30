;;;
;;; Closette Version 1.0 (February 10, 1991)
;;;
;;; Minor revisions of September 27, 1991 by desRivieres@parc.xerox.com:
;;;   - remove spurious "&key" in header of initialize-instance method
;;;     for standard-class (bottom of pg.310 of AMOP)
;;;   - add recommendation about not compiling this file
;;;   - change comment to reflect PARC ftp server name change
;;;   - add a BOA constructor to std-instance to please AKCL
;;;   - by default, efunctuate methods rather than compile them
;;;   - also make minor changes to newcl.lisp
;;;
;;; Copyright (c) 1990, 1991 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;;
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;;
;;;
;;; Closette is an implementation of a subset of CLOS with a metaobject
;;; protocol as described in "The Art of The Metaobject Protocol",
;;; MIT Press, 1991.
;;;
;;; This program is available by anonymous FTP, from the /pub/pcl/mop
;;; directory on parcftp.xerox.com.

;;; This is the file closette.lisp

(in-package :system.closette)

;;;
;;; Standard instances
;;;

(defun print-std-instance (instance stream depth)
  (declare (ignore depth))
  (print-object instance stream))

;;; Standard instance allocation

(defparameter *secret-unbound-value* (list "slot unbound"))

(defun instance-slot-p (slot)
  (eq (slot-definition-allocation slot) ':instance))

(defun std-allocate-instance (class)
  (allocate-std-instance
    class
    (allocate-slot-storage (count-if #'instance-slot-p (class-slots class))
                           *secret-unbound-value*)
    (class-slot-storage-layout class)))

(defun fc-std-allocate-instance (class)
  (allocate-funcallable-std-instance
   (lambda (&rest x)
     (declare (ignore x))
     (error "The function of this funcallable instance has not been set."))
    class
    (allocate-slot-storage (count-if #'instance-slot-p (class-slots class))
                           *secret-unbound-value*)
    (class-slot-storage-layout class)))

(defun set-funcallable-instance-function (funcallable-instance function)
  (setf (funcallable-std-instance-function funcallable-instance) function))

;;; Simple vectors are used for slot storage.

(defun allocate-slot-storage (size initial-value)
  (make-array size :initial-element initial-value))

;;; Standard instance slot access

;;; N.B. The location of the effective-slots slots in the class metaobject for
;;; standard-class must be determined without making any further slot
;;; references.

(defvar *the-class-standard-class*)    ;standard-class's class metaobject
(defvar *the-class-funcallable-standard-class*)
(defvar *standard-class-effective-slots-position*) ; Position of the effective-slots slot in standard-class.
(defvar *standard-class-slot-storage-layout-position*)
(defvar *standard-class-hash-position*)

(defun slot-location (class slot-name)
  (if (and (eq slot-name 'effective-slots)
           (eq class *the-class-standard-class*))
      *standard-class-effective-slots-position*
      (let ((slot (find slot-name
                        (class-slots class)
                        :key #'slot-definition-name)))
        (when (null slot)
          (error "The slot ~S is missing from the class ~S."
                 slot-name class))
        (case (slot-definition-allocation slot)
          (:instance
           (position slot (remove-if-not #'instance-slot-p
                                         (class-slots class))))
          (:class
           (slot-definition-location slot))
          (t
           (error "The slot ~S is not an instance or class~@
                           slot in the class ~S."
                          slot-name class))))))

(defun slot-contents (slots location)
  (if (consp location)
      (cdr location)
      (svref slots location)))

(defun (setf slot-contents) (new-value slots location)
  (if (consp location)
      (setf (cdr location) new-value)
      (setf (svref slots location) new-value)))

(defun fast-sv-position (value simple-vector)
  (dotimes (i (array-dimension simple-vector 0))
    (when (eq value (svref simple-vector i))
      (return i))))

(defun fast-slot-read (instance location)
  (multiple-value-bind (slots layout)
      (fetch-up-to-date-instance-slots-and-layout instance)
    (declare (ignore layout))
    (let* ((val (slot-contents slots location)))
      (if (eq *secret-unbound-value* val)
          (slot-unbound (class-of instance)
                        instance
                        (slot-definition-name (elt (class-slots (class-of instance)) location)))
          val))))

(defun fast-slot-write (new-value instance location)
  (multiple-value-bind (slots layout)
      (fetch-up-to-date-instance-slots-and-layout instance)
    (declare (ignore layout))
    (setf (slot-contents slots location) new-value)))

(defun fetch-up-to-date-instance-slots-and-layout (instance)
  (loop
     (let* ((storage-layout (slot-contents (std-instance-slots (class-of instance))
                                           *standard-class-slot-storage-layout-position*))
            (funcallable-instance-p (funcallable-std-instance-p instance))
            (instance-layout (if funcallable-instance-p
                                 (funcallable-std-instance-layout instance)
                                 (std-instance-layout instance)))
            (slots (if funcallable-instance-p
                       (funcallable-std-instance-slots instance)
                       (std-instance-slots instance))))
       (when (eql storage-layout instance-layout)
         (return (values slots instance-layout)))
       (update-instance-for-new-layout instance))))

(defun slot-location-in-instance (instance slot-name)
  (when (not (symbolp slot-name))
    (setf slot-name (slot-definition-name slot-name)))
  (multiple-value-bind (slots layout)
      (fetch-up-to-date-instance-slots-and-layout instance)
    (let ((location (fast-sv-position slot-name layout)))
      (cond (location
             (values slots location))
            (t
             ;; Unknown slot, fall back on SLOT-LOCATION.
             (values slots
                     (slot-location (class-of instance) slot-name)))))))

(defun std-slot-value (instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (let ((val (slot-contents slots location)))
      (if (eq *secret-unbound-value* val)
          (slot-unbound (class-of instance) instance slot-name)
          val))))
(defun slot-value (object slot-name)
  (cond ((standardish-class-p (class-of (class-of object)))
         (std-slot-value object slot-name))
        (t (slot-value-using-class (class-of object) object slot-name))))

(defun (setf std-slot-value) (value instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (setf (slot-contents slots location) value)))
(defun (setf slot-value) (new-value object slot-name)
  (cond ((standardish-class-p (class-of (class-of object)))
         (setf (std-slot-value object slot-name) new-value))
        (t (setf-slot-value-using-class
            new-value (class-of object) object slot-name))))

(defun std-slot-boundp (instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (not (eq *secret-unbound-value* (slot-contents slots location)))))
(defun slot-boundp (object slot-name)
  (let ((metaclass (class-of (class-of object))))
    (cond ((standardish-class-p metaclass)
           (std-slot-boundp object slot-name))
          (t (slot-boundp-using-class (class-of object) object slot-name)))))

(defun std-slot-makunbound (instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (setf (slot-contents slots location) *secret-unbound-value*))
  instance)
(defun slot-makunbound (object slot-name)
  (let ((metaclass (class-of (class-of object))))
    (cond ((standardish-class-p metaclass)
           (std-slot-makunbound object slot-name))
          (t (slot-makunbound-using-class (class-of object) object slot-name)))))

(defun std-slot-exists-p (instance slot-name)
  (when (not (symbolp slot-name))
    (setf slot-name (slot-definition-name slot-name)))
  (not (null (find slot-name (class-slots (class-of instance))
                   :key #'slot-definition-name))))
(defun slot-exists-p (object slot-name)
  (let ((metaclass (class-of (class-of object))))
    (cond ((standardish-class-p metaclass)
           (std-slot-exists-p object slot-name))
          (t (slot-exists-p-using-class (class-of object) object slot-name)))))

;;; class-of

(defun class-of (x)
  (cond ((std-instance-p x)
         (std-instance-class x))
        ((funcallable-std-instance-p x)
         (funcallable-std-instance-class x))
        (t (built-in-class-of x))))

;;; N.B. This version of built-in-class-of is straightforward but very slow.
#+nil
(defun built-in-class-of (x)
  (typecase x
    (null                                          (find-class 'null))
    ((and symbol (not null))                       (find-class 'symbol))
    ((complex *)                                   (find-class 'complex))
    ((integer * *)                                 (find-class 'integer))
    ((float * *)                                   (find-class 'float))
    (cons                                          (find-class 'cons))
    (character                                     (find-class 'character))
    (hash-table                                    (find-class 'hash-table))
    (package                                       (find-class 'package))
    (pathname                                      (find-class 'pathname))
    (readtable                                     (find-class 'readtable))
    (stream                                        (find-class 'stream))
    ((and number (not (or integer complex float))) (find-class 'number))
    ((string *)                                    (find-class 'string))
    ((bit-vector *)                                (find-class 'bit-vector))
    ((and (vector * *) (not (or string vector)))   (find-class 'vector))
    ((and (array * *) (not vector))                (find-class 'array))
    ((and sequence (not (or vector list)))         (find-class 'sequence))
    (function                                      (find-class 'function))
    (t                                             (find-class 't))))

(defun canonicalize-struct-slot (slot)
  (list :name (sys.int::structure-slot-name slot)
        :accessor-name (sys.int::structure-slot-accessor slot)
        :type (sys.int::structure-slot-type slot)
        :read-only-p (sys.int::structure-slot-read-only slot)))

(defun canonicalize-struct-slots (struct-def)
  (mapcar 'canonicalize-struct-slot (sys.int::structure-slots struct-def)))

(defun make-structure-class (struct-def)
  (make-instance 'structure-class
                 :name (sys.int::structure-name struct-def)
                 :definition struct-def
                 :direct-slots (canonicalize-struct-slots struct-def)
                 :direct-superclasses (list (if (sys.int::structure-parent struct-def)
                                                (class-of-structure-definition (sys.int::structure-parent struct-def))
                                                (find-class 'structure-object)))))

(defun class-of-structure-definition (struct-def)
  (let* ((class (sys.int::structure-definition-class struct-def)))
    ;; Lazily create classes for structs.
    (when (null class)
      (setf class (make-structure-class struct-def)
            (sys.int::structure-definition-class struct-def) class))
    class))

(defmacro find-class-cached (class)
  `(let ((cache (load-time-value (cons nil nil))))
     (let ((value (car cache)))
       (when (not value)
         (setf value (find-class ,class)
               (car cache) value))
       value)))

(defun built-in-class-of (x)
  (typecase x
    (null                                          (find-class-cached 'null))
    ;;((and symbol (not null))                       (find-class-cached 'symbol))
    (symbol                                        (find-class-cached 'symbol))
    ;;((complex *)                                   (find-class-cached 'complex))
    ;;((integer * *)                                 (find-class-cached 'integer))
    (integer                                       (find-class-cached 'integer))
    ;;((float * *)                                   (find-class-cached 'float))
    (float                                         (find-class-cached 'float))
    (complex                                       (find-class-cached 'complex))
    ((satisfies sys.int::ratiop)                   (find-class-cached 'ratio))
    (cons                                          (find-class-cached 'cons))
    (character                                     (find-class-cached 'character))
    ;;((and number (not (or integer complex float))) (find-class-cached 'number))
    ;;((string *)                                    (find-class-cached 'string))
    (simple-string                                 (find-class-cached 'simple-string))
    (string                                        (find-class-cached 'string))
    ;;((bit-vector *)                                (find-class-cached 'bit-vector))
    (bit-vector                                    (find-class-cached 'bit-vector))
    ;;((and (vector * *) (not (or string vector)))   (find-class-cached 'vector))
    (simple-vector                                 (find-class-cached 'simple-vector))
    (vector                                        (find-class-cached 'vector))
    (array                                         (find-class-cached 'array))
    ;;((and sequence (not (or vector list)))         (find-class-cached 'sequence))
    (function                                      (find-class-cached 'function))
    (mezzano.supervisor:thread                     (find-class-cached 'mezzano.supervisor:thread))
    (sys.int::function-reference                   (find-class-cached 'sys.int::function-reference))
    (sys.int::weak-pointer                         (find-class-cached 'sys.int::weak-pointer))
    (byte                                          (find-class-cached 'byte))
    (structure-object
     (class-of-structure-definition (sys.int::%struct-slot x 0)))
    (t                                             (find-class-cached 't))))

;;; subclassp and sub-specializer-p

(defun subclassp (c1 c2)
  (not (null (find c2 (class-precedence-list c1)))))

(defun sub-specializer-p (c1 c2 c-arg)
  (let ((cpl (class-precedence-list c-arg)))
    (not (null (find c2 (cdr (member c1 cpl)))))))

;;;
;;; Class metaobjects and standard-class
;;;

;; Bootstrapping assumes that only CLOS-CLASS has any slots.
;; STANDARD-CLASS/FUNCALLABLE-STANDARD-CLASS and the superclasses must all be
;; slot free.

(defparameter *the-defclass-clos-class*
  '(defclass clos-class (class)
    ((name :initarg :name)              ; :accessor class-name
     (direct-superclasses               ; :accessor class-direct-superclasses
      :initarg :direct-superclasses)
     (direct-slots)                     ; :accessor class-direct-slots
     (class-precedence-list)            ; :accessor class-precedence-list
     (effective-slots)                  ; :accessor class-slots
     (slot-storage-layout :initform ()) ; :accessor class-slot-storage-layout
     (direct-subclasses :initform ())   ; :accessor class-direct-subclasses
     (direct-methods :initform ())      ; :accessor class-direct-methods
     (direct-default-initargs :initform ()) ; :accessor class-direct-default-initargs
     (dependents :initform '())
     (hash :initform (next-class-hash-value)))
    (:default-initargs :name nil)))

;; STANDARD-CLASS and FUNCALLABLE-STANDARD-CLASS must have the same layout.

;;; Defining the metaobject slot accessor function as regular functions
;;; greatly simplifies the implementation without removing functionality.

(defun class-name (class)
  (std-slot-value class 'name))
(defun (setf class-name) (new-value class)
  (setf (slot-value class 'name) new-value))

(defun class-direct-superclasses (class)
  (slot-value class 'direct-superclasses))
(defun (setf class-direct-superclasses) (new-value class)
  (setf (slot-value class 'direct-superclasses) new-value))

(defun class-direct-slots (class)
  (slot-value class 'direct-slots))
(defun (setf class-direct-slots) (new-value class)
  (setf (slot-value class 'direct-slots) new-value))

(defun class-precedence-list (class)
  (slot-value class 'class-precedence-list))
(defun (setf class-precedence-list) (new-value class)
  (setf (slot-value class 'class-precedence-list) new-value))

(defun class-slots (class)
  (slot-value class 'effective-slots))
(defun (setf class-slots) (new-value class)
  (setf (slot-value class 'effective-slots) new-value))

(defun class-slot-storage-layout (class)
  (slot-value class 'slot-storage-layout))
(defun (setf class-slot-storage-layout) (new-value class)
  (setf (slot-value class 'slot-storage-layout) new-value))

(defun class-direct-subclasses (class)
  (slot-value class 'direct-subclasses))
(defun (setf class-direct-subclasses) (new-value class)
  (setf (slot-value class 'direct-subclasses) new-value))

(defun class-direct-methods (class)
  (slot-value class 'direct-methods))
(defun (setf class-direct-methods) (new-value class)
  (setf (slot-value class 'direct-methods) new-value))

(defun class-direct-default-initargs (class)
  (slot-value class 'direct-default-initargs))
(defun (setf class-direct-default-initargs) (new-value class)
  (setf (slot-value class 'direct-default-initargs) new-value))

(defun class-dependents (class)
  (std-slot-value class 'dependents))
(defun (setf class-dependents) (new-value class)
  (setf (slot-value class 'dependents) new-value))

(defun class-hash (class)
  (let ((class-of-class (class-of class)))
    (cond ((standardish-class-p class-of-class)
           (svref (std-instance-slots class) *standard-class-hash-position*))
          (t (std-slot-value class 'hash)))))
(defun (setf class-hash) (new-value class)
  (setf (slot-value class 'hash) new-value))

;;; find-class

(defvar *class-table* (make-hash-table :test #'eq))

(defun find-class (symbol &optional (errorp t) environment)
  (let ((class (gethash symbol *class-table* nil)))
    (when (not class)
      (let ((struct (get symbol 'sys.int::structure-type)))
        (when struct
          (setf class (class-of-structure-definition struct)))))
    (if (and (null class) errorp)
        (error "No class named ~S." symbol)
        class)))

(defun (setf find-class) (new-value symbol &optional (errorp t) environment)
  (setf (gethash symbol *class-table*) new-value))

(defun forget-all-classes ()
  (clrhash *class-table*)
  (values))

(defvar *next-class-hash-value* 1)

(defun next-class-hash-value ()
  (sys.int::%atomic-fixnum-add-object '*next-class-hash-value*
                                      sys.int::+symbol-value+
                                      1))

;;; Ensure class

(defun find-class-forward-ref (name)
  "Find a class named NAME, or create and install a new FORWARD-REFERENCED-CLASS if no class with that name exists."
  (cond ((find-class name nil))
        (t
         (setf (find-class name) (make-instance 'forward-referenced-class
                                                :name name)))))

(defun compute-class-initialization-arguments (all-keys)
  "Convert a list of keyword arguments passed to ENSURE-CLASS or ENSURE-CLASS-USING-CLASS to an argument list suitable for initializing a class metaobject.
The metaclass argument is removed, and the direct-superclass argument is converted to a list of class metaobjects.
Other arguments are included directly."
  (loop
     for (key value) on all-keys by #'cddr
     append (case key
              (:direct-superclasses
               `(:direct-superclasses
                 ,(loop
                     for sc in value
                     collect (if (symbolp sc)
                                 (find-class-forward-ref sc)
                                 sc))))
              (:metaclass
               '())
              (t
               `(,key ,value)))))

(defun ensure-class (name &rest all-keys
                          &key
                            (metaclass *the-class-standard-class*)
                            direct-superclasses
                            direct-default-initargs
                          &allow-other-keys)
  (when (symbolp metaclass)
    (setf metaclass (find-class metaclass)))
  (let ((existing (find-class name nil)))
    (cond ((and (not existing)
                (standardish-class-p metaclass))
           ;; Non-MOP path. Used when creating a new standard (funcallable) class.
           ;; Avoids generic function dispatch.
           (let ((class (apply #'make-instance-clos-class
                               metaclass
                               :name name
                               (compute-class-initialization-arguments all-keys))))
             (setf (find-class name) class)
             class))
          (t
           ;; MOP path.
           (apply #'ensure-class-using-class
                  existing
                  name
                  all-keys)))))

;;; make-instance-standard-class creates and initializes an instance of
;;; standard-class without falling into method lookup.  However, it cannot be
;;; called until standard-class itself exists.

(defun default-direct-superclasses (metaclass)
  (cond ((eql metaclass *the-class-standard-class*)
         (list (find-class 'standard-object)))
        ((eql metaclass *the-class-funcallable-standard-class*)
         (list (find-class 'funcallable-standard-object)))
        (t (error "Unsupported metaclass ~S." metaclass))))

(defun standardish-class-p (metaclass)
  "Returns true if METACLASS is either STANDARD-CLASS or FUNCALLABLE-STANDARD-CLASS."
  (or (eql metaclass *the-class-standard-class*)
      (eql metaclass *the-class-funcallable-standard-class*)))

(defun make-instance-clos-class
       (metaclass &key name direct-superclasses direct-slots
        direct-default-initargs
        &allow-other-keys)
  (let ((class (std-allocate-instance metaclass)))
    (setf (class-name class) name)
    (setf (class-direct-subclasses class) ())
    (setf (class-direct-methods class) ())
    (setf (class-dependents class) ())
    (setf (class-hash class) (next-class-hash-value))
    (std-after-initialization-for-classes class
       :direct-slots direct-slots
       :direct-superclasses (or direct-superclasses
                                (default-direct-superclasses metaclass))
       :direct-default-initargs direct-default-initargs)
    class))

(defun std-after-initialization-for-classes
       (class &key direct-superclasses direct-slots direct-default-initargs &allow-other-keys)
  (let ((supers direct-superclasses))
    (setf (class-direct-superclasses class) supers)
    (dolist (superclass supers)
      (push class (class-direct-subclasses superclass))))
  (let ((slots
          (mapcar #'(lambda (slot-properties)
                      (apply #'make-direct-slot-definition
                             (if (standard-slot-definition-p slot-properties)
                                 (standard-slot-definition-properties slot-properties)
                                 slot-properties)))
                    direct-slots)))
    (setf (class-direct-slots class) slots)
    (dolist (direct-slot slots)
      (dolist (reader (slot-definition-readers direct-slot))
        (add-reader-method
          class reader (slot-definition-name direct-slot)))
      (dolist (writer (slot-definition-writers direct-slot))
        (add-writer-method
          class writer (slot-definition-name direct-slot)))))
  (setf (class-direct-default-initargs class) direct-default-initargs)
  (funcall (if (standardish-class-p (class-of class))
               #'std-finalize-inheritance
               #'finalize-inheritance)
           class)
  (values))

;;; Slot definition metaobjects

;; Simple wrapper over closette's plist implementation just to give them a
;; dispatchable type.
(defstruct standard-slot-definition
  properties)

;;; N.B. Quietly retain all unknown slot options (rather than signaling an
;;; error), so that it's easy to add new ones.

(defun make-direct-slot-definition
       (&rest properties
        &key name (initargs ()) (initform nil) (initfunction nil)
             (readers ()) (writers ()) (allocation :instance)
        &allow-other-keys)
  (let ((slot (copy-list properties))) ; Don't want to side effect &rest list
    (setf (getf* slot ':name) name)
    (setf (getf* slot ':initargs) initargs)
    (setf (getf* slot ':initform) initform)
    (setf (getf* slot ':initfunction) initfunction)
    (setf (getf* slot ':readers) readers)
    (setf (getf* slot ':writers) writers)
    (setf (getf* slot ':allocation) allocation)
    (when (eql allocation :class)
      (setf (getf* slot ':location) (cons name *secret-unbound-value*)))
    (make-standard-slot-definition :properties slot)))

(defun make-effective-slot-definition
       (&rest properties
        &key name (initargs ()) (initform nil) (initfunction nil)
             (allocation :instance)
        &allow-other-keys)
  (let ((slot (copy-list properties)))  ; Don't want to side effect &rest list
    (setf (getf* slot ':name) name)
    (setf (getf* slot ':initargs) initargs)
    (setf (getf* slot ':initform) initform)
    (setf (getf* slot ':initfunction) initfunction)
    (setf (getf* slot ':allocation) allocation)
    (make-standard-slot-definition :properties slot)))

(defun slot-definition-name (slot)
  (getf (standard-slot-definition-properties slot) ':name))
(defun (setf slot-definition-name) (new-value slot)
  (setf (getf* (standard-slot-definition-properties slot) ':name) new-value))

(defun slot-definition-initfunction (slot)
  (getf (standard-slot-definition-properties slot) ':initfunction))
(defun (setf slot-definition-initfunction) (new-value slot)
  (setf (getf* (standard-slot-definition-properties slot) ':initfunction) new-value))

(defun slot-definition-initform (slot)
  (getf (standard-slot-definition-properties slot) ':initform))
(defun (setf slot-definition-initform) (new-value slot)
  (setf (getf* (standard-slot-definition-properties slot) ':initform) new-value))

(defun slot-definition-initargs (slot)
  (getf (standard-slot-definition-properties slot) ':initargs))
(defun (setf slot-definition-initargs) (new-value slot)
  (setf (getf* (standard-slot-definition-properties slot) ':initargs) new-value))

(defun slot-definition-readers (slot)
  (getf (standard-slot-definition-properties slot) ':readers))
(defun (setf slot-definition-readers) (new-value slot)
  (setf (getf* (standard-slot-definition-properties slot) ':readers) new-value))

(defun slot-definition-writers (slot)
  (getf (standard-slot-definition-properties slot) ':writers))
(defun (setf slot-definition-writers) (new-value slot)
  (setf (getf* (standard-slot-definition-properties slot) ':writers) new-value))

(defun slot-definition-allocation (slot)
  (getf (standard-slot-definition-properties slot) ':allocation))
(defun (setf slot-definition-allocation) (new-value slot)
  (setf (getf* (standard-slot-definition-properties slot) ':allocation) new-value))

(defun slot-definition-location (slot)
  (getf (standard-slot-definition-properties slot) ':location))
(defun (setf slot-definition-location) (new-value slot)
  (setf (getf* (standard-slot-definition-properties slot) ':location) new-value))

;;; finalize-inheritance

(defun std-finalize-inheritance (class)
  (setf (class-precedence-list class)
        (funcall (if (standardish-class-p (class-of class))
                     #'std-compute-class-precedence-list
                     #'compute-class-precedence-list)
                 class))
  (setf (class-slots class)
        (funcall (if (standardish-class-p (class-of class))
                     #'std-compute-slots
                     #'compute-slots)
                 class))
  (let ((instance-slots (remove-if-not 'instance-slot-p
                                       (class-slots class))))
    (setf (class-slot-storage-layout class)
          (make-array (length instance-slots)
                      :initial-contents (mapcar 'slot-definition-name instance-slots))))
  (values))

;;; Class precedence lists

(defun std-compute-class-precedence-list (class)
  (let ((classes-to-order (collect-superclasses* class)))
    (topological-sort classes-to-order
                      (remove-duplicates
                        (mapappend #'local-precedence-ordering
                                   classes-to-order))
                      #'std-tie-breaker-rule)))

;;; topological-sort implements the standard algorithm for topologically
;;; sorting an arbitrary set of elements while honoring the precedence
;;; constraints given by a set of (X,Y) pairs that indicate that element
;;; X must precede element Y.  The tie-breaker procedure is called when it
;;; is necessary to choose from multiple minimal elements; both a list of
;;; candidates and the ordering so far are provided as arguments.

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ()))
    (do () (nil)
      (let ((minimal-elements
	     (remove-if
	      #'(lambda (class)
		  (member class remaining-constraints
			  :key #'cadr))
	      remaining-elements)))
	(when (null minimal-elements)
	  (if (null remaining-elements)
	      (return-from topological-sort result)
	      (error "Inconsistent precedence graph.")))
	(let ((choice (if (null (cdr minimal-elements))
			  (car minimal-elements)
			  (funcall tie-breaker
				   minimal-elements
				   result))))
	  (setq result (append result (list choice)))
	  (setq remaining-elements
		(remove choice remaining-elements))
	  (setq remaining-constraints
		(remove choice
			remaining-constraints
			:test #'member)))))))

;;; In the event of a tie while topologically sorting class precedence lists,
;;; the CLOS Specification says to "select the one that has a direct subclass
;;; rightmost in the class precedence list computed so far."  The same result
;;; is obtained by inspecting the partially constructed class precedence list
;;; from right to left, looking for the first minimal element to show up among
;;; the direct superclasses of the class precedence list constituent.
;;; (There's a lemma that shows that this rule yields a unique result.)

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (class-direct-superclasses cpl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

;;; This version of collect-superclasses* isn't bothered by cycles in the class
;;; hierarchy, which sometimes happen by accident.

(defun collect-superclasses* (class)
  (labels ((all-superclasses-loop (seen superclasses)
              (let ((to-be-processed
                       (set-difference superclasses seen)))
                (if (null to-be-processed)
                    superclasses
                    (let ((class-to-process
                             (car to-be-processed)))
                      (all-superclasses-loop
                        (cons class-to-process seen)
                        (union (class-direct-superclasses
                                 class-to-process)
                               superclasses)))))))
    (all-superclasses-loop () (list class))))

;;; The local precedence ordering of a class C with direct superclasses C_1,
;;; C_2, ..., C_n is the set ((C C_1) (C_1 C_2) ...(C_n-1 C_n)).

(defun local-precedence-ordering (class)
  (mapcar #'list
          (cons class
                (butlast (class-direct-superclasses class)))
          (class-direct-superclasses class)))

;;; Slot inheritance

(defun std-compute-slots (class)
  (let* ((all-slots (mapappend #'class-direct-slots
                               (class-precedence-list class)))
         (all-names (remove-duplicates
                      (mapcar #'slot-definition-name all-slots))))
    (mapcar #'(lambda (name)
                (funcall
                  (if (standardish-class-p (class-of class))
                      #'std-compute-effective-slot-definition
                      #'compute-effective-slot-definition)
                  class
                  (remove name all-slots
                          :key #'slot-definition-name
                          :test-not #'eq)))
            all-names)))

(defun std-compute-effective-slot-definition (class direct-slots)
  (declare (ignore class))
  (let ((initer (find-if-not #'null direct-slots
                             :key #'slot-definition-initfunction)))
    (make-effective-slot-definition
      :name (slot-definition-name (car direct-slots))
      :initform (if initer
                    (slot-definition-initform initer)
                    nil)
      :initfunction (if initer
                        (slot-definition-initfunction initer)
                        nil)
      :initargs (remove-duplicates
                  (mapappend #'slot-definition-initargs
                             direct-slots))
      :allocation (slot-definition-allocation (car direct-slots))
      :location (if (eql (slot-definition-allocation (car direct-slots)) :class)
                    (slot-definition-location (car direct-slots))
                    nil))))

;;;
;;; Generic function metaobjects and standard-generic-function
;;;

(defvar *the-class-standard-gf*) ;standard-generic-function's class metaobject

(defun generic-function-name (gf)
  (slot-value gf 'name))
(defun (setf generic-function-name) (new-value gf)
  (setf (slot-value gf 'name) new-value))

(defun generic-function-lambda-list (gf)
  (slot-value gf 'lambda-list))
(defun (setf generic-function-lambda-list) (new-value gf)
  (setf (slot-value gf 'lambda-list) new-value))

(defun generic-function-methods (gf)
  (slot-value gf 'methods))
(defun (setf generic-function-methods) (new-value gf)
  (setf (slot-value gf 'methods) new-value))

(defun generic-function-discriminating-function (gf)
  (slot-value gf 'discriminating-function))
(defun (setf generic-function-discriminating-function) (new-value gf)
  (setf (slot-value gf 'discriminating-function) new-value))

(defun generic-function-method-class (gf)
  (slot-value gf 'method-class))
(defun (setf generic-function-method-class) (new-value gf)
  (setf (slot-value gf 'method-class) new-value))

(defun generic-function-relevant-arguments (gf)
  (slot-value gf 'relevant-arguments))
(defun (setf generic-function-relevant-arguments) (new-value gf)
  (setf (slot-value gf 'relevant-arguments) new-value))

(defun generic-function-has-unusual-specializers (gf)
  (slot-value gf 'weird-specializers-p))
(defun (setf generic-function-has-unusual-specializers) (new-value gf)
  (setf (slot-value gf 'weird-specializers-p) new-value))

(defun generic-function-method-combination (gf)
  (slot-value gf 'method-combination))
(defun (setf generic-function-method-combination) (new-value gf)
  (setf (slot-value gf 'method-combination) new-value))

;;; Internal accessor for effective method function table

(defun classes-to-emf-table (gf)
  (slot-value gf 'classes-to-emf-table))
(defun (setf classes-to-emf-table) (new-value gf)
  (setf (slot-value gf 'classes-to-emf-table) new-value))

;;;
;;; Method metaobjects and standard-method
;;;

(defvar *the-class-standard-method*)    ;standard-method's class metaobject

(defun method-lambda-list (method) (slot-value method 'lambda-list))
(defun (setf method-lambda-list) (new-value method)
  (setf (slot-value method 'lambda-list) new-value))

(defun method-qualifiers (method) (slot-value method 'qualifiers))
(defun (setf method-qualifiers) (new-value method)
  (setf (slot-value method 'qualifiers) new-value))

(defun method-specializers (method) (slot-value method 'specializers))
(defun (setf method-specializers) (new-value method)
  (setf (slot-value method 'specializers) new-value))

(defun method-generic-function (method)
  (slot-value method 'generic-function))
(defun (setf method-generic-function) (new-value method)
  (setf (slot-value method 'generic-function) new-value))

(defun method-function (method) (slot-value method 'function))
(defun (setf method-function) (new-value method)
  (setf (slot-value method 'function) new-value))

;;; ensure-generic-function

(defun ensure-generic-function
       (function-name
        &rest all-keys
        &key (generic-function-class *the-class-standard-gf*)
             (method-class *the-class-standard-method*)
        &allow-other-keys)
  (cond ((and (symbolp function-name)
              (special-operator-p function-name))
         ;; Can't override special operators.
         (error "~S names a special operator" function-name))
        ((or (and (fboundp function-name)
                  (not (typep (fdefinition function-name) 'standard-generic-function)))
             (and (symbolp function-name)
                  (macro-function function-name)))
         (cerror "Clobber it" 'simple-error
                 :format-control "~S is already defined as a non-generic function or macro."
                 :format-arguments (list function-name))
         ;; Make sure it is well and truly clobbered.
         (when (symbolp function-name)
           (setf (macro-function function-name) nil))
         (when (fboundp function-name)
           (fmakunbound function-name))
         (setf (compiler-macro-function function-name) nil)))
  (cond ((fboundp function-name)
         ;; This should call reinitialize-instance here, but whatever.
         (fdefinition function-name))
        (t
         (let ((gf (apply (if (eq generic-function-class *the-class-standard-gf*)
                              #'make-instance-standard-generic-function
                              #'make-instance)
                          generic-function-class
                          :name function-name
                          :method-class method-class
                          all-keys)))
           ;; Not entirely sure where this should be done.
           ;; SBCL seems to do it in (ENSURE-GENERIC-FUNCTION-USING-CLASS NULL).
           (setf (fdefinition function-name) gf)
           gf))))

(defun generic-function-single-dispatch-p (gf)
  "Returns true when the generic function only one non-t specialized argument and
has only has class specializer."
  (when (and (eq (class-of gf) *the-class-standard-gf*)
             (not (generic-function-has-unusual-specializers gf)))
    (let ((specializers (generic-function-relevant-arguments gf))
          (count 0)
          (offset 0))
      (dotimes (i (length specializers))
        (unless (zerop (bit specializers i))
          (setf offset i)
          (incf count)))
      (if (eql count 1)
          (values t offset)
          nil))))

;;; finalize-generic-function

;;; N.B. Same basic idea as finalize-inheritance.  Takes care of recomputing
;;; and storing the discriminating function, and clearing the effective method
;;; function table.

(defun reset-gf-emf-table (gf)
  (cond ((single-dispatch-emf-table-p (classes-to-emf-table gf))
         (map-single-dispatch-emf-table
          (lambda (class fn)
            (declare (ignore fn))
            (setf (class-dependents class) (remove gf (class-dependents class))))
          (classes-to-emf-table gf))
         (clear-single-dispatch-emf-table (classes-to-emf-table gf)))
        (t (loop
              for classes being the hash-keys in (classes-to-emf-table gf)
              do (loop
                    for class in classes
                    do (setf (class-dependents class) (remove gf (class-dependents class)))))
           (clrhash (classes-to-emf-table gf)))))

(defun required-portion (gf args)
  (let ((number-required (length (gf-required-arglist gf))))
    (when (< (length args) number-required)
      (error "Too few arguments to generic function ~S." gf))
    (subseq args 0 number-required)))

(defun gf-required-arglist (gf)
  (let ((plist
          (analyze-lambda-list
            (generic-function-lambda-list gf))))
    (getf plist ':required-args)))

(defun finalize-generic-function (gf)
  ;; Examine all methods and compute the relevant argument bit-vector.
  (let* ((required-args (gf-required-arglist gf))
         (relevant-args (make-array (length required-args)
                                    :element-type 'bit
                                    :initial-element 0))
         (class-t (find-class 't)))
    (setf (generic-function-has-unusual-specializers gf) nil)
    (dolist (m (generic-function-methods gf))
      (do ((i 0 (1+ i))
           (spec (method-specializers m) (rest spec)))
          ((null spec))
        (unless (typep (first spec) '(or standard-class funcallable-standard-class class))
          (setf (generic-function-has-unusual-specializers gf) t))
        (unless (eql (first spec) class-t)
          (setf (bit relevant-args i) 1))))
    (setf (generic-function-relevant-arguments gf) relevant-args))
  (reset-gf-emf-table gf)
  (setf (classes-to-emf-table gf) (if (generic-function-single-dispatch-p gf)
                                      (make-single-dispatch-emf-table)
                                      (make-hash-table :test #'equal)))
  (setf (generic-function-discriminating-function gf)
        (funcall (if (eq (class-of gf) *the-class-standard-gf*)
                     #'std-compute-discriminating-function
                     #'compute-discriminating-function)
                 gf))
  (set-funcallable-instance-function gf (generic-function-discriminating-function gf))
  (values))

;;; make-instance-standard-generic-function creates and initializes an
;;; instance of standard-generic-function without falling into method lookup.
;;; However, it cannot be called until standard-generic-function exists.

(defun make-instance-standard-generic-function
       (generic-function-class &key name lambda-list method-class documentation method-combination)
  (declare (ignore generic-function-class))
  (let ((gf (fc-std-allocate-instance *the-class-standard-gf*)))
    (setf (generic-function-name gf) name)
    (setf (generic-function-lambda-list gf) lambda-list)
    (setf (generic-function-methods gf) ())
    (setf (generic-function-method-class gf) method-class)
    (setf (generic-function-method-combination gf) method-combination)
    (setf (classes-to-emf-table gf) (make-hash-table))
    (finalize-generic-function gf)
    gf))

;;; defmethod

(defun defmethod-1 (gf-name &rest args)
  (when (not (fboundp gf-name))
    (warn "Implicit defintion of generic function ~S." gf-name))
  (apply #'ensure-method
         (ensure-generic-function gf-name)
         args))

;;; ensure method

(defun ensure-method (gf &rest all-keys)
  (let ((new-method
           (apply
              (if (eq (generic-function-method-class gf)
                      *the-class-standard-method*)
                  #'make-instance-standard-method
                  #'make-instance)
              (generic-function-method-class gf)
              all-keys)))
    (add-method gf new-method)
    new-method))

;;; make-instance-standard-method creates and initializes an instance of
;;; standard-method without falling into method lookup.  However, it cannot
;;; be called until standard-method exists.

(defun make-instance-standard-method (method-class
                                      &key lambda-list qualifiers
                                           specializers function)
  (declare (ignore method-class))
  (let ((method (std-allocate-instance *the-class-standard-method*)))
    (setf (method-lambda-list method) lambda-list)
    (setf (method-qualifiers method) qualifiers)
    (setf (method-specializers method) specializers)
    (setf (method-generic-function method) nil)
    (setf (method-function method) function)
    method))

;;; add-method

;;; N.B. This version first removes any existing method on the generic function
;;; with the same qualifiers and specializers.  It's a pain to develop
;;; programs without this feature of full CLOS.

(defun add-method (gf method)
  ;; If the GF has no methods and an empty lambda-list, then it may have
  ;; been implicitly created via defmethod. Fill in the lambda-list.
  (when (and (endp (generic-function-methods gf))
             (endp (generic-function-lambda-list gf)))
    (setf (generic-function-lambda-list gf) (method-lambda-list method)))
  (let ((old-method
           (find-method gf (method-qualifiers method)
                           (method-specializers method) nil)))
    (when old-method (remove-method gf old-method)))
  (setf (method-generic-function method) gf)
  (push method (generic-function-methods gf))
  (dolist (specializer (method-specializers method))
    (when (typep specializer '(or standard-class funcallable-standard-class class))
      (pushnew method (class-direct-methods specializer))))
  (finalize-generic-function gf)
  method)

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
        (remove method (generic-function-methods gf)))
  (setf (method-generic-function method) nil)
  (dolist (class (method-specializers method))
    (when (typep class '(or standard-class funcallable-standard-class class))
      (setf (class-direct-methods class)
            (remove method (class-direct-methods class)))))
  (finalize-generic-function gf)
  method)

(defun find-method (gf qualifiers specializers
                    &optional (errorp t))
  (let ((method
          (find-if #'(lambda (method)
                       (and (equal qualifiers
                                   (method-qualifiers method))
                            (equal specializers
                                   (method-specializers method))))
                   (generic-function-methods gf))))
      (if (and (null method) errorp)
          (error "No such method for ~S." (generic-function-name gf))
          method)))

;;; Reader and write methods

(defun add-reader-method (class fn-name slot-name)
  (add-method (ensure-generic-function fn-name :lambda-list '(object))
              (make-instance (reader-method-class class slot-name :slot-definition slot-name)
                             :lambda-list '(object)
                             :qualifiers ()
                             :specializers (list class)
                             :function (lambda (args next-emfun)
                                         (declare (ignore next-emfun))
                                         (apply (lambda (object)
                                                  (slot-value object slot-name))
                                                args))
                             :slot-definition slot-name))
  (values))

(defun add-writer-method (class fn-name slot-name)
  (add-method (ensure-generic-function fn-name :lambda-list '(new-value object))
              (make-instance (writer-method-class class slot-name :slot-definition slot-name)
                             :lambda-list '(new-value object)
                             :qualifiers ()
                             :specializers (list (find-class 't) class)
                             :function (lambda (args next-emfun)
                                         (declare (ignore next-emfun))
                                         (apply (lambda (new-value object)
                                                  (setf (slot-value object slot-name) new-value))
                                                args))
                             :slot-definition slot-name))
  (values))

;;;
;;; Generic function invocation
;;;

;;; apply-generic-function

(defun apply-generic-function (gf args)
  (apply (generic-function-discriminating-function gf) args))

;;; compute-discriminating-function

(defun compute-reader-discriminator (gf emf-table argument-offset)
  (lambda (object) ;ehhh...
    (let* ((class (class-of object))
           (location (single-dispatch-emf-entry emf-table class)))
      (if location
          (fast-slot-read object location)
          (slow-single-dispatch-method-lookup* gf argument-offset (list object) :reader)))))

(defun compute-writer-discriminator (gf emf-table argument-offset)
  (lambda (new-value object) ;ehhh...
    (let* ((class (class-of object))
           (location (single-dispatch-emf-entry emf-table class)))
      (if location
          (fast-slot-write new-value object location)
          (slow-single-dispatch-method-lookup* gf argument-offset (list new-value object) :writer)))))

(defun compute-1-effective-discriminator (gf emf-table argument-offset)
  (lambda (&rest args)
    (let* ((class (class-of (nth argument-offset args)))
           (emfun (single-dispatch-emf-entry emf-table class)))
      (if emfun
          (funcall emfun args)
          (slow-single-dispatch-method-lookup gf args class)))))

(defun compute-n-effective-discriminator (gf emf-table n-required-args)
  (lambda (&rest args)
    (when (< (length args) n-required-args)
      (error 'sys.int::simple-program-error
             :format-control "Too few arguments to generic function ~S."
             :format-arguments (list gf)))
    (let* ((classes (mapcar #'class-of (subseq args 0 n-required-args)))
           (emfun (gethash classes emf-table nil)))
      (if emfun
          (funcall emfun args)
          (slow-method-lookup gf args classes)))))

(defun slow-single-dispatch-method-lookup* (gf argument-offset args state)
  (let ((emf-table (classes-to-emf-table gf)))
    (ecase state
      (:reader
       (let* ((classes (mapcar #'class-of (required-portion gf args)))
              (class (nth argument-offset classes))
              (applicable-methods
               (compute-applicable-methods-using-classes gf classes)))
         (cond ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-reader-method)
                     (standardish-class-p (class-of class)))
                (let ((location (slot-location class (slot-value (first applicable-methods) 'slot-definition))))
                  (setf (single-dispatch-emf-entry emf-table class) location)
                  (pushnew gf (class-dependents class))
                  (fast-slot-read (first args) location)))
               (t ;; Give up and use the full path.
                (slow-single-dispatch-method-lookup* gf argument-offset args :never-called)))))
      (:writer
       (let* ((classes (mapcar #'class-of (required-portion gf args)))
              (class (nth argument-offset classes))
              (applicable-methods
               (compute-applicable-methods-using-classes gf classes)))
         (cond ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-writer-method)
                     (standardish-class-p (class-of class)))
                (let ((location (slot-location class (slot-value (first applicable-methods) 'slot-definition))))
                  (setf (single-dispatch-emf-entry emf-table class) location)
                  (pushnew gf (class-dependents class))
                  (fast-slot-write (first args) (second args) location)))
               (t ;; Give up and use the full path.
                (slow-single-dispatch-method-lookup* gf argument-offset args :never-called)))))
      (:never-called
       (reset-gf-emf-table gf)
       (let* ((classes (mapcar #'class-of (required-portion gf args)))
              (class (nth argument-offset classes))
              (applicable-methods
               (compute-applicable-methods-using-classes gf classes)))
         (cond ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-reader-method)
                     (standardish-class-p (class-of class)))
                ;; Switch to reader-method.
                (setf (generic-function-discriminating-function gf)
                      (compute-reader-discriminator gf emf-table argument-offset))
                (set-funcallable-instance-function gf (generic-function-discriminating-function gf))
                (apply gf args))
               ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-writer-method)
                     (standardish-class-p (class-of class)))
                ;; Switch to writer-method.
                (setf (generic-function-discriminating-function gf)
                      (compute-writer-discriminator gf emf-table argument-offset))
                (set-funcallable-instance-function gf (generic-function-discriminating-function gf))
                (apply gf args))
               (t ;; Switch to 1-effective.
                (setf (generic-function-discriminating-function gf)
                      (compute-1-effective-discriminator gf emf-table argument-offset))
                (set-funcallable-instance-function gf (generic-function-discriminating-function gf))
                (slow-single-dispatch-method-lookup gf args (class-of (nth argument-offset args))))))))))

(defun std-compute-discriminating-function (gf)
  (lambda (&rest args)
    (multiple-value-bind (single-dispatch-p argument-offset)
        (generic-function-single-dispatch-p gf)
      (cond (single-dispatch-p
             (slow-single-dispatch-method-lookup* gf argument-offset args :never-called))
            (t (setf (generic-function-discriminating-function gf)
                     (compute-n-effective-discriminator gf (classes-to-emf-table gf) (length (gf-required-arglist gf))))
               (set-funcallable-instance-function gf (generic-function-discriminating-function gf))
               (apply gf args))))))

(defun slow-method-lookup (gf args classes)
  (multiple-value-bind (applicable-methods validp)
      (compute-applicable-methods-using-classes gf classes)
    (unless validp
      (setf applicable-methods (compute-applicable-methods gf args)))
    (let ((emfun (cond (applicable-methods
                        (funcall
                         (if (eq (class-of gf) *the-class-standard-gf*)
                             #'std-compute-effective-method-function
                             #'compute-effective-method-function)
                         gf applicable-methods))
                       (t
                        (lambda (args)
                          (apply #'no-applicable-method gf args))))))
      ;; Cache is only valid for non-eql methods.
      (when validp
        (setf (gethash classes (classes-to-emf-table gf)) emfun)
        (dolist (class classes)
          (pushnew gf (class-dependents class))))
      (funcall emfun args))))

(defun slow-single-dispatch-method-lookup (gf args class)
  (let* ((classes (mapcar #'class-of
                          (required-portion gf args)))
         (applicable-methods
          (compute-applicable-methods-using-classes gf classes)))
    (let ((emfun (cond (applicable-methods
                        (std-compute-effective-method-function gf applicable-methods))
                       (t
                        (lambda (args)
                          (apply #'no-applicable-method gf args))))))
      (setf (single-dispatch-emf-entry (classes-to-emf-table gf) class) emfun)
      (pushnew gf (class-dependents class))
      (funcall emfun args))))

;;; compute-applicable-methods-using-classes

(defun compute-applicable-methods-using-classes
       (gf required-classes)
  (values (sort
           (copy-list
            (remove-if-not #'(lambda (method)
                               (every (lambda (class specializer)
                                        (etypecase specializer
                                          (eql-specializer
                                           (return-from compute-applicable-methods-using-classes
                                             (values nil nil)))
                                          ((or standard-class funcallable-standard-class class)
                                           (subclassp class specializer))))
                                      required-classes
                                      (method-specializers method)))
                           (generic-function-methods gf)))
           #'(lambda (m1 m2)
               (funcall
                (if (eq (class-of gf) *the-class-standard-gf*)
                    #'std-method-more-specific-p
                    #'method-more-specific-p)
                gf m1 m2 required-classes)))
          t))

(defun compute-applicable-methods (gf args)
  (sort
   (copy-list
    (remove-if-not #'(lambda (method)
                       (every (lambda (arg specializer)
                                (etypecase specializer
                                  (eql-specializer
                                   (eql (eql-specializer-object specializer) arg))
                                  ((or standard-class funcallable-standard-class class)
                                   (subclassp (class-of arg) specializer))))
                              args
                              (method-specializers method)))
                   (generic-function-methods gf)))
   #'(lambda (m1 m2)
       (funcall
        (if (eq (class-of gf) *the-class-standard-gf*)
            #'std-method-more-specific-with-args-p
            #'method-more-specific-with-args-p)
        gf m1 m2 args))))

;;; method-more-specific-p

(defun std-method-more-specific-p (gf method1 method2 required-classes)
  (declare (ignore gf))
  (mapc #'(lambda (spec1 spec2 arg-class)
            (cond ((and (typep spec1 'eql-specializer)
                        (not (typep spec2 'eql-specializer)))
                   (return-from std-method-more-specific-p t))
                  ((and (typep spec1 'eql-specializer)
                        (typep spec2 'eql-specializer))
                   (return-from std-method-more-specific-p nil))
                  (t (unless (eq spec1 spec2)
                       (return-from std-method-more-specific-p
                         (sub-specializer-p spec1 spec2 arg-class))))))
        (method-specializers method1)
        (method-specializers method2)
        required-classes)
  nil)

(defun std-method-more-specific-with-args-p (gf method1 method2 args)
  (declare (ignore gf))
  (mapc #'(lambda (spec1 spec2 arg)
            (cond ((and (typep spec1 'eql-specializer)
                        (not (typep spec2 'eql-specializer)))
                   (return-from std-method-more-specific-with-args-p t))
                  ((and (typep spec1 'eql-specializer)
                        (typep spec2 'eql-specializer))
                   (return-from std-method-more-specific-with-args-p nil))
                  (t (unless (eq spec1 spec2)
                       (return-from std-method-more-specific-with-args-p
                         (sub-specializer-p spec1 spec2 (class-of arg)))))))
        (method-specializers method1)
        (method-specializers method2)
        args)
  nil)

;;; apply-methods and compute-effective-method-function

(defun apply-methods (gf args methods)
  (funcall (compute-effective-method-function gf methods)
           args))

(defun primary-method-p (method)
  (null (method-qualifiers method)))
(defun before-method-p (method)
  (equal '(:before) (method-qualifiers method)))
(defun after-method-p (method)
  (equal '(:after) (method-qualifiers method)))
(defun around-method-p (method)
  (equal '(:around) (method-qualifiers method)))

(defun std-compute-effective-method-function-with-standard-method-combination (gf methods)
  (let ((primaries (remove-if-not #'primary-method-p methods))
        (around (find-if #'around-method-p methods)))
    (when (null primaries)
      (error "No primary methods for the generic function ~S." gf))
    (if around
        (let ((next-emfun
                (funcall
                   (if (eq (class-of gf) *the-class-standard-gf*)
                       #'std-compute-effective-method-function
                       #'compute-effective-method-function)
                   gf (remove around methods)))
              (around-fn (method-function around)))
          #'(lambda (args)
              (funcall around-fn args next-emfun)))
        (let ((next-emfun (compute-primary-emfun (cdr primaries)))
              (primary (method-function (car primaries)))
              (befores (mapcar 'method-function (remove-if-not #'before-method-p methods)))
              (reverse-afters
                (mapcar 'method-function (reverse (remove-if-not #'after-method-p methods)))))
          (cond ((and befores reverse-afters)
                 (lambda (args)
                   (dolist (before befores)
                     (funcall before args nil))
                   (multiple-value-prog1
                       (funcall primary args next-emfun)
                     (dolist (after reverse-afters)
                       (funcall after args nil)))))
                (befores
                 (lambda (args)
                   (dolist (before befores)
                     (funcall before args nil))
                   (funcall primary args next-emfun)))
                (reverse-afters
                 (lambda (args)
                   (multiple-value-prog1
                       (funcall primary args next-emfun)
                     (dolist (after reverse-afters)
                       (funcall after args nil)))))
                (t (lambda (args)
                     (funcall primary args next-emfun))))))))

(defun generate-method-combination-effective-method (name effective-method-body)
  (let ((method-args (gensym "ARGS"))
        (next-emfun (gensym "NEXT-EMFUN")))
    `(lambda (,method-args)
       (declare (system:lambda-name (effective-method ,@name)))
       (macrolet ((call-method (method &optional next-method-list)
                    (when (listp method)
                      (assert (eql (first method) 'make-method)))
                    (cond ((listp method)
                           (assert (eql (first method) 'make-method))
                           (assert (eql (length method) 2))
                           (second method))
                          (t
                           `(funcall ',(method-function method)
                                     ,',method-args
                                     ,(if next-method-list
                                          `(lambda (,',method-args)
                                             (call-method ,(first next-method-list)
                                                          ,(rest next-method-list)))
                                          nil)))))
                  (make-method (form)
                    (error "MAKE-METHOD must be either the method argument or a next-method supplied to CALL-METHOD.")))
         ,effective-method-body))))

(defun generate-method-combination-effective-method-name (gf mc-object methods)
  (list* (generic-function-name gf)
         (method-combination-name mc-object)
         (mapcar (lambda (method)
                   (list (method-qualifiers method)
                         (mapcar (lambda (specializer)
                                   (typecase specializer
                                     ((or standard-class funcallable-standard-class class)
                                      (class-name specializer))
                                     (t specializer)))
                                 (method-specializers method))))
                 methods)))

(defun std-compute-effective-method-function (gf methods)
  (let ((mc (generic-function-method-combination gf)))
    (cond (mc
           (let* ((mc-object (method-combination-object-method-combination mc))
                  (mc-args (method-combination-object-arguments mc))
                  (effective-method-body (apply (method-combination-combiner mc-object)
                                                gf
                                                methods
                                                mc-args))
                  (name (generate-method-combination-effective-method-name gf mc-object methods)))
             (eval (generate-method-combination-effective-method name effective-method-body))))
          (t
           (std-compute-effective-method-function-with-standard-method-combination
            gf methods)))))

;;; compute an effective method function from a list of primary methods:

(defun compute-primary-emfun (methods)
  (if (null methods)
      nil
      (let ((next-emfun (compute-primary-emfun (cdr methods)))
            (fn (method-function (car methods))))
        #'(lambda (args)
            (funcall fn args next-emfun)))))

;;; apply-method and compute-method-function

(defun apply-method (method args next-methods)
  (funcall (method-function method)
           args
           (if (null next-methods)
               nil
               (compute-effective-method-function
                 (method-generic-function method) next-methods))))

;;; N.B. The function kludge-arglist is used to pave over the differences
;;; between argument keyword compatibility for regular functions versus
;;; generic functions.

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun kludge-arglist (lambda-list)
  (if (and (member '&key lambda-list)
           (not (member '&allow-other-keys lambda-list)))
      (append lambda-list '(&allow-other-keys))
      (if (and (not (member '&rest lambda-list))
               (not (member '&key lambda-list)))
          (append lambda-list '(&key &allow-other-keys))
          lambda-list)))

)

;;;
;;; Bootstrap
;;;

(defun closette-bootstrap ()
  (format t "Beginning to bootstrap Closette...~%")
  (forget-all-classes)
  ;; How to create the class hierarchy in 10 easy steps:
  ;; 1. Figure out standard-class's slots.
  (let ((clos-class-slots
         (mapcar #'(lambda (slotd)
                     (make-effective-slot-definition
                      :name (car slotd)
                      :initargs
                      (let ((a (getf (cdr slotd) ':initarg)))
                        (if a (list a) ()))
                      :initform (getf (cdr slotd) ':initform)
                      :initfunction
                      (let ((a (getf (cdr slotd) ':initform)))
                        (if a #'(lambda () (eval a)) nil))
                      :allocation ':instance))
                 (nth 3 *the-defclass-clos-class*))))
    (setf *standard-class-effective-slots-position*
          (position 'effective-slots clos-class-slots
                    :key #'slot-definition-name))
    (setf *standard-class-slot-storage-layout-position*
          (position 'slot-storage-layout clos-class-slots
                    :key #'slot-definition-name))
    (setf *standard-class-hash-position*
          (position 'hash clos-class-slots
                    :key #'slot-definition-name))
    (format t "Slots for STANDARD-CLASS: ~S~%" clos-class-slots)
    ;; 2. Create the standard-class metaobject by hand.
    (let ((layout (make-array (length clos-class-slots)))
          (slots (make-array (length clos-class-slots)
                             :initial-element *secret-unbound-value*)))
      (loop
         for slot in clos-class-slots
         for i from 0
         do (setf (svref layout i) (slot-definition-name slot)))
      (setf *the-class-standard-class*
            (allocate-std-instance
             'tba
             slots
             layout))
      (setf (svref slots *standard-class-slot-storage-layout-position*) layout))
    ;; 3. Install standard-class's (circular) class-of link.
    (setf (std-instance-class *the-class-standard-class*)
          *the-class-standard-class*)
    ;; (It's now okay to use class-... accessor).
    ;; 4. Fill in standard-class's class-slots.
    (setf (class-slots *the-class-standard-class*) clos-class-slots))
  ;; (Skeleton built; it's now okay to call make-instance-standard-class.)
  ;; 5. Hand build the class t so that it has no direct superclasses.
  (format t "Building class T.~%")
  (setf (find-class 't)
        (let ((class (std-allocate-instance *the-class-standard-class*)))
          (setf (class-name class) 't)
          (setf (class-direct-subclasses class) ())
          (setf (class-direct-superclasses class) ())
          (setf (class-direct-methods class) ())
          (setf (class-direct-slots class) ())
          (setf (class-direct-default-initargs class) ())
          (setf (class-precedence-list class) (list class))
          (setf (class-slot-storage-layout class) (make-array 0))
          (setf (class-slots class) ())
          (setf (class-dependents class) ())
          (setf (class-hash class) (next-class-hash-value))
          class))
  ;; (It's now okay to define subclasses of t.)
  ;; 6. Create the other superclasses of standard-class (i.e., standard-object).
  (format t "Defining STANDARD-OBJECT and other classes.~%")
  (defclass standard-object (t) ())
  (defclass metaobject () ())
  (defclass specializer (metaobject) ())
  (defclass class (specializer) ())
  (eval *the-defclass-clos-class*)
  ;; 7. Define the full-blown version of standard-class.
  ;;    The temporary standard-class was never installed into the class table,
  ;;    so DEFCLASS won't trigger redefinition, instead it will produce an
  ;;    entirely new class object.
  (format t "Defining real STANDARD-CLASS.~%")
  (setf *the-class-standard-class*
        (defclass standard-class (clos-class)
          ()
          (:default-initargs
           :direct-superclasses (list (find-class 'standard-object)))))
  ;; 8. Replace all existing pointers to the skeleton with real one.
  ;;    Update layout pointers too.
  (let ((real-layout (svref (std-instance-slots *the-class-standard-class*)
                            *standard-class-slot-storage-layout-position*)))
    (dolist (name '(t clos-class standard-object standard-class metaobject specializer class))
      (format t "Fixing up class ~S.~%" name)
      (let ((class (find-class name)))
        (setf (std-instance-class class) *the-class-standard-class*
              (std-instance-layout class) real-layout))))
  (format t "Defining remaining bootstrap classes.~%")
  ;; Define BUILT-IN-CLASS.
  (defclass built-in-class (clos-class) ())
  ;; Define FUNCTION. This is needed for FUNCALLABLE-STANDARD-OBJECT.
  ;; It is initially defined as a standard class, and then hacked to be a
  ;; built-in-class, as the machinery for defining built-in-classes does not
  ;; work at this point.
  (defclass function (t) ())
  ;; Define a few other similar classes needed for the rest of the bootstrap.
  (defclass symbol (t) ())
  (defclass sequence (t) ())
  (defclass list (sequence) ())
  (defclass null (symbol list) ())
  ;; Fudge some classes so they becomes instances of BUILT-IN-CLASS.
  ;; B-I-C has the same layout as S-C, so it's not a huge leap.
  (dolist (name '(t function symbol sequence list null))
    (let ((class (find-class name))
          (bic-class (find-class 'built-in-class)))
      (setf (std-instance-class class) bic-class
            (std-instance-layout class) (svref (std-instance-slots bic-class)
                                               *standard-class-slot-storage-layout-position*))))
  ;; 9. Define the other standard metaobject classes.
  (format t "Defining remaining standard metaobject classes.~%")
  (setf *the-class-funcallable-standard-class*
        (defclass funcallable-standard-class (clos-class)
          ()
          (:default-initargs
           :direct-superclasses (list (find-class 'funcallable-standard-object)))))
  (defclass funcallable-standard-object (standard-object function)
    ()
    (:metaclass funcallable-standard-class))
  (setf *the-class-standard-gf*
        (defclass standard-generic-function ()
          ((name :initarg :name)      ; :accessor generic-function-name
           (lambda-list               ; :accessor generic-function-lambda-list
            :initarg :lambda-list)
           (methods :initform ())     ; :accessor generic-function-methods
           (method-class              ; :accessor generic-function-method-class
            :initarg :method-class)
           (discriminating-function)  ; :accessor generic-function-
                                        ;    -discriminating-function
           (classes-to-emf-table      ; :accessor classes-to-emf-table
            :initform (make-hash-table :test #'equal))
           (relevant-arguments)       ; :accessor generic-function-relevant-arguments
           (weird-specializers-p)     ; :accessor generic-function-has-unusual-specializers
           (method-combination        ; :accessor generic-function-method-combination
            :initarg :method-combination)
           )
          (:default-initargs
           :name nil
            :lambda-list '()
            :method-class (find-class 'standard-method)
            :method-combination nil)
          (:metaclass funcallable-standard-class)))
  (setf *the-class-standard-method*
        (defclass standard-method ()
          ((lambda-list :initarg :lambda-list)     ; :accessor method-lambda-list
           (qualifiers :initarg :qualifiers)       ; :accessor method-qualifiers
           (specializers :initarg :specializers)   ; :accessor method-specializers
           (generic-function :initform nil)        ; :accessor method-generic-function
           (function :initarg :function))          ; :accessor method-function
          (:default-initargs
           :qualifiers '()
            :specializers '())))
  (defclass standard-accessor-method (standard-method)
    ((slot-definition :initarg :slot-definition)))
  (defclass standard-reader-method (standard-accessor-method) ())
  (defclass standard-writer-method (standard-accessor-method) ())
  ;; (Clear sailing from here on in).
  ;; Voila! The class hierarchy is in place.
  (format t "Class hierarchy created.~%")
  ;; (It's now okay to define generic functions and methods.)
  )

(when (not (find-class 't nil))
  (closette-bootstrap))

(when (and (fboundp 'print-object)
           (not (typep (fdefinition 'print-object) 'standard-generic-function)))
  ;; Print-object starts off as a normal object.
  ;; Clobber it during bootstrap, but not after.
  (fmakunbound 'print-object))

(defgeneric print-object (instance stream))

(defmethod print-object ((instance t) stream)
  (print-unreadable-object (instance stream :type t :identity t)))

(defmethod print-object ((instance standard-object) stream)
  (print-unreadable-object (instance stream :identity t)
    (format stream "~:(~S~)"
            (class-name (class-of instance))))
  instance)

;;; Slot access

(defgeneric slot-value-using-class (class instance slot-name))
(defmethod slot-value-using-class
           ((class standard-class) instance slot-name)
  (std-slot-value instance slot-name))
(defmethod slot-value-using-class
           ((class funcallable-standard-class) instance slot-name)
  (std-slot-value instance slot-name))

(defgeneric (setf slot-value-using-class) (new-value class instance slot-name))
(defmethod (setf slot-value-using-class)
           (new-value (class standard-class) instance slot-name)
  (setf (std-slot-value instance slot-name) new-value))
(defmethod (setf slot-value-using-class)
           (new-value (class funcallable-standard-class) instance slot-name)
  (setf (std-slot-value instance slot-name) new-value))
;;; N.B. To avoid making a forward reference to a (setf xxx) generic function:
(defun setf-slot-value-using-class (new-value class object slot-name)
  (setf (slot-value-using-class class object slot-name) new-value))

(defgeneric slot-exists-p-using-class (class instance slot-name))
(defmethod slot-exists-p-using-class
           ((class standard-class) instance slot-name)
  (std-slot-exists-p instance slot-name))
(defmethod slot-exists-p-using-class
           ((class funcallable-standard-class) instance slot-name)
  (std-slot-exists-p instance slot-name))

(defgeneric slot-boundp-using-class (class instance slot-name))
(defmethod slot-boundp-using-class
           ((class standard-class) instance slot-name)
  (std-slot-boundp instance slot-name))
(defmethod slot-boundp-using-class
           ((class funcallable-standard-class) instance slot-name)
  (std-slot-boundp instance slot-name))

(defgeneric slot-makunbound-using-class (class instance slot-name))
(defmethod slot-makunbound-using-class
           ((class standard-class) instance slot-name)
  (std-slot-makunbound instance slot-name))
(defmethod slot-makunbound-using-class
           ((class funcallable-standard-class) instance slot-name)
  (std-slot-makunbound instance slot-name))

;;; Instance creation and initialization

(defgeneric allocate-instance (class))
(defmethod allocate-instance ((class standard-class))
  (std-allocate-instance class))
(defmethod allocate-instance ((class funcallable-standard-class))
  (fc-std-allocate-instance class))

(defun std-compute-initargs (class initargs)
  (let ((default-initargs '()))
    (dolist (c (class-precedence-list class))
      (loop
         for (initarg form fn) in (class-direct-default-initargs c)
         do (when (and (not (member initarg initargs))
                       (not (member initargs default-initargs)))
              (push initarg default-initargs)
              (push (funcall fn) default-initargs))))
    (append initargs (nreverse default-initargs))))

(defgeneric make-instance (class &key))
(defmethod make-instance ((class standard-class) &rest initargs)
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance (std-compute-initargs class initargs))
    instance))
(defmethod make-instance ((class funcallable-standard-class) &rest initargs)
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance (std-compute-initargs class initargs))
    instance))
(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(defgeneric initialize-instance (instance &key))
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &key))
(defmethod reinitialize-instance
           ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance () initargs))

(defgeneric shared-initialize (instance slot-names &key))
(defmethod shared-initialize ((instance standard-object)
                              slot-names &rest all-keys)
  (dolist (slot (class-slots (class-of instance)))
    (let ((slot-name (slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
            (get-properties
              all-keys (slot-definition-initargs slot))
         (declare (ignore init-key))
         (if foundp
             (setf (slot-value instance slot-name) init-value)
	     (when (and (not (slot-boundp instance slot-name))
                        (not (null (slot-definition-initfunction slot)))
                        (or (eq slot-names t)
                            (member slot-name slot-names)))
               (setf (slot-value instance slot-name)
                     (funcall (slot-definition-initfunction slot))))))))
  instance)

;;; change-class

(defgeneric change-class (instance new-class &key))
(defmethod change-class
           ((old-instance standard-object)
            (new-class standard-class)
            &rest initargs)
  (let ((new-instance (allocate-instance new-class)))
    (dolist (slot-name (mapcar #'slot-definition-name
                               (class-slots new-class)))
      (when (and (slot-exists-p old-instance slot-name)
                 (slot-boundp old-instance slot-name))
        (setf (slot-value new-instance slot-name)
              (slot-value old-instance slot-name))))
    (rotatef (std-instance-slots new-instance)
             (std-instance-slots old-instance))
    (rotatef (std-instance-class new-instance)
             (std-instance-class old-instance))
    (rotatef (std-instance-layout new-instance)
             (std-instance-layout old-instance))
    (apply #'update-instance-for-different-class
           new-instance old-instance initargs)
    old-instance))

(defmethod change-class
           ((instance standard-object) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(defgeneric update-instance-for-different-class (old new &key))
(defmethod update-instance-for-different-class
           ((old standard-object) (new standard-object) &rest initargs)
  (let ((added-slots
          (remove-if #'(lambda (slot-name)
                         (slot-exists-p old slot-name))
                     (mapcar #'slot-definition-name
                             (class-slots (class-of new))))))
    (apply #'shared-initialize new added-slots initargs)))

;;;
;;;  Methods having to do with class metaobjects.
;;;

(defmethod print-object ((class standard-class) stream)
  (print-unreadable-object (class stream :identity t)
    (format stream "~:(~S~) ~S"
            (class-name (class-of class))
            (class-name class)))
  class)

(defmethod print-object ((class funcallable-standard-class) stream)
  (print-unreadable-object (class stream :identity t)
    (format stream "~:(~S~) ~S"
            (class-name (class-of class))
            (class-name class)))
  class)

(defmethod initialize-instance :after ((class standard-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

(defmethod initialize-instance :after ((class funcallable-standard-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

(defgeneric reader-method-class (class direct-slot &rest initargs))
;; ### slot objects.
(defmethod reader-method-class ((class standard-class) direct-slot &rest initargs)
  (find-class 'standard-reader-method))
(defmethod reader-method-class ((class funcallable-standard-class) direct-slot &rest initargs)
  (find-class 'standard-reader-method))

(defgeneric writer-method-class (class direct-slot &rest initargs))
;; ### slot objects.
(defmethod writer-method-class ((class standard-class) direct-slot &rest initargs)
  (find-class 'standard-writer-method))
(defmethod writer-method-class ((class funcallable-standard-class) direct-slot &rest initargs)
  (find-class 'standard-writer-method))

;;; Finalize inheritance

(defgeneric finalize-inheritance (class))
(defmethod finalize-inheritance ((class standard-class))
  (std-finalize-inheritance class)
  (values))
(defmethod finalize-inheritance ((class funcallable-standard-class))
  (std-finalize-inheritance class)
  (values))

;;; Class precedence lists

(defgeneric compute-class-precedence-list (class))
(defmethod compute-class-precedence-list ((class standard-class))
  (std-compute-class-precedence-list class))
(defmethod compute-class-precedence-list ((class funcallable-standard-class))
  (std-compute-class-precedence-list class))

;;; Slot inheritance

(defgeneric compute-slots (class))
(defmethod compute-slots ((class standard-class))
  (std-compute-slots class))
(defmethod compute-slots ((class funcallable-standard-class))
  (std-compute-slots class))

(defgeneric compute-effective-slot-definition (class direct-slots))
(defmethod compute-effective-slot-definition
           ((class standard-class) direct-slots)
  (std-compute-effective-slot-definition class direct-slots))
(defmethod compute-effective-slot-definition
           ((class funcallable-standard-class) direct-slots)
  (std-compute-effective-slot-definition class direct-slots))

;;;
;;; Methods having to do with generic function metaobjects.
;;;

(defmethod print-object ((gf standard-generic-function) stream)
  (print-unreadable-object (gf stream :identity t)
    (format stream "~:(~S~) ~S"
            (class-name (class-of gf))
            (generic-function-name gf))
    (let ((mc (generic-function-method-combination gf)))
      (when mc
        (format stream " ~S"
                (method-combination-name
                 (method-combination-object-method-combination mc))))))
  gf)

(defmethod initialize-instance :after ((gf standard-generic-function) &key)
  (finalize-generic-function gf))

;;;
;;; Methods having to do with method metaobjects.
;;;

(defmethod print-object ((method standard-method) stream)
  (print-unreadable-object (method stream :identity t)
    (format stream "~:(~S~) ~S ~S ~S"
	    (class-name (class-of method))
	    (when (method-generic-function method)
              (generic-function-name (method-generic-function method)))
	    (method-qualifiers method)
	    (mapcar (lambda (x) (typecase x
                                  ((or standard-class funcallable-standard-class class)
                                   (class-name x))
                                  (t x)))
                    (method-specializers method))))
  method)

;;;
;;; Methods having to do with generic function invocation.
;;;

(defgeneric compute-discriminating-function (gf))
(defmethod compute-discriminating-function ((gf standard-generic-function))
  (std-compute-discriminating-function gf))

(defgeneric method-more-specific-p (gf method1 method2 required-classes))
(defmethod method-more-specific-p
           ((gf standard-generic-function) method1 method2 required-classes)
  (std-method-more-specific-p gf method1 method2 required-classes))

(defgeneric method-more-specific-with-args-p (gf method1 method2 args))
(defmethod method-more-specific-with-args-p
           ((gf standard-generic-function) method1 method2 args)
  (std-method-more-specific-with-args-p gf method1 method2 args))

(defgeneric compute-effective-method-function (gf methods))
(defmethod compute-effective-method-function
           ((gf standard-generic-function) methods)
  (std-compute-effective-method-function gf methods))

;;; describe-object is a handy tool for enquiring minds:

(defgeneric describe-object (object stream))
(defmethod describe-object ((object standard-object) stream)
  (format stream "A Closette object~
             ~%Printed representation: ~S~
             ~%Class: ~S~
             ~%Structure "
          object
          (class-of object))
  (dolist (sn (mapcar #'slot-definition-name
                      (class-slots (class-of object))))
    (if (slot-boundp object sn)
        (format stream "~%    ~S <- ~S"
                sn
                (slot-value object sn))
        (format stream "~%    ~S <- not bound" sn)))
  (values))

(format t "~%Closette is a Knights of the Lambda Calculus production.~%")

;;; Metaclasses.

(defclass forward-referenced-class (class) ())

(defgeneric ensure-class-using-class (class name &key &allow-other-keys))

(defmethod ensure-class-using-class ((class class) name
                                     &rest all-keys
                                     &key (metaclass 'standard-class) &allow-other-keys)
  (assert (eql name (class-name class)))
  (assert (eql (class-of class) (find-class metaclass)))
  (apply #'reinitialize-instance class (compute-class-initialization-arguments all-keys))
  (setf (find-class name) class)
  class)

(defmethod ensure-class-using-class ((class forward-referenced-class) name
                                     &rest all-keys
                                     &key (metaclass 'standard-class) &allow-other-keys)
  (assert (eql name (class-name class)))
  (let ((initargs (compute-class-initialization-arguments all-keys)))
    (apply #'change-class class metaclass initargs)
    (apply #'reinitialize-instance class initargs))
  (setf (find-class name) class)
  class)

(defmethod ensure-class-using-class ((class null) name
                                     &rest all-keys
                                     &key (metaclass 'standard-class) &allow-other-keys)
  (let ((class (apply #'make-instance metaclass
                      :name name
                      (compute-class-initialization-arguments all-keys))))
    (setf (find-class name) class)
    class))

;;; Built-in-class.

(defmethod initialize-instance :after ((class built-in-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

(defmethod reinitialize-instance :before ((class built-in-class) &rest args)
  (error "Cannot reinitialize built-in classes."))

(defmethod finalize-inheritance ((class built-in-class))
  (std-finalize-inheritance class)
  (values))
(defmethod compute-slots ((class built-in-class))
  (std-compute-slots class))
(defmethod compute-class-precedence-list ((class built-in-class))
  (std-compute-class-precedence-list class))

(defmethod print-object ((class built-in-class) stream)
  (print-unreadable-object (class stream :identity t)
    (format stream "~:(~S~) ~S"
            (class-name (class-of class))
            (class-name class)))
  class)

;;; Structure-class.

(defclass structure-class (clos-class)
  ((structure-definition :initarg :definition)))

(defmethod initialize-instance :after ((class structure-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

(defmethod reinitialize-instance :before ((class structure-class) &rest args)
  (error "Cannot reinitialize structure classes."))

(defmethod slot-value-using-class ((class structure-class) instance slot-name)
  (dolist (slot (class-slots class)
           (error "The slot ~S is missing from the class ~S." slot-name class))
    (when (eql (slot-definition-name slot) slot-name)
      (return (funcall (getf (standard-slot-definition-properties slot) ':accessor-name) instance)))))

(defmethod (setf slot-value-using-class) (new-value (class structure-class) instance slot-name)
  (dolist (slot (class-slots class)
           (error "The slot ~S is missing from the class ~S." slot-name class))
    (when (eql (slot-definition-name slot) slot-name)
      (return (funcall (fdefinition `(setf ,(getf (standard-slot-definition-properties slot) ':accessor-name))) new-value instance)))))

(defmethod finalize-inheritance ((class structure-class))
  (std-finalize-inheritance class)
  (values))
(defmethod compute-slots ((class structure-class))
  (std-compute-slots class))
(defmethod compute-class-precedence-list ((class structure-class))
  (std-compute-class-precedence-list class))

(defmethod compute-effective-slot-definition ((class structure-class) direct-slots)
  (declare (ignore class))
  (make-effective-slot-definition
   :name (slot-definition-name (car direct-slots))
   :accessor-name (getf (standard-slot-definition-properties (car direct-slots)) :accessor-name)
   :allocation (slot-definition-allocation (car direct-slots))))

(defclass structure-object (t)
  ()
  (:metaclass structure-class))

(defmethod print-object ((class structure-class) stream)
  (print-unreadable-object (class stream :identity t)
    (format stream "~:(~S~) ~S"
            (class-name (class-of class))
            (class-name class)))
  class)

;;; eql specializers.

(defvar *interned-eql-specializers* (make-hash-table))

(defclass eql-specializer (specializer)
  ((object :initarg :object :reader eql-specializer-object)))

(defun intern-eql-specializer (object)
  (or (gethash object *interned-eql-specializers*)
      (setf (gethash object *interned-eql-specializers*)
            (make-instance 'eql-specializer
                           :object object))))

(defmacro with-slots (slot-entries instance-form &body body)
  (let ((in (gensym)))
    `(let ((,in ,instance-form))
       (symbol-macrolet ,(mapcar (lambda (slot)
                                   (if (symbolp slot)
                                       `(,slot (slot-value ,in ',slot))
                                       (destructuring-bind (variable-name slot-name)
                                           slot
                                         `(,variable-name (slot-value ,in ',slot-name)))))
                                 slot-entries)
         ,@body))))

(defmacro with-accessors (slot-entries instance-form &body body)
  (let ((instance (gensym)))
    `(let ((,instance ,instance-form))
       (symbol-macrolet ,(loop
                            for entry in slot-entries
                            collect (destructuring-bind (variable-name accessor-name)
                                        entry
                                      `(,variable-name (,accessor-name ,instance))))
         ,@body))))

;;; Class redefinition.

(defun std-after-reinitialization-for-classes (class &rest args &key &allow-other-keys)
  ;; Remove the class as a subclass from all existing superclasses.
  (dolist (superclass (class-direct-superclasses class))
    (setf (class-direct-subclasses superclass) (remove class (class-direct-subclasses superclass))))
  ;; Remove any slot reader/writer methods.
  #+(or)(dolist (direct-slot (class-direct-slots class))
    (dolist (reader (slot-definition-readers direct-slot))
      (remove-reader-method class reader (slot-definition-name direct-slot)))
    (dolist (writer (slot-definition-writers direct-slot))
      (remove-writer-method class writer (slot-definition-name direct-slot))))
  ;; Fall into the initialize-instance code.
  (apply #'std-after-initialization-for-classes class (append args
                                                              (list :direct-superclasses (class-direct-superclasses class))
                                                              (list :direct-slots (class-direct-slots class))
                                                              (list :direct-default-initargs (class-direct-default-initargs class))))
  ;; Flush the EMF tables of generic functions.
  (dolist (gf (class-dependents class))
    (reset-gf-emf-table gf))
  ;; Refinalize any subclasses.
  (dolist (subclass (class-direct-subclasses class))
    (std-after-reinitialization-for-classes subclass)))

(defmethod reinitialize-instance :after ((class standard-class) &rest args &key direct-superclasses &allow-other-keys)
  (apply #'std-after-reinitialization-for-classes
         class
         :direct-superclasses (or direct-superclasses
                                  (list (find-class 'standard-object)))
         args))

(defmethod reinitialize-instance :after ((class funcallable-standard-class) &rest args &key direct-superclasses &allow-other-keys)
  (apply #'std-after-reinitialization-for-classes
         class
         :direct-superclasses (or direct-superclasses
                                  (list (find-class 'funcallable-standard-object)))
         args))

(defun remove-reader-method (class reader slot-name)
  (declare (ignore slot-name))
  (remove-method (fdefinition reader)
                 (find-method (fdefinition reader)
                              '()
                              (list class))))

(defun remove-writer-method (class writer slot-name)
  (declare (ignore slot-name))
  (remove-method (fdefinition writer)
                 (find-method (fdefinition writer)
                              '()
                              (list (find-class 't) class))))

(defun update-instance-for-new-layout (instance)
  (let* ((class (class-of instance))
         (funcallable-instance-p (funcallable-std-instance-p instance))
         (old-slots (if funcallable-instance-p
                        (funcallable-std-instance-slots instance)
                        (std-instance-slots instance)))
         (old-layout (if funcallable-instance-p
                         (funcallable-std-instance-layout instance)
                         (std-instance-layout instance)))
         (new-layout (class-slot-storage-layout class)))
    (if funcallable-instance-p
        (setf (funcallable-std-instance-layout instance) new-layout)
        (setf (std-instance-layout instance) new-layout))
    ;; Don't do anything if the layout hasn't really changed.
    (when (not (equal old-layout new-layout))
      (let* ((old-layout-list (coerce old-layout 'list))
             (new-layout-list (coerce new-layout 'list))
             (added-slots (set-difference new-layout-list old-layout-list))
             (discarded-slots (set-difference old-layout-list new-layout-list)))
        (cond ((and (endp added-slots)
                    (endp discarded-slots))
               ;; No slots added or removed, only the order changed.
               ;; Just build a new slot vector with the correct layout.
               (let ((new-slots (make-array (length new-layout))))
                 (loop
                    for location from 0
                    for slot in new-layout-list
                    do (setf (svref new-slots location) (svref old-slots (fast-sv-position slot old-layout))))
                 (if funcallable-instance-p
                     (setf (funcallable-std-instance-slots instance) new-slots)
                     (setf (std-instance-slots instance) new-slots))))
              (t
               ;; The complicated bit.
               ;; Slots have been added or removed.
               (let ((new-slots (make-array (length new-layout)
                                            :initial-element *secret-unbound-value*))
                     (property-list '()))
                 ;; Copy slots that were not added/discarded.
                 (loop
                    for slot in (intersection new-layout-list old-layout-list)
                    do (setf (svref new-slots (fast-sv-position slot new-layout))
                             (svref old-slots (fast-sv-position slot old-layout))))
                 ;; Assemble the list of discarded values.
                 (loop
                    for slot in discarded-slots
                    do (let ((value (svref old-slots (fast-sv-position slot old-layout))))
                         (when (not (eql value *secret-unbound-value*))
                           (setf property-list (list* slot value
                                                      property-list)))))
                 ;; New slots.
                 (if funcallable-instance-p
                     (setf (funcallable-std-instance-slots instance) new-slots)
                     (setf (std-instance-slots instance) new-slots))
                 ;; Magic.
                 (update-instance-for-redefined-class instance added-slots discarded-slots property-list))))))))

(defgeneric update-instance-for-redefined-class (instance added-slots discarded-slots property-list &rest initargs &key &allow-other-keys))

(defmethod update-instance-for-redefined-class ((instance standard-object) added-slots discarded-slots property-list &rest initargs &key &allow-other-keys)
  (declare (ignore discarded-slots property-list))
  (apply #'shared-initialize instance added-slots initargs))

(defgeneric slot-unbound (class instance slot-name))

(defmethod slot-unbound ((class t) instance slot-name)
  (error "The slot ~S is unbound in the object ~S."
         slot-name
         instance))

(defgeneric no-applicable-method (generic-function &rest function-arguments))

(defmethod no-applicable-method ((generic-function t) &rest function-arguments)
  (error "No applicable methods to generic function ~S when called with ~S."
         generic-function function-arguments))

;;; Other built-in classes.

(defclass array (t) () (:metaclass built-in-class))
(defclass number (t) () (:metaclass built-in-class))
(defclass character (t) () (:metaclass built-in-class))
(defclass stream (t) () (:metaclass built-in-class))
(defclass cons (list) () (:metaclass built-in-class))
(defclass simple-array (array) () (:metaclass built-in-class))
(defclass vector (array sequence) () (:metaclass built-in-class))
(defclass simple-vector (vector simple-array) () (:metaclass built-in-class))
(defclass bit-vector (vector) () (:metaclass built-in-class))
(defclass string (vector) () (:metaclass built-in-class))
(defclass simple-string (string simple-array) () (:metaclass built-in-class))
(defclass integer (number) () (:metaclass built-in-class))
(defclass float (number) () (:metaclass built-in-class))
(defclass ratio (number) () (:metaclass built-in-class))
(defclass complex (number) () (:metaclass built-in-class))
(defclass mezzano.supervisor:thread (t) () (:metaclass built-in-class))
(defclass sys.int::function-reference (t) () (:metaclass built-in-class))
(defclass sys.int::weak-pointer (t) () (:metaclass built-in-class))
(defclass byte (t) () (:metaclass built-in-class))
