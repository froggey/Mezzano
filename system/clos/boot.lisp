;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; CLOS bootstrapping.
;;;;
;;;; This file defines bootstrap implementations of various class-related
;;;; functions, enough to define the class hierarchy.
;;;;

(in-package :mezzano.clos)

;;; FIND-CLASS.
;;; This is the real find-class.
;;; Primordial class objects installed in the class table during boot will
;;; be converted in-place to real classes after boot.

(sys.int::defglobal *class-reference-table* (make-hash-table :test #'eq))

(defstruct class-reference
  name
  class)

(defun class-reference (symbol)
  (check-type symbol symbol)
  (or (gethash symbol *class-reference-table*)
      (setf (gethash symbol *class-reference-table*)
            (make-class-reference :name symbol))))

(define-compiler-macro find-class (&whole whole symbol &optional (errorp t) environment)
  (if (and (null environment)
           (consp symbol)
           (eql (first symbol) 'quote)
           (consp (rest symbol))
           (symbolp (second symbol))
           (null (rest (rest symbol))))
      `(find-class-in-reference
        (load-time-value (mezzano.clos:class-reference ',(second symbol)))
        ,errorp)
      whole))

(defun find-class-in-reference (reference &optional (errorp t))
  (let ((class (class-reference-class reference))
        (symbol (class-reference-name reference)))
    (when (not class)
      (let ((struct (sys.int::get-structure-type symbol nil)))
        (when struct
          (setf class (class-of-structure-definition struct)))))
    (if (and (null class) errorp)
        (error "No class named ~S." symbol)
        class)))

(defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (find-class-in-reference (class-reference symbol) errorp))

(defun (setf find-class) (new-value symbol &optional (errorp t) environment)
  (declare (ignore errorp environment))
  (let ((reference (class-reference symbol)))
    (cond (new-value
           (setf (get symbol 'sys.int::maybe-class) t))
          (t
           (remprop symbol 'sys.int::maybe-class)))
    (setf (class-reference-class reference) new-value)))

(sys.int::defglobal *next-class-hash-value* 1)

(defun next-class-hash-value ()
  (sys.int::%atomic-fixnum-add-symbol '*next-class-hash-value* 1))

(sys.int::defglobal *secret-unbound-value* (list "slot unbound"))

;;; Primordial classes. These map from class names (not class objects!)
;;; to the argument list passed to ensure-class (plus a :name argument).

(sys.int::defglobal *primordial-class-table* (make-hash-table))

(defun ensure-class (name
                     &rest all-keys
                     &key
                       (metaclass 'standard-class)
                       direct-superclasses
                       &allow-other-keys)
  (let* ((slots (list* :name name
                       :metaclass metaclass
                       :direct-superclasses (cond ((and (endp direct-superclasses)
                                                        (eql metaclass 'standard-class))
                                                   '(standard-object))
                                                  ((and (endp direct-superclasses)
                                                        (eql metaclass 'funcallable-standard-class))
                                                   '(funcallable-standard-object))
                                                  (t direct-superclasses))
                       all-keys)))
    (setf (gethash name *primordial-class-table*) slots)))

(defun get-primordial-slot-definition (class-name slot-name)
  (let* ((class-def (gethash class-name *primordial-class-table*))
         (direct-slots (getf class-def :direct-slots))
         (slot (find slot-name direct-slots :key (lambda (direct-slot) (getf direct-slot :name)))))
    (or slot
        (dolist (c (getf class-def :direct-superclasses)
                 (error "Can't find slot ~S in class ~S." slot-name class-name))
          (let ((s (get-primordial-slot-definition c slot-name)))
            (when s
              (return s)))))))

(defun primordial-initialize-instance (class-name instance &rest initargs)
  (let* ((class-def (gethash class-name *primordial-class-table*))
         (class-layout (getf class-def :layout)))
    ;; Add default initargs to the list.
    (let ((default-initargs '()))
      (labels ((frob (c)
                 (loop
                    for (initarg form fn) in (getf (gethash c *primordial-class-table*) :direct-default-initargs)
                    do (when (and (not (member initarg initargs))
                                  (not (member initargs default-initargs)))
                         (push initarg default-initargs)
                         (push (funcall fn) default-initargs)))
                 (dolist (super (getf (gethash c *primordial-class-table*) :direct-superclasses))
                   (frob super))))
        (frob class-name))
      (setf initargs (append initargs (nreverse default-initargs))))
    (loop
       for slot-name across class-layout
       for slot-def = (get-primordial-slot-definition class-name slot-name)
       do
         (multiple-value-bind (init-key init-value foundp)
             (get-properties initargs (getf slot-def :initargs))
           (declare (ignore init-key))
           (setf (primordial-slot-value instance slot-name)
                 (cond (foundp
                        init-value)
                       ((getf slot-def :initfunction)
                        (funcall (getf slot-def :initfunction)))
                       (t
                        *secret-unbound-value*)))))))

(defun primordial-make-instance (class-name &rest initargs)
  (let* ((layout (getf (gethash class-name *primordial-class-table*) :instance-layout))
         (instance (sys.int::%allocate-instance layout)))
    (apply #'primordial-initialize-instance class-name instance initargs)
    instance))

(defun primordial-slot-location-in-layout (layout slot-name)
  (loop
     with instance-slots = (sys.int::layout-instance-slots layout)
     for i below (length instance-slots) by 2
     do
       (when (eq (svref instance-slots i) slot-name)
         (return (svref instance-slots (1+ i))))
     finally
       (return nil)))

(defun primordial-slot-location (object slot-name)
  (or (primordial-slot-location-in-layout
       (sys.int::%instance-layout object) slot-name)
      (error "Slot ~S missing from the object ~S" slot-name object)))

(defun primordial-slot-value (object slot-name)
  (let ((value (mezzano.runtime::instance-access object (primordial-slot-location object slot-name))))
    (when (eql value *secret-unbound-value*)
      (error "Slot ~S unbound in the object ~S." slot-name object))
    value))

(defun (setf primordial-slot-value) (value object slot-name)
  (setf (mezzano.runtime::instance-access object (primordial-slot-location object slot-name))
        value))

(defun primordial-slot-boundp (object slot-name)
  (not (eql (mezzano.runtime::instance-access object (primordial-slot-location object slot-name))
            *secret-unbound-value*)))

(defun primordial-class-of (object)
  (sys.int::layout-class (sys.int::%instance-layout object)))

;;; Define the standard class hierarchy.
;;;
;;; ENSURE-CLASS is currently set to the primordial ENSURE-CLASS. This will
;;; install primordial class definitions in *PRIMORDIAL-CLASS-TABLE*.
;;; These primordial classes will be converted to real classes by
;;; INITIALIZE-CLOS.
;;; INITIALIZE-CLOS does not respect VALIDATE-SUPERCLASS, which normally
;;; prevents new built-in classes from being defined.

;;(format t "Defining bootstrap classes.~%")

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
   (method-class)
   (discriminating-function)
   (classes-to-emf-table :initform nil)
   (relevant-arguments)
   (weird-specializers-p)
   (method-combination :initarg :method-combination)
   (argument-precedence-order :initarg :argument-precedence-order)
   (argument-reordering-table :initform nil)
   (declarations :initarg :declarations :initform nil)
   (documentation :initform nil :initarg :documentation)
   (dependents :initform '()))
  (:default-initargs
   :name nil
    :lambda-list '()
    :method-class 'standard-method
    :method-combination nil
    :argument-precedence-order '())
  (:metaclass funcallable-standard-class))

(defclass method (metaobject) ())

(defclass standard-method (method)
  ((lambda-list :initarg :lambda-list)     ; :reader method-lambda-list
   (qualifiers :initarg :qualifiers)       ; :reader method-qualifiers
   (specializers :initarg :specializers)   ; :reader method-specializers
   (generic-function :initform nil)        ; :reader method-generic-function
   (function :initarg :function)           ; :reader method-function
   (documentation :initform nil :initarg :documentation))
  (:default-initargs
   :qualifiers '()
    :specializers '()))

(defclass standard-accessor-method (standard-method)
  ((slot-definition                        ; :reader accessor-method-slot-definition
    :initarg :slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())
(defclass standard-writer-method (standard-accessor-method) ())

(defclass method-combination (metaobject) ())

(defclass slot-definition (metaobject) ())

(defclass direct-slot-definition (slot-definition) ())

(defclass effective-slot-definition (slot-definition) ())

(defclass standard-slot-definition (slot-definition)
  ((name                                   ; :reader slot-definition-name
    :initform nil :initarg :name)
   (allocation                             ; :reader slot-definition-allocation
    :initform :instance :initarg :allocation)
   (type                                   ; :reader slot-definition-type
    :initform 't :initarg :type)
   (initform                               ; :reader slot-definition-initform
    :initform nil :initarg :initform)
   (initfunction                           ; :reader slot-definition-initfunction
    :initform nil :initarg :initfunction)
   (initargs                               ; :reader slot-definition-initargs
    :initform '() :initarg :initargs)
   (documentation :initform nil :initarg :documentation)))

(defclass standard-direct-slot-definition (standard-slot-definition direct-slot-definition)
  ((readers :initform '() :initarg :readers)
   (writers :initform '() :initarg :writers)))

(defclass standard-effective-slot-definition (standard-slot-definition effective-slot-definition)
  ((location :initarg :location)))

(defclass specializer (metaobject)
  ((direct-methods :initform ())))

(defclass class (specializer)
  ((direct-subclasses :initform '())))

(defclass forward-referenced-class (class)
  ((name :initarg :name)))

(defclass clos-class (class)
  ((name :initarg :name)              ; :reader class-name
   (direct-superclasses               ; :reader class-direct-superclasses
    :initarg :direct-superclasses)
   (direct-slots :initform ())        ; :reader class-direct-slots
   (class-precedence-list)            ; :reader class-precedence-list
   (effective-slots :initform ())     ; :reader class-slots
   (slot-storage-layout :initform ())
   (direct-default-initargs :initform ()) ; :reader class-direct-default-initargs
   (hash :initform (next-class-hash-value))
   (finalized-p :initform nil)
   (prototype)
   (default-initargs))                ; :reader class-default-initargs
  (:default-initargs :name nil))

(defclass built-in-class (clos-class) ())

(defclass std-class (clos-class)
  ((dependents :initform '())))
(defclass standard-class (std-class) ())
(defclass funcallable-standard-class (std-class) ())

(defclass function (t) () (:metaclass built-in-class))
(defclass compiled-function (function) () (:metaclass built-in-class))
(defclass sys.int::closure (compiled-function) () (:metaclass built-in-class))
(defclass mezzano.delimited-continuations:delimited-continuation (function) () (:metaclass built-in-class))
(defclass symbol (t) () (:metaclass built-in-class))
(defclass character (t) () (:metaclass built-in-class))
;; Streams are ordinary objects with no special representation,
;; they don't need to be BUILT-IN-CLASSes.
(defclass stream (t) ())
(defclass sys.int::function-reference (t) () (:metaclass built-in-class))
(defclass mezzano.runtime::symbol-value-cell (t) () (:metaclass built-in-class))
(defclass sys.int::weak-pointer (t) () (:metaclass built-in-class))
;; FIXME: This doesn't quite work with large bytes.
(defclass byte (t) () (:metaclass built-in-class))

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
(defclass sys.int::simple-array-single-float (vector simple-array) () (:metaclass built-in-class))
(defclass sys.int::simple-array-double-float (vector simple-array) () (:metaclass built-in-class))
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
(defclass single-float (float) () (:metaclass built-in-class))
(defclass double-float (float) () (:metaclass built-in-class))
(defclass complex (number) () (:metaclass built-in-class))
(defclass sys.int::complex-rational (complex) () (:metaclass built-in-class))
(defclass sys.int::complex-single-float (complex) () (:metaclass built-in-class))
(defclass sys.int::complex-double-float (complex) () (:metaclass built-in-class))

(defclass mezzano.simd:mmx-vector (t) () (:metaclass built-in-class))
(defclass mezzano.simd:sse-vector (t) () (:metaclass built-in-class))

;;; Done defining classes.
;;; Don't define any new classes in this file after this point!

(defun compute-primordial-slot-layout (class-name)
  (let* ((initargs (gethash class-name *primordial-class-table*))
         (layout (getf initargs :layout))
         (metaclass (getf (gethash class-name *primordial-class-table*) :metaclass)))
    (when (not layout)
      (let ((direct-slots (getf initargs :direct-slots)))
        (setf layout (append (loop
                                for direct-slot in direct-slots
                                when (eql (getf direct-slot :allocation :instance) :instance)
                                collect (getf direct-slot :name))
                             (loop
                                for super in (getf initargs :direct-superclasses)
                                append (loop
                                          for slot-name across (compute-primordial-slot-layout super)
                                          collect slot-name))))
        ;; Duplicate or overridden slots are not implemented.
        ;;(format t "~S: ~:S~%" class-name layout)
        (assert (eql (length layout)
                     (length (remove-duplicates layout))))
        (setf layout (make-array (length layout) :initial-contents layout))
        (setf (getf (gethash class-name *primordial-class-table*) :layout) layout)
        ;; Built-in classes don't have layouts.
        (when (not (eql metaclass 'built-in-class))
          (let* ((funcallable-offset (if (eql metaclass 'funcallable-standard-class)
                                         2 ; Skip the first two slots of funcallable instances, used for the function & entry point
                                         0))
                 (instance-slots (make-array (* (length layout) 2))))
            (loop
               for slot-index from (* funcallable-offset 8) by 8
               for i from 0 by 2
               for slot-name across layout
               do (setf (aref instance-slots i) slot-name
                        (aref instance-slots (1+ i)) (mezzano.runtime::make-location mezzano.runtime::+location-type-t+ slot-index)))
            (setf (getf (gethash class-name *primordial-class-table*) :instance-layout)
                  (sys.int::make-layout :class nil ; Fixed up later.
                                        :obsolete nil
                                        :heap-size (+ funcallable-offset (length layout))
                                        :heap-layout t
                                        :area nil
                                        :instance-slots instance-slots))))))
    layout))

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

(defun primordial-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (primordial-slot-value cpl-constituent 'direct-superclasses))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from primordial-tie-breaker-rule (car common))))))

(defun primordial-collect-superclasses (class)
  (list* class
         (union (primordial-slot-value class 'direct-superclasses)
                (reduce #'union
                        (loop
                           for superclass in (primordial-slot-value class 'direct-superclasses)
                           collect (primordial-collect-superclasses superclass))
                        :initial-value '()))))

(defun primordial-local-precedence-ordering (class)
  (mapcar #'list
          (cons class
                (butlast (primordial-slot-value class 'direct-superclasses)))
          (primordial-slot-value class 'direct-superclasses)))

(defun primordial-compute-class-precedence-list (class)
  (let ((classes-to-order (primordial-collect-superclasses class)))
    (topological-sort classes-to-order
                      (remove-duplicates
                        (mapappend #'primordial-local-precedence-ordering
                                   classes-to-order))
                      #'primordial-tie-breaker-rule)))

(defun primordial-compute-effective-slot-definition (class direct-slots)
  (declare (ignore class))
  (let ((initer (find-if-not #'null direct-slots
                             :key (lambda (def) (primordial-slot-value def 'initfunction)))))
    (primordial-make-instance 'standard-effective-slot-definition
                              :name (primordial-slot-value (first direct-slots) 'name)
                              :initform (if initer
                                            (primordial-slot-value initer 'initform)
                                            nil)
                              :initfunction (if initer
                                                (primordial-slot-value initer 'initfunction)
                                                nil)
                              ;; TODO: Should make sure type is consistent across direct slots.
                              :type (primordial-slot-value (first direct-slots) 'type)
                              :initargs (remove-duplicates
                                         (loop
                                            for direct-slot in direct-slots
                                            append (primordial-slot-value direct-slot 'initargs)))
                              ;; TODO: Should make sure allocation is consistent across direct slots.
                              :allocation (primordial-slot-value (first direct-slots) 'allocation)
                              :documentation (primordial-slot-value (first direct-slots) 'documentation))))

(defun primordial-compute-slots (class)
  (let* ((all-slots (mapappend (lambda (c) (primordial-slot-value c 'direct-slots))
                               (primordial-slot-value class 'class-precedence-list)))
         (all-names (remove-duplicates
                     (mapcar (lambda (def) (primordial-slot-value def 'name)) all-slots)))
         (effective-slots (mapcar (lambda (name)
                                    (primordial-compute-effective-slot-definition
                                     class
                                     (remove name all-slots
                                             :key (lambda (def) (primordial-slot-value def 'name))
                                             :test-not #'eq)))
                                  all-names))
         (metaclass (primordial-slot-value (primordial-class-of class) 'name)))
    (loop
       with next-instance-slot-index = (if (eql metaclass 'funcallable-standard-class)
                                           16 ; Skip the first two slots of funcallable instances, used for the function & entry point
                                           0)
       for slot in effective-slots
       do (case (primordial-slot-value slot 'allocation)
            (:instance
             (setf (primordial-slot-value slot 'location)
                   (mezzano.runtime::make-location mezzano.runtime::+location-type-t+ next-instance-slot-index))
             (incf next-instance-slot-index 8))
            (:class
             ;; Search through the precedence list looking for an existing effective slot.
             (dolist (super (rest (primordial-slot-value class 'class-precedence-list))
                      ;; None found, create a new slot.
                      (setf (primordial-slot-value slot 'location) (cons (primordial-slot-value slot 'name)
                                                                         *secret-unbound-value*)))
               (let ((existing (find (primordial-slot-value slot 'name)
                                     (primordial-slot-value super 'effective-slots)
                                     :key (lambda (x) (primordial-slot-value x 'name)))))
                 (when existing
                   (assert (consp (primordial-slot-value existing 'location)))
                   (setf (primordial-slot-value slot 'location) (primordial-slot-value existing 'location))
                   (return)))))
            (t
             (setf (primordial-slot-value slot 'location) nil))))
    effective-slots))

(defun primordial-compute-default-initargs (class)
  (let ((default-initargs '()))
    (dolist (c (primordial-slot-value class 'class-precedence-list))
      (loop
         for (initarg form fn) in (primordial-slot-value class 'direct-default-initargs)
         do (when (not (member initarg default-initargs :key #'first))
              (push (list initarg form fn) default-initargs))))
    (nreverse default-initargs)))

(defun finalize-primordial-class (class)
  (when (not (primordial-slot-value class 'finalized-p))
    (dolist (super (primordial-slot-value class 'direct-superclasses))
      (push class (primordial-slot-value super 'direct-subclasses))
      (finalize-primordial-class super))
    ;;(format t "Finalizing class ~S.~%" (primordial-slot-value class 'name))
    (setf (primordial-slot-value class 'class-precedence-list)
          (primordial-compute-class-precedence-list class))
    ;;(format t "  Class-Precedence-List: ~:S~%" (mapcar (lambda (x) (primordial-slot-value x 'name)) (primordial-slot-value class 'class-precedence-list)))
    (setf (primordial-slot-value class 'effective-slots)
          (primordial-compute-slots class))
    ;;(format t "  Slots: ~:S~%" (mapcar (lambda (x) (primordial-slot-value x 'name)) (primordial-slot-value class 'effective-slots)))
    (setf (primordial-slot-value class 'default-initargs)
          (primordial-compute-default-initargs class))
    (let ((instance-slots (remove-if-not (lambda (x) (eql (primordial-slot-value x 'allocation) :instance))
                                         (primordial-slot-value class 'effective-slots)))
          (layout (primordial-slot-value class 'slot-storage-layout)))
      ;; Check that the early layout and computed layout match up.
      (cond (layout
             (assert (eql (length instance-slots) (/ (length (sys.int::layout-instance-slots layout)) 2)))
             (loop
                for slot-definition in instance-slots
                for slot-name = (primordial-slot-value slot-definition 'name)
                for slot-location = (primordial-slot-location-in-layout layout slot-name)
                do
                  (when (not (eql (primordial-slot-value slot-definition 'location) slot-location))
                    (error "Instance slots and computed early layout mismatch in class ~S."
                           (primordial-slot-value class 'name)))))
            (t
             (assert (endp instance-slots)))))
    (setf (primordial-slot-value class 'finalized-p) t)))

(defun convert-primordial-direct-slot (direct-slot-definition)
  (apply #'primordial-make-instance 'standard-direct-slot-definition direct-slot-definition))

(defun convert-primordial-class (name)
  (let ((class (find-class name)))
    (destructuring-bind (&key name metaclass direct-superclasses direct-slots direct-default-initargs layout instance-layout)
        (gethash name *primordial-class-table*)
      #|
      (format t "Converting class ~S~%" name)
      (format t "  Metaclass: ~S~%" metaclass)
      (format t "  Direct-Superclasses: ~:S~%" direct-superclasses)
      (format t "  Direct-Slots: ~:S~%" direct-slots)
      (format t "  Direct-Default-Initargs: ~:S~%" direct-default-initargs)
      (format t "  Layout: ~:S~%" layout)
      |#
      (let ((converted-direct-slots (loop
                                       for direct-slot in direct-slots
                                       collect (convert-primordial-direct-slot direct-slot))))
        (primordial-initialize-instance metaclass
                                        class
                                        :name name
                                        :direct-superclasses (mapcar #'find-class direct-superclasses))
        (setf (primordial-slot-value class 'direct-slots) converted-direct-slots
              (primordial-slot-value class 'direct-default-initargs) direct-default-initargs
              (primordial-slot-value class 'slot-storage-layout) instance-layout)))))

(defun initialize-clos ()
  ;; Compute slot layouts for each class.
  ;;(format t "Computing slot layouts...~%")
  (maphash (lambda (name def)
             (declare (ignore def))
             (compute-primordial-slot-layout name))
           *primordial-class-table*)
  ;; Now that the layouts are known, the real class instances can be created.
  (maphash (lambda (name def)
             (let* ((mc-name (getf def :metaclass))
                    (mc (gethash mc-name *primordial-class-table*))
                    (mc-layout (getf mc :instance-layout)))
               (setf (find-class name) (sys.int::%allocate-instance mc-layout))))
           *primordial-class-table*)
  ;; Instance layouts can now be properly associated with their classes.
  (maphash (lambda (name def)
             (let ((layout (getf def :instance-layout)))
               (when layout
                 ;; The class slot of LAYOUT is read-only, hack around it...
                 (setf (sys.int::%object-ref-t layout 0) (find-class name)))))
           *primordial-class-table*)
  ;; Start defining real classes.
  ;;(format t "Transitioning to real class hierarchy.~%")
  ;; Initialize every real class object, initializing it and preparing for
  ;; finalization.
  (maphash (lambda (name def)
             (declare (ignore def))
             (convert-primordial-class name))
           *primordial-class-table*)
  ;; Now all classes have been initialized to the point where they
  ;; can be finalized.
  (maphash (lambda (name def)
             (declare (ignore def))
             (finalize-primordial-class (find-class name)))
           *primordial-class-table*)
  ;; Final setup.
  ;; Known important classes.
  (setf *the-class-standard-class* (find-class 'standard-class)
        *the-class-funcallable-standard-class* (find-class 'funcallable-standard-class)
        *the-layout-funcallable-standard-class* (primordial-slot-value *the-class-funcallable-standard-class* 'slot-storage-layout)
        *the-class-built-in-class* (find-class 'built-in-class)
        *the-layout-built-in-class* (primordial-slot-value *the-class-built-in-class* 'slot-storage-layout)
        *the-class-standard-direct-slot-definition* (find-class 'standard-direct-slot-definition)
        *the-layout-standard-direct-slot-definition* (primordial-slot-value *the-class-standard-direct-slot-definition* 'slot-storage-layout)
        *the-class-standard-effective-slot-definition* (find-class 'standard-effective-slot-definition)
        *the-class-standard-gf* (find-class 'standard-generic-function)
        *the-layout-standard-generic-function* (primordial-slot-value *the-class-standard-gf* 'slot-storage-layout)
        *the-class-standard-method* (find-class 'standard-method)
        *the-layout-standard-method* (primordial-slot-value *the-class-standard-method* 'slot-storage-layout)
        *the-class-t* (find-class 't))
  ;; Locations of important slots in metaobjects.
  (let ((s-c-layout (primordial-slot-value (find-class 'standard-class) 'slot-storage-layout)))
    (setf *the-layout-standard-class* s-c-layout)
    (setf *standard-class-effective-slots-location* (primordial-slot-location-in-layout s-c-layout 'effective-slots)
          *standard-class-slot-storage-layout-location* (primordial-slot-location-in-layout s-c-layout 'slot-storage-layout)
          *standard-class-hash-location* (primordial-slot-location-in-layout s-c-layout 'hash)
          *standard-class-finalized-p-location* (primordial-slot-location-in-layout s-c-layout 'finalized-p)
          *standard-class-precedence-list-location* (primordial-slot-location-in-layout s-c-layout 'class-precedence-list)
          *standard-class-direct-default-initargs-location* (primordial-slot-location-in-layout s-c-layout 'direct-default-initargs)
          *standard-class-default-initargs-location* (primordial-slot-location-in-layout s-c-layout 'default-initargs)))
  (let ((s-e-s-d-layout (primordial-slot-value (find-class 'standard-effective-slot-definition) 'slot-storage-layout)))
    (setf *the-layout-standard-effective-slot-definition* s-e-s-d-layout)
    (setf *standard-effective-slot-definition-name-location* (primordial-slot-location-in-layout s-e-s-d-layout 'name)
          *standard-effective-slot-definition-location-location* (primordial-slot-location-in-layout s-e-s-d-layout 'location))))

(initialize-clos)
