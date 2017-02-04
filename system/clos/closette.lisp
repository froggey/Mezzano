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

(in-package :mezzano.clos)

;;;
;;; Standard instances
;;;

(defun print-std-instance (instance stream depth)
  (declare (ignore depth))
  (print-object instance stream))

;;; Standard instance allocation

(defun instance-slot-p (slot)
  (eq (slot-definition-allocation slot) ':instance))

(defun std-allocate-instance (class)
  (ensure-class-finalized class)
  (allocate-std-instance
    class
    (allocate-slot-storage (count-if #'instance-slot-p (class-slots class))
                           *secret-unbound-value*)
    (class-slot-storage-layout class)))

(defun fc-std-allocate-instance (class)
  (ensure-class-finalized class)
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

(sys.int::defglobal *the-class-standard-class*)    ;standard-class's class metaobject
(sys.int::defglobal *the-class-funcallable-standard-class*)
(sys.int::defglobal *the-class-standard-direct-slot-definition*)
(sys.int::defglobal *the-class-standard-effective-slot-definition*)
(sys.int::defglobal *the-class-t*)
(sys.int::defglobal *standard-class-effective-slots-position*) ; Position of the effective-slots slot in standard-class.
(sys.int::defglobal *standard-class-slot-storage-layout-position*)
(sys.int::defglobal *standard-class-hash-position*)

(defun slot-location (class slot-name)
  (if (and (eq slot-name 'effective-slots)
           (eq class *the-class-standard-class*))
      *standard-class-effective-slots-position*
      (let ((slot (find slot-name
                        (class-slots class)
                        :key #'slot-definition-name)))
        (when (null slot)
          (return-from slot-location
            nil))
        (case (slot-definition-allocation slot)
          ((:instance :class)
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
          (values (slot-unbound (class-of instance)
                                instance
                                (slot-definition-name (elt (class-slots (class-of instance)) location))))
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

(defun find-effective-slot (object slot-name)
  (find slot-name (class-slots (class-of object))
        :key #'slot-definition-name))

(defun std-slot-value (instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (when (not location)
      (return-from std-slot-value
        (values (slot-missing (class-of instance) instance
                              slot-name 'slot-value))))
    (let ((val (slot-contents slots location)))
      (if (eq *secret-unbound-value* val)
          (values (slot-unbound (class-of instance) instance slot-name))
          val))))
(defun slot-value (object slot-name)
  (cond ((std-class-p (class-of (class-of object)))
         (std-slot-value object slot-name))
        (t
         (let ((slot (find-effective-slot object slot-name)))
           (if slot
               (slot-value-using-class (class-of object) object slot)
               (values (slot-missing (class-of object) object slot-name 'slot-value)))))))

(defun (setf std-slot-value) (value instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (when (not location)
      (slot-missing (class-of instance) instance slot-name 'setf value)
      (return-from std-slot-value
        value))
    (setf (slot-contents slots location) value)))
(defun (setf slot-value) (new-value object slot-name)
  (cond ((std-class-p (class-of (class-of object)))
         (setf (std-slot-value object slot-name) new-value))
        (t
         (let ((slot (find-effective-slot object slot-name)))
           (cond (slot
                  (setf (slot-value-using-class (class-of object) object slot) new-value))
                 (t
                  (slot-missing (class-of object) object slot-name 'setf new-value)
                  new-value))))))

(defun std-slot-boundp (instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (when (not location)
      (return-from std-slot-boundp
        (values (slot-missing (class-of instance) instance
                              slot-name 'slot-boundp))))
    (not (eq *secret-unbound-value* (slot-contents slots location)))))
(defun slot-boundp (object slot-name)
  (let ((metaclass (class-of (class-of object))))
    (cond ((std-class-p metaclass)
           (std-slot-boundp object slot-name))
          (t
           (let ((slot (find-effective-slot object slot-name)))
             (if slot
                 (slot-boundp-using-class (class-of object) object slot)
                 (values (slot-missing (class-of object) object slot-name 'slot-boundp))))))))

(defun std-slot-makunbound (instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (when (not location)
      (return-from std-slot-makunbound
        (values (slot-missing (class-of instance) instance
                              slot-name 'slot-makunbound))))
    (setf (slot-contents slots location) *secret-unbound-value*))
  instance)
(defun slot-makunbound (object slot-name)
  (let ((metaclass (class-of (class-of object))))
    (cond ((std-class-p metaclass)
           (std-slot-makunbound object slot-name))
          (t
           (let ((slot (find-effective-slot object slot-name)))
             (if slot
                 (slot-makunbound-using-class (class-of object) object slot)
                 (values (slot-missing (class-of object) object slot-name 'slot-makunbound))))))))

(defun slot-exists-p (instance slot-name)
  (not (null (find-effective-slot instance slot-name))))

(defun slot-exists-in-class-p (class slot-name)
  (not (null (find slot-name (class-slots class)
                   :key #'slot-definition-name))))

;;; class-of

(defun class-of (x)
  (cond ((std-instance-p x)
         (std-instance-class x))
        ((funcallable-std-instance-p x)
         (funcallable-std-instance-class x))
        (t (built-in-class-of x))))

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
    (symbol                                        (find-class-cached 'symbol))
    (integer                                       (find-class-cached 'integer))
    (float                                         (find-class-cached 'float))
    (complex                                       (find-class-cached 'complex))
    (ratio                                         (find-class-cached 'ratio))
    (cons                                          (find-class-cached 'cons))
    (character                                     (find-class-cached 'character))
    (simple-string                                 (find-class-cached 'simple-string))
    (string                                        (find-class-cached 'string))
    (bit-vector                                    (find-class-cached 'bit-vector))
    (simple-vector                                 (find-class-cached 'simple-vector))
    (vector                                        (find-class-cached 'vector))
    (array                                         (find-class-cached 'array))
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
    (cond ((std-class-p class-of-class)
           (svref (std-instance-slots class) *standard-class-hash-position*))
          (t (std-slot-value class 'hash)))))
(defun (setf class-hash) (new-value class)
  (setf (slot-value class 'hash) new-value))

(defun class-finalized-p (class)
  (std-slot-value class 'finalized-p))
(defun (setf class-finalized-p) (new-value class)
  (setf (slot-value class 'finalized-p) new-value))

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
                       &allow-other-keys)
  (apply #'ensure-class-using-class
         (find-class name nil)
         name
         all-keys))

;;; make-instance-standard-class creates and initializes an instance of
;;; standard-class without falling into method lookup.  However, it cannot be
;;; called until standard-class itself exists.

(defun default-direct-superclasses (metaclass)
  (cond ((or (eql metaclass *the-class-standard-class*)
             (subclassp metaclass *the-class-standard-class*))
         (list (find-class 'standard-object)))
        ((or (eql metaclass *the-class-funcallable-standard-class*)
             (subclassp metaclass *the-class-funcallable-standard-class*))
         (list (find-class 'funcallable-standard-object)))
        (t (error "Unsupported metaclass ~S." metaclass))))

(defun std-class-p (metaclass)
  "Returns true if METACLASS is either STANDARD-CLASS or FUNCALLABLE-STANDARD-CLASS."
  (or (eql metaclass *the-class-standard-class*)
      (eql metaclass *the-class-funcallable-standard-class*)))

(defun convert-to-direct-slot-definition (class canonicalized-slot)
  (apply #'make-instance
         (apply #'direct-slot-definition-class
                class canonicalized-slot)
         canonicalized-slot))

(defun std-after-initialization-for-classes
       (class &key direct-superclasses direct-slots direct-default-initargs &allow-other-keys)
  (when (endp direct-superclasses)
    (setf direct-superclasses (default-direct-superclasses (class-of class))))
  (dolist (superclass direct-superclasses)
    (when (not (validate-superclass class superclass))
      (error "Superclass ~S incompatible with class ~S." (class-of superclass) (class-of class))))
  (setf (class-direct-superclasses class) direct-superclasses)
  (dolist (superclass direct-superclasses)
    (push class (class-direct-subclasses superclass)))
  (let ((slots (mapcar (lambda (direct-slot)
                         (convert-to-direct-slot-definition class direct-slot))
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
  (maybe-finalize-inheritance class)
  (values))

(defun maybe-finalize-inheritance (class)
  "If CLASS is not finalized and can be finalized, finalize it, otherwise do nothing."
  (when (and (not (class-finalized-p class))
             (every #'class-finalized-p
                    (class-direct-superclasses class)))
    (finalize-inheritance class)))

(defun ensure-class-finalized (class)
  "If CLASS is not finalized, call FINALIZE-INHERITANCE on it."
  (when (not (class-finalized-p class))
    (finalize-inheritance class)))

;;; Slot definition metaobjects

(defun slot-definition-name (slot)
  (slot-value slot 'name))
(defun (setf slot-definition-name) (new-value slot)
  (setf (slot-value slot 'name) new-value))

(defun slot-definition-initfunction (slot)
  (slot-value slot 'initfunction))
(defun (setf slot-definition-initfunction) (new-value slot)
  (setf (slot-value slot 'initfunction) new-value))

(defun slot-definition-initform (slot)
  (slot-value slot 'initform))
(defun (setf slot-definition-initform) (new-value slot)
  (setf (slot-value slot 'initform) new-value))

(defun slot-definition-initargs (slot)
  (slot-value slot 'initargs))
(defun (setf slot-definition-initargs) (new-value slot)
  (setf (slot-value slot 'initargs) new-value))

(defun slot-definition-readers (slot)
  (slot-value slot 'readers))
(defun (setf slot-definition-readers) (new-value slot)
  (setf (slot-value slot 'readers) new-value))

(defun slot-definition-writers (slot)
  (slot-value slot 'writers))
(defun (setf slot-definition-writers) (new-value slot)
  (setf (slot-value slot 'writers) new-value))

(defun slot-definition-allocation (slot)
  (slot-value slot 'allocation))
(defun (setf slot-definition-allocation) (new-value slot)
  (setf (slot-value slot 'allocation) new-value))

(defun slot-definition-location (slot)
  (slot-value slot 'location))
(defun (setf slot-definition-location) (new-value slot)
  (setf (slot-value slot 'location) new-value))

(defun slot-definition-type (slot)
  (slot-value slot 'type))
(defun (setf slot-definition-type) (new-value slot)
  (setf (slot-value slot 'type) new-value))

(defun slot-definition-documentation (slot)
  (slot-value slot 'documentation))
(defun (setf slot-definition-documentation) (new-value slot)
  (setf (slot-value slot 'documentation) new-value))

;;; finalize-inheritance

(defun std-finalize-inheritance (class)
  (dolist (super (class-direct-superclasses class))
    (ensure-class-finalized super))
  (setf (class-precedence-list class) (compute-class-precedence-list class))
  (setf (class-slots class) (compute-slots class))
  (let* ((instance-slots (remove-if-not 'instance-slot-p
                                        (class-slots class)))
         (layout (make-array (length instance-slots))))
    (dolist (slot instance-slots)
      (setf (aref layout (slot-definition-location slot)) (slot-definition-name slot)))
    (setf (class-slot-storage-layout class) layout))
  (setf (class-finalized-p class) t)
  (values))

;;; Class precedence lists

(defun std-compute-class-precedence-list (class)
  (let ((classes-to-order (collect-superclasses* class)))
    (topological-sort classes-to-order
                      (remove-duplicates
                        (mapappend #'local-precedence-ordering
                                   classes-to-order))
                      #'std-tie-breaker-rule)))

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
                (compute-effective-slot-definition
                 class
                 name
                 (remove name all-slots
                         :key #'slot-definition-name
                         :test-not #'eq)))
            all-names)))

(defun std-compute-slot-layouts (class effective-slots)
  (loop
     with next-instance-slot-index = 0
     for slot in effective-slots
     do (case (slot-definition-allocation slot)
          (:instance
           (setf (slot-definition-location slot) next-instance-slot-index)
           (incf next-instance-slot-index))
          (:class
           ;; Walk up the precedence list looking for the nearest class that defines this slot.
           (dolist (super (class-precedence-list class)
                    (error "Unable to locate storage location for :class slot ~S in class ~S"
                           (slot-definition-name slot) (class-name class)))
             (let ((existing (find (slot-definition-name slot)
                                   (class-direct-slots super)
                                   :key #'slot-definition-name)))
               (when existing
                 (cond ((eql super class)
                        ;; This class defines the direct slot. Create a new cell to hold the value.
                        ;; (FIXME: Need to preserve the location over class redefinition.)
                        (setf (slot-definition-location slot) (cons (slot-definition-name slot) *secret-unbound-value*)))
                       (t
                        (let ((existing-effective (find (slot-definition-name slot)
                                                        (class-slots super)
                                                        :key #'slot-definition-name)))
                          (assert (consp (slot-definition-location existing-effective)))
                          (setf (slot-definition-location slot) (slot-definition-location existing-effective)))))
                 (return)))))
          (t
           (setf (slot-definition-location slot) nil)))))

(defun make-effective-slot-definition (class &rest initargs)
  (apply #'make-instance
         (apply #'effective-slot-definition-class class initargs)
         initargs))

(defun std-compute-effective-slot-definition (class name direct-slots)
  (let ((initer (find-if-not #'null direct-slots
                             :key #'slot-definition-initfunction)))
    (make-effective-slot-definition
     class
     :name name
     :initform (if initer
                   (slot-definition-initform initer)
                   nil)
     :initfunction (if initer
                       (slot-definition-initfunction initer)
                       nil)
     :initargs (remove-duplicates
                (mapappend #'slot-definition-initargs
                           direct-slots))
     :allocation (slot-definition-allocation (car direct-slots)))))

;;;
;;; Generic function metaobjects and standard-generic-function
;;;

(sys.int::defglobal *the-class-standard-gf*) ;standard-generic-function's class metaobject

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

(defun generic-function-argument-precedence-order (gf)
  (slot-value gf 'argument-precedence-order))
(defun (setf generic-function-argument-precedence-order) (new-value gf)
  (setf (slot-value gf 'argument-precedence-order) new-value))

(defun generic-function-declarations (gf)
  (slot-value gf 'declarations))
(defun (setf generic-function-declarations) (new-value gf)
  (setf (slot-value gf 'declarations) new-value))

;;; Internal accessor for effective method function table

(defun classes-to-emf-table (gf)
  (slot-value gf 'classes-to-emf-table))
(defun (setf classes-to-emf-table) (new-value gf)
  (setf (slot-value gf 'classes-to-emf-table) new-value))

(defun argument-reordering-table (gf)
  (slot-value gf 'argument-reordering-table))
(defun (setf argument-reordering-table) (value gf)
  (setf (slot-value gf 'argument-reordering-table) value))

;;;
;;; Method metaobjects and standard-method
;;;

(sys.int::defglobal *the-class-standard-method*)    ;standard-method's class metaobject

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
         (error 'sys.int::simple-program-error
                :format-control "~S names a special operator"
                :format-arguments (list function-name)))
        ((or (and (fboundp function-name)
                  (not (typep (fdefinition function-name) 'standard-generic-function)))
             (and (symbolp function-name)
                  (macro-function function-name)))
         (cerror "Clobber it" 'sys.int::simple-program-error
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
      (error 'sys.int::simple-program-error
             :format-control "Too few arguments to generic function ~S."
             :format-arguments (list gf)))
    (subseq args 0 number-required)))

(defun gf-required-arglist (gf)
  (let ((plist
          (analyze-lambda-list
            (generic-function-lambda-list gf))))
    (getf plist ':required-args)))

(defun gf-optional-arglist (gf)
  (let ((plist
          (analyze-lambda-list
            (generic-function-lambda-list gf))))
    (getf plist ':optional-args)))

(defun gf-rest-arg-p (gf)
  (let ((ll (generic-function-lambda-list gf)))
    (or (member '&rest ll)
        (member '&key ll))))

(defun finalize-generic-function (gf)
  (let* ((required-args (gf-required-arglist gf))
         (relevant-args (make-array (length required-args)
                                    :element-type 'bit
                                    :initial-element 0))
         (class-t (find-class 't))
         (argument-precedence (generic-function-argument-precedence-order gf)))
    (when (not (eql (length required-args)
                    (length (remove-duplicates required-args))))
      (error "Generic function has duplicate required argument in its lambda list ~S."
             (generic-function-lambda-list gf)))
    ;; Verify that the argument-precedence order is correct.
    (when (endp argument-precedence)
      (setf argument-precedence (copy-list required-args)
            (generic-function-argument-precedence-order gf) argument-precedence))
    (assert (eql (length required-args) (length argument-precedence)))
    (assert (endp (set-difference required-args argument-precedence)))
    (assert (endp (set-difference argument-precedence required-args)))
    (cond ((every #'eql required-args argument-precedence)
           ;; No argument reordering required.
           (setf (argument-reordering-table gf) nil))
          (t
           ;; Build a table that maps from reordered arguments to
           ;; required argument indices.
           (let ((table (make-array (length required-args))))
             (loop
                for i from 0
                for arg in argument-precedence
                  do (setf (aref table i) (position arg required-args)))
             (setf (argument-reordering-table gf) table))))
    ;; Examine all methods and compute the relevant argument bit-vector.
    (setf (generic-function-has-unusual-specializers gf) nil)
    (dolist (m (generic-function-methods gf))
      (do ((i 0 (1+ i))
           (spec (method-specializers m) (rest spec)))
          ((null spec))
        (unless (typep (first spec) 'class)
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
       (generic-function-class &key
                                 name
                                 lambda-list
                                 method-class
                                 documentation
                                 method-combination
                                 argument-precedence-order
                                 declarations)
  (declare (ignore generic-function-class))
  (let ((gf (fc-std-allocate-instance *the-class-standard-gf*)))
    (setf (generic-function-name gf) name)
    (setf (generic-function-lambda-list gf) lambda-list)
    (setf (generic-function-methods gf) ())
    (setf (generic-function-method-class gf) method-class)
    (setf (generic-function-method-combination gf) method-combination)
    (setf (generic-function-declarations gf) declarations)
    (setf (classes-to-emf-table gf) (make-hash-table))
    (setf (generic-function-argument-precedence-order gf) argument-precedence-order)
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

(defun check-method-lambda-list-congruence (gf method)
  "Ensure that the lambda lists of GF and METHOD are compatible."
  (let ((gf-ll (analyze-lambda-list (generic-function-lambda-list gf)))
        (method-ll (analyze-lambda-list (method-lambda-list method))))
    (assert (eql (length (getf gf-ll :required-args))
                 (length (getf method-ll :required-args)))
            (gf method)
            "Generic function ~S and method ~S have differing required arguments."
            gf method)
    (assert (eql (length (getf gf-ll :optional-args))
                 (length (getf method-ll :optional-args)))
            (gf method)
            "Generic function ~S and method ~S have differing optional arguments."
            gf method)
    (let ((gf-accepts-key-or-rest (or (getf gf-ll :rest-var)
                                      (member '&key (generic-function-lambda-list gf))))
          (method-accepts-key-or-rest (or (getf method-ll :rest-var)
                                          (member '&key (method-lambda-list method)))))
      (assert (or (and gf-accepts-key-or-rest
                       method-accepts-key-or-rest)
                  (and (not gf-accepts-key-or-rest)
                       (not method-accepts-key-or-rest)))
              (gf method)
            "Generic function ~S and method ~S differ in their acceptance of &KEY or &REST arguments."
            gf method))))

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
  (check-method-lambda-list-congruence gf method)
  (assert (not (method-generic-function method)))
  (let ((old-method
           (find-method gf (method-qualifiers method)
                           (method-specializers method) nil)))
    (when old-method (remove-method gf old-method)))
  (setf (method-generic-function method) gf)
  (push method (generic-function-methods gf))
  (dolist (specializer (method-specializers method))
    (when (typep specializer 'class)
      (pushnew method (class-direct-methods specializer))))
  (finalize-generic-function gf)
  gf)

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
        (remove method (generic-function-methods gf)))
  (setf (method-generic-function method) nil)
  (dolist (class (method-specializers method))
    (when (typep class 'class)
      (setf (class-direct-methods class)
            (remove method (class-direct-methods class)))))
  (finalize-generic-function gf)
  gf)

(defun find-method (gf qualifiers specializers
                    &optional (errorp t))
  (setf specializers (loop
                        for spec in specializers
                        collect (if (and (consp spec)
                                         (eql (first spec) 'eql))
                                    (intern-eql-specializer (second spec))
                                    spec)))
  (assert (eql (length specializers)
               (length (gf-required-arglist gf))))
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
                             :function (lambda (method next-emfun)
                                         (declare (ignore method next-emfun))
                                         (lambda (object)
                                           (slot-value object slot-name)))
                             :slot-definition slot-name))
  (values))

(defun add-writer-method (class fn-name slot-name)
  (add-method (ensure-generic-function fn-name :lambda-list '(new-value object))
              (make-instance (writer-method-class class slot-name :slot-definition slot-name)
                             :lambda-list '(new-value object)
                             :qualifiers ()
                             :specializers (list (find-class 't) class)
                             :function (lambda (method next-emfun)
                                         (declare (ignore method next-emfun))
                                         (lambda (new-value object)
                                           (setf (slot-value object slot-name) new-value)))
                             :slot-definition slot-name))
  (values))

;;;
;;; Generic function invocation
;;;

;;; compute-discriminating-function

(defun compute-reader-discriminator (gf emf-table argument-offset)
  (lambda (object)
    (let* ((class (class-of object))
           (location (single-dispatch-emf-entry emf-table class)))
      (if location
          (fast-slot-read object location)
          (slow-single-dispatch-method-lookup* gf argument-offset (list object) :reader)))))

(defun compute-writer-discriminator (gf emf-table argument-offset)
  (lambda (new-value object)
    (let* ((class (class-of object))
           (location (single-dispatch-emf-entry emf-table class)))
      (if location
          (fast-slot-write new-value object location)
          (slow-single-dispatch-method-lookup* gf argument-offset (list new-value object) :writer)))))

(defun compute-1-effective-discriminator (gf emf-table argument-offset)
  ;; Generate specialized dispatch functions for various combinations of
  ;; arguments.
  (macrolet ((gen-one (index n-required restp)
               (let ((req-args (loop
                                  for i below n-required
                                  collect (gensym)))
                     (rest-arg (when restp
                                 (gensym))))
                 `(when (and (eql ',index argument-offset)
                             (eql (length (gf-required-arglist gf)) ',n-required)
                             (eql (length (gf-optional-arglist gf)) '0)
                             (or (and ',restp (gf-rest-arg-p gf))
                                 (and (not ',restp) (not (gf-rest-arg-p gf)))))
                    (lambda (,@req-args ,@(if rest-arg
                                              `(&rest ,rest-arg)
                                              '()))
                      (declare (sys.int::lambda-name (1-effective-discriminator ,index ,n-required ,restp)))
                      (let* ((class (class-of ,(nth index req-args)))
                             (emfun (single-dispatch-emf-entry emf-table class)))
                        (if emfun
                            ,(if rest-arg
                                 `(apply emfun ,@req-args ,rest-arg)
                                 `(funcall emfun ,@req-args))
                            (slow-single-dispatch-method-lookup
                             gf
                             ,(if rest-arg
                                  `(list* ,@req-args ,rest-arg)
                                  `(list ,@req-args))
                             class)))))))
             (gen-all ()
               `(or
                 ,@(loop
                      for idx from 0 below 5
                      appending
                        (loop
                           for req from 1 to 5
                           collect `(gen-one ,idx ,req nil)
                           collect `(gen-one ,idx ,req t))))))
    (or (gen-all)
        (lambda (&rest args)
          (let* ((class (class-of (nth argument-offset args)))
                 (emfun (single-dispatch-emf-entry emf-table class)))
            (if emfun
                (apply emfun args)
                (slow-single-dispatch-method-lookup gf args class)))))))

(defun compute-n-effective-discriminator (gf emf-table n-required-args)
  (lambda (&rest args)
    (when (< (length args) n-required-args)
      (error 'sys.int::simple-program-error
             :format-control "Too few arguments to generic function ~S."
             :format-arguments (list gf)))
    (let* ((classes (mapcar #'class-of (subseq args 0 n-required-args)))
           (emfun (gethash classes emf-table nil)))
      (if emfun
          (apply emfun args)
          (slow-method-lookup gf args classes)))))

(defun slow-single-dispatch-method-lookup* (gf argument-offset args state)
  (let ((emf-table (classes-to-emf-table gf)))
    (ecase state
      (:reader
       (let* ((classes (mapcar #'class-of (required-portion gf args)))
              (class (nth argument-offset classes))
              (applicable-methods
               (if (eql (class-of gf) *the-class-standard-gf*)
                   (std-compute-applicable-methods-using-classes gf classes)
                   (compute-applicable-methods-using-classes gf classes))))
         (cond ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-reader-method)
                     (std-class-p (class-of class))
                     (slot-exists-in-class-p class (slot-value (first applicable-methods) 'slot-definition)))
                (let ((location (slot-location class (slot-value (first applicable-methods) 'slot-definition))))
                  (assert location)
                  (setf (single-dispatch-emf-entry emf-table class) location)
                  (pushnew gf (class-dependents class))
                  (fast-slot-read (first args) location)))
               (t ;; Give up and use the full path.
                (slow-single-dispatch-method-lookup* gf argument-offset args :never-called)))))
      (:writer
       (let* ((classes (mapcar #'class-of (required-portion gf args)))
              (class (nth argument-offset classes))
              (applicable-methods
               (if (eql (class-of gf) *the-class-standard-gf*)
                   (std-compute-applicable-methods-using-classes gf classes)
                   (compute-applicable-methods-using-classes gf classes))))
         (cond ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-writer-method)
                     (std-class-p (class-of class))
                     (slot-exists-in-class-p class (slot-value (first applicable-methods) 'slot-definition)))
                (let ((location (slot-location class (slot-value (first applicable-methods) 'slot-definition))))
                  (assert location)
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
               (if (eql (class-of gf) *the-class-standard-gf*)
                   (std-compute-applicable-methods-using-classes gf classes)
                   (compute-applicable-methods-using-classes gf classes))))
         (cond ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-reader-method)
                     (std-class-p (class-of class)))
                ;; Switch to reader-method.
                (setf (generic-function-discriminating-function gf)
                      (compute-reader-discriminator gf emf-table argument-offset))
                (set-funcallable-instance-function gf (generic-function-discriminating-function gf))
                (apply gf args))
               ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-writer-method)
                     (std-class-p (class-of class)))
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
      (if (eql (class-of gf) *the-class-standard-gf*)
          (std-compute-applicable-methods-using-classes gf classes)
          (compute-applicable-methods-using-classes gf classes))
    (unless validp
      (setf applicable-methods (if (eql (class-of gf) *the-class-standard-gf*)
                                   (std-compute-applicable-methods gf args)
                                   (compute-applicable-methods gf args))))
    (let ((emfun (cond (applicable-methods
                        (funcall
                         (if (eq (class-of gf) *the-class-standard-gf*)
                             #'std-compute-effective-method-function
                             #'compute-effective-method-function)
                         gf applicable-methods))
                       (t
                        (lambda (&rest args)
                          (apply #'no-applicable-method gf args))))))
      ;; Cache is only valid for non-eql methods.
      (when validp
        (setf (gethash classes (classes-to-emf-table gf)) emfun)
        (dolist (class classes)
          (pushnew gf (class-dependents class))))
      (apply emfun args))))

(defun slow-single-dispatch-method-lookup (gf args class)
  (let* ((classes (mapcar #'class-of
                          (required-portion gf args)))
         (applicable-methods
          (if (eql (class-of gf) *the-class-standard-gf*)
              (std-compute-applicable-methods-using-classes gf classes)
              (compute-applicable-methods-using-classes gf classes))))
    (let ((emfun (cond (applicable-methods
                        (std-compute-effective-method-function gf applicable-methods))
                       (t
                        (lambda (&rest args)
                          (apply #'no-applicable-method gf args))))))
      (setf (single-dispatch-emf-entry (classes-to-emf-table gf) class) emfun)
      (pushnew gf (class-dependents class))
      (apply emfun args))))

;;; compute-applicable-methods-using-classes

(defun std-compute-applicable-methods-using-classes (gf required-classes)
  (values (sort
           (copy-list
            (remove-if-not #'(lambda (method)
                               (every (lambda (class specializer)
                                        (etypecase specializer
                                          (eql-specializer
                                           (return-from std-compute-applicable-methods-using-classes
                                             (values nil nil)))
                                          (class
                                           (subclassp class specializer))))
                                      required-classes
                                      (method-specializers method)))
                           (generic-function-methods gf)))
           #'(lambda (m1 m2)
               (method-more-specific-p gf m1 m2 required-classes)))
          t))

(defun std-compute-applicable-methods (gf args)
  (sort
   (copy-list
    (remove-if-not #'(lambda (method)
                       (every (lambda (arg specializer)
                                (etypecase specializer
                                  (eql-specializer
                                   (eql (eql-specializer-object specializer) arg))
                                  (class
                                   (subclassp (class-of arg) specializer))))
                              args
                              (method-specializers method)))
                   (generic-function-methods gf)))
   #'(lambda (m1 m2)
       (method-more-specific-with-args-p gf m1 m2 args))))

;;; method-more-specific-p

(defun reorder-method-specializers (gf method-specializers)
  (let ((ordering-table (argument-reordering-table gf)))
    (cond (ordering-table
           (loop
              for index across ordering-table
              collect (nth index method-specializers)))
          (t
           method-specializers))))

(defun method-more-specific-p (gf method1 method2 required-classes)
  (loop
     for spec1 in (reorder-method-specializers gf (method-specializers method1))
     for spec2 in (reorder-method-specializers gf (method-specializers method2))
     for arg-class in (reorder-method-specializers gf required-classes)
     do (cond ((and (typep spec1 'eql-specializer)
                    (not (typep spec2 'eql-specializer)))
               (return t))
              ((and (typep spec1 'eql-specializer)
                    (typep spec2 'eql-specializer))
               (return nil))
              ((not (eq spec1 spec2))
               (return (sub-specializer-p spec1 spec2 arg-class))))
     finally (return nil)))

(defun method-more-specific-with-args-p (gf method1 method2 args)
  (loop
     for spec1 in (reorder-method-specializers gf (method-specializers method1))
     for spec2 in (reorder-method-specializers gf (method-specializers method2))
     for arg in (reorder-method-specializers gf args)
     do (cond ((and (typep spec1 'eql-specializer)
                    (not (typep spec2 'eql-specializer)))
               (return t))
              ((and (typep spec1 'eql-specializer)
                    (typep spec2 'eql-specializer))
               (return nil))
              ((not (eq spec1 spec2))
               (return (sub-specializer-p spec1 spec2 (class-of arg)))))
     finally (return nil)))

;;; compute-effective-method-function

(defun primary-method-p (method)
  (null (method-qualifiers method)))
(defun before-method-p (method)
  (equal '(:before) (method-qualifiers method)))
(defun after-method-p (method)
  (equal '(:after) (method-qualifiers method)))
(defun around-method-p (method)
  (equal '(:around) (method-qualifiers method)))

;;; compute an effective method function from a list of primary methods:

(defun method-fast-function (method next-emfun)
  (funcall (method-function method) method next-emfun))

(defun compute-primary-emfun (methods)
  (if (null methods)
      nil
      (let ((next-emfun (compute-primary-emfun (cdr methods))))
        (method-fast-function (car methods) next-emfun))))

(defun std-compute-effective-method-function-with-standard-method-combination (gf methods)
  (let ((primaries (remove-if-not #'primary-method-p methods))
        (around (find-if #'around-method-p methods)))
    (when (null primaries)
      (error "No applicable primary methods for the generic function ~S." gf))
    (if around
        (let ((next-emfun
                (funcall
                   (if (eq (class-of gf) *the-class-standard-gf*)
                       #'std-compute-effective-method-function
                       #'compute-effective-method-function)
                   gf (remove around methods))))
          (method-fast-function around next-emfun))
        (let ((primary (compute-primary-emfun primaries))
              (befores (mapcar (lambda (m) (method-fast-function m nil))
                               (remove-if-not #'before-method-p methods)))
              (reverse-afters (mapcar (lambda (m) (method-fast-function m nil))
                                      (reverse (remove-if-not #'after-method-p methods)))))
          (cond ((and befores reverse-afters)
                 (lambda (&rest args)
                   (declare (dynamic-extent args))
                   (dolist (before befores)
                     (apply before args))
                   (multiple-value-prog1
                       (apply primary args)
                     (dolist (after reverse-afters)
                       (apply after args)))))
                (befores
                 (lambda (&rest args)
                   (declare (dynamic-extent args))
                   (dolist (before befores)
                     (apply before args))
                   (apply primary args)))
                (reverse-afters
                 (lambda (&rest args)
                   (declare (dynamic-extent args))
                   (multiple-value-prog1
                       (apply primary args)
                     (dolist (after reverse-afters)
                       (apply after args)))))
                (t
                 primary))))))

(defun generate-method-combination-effective-method (name effective-method-body)
  (let ((method-args (gensym "ARGS"))
        (next-emfun (gensym "NEXT-EMFUN")))
    `(lambda (&rest ,method-args)
       (declare (sys.int::lambda-name (effective-method ,@name)))
       (macrolet ((call-method (method &optional next-method-list)
                    (when (listp method)
                      (assert (eql (first method) 'make-method)))
                    (cond ((listp method)
                           (assert (eql (first method) 'make-method))
                           (assert (eql (length method) 2))
                           (second method))
                          (t
                           `(apply (funcall ',(method-function method)
                                            ',method
                                            ,(if next-method-list
                                                 `(lambda (&rest ,',method-args)
                                                    (call-method ,(first next-method-list)
                                                                 ,(rest next-method-list)))
                                                 nil))
                                   ,',method-args))))
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
                                     (class
                                      (class-name specializer))
                                     (t specializer)))
                                 (method-specializers method))))
                 methods)))

(defun std-compute-effective-method (gf mc methods)
  (apply (method-combination-combiner (method-combination-object-method-combination mc))
         gf
         methods
         (method-combination-object-arguments mc)))

(defun std-compute-effective-method-function (gf methods)
  (let ((mc (generic-function-method-combination gf)))
    ;; FIXME: Still should call COMPUTE-EFFECTIVE-METHOD when
    ;; the generic function is not a standard-generic-function and mc is the standard method combination.
    (cond (mc
           (let* ((mc-object (method-combination-object-method-combination mc))
                  (mc-args (method-combination-object-arguments mc))
                  (effective-method-body (compute-effective-method gf mc methods))
                  (name (generate-method-combination-effective-method-name gf mc-object methods)))
             (eval (generate-method-combination-effective-method name effective-method-body))))
          (t
           (std-compute-effective-method-function-with-standard-method-combination
            gf methods)))))

;;;
;;; Bootstrap
;;;

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

(defgeneric slot-value-using-class (class instance slot))
(defmethod slot-value-using-class ((class std-class) instance (slot standard-effective-slot-definition))
  (std-slot-value instance (slot-definition-name slot)))

(defgeneric (setf slot-value-using-class) (new-value class instance slot))
(defmethod (setf slot-value-using-class) (new-value (class std-class) instance (slot standard-effective-slot-definition))
  (setf (std-slot-value instance (slot-definition-name slot)) new-value))

(defgeneric slot-boundp-using-class (class instance slot))
(defmethod slot-boundp-using-class ((class std-class) instance (slot standard-effective-slot-definition))
  (std-slot-boundp instance (slot-definition-name slot)))

(defgeneric slot-makunbound-using-class (class instance slot))
(defmethod slot-makunbound-using-class ((class std-class) instance (slot standard-effective-slot-definition))
  (std-slot-makunbound instance (slot-definition-name slot)))

;;; Stuff...

(defgeneric direct-slot-definition-class (class &rest initargs))
(defmethod direct-slot-definition-class ((class std-class) &rest initargs)
  *the-class-standard-direct-slot-definition*)

(defgeneric effective-slot-definition-class (class &rest initargs))
(defmethod effective-slot-definition-class ((class std-class) &rest initargs)
  *the-class-standard-effective-slot-definition*)

;;; Instance creation and initialization

(defgeneric allocate-instance (class &rest initargs &key &allow-other-keys))
(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (std-allocate-instance class))
(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (fc-std-allocate-instance class))

(defun std-compute-initargs (class initargs)
  (let ((default-initargs '()))
    (dolist (c (class-precedence-list class))
      (loop
         for (initarg form fn) in (class-direct-default-initargs c)
         do (when (and (not (member initarg initargs))
                       (not (member initarg default-initargs)))
              (push initarg default-initargs)
              (push (funcall fn) default-initargs))))
    (append initargs (nreverse default-initargs))))

(defgeneric make-instance (class &rest initargs &key &allow-other-keys))
(defmethod make-instance ((class std-class) &rest initargs)
  (let* ((true-initargs (std-compute-initargs class initargs))
         (instance (apply #'allocate-instance class true-initargs)))
    (apply #'initialize-instance instance true-initargs)
    instance))
(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(defgeneric initialize-instance (instance &key &allow-other-keys))
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &key &allow-other-keys))
(defmethod reinitialize-instance
           ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance () initargs))

(defgeneric shared-initialize (instance slot-names &key &allow-other-keys))
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

(defgeneric change-class (instance new-class &key &allow-other-keys))
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

(defgeneric update-instance-for-different-class (old new &key &allow-other-keys))
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

(defmethod print-object ((class class) stream)
  (print-unreadable-object (class stream :identity t)
    (format stream "~:(~S~) ~S"
            (class-name (class-of class))
            (class-name class)))
  class)

(defmethod print-object ((slot-definition standard-slot-definition) stream)
  (print-unreadable-object (slot-definition stream :type t :identity t)
    (format stream "~S" (slot-definition-name slot-definition))))

(defmethod initialize-instance :after ((class std-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))


(defgeneric reader-method-class (class direct-slot &rest initargs))
;; ### slot objects.
(defmethod reader-method-class ((class std-class) direct-slot &rest initargs)
  (find-class 'standard-reader-method))

(defgeneric writer-method-class (class direct-slot &rest initargs))
;; ### slot objects.
(defmethod writer-method-class ((class std-class) direct-slot &rest initargs)
  (find-class 'standard-writer-method))

;;; Finalize inheritance

(defgeneric finalize-inheritance (class))
(defmethod finalize-inheritance ((class std-class))
  (std-finalize-inheritance class)
  (values))

;;; Class precedence lists

(defgeneric compute-class-precedence-list (class))
(defmethod compute-class-precedence-list ((class std-class))
  (std-compute-class-precedence-list class))

;;; Slot inheritance

(defgeneric compute-slots (class))
(defmethod compute-slots ((class std-class))
  (std-compute-slots class))

(defmethod compute-slots :around ((class std-class))
  (let ((effective-slots (call-next-method)))
    (std-compute-slot-layouts class effective-slots)
    effective-slots))

(defgeneric compute-effective-slot-definition (class name direct-slots))
(defmethod compute-effective-slot-definition ((class std-class) name direct-slots)
  (std-compute-effective-slot-definition class name direct-slots))

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
                                  (class
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

(defgeneric compute-effective-method-function (gf methods))
(defmethod compute-effective-method-function ((gf standard-generic-function) methods)
  (std-compute-effective-method-function gf methods))

(defgeneric compute-effective-method (generic-function method-combination methods))
(defmethod compute-effective-method ((gf standard-generic-function) method-combination methods)
  (std-compute-effective-method gf method-combination methods))

(format t "Closette is a Knights of the Lambda Calculus production.~%")

;;; Metaclasses.

(defmethod update-instance-for-different-class :before
           ((old forward-referenced-class) (new standard-class) &rest initargs)
  (setf (class-direct-superclasses new) (list (find-class 'standard-class))))

(defmethod update-instance-for-different-class :before
           ((old forward-referenced-class) (new funcallable-standard-class) &rest initargs)
  (setf (class-direct-superclasses new) (list (find-class 'funcallable-standard-class))))

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

(defgeneric validate-superclass (class superclass))

(defmethod validate-superclass ((class class) (superclass class))
  (or (eql superclass *the-class-t*)
      (eql (class-of class) (class-of superclass))
      (and (eql (class-of class) *the-class-standard-class*)
           (eql (class-of superclass) *the-class-funcallable-standard-class*))
      (and (eql (class-of class) *the-class-funcallable-standard-class*)
           (eql (class-of superclass) *the-class-standard-class*))))

(defmethod validate-superclass ((class std-class) (superclass forward-referenced-class))
  t)

(defgeneric no-applicable-method (generic-function &rest function-arguments))

(defmethod no-applicable-method ((generic-function t) &rest function-arguments)
  (error "No applicable methods to generic function ~S (~S) when called with ~S."
         generic-function (generic-function-name generic-function) function-arguments))

;;; Built-in-class.

(defmethod initialize-instance :after ((class built-in-class) &rest args)
  (apply #'std-after-initialization-for-classes class args)
  (when (not (slot-boundp class 'prototype))
    (setf (slot-value class 'prototype) (std-allocate-instance class))))

(defmethod reinitialize-instance :before ((class built-in-class) &rest args)
  (error "Cannot reinitialize built-in classes."))

;;; Structure-class.

(defclass structure-class (clos-class)
  ((structure-definition :initarg :definition)))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (let* ((def (slot-value class 'structure-definition))
         (slots (sys.int::structure-slots def))
         (n-slots (length slots))
         (struct (sys.int::%make-struct (1+ n-slots)
                                        (sys.int::structure-area def))))
    (setf (sys.int::%struct-slot struct 0) def)
    struct))

(defmethod direct-slot-definition-class ((class structure-class) &rest initargs)
  *the-class-standard-direct-slot-definition*)
(defmethod effective-slot-definition-class ((class structure-class) &rest initargs)
  *the-class-standard-effective-slot-definition*)

(defmethod initialize-instance :after ((class structure-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

(defmethod reinitialize-instance :before ((class structure-class) &rest args)
  (error "Cannot reinitialize structure classes."))

(defmethod slot-value-using-class ((class structure-class) instance (slot standard-effective-slot-definition))
  (let ((def (slot-value class 'structure-definition))
        (slot-name (slot-definition-name slot)))
    (when (not (eql (sys.int::%object-ref-t instance 0) def))
      (error "Class for structure ~S or instance is outdated?" (class-name class)))
    (dolist (slot (sys.int::structure-slots def)
             (values (slot-missing class instance slot-name 'slot-value)))
      (when (eql (sys.int::structure-slot-name slot) slot-name)
        (return (funcall (sys.int::structure-slot-accessor slot) instance))))))

(defmethod (setf slot-value-using-class) (new-value (class structure-class) instance (slot standard-effective-slot-definition))
  (let ((def (slot-value class 'structure-definition))
        (slot-name (slot-definition-name slot)))
    (when (not (eql (sys.int::%object-ref-t instance 0) def))
      (error "Class for structure ~S or instance is outdated?" (class-name class)))
    (dolist (slot (sys.int::structure-slots def)
             (progn
               (slot-missing class instance slot-name 'setf new-value)
               new-value))
      (when (eql (sys.int::structure-slot-name slot) slot-name)
        (when (sys.int::structure-slot-read-only slot)
          (error "The slot ~S in class ~S is read-only." slot-name (class-name class)))
        (return (funcall `(setf ,(sys.int::structure-slot-accessor slot)) new-value instance))))))

(defmethod slot-boundp-using-class ((class structure-class) instance (slot standard-effective-slot-definition))
  t)

(defmethod slot-makunbound-using-class ((class structure-class) instance (slot standard-effective-slot-definition))
  (error "Cannot make structure slots unbound."))

(defmethod finalize-inheritance ((class structure-class))
  (std-finalize-inheritance class)
  (values))

(defmethod compute-slots ((class structure-class))
  (std-compute-slots class))
(defmethod compute-slots :around ((class structure-class))
  (let ((effective-slots (call-next-method)))
    (std-compute-slot-layouts class effective-slots)
    effective-slots))

(defmethod compute-class-precedence-list ((class structure-class))
  (std-compute-class-precedence-list class))

(defmethod compute-effective-slot-definition ((class structure-class) name direct-slots)
  (std-compute-effective-slot-definition class name direct-slots))

(defclass structure-object (t)
  ()
  (:metaclass structure-class))

;;; eql specializers.

(sys.int::defglobal *interned-eql-specializers* (make-hash-table))

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

(defun convert-direct-slot-definition-to-canonical-direct-slot (direct-slot)
  `(:name ,(slot-definition-name direct-slot)
    :allocation ,(slot-definition-allocation direct-slot)
    :type ,(slot-definition-type direct-slot)
    :initform ,(slot-definition-initform direct-slot)
    :initfunction ,(slot-definition-initfunction direct-slot)
    :initargs ,(slot-definition-initargs direct-slot)
    :documentation ,(slot-definition-documentation direct-slot)
    :readers ,(slot-definition-readers direct-slot)
    :writers ,(slot-definition-writers direct-slot)))

(defun std-after-reinitialization-for-classes (class &rest args &key &allow-other-keys)
  ;; Unfinalize the class.
  (setf (class-finalized-p class) nil)
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
  (apply #'std-after-initialization-for-classes
         class
         (append args
                 (list :direct-superclasses (class-direct-superclasses class))
                 (list :direct-slots (mapcar #'convert-direct-slot-definition-to-canonical-direct-slot (class-direct-slots class)))
                 (list :direct-default-initargs (class-direct-default-initargs class))))
  ;; Flush the EMF tables of generic functions.
  (dolist (gf (class-dependents class))
    (reset-gf-emf-table gf))
  ;; Refinalize any subclasses.
  (dolist (subclass (class-direct-subclasses class))
    (std-after-reinitialization-for-classes subclass)))

(defmethod reinitialize-instance :after ((class std-class) &rest args &key direct-superclasses &allow-other-keys)
  (apply #'std-after-reinitialization-for-classes class args))

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
  (error 'unbound-slot
         :name slot-name
         :instance instance))

(defgeneric class-prototype (class))

(defmethod class-prototype :before ((class class))
  (when (not (class-finalized-p class))
    (error "Class ~S has not been finalized." class)))

(defmethod class-prototype ((class clos-class))
  (when (not (slot-boundp class 'prototype))
    (setf (slot-value class 'prototype) (allocate-instance class)))
  (slot-value class 'prototype))

(defgeneric compute-applicable-methods-using-classes (generic-function classes))
(defmethod compute-applicable-methods-using-classes ((generic-function standard-generic-function) classes)
  (std-compute-applicable-methods-using-classes generic-function classes))

(defgeneric compute-applicable-methods (generic-function arguments))
(defmethod compute-applicable-methods ((generic-function standard-generic-function) arguments)
  (std-compute-applicable-methods generic-function arguments))

(defgeneric slot-missing (class object slot-name operation &optional new-value))
(defmethod slot-missing ((class t) object slot-name operation &optional new-value)
  (error "Slot ~S missing from class ~S when performing ~S." slot-name class operation))
