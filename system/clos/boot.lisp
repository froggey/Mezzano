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

(sys.int::defglobal *class-table* (make-hash-table :test #'eq))

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
                       all-keys))
         (class (allocate-std-instance 'primordial-class
                                       name
                                       nil)))
    (setf (gethash name *primordial-class-table*) slots)
    (setf (find-class name) class)))

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
         (class-layout (getf class-def :layout))
         (slots (make-array (length class-layout) :initial-element *secret-unbound-value*)))
    (setf (std-instance-slots instance) slots
          (std-instance-layout instance) class-layout)
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
       for slot-index from 0
       for slot-name across class-layout
       for slot-def = (get-primordial-slot-definition class-name slot-name)
       do
         (multiple-value-bind (init-key init-value foundp)
             (get-properties initargs (getf slot-def :initargs))
           (declare (ignore init-key))
           (cond (foundp
                  (setf (aref slots slot-index) init-value))
                 ((getf slot-def :initfunction)
                  (setf (aref slots slot-index) (funcall (getf slot-def :initfunction)))))))))


(defun primordial-make-instance (class-name &rest initargs)
  (let ((instance (allocate-std-instance (find-class class-name) nil nil)))
    (apply #'primordial-initialize-instance class-name instance initargs)
    instance))

(defun primordial-slot-value (object slot-name)
  (let ((value (svref (std-instance-slots object)
                      (position slot-name (std-instance-layout object)))))
    (when (eql value *secret-unbound-value*)
      (error "Slot ~S unbound in the object ~S." slot-name object))
    value))

(defun (setf primordial-slot-value) (value object slot-name)
  (setf (svref (std-instance-slots object)
               (position slot-name (std-instance-layout object)))
        value))

(defun primordial-slot-boundp (object slot-name)
  (not (eql (svref (std-instance-slots object)
                   (position slot-name (std-instance-layout object)))
            *secret-unbound-value*)))

;;; Define the standard class hierarchy.

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
   (argument-precedence-order
    :initarg :argument-precedence-order)
   (argument-reordering-table :initform nil)
   (declarations :initarg :declarations :initform nil)
   )
  (:default-initargs
   :name nil
    :lambda-list '()
    :method-class (find-class 'standard-method)
    :method-combination nil
    :argument-precedence-order '())
  (:metaclass funcallable-standard-class))

(defclass method (metaobject) ())

(defclass standard-method (method)
  ((lambda-list :initarg :lambda-list)     ; :accessor method-lambda-list
   (qualifiers :initarg :qualifiers)       ; :accessor method-qualifiers
   (specializers :initarg :specializers)   ; :accessor method-specializers
   (generic-function :initform nil)        ; :accessor method-generic-function
   (function :initarg :function))          ; :accessor method-function
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
  ((location :initarg :location)))

(defclass specializer (metaobject) ())

(defclass class (specializer) ())

(defclass forward-referenced-class (class)
  ((name :initarg :name)
   (direct-subclasses :initform '())
   (finalized-p :initform nil)))

(defclass clos-class (class)
  ((name :initarg :name)              ; :accessor class-name
   (direct-superclasses               ; :accessor class-direct-superclasses
    :initarg :direct-superclasses)
   (direct-slots :initform ())        ; :accessor class-direct-slots
   (class-precedence-list)            ; :accessor class-precedence-list
   (effective-slots :initform ())     ; :accessor class-slots
   (slot-storage-layout :initform ()) ; :accessor class-slot-storage-layout
   (direct-subclasses :initform ())   ; :accessor class-direct-subclasses
   (direct-methods :initform ())      ; :accessor class-direct-methods
   (direct-default-initargs :initform ()) ; :accessor class-direct-default-initargs
   (dependents :initform '())
   (hash :initform (next-class-hash-value))
   (finalized-p :initform nil)
   (prototype))
  (:default-initargs :name nil))

(defclass built-in-class (clos-class) ())

(defclass std-class (clos-class) ())
(defclass standard-class (std-class) ())
(defclass funcallable-standard-class (std-class) ())

(defclass function (t) () (:metaclass built-in-class))
(defclass symbol (t) () (:metaclass built-in-class))
(defclass character (t) () (:metaclass built-in-class))
;; FIXME: This should be a built-in class, but this is tricky to make work
;; with gray streams.
;; Specifically, FUNDAMENTAL-STREAM needs to inherit from STREAM, but
;; this will fail because BUILT-IN-CLASSes are not compatible with STANDARD-CLASSes.
;; One solution would be to just define early FUNDAMENTAL-STREAM here...
(defclass stream (t) ())
(defclass mezzano.supervisor:thread (t) () (:metaclass built-in-class))
(defclass sys.int::function-reference (t) () (:metaclass built-in-class))
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
(defclass string (vector) () (:metaclass built-in-class))
(defclass simple-string (string simple-array) () (:metaclass built-in-class))

(defclass number (t) () (:metaclass built-in-class))
(defclass real (number) () (:metaclass built-in-class))
(defclass rational (real) () (:metaclass built-in-class))
(defclass integer (rational) () (:metaclass built-in-class))
(defclass ratio (rational) () (:metaclass built-in-class))
(defclass float (real) () (:metaclass built-in-class))
(defclass complex (number) () (:metaclass built-in-class))


;;; Done defining classes.
;;; Don't define any new classes in this file after this point!

;;; Compute slot layouts for each class.

;;(format t "Computing slot layouts...~%")

(defun compute-primordial-slot-layout (class-name)
  (let* ((initargs (gethash class-name *primordial-class-table*))
         (layout (getf initargs :layout)))
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
        (setf (getf (gethash class-name *primordial-class-table*) :layout) layout)))
    layout))

(maphash (lambda (name def)
           (declare (ignore def))
           (compute-primordial-slot-layout name))
         *primordial-class-table*)

;;; Start defining real classes.

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
                                  all-names)))
    (loop
       with next-instance-slot-index = 0
       for slot in effective-slots
       do (case (primordial-slot-value slot 'allocation)
            (:instance
             (setf (primordial-slot-value slot 'location) next-instance-slot-index)
             (incf next-instance-slot-index))
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
    ;; Check that the early layout and computed layout match up.
    (let ((instance-slots (remove-if-not (lambda (x) (eql (primordial-slot-value x 'allocation) :instance))
                                         (primordial-slot-value class 'effective-slots)))
          (layout (primordial-slot-value class 'slot-storage-layout)))
      (assert (eql (length instance-slots) (length layout)))
      (loop
         for slot-definition in instance-slots
         for slot-name across layout
         do
           (when (not (eql (primordial-slot-value slot-definition 'name) slot-name))
             (error "Instance slots and computed early layout mismatch in class ~S."
                    (primordial-slot-value class 'name)))))
    (setf (primordial-slot-value class 'finalized-p) t)))

(defun convert-primordial-direct-slot (direct-slot-definition)
  (apply #'primordial-make-instance 'standard-direct-slot-definition direct-slot-definition))

(defun convert-primordial-class (name)
  (let ((class (find-class name)))
    (destructuring-bind (&key name metaclass direct-superclasses direct-slots direct-default-initargs layout)
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
        (setf (std-instance-class class) (find-class metaclass))
        (primordial-initialize-instance metaclass
                                        class
                                        :name name
                                        :direct-superclasses (mapcar #'find-class direct-superclasses))
        (setf (primordial-slot-value class 'direct-slots) converted-direct-slots
              (primordial-slot-value class 'direct-default-initargs) direct-default-initargs
              (primordial-slot-value class 'slot-storage-layout) layout)))))

;;(format t "Transitioning to real class hierarchy.~%")

;; Initialize every real class object, initializing it and preparing for
;; finalization.
(maphash (lambda (name def)
           (declare (ignore def))
           (convert-primordial-class name))
         *primordial-class-table*)

;; Now all classes have been initialized to the point where they can be finalized.
(maphash (lambda (name def)
           (declare (ignore def))
           (finalize-primordial-class (find-class name)))
         *primordial-class-table*)

;; Final setup..

;; Known important classes.
(setf *the-class-standard-class* (find-class 'standard-class)
      *the-class-funcallable-standard-class* (find-class 'funcallable-standard-class)
      *the-class-standard-direct-slot-definition* (find-class 'standard-direct-slot-definition)
      *the-class-standard-effective-slot-definition* (find-class 'standard-effective-slot-definition)
      *the-class-standard-gf* (find-class 'standard-generic-function)
      *the-class-standard-method* (find-class 'standard-method)
      *the-class-t* (find-class 't))

;; Positions of various slots in standard-class and funcallable-standard-class.
(let ((s-c-layout (primordial-slot-value (find-class 'standard-class) 'slot-storage-layout)))
  ;; Verify that standard-class and funcallable-standard-class have the same layout.
  (when (not (equalp s-c-layout
                     (primordial-slot-value (find-class 'funcallable-standard-class) 'slot-storage-layout)))
    (error (format nil "STANARD-CLASS and FUNCALLABLE-STANDARD-CLASS have different layouts.~%S-C: ~S~%F-S-C: ~S~%"
                   s-c-layout
                   (primordial-slot-value (find-class 'funcallable-standard-class) 'slot-storage-layout))))
  (setf *standard-class-effective-slots-position* (position 'effective-slots s-c-layout)
        *standard-class-slot-storage-layout-position* (position 'slot-storage-layout s-c-layout)
        *standard-class-hash-position* (position 'hash s-c-layout)))
