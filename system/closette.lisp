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

(defpackage :system.closette
  (:nicknames :sys.clos :clos)
  (:use :cl)
  (:import-from :sys.int
		:allocate-std-instance
		:std-instance-p
		:std-instance-class
		:std-instance-slots
                :allocate-funcallable-std-instance
                :funcallable-std-instance-p
                :funcallable-std-instance-function
                :funcallable-std-instance-class
                :funcallable-std-instance-slots))

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
          find-generic-function  ; Necessary artifact of this implementation

          metaobject specializer class
          structure-class structure-object
          intern-eql-specializer eql-specializer eql-specializer-object

          with-slots
          ))

(export exports)

)

;;;
;;; Utilities
;;;

;;; push-on-end is like push except it uses the other end:

(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

;;; (setf getf*) is like (setf getf) except that it always changes the list,
;;;              which must be non-nil.

(defun (setf getf*) (new-value plist key)
  (block body
    (do ((x plist (cddr x)))
      ((null x))
      (when (eq (car x) key)
        (setf (car (cdr x)) new-value)
        (return-from body new-value)))
    (push-on-end key plist)
    (push-on-end new-value plist)
    new-value))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; mapappend is like mapcar except that the results are appended together:

(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))

;;; mapplist is mapcar for property lists:

(defun mapplist (fun x)
  (if (null x)
      ()
      (cons (funcall fun (car x) (cadr x))
            (mapplist fun (cddr x)))))

)

;;;
;;; Standard instances
;;;

(defun print-std-instance (instance stream depth)
  (declare (ignore depth))
  (print-object instance stream))

;;; Standard instance allocation

(defparameter secret-unbound-value (list "slot unbound"))

(defun instance-slot-p (slot)
  (eq (slot-definition-allocation slot) ':instance))

(defun std-allocate-instance (class)
  (allocate-std-instance
    class
    (allocate-slot-storage (count-if #'instance-slot-p (class-slots class))
                           secret-unbound-value)))

(defun fc-std-allocate-instance (class)
  (allocate-funcallable-std-instance
   (lambda (&rest x)
     (declare (ignore x))
     (error "The function of this funcallable instance has not been set."))
    class
    (allocate-slot-storage (count-if #'instance-slot-p (class-slots class))
                           secret-unbound-value)))

(defun set-funcallable-instance-function (funcallable-instance function)
  (setf (funcallable-std-instance-function funcallable-instance) function))

;;; Simple vectors are used for slot storage.

(defun allocate-slot-storage (size initial-value)
  (make-array size :initial-element initial-value))

;;; Standard instance slot access

;;; N.B. The location of the effective-slots slots in the class metaobject for
;;; standard-class must be determined without making any further slot
;;; references.

(defvar the-slots-of-standard-class) ;standard-class's class-slots
(defvar the-class-standard-class)    ;standard-class's class metaobject
(defvar the-class-funcallable-standard-class)
(defvar *standard-class-effective-slots-position*) ; Position of the effective-slots slot in standard-class.
(defvar *standard-class-slot-cache-position*)

(defun slot-location (class slot-name)
  (if (and (eq slot-name 'effective-slots)
           (eq class the-class-standard-class))
      *standard-class-effective-slots-position*
      (let ((slot (find slot-name
                        (class-slots class)
                        :key #'slot-definition-name)))
        (if (null slot)
            (error "The slot ~S is missing from the class ~S."
                   slot-name class)
            (let ((pos (position slot
                                 (remove-if-not #'instance-slot-p
                                                (class-slots class)))))
               (if (null pos)
                   (error "The slot ~S is not an instance~@
                           slot in the class ~S."
                          slot-name class)
                   pos))))))

(defun slot-contents (slots location)
  (svref slots location))

(defun (setf slot-contents) (new-value slots location)
  (setf (svref slots location) new-value))

(defun fast-sv-position (value simple-vector)
  (dotimes (i (array-dimension simple-vector 0))
    (when (eq value (svref simple-vector i))
      (return i))))

(defun fast-slot-read (instance location)
  (let* ((slots (if (funcallable-std-instance-p instance)
                    (funcallable-std-instance-slots instance)
                    (std-instance-slots instance)))
         (val (slot-contents slots location)))
    (if (eq secret-unbound-value val)
        (error "The slot ~S is unbound in the object ~S."
               (slot-definition-name (elt (class-slots (class-of instance)) location))
               instance)
        val)))

(defun fast-slot-write (new-value instance location)
  (let* ((slots (if (funcallable-std-instance-p instance)
                    (funcallable-std-instance-slots instance)
                    (std-instance-slots instance))))
    (setf (slot-contents slots location) new-value)))

(defun std-slot-value (instance slot-name)
  (when (not (symbolp slot-name))
    (setf slot-name (slot-definition-name slot-name)))
  (flet ((cached-location ()
           (let ((cache (slot-contents (std-instance-slots (class-of instance))
                                       *standard-class-slot-cache-position*)))
             (when (simple-vector-p cache)
               (fast-sv-position slot-name cache)))))
    (let* ((location (or (cached-location)
                         (slot-location (class-of instance) slot-name)))
           (slots (if (funcallable-std-instance-p instance)
                      (funcallable-std-instance-slots instance)
                      (std-instance-slots instance)))
           (val (slot-contents slots location)))
      (if (eq secret-unbound-value val)
          (error "The slot ~S is unbound in the object ~S."
                 slot-name instance)
          val))))

(defun slot-value (object slot-name)
  (cond ((or (eq (class-of (class-of object)) the-class-standard-class)
             (eq (class-of (class-of object)) the-class-funcallable-standard-class))
         (std-slot-value object slot-name))
        (t (slot-value-using-class (class-of object) object slot-name))))

(defun (setf std-slot-value) (new-value instance slot-name)
  (when (not (symbolp slot-name))
    (setf slot-name (slot-definition-name slot-name)))
  (flet ((cached-location ()
           (let ((cache (slot-contents (std-instance-slots (class-of instance))
                                       *standard-class-slot-cache-position*)))
             (when (simple-vector-p cache)
               (fast-sv-position slot-name cache)))))
    (let* ((location (or (cached-location)
                         (slot-location (class-of instance) slot-name)))
           (slots (if (funcallable-std-instance-p instance)
                      (funcallable-std-instance-slots instance)
                      (std-instance-slots instance))))
      (setf (slot-contents slots location) new-value))))

(defun (setf slot-value) (new-value object slot-name)
  (cond ((or (eq (class-of (class-of object)) the-class-standard-class)
             (eq (class-of (class-of object)) the-class-funcallable-standard-class))
         (setf (std-slot-value object slot-name) new-value))
        (t (setf-slot-value-using-class
            new-value (class-of object) object slot-name))))

(defun std-slot-boundp (instance slot-name)
  (when (not (symbolp slot-name))
    (setf slot-name (slot-definition-name slot-name)))
  (let ((location (slot-location (class-of instance) slot-name))
        (slots (std-instance-slots instance)))
    (not (eq secret-unbound-value (slot-contents slots location)))))
(defun fc-std-slot-boundp (instance slot-name)
  (when (not (symbolp slot-name))
    (setf slot-name (slot-definition-name slot-name)))
  (let ((location (slot-location (class-of instance) slot-name))
        (slots (funcallable-std-instance-slots instance)))
    (not (eq secret-unbound-value (slot-contents slots location)))))
(defun slot-boundp (object slot-name)
  (cond ((eq (class-of (class-of object)) the-class-standard-class)
         (std-slot-boundp object slot-name))
        ((eq (class-of (class-of object)) the-class-funcallable-standard-class)
         (fc-std-slot-boundp object slot-name))
        (t (slot-boundp-using-class (class-of object) object slot-name))))

(defun std-slot-makunbound (instance slot-name)
  (when (not (symbolp slot-name))
    (setf slot-name (slot-definition-name slot-name)))
  (let ((location (slot-location (class-of instance) slot-name))
        (slots (std-instance-slots instance)))
    (setf (slot-contents slots location) secret-unbound-value))
  instance)
(defun fc-std-slot-makunbound (instance slot-name)
  (when (not (symbolp slot-name))
    (setf slot-name (slot-definition-name slot-name)))
  (let ((location (slot-location (class-of instance) slot-name))
        (slots (funcallable-std-instance-slots instance)))
    (setf (slot-contents slots location) secret-unbound-value))
  instance)
(defun slot-makunbound (object slot-name)
  (cond ((eq (class-of (class-of object)) the-class-standard-class)
         (std-slot-makunbound object slot-name))
        ((eq (class-of (class-of object)) the-class-funcallable-standard-class)
         (fc-std-slot-makunbound object slot-name))
        (t (slot-makunbound-using-class (class-of object) object slot-name))))

(defun std-slot-exists-p (instance slot-name)
  (when (not (symbolp slot-name))
    (setf slot-name (slot-definition-name slot-name)))
  (not (null (find slot-name (class-slots (class-of instance))
                   :key #'slot-definition-name))))
(defun slot-exists-p (object slot-name)
  (if (or (eq (class-of (class-of object)) the-class-standard-class)
          (eq (class-of (class-of object)) the-class-funcallable-standard-class))
      (std-slot-exists-p object slot-name)
      (slot-exists-p-using-class (class-of object) object slot-name)))

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
  (destructuring-bind (slot-name accessor-name initform type read-only atomic)
      slot
    (list :name slot-name
          :accessor-name accessor-name
          :type type
          :read-only-p read-only
          :atomic atomic)))

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
  (let* ((class (sys.int::structure-class struct-def)))
    ;; Lazily create classes for structs.
    (when (null class)
      (setf class (make-structure-class struct-def)
            (sys.int::structure-class struct-def) class))
    class))

(defun built-in-class-of (x)
  (typecase x
    (null                                          (find-class 'null))
    ;;((and symbol (not null))                       (find-class 'symbol))
    (symbol                                        (find-class 'symbol))
    ;;((complex *)                                   (find-class 'complex))
    ;;((integer * *)                                 (find-class 'integer))
    (integer                                       (find-class 'integer))
    ;;((float * *)                                   (find-class 'float))
    (float                                         (find-class 'float))
    (cons                                          (find-class 'cons))
    (character                                     (find-class 'character))
    ;;((and number (not (or integer complex float))) (find-class 'number))
    ;;((string *)                                    (find-class 'string))
    (simple-string                                 (find-class 'simple-string))
    (string                                        (find-class 'string))
    ;;((bit-vector *)                                (find-class 'bit-vector))
    (bit-vector                                    (find-class 'bit-vector))
    ;;((and (vector * *) (not (or string vector)))   (find-class 'vector))
    (simple-vector                                 (find-class 'simple-vector))
    (vector                                        (find-class 'vector))
    (array                                         (find-class 'array))
    ;;((and sequence (not (or vector list)))         (find-class 'sequence))
    (function                                      (find-class 'function))
    (sys.int::thread                               (find-class 'sys.int::thread))
    (sys.int::function-reference                   (find-class 'sys.int::function-reference))
    (structure-object
     (class-of-structure-definition (sys.int::%struct-slot x 0)))
    (t                                             (find-class 't))))

;;; subclassp and sub-specializer-p

(defun subclassp (c1 c2)
  (not (null (find c2 (class-precedence-list c1)))))

(defun sub-specializer-p (c1 c2 c-arg)
  (let ((cpl (class-precedence-list c-arg)))
    (not (null (find c2 (cdr (member c1 cpl)))))))

;;;
;;; Class metaobjects and standard-class
;;;

(defparameter the-defclass-standard-class  ;standard-class's defclass form
 '(defclass standard-class ()
      ((name :initarg :name)              ; :accessor class-name
       (direct-superclasses               ; :accessor class-direct-superclasses
        :initarg :direct-superclasses)
       (direct-slots)                     ; :accessor class-direct-slots
       (class-precedence-list)            ; :accessor class-precedence-list
       (effective-slots)                  ; :accessor class-slots
       (slot-position-cache :initform ()) ; :accessor class-slot-cache
       (direct-subclasses :initform ())   ; :accessor class-direct-subclasses
       (direct-methods :initform ())      ; :accessor class-direct-methods
       (direct-default-initargs :initform ())))) ; :accessor class-direct-default-initargs

(defparameter the-defclass-funcallable-standard-class
  '(defclass funcallable-standard-class ()
      ((name :initarg :name)              ; :accessor class-name
       (direct-superclasses               ; :accessor class-direct-superclasses
        :initarg :direct-superclasses)
       (direct-slots)                     ; :accessor class-direct-slots
       (class-precedence-list)            ; :accessor class-precedence-list
       (effective-slots)                  ; :accessor class-slots
       (slot-position-cache :initform ()) ; :accessor class-slot-cache
       (direct-subclasses :initform ())   ; :accessor class-direct-subclasses
       (direct-methods :initform ())      ; :accessor class-direct-methods
       (direct-default-initargs :initform ())))) ; :accessor class-direct-default-initargs

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

(defun class-slot-cache (class)
  (slot-value class 'slot-position-cache))
(defun (setf class-slot-cache) (new-value class)
  (setf (slot-value class 'slot-position-cache) new-value))

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

;;; defclass

(defmacro defclass (name direct-superclasses direct-slots
                    &rest options)
  `(ensure-class ',name
     :direct-superclasses
       ,(canonicalize-direct-superclasses direct-superclasses)
     :direct-slots
       ,(canonicalize-direct-slots direct-slots)
     ,@(canonicalize-defclass-options options)))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun canonicalize-direct-slots (direct-slots)
   `(list ,@(mapcar #'canonicalize-direct-slot direct-slots)))

(defun canonicalize-direct-slot (spec)
  (if (symbolp spec)
      `(list :name ',spec)
      (let ((name (car spec))
            (initfunction nil)
            (initform nil)
            (initargs ())
            (readers ())
            (writers ())
            (other-options ()))
        (do ((olist (cdr spec) (cddr olist)))
            ((null olist))
          (case (car olist)
            (:initform
             (setq initfunction
                   `(function (lambda () ,(cadr olist))))
             (setq initform `',(cadr olist)))
            (:initarg
             (push-on-end (cadr olist) initargs))
            (:reader
             (push-on-end (cadr olist) readers))
            (:writer
             (push-on-end (cadr olist) writers))
            (:accessor
             (push-on-end (cadr olist) readers)
             (push-on-end `(setf ,(cadr olist)) writers))
            (otherwise
             (push-on-end `',(car olist) other-options)
             (push-on-end `',(cadr olist) other-options))))
        `(list
           :name ',name
           ,@(when initfunction
               `(:initform ,initform
                 :initfunction ,initfunction))
           ,@(when initargs `(:initargs ',initargs))
           ,@(when readers `(:readers ',readers))
           ,@(when writers `(:writers ',writers))
           ,@other-options))))

(defun canonicalize-direct-superclasses (direct-superclasses)
  `(list ,@(mapcar #'canonicalize-direct-superclass direct-superclasses)))

(defun canonicalize-direct-superclass (class-name)
  `(find-class ',class-name))

(defun canonicalize-defclass-options (options)
  (mapappend #'canonicalize-defclass-option options))

(defun canonicalize-defclass-option (option)
  (case (car option)
    (:metaclass
      (list ':metaclass
       `(find-class ',(cadr option))))
    (:default-initargs
      (list
       ':direct-default-initargs
       `(list ,@(mapappend
                  #'(lambda (x) x)
                  (mapplist
                    #'(lambda (key value)
                        `(',key (lambda () ,value)))
                    (cdr option))))))
    (t (list `',(car option) `',(cadr option)))))

)

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

;;; Ensure class

(defun ensure-class (name &rest all-keys
                          &key (metaclass the-class-standard-class)
                          &allow-other-keys)
  (when (find-class name nil)
    (cerror "Smash the existing class." "Can't redefine the class named ~S." name))
  (let ((class (apply (cond ((eq metaclass the-class-standard-class)
                             #'make-instance-standard-class)
                            ((eq metaclass the-class-funcallable-standard-class)
                             #'make-instance-funcallable-standard-class)
                            (t #'make-instance))
                      metaclass :name name all-keys)))
    (setf (find-class name) class)
    class))

;;; make-instance-standard-class creates and initializes an instance of
;;; standard-class without falling into method lookup.  However, it cannot be
;;; called until standard-class itself exists.

(defun make-instance-standard-class
       (metaclass &key name direct-superclasses direct-slots
        direct-default-initargs
        &allow-other-keys)
  (declare (ignore metaclass))
  (let ((class (std-allocate-instance the-class-standard-class)))
    (setf (class-name class) name)
    (setf (class-direct-subclasses class) ())
    (setf (class-direct-methods class) ())
    (std-after-initialization-for-classes class
       :direct-slots direct-slots
       :direct-superclasses (or direct-superclasses
                                (list (find-class 'standard-object)))
       :direct-default-initargs direct-default-initargs)
    class))

(defun make-instance-funcallable-standard-class
       (metaclass &key name direct-superclasses direct-slots
        direct-default-initargs
        &allow-other-keys)
  (declare (ignore metaclass))
  (let ((class (std-allocate-instance the-class-funcallable-standard-class)))
    (setf (class-name class) name)
    (setf (class-direct-subclasses class) ())
    (setf (class-direct-methods class) ())
    (std-after-initialization-for-classes class
       :direct-slots direct-slots
       :direct-superclasses (or direct-superclasses
                                (list (find-class 'funcallable-standard-object)))
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
                             slot-properties))
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
  (funcall (if (or (eq (class-of class) the-class-standard-class)
                   (eq (class-of class) the-class-funcallable-standard-class))
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

;;; finalize-inheritance

(defun std-finalize-inheritance (class)
  (setf (class-precedence-list class)
        (funcall (if (or (eq (class-of class) the-class-standard-class)
                         (eq (class-of class) the-class-funcallable-standard-class))
                     #'std-compute-class-precedence-list
                     #'compute-class-precedence-list)
                 class))
  (setf (class-slots class)
        (funcall (if (or (eq (class-of class) the-class-standard-class)
                         (eq (class-of class) the-class-funcallable-standard-class))
                     #'std-compute-slots
                     #'compute-slots)
                 class))
  (let ((instance-slots (remove-if-not 'instance-slot-p
                                       (class-slots class))))
    (setf (class-slot-cache class)
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
                  (if (or (eq (class-of class) the-class-standard-class)
                          (eq (class-of class) the-class-funcallable-standard-class))
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
      :allocation (slot-definition-allocation (car direct-slots)))))

;;;
;;; Generic function metaobjects and standard-generic-function
;;;

(defparameter the-defclass-standard-generic-function
 '(defclass standard-generic-function ()
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
       )
      (:metaclass funcallable-standard-class)))

(defvar the-class-standard-gf) ;standard-generic-function's class metaobject

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

;;; Internal accessor for effective method function table

(defun classes-to-emf-table (gf)
  (slot-value gf 'classes-to-emf-table))
(defun (setf classes-to-emf-table) (new-value gf)
  (setf (slot-value gf 'classes-to-emf-table) new-value))

;;;
;;; Method metaobjects and standard-method
;;;

(defparameter the-defclass-standard-method
 '(defclass standard-method ()
   ((lambda-list :initarg :lambda-list)     ; :accessor method-lambda-list
    (qualifiers :initarg :qualifiers)       ; :accessor method-qualifiers
    (specializers :initarg :specializers)   ; :accessor method-specializers
    (generic-function :initform nil)        ; :accessor method-generic-function
    (function :initarg :function))))        ; :accessor method-function

(defvar the-class-standard-method)    ;standard-method's class metaobject
(defvar the-class-standard-reader-method)    ;standard-reader-method's class metaobject
(defvar the-class-standard-writer-method)    ;standard-writer-method's class metaobject

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

;;; defgeneric

(defmacro defgeneric (function-name lambda-list &rest options)
  (let ((decleration (when (and (listp (car options))
				(eq (caar options) 'declare))
		       (let ((decleration (car options)))
			 (setf options (cdr options))
			 decleration))))
    `(progn
       ,(substitute 'declaim 'declare decleration)
       (ensure-generic-function
	',function-name
	:lambda-list ',lambda-list
	,@(canonicalize-defgeneric-options options))
       ,@(defgeneric-methods function-name options))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun canonicalize-defgeneric-options (options)
  (mapappend #'canonicalize-defgeneric-option options))

(defun canonicalize-defgeneric-option (option)
  (case (car option)
    (:generic-function-class
      (list ':generic-function-class
            `(find-class ',(cadr option))))
    (:method-class
      (list ':method-class
            `(find-class ',(cadr option))))
    (:method '())
    (t (list `',(car option) `',(cadr option)))))

(defun defgeneric-methods (name options)
  (mapcar (lambda (x) (defgeneric-method name x))
          (remove :method options :key 'first :test-not 'eql)))

(defun defgeneric-method (name def)
  `(defmethod ,name ,(second def) ,@(cddr def)))

)

;;; find-generic-function looks up a generic function by name.  It's an
;;; artifact of the fact that our generic function metaobjects can't legally
;;; be stored a symbol's function value.

(defvar *generic-function-table* (make-hash-table :test #'equal))

(defun find-generic-function (symbol &optional (errorp t))
  (let ((gf (gethash symbol *generic-function-table* nil)))
    (if (and (null gf) errorp)
        (error "No generic function named ~S." symbol)
        gf)))

(defun (setf find-generic-function) (new-value symbol)
  (setf (gethash symbol *generic-function-table*) new-value))

(defun forget-all-generic-functions ()
  (clrhash *generic-function-table*)
  (values))

;;; ensure-generic-function

(defun ensure-generic-function
       (function-name
        &rest all-keys
        &key (generic-function-class the-class-standard-gf)
             (method-class the-class-standard-method)
        &allow-other-keys)
  (cond ((and (symbolp function-name)
              (special-operator-p function-name))
         ;; Can't override special operators.
         (error "~S names a special operator" function-name))
        ((or (and (fboundp function-name)
                  (not (find-generic-function function-name nil)))
             (and (symbolp function-name)
                  (macro-function function-name)))
         (cerror "Clobber it" 'simple-error
                 :format-control "~S is already defined as a non-generic function or macro."
                 :format-arguments (list function-name))
         ;; Make sure it is well and truely clobbered.
         (when (symbolp function-name)
           (setf (macro-function function-name) nil))
         (when (fboundp function-name)
           (fmakunbound function-name))))
  (if (find-generic-function function-name nil)
      (find-generic-function function-name)
      (let ((gf (apply (if (eq generic-function-class the-class-standard-gf)
                           #'make-instance-standard-generic-function
                           #'make-instance)
                       generic-function-class
                       :name function-name
                       :method-class method-class
                       all-keys)))
         (setf (find-generic-function function-name) gf)
         gf)))

(defun generic-function-single-dispatch-p (gf)
  "Returns true when the generic function only one non-t specialized argument and
has only has class specializer."
  (when (and (eq (class-of gf) the-class-standard-gf)
             (not (generic-function-has-unusual-specializers gf)))
    (let ((specializers (generic-function-relevant-arguments gf))
          (count 0)
          (offset 0))
      (dotimes (i (length specializers))
        (unless (zerop (aref specializers i))
          (setf offset i)
          (incf count)))
      (if (eql count 1)
          (values t offset)
          nil))))

;;; finalize-generic-function

;;; N.B. Same basic idea as finalize-inheritance.  Takes care of recomputing
;;; and storing the discriminating function, and clearing the effective method
;;; function table.

(defun finalize-generic-function (gf)
  ;; Examine all methods and compute the relevant argument bit-vector.
  (let* ((required-args (gf-required-arglist gf))
         (relevant-args (make-array (length required-args)
                                    ;:element-type 'bit
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
          (setf (aref relevant-args i) 1))))
    (setf (generic-function-relevant-arguments gf) relevant-args))
  (setf (classes-to-emf-table gf) (make-hash-table :test (if (generic-function-single-dispatch-p gf)
                                                             #'eq
                                                             #'equal)))
  (setf (generic-function-discriminating-function gf)
        (funcall (if (eq (class-of gf) the-class-standard-gf)
                     #'std-compute-discriminating-function
                     #'compute-discriminating-function)
                 gf))
  (set-funcallable-instance-function gf (generic-function-discriminating-function gf))
  (setf (fdefinition (generic-function-name gf)) gf)
  (values))

;;; make-instance-standard-generic-function creates and initializes an
;;; instance of standard-generic-function without falling into method lookup.
;;; However, it cannot be called until standard-generic-function exists.

(defun make-instance-standard-generic-function
       (generic-function-class &key name lambda-list method-class documentation)
  (declare (ignore generic-function-class))
  (let ((gf (fc-std-allocate-instance the-class-standard-gf)))
    (setf (generic-function-name gf) name)
    (setf (generic-function-lambda-list gf) lambda-list)
    (setf (generic-function-methods gf) ())
    (setf (generic-function-method-class gf) method-class)
    (finalize-generic-function gf)
    gf))

;;; defmethod

(defmacro defmethod (&rest args)
  (multiple-value-bind (function-name qualifiers
                        lambda-list specializers function)
        (parse-defmethod args)
    `(ensure-method (find-generic-function ',function-name)
       :lambda-list ',lambda-list
       :qualifiers ',qualifiers
       :specializers ,(canonicalize-specializers specializers)
       :function #',function)))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun canonicalize-specializers (specializers)
  `(list ,@(mapcar #'canonicalize-specializer specializers)))

(defun canonicalize-specializer (specializer)
  (cond ((and (listp specializer)
              (eql (length specializer) 2)
              (eql (first specializer) 'eql))
         `(intern-eql-specializer ,(second specializer)))
        ((symbolp specializer)
         `(find-class ',specializer))
        (t (error "Bad method specializer ~S." specializer))))

(defun parse-defmethod (args)
  (let ((fn-spec (car args))
        (qualifiers ())
        (specialized-lambda-list nil)
        (body ())
        (parse-state :qualifiers))
    (dolist (arg (cdr args))
       (ecase parse-state
         (:qualifiers
           (if (and (atom arg) (not (null arg)))
               (push-on-end arg qualifiers)
               (progn (setq specialized-lambda-list arg)
                      (setq parse-state :body))))
         (:body (push-on-end arg body))))
    (let ((lambda-list (extract-lambda-list specialized-lambda-list))
          (specializers (extract-specializers specialized-lambda-list)))
      (values fn-spec
              qualifiers
              lambda-list
              specializers
              (compute-method-lambda lambda-list qualifiers specializers body fn-spec)))))

(defun compute-method-lambda (lambda-list qualifiers specializers body fn-spec)
  (multiple-value-bind (forms declares docstring)
      (sys.int::parse-declares body :permit-docstring t)
    (let ((form (list* 'block
                       (if (consp fn-spec)
                           (cadr fn-spec)
                           fn-spec)
                       forms)))
      `(lambda (args next-emfun)
         (declare (system:lambda-name (defmethod ,fn-spec ,@qualifiers ,specializers)))
         (flet ((call-next-method (&rest cnm-args)
                  (if (null next-emfun)
                      (error "No next method.")
                      (funcall next-emfun (or cnm-args args))))
                (next-method-p ()
                  (not (null next-emfun))))
           (apply (lambda ,(kludge-arglist lambda-list)
                    (declare ,@declares)
                    ,form)
                  args))))))

;;; Several tedious functions for analyzing lambda lists

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

(defun extract-lambda-list (specialized-lambda-list)
  (let* ((plist (analyze-lambda-list specialized-lambda-list))
         (requireds (getf plist ':required-names))
         (rv (getf plist ':rest-var))
         (ks (getf plist ':key-args))
         (aok (getf plist ':allow-other-keys))
         (opts (getf plist ':optional-args))
         (auxs (getf plist ':auxiliary-args)))
    `(,@requireds
      ,@(if rv `(&rest ,rv) ())
      ,@(if (or ks aok) `(&key ,@ks) ())
      ,@(if aok '(&allow-other-keys) ())
      ,@(if opts `(&optional ,@opts) ())
      ,@(if auxs `(&aux ,@auxs) ()))))

(defun extract-specializers (specialized-lambda-list)
  (let ((plist (analyze-lambda-list specialized-lambda-list)))
    (getf plist ':specializers)))

(defun analyze-lambda-list (lambda-list)
  (labels ((make-keyword (symbol)
              (intern (symbol-name symbol)
                      (find-package 'keyword)))
           (get-keyword-from-arg (arg)
              (if (listp arg)
                  (if (listp (car arg))
                      (caar arg)
                      (make-keyword (car arg)))
                  (make-keyword arg))))
    (let ((keys ())           ; Just the keywords
          (key-args ())       ; Keywords argument specs
          (required-names ()) ; Just the variable names
          (required-args ())  ; Variable names & specializers
          (specializers ())   ; Just the specializers
          (rest-var nil)
          (optionals ())
          (auxs ())
          (allow-other-keys nil)
          (state :parsing-required))
      (dolist (arg lambda-list)
        (if (member arg lambda-list-keywords)
          (ecase arg
            (&optional
              (setq state :parsing-optional))
            (&rest
              (setq state :parsing-rest))
            (&key
              (setq state :parsing-key))
            (&allow-other-keys
              (setq allow-other-keys 't))
            (&aux
              (setq state :parsing-aux)))
          (case state
            (:parsing-required
             (push-on-end arg required-args)
             (if (listp arg)
                 (progn (push-on-end (car arg) required-names)
                        (push-on-end (cadr arg) specializers))
                 (progn (push-on-end arg required-names)
                        (push-on-end 't specializers))))
            (:parsing-optional (push-on-end arg optionals))
            (:parsing-rest (setq rest-var arg))
            (:parsing-key
             (push-on-end (get-keyword-from-arg arg) keys)
             (push-on-end arg key-args))
            (:parsing-aux (push-on-end arg auxs)))))
      (list  :required-names required-names
             :required-args required-args
             :specializers specializers
             :rest-var rest-var
             :keywords keys
             :key-args key-args
             :auxiliary-args auxs
             :optional-args optionals
             :allow-other-keys allow-other-keys))))

)

;;; ensure method

(defun ensure-method (gf &rest all-keys)
  (let ((new-method
           (apply
              (if (eq (generic-function-method-class gf)
                      the-class-standard-method)
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
  (let ((method (std-allocate-instance the-class-standard-method)))
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
           (location (gethash class emf-table nil)))
      (if location
          (fast-slot-read object location)
          (slow-single-dispatch-method-lookup* gf argument-offset (list object) :reader)))))

(defun compute-writer-discriminator (gf emf-table argument-offset)
  (lambda (new-value object) ;ehhh...
    (let* ((class (class-of object))
           (location (gethash class emf-table nil)))
      (if location
          (fast-slot-write new-value object location)
          (slow-single-dispatch-method-lookup* gf argument-offset (list new-value object) :writer)))))

(defun compute-1-effective-discriminator (gf emf-table argument-offset)
  (lambda (&rest args)
    (let* ((class (class-of (nth argument-offset args)))
           (emfun (gethash class emf-table nil)))
      (if emfun
          (funcall emfun args)
          (slow-single-dispatch-method-lookup gf args class)))))

(defun compute-n-effective-discriminator (gf emf-table n-required-args)
  (lambda (&rest args)
    (when (< (length args) n-required-args)
      (error "Too few arguments to generic function ~S." gf))
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
         (when (null applicable-methods)
           (error "No applicable methods to generic function ~S.
Dispatching on class ~S." gf (nth argument-offset args)))
         (cond ((and (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-reader-method)
                     (or (eql (class-of class) the-class-standard-class)
                         (eql (class-of class) the-class-funcallable-standard-class)))
                (let ((location (slot-location class (slot-value (first applicable-methods) 'slot-definition))))
                  (setf (gethash class emf-table) location)
                  (fast-slot-read (first args) location)))
               (t ;; Give up and use the full path.
                (slow-single-dispatch-method-lookup* gf argument-offset args :never-called)))))
      (:writer
       (let* ((classes (mapcar #'class-of (required-portion gf args)))
              (class (nth argument-offset classes))
              (applicable-methods
               (compute-applicable-methods-using-classes gf classes)))
         (when (null applicable-methods)
           (error "No applicable methods to generic function ~S.
Dispatching on class ~S." gf (nth argument-offset args)))
         (cond ((and (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-writer-method)
                     (or (eql (class-of class) the-class-standard-class)
                         (eql (class-of class) the-class-funcallable-standard-class)))
                (let ((location (slot-location class (slot-value (first applicable-methods) 'slot-definition))))
                  (setf (gethash class emf-table) location)
                  (fast-slot-write (first args) (second args) location)))
               (t ;; Give up and use the full path.
                (slow-single-dispatch-method-lookup* gf argument-offset args :never-called)))))
      (:never-called
       (clrhash emf-table)
       (let* ((classes (mapcar #'class-of (required-portion gf args)))
              (class (nth argument-offset classes))
              (applicable-methods
               (compute-applicable-methods-using-classes gf classes)))
         (when (null applicable-methods)
           (error "No applicable methods to generic function ~S.
Dispatching on class ~S." gf (nth argument-offset args)))
         (cond ((and (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-reader-method)
                     (or (eql (class-of class) the-class-standard-class)
                         (eql (class-of class) the-class-funcallable-standard-class)))
                ;; Switch to reader-method.
                (setf (generic-function-discriminating-function gf)
                      (compute-reader-discriminator gf emf-table argument-offset))
                (set-funcallable-instance-function gf (generic-function-discriminating-function gf))
                (apply gf args))
               ((and (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-writer-method)
                     (or (eql (class-of class) the-class-standard-class)
                         (eql (class-of class) the-class-funcallable-standard-class)))
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
    (when (null applicable-methods)
      (error "No applicable methods to generic function ~S.
Dispatching on classes ~S." gf classes))
    (let ((emfun (funcall
                  (if (eq (class-of gf) the-class-standard-gf)
                      #'std-compute-effective-method-function
                      #'compute-effective-method-function)
                  gf applicable-methods)))
      ;; Cache is only valid for non-eql methods.
      (when validp
        (setf (gethash classes (classes-to-emf-table gf)) emfun))
      (funcall emfun args))))

(defun slow-single-dispatch-method-lookup (gf args class)
  (let* ((classes (mapcar #'class-of
                          (required-portion gf args)))
         (applicable-methods
          (compute-applicable-methods-using-classes gf classes)))
    (when (null applicable-methods)
      (error "No applicable methods to generic function ~S.
Dispatching on class ~S." gf class))
    (let ((emfun (std-compute-effective-method-function gf applicable-methods)))
      (setf (gethash class (classes-to-emf-table gf)) emfun)
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
                (if (eq (class-of gf) the-class-standard-gf)
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
        (if (eq (class-of gf) the-class-standard-gf)
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

(defun std-compute-effective-method-function (gf methods)
  (let ((primaries (remove-if-not #'primary-method-p methods))
        (around (find-if #'around-method-p methods)))
    (when (null primaries)
      (error "No primary methods for the generic function ~S. Called with " gf))
    (if around
        (let ((next-emfun
                (funcall
                   (if (eq (class-of gf) the-class-standard-gf)
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

(progn  ; Extends to end-of-file (to avoid printing intermediate results).
(format t "Beginning to bootstrap Closette...")
(forget-all-classes)
(forget-all-generic-functions)
;; How to create the class hierarchy in 10 easy steps:
;; 1. Figure out standard-class's slots.
(setq the-slots-of-standard-class
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
              (nth 3 the-defclass-standard-class)))
(setf *standard-class-effective-slots-position*
      (position 'effective-slots the-slots-of-standard-class
                :key #'slot-definition-name))
(setf *standard-class-slot-cache-position*
      (position 'slot-position-cache the-slots-of-standard-class
                :key #'slot-definition-name))
;; 2. Create the standard-class metaobject by hand.
(setq the-class-standard-class
      (allocate-std-instance
         'tba
         (make-array (length the-slots-of-standard-class)
                     :initial-element secret-unbound-value)))
;; 3. Install standard-class's (circular) class-of link.
(setf (std-instance-class the-class-standard-class)
      the-class-standard-class)
;; (It's now okay to use class-... accessor).
;; 4. Fill in standard-class's class-slots.
(setf (class-slots the-class-standard-class) the-slots-of-standard-class)
;; (Skeleton built; it's now okay to call make-instance-standard-class.)
;; 5. Hand build the class t so that it has no direct superclasses.
(setf (find-class 't)
  (let ((class (std-allocate-instance the-class-standard-class)))
    (setf (class-name class) 't)
    (setf (class-direct-subclasses class) ())
    (setf (class-direct-superclasses class) ())
    (setf (class-direct-methods class) ())
    (setf (class-direct-slots class) ())
    (setf (class-direct-default-initargs class) ())
    (setf (class-precedence-list class) (list class))
    (setf (class-slot-cache class) (make-array 0))
    (setf (class-slots class) ())
    class))
;; (It's now okay to define subclasses of t.)
;; 6. Create the other superclass of standard-class (i.e., standard-object).
(defclass standard-object (t) ())
;; 7. Define the full-blown version of standard-class.
(setq the-class-standard-class (eval the-defclass-standard-class))
;; 8. Replace all (3) existing pointers to the skeleton with real one.
(setf (std-instance-class (find-class 't))
      the-class-standard-class)
(setf (std-instance-class (find-class 'standard-object))
      the-class-standard-class)
(setf (std-instance-class the-class-standard-class)
      the-class-standard-class)
;; (Clear sailing from here on in).
;; 9. Define the other built-in classes.
(defclass symbol (t) ())
(defclass sequence (t) ())
(defclass array (t) ())
(defclass number (t) ())
(defclass character (t) ())
(defclass function (t) ())
(defclass stream (t) ())
(defclass list (sequence) ())
(defclass null (symbol list) ())
(defclass cons (list) ())
(defclass simple-array (array) ())
(defclass vector (array sequence) ())
(defclass simple-vector (vector simple-array) ())
(defclass bit-vector (vector) ())
(defclass string (vector) ())
(defclass simple-string (string simple-array) ())
(defclass integer (number) ())
(defclass float (number) ())
(defclass sys.int::thread (t) ())
(defclass sys.int::function-reference (t) ())
;; 10. Define the other standard metaobject classes.
(setq the-class-funcallable-standard-class (eval the-defclass-funcallable-standard-class))
(defclass funcallable-standard-object (standard-object function)
  ()
  (:metaclass funcallable-standard-class))
(setq the-class-standard-gf (eval the-defclass-standard-generic-function))
(setq the-class-standard-method (eval the-defclass-standard-method))
(defclass standard-accessor-method (standard-method)
  ((slot-definition :initarg :slot-definition)))
(setq the-class-standard-reader-method (defclass standard-reader-method (standard-accessor-method) ()))
(setq the-class-standard-writer-method (defclass standard-writer-method (standard-accessor-method) ()))
;; Voila! The class hierarchy is in place.
(format t "Class hierarchy created.")
;; (It's now okay to define generic functions and methods.)

(fmakunbound 'print-object)
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
  (fc-std-slot-exists-p instance slot-name))

(defgeneric slot-boundp-using-class (class instance slot-name))
(defmethod slot-boundp-using-class
           ((class standard-class) instance slot-name)
  (std-slot-boundp instance slot-name))
(defmethod slot-boundp-using-class
           ((class funcallable-standard-class) instance slot-name)
  (fc-std-slot-boundp instance slot-name))

(defgeneric slot-makunbound-using-class (class instance slot-name))
(defmethod slot-makunbound-using-class
           ((class standard-class) instance slot-name)
  (std-slot-makunbound instance slot-name))
(defmethod slot-makunbound-using-class
           ((class funcallable-standard-class) instance slot-name)
  (fc-std-slot-makunbound instance slot-name))

;;; Instance creation and initialization

(defgeneric allocate-instance (class))
(defmethod allocate-instance ((class standard-class))
  (std-allocate-instance class))
(defmethod allocate-instance ((class funcallable-standard-class))
  (fc-std-allocate-instance class))

(defun std-compute-initargs (class initargs)
  (let ((default-initargs '()))
    (dolist (c (class-precedence-list class))
      (do ((i (class-direct-default-initargs c) (cddr i)))
          ((endp i))
        (when (and (not (member (first i) initargs))
                   (not (member (first i) default-initargs)))
          (push (first i) default-initargs)
          (push (funcall (second i)) default-initargs))))
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
            (generic-function-name gf)))
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
  (format t "A Closette object~
             ~%Printed representation: ~S~
             ~%Class: ~S~
             ~%Structure "
          object
          (class-of object))
  (dolist (sn (mapcar #'slot-definition-name
                      (class-slots (class-of object))))
    (if (slot-boundp object sn)
        (format t "~%    ~S <- ~S"
                sn
                (slot-value object sn))
        (format t "~%    ~S <- not bound" sn)))
  (values))

(format t "~%Closette is a Knights of the Lambda Calculus production.~%")

;;; Metaclasses.

(defclass metaobject () ())
(defclass specializer (metaobject) ())
(defclass class (specializer) ())

;;; Structure-class.

(defclass structure-class (class)
  ((name :initarg :name)              ; :accessor class-name
   (direct-superclasses               ; :accessor class-direct-superclasses
    :initarg :direct-superclasses)
   (direct-slots)                     ; :accessor class-direct-slots
   (class-precedence-list)            ; :accessor class-precedence-list
   (effective-slots)                  ; :accessor class-slots
   (slot-position-cache :initform ()) ; :accessor class-slot-cache
   (direct-subclasses :initform ())   ; :accessor class-direct-subclasses
   (direct-methods :initform ())      ; :accessor class-direct-methods
   (direct-default-initargs :initform ())
   (structure-definition :initarg :definition)))

(defmethod initialize-instance :after ((class structure-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

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

(values)) ;end progn

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
