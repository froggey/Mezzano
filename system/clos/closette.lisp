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
  (eq (safe-slot-definition-allocation slot) ':instance))

(defun std-allocate-instance (class)
  (ensure-class-finalized class)
  (let* ((layout (safe-class-slot-storage-layout class))
         (instance (sys.int::%allocate-instance layout)))
    (loop
       for i below (sys.int::layout-heap-size layout)
       do (setf (sys.int::%object-ref-t instance i) *secret-unbound-value*))
    instance))

(defun fc-std-allocate-instance (class)
  (ensure-class-finalized class)
  (let* ((layout (safe-class-slot-storage-layout class))
         (instance (sys.int::%allocate-funcallable-instance
                    (lambda (&rest x)
                      (declare (ignore x))
                      (error "The function of this funcallable instance has not been set."))
                    layout)))
    (loop
       ;; Leave the first two words alone. These hold the entry point & function.
       for i from 2 below (sys.int::layout-heap-size layout)
       do (setf (sys.int::%object-ref-t instance i) *secret-unbound-value*))
    instance))

(defun set-funcallable-instance-function (funcallable-instance function)
  (setf (sys.int::funcallable-instance-function funcallable-instance) function))

;;; Standard instance slot access

;;; N.B. The location of the effective-slots slots in the class metaobject for
;;; standard-class must be determined without making any further slot
;;; references.

(sys.int::defglobal *the-class-standard-class*)    ;standard-class's class metaobject
(sys.int::defglobal *the-layout-standard-class*)
(sys.int::defglobal *the-class-funcallable-standard-class*)
(sys.int::defglobal *the-layout-funcallable-standard-class*)
(sys.int::defglobal *the-class-built-in-class*)
(sys.int::defglobal *the-layout-built-in-class*)
(sys.int::defglobal *the-class-standard-direct-slot-definition*)
(sys.int::defglobal *the-layout-standard-direct-slot-definition*)
(sys.int::defglobal *the-class-standard-effective-slot-definition*)
(sys.int::defglobal *the-layout-standard-effective-slot-definition*)
(sys.int::defglobal *the-class-t*)
(sys.int::defglobal *the-class-standard-gf*) ;standard-generic-function's class metaobject
(sys.int::defglobal *the-layout-standard-generic-function*)
(sys.int::defglobal *the-class-standard-method*)    ;standard-method's class metaobject
(sys.int::defglobal *the-layout-standard-method*)
(sys.int::defglobal *standard-class-effective-slots-location*) ; Position of the effective-slots slot in standard-class.
(sys.int::defglobal *standard-class-slot-storage-layout-location*)
(sys.int::defglobal *standard-class-hash-location*)
(sys.int::defglobal *standard-class-finalized-p-location*)
(sys.int::defglobal *standard-class-precedence-list-location*)
(sys.int::defglobal *standard-class-direct-default-initargs-location*)
(sys.int::defglobal *standard-class-default-initargs-location*)
(sys.int::defglobal *standard-effective-slot-definition-location-location*)
(sys.int::defglobal *standard-effective-slot-definition-name-location*)

(defun standard-instance-access (instance location)
  (if (consp location)
      (cdr location)
      (sys.int::%object-ref-t instance location)))

(defun (setf standard-instance-access) (new-value instance location)
  (if (consp location)
      (setf (cdr location) new-value)
      (setf (sys.int::%object-ref-t instance location) new-value)))

(defun (sys.int::cas standard-instance-access) (old new instance location)
  (if (consp location)
      (sys.int::cas (cdr location) old new)
      (sys.int::cas (sys.int::%object-ref-t instance location) old new)))

;; Instance and funcallable instances are accessed in the same way,
;; these are provided for compatibility with other MOP implementations.
(defun funcallable-standard-instance-access (instance location)
  (standard-instance-access instance location))

(defun (setf funcallable-standard-instance-access) (value instance location)
  (setf (standard-instance-access instance location) value))

(defun (sys.int::cas funcallable-standard-instance-access) (old new instance location)
  (sys.int::cas (standard-instance-access instance location) old new))

(defun slot-location-using-layout (slot-name layout)
  "If SLOT-NAME names an instance slot in LAYOUT, return the location otherwise NIL"
  (loop
     with instance-slots = (sys.int::layout-instance-slots layout)
     for i from 0 below (length instance-slots) by 2
     when (eq (svref instance-slots i) slot-name)
     do (return (svref instance-slots (1+ i)))
     finally (return nil)))

;; TODO: This and FAST-SLOT-WRITE should use the correct slot access function.
;; It doesn't really matter though, as instances and funcallable instances
;; are all compatible.
(defun fast-slot-read (instance location)
  (multiple-value-bind (slots layout)
      ;; This is required in case the instance is obsolete.
      (fetch-up-to-date-instance-slots-and-layout instance)
    (declare (ignore layout))
    (let* ((val (standard-instance-access slots location)))
      (if (eq *secret-unbound-value* val)
          (values (slot-unbound (class-of instance)
                                instance
                                (if (consp location)
                                    (car location)
                                    (safe-slot-definition-name (elt (safe-class-slots (class-of instance)) location)))))
          val))))

(defun fast-slot-write (new-value instance location)
  (multiple-value-bind (slots layout)
      ;; This is required in case the instance is obsolete.
      (fetch-up-to-date-instance-slots-and-layout instance)
    (declare (ignore layout))
    (setf (standard-instance-access slots location) new-value)))

(defun fetch-up-to-date-instance-slots-and-layout (instance)
  (loop
     (let ((layout (sys.int::%instance-layout instance)))
       (cond ((sys.int::layout-p layout)
              (cond ((sys.int::layout-obsolete layout)
                     (update-instance-for-new-layout instance))
                    (t
                     (return (values instance layout)))))
             (t
              ;; Obsolete instance.
              ;; There should never be nested layers of obsolete instances.
              (let* ((real-instance (mezzano.runtime::obsolete-instance-layout-new-instance layout))
                     (real-layout (sys.int::%instance-layout real-instance)))
                ;; But it's possible that the layout of the new instance is obsolete
                (cond ((sys.int::layout-obsolete real-layout)
                       (update-instance-for-new-layout instance))
                      (t
                       (return (values real-instance real-layout))))))))))

(defun find-effective-slot (object slot-name)
  (find slot-name (safe-class-slots (class-of object))
        :key #'safe-slot-definition-name))

(defun slot-location-in-instance (instance slot-name)
  (multiple-value-bind (slots layout)
      (fetch-up-to-date-instance-slots-and-layout instance)
    (let ((location (slot-location-using-layout slot-name layout)))
      (cond (location
             (values slots location))
            (t
             ;; Unknown slot, fall back on SLOT-DEFINITION-LOCATION.
             ;; This can happen for class slots.
             (let ((effective-slot (find-effective-slot instance slot-name)))
               (cond (effective-slot
                      (values slots (safe-slot-definition-location effective-slot)))
                     (t
                      ;; No such slot, caller must call SLOT-MISSING.
                      (values nil nil)))))))))

(defun std-slot-value (instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (when (not location)
      (return-from std-slot-value
        (values (slot-missing (class-of instance) instance
                              slot-name 'slot-value))))
    (let ((val (standard-instance-access slots location)))
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

(defparameter *fast-slot-value-readers* (make-hash-table))

(defun fast-slot-value-reader (slot-name)
  (let ((gf (gethash slot-name *fast-slot-value-readers*)))
    (when (not gf)
      ;; Slot readers/writers require a direct-slot-definition.
      (let ((direct-slot (make-instance 'standard-direct-slot-definition
                                        :name slot-name)))
        (setf gf (make-instance 'standard-generic-function
                                :name `(slot-value ,slot-name)
                                :lambda-list '(object)
                                :method-class (find-class 'standard-method)))
        ;; Add methods for both standard-object and structure-object to force
        ;; the discriminator to take the 1-effective path instead of the
        ;; unspecialized path.
        ;; Only the 1-effective path supports optimized access, the
        ;; unspecialized path will end up calling slot-value again.
        (std-add-method gf
                        (make-instance 'standard-reader-method
                                       :lambda-list '(object)
                                       :qualifiers ()
                                       :specializers (list (find-class 'standard-object))
                                       :function (lambda (method next-emfun)
                                                   (declare (ignore method next-emfun))
                                                   (lambda (object)
                                                     (slot-value object slot-name)))
                                       :slot-definition direct-slot))
        (std-add-method gf
                        (make-instance 'standard-reader-method
                                       :lambda-list '(object)
                                       :qualifiers ()
                                       :specializers (list (find-class 'structure-object))
                                       :function (lambda (method next-emfun)
                                                   (declare (ignore method next-emfun))
                                                   (lambda (object)
                                                     (slot-value object slot-name)))
                                       :slot-definition direct-slot))
        (setf (gethash slot-name *fast-slot-value-readers*) gf)))
    gf))

(define-compiler-macro slot-value (&whole whole object slot-name)
  (cond ((typep slot-name '(cons (eql quote) (cons symbol null)))
         `(funcall (load-time-value (fast-slot-value-reader ,slot-name))
                   ,object))
        (t
         whole)))

(defun (setf std-slot-value) (value instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (when (not location)
      (slot-missing (class-of instance) instance slot-name 'setf value)
      (return-from std-slot-value
        value))
    (setf (standard-instance-access slots location) value)))
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

(defparameter *fast-slot-value-writers* (make-hash-table))

(defun fast-slot-value-writer (slot-name)
  (let ((gf (gethash slot-name *fast-slot-value-writers*)))
    (when (not gf)
      ;; Slot readers/writers require a direct-slot-definition.
      (let ((direct-slot (make-instance 'standard-direct-slot-definition
                                        :name slot-name)))
        (setf gf (make-instance 'standard-generic-function
                                :name `((setf slot-value) ,slot-name)
                                :lambda-list '(value object)
                                :method-class (find-class 'standard-method)))
        (std-add-method gf
                        (make-instance 'standard-writer-method
                                       :lambda-list '(value object)
                                       :qualifiers ()
                                       :specializers (list *the-class-t* (find-class 'standard-object))
                                       :function (lambda (method next-emfun)
                                                   (declare (ignore method next-emfun))
                                                   (lambda (value object)
                                                     (setf (slot-value object slot-name) value)))
                                       :slot-definition direct-slot))
        (std-add-method gf
                        (make-instance 'standard-writer-method
                                       :lambda-list '(value object)
                                       :qualifiers ()
                                       :specializers (list *the-class-t* (find-class 'structure-object))
                                       :function (lambda (method next-emfun)
                                                   (declare (ignore method next-emfun))
                                                   (lambda (value object)
                                                     (setf (slot-value object slot-name) value)))
                                       :slot-definition direct-slot))
        (setf (gethash slot-name *fast-slot-value-writers*) gf)))
    gf))

(define-compiler-macro (setf slot-value) (&whole whole value object slot-name)
  (cond ((typep slot-name '(cons (eql quote) (cons symbol null)))
         `(funcall (load-time-value (fast-slot-value-writer ,slot-name))
                   ,value
                   ,object))
        (t
         whole)))

(defun (sys.int::cas std-slot-value) (old new instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (when (not location)
      (return-from std-slot-value
        (slot-missing (class-of instance) instance slot-name 'sys.int::cas (list old new))))
    (sys.int::cas (standard-instance-access slots location) old new)))
(defun (sys.int::cas slot-value) (old new object slot-name)
  (cond ((std-class-p (class-of (class-of object)))
         (sys.int::cas (std-slot-value object slot-name) old new))
        (t
         (let ((slot (find-effective-slot object slot-name)))
           (cond (slot
                  (sys.int::cas (slot-value-using-class (class-of object) object slot) old new))
                 (t
                  (values (slot-missing (class-of object) object slot-name 'sys.int::cas (list old new)))))))))

(defun std-slot-boundp (instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (when (not location)
      (return-from std-slot-boundp
        (values (slot-missing (class-of instance) instance
                              slot-name 'slot-boundp))))
    (not (eq *secret-unbound-value* (standard-instance-access slots location)))))
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
    (setf (standard-instance-access slots location) *secret-unbound-value*))
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
  (not (null (find slot-name (safe-class-slots class)
                   :key #'safe-slot-definition-name))))

;;; class-of

(defun class-of (x)
  (cond ((or (sys.int::instance-p x)
             (sys.int::funcallable-instance-p x))
         (let ((layout (sys.int::%instance-layout x)))
           (cond ((sys.int::layout-p layout)
                  (let ((class (sys.int::layout-class layout)))
                    (if (sys.int::structure-definition-p class)
                        (class-of-structure-definition class)
                        class)))
                 (t
                  ;; Obsolete instance.
                  (class-of
                   (mezzano.runtime::obsolete-instance-layout-new-instance
                    layout))))))
        (t
         (built-in-class-of x))))

(defun canonicalize-struct-slot (slot)
  (list :name (sys.int::structure-slot-definition-name slot)
        :accessor-name (sys.int::structure-slot-definition-accessor slot)
        :type (sys.int::structure-slot-definition-type slot)
        :read-only-p (sys.int::structure-slot-definition-read-only slot)))

(defun canonicalize-struct-slots (struct-def)
  (mapcar 'canonicalize-struct-slot (sys.int::structure-definition-slots struct-def)))

(defun make-structure-class (struct-def)
  (make-instance 'structure-class
                 :name (sys.int::structure-definition-name struct-def)
                 :definition struct-def
                 :direct-slots (canonicalize-struct-slots struct-def)
                 :direct-superclasses (list (if (sys.int::structure-definition-parent struct-def)
                                                (class-of-structure-definition (sys.int::structure-definition-parent struct-def))
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
    (fixnum                                        (find-class-cached 'fixnum))
    (bignum                                        (find-class-cached 'bignum))
    (single-float                                  (find-class-cached 'single-float))
    (double-float                                  (find-class-cached 'double-float))
    ((complex single-float)                        (find-class-cached 'sys.int::complex-single-float))
    ((complex double-float)                        (find-class-cached 'sys.int::complex-double-float))
    ((complex rational)                            (find-class-cached 'sys.int::complex-rational))
    (ratio                                         (find-class-cached 'ratio))
    (cons                                          (find-class-cached 'cons))
    (character                                     (find-class-cached 'character))
    (simple-string                                 (find-class-cached 'simple-string))
    (string                                        (find-class-cached 'string))
    (simple-bit-vector                             (find-class-cached 'simple-bit-vector))
    (bit-vector                                    (find-class-cached 'bit-vector))
    (simple-vector                                 (find-class-cached 'simple-vector))
    ((simple-array fixnum (*))                     (find-class-cached 'sys.int::simple-array-fixnum))
    ((simple-array (unsigned-byte 2) (*))          (find-class-cached 'sys.int::simple-array-unsigned-byte-2))
    ((simple-array (unsigned-byte 4) (*))          (find-class-cached 'sys.int::simple-array-unsigned-byte-4))
    ((simple-array (unsigned-byte 8) (*))          (find-class-cached 'sys.int::simple-array-unsigned-byte-8))
    ((simple-array (unsigned-byte 16) (*))         (find-class-cached 'sys.int::simple-array-unsigned-byte-16))
    ((simple-array (unsigned-byte 32) (*))         (find-class-cached 'sys.int::simple-array-unsigned-byte-32))
    ((simple-array (unsigned-byte 64) (*))         (find-class-cached 'sys.int::simple-array-unsigned-byte-64))
    ((simple-array (signed-byte 1) (*))            (find-class-cached 'sys.int::simple-array-signed-byte-1))
    ((simple-array (signed-byte 2) (*))            (find-class-cached 'sys.int::simple-array-signed-byte-2))
    ((simple-array (signed-byte 4) (*))            (find-class-cached 'sys.int::simple-array-signed-byte-4))
    ((simple-array (signed-byte 8) (*))            (find-class-cached 'sys.int::simple-array-signed-byte-8))
    ((simple-array (signed-byte 16) (*))           (find-class-cached 'sys.int::simple-array-signed-byte-16))
    ((simple-array (signed-byte 32) (*))           (find-class-cached 'sys.int::simple-array-signed-byte-32))
    ((simple-array (signed-byte 64) (*))           (find-class-cached 'sys.int::simple-array-signed-byte-64))
    ((simple-array single-float (*))               (find-class-cached 'sys.int::simple-array-single-float))
    ((simple-array double-float (*))               (find-class-cached 'sys.int::simple-array-double-float))
    ((simple-array (complex single-float) (*))     (find-class-cached 'sys.int::simple-array-complex-single-float))
    ((simple-array (complex double-float) (*))     (find-class-cached 'sys.int::simple-array-complex-double-float))
    (vector                                        (find-class-cached 'vector))
    (array                                         (find-class-cached 'array))
    (mezzano.delimited-continuations:delimited-continuation
     (find-class-cached 'mezzano.delimited-continuations:delimited-continuation))
    (sys.int::closure                              (find-class-cached 'sys.int::closure))
    (compiled-function                             (find-class-cached 'compiled-function))
    (function                                      (find-class-cached 'function))
    (sys.int::function-reference                   (find-class-cached 'sys.int::function-reference))
    (sys.int::weak-pointer                         (find-class-cached 'sys.int::weak-pointer))
    ;; FIXME: Should return LARGE-BYTE for large bytes.
    ;; LARGE-BYTE needs to be a subclass of BYTE.
    (byte                                          (find-class-cached 'byte))
    (mezzano.simd:mmx-vector                       (find-class-cached 'mezzano.simd:mmx-vector))
    (mezzano.simd:sse-vector                       (find-class-cached 'mezzano.simd:sse-vector))
    (mezzano.runtime::symbol-value-cell            (find-class-cached 'mezzano.runtime::symbol-value-cell))
    ;; TODO: Replace this with an error. Every object in the system should
    ;; have a sensible class.
    (t                                             (find-class-cached 't))))

;;; subclassp and sub-specializer-p

(defun subclassp (c1 c2)
  (not (null (find c2 (safe-class-precedence-list c1)))))

(defun sub-specializer-p (c1 c2 c-arg)
  (let ((cpl (safe-class-precedence-list c-arg)))
    (not (null (find c2 (cdr (member c1 cpl)))))))

;;;
;;; Class metaobjects and standard-class
;;;

(defun safe-class-default-initargs (class)
  (cond ((standard-class-instance-p class)
         (standard-instance-access class *standard-class-default-initargs-location*))
        ((or (funcallable-standard-class-instance-p class)
             (built-in-class-instance-p class))
         (std-slot-value class 'default-initargs))
        (t
         (class-default-initargs class))))
(defun (setf safe-class-default-initargs) (value class)
  (setf (std-slot-value class 'default-initargs) value))

(defun safe-class-direct-default-initargs (class)
  (cond ((standard-class-instance-p class)
         (standard-instance-access class *standard-class-direct-default-initargs-location*))
        ((or (funcallable-standard-class-instance-p class)
             (built-in-class-instance-p class))
         (std-slot-value class 'direct-default-initargs))
        (t
         (class-direct-default-initargs class))))
(defun (setf safe-class-direct-default-initargs) (value class)
  (setf (std-slot-value class 'direct-default-initargs) value))

(defun safe-class-direct-slots (class)
  (cond ((standard-class-instance-p class)
         (std-slot-value class 'direct-slots))
        ((or (funcallable-standard-class-instance-p class)
             (built-in-class-instance-p class))
         (std-slot-value class 'direct-slots))
        (t
         (class-direct-slots class))))
(defun (setf safe-class-direct-slots) (value class)
  (setf (std-slot-value class 'direct-slots) value))

(defun safe-class-direct-subclasses (class)
  (cond ((standard-class-instance-p class)
         (std-slot-value class 'direct-subclasses))
        ((or (funcallable-standard-class-instance-p class)
             (built-in-class-instance-p class))
         (std-slot-value class 'direct-subclasses))
        (t
         (class-direct-subclasses class))))
(defun (setf safe-class-direct-subclasses) (value class)
  (setf (std-slot-value class 'direct-subclasses) value))

(defun safe-class-direct-superclasses (class)
  (cond ((standard-class-instance-p class)
         (std-slot-value class 'direct-superclasses))
        ((or (funcallable-standard-class-instance-p class)
             (built-in-class-instance-p class))
         (std-slot-value class 'direct-superclasses))
        (t
         (class-direct-superclasses class))))
(defun (setf safe-class-direct-superclasses) (value class)
  (setf (std-slot-value class 'direct-superclasses) value))

(defun safe-class-finalized-p (class)
  (cond ((standard-class-instance-p class)
         (let ((val (standard-instance-access class *standard-class-finalized-p-location*)))
           (and val
                (not (eq val *secret-unbound-value*)))))
        ((or (funcallable-standard-class-instance-p class)
             (built-in-class-instance-p class))
         (and (std-slot-boundp class 'finalized-p)
              (std-slot-value class 'finalized-p)))
        (t
         (class-finalized-p class))))
(defun (setf safe-class-finalized-p) (value class)
  (setf (std-slot-value class 'finalized-p) value))

(defun safe-class-name (class)
  (cond ((standard-class-instance-p class)
         (std-slot-value class 'name))
        ((or (funcallable-standard-class-instance-p class)
             (built-in-class-instance-p class))
         (std-slot-value class 'name))
        (t
         (class-name class))))
(defun (setf safe-class-name) (value class)
  (cond ((standard-class-instance-p class)
         (setf (std-slot-value class 'name) value))
        ((funcallable-standard-class-instance-p class)
         (setf (std-slot-value class 'name) value))
        (t
         (setf (class-name class) value))))

(defun safe-class-precedence-list (class)
  (cond ((standard-class-instance-p class)
         (standard-instance-access class *standard-class-precedence-list-location*))
        ((or (funcallable-standard-class-instance-p class)
             (built-in-class-instance-p class))
         (std-slot-value class 'class-precedence-list))
        (t
         (class-precedence-list class))))
(defun (setf safe-class-precedence-list) (value class)
  (setf (std-slot-value class 'class-precedence-list) value))

(defun safe-class-slots (class)
  (cond ((standard-class-instance-p class)
         (standard-instance-access class *standard-class-effective-slots-location*))
        ((or (funcallable-standard-class-instance-p class)
             (built-in-class-instance-p class))
         (std-slot-value class 'effective-slots))
        (t
         (class-slots class))))
(defun (setf safe-class-slots) (values class)
  (setf (std-slot-value class 'effective-slots) values))

(defun safe-class-slot-storage-layout (class)
  (cond ((standard-class-instance-p class)
         (standard-instance-access class *standard-class-slot-storage-layout-location*))
        (t
         (std-slot-value class 'slot-storage-layout))))
(defun (setf safe-class-slot-storage-layout) (value class)
  (setf (std-slot-value class 'slot-storage-layout) value))

(defun safe-class-hash (class)
  (cond ((standard-class-instance-p class)
         (standard-instance-access class *standard-class-hash-location*))
        (t
         (std-slot-value class 'hash))))

(defun safe-class-direct-methods (class)
  (std-slot-value class 'direct-methods))
(defun (setf safe-class-direct-methods) (value class)
  (setf (std-slot-value class 'direct-methods) value))

(defun safe-class-dependents (class)
  (std-slot-value class 'dependents))
(defun (setf safe-class-dependents) (value class)
  (setf (std-slot-value class 'dependents) value))

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
  (or (eq metaclass *the-class-standard-class*)
      (eq metaclass *the-class-funcallable-standard-class*)))

(defun standard-class-p (metaclass)
  "Returns true if METACLASS is exactly STANDARD-CLASS."
  (eq metaclass *the-class-standard-class*))

(defun standard-class-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly STANDARD-CLASS."
  (and (sys.int::instance-p object)
       (eq (sys.int::%instance-layout object) *the-layout-standard-class*)))

(defun funcallable-standard-class-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly FUNCALLABLE-STANDARD-CLASS."
  (and (sys.int::instance-p object)
       (eq (sys.int::%instance-layout object) *the-layout-funcallable-standard-class*)))

(defun built-in-class-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly BUILT-IN-CLASS."
  (and (sys.int::instance-p object)
       (eq (sys.int::%instance-layout object) *the-layout-built-in-class*)))

(defun standard-generic-function-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly STANDARD-GENERIC-FUNCTION."
  (and (sys.int::funcallable-instance-p object)
       (eq (sys.int::%instance-layout object) *the-layout-standard-generic-function*)))

(defun standard-method-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly STANDARD-METHOD."
  (and (sys.int::instance-p object)
       (eq (sys.int::%instance-layout object) *the-layout-standard-method*)))

(defun clos-class-p (metaclass)
  "Returns true if METACLASS is either STANDARD-CLASS, FUNCALLABLE-STANDARD-CLASS, or BUILT-IN-CLASS."
  (or (eq metaclass *the-class-standard-class*)
      (eq metaclass *the-class-funcallable-standard-class*)
      (eq metaclass *the-class-built-in-class*)))

(defun standard-effective-slot-definition-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly STANDARD-EFFECTIVE-SLOT-DEFINITION."
  (and (sys.int::instance-p object)
       (eq (sys.int::%instance-layout object) *the-layout-standard-effective-slot-definition*)))

(defun standard-direct-slot-definition-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly STANDARD-EFFECTIVE-SLOT-DEFINITION."
  (and (sys.int::instance-p object)
       (eq (sys.int::%instance-layout object) *the-layout-standard-direct-slot-definition*)))

(defun standard-slot-definition-instance-p (object)
  (or (standard-effective-slot-definition-instance-p object)
      (standard-direct-slot-definition-instance-p object)))

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
  (setf (safe-class-direct-superclasses class) direct-superclasses)
  (dolist (superclass direct-superclasses)
    (push class (safe-class-direct-subclasses superclass)))
  (let ((slots (mapcar (lambda (direct-slot)
                         (convert-to-direct-slot-definition class direct-slot))
                       direct-slots)))
    (setf (safe-class-direct-slots class) slots)
    (dolist (direct-slot slots)
      (dolist (reader (safe-slot-definition-readers direct-slot))
        (add-reader-method class reader direct-slot))
      (dolist (writer (safe-slot-definition-writers direct-slot))
        (add-writer-method class writer direct-slot))))
  (setf (safe-class-direct-default-initargs class) direct-default-initargs)
  (maybe-finalize-inheritance class)
  (values))

(defun maybe-finalize-inheritance (class)
  "If CLASS is not finalized and can be finalized, finalize it, otherwise do nothing."
  (when (and (not (safe-class-finalized-p class))
             (every #'safe-class-finalized-p
                    (safe-class-direct-superclasses class)))
    (finalize-inheritance class)))

(defun ensure-class-finalized (class)
  "If CLASS is not finalized, call FINALIZE-INHERITANCE on it."
  (when (not (safe-class-finalized-p class))
    (finalize-inheritance class)))

;;; Slot definition metaobjects

(defun safe-slot-definition-allocation (slot-definition)
  (if (standard-slot-definition-instance-p slot-definition)
      (std-slot-value slot-definition 'allocation)
      (slot-definition-allocation slot-definition)))
(defun (setf safe-slot-definition-allocation) (value slot-definition)
  (setf (std-slot-value slot-definition 'allocation) value))

(defun safe-slot-definition-initargs (slot-definition)
  (if (standard-slot-definition-instance-p slot-definition)
      (std-slot-value slot-definition 'initargs)
      (slot-definition-initargs slot-definition)))
(defun (setf safe-slot-definition-initargs) (value slot-definition)
  (setf (std-slot-value slot-definition 'initargs) value))

(defun safe-slot-definition-initform (slot-definition)
  (if (standard-slot-definition-instance-p slot-definition)
      (std-slot-value slot-definition 'initform)
      (slot-definition-initform slot-definition)))
(defun (setf safe-slot-definition-initform) (value slot-definition)
  (setf (std-slot-value slot-definition 'initform) value))

(defun safe-slot-definition-initfunction (slot-definition)
  (if (standard-slot-definition-instance-p slot-definition)
      (std-slot-value slot-definition 'initfunction)
      (slot-definition-initfunction slot-definition)))
(defun (setf safe-slot-definition-initfunction) (value slot-definition)
  (setf (std-slot-value slot-definition 'initfunction) value))

(defun safe-slot-definition-name (slot-definition)
  (cond ((standard-effective-slot-definition-instance-p slot-definition)
         (standard-instance-access slot-definition *standard-effective-slot-definition-name-location*))
        ((standard-direct-slot-definition-instance-p slot-definition)
         (std-slot-value slot-definition 'name))
        (t
         (slot-definition-name slot-definition))))
(defun (setf safe-slot-definition-name) (value slot-definition)
  (setf (std-slot-value slot-definition 'name) value))

(defun safe-slot-definition-type (slot-definition)
  (if (standard-slot-definition-instance-p slot-definition)
      (std-slot-value slot-definition 'type)
      (slot-definition-type slot-definition)))
(defun (setf safe-slot-definition-type) (value slot-definition)
  (setf (std-slot-value slot-definition 'type) value))

(defun safe-slot-definition-readers (direct-slot-definition)
  (if (standard-direct-slot-definition-instance-p direct-slot-definition)
      (std-slot-value direct-slot-definition 'readers)
      (slot-definition-readers direct-slot-definition)))
(defun (setf safe-slot-definition-readers) (value direct-slot-definition)
  (setf (std-slot-value direct-slot-definition 'readers) value))

(defun safe-slot-definition-writers (direct-slot-definition)
  (if (standard-direct-slot-definition-instance-p direct-slot-definition)
      (std-slot-value direct-slot-definition 'writers)
      (slot-definition-writers direct-slot-definition)))
(defun (setf safe-slot-definition-writers) (value direct-slot-definition)
  (setf (std-slot-value direct-slot-definition 'writers) value))

(defun safe-slot-definition-location (effective-slot-definition)
  (if (standard-effective-slot-definition-instance-p effective-slot-definition)
      (standard-instance-access effective-slot-definition *standard-effective-slot-definition-location-location*)
      (slot-definition-location effective-slot-definition)))
(defun (setf safe-slot-definition-location) (value effective-slot-definition)
  (setf (std-slot-value effective-slot-definition 'location) value))

(defun safe-slot-definition-documentation (slot-definition)
  (if (standard-slot-definition-instance-p slot-definition)
      (std-slot-value slot-definition 'documentation)
      (documentation slot-definition t)))
(defun (setf safe-slot-definition-documentation) (value slot-definition)
  (if (standard-slot-definition-instance-p slot-definition)
      (setf (std-slot-value slot-definition 'documentation) value)
      (setf (documentation slot-definition t) value)))

;;; finalize-inheritance

(defun class-layouts-compatible-p (layout-a layout-b)
  (and (eql (sys.int::layout-heap-size layout-a)
            (sys.int::layout-heap-size layout-b))
       (equal (sys.int::layout-heap-layout layout-a)
              (sys.int::layout-heap-layout layout-b))
       (equal (sys.int::layout-area layout-a)
              (sys.int::layout-area layout-b))
       ;; TODO: This could be less conservative.
       ;; Only the slot-name/location pairs matter, not the ordering of the pairs.
       (equalp (sys.int::layout-instance-slots layout-a)
               (sys.int::layout-instance-slots layout-b))))

(defun std-finalize-inheritance (class)
  (dolist (super (safe-class-direct-superclasses class))
    (ensure-class-finalized super))
  (setf (safe-class-precedence-list class) (compute-class-precedence-list class))
  (setf (safe-class-slots class) (compute-slots class))
  (setf (safe-class-default-initargs class) (compute-default-initargs class))
  (let* ((instance-slots (remove-if-not 'instance-slot-p
                                        (safe-class-slots class)))
         (instance-slot-vector
          (make-array (* (length instance-slots) 2)))
         (layout (sys.int::make-layout
                  :class class
                  :obsolete nil
                  :heap-size (loop
                                ;; Using the effective slot locations
                                ;; accounts for the FC instance offset.
                                for slot in instance-slots
                                for location = (safe-slot-definition-location slot)
                                maximize (1+ location))
                  :heap-layout t
                  :area nil
                  :instance-slots instance-slot-vector)))
    (loop
       for i from 0 by 2
       for slot in instance-slots
       for slot-name = (safe-slot-definition-name slot)
       for location = (safe-slot-definition-location slot)
       do (setf (aref instance-slot-vector i) slot-name
                (aref instance-slot-vector (1+ i)) location))
    ;; TODO: Should call MAKE-INSTANCES-OBSOLETE here and have that rebuild
    ;; the layout.
    (let ((prev-layout (safe-class-slot-storage-layout class)))
      ;; Don't obsolete instances if the existing layout is compatible.
      (cond ((not prev-layout)
             (setf (safe-class-slot-storage-layout class) layout))
            ((not (class-layouts-compatible-p prev-layout layout))
             (setf (safe-class-slot-storage-layout class) layout
                   (sys.int::layout-obsolete prev-layout) layout)))))
  (setf (safe-class-finalized-p class) t)
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
    (let* ((supers (safe-class-direct-superclasses cpl-constituent))
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
                        (union (safe-class-direct-superclasses
                                 class-to-process)
                               superclasses)))))))
    (all-superclasses-loop () (list class))))

;;; The local precedence ordering of a class C with direct superclasses C_1,
;;; C_2, ..., C_n is the set ((C C_1) (C_1 C_2) ...(C_n-1 C_n)).

(defun local-precedence-ordering (class)
  (mapcar #'list
          (cons class
                (butlast (safe-class-direct-superclasses class)))
          (safe-class-direct-superclasses class)))

;;; Slot inheritance

(defun std-compute-slots (class)
  (let* ((all-slots (mapappend #'safe-class-direct-slots
                               (safe-class-precedence-list class)))
         (all-names (remove-duplicates
                      (mapcar #'safe-slot-definition-name all-slots))))
    (mapcar #'(lambda (name)
                (compute-effective-slot-definition
                 class
                 name
                 (remove name all-slots
                         :key #'safe-slot-definition-name
                         :test-not #'eq)))
            all-names)))

(defun std-compute-slot-layouts (class effective-slots)
  (loop
     with next-instance-slot-index = (if (subclassp (class-of class) *the-class-funcallable-standard-class*)
                                         2
                                         0)
     for slot in effective-slots
     do (case (safe-slot-definition-allocation slot)
          (:instance
           (setf (safe-slot-definition-location slot) next-instance-slot-index)
           (incf next-instance-slot-index))
          (:class
           ;; Walk up the precedence list looking for the nearest class that defines this slot.
           (dolist (super (safe-class-precedence-list class)
                    (error "Unable to locate storage location for :class slot ~S in class ~S"
                           (safe-slot-definition-name slot) (safe-class-name class)))
             (let ((existing (find (safe-slot-definition-name slot)
                                   (safe-class-direct-slots super)
                                   :key #'safe-slot-definition-name)))
               (when existing
                 (let ((existing-effective (find (safe-slot-definition-name slot)
                                                 (safe-class-slots super)
                                                 :key #'safe-slot-definition-name)))
                   (cond ((eql super class)
                          ;; This class defines the direct
                          ;; slot. Create a new cell to hold the
                          ;; value, or preserve the existing one if
                          ;; the class is being redefined.
                          (setf (safe-slot-definition-location slot)
                                (if existing-effective
                                    (safe-slot-definition-location existing-effective)
                                    (cons (safe-slot-definition-name slot) *secret-unbound-value*))))
                         (t
                          (assert (consp (safe-slot-definition-location existing-effective)))
                          (setf (safe-slot-definition-location slot) (safe-slot-definition-location existing-effective))))
                   (return))))))
          (t
           (setf (safe-slot-definition-location slot) nil)))))

(defun make-effective-slot-definition (class &rest initargs)
  (apply #'make-instance
         (apply #'effective-slot-definition-class class initargs)
         initargs))

(defun std-compute-effective-slot-definition (class name direct-slots)
  (let ((initer (find-if-not #'null direct-slots
                             :key #'safe-slot-definition-initfunction)))
    (make-effective-slot-definition
     class
     :name name
     :initform (if initer
                   (safe-slot-definition-initform initer)
                   nil)
     :initfunction (if initer
                       (safe-slot-definition-initfunction initer)
                       nil)
     :initargs (remove-duplicates
                (mapappend #'safe-slot-definition-initargs
                           direct-slots))
     :allocation (safe-slot-definition-allocation (car direct-slots)))))

;;;
;;; Generic function metaobjects and standard-generic-function
;;;

(defun safe-generic-function-argument-precedence-order (generic-function)
  (if (standard-generic-function-instance-p generic-function)
      (std-slot-value generic-function 'argument-precedence-order)
      (generic-function-argument-precedence-order generic-function)))
(defun (setf safe-generic-function-argument-precedence-order) (value generic-function)
  (setf (std-slot-value generic-function 'argument-precedence-order) value))

(defun safe-generic-function-declarations (generic-function)
  (if (standard-generic-function-instance-p generic-function)
      (std-slot-value generic-function 'declarations)
      (generic-function-declarations generic-function)))
(defun (setf safe-generic-function-declarations) (value generic-function)
  (setf (std-slot-value generic-function 'declarations) value))

(defun safe-generic-function-lambda-list (generic-function)
  (if (standard-generic-function-instance-p generic-function)
      (std-slot-value generic-function 'lambda-list)
      (generic-function-lambda-list generic-function)))
(defun (setf safe-generic-function-lambda-list) (value generic-function)
  (setf (std-slot-value generic-function 'lambda-list) value))

(defun safe-generic-function-method-class (generic-function)
  (if (standard-generic-function-instance-p generic-function)
      (std-slot-value generic-function 'method-class)
      (generic-function-method-class generic-function)))
(defun (setf safe-generic-function-method-class) (value generic-function)
  (setf (std-slot-value generic-function 'method-class) value))

(defun safe-generic-function-method-combination (generic-function)
  (if (standard-generic-function-instance-p generic-function)
      (std-slot-value generic-function 'method-combination)
      (generic-function-method-combination generic-function)))
(defun (setf safe-generic-function-method-combination) (value generic-function)
  (setf (std-slot-value generic-function 'method-combination) value))

(defun safe-generic-function-methods (generic-function)
  (if (standard-generic-function-instance-p generic-function)
      (std-slot-value generic-function 'methods)
      (generic-function-methods generic-function)))
(defun (setf safe-generic-function-methods) (value generic-function)
  (setf (std-slot-value generic-function 'methods) value))

(defun safe-generic-function-name (generic-function)
  (if (standard-generic-function-instance-p generic-function)
      (std-slot-value generic-function 'name)
      (generic-function-name generic-function)))
(defun (setf safe-generic-function-name) (value generic-function)
  (setf (std-slot-value generic-function 'name) value))

(defun safe-generic-function-discriminating-function (generic-function)
  (std-slot-value generic-function 'discriminating-function))
(defun (setf safe-generic-function-discriminating-function) (value generic-function)
  (setf (std-slot-value generic-function 'discriminating-function) value))

(defun safe-generic-function-relevant-arguments (generic-function)
  (std-slot-value generic-function 'relevant-arguments))
(defun (setf safe-generic-function-relevant-arguments) (value generic-function)
  (setf (std-slot-value generic-function 'relevant-arguments) value))

(defun safe-generic-function-has-unusual-specializers (generic-function)
  (std-slot-value generic-function 'weird-specializers-p))
(defun (setf safe-generic-function-has-unusual-specializers) (value generic-function)
  (setf (std-slot-value generic-function 'weird-specializers-p) value))

(defun classes-to-emf-table (generic-function)
  (std-slot-value generic-function 'classes-to-emf-table))
(defun (setf classes-to-emf-table) (new-value generic-function)
  (setf (std-slot-value generic-function 'classes-to-emf-table) new-value))

(defun argument-reordering-table (generic-function)
  (std-slot-value generic-function 'argument-reordering-table))
(defun (setf argument-reordering-table) (value generic-function)
  (setf (std-slot-value generic-function 'argument-reordering-table) value))

;;;
;;; Method metaobjects and standard-method
;;;

(defun safe-method-function (method)
  (if (standard-method-instance-p method)
      (std-slot-value method 'function)
      (method-function method)))
(defun (setf safe-method-function) (value method)
  (setf (std-slot-value method 'function) value))

(defun safe-method-generic-function (method)
  (if (standard-method-instance-p method)
      (std-slot-value method 'generic-function)
      (method-generic-function method)))
(defun (setf safe-method-generic-function) (value method)
  (setf (std-slot-value method 'generic-function) value))

(defun safe-method-lambda-list (method)
  (if (standard-method-instance-p method)
      (std-slot-value method 'lambda-list)
      (method-lambda-list method)))
(defun (setf safe-method-lambda-list) (value method)
  (setf (std-slot-value method 'lambda-list) value))

(defun safe-method-specializers (method)
  (if (standard-method-instance-p method)
      (std-slot-value method 'specializers)
      (method-specializers method)))
(defun (setf safe-method-specializers) (value method)
  (setf (std-slot-value method 'specializers) value))

(defun safe-method-qualifiers (method)
  (if (standard-method-instance-p method)
      (std-slot-value method 'qualifiers)
      (method-qualifiers method)))
(defun (setf safe-method-qualifiers) (value method)
  (setf (std-slot-value method 'qualifiers) value))

(defun safe-accessor-method-slot-definition (method)
  (if (standard-method-instance-p method)
      (std-slot-value method 'slot-definition)
      (accessor-method-slot-definition method)))
(defun (setf safe-accessor-method-slot-definition) (value method)
  (setf (std-slot-value method 'slot-definition) value))

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

(defun generic-function-unspecialized-dispatch-p (gf)
  "Returns true when the generic function has no methods with non-t specialized arguments."
  (and (eq (class-of gf) *the-class-standard-gf*)
       (every (lambda (method)
                (every (lambda (spec)
                         (eql spec *the-class-t*))
                       (safe-method-specializers method)))
              (safe-generic-function-methods gf))))

(defun generic-function-single-dispatch-p (gf)
  "Returns true when the generic function only one non-t specialized argument and
has only has class specializer."
  (when (and (eq (class-of gf) *the-class-standard-gf*)
             (or (not (safe-generic-function-has-unusual-specializers gf))
                 (eql (safe-generic-function-has-unusual-specializers gf) :eql)))
    (let ((specializers (safe-generic-function-relevant-arguments gf))
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
            (setf (safe-class-dependents class) (remove gf (safe-class-dependents class))))
          (classes-to-emf-table gf))
         (clear-single-dispatch-emf-table (classes-to-emf-table gf)))
        ((classes-to-emf-table gf)
         (loop
            for classes being the hash-keys in (classes-to-emf-table gf)
            do (loop
                  for class in classes
                  do (setf (safe-class-dependents class) (remove gf (safe-class-dependents class)))))
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
            (safe-generic-function-lambda-list gf))))
    (getf plist ':required-args)))

(defun gf-optional-arglist (gf)
  (let ((plist
          (analyze-lambda-list
            (safe-generic-function-lambda-list gf))))
    (getf plist ':optional-args)))

(defun gf-rest-arg-p (gf)
  (let ((ll (safe-generic-function-lambda-list gf)))
    (or (member '&rest ll)
        (member '&key ll))))

(defun finalize-generic-function (gf)
  (let* ((required-args (gf-required-arglist gf))
         (relevant-args (make-array (length required-args)
                                    :element-type 'bit
                                    :initial-element 0))
         (class-t (find-class 't))
         (argument-precedence (safe-generic-function-argument-precedence-order gf)))
    (when (not (eql (length required-args)
                    (length (remove-duplicates required-args))))
      (error "Generic function has duplicate required argument in its lambda list ~S."
             (safe-generic-function-lambda-list gf)))
    ;; Verify that the argument-precedence order is correct.
    (when (endp argument-precedence)
      (setf argument-precedence (copy-list required-args)
            (safe-generic-function-argument-precedence-order gf) argument-precedence))
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
    (setf (safe-generic-function-has-unusual-specializers gf) nil)
    (dolist (m (safe-generic-function-methods gf))
      (do ((i 0 (1+ i))
           (spec (safe-method-specializers m) (rest spec)))
          ((null spec))
        (typecase (first spec)
          (class)
          (eql-specializer
           (setf (safe-generic-function-has-unusual-specializers gf) (or (safe-generic-function-has-unusual-specializers gf)
                                                                    :eql)))
          (t
           (setf (safe-generic-function-has-unusual-specializers gf) t)))
        (unless (eql (first spec) class-t)
          (setf (bit relevant-args i) 1))))
    (setf (safe-generic-function-relevant-arguments gf) relevant-args))
  (reset-gf-emf-table gf)
  (setf (classes-to-emf-table gf) (cond ((generic-function-single-dispatch-p gf)
                                         (make-single-dispatch-emf-table))
                                        ((generic-function-unspecialized-dispatch-p gf)
                                         nil)
                                        (t (make-hash-table :test #'equal))))
  (setf (safe-generic-function-discriminating-function gf)
        (funcall (if (eq (class-of gf) *the-class-standard-gf*)
                     #'std-compute-discriminating-function
                     #'compute-discriminating-function)
                 gf))
  (set-funcallable-instance-function gf (safe-generic-function-discriminating-function gf))
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
    (setf (safe-generic-function-name gf) name)
    (setf (safe-generic-function-lambda-list gf) lambda-list)
    (setf (safe-generic-function-methods gf) ())
    (setf (safe-generic-function-method-class gf) method-class)
    (setf (safe-generic-function-method-combination gf) method-combination)
    (setf (safe-generic-function-declarations gf) declarations)
    (setf (classes-to-emf-table gf) (make-hash-table))
    (setf (safe-generic-function-argument-precedence-order gf) argument-precedence-order)
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
              (if (eq (safe-generic-function-method-class gf)
                      *the-class-standard-method*)
                  #'make-instance-standard-method
                  #'make-instance)
              (safe-generic-function-method-class gf)
              all-keys)))
    (if (standard-generic-function-instance-p gf)
        (std-add-method gf new-method)
        (add-method gf new-method))
    new-method))

;;; make-instance-standard-method creates and initializes an instance of
;;; standard-method without falling into method lookup.  However, it cannot
;;; be called until standard-method exists.

(defun make-instance-standard-method (method-class
                                      &key lambda-list qualifiers
                                           specializers function)
  (declare (ignore method-class))
  (let ((method (std-allocate-instance *the-class-standard-method*)))
    (setf (safe-method-lambda-list method) lambda-list)
    (setf (safe-method-qualifiers method) qualifiers)
    (setf (safe-method-specializers method) specializers)
    (setf (safe-method-generic-function method) nil)
    (setf (safe-method-function method) function)
    method))

(defun check-method-lambda-list-congruence (gf method)
  "Ensure that the lambda lists of GF and METHOD are compatible."
  (let ((gf-ll (analyze-lambda-list (safe-generic-function-lambda-list gf)))
        (method-ll (analyze-lambda-list (safe-method-lambda-list method))))
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
                                      (member '&key (safe-generic-function-lambda-list gf))))
          (method-accepts-key-or-rest (or (getf method-ll :rest-var)
                                          (member '&key (safe-method-lambda-list method)))))
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

(defun std-add-method (gf method)
  ;; If the GF has no methods and an empty lambda-list, then it may have
  ;; been implicitly created via defmethod. Fill in the lambda-list.
  (when (and (endp (safe-generic-function-methods gf))
             (endp (safe-generic-function-lambda-list gf)))
    (setf (safe-generic-function-lambda-list gf) (safe-method-lambda-list method)))
  (check-method-lambda-list-congruence gf method)
  (assert (not (safe-method-generic-function method)))
  (let ((old-method
           (find-method gf (safe-method-qualifiers method)
                           (safe-method-specializers method) nil)))
    (when old-method (remove-method gf old-method)))
  (setf (safe-method-generic-function method) gf)
  (push method (safe-generic-function-methods gf))
  (dolist (specializer (safe-method-specializers method))
    (when (typep specializer 'class)
      (pushnew method (safe-class-direct-methods specializer))))
  (finalize-generic-function gf)
  gf)

(defun std-remove-method (gf method)
  (setf (safe-generic-function-methods gf)
        (remove method (safe-generic-function-methods gf)))
  (setf (safe-method-generic-function method) nil)
  (dolist (class (safe-method-specializers method))
    (when (typep class 'class)
      (setf (safe-class-direct-methods class)
            (remove method (safe-class-direct-methods class)))))
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
                                   (safe-method-qualifiers method))
                            (equal specializers
                                   (safe-method-specializers method))))
                   (safe-generic-function-methods gf))))
      (if (and (null method) errorp)
          (error "No such method for ~S." (safe-generic-function-name gf))
          method)))

;;; Reader and write methods

(defun add-reader-method (class fn-name direct-slot-definition)
  (add-method (ensure-generic-function fn-name :lambda-list '(object))
              (let* ((slot-name (safe-slot-definition-name direct-slot-definition))
                     (initargs (list :lambda-list '(object)
                                     :qualifiers ()
                                     :specializers (list class)
                                     :function (lambda (method next-emfun)
                                                 (declare (ignore method next-emfun))
                                                 (lambda (object)
                                                   (slot-value object slot-name)))
                                     :slot-definition direct-slot-definition)))
                (apply #'make-instance
                       (apply #'reader-method-class class direct-slot-definition initargs)
                       initargs)))
  (values))

(defun add-writer-method (class fn-name direct-slot-definition)
  (add-method (ensure-generic-function fn-name :lambda-list '(new-value object))
              (let* ((slot-name (safe-slot-definition-name direct-slot-definition))
                    (initargs (list :lambda-list '(new-value object)
                                    :qualifiers ()
                                    :specializers (list (find-class 't) class)
                                    :function (lambda (method next-emfun)
                                                (declare (ignore method next-emfun))
                                                (lambda (new-value object)
                                                  (setf (slot-value object slot-name) new-value)))
                                    :slot-definition direct-slot-definition)))
                (apply #'make-instance
                       (apply #'writer-method-class class direct-slot-definition initargs)
                       initargs)))
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
  (let ((eql-table (compute-1-effective-eql-table gf argument-offset)))
    ;; Generate specialized dispatch functions for various combinations of
    ;; arguments.
    (macrolet ((gen-one (index n-required restp eql-spec-p)
                 (let ((req-args (loop
                                    for i below n-required
                                    collect (gensym)))
                       (rest-arg (when restp
                                   (gensym))))
                   `(when (and (eql ',index argument-offset)
                               (eql (length (gf-required-arglist gf)) ',n-required)
                               (or (and ',restp (or (gf-optional-arglist gf)
                                                    (gf-rest-arg-p gf)))
                                   (and (not ',restp) (not (or (gf-optional-arglist gf)
                                                               (gf-rest-arg-p gf)))))
                               ,(if eql-spec-p
                                    'eql-table
                                    `(not eql-table)))
                      (lambda (,@req-args ,@(if rest-arg
                                                `(&rest ,rest-arg)
                                                '()))
                        (declare (sys.int::lambda-name (1-effective-discriminator ,index ,n-required ,restp ,eql-spec-p))
                                 ,@(if rest-arg
                                       `((dynamic-extent ,rest-arg))
                                       `()))
                        (block nil
                          ,(when eql-spec-p
                             `(let ((eql-emfun (assoc ,(nth index req-args) eql-table)))
                                (when eql-emfun
                                  (return ,(if rest-arg
                                               `(apply (cdr eql-emfun) ,@req-args ,rest-arg)
                                               `(funcall (cdr eql-emfun) ,@req-args))))))
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
                                 class))))))))
               (gen-all ()
                 `(or
                   ,@(loop
                        for idx from 0 below 5
                        appending
                          (loop
                             for req from 1 to 5
                             collect `(gen-one ,idx ,req nil nil)
                             collect `(gen-one ,idx ,req nil t)
                             collect `(gen-one ,idx ,req t   nil)
                             collect `(gen-one ,idx ,req t   t))))))
      (or (gen-all)
          (lambda (&rest args)
            (declare (dynamic-extent args))
            (let* ((class (class-of (nth argument-offset args)))
                   (emfun (single-dispatch-emf-entry emf-table class)))
              (if emfun
                  (apply emfun args)
                  (slow-single-dispatch-method-lookup gf args class))))))))

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

(defun compute-1-effective-eql-table (gf argument-offset)
  (loop
     with n-required = (length (gf-required-arglist gf))
     for method in (safe-generic-function-methods gf)
     for spec = (elt (safe-method-specializers method) argument-offset)
     when (typep spec 'eql-specializer)
     collect (cons (eql-specializer-object spec)
                   (std-compute-effective-method-function
                    gf
                    (compute-applicable-methods gf
                                                (loop
                                                   repeat n-required
                                                   collect (eql-specializer-object spec)))))))

(defun slow-single-dispatch-method-lookup* (gf argument-offset args state)
  (declare (notinline slot-value (setf slot-value))) ; Bootstrap hack
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
                     (std-class-p (class-of class)))
                (let* ((instance (first args))
                       (slot-name (slot-definition-name (accessor-method-slot-definition (first applicable-methods))))
                       (effective-slot (find-effective-slot instance slot-name)))
                  (cond (effective-slot
                         (let ((location (safe-slot-definition-location effective-slot)))
                           (setf (single-dispatch-emf-entry emf-table class) location)
                           (pushnew gf (safe-class-dependents class))
                           (fast-slot-read (first args) location)))
                        (t
                         ;; Slot not present, fall back on SLOT-VALUE.
                         (slot-value instance slot-name)))))
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
                     (std-class-p (class-of class)))
                (let* ((instance (second args))
                       (slot-name (slot-definition-name (accessor-method-slot-definition (first applicable-methods))))
                       (effective-slot (find-effective-slot instance slot-name)))
                  (cond (effective-slot
                         (let ((location (safe-slot-definition-location effective-slot)))
                           (setf (single-dispatch-emf-entry emf-table class) location)
                           (pushnew gf (safe-class-dependents class))
                           (fast-slot-write (first args) instance location)))
                        (t
                         ;; Slot not present, fall back on SLOT-VALUE.
                         (setf (slot-value instance slot-name) (first args))))))
               (t ;; Give up and use the full path.
                (slow-single-dispatch-method-lookup* gf argument-offset args :never-called)))))
      (:never-called
       (reset-gf-emf-table gf)
       (let* ((classes (mapcar #'class-of (required-portion gf args)))
              (class (nth argument-offset classes))
              (applicable-methods (if (eql (class-of gf) *the-class-standard-gf*)
                                      (std-compute-applicable-methods-using-classes gf classes)
                                      (compute-applicable-methods-using-classes gf classes))))
         (cond ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-reader-method)
                     (std-class-p (class-of class)))
                ;; Switch to reader-method.
                (setf (safe-generic-function-discriminating-function gf)
                      (compute-reader-discriminator gf emf-table argument-offset))
                (set-funcallable-instance-function gf (safe-generic-function-discriminating-function gf))
                (apply gf args))
               ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-writer-method)
                     (std-class-p (class-of class)))
                ;; Switch to writer-method.
                (setf (safe-generic-function-discriminating-function gf)
                      (compute-writer-discriminator gf emf-table argument-offset))
                (set-funcallable-instance-function gf (safe-generic-function-discriminating-function gf))
                (apply gf args))
               (t ;; Switch to 1-effective.
                (setf (safe-generic-function-discriminating-function gf)
                      (compute-1-effective-discriminator gf emf-table argument-offset))
                (set-funcallable-instance-function gf (safe-generic-function-discriminating-function gf))
                (slow-single-dispatch-method-lookup gf args (class-of (nth argument-offset args))))))))))

(defun std-compute-discriminating-function (gf)
  (lambda (&rest args)
    (multiple-value-bind (single-dispatch-p argument-offset)
        (generic-function-single-dispatch-p gf)
      (cond (single-dispatch-p
             (slow-single-dispatch-method-lookup* gf argument-offset args :never-called))
            ((generic-function-unspecialized-dispatch-p gf)
             (slow-unspecialized-dispatch-method-lookup gf args))
            (t (setf (safe-generic-function-discriminating-function gf)
                     (compute-n-effective-discriminator gf (classes-to-emf-table gf) (length (gf-required-arglist gf))))
               (set-funcallable-instance-function gf (safe-generic-function-discriminating-function gf))
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
          (pushnew gf (safe-class-dependents class))))
      (apply emfun args))))

(defun slow-single-dispatch-method-lookup (gf args class)
  (let* ((classes (mapcar #'class-of (required-portion gf args))))
    (multiple-value-bind (applicable-methods validp)
        (if (eql (class-of gf) *the-class-standard-gf*)
            (std-compute-applicable-methods-using-classes gf classes)
            (compute-applicable-methods-using-classes gf classes))
      (when (not validp)
        ;; EQL specialized.
        (setf applicable-methods (if (eql (class-of gf) *the-class-standard-gf*)
                                     (std-compute-applicable-methods gf args)
                                     (compute-applicable-methods gf args))))
      (let ((emfun (cond (applicable-methods
                          (std-compute-effective-method-function gf applicable-methods))
                         (t
                          (lambda (&rest args)
                            (apply #'no-applicable-method gf args))))))
        ;; Cache is only valid for non-eql methods.
        (when validp
          (setf (single-dispatch-emf-entry (classes-to-emf-table gf) class) emfun)
          (pushnew gf (safe-class-dependents class)))
        (apply emfun args)))))

(defun slow-unspecialized-dispatch-method-lookup (gf args)
  (let* ((classes (loop
                     for req in (required-portion gf args)
                     collect *the-class-t*))
         (applicable-methods (std-compute-applicable-methods-using-classes gf classes))
         (emfun (cond (applicable-methods
                        (std-compute-effective-method-function gf applicable-methods))
                       (t
                        (apply #'no-applicable-method gf args)))))
    (setf (safe-generic-function-discriminating-function gf) emfun)
    (set-funcallable-instance-function gf emfun)
    (apply emfun args)))

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
                                      (safe-method-specializers method)))
                           (safe-generic-function-methods gf)))
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
                              (safe-method-specializers method)))
                   (safe-generic-function-methods gf)))
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
     for spec1 in (reorder-method-specializers gf (safe-method-specializers method1))
     for spec2 in (reorder-method-specializers gf (safe-method-specializers method2))
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
     for spec1 in (reorder-method-specializers gf (safe-method-specializers method1))
     for spec2 in (reorder-method-specializers gf (safe-method-specializers method2))
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
  (null (safe-method-qualifiers method)))
(defun before-method-p (method)
  (equal '(:before) (safe-method-qualifiers method)))
(defun after-method-p (method)
  (equal '(:after) (safe-method-qualifiers method)))
(defun around-method-p (method)
  (equal '(:around) (safe-method-qualifiers method)))

;;; compute an effective method function from a list of primary methods:

(defun method-fast-function (method next-emfun)
  (funcall (safe-method-function method) method next-emfun))

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
                           `(apply (funcall ',(safe-method-function method)
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
  (list* (safe-generic-function-name gf)
         (method-combination-name mc-object)
         (mapcar (lambda (method)
                   (list (safe-method-qualifiers method)
                         (mapcar (lambda (specializer)
                                   (typecase specializer
                                     (class
                                      (safe-class-name specializer))
                                     (t specializer)))
                                 (safe-method-specializers method))))
                 methods)))

(defun std-compute-effective-method-standard-method-combination (gf applicable-methods)
  (let (around before primary after)
    (dolist (method
              applicable-methods)
      (cond ((match-qualifier-pattern '(:around) (safe-method-qualifiers method))
             (push method around))
            ((match-qualifier-pattern '(:before) (safe-method-qualifiers method))
             (push method before))
            ((match-qualifier-pattern 'nil (safe-method-qualifiers method))
             (push method primary))
            ((match-qualifier-pattern '(:after) (safe-method-qualifiers method))
             (push method after))
            (t
             (invalid-method-error method "No specifiers matched."))))
    (when (endp primary)
      (error "No primary methods in generic function ~S." gf))
    (setf around (reverse around))
    (setf before (reverse before))
    (setf primary (reverse primary))
    (setf after (reverse after))
    (flet ((call-methods (methods)
             (mapcar #'(lambda (method)
                         `(call-method ,method))
                     methods)))
      (let ((form (if (or before
                          after
                          (rest primary))
                      `(multiple-value-prog1
                           (progn ,@(call-methods before)
                                  (call-method ,(first primary) ,(rest primary)))
                         ,@(call-methods (reverse after)))
                      `(call-method ,(first primary)))))
        (if around
            `(call-method ,(first around)
                          (,@(rest around)
                           (make-method ,form)))
            form)))))

(defun std-compute-effective-method (gf mc methods)
  (if mc
      (apply (method-combination-combiner (method-combination-object-method-combination mc))
             gf
             methods
             (method-combination-object-arguments mc))
      (std-compute-effective-method-standard-method-combination gf methods)))

(defun std-compute-effective-method-function (gf methods)
  (let ((mc (safe-generic-function-method-combination gf)))
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
           (compiled-function-p (fdefinition 'print-object)))
  ;; Print-object starts off as a normal function.
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

;;; TODO: The following reader function should use standard class readers
;;; instead of open-coded methods.

;;; Class metaobject readers
;;; FIXME: CLASS-DEFAULT-INITARGS, CLASS-PRECEDENCE-LIST, and CLASS-SLOTS
;;; must signal an error if the class has not been finalized, but doing
;;; this causing issues during finalization.

(defgeneric class-default-initargs (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'default-initargs)))
(defgeneric class-direct-default-initargs (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'direct-default-initargs))
  (:method ((class forward-referenced-class))
    '()))
(defgeneric class-direct-slots (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'direct-slots))
  (:method ((class forward-referenced-class))
    '()))
(defgeneric class-direct-subclasses (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'direct-subclasses))
  (:method ((class forward-referenced-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'direct-subclasses)))
(defgeneric class-direct-superclasses (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'direct-superclasses))
  (:method ((class forward-referenced-class))
    '()))
(defgeneric class-finalized-p (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    ;; The slot may be unbound if the class has not been initialized yet.
    (and (slot-boundp class 'finalized-p)
         (slot-value class 'finalized-p)))
  (:method ((class forward-referenced-class))
    nil))
(defgeneric class-name (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'name))
  (:method ((class forward-referenced-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'name)))
(defgeneric class-precedence-list (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'class-precedence-list)))
(defgeneric class-slots (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'effective-slots)))

;;; Slot definition metaobject readers

(defgeneric slot-definition-allocation (slot-definition)
  (:method ((slot-definition standard-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'allocation)))
(defgeneric slot-definition-initargs (slot-definition)
  (:method ((slot-definition standard-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'initargs)))
(defgeneric slot-definition-initform (slot-definition)
  (:method ((slot-definition standard-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'initform)))
(defgeneric slot-definition-initfunction (slot-definition)
  (:method ((slot-definition standard-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'initfunction)))
(defgeneric slot-definition-name (slot-definition)
  (:method ((slot-definition standard-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'name)))
(defgeneric slot-definition-type (slot-definition)
  (:method ((slot-definition standard-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'type)))
(defgeneric slot-definition-readers (direct-slot-definition)
  (:method ((direct-slot-definition standard-direct-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value direct-slot-definition 'readers)))
(defgeneric slot-definition-writers (direct-slot-definition)
  (:method ((direct-slot-definition standard-direct-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value direct-slot-definition 'writers)))
(defgeneric slot-definition-location (effective-slot-definition)
  (:method ((effective-slot-definition standard-effective-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value effective-slot-definition 'location)))

;;; Generic function metaobject readers

(defgeneric generic-function-argument-precedence-order (generic-function)
  (:method ((generic-function standard-generic-function))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value generic-function 'argument-precedence-order)))
(defgeneric generic-function-declarations (generic-function)
  (:method ((generic-function standard-generic-function))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value generic-function 'declarations)))
(defgeneric generic-function-lambda-list (generic-function)
  (:method ((generic-function standard-generic-function))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value generic-function 'lambda-list)))
(defgeneric generic-function-method-class (generic-function)
  (:method ((generic-function standard-generic-function))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value generic-function 'method-class)))
(defgeneric generic-function-method-combination (generic-function)
  (:method ((generic-function standard-generic-function))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value generic-function 'method-combination)))
(defgeneric generic-function-methods (generic-function)
  (:method ((generic-function standard-generic-function))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value generic-function 'methods)))
(defgeneric generic-function-name (generic-function)
  (:method ((generic-function standard-generic-function))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value generic-function 'name)))

;;; Method metaobject readers

(defgeneric method-function (method)
  (:method ((method standard-method))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value method 'function)))
(defgeneric method-generic-function (method)
  (:method ((method standard-method))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value method 'generic-function)))
(defgeneric method-lambda-list (method)
  (:method ((method standard-method))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value method 'lambda-list)))
(defgeneric method-specializers (method)
  (:method ((method standard-method))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value method 'specializers)))
(defgeneric method-qualifiers (method)
  (:method ((method standard-method))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value method 'qualifiers)))
(defgeneric accessor-method-slot-definition (accessor-method)
  (:method ((accessor-method standard-accessor-method))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value accessor-method 'slot-definition)))

;;; Method management
;;;
;;; ###: CLHS says these should be specialized on METHOD, MOP says
;;; on STANDARD-METHOD. Which is right? STANDARD-METHOD seems more sensible.

(defgeneric add-method (generic-function method))
(defmethod add-method ((generic-function standard-generic-function)
                       (method standard-method))
  (std-add-method generic-function method))

(defgeneric remove-method (generic-function method))
(defmethod remove-method ((generic-function standard-generic-function)
                          (method standard-method))
  (std-remove-method generic-function method))

;;; Slot access

(defgeneric slot-value-using-class (class instance slot))
(defmethod slot-value-using-class ((class std-class) instance (slot standard-effective-slot-definition))
  (std-slot-value instance (safe-slot-definition-name slot)))

(defgeneric (setf slot-value-using-class) (new-value class instance slot))
(defmethod (setf slot-value-using-class) (new-value (class std-class) instance (slot standard-effective-slot-definition))
  (setf (std-slot-value instance (safe-slot-definition-name slot)) new-value))

(defgeneric (sys.int::cas slot-value-using-class) (old new class instance slot))
(defmethod (sys.int::cas slot-value-using-class) (old new (class std-class) instance (slot standard-effective-slot-definition))
  (sys.int::cas (std-slot-value instance (safe-slot-definition-name slot)) old new))

(defgeneric slot-boundp-using-class (class instance slot))
(defmethod slot-boundp-using-class ((class std-class) instance (slot standard-effective-slot-definition))
  (std-slot-boundp instance (safe-slot-definition-name slot)))

(defgeneric slot-makunbound-using-class (class instance slot))
(defmethod slot-makunbound-using-class ((class std-class) instance (slot standard-effective-slot-definition))
  (std-slot-makunbound instance (safe-slot-definition-name slot)))

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

(defgeneric compute-default-initargs (class))
(defmethod compute-default-initargs ((class std-class))
  (std-compute-default-initargs class))

(defun std-compute-default-initargs (class)
  (let ((default-initargs '()))
    (dolist (c (safe-class-precedence-list class))
      (loop
         for (initarg form fn) in (safe-class-direct-default-initargs c)
         do (when (not (member initarg default-initargs :key #'first))
              (push (list initarg form fn) default-initargs))))
    (nreverse default-initargs)))

(defun std-compute-initargs (class initargs)
  (let ((default-initargs
         (loop
            for (initarg form fn) in (safe-class-default-initargs class)
            when (loop
                    for (key value) on initargs by #'cddr
                    when (eql key initarg)
                    do (return nil)
                    finally (return t))
            collect initarg
            and collect (funcall fn))))
    (if default-initargs
        (append initargs default-initargs)
        initargs)))

(defgeneric make-instance (class &rest initargs &key &allow-other-keys))
(defmethod make-instance ((class std-class) &rest initargs)
  (let* ((true-initargs (std-compute-initargs class initargs))
         (instance (apply #'allocate-instance class true-initargs)))
    (apply #'initialize-instance instance true-initargs)
    instance))
(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(define-compiler-macro make-instance (&whole whole class &rest initargs &key &allow-other-keys)
  (cond ((typep class '(cons (eql quote) (cons symbol null)))
         ;; Avoid the dispatch on symbol and the class lookup.
         `(make-instance (find-class ,class) ,@initargs))
        (t
         whole)))

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
  (dolist (slot (safe-class-slots (class-of instance)))
    (let ((slot-name (safe-slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
            (get-properties
              all-keys (safe-slot-definition-initargs slot))
         (declare (ignore init-key))
         (cond (foundp
                (setf (slot-value instance slot-name) init-value))
               ((and (not (slot-boundp instance slot-name))
                     (not (null (safe-slot-definition-initfunction slot)))
                     (or (eq slot-names t)
                         (member slot-name slot-names)))
                (setf (slot-value instance slot-name)
                      (funcall (safe-slot-definition-initfunction slot))))))))
  instance)

;;; change-class

(defgeneric change-class (instance new-class &key &allow-other-keys))
(defmethod change-class ((old-instance standard-object)
                         (new-class standard-class)
                         &rest initargs)
  (let* ((new-instance (allocate-instance new-class))
         (old-class (class-of old-instance))
         (old-copy (allocate-instance old-class)))
    ;; Make a copy of the old instance.
    (dolist (old-slot (safe-class-slots old-class))
      (let ((slot-name (safe-slot-definition-name old-slot)))
        (when (and (instance-slot-p old-slot)
                   (slot-boundp old-instance slot-name))
          (setf (slot-value old-copy slot-name)
                (slot-value old-instance slot-name)))))
    ;; Initialize the new instance with the old slots.
    (dolist (slot-name (mapcar #'safe-slot-definition-name
                               (safe-class-slots new-class)))
      (when (and (slot-exists-p old-instance slot-name)
                 (slot-boundp old-instance slot-name))
        (setf (slot-value new-instance slot-name)
              (slot-value old-instance slot-name))))
    ;; Obsolete the old instance, replacing it with the new instance.
    (mezzano.runtime::supersede-instance old-instance new-instance)
    (apply #'update-instance-for-different-class
           old-copy old-instance initargs)
    old-instance))

(defmethod change-class
           ((instance standard-object) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(defgeneric update-instance-for-different-class (old new &key &allow-other-keys))
(defmethod update-instance-for-different-class ((old standard-object)
                                                (new standard-object)
                                                &rest initargs)
  (let ((added-slots
          (remove-if #'(lambda (slot-name)
                         (slot-exists-p old slot-name))
                     (mapcar #'safe-slot-definition-name
                             (safe-class-slots (class-of new))))))
    (apply #'shared-initialize new added-slots initargs)))

;;;
;;;  Methods having to do with class metaobjects.
;;;

(defmethod print-object ((class class) stream)
  (print-unreadable-object (class stream :identity t)
    (format stream "~:(~S~) ~S"
            (safe-class-name (class-of class))
            (safe-class-name class)))
  class)

(defmethod print-object ((slot-definition standard-slot-definition) stream)
  (print-unreadable-object (slot-definition stream :type t :identity t)
    (format stream "~S" (safe-slot-definition-name slot-definition))))

(defmethod initialize-instance :after ((class std-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))


(defgeneric reader-method-class (class direct-slot &rest initargs))
(defmethod reader-method-class ((class std-class) (direct-slot standard-direct-slot-definition) &rest initargs)
  (find-class 'standard-reader-method))

(defgeneric writer-method-class (class direct-slot &rest initargs))
(defmethod writer-method-class ((class std-class) (direct-slot standard-direct-slot-definition) &rest initargs)
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
            (safe-class-name (class-of gf))
            (safe-generic-function-name gf))
    (let ((mc (safe-generic-function-method-combination gf)))
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
	    (safe-class-name (class-of method))
	    (when (safe-method-generic-function method)
              (safe-generic-function-name (safe-method-generic-function method)))
	    (safe-method-qualifiers method)
	    (mapcar (lambda (x) (typecase x
                                  (class
                                   (safe-class-name x))
                                  (t x)))
                    (safe-method-specializers method))))
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
  (setf (safe-class-direct-superclasses new) (list (find-class 'standard-class))))

(defmethod update-instance-for-different-class :before
           ((old forward-referenced-class) (new funcallable-standard-class) &rest initargs)
  (setf (safe-class-direct-superclasses new) (list (find-class 'funcallable-standard-class))))

(defgeneric ensure-class-using-class (class name &key &allow-other-keys))

(defmethod ensure-class-using-class ((class class) name
                                     &rest all-keys
                                     &key (metaclass 'standard-class) &allow-other-keys)
  (assert (eql name (safe-class-name class)))
  (assert (eql (class-of class) (find-class metaclass)))
  (apply #'reinitialize-instance class (compute-class-initialization-arguments all-keys))
  (setf (find-class name) class)
  class)

(defmethod ensure-class-using-class ((class forward-referenced-class) name
                                     &rest all-keys
                                     &key (metaclass 'standard-class) &allow-other-keys)
  (assert (eql name (safe-class-name class)))
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
  (restart-case
      (error "No applicable methods to generic function ~S (~S) when called with ~S."
             generic-function (safe-generic-function-name generic-function) function-arguments)
    (continue ()
      :report "Retry calling the generic function."
      (apply generic-function function-arguments))))

;;; Built-in-class.

(defmethod initialize-instance :after ((class built-in-class) &rest args)
  (declare (notinline slot-value (setf slot-value))) ; Bootstrap hack
  (apply #'std-after-initialization-for-classes class args)
  (when (not (slot-boundp class 'prototype))
    (setf (slot-value class 'prototype) (std-allocate-instance class))))

(defmethod reinitialize-instance :before ((class built-in-class) &rest args)
  (error "Cannot reinitialize built-in classes."))

;;; Structure-class.

(defclass structure-class (clos-class)
  ((structure-definition :initarg :definition)))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (notinline slot-value (setf slot-value))) ; Bootstrap hack
  (declare (ignore initargs))
  (sys.int::%make-struct
   (slot-value class 'structure-definition)))

(defmethod direct-slot-definition-class ((class structure-class) &rest initargs)
  *the-class-standard-direct-slot-definition*)
(defmethod effective-slot-definition-class ((class structure-class) &rest initargs)
  *the-class-standard-effective-slot-definition*)

(defmethod initialize-instance :after ((class structure-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

(defmethod reinitialize-instance :before ((class structure-class) &rest args)
  (error "Cannot reinitialize structure classes."))

(defmethod slot-value-using-class ((class structure-class) instance (slot standard-effective-slot-definition))
  (declare (notinline slot-value (setf slot-value))) ; Bootstrap hack
  (let ((def (slot-value class 'structure-definition))
        (slot-name (safe-slot-definition-name slot)))
    (when (not (eql (sys.int::%instance-layout instance) (sys.int::structure-definition-layout def)))
      (error "Class for structure ~S or instance is outdated?" (safe-class-name class)))
    (dolist (slot (sys.int::structure-definition-slots def)
             (values (slot-missing class instance slot-name 'slot-value)))
      (when (eql (sys.int::structure-slot-definition-name slot) slot-name)
        (when (sys.int::structure-slot-definition-fixed-vector slot)
          (error "Slot ~S is a fixed-vector" slot-name))
        (return (funcall (sys.int::structure-slot-definition-accessor slot) instance))))))

(defmethod (setf slot-value-using-class) (new-value (class structure-class) instance (slot standard-effective-slot-definition))
  (declare (notinline slot-value (setf slot-value))) ; Bootstrap hack
  (let ((def (slot-value class 'structure-definition))
        (slot-name (safe-slot-definition-name slot)))
    (when (not (eql (sys.int::%instance-layout instance) (sys.int::structure-definition-layout def)))
      (error "Class for structure ~S or instance is outdated?" (safe-class-name class)))
    (dolist (slot (sys.int::structure-definition-slots def)
             (progn
               (slot-missing class instance slot-name 'setf new-value)
               new-value))
      (when (eql (sys.int::structure-slot-definition-name slot) slot-name)
        (when (sys.int::structure-slot-definition-fixed-vector slot)
          (error "Slot ~S is a fixed-vector" slot-name))
        (when (sys.int::structure-slot-definition-read-only slot)
          (error "The slot ~S in class ~S is read-only." slot-name (safe-class-name class)))
        (return (funcall (fdefinition `(setf ,(sys.int::structure-slot-definition-accessor slot))) new-value instance))))))

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

(defmethod compute-default-initargs ((class structure-class))
  (std-compute-default-initargs class))

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
  `(:name ,(safe-slot-definition-name direct-slot)
    :allocation ,(safe-slot-definition-allocation direct-slot)
    :type ,(safe-slot-definition-type direct-slot)
    :initform ,(safe-slot-definition-initform direct-slot)
    :initfunction ,(safe-slot-definition-initfunction direct-slot)
    :initargs ,(safe-slot-definition-initargs direct-slot)
    :documentation ,(safe-slot-definition-documentation direct-slot)
    :readers ,(safe-slot-definition-readers direct-slot)
    :writers ,(safe-slot-definition-writers direct-slot)))

(defun std-after-reinitialization-for-classes (class &rest args &key &allow-other-keys)
  ;; Unfinalize the class.
  (setf (safe-class-finalized-p class) nil)
  ;; Remove the class as a subclass from all existing superclasses.
  (dolist (superclass (safe-class-direct-superclasses class))
    (setf (safe-class-direct-subclasses superclass) (remove class (safe-class-direct-subclasses superclass))))
  ;; Remove any slot reader/writer methods.
  #+(or)(dolist (direct-slot (safe-class-direct-slots class))
    (dolist (reader (safe-slot-definition-readers direct-slot))
      (remove-reader-method class reader (safe-slot-definition-name direct-slot)))
    (dolist (writer (safe-slot-definition-writers direct-slot))
      (remove-writer-method class writer (safe-slot-definition-name direct-slot))))
  ;; Fall into the initialize-instance code.
  (apply #'std-after-initialization-for-classes
         class
         (append args
                 (list :direct-superclasses (safe-class-direct-superclasses class))
                 (list :direct-slots (mapcar #'convert-direct-slot-definition-to-canonical-direct-slot (safe-class-direct-slots class)))
                 (list :direct-default-initargs (safe-class-direct-default-initargs class))))
  ;; Flush the EMF tables of generic functions.
  (dolist (gf (safe-class-dependents class))
    (reset-gf-emf-table gf))
  ;; Refinalize any subclasses.
  (dolist (subclass (safe-class-direct-subclasses class))
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

(defun layout-instance-slots-list (layout)
  "Return layout's instance-slots as a list of instance slot names."
  (loop
     with instance-slots = (sys.int::layout-instance-slots layout)
     for i below (length instance-slots) by 2
     collect (aref instance-slots i)))

(defun update-instance-for-new-layout (instance)
  (let* ((class (class-of instance))
         (old-potentially-obsolete-layout (sys.int::%instance-layout instance))
         (old-instance (if (sys.int::layout-p old-potentially-obsolete-layout)
                           instance
                           (mezzano.runtime::obsolete-instance-layout-new-instance
                            old-potentially-obsolete-layout)))
         (old-layout (sys.int::%instance-layout old-instance))
         (new-layout (safe-class-slot-storage-layout class))
         (old-layout-slots (layout-instance-slots-list old-layout))
         (new-layout-slots (layout-instance-slots-list new-layout))
         (added-slots (set-difference new-layout-slots old-layout-slots))
         (discarded-slots (set-difference old-layout-slots new-layout-slots))
         (new-instance (allocate-instance class))
         (property-list '()))
    ;; The complicated bit.
    ;; Slots have been added or removed.
    ;; Copy slots that were not added/discarded.
    (loop
       for slot in (intersection new-layout-slots old-layout-slots)
       do
         (setf (standard-instance-access new-instance (slot-location-using-layout slot new-layout))
               (standard-instance-access old-instance (slot-location-using-layout slot old-layout))))
    ;; Assemble the list of discarded values.
    (loop
       for slot in discarded-slots
       do (let ((value (standard-instance-access old-instance (slot-location-using-layout slot old-layout))))
            (when (not (eql value *secret-unbound-value*))
              (setf property-list (list* slot value
                                         property-list)))))
    ;; Obsolete the old instance, replacing it with the new instance.
    (mezzano.runtime::supersede-instance instance new-instance)
    ;; Magic.
    (update-instance-for-redefined-class instance added-slots discarded-slots property-list)))

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

(defmethod class-prototype ((class built-in-class))
  (when (not (slot-boundp class 'prototype))
    ;; This isn't really right...
    (setf (slot-value class 'prototype)
          (allocate-std-instance
           class
           (allocate-slot-storage (length (safe-class-slot-storage-layout class))
                                  *secret-unbound-value*)
           (safe-class-slot-storage-layout class))))
  (slot-value class 'prototype))

(defmethod class-prototype ((class clos-class))
  (declare (notinline slot-value (setf slot-value))) ; Bootstrap hack
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

(defgeneric function-keywords (method))

(defmethod function-keywords ((method standard-method))
  (let ((lambda-list-info (analyze-lambda-list (safe-method-lambda-list method))))
    (values (getf lambda-list-info :keywords)
            (getf lambda-list-info :allow-other-keys))))
