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

(defconstant +slot-unbound+ '+slot-unbound+
  "This value can be passed to (CAS SLOT-VALUE) to indicate that
the old or new values are expected to be unbound.")

(defun mapappend (fun &rest args)
  "mapappend is like mapcar except that the results are appended together:"
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))

;;;
;;; Standard instances
;;;

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

(defun uninitialized-standard-funcallable-instance-function (&rest x)
  (declare (ignore x))
  (error "The function of this funcallable instance has not been set."))

(defun fc-std-allocate-instance (class)
  (ensure-class-finalized class)
  (let* ((layout (safe-class-slot-storage-layout class))
         (instance (sys.int::%allocate-funcallable-instance
                    #'uninitialized-standard-funcallable-instance-function
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
(sys.int::defglobal *the-class-standard-reader-method*)
(sys.int::defglobal *the-class-standard-writer-method*)
(sys.int::defglobal *standard-class-effective-slots-location*) ; Position of the effective-slots slot in standard-class.
(sys.int::defglobal *standard-class-slot-storage-layout-location*)
(sys.int::defglobal *standard-class-hash-location*)
(sys.int::defglobal *standard-class-finalized-p-location*)
(sys.int::defglobal *standard-class-precedence-list-location*)
(sys.int::defglobal *standard-class-direct-default-initargs-location*)
(sys.int::defglobal *standard-class-default-initargs-location*)
(sys.int::defglobal *standard-effective-slot-definition-location-location*)
(sys.int::defglobal *standard-effective-slot-definition-name-location*)
(sys.int::defglobal *funcallable-standard-class-hash-location*)
(sys.int::defglobal *built-in-class-precedence-list-location*)
(sys.int::defglobal *built-in-class-hash-location*)

(defun standard-instance-access (instance location)
  (if (consp location)
      (cdr location)
      ;; Bypass INSTANCE-ACCESS because we know all locations in
      ;; standard instances have type T
      (sys.int::%object-ref-t instance (mezzano.runtime::location-offset-t location))))

(defun (setf standard-instance-access) (new-value instance location)
  (if (consp location)
      (setf (cdr location) new-value)
      (setf (sys.int::%object-ref-t instance (mezzano.runtime::location-offset-t location))
            new-value)))

(defun (sys.int::cas standard-instance-access) (old new instance location)
  (if (consp location)
      (sys.int::cas (cdr location) old new)
      (sys.int::cas (sys.int::%object-ref-t instance (mezzano.runtime::location-offset-t location))
                    old new)))

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
(defun fast-slot-read (instance location slot-definition)
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
                                    (safe-slot-definition-name slot-definition))))
          val))))

(defun fast-slot-write (new-value instance location slot-definition)
  (declare (ignore slot-definition))
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

(defparameter *fast-slot-value-readers*
  (make-hash-table :synchronized t :weakness :key))

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
                                       :fast-function (lambda (method next-emfun)
                                                        (declare (ignore method next-emfun))
                                                        (lambda (object)
                                                          (slot-value object slot-name)))
                                       :slot-definition direct-slot))
        (std-add-method gf
                        (make-instance 'standard-reader-method
                                       :lambda-list '(object)
                                       :qualifiers ()
                                       :specializers (list (find-class 'structure-object))
                                       :fast-function (lambda (method next-emfun)
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

(defun check-slot-type (value instance slot-name)
  (let* ((effective-slot (find-effective-slot instance slot-name))
         (typecheck (safe-slot-definition-typecheck effective-slot)))
    (when (and typecheck (not (funcall typecheck value)))
      (error 'type-error
             :datum value
             :expected-type (safe-slot-definition-type effective-slot)))))

(defun (setf std-slot-value) (value instance slot-name)
  (multiple-value-bind (slots location)
      (slot-location-in-instance instance slot-name)
    (when (not location)
      (slot-missing (class-of instance) instance slot-name 'setf value)
      (return-from std-slot-value
        value))
    (check-slot-type value instance slot-name)
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

(defparameter *fast-slot-value-writers*
  (make-hash-table :synchronized t :weakness :key))

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
                                       :fast-function (lambda (method next-emfun)
                                                        (declare (ignore method next-emfun))
                                                        (lambda (value object)
                                                          (setf (slot-value object slot-name) value)))
                                       :slot-definition direct-slot))
        (std-add-method gf
                        (make-instance 'standard-writer-method
                                       :lambda-list '(value object)
                                       :qualifiers ()
                                       :specializers (list *the-class-t* (find-class 'structure-object))
                                       :fast-function (lambda (method next-emfun)
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
    (when (not (eql new '+slot-unbound+))
      (check-slot-type new instance slot-name))
    (let* ((real-old (if (eql old '+slot-unbound+) *secret-unbound-value* old))
           (real-new (if (eql new '+slot-unbound+) *secret-unbound-value* new))
           (prev (sys.int::cas (standard-instance-access slots location) real-old real-new)))
      (cond ((eql prev *secret-unbound-value*)
             (if (eql old '+slot-unbound+)
                 '+slot-unbound+
                 (values (slot-unbound (class-of instance) instance slot-name))))
            (t
             prev)))))
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
    (if location
        (setf (standard-instance-access slots location) *secret-unbound-value*)
        (slot-missing (class-of instance) instance slot-name 'slot-makunbound)))
  instance)
(defun slot-makunbound (object slot-name)
  (let ((metaclass (class-of (class-of object))))
    (cond ((std-class-p metaclass)
           (std-slot-makunbound object slot-name))
          (t
           (let ((slot (find-effective-slot object slot-name)))
             (cond (slot
                    (slot-makunbound-using-class (class-of object) object slot))
                   (t
                    (slot-missing (class-of object) object slot-name 'slot-makunbound)
                    object)))))))

(defun std-slot-exists-p (object slot-name)
  (not (null (find-effective-slot object slot-name))))

(defun slot-exists-p (object slot-name)
  (let ((class (class-of object)))
    (cond ((standard-class-instance-p class)
           (std-slot-exists-p object slot-name))
          (t
           (slot-exists-p-using-class class object slot-name)))))

;;; class-of

(defun class-of (x)
  (cond ((sys.int::instance-or-funcallable-instance-p x)
         (let ((layout (sys.int::%instance-layout x)))
           (cond ((sys.int::layout-p layout)
                  (sys.int::layout-class layout))
                 (t
                  ;; Obsolete instance.
                  (class-of
                   (mezzano.runtime::obsolete-instance-layout-new-instance
                    layout))))))
        (t
         (built-in-class-of x))))

(defmacro find-class-cached (class)
  `(load-time-value (find-class ,class)))

(defun built-in-class-of (x)
  (cond
    ((null x)                       (find-class-cached 'null))
    ((sys.int::%value-has-tag-p x sys.int::+tag-object+)
     (let ((class (svref (load-time-value
                          (let ((vec (make-array (expt 2 sys.int::+object-type-size+)
                                                 :initial-element nil)))
                            (setf (aref vec sys.int::+object-tag-array-t+)
                                  (find-class 'simple-vector))
                            (setf (aref vec sys.int::+object-tag-array-fixnum+)
                                  (find-class 'sys.int::simple-array-fixnum))
                            (setf (aref vec sys.int::+object-tag-array-bit+)
                                  (find-class 'simple-bit-vector))
                            (setf (aref vec sys.int::+object-tag-array-unsigned-byte-2+)
                                  (find-class 'sys.int::simple-array-unsigned-byte-2))
                            (setf (aref vec sys.int::+object-tag-array-unsigned-byte-4+)
                                  (find-class 'sys.int::simple-array-unsigned-byte-4))
                            (setf (aref vec sys.int::+object-tag-array-unsigned-byte-8+)
                                  (find-class 'sys.int::simple-array-unsigned-byte-8))
                            (setf (aref vec sys.int::+object-tag-array-unsigned-byte-16+)
                                  (find-class 'sys.int::simple-array-unsigned-byte-16))
                            (setf (aref vec sys.int::+object-tag-array-unsigned-byte-32+)
                                  (find-class 'sys.int::simple-array-unsigned-byte-32))
                            (setf (aref vec sys.int::+object-tag-array-unsigned-byte-64+)
                                  (find-class 'sys.int::simple-array-unsigned-byte-64))
                            (setf (aref vec sys.int::+object-tag-array-signed-byte-1+)
                                  (find-class 'sys.int::simple-array-signed-byte-1))
                            (setf (aref vec sys.int::+object-tag-array-signed-byte-2+)
                                  (find-class 'sys.int::simple-array-signed-byte-2))
                            (setf (aref vec sys.int::+object-tag-array-signed-byte-4+)
                                  (find-class 'sys.int::simple-array-signed-byte-4))
                            (setf (aref vec sys.int::+object-tag-array-signed-byte-8+)
                                  (find-class 'sys.int::simple-array-signed-byte-8))
                            (setf (aref vec sys.int::+object-tag-array-signed-byte-16+)
                                  (find-class 'sys.int::simple-array-signed-byte-16))
                            (setf (aref vec sys.int::+object-tag-array-signed-byte-32+)
                                  (find-class 'sys.int::simple-array-signed-byte-32))
                            (setf (aref vec sys.int::+object-tag-array-signed-byte-64+)
                                  (find-class 'sys.int::simple-array-signed-byte-64))
                            (setf (aref vec sys.int::+object-tag-array-single-float+)
                                  (find-class 'sys.int::simple-array-single-float))
                            (setf (aref vec sys.int::+object-tag-array-double-float+)
                                  (find-class 'sys.int::simple-array-double-float))
                            (setf (aref vec sys.int::+object-tag-array-short-float+)
                                  (find-class 'sys.int::simple-array-short-float))
                            #+(or)
                            (setf (aref vec sys.int::+object-tag-array-long-float+)
                                  (find-class 'sys.int::simple-array-long-float))
                            (setf (aref vec sys.int::+object-tag-array-complex-single-float+)
                                  (find-class 'sys.int::simple-array-complex-single-float))
                            (setf (aref vec sys.int::+object-tag-array-complex-double-float+)
                                  (find-class 'sys.int::simple-array-complex-double-float))
                            (setf (aref vec sys.int::+object-tag-array-complex-short-float+)
                                  (find-class 'sys.int::simple-array-complex-short-float))
                            #+(or)
                            (setf (aref vec sys.int::+object-tag-array-complex-long-float+)
                                  (find-class 'sys.int::simple-array-complex-long-float))
                            (setf (aref vec sys.int::+object-tag-bignum+)
                                  (find-class 'bignum))
                            (setf (aref vec sys.int::+object-tag-ratio+)
                                  (find-class 'ratio))
                            (setf (aref vec sys.int::+object-tag-double-float+)
                                  (find-class 'double-float))
                            (setf (aref vec sys.int::+object-tag-short-float+)
                                  (find-class 'short-float))
                            #+(or)
                            (setf (aref vec sys.int::+object-tag-long-float+)
                                  (find-class 'long-float))
                            (setf (aref vec sys.int::+object-tag-complex-rational+)
                                  (find-class 'sys.int::complex-rational))
                            (setf (aref vec sys.int::+object-tag-complex-single-float+)
                                  (find-class 'sys.int::complex-single-float))
                            (setf (aref vec sys.int::+object-tag-complex-double-float+)
                                  (find-class 'sys.int::complex-double-float))
                            (setf (aref vec sys.int::+object-tag-complex-short-float+)
                                  (find-class 'sys.int::complex-short-float))
                            #+(or)
                            (setf (aref vec sys.int::+object-tag-complex-long-float+)
                                  (find-class 'sys.int::complex-long-float))
                            (setf (aref vec sys.int::+object-tag-symbol-value-cell+)
                                  (find-class 'mezzano.runtime::symbol-value-cell))
                            (setf (aref vec sys.int::+object-tag-mmx-vector+)
                                  (find-class 'mezzano.simd:mmx-vector))
                            ;; NIL is taken care of above.
                            (setf (aref vec sys.int::+object-tag-symbol+)
                                  (find-class 'symbol))
                            (setf (aref vec sys.int::+object-tag-sse-vector+)
                                  (find-class 'mezzano.simd:sse-vector))
                            (setf (aref vec sys.int::+object-tag-function-reference+)
                                  (find-class 'sys.int::function-reference))
                            (setf (aref vec sys.int::+object-tag-interrupt-frame+)
                                  (find-class 'sys.int::interrupt-frame))
                            (setf (aref vec sys.int::+object-tag-weak-pointer+)
                                  (find-class 'sys.int::weak-pointer))
                            (setf (aref vec sys.int::+object-tag-delimited-continuation+)
                                  (find-class 'mezzano.delimited-continuations:delimited-continuation))
                            (setf (aref vec sys.int::+object-tag-function+)
                                  (find-class 'compiled-function))
                            (setf (aref vec sys.int::+object-tag-closure+)
                                  (find-class 'sys.int::closure))
                            vec))
                         (sys.int::%object-tag x))))
       (cond (class)
             ((simple-string-p x)
              (find-class-cached 'simple-string))
             ((stringp x)
              (find-class-cached 'string))
             ((sys.int::simple-array-p x)
              (find-class-cached 'simple-array))
             ((bit-vector-p x)
              (find-class-cached 'bit-vector))
             ((vectorp x)
              (find-class-cached 'vector))
             ((arrayp x)
              (find-class-cached 'array))
             (t
              (error "Internal error, no class for value ~S" x)))))
    ((sys.int::fixnump x)           (find-class-cached 'fixnum))
    ((consp x)                      (find-class-cached 'cons))
    ((characterp x)                 (find-class-cached 'character))
    ((sys.int::short-float-p x)     (find-class-cached 'short-float))
    ((sys.int::single-float-p x)    (find-class-cached 'single-float))
    ((mezzano.internals.numbers.logical::small-byte-p x)
     (find-class-cached 'byte))
    ((sys.int::instance-header-p x) (find-class-cached 'sys.int::instance-header))
    (t
     (error "Internal error, no class for value ~S" x))))

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

(defun std-class-direct-subclasses (class)
  (let ((list (std-slot-value class 'direct-subclasses)))
    (and list ; list might not be initialized at this point.
         (mezzano.garbage-collection.weak-objects:weak-list-list list))))

(defun safe-class-direct-subclasses (class)
  (cond ((clos-class-instance-p class)
         (std-class-direct-subclasses class))
        (t
         (class-direct-subclasses class))))

(defun std-add-direct-subclass (superclass subclass)
  (let ((list (std-slot-value superclass 'direct-subclasses)))
    ;; The DIRECT-SUBCLASSES slot is initialized to NIL because the
    ;; cold-generator is unable to create weak lists.
    ;; Lazily initialize the list here, if needed.
    (when (not list)
      (setf (std-slot-value superclass 'direct-subclasses)
            (setf list (mezzano.garbage-collection.weak-objects:make-weak-list '()))))
    (pushnew subclass (mezzano.garbage-collection.weak-objects:weak-list-list list)))
  (values))

(defun safe-add-direct-subclass (superclass subclass)
  (cond ((and (clos-class-instance-p superclass)
              (clos-class-instance-p subclass))
         (std-add-direct-subclass superclass subclass))
        (t
         (add-direct-subclass superclass subclass))))

(defun std-remove-direct-subclass (superclass subclass)
  (let ((list (std-slot-value superclass 'direct-subclasses)))
    (when list
      (setf (mezzano.garbage-collection.weak-objects:weak-list-list list)
            (remove subclass (mezzano.garbage-collection.weak-objects:weak-list-list list)))))
  (values))

(defun safe-remove-direct-subclass (superclass subclass)
  (cond ((and (clos-class-instance-p superclass)
              (clos-class-instance-p subclass))
         (std-remove-direct-subclass superclass subclass))
        (t
         (remove-direct-subclass superclass subclass))))

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

(defun safe-class-precedence-list (class)
  (cond ((standard-class-instance-p class)
         (standard-instance-access class *standard-class-precedence-list-location*))
        ((built-in-class-instance-p class)
         (standard-instance-access class *built-in-class-precedence-list-location*))
        ((funcallable-standard-class-instance-p class)
         (std-slot-value class 'precedence-list))
        (t
         (class-precedence-list class))))
(defun (setf safe-class-precedence-list) (value class)
  (setf (std-slot-value class 'precedence-list) value))

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
        ((funcallable-standard-class-instance-p class)
         (standard-instance-access class *funcallable-standard-class-hash-location*))
        ((built-in-class-instance-p class)
         (standard-instance-access class *built-in-class-hash-location*))
        (t
         (std-slot-value class 'hash))))

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
         ;; Ignore any existing class if the proper name does not
         ;; match the supplied name.
         ;; SBCL does this check here in ENSURE-CLASS, CLISP does it
         ;; in ENSURE-CLASS-USING-CLASS. Both options seem valid.
         (let ((class (find-class name nil)))
           (if (and class (eql name (class-name class)))
               class
               nil))
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

(declaim (inline instance-of-p))
(defun instance-of-p (object layout)
  (and (sys.int::instance-p object)
       (eq (sys.int::%instance-layout object) layout)))

(declaim (inline funcallable-instance-of-p))
(defun funcallable-instance-of-p (object layout)
  (and (sys.int::funcallable-instance-p object)
       (eq (sys.int::%instance-layout object) layout)))

(defun std-class-p (metaclass)
  "Returns true if METACLASS is either STANDARD-CLASS or FUNCALLABLE-STANDARD-CLASS."
  (or (eq metaclass *the-class-standard-class*)
      (eq metaclass *the-class-funcallable-standard-class*)))

(defun standard-class-p (metaclass)
  "Returns true if METACLASS is exactly STANDARD-CLASS."
  (eq metaclass *the-class-standard-class*))

(defun standard-class-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly STANDARD-CLASS."
  (instance-of-p object *the-layout-standard-class*))

(defun funcallable-standard-class-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly FUNCALLABLE-STANDARD-CLASS."
  (instance-of-p object *the-layout-funcallable-standard-class*))

(defun built-in-class-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly BUILT-IN-CLASS."
  (instance-of-p object *the-layout-built-in-class*))

(defun standard-generic-function-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly STANDARD-GENERIC-FUNCTION."
  (funcallable-instance-of-p object *the-layout-standard-generic-function*))

(defun standard-method-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly STANDARD-METHOD."
  (instance-of-p object *the-layout-standard-method*))

(defun clos-class-p (metaclass)
  "Returns true if METACLASS is either STANDARD-CLASS, FUNCALLABLE-STANDARD-CLASS, or BUILT-IN-CLASS."
  (or (eq metaclass *the-class-standard-class*)
      (eq metaclass *the-class-funcallable-standard-class*)
      (eq metaclass *the-class-built-in-class*)))

(defun clos-class-instance-p (object)
  "Returns true if OBJECT is a direct instance of one of the clos classes."
  (clos-class-p (class-of object)))

(defun std-class-p (metaclass)
  "Returns true if METACLASS is either STANDARD-CLASS, FUNCALLABLE-STANDARD-CLASS."
  (or (eq metaclass *the-class-standard-class*)
      (eq metaclass *the-class-funcallable-standard-class*)))

(defun std-class-instance-p (object)
  "Returns true if OBJECT is a direct instance of one of either STANDARD-CLASS or FUNCALLABLE-STANDARD-CLASS."
  (std-class-p (class-of object)))

(defun standard-effective-slot-definition-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly STANDARD-EFFECTIVE-SLOT-DEFINITION."
  (instance-of-p object *the-layout-standard-effective-slot-definition*))

(defun standard-direct-slot-definition-instance-p (object)
  "Returns true if OBJECT is an up-to-date instance of exactly STANDARD-EFFECTIVE-SLOT-DEFINITION."
  (instance-of-p object *the-layout-standard-direct-slot-definition*))

(defun standard-slot-definition-instance-p (object)
  (or (standard-effective-slot-definition-instance-p object)
      (standard-direct-slot-definition-instance-p object)))

(defun convert-to-direct-slot-definition (class canonicalized-slot)
  (apply #'make-instance
         (apply #'direct-slot-definition-class
                class canonicalized-slot)
         canonicalized-slot))

(defun std-after-initialization-for-classes
    (class &key direct-superclasses direct-slots direct-default-initargs area sealed &allow-other-keys)
  (check-type area (or null (cons symbol null)))
  (check-type sealed (or null (cons boolean null)))
  (when (endp direct-superclasses)
    (setf direct-superclasses (default-direct-superclasses (class-of class))))
  (dolist (superclass direct-superclasses)
    (when (not (validate-superclass class superclass))
      (error "Superclass ~S incompatible with class ~S." (class-of superclass) (class-of class))))
  (setf (safe-class-direct-superclasses class) direct-superclasses)
  (dolist (superclass direct-superclasses)
    (safe-add-direct-subclass superclass class))
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
  (setf (std-slot-value class 'allocation-area) (first area))
  (setf (std-slot-value class 'sealed) (first sealed))
  (when (not (std-slot-boundp class 'hash))
    (setf (std-slot-value class 'hash) (next-class-hash-value)))
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

(defun safe-slot-definition-typecheck (effective-slot-definition)
  (declare (notinline slot-value)) ; Bootstrap hack
  (if (standard-effective-slot-definition-instance-p effective-slot-definition)
      (std-slot-value effective-slot-definition 'typecheck)
      (slot-definition-typecheck effective-slot-definition)))

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

(defun compute-class-heap-size (class instance-slots)
  (declare (notinline typep)) ; bootstrap hack.
  (cond ((and (endp instance-slots)
              (typep class 'funcallable-standard-class))
         ;; Funcallable instances must always be at least
         ;; 2 words long, for the implicit leading entry
         ;; point and function slots.
         2)
        (t
         (loop
            ;; Using the effective slot locations
            ;; accounts for the FC instance offset.
            for slot in instance-slots
            for location = (mezzano.runtime::location-offset-t
                            (safe-slot-definition-location slot))
            maximize (1+ location)))))

(defun std-finalize-inheritance (class)
  (dolist (super (safe-class-direct-superclasses class))
    (ensure-class-finalized super)
    (when (and (typep super 'std-class)
               (std-slot-value super 'sealed))
      (error "Superclass ~S of ~S is sealed and cannot be inherited from."
             super class)))
  (setf (safe-class-precedence-list class) (compute-class-precedence-list class))
  ;; Don't allow exotic function classes to be created.
  (cond ((typep class 'funcallable-standard-class)
         (unless (member (find-class 'funcallable-standard-object)
                         (safe-class-precedence-list class))
           (error "FUNCALLABLE-STANARD-OBJECT missing from CPL of class ~S, CPL: ~:S"
                  class (safe-class-precedence-list class))))
        (t
         (when (member (find-class 'funcallable-standard-object)
                       (safe-class-precedence-list class))
           (error "FUNCALLABLE-STANARD-OBJECT present in CPL of class ~S, CPL: ~:S"
                  class (safe-class-precedence-list class)))))
  (setf (safe-class-slots class) (compute-slots class))
  (setf (safe-class-default-initargs class) (compute-default-initargs class))
  (let* ((instance-slots (remove-if-not 'instance-slot-p
                                        (safe-class-slots class)))
         (instance-slot-vector
          (make-array (* (length instance-slots) 2)))
         (layout (sys.int::make-layout
                  :class class
                  :obsolete nil
                  :heap-size (compute-class-heap-size class instance-slots)
                  :heap-layout t
                  :area (std-slot-value class 'allocation-area)
                  :instance-slots instance-slot-vector)))
    (loop
       for i from 0 by 2
       for slot in instance-slots
       for slot-name = (safe-slot-definition-name slot)
       for location = (safe-slot-definition-location slot)
       do
       ;; (funcallable-)standard-instance-access requires that all slots
       ;; be of type T, as does the above heap layout.
         (assert (eql (mezzano.runtime::location-type location)
                      mezzano.runtime::+location-type-t+))
         (setf (aref instance-slot-vector i) slot-name
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
                                         16
                                         0)
     for slot in effective-slots
     do (case (safe-slot-definition-allocation slot)
          (:instance
           (setf (safe-slot-definition-location slot)
                 (mezzano.runtime::make-location
                  mezzano.runtime::+location-type-t+
                  next-instance-slot-index))
           (incf next-instance-slot-index 8))
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

(defun combine-direct-slot-types (direct-slots)
  (let* ((types (remove-duplicates (remove 't (mapcar #'safe-slot-definition-type direct-slots))
                                   :test #'sys.int::type-equal)))
    (cond ((endp types)
           't)
          ((endp (rest types))
           (first types))
          (t
           `(and ,@types)))))

(defun compute-typecheck-function (type-specifier)
  (when (eql type-specifier 't)
    ;; No type check function required.
    (return-from compute-typecheck-function nil))
  (let* ((type-symbol (cond ((symbolp type-specifier)
                             type-specifier)
                            ((and (consp type-specifier)
                                  (null (rest type-specifier)))
                             (first type-specifier))))
         (type-fn (and type-symbol
                       (let ((info (sys.int::type-info-for type-symbol nil)))
                         (and info (sys.int::type-info-type-symbol info))))))
    (macrolet ((specific-type (type)
                 `(and (sys.int::type-equal type-specifier ',type)
                       (lambda (object)
                         (declare (sys.int::lambda-name (typecheck ,type)))
                         (typep object ',type))))
               (known-type (type function)
                 `(and (sys.int::type-equal type-specifier ',type)
                       #',function)))
      (cond (type-fn)
            ;; Pick off some common and known types to avoid
            ;; going through the compiler.
            ;; These were picked by looking through all slot type declarations
            ;; and picking the most common/generic looking ones.
            ;; These are also the types required for bootstrapping, avoiding
            ;; a call into the compiler before the compiler has been loaded.
            ((specific-type (unsigned-byte 8)))
            ((specific-type (unsigned-byte 16)))
            ((specific-type (unsigned-byte 32)))
            ((specific-type (unsigned-byte 64)))
            ((specific-type (signed-byte 8)))
            ((specific-type (signed-byte 16)))
            ((specific-type (signed-byte 32)))
            ((specific-type (signed-byte 64)))
            ((known-type integer integerp))
            ((specific-type (integer 0)))
            ((known-type fixnum sys.int::fixnump))
            ((specific-type (and fixnum (integer 0))))
            ((specific-type boolean))
            ((known-type simple-vector simple-vector-p))
            ((known-type string stringp))
            ;; Bootstrap types
            ((specific-type (or null (integer 1)))) ; mailbox
            ((specific-type (or null (integer 0)))) ; semaphore
            ((specific-type (member :lf-ignore-cr :cr :lf :crlf :lfcr))) ; external-format
            (t
             (multiple-value-bind (expansion expanded-p)
                 (sys.int::typeexpand-1 type-specifier)
               (if expanded-p
                   (compute-typecheck-function expansion)
                   (compile nil `(lambda (object)
                                   (declare (sys.int::lambda-name (typecheck ,type-specifier)))
                                   (typep object ',type-specifier))))))))))

(defun std-compute-effective-slot-definition (class name direct-slots)
  (let ((initer (find-if-not #'null direct-slots
                             :key #'safe-slot-definition-initfunction))
        (type (combine-direct-slot-types direct-slots)))
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
     :allocation (safe-slot-definition-allocation (car direct-slots))
     :documentation (loop
                       for slot in direct-slots
                       when (safe-slot-definition-allocation slot)
                       do (return (safe-slot-definition-allocation slot)))
     :type type
     :typecheck (compute-typecheck-function type))))

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

(defun safe-generic-function-relevant-arguments (generic-function)
  (std-slot-value generic-function 'relevant-arguments))
(defun (setf safe-generic-function-relevant-arguments) (value generic-function)
  (setf (std-slot-value generic-function 'relevant-arguments) value))

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

(defun normalize-e-g-f-args (&rest all-keys &key generic-function-class method-class &allow-other-keys)
    ;; :GENERIC-FUNCTION-CLASS is not included as an initarg.
  (remf all-keys :generic-function-class)
  ;; Passing our own.
  (remf all-keys :method-class)
  ;; FIXME: What to do with this?
  (remf all-keys :environment)
  (when (and generic-function-class
             (symbolp generic-function-class))
    (setf generic-function-class (find-class generic-function-class)))
  (cond ((null method-class)
         (setf method-class *the-class-standard-method*))
        ((symbolp method-class)
         (setf method-class (find-class method-class))))
  (values generic-function-class
          (list* :method-class method-class
                 all-keys)))

;; Bootstrap note: This partially replicates the behaviour of
;; ENSURE-GENERIC-FUNCTION-USING-CLASS for standard-generic-function so
;; as to avoid calling generic functions when initially defining them.
(defun ensure-generic-function
       (function-name
        &rest all-keys
        &key generic-function-class &allow-other-keys)
  (cond ((and (symbolp function-name)
              (special-operator-p function-name))
         ;; Can't override special operators.
         (error 'sys.int::simple-program-error
                :format-control "~S names a special operator"
                :format-arguments (list function-name)))
        ((or (and (fboundp function-name)
                  (not (typep (fdefinition function-name) 'generic-function)))
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
  (let ((existing-gf (and (fboundp function-name)
                          (fdefinition function-name))))
    (cond ((and existing-gf
                (not all-keys)
                (standard-generic-function-instance-p existing-gf))
           ;; Bootstrap hack: Don't call reinitialize-instance for
           ;; standard generic functions when no initargs are specifed.
           ;; DEFMETHOD-1 does this to fetch the generic function.
           existing-gf)
          ((and (not existing-gf)
                (not generic-function-class))
           ;; Bootstrap hack: Bypass E-G-F-U-C when defining standard
           ;; generic functions, the required generics/methods might not have
           ;; been defined yet.
           (multiple-value-bind (generic-function-class initargs)
               (apply #'normalize-e-g-f-args all-keys)
             (declare (ignore generic-function-class))
             (let ((gf (apply #'make-instance-standard-generic-function
                              *the-class-standard-gf*
                              :name function-name
                              initargs)))
               ;; Not entirely sure where this should be done.
               ;; SBCL seems to do it in (ENSURE-GENERIC-FUNCTION-USING-CLASS NULL).
               (setf (fdefinition function-name) gf)
               gf)))
          (t
           ;; Off we go, this is the normal path.
           (apply #'ensure-generic-function-using-class
                  existing-gf
                  function-name
                  all-keys)))))

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
  (when (eq (class-of gf) *the-class-standard-gf*)
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
         (clear-single-dispatch-emf-table (classes-to-emf-table gf)))
        ((classes-to-emf-table gf)
         (clear-emf-cache (classes-to-emf-table gf)))))

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
    (when (or (not (eql (length required-args) (length argument-precedence)))
              (set-difference required-args argument-precedence)
              (set-difference argument-precedence required-args))
      (error 'sys.int::simple-program-error
             :format-control "Argument precedence list ~:S does not match required arguments ~:S"
             :format-arguments (list argument-precedence required-args)))
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
    (dolist (m (safe-generic-function-methods gf))
      (do ((i 0 (1+ i))
           (spec (safe-method-specializers m) (rest spec)))
          ((null spec))
        (unless (eql (first spec) class-t)
          (setf (bit relevant-args i) 1))))
    (setf (safe-generic-function-relevant-arguments gf) relevant-args))
  (reset-gf-emf-table gf)
  (let ((tracep (safe-generic-function-trace-p gf)))
    (setf (classes-to-emf-table gf) (cond ((and (not tracep)
                                                (generic-function-single-dispatch-p gf))
                                           (make-single-dispatch-emf-table gf))
                                          ((and (not tracep)
                                                (generic-function-unspecialized-dispatch-p gf))
                                           nil)
                                          (t
                                           (make-emf-cache gf)))))
  (set-funcallable-instance-function
   gf
   (funcall (if (standard-generic-function-instance-p gf)
                #'std-compute-discriminating-function
                #'compute-discriminating-function)
            gf))
  (values))

;;; make-instance-standard-generic-function creates and initializes an
;;; instance of standard-generic-function without falling into method lookup.
;;; However, it cannot be called until standard-generic-function exists.

(defun initialize-instance-standard-generic-function (gf &key
                                                           name
                                                           lambda-list
                                                           method-class
                                                           documentation
                                                           method-combination
                                                           argument-precedence-order
                                                           declarations
                                                           source-location)
  (setf (safe-generic-function-name gf) name)
  (setf (safe-generic-function-lambda-list gf) lambda-list)
  (setf (safe-generic-function-methods gf) ())
  (setf (safe-generic-function-method-combination gf) method-combination)
  (setf (safe-generic-function-declarations gf) declarations)
  (setf (std-slot-value gf 'documentation) documentation)
  (setf (std-slot-value gf 'dependents) '())
  (setf (classes-to-emf-table gf) nil)
  (setf (safe-generic-function-argument-precedence-order gf) argument-precedence-order)
  (setf (safe-generic-function-method-class gf) method-class)
  (setf (std-slot-value gf 'source-location) source-location)
  (setf (std-slot-value gf 'tracep) 'nil)
  (finalize-generic-function gf))

(defun make-instance-standard-generic-function
       (generic-function-class &rest initargs)
  (declare (ignore generic-function-class))
  (let ((gf (fc-std-allocate-instance *the-class-standard-gf*)))
    (apply #'initialize-instance-standard-generic-function
           gf initargs)
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
                                      &rest args
                                      &key
                                        lambda-list
                                        qualifiers
                                        specializers
                                        function
                                        fast-function
                                        documentation)
  (declare (ignore method-class))
  (let ((method (std-allocate-instance *the-class-standard-method*)))
    (setf (safe-method-lambda-list method) lambda-list)
    (setf (safe-method-qualifiers method) qualifiers)
    (setf (safe-method-specializers method) specializers)
    (setf (safe-method-generic-function method) nil)
    (when function
      (setf (safe-method-function method) function))
    (when fast-function
      (setf (std-slot-value method 'fast-function) fast-function))
    (setf (std-slot-value method 'documentation) documentation)
    (apply #'std-after-initialization-for-methods method args)
    method))

(defun std-after-initialization-for-methods (method &rest initargs)
  (declare (ignore initargs))
  (when (and (not (std-slot-boundp method 'function))
             (std-slot-boundp method 'fast-function))
    (setf (std-slot-value method 'function)
          (lambda (args next-method-list)
            (apply (method-fast-function
                    method
                    (if next-method-list
                        (lambda (&rest args)
                          (funcall (method-function (car next-method-list))
                                   args
                                   (cdr next-method-list)))
                        nil)
                    next-method-list)
                   args)))))

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
              gf method))
    ;; If a method accepts keywords, then it must accept all the keywords that the generic function accepts
    (when (and (member '&key (safe-method-lambda-list method))
               (not (member '&allow-other-keys (safe-method-lambda-list method))))
      (let ((missing-keywords (set-difference (getf gf-ll :keywords)
                                              (getf method-ll :keywords))))
        (assert (endp missing-keywords)
                (gf method)
                "Method ~S must accept all keywords accepted by generic function ~S. It is missing ~:S"
                method gf missing-keywords)))))

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
    (safe-add-direct-method specializer method))
  (finalize-generic-function gf)
  (safe-map-dependents gf (lambda (dep)
                            (update-dependent gf dep 'add-method method)))
  gf)

(defun std-remove-method (gf method)
  (setf (safe-generic-function-methods gf)
        (remove method (safe-generic-function-methods gf)))
  (setf (safe-method-generic-function method) nil)
  (dolist (specializer (safe-method-specializers method))
    (safe-remove-direct-method specializer method))
  (finalize-generic-function gf)
  (safe-map-dependents gf (lambda (dep)
                            (update-dependent gf dep 'remove-method method)))
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

;;; Standard specializer direct methods.

(defun std-add-direct-method (specializer method)
  (pushnew method (std-slot-value specializer 'direct-methods))
  (values))

(defun safe-add-direct-method (specializer method)
  (cond ((and (clos-class-instance-p specializer)
              (standard-method-instance-p method))
         (std-add-direct-method specializer method))
        (t
         (add-direct-method specializer method))))

(defun std-remove-direct-method (specializer method)
  (setf (std-slot-value specializer 'direct-methods)
        (remove method (std-slot-value specializer 'direct-methods)))
  (values))

(defun safe-remove-direct-method (specializer method)
  (cond ((and (clos-class-instance-p specializer)
              (standard-method-instance-p method))
         (std-remove-direct-method specializer method))
        (t
         (add-direct-method specializer method))))

(defun std-specializer-direct-generic-functions (specializer)
  (loop
     with result = '()
     for method in (std-specializer-direct-methods specializer)
     for gf = (safe-method-generic-function method)
     when gf
     do (pushnew gf result)
     finally (return result)))

(defun safe-specializer-direct-generic-functions (specializer)
  (cond ((clos-class-instance-p specializer)
         (std-specializer-direct-generic-functions specializer))
        (t
         (specializer-direct-generic-functions specializer))))

(defun std-specializer-direct-methods (specializer)
  (std-slot-value specializer 'direct-methods))

(defun safe-specializer-direct-methods (specializer)
  (cond ((clos-class-instance-p specializer)
         (std-specializer-direct-methods specializer))
        (t
         (specializer-direct-methods specializer))))

(defun std-add-dependent (metaobject dependent)
  (pushnew dependent (std-slot-value metaobject 'dependents))
  (values))

(defun safe-add-dependent (metaobject dependent)
  (add-dependent metaobject dependent))

(defun std-remove-dependent (metaobject dependent)
  (setf (std-slot-value metaobject 'dependents)
        (remove dependent (std-slot-value metaobject 'dependents)))
  (values))

(defun safe-remove-dependent (metaobject dependent)
  (remove-dependent metaobject dependent))

(defun std-map-dependents (metaobject function)
  (mapc function (std-slot-value metaobject 'dependents))
  (values))

(defun safe-map-dependents (metaobject function)
  (cond ((or (std-class-instance-p metaobject)
             (standard-generic-function-instance-p metaobject))
         (std-map-dependents metaobject function))
        (t
         (map-dependents metaobject function))))

;;; Reader and write methods

(defun add-reader-method (class fn-name direct-slot-definition)
  (add-method (ensure-generic-function fn-name :lambda-list '(object))
              (let* ((slot-name (safe-slot-definition-name direct-slot-definition))
                     (initargs (list :lambda-list '(object)
                                     :qualifiers ()
                                     :specializers (list class)
                                     :fast-function (lambda (method next-emfun)
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
                                    :fast-function (lambda (method next-emfun)
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

(defun compute-reader-discriminator (gf emf-table argument-offset slot-definition)
  (lambda (object)
    (let* ((class (class-of object))
           (location (single-dispatch-emf-entry emf-table class)))
      (if location
          (fast-slot-read object location slot-definition)
          (slow-single-dispatch-method-lookup* gf argument-offset (list object) :reader)))))

(defstruct (writer-discriminator-entry
             :sealed)
  location
  typecheck
  type)

(defun compute-writer-discriminator (gf emf-table argument-offset slot-definition)
  (lambda (new-value object)
    (let* ((class (class-of object))
           (location (single-dispatch-emf-entry emf-table class)))
      (cond (location
             (let ((typecheck (writer-discriminator-entry-typecheck location)))
               (when (and typecheck (not (funcall typecheck new-value)))
                 (error 'type-error
                        :datum new-value
                        :expected-type (writer-discriminator-entry-type location))))
             (fast-slot-write new-value object (writer-discriminator-entry-location location) slot-definition))
            (t
             (slow-single-dispatch-method-lookup* gf argument-offset (list new-value object) :writer))))))

(defun compute-1-effective-discriminator (gf emf-table argument-offset)
  ;; TODO: This table should be a weak-alist, but that's not thread-safe.
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
                            ;; Hack around a bug... The table gets corrupted somehow and returns an integer
                            (when (and emfun (not (functionp emfun)))
                              (setf emfun nil)
                              (clear-single-dispatch-emf-table emf-table))
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
            (let* ((arg (nth argument-offset args))
                   (eql-emfun (assoc arg eql-table)))
              (if eql-emfun
                  (apply (cdr eql-emfun) args)
                  (let* ((class (class-of arg))
                         (emfun (single-dispatch-emf-entry emf-table class)))
                    (if emfun
                        (apply emfun args)
                        (slow-single-dispatch-method-lookup gf args class))))))))))

(defun compute-n-effective-discriminator (gf emf-table n-required-args)
  (lambda (&rest args sys.int::&count arg-count)
    (declare (dynamic-extent args))
    (when (< arg-count n-required-args)
      (error 'sys.int::simple-program-error
             :format-control "Too few arguments to generic function ~S."
             :format-arguments (list gf)))
    (let* ((emfun (emf-cache-lookup emf-table args)))
      (if emfun
          (apply emfun args)
          ;; Delay copying the argument list until it really needs to have indefinite-extent.
          (slow-method-lookup gf (copy-list args))))))

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

(sys.int::defglobal *redefinition-flush-table* (make-hash-table))
(sys.int::defglobal *redefinition-flush-table-lock*
    (mezzano.supervisor:make-mutex '*redefinition-flush-table*))

(defun register-redefinition-flusher (gf class)
  "Cause GF's EMF table to be flushed when CLASS is redefined."
  (mezzano.supervisor:with-mutex (*redefinition-flush-table-lock*)
    (pushnew gf (gethash class *redefinition-flush-table* '()))))

(defun flush-emf-tables-on-class-redefinition (class)
  ;; CLASS is being redefined, flush any associated the EMF tables and wipe away the entry.
  (let ((gfs (mezzano.supervisor:with-mutex (*redefinition-flush-table-lock*)
               (prog1
                   (gethash class *redefinition-flush-table*)
                 (remhash class *redefinition-flush-table*)))))
    (dolist (gf gfs)
      (reset-gf-emf-table gf))))

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
                (register-redefinition-flusher gf class)
                (let* ((instance (first args))
                       (slot-def (accessor-method-slot-definition (first applicable-methods)))
                       (slot-name (slot-definition-name slot-def))
                       (effective-slot (find-effective-slot instance slot-name)))
                  (cond (effective-slot
                         (let ((location (safe-slot-definition-location effective-slot)))
                           (setf (single-dispatch-emf-entry emf-table class) location)
                           (fast-slot-read (first args) location slot-def)))
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
                       (slot-def (accessor-method-slot-definition (first applicable-methods)))
                       (slot-name (slot-definition-name slot-def))
                       (effective-slot (find-effective-slot instance slot-name))
                       (new-value (first args)))
                  (register-redefinition-flusher gf class)
                  (cond (effective-slot
                         (let* ((location (safe-slot-definition-location effective-slot))
                                (typecheck (safe-slot-definition-typecheck effective-slot))
                                (type (safe-slot-definition-type effective-slot))
                                (entry (make-writer-discriminator-entry
                                        :location location
                                        :typecheck typecheck
                                        :type type)))
                           (setf (single-dispatch-emf-entry emf-table class) entry)
                           (when (and typecheck (not (funcall typecheck new-value)))
                             (error 'type-error
                                    :datum new-value
                                    :expected-type type))
                           (fast-slot-write new-value instance location slot-def)))
                        (t
                         ;; Slot not present, fall back on SLOT-VALUE.
                         (setf (slot-value instance slot-name) new-value)))))
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
                (set-funcallable-instance-function
                 gf
                 (compute-reader-discriminator gf emf-table argument-offset
                                               (accessor-method-slot-definition (first applicable-methods))))
                (apply gf args))
               ((and (not (null applicable-methods))
                     (every 'primary-method-p applicable-methods)
                     (typep (first applicable-methods) 'standard-writer-method)
                     (std-class-p (class-of class)))
                ;; Switch to writer-method.
                (set-funcallable-instance-function
                 gf
                 (compute-writer-discriminator gf emf-table argument-offset
                                               (accessor-method-slot-definition (first applicable-methods))))
                (apply gf args))
               (t ;; Switch to 1-effective.
                (set-funcallable-instance-function
                 gf
                 (compute-1-effective-discriminator gf emf-table argument-offset))
                (slow-single-dispatch-method-lookup gf args (class-of (nth argument-offset args))))))))))

(defun safe-generic-function-trace-p (generic-function)
  (if (standard-generic-function-instance-p generic-function)
      (std-slot-value generic-function 'tracep)
      (generic-function-trace-p generic-function)))

(defun std-compute-discriminating-function (gf)
  (lambda (&rest args)
    (multiple-value-bind (single-dispatch-p argument-offset)
        (generic-function-single-dispatch-p gf)
      (let ((tracep (safe-generic-function-trace-p gf)))
        ;; Force the n-effective discriminator when tracing, that
        ;; eliminate special cases for reader/writer methods too.
        (cond ((and (not tracep) single-dispatch-p)
               (slow-single-dispatch-method-lookup* gf argument-offset args :never-called))
              ((and (not tracep) (generic-function-unspecialized-dispatch-p gf))
               (slow-unspecialized-dispatch-method-lookup gf args))
              (t
               (set-funcallable-instance-function
                gf
                (compute-n-effective-discriminator gf (classes-to-emf-table gf) (length (gf-required-arglist gf))))
               (apply gf args)))))))

(defun slow-method-lookup (gf args)
  (let ((std-gf-p (eq (class-of gf) *the-class-standard-gf*))
        (classes (mapcar #'class-of (required-portion gf args))))
    (multiple-value-bind (applicable-methods validp)
        (if std-gf-p
            (std-compute-applicable-methods-using-classes gf classes)
            (compute-applicable-methods-using-classes gf classes))
      (unless validp
        (setf applicable-methods (if std-gf-p
                                     (std-compute-applicable-methods gf args)
                                     (compute-applicable-methods gf args))))
      (when (not applicable-methods)
        (return-from slow-method-lookup
          (apply #'no-applicable-method gf args)))
      (let ((emfun (if std-gf-p
                       (std-compute-effective-method-function gf applicable-methods)
                       (compute-effective-method-function gf applicable-methods))))
        (insert-into-emf-cache (classes-to-emf-table gf) args emfun)
        (apply emfun args)))))

(defun reordered-argument (gf arguments index)
  (nth (reordered-method-argument-index gf index)
       arguments))

(defun reordered-method-specializer (gf method index)
  (nth (reordered-method-argument-index gf index)
       (method-specializers method)))

(defun has-eql-specializer-in-reordered-position-p (gf method index)
  (typep (reordered-method-specializer gf method index) 'eql-specializer))

(defun method-partial-specializer-match-p (gf method specializers-reordered)
  (loop
     for specializer in specializers-reordered
     for index from 0
     for method-spec in (reorder-method-specializers
                         gf (safe-method-specializers method))
     unless (etypecase method-spec
              (class
               (etypecase specializer
                 (class
                  (subclassp specializer method-spec))
                 (eql-specializer
                  (typep (eql-specializer-object specializer) method-spec))))
              (eql-specializer
               (etypecase specializer
                 (class
                  ;; Filter these out entirely, matching the behaviour
                  ;; of the emf cache.
                  nil)
                 (eql-specializer
                  (eql specializer method-spec)))))
     do (return nil)
     finally (return t)))

(defun compute-applicable-methods-partial (gf specializers-reordered)
  "Return a list of all methods matching SPECIALIZERS-REORDERED.
This list may be shorter than the required portion. Missing argumnts will
always match."
  (loop
     for method in (safe-generic-function-methods gf)
     when (method-partial-specializer-match-p gf method specializers-reordered)
     collect method))

(defun slow-single-dispatch-method-lookup (gf args class)
  (let* ((classes (mapcar #'class-of (required-portion gf args)))
         (standard-gf-p (eql (class-of gf) *the-class-standard-gf*)))
    (multiple-value-bind (applicable-methods validp)
        (if standard-gf-p
            (std-compute-applicable-methods-using-classes gf classes)
            (compute-applicable-methods-using-classes gf classes))
      (when (not validp)
        ;; EQL specialized.
        (setf applicable-methods (if standard-gf-p
                                     (std-compute-applicable-methods gf args)
                                     (compute-applicable-methods gf args))))
      (let ((emfun (cond ((and applicable-methods
                               (every 'primary-method-p applicable-methods)
                               standard-gf-p
                               (eql (class-of (first applicable-methods))
                                    *the-class-standard-reader-method*)
                               (std-class-instance-p class))
                          ;; This is a reader method and a specialized accessor can be used.
                          (let* ((instance (first args))
                                 (slot-def (accessor-method-slot-definition (first applicable-methods)))
                                 (slot-name (slot-definition-name slot-def))
                                 (effective-slot (find-effective-slot instance slot-name)))
                            (register-redefinition-flusher gf class)
                            (cond (effective-slot
                                   (let ((location (safe-slot-definition-location effective-slot)))
                                     (lambda (object)
                                       (declare (sys.int::lambda-name fast-slot-reader))
                                       (fast-slot-read object location slot-def))))
                                  (t
                                   ;; Slot not present, fall back on SLOT-VALUE.
                                   (lambda (object)
                                     (declare (sys.int::lambda-name slow-slot-reader))
                                     (slot-value object slot-name))))))
                         ((and applicable-methods
                               (every 'primary-method-p applicable-methods)
                               standard-gf-p
                               (eql (class-of (first applicable-methods))
                                    *the-class-standard-writer-method*)
                               (std-class-instance-p class))
                          ;; This is a writer method and a specialized accessor can be used.
                          (let* ((instance (second args))
                                 (slot-def (accessor-method-slot-definition (first applicable-methods)))
                                 (slot-name (slot-definition-name slot-def))
                                 (effective-slot (find-effective-slot instance slot-name)))
                            (register-redefinition-flusher gf class)
                            (cond (effective-slot
                                   (let* ((location (safe-slot-definition-location effective-slot))
                                          (typecheck (safe-slot-definition-typecheck effective-slot))
                                          (type (safe-slot-definition-type effective-slot)))
                                     (lambda (new-value object)
                                       (when (and typecheck (not (funcall typecheck new-value)))
                                         (error 'type-error
                                                :datum new-value
                                                :expected-type type))
                                       (fast-slot-write new-value object location slot-def))))
                                  (t
                                   ;; Slot not present, fall back on SLOT-VALUE.
                                   (lambda (value object)
                                     (declare (sys.int::lambda-name slow-slot-writer))
                                     (setf (slot-value object slot-name) value))))))
                         (applicable-methods
                          (std-compute-effective-method-function gf applicable-methods))
                         (t
                          (lambda (&rest args)
                            (apply #'no-applicable-method gf args))))))
        ;; Cache is only valid for non-eql methods.
        (when validp
          (setf (single-dispatch-emf-entry (classes-to-emf-table gf) class) emfun))
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

(defun reordered-method-argument-index (gf index)
  "Convert from a reordered argument index to a normal argument index."
  (let ((ordering-table (argument-reordering-table gf)))
    (cond (ordering-table
           (aref ordering-table index))
          (t
           index))))

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

(defun method-fast-function (method next-emfun next-methods)
  (declare (notinline slot-value))
  (let ((fast-fn (cond ((standard-method-instance-p method)
                        (and (std-slot-boundp method 'fast-function)
                             (std-slot-value method 'fast-function)))
                       ((typep method 'standard-method)
                        (and (slot-boundp method 'fast-function)
                             (slot-value method 'fast-function))))))
    (cond (fast-fn
           (funcall fast-fn method next-emfun))
          (t
           (let ((fn (safe-method-function method)))
             (lambda (&rest args)
               (funcall fn args next-methods)))))))

(defun compute-primary-emfun (methods)
  (if (null methods)
      nil
      (let ((next-emfun (compute-primary-emfun (cdr methods))))
        (method-fast-function (car methods) next-emfun (cdr methods)))))

(defun applicable-methods-keywords (gf methods)
  (let* ((gf-lambda-list-info (analyze-lambda-list (safe-generic-function-lambda-list gf)))
         (any-has-keys (member '&key (safe-generic-function-lambda-list gf)))
         (keys (getf gf-lambda-list-info :keywords)))
    (when (getf gf-lambda-list-info :allow-other-keys)
      (return-from applicable-methods-keywords (values nil t)))
    (dolist (method methods)
      (when (member '&key (safe-method-lambda-list method))
        (multiple-value-bind (method-keys method-aok)
            (function-keywords method)
          (when method-aok
            (return-from applicable-methods-keywords (values nil t)))
          (setf keys (union keys method-keys))
          (setf any-has-keys t))))
    (values keys (not any-has-keys))))

(defun check-method-keyword-arguments (generic-function keyword-args valid-keywords)
  (when (getf keyword-args :allow-other-keys)
    (return-from check-method-keyword-arguments))
  (loop
     for key in keyword-args by #'cddr
     when (and (not (eql key :allow-other-keys))
               (not (member key valid-keywords)))
     do (error 'sys.int::simple-program-error
               :format-control "Unknown &KEY argument ~S when calling ~S. Expected one of ~:S."
               :format-arguments (list key generic-function valid-keywords))))

(defun std-compute-effective-method-function-with-standard-method-combination (gf methods)
  (multiple-value-bind (keywords suppress-keyword-checking)
      (applicable-methods-keywords gf methods)
    (let ((inner-fn (std-compute-effective-method-function-with-standard-method-combination-1 gf methods)))
      (cond (suppress-keyword-checking
             inner-fn)
            (t
             (let* ((gf-lambda-list-info (analyze-lambda-list (safe-generic-function-lambda-list gf)))
                    (key-arg-index (+ (length (getf gf-lambda-list-info :required-names))
                                      (length (getf gf-lambda-list-info :optional-args)))))
               (lambda (&rest args)
                 (declare (dynamic-extent args))
                 (check-method-keyword-arguments gf (nthcdr key-arg-index args) keywords)
                 (apply inner-fn args))))))))

(defun std-compute-effective-method-function-with-standard-method-combination-1 (gf methods)
  (let ((primaries (remove-if-not #'primary-method-p methods))
        (around (find-if #'around-method-p methods)))
    (when (null primaries)
      (error "No applicable primary methods for the generic function ~S." gf))
    (if around
        (let ((next-emfun
                (std-compute-effective-method-function-with-standard-method-combination-1
                 gf (remove around methods))))
          ;; FIXME: Method list isn't sorted properly for this...
          (method-fast-function around next-emfun (remove around methods)))
        (let ((primary (compute-primary-emfun primaries))
              (befores (mapcar (lambda (m) (method-fast-function m nil '()))
                               (remove-if-not #'before-method-p methods)))
              (reverse-afters (mapcar (lambda (m) (method-fast-function m nil '()))
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

(defun generate-method-combination-effective-method (name effective-method-body gf methods)
  (multiple-value-bind (keywords suppress-keyword-checking)
      (applicable-methods-keywords gf methods)
    (let* ((method-args (gensym "ARGS"))
           (gf-lambda-list-info (analyze-lambda-list (safe-generic-function-lambda-list gf)))
           (key-arg-index (+ (length (getf gf-lambda-list-info :required-names))
                             (length (getf gf-lambda-list-info :optional-args))))
           (tracep (safe-generic-function-trace-p gf)))
      ;; TODO: make the lambda-list here refect the actual lambda list more accurately.
      `(lambda (&rest ,method-args)
         (declare (sys.int::lambda-name (effective-method ,@name)))
         ,@(when (not suppress-keyword-checking)
             (list `(check-method-keyword-arguments ',gf (nthcdr ',key-arg-index ,method-args) ',keywords)))
         (macrolet ((call-method (method &optional next-method-list)
                      (when (listp method)
                        (assert (eql (first method) 'make-method)))
                      (cond ((listp method)
                             (assert (eql (first method) 'make-method))
                             (assert (eql (length method) 2))
                             (second method))
                            (t
                             `(,(if ',tracep
                                    `(lambda (ff args)
                                       (mezzano.internals::trace-method-invocation ',',gf ',method ff args))
                                    'apply)
                                (method-fast-function
                                 ',method
                                 ,(if next-method-list
                                      `(lambda (&rest ,',method-args)
                                         (call-method ,(first next-method-list)
                                                      ,(rest next-method-list)))
                                      nil)
                                 ;; Ugh.
                                 (list ,@(loop
                                            for next in next-method-list
                                            collect (cond ((listp next)
                                                           (assert (eql (first next) 'make-method))
                                                           (assert (eql (length next) 2))
                                                           ;; ???
                                                           `(make-instance 'standard-method
                                                                           :function (lambda (,',method-args .next-methods.)
                                                                                       (declare (ignore .next-methods.))
                                                                                       (progn ,(second next)))))
                                                          (t `',next)))))
                                ,',method-args))))
                    (make-method (form)
                      (declare (ignore form))
                      (error "MAKE-METHOD must be either the method argument or a next-method supplied to CALL-METHOD.")))
           ,effective-method-body)))))

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
    (cond ((or mc
               (safe-generic-function-trace-p gf))
           (let* ((mc-object (method-combination-object-method-combination mc))
                  (effective-method-body (compute-effective-method gf mc methods))
                  (name (generate-method-combination-effective-method-name gf mc-object methods)))
             (eval (generate-method-combination-effective-method name effective-method-body gf methods))))
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
;;; I guess making the appropriate slots unbound if the class is not finalized,
;;; then filling them in during finalization would work.

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
    (slot-value class 'precedence-list)))
(defgeneric class-slots (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'effective-slots)))
;; TODO: Prototypes for built-in classes
(defgeneric class-prototype (class)
  (:method ((class std-class))
    (declare (notinline slot-boundp slot-value (setf slot-value))) ; bootstrap hack
    (when (not (slot-boundp class 'prototype))
      (setf (slot-value class 'prototype) (allocate-instance class)))
    (slot-value class 'prototype))
  (:method ((class built-in-class))
    ;; FIXME: This is a bit weird.
    ;; Cook up a layout & instance for this class.
    (sys.int::%allocate-instance
     (sys.int::make-layout :class class
                           :obsolete nil
                           :heap-size 0
                           :heap-layout t
                           :area nil
                           :instance-slots #()))))
(defgeneric class-sealed (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'sealed)))
(defgeneric class-allocation-area (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'allocation-area)))
(defgeneric class-layout (class)
  (:method ((class clos-class))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value class 'slot-storage-layout)))

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
(defgeneric slot-definition-typecheck (effective-slot-definition)
  (:method ((slot-definition standard-effective-slot-definition))
    (declare (notinline slot-value)) ; bootstrap hack
    (slot-value slot-definition 'typecheck)))
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

;;; Specializer metaobject readers

(defgeneric specializer-direct-generic-functions (specializer))
(defmethod specializer-direct-generic-functions ((specializer class))
  (std-specializer-direct-generic-functions specializer))

(defgeneric specializer-direct-methods (specializer))
(defmethod specializer-direct-methods ((specializer class))
  (std-specializer-direct-methods specializer))

;;; Metaobject writer methods

(defgeneric (setf class-name) (new-name class)
  (:method (new-name (class class))
    (reinitialize-instance class :name new-name)
    new-name))

(defgeneric (setf generic-function-name) (new-name generic-function)
  (:method (new-name (generic-function generic-function))
    (reinitialize-instance generic-function :name new-name)
    new-name))

;;; Subclass management

(defgeneric class-direct-subclasses (class)
  (:method ((class class))
    (std-class-direct-subclasses class)))

(defgeneric add-direct-subclass (superclass subclass)
  (:method ((superclass class) (subclass class))
    (std-add-direct-subclass superclass subclass)))

(defgeneric remove-direct-subclass (superclass subclass)
  (:method ((superclass class) (subclass class))
    (std-remove-direct-subclass superclass subclass)))

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

(defgeneric add-direct-method (specializer method))
(defmethod add-direct-method ((specializer class) (method method))
  (std-add-direct-method specializer method))

(defgeneric remove-direct-method (specializer method))
(defmethod remove-direct-method ((specializer class) (method method))
  (std-remove-direct-method specializer method))

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

(defgeneric slot-exists-p-using-class (class object slot-name)
  (:method ((class std-class) object slot-name)
    (std-slot-exists-p object slot-name))
  (:method ((class built-in-class) object slot-name)
    nil))

;;; Stuff...

(defgeneric direct-slot-definition-class (class &rest initargs))
(defmethod direct-slot-definition-class ((class std-class) &rest initargs)
  (declare (ignore initargs))
  *the-class-standard-direct-slot-definition*)

(defgeneric effective-slot-definition-class (class &rest initargs))
(defmethod effective-slot-definition-class ((class std-class) &rest initargs)
  (declare (ignore initargs))
  *the-class-standard-effective-slot-definition*)

(defgeneric function-keywords (method))
(defmethod function-keywords ((method standard-method))
  (let ((lambda-list-info (analyze-lambda-list (safe-method-lambda-list method))))
    (values (getf lambda-list-info :keywords)
            (getf lambda-list-info :allow-other-keys))))

(defgeneric compute-applicable-methods-using-classes (generic-function classes))
(defmethod compute-applicable-methods-using-classes ((generic-function standard-generic-function) classes)
  (std-compute-applicable-methods-using-classes generic-function classes))

(defgeneric compute-applicable-methods (generic-function arguments))
(defmethod compute-applicable-methods ((generic-function standard-generic-function) arguments)
  (std-compute-applicable-methods generic-function arguments))

(defgeneric ensure-generic-function-using-class
    (generic-function
     function-name
     &key generic-function-class &allow-other-keys))

(defmethod ensure-generic-function-using-class
    ((generic-function generic-function)
     function-name
     &rest all-keys
     &key &allow-other-keys)
  (multiple-value-bind (generic-function-class initargs)
      (apply #'normalize-e-g-f-args all-keys)
    (when (and generic-function-class
               (not (eql (class-of generic-function) generic-function-class)))
      (error "Redefinition of generic function ~S (~S) with different class. Changing from ~S to ~S."
             function-name generic-function
             (class-of generic-function) generic-function-class))
    (apply #'reinitialize-instance
           generic-function
           :name function-name
           initargs)))

(defmethod ensure-generic-function-using-class
    ((generic-function null)
     function-name
     &rest all-keys
     &key &allow-other-keys)
  (multiple-value-bind (generic-function-class initargs)
      (apply #'normalize-e-g-f-args all-keys)
    (let ((gf (apply #'make-instance
                     (or generic-function-class
                         *the-class-standard-gf*)
                     :name function-name
                     initargs)))
      ;; Not entirely sure where this should be done.
      ;; SBCL seems to do it in (ENSURE-GENERIC-FUNCTION-USING-CLASS NULL).
      (setf (fdefinition function-name) gf)
      gf)))

;;; Dependent maintenance protocol

(defgeneric add-dependent (metaobject dependent))
(defmethod add-dependent ((metaobject std-class) dependent)
  (std-add-dependent metaobject dependent)
  (values))
(defmethod add-dependent ((metaobject standard-generic-function) dependent)
  (std-add-dependent metaobject dependent)
  (values))

(defgeneric remove-dependent (metaobject dependent))
(defmethod remove-dependent ((metaobject std-class) dependent)
  (std-remove-dependent metaobject dependent)
  (values))
(defmethod remove-dependent ((metaobject standard-generic-function) dependent)
  (std-remove-dependent metaobject dependent)
  (values))

(defgeneric map-dependents (metaobject function))
(defmethod map-dependents ((metaobject std-class) function)
  (std-map-dependents metaobject function)
  (values))
(defmethod map-dependents ((metaobject standard-generic-function) function)
  (std-map-dependents metaobject function)
  (values))

(defgeneric update-dependent (metaobject dependent &rest initargs))

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

(defun class-slot-initargs (class)
  "Return a list of initargs accepted by CLASS's slots."
  (loop
     with result = '()
     for slot in (safe-class-slots class)
     do
       (loop
          for initarg in (safe-slot-definition-initargs slot)
          do (pushnew initarg result))
     finally
       (return result)))

(defun applicable-methods-initargs (applicable-methods)
  (let ((initargs '()))
    (dolist (method applicable-methods)
      (multiple-value-bind (keys aok)
          (function-keywords method)
        (when aok
          (return-from applicable-methods-initargs
            (values nil t)))
        (dolist (key keys)
          (pushnew key initargs))))
    (values initargs nil)))

(defun applicable-method-initargs (generic-function arguments)
  (applicable-methods-initargs (compute-applicable-methods generic-function arguments)))

(defun valid-initargs (class cache functions)
  (multiple-value-bind (cached-initargs cache-validp)
      (gethash class cache)
    ;; FIXME: This must take EQL specializers into account.
    (cond (cache-validp
           cached-initargs)
          (t
           (let ((initargs (class-slot-initargs class)))
             (loop
                for (fn . args) in functions
                do (multiple-value-bind (keys aok)
                       (applicable-method-initargs (fdefinition fn) args)
                     (when aok
                       (setf (gethash class cache) t)
                       (return-from valid-initargs
                         t))
                     (dolist (key keys)
                       (pushnew key initargs))))
             (when cache
               (setf (gethash class cache) initargs))
             initargs)))))

;; Avoid evaluating functions & error-fn until as late as possible.
(defmacro check-initargs (class cache functions initargs error-fn)
  (let ((valid-initargs (gensym "VALID-INITARGS"))
        (invalid-initargs (gensym "INVALID-INITARGS"))
        (initarg (gensym "INITARG"))
        (cache-validp (gensym "CACHE-VALIDP"))
        (class-sym (gensym "CLASS"))
        (cache-sym (gensym "CACHE"))
        (initargs-sym (gensym "INITARGS")))
    `(let ((,class-sym ,class)
           (,cache-sym ,cache)
           (,initargs-sym ,initargs))
       (multiple-value-bind (,valid-initargs ,cache-validp)
           (gethash ,class-sym ,cache-sym)
         (when (not ,cache-validp)
           (setf ,valid-initargs (valid-initargs ,class-sym ,cache-sym ,functions)))
         (when (and (not (eql ,valid-initargs 't))
                    (not (getf ,initargs-sym :allow-other-keys)))
           (let ((,invalid-initargs (loop
                                       for ,initarg in ,initargs-sym by #'cddr
                                       when (and (not (eql ,initarg :allow-other-keys))
                                                 (not (member ,initarg ,valid-initargs)))
                                       collect ,initarg)))
             (when ,invalid-initargs
               (funcall ,error-fn ,valid-initargs ,invalid-initargs))))))))

(sys.int::defglobal *make-instance-initargs-cache*
    (make-hash-table :synchronized t :weakness :key))

(defun check-make-instance-initargs (class initargs)
  (check-initargs
   class *make-instance-initargs-cache*
   (list (list 'allocate-instance class)
         (list 'initialize-instance (class-prototype class))
         (list 'shared-initialize (class-prototype class) t))
   initargs
   (lambda (valid-initargs invalid-initargs)
     (error 'sys.int::simple-program-error
            :format-control "Invalid initargs ~:S when creating instance of ~S (~S).~%Supplied: ~:S~%valid: ~:S"
            :format-arguments (list invalid-initargs class (class-name class) initargs valid-initargs)))))

(defgeneric make-instance (class &rest initargs &key &allow-other-keys))
(defmethod make-instance ((class std-class) &rest initargs)
  (let ((ctor (safe-class-constructor class)))
    (when ctor
      (return-from make-instance
        (funcall ctor initargs))))
  (let ((true-initargs (std-compute-initargs class initargs)))
    (check-make-instance-initargs class true-initargs)
    (let ((instance (apply #'allocate-instance class true-initargs)))
      (apply #'initialize-instance instance true-initargs)
      instance)))

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

(sys.int::defglobal *reinitialize-instance-initargs-cache*
    (make-hash-table :synchronized t :weakness :key))

(defun check-reinitialize-instance-initargs (object initargs)
  (check-initargs
   (class-of object) *reinitialize-instance-initargs-cache*
   (list (list 'reinitialize-instance object)
         (list 'shared-initialize object t))
   initargs
   (lambda (valid-initargs invalid-initargs)
     (error 'sys.int::simple-program-error
            :format-control "Invalid initargs ~S when reinitializing ~S (~S).~%Supplied: ~:S~%valid: ~:S"
            :format-arguments (list invalid-initargs object (class-name (class-of object)) initargs valid-initargs)))))

(defgeneric reinitialize-instance (instance &key &allow-other-keys))
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (check-reinitialize-instance-initargs instance initargs)
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

;; Common implementation for changing std-class types.
(defun std-change-class (old-instance new-class &rest initargs)
  (let* ((new-instance (allocate-instance new-class))
         (old-class (class-of old-instance))
         (old-copy (allocate-instance old-class)))
    (cond ((sys.int::instance-p old-instance)
           (assert (sys.int::instance-p new-instance)))
          (t
           (assert (sys.int::funcallable-instance-p old-instance))
           (assert (sys.int::funcallable-instance-p new-instance))))
    ;; Make a copy of the old instance.
    (dolist (old-slot (safe-class-slots old-class))
      (let ((slot-name (safe-slot-definition-name old-slot)))
        (when (and (instance-slot-p old-slot)
                   (slot-boundp old-instance slot-name))
          (setf (slot-value old-copy slot-name)
                (slot-value old-instance slot-name)))))
    ;; Initialize the new instance with the old slots.
    (dolist (new-slot (safe-class-slots new-class))
      (when (instance-slot-p new-slot)
        (let ((slot-name (safe-slot-definition-name new-slot)))
          (when (and (slot-exists-p old-instance slot-name)
                     (slot-boundp old-instance slot-name))
            (setf (slot-value new-instance slot-name)
                  (slot-value old-instance slot-name))))))
    ;; Obsolete the old instance, replacing it with the new instance.
    (mezzano.runtime::supersede-instance old-instance new-instance)
    (apply #'update-instance-for-different-class
           old-copy old-instance initargs)
    old-instance))

(defgeneric change-class (instance new-class &key &allow-other-keys))
(defmethod change-class ((old-instance standard-object)
                         (new-class standard-class)
                         &rest initargs)
  (when (typep old-instance 'funcallable-standard-object)
    ;; Can't change a FUNCALLABLE-STANDARD-OBJECT to a non-FUNCALLABLE-STANDARD-OBJECT.
    (error "Object ~S of class ~S has metaclass ~S which is incompatible with class ~S with metaclass ~S"
           old-instance (class-of old-instance) (class-of (class-of old-instance))
           new-class (class-of new-class)))
  (apply #'std-change-class old-instance new-class initargs))

(defmethod change-class ((old-instance funcallable-standard-object)
                         (new-class funcallable-standard-class)
                         &rest initargs)
  (apply #'std-change-class old-instance new-class initargs))

(defmethod change-class (instance (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(sys.int::defglobal *u-i-f-d-c-initargs-cache*
    (make-hash-table :synchronized t :weakness :key))

(defun check-update-instance-for-different-class-initargs (old new initargs)
  (check-initargs
   (class-of new) *u-i-f-d-c-initargs-cache*
   (list (list 'update-instance-for-different-class old new)
         (list 'shared-initialize new t))
   initargs
   (lambda (valid-initargs invalid-initargs)
     (error 'sys.int::simple-program-error
            :format-control "Invalid initargs ~:S when updating ~S (~S) for different class.~%Supplied: ~:S~%valid: ~:S"
            :format-initargs (list invalid-initargs new (class-name (class-of new)) initargs valid-initargs)))))

(defgeneric update-instance-for-different-class (old new &key &allow-other-keys))
(defmethod update-instance-for-different-class ((old standard-object)
                                                (new standard-object)
                                                &rest initargs)
  (check-update-instance-for-different-class-initargs old new initargs)
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

(defmethod print-object ((slot-definition structure-slot-definition) stream)
  (print-unreadable-object (slot-definition stream :type t :identity t)
    (format stream "~S" (safe-slot-definition-name slot-definition))))

(defmethod initialize-instance :after ((class std-class) &rest args &key direct-superclasses direct-slots direct-default-initargs documentation area sealed)
  (declare (ignore direct-superclasses direct-slots direct-default-initargs documentation area sealed))
  (apply #'std-after-initialization-for-classes class args))

(defgeneric reader-method-class (class direct-slot &rest initargs))
(defmethod reader-method-class ((class std-class) (direct-slot standard-direct-slot-definition) &rest initargs)
  (declare (ignore initargs))
  (find-class 'standard-reader-method))

(defgeneric writer-method-class (class direct-slot &rest initargs))
(defmethod writer-method-class ((class std-class) (direct-slot standard-direct-slot-definition) &rest initargs)
  (declare (ignore initargs))
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

(defmethod initialize-instance :after ((generic-function standard-generic-function) &key)
  (finalize-generic-function generic-function))

(defmethod reinitialize-instance :before ((generic-function standard-generic-function) &key (lambda-list nil lambda-list-p))
  (when lambda-list-p
    (let ((gf-ll (analyze-lambda-list lambda-list))
          (methods (copy-list (generic-function-methods generic-function))))
      (dolist (method methods)
        ;; Make sure that the new lambda-list is congruent with existing methods.
        (restart-case
            (let ((method-ll (analyze-lambda-list (safe-method-lambda-list method))))
              (assert (eql (length (getf gf-ll :required-args))
                           (length (getf method-ll :required-args)))
                      (generic-function method)
                      "New lambda-list ~:S for generic function ~S and method ~S have differing required arguments."
                      lambda-list generic-function method)
              (assert (eql (length (getf gf-ll :optional-args))
                           (length (getf method-ll :optional-args)))
                      (generic-function method)
                      "New lambda-list ~:S for generic function ~S and method ~S have differing optional arguments."
                      lambda-list generic-function method)
              (let ((gf-accepts-key-or-rest (or (getf gf-ll :rest-var)
                                                (member '&key lambda-list)))
                    (method-accepts-key-or-rest (or (getf method-ll :rest-var)
                                                    (member '&key (safe-method-lambda-list method)))))
                (assert (or (and gf-accepts-key-or-rest
                                 method-accepts-key-or-rest)
                            (and (not gf-accepts-key-or-rest)
                                 (not method-accepts-key-or-rest)))
                        (generic-function method)
                        "New lambda-list ~:S for generic function ~S and method ~S differ in their acceptance of &KEY or &REST arguments."
                        lambda-list generic-function method))
              ;; If a method accepts keywords, then it must accept all the keywords that the generic function accepts
              (when (and (member '&key (safe-method-lambda-list method))
                         (not (member '&allow-other-keys (safe-method-lambda-list method))))
                (let ((missing-keywords (set-difference (getf gf-ll :keywords)
                                                        (getf method-ll :keywords))))
                  (assert (endp missing-keywords)
                          (generic-function method)
                          "Method ~S must accept all keywords accepted by new lambda-list ~:S for generic function ~S. It is missing ~:S"
                          method lambda-list generic-function missing-keywords))))
          (remove-method ()
            :report (lambda (stream) (format stream "Remove method ~S from ~S" method generic-function))
            (remove-method generic-function method)))))))

(defmethod reinitialize-instance :after ((generic-function standard-generic-function) &rest initargs &key argument-precedence-order lambda-list)
  (when (and lambda-list (not argument-precedence-order))
    ;; If :LAMBDA-LIST is supplied and :ARGUMENT-PRECEDENCE-ORDER is not, then
    ;; ARGUMENT-PRECEDENCE-ORDER defaults to the required portion of LAMBDA-LIST.
    ;; Setting it to NIL here will make FINALIZE-GENERIC-FUNCTION do the right thing.
    (setf (safe-generic-function-argument-precedence-order generic-function) nil))
  (finalize-generic-function generic-function)
  ;; Update dependents
  (safe-map-dependents generic-function
                       (lambda (dep)
                         (apply #'update-dependent generic-function dep initargs))))

;;;
;;; Methods having to do with method metaobjects.
;;;

(defmethod print-object ((method standard-method) stream)
  (print-unreadable-object (method stream :identity t)
    (format stream "~:(~S~) ~S ~:S ~:S"
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

(defmethod initialize-instance :after ((method standard-method) &rest args &key lambda-list qualifiers specializers function fast-function documentation)
  (declare (ignore lambda-list qualifiers specializers function fast-function documentation))
  (apply #'std-after-initialization-for-methods method args))

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

(defmethod update-instance-for-different-class :after ((old forward-referenced-class) (new std-class)
                                                       &rest initargs
                                                       &key direct-superclasses direct-slots direct-default-initargs documentation)
  (declare (ignore initargs direct-superclasses direct-slots direct-default-initargs documentation))
  nil)

(defmethod update-instance-for-different-class :before
    ((old forward-referenced-class) (new standard-class) &rest initargs)
  (declare (ignore initargs))
  (setf (safe-class-direct-superclasses new) (list (find-class 'standard-class))))

(defmethod update-instance-for-different-class :before
    ((old forward-referenced-class) (new funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (setf (safe-class-direct-superclasses new) (list (find-class 'funcallable-standard-class))))

(defgeneric ensure-class-using-class (class name &key &allow-other-keys))

(defmethod ensure-class-using-class ((class class) name
                                     &rest all-keys
                                     &key (metaclass 'standard-class) &allow-other-keys)
  (assert (eql name (safe-class-name class)))
  (when (symbolp metaclass)
    (setf metaclass (find-class metaclass)))
  (check-type metaclass class)
  (assert (eql (class-of class) metaclass))
  (apply #'reinitialize-instance class (compute-class-initialization-arguments all-keys))
  (setf (find-class name) class)
  class)

(defmethod ensure-class-using-class ((class forward-referenced-class) name
                                     &rest all-keys
                                     &key (metaclass 'standard-class) &allow-other-keys)
  (assert (eql name (safe-class-name class)))
  (when (symbolp metaclass)
    (setf metaclass (find-class metaclass)))
  (check-type metaclass class)
  (let ((initargs (compute-class-initialization-arguments all-keys)))
    (apply #'change-class class metaclass initargs)
    (apply #'reinitialize-instance class initargs))
  (setf (find-class name) class)
  class)

(defmethod ensure-class-using-class ((class null) name
                                     &rest all-keys
                                     &key (metaclass 'standard-class) &allow-other-keys)
  (when (symbolp metaclass)
    (setf metaclass (find-class metaclass)))
  (check-type metaclass class)
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
  (declare (ignore args))
  (error "Cannot reinitialize built-in classes."))

;;; eql specializers.

(sys.int::defglobal *interned-eql-specializers*
    (make-hash-table :synchronized t :weakness :value))

(defclass eql-specializer (specializer)
  ((object :initarg :object :reader eql-specializer-object)))

(defmethod print-object ((object eql-specializer) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (eql-specializer-object object))))

(defun intern-eql-specializer (object)
  (let ((eql-spec (gethash object *interned-eql-specializers*)))
    (when (not eql-spec)
      (let ((new (make-instance 'eql-specializer :object object)))
        (setf eql-spec (or (sys.int::cas (gethash object *interned-eql-specializers*)
                                         nil new)
                           new))))
    eql-spec))

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

(defmethod add-direct-method ((specializer eql-specializer) (method method))
  (std-add-direct-method specializer method))

(defmethod remove-direct-method ((specializer eql-specializer) (method method))
  (std-remove-direct-method specializer method))

(defmethod specializer-direct-generic-functions ((specializer eql-specializer))
  (std-specializer-direct-generic-functions specializer))

(defmethod specializer-direct-methods ((specializer eql-specializer))
  (std-specializer-direct-methods specializer))

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
    (safe-remove-direct-subclass superclass class))
  ;; Remove any slot reader/writer methods.
  #+(or)(dolist (direct-slot (safe-class-direct-slots class))
    (dolist (reader (safe-slot-definition-readers direct-slot))
      (remove-reader-method class reader (safe-slot-definition-name direct-slot)))
    (dolist (writer (safe-slot-definition-writers direct-slot))
      (remove-writer-method class writer (safe-slot-definition-name direct-slot))))
  ;; Flush the constructor function.
  (setf (std-slot-value class 'constructor) nil)
  ;; Fall into the initialize-instance code.
  (apply #'std-after-initialization-for-classes
         class
         (append args
                 (list :direct-superclasses (safe-class-direct-superclasses class))
                 (list :direct-slots (mapcar #'convert-direct-slot-definition-to-canonical-direct-slot (safe-class-direct-slots class)))
                 (list :direct-default-initargs (safe-class-direct-default-initargs class))))
  ;; Flush the EMF tables of generic functions.
  ;; FIXME: Make this cleaner, needs to cover any generic function that indirectly specializes on this class.
  (dolist (gf (safe-specializer-direct-generic-functions class))
    (reset-gf-emf-table gf))
  (flush-emf-tables-on-class-redefinition class)
  ;; Refinalize any subclasses.
  (dolist (subclass (safe-class-direct-subclasses class))
    (std-after-reinitialization-for-classes subclass))
  ;; Flush the initarg caches for this class.
  (flush-initarg-caches class)
  ;; Update dependents
  (safe-map-dependents class (lambda (dep)
                               (apply #'update-dependent class dep args))))

(defmethod reinitialize-instance :before ((class std-class) &key)
  (when (std-slot-value class 'sealed)
    (error "Cannot reinitialize sealed classes.")))

(defmethod reinitialize-instance :after ((class std-class) &rest args &key direct-superclasses direct-slots direct-default-initargs documentation area sealed)
  (declare (ignore direct-superclasses direct-slots direct-default-initargs documentation area sealed))
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
         (setf (mezzano.runtime::instance-access new-instance (slot-location-using-layout slot new-layout))
               (mezzano.runtime::instance-access old-instance (slot-location-using-layout slot old-layout))))
    ;; Assemble the list of discarded values.
    (loop
       for slot in discarded-slots
       do (let ((value (mezzano.runtime::instance-access old-instance (slot-location-using-layout slot old-layout))))
            (when (not (eql value *secret-unbound-value*))
              (setf property-list (list* slot value
                                         property-list)))))
    ;; Obsolete the old instance, replacing it with the new instance.
    (mezzano.runtime::supersede-instance instance new-instance)
    ;; Magic.
    (update-instance-for-redefined-class instance added-slots discarded-slots property-list)))

(sys.int::defglobal *u-i-f-r-c-initargs-cache*
    (make-hash-table :synchronized t :weakness :key))

(defun check-update-instance-for-redefined-class-initargs (object added-slots discarded-slots property-list initargs)
  (check-initargs
   (class-of object) *u-i-f-r-c-initargs-cache*
   (list (list 'update-instance-for-redefined-class object added-slots discarded-slots property-list)
         (list 'shared-initialize object added-slots))
   initargs
   (lambda (valid-initargs invalid-initargs)
     (error 'sys.int::simple-program-error
            :format-control "Invalid initargs ~:S when updating ~S (~S) for redefined class.~%Supplied: ~:S~%valid: ~:S"
            :format-arguments (list invalid-initargs object (class-name (class-of object)) initargs valid-initargs)))))

(defgeneric update-instance-for-redefined-class (instance added-slots discarded-slots property-list &rest initargs &key &allow-other-keys))

(defmethod update-instance-for-redefined-class ((instance standard-object) added-slots discarded-slots property-list &rest initargs)
  (check-update-instance-for-redefined-class-initargs instance added-slots discarded-slots property-list initargs)
  (apply #'shared-initialize instance added-slots initargs))

(defgeneric slot-unbound (class instance slot-name))

(defmethod slot-unbound ((class t) instance slot-name)
  (error 'unbound-slot
         :name slot-name
         :instance instance))

(defgeneric slot-missing (class object slot-name operation &optional new-value))
(defmethod slot-missing ((class t) object slot-name operation &optional new-value)
  (declare (ignore new-value))
  (error "Slot ~S missing from class ~S when performing ~S." slot-name class operation))

;;; Initarg cache maintenance

(defclass initarg-cache-flusher ()
  ((%cache :initarg :cache :reader initarg-cache-flusher-cache)
   (%purge :initarg :purge :initform nil :reader initarg-cache-flusher-purge)))

(defun flush-initarg-caches (class)
  "Remove the entries for CLASS from each cache.
Does not handle subclasses."
  (remhash class *make-instance-initargs-cache*)
  (remhash class *reinitialize-instance-initargs-cache*)
  (remhash class *u-i-f-d-c-initargs-cache*)
  (remhash class *u-i-f-r-c-initargs-cache*))

(defun flush-initarg-cache-recursively (cache class)
  "Remove CLASS and all subclasses from CACHE."
  (remhash class cache)
  (dolist (subclass (class-direct-subclasses class))
    (flush-initarg-cache-recursively cache subclass)))

(defmethod update-dependent (metaobject (dependent initarg-cache-flusher) &key ((add-method added-method)) ((remove-method removed-method)) &allow-other-keys)
  (flet ((doit (method)
           (flush-initarg-cache-recursively (initarg-cache-flusher-cache dependent)
                                            (first (method-specializers method)))))
    (cond ((initarg-cache-flusher-purge dependent)
           (clrhash (initarg-cache-flusher-cache dependent)))
          (t
           (when added-method (doit added-method))
           (when removed-method (doit removed-method))))))

;; Make-instance
(safe-add-dependent #'allocate-instance (make-instance 'initarg-cache-flusher :cache *make-instance-initargs-cache* :purge t))
(safe-add-dependent #'initialize-instance (make-instance 'initarg-cache-flusher :cache *make-instance-initargs-cache*))
(safe-add-dependent #'shared-initialize (make-instance 'initarg-cache-flusher :cache *make-instance-initargs-cache*))

;; Reinitialize-instance
(safe-add-dependent #'reinitialize-instance (make-instance 'initarg-cache-flusher :cache *reinitialize-instance-initargs-cache*))
(safe-add-dependent #'shared-initialize (make-instance 'initarg-cache-flusher :cache *reinitialize-instance-initargs-cache*))

;; Update-instance-for-different-class
(safe-add-dependent #'update-instance-for-different-class (make-instance 'initarg-cache-flusher :cache *u-i-f-d-c-initargs-cache*))
(safe-add-dependent #'shared-initialize (make-instance 'initarg-cache-flusher :cache *u-i-f-d-c-initargs-cache*))

;; Update-instance-for-redefined-class
(safe-add-dependent #'update-instance-for-redefined-class (make-instance 'initarg-cache-flusher :cache *u-i-f-r-c-initargs-cache*))
(safe-add-dependent #'shared-initialize (make-instance 'initarg-cache-flusher :cache *u-i-f-r-c-initargs-cache*))

(defmethod print-object ((instance class-reference) stream)
  (print-unreadable-object (instance stream :type t :identity t)
    (format stream "~S~:[ [undefined]~;~]" (class-reference-name instance) (class-reference-class instance))))

(defgeneric generic-function-trace-p (generic-function))
(defgeneric (setf generic-function-trace-p) (value generic-function))

(defmethod generic-function-trace-p ((generic-function standard-generic-function))
  (slot-value generic-function 'tracep))

(defmethod (setf generic-function-trace-p) (value (generic-function standard-generic-function))
  (setf (slot-value generic-function 'tracep) value)
  ;; Refinalize the GF to update the discriminating function and the emf cache.
  (finalize-generic-function generic-function)
  value)
