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

(sys.int::defglobal *class-reference-table*)
(sys.int::defglobal *class-reference-table-lock*)

(defstruct class-reference
  name
  class)

(defun class-reference (name)
  (check-type name symbol)
  (let ((entry (mezzano.supervisor:with-rw-lock-read (*class-reference-table-lock*)
                 (gethash name *class-reference-table*))))
    (when (not entry)
      (mezzano.supervisor:with-rw-lock-write (*class-reference-table-lock*)
        (let ((new-entry (make-class-reference :name name)))
          (setf entry (or (sys.int::cas (gethash name *class-reference-table*)
                                        nil
                                        new-entry)
                          new-entry)))))
    entry))

(define-compiler-macro find-class (&whole whole symbol &optional (errorp t) environment)
  (if (and (null environment)
           (consp symbol)
           (eql (first symbol) 'quote)
           (consp (rest symbol))
           (symbolp (second symbol))
           (null (rest (rest symbol))))
      `(find-class-in-reference
        (load-time-value (class-reference ',(second symbol)))
        ,errorp)
      whole))

(defun find-class-in-reference (reference &optional (errorp t))
  (or (class-reference-class reference)
      (and errorp
           (error 'unknown-class :name (class-reference-name reference)))))

(defun find-class (symbol &optional (errorp t) environment)
  (declare (ignore environment))
  (find-class-in-reference (class-reference symbol) errorp))

(defun (setf find-class) (new-value symbol &optional (errorp t) environment)
  (declare (ignore errorp environment))
  (let ((reference (class-reference symbol))
        (type-info (sys.int::type-info-for symbol)))
    (setf (sys.int::type-info-maybe-class type-info)
          (if new-value t nil))
    (setf (class-reference-class reference) new-value)))

(sys.int::defglobal *next-class-hash-value*)
(declaim (type fixnum *next-class-hash-value*))

(defun next-class-hash-value ()
  (sys.int::atomic-incf *next-class-hash-value*))

(sys.int::defglobal *secret-unbound-value*); (list "slot unbound"))

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

(sys.int::defglobal *initial-class-table*)

(defun primordial-slot-location-in-layout (layout slot-name)
  (loop
     with instance-slots = (sys.int::layout-instance-slots layout)
     for i below (length instance-slots) by 2
     do
       (when (eq (svref instance-slots i) slot-name)
         (return (svref instance-slots (1+ i))))
     finally
       (return nil)))

(defun primordial-slot-value (object slot-name)
  (mezzano.runtime::instance-access
   object
   (or (primordial-slot-location-in-layout
        (sys.int::%instance-layout object)
        slot-name)
       (error "Slot ~S missing from ~S?" slot-name object))))

(defun (setf primordial-slot-value) (value object slot-name)
  (setf (mezzano.runtime::instance-access
         object
         (or (primordial-slot-location-in-layout
              (sys.int::%instance-layout object)
              slot-name)
             (error "Slot ~S missing from ~S?" slot-name object)))
        value))

(defun initialize-clos ()
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
        *the-class-standard-reader-method* (find-class 'standard-reader-method)
        *the-class-standard-writer-method* (find-class 'standard-reader-method)
        *the-class-t* (find-class 't))
  ;; Locations of important slots in metaobjects.
  (let ((s-c-layout (primordial-slot-value (find-class 'standard-class) 'slot-storage-layout)))
    (setf *the-layout-standard-class* s-c-layout)
    (setf *standard-class-effective-slots-location* (primordial-slot-location-in-layout s-c-layout 'effective-slots)
          *standard-class-slot-storage-layout-location* (primordial-slot-location-in-layout s-c-layout 'slot-storage-layout)
          *standard-class-hash-location* (primordial-slot-location-in-layout s-c-layout 'hash)
          *standard-class-finalized-p-location* (primordial-slot-location-in-layout s-c-layout 'finalized-p)
          *standard-class-precedence-list-location* (primordial-slot-location-in-layout s-c-layout 'precedence-list)
          *standard-class-direct-default-initargs-location* (primordial-slot-location-in-layout s-c-layout 'direct-default-initargs)
          *standard-class-default-initargs-location* (primordial-slot-location-in-layout s-c-layout 'default-initargs)
          *standard-class-constructor-location* (primordial-slot-location-in-layout s-c-layout 'constructor)))
  (let ((s-e-s-d-layout (primordial-slot-value (find-class 'standard-effective-slot-definition) 'slot-storage-layout)))
    (setf *the-layout-standard-effective-slot-definition* s-e-s-d-layout)
    (setf *standard-effective-slot-definition-name-location* (primordial-slot-location-in-layout s-e-s-d-layout 'name)
          *standard-effective-slot-definition-location-location* (primordial-slot-location-in-layout s-e-s-d-layout 'location)))
  (let ((f-s-c-layout (primordial-slot-value (find-class 'funcallable-standard-class) 'slot-storage-layout)))
    (setf *funcallable-standard-class-hash-location* (primordial-slot-location-in-layout f-s-c-layout 'hash)))
  (let ((b-i-c-layout (primordial-slot-value (find-class 'built-in-class) 'slot-storage-layout)))
    (setf *built-in-class-precedence-list-location* (primordial-slot-location-in-layout b-i-c-layout 'precedence-list)
          *built-in-class-hash-location* (primordial-slot-location-in-layout b-i-c-layout 'hash)))
  ;; Patch slots in classes.
  ;; The cold generator leaves these as lists, but they need to be weak lists.
  (loop
     for (name . class) across *initial-class-table*
     do
       (setf (primordial-slot-value class 'direct-subclasses)
              (mezzano.garbage-collection.weak-objects:make-weak-list
               (primordial-slot-value class 'direct-subclasses)))))

;; Initial version of class-constructor, replaced after the compiler is loaded.
(defun safe-class-constructor (class)
  (declare (ignore class))
  nil)
