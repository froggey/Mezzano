;;;; -------------------------------------------------------------------------
;;;; Operations

(uiop/package:define-package :asdf/operation
  (:recycle :asdf/operation :asdf/action :asdf) ;; asdf/action for FEATURE pre 2.31.5.
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session)
  (:export
   #:operation
   #:*operations* #:make-operation #:find-operation
   #:feature)) ;; TODO: stop exporting the deprecated FEATURE feature.
(in-package :asdf/operation)

;;; Operation Classes
(when-upgrading (:version "2.27" :when (find-class 'operation nil))
  ;; override any obsolete shared-initialize method when upgrading from ASDF2.
  (defmethod shared-initialize :after ((o operation) (slot-names t) &key)
    (values)))

(with-upgradability ()
  (defclass operation ()
    ()
    (:documentation "The base class for all ASDF operations.

ASDF does NOT and never did distinguish between multiple operations of the same class.
Therefore, all slots of all operations MUST have :allocation :class and no initargs. No exceptions.
"))

  (defvar *in-make-operation* nil)

  (defun check-operation-constructor ()
    "Enforce that OPERATION instances must be created with MAKE-OPERATION."
    (unless *in-make-operation*
      (sysdef-error "OPERATION instances must only be created through MAKE-OPERATION.")))

  (defmethod print-object ((o operation) stream)
    (print-unreadable-object (o stream :type t :identity nil)))

  ;;; Override previous methods (from 3.1.7 and earlier) and add proper error checking.
  #-genera ;; Genera adds its own system initargs, e.g. clos-internals:storage-area 8
  (defmethod initialize-instance :after ((o operation) &rest initargs &key &allow-other-keys)
    (unless (null initargs)
      (parameter-error "~S does not accept initargs" 'operation))))


;;; make-operation, find-operation

(with-upgradability ()
  ;; A table to memoize instances of a given operation. There shall be only one.
  (defparameter* *operations* (make-hash-table :test 'equal))

  ;; A memoizing way of creating instances of operation.
  (defun make-operation (operation-class)
    "This function creates and memoizes an instance of OPERATION-CLASS.
All operation instances MUST be created through this function.

Use of INITARGS is not supported at this time."
    (let ((class (coerce-class operation-class
                               :package :asdf/interface :super 'operation :error 'sysdef-error))
          (*in-make-operation* t))
      (ensure-gethash class *operations* `(make-instance ,class))))

  ;; This function is mostly for backward and forward compatibility:
  ;; operations used to preserve the operation-original-initargs of the context,
  ;; and may in the future preserve some operation-canonical-initargs.
  ;; Still, the treatment of NIL as a disabling context is useful in some cases.
  (defgeneric find-operation (context spec)
    (:documentation "Find an operation by resolving the SPEC in the CONTEXT"))
  (defmethod find-operation ((context t) (spec operation))
    spec)
  (defmethod find-operation ((context t) (spec symbol))
    (when spec ;; NIL designates itself, i.e. absence of operation
      (make-operation spec))) ;; TODO: preserve the (operation-canonical-initargs context)
  (defmethod find-operation ((context t) (spec string))
    (make-operation spec))) ;; TODO: preserve the (operation-canonical-initargs context)

