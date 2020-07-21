;;;; Low-level support functions for instances.
;;;;
;;;; Instances are the underlying representation of STRUCTURE-OBJECTs,
;;;; STANDARD-OBJECTs, and FUNCALLABLE-STANDARD-OBJECTs.

(in-package :mezzano.runtime)

;;; Location encoding.

(defconstant +location-type+ (byte 4 0))
(defconstant +location-type-t+                0)
(defconstant +location-type-unsigned-byte-8+  1)
(defconstant +location-type-unsigned-byte-16+ 2)
(defconstant +location-type-unsigned-byte-32+ 3)
(defconstant +location-type-unsigned-byte-64+ 4)
(defconstant +location-type-signed-byte-8+    5)
(defconstant +location-type-signed-byte-16+   6)
(defconstant +location-type-signed-byte-32+   7)
(defconstant +location-type-signed-byte-64+   8)
(defconstant +location-type-single-float+     9)
(defconstant +location-type-double-float+     10)
(defconstant +location-type-short-float+      11)

(defun sys.int::%instance-layout (object)
  "Return an instance's direct layout."
  (sys.int::%instance-layout object))

(defun sys.int::%fast-instance-layout-eq-p (object instance-header)
  "Test if an instance's direct layout & tag match instance-header."
  (sys.int::%fast-instance-layout-eq-p object instance-header))

(declaim (inline sys.int::instance-or-funcallable-instance-p))
(defun sys.int::instance-or-funcallable-instance-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (sys.int::%instance-or-funcallable-instance-p object)))

(defun sys.int::%instance-or-funcallable-instance-p (object)
  (sys.int::%instance-or-funcallable-instance-p object))

(declaim (inline sys.int::instance-p sys.int::funcallable-instance-p))

(defun sys.int::instance-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-instance+))

(defun sys.int::funcallable-instance-p (object)
  (sys.int::%object-of-type-p object sys.int::+object-tag-funcallable-instance+))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defun location-types ()
  '(#.+location-type-t+
    #.+location-type-unsigned-byte-8+
    #.+location-type-unsigned-byte-16+
    #.+location-type-unsigned-byte-32+
    #.+location-type-unsigned-byte-64+
    #.+location-type-signed-byte-8+
    #.+location-type-signed-byte-16+
    #.+location-type-signed-byte-32+
    #.+location-type-signed-byte-64+
    #.+location-type-single-float+
    #.+location-type-double-float+
    #.+location-type-short-float+))

(defun make-location (type offset)
  (logior (dpb type +location-type+ 0)
          (ash offset 4)))

(defun location-type (location)
  (ldb +location-type+ location))

(declaim (inline location-offset-t))
(defun location-offset-t (location)
  "Return the location offset scaled appropriately for %object-ref-t"
  (ash location -7))

(declaim (inline location-offset))
(defun location-offset (location)
  (ash location -4))

(defun location-type-scale (location-type)
  (ecase location-type
    (#.+location-type-t+                1)
    (#.+location-type-unsigned-byte-8+  1)
    (#.+location-type-unsigned-byte-16+ 2)
    (#.+location-type-unsigned-byte-32+ 4)
    (#.+location-type-unsigned-byte-64+ 8)
    (#.+location-type-signed-byte-8+    1)
    (#.+location-type-signed-byte-16+   2)
    (#.+location-type-signed-byte-32+   4)
    (#.+location-type-signed-byte-64+   8)
    (#.+location-type-single-float+     4)
    (#.+location-type-double-float+     8)
    (#.+location-type-short-float+      2)))

(defun location-type-accessor (location-type)
  (ecase location-type
    (#.+location-type-t+
     'sys.int::%object-ref-t)
    (#.+location-type-unsigned-byte-8+
     'sys.int::%object-ref-unsigned-byte-8-unscaled)
    (#.+location-type-unsigned-byte-16+
     'sys.int::%object-ref-unsigned-byte-16-unscaled)
    (#.+location-type-unsigned-byte-32+
     'sys.int::%object-ref-unsigned-byte-32-unscaled)
    (#.+location-type-unsigned-byte-64+
     'sys.int::%object-ref-unsigned-byte-64-unscaled)
    (#.+location-type-signed-byte-8+
     'sys.int::%object-ref-signed-byte-8-unscaled)
    (#.+location-type-signed-byte-16+
     'sys.int::%object-ref-signed-byte-16-unscaled)
    (#.+location-type-signed-byte-32+
     'sys.int::%object-ref-signed-byte-32-unscaled)
    (#.+location-type-signed-byte-64+
     'sys.int::%object-ref-signed-byte-64-unscaled)
    (#.+location-type-single-float+
     'sys.int::%object-ref-single-float-unscaled)
    (#.+location-type-double-float+
     'sys.int::%object-ref-double-float-unscaled)
    (#.+location-type-short-float+
     'sys.int::%object-ref-short-float-unscaled)))
)

(macrolet ((def (name-base args)
             `(defun ,(if name-base
                          (list name-base 'instance-access)
                          'instance-access)
                  (,@args object location &optional (index 0))
                (ecase (location-type location)
                  (#.+location-type-t+
                   (funcall
                    #',(if name-base (list name-base 'sys.int::%object-ref-t) 'sys.int::%object-ref-t)
                    ,@args
                    object (+ (location-offset-t location) index)))
                  ,@(loop
                       for type in (location-types)
                       when (not (eql type +location-type-t+))
                       collect `(,type (funcall #',(if name-base
                                                       (list name-base (location-type-accessor type))
                                                       (location-type-accessor type))
                                                ,@args
                                                object
                                                (+ (location-offset location)
                                                   (* index ,(location-type-scale type))))))))))
  (def nil ())
  (def setf (value))
  (def sys.int::cas (new old)))

(defun %instance-dcas (instance offset old-1 old-2 new-1 new-2)
  (sys.int::%dcas-object instance offset old-1 old-2 new-1 new-2))

(defun instance-dcas (instance loc-1 loc-2 old-1 old-2 new-1 new-2)
  ;; TODO: Eventually combinations of same-width locations could be supported.
  ;; T and UB64, or UB32 and SINGLE-FLOAT, etc
  (when (not (eql (location-type loc-1) (location-type loc-2)))
    (error "DCAS is not supported on locations of disparate types"))
  ;; TODO: Support DCAS on unboxed slots at all.
  (when (not (eql (location-type loc-1) +location-type-t+))
    (error "DCAS is only supported on locations of type T"))
  ;; The compiler has some trouble compiling this function because of the
  ;; large number of registers used by the dcas instruction, exacerbated
  ;; by the non-constant offset.
  ;; Call out to an external function to help it out.
  (let ((ofs-1 (location-offset-t loc-1))
        (ofs-2 (location-offset-t loc-2)))
    (cond ((eql (1+ ofs-1) ofs-2)
           ;; Normal location ordering.
           (%instance-dcas instance ofs-1 old-1 old-2 new-1 new-2))
          ((eql (1+ ofs-2) ofs-1)
           ;; Reversed location ordering.
           (multiple-value-bind (successp value-2 value-1)
               (%instance-dcas instance ofs-2 old-2 old-1 new-2 new-1)
             (values successp value-1 value-2)))
          (t
           (error "DCAS is not supported on discontigious locations ~S and ~S" loc-1 loc-2)))))

(defun slot-location-in-layout (layout slot-name)
  (let ((instance-slots (sys.int::layout-instance-slots layout)))
    (loop
       for i below (sys.int::%object-header-data instance-slots) by 2 ; avoid calling length on the slot vector, it's a simple-vector.
       when (eql slot-name (sys.int::%object-ref-t instance-slots i))
       do (return (sys.int::%object-ref-t instance-slots (1+ i)))
       finally (error "Instance slot ~S missing from layout ~S" slot-name layout))))

(defun instance-slot-location (object slot-name)
  (slot-location-in-layout (sys.int::%instance-layout object)
                           slot-name))

(defun instance-access-by-name (object slot-name &optional (index 0))
  (instance-access object (instance-slot-location object slot-name) index))

(defun (setf instance-access-by-name) (value object slot-name &optional (index 0))
  (setf (instance-access object (instance-slot-location object slot-name) index) value))

(defun (sys.int::cas instance-access-by-name) (old new object slot-name &optional (index 0))
  (sys.int::cas (instance-access object (instance-slot-location object slot-name) index) old new))

(defun sys.int::instance-header-p (object)
  (sys.int::%value-has-tag-p object sys.int::+tag-instance-header+))

(defun %make-instance-header (layout &optional (tag sys.int::+object-tag-instance+))
  (check-type layout sys.int::layout)
  (with-live-objects (layout)
    (sys.int::%%assemble-value
     (logior (ash (sys.int::lisp-object-address layout)
                  sys.int::+object-data-shift+)
             (ash tag
                  sys.int::+object-type-shift+))
     sys.int::+tag-instance-header+)))

(defun %unpack-instance-header (instance-header)
  (sys.int::%%assemble-value
   (ash (sys.int::lisp-object-address instance-header)
        (- sys.int::+object-data-shift+))
   0))

(defstruct (obsolete-instance-layout
             ;; Pinned, as the GC needs to read it.
             ;; Don't make it wired to avoid thrashing the wired area.
             (:area :pinned))
  new-instance
  old-layout)

(defstruct (wired-obsolete-instance-layout
             (:include obsolete-instance-layout)
             (:area :wired))
  ;; No new slots needed here, just changing the allocation area.
  )

(defun supersede-instance (old-instance replacement)
  (let ((layout (sys.int::%instance-layout old-instance)))
    (cond ((sys.int::layout-p layout)
           ;; This really is a layout, not a superseded instance
           (let ((new-layout (if (eql (sys.int::layout-area layout) :wired)
                                 (make-wired-obsolete-instance-layout
                                  :old-layout layout
                                  :new-instance replacement)
                                 (make-obsolete-instance-layout
                                  :old-layout layout
                                  :new-instance replacement))))
             (with-live-objects (new-layout)
               ;; ###: Should this be a CAS?
               (setf (sys.int::%object-ref-unsigned-byte-64 old-instance -1)
                     ;; Construct a new obsolete-instance header containing
                     ;; our new obsolete layout.
                     (logior (ash (sys.int::lisp-object-address new-layout)
                                  sys.int::+object-data-shift+)
                             (if (sys.int::funcallable-instance-p old-instance)
                                 (ash sys.int::+object-tag-funcallable-instance+
                                      sys.int::+object-type-shift+)
                                 (ash sys.int::+object-tag-instance+
                                      sys.int::+object-type-shift+)))))))
          (t
           ;; This instance has already been superseded, replace it in-place.
           ;; FIXME: Can race with the GC. It can snap the old instance away
           ;; from underneath us, losing the replacement.
           ;; Check if the old instance's layout matches after this?
           (setf (obsolete-instance-layout-new-instance layout)
                 replacement))))
  (values))

(in-package :mezzano.internals)

(defstruct (layout
             (:area :wired)
             :sealed
             :slot-locations)
  (class nil :read-only t)
  (obsolete nil)
  (heap-size nil)
  (heap-layout nil)
  (area nil)
  (instance-slots nil))
