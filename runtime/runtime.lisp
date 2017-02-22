;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(defun sys.int::%%unwind-to (target-special-stack-pointer)
  (declare (sys.int::suppress-ssp-checking))
  (loop (when (eq target-special-stack-pointer (sys.int::%%special-stack-pointer))
          (return))
     (assert (sys.int::%%special-stack-pointer))
     (etypecase (svref (sys.int::%%special-stack-pointer) 1)
       (symbol
        (sys.int::%%unbind))
       (simple-vector
        (sys.int::%%disestablish-block-or-tagbody))
       (function
        (sys.int::%%disestablish-unwind-protect)))))

(defvar *active-catch-handlers*)
(defun sys.int::%catch (tag fn)
  ;; Catch is used in low levelish code, so must avoid allocation.
  (let ((vec (sys.c::make-dx-simple-vector 3)))
    (setf (svref vec 0) *active-catch-handlers*
          (svref vec 1) tag
          (svref vec 2) (flet ((exit-fn (values)
                                 (return-from sys.int::%catch (values-list values))))
                          (declare (dynamic-extent (function exit-fn)))
                          #'exit-fn))
    (let ((*active-catch-handlers* vec))
      (funcall fn))))

(defun sys.int::%throw (tag values)
  ;; Note! The VALUES list has dynamic extent!
  ;; This is fine, as the exit function calls VALUES-LIST on it before unwinding.
  (do ((current *active-catch-handlers* (svref current 0)))
      ((not current)
       (error 'sys.int::bad-catch-tag-error :tag tag))
    (when (eq (svref current 1) tag)
      (funcall (svref current 2) values))))

(defun sys.int::%coerce-to-callable (object)
  (etypecase object
    (function object)
    (symbol
     ;; Fast-path for symbols.
     (let ((fref (sys.int::%object-ref-t object sys.int::+symbol-function+)))
       (when (not fref)
         (return-from sys.int::%coerce-to-callable
           (fdefinition object)))
       (let ((fn (sys.int::%object-ref-t fref sys.int::+fref-function+)))
         (if (sys.int::%undefined-function-p fn)
             (fdefinition object)
             fn))))))

(in-package :sys.int)

(defun return-address-to-function (return-address)
  "Convert a return address to a function pointer.
Dangerous! The return address must be kept live as a return address on a
thread's stack if this function is called from normal code."
  ;; Return address must be within the pinned or wired area.
  (assert (< return-address sys.int::*pinned-area-bump*))
  ;; Walk backwards looking for an object header with a function type and
  ;; an appropriate entry point.
  (loop
     with address = (logand return-address -16)
     ;; Be careful when reading to avoid bignums.
     for potential-header-type = (ldb (byte +object-type-size+ +object-type-shift+)
                                      (memref-unsigned-byte-8 address 0))
     do
       (when (and
              ;; Closures never contain code.
              (or (eql potential-header-type +object-tag-function+)
                  (eql potential-header-type +object-tag-funcallable-instance+))
              ;; Check entry point halves individually, avoiding bignums.
              ;; Currently the entry point of every non-closure function
              ;; points to the base-address + 16.
              (eql (logand (+ address 16) #xFFFFFFFF)
                   (memref-unsigned-byte-32 (+ address 8) 0))
              (eql (logand (ash (+ address 16) -32) #xFFFFFFFF)
                   (memref-unsigned-byte-32 (+ address 12) 0)))
         (return (values (%%assemble-value address sys.int::+tag-object+)
                         (- return-address address))))
       (decf address 16)))

(defun map-function-gc-metadata (function function-to-inspect)
  "Call FUNCTION with every GC metadata entry in FUNCTION-TO-INSPECT.
Arguments to FUNCTION:
 start-offset
 framep
 interruptp
 pushed-values
 pushed-values-register
 layout-address
 layout-length
 multiple-values
 incoming-arguments
 block-or-tagbody-thunk
 extra-registers
 restart"
  (check-type function function)
  (let* ((fn-address (logand (lisp-object-address function-to-inspect) -16))
         (header-data (%object-header-data function-to-inspect))
         (mc-size (* (ldb (byte +function-machine-code-size+
                                +function-machine-code-position+)
                          header-data)
                     16))
         (n-constants (ldb (byte +function-constant-pool-size+
                                 +function-constant-pool-position+)
                           header-data))
         ;; Address of GC metadata & the length.
         (address (+ fn-address mc-size (* n-constants 8)))
         (length (ldb (byte +function-gc-metadata-size+
                            +function-gc-metadata-position+)
                      header-data))
         ;; Position within the metadata.
         (position 0))
    (flet ((consume (&optional (errorp t))
             (when (>= position length)
               (when errorp
                 (mezzano.supervisor:panic "Corrupt GC info in function " function-to-inspect))
               (return-from map-function-gc-metadata))
             (prog1 (memref-unsigned-byte-8 address position)
               (incf position))))
      (declare (dynamic-extent #'consume))
      (loop (let ((start-offset-in-function 0)
                  flags-and-pvr
                  mv-and-ia
                  (pv 0)
                  (n-layout-bits 0)
                  layout-address)
              ;; Read first byte of address, this is where we can terminate.
              (let ((byte (consume nil))
                    (offset 0))
                (setf start-offset-in-function (ldb (byte 7 0) byte)
                      offset 7)
                (when (logtest byte #x80)
                  ;; Read remaining bytes.
                  (loop (let ((byte (consume)))
                          (setf (ldb (byte 7 offset) start-offset-in-function)
                                (ldb (byte 7 0) byte))
                          (incf offset 7)
                          (unless (logtest byte #x80)
                            (return))))))
              ;; Read flag/pvr byte
              (setf flags-and-pvr (consume))
              ;; Read mv-and-ia
              (setf mv-and-ia (consume))
              ;; Read vs32 pv.
              (let ((shift 0))
                (loop
                   (let ((b (consume)))
                     (when (not (logtest b #x80))
                       (setf pv (logior pv (ash (logand b #x3F) shift)))
                       (when (logtest b #x40)
                         (setf pv (- pv)))
                       (return))
                     (setf pv (logior pv (ash (logand b #x7F) shift)))
                     (incf shift 7))))
              ;; Read vu32 n-layout bits.
              (let ((shift 0))
                (loop
                   (let ((b (consume)))
                     (setf n-layout-bits (logior n-layout-bits (ash (logand b #x7F) shift)))
                     (when (not (logtest b #x80))
                       (return))
                     (incf shift 7))))
              (setf layout-address (+ address position))
              ;; Consume layout bits.
              (incf position (ceiling n-layout-bits 8))
              ;; Decode this entry and do something else.
              (funcall function
                       ;; Start offset in the function.
                       start-offset-in-function
                       ;; Frame/no-frame.
                       (logtest flags-and-pvr #b00001)
                       ;; Interrupt.
                       (logtest flags-and-pvr #b00010)
                       ;; Pushed-values.
                       pv
                       ;; Pushed-values-register.
                       (if (logtest flags-and-pvr #b10000)
                           :rcx
                           nil)
                       ;; Layout-address. Fixnum pointer to virtual memory
                       ;; the inspected function must remain live to keep
                       ;; this valid.
                       layout-address
                       ;; Number of bits in the layout.
                       n-layout-bits
                       ;; Multiple-values.
                       (if (eql (ldb (byte 4 0) mv-and-ia) 15)
                           nil
                           (ldb (byte 4 0) mv-and-ia))
                       ;; Incoming-arguments.
                       (if (logtest flags-and-pvr #b1000)
                           (if (eql (ldb (byte 4 4) mv-and-ia) 15)
                               :rcx
                               (ldb (byte 4 4) mv-and-ia))
                           nil)
                       ;; Block-or-tagbody-thunk.
                       (if (logtest flags-and-pvr #b0100)
                           :rax
                           nil)
                       ;; Extra-registers.
                       (case (ldb (byte 2 6) flags-and-pvr)
                         (0 nil)
                         (1 :rax)
                         (2 :rax-rcx)
                         (3 :rax-rcx-rdx))
                       ;; Restart
                       (logtest flags-and-pvr #b10000000)))))))

(declaim (inline memref-t (setf memref-t)))
(defun memref-t (base &optional (index 0))
  (%memref-t base index))
(defun (setf memref-t) (value base &optional (index 0))
  (setf (%memref-t base index) value))

(declaim (inline memref-unsigned-byte-8 (setf memref-unsigned-byte-8)))
(defun memref-unsigned-byte-8 (base &optional (index 0))
  (%memref-unsigned-byte-8 base index))
(defun (setf memref-unsigned-byte-8) (value base &optional (index 0))
  (setf (%memref-unsigned-byte-8 base index) value))

(declaim (inline memref-unsigned-byte-16 (setf memref-unsigned-byte-16)))
(defun memref-unsigned-byte-16 (base &optional (index 0))
  (%memref-unsigned-byte-16 base index))
(defun (setf memref-unsigned-byte-16) (value base &optional (index 0))
  (setf (%memref-unsigned-byte-16 base index) value))

(declaim (inline memref-unsigned-byte-32 (setf memref-unsigned-byte-32)))
(defun memref-unsigned-byte-32 (base &optional (index 0))
  (%memref-unsigned-byte-32 base index))
(defun (setf memref-unsigned-byte-32) (value base &optional (index 0))
  (setf (%memref-unsigned-byte-32 base index) value))

(declaim (inline memref-unsigned-byte-64 (setf memref-unsigned-byte-64)))
(defun memref-unsigned-byte-64 (base &optional (index 0))
  (%memref-unsigned-byte-64 base index))
(defun (setf memref-unsigned-byte-64) (value base &optional (index 0))
  (setf (%memref-unsigned-byte-64 base index) value))

(declaim (inline memref-signed-byte-8 (setf memref-signed-byte-8)))
(defun memref-signed-byte-8 (base &optional (index 0))
  (%memref-signed-byte-8 base index))
(defun (setf memref-signed-byte-8) (value base &optional (index 0))
  (setf (%memref-signed-byte-8 base index) value))

(declaim (inline memref-signed-byte-16 (setf memref-signed-byte-16)))
(defun memref-signed-byte-16 (base &optional (index 0))
  (%memref-signed-byte-16 base index))
(defun (setf memref-signed-byte-16) (value base &optional (index 0))
  (setf (%memref-signed-byte-16 base index) value))

(declaim (inline memref-signed-byte-32 (setf memref-signed-byte-32)))
(defun memref-signed-byte-32 (base &optional (index 0))
  (%memref-signed-byte-32 base index))
(defun (setf memref-signed-byte-32) (value base &optional (index 0))
  (setf (%memref-signed-byte-32 base index) value))

(declaim (inline memref-signed-byte-64 (setf memref-signed-byte-64)))
(defun memref-signed-byte-64 (base &optional (index 0))
  (%memref-signed-byte-64 base index))
(defun (setf memref-signed-byte-64) (value base &optional (index 0))
  (setf (%memref-signed-byte-64 base index) value))

(declaim (inline %object-ref-unsigned-byte-8 (setf %object-ref-unsigned-byte-8)))
(defun %object-ref-unsigned-byte-8 (object index)
  (%%object-ref-unsigned-byte-8 object index))
(defun (setf %object-ref-unsigned-byte-8) (value object index)
  (check-type value (unsigned-byte 8))
  (setf (%%object-ref-unsigned-byte-8 object index) value))

(declaim (inline %object-ref-unsigned-byte-16 (setf %object-ref-unsigned-byte-16)))
(defun %object-ref-unsigned-byte-16 (object index)
  (%%object-ref-unsigned-byte-16 object index))
(defun (setf %object-ref-unsigned-byte-16) (value object index)
  (check-type value (unsigned-byte 16))
  (setf (%%object-ref-unsigned-byte-16 object index) value))

(declaim (inline %object-ref-unsigned-byte-32 (setf %object-ref-unsigned-byte-32)))
(defun %object-ref-unsigned-byte-32 (object index)
  (%%object-ref-unsigned-byte-32 object index))
(defun (setf %object-ref-unsigned-byte-32) (value object index)
  (check-type value (unsigned-byte 32))
  (setf (%%object-ref-unsigned-byte-32 object index) value))

(declaim (inline %object-ref-signed-byte-8 (setf %object-ref-signed-byte-8)))
(defun %object-ref-signed-byte-8 (object index)
  (%%object-ref-signed-byte-8 object index))
(defun (setf %object-ref-signed-byte-8) (value object index)
  (check-type value (signed-byte 8))
  (setf (%%object-ref-signed-byte-8 object index) value))

(declaim (inline %object-ref-signed-byte-16 (setf %object-ref-signed-byte-16)))
(defun %object-ref-signed-byte-16 (object index)
  (%%object-ref-signed-byte-16 object index))
(defun (setf %object-ref-signed-byte-16) (value object index)
  (check-type value (signed-byte 16))
  (setf (%%object-ref-signed-byte-16 object index) value))

(declaim (inline %object-ref-signed-byte-32 (setf %object-ref-signed-byte-32)))
(defun %object-ref-signed-byte-32 (object index)
  (%%object-ref-signed-byte-32 object index))
(defun (setf %object-ref-signed-byte-32) (value object index)
  (check-type value (signed-byte 32))
  (setf (%%object-ref-signed-byte-32 object index) value))

(declaim (inline %object-ref-single-float (setf %object-ref-single-float)))
(defun %object-ref-single-float (object index)
  (%%object-ref-single-float object index))
(defun (setf %object-ref-single-float) (value object index)
  (check-type value single-float)
  (setf (%%object-ref-single-float object index) value)
  value)

(declaim (inline %%object-ref-single-float (setf %%object-ref-single-float)))
(defun %%object-ref-single-float (object index)
  (%integer-as-single-float (%%object-ref-unsigned-byte-32 object index)))
(defun (setf %%object-ref-single-float) (value object index)
  (setf (%%object-ref-unsigned-byte-32 object index)
        (%single-float-as-integer value))
  value)

(declaim (inline %object-ref-double-float (setf %object-ref-double-float)))
(defun %object-ref-double-float (object index)
  (%integer-as-double-float (%object-ref-unsigned-byte-64 object index)))
(defun (setf %object-ref-double-float) (value object index)
  (setf (%object-ref-unsigned-byte-64 object index)
        (%double-float-as-integer value))
  value)
