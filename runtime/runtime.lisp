;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
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

(sys.int::define-lap-function values-list ((list)
                                           ((list 0)))
  "Returns the elements of LIST as multiple values."
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:sub64 :rsp 16) ; 2 slots
  (sys.lap-x86:cmp32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:jne bad-arguments)
  ;; RBX = iterator, (:stack 0) = list.
  (sys.lap-x86:mov64 :rbx :r8)
  (sys.lap-x86:mov64 (:stack 0) :r8)
  (:gc :frame :layout #*10)
  ;; ECX = value count.
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Pop into R8.
  ;; If LIST is NIL, then R8 must be NIL, so no need to
  ;; set R8 to NIL in the 0-values case.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r8 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R9.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r9 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R10.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r10 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R11.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r11 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R12.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r12 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Registers are populated, now unpack into the MV-area
  (sys.lap-x86:mov32 :edi #.(+ (- 8 sys.int::+tag-object+)
                               (* mezzano.supervisor::+thread-mv-slots-start+ 8)))
  (:gc :frame :layout #*10 :multiple-values 0)
  unpack-loop
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:cmp32 :ecx #.(ash (+ (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+) 5) sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jae too-many-values)
  (sys.lap-x86:mov64 :r13 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:rdi) :r13)
  (:gc :frame :layout #*10 :multiple-values 1)
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :layout #*10 :multiple-values 0)
  (sys.lap-x86:add64 :rdi 8)
  (sys.lap-x86:jmp unpack-loop)
  done
  (sys.lap-x86:leave)
  (:gc :no-frame :multiple-values 0)
  (sys.lap-x86:ret)
  type-error
  (:gc :frame :layout #*10)
  (sys.lap-x86:mov64 :r8 (:stack 0))
  (sys.lap-x86:mov64 :r9 (:constant proper-list))
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-type-error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2)
  too-many-values
  (sys.lap-x86:mov64 :r8 (:constant "Too many values in list ~S."))
  (sys.lap-x86:mov64 :r9 (:stack 0))
  (sys.lap-x86:mov64 :r13 (:function error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2)
  bad-arguments
  (:gc :frame)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%invalid-argument-error))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  (sys.lap-x86:ud2))

(sys.int::define-lap-function sys.int::values-simple-vector ((simple-vector))
  "Returns the elements of SIMPLE-VECTOR as multiple values."
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  ;; Check arg count.
  (sys.lap-x86:cmp64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:jne bad-arguments)
  ;; Check type.
  (sys.lap-x86:mov8 :al :r8l)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-object+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :rax (:object :r8 -1))
  ;; Simple vector object tag is zero.
  (sys.lap-x86:test8 :al #.(ash (1- (ash 1 sys.int::+object-type-size+))
                                sys.int::+object-type-shift+))
  (sys.lap-x86:jnz type-error)
  ;; Get number of values.
  (sys.lap-x86:shr64 :rax #.sys.int::+object-data-shift+)
  (sys.lap-x86:jz zero-values)
  (sys.lap-x86:cmp64 :rax #.(+ (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+) 5))
  (sys.lap-x86:jae too-many-values)
  ;; Set up. RBX = vector, RCX = number of values loaded so far, RAX = total number of values.
  (sys.lap-x86:mov64 :rbx :r8)
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Load register values.
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r8 (:object :rbx 0))
  (sys.lap-x86:cmp64 :rax 1)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r9 (:object :rbx 1))
  (sys.lap-x86:cmp64 :rax 2)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r10 (:object :rbx 2))
  (sys.lap-x86:cmp64 :rax 3)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r11 (:object :rbx 3))
  (sys.lap-x86:cmp64 :rax 4)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r12 (:object :rbx 4))
  (sys.lap-x86:cmp64 :rax 5)
  (sys.lap-x86:je done)
  ;; Registers are populated, now unpack into the MV-area
  (sys.lap-x86:mov32 :edi #.(+ (- 8 sys.int::+tag-object+)
                               (* mezzano.supervisor::+thread-mv-slots-start+ 8)))
  (sys.lap-x86:mov32 :edx 5) ; Current value.
  (:gc :frame :multiple-values 0)
  unpack-loop
  (sys.lap-x86:mov64 :r13 (:object :rbx 0 :rdx))
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:rdi) :r13)
  (:gc :frame :multiple-values 1)
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :multiple-values 0)
  (sys.lap-x86:add64 :rdi 8)
  (sys.lap-x86:add64 :rdx 1)
  (sys.lap-x86:cmp64 :rdx :rax)
  (sys.lap-x86:jne unpack-loop)
  done
  (sys.lap-x86:leave)
  (:gc :no-frame :multiple-values 0)
  (sys.lap-x86:ret)
  ;; Special-case 0 values as it requires NIL in R8.
  zero-values
  (:gc :frame)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:jmp done)
  (:gc :frame)
  type-error
  (sys.lap-x86:mov64 :r9 (:constant simple-vector))
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-type-error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2)
  too-many-values
  (sys.lap-x86:mov64 :r8 (:constant "Too many values in simple-vector ~S."))
  (sys.lap-x86:mov64 :r9 :rbx)
  (sys.lap-x86:mov64 :r13 (:function error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2)
  bad-arguments
  (sys.lap-x86:mov64 :r13 (:function sys.int::%invalid-argument-error))
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2))

;;; TODO: This requires a considerably more flexible mechanism.
(defvar *tls-lock*)
(defvar sys.int::*next-symbol-tls-slot*)
(defconstant +maximum-tls-slot+ (1+ mezzano.supervisor::+thread-tls-slots-end+))
(defun sys.int::%allocate-tls-slot (symbol)
  (mezzano.supervisor::safe-without-interrupts (symbol)
    (mezzano.supervisor::with-symbol-spinlock (*tls-lock*)
      ;; Make sure that another thread didn't allocate a slot while we were waiting for the lock.
      (cond ((zerop (ldb (byte sys.int::+symbol-header-tls-size+ sys.int::+symbol-header-tls-position+)
                         (sys.int::%object-header-data symbol)))
             (when (>= sys.int::*next-symbol-tls-slot* +maximum-tls-slot+)
               (error "Critial error! TLS slots exhausted!"))
             (let ((slot sys.int::*next-symbol-tls-slot*))
               (incf sys.int::*next-symbol-tls-slot*)
               ;; Twiddle TLS bits directly in the symbol header.
               (setf (ldb (byte sys.int::+symbol-header-tls-size+ sys.int::+symbol-header-tls-position+)
                          (sys.int::%object-header-data symbol))
                     slot)
               slot))
            (t (ldb (byte sys.int::+symbol-header-tls-size+ sys.int::+symbol-header-tls-position+)
                    (sys.int::%object-header-data symbol)))))))

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
  (do ((current *active-catch-handlers* (svref current 0)))
      ((not current)
       (error 'bad-catch-tag-error :tag tag))
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
         (or fn
             (fdefinition object)))))))

(defvar sys.int::*structure-type-type* nil)

;;; Manually define accessors & constructors for the structure-definition type.
;;; This is required because the structure-definition for structure-definition
;;; must be kept in *structure-type-type* and it must be wired.
;;; The structure-definition is currently created by supervisor;entry.lisp

(defun sys.int::make-struct-definition (name slots parent area)
  (let ((x (sys.int::%make-struct 6 :wired)))
    (setf (sys.int::%struct-slot x 0) sys.int::*structure-type-type*
	  (sys.int::%struct-slot x 1) name
	  (sys.int::%struct-slot x 2) slots
          (sys.int::%struct-slot x 3) parent
          (sys.int::%struct-slot x 4) area
          (sys.int::%struct-slot x 5) nil)
    x))

(defun sys.int::structure-definition-p (object)
  (eq (sys.int::%struct-slot object 0) sys.int::*structure-type-type*))

(macrolet ((def (name field)
             `(defun ,name (object)
                (unless (sys.int::structure-definition-p object)
                  (error 'type-error :datum object :expected-type 'sys.int::structure-definition))
                (sys.int::%struct-slot object ,field))))
  (def sys.int::structure-name 1)
  (def sys.int::structure-slots 2)
  (def sys.int::structure-parent 3)
  (def sys.int::structure-area 4)
  (def sys.int::structure-class 5))

(defun (setf sys.int::structure-class) (value object)
  (unless (sys.int::structure-definition-p object)
    (error 'type-error :datum object :expected-type 'sys.int::structure-definition))
  (setf (sys.int::%struct-slot object 5) value))

(defun sys.int::structure-type-p (object struct-type)
  "Test if OBJECT is a structure object of type STRUCT-TYPE."
  (when (sys.int::structure-object-p object)
    (do ((object-type (sys.int::%struct-slot object 0) (sys.int::structure-parent object-type)))
        ;; Stop when the object-type stops being a structure-definition, not
        ;; when it becomes NIL.
        ;; This avoids a race condition in the GC when it is
        ;; scavenging a partially initialized structure.
        ((not (and (sys.int::structure-object-p object-type)
                   (eql (sys.int::%struct-slot object-type 0)
                        sys.int::*structure-type-type*)))
         nil)
      (when (eq object-type struct-type)
        (return t)))))

(in-package :sys.int)

(defun return-address-to-function (return-address)
  "Convert a return address to a function pointer.
Dangerous! The return address must be kept live as a return address on a
thread's stack if this function is called from normal code."
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
         (return (%%assemble-value address sys.int::+tag-object+)))
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
 extra-registers"
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
                         (3 :rax-rcx-rdx))))))))
