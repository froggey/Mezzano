(in-package :mezzanine.runtime)

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

(sys.int::define-lap-function values-list ()
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
                               (* mezzanine.supervisor::+thread-mv-slots-start+ 8)))
  (:gc :frame :layout #*10 :multiple-values 0)
  unpack-loop
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:cmp32 :ecx #.(ash (+ (- mezzanine.supervisor::+thread-mv-slots-end+ mezzanine.supervisor::+thread-mv-slots-start+) 5) sys.int::+n-fixnum-bits+))
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
  (sys.lap-x86:mov64 :r13 (:function raise-type-error))
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

;;; TODO: This requires a considerably more flexible mechanism.
(defvar *tls-lock*)
(defvar sys.int::*next-symbol-tls-slot*)
(defconstant +maximum-tls-slot+ (1+ mezzanine.supervisor::+thread-tls-slots-end+))
(defun sys.int::%allocate-tls-slot (symbol)
  (mezzanine.supervisor::with-symbol-spinlock (*tls-lock*)
    ;; Make sure that another thread didn't allocate a slot while we were waiting for the lock.
    (cond ((zerop (ldb (byte 16 10) (sys.int::%array-like-ref-unsigned-byte-64 symbol -1)))
           (when (>= sys.int::*next-symbol-tls-slot* +maximum-tls-slot+)
             (error "Critial error! TLS slots exhausted!"))
           (let ((slot sys.int::*next-symbol-tls-slot*))
             (incf sys.int::*next-symbol-tls-slot*)
             ;; Twiddle TLS bits directly in the symbol header.
             (setf (ldb (byte 16 10) (sys.int::%array-like-ref-unsigned-byte-64 symbol -1)) slot)
             slot))
          (t (ldb (byte 16 10) (sys.int::%array-like-ref-unsigned-byte-64 symbol -1))))))

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
     (let ((fref (sys.int::symbol-fref object)))
       (when (not fref)
         (return-from sys.int::%coerce-to-callable
           (fdefinition object)))
       (let ((fn (sys.int::%array-like-ref-t fref sys.int::+fref-function+)))
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
