;;;; Object related builtins.

(in-package :mezzano.compiler.backend.x86-64)

(define-builtin sys.int::%value-has-tag-p ((object (:constant tag (typep tag '(unsigned-byte 4))))
                                           :z
                                           :has-wrapper nil)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:lea64
                         :operands (list temp `(,object ,(- tag)))
                         :inputs (list object)
                         :outputs (list temp)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:test64
                         :operands (list temp #b1111)
                         :inputs (list temp)
                         :outputs '()))))

(define-builtin sys.int::%value-has-immediate-tag-p ((object (:constant tag (typep tag '(unsigned-byte 2))))
                                                     :z
                                                     :has-wrapper nil)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:lea64
                         :operands (list temp `(,object ,(- (logior (ash tag 4) sys.int::+tag-immediate+))))
                         :inputs (list object)
                         :outputs (list temp)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:test64
                         :operands (list temp #b111111)
                         :inputs (list temp)
                         :outputs '()))))

(define-builtin mezzano.runtime::%%object-of-type-p ((object (:constant object-tag (typep object-tag '(unsigned-byte 6))))
                                                     :e
                                                     :has-wrapper nil)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp8
                       :operands (list `(:object ,object -1) (ash object-tag sys.int::+object-type-shift+))
                       :inputs (list object)
                       :outputs '())))

(define-builtin mezzano.runtime::%%object-of-type-range-p ((object
                                                            (:constant first-tag (typep first-tag '(unsigned-byte 6)))
                                                            (:constant last-tag (typep last-tag '(unsigned-byte 6))))
                                                           :be
                                                           :has-wrapper nil)
  ;; TODO: Use an integer vreg instead of rax here. x86-instruction must be extended to support converting allocated pregs to their 8-bit counterparts.
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (when (not (eql first-tag 0))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:sub8
                         :operands (list :al (ash first-tag
                                                  sys.int::+object-type-shift+))
                         :inputs (list :rax)
                         :outputs (list :rax))))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp8
                       :operands (list :al (ash (- last-tag first-tag)
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs '())))

(define-builtin sys.int::%instance-or-funcallable-instance-p ((object) :e)
  ;; TODO: Use an integer vreg instead of rax here. x86-instruction must be extended to support converting allocated pregs to their 8-bit counterparts.
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:and8
                       :operands (list :al (ash (logxor (1- (ash 1 sys.int::+object-type-size+))
                                                        sys.int::+object-tag-instance+
                                                        sys.int::+object-tag-funcallable-instance+)
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp8
                       :operands (list :al (ash (logand sys.int::+object-tag-instance+
                                                        sys.int::+object-tag-funcallable-instance+)
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs '())))

(define-builtin sys.int::%object-tag ((object) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:movzx8
                       :operands (list :eax `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:shr32
                       :operands (list :eax 1)
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'ir:move-instruction
                       :source :rax
                       :destination result)))

;;; Constructing and deconstructing Lisp values.

(define-builtin sys.int::lisp-object-address ((value) result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:lea64
                       :operands (list result (list value value))
                       :inputs (list value)
                       :outputs (list result))))

(define-builtin sys.int::%%assemble-value ((pointer tag) result)
  (cond ((constant-value-p tag '(eql 0))
         (emit (make-instance 'x86-fake-three-operand-instruction
                              :opcode 'lap:sar64
                              :result result
                              :lhs pointer
                              :rhs sys.int::+n-fixnum-bits+)))
        ((constant-value-p tag '(signed-byte 29))
         (let ((raw-pointer (make-instance 'ir:virtual-register)))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:sar64
                                :result raw-pointer
                                :lhs pointer
                                :rhs sys.int::+n-fixnum-bits+))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:or64
                                :result result
                                :lhs raw-pointer
                                :rhs (fetch-constant-value tag)))))
        (t
         (let ((raw-pointer (make-instance 'ir:virtual-register))
               (raw-tag (make-instance 'ir:virtual-register :kind :integer)))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:sar64
                                :result raw-pointer
                                :lhs pointer
                                :rhs sys.int::+n-fixnum-bits+))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:sar64
                                :result raw-tag
                                :lhs tag
                                :rhs sys.int::+n-fixnum-bits+))
           (emit (make-instance 'x86-fake-three-operand-instruction
                                :opcode 'lap:or64
                                :result result
                                :lhs raw-pointer
                                :rhs raw-tag))))))

(define-builtin sys.int::%pointer-field ((value) result)
  (let ((temp (make-instance 'ir:virtual-register)))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:and64
                         :result temp
                         :lhs value
                         :rhs -16))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:sar64
                         :result result
                         :lhs temp
                         :rhs (- (byte-size sys.int::+tag-field+)
                                 sys.int::+n-fixnum-bits+)))))

(define-builtin sys.int::%tag-field ((value) result)
  (let ((temp (make-instance 'ir:virtual-register)))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:shl64
                         :result temp
                         :lhs value
                         :rhs sys.int::+n-fixnum-bits+))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:and64
                         :result result
                         :lhs temp
                         :rhs (ash (1- (ash 1 (byte-size sys.int::+tag-field+)))
                                   sys.int::+n-fixnum-bits+)))))

;;; Support objects

(defmacro define-support-object (name symbol)
  (let ((predicate-name (intern (format nil "~A-P" name) (symbol-package name))))
    `(progn
       (define-builtin ,predicate-name ((object) :e)
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list object ,symbol)
                              :inputs (list object)
                              :outputs (list))))
       (define-builtin ,name (() result)
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:mov64
                              :operands (list result ,symbol)
                              :inputs (list)
                              :outputs (list result)))))))

(define-support-object sys.int::%unbound-value :unbound-value)
(define-support-object sys.int::%symbol-binding-cache-sentinel :symbol-binding-cache-sentinel)
(define-support-object sys.int::%layout-instance-header :layout-instance-header)

(define-builtin eq ((lhs rhs) :e)
  (cond ((constant-value-p rhs '(eql nil))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list lhs nil)
                              :inputs (list lhs)
                              :outputs '())))
        ((constant-value-p rhs '(eql t))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list lhs t)
                              :inputs (list lhs)
                              :outputs '())))
        ((constant-value-p rhs '(eql 0))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:test64
                              :operands (list lhs lhs)
                              :inputs (list lhs)
                              :outputs '())))
        ((constant-value-p rhs '(signed-byte 31))
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list lhs (ash (fetch-constant-value rhs)
                                                       sys.int::+n-fixnum-bits+))
                              :inputs (list lhs)
                              :outputs '())))
        (t
         (emit (make-instance 'x86-instruction
                              :opcode 'lap:cmp64
                              :operands (list lhs rhs)
                              :inputs (list lhs rhs)
                              :outputs '())))))

(defmacro with-builtin-object-access ((effective-address additional-inputs object index scale) &body body)
  "Generate an effective address that deals properly with scaling and constant indices."
  (check-type scale (member 1 2 4 8))
  `(cond ((constant-value-p ,index '(signed-byte 29))
          (let ((,effective-address `(:object-unscaled ,,object ,(* (fetch-constant-value ,index) ,scale)))
                (,additional-inputs (list ,object)))
            ,@body))
         (t
          ,(if (eql scale 1)
               (let ((unboxed-index (gensym)))
                 `(let ((,unboxed-index (make-instance 'ir:virtual-register :kind :integer)))
                    (emit (make-instance 'ir:unbox-fixnum-instruction
                                         :source ,index
                                         :destination ,unboxed-index))
                    (let ((,effective-address `(:object ,,object 0 ,,unboxed-index 1))
                          (,additional-inputs (list ,object ,unboxed-index)))
                      ,@body)))
               `(let ((,effective-address `(:object ,,object 0 ,,index ,',(/ scale 2)))
                      (,additional-inputs (list ,object ,index)))
                  ,@body)))))

(define-builtin sys.int::%object-ref-t ((object index) result)
  (with-builtin-object-access (ea ea-inputs object index 8)
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list result ea)
                         :inputs ea-inputs
                         :outputs (list result)))))

(define-builtin (setf sys.int::%object-ref-t) ((value object index) result)
  (with-builtin-object-access (ea ea-inputs object index 8)
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list ea value)
                         :inputs (list* value ea-inputs)
                         :outputs (list))))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))

(define-builtin sys.int::%object-header-data ((object) result)
  (let ((temp1 (make-instance 'ir:virtual-register))
        (temp2 (make-instance 'ir:virtual-register)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:and64
                         :result temp2
                         :lhs temp1
                         :rhs (lognot (1- (ash 1 sys.int::+object-data-shift+)))))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:shr64
                         :result result
                         :lhs temp2
                         :rhs (- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+)))))

(define-builtin (setf sys.int::%object-header-data) ((value object) result)
  (emit (make-instance 'ir:move-instruction
                       :destination :rax
                       :source value))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:shl64
                       :operands (list :rax (- sys.int::+object-data-shift+
                                               sys.int::+n-fixnum-bits+))
                       :inputs (list :rax)
                       :outputs (list :rax)
                       :clobbers (list :rax)))
  ;; low 8 bits of the header only.
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list :rax object)
                       :outputs (list :rax)
                       :clobbers (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list `(:object ,object -1) :rax)
                       :inputs (list :rax object)
                       :outputs (list)
                       :clobbers (list)))
  (emit (make-instance 'ir:move-instruction
                       :destination result
                       :source value)))

(define-builtin sys.int::%instance-layout ((object) result)
  (let ((temp1 (make-instance 'ir:virtual-register)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:shr64
                         :result result
                         :lhs temp1
                         :rhs sys.int::+object-data-shift+))
    ;; The object must be kept live over the shift, as the header will
    ;; initially be read as a fixnum. If the object is the only thing keeping
    ;; the structure definition live there is a possibility that it and the
    ;; structure definition could be end up being GC'd between the load & shift.
    ;; Then the shift would resurrect a dead object, leading to trouble.
    (emit (make-instance 'ir:spice-instruction :value object))))

(define-builtin sys.int::%fast-instance-layout-eq-p ((object instance-header) :e)
  (let ((temp1 (make-instance 'ir:virtual-register :kind :integer))
        (temp2 (make-instance 'ir:virtual-register :kind :integer)))
    ;; Read the object header.
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    ;; Set the two low bits to potentially convert the header to a
    ;; structure-header. This must be performed in an integer register
    ;; as this will construct some random bad value if the object isn't a
    ;; structure-object.
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:or64
                         :result temp2
                         :lhs temp1
                         :rhs 3))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cmp64
                         :operands (list temp2 instance-header)
                         :inputs (list temp2 instance-header)
                         :outputs '()))))

(defmacro define-object-ref-integer-accessor (name read-op write-op reg scale box-op unbox-op)
  `(progn
     (define-builtin ,name ((object index) result)
       (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
         ;; Need to use :eax and a temporary here because it's currently impossible
         ;; to replace vregs with non-64-bit gprs.
         ;; Using a temporary & a move before the box allows the box to safely
         ;; eliminated.
         (with-builtin-object-access (ea ea-inputs object index ,scale)
           (emit (make-instance 'x86-instruction
                                :opcode ',read-op
                                :operands (list ,(if (eql read-op 'lap:mov32) reg 'temp) ea)
                                :inputs ea-inputs
                                :outputs (list ,(if (eql read-op 'lap:mov32) :rax 'temp))
                                :clobbers ,(if (eql read-op 'lap:mov32) ''(:rax) 'nil))))
         ,@(when (eql read-op 'lap:mov32)
             `((emit (make-instance 'ir:move-instruction
                                    :source :rax
                                    :destination temp))))
         (emit (make-instance ',box-op
                              :source temp
                              :destination result))))
     (define-builtin (setf ,name) ((value object index) result)
       (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
         ;; Need to use :eax and a temporary here because it's currently impossible
         ;; to replace vregs with non-64-bit gprs.
         ;; Using a temporary & a move before the box allows the box to safely
         ;; eliminated.
         (emit (make-instance ',unbox-op
                              :source value
                              :destination temp))
         ,@(when reg
             `((emit (make-instance 'ir:move-instruction
                                    :source temp
                                    :destination :rax))))
         (with-builtin-object-access (ea ea-inputs object index ,scale)
           (emit (make-instance 'x86-instruction
                                :opcode ',write-op
                                :operands (list ea ,(or reg 'temp))
                                :inputs (list* ,(if reg :rax 'temp) ea-inputs)
                                :outputs (list))))
         (emit (make-instance 'ir:move-instruction
                              :source value
                              :destination result))))
     (define-builtin (sys.int::cas ,name) ((old new object index) result)
       (let ((old-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (new-unboxed (make-instance 'ir:virtual-register :kind :integer))
             (result-unboxed (make-instance 'ir:virtual-register :kind :integer)))
         (emit (make-instance ',unbox-op
                              :source old
                              :destination old-unboxed))
         (emit (make-instance ',unbox-op
                              :source new
                              :destination new-unboxed))
         (emit (make-instance 'ir:move-instruction
                              :source old-unboxed
                              :destination :rax))
         (emit (make-instance 'ir:move-instruction
                              :source new-unboxed
                              :destination :rdx))
         (with-builtin-object-access (ea ea-inputs object index ,scale)
           (emit (make-instance 'x86-instruction
                                :opcode 'lap:cmpxchg
                                :prefix '(lap:lock)
                                :operands (list ea ,(ecase reg
                                                      (:al :dl)
                                                      (:ax :dx)
                                                      (:eax :edx)
                                                      ((nil) :rdx)))
                                :inputs (list* :rax :rdx ea-inputs)
                                :outputs (list :rax)
                                :clobbers '(:rax))))
         ,@(when (not (member read-op '(lap:mov32 lap:mov64)))
             `((emit (make-instance 'x86-instruction
                                    :opcode ',read-op
                                    :operands (list :rax ,reg)
                                    :inputs (list :rax)
                                    :outputs (list :rax)))))
         (emit (make-instance 'ir:move-instruction
                              :source :rax
                              :destination result-unboxed))
         (emit (make-instance ',box-op
                              :source result-unboxed
                              :destination result))))))

(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-8  lap:movzx8  lap:mov8  :al  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-16 lap:movzx16 lap:mov16 :ax  2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-32 lap:mov32   lap:mov32 :eax 4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-64 lap:mov64   lap:mov64 nil  8 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-8    lap:movsx8  lap:mov8  :al  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-16   lap:movsx16 lap:mov16 :ax  2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-32   lap:movsx32 lap:mov32 :eax 4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-64   lap:mov64   lap:mov64 nil  8 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)

(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-8-unscaled  lap:movzx8  lap:mov8  :al  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-16-unscaled lap:movzx16 lap:mov16 :ax  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-32-unscaled lap:mov32   lap:mov32 :eax 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-64-unscaled lap:mov64   lap:mov64 nil  1 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-8-unscaled    lap:movsx8  lap:mov8  :al  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-16-unscaled   lap:movsx16 lap:mov16 :ax  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-32-unscaled   lap:movsx32 lap:mov32 :eax 1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-64-unscaled   lap:mov64   lap:mov64 nil  1 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)

;;; Atomic operations.
;;; These functions index into the object like %OBJECT-REF-T.
;;; There are no atomic functions that access memory like MEMREF.

;; Add DELTA to the slot at SLOT in OBJECT.
;; Returns the old value of the slot.
;; DELTA and the value of the slot must both be fixnums.
;; (defun fixnum-add (object slot delta)
;;   (prog1 (%object-ref-t object slot)
;;     (incf (%object-ref-t object slot) delta)))
(define-builtin sys.int::%atomic-fixnum-add-object ((object offset delta) result)
  (emit (make-instance 'x86-atomic-instruction
                       :opcode 'lap:xadd64
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :rhs delta
                       :result result
                       :prefix '(lap:lock))))

;; Perform LOGAND of the slot at SLOT in OBJECT and VALUE.
;; Returns no values.
;; DELTA and the value of the slot must both be fixnums.
;; (defun fixnum-logand (object slot value)
;;   (setf (%object-ref-t object slot)
;;         (logand (%object-ref-t object slot) value))
;;   (values))
(define-builtin sys.int::%atomic-fixnum-logand-object ((object offset value) ())
  (emit (make-instance 'x86-atomic-instruction
                       :opcode 'lap:and64
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :rhs value
                       :result nil
                       :prefix '(lap:lock))))

;; Perform LOGIOR of the slot at SLOT in OBJECT and VALUE.
;; Returns no values.
;; DELTA and the value of the slot must both be fixnums.
;; (defun fixnum-logior (object slot value)
;;   (setf (%object-ref-t object slot)
;;         (logior (%object-ref-t object slot) value))
;;   (values))
(define-builtin sys.int::%atomic-fixnum-logior-object ((object offset value) ())
  (emit (make-instance 'x86-atomic-instruction
                       :opcode 'lap:or64
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :rhs value
                       :result nil
                       :prefix '(lap:lock))))

;; Perform LOGXOR of the slot at SLOT in OBJECT and VALUE.
;; Returns no values.
;; DELTA and the value of the slot must both be fixnums.
;; (defun fixnum-logxor (object slot value)
;;   (setf (%object-ref-t object slot)
;;         (logxor (%object-ref-t object slot) value))
;;   (values))
(define-builtin sys.int::%atomic-fixnum-logxor-object ((object offset value) ())
  (emit (make-instance 'x86-atomic-instruction
                       :opcode 'lap:xor64
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :rhs value
                       :result nil
                       :prefix '(lap:lock))))

;; Set the value in SLOT to NEW, and return the old value.
;; (defun xchg (object slot new)
;;   (prog1 (%object-ref-t object slot)
;;     (setf (%object-ref-t object slot) new)))
(define-builtin sys.int::%xchg-object ((object offset new) result)
  (emit (make-instance 'x86-atomic-instruction
                       :opcode 'lap:xchg64
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :rhs new
                       :result result)))

;; If the value in SLOT matches OLD, set it to NEW; otherwise do nothing.
;; Returns true as the primary value if the slot was modified, false otherwise.
;; Additionally returns the old value of SLOT as the second value.
;; (defun cas (object offset old new)
;;   (let ((slot-value (%object-ref-t object slot)))
;;     (values (cond ((eq slot-value old)
;;                    (setf (%object-ref-t object slot) new)
;;                    t)
;;                   (t nil))
;;             slot-value)))
(define-builtin sys.int::%cas-object ((object offset old new) (:z result))
  (emit (make-instance 'x86-cmpxchg-instruction
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :old old
                       :new new
                       :result result
                       :prefix '(lap:lock))))

;; Similar to %CAS-OBJECT, but performs two CAS operations on adjacent slots.
;; Returns the two old slot values and a success boolean.
;; (defun dcas (object offset old-1 old-2 new-1 new-2)
;;   (let ((slot-value-1 (%object-ref-t object slot))
;;         (slot-value-2 (%object-ref-t object (1+ slot))))
;;     (values (cond ((and (eq slot-value-1 old-1)
;;                         (eq slot-value-2 old-2))
;;                    (setf (%object-ref-t object slot) new-1
;;                          (%object-ref-t object (1+ slot)) new-2)
;;                    t)
;;                   (t nil))
;;             slot-value-1
;;             slot-value-2)))
(define-builtin sys.int::%dcas-object ((object offset old-1 old-2 new-1 new-2) (:z result-1 result-2))
  (emit (make-instance 'x86-cmpxchg16b-instruction
                       :object object
                       :index (if (constant-value-p offset '(signed-byte 29))
                                  (fetch-constant-value offset)
                                  offset)
                       :old-1 old-1
                       :old-2 old-2
                       :new-1 new-1
                       :new-2 new-2
                       :result-1 result-1
                       :result-2 result-2
                       :prefix '(lap:lock))))
