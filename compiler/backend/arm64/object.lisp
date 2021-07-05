;;;; Object and memory related ARM64 builtins.

(in-package :mezzano.compiler.backend.arm64)

(define-builtin sys.int::%value-has-tag-p ((object (:constant tag (typep tag '(unsigned-byte 4)))) :eq :has-wrapper nil)
  (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp object #b1111)
                         :inputs (list object)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp tag)
                         :inputs (list temp)
                         :outputs '()))))

(define-builtin mezzano.runtime::%%object-of-type-p ((object (:constant object-tag (typep object-tag '(unsigned-byte 6)))) :eq :has-wrapper nil)
  (let ((header (make-instance 'ir:virtual-register :kind :integer))
        (temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list header `(,object ,(object-slot-displacement -1)))
                         :inputs (list object)
                         :outputs (list header)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp header #b11111100)
                         :inputs (list header)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp (ash object-tag sys.int::+object-type-shift+))
                         :inputs (list temp)
                         :outputs (list)))))

(define-builtin mezzano.runtime::%%simple-1d-array-p ((object) :ls)
  (let ((header (make-instance 'ir:virtual-register :kind :integer))
        (temp (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list header `(,object ,(object-slot-displacement -1)))
                         :inputs (list object)
                         :outputs (list header)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp header #b11111100)
                         :inputs (list header)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp (ash sys.int::+last-simple-1d-array-object-tag+
                                                        sys.int::+object-type-shift+))
                         :inputs (list temp)
                         :outputs (list)))))

(define-builtin sys.int::%instance-or-funcallable-instance-p ((object) :eq)
  (let ((header (make-instance 'ir:virtual-register :kind :integer))
        (temp (make-instance 'ir:virtual-register :kind :integer))
        (temp2 (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list header `(,object ,(object-slot-displacement -1)))
                         :inputs (list object)
                         :outputs (list header)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:movz
                         :operands (list temp2 (ash (logxor (1- (ash 1 sys.int::+object-type-size+))
                                                            sys.int::+object-tag-instance+
                                                            sys.int::+object-tag-funcallable-instance+)
                                                    sys.int::+object-type-shift+))
                         :inputs (list)
                         :outputs (list temp2)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp header temp2)
                         :inputs (list header temp2)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:subs
                         :operands (list :xzr temp (ash (logand sys.int::+object-tag-instance+
                                                                sys.int::+object-tag-funcallable-instance+)
                                                        sys.int::+object-type-shift+))
                         :inputs (list temp)
                         :outputs (list)))))

(define-builtin sys.int::%object-tag ((object) result)
  (let ((temp1 (make-instance 'ir:virtual-register :kind :integer))
        (temp2 (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp2 temp1 #b11111100)
                         :inputs (list temp1)
                         :outputs (list temp2)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list result :xzr temp2 :lsr sys.int::+n-fixnum-bits+)
                         :inputs (list temp2)
                         :outputs (list result)))))

(defmacro with-scaled-fixnum-index ((scaled-index index scale) &body body)
  "Scale index (a fixnum) appropriately based on scale, producing the scaled result in SCALED-INDEX."
  (ecase scale
    (1
     `(let ((,scaled-index (make-instance 'ir:virtual-register :kind :integer)))
        (emit (make-instance 'ir:unbox-fixnum-instruction
                             :source ,index
                             :destination ,scaled-index))
        ,@body))
    (2
     `(let ((,scaled-index ,index))
        ,@body))
    ((4 8)
     ;; Note: Can't used :scale here since the instruction width isn't known
     ;; and it wouldn't match up anyway since index is boxed.
     (let ((scale-shift (ecase scale (4 1) (8 2))))
       `(let ((,scaled-index (make-instance 'ir:virtual-register :kind :integer)))
          (emit (make-instance 'arm64-instruction
                               :opcode 'lap:add
                               :operands (list ,scaled-index :xzr ,index :lsl ,scale-shift)
                               :inputs (list ,index)
                               :outputs (list ,scaled-index)))
          ,@body)))))

(defmacro with-builtin-object-access ((effective-address additional-inputs object index scale) &body body)
  "Generate an effective address that deals properly with scaling and constant indices."
  (check-type scale (member 1 2 4 8))
  (let ((disp (gensym "DISP"))
        (disp-reg (gensym "DISP-REG"))
        (scaled-index (gensym "SCALED-INDEX")))
    `(cond ((constant-value-p index 'fixnum)
            (let ((,disp (object-slot-displacement (fetch-constant-value index) ',scale)))
              ; TODO: This could be cleverer based on the transfer size, instructions can support a 12-bit unsigned scaled-by-width immediate
              (cond ((typep ,disp '(signed-byte 6))
                     (let ((,effective-address (list ,object ,disp))
                           (,additional-inputs (list ,object)))
                       ,@body))
                    (t
                     (let ((,disp-reg (make-instance 'ir:virtual-register :kind :integer)))
                       (emit (make-instance 'arm64-instruction
                                            :opcode 'lap:ldr
                                            :operands (list ,disp-reg `(:literal ,,disp))
                                            :inputs (list)
                                            :outputs (list ,disp-reg)))
                       (let ((,effective-address (list ,object ,disp-reg))
                             (,additional-inputs (list ,object ,disp-reg)))
                         ,@body))))))
           (t
            (with-scaled-fixnum-index (,scaled-index ,index ,scale)
              (let ((,disp-reg (make-instance 'ir:virtual-register :kind :integer)))
                (emit (make-instance 'arm64-instruction
                                     :opcode 'lap:sub
                                     :operands (list ,disp-reg ,scaled-index (- (+ 8 (- sys.int::+tag-object+))))
                                     :inputs (list ,scaled-index)
                                     :outputs (list ,disp-reg)))
                (let ((,effective-address (list ,object ,disp-reg))
                      (,additional-inputs (list ,object ,disp-reg)))
                  ,@body)))))))

(defmacro define-object-ref-integer-accessor (name read-op write-op scale box-op unbox-op)
  `(progn
     (define-builtin ,name ((object index) result)
       (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
         (with-builtin-object-access (ea ea-inputs object index ,scale)
           (emit (make-instance 'arm64-instruction
                                :opcode ',read-op
                                :operands (list temp ea)
                                :inputs ea-inputs
                                :outputs (list temp))))
         (emit (make-instance ',box-op
                              :source temp
                              :destination result))))
     (define-builtin (setf ,name) ((value object index) result)
       (let ((temp (make-instance 'ir:virtual-register :kind :integer)))
         (emit (make-instance ',unbox-op
                              :source value
                              :destination temp))
         (with-builtin-object-access (ea ea-inputs object index ,scale)
           (emit (make-instance 'arm64-instruction
                                :opcode ',write-op
                                :operands (list temp ea)
                                :inputs (list* temp ea-inputs)
                                :outputs (list))))
         (emit (make-instance 'ir:move-instruction
                              :source value
                              :destination result))))
     #+(or) ; todo
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

(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-8  lap:ldrb  lap:strb  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-16 lap:ldrh  lap:strh  2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-32 lap:ldrw  lap:strw  4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-64 lap:ldr   lap:str   8 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-8    lap:ldrsb   lap:strb  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-16   lap:ldrsh   lap:strh  2 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-32   lap:ldrsw   lap:strw  4 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-64   lap:ldr     lap:str   8 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)

(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-8-unscaled  lap:ldrb  lap:strb  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-16-unscaled lap:ldrh  lap:strh  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-32-unscaled lap:ldrw  lap:strw  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-unsigned-byte-64-unscaled lap:ldr   lap:str   1 ir:box-unsigned-byte-64-instruction ir:unbox-unsigned-byte-64-instruction)

(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-8-unscaled    lap:ldrsb   lap:strb  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-16-unscaled   lap:ldrsh   lap:strh  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-32-unscaled   lap:ldrsw   lap:strw  1 ir:box-fixnum-instruction ir:unbox-fixnum-instruction)
(define-object-ref-integer-accessor sys.int::%%object-ref-signed-byte-64-unscaled   lap:ldr     lap:str   1 ir:box-signed-byte-64-instruction ir:unbox-signed-byte-64-instruction)

(define-builtin sys.int::%object-ref-t ((object index) result)
  (with-builtin-object-access (ea ea-inputs object index 8)
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list result ea)
                         :inputs ea-inputs
                         :outputs (list result)))))

(define-builtin (setf sys.int::%object-ref-t) ((value object index) result)
  (with-builtin-object-access (ea ea-inputs object index 8)
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:str
                         :operands (list value ea)
                         :inputs (list* value ea-inputs)
                         :outputs (list))))
  (emit (make-instance 'ir:move-instruction
                       :source value
                       :destination result)))

(define-builtin sys.int::%object-header-data ((object) result)
  (let ((temp1 (make-instance 'ir:virtual-register))
        (temp2 (make-instance 'ir:virtual-register)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldr
                         :operands (list temp1 `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp1)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp2 temp1 (lognot (1- (ash 1 sys.int::+object-data-shift+))))
                         :inputs (list temp1)
                         :outputs (list temp2)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list result :xzr temp2 :lsr (- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+))
                         :inputs (list temp2)
                         :outputs (list result)))))

(define-builtin (setf sys.int::%object-header-data) ((value object) result)
  (let ((temp1 (make-instance 'ir:virtual-register :kind :integer))
        (temp2 (make-instance 'ir:virtual-register :kind :integer))
        (object-tag (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list temp1 :xzr value :lsl (- sys.int::+object-data-shift+
                                                                  sys.int::+n-fixnum-bits+))
                         :inputs (list value)
                         :outputs (list temp1)))
    ;; low 8 bits of the header only.
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:ldrb
                         :operands (list object-tag `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list object-tag)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:orr
                         :operands (list temp2 temp1 object-tag)
                         :inputs (list temp1 object-tag)
                         :outputs (list temp2)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:str
                         :operands (list temp2 `(:object ,object -1))
                         :inputs (list temp2 object)
                         :outputs (list)))
    (emit (make-instance 'ir:move-instruction
                         :destination result
                         :source value))))

(define-builtin sys.int::lisp-object-address ((value) result)
  (emit (make-instance 'arm64-instruction
                       :opcode 'lap:add
                       :operands (list result value value)
                       :inputs (list value)
                       :outputs (list result))))

(define-builtin sys.int::%%assemble-value ((address tag) result)
  (let ((address-unboxed (make-instance 'ir:virtual-register :kind :integer))
        (tag-unboxed (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source address
                         :destination address-unboxed))
    (emit (make-instance 'ir:unbox-fixnum-instruction
                         :source tag
                         :destination tag-unboxed))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:orr
                         :operands (list result address-unboxed tag-unboxed)
                         :inputs (list address-unboxed tag-unboxed)
                         :outputs (list result)))))

(define-builtin sys.int::%pointer-field ((value) result)
  (let ((temp (make-instance 'ir:virtual-register)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list temp value -16)
                         :inputs (list value)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list result :xzr temp :asr (- (byte-size sys.int::+tag-field+)
                                                                  sys.int::+n-fixnum-bits+))
                         :inputs (list temp)
                         :outputs (list result)))))

(define-builtin sys.int::%tag-field ((value) result)
  (let ((temp (make-instance 'ir:virtual-register)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:add
                         :operands (list temp :xzr value :lsl sys.int::+n-fixnum-bits+)
                         :inputs (list value)
                         :outputs (list temp)))
    (emit (make-instance 'arm64-instruction
                         :opcode 'lap:and
                         :operands (list result temp (ash (1- (ash 1 (byte-size sys.int::+tag-field+)))
                                                          sys.int::+n-fixnum-bits+))
                         :inputs (list temp)
                         :outputs (list result)))))

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
  (let ((new-value (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:move-instruction
                         :source object
                         :destination :x1))
    (emit (make-instance 'arm64-atomic-instruction
                         :opcode 'lap:add
                         :new-value new-value
                         :old-value result
                         :index offset
                         :rhs delta))))

;; Set the value in SLOT to NEW, and return the old value.
;; (defun xchg (object slot new)
;;   (prog1 (%object-ref-t object slot)
;;     (setf (%object-ref-t object slot) new)))
(define-builtin sys.int::%xchg-object ((object offset new) result)
  (let ((new-value (make-instance 'ir:virtual-register :kind :integer)))
    (emit (make-instance 'ir:move-instruction
                         :source object
                         :destination :x1))
    (emit (make-instance 'arm64-atomic-instruction
                         :opcode nil
                         :new-value new-value
                         :old-value result
                         :index offset
                         :rhs new))))

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
(define-builtin sys.int::%cas-object ((object offset old new) (result slot-value))
  (emit (make-instance 'ir:move-instruction
                       :source object
                       :destination :x1))
  (emit (make-instance 'arm64-cas-instruction
                       :new-value new
                       :old-value old
                       :result result
                       :current-value slot-value
                       :index offset)))
