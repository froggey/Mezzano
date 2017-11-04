;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.x86-64)

(defclass x86-instruction (mezzano.compiler.backend::backend-instruction)
  ((%inputs :initarg :inputs :reader mezzano.compiler.backend::instruction-inputs)
   (%outputs :initarg :outputs :reader mezzano.compiler.backend::instruction-outputs)
   (%opcode :initarg :opcode :reader x86-instruction-opcode)
   (%operands :initarg :operands :reader x86-instruction-operands)
   (%clobbers :initarg :clobbers :reader x86-instruction-clobbers))
  (:default-initargs :clobbers '()))

(defmethod mezzano.compiler.backend.register-allocator::instruction-clobbers ((instruction x86-instruction) (architecture (eql :x86-64)))
  (x86-instruction-clobbers instruction))

(defmethod mezzano.compiler.backend::replace-all-registers ((instruction x86-instruction) substitution-function)
  (setf (slot-value instruction '%inputs) (mapcar substitution-function (slot-value instruction '%inputs)))
  (setf (slot-value instruction '%outputs) (mapcar substitution-function (slot-value instruction '%outputs)))
  (setf (slot-value instruction '%operands)
        (loop
           for operand in (slot-value instruction '%operands)
           collect (cond ((typep operand 'virtual-register)
                          (funcall substitution-function operand))
                         ((and (consp operand)
                               (not (member (first operand) '(:constant :function))))
                          (mapcar substitution-function operand))
                         (t operand)))))

(defmethod mezzano.compiler.backend::print-instruction ((instruction x86-instruction))
  (format t "   ~S~%"
          `(:x86 ,(x86-instruction-opcode instruction) ,(x86-instruction-operands instruction))))

(defclass x86-fake-three-operand-instruction (mezzano.compiler.backend::backend-instruction)
  ((%opcode :initarg :opcode :reader x86-instruction-opcode)
   (%result :initarg :result :accessor x86-fake-three-operand-result)
   (%lhs :initarg :lhs :accessor x86-fake-three-operand-lhs)
   (%rhs :initarg :rhs :accessor x86-fake-three-operand-rhs)
   (%clobbers :initarg :clobbers :reader x86-instruction-clobbers))
  (:default-initargs :clobbers '()))

(defmethod mezzano.compiler.backend.register-allocator::instruction-clobbers ((instruction x86-fake-three-operand-instruction) (architecture (eql :x86-64)))
  (x86-instruction-clobbers instruction))

(defmethod mezzano.compiler.backend::instruction-inputs ((instruction x86-fake-three-operand-instruction))
  (list (x86-fake-three-operand-lhs instruction)
        (x86-fake-three-operand-rhs instruction)))

(defmethod mezzano.compiler.backend::instruction-outputs ((instruction x86-fake-three-operand-instruction))
  (list (x86-fake-three-operand-result instruction)))

(defmethod mezzano.compiler.backend::replace-all-registers ((instruction x86-fake-three-operand-instruction) substitution-function)
  (setf (x86-fake-three-operand-result instruction) (funcall substitution-function (x86-fake-three-operand-result instruction)))
  (setf (x86-fake-three-operand-lhs instruction) (funcall substitution-function (x86-fake-three-operand-lhs instruction)))
  (setf (x86-fake-three-operand-rhs instruction) (funcall substitution-function (x86-fake-three-operand-rhs instruction))))

(defmethod mezzano.compiler.backend::print-instruction ((instruction x86-fake-three-operand-instruction))
  (format t "   ~S~%"
          `(:x86-fake-three-operand ,(x86-instruction-opcode instruction)
                                    ,(x86-fake-three-operand-result instruction)
                                    ,(x86-fake-three-operand-lhs instruction)
                                    ,(x86-fake-three-operand-rhs instruction))))

(defclass x86-branch-instruction (mezzano.compiler.backend::terminator-instruction)
  ((%opcode :initarg :opcode :accessor x86-instruction-opcode)
   (%target :initarg :target :accessor x86-branch-target)))

(defmethod mezzano.compiler.backend::successors (function (instruction x86-branch-instruction))
  (list (next-instruction function instruction)
        (x86-branch-target instruction)))

(defmethod mezzano.compiler.backend::instruction-inputs ((instruction x86-branch-instruction))
  '())

(defmethod mezzano.compiler.backend::instruction-outputs ((instruction x86-branch-instruction))
  '())

(defmethod mezzano.compiler.backend::replace-all-registers ((instruction x86-branch-instruction) substitution-function)
  )

(defmethod mezzano.compiler.backend::print-instruction ((instruction x86-branch-instruction))
  (format t "   ~S~%"
          `(:x86-branch ,(x86-instruction-opcode instruction) ,(x86-branch-target instruction))))

(defclass box-mmx-vector-instruction (box-instruction)
  ())

(defmethod box-type ((instruction box-mmx-vector-instruction))
  'mezzano.simd:mmx-vector)

(defmethod mezzano.compiler.backend::print-instruction ((instruction box-mmx-vector-instruction))
  (format t "   ~S~%"
          `(:box-mmx-vector
            ,(box-destination instruction)
            ,(box-source instruction))))

(defclass unbox-mmx-vector-instruction (unbox-instruction)
  ())

(defmethod box-type ((instruction unbox-mmx-vector-instruction))
  'mezzano.simd:mmx-vector)

(defmethod mezzano.compiler.backend::print-instruction ((instruction unbox-mmx-vector-instruction))
  (format t "   ~S~%"
          `(:unbox-mmx-vector
            ,(unbox-destination instruction)
            ,(unbox-source instruction))))

(defun resolve-constant (register defs)
  (let ((register-defs (gethash register defs)))
    (cond ((and register-defs
                (typep (first register-defs) 'constant-instruction)
                (endp (rest register-defs)))
           (values (constant-value (first register-defs)) t))
          (t
           (values nil nil)))))

(defun maybe-constant-operand (operand defs)
  (multiple-value-bind (value validp)
      (resolve-constant operand defs)
    (cond (validp
           (cond ((member value '(nil t))
                  value)
                 ((and (sys.c::fixnump value)
                       (typep (mezzano.compiler.codegen.x86-64::fixnum-to-raw value)
                              '(signed-byte 32)))
                  (mezzano.compiler.codegen.x86-64::fixnum-to-raw value))
                 ((characterp value)
                  (mezzano.compiler.codegen.x86-64::character-to-raw value))
                 (t
                  `(:constant ,value))))
          (t
           operand))))

(defun consumed-by-p (definition consumer uses defs)
  "Return true if all DEFINITION's outputs are only used by CONSUMER."
  (dolist (out (mezzano.compiler.backend::instruction-outputs definition)
           t)
    (when (typep out 'virtual-register)
      (let ((out-defs (gethash out defs))
            (out-uses (gethash out uses)))
        ;(format t "Out: ~S  defs: ~S  uses: ~S~%" out out-defs out-uses)
        ;; Must have one definition.
        (when (not (and out-defs
                        (eql (first out-defs) definition)
                        (endp (rest out-defs))))
          (return nil))
        ;; Must be used only by the consumer.
        (when (or (endp out-uses)
                  (not (endp (rest out-uses)))
                  (not (eql (first out-uses) consumer)))
          (return nil))))))

(defmacro define-builtin (name (lambda-list results &key early) &body body)
  (when (not (listp results))
    (setf results (list results)))
  (let ((backend-function (gensym))
        (insertion-point (gensym))
        (the-block (gensym))
        (real-lambda-list (loop
                             for arg in lambda-list
                             collect (if (symbolp arg)
                                         arg
                                         (gensym))))
        (defs (gensym)))
    (loop
       for arg in lambda-list
       for real-arg in real-lambda-list
       when (consp arg)
       do
         (assert (eql (first arg) :constant))
         (destructuring-bind (name &optional (predicate t))
             (rest arg)
           (setf body `((let ((,name (let ((arg-defs (gethash ,real-arg ,defs)))
                                       (cond ((and arg-defs
                                                   (endp (rest arg-defs))
                                                   (typep (first arg-defs) 'constant-instruction))
                                              (constant-value (first arg-defs)))
                                             (t (give-up))))))
                          (when (not ,predicate)
                            (give-up))
                          ,@body)))))
    `(%defbuiltin ',name
                  ',real-lambda-list
                  ',results
                  (lambda (,backend-function ,insertion-point ,defs ,@real-lambda-list ,@(remove-if #'keywordp results))
                    (declare (ignorable ,defs ,@real-lambda-list ,@(remove-if #'keywordp results)))
                    (block ,the-block
                      (flet ((emit (inst)
                               (mezzano.compiler.backend::insert-before ,backend-function ,insertion-point inst))
                             (give-up ()
                               (return-from ,the-block nil)))
                        (declare (ignorable #'emit #'give-up))
                        ,@body
                        t)))
                  ',early)))

(defclass builtin ()
  ((%name :initarg :name :reader builtin-name)
   (%earlyp :initarg :earlyp :reader builtin-earlyp)
   (%lambda-list :initarg :lambda-list :reader builtin-lambda-list)
   (%result-list :initarg :result-list :reader builtin-result-list)
   (%generator :initarg :generator :reader builtin-generator))
  (:default-initargs :earlyp nil))

(defvar *builtins* (make-hash-table :test 'equal))

(defun %defbuiltin (name lambda-list result-list generator early)
  (setf (gethash name *builtins*)
        (make-instance 'builtin
                       :name name
                       :lambda-list lambda-list
                       :result-list result-list
                       :generator generator
                       :earlyp early))
  name)

(define-builtin mezzano.runtime::%car ((cons) result :early t)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list result `(:car ,cons))
                       :inputs (list cons)
                       :outputs (list result))))

(define-builtin mezzano.runtime::%cdr ((cons) result :early t)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list result `(:cdr ,cons))
                       :inputs (list cons)
                       :outputs (list result))))

(define-builtin (setf mezzano.runtime::%car) ((value cons) result :early t)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list `(:car ,cons) value)
                       :inputs (list cons value)
                       :outputs '()))
  (emit (make-instance 'move-instruction
                       :destination result
                       :source value)))

(define-builtin (setf mezzano.runtime::%cdr) ((value cons) result :early t)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov64
                       :operands (list `(:cdr ,cons) value)
                       :inputs (list cons value)
                       :outputs '()))
  (emit (make-instance 'move-instruction
                       :destination result
                       :source value)))

(define-builtin sys.int::read-frame-pointer (() result :early t)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:lea64
                       :operands (list result `((:rbp ,(ash 1 sys.int::+n-fixnum-bits+))))
                       :inputs (list)
                       :outputs (list result))))

(define-builtin sys.int::fixnump ((object) :z :early t)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:test64
                       :operands (list object sys.int::+fixnum-tag-mask+)
                       :inputs (list object)
                       :outputs '())))

(define-builtin sys.int::%value-has-tag-p ((object (:constant tag (typep tag '(unsigned-byte 4)))) :z :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
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

(define-builtin mezzano.runtime::%%object-of-type-p ((object (:constant object-tag (typep object-tag '(unsigned-byte 6)))) :e)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:and8
                       :operands (list :al (ash (1- (ash 1 sys.int::+object-type-size+))
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp8
                       :operands (list :al (ash object-tag sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs '())))

(define-builtin mezzano.runtime::%functionp ((object) :be)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:sub8
                       :operands (list :al (ash sys.int::+first-function-object-tag+
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:and8
                       :operands (list :al (ash (1- (ash 1 sys.int::+object-type-size+))
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp8
                       :operands (list :al (ash (- sys.int::+last-function-object-tag+
                                                   sys.int::+first-function-object-tag+)
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs '())))

(define-builtin mezzano.runtime::%%simple-1d-array-p ((object) :be)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:and8
                       :operands (list :al (ash (1- (ash 1 sys.int::+object-type-size+))
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp8
                       :operands (list :al (ash sys.int::+last-simple-1d-array-object-tag+
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs '())))

(define-builtin mezzano.runtime::%%arrayp ((object) :be)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:and8
                       :operands (list :al (ash (1- (ash 1 sys.int::+object-type-size+))
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp8
                       :operands (list :al (ash sys.int::+last-complex-array-object-tag+
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs '())))

(define-builtin mezzano.runtime::%%complex-array-p ((object) :be)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:mov8
                       :operands (list :al `(:object ,object -1))
                       :inputs (list object)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:sub8
                       :operands (list :al (ash sys.int::+first-complex-array-object-tag+
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:and8
                       :operands (list :al (ash (1- (ash 1 sys.int::+object-type-size+))
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs (list :rax)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp8
                       :operands (list :al (ash (- sys.int::+last-complex-array-object-tag+
                                                   sys.int::+first-complex-array-object-tag+)
                                                sys.int::+object-type-shift+))
                       :inputs (list :rax)
                       :outputs '())))

(define-builtin sys.int::%unbound-value-p ((object) :e :early t)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp64
                       :operands (list object :unbound-value)
                       :inputs (list object)
                       :outputs (list))))

(define-builtin sys.int::%undefined-function-p ((object) :e :early t)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp64
                       :operands (list object :undefined-function)
                       :inputs (list object)
                       :outputs (list))))

(define-builtin mezzano.runtime::%fixnum-+ ((lhs rhs) result)
  (let ((out (make-instance 'label)))
    (emit (make-instance 'move-instruction
                         :destination result
                         :source lhs))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:add64
                         :operands (list result rhs)
                         :inputs (list result rhs)
                         :outputs (list result)))
    (emit (make-instance 'x86-branch-instruction
                         :opcode 'lap:jno
                         :target out))
    ;; Build a bignum on overflow.
    ;; Recover the full value using the carry bit.
    (emit (make-instance 'label :name :+-overflow))
    (emit (make-instance 'move-instruction
                         :source result
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:rcr64
                         :operands (list :rax 1)
                         :inputs (list :rax)
                         :outputs (list :rax)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list :r13 '(:function sys.int::%%make-bignum-64-rax))
                         :inputs '()
                         :outputs (list :r13)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:call
                         :operands (list `(:object :r13 ,sys.int::+fref-entry-point+))
                         :inputs '(:r13 :rax)
                         :outputs (list :r8)
                         :clobbers '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
                                     :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
                                     :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
                                     :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15)))
    (emit (make-instance 'move-instruction
                         :destination result
                         :source :r8))
    (emit (make-instance 'jump-instruction :target out))
    (emit out)))

(define-builtin mezzano.compiler::%fast-fixnum-+ ((lhs rhs) result :early t)
  (emit (make-instance 'x86-fake-three-operand-instruction
                       :opcode 'lap:add64
                       :result result
                       :lhs lhs
                       :rhs rhs)))

(define-builtin mezzano.runtime::%fixnum-- ((lhs rhs) result)
  (let ((out (make-instance 'label)))
    (emit (make-instance 'move-instruction
                         :destination result
                         :source lhs))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:sub64
                         :operands (list result rhs)
                         :inputs (list result rhs)
                         :outputs (list result)))
    (emit (make-instance 'x86-branch-instruction
                         :opcode 'lap:jno
                         :target out))
    ;; Build a bignum on overflow.
    ;; Recover the full value using the carry bit.
    (emit (make-instance 'label :name :--overflow))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cmc
                         :operands (list)
                         :inputs (list)
                         :outputs (list)))
    (emit (make-instance 'move-instruction
                         :source result
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:rcr64
                         :operands (list :rax 1)
                         :inputs (list :rax)
                         :outputs (list :rax)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list :r13 '(:function sys.int::%%make-bignum-64-rax))
                         :inputs '()
                         :outputs (list :r13)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:call
                         :operands (list `(:object :r13 ,sys.int::+fref-entry-point+))
                         :inputs '(:r13 :rax)
                         :outputs (list :r8)
                         :clobbers '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
                                     :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
                                     :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
                                     :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15)))
    (emit (make-instance 'move-instruction
                         :destination result
                         :source :r8))
    (emit (make-instance 'jump-instruction :target out))
    (emit out)))

(define-builtin mezzano.compiler::%fast-fixnum-- ((lhs rhs) result :early t)
  (emit (make-instance 'x86-fake-three-operand-instruction
                       :opcode 'lap:sub64
                       :result result
                       :lhs lhs
                       :rhs rhs)))

(define-builtin mezzano.runtime::%fixnum-* ((lhs rhs) result)
  (let ((fixnum-result (make-instance 'label))
        (out (make-instance 'label)))
    ;; Convert the lhs to a raw integer, leaving the rhs as a fixnum.
    ;; This will cause the result to be a fixnum.
    (emit (make-instance 'move-instruction
                         :source lhs
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:sar64
                         :operands (list :rax sys.int::+n-fixnum-bits+)
                         :inputs (list :rax)
                         :outputs (list :rax)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:imul64
                         :operands (list rhs)
                         :inputs (list :rax rhs)
                         :outputs (list :rax :rdx)))
    (emit (make-instance 'x86-branch-instruction
                         :opcode 'lap:jno
                         :target fixnum-result))
    ;; Build a bignum on overflow.
    ;; 128-bit result in rdx:rax.
    ;; Unbox the result.
    (emit (make-instance 'label :name :*-overflow))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:shrd64
                         :operands (list :rax :rdx sys.int::+n-fixnum-bits+)
                         :inputs (list :rax :rdx)
                         :outputs (list :rax)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:sar64
                         :operands (list :rdx sys.int::+n-fixnum-bits+)
                         :inputs (list :rdx)
                         :outputs (list :rdx)))
    ;; Check if the result will fit in 64 bits.
    ;; Save the high bits.
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list :rcx :rdx)
                         :inputs (list :rdx)
                         :outputs (list :rcx)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cqo
                         :operands (list)
                         :inputs (list :rax)
                         :outputs (list :rdx)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cmp64
                         :operands (list :rcx :rdx)
                         :inputs (list :rcx :rdx)
                         :outputs (list)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list :rdx :rcx)
                         :inputs (list :rcx)
                         :outputs (list :rdx)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list :r13 '(:function sys.int::%%make-bignum-128-rdx-rax))
                         :inputs (list)
                         :outputs (list :r13)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cmov64e
                         :operands (list :r13 '(:function sys.int::%%make-bignum-64-rax))
                         :inputs (list :r13)
                         :outputs (list :r13)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:call
                         :operands (list `(:object :r13 ,sys.int::+fref-entry-point+))
                         :inputs (list :r13 :rax :rdx)
                         :outputs (list :r8)
                         :clobbers '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
                                     :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
                                     :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
                                     :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15)))
    (emit (make-instance 'move-instruction
                         :source :r8
                         :destination result))
    (emit (make-instance 'jump-instruction
                         :target out))
    (emit fixnum-result)
    (emit (make-instance 'move-instruction
                         :source :rax
                         :destination result))
    (emit (make-instance 'jump-instruction :target out))
    (emit out)))

(define-builtin mezzano.runtime::%fixnum-truncate ((lhs rhs) (quot rem))
  (emit (make-instance 'move-instruction
                       :source lhs
                       :destination :rax))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cqo
                       :operands (list)
                       :inputs (list :rax)
                       :outputs (list :rdx)))
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:idiv64
                       :operands (list rhs)
                       :inputs (list :rax :rdx rhs)
                       :outputs (list :rax :rdx)))
  ;; :rax holds the dividend as a integer.
  ;; :rdx holds the remainder as a fixnum.
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:lea64
                       :operands (list quot `((:rax 2)))
                       :inputs (list :rax)
                       :outputs (list quot)))
  (emit (make-instance 'move-instruction
                       :source :rdx
                       :destination rem)))

(define-builtin mezzano.runtime::%fixnum-logand ((lhs rhs) result :early t)
  (emit (make-instance 'x86-fake-three-operand-instruction
                       :opcode 'lap:and64
                       :result result
                       :lhs lhs
                       :rhs rhs)))

(define-builtin sys.c::%fast-fixnum-logand ((lhs rhs) result :early t)
  (emit (make-instance 'x86-fake-three-operand-instruction
                       :opcode 'lap:and64
                       :result result
                       :lhs lhs
                       :rhs rhs)))

(define-builtin mezzano.runtime::%fixnum-logior ((lhs rhs) result :early t)
  (emit (make-instance 'x86-fake-three-operand-instruction
                       :opcode 'lap:or64
                       :result result
                       :lhs lhs
                       :rhs rhs)))

(define-builtin mezzano.runtime::%fixnum-logxor ((lhs rhs) result :early t)
  (emit (make-instance 'x86-fake-three-operand-instruction
                       :opcode 'lap:xor64
                       :result result
                       :lhs lhs
                       :rhs rhs)))

(define-builtin eq ((lhs rhs) :e :early t)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp64
                       :operands (list lhs rhs)
                       :inputs (list lhs rhs)
                       :outputs '())))

(define-builtin sys.int::%object-ref-t ((object index) result :early t)
  (emit (make-instance 'object-get-t-instruction
                       :destination result
                       :object object
                       :index index)))

(define-builtin (setf sys.int::%object-ref-t) ((value object index) result :early t)
  (emit (make-instance 'object-set-t-instruction
                       :value value
                       :object object
                       :index index))
  (emit (make-instance 'move-instruction
                       :source value
                       :destination result)))

(define-builtin sys.int::%object-header-data ((object) result)
  (let ((temp (make-instance 'virtual-register)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp `(:object ,object -1))
                         :inputs (list object)
                         :outputs (list temp)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:and64
                         :operands (list temp (lognot (1- (ash 1 sys.int::+object-data-shift+))))
                         :inputs (list temp)
                         :outputs (list temp)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:shr64
                         :operands (list temp (- sys.int::+object-data-shift+
                                                 sys.int::+n-fixnum-bits+))
                         :inputs (list temp)
                         :outputs (list temp)))
    (emit (make-instance 'move-instruction
                         :source temp
                         :destination result))))

(define-builtin sys.int::%%object-ref-unsigned-byte-32 ((object index) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov32
                         :operands (list :eax `(:object ,object 0 ,index 2))
                         :inputs (list object index)
                         :outputs (list :rax)
                         :clobbers '(:rax)))
    (emit (make-instance 'move-instruction
                         :source :rax
                         :destination temp))
    (emit (make-instance 'box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-32) ((value object index) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    ;; Need to use :eax and a temporary here because it's currently impossible
    ;; to replace vregs with non-64-bit gprs.
    ;; Using a temporary & a move before the box allows the box to safely
    ;; eliminated.
    (emit (make-instance 'unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'move-instruction
                         :source temp
                         :destination :rax))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov32
                         :operands (list `(:object ,object 0 ,index 2) :eax)
                         :inputs (list object index :rax)
                         :outputs (list)))
    (emit (make-instance 'move-instruction
                         :source value
                         :destination result))))

(define-builtin sys.int::%%object-ref-unsigned-byte-64 ((object index) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list temp `(:object ,object 0 ,index 4))
                         :inputs (list object index)
                         :outputs (list temp)))
    (emit (make-instance 'box-unsigned-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin (setf sys.int::%%object-ref-unsigned-byte-64) ((value object index) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:mov64
                         :operands (list `(:object ,object 0 ,index 4) temp)
                         :inputs (list object index temp)
                         :outputs (list)))
    (emit (make-instance 'move-instruction
                         :source value
                         :destination result))))

(define-builtin mezzano.runtime::%fixnum-< ((lhs rhs) :l :early t)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:cmp64
                       :operands (list lhs rhs)
                       :inputs (list lhs rhs)
                       :outputs '())))

;;; SINGLE-FLOAT operations.

(define-builtin sys.int::%single-float-as-integer ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-single-float-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-fixnum-instruction
                         :source temp
                         :destination result))))

(define-builtin sys.int::%integer-as-single-float ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-single-float-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.runtime::%%coerce-fixnum-to-single-float ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer))
        (result-unboxed (make-instance 'virtual-register :kind :single-float)))
    (emit (make-instance 'unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvtsi2ss64
                         :operands (list result-unboxed temp)
                         :inputs (list temp)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'box-single-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin mezzano.runtime::%%coerce-double-float-to-single-float ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :double-float))
        (result-unboxed (make-instance 'virtual-register :kind :single-float)))
    (emit (make-instance 'unbox-double-float-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvtsd2ss64
                         :operands (list result-unboxed temp)
                         :inputs (list temp)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'box-single-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin sys.int::%%single-float-< ((lhs rhs) :b :early t)
  (let ((lhs-unboxed (make-instance 'virtual-register :kind :single-float))
        (rhs-unboxed (make-instance 'virtual-register :kind :single-float)))
    (emit (make-instance 'unbox-single-float-instruction
                         :source lhs
                         :destination lhs-unboxed))
    (emit (make-instance 'unbox-single-float-instruction
                         :source rhs
                         :destination rhs-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:ucomiss
                         :operands (list lhs-unboxed rhs-unboxed)
                         :inputs (list lhs-unboxed rhs-unboxed)
                         :outputs '()))))

;; TODO: This needs to check two conditions (P & NE), which the
;; compiler can't currently do efficiently.
(define-builtin sys.int::%%single-float-= ((lhs rhs) result :early t)
  (let ((lhs-unboxed (make-instance 'virtual-register :kind :single-float))
        (rhs-unboxed (make-instance 'virtual-register :kind :single-float))
        (temp-result1 (make-instance 'virtual-register))
        (temp-result2 (make-instance 'virtual-register)))
    (emit (make-instance 'unbox-single-float-instruction
                         :source lhs
                         :destination lhs-unboxed))
    (emit (make-instance 'unbox-single-float-instruction
                         :source rhs
                         :destination rhs-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:ucomiss
                         :operands (list lhs-unboxed rhs-unboxed)
                         :inputs (list lhs-unboxed rhs-unboxed)
                         :outputs '()))
    (emit (make-instance 'constant-instruction
                         :destination temp-result1
                         :value t))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:cmov64p
                         :result temp-result2
                         :lhs temp-result1
                         :rhs `(:constant nil)))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:cmov64ne
                         :result result
                         :lhs temp-result2
                         :rhs `(:constant nil)))))

(define-builtin sys.int::%%truncate-single-float ((value) result :early t)
  (let ((value-unboxed (make-instance 'virtual-register :kind :single-float))
        (result-unboxed (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-single-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvttss2si64
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(macrolet ((frob (name instruction)
             `(define-builtin ,name ((lhs rhs) result :early t)
                (let ((lhs-unboxed (make-instance 'virtual-register :kind :single-float))
                      (rhs-unboxed (make-instance 'virtual-register :kind :single-float))
                      (result-unboxed (make-instance 'virtual-register :kind :single-float)))
                  (emit (make-instance 'unbox-single-float-instruction
                                       :source lhs
                                       :destination lhs-unboxed))
                  (emit (make-instance 'unbox-single-float-instruction
                                       :source rhs
                                       :destination rhs-unboxed))
                  (emit (make-instance 'x86-fake-three-operand-instruction
                                       :opcode ',instruction
                                       :result result-unboxed
                                       :lhs lhs-unboxed
                                       :rhs rhs-unboxed))
                  (emit (make-instance 'box-single-float-instruction
                                       :source result-unboxed
                                       :destination result))))))
  (frob sys.int::%%single-float-/ lap:divss)
  (frob sys.int::%%single-float-+ lap:addss)
  (frob sys.int::%%single-float-- lap:subss)
  (frob sys.int::%%single-float-* lap:mulss))

(define-builtin sys.int::%%single-float-sqrt ((value) result :early t)
  (let ((value-unboxed (make-instance 'virtual-register :kind :single-float))
        (result-unboxed (make-instance 'virtual-register :kind :single-float)))
    (emit (make-instance 'unbox-single-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:sqrtss
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'box-single-float-instruction
                         :source result-unboxed
                         :destination result))))

;;; DOUBLE-FLOAT operations.

(define-builtin sys.int::%double-float-as-integer ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-double-float-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-unsigned-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin sys.int::%integer-as-double-float ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-double-float-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.runtime::%%coerce-fixnum-to-double-float ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer))
        (result-unboxed (make-instance 'virtual-register :kind :double-float)))
    (emit (make-instance 'unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvtsi2sd64
                         :operands (list result-unboxed temp)
                         :inputs (list temp)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'box-double-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin mezzano.runtime::%%coerce-single-float-to-double-float ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :single-float))
        (result-unboxed (make-instance 'virtual-register :kind :double-float)))
    (emit (make-instance 'unbox-single-float-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvtss2sd64
                         :operands (list result-unboxed temp)
                         :inputs (list temp)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'box-double-float-instruction
                         :source result-unboxed
                         :destination result))))

(define-builtin sys.int::%%double-float-< ((lhs rhs) :b :early t)
  (let ((lhs-unboxed (make-instance 'virtual-register :kind :double-float))
        (rhs-unboxed (make-instance 'virtual-register :kind :double-float)))
    (emit (make-instance 'unbox-double-float-instruction
                         :source lhs
                         :destination lhs-unboxed))
    (emit (make-instance 'unbox-double-float-instruction
                         :source rhs
                         :destination rhs-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:ucomisd
                         :operands (list lhs-unboxed rhs-unboxed)
                         :inputs (list lhs-unboxed rhs-unboxed)
                         :outputs '()))))

;; TODO: This needs to check two conditions (P & NE), which the
;; compiler can't currently do efficiently.
(define-builtin sys.int::%%double-float-= ((lhs rhs) result :early t)
  (let ((lhs-unboxed (make-instance 'virtual-register :kind :double-float))
        (rhs-unboxed (make-instance 'virtual-register :kind :double-float))
        (temp-result1 (make-instance 'virtual-register))
        (temp-result2 (make-instance 'virtual-register)))
    (emit (make-instance 'unbox-double-float-instruction
                         :source lhs
                         :destination lhs-unboxed))
    (emit (make-instance 'unbox-double-float-instruction
                         :source rhs
                         :destination rhs-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:ucomisd
                         :operands (list lhs-unboxed rhs-unboxed)
                         :inputs (list lhs-unboxed rhs-unboxed)
                         :outputs '()))
    (emit (make-instance 'constant-instruction
                         :destination temp-result1
                         :value t))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:cmov64p
                         :result temp-result2
                         :lhs temp-result1
                         :rhs `(:constant nil)))
    (emit (make-instance 'x86-fake-three-operand-instruction
                         :opcode 'lap:cmov64ne
                         :result result
                         :lhs temp-result2
                         :rhs `(:constant nil)))))

(define-builtin sys.int::%%truncate-double-float ((value) result :early t)
  (let ((value-unboxed (make-instance 'virtual-register :kind :double-float))
        (result-unboxed (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-double-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:cvttsd2si64
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'box-fixnum-instruction
                         :source result-unboxed
                         :destination result))))

(macrolet ((frob (name instruction)
             `(define-builtin ,name ((lhs rhs) result :early t)
                (let ((lhs-unboxed (make-instance 'virtual-register :kind :double-float))
                      (rhs-unboxed (make-instance 'virtual-register :kind :double-float))
                      (result-unboxed (make-instance 'virtual-register :kind :double-float)))
                  (emit (make-instance 'unbox-double-float-instruction
                                       :source lhs
                                       :destination lhs-unboxed))
                  (emit (make-instance 'unbox-double-float-instruction
                                       :source rhs
                                       :destination rhs-unboxed))
                  (emit (make-instance 'x86-fake-three-operand-instruction
                                       :opcode ',instruction
                                       :result result-unboxed
                                       :lhs lhs-unboxed
                                       :rhs rhs-unboxed))
                  (emit (make-instance 'box-double-float-instruction
                                       :source result-unboxed
                                       :destination result))))))
  (frob sys.int::%%double-float-/ lap:divsd)
  (frob sys.int::%%double-float-+ lap:addsd)
  (frob sys.int::%%double-float-- lap:subsd)
  (frob sys.int::%%double-float-* lap:mulsd))

(define-builtin sys.int::%%double-float-sqrt ((value) result :early t)
  (let ((value-unboxed (make-instance 'virtual-register :kind :double-float))
        (result-unboxed (make-instance 'virtual-register :kind :double-float)))
    (emit (make-instance 'unbox-double-float-instruction
                         :source value
                         :destination value-unboxed))
    (emit (make-instance 'x86-instruction
                         :opcode 'lap:sqrtsd
                         :operands (list result-unboxed value-unboxed)
                         :inputs (list value-unboxed)
                         :outputs (list result-unboxed)))
    (emit (make-instance 'box-double-float-instruction
                         :source result-unboxed
                         :destination result))))

;;; MMX operations.

(define-builtin mezzano.simd::%make-mmx-vector ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-unsigned-byte-64-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-mmx-vector-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.simd::%make-mmx-vector/fixnum ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-fixnum-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-mmx-vector-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.simd::%mmx-vector-value ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-mmx-vector-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-unsigned-byte-64-instruction
                         :source temp
                         :destination result))))

(define-builtin mezzano.simd::%mmx-vector-value/fixnum ((value) result :early t)
  (let ((temp (make-instance 'virtual-register :kind :integer)))
    (emit (make-instance 'unbox-mmx-vector-instruction
                         :source value
                         :destination temp))
    (emit (make-instance 'box-fixnum-instruction
                         :source temp
                         :destination result))))

(macrolet ((frob (fn inst)
             `(define-builtin ,fn ((lhs rhs) result :early t)
                (let ((lhs-unboxed (make-instance 'virtual-register :kind :mmx))
                      (rhs-unboxed (make-instance 'virtual-register :kind :mmx))
                      (result-unboxed (make-instance 'virtual-register :kind :mmx)))
                  (emit (make-instance 'unbox-mmx-vector-instruction
                                       :source lhs
                                       :destination lhs-unboxed))
                  (emit (make-instance 'unbox-mmx-vector-instruction
                                       :source rhs
                                       :destination rhs-unboxed))
                  (emit (make-instance 'x86-fake-three-operand-instruction
                                       :opcode ',inst
                                       :result result-unboxed
                                       :lhs lhs-unboxed
                                       :rhs rhs-unboxed))
                  (emit (make-instance 'box-mmx-vector-instruction
                                       :source result-unboxed
                                       :destination result))))))
  ;; MMX
  (frob mezzano.simd::%packssdw/mmx lap:packssdw)
  (frob mezzano.simd::%packsswb/mmx lap:packsswb)
  (frob mezzano.simd::%packuswb/mmx lap:packuswb)
  (frob mezzano.simd::%paddb/mmx lap:paddb)
  (frob mezzano.simd::%paddw/mmx lap:paddw)
  (frob mezzano.simd::%paddd/mmx lap:paddd)
  (frob mezzano.simd::%paddsb/mmx lap:paddsb)
  (frob mezzano.simd::%paddsw/mmx lap:paddsw)
  (frob mezzano.simd::%paddusb/mmx lap:paddusb)
  (frob mezzano.simd::%paddusw/mmx lap:paddusw)
  (frob mezzano.simd::%pand/mmx lap:pand)
  (frob mezzano.simd::%pandn/mmx lap:pandn)
  (frob mezzano.simd::%pcmpeqb/mmx lap:pcmpeqb)
  (frob mezzano.simd::%pcmpeqw/mmx lap:pcmpeqw)
  (frob mezzano.simd::%pcmpeqd/mmx lap:pcmpeqd)
  (frob mezzano.simd::%pcmpgtb/mmx lap:pcmpgtb)
  (frob mezzano.simd::%pcmpgtw/mmx lap:pcmpgtw)
  (frob mezzano.simd::%pcmpgtd/mmx lap:pcmpgtd)
  (frob mezzano.simd::%pmaddwd/mmx lap:pmaddwd)
  (frob mezzano.simd::%pmulhuw/mmx lap:pmulhuw)
  (frob mezzano.simd::%pmulhw/mmx lap:pmulhw)
  (frob mezzano.simd::%pmullw/mmx lap:pmullw)
  (frob mezzano.simd::%por/mmx lap:por)
  (frob mezzano.simd::%psllw/mmx lap:psllw)
  (frob mezzano.simd::%pslld/mmx lap:pslld)
  (frob mezzano.simd::%psllq/mmx lap:psllq)
  (frob mezzano.simd::%psraw/mmx lap:psraw)
  (frob mezzano.simd::%psrad/mmx lap:psrad)
  (frob mezzano.simd::%psrlw/mmx lap:psrlw)
  (frob mezzano.simd::%psrld/mmx lap:psrld)
  (frob mezzano.simd::%psrlq/mmx lap:psrlq)
  (frob mezzano.simd::%psubb/mmx lap:psubb)
  (frob mezzano.simd::%psubw/mmx lap:psubw)
  (frob mezzano.simd::%psubd/mmx lap:psubd)
  (frob mezzano.simd::%psubsb/mmx lap:psubsb)
  (frob mezzano.simd::%psubsw/mmx lap:psubsw)
  (frob mezzano.simd::%psubusb/mmx lap:psubusb)
  (frob mezzano.simd::%psubusw/mmx lap:psubusw)
  (frob mezzano.simd::%punpckhbw/mmx lap:punpckhbw)
  (frob mezzano.simd::%punpckhwd/mmx lap:punpckhwd)
  (frob mezzano.simd::%punpckhdq/mmx lap:punpckhdq)
  (frob mezzano.simd::%punpcklbw/mmx lap:punpcklbw)
  (frob mezzano.simd::%punpcklwd/mmx lap:punpcklwd)
  (frob mezzano.simd::%punpckldq/mmx lap:punpckldq)
  (frob mezzano.simd::%pxor/mmx lap:pxor)

  ;; SSE1
  (frob mezzano.simd::%pavgb/mmx lap:pavgb)
  (frob mezzano.simd::%pavgw/mmx lap:pavgw)
  (frob mezzano.simd::%pmaxsw/mmx lap:pmaxsw)
  (frob mezzano.simd::%pmaxub/mmx lap:pmaxub)
  (frob mezzano.simd::%pminsw/mmx lap:pminsw)
  (frob mezzano.simd::%pminub/mmx lap:pminub)
  (frob mezzano.simd::%psadbw/mmx lap:psadbw)

  ;; SSE2
  (frob mezzano.simd::%paddq/mmx lap:paddq)
  (frob mezzano.simd::%pmuludq/mmx lap:pmuludq)
  (frob mezzano.simd::%psubq/mmx lap:psubq)
  )

(defun lower (backend-function early target)
  (multiple-value-bind (uses defs)
      (mezzano.compiler.backend::build-use/def-maps backend-function)
    (do* ((inst (mezzano.compiler.backend::first-instruction backend-function) next-inst)
          (next-inst (mezzano.compiler.backend::next-instruction backend-function inst) (if inst (mezzano.compiler.backend::next-instruction backend-function inst))))
         ((null inst))
      (cond ((typep inst 'object-get-t-instruction)
             ;; (object-get-t dest obj constant-index) => (mov dest (obj ind))
             (let* ((dest (object-get-destination inst))
                    (obj (object-get-object inst))
                    (index (object-get-index inst)))
               (multiple-value-bind (index-value index-validp)
                   (resolve-constant index defs)
                 (when (and index-validp
                            (typep index-value '(signed-byte 29)))
                   (mezzano.compiler.backend::insert-before
                    backend-function inst
                    (make-instance 'x86-instruction
                                   :opcode 'lap:mov64
                                   :operands (list dest `(:object ,obj ,index-value))
                                   :inputs (list obj)
                                   :outputs (list dest)))
                   (mezzano.compiler.backend::remove-instruction backend-function inst)))))
            ((typep inst 'object-set-t-instruction)
             ;; (object-set-t value obj constant-index) => (mov (obj ind) value)
             (let* ((value (object-set-value inst))
                    (obj (object-set-object inst))
                    (index (object-set-index inst)))
               (multiple-value-bind (index-value index-validp)
                   (resolve-constant index defs)
                 (when (and index-validp
                            (typep index-value '(signed-byte 29)))
                   (mezzano.compiler.backend::insert-before
                    backend-function inst
                    (make-instance 'x86-instruction
                                   :opcode 'lap:mov64
                                   :operands (list `(:object ,obj ,index-value) value)
                                   :inputs (list value obj)
                                   :outputs (list)))
                   (mezzano.compiler.backend::remove-instruction backend-function inst)))))
            ((and (not early)
                  (typep inst 'box-unsigned-byte-64-instruction))
             ;; (box-ub64 value) => (call make-ub64-rax value)
             (let* ((value (box-source inst))
                    (result (box-destination inst)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'move-instruction
                               :destination :rax
                               :source value))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'x86-instruction
                               :opcode 'lap:mov64
                               :operands (list :r13 `(:function mezzano.runtime::%%make-unsigned-byte-64-rax))
                               :inputs (list)
                               :outputs (list :r13)
                               :clobbers '(:r13)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'x86-instruction
                               :opcode 'lap:call
                               :operands (list `(:object :r13 ,sys.int::+fref-entry-point+))
                               :inputs (list :r13 :rax)
                               :outputs (list :r8)
                               :clobbers '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
                                           :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
                                           :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
                                           :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'move-instruction
                               :destination result
                               :source :r8))
               (mezzano.compiler.backend::remove-instruction backend-function inst)))
            ((and (not early)
                  (typep inst 'box-double-float-instruction))
             ;; (box-double value) => (call make-double-rax value)
             (let* ((value (box-source inst))
                    (result (box-destination inst)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'move-instruction
                               :destination :rax
                               :source value))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'x86-instruction
                               :opcode 'lap:mov64
                               :operands (list :r13 `(:function sys.int::%%make-double-float-rax))
                               :inputs (list)
                               :outputs (list :r13)
                               :clobbers '(:r13)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'x86-instruction
                               :opcode 'lap:call
                               :operands (list `(:object :r13 ,sys.int::+fref-entry-point+))
                               :inputs (list :r13 :rax)
                               :outputs (list :r8)
                               :clobbers '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
                                           :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
                                           :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
                                           :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'move-instruction
                               :destination result
                               :source :r8))
               (mezzano.compiler.backend::remove-instruction backend-function inst)))
            ((and (not early)
                  (typep inst 'box-mmx-vector-instruction))
             ;; (box-mmx-vector value) => (call make-mmx-vector-rax value)
             (let* ((vector (box-source inst))
                    (result (box-destination inst)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'move-instruction
                               :destination :rax
                               :source vector))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'x86-instruction
                               :opcode 'lap:mov64
                               :operands (list :r13 `(:function mezzano.simd::%%make-mmx-vector-rax))
                               :inputs (list)
                               :outputs (list :r13)
                               :clobbers '(:r13)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'x86-instruction
                               :opcode 'lap:call
                               :operands (list `(:object :r13 ,sys.int::+fref-entry-point+))
                               :inputs (list :r13 :rax)
                               :outputs (list :r8)
                               :clobbers '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
                                           :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
                                           :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
                                           :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15)))
               (mezzano.compiler.backend::insert-before
                backend-function inst
                (make-instance 'move-instruction
                               :destination result
                               :source :r8))
               (mezzano.compiler.backend::remove-instruction backend-function inst)))
            (t
             (let ((next (or (lower-predicate-builtin backend-function inst uses defs early target)
                             (lower-builtin backend-function inst defs early target))))
               (when next
                 (setf next-inst next))))))))

(defgeneric match-builtin (name n-arguments architecture early))

(defmethod match-builtin (name n-arguments architecture early)
  nil)

(defmethod match-builtin (name n-arguments (architecture sys.c:x86-64-target) early)
  (let ((builtin (gethash name *builtins*)))
    (if (and builtin
             (eql (length (builtin-lambda-list builtin)) n-arguments)
             (if early
                 (builtin-earlyp builtin)
                 t))
        builtin
        nil)))

(defgeneric reify-predicate (predicate output emitter architecture))

(defmethod reify-predicate (predicate result emitter (architecture sys.c:x86-64-target))
  (let ((tmp (make-instance 'virtual-register)))
    (funcall emitter (make-instance 'constant-instruction
                                    :destination tmp
                                    :value nil))
    (funcall emitter (make-instance 'x86-fake-three-operand-instruction
                                    :opcode (mezzano.compiler.codegen.x86-64::predicate-instruction-cmov-instruction
                                             (mezzano.compiler.codegen.x86-64::predicate-info
                                              predicate))
                                    :result result
                                    :lhs tmp
                                    :rhs '(:constant t)))))

;; Lower (branch (call foo ...) target) when FOO produces a predicate result.
(defun lower-predicate-builtin (backend-function inst uses defs early target)
  (let ((next-inst (next-instruction backend-function inst)))
    (when (and (typep inst 'call-instruction)
               (typep next-inst 'branch-instruction)
               (consumed-by-p inst next-inst uses defs))
      (let ((builtin (match-builtin (call-function inst)
                                    (length (call-arguments inst))
                                    target
                                    early)))
        (when (and builtin
                   ;; Predicate result.
                   ;; FIXME: This should work when the result consumed by the branch is a predicate and other results are ignored.
                   (eql (length (builtin-result-list builtin)) 1)
                   (keywordp (first (builtin-result-list builtin))))
          (when (not (apply (builtin-generator builtin)
                            backend-function inst
                            defs
                            (call-arguments inst)))
            (return-from lower-predicate-builtin nil))
          (let ((pred (first (builtin-result-list builtin))))
            (mezzano.compiler.backend::insert-before
             backend-function inst
             (make-instance 'x86-branch-instruction
                            :opcode (mezzano.compiler.codegen.x86-64::predicate-instruction-jump-instruction
                                     (mezzano.compiler.codegen.x86-64::predicate-info
                                      (if (typep next-inst 'branch-true-instruction)
                                          pred
                                          (mezzano.compiler.codegen.x86-64::invert-predicate pred))))
                            :target (branch-target next-inst)))
            (let ((advance (next-instruction backend-function next-inst)))
              (remove-instruction backend-function inst)
              (remove-instruction backend-function next-inst)
              advance)))))))

(defun lower-builtin (backend-function inst defs early target)
  (let ((builtin (and (typep inst '(or
                                    call-instruction
                                    call-multiple-instruction))
                      (match-builtin (call-function inst)
                                     (length (call-arguments inst))
                                     target
                                     early))))
    (when builtin
      (let* ((result-regs (if (typep inst 'call-instruction)
                              (list* (call-result inst)
                                     (loop
                                        for r in (rest (builtin-result-list builtin))
                                        collect (make-instance 'virtual-register)))
                              (loop
                                 for r in (builtin-result-list builtin)
                                 collect (make-instance 'virtual-register))))
             (results (loop
                         for result in (builtin-result-list builtin)
                         for reg in result-regs
                         when (not (keywordp result))
                         collect reg)))
        (when (not (apply (builtin-generator builtin)
                          backend-function inst
                          defs
                          (append (call-arguments inst)
                                  results)))
          (return-from lower-builtin nil))
        (cond ((and result-regs
                    (endp (builtin-result-list builtin)))
               ;; Builtin produces no results, but one value expected.
               (assert (endp (rest result-regs)))
               (when (typep inst 'call-instruction)
                 (mezzano.compiler.backend::insert-before
                  backend-function inst
                  (make-instance 'constant-instruction
                                 :destination (first result-regs)
                                 :value nil))))
              (t
               ;; Convert predicate results to NIL/T.
               (loop
                  for result in (builtin-result-list builtin)
                  for reg in result-regs
                  when (keywordp result)
                  do (reify-predicate result reg
                                      (lambda (new-inst)
                                        (mezzano.compiler.backend::insert-before
                                         backend-function inst new-inst))
                                      target))))
        ;; Fix up multiple values.
        (when (typep inst 'call-multiple-instruction)
          (mezzano.compiler.backend::insert-before
           backend-function inst
           (make-instance 'values-instruction
                          :values (if (endp (builtin-result-list builtin))
                                      '()
                                      result-regs))))
        (let ((advance (next-instruction backend-function inst)))
          (remove-instruction backend-function inst)
          advance)))))

(defun peephole (backend-function)
  (do* ((inst (mezzano.compiler.backend::first-instruction backend-function) next-inst)
        (next-inst (mezzano.compiler.backend::next-instruction backend-function inst) (if inst (mezzano.compiler.backend::next-instruction backend-function inst))))
       ((null inst))
    (cond ((and (typep inst 'move-instruction)
                (eql (move-source inst) (move-destination inst)))
           ;; Delete (move foo foo)
           (mezzano.compiler.backend::remove-instruction backend-function inst))
          #+(or)
          ((and (typep inst 'move-instruction)
                (typep next-inst 'spill-instruction)
                (eql (move-destination inst) (spill-source next-inst)))
           ;; (move t reg) (spill vreg t) => (spill vreg reg)
           (setf (spill-source next-inst) (move-source inst))
           (mezzano.compiler.backend::remove-instruction backend-function inst))
          #+(or)
          ((and (typep inst 'fill-instruction)
                (typep next-inst 'move-instruction)
                (eql (fill-destination inst) (move-source next-inst)))
           ;; (fill t vreg) (move reg t) => (fill reg vreg)
           (setf (fill-destination inst) (move-destination next-inst))
           (mezzano.compiler.backend::remove-instruction backend-function next-inst)
           (setf next-inst inst))
          ((and (typep inst 'spill-instruction)
                (typep next-inst 'fill-instruction)
                (eql (spill-destination inst) (fill-source next-inst)))
           ;; (spill vreg r1) (fill r2 vreg) => (spill vreg r2) (move r2 r1)
           (when (not (eql (spill-source inst) (fill-destination next-inst)))
             (mezzano.compiler.backend::insert-after
              backend-function inst
              (make-instance 'move-instruction
                             :destination (fill-destination next-inst)
                             :source (spill-source inst))))
           (mezzano.compiler.backend::remove-instruction backend-function next-inst)
           (setf next-inst inst)))))

(defun lower-fake-three-operand-instructions (backend-function)
  "Lower x86-fake-three-operand-instructions to a move & x86-instruction.
The resulting code is not in SSA form so this pass must be late in the compiler."
  (do-instructions (inst backend-function)
    (when (typep inst 'x86-fake-three-operand-instruction)
      (insert-before backend-function inst
                     (make-instance 'move-instruction
                                    :destination (x86-fake-three-operand-result inst)
                                    :source (x86-fake-three-operand-lhs inst)))
      (change-class inst 'x86-instruction
                    :operands (list (x86-fake-three-operand-result inst) (x86-fake-three-operand-rhs inst))
                    :inputs (list (x86-fake-three-operand-result inst) (x86-fake-three-operand-rhs inst))
                    :outputs (list (x86-fake-three-operand-result inst))))))

(defun compile-backend-function-0 (backend-function target)
  (mezzano.compiler.backend::simplify-cfg backend-function)
  (when (> (sys.c::optimize-quality (mezzano.compiler.backend::ast backend-function) 'speed) 1)
    ;; Always perform SSA construction above speed 1.
    (mezzano.compiler.backend::construct-ssa backend-function))
  (when (= (sys.c::optimize-quality (mezzano.compiler.backend::ast backend-function) 'debug) 0)
    ;; Leave local variables in place unless the user really wants them gone.
    (mezzano.compiler.backend::remove-unused-local-variables backend-function))
  (sys.c:with-metering (:backend-misc)
    (mezzano.compiler.backend.x86-64::lower backend-function t target))
  (sys.c:with-metering (:backend-optimize)
    (loop
       (let ((total 0))
         (incf total (mezzano.compiler.backend::unbox-phis backend-function))
         (incf total (mezzano.compiler.backend::eliminate-redundant-boxing backend-function))
         (incf total (mezzano.compiler.backend::remove-unused-instructions backend-function))
         (when (zerop total)
           (return)))))
  (mezzano.compiler.backend::deconstruct-ssa backend-function)
  (sys.c:with-metering (:backend-misc)
    (mezzano.compiler.backend.x86-64::lower backend-function nil target)
    (mezzano.compiler.backend.register-allocator::canonicalize-call-operands backend-function target)
    (mezzano.compiler.backend.register-allocator::canonicalize-argument-setup backend-function target)
    (mezzano.compiler.backend.register-allocator::canonicalize-nlx-values backend-function target)
    (mezzano.compiler.backend.register-allocator::canonicalize-values backend-function target)
    (lower-fake-three-operand-instructions backend-function)
    (mezzano.compiler.backend::remove-unused-instructions backend-function)
    (mezzano.compiler.backend::check-cfg backend-function)))

(defun compile-backend-function-1 (backend-function target)
  (compile-backend-function-0 backend-function target)
  (mezzano.compiler.backend.register-allocator::allocate-registers backend-function target)
  (sys.c:with-metering (:backend-misc)
    (mezzano.compiler.backend.x86-64::peephole backend-function)))

(defun compile-backend-function-2 (backend-function *target*)
  (multiple-value-bind (lap debug-layout environment-slot)
      (sys.c:with-metering (:backend-lap-generation)
        (to-lap backend-function))
    (when sys.c::*trace-asm*
      (format t "~S:~%" (backend-function-name backend-function))
      (format t "~{~S~%~}" lap))
    (sys.c:with-metering (:lap-assembly)
      (sys.int::assemble-lap
       lap
       (backend-function-name backend-function)
       (let* ((ast-lambda (mezzano.compiler.backend::ast backend-function)))
         (list :debug-info
               (backend-function-name backend-function) ; name
               debug-layout ; local variable stack positions
               ;; Environment index
               environment-slot
               ;; Environment layout
               (second (sys.c:lambda-information-environment-layout ast-lambda))
               ;; Source file
               (if *compile-file-pathname*
                   (namestring *compile-file-pathname*)
                   nil)
               ;; Top-level form number
               sys.int::*top-level-form-number*
               (sys.c:lambda-information-lambda-list ast-lambda) ; lambda-list
               (sys.c:lambda-information-docstring ast-lambda))) ; docstring
       nil
       :x86-64))))

(defun compile-backend-function (backend-function target)
  (compile-backend-function-1 backend-function target)
  (compile-backend-function-2 backend-function target))
