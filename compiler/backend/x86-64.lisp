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

(defclass box-sse-vector-instruction (box-instruction)
  ())

(defmethod box-type ((instruction box-sse-vector-instruction))
  'mezzano.simd:sse-vector)

(defmethod mezzano.compiler.backend::print-instruction ((instruction box-sse-vector-instruction))
  (format t "   ~S~%"
          `(:box-sse-vector
            ,(box-destination instruction)
            ,(box-source instruction))))

(defclass unbox-sse-vector-instruction (unbox-instruction)
  ())

(defmethod box-type ((instruction unbox-sse-vector-instruction))
  'mezzano.simd:sse-vector)

(defmethod mezzano.compiler.backend::print-instruction ((instruction unbox-sse-vector-instruction))
  (format t "   ~S~%"
          `(:unbox-sse-vector
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

(defmacro define-builtin (name (lambda-list results) &body body)
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
                        t))))))

(defclass builtin ()
  ((%name :initarg :name :reader builtin-name)
   (%lambda-list :initarg :lambda-list :reader builtin-lambda-list)
   (%result-list :initarg :result-list :reader builtin-result-list)
   (%generator :initarg :generator :reader builtin-generator)))

(defvar *builtins* (make-hash-table :test 'equal))

(defun %defbuiltin (name lambda-list result-list generator)
  (setf (gethash name *builtins*)
        (make-instance 'builtin
                       :name name
                       :lambda-list lambda-list
                       :result-list result-list
                       :generator generator))
  name)

(define-builtin sys.int::read-frame-pointer (() result)
  (emit (make-instance 'x86-instruction
                       :opcode 'lap:lea64
                       :operands (list result `((:rbp ,(ash 1 sys.int::+n-fixnum-bits+))))
                       :inputs (list)
                       :outputs (list result))))

(defun lower-builtins (backend-function target)
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
            (t
             (let ((next (or (lower-predicate-builtin backend-function inst uses defs target)
                             (lower-builtin backend-function inst defs target))))
               (when next
                 (setf next-inst next))))))))

(defun lower-complicated-box-instructions (backend-function)
  (do* ((inst (mezzano.compiler.backend::first-instruction backend-function) next-inst)
        (next-inst (mezzano.compiler.backend::next-instruction backend-function inst) (if inst (mezzano.compiler.backend::next-instruction backend-function inst))))
       ((null inst))
    (cond ((typep inst 'box-unsigned-byte-64-instruction)
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
          ((typep inst 'box-double-float-instruction)
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
          ((typep inst 'box-mmx-vector-instruction)
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
          ((typep inst 'box-sse-vector-instruction)
           ;; (box-sse-vector value) => (call make-sse-vector-xmm0 value)
           (let* ((vector (box-source inst))
                  (result (box-destination inst)))
             (mezzano.compiler.backend::insert-before
              backend-function inst
              (make-instance 'move-instruction
                             :destination :xmm0
                             :source vector))
             (mezzano.compiler.backend::insert-before
              backend-function inst
              (make-instance 'x86-instruction
                             :opcode 'lap:mov64
                             :operands (list :r13 `(:function mezzano.simd::%%make-sse-vector-xmm0))
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
             (mezzano.compiler.backend::remove-instruction backend-function inst))))))

(defgeneric match-builtin (name n-arguments architecture))

(defmethod match-builtin (name n-arguments architecture)
  nil)

(defmethod match-builtin (name n-arguments (architecture sys.c:x86-64-target))
  (let ((builtin (gethash name *builtins*)))
    (if (and builtin
             (eql (length (builtin-lambda-list builtin)) n-arguments))
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
(defun lower-predicate-builtin (backend-function inst uses defs target)
  (let ((next-inst (next-instruction backend-function inst)))
    (when (and (typep inst 'call-instruction)
               (typep next-inst 'branch-instruction)
               (consumed-by-p inst next-inst uses defs))
      (let ((builtin (match-builtin (call-function inst)
                                    (length (call-arguments inst))
                                    target)))
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

(defun lower-builtin (backend-function inst defs target)
  (let ((builtin (and (typep inst '(or
                                    call-instruction
                                    call-multiple-instruction))
                      (match-builtin (call-function inst)
                                     (length (call-arguments inst))
                                     target))))
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

(defun compile-backend-function-1 (backend-function target)
  (mezzano.compiler.backend::simplify-cfg backend-function)
  (when (> (sys.c::optimize-quality (mezzano.compiler.backend::ast backend-function) 'speed) 1)
    ;; Always perform SSA construction above speed 1.
    (mezzano.compiler.backend::construct-ssa backend-function))
  (when (= (sys.c::optimize-quality (mezzano.compiler.backend::ast backend-function) 'debug) 0)
    ;; Leave local variables in place unless the user really wants them gone.
    (mezzano.compiler.backend::remove-unused-local-variables backend-function))
  (sys.c:with-metering (:backend-misc)
    (mezzano.compiler.backend.x86-64::lower-builtins backend-function target))
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
    (lower-complicated-box-instructions backend-function)
    (mezzano.compiler.backend.register-allocator::canonicalize-call-operands backend-function target)
    (mezzano.compiler.backend.register-allocator::canonicalize-argument-setup backend-function target)
    (mezzano.compiler.backend.register-allocator::canonicalize-nlx-values backend-function target)
    (mezzano.compiler.backend.register-allocator::canonicalize-values backend-function target)
    (lower-fake-three-operand-instructions backend-function)
    (mezzano.compiler.backend::remove-unused-instructions backend-function)
    (mezzano.compiler.backend::check-cfg backend-function)))

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
  (mezzano.compiler.backend.register-allocator::allocate-registers backend-function target)
  (compile-backend-function-2 backend-function target))
