;;;; ARM64 compiler backend.

(in-package :mezzano.compiler.backend.arm64)

;;; Wrapper around an arbitrary arm64 instruction.
(defclass arm64-instruction (ir:backend-instruction)
  ((%inputs :initarg :inputs :reader ir:instruction-inputs)
   (%outputs :initarg :outputs :reader ir:instruction-outputs)
   (%opcode :initarg :opcode :reader arm64-instruction-opcode)
   (%operands :initarg :operands :reader arm64-instruction-operands)
   (%clobbers :initarg :clobbers :reader arm64-instruction-clobbers)
   (%early-clobber :initarg :early-clobber :reader arm64-instruction-early-clobber)
   (%prefix :initarg :prefix :reader arm64-instruction-prefix))
  (:default-initargs :clobbers '() :early-clobber nil :prefix nil))

(defmethod ra:instruction-clobbers ((instruction arm64-instruction) (architecture c:arm64-target))
  (arm64-instruction-clobbers instruction))

(defmethod ra:instruction-inputs-read-before-outputs-written-p ((instruction arm64-instruction) (architecture c:arm64-target))
  (not (arm64-instruction-early-clobber instruction)))

(defmethod ir:replace-all-registers ((instruction arm64-instruction) substitution-function)
  (setf (slot-value instruction '%inputs) (mapcar substitution-function (slot-value instruction '%inputs)))
  (setf (slot-value instruction '%outputs) (mapcar substitution-function (slot-value instruction '%outputs)))
  (setf (slot-value instruction '%operands)
        (loop
           for operand in (slot-value instruction '%operands)
           collect (cond ((typep operand 'ir:virtual-register)
                          (funcall substitution-function operand))
                         ((and (consp operand)
                               (not (member (first operand) '(:constant :function))))
                          (mapcar substitution-function operand))
                         (t operand)))))

(defmethod ir:print-instruction ((instruction arm64-instruction))
  (format t "   ~S~%"
          `(:arm64 ,(arm64-instruction-opcode instruction) ,(arm64-instruction-operands instruction))))

;;; Wrapper around arm64 branch instructions.
(defclass arm64-branch-instruction (ir:terminator-instruction)
  ((%opcode :initarg :opcode :accessor arm64-instruction-opcode)
   (%true-target :initarg :true-target :accessor arm64-branch-true-target)
   (%false-target :initarg :false-target :accessor arm64-branch-false-target)))

(defmethod ir:successors (function (instruction arm64-branch-instruction))
  (list (arm64-branch-true-target instruction)
        (arm64-branch-false-target instruction)))

(defmethod ir:instruction-inputs ((instruction arm64-branch-instruction))
  '())

(defmethod ir:instruction-outputs ((instruction arm64-branch-instruction))
  '())

(defmethod ir:replace-all-registers ((instruction arm64-branch-instruction) substitution-function)
  )

(defmethod ir:print-instruction ((instruction arm64-branch-instruction))
  (format t "   ~S~%"
          `(:arm64-branch ,(arm64-instruction-opcode instruction) ,(arm64-branch-true-target instruction) ,(arm64-branch-false-target instruction))))

(defun lower-complicated-box-instructions (backend-function)
  (do* ((inst (ir:first-instruction backend-function) next-inst)
        (next-inst (ir:next-instruction backend-function inst)
                   (if inst
                       (ir:next-instruction backend-function inst)
                       nil)))
       ((null inst))
    (multiple-value-bind (box-function box-register)
        (typecase inst
          (ir:box-unsigned-byte-64-instruction
           (values 'mezzano.runtime::%%make-unsigned-byte-64-x10 :x10))
          (ir:box-signed-byte-64-instruction
           (values 'mezzano.runtime::%%make-signed-byte-64-x10 :x10))
          (ir:box-double-float-instruction
           (values 'sys.int::%%make-double-float-x10 :x10)))
      (when box-function
        (let* ((value (ir:box-source inst))
               (result (ir:box-destination inst)))
          (ir:insert-before
           backend-function inst
           (make-instance 'ir:move-instruction
                          :destination box-register
                          :source value))
          (ir:insert-before
           backend-function inst
           (make-instance 'arm64-instruction
                          :opcode 'lap:named-call
                          :operands (list box-function)
                          :inputs (list box-register)
                          :outputs (list :x0)
                          :clobbers '(:x0 :x1 :x2 :x3 :x4 :x5 :x6 :x7
                                      :x8 :x9 :x10 :x11 :x12 :x13 :x14 :x15
                                      :x16 :x17 :x18 :x19 :x20 :x21 :x22 :x23
                                      :x24 :x25
                                      :q0 :q1 :q2 :q3 :q4 :q5 :q6 :q7
                                      :q8 :q9 :q10 :q11 :q12 :q13 :q14 :q15
                                      :q16 :q17 :q18 :q19 :q20 :q21 :q22 :q23
                                      :q24 :q25 :q26 :q27 :q28 :q29 :q30 :q31)))
          (ir:insert-before
           backend-function inst
           (make-instance 'ir:move-instruction
                          :destination result
                          :source :x0))
          (ir:remove-instruction backend-function inst))))))

(defmethod ir:perform-target-lowering (backend-function (target c:arm64-target))
  (lower-builtins backend-function))

(defmethod ir:perform-target-lowering-post-ssa (backend-function (target c:arm64-target))
  (lower-complicated-box-instructions backend-function))
