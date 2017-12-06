;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

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

(defmethod ra:instruction-clobbers ((instruction arm64-instruction) (architecture sys.c:arm64-target))
  (arm64-instruction-clobbers instruction))

(defmethod ra:instruction-inputs-read-before-outputs-written-p ((instruction arm64-instruction) (architecture sys.c:arm64-target))
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
   (%target :initarg :target :accessor arm64-branch-target)))

(defmethod ir:successors (function (instruction arm64-branch-instruction))
  (list (ir:next-instruction function instruction)
        (arm64-branch-target instruction)))

(defmethod ir:instruction-inputs ((instruction arm64-branch-instruction))
  '())

(defmethod ir:instruction-outputs ((instruction arm64-branch-instruction))
  '())

(defmethod ir:replace-all-registers ((instruction arm64-branch-instruction) substitution-function)
  )

(defmethod ir:print-instruction ((instruction arm64-branch-instruction))
  (format t "   ~S~%"
          `(:arm64-branch ,(arm64-instruction-opcode instruction) ,(arm64-branch-target instruction))))

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
           (values 'mezzano.runtime::%%make-unsigned-byte-64-x0 :x0))
          (ir:box-signed-byte-64-instruction
           (values 'mezzano.runtime::%%make-signed-byte-64-x0 :x0))
          (ir:box-double-float-instruction
           (values 'sys.int::%%make-double-float-x0 :x0)))
      (when box-function
        (let* ((value (ir:box-source inst))
               (result (ir:box-destination inst)))
          (ir:insert-before
           backend-function inst
           (make-instance 'move-instruction
                          :destination box-register
                          :source value))
          (ir:insert-before
           backend-function inst
           (make-instance 'arm64-instruction
                          :opcode 'lap:ldr
                          :operands (list :x7 `(:function ,box-function))
                          :inputs (list)
                          :outputs (list :x7)
                          :clobbers '(:x7)))
          (ir:insert-before
           backend-function inst
           (make-instance 'arm64-instruction
                          :opcode 'lap:ldr
                          :operands (list :x9 `(:object :x7 ,sys.int::+fref-entry-point+))
                          :inputs (list :x7)
                          :outputs (list :x9)
                          :clobbers '(:x9)))
          (ir:insert-before
           backend-function inst
           (make-instance 'arm64-instruction
                          :opcode 'lap:blr
                          :operands (list :x9)
                          :inputs (list :x9 box-register)
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
           (make-instance 'move-instruction
                          :destination result
                          :source :x0))
          (ir:remove-instruction backend-function inst))))))

(defun compile-backend-function-1 (backend-function target)
  (ir::simplify-cfg backend-function)
  (when (> (sys.c::optimize-quality (ir::ast backend-function) 'speed) 1)
    ;; Always perform SSA construction above speed 1.
    (ir::construct-ssa backend-function))
  (when (= (sys.c::optimize-quality (ir::ast backend-function) 'debug) 0)
    ;; Leave local variables in place unless the user really wants them gone.
    (ir::remove-unused-local-variables backend-function))
  (sys.c:with-metering (:backend-misc)
    (lower-builtins backend-function))
  (sys.c:with-metering (:backend-optimize)
    (loop
       (let ((total 0))
         (incf total (ir::unbox-phis backend-function))
         (incf total (ir::unbox-debug-values backend-function))
         (incf total (ir::eliminate-redundant-boxing backend-function))
         (incf total (ir::remove-unused-instructions backend-function))
         (when (zerop total)
           (return)))))
  (ir::deconstruct-ssa backend-function)
  (ir::lower-local-variables backend-function)
  (sys.c:with-metering (:backend-misc)
    (ra::canonicalize-call-operands backend-function target)
    (ra::canonicalize-argument-setup backend-function target)
    (ra::canonicalize-nlx-values backend-function target)
    (ra::canonicalize-values backend-function target)
    (ir::remove-unused-instructions backend-function)
    (ir::check-cfg backend-function)))

(defun compile-backend-function-2 (backend-function debug-map *target*)
  (declare (special *target*))
  (multiple-value-bind (lap debug-layout environment-slot)
      (sys.c:with-metering (:backend-lap-generation)
        (to-lap backend-function debug-map))
    (when sys.c::*trace-asm*
      (format t "~S:~%" (ir:backend-function-name backend-function))
      (format t "~{~S~%~}" lap))
    (sys.c:with-metering (:lap-assembly)
      (sys.int::assemble-lap
       lap
       (ir:backend-function-name backend-function)
       (let* ((ast-lambda (ir::ast backend-function)))
         (list :debug-info
               (ir:backend-function-name backend-function) ; name
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
               (sys.c:lambda-information-docstring ast-lambda) ; docstring
               nil)) ; precise debug info
       nil
       :arm64))))

(defun compile-backend-function (backend-function target)
  (compile-backend-function-1 backend-function target)
  (let ((debug-map (ra::allocate-registers backend-function target)))
    (compile-backend-function-2 backend-function debug-map target)))
