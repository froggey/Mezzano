;;;; Basic definitions for the compiler backend.

(in-package :mezzano.compiler.backend)

(defvar *shut-up* t)

(defclass virtual-register ()
  ((%name :initarg :name)
   (%kind :initarg :kind :reader virtual-register-kind))
  (:default-initargs :kind :value :name nil))

(defmethod print-object ((object virtual-register) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S" (slot-value object '%name) (virtual-register-kind object))))

(defclass backend-function ()
  ((%ast-lambda :initarg :ast-lambda :reader ast)
   (%first-instruction :initform nil)
   (%last-instruction :initform nil)))

(defun backend-function-name (backend-function)
  (lambda-information-name (ast backend-function)))

(defclass backend-instruction ()
  ((%next-instruction)
   (%prev-instruction)))

(defun first-instruction (function)
  (slot-value function '%first-instruction))

(defun last-instruction (function)
  (slot-value function '%last-instruction))

(defun next-instruction (function instruction)
  (declare (ignore function))
  (slot-value instruction '%next-instruction))

(defun prev-instruction (function instruction)
  (declare (ignore function))
  (slot-value instruction '%prev-instruction))

(defun insert-before (function instruction new-instruction)
  (setf (slot-value new-instruction '%prev-instruction) (slot-value instruction '%prev-instruction)
        (slot-value new-instruction '%next-instruction) instruction)
  (cond ((null (slot-value instruction '%prev-instruction))
         (setf (slot-value function '%first-instruction) new-instruction))
        (t
         (setf (slot-value (slot-value instruction '%prev-instruction)
                           '%next-instruction)
               new-instruction)))
  (setf (slot-value instruction '%prev-instruction) new-instruction)
  new-instruction)

(defun insert-after (function instruction new-instruction)
  (setf (slot-value new-instruction '%next-instruction) (slot-value instruction '%next-instruction)
        (slot-value new-instruction '%prev-instruction) instruction)
  (cond ((null (slot-value instruction '%next-instruction))
         (setf (slot-value function '%last-instruction) new-instruction))
        (t
         (setf (slot-value (slot-value instruction '%next-instruction)
                           '%prev-instruction)
               new-instruction)))
  (setf (slot-value instruction '%next-instruction) new-instruction)
  new-instruction)

(defun append-instruction (function new-instruction)
  (cond ((null (slot-value function '%last-instruction))
         (setf (slot-value new-instruction '%next-instruction) nil
               (slot-value new-instruction '%prev-instruction) nil)
         (setf (slot-value function '%first-instruction) new-instruction
               (slot-value function '%last-instruction) new-instruction))
        (t
         (insert-after function (last-instruction function) new-instruction)))
  new-instruction)

(defun remove-instruction (function instruction)
  (cond ((and (eql (slot-value function '%first-instruction) instruction)
              (eql (slot-value function '%last-instruction) instruction))
         (setf (slot-value function '%first-instruction) nil
               (slot-value function '%last-instruction) nil))
        ((eql (slot-value function '%first-instruction) instruction)
         (setf (slot-value function '%first-instruction)
               (slot-value instruction '%next-instruction))
         (setf (slot-value (slot-value instruction '%next-instruction) '%prev-instruction) nil))
        ((eql (slot-value function '%last-instruction) instruction)
         (setf (slot-value function '%last-instruction)
               (slot-value instruction '%prev-instruction))
         (setf (slot-value (slot-value instruction '%prev-instruction) '%next-instruction) nil))
        (t
         (setf (slot-value (slot-value instruction '%prev-instruction) '%next-instruction)
               (slot-value instruction '%next-instruction))
         (setf (slot-value (slot-value instruction '%next-instruction) '%prev-instruction)
               (slot-value instruction '%prev-instruction))))
  (slot-makunbound instruction '%next-instruction)
  (slot-makunbound instruction '%prev-instruction)
  (values))

(defmacro do-instructions ((instruction function &optional result-value) &body body)
  (let ((fn-sym (gensym)))
    `(do* ((,fn-sym ,function)
           (,instruction (first-instruction ,fn-sym) (next-instruction ,fn-sym ,instruction)))
          ((null ,instruction)
           ,result-value)
      ,@body)))

(defmacro do-reversed-instructions ((instruction function &optional result-value) &body body)
  (let ((fn-sym (gensym)))
    `(do* ((,fn-sym ,function)
           (,instruction (last-instruction ,fn-sym) (prev-instruction ,fn-sym ,instruction)))
          ((null ,instruction)
           ,result-value)
      ,@body)))

(defclass terminator-instruction (backend-instruction)
  ()
  (:documentation "Base class of instructions that provide non-fallthrough control flow."))

(defgeneric print-instruction (instruction))

;; Uses.
(defgeneric instruction-inputs (instruction))
;; Defs.
(defgeneric instruction-outputs (instruction))

(defgeneric produces-multiple-p (instruction)
  (:method ((inst backend-instruction))
    nil))

(defgeneric consumes-multiple-p (instruction)
  (:method ((inst backend-instruction))
    nil))

(defgeneric multiple-value-safe-p (instruction architecture)
  (:method (instruction architecture) nil))

(defgeneric instruction-pure-p (instruction)
  (:method ((instruction backend-instruction))
    nil))

(defgeneric successors (function instruction)
  (:method (function (instruction backend-instruction))
    (list (next-instruction function instruction)))
  (:method (function (instruction terminator-instruction))
    (error "Terminator instructions must implement a method on SUCCESSORS.")))

(defgeneric box-type (box-instruction))

(defgeneric replace-all-registers (instruction substitution-function))

(defclass label (backend-instruction)
  ((%name :initarg :name :reader label-name)
   (%phis :initarg :phis :accessor label-phis))
  (:default-initargs :phis '()))

(defmethod instruction-inputs ((instruction label))
  (list))

(defmethod instruction-outputs ((instruction label))
  (label-phis instruction))

(defmethod replace-all-registers ((instruction label) substitution-function)
  (setf (label-phis instruction) (mapcar substitution-function (label-phis instruction))))

(defmethod multiple-value-safe-p ((instruction label) architecture)
  t)

(defmethod print-instruction ((instruction label))
  (format t "~S ~:S~%" instruction (label-phis instruction)))

(defun print-function (function)
  (let ((*print-pretty* nil))
    (format t "~S~%" function)
    (do-instructions (c function)
      (cond ((typep c 'label)
             (format t " ~S ~:S~%" c (label-phis c)))
            (t
             (print-instruction c))))))

(defgeneric perform-target-lowering (backend-function target))
(defgeneric perform-target-lowering-post-ssa (backend-function target))
(defgeneric perform-target-lap-generation (backend-function debug-map spill-locations stack-layout target))

(defun compile-backend-function-1 (backend-function target)
  (simplify-cfg backend-function)
  (break-critical-edges backend-function)
  (construct-ssa backend-function)
  (localize-constants backend-function)
  (convert-rest-arg-to-dx backend-function)
  (perform-target-lowering backend-function target)
  (loop
     (let ((total 0))
       (incf total (unbox-phis backend-function))
       (incf total (unbox-debug-values backend-function))
       (incf total (eliminate-redundant-boxing backend-function))
       (incf total (remove-unused-instructions backend-function))
       (incf total (remove-unused-phis backend-function))
       (when (zerop total)
         (return))))
  (remove-extraneous-multiple-value-saves backend-function)
  (deconstruct-ssa backend-function)
  (lower-local-variables backend-function)
  (when (= (mezzano.compiler::optimize-quality (ast backend-function) 'debug) 0)
    (remove-debug-variable-instructions backend-function))
  (perform-target-lowering-post-ssa backend-function target)
  (mezzano.compiler.backend.register-allocator::canonicalize-call-operands backend-function target)
  (mezzano.compiler.backend.register-allocator::canonicalize-argument-setup backend-function target)
  (mezzano.compiler.backend.register-allocator::canonicalize-nlx-values backend-function target)
  (mezzano.compiler.backend.register-allocator::canonicalize-values backend-function target)
  (remove-unused-instructions backend-function)
  (check-cfg backend-function))

(defun compile-backend-function-2 (backend-function debug-map spill-locations stack-layout target)
  ;; Register allocation will break critical edges by inserting empty basic blocks.
  ;; Undo that.
  (simplify-cfg backend-function)
  (multiple-value-bind (lap environment-slot)
      (perform-target-lap-generation backend-function debug-map spill-locations stack-layout target)
    (when mezzano.compiler::*trace-asm*
      (format t "~S:~%" (backend-function-name backend-function))
      (dolist (inst lap)
        (when (not (and (not (eql mezzano.compiler::*trace-asm* :full))
                        (consp inst)
                        (member (first inst) '(:gc :debug))))
          (cond ((symbolp inst)
                 (format t "~S~%" inst))
                (t
                 (format t "  ~S~%" inst))))))
    (sys.int::assemble-lap
     lap
     (backend-function-name backend-function)
     (let* ((ast-lambda (ast backend-function)))
       (list :debug-info
             (backend-function-name backend-function) ; name
             nil ; local variable stack positions
             ;; Environment index
             environment-slot
             ;; Environment layout
             (second (lambda-information-environment-layout ast-lambda))
             ;; Source file
             (if *compile-file-pathname*
                 (namestring *compile-file-pathname*)
                 nil)
             ;; Top-level form number
             sys.int::*top-level-form-number*
             (lambda-information-lambda-list ast-lambda) ; lambda-list
             (lambda-information-docstring ast-lambda) ; docstring
             nil)) ; precise debug info
     nil
     target)))

(defun compile-backend-function (backend-function target)
  (compile-backend-function-1 backend-function target)
  (multiple-value-bind (debug-map spill-locations stack-layout)
      (mezzano.compiler.backend.register-allocator::allocate-registers backend-function target)
    (compile-backend-function-2 backend-function debug-map spill-locations stack-layout target)))
