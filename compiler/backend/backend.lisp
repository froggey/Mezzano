;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend)

(defclass virtual-register ()
  ((%name :initarg :name)
   (%kind :initarg :kind :reader register-kind))
  (:default-initargs :kind :value))

(defclass backend-function ()
  ((%ast-lambda :initarg :ast-lambda)
   (%code :initarg :code :accessor backend-function-code)))

(defclass backend-instruction ()
  ())

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

(defgeneric instruction-pure-p (instruction)
  (:method ((instruction backend-instruction))
    nil))

(defclass argument-setup-instruction (backend-instruction)
  ((%fref :initarg :fref :accessor argument-setup-fref)
   (%closure :initarg :closure :accessor argument-setup-closure)
   (%count :initarg :count :accessor argument-setup-count)
   (%required :initarg :required :accessor argument-setup-required)
   (%optional :initarg :optional :accessor argument-setup-optional)
   (%rest :initarg :rest :accessor argument-setup-rest)))

(defmethod instruction-inputs ((instruction argument-setup-instruction))
  (list))

(defmethod instruction-outputs ((instruction argument-setup-instruction))
  (append (list (argument-setup-fref instruction)
                (argument-setup-closure instruction)
                (argument-setup-count instruction))
          (argument-setup-required instruction)
          (argument-setup-optional instruction)
          (if (argument-setup-rest instruction)
              (list (argument-setup-rest instruction))
              (list))))

(defmethod print-instruction ((instruction argument-setup-instruction))
  (format t "   ~S~%"
          `(:argument-setup ,(argument-setup-fref instruction)
                            ,(argument-setup-closure instruction)
                            ,(argument-setup-count instruction)
                            ,(argument-setup-required instruction)
                            ,(argument-setup-optional instruction)
                            ,(argument-setup-rest instruction))))

(defclass move-instruction (backend-instruction)
  ((%destination :initarg :destination :accessor move-destination)
   (%source :initarg :source :accessor move-source))
  (:documentation "Set the destination register to value of the source register."))

(defmethod instruction-inputs ((instruction move-instruction))
  (list (move-source instruction)))

(defmethod instruction-outputs ((instruction move-instruction))
  (list (move-destination instruction)))

(defmethod print-instruction ((instruction move-instruction))
  (format t "   ~S~%"
          `(:move ,(move-destination instruction)
                  ,(move-source instruction))))

(defmethod instruction-pure-p ((instruction move-instruction))
  t)

(defclass spill-instruction (backend-instruction)
  ((%destination :initarg :destination :accessor spill-destination)
   (%source :initarg :source :accessor spill-source))
  (:documentation "Store a physical register into a virtual register's spill slot."))

(defmethod instruction-inputs ((instruction spill-instruction))
  (list (spill-source instruction)))

(defmethod instruction-outputs ((instruction spill-instruction))
  (list (spill-destination instruction)))

(defmethod print-instruction ((instruction spill-instruction))
  (format t "   ~S~%"
          `(:spill ,(spill-destination instruction)
                   ,(spill-source instruction))))

(defclass fill-instruction (backend-instruction)
  ((%destination :initarg :destination :accessor fill-destination)
   (%source :initarg :source :accessor fill-source))
  (:documentation "Load a physical register from a virtual register's fill slot."))

(defmethod instruction-inputs ((instruction fill-instruction))
  (list (fill-source instruction)))

(defmethod instruction-outputs ((instruction fill-instruction))
  (list (fill-destination instruction)))

(defmethod print-instruction ((instruction fill-instruction))
  (format t "   ~S~%"
          `(:fill ,(fill-destination instruction)
                  ,(fill-source instruction))))

(defclass constant-instruction (backend-instruction)
  ((%destination :initarg :destination :accessor constant-destination)
   (%value :initarg :value :accessor constant-value))
  (:documentation "Set the destination register to a constant value."))

(defmethod instruction-inputs ((instruction constant-instruction))
  (list))

(defmethod instruction-outputs ((instruction constant-instruction))
  (list (constant-destination instruction)))

(defmethod print-instruction ((instruction constant-instruction))
  (format t "   ~S~%"
          `(:constant ,(constant-destination instruction)
                      ,(constant-value instruction))))

(defmethod instruction-pure-p ((instruction constant-instruction))
  t)

(defclass values-instruction (backend-instruction)
  ((%values :initarg :values :accessor values-values))
  (:documentation "Produce multiple values from a list of single values."))

(defmethod instruction-inputs ((instruction values-instruction))
  (values-values instruction))

(defmethod instruction-outputs ((instruction values-instruction))
  (list))

(defmethod print-instruction ((instruction values-instruction))
  (format t "   ~S~%"
          `(:values ,@(values-values instruction))))

(defmethod produces-multiple-p ((instruction values-instruction))
  t)

(defclass multiple-value-bind-instruction (backend-instruction)
  ((%values :initarg :values :accessor multiple-value-bind-values)))

(defmethod instruction-inputs ((instruction multiple-value-bind-instruction))
  (list))

(defmethod instruction-outputs ((instruction multiple-value-bind-instruction))
  (multiple-value-bind-values instruction))

(defmethod print-instruction ((instruction multiple-value-bind-instruction))
  (format t "   ~S~%"
          `(:multiple-value-bind ,@(multiple-value-bind-values instruction))))

(defmethod consumes-multiple-p ((instruction multiple-value-bind-instruction))
  t)

(defclass save-multiple-instruction (backend-instruction)
  ((%context :initarg :context :accessor save-multiple-context))
  (:documentation "Save current multiple values."))

(defmethod instruction-inputs ((instruction save-multiple-instruction))
  (list))

(defmethod instruction-outputs ((instruction save-multiple-instruction))
  (list (save-multiple-context instruction)))

(defmethod print-instruction ((instruction save-multiple-instruction))
  (format t "   ~S~%"
          `(:save-multiple ,(save-multiple-context instruction))))

(defmethod consumes-multiple-p ((instruction save-multiple-instruction))
  t)

(defclass restore-multiple-instruction (backend-instruction)
  ((%context :initarg :context :accessor restore-multiple-context))
  (:documentation "Restore current multiple values. Must be matched in a stack-like way with a save instruction."))

(defmethod instruction-inputs ((instruction restore-multiple-instruction))
  (list (restore-multiple-context instruction)))

(defmethod instruction-outputs ((instruction restore-multiple-instruction))
  (list))

(defmethod print-instruction ((instruction restore-multiple-instruction))
  (format t "   ~S~%"
          `(:restore-multiple ,(restore-multiple-context instruction))))

(defmethod produces-multiple-p ((instruction restore-multiple-instruction))
  t)

(defclass forget-multiple-instruction (backend-instruction)
  ((%context :initarg :context :accessor forget-multiple-context))
  (:documentation "Forget an unused set of saved multiple values."))

(defmethod instruction-inputs ((instruction forget-multiple-instruction))
  (list (forget-multiple-context instruction)))

(defmethod instruction-outputs ((instruction forget-multiple-instruction))
  (list))

(defmethod print-instruction ((instruction forget-multiple-instruction))
  (format t "   ~S~%"
          `(:forget-multiple ,(forget-multiple-context instruction))))

(defclass jump-instruction (terminator-instruction)
  ((%target :initarg :target :accessor jump-target))
  (:documentation "Unconditionally transfer control to the target label."))

(defmethod instruction-inputs ((instruction jump-instruction))
  (list))

(defmethod instruction-outputs ((instruction jump-instruction))
  (list))

(defmethod print-instruction ((instruction jump-instruction))
  (format t "   ~S~%"
          `(:jump ,(jump-target instruction))))

(defclass branch-instruction (terminator-instruction)
  ((%value :initarg :value :accessor branch-value)
   (%target :initarg :target :accessor branch-target))
  (:documentation "Conditionally transfer control to the target label."))

(defmethod instruction-inputs ((instruction branch-instruction))
  (list (branch-value instruction)))

(defmethod instruction-outputs ((instruction branch-instruction))
  (list))

(defclass branch-true-instruction (branch-instruction)
  ()
  (:documentation "Transfer control to the target label if the value is true."))

(defmethod print-instruction ((instruction branch-true-instruction))
  (format t "   ~S~%"
          `(:branch-true ,(branch-value instruction) ,(branch-target instruction))))

(defclass branch-false-instruction (branch-instruction)
  ()
  (:documentation "Transfer control to the target label if the value is false."))

(defmethod print-instruction ((instruction branch-false-instruction))
  (format t "   ~S~%"
          `(:branch-true ,(branch-value instruction) ,(branch-target instruction))))

(defclass switch-instruction (terminator-instruction)
  ((%value :initarg :value :accessor switch-value)
   (%targets :initarg :targets :accessor switch-targets))
  (:documentation "Transfer control to the label indexed by the value."))

(defmethod instruction-inputs ((instruction switch-instruction))
  (list (switch-value instruction)))

(defmethod instruction-outputs ((instruction switch-instruction))
  (list))

(defmethod print-instruction ((instruction switch-instruction))
  (format t "   ~S~%"
          `(:switch ,(switch-value instruction) ,(switch-targets instruction))))

(defclass base-call-instruction (backend-instruction)
  ())

(defclass call-instruction (base-call-instruction)
  ((%result :initarg :result :accessor call-result)
   (%function :initarg :function :accessor call-function)
   (%arguments :initarg :arguments :accessor call-arguments))
  (:documentation "Call the named function with the given arguments and produce a single result value."))

(defmethod instruction-inputs ((instruction call-instruction))
  (call-arguments instruction))

(defmethod instruction-outputs ((instruction call-instruction))
  (list (call-result instruction)))

(defmethod print-instruction ((instruction call-instruction))
  (format t "   ~S~%"
          `(:call ,(call-result instruction) ,(call-function instruction) ,(call-arguments instruction))))

(defclass call-multiple-instruction (base-call-instruction)
  ((%function :initarg :function :accessor call-function)
   (%arguments :initarg :arguments :accessor call-arguments))
  (:documentation "Call the named function with the given arguments and produce multiple values."))

(defmethod instruction-inputs ((instruction call-multiple-instruction))
  (call-arguments instruction))

(defmethod instruction-outputs ((instruction call-multiple-instruction))
  (list))

(defmethod print-instruction ((instruction call-multiple-instruction))
  (format t "   ~S~%"
          `(:call-multiple ,(call-function instruction) ,(call-arguments instruction))))

(defmethod produces-multiple-p ((instruction call-multiple-instruction))
  t)

(defclass funcall-instruction (base-call-instruction)
  ((%result :initarg :result :accessor call-result)
   (%function :initarg :function :accessor call-function)
   (%arguments :initarg :arguments :accessor call-arguments))
  (:documentation "Call the function object with the given arguments and produce a single result value."))

(defmethod instruction-inputs ((instruction funcall-instruction))
  (list* (call-function instruction)
         (call-arguments instruction)))

(defmethod instruction-outputs ((instruction funcall-instruction))
  (list (call-result instruction)))

(defmethod print-instruction ((instruction funcall-instruction))
  (format t "   ~S~%"
          `(:funcall ,(call-result instruction) ,(call-function instruction) ,(call-arguments instruction))))

(defclass funcall-multiple-instruction (base-call-instruction)
  ((%function :initarg :function :accessor call-function)
   (%arguments :initarg :arguments :accessor call-arguments))
  (:documentation "Call the function object with the given arguments and produce multiple values."))

(defmethod instruction-inputs ((instruction funcall-multiple-instruction))
  (list* (call-function instruction)
         (call-arguments instruction)))

(defmethod instruction-outputs ((instruction funcall-multiple-instruction))
  (list))

(defmethod print-instruction ((instruction funcall-multiple-instruction))
  (format t "   ~S~%"
          `(:funcall-multiple ,(call-function instruction) ,(call-arguments instruction))))

(defmethod produces-multiple-p ((instruction funcall-multiple-instruction))
  t)

(defclass multiple-value-funcall-instruction (base-call-instruction)
  ((%result :initarg :result :accessor call-result)
   (%function :initarg :function :accessor call-function))
  (:documentation "Call the function object with the current multiple values and produce a single result value."))

(defmethod instruction-inputs ((instruction multiple-value-funcall-instruction))
  (list (call-function instruction)))

(defmethod instruction-outputs ((instruction multiple-value-funcall-instruction))
  (list (call-result instruction)))

(defmethod print-instruction ((instruction multiple-value-funcall-instruction))
  (format t "   ~S~%"
          `(:multiple-value-funcall ,(call-result instruction) ,(call-function instruction))))

(defmethod consumes-multiple-p ((instruction multiple-value-funcall-instruction))
  t)

(defclass multiple-value-funcall-multiple-instruction (base-call-instruction)
  ((%function :initarg :function :accessor call-function))
  (:documentation "Call the function object with the current multiple values and produce multiple values."))

(defmethod instruction-inputs ((instruction multiple-value-funcall-multiple-instruction))
  (list (call-function instruction)))

(defmethod instruction-outputs ((instruction multiple-value-funcall-multiple-instruction))
  (list))

(defmethod print-instruction ((instruction multiple-value-funcall-multiple-instruction))
  (format t "   ~S~%"
          `(:multiple-value-funcall-multiple ,(call-function instruction))))

(defmethod produces-multiple-p ((instruction multiple-value-funcall-multiple-instruction))
  t)

(defmethod consumes-multiple-p ((instruction multiple-value-funcall-multiple-instruction))
  t)

(defclass return-instruction (terminator-instruction)
  ((%value :initarg :value :accessor return-value))
  (:documentation "Return a single value from the function."))

(defmethod instruction-inputs ((instruction return-instruction))
  (list (return-value instruction)))

(defmethod instruction-outputs ((instruction return-instruction))
  (list))

(defmethod print-instruction ((instruction return-instruction))
  (format t "   ~S~%"
          `(:return ,(return-value instruction))))

(defclass return-multiple-instruction (terminator-instruction)
  ()
  (:documentation "Return multiple values from the function."))

(defmethod instruction-inputs ((instruction return-multiple-instruction))
  (list))

(defmethod instruction-outputs ((instruction return-multiple-instruction))
  (list))

(defmethod print-instruction ((instruction return-multiple-instruction))
  (format t "   ~S~%"
          `(:return-multiple)))

(defmethod consumes-multiple-p ((instruction return-multiple-instruction))
  t)

(defclass unreachable-instruction (terminator-instruction)
  ()
  (:documentation "An instruction that should never be reached. Used to terminate control after calls to no-return functions."))

(defmethod instruction-inputs ((instruction unreachable-instruction))
  (list))

(defmethod instruction-outputs ((instruction unreachable-instruction))
  (list))

(defmethod print-instruction ((instruction unreachable-instruction))
  (format t "   ~S~%"
          `(:unreachable)))

(defclass begin-nlx-instruction (backend-instruction)
  ((%context :initarg :context :accessor nlx-context)
   (%targets :initarg :targets :accessor begin-nlx-targets))
  (:documentation "Create a non-local-exit context for the given branch targets."))

(defmethod instruction-inputs ((instruction begin-nlx-instruction))
  (list))

(defmethod instruction-outputs ((instruction begin-nlx-instruction))
  (list (nlx-context instruction)))

(defmethod print-instruction ((instruction begin-nlx-instruction))
  (format t "   ~S~%"
          `(:begin-nlx ,(nlx-context instruction) ,(begin-nlx-targets instruction))))

(defclass finish-nlx-instruction (backend-instruction)
  ((%region :initarg :region :accessor nlx-region)
   (%context :initarg :context :accessor nlx-context))
  (:documentation "Tear down the given NLX context."))

(defmethod instruction-inputs ((instruction finish-nlx-instruction))
  (list (nlx-context instruction)))

(defmethod instruction-outputs ((instruction finish-nlx-instruction))
  (list))

(defmethod print-instruction ((instruction finish-nlx-instruction))
  (format t "   ~S~%"
          `(:finish-nlx ,(nlx-context instruction))))

(defclass invoke-nlx-instruction (backend-instruction)
  ((%context :initarg :context :accessor nlx-context)
   (%index :initarg :index :accessor invoke-nlx-index)
   (%value :initarg :value :accessor invoke-nlx-value))
  (:documentation "Jump to the indexed target in the given context."))

(defmethod instruction-inputs ((instruction invoke-nlx-instruction))
  (list (nlx-context instruction) (invoke-nlx-value instruction)))

(defmethod instruction-outputs ((instruction invoke-nlx-instruction))
  (list))

(defmethod print-instruction ((instruction invoke-nlx-instruction))
  (format t "   ~S~%"
          `(:invoke-nlx ,(nlx-context instruction) ,(invoke-nlx-index instruction) ,(invoke-nlx-value instruction))))

(defclass invoke-nlx-multiple-instruction (backend-instruction)
  ((%context :initarg :context :accessor nlx-context)
   (%index :initarg :index :accessor invoke-nlx-index))
  (:documentation "Jump to the indexed target in the given context."))

(defmethod instruction-inputs ((instruction invoke-nlx-multiple-instruction))
  (list (nlx-context instruction)))

(defmethod instruction-outputs ((instruction invoke-nlx-multiple-instruction))
  (list))

(defmethod print-instruction ((instruction invoke-nlx-multiple-instruction))
  (format t "   ~S~%"
          `(:invoke-nlx-multiple ,(nlx-context instruction) ,(invoke-nlx-index instruction))))

(defmethod consumes-multiple-p ((instruction invoke-nlx-multiple-instruction))
  t)

(defclass nlx-entry-instruction (backend-instruction)
  ((%region :initarg :region :accessor nlx-region)
   (%context :initarg :context :accessor nlx-context)
   (%value :initarg :value :accessor nlx-entry-value))
  (:documentation "Mark the entry point of an NLX target that accepts a single value."))

(defmethod instruction-inputs ((instruction nlx-entry-instruction))
  (list (nlx-context instruction)))

(defmethod instruction-outputs ((instruction nlx-entry-instruction))
  (list (nlx-entry-value instruction)))

(defmethod print-instruction ((instruction nlx-entry-instruction))
  (format t "   ~S~%"
          `(:nlx-entry ,(nlx-context instruction) ,(nlx-entry-value instruction))))

(defclass nlx-entry-multiple-instruction (backend-instruction)
  ((%region :initarg :region :accessor nlx-region)
   (%context :initarg :context :accessor nlx-context))
  (:documentation "Mark the entry point of an NLX target that accepts multiple values."))

(defmethod instruction-inputs ((instruction nlx-entry-multiple-instruction))
  (list (nlx-context instruction)))

(defmethod instruction-outputs ((instruction nlx-entry-multiple-instruction))
  (list))

(defmethod print-instruction ((instruction nlx-entry-multiple-instruction))
  (format t "   ~S~%"
          `(:nlx-entry-multiple ,(nlx-context instruction))))

(defmethod produces-multiple-p ((instruction nlx-entry-multiple-instruction))
  t)

(defclass object-get-instruction (backend-instruction)
  ((%destination :initarg :destination :accessor object-get-destination)
   (%object :initarg :object :accessor object-get-object)
   (%index :initarg :index :accessor object-get-index))
  (:documentation "Load a field from an object at the given index."))

(defmethod instruction-inputs ((instruction object-get-instruction))
  (list (object-get-object instruction) (object-get-index instruction)))

(defmethod instruction-outputs ((instruction object-get-instruction))
  (list (object-get-destination instruction)))

(defmethod instruction-pure-p ((instruction object-get-instruction))
  t)

(defclass object-get-t-instruction (object-get-instruction)
  ()
  (:documentation "Load a lisp value from an object."))

(defmethod print-instruction ((instruction object-get-t-instruction))
  (format t "   ~S~%"
          `(:object-get-t ,(object-get-destination instruction) ,(object-get-object instruction) ,(object-get-index instruction))))

(defclass object-set-instruction (backend-instruction)
  ((%value :initarg :value :accessor object-set-value)
   (%object :initarg :object :accessor object-set-object)
   (%index :initarg :index :accessor object-set-index))
  (:documentation "Store a value into a field in an object at the given index."))

(defmethod instruction-inputs ((instruction object-set-instruction))
  (list (object-set-value instruction) (object-set-object instruction) (object-set-index instruction)))

(defmethod instruction-outputs ((instruction object-set-instruction))
  (list))

(defclass object-set-t-instruction (object-set-instruction)
  ()
  (:documentation "Store a lisp value into an object."))

(defmethod print-instruction ((instruction object-set-t-instruction))
  (format t "   ~S~%"
          `(:object-set-t ,(object-set-value instruction) ,(object-set-object instruction) ,(object-set-index instruction))))

(defclass eq-instruction (backend-instruction)
  ((%result :initarg :result :accessor eq-result)
   (%lhs :initarg :lhs :accessor eq-lhs)
   (%rhs :initarg :rhs :accessor eq-rhs)))

(defmethod instruction-inputs ((instruction eq-instruction))
  (list (eq-lhs instruction) (eq-rhs instruction)))

(defmethod instruction-outputs ((instruction eq-instruction))
  (list (eq-result instruction)))

(defmethod print-instruction ((instruction eq-instruction))
  (format t "   ~S~%"
          `(:eq ,(eq-result instruction) ,(eq-lhs instruction) ,(eq-rhs instruction))))

(defmethod instruction-pure-p ((instruction eq-instruction))
  t)

(defclass undefined-function-p-instruction (backend-instruction)
  ((%result :initarg :result :accessor undefined-function-p-result)
   (%value :initarg :value :accessor undefined-function-p-value)))

(defmethod instruction-inputs ((instruction undefined-function-p-instruction))
  (list (undefined-function-p-value instruction)))

(defmethod instruction-outputs ((instruction undefined-function-p-instruction))
  (list (undefined-function-p-result instruction)))

(defmethod print-instruction ((instruction undefined-function-p-instruction))
  (format t "   ~S~%"
          `(:undefined-function-p ,(undefined-function-p-result instruction) ,(undefined-function-p-value instruction))))

(defmethod instruction-pure-p ((instruction undefined-function-p-instruction))
  t)

(defclass fixnum-<-instruction (backend-instruction)
  ((%result :initarg :result :accessor fixnum-<-result)
   (%lhs :initarg :lhs :accessor fixnum-<-lhs)
   (%rhs :initarg :rhs :accessor fixnum-<-rhs)))

(defmethod instruction-inputs ((instruction fixnum-<-instruction))
  (list (fixnum-<-lhs instruction) (fixnum-<-rhs instruction)))

(defmethod instruction-outputs ((instruction fixnum-<-instruction))
  (list (fixnum-<-result instruction)))

(defmethod print-instruction ((instruction fixnum-<-instruction))
  (format t "   ~S~%"
          `(:fixnum-< ,(fixnum-<-result instruction) ,(fixnum-<-lhs instruction) ,(fixnum-<-rhs instruction))))

(defmethod instruction-pure-p ((instruction fixnum-<-instruction))
  t)

(defclass push-special-stack-instruction (backend-instruction)
  ((%a-value :initarg :a-value :accessor push-special-stack-a-value)
   (%b-value :initarg :b-value :accessor push-special-stack-b-value)))

(defmethod instruction-inputs ((instruction push-special-stack-instruction))
  (list (push-special-stack-a-value instruction) (push-special-stack-b-value instruction)))

(defmethod instruction-outputs ((instruction push-special-stack-instruction))
  (list))

(defmethod print-instruction ((instruction push-special-stack-instruction))
  (format t "   ~S~%"
          `(:push-special-stack ,(push-special-stack-a-value instruction) ,(push-special-stack-b-value instruction))))

(defclass flush-binding-cache-entry-instruction (backend-instruction)
  ((%symbol :initarg :symbol :accessor flush-binding-cache-entry-symbol)))

(defmethod instruction-inputs ((instruction flush-binding-cache-entry-instruction))
  (list (flush-binding-cache-entry-symbol instruction)))

(defmethod instruction-outputs ((instruction flush-binding-cache-entry-instruction))
  (list))

(defmethod print-instruction ((instruction flush-binding-cache-entry-instruction))
  (format t "   ~S~%"
          `(:flush-binding-cache-entry ,(flush-binding-cache-entry-symbol instruction))))

(defclass unbind-instruction (backend-instruction)
  ())

(defmethod instruction-inputs ((instruction unbind-instruction))
  (list))

(defmethod instruction-outputs ((instruction unbind-instruction))
  (list))

(defmethod print-instruction ((instruction unbind-instruction))
  (format t "   ~S~%"
          `(:unbind)))

(defclass disestablish-block-or-tagbody-instruction (backend-instruction)
  ())

(defmethod instruction-inputs ((instruction disestablish-block-or-tagbody-instruction))
  (list))

(defmethod instruction-outputs ((instruction disestablish-block-or-tagbody-instruction))
  (list))

(defmethod print-instruction ((instruction disestablish-block-or-tagbody-instruction))
  (format t "   ~S~%"
          `(:disestablish-block-or-tagbody)))

(defclass disestablish-unwind-protect-instruction (backend-instruction)
  ())

(defmethod instruction-inputs ((instruction disestablish-unwind-protect-instruction))
  (list))

(defmethod instruction-outputs ((instruction disestablish-unwind-protect-instruction))
  (list))

(defmethod print-instruction ((instruction disestablish-unwind-protect-instruction))
  (format t "   ~S~%"
          `(:disestablish-unwind-protect)))

(defun print-function (function)
  (format t "~S~%" function)
  (dolist (c (backend-function-code function))
    (cond ((symbolp c)
           (format t " ~S~%" c))
          (t
           (print-instruction c)))))
