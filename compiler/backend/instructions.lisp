;;;; Backend IR instruction definitions.

(in-package :mezzano.compiler.backend)

(defclass argument-setup-instruction (backend-instruction)
  ((%closure :initarg :closure :accessor argument-setup-closure)
   (%count :initarg :count :accessor argument-setup-count)
   (%required :initarg :required :accessor argument-setup-required)
   (%optional :initarg :optional :accessor argument-setup-optional)
   (%rest :initarg :rest :accessor argument-setup-rest)))

(defmethod instruction-inputs ((instruction argument-setup-instruction))
  (list))

(defmethod instruction-outputs ((instruction argument-setup-instruction))
  (append (list (argument-setup-closure instruction)
                (argument-setup-count instruction))
          (argument-setup-required instruction)
          (argument-setup-optional instruction)
          (if (argument-setup-rest instruction)
              (list (argument-setup-rest instruction))
              (list))))

(defmethod replace-all-registers ((instruction argument-setup-instruction) substitution-function)
  (setf (argument-setup-closure instruction) (funcall substitution-function (argument-setup-closure instruction)))
  (setf (argument-setup-count instruction) (funcall substitution-function (argument-setup-count instruction)))
  (setf (argument-setup-required instruction) (mapcar substitution-function (argument-setup-required instruction)))
  (setf (argument-setup-optional instruction) (mapcar substitution-function (argument-setup-optional instruction)))
  (when (argument-setup-rest instruction)
    (setf (argument-setup-rest instruction) (funcall substitution-function (argument-setup-rest instruction)))))

(defmethod print-instruction ((instruction argument-setup-instruction))
  (format t "   ~S~%"
          `(:argument-setup ,(argument-setup-closure instruction)
                            ,(argument-setup-count instruction)
                            ,(argument-setup-required instruction)
                            ,(argument-setup-optional instruction)
                            ,(argument-setup-rest instruction))))

(defclass bind-local-instruction (backend-instruction)
  ((%ast :initarg :ast :accessor bind-local-ast)
   (%value :initarg :value :accessor bind-local-value)))

(defmethod print-object ((object bind-local-instruction) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S" (bind-local-ast object))))

(defmethod instruction-inputs ((instruction bind-local-instruction))
  (list (bind-local-value instruction)))

(defmethod instruction-outputs ((instruction bind-local-instruction))
  (list))

(defmethod replace-all-registers ((instruction bind-local-instruction) substitution-function)
  (setf (bind-local-value instruction) (funcall substitution-function (bind-local-value instruction))))

(defmethod multiple-value-safe-p ((instruction bind-local-instruction) architecture)
  t)

(defmethod print-instruction ((instruction bind-local-instruction))
  (format t "   ~S~%"
          `(:bind-local ,(bind-local-ast instruction)
                        ,(bind-local-value instruction)
                        ,instruction)))

(defclass unbind-local-instruction (backend-instruction)
  ((%local :initarg :local :accessor unbind-local-local)))

(defmethod instruction-inputs ((instruction unbind-local-instruction))
  (list))

(defmethod instruction-outputs ((instruction unbind-local-instruction))
  (list))

(defmethod replace-all-registers ((instruction unbind-local-instruction) substitution-function)
  nil)

(defmethod multiple-value-safe-p ((instruction unbind-local-instruction) architecture)
  t)

(defmethod print-instruction ((instruction unbind-local-instruction))
  (format t "   ~S~%"
          `(:unbind-local ,(unbind-local-local instruction))))

(defclass load-local-instruction (backend-instruction)
  ((%destination :initarg :destination :accessor load-local-destination)
   (%local :initarg :local :accessor load-local-local)))

(defmethod instruction-inputs ((instruction load-local-instruction))
  (list))

(defmethod instruction-outputs ((instruction load-local-instruction))
  (list (load-local-destination instruction)))

(defmethod replace-all-registers ((instruction load-local-instruction) substitution-function)
  (setf (load-local-destination instruction) (funcall substitution-function (load-local-destination instruction))))

(defmethod multiple-value-safe-p ((instruction load-local-instruction) architecture)
  t)

(defmethod print-instruction ((instruction load-local-instruction))
  (format t "   ~S~%"
          `(:load-local ,(load-local-destination instruction)
                        ,(load-local-local instruction))))

(defmethod instruction-pure-p ((instruction load-local-instruction))
  t)

(defclass store-local-instruction (backend-instruction)
  ((%value :initarg :value :accessor store-local-value)
   (%local :initarg :local :accessor store-local-local)))

(defmethod instruction-inputs ((instruction store-local-instruction))
  (list (store-local-value instruction)))

(defmethod instruction-outputs ((instruction store-local-instruction))
  (list))

(defmethod replace-all-registers ((instruction store-local-instruction) substitution-function)
  (setf (store-local-value instruction) (funcall substitution-function (store-local-value instruction))))

(defmethod multiple-value-safe-p ((instruction store-local-instruction) architecture)
  t)

(defmethod print-instruction ((instruction store-local-instruction))
  (format t "   ~S~%"
          `(:store-local ,(store-local-local instruction)
                         ,(store-local-value instruction))))

(defclass move-instruction (backend-instruction)
  ((%destination :initarg :destination :accessor move-destination)
   (%source :initarg :source :accessor move-source))
  (:documentation "Set the destination register to value of the source register."))

(defmethod instruction-inputs ((instruction move-instruction))
  (list (move-source instruction)))

(defmethod instruction-outputs ((instruction move-instruction))
  (list (move-destination instruction)))

(defmethod replace-all-registers ((instruction move-instruction) substitution-function)
  (setf (move-destination instruction) (funcall substitution-function (move-destination instruction)))
  (setf (move-source instruction) (funcall substitution-function (move-source instruction))))

(defmethod multiple-value-safe-p ((instruction move-instruction) architecture)
  t)

(defmethod print-instruction ((instruction move-instruction))
  (format t "   ~S~%"
          `(:move ,(move-destination instruction)
                  ,(move-source instruction))))

(defmethod instruction-pure-p ((instruction move-instruction))
  t)

(defclass swap-instruction (backend-instruction)
  ((%lhs :initarg :lhs :accessor swap-lhs)
   (%rhs :initarg :rhs :accessor swap-rhs))
  (:documentation "Swap the values in the LHS & RHS registers."))

(defmethod instruction-inputs ((instruction swap-instruction))
  (list (swap-lhs instruction) (swap-rhs instruction)))

(defmethod instruction-outputs ((instruction swap-instruction))
  (list (swap-lhs instruction) (swap-rhs instruction)))

(defmethod replace-all-registers ((instruction swap-instruction) substitution-function)
  (setf (swap-lhs instruction) (funcall substitution-function (swap-lhs instruction)))
  (setf (swap-rhs instruction) (funcall substitution-function (swap-rhs instruction))))

(defmethod print-instruction ((instruction swap-instruction))
  (format t "   ~S~%"
          `(:swap ,(swap-lhs instruction)
                  ,(swap-rhs instruction))))

(defmethod instruction-pure-p ((instruction swap-instruction))
  t)

(defclass spill-instruction (backend-instruction)
  ((%destination :initarg :destination :accessor spill-destination)
   (%source :initarg :source :accessor spill-source))
  (:documentation "Store a physical register into a virtual register's spill slot."))

(defmethod instruction-inputs ((instruction spill-instruction))
  (list (spill-source instruction)))

(defmethod instruction-outputs ((instruction spill-instruction))
  (list (spill-destination instruction)))

(defmethod multiple-value-safe-p ((instruction spill-instruction) architecture)
  t)

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

(defmethod multiple-value-safe-p ((instruction fill-instruction) architecture)
  t)

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

(defmethod replace-all-registers ((instruction constant-instruction) substitution-function)
  (setf (constant-destination instruction) (funcall substitution-function (constant-destination instruction))))

(defmethod multiple-value-safe-p ((instruction constant-instruction) architecture)
  t)

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

(defmethod replace-all-registers ((instruction values-instruction) substitution-function)
  (setf (values-values instruction) (mapcar substitution-function (values-values instruction))))

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

(defmethod replace-all-registers ((instruction multiple-value-bind-instruction) substitution-function)
  (setf (multiple-value-bind-values instruction) (mapcar substitution-function (multiple-value-bind-values instruction))))

(defmethod print-instruction ((instruction multiple-value-bind-instruction))
  (format t "   ~S~%"
          `(:multiple-value-bind ,@(multiple-value-bind-values instruction))))

(defmethod consumes-multiple-p ((instruction multiple-value-bind-instruction))
  t)

(defclass save-multiple-instruction (backend-instruction)
  ()
  (:documentation "Save current multiple values."))

(defmethod instruction-inputs ((instruction save-multiple-instruction))
  (list))

(defmethod instruction-outputs ((instruction save-multiple-instruction))
  (list))

(defmethod replace-all-registers ((instruction save-multiple-instruction) substitution-function)
  nil)

(defmethod print-instruction ((instruction save-multiple-instruction))
  (format t "   ~S~%"
          `(:save-multiple ,instruction)))

(defmethod consumes-multiple-p ((instruction save-multiple-instruction))
  t)

(defclass restore-multiple-instruction (backend-instruction)
  ((%context :initarg :context :accessor restore-multiple-context))
  (:documentation "Restore current multiple values. Must be matched in a stack-like way with a save instruction."))

(defmethod instruction-inputs ((instruction restore-multiple-instruction))
  (list))

(defmethod instruction-outputs ((instruction restore-multiple-instruction))
  (list))

(defmethod replace-all-registers ((instruction restore-multiple-instruction) substitution-function)
  nil)

(defmethod print-instruction ((instruction restore-multiple-instruction))
  (format t "   ~S~%"
          `(:restore-multiple ,(restore-multiple-context instruction))))

(defmethod produces-multiple-p ((instruction restore-multiple-instruction))
  t)

(defclass forget-multiple-instruction (backend-instruction)
  ((%context :initarg :context :accessor forget-multiple-context))
  (:documentation "Forget an unused set of saved multiple values."))

(defmethod instruction-inputs ((instruction forget-multiple-instruction))
  (list))

(defmethod instruction-outputs ((instruction forget-multiple-instruction))
  (list))

(defmethod replace-all-registers ((instruction forget-multiple-instruction) substitution-function)
  nil)

(defmethod multiple-value-safe-p ((instruction forget-multiple-instruction) architecture)
  t)

(defmethod print-instruction ((instruction forget-multiple-instruction))
  (format t "   ~S~%"
          `(:forget-multiple ,(forget-multiple-context instruction))))

(defclass jump-instruction (terminator-instruction)
  ((%target :initarg :target :accessor jump-target)
   (%values :initarg :values :accessor jump-values))
  (:documentation "Unconditionally transfer control to the target label.")
  (:default-initargs :values '()))

(defmethod instruction-inputs ((instruction jump-instruction))
  (jump-values instruction))

(defmethod instruction-outputs ((instruction jump-instruction))
  (list))

(defmethod successors (function (instruction jump-instruction))
  (list (jump-target instruction)))

(defmethod replace-all-registers ((instruction jump-instruction) substitution-function)
  (setf (jump-values instruction) (mapcar substitution-function (jump-values instruction))))

(defmethod multiple-value-safe-p ((instruction jump-instruction) architecture)
  t)

(defmethod print-instruction ((instruction jump-instruction))
  (format t "   ~S~%"
          `(:jump ,(jump-target instruction) ,(jump-values instruction))))

(defclass branch-instruction (terminator-instruction)
  ((%value :initarg :value :accessor branch-value)
   (%true-target :initarg :true-target :accessor branch-true-target)
   (%false-target :initarg :false-target :accessor branch-false-target))
  (:documentation "Conditionally transfer control to the target label."))

(defmethod instruction-inputs ((instruction branch-instruction))
  (list (branch-value instruction)))

(defmethod instruction-outputs ((instruction branch-instruction))
  (list))

(defmethod replace-all-registers ((instruction branch-instruction) substitution-function)
  (setf (branch-value instruction) (funcall substitution-function (branch-value instruction))))

(defmethod successors (function (instruction branch-instruction))
  (list (branch-true-target instruction)
        (branch-false-target instruction)))

(defmethod print-instruction ((instruction branch-instruction))
  (format t "   ~S~%"
          `(:branch ,(branch-value instruction) ,(branch-true-target instruction) ,(branch-false-target instruction))))

(defclass switch-instruction (terminator-instruction)
  ((%value :initarg :value :accessor switch-value)
   (%targets :initarg :targets :accessor switch-targets))
  (:documentation "Transfer control to the label indexed by the value."))

(defmethod instruction-inputs ((instruction switch-instruction))
  (list (switch-value instruction)))

(defmethod instruction-outputs ((instruction switch-instruction))
  (list))

(defmethod replace-all-registers ((instruction switch-instruction) substitution-function)
  (setf (switch-value instruction) (funcall substitution-function (switch-value instruction))))

(defmethod successors (function (instruction switch-instruction))
  (switch-targets instruction))

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

(defmethod replace-all-registers ((instruction call-instruction) substitution-function)
  (setf (call-result instruction) (funcall substitution-function (call-result instruction)))
  (setf (call-arguments instruction) (mapcar substitution-function (call-arguments instruction))))

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

(defmethod replace-all-registers ((instruction call-multiple-instruction) substitution-function)
  (setf (call-arguments instruction) (mapcar substitution-function (call-arguments instruction))))

(defmethod print-instruction ((instruction call-multiple-instruction))
  (format t "   ~S~%"
          `(:call-multiple ,(call-function instruction) ,(call-arguments instruction))))

(defmethod produces-multiple-p ((instruction call-multiple-instruction))
  t)

(defclass tail-call-instruction (base-call-instruction terminator-instruction)
  ((%function :initarg :function :accessor call-function)
   (%arguments :initarg :arguments :accessor call-arguments)))

(defmethod mezzano.compiler.backend::successors (function (instruction tail-call-instruction))
  '())

(defmethod mezzano.compiler.backend::instruction-inputs ((instruction tail-call-instruction))
  (call-arguments instruction))

(defmethod mezzano.compiler.backend::instruction-outputs ((instruction tail-call-instruction))
  '())

(defmethod mezzano.compiler.backend::replace-all-registers ((instruction tail-call-instruction) substitution-function)
  (setf (call-arguments instruction) (mapcar substitution-function (call-arguments instruction))))

(defmethod mezzano.compiler.backend::print-instruction ((instruction tail-call-instruction))
  (format t "   ~S~%"
          `(:tail-call ,(call-function instruction) ,(call-arguments instruction))))

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

(defmethod replace-all-registers ((instruction funcall-instruction) substitution-function)
  (setf (call-result instruction) (funcall substitution-function (call-result instruction)))
  (setf (call-function instruction) (funcall substitution-function (call-function instruction)))
  (setf (call-arguments instruction) (mapcar substitution-function (call-arguments instruction))))

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

(defmethod replace-all-registers ((instruction funcall-multiple-instruction) substitution-function)
  (setf (call-function instruction) (funcall substitution-function (call-function instruction)))
  (setf (call-arguments instruction) (mapcar substitution-function (call-arguments instruction))))

(defmethod print-instruction ((instruction funcall-multiple-instruction))
  (format t "   ~S~%"
          `(:funcall-multiple ,(call-function instruction) ,(call-arguments instruction))))

(defmethod produces-multiple-p ((instruction funcall-multiple-instruction))
  t)

(defclass tail-funcall-instruction (base-call-instruction terminator-instruction)
  ((%function :initarg :function :accessor call-function)
   (%arguments :initarg :arguments :accessor call-arguments)))

(defmethod successors (function (instruction tail-funcall-instruction))
  '())

(defmethod instruction-inputs ((instruction tail-funcall-instruction))
  (list* (call-function instruction)
         (call-arguments instruction)))

(defmethod instruction-outputs ((instruction tail-funcall-instruction))
  '())

(defmethod replace-all-registers ((instruction tail-funcall-instruction) substitution-function)
  (setf (call-function instruction) (funcall substitution-function (call-function instruction)))
  (setf (call-arguments instruction) (mapcar substitution-function (call-arguments instruction))))

(defmethod print-instruction ((instruction tail-funcall-instruction))
  (format t "   ~S~%"
          `(:tail-funcall ,(call-function instruction) ,(call-arguments instruction))))

(defclass multiple-value-funcall-instruction (base-call-instruction)
  ((%result :initarg :result :accessor call-result)
   (%function :initarg :function :accessor call-function))
  (:documentation "Call the function object with the current multiple values and produce a single result value."))

(defmethod instruction-inputs ((instruction multiple-value-funcall-instruction))
  (list (call-function instruction)))

(defmethod instruction-outputs ((instruction multiple-value-funcall-instruction))
  (list (call-result instruction)))

(defmethod replace-all-registers ((instruction multiple-value-funcall-instruction) substitution-function)
  (setf (call-result instruction) (funcall substitution-function (call-result instruction)))
  (setf (call-function instruction) (funcall substitution-function (call-function instruction))))

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

(defmethod replace-all-registers ((instruction multiple-value-funcall-multiple-instruction) substitution-function)
  (setf (call-function instruction) (funcall substitution-function (call-function instruction))))

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

(defmethod successors (function (instruction return-instruction))
  '())

(defmethod replace-all-registers ((instruction return-instruction) substitution-function)
  (setf (return-value instruction) (funcall substitution-function (return-value instruction))))

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

(defmethod successors (function (instruction return-multiple-instruction))
  '())

(defmethod replace-all-registers ((instruction return-multiple-instruction) substitution-function)
  nil)

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

(defmethod replace-all-registers ((instruction unreachable-instruction) substitution-function)
  nil)

(defmethod successors (function (instruction unreachable-instruction))
  '())

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

(defmethod successors (function (instruction begin-nlx-instruction))
  (append (call-next-method)
          (begin-nlx-targets instruction)))

(defmethod replace-all-registers ((instruction begin-nlx-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction))))

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

(defmethod replace-all-registers ((instruction finish-nlx-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction))))

(defmethod multiple-value-safe-p ((instruction finish-nlx-instruction) architecture)
  t)

(defmethod print-instruction ((instruction finish-nlx-instruction))
  (format t "   ~S~%"
          `(:finish-nlx ,(nlx-context instruction))))

(defclass invoke-nlx-instruction (terminator-instruction)
  ((%context :initarg :context :accessor nlx-context)
   (%index :initarg :index :accessor invoke-nlx-index)
   (%value :initarg :value :accessor invoke-nlx-value))
  (:documentation "Jump to the indexed target in the given context."))

(defmethod instruction-inputs ((instruction invoke-nlx-instruction))
  (list (nlx-context instruction) (invoke-nlx-value instruction)))

(defmethod instruction-outputs ((instruction invoke-nlx-instruction))
  (list))

(defmethod successors (function (instruction invoke-nlx-instruction))
  '())

(defmethod replace-all-registers ((instruction invoke-nlx-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction)))
  (setf (invoke-nlx-value instruction) (funcall substitution-function (invoke-nlx-value instruction))))

(defmethod print-instruction ((instruction invoke-nlx-instruction))
  (format t "   ~S~%"
          `(:invoke-nlx ,(nlx-context instruction) ,(invoke-nlx-index instruction) ,(invoke-nlx-value instruction))))

(defclass invoke-nlx-multiple-instruction (terminator-instruction)
  ((%context :initarg :context :accessor nlx-context)
   (%index :initarg :index :accessor invoke-nlx-index))
  (:documentation "Jump to the indexed target in the given context."))

(defmethod instruction-inputs ((instruction invoke-nlx-multiple-instruction))
  (list (nlx-context instruction)))

(defmethod instruction-outputs ((instruction invoke-nlx-multiple-instruction))
  (list))

(defmethod successors (function (instruction invoke-nlx-multiple-instruction))
  '())

(defmethod replace-all-registers ((instruction invoke-nlx-multiple-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction))))

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

(defmethod replace-all-registers ((instruction nlx-entry-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction)))
  (setf (nlx-entry-value instruction) (funcall substitution-function (nlx-entry-value instruction))))

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

(defmethod replace-all-registers ((instruction nlx-entry-multiple-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction))))

(defmethod print-instruction ((instruction nlx-entry-multiple-instruction))
  (format t "   ~S~%"
          `(:nlx-entry-multiple ,(nlx-context instruction))))

(defmethod produces-multiple-p ((instruction nlx-entry-multiple-instruction))
  t)

(defclass push-special-stack-instruction (backend-instruction)
  ((%a-value :initarg :a-value :accessor push-special-stack-a-value)
   (%b-value :initarg :b-value :accessor push-special-stack-b-value)
   (%frame :initarg :frame :accessor push-special-stack-frame)
   (%tag :initarg :tag :accessor push-special-stack-tag))
  (:default-initargs :tag sys.int::+object-tag-array-t+))

(defmethod instruction-inputs ((instruction push-special-stack-instruction))
  (list (push-special-stack-a-value instruction) (push-special-stack-b-value instruction)))

(defmethod instruction-outputs ((instruction push-special-stack-instruction))
  (list (push-special-stack-frame instruction)))

(defmethod replace-all-registers ((instruction push-special-stack-instruction) substitution-function)
  (setf (push-special-stack-a-value instruction) (funcall substitution-function (push-special-stack-a-value instruction)))
  (setf (push-special-stack-b-value instruction) (funcall substitution-function (push-special-stack-b-value instruction)))
  (setf (push-special-stack-frame instruction) (funcall substitution-function (push-special-stack-frame instruction))))

(defmethod print-instruction ((instruction push-special-stack-instruction))
  (format t "   ~S~%"
          `(:push-special-stack ,(push-special-stack-frame instruction)
                                ,(push-special-stack-a-value instruction)
                                ,(push-special-stack-b-value instruction))))

(defclass flush-binding-cache-entry-instruction (backend-instruction)
  ((%symbol :initarg :symbol :accessor flush-binding-cache-entry-symbol)
   (%new-value :initarg :new-value :accessor flush-binding-cache-entry-new-value)))

(defmethod instruction-inputs ((instruction flush-binding-cache-entry-instruction))
  (list (flush-binding-cache-entry-symbol instruction)
        (flush-binding-cache-entry-new-value instruction)))

(defmethod instruction-outputs ((instruction flush-binding-cache-entry-instruction))
  (list))

(defmethod replace-all-registers ((instruction flush-binding-cache-entry-instruction) substitution-function)
  (setf (flush-binding-cache-entry-symbol instruction) (funcall substitution-function (flush-binding-cache-entry-symbol instruction)))
  (setf (flush-binding-cache-entry-new-value instruction) (funcall substitution-function (flush-binding-cache-entry-new-value instruction))))

(defmethod print-instruction ((instruction flush-binding-cache-entry-instruction))
  (format t "   ~S~%"
          `(:flush-binding-cache-entry ,(flush-binding-cache-entry-symbol instruction)
                                       ,(flush-binding-cache-entry-new-value instruction))))

(defclass unbind-instruction (backend-instruction)
  ())

(defmethod instruction-inputs ((instruction unbind-instruction))
  (list))

(defmethod instruction-outputs ((instruction unbind-instruction))
  (list))

(defmethod replace-all-registers ((instruction unbind-instruction) substitution-function)
  nil)

(defmethod multiple-value-safe-p ((instruction unbind-instruction) architecture)
  t)

(defmethod print-instruction ((instruction unbind-instruction))
  (format t "   ~S~%"
          `(:unbind)))

(defclass disestablish-block-or-tagbody-instruction (backend-instruction)
  ())

(defmethod instruction-inputs ((instruction disestablish-block-or-tagbody-instruction))
  (list))

(defmethod instruction-outputs ((instruction disestablish-block-or-tagbody-instruction))
  (list))

(defmethod replace-all-registers ((instruction disestablish-block-or-tagbody-instruction) substitution-function)
  nil)

(defmethod multiple-value-safe-p ((instruction disestablish-block-or-tagbody-instruction) architecture)
  t)

(defmethod print-instruction ((instruction disestablish-block-or-tagbody-instruction))
  (format t "   ~S~%"
          `(:disestablish-block-or-tagbody)))

(defclass disestablish-unwind-protect-instruction (backend-instruction)
  ())

(defmethod instruction-inputs ((instruction disestablish-unwind-protect-instruction))
  (list))

(defmethod instruction-outputs ((instruction disestablish-unwind-protect-instruction))
  (list))

(defmethod replace-all-registers ((instruction disestablish-unwind-protect-instruction) substitution-function)
  nil)

(defmethod print-instruction ((instruction disestablish-unwind-protect-instruction))
  (format t "   ~S~%"
          `(:disestablish-unwind-protect)))

(defclass make-dx-simple-vector-instruction (backend-instruction)
  ((%result :initarg :result :accessor make-dx-simple-vector-result)
   (%size :initarg :size :accessor make-dx-simple-vector-size)))

(defmethod instruction-inputs ((instruction make-dx-simple-vector-instruction))
  (list))

(defmethod instruction-outputs ((instruction make-dx-simple-vector-instruction))
  (list (make-dx-simple-vector-result instruction)))

(defmethod replace-all-registers ((instruction make-dx-simple-vector-instruction) substitution-function)
  (setf (make-dx-simple-vector-result instruction) (funcall substitution-function (make-dx-simple-vector-result instruction))))

(defmethod print-instruction ((instruction make-dx-simple-vector-instruction))
  (format t "   ~S~%"
          `(:make-dx-simple-vector
            ,(make-dx-simple-vector-result instruction)
            ,(make-dx-simple-vector-size instruction))))

(defmethod instruction-pure-p ((instruction make-dx-simple-vector-instruction))
  t)

(defclass make-dx-typed-vector-instruction (backend-instruction)
  ((%result :initarg :result :accessor make-dx-typed-vector-result)
   (%size :initarg :size :accessor make-dx-typed-vector-size)
   (%type :initarg :type :accessor make-dx-typed-vector-type)
   (%zero-fill-p :initarg :zero-fill-p :accessor make-dx-typed-vector-zero-fill-p)))

(defmethod instruction-inputs ((instruction make-dx-typed-vector-instruction))
  (list))

(defmethod instruction-outputs ((instruction make-dx-typed-vector-instruction))
  (list (make-dx-typed-vector-result instruction)))

(defmethod replace-all-registers ((instruction make-dx-typed-vector-instruction) substitution-function)
  (setf (make-dx-typed-vector-result instruction) (funcall substitution-function (make-dx-typed-vector-result instruction))))

(defmethod print-instruction ((instruction make-dx-typed-vector-instruction))
  (format t "   ~S~%"
          `(:make-dx-typed-vector
            ,(make-dx-typed-vector-result instruction)
            ,(make-dx-typed-vector-size instruction)
            ,(make-dx-typed-vector-type instruction)
            ,(make-dx-typed-vector-zero-fill-p instruction))))

(defmethod instruction-pure-p ((instruction make-dx-typed-vector-instruction))
  t)

(defclass make-dx-cons-instruction (backend-instruction)
  ((%result :initarg :result :accessor make-dx-cons-result)))

(defmethod instruction-inputs ((instruction make-dx-cons-instruction))
  (list))

(defmethod instruction-outputs ((instruction make-dx-cons-instruction))
  (list (make-dx-cons-result instruction)))

(defmethod replace-all-registers ((instruction make-dx-cons-instruction) substitution-function)
  (setf (make-dx-cons-result instruction) (funcall substitution-function (make-dx-cons-result instruction))))

(defmethod print-instruction ((instruction make-dx-cons-instruction))
  (format t "   ~S~%"
          `(:make-dx-cons
            ,(make-dx-cons-result instruction))))

(defmethod instruction-pure-p ((instruction make-dx-cons-instruction))
  t)

;; TODO: Support arbitrary environments.
(defclass make-dx-closure-instruction (backend-instruction)
  ((%result :initarg :result :accessor make-dx-closure-result)
   (%function :initarg :function :accessor make-dx-closure-function)
   (%environment :initarg :environment :accessor make-dx-closure-environment)))

(defmethod instruction-inputs ((instruction make-dx-closure-instruction))
  (list (make-dx-closure-function instruction)
        (make-dx-closure-environment instruction)))

(defmethod instruction-outputs ((instruction make-dx-closure-instruction))
  (list (make-dx-closure-result instruction)))

(defmethod replace-all-registers ((instruction make-dx-closure-instruction) substitution-function)
  (setf (make-dx-closure-result instruction) (funcall substitution-function (make-dx-closure-result instruction)))
  (setf (make-dx-closure-function instruction) (funcall substitution-function (make-dx-closure-function instruction)))
  (setf (make-dx-closure-environment instruction) (funcall substitution-function (make-dx-closure-environment instruction))))

(defmethod print-instruction ((instruction make-dx-closure-instruction))
  (format t "   ~S~%"
          `(:make-dx-closure
            ,(make-dx-closure-result instruction)
            ,(make-dx-closure-function instruction)
            ,(make-dx-closure-environment instruction))))

(defmethod instruction-pure-p ((instruction make-dx-closure-instruction))
  t)

;;; Instructions for boxing/unboxing.

(defclass box-instruction (backend-instruction)
  ((%destination :initarg :destination :accessor box-destination)
   (%source :initarg :source :accessor box-source))
  (:documentation "Box a native value into a Lisp value."))

(defmethod instruction-inputs ((instruction box-instruction))
  (list (box-source instruction)))

(defmethod instruction-outputs ((instruction box-instruction))
  (list (box-destination instruction)))

(defmethod replace-all-registers ((instruction box-instruction) substitution-function)
  (setf (box-destination instruction) (funcall substitution-function (box-destination instruction)))
  (setf (box-source instruction) (funcall substitution-function (box-source instruction))))

(defmethod instruction-pure-p ((instruction box-instruction))
  t)

(defclass box-fixnum-instruction (box-instruction)
  ())

(defmethod box-type ((instruction box-fixnum-instruction))
  'fixnum)

(defmethod print-instruction ((instruction box-fixnum-instruction))
  (format t "   ~S~%"
          `(:box-fixnum
            ,(box-destination instruction)
            ,(box-source instruction))))

(defclass box-unsigned-byte-64-instruction (box-instruction)
  ())

(defmethod box-type ((instruction box-unsigned-byte-64-instruction))
  :unsigned-byte-64)

(defmethod print-instruction ((instruction box-unsigned-byte-64-instruction))
  (format t "   ~S~%"
          `(:box-unsigned-byte-64
            ,(box-destination instruction)
            ,(box-source instruction))))

(defclass box-signed-byte-64-instruction (box-instruction)
  ())

(defmethod box-type ((instruction box-signed-byte-64-instruction))
  :signed-byte-64)

(defmethod print-instruction ((instruction box-signed-byte-64-instruction))
  (format t "   ~S~%"
          `(:box-signed-byte-64
            ,(box-destination instruction)
            ,(box-source instruction))))

(defclass box-single-float-instruction (box-instruction)
  ())

(defmethod box-type ((instruction box-single-float-instruction))
  'single-float)

(defmethod print-instruction ((instruction box-single-float-instruction))
  (format t "   ~S~%"
          `(:box-single-float
            ,(box-destination instruction)
            ,(box-source instruction))))

(defclass box-double-float-instruction (box-instruction)
  ())

(defmethod box-type ((instruction box-double-float-instruction))
  'double-float)

(defmethod print-instruction ((instruction box-double-float-instruction))
  (format t "   ~S~%"
          `(:box-double-float
            ,(box-destination instruction)
            ,(box-source instruction))))

(defclass unbox-instruction (backend-instruction)
  ((%destination :initarg :destination :accessor unbox-destination)
   (%source :initarg :source :accessor unbox-source))
  (:documentation "Unbox a Lisp value into it's native representation."))

(defmethod instruction-inputs ((instruction unbox-instruction))
  (list (unbox-source instruction)))

(defmethod instruction-outputs ((instruction unbox-instruction))
  (list (unbox-destination instruction)))

(defmethod replace-all-registers ((instruction unbox-instruction) substitution-function)
  (setf (unbox-destination instruction) (funcall substitution-function (unbox-destination instruction)))
  (setf (unbox-source instruction) (funcall substitution-function (unbox-source instruction))))

(defmethod instruction-pure-p ((instruction unbox-instruction))
  t)

(defclass unbox-fixnum-instruction (unbox-instruction)
  ())

(defmethod box-type ((instruction unbox-fixnum-instruction))
  'fixnum)

(defmethod print-instruction ((instruction unbox-fixnum-instruction))
  (format t "   ~S~%"
          `(:unbox-fixnum
            ,(unbox-destination instruction)
            ,(unbox-source instruction))))

(defclass unbox-unsigned-byte-64-instruction (unbox-instruction)
  ())

(defmethod box-type ((instruction unbox-unsigned-byte-64-instruction))
  :unsigned-byte-64)

(defmethod print-instruction ((instruction unbox-unsigned-byte-64-instruction))
  (format t "   ~S~%"
          `(:unbox-unsigned-byte-64
            ,(unbox-destination instruction)
            ,(unbox-source instruction))))

(defclass unbox-signed-byte-64-instruction (unbox-instruction)
  ())

(defmethod box-type ((instruction unbox-signed-byte-64-instruction))
  :signed-byte-64)

(defmethod print-instruction ((instruction unbox-signed-byte-64-instruction))
  (format t "   ~S~%"
          `(:unbox-signed-byte-64
            ,(unbox-destination instruction)
            ,(unbox-source instruction))))

(defclass unbox-single-float-instruction (unbox-instruction)
  ())

(defmethod box-type ((instruction unbox-single-float-instruction))
  'single-float)

(defmethod print-instruction ((instruction unbox-single-float-instruction))
  (format t "   ~S~%"
          `(:unbox-single-float
            ,(unbox-destination instruction)
            ,(unbox-source instruction))))

(defclass unbox-double-float-instruction (unbox-instruction)
  ())

(defmethod box-type ((instruction unbox-double-float-instruction))
  'double-float)

(defmethod print-instruction ((instruction unbox-double-float-instruction))
  (format t "   ~S~%"
          `(:unbox-double-float
            ,(unbox-destination instruction)
            ,(unbox-source instruction))))

;;; Instruction-level debug info.

(defclass debug-instruction (backend-instruction) ())

(defmethod multiple-value-safe-p ((instruction debug-instruction) architecture)
  t)

(defclass debug-bind-variable-instruction (debug-instruction)
  ((%variable :initarg :variable :accessor debug-variable)
   (%value :initarg :value :accessor debug-value)
   (%representation :initarg :representation :accessor debug-representation))
  (:documentation "Update a variable's value in the debug info.")
  (:default-initargs :representation :value))

(defmethod instruction-inputs ((instruction debug-bind-variable-instruction))
  (list (debug-value instruction)))

(defmethod instruction-outputs ((instruction debug-bind-variable-instruction))
  '())

(defmethod replace-all-registers ((instruction debug-bind-variable-instruction) substitution-function)
  (setf (debug-value instruction) (funcall substitution-function (debug-value instruction))))

(defmethod print-instruction ((instruction debug-bind-variable-instruction))
  (format t "   ~S~%"
          `(:debug-bind-variable
            ,(debug-variable instruction)
            ,(debug-value instruction)
            ,(debug-representation instruction))))

(defclass debug-unbind-variable-instruction (debug-instruction)
  ((%variable :initarg :variable :accessor debug-variable))
  (:documentation "Update a variable's value in the debug info."))

(defmethod instruction-inputs ((instruction debug-unbind-variable-instruction))
  '())

(defmethod instruction-outputs ((instruction debug-unbind-variable-instruction))
  '())

(defmethod replace-all-registers ((instruction debug-unbind-variable-instruction) substitution-function)
  nil)

(defmethod print-instruction ((instruction debug-unbind-variable-instruction))
  (format t "   ~S~%"
          `(:debug-unbind-variable
            ,(debug-variable instruction))))

(defclass debug-update-variable-instruction (debug-instruction)
  ((%variable :initarg :variable :accessor debug-variable)
   (%value :initarg :value :accessor debug-value)
   (%representation :initarg :representation :accessor debug-representation))
  (:documentation "Update a variable's value in the debug info.")
  (:default-initargs :representation :value))

(defmethod instruction-inputs ((instruction debug-update-variable-instruction))
  (list (debug-value instruction)))

(defmethod instruction-outputs ((instruction debug-update-variable-instruction))
  '())

(defmethod replace-all-registers ((instruction debug-update-variable-instruction) substitution-function)
  (setf (debug-value instruction) (funcall substitution-function (debug-value instruction))))

(defmethod print-instruction ((instruction debug-update-variable-instruction))
  (format t "   ~S~%"
          `(:debug-update-variable
            ,(debug-variable instruction)
            ,(debug-value instruction)
            ,(debug-representation instruction))))

;;; Extend life of a value.

(defclass spice-instruction (backend-instruction)
  ((%value :initarg :value :accessor spice-value)))

(defmethod instruction-inputs ((instruction spice-instruction))
  (list (spice-value instruction)))

(defmethod instruction-outputs ((instruction spice-instruction))
  '())

(defmethod replace-all-registers ((instruction spice-instruction) substitution-function)
  (setf (spice-value instruction) (funcall substitution-function (spice-value instruction))))

(defmethod print-instruction ((instruction spice-instruction))
  (format t "   ~S~%"
          `(:spice
            ,(spice-value instruction))))
