;;;; Canonicalization for various instructions.

(in-package :mezzano.compiler.backend.register-allocator)

(defun canonicalize-call-operands (backend-function target)
  (let ((arg-regs (target-argument-registers target))
        (return-reg (target-return-register target))
        (funcall-reg (target-funcall-register target)))
    (ir:do-instructions (inst backend-function)
      (flet ((frob-inputs ()
               ;; Insert moves to physical registers.
               (loop
                  for preg in arg-regs
                  for vreg in (ir:call-arguments inst)
                  do
                    (ir:insert-before backend-function inst
                                      (make-instance 'ir:move-instruction
                                                     :destination preg
                                                     :source vreg)))
               ;; Replace input operands with physical registers.
               (setf (ir:call-arguments inst)
                     (append (subseq arg-regs 0
                                     (min (length (ir:call-arguments inst))
                                          (length arg-regs)))
                             (nthcdr (length arg-regs)
                                     (ir:call-arguments inst)))))
             (frob-outputs ()
               ;; TODO: Insert debug variable updates where needed.
               (ir:insert-after backend-function inst
                                (make-instance 'ir:move-instruction
                                               :destination (ir:call-result inst)
                                               :source return-reg))
               (setf (ir:call-result inst) return-reg))
             (frob-function ()
               (ir:insert-before backend-function inst
                                 (make-instance 'ir:move-instruction
                                                :destination funcall-reg
                                                :source (ir:call-function inst)))
               (setf (ir:call-function inst) funcall-reg)))
        (typecase inst
          (ir:call-instruction
           (frob-inputs)
           (frob-outputs))
          (ir:call-multiple-instruction
           (frob-inputs))
          (ir:funcall-instruction
           (frob-inputs)
           (frob-outputs)
           (frob-function))
          (ir:funcall-multiple-instruction
           (frob-inputs)
           (frob-function))
          (ir:multiple-value-funcall-instruction
           (frob-outputs)
           (frob-function))
          (ir:multiple-value-funcall-multiple-instruction
           (frob-function))
          (ir:tail-call-instruction
           (frob-inputs))
          (ir:tail-funcall-instruction
           (frob-inputs)
           (frob-function)))))))

(defun canonicalize-argument-setup (backend-function target)
  (let ((aset (ir:first-instruction backend-function))
        (arg-regs (target-argument-registers target))
        (funcall-regs (target-funcall-register target)))
    ;; Required, optional, and the closure arguments are put in registers, but
    ;; count is forced to be spilled. rcx (count) and r13 are
    ;; required by the &rest-list construction code.
    (ir:insert-after backend-function aset
                     (make-instance 'ir:move-instruction
                                    :destination (ir:argument-setup-closure aset)
                                    :source funcall-regs))
    (setf (ir:argument-setup-closure aset) funcall-regs)
    ;; Even though the rest list is generated in r13 this code does not
    ;; emit it in r13, choosing to keep it in a vreg and spill it.
    ;; This is due to the fact that there is no way to communicate usage
    ;; of the rest list to the final code emitter.
    (do ((req (ir:argument-setup-required aset) (cdr req)))
        ((or (endp arg-regs)
             (endp req)))
      (let ((reg (pop arg-regs)))
        (ir:insert-after backend-function aset
                         (make-instance 'ir:move-instruction
                                        :destination (car req)
                                        :source reg))
        (setf (car req) reg)))
    (do ((opt (ir:argument-setup-optional aset) (cdr opt)))
        ((or (endp arg-regs)
             (endp opt)))
      (let ((reg (pop arg-regs)))
        (ir:insert-after backend-function aset
                         (make-instance 'ir:move-instruction
                                        :destination (car opt)
                                        :source reg))
        (setf (car opt) reg)))))

(defun canonicalize-nlx-values (backend-function target)
  (let ((return-reg (target-return-register target)))
    (ir:do-instructions (inst backend-function)
      (when (typep inst 'ir:nlx-entry-instruction)
        (ir:insert-after backend-function inst
                         (make-instance 'ir:move-instruction
                                        :destination (ir:nlx-entry-value inst)
                                        :source return-reg))
        (setf (ir:nlx-entry-value inst) return-reg))
      (when (typep inst 'ir:invoke-nlx-instruction)
        (ir:insert-before backend-function inst
                          (make-instance 'ir:move-instruction
                                         :destination return-reg
                                         :source (ir:invoke-nlx-value inst)))
        (setf (ir:invoke-nlx-value inst) return-reg)))))

(defun canonicalize-values (backend-function target)
  (let ((arg-regs (target-argument-registers target))
        (return-reg (target-return-register target)))
    (ir:do-instructions (inst backend-function)
      (when (typep inst 'ir:values-instruction)
        (do ((regs arg-regs (rest regs))
             (values (ir:values-values inst) (rest values)))
            ((or (endp regs)
                 (endp values)))
          (ir:insert-before backend-function inst
                            (make-instance 'ir:move-instruction
                                           :destination (first regs)
                                           :source (first values)))
          (setf (first values) (first regs))))
      (when (typep inst 'ir:multiple-value-bind-instruction)
        (do ((regs arg-regs (rest regs))
             (values (ir:multiple-value-bind-values inst) (rest values)))
            ((or (endp regs)
                 (endp values)))
          (ir:insert-after backend-function inst
                           (make-instance 'ir:move-instruction
                                          :destination (first values)
                                          :source (first regs)))
          (setf (first values) (first regs))))
      (when (typep inst 'ir:return-instruction)
        (ir:insert-before backend-function inst
                          (make-instance 'ir:move-instruction
                                         :destination return-reg
                                         :source (ir:return-value inst)))
        (setf (ir:return-value inst) return-reg)))))
