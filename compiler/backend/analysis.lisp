;;;; Basic IR analysis passes.

(in-package :mezzano.compiler.backend)

(defun set-equal (list-1 list-2)
  (and (dolist (elt list-1 t)
         (when (not (member elt list-2))
           (return nil)))
       (dolist (elt list-2 t)
         (when (not (member elt list-1))
           (return nil)))))

(defun compute-actual-successors (backend-function)
  ;; Add control edges from calls/invoke-nlx instructions to all live NLX thunks.
  (let ((actual-successors (make-hash-table :test 'eq))
        (additional-successors '())
        (dc (dynamic-contours backend-function))
        (current-bb (first-instruction backend-function)))
    (do-instructions (inst backend-function)
      (when (typep inst '(or base-call-instruction invoke-nlx-instruction invoke-nlx-multiple-instruction))
        (dolist (c (gethash inst dc))
          (when (typep c 'begin-nlx-instruction)
            (setf additional-successors (union additional-successors
                                               (begin-nlx-targets c))))))
      (when (typep inst 'terminator-instruction)
        (setf (gethash current-bb actual-successors) (append (successors backend-function inst)
                                                             additional-successors)
              (gethash inst actual-successors) (gethash current-bb actual-successors)
              additional-successors '()))
      (when (typep inst 'label)
        (setf current-bb inst)))
    actual-successors))

(defun compute-basic-block-input-output-sets (backend-function mv-regs)
  ;; Produce input/output sets for basic blocks.
  (let ((bb-inputs (make-hash-table :test 'eq))
        (bb-outputs (make-hash-table :test 'eq))
        (terminator-to-bb (make-hash-table :test 'eq))
        (basic-blocks '())
        (current-inputs '())
        (current-outputs '())
        (terminator nil))
    (do-reversed-instructions (inst backend-function)
      (let ((uses (remove-duplicates (instruction-inputs inst)))
            (defs (remove-duplicates (instruction-outputs inst))))
        (when (produces-multiple-p inst)
          (setf defs (union defs mv-regs)))
        (when (consumes-multiple-p inst)
          (setf uses (union uses mv-regs)))
        ;; Remove defs from inputs.
        (setf current-inputs (set-difference current-inputs defs))
        ;; Add defs to outputs.
        (setf current-outputs (union current-outputs defs))
        ;; Add uses to inputs.
        (setf current-inputs (union current-inputs uses))
        (when (typep inst '(or label argument-setup-instruction))
          ;; Reached start of this basic block, finish up.
          (setf (gethash inst bb-inputs) current-inputs
                (gethash inst bb-outputs) current-outputs
                (gethash terminator terminator-to-bb) inst)
          (push inst basic-blocks)
          (setf current-inputs '()
                current-outputs '()))
        (when (typep inst 'terminator-instruction)
          (setf terminator inst))))
    (values bb-inputs bb-outputs basic-blocks terminator-to-bb)))

(defun compute-liveness (backend-function target)
  (let ((live-in (make-hash-table :test 'eq))
        (live-in* (make-hash-table :test 'eq))
        (live-out (make-hash-table :test 'eq))
        (live-out* (make-hash-table :test 'eq))
        (actual-successors (compute-actual-successors backend-function))
        (mv-regs (list* (mezzano.compiler.backend.register-allocator:target-count-register target)
                        (mezzano.compiler.backend.register-allocator:target-argument-registers target))))
    (multiple-value-bind (bb-inputs bb-outputs basic-blocks terminator-to-bb)
        (compute-basic-block-input-output-sets backend-function mv-regs)
      ;; Iterate basic-blocks in reverse order, this distantly approximates
      ;; a reverse post-order traversal of the CFG.
      (setf basic-blocks (reverse basic-blocks))
      ;; Iteratively solve for liveness over basic blocks.
      ;; in[n] = use[n] U (out[n] - def[n])
      ;; out[n] = union in[s] forall s in successors[n]
      (loop
         (dolist (inst basic-blocks)
           (setf (gethash inst live-in*) (gethash inst live-in)
                 (gethash inst live-out*) (gethash inst live-out))
           (let ((uses (gethash inst bb-inputs))
                 (defs (gethash inst bb-outputs)))
             (setf (gethash inst live-out) '())
             (dolist (succ (gethash inst actual-successors))
               (setf (gethash inst live-out) (union (gethash inst live-out)
                                                    (gethash succ live-in))))
             (setf (gethash inst live-in) (union uses
                                                 (set-difference (gethash inst live-out)
                                                                 defs)))))
         (when (dolist (inst basic-blocks t)
                 (when (not (and (set-equal (gethash inst live-in) (gethash inst live-in*))
                                 (set-equal (gethash inst live-out) (gethash inst live-out*))))
                   (return nil)))
           (return)))
      ;; Compute liveness for individual instructions.
      ;; This requires no iteration as control flow in a basic block is linear.
      (do-reversed-instructions (inst backend-function)
        (let ((uses (instruction-inputs inst))
              (defs (instruction-outputs inst)))
          (when (produces-multiple-p inst)
            (setf defs (union defs mv-regs)))
          (when (consumes-multiple-p inst)
            (setf uses (union uses mv-regs)))
          (typecase inst
            (terminator-instruction
             ;; Use live-out information from the basic block.
             (setf (gethash inst live-out)
                   (gethash (gethash inst terminator-to-bb) live-out)))
            (t
             (setf (gethash inst live-out)
                   (gethash (next-instruction backend-function inst) live-in))))
          (setf (gethash inst live-in) (union uses
                                              (set-difference (gethash inst live-out)
                                                              defs)))))
      (values live-in live-out))))

(defun build-use/def-maps (backend-function)
  (let ((uses (make-hash-table :test 'eq))
        (defs (make-hash-table :test 'eq)))
    (do-instructions (inst backend-function)
      (dolist (use (instruction-inputs inst))
        (when (typep use 'virtual-register)
          (pushnew inst (gethash use uses '()))))
      (dolist (def (instruction-outputs inst))
        (when (typep def 'virtual-register)
          (pushnew inst (gethash def defs '())))))
    (values uses defs)))

(defun dynamic-contours (function)
  (let ((contour (make-hash-table :test 'eq))
        (worklist (list (cons (first-instruction function) '()))))
    (loop
       (when (endp worklist)
         (return))
       (let* ((entry (pop worklist))
              (inst (car entry))
              (stack (cdr entry)))
         (setf (gethash inst contour) stack)
         (typecase inst
           (begin-nlx-instruction
            (push inst stack))
           (finish-nlx-instruction
            (assert (eql (nlx-region inst) (first stack)))
            (pop stack))
           (save-multiple-instruction
            (push inst stack))
           (restore-multiple-instruction
            (assert (eql (restore-multiple-context inst) (first stack)))
            (pop stack))
           (forget-multiple-instruction
            (assert (eql (forget-multiple-context inst) (first stack)))
            (pop stack))
           (bind-local-instruction
            (push inst stack))
           (unbind-local-instruction
            (assert (eql (unbind-local-local inst)
                         (first stack)))
            (pop stack))
           (load-local-instruction
            (assert (member (load-local-local inst) stack)))
           (store-local-instruction
            (assert (member (store-local-local inst) stack)))
           (debug-bind-variable-instruction
            (assert (not (member (debug-variable inst) stack)))
            (push (debug-variable inst) stack))
           (debug-update-variable-instruction
            (assert (member (debug-variable inst) stack)))
           (debug-unbind-variable-instruction
            (assert (eql (debug-variable inst) (first stack)))
            (pop stack)))
         (dolist (next (successors function inst))
           (multiple-value-bind (next-stack visitedp)
               (gethash next contour)
             (cond
               (visitedp
                (assert (eql stack next-stack)))
               (t
                (push (cons next stack) worklist)))))))
    contour))
