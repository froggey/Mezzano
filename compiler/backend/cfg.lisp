;;;; CFG-related functions.

(in-package :mezzano.compiler.backend)

(defun build-cfg (backend-function)
  (let ((predecessors (make-hash-table :test 'eq))
        (successors (make-hash-table :test 'eq))
        (basic-blocks (list (first-instruction backend-function)))
        (active-bb (first-instruction backend-function))
        (targets '()))
    (check-type active-bb argument-setup-instruction)
    (do-instructions (inst backend-function)
      (typecase inst
        (terminator-instruction
         (dolist (succ (successors backend-function inst))
           (assert (typep succ 'label))
           (pushnew succ targets)
           (pushnew succ (gethash active-bb successors))
           (pushnew active-bb (gethash succ predecessors)))
         (setf active-bb (next-instruction backend-function inst))
         (when active-bb
           (push active-bb basic-blocks)))
        (begin-nlx-instruction
         (dolist (succ (begin-nlx-targets inst))
           (assert (typep succ 'label))
           (pushnew succ targets)
           (pushnew succ (gethash active-bb successors))
           (pushnew active-bb (gethash succ predecessors))))))
    (dolist (targ targets)
      (assert (member targ basic-blocks)))
    (values (reverse basic-blocks) predecessors successors)))

(defun discover-reachable-basic-blocks (backend-function bb-successors)
  ;; Search from the entry point to discover reachable basic blocks.
  (let ((visited '())
        (worklist (list (first-instruction backend-function))))
    (loop
       (when (endp worklist)
         (return))
       (let ((block (pop worklist)))
         (push block visited)
         (dolist (succ (gethash block bb-successors))
           (when (not (member succ visited))
             (pushnew succ worklist)))))
    visited))

(defun remove-basic-block (backend-function basic-block)
  (let ((inst basic-block))
    (loop
       (let ((next (next-instruction backend-function inst)))
         (remove-instruction backend-function inst)
         (when (typep inst 'terminator-instruction)
           (return))
         (setf inst next)))))

(defun remove-unreachable-basic-blocks (backend-function)
  "Eliminate unreachable basic blocks.
This is required for register allocation, as the allocator
does not visit unreachable blocks."
  (multiple-value-bind (basic-blocks bb-preds bb-succs)
      (build-cfg backend-function)
    (declare (ignore bb-preds))
    (let* ((reachable (discover-reachable-basic-blocks backend-function bb-succs))
           (unreachable (set-difference basic-blocks reachable)))
      (dolist (bb unreachable)
        (remove-basic-block backend-function bb))
      unreachable)))

(defun simplify-cfg-1 (backend-function)
  "Eliminate jumps and branches to jumps."
  (let ((total 0))
    (do-instructions (inst backend-function)
      (typecase inst
        (branch-instruction
         (let* ((true-target (branch-true-target inst))
                (true-next (next-instruction backend-function true-target))
                (false-target (branch-false-target inst))
                (false-next (next-instruction backend-function false-target)))
           (when (and (typep true-next 'jump-instruction)
                      ;; Don't snap if there are phi nodes at the target.
                      (endp (jump-values true-next))
                      (not (eql (jump-target true-next) true-target)))
             (setf true-target (jump-target true-next)
                   (branch-true-target inst) true-target)
             (incf total))
           (when (and (typep false-next 'jump-instruction)
                      ;; Don't snap if there are phi nodes at the target.
                      (endp (jump-values false-next))
                      (not (eql (jump-target false-next) false-target)))
             (setf false-target (jump-target false-next)
                   (branch-false-target inst) false-target)
             (incf total))
           (when (eql true-target false-target)
             (change-class inst
                           'jump-instruction
                           :target true-target
                           :values '())
             (incf total))))
        (mezzano.compiler.backend.x86-64::x86-branch-instruction
         (let* ((true-target (mezzano.compiler.backend.x86-64::x86-branch-true-target inst))
                (true-next (next-instruction backend-function true-target))
                (false-target (mezzano.compiler.backend.x86-64::x86-branch-false-target inst))
                (false-next (next-instruction backend-function false-target)))
           (when (and (typep true-next 'jump-instruction)
                      ;; Don't snap if there are phi nodes at the target.
                      (endp (jump-values true-next))
                      (not (eql (jump-target true-next) true-target)))
             (setf (mezzano.compiler.backend.x86-64::x86-branch-true-target inst) (jump-target true-next))
             (incf total))
           (when (and (typep false-next 'jump-instruction)
                       ;; Don't snap if there are phi nodes at the target.
                      (endp (jump-values false-next))
                      (not (eql (jump-target false-next) false-target)))
             (setf (mezzano.compiler.backend.x86-64::x86-branch-false-target inst) (jump-target false-next))
             (incf total))))
        (jump-instruction
         (when (endp (jump-values inst))
           (let* ((target (jump-target inst))
                  (target-next (next-instruction backend-function target)))
             (cond ((and (typep target-next 'jump-instruction)
                         ;; Don't snap if there are phi nodes at the target.
                         (endp (jump-values target-next))
                         (not (eql (jump-target inst) (jump-target target-next))))
                    (setf (jump-target inst) (jump-target target-next))
                    (incf total))
                   ((typep target 'branch-instruction)
                    ;; Convert to branch.
                    (change-class inst
                                  'branch-instruction
                                  :value (branch-value target)
                                  :true-target (branch-true-target target-next)
                                  :false-target (branch-false-target target-next))
                    (incf total))))))))
    total))

(defun branch-pushback (backend-function uses)
  "Convert jumps to branches with single-use phi variables to just the branches."
  ;; Before:
  ;;   jump foo(val)
  ;;  foo(phi): [where phi is only used by the branch]
  ;;   branch phi target
  ;; After:
  ;;   branch val target
  ;;  tmp():
  ;;   jump foo2()
  ;;  foo2():
  (let ((total 0))
    (do-instructions (inst backend-function)
      (when (and (typep inst 'jump-instruction)
                 ;; Jump to label with one phi.
                 (eql (length (jump-values inst)) 1)
                 ;; Instruction following label is a branch...
                 (typep (next-instruction backend-function (jump-target inst))
                        'branch-instruction)
                 ;; ... that branches on the label's phi.
                 (eql (branch-value (next-instruction backend-function (jump-target inst)))
                      (first (label-phis (jump-target inst))))
                 ;; And that phi is only used by the branch.
                 (equal (gethash (first (label-phis (jump-target inst))) uses)
                        (list (next-instruction backend-function (jump-target inst)))))
        (let* ((target-branch (next-instruction backend-function (jump-target inst)))
               (branch-value (first (jump-values inst)))
               (new-label (next-instruction backend-function target-branch))
               (new-branch (insert-before backend-function inst
                                          (make-instance 'branch-instruction
                                                         :value branch-value
                                                         :true-target (branch-true-target target-branch)
                                                         :false-target (branch-false-target target-branch)))))
          (insert-before backend-function inst (make-instance 'label))
          (setf (jump-target inst) new-label)
          (setf (jump-values inst) '())
          ;; Update the use map.
          (push new-branch (gethash branch-value uses))
          (setf (gethash branch-value uses) (remove inst (gethash branch-value uses)))
          (incf total))))
    total))

(defun remove-trivially-constant-branches (backend-function uses defs)
  "Remove (branch (constant some-constant) target)"
  (let ((total 0)
        (remove-me '()))
    (do-instructions (inst backend-function)
      (when (typep inst 'branch-instruction)
        (let ((value (first (gethash (branch-value inst) defs))))
          (when (typep value 'constant-instruction)
            ;; Branch to the target if true, otherwise the following basic block.
            (insert-before backend-function inst
                           (make-instance 'jump-instruction
                                          :target (if (constant-value value)
                                                      (branch-true-target inst)
                                                      (branch-false-target inst))))
            (push inst remove-me)
            (incf total)))))
    (dolist (inst remove-me)
      (remove-instruction backend-function inst)
      (setf (gethash (branch-value inst) uses) (remove inst (gethash (branch-value inst) uses))))
    total))

(defun simplify-cfg (backend-function)
  (remove-unreachable-basic-blocks backend-function)
  (multiple-value-bind (uses defs)
      (build-use/def-maps backend-function)
    (let ((total 0))
      (loop
         (let ((n (+ (simplify-cfg-1 backend-function)
                     (branch-pushback backend-function uses)
                     (remove-trivially-constant-branches backend-function uses defs)
                     ;; Not technically part of CFG simplification, but helps the other passes.
                     (remove-unused-instructions-1 backend-function uses))))
           (when (zerop n)
             (return))
           (remove-unreachable-basic-blocks backend-function)
           (incf total n)))
      ;; TODO: Break critical edges.
      (check-cfg backend-function)
      total)))

(defun check-cfg (backend-function)
  "Check the validity of the CFG.
All basic blocks must start with a label (except the first, which must start with
an argument-setup instruction) and finish with a terminator instruction.
Successors of jumps and branches must be labels."
  (assert (typep (first-instruction backend-function) 'argument-setup-instruction)
          (backend-function)
          "First instruction must be an ARGUMENT-SETUP instruction.")
  (do ((inst (first-instruction backend-function)
             (next-instruction backend-function inst)))
      ((null inst)
       (error "Missing terminator on last basic block in ~S" backend-function))
    (when (typep inst 'label)
      (assert (typep (prev-instruction backend-function inst) 'terminator-instruction)
              (backend-function inst)
              "Instruction preceeding label ~S is not a terminator"
              inst (prev-instruction backend-function inst)))
    (when (typep inst 'terminator-instruction)
      (dolist (succ (successors backend-function inst))
        (assert (typep succ 'label) (backend-function inst succ)
                "Successor ~S of terminator ~S is not a label."
                succ inst))
      (cond ((null (next-instruction backend-function inst))
             (return))
            (t
             (assert (typep (next-instruction backend-function inst) 'label)
                     (backend-function inst)
                     "Instruction following ~S terminator ~S is not a label"
                     (next-instruction backend-function inst) inst))))))

(defun break-critical-edges (backend-function)
  (multiple-value-bind (basic-blocks bb-preds bb-succs)
      (build-cfg backend-function)
    (declare (ignore basic-blocks bb-succs))
    (flet ((split-edge (bb)
             (let ((new-bb (make-instance 'label :phis () :name nil)))
               (insert-before backend-function bb new-bb)
               (insert-before backend-function bb (make-instance 'jump-instruction :target bb :values '()))
               new-bb)))
      (do-instructions (inst backend-function)
        ;; TODO: Support switches too.
        (when (typep inst 'branch-instruction)
          (when (not (endp (rest (gethash (branch-true-target inst) bb-preds))))
            (when (not *shut-up*)
              (format t "Split edge ~S -> ~S~%" inst (branch-true-target inst)))
            (setf (branch-true-target inst) (split-edge (branch-true-target inst))))
          (when (not (endp (rest (gethash (branch-false-target inst) bb-preds))))
            (when (not *shut-up*)
              (format t "Split edge ~S -> ~S~%" inst (branch-false-target inst)))
            (setf (branch-false-target inst) (split-edge (branch-false-target inst)))))))))
