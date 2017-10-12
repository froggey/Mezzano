;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; CFG-related functions.

(in-package :mezzano.compiler.backend)

(defun build-cfg (backend-function)
  (let ((predecessors (make-hash-table))
        (successors (make-hash-table))
        (basic-blocks (list (first-instruction backend-function)))
        (active-bb (first-instruction backend-function))
        (targets '()))
    (check-type active-bb argument-setup-instruction)
    (do-instructions (inst backend-function)
      (typecase inst
        (terminator-instruction
         (dolist (succ (successors backend-function inst))
           (pushnew succ targets)
           (pushnew succ (gethash active-bb successors))
           (pushnew active-bb (gethash succ predecessors)))
         (setf active-bb (next-instruction backend-function inst))
         (when active-bb
           (push active-bb basic-blocks)))
        (begin-nlx-instruction
         (dolist (succ (begin-nlx-targets inst))
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

(defun skip-label (backend-function inst)
  (loop
     (when (or (not (typep inst 'label))
               (not (endp (label-phis inst))))
       (return inst))
     (setf inst (next-instruction backend-function inst))))

(defun simplify-cfg-1 (backend-function)
  "Eliminate jumps and branches to jumps."
  (let ((total 0))
    (do-instructions (inst backend-function)
      (typecase inst
        (branch-instruction
         (let ((target (skip-label backend-function (branch-target inst))))
           (cond ((and (typep target 'jump-instruction)
                       ;; Don't snap if there are phi nodes at the target.
                       (endp (jump-values target)))
                  (setf (branch-target inst) (jump-target target))
                  (incf total))
                 ((and (typep (next-instruction backend-function inst) 'jump-instruction)
                       (endp (jump-values (next-instruction backend-function inst)))
                       (eql (next-instruction backend-function (next-instruction backend-function inst)) (branch-target inst)))
                  ;; Rewrite "(branch foo t1) (jump t2) t1"
                  ;; to "(branch (not foo) t2) t1"
                  (setf (branch-target inst) (jump-target (next-instruction backend-function inst)))
                  (remove-instruction backend-function (next-instruction backend-function inst))
                  (change-class inst (if (typep inst 'branch-true-instruction)
                                         'branch-false-instruction
                                         'branch-true-instruction))
                  (incf total)))))
        (mezzano.compiler.backend.x86-64::x86-branch-instruction
         (let ((target (skip-label backend-function (mezzano.compiler.backend.x86-64::x86-branch-target inst))))
           (cond ((and (typep target 'jump-instruction)
                       ;; Don't snap if there are phi nodes at the target.
                       (endp (jump-values target)))
                  (setf (mezzano.compiler.backend.x86-64::x86-branch-target inst) (jump-target target))
                  (incf total)))))
        (jump-instruction
         (when (endp (jump-values inst))
           (let ((target (skip-label backend-function (jump-target inst))))
             (cond ((and (typep target 'jump-instruction)
                         ;; Don't snap if there are phi nodes at the target.
                         (endp (jump-values target)))
                    (setf (jump-target inst) (jump-target target))
                    (incf total))
                   ((typep target 'branch-instruction)
                    ;; Insert a label after the branch to give the jump
                    ;; somewhere to target.
                    (let ((new-label (next-instruction backend-function target)))
                      (insert-before backend-function inst
                                     (make-instance (class-of target)
                                                    :value (branch-value target)
                                                    :target (branch-target target)))
                      (setf (jump-target inst) new-label)
                      (incf total)))))))))
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
                                          (make-instance (class-of target-branch)
                                                         :value branch-value
                                                         :target (branch-target target-branch)))))
          (insert-before backend-function inst (make-instance 'label))
          (setf (jump-target inst) new-label)
          (setf (jump-values inst) '())
          ;; Update the use map.
          (push new-branch (gethash branch-value uses))
          (setf (gethash branch-value uses) (remove inst (gethash branch-value uses)))
          (incf total))))
    total))

(defun remove-trivially-constant-branches (backend-function uses defs)
  "Remove (branch-true/-false (constant some-constant) target)"
  (let ((total 0)
        (remove-me '()))
    (do-instructions (inst backend-function)
      (when (typep inst 'branch-instruction)
        (let ((value (first (gethash (branch-value inst) defs))))
          (when (typep value 'constant-instruction)
            (let ((known-value (constant-value value)))
              ;; If the condition is satisfied then replace with a jump, otherwise just remove.
              (when (if (typep inst 'branch-true-instruction)
                        known-value
                        (not known-value))
                (insert-before backend-function inst
                               (make-instance 'jump-instruction :target (branch-target inst)))))
            (push inst remove-me)
            (incf total)))))
    (dolist (inst remove-me)
      (remove-instruction backend-function inst)
      (setf (gethash (branch-value inst) uses) (remove inst (gethash (branch-value inst) uses))))
    total))

(defun simplify-cfg (backend-function)
  (sys.c:with-metering (:backend-simplify-cfg)
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
             (incf total n)))
        (when (not (zerop total))
          (remove-unreachable-basic-blocks backend-function))
        ;; TODO: Break critical edges.
        (check-cfg backend-function)
        total))))

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
