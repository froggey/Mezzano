;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend)

(defparameter *argument-registers* '(:r8 :r9 :r10 :r11 :r12))
(defparameter *return-register* :r8)
(defparameter *funcall-register* :rbx)
(defparameter *fref-register* :r13)
(defparameter *count-register* :rcx)

(defun canonicalize-call-operands (backend-function)
  (do-instructions (inst backend-function)
    (flet ((frob-inputs ()
             ;; Insert moves to physical registers.
             (loop
                for preg in *argument-registers*
                for vreg in (call-arguments inst)
                do
                  (insert-before backend-function inst
                                 (make-instance 'move-instruction
                                                :destination preg
                                                :source vreg)))
             ;; Replace input operands with physical registers.
             (setf (call-arguments inst)
                   (append (subseq *argument-registers* 0
                                   (min (length (call-arguments inst))
                                        (length *argument-registers*)))
                           (nthcdr (length *argument-registers*)
                                   (call-arguments inst)))))
           (frob-outputs ()
             (insert-after backend-function inst
                           (make-instance 'move-instruction
                                          :destination (call-result inst)
                                          :source *return-register*))
             (setf (call-result inst) *return-register*))
           (frob-function ()
             (insert-before backend-function inst
                            (make-instance 'move-instruction
                                           :destination *funcall-register*
                                           :source (call-function inst)))
             (setf (call-function inst) *funcall-register*)))
      (typecase inst
        (call-instruction
         (frob-inputs)
         (frob-outputs))
        (call-multiple-instruction
         (frob-inputs))
        (funcall-instruction
         (frob-inputs)
         (frob-outputs)
         (frob-function))
        (funcall-multiple-instruction
         (frob-inputs)
         (frob-function))
        (multiple-value-funcall-instruction
         (frob-outputs)
         (frob-function))
        (multiple-value-funcall-multiple-instruction
         (frob-function))
        (mezzano.compiler.backend.x86-64::x86-tail-call-instruction
         (frob-inputs))
        (mezzano.compiler.backend.x86-64::x86-tail-funcall-instruction
         (frob-inputs)
         (frob-function))))))

(defun canonicalize-argument-setup (backend-function)
  (let ((aset (first-instruction backend-function)))
    ;; Required, optional, and the closure arguments are put in registers, but
    ;; count/fref are forced to be spilled. rcx (count) and r13 (fref) are
    ;; required by the &rest-list construction code.
    (insert-after backend-function aset
                  (make-instance 'move-instruction
                                 :destination (argument-setup-closure aset)
                                 :source :rbx))
    (setf (argument-setup-closure aset) :rbx)
    ;; Even though the rest list is generated in r13 this code does not
    ;; emit it in r13, choosing to keep it in a vreg and spill it.
    ;; This is due to the fact that there is no way to communicate usage
    ;; of the rest list to the final code emitter.
    (let ((arg-regs *argument-registers*))
      (do ((req (argument-setup-required aset) (cdr req)))
          ((or (endp arg-regs)
               (endp req)))
        (let ((reg (pop arg-regs)))
          (insert-after backend-function aset
                        (make-instance 'move-instruction
                                       :destination (car req)
                                       :source reg))
          (setf (car req) reg)))
      (do ((opt (argument-setup-optional aset) (cdr opt)))
          ((or (endp arg-regs)
               (endp opt)))
        (let ((reg (pop arg-regs)))
          (insert-after backend-function aset
                        (make-instance 'move-instruction
                                       :destination (car opt)
                                       :source reg))
          (setf (car opt) reg))))))

(defun canonicalize-nlx-values (backend-function)
  (do-instructions (inst backend-function)
    (when (typep inst 'nlx-entry-instruction)
      (insert-after backend-function inst
                    (make-instance 'move-instruction
                                   :destination (nlx-entry-value inst)
                                   :source :r8))
      (setf (nlx-entry-value inst) :r8))
    (when (typep inst 'invoke-nlx-instruction)
      (insert-before backend-function inst
                     (make-instance 'move-instruction
                                    :destination :r8
                                    :source (invoke-nlx-value inst)))
      (setf (invoke-nlx-value inst) :r8))))

(defun canonicalize-values (backend-function)
  (do-instructions (inst backend-function)
    (when (typep inst 'values-instruction)
      (do ((regs *argument-registers* (rest regs))
           (values (values-values inst) (rest values)))
          ((or (endp regs)
               (endp values)))
        (insert-before backend-function inst
                       (make-instance 'move-instruction
                                      :destination (first regs)
                                      :source (first values)))
        (setf (first values) (first regs))))
    (when (typep inst 'multiple-value-bind-instruction)
      (do ((regs *argument-registers* (rest regs))
           (values (multiple-value-bind-values inst) (rest values)))
          ((or (endp regs)
               (endp values)))
        (insert-after backend-function inst
                      (make-instance 'move-instruction
                                     :destination (first values)
                                     :source (first regs)))
        (setf (first values) (first regs))))
    (when (typep inst 'return-instruction)
      (insert-before backend-function inst
                     (make-instance 'move-instruction
                                    :destination :r8
                                    :source (return-value inst)))
      (setf (return-value inst) :r8))))

(defun set-equal (list-1 list-2)
  (and (dolist (elt list-1 t)
         (when (not (member elt list-2))
           (return nil)))
       (dolist (elt list-2 t)
         (when (not (member elt list-1))
           (return nil)))))

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

(defun compute-liveness (backend-function)
  (let ((live-in (make-hash-table))
        (live-in* (make-hash-table))
        (live-out (make-hash-table))
        (live-out* (make-hash-table))
        (mv-regs (list* *count-register* *argument-registers*)))
    (loop
       (do-reversed-instructions (inst backend-function)
         (setf (gethash inst live-in*) (gethash inst live-in)
               (gethash inst live-out*) (gethash inst live-out))
         (let ((uses (instruction-inputs inst))
               (defs (instruction-outputs inst)))
           (when (produces-multiple-p inst)
             (setf defs (union defs mv-regs)))
           (when (consumes-multiple-p inst)
             (setf uses (union uses mv-regs)))
           (setf (gethash inst live-out) '())
           (dolist (succ (successors backend-function inst))
             (setf (gethash inst live-out) (union (gethash inst live-out)
                                                  (gethash succ live-in))))
           (setf (gethash inst live-in) (union uses
                                               (set-difference (gethash inst live-out)
                                                               defs)))))
       (when (do-reversed-instructions (inst backend-function t)
               (when (not (and (set-equal (gethash inst live-in) (gethash inst live-in*))
                               (set-equal (gethash inst live-out) (gethash inst live-out*))))
                 (return nil)))
         (return)))
    (values live-in live-out)))

(defun instructions-reverse-postorder (backend-function)
  "Return instructions in reverse postorder."
  (let ((visited (make-hash-table))
        (order '()))
    (labels ((visit (inst)
               (setf (gethash inst visited) t)
               (dolist (succ (successors backend-function inst))
                 (when (not (gethash succ visited))
                   (visit succ)))
               (push inst order)))
      (visit (first-instruction backend-function)))
    order))

(defun all-virtual-registers (backend-function)
  (let ((regs '()))
    (do-instructions (inst backend-function)
      (when (not (typep inst 'label))
        (dolist (out (instruction-outputs inst))
          (when (typep out 'virtual-register)
            (pushnew out regs)))))
    regs))

(defgeneric instruction-clobbers (instruction architecture)
  (:method (i a) '()))

(defmethod instruction-clobbers ((instruction base-call-instruction) (architecture (eql :x86-64)))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod instruction-clobbers ((instruction argument-setup-instruction) (architecture (eql :x86-64)))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod instruction-clobbers ((instruction save-multiple-instruction) (architecture (eql :x86-64)))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod instruction-clobbers ((instruction restore-multiple-instruction) (architecture (eql :x86-64)))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod instruction-clobbers ((instruction nlx-entry-instruction) (architecture (eql :x86-64)))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod instruction-clobbers ((instruction nlx-entry-multiple-instruction) (architecture (eql :x86-64)))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod instruction-clobbers ((instruction values-instruction) (architecture (eql :x86-64)))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod instruction-clobbers ((instruction multiple-value-bind-instruction) (architecture (eql :x86-64)))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod instruction-clobbers ((instruction switch-instruction) (architecture (eql :x86-64)))
  '(:rax))

(defmethod instruction-clobbers ((instruction push-special-stack-instruction) (architecture (eql :x86-64)))
  '(:r13))

(defmethod instruction-clobbers ((instruction flush-binding-cache-entry-instruction) (architecture (eql :x86-64)))
  '(:rax))

(defmethod instruction-clobbers ((instruction unbind-instruction) (architecture (eql :x86-64)))
  '(:rbx :r13 :rax))

(defmethod instruction-clobbers ((instruction disestablish-block-or-tagbody-instruction) (architecture (eql :x86-64)))
  '(:rbx :r13 :rcx))

(defmethod instruction-clobbers ((instruction disestablish-unwind-protect-instruction) (architecture (eql :x86-64)))
  '(:rax :rcx :rdx :rsi :rdi :rbx :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15
    :mm0 :mm1 :mm2 :mm3 :mm4 :mm5 :mm6 :mm7
    :xmm0 :xmm1 :xmm2 :xmm3 :xmm4 :xmm5 :xmm6 :xmm7 :xmm8
    :xmm9 :xmm10 :xmm11 :xmm12 :xmm13 :xmm14 :xmm15))

(defmethod instruction-clobbers ((instruction make-dx-closure-instruction) (architecture (eql :x86-64)))
  '(:rax :rcx))

(defgeneric allow-memory-operand-p (instruction operand architecture)
  (:method (i o a)
    nil))

(defmethod allow-memory-operand-p ((instruction call-instruction) operand (architecture (eql :x86-64)))
  (not (or (eql (call-result instruction) operand)
           (eql (first (call-arguments instruction)) operand)
           (eql (second (call-arguments instruction)) operand)
           (eql (third (call-arguments instruction)) operand)
           (eql (fourth (call-arguments instruction)) operand)
           (eql (fifth (call-arguments instruction)) operand))))

(defmethod allow-memory-operand-p ((instruction call-multiple-instruction) operand (architecture (eql :x86-64)))
  (not (or (eql (first (call-arguments instruction)) operand)
           (eql (second (call-arguments instruction)) operand)
           (eql (third (call-arguments instruction)) operand)
           (eql (fourth (call-arguments instruction)) operand)
           (eql (fifth (call-arguments instruction)) operand))))

(defmethod allow-memory-operand-p ((instruction funcall-instruction) operand (architecture (eql :x86-64)))
  (not (or (eql (call-result instruction) operand)
           (eql (call-function instruction) operand)
           (eql (first (call-arguments instruction)) operand)
           (eql (second (call-arguments instruction)) operand)
           (eql (third (call-arguments instruction)) operand)
           (eql (fourth (call-arguments instruction)) operand)
           (eql (fifth (call-arguments instruction)) operand))))

(defmethod allow-memory-operand-p ((instruction funcall-multiple-instruction) operand (architecture (eql :x86-64)))
  (not (or (eql (call-function instruction) operand)
           (eql (first (call-arguments instruction)) operand)
           (eql (second (call-arguments instruction)) operand)
           (eql (third (call-arguments instruction)) operand)
           (eql (fourth (call-arguments instruction)) operand)
           (eql (fifth (call-arguments instruction)) operand))))

(defmethod allow-memory-operand-p ((instruction save-multiple-instruction) operand (architecture (eql :x86-64)))
  t)

(defmethod allow-memory-operand-p ((instruction restore-multiple-instruction) operand (architecture (eql :x86-64)))
  t)

(defmethod allow-memory-operand-p ((instruction forget-multiple-instruction) operand (architecture (eql :x86-64)))
  t)

(defmethod allow-memory-operand-p ((instruction argument-setup-instruction) operand (architecture (eql :x86-64)))
  t)

(defmethod allow-memory-operand-p ((instruction finish-nlx-instruction) operand (architecture (eql :x86-64)))
  t)

(defmethod allow-memory-operand-p ((instruction nlx-entry-instruction) operand (architecture (eql :x86-64)))
  (not (eql operand (nlx-entry-value instruction))))

(defmethod allow-memory-operand-p ((instruction nlx-entry-multiple-instruction) operand (architecture (eql :x86-64)))
  t)

(defmethod allow-memory-operand-p ((instruction values-instruction) operand (architecture (eql :x86-64)))
  t)

(defmethod allow-memory-operand-p ((instruction multiple-value-bind-instruction) operand (architecture (eql :x86-64)))
  t)

(defclass live-range ()
  ((%vreg :initarg :vreg :reader live-range-vreg)
   (%start :initarg :start :reader live-range-start)
   (%end :initarg :end :reader live-range-end)
   (%conflicts :initarg :conflicts :reader live-range-conflicts)
   (%hint :initarg :hint :reader live-range-hint)))

(defmethod print-object ((object live-range) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S-~S ~:S ~S"
            (live-range-vreg object)
            (live-range-start object) (live-range-end object)
            (live-range-conflicts object)
            (live-range-hint object))))

;;; Pass 1: Order & number instructions. (instructions-reverse-postorder)
;;; Pass 2: Compute live ranges & preg conflicts. (build-live-ranges)
;;; Pass 3: Linear scan allocation. (linear-scan-allocate)
;;; Spilled vregs get instantaneous ranges at use points.
;;; When vreg is spilled, use points before the spill will
;;; use the previously allocated preg at use points. Points
;;; after will use allocation at instantaneous ranges.
;;; Pass 4: Rewrite code, insert load/stores. (apply-register-allocation)

(deftype physical-register ()
  'keyword)

(defun instruction-all-clobbers (inst architecture mv-flow live-in live-out)
  (union
   (union
    (union
     (union
      (union
       (remove-if-not (lambda (x) (typep x 'physical-register))
                      (instruction-inputs inst))
       (remove-if-not (lambda (x) (typep x 'physical-register))
                      (instruction-outputs inst)))
      (instruction-clobbers inst architecture))
     (if (eql (gethash inst mv-flow) :multiple)
         '(:rcx :r8 :r9 :r10 :r11 :r12)
         '()))
    (remove-if-not (lambda (x) (typep x 'physical-register))
                   (gethash inst live-in)))
   (remove-if-not (lambda (x) (typep x 'physical-register))
                  (gethash inst live-out))))

(defun build-live-ranges (backend-function ordering architecture live-in live-out)
  (let ((vreg-liveness-start (make-hash-table))
        (vreg-liveness-end (make-hash-table))
        (vreg-conflicts (make-hash-table))
        (vreg-move-hint (make-hash-table))
        (mv-flow (multiple-value-flow backend-function architecture)))
    (loop
       for i from 0
       for inst in ordering
       do
         (let ((clobbers (instruction-all-clobbers inst architecture mv-flow live-in live-out))
               (vregs (union (union
                              (remove-if-not (lambda (x) (typep x 'virtual-register))
                                             (gethash inst live-in))
                              (remove-if-not (lambda (x) (typep x 'virtual-register))
                                             (gethash inst live-out)))
                             ;; If a vreg isn't used, then it won't show up in the liveness maps.
                             ;; Scan the instruction's outputs to catch this.
                             (remove-if-not (lambda (x) (typep x 'virtual-register))
                                            (instruction-outputs inst)))))
           (dolist (vreg vregs)
             (setf (gethash vreg vreg-liveness-start) (min i (gethash vreg vreg-liveness-start most-positive-fixnum)))
             (setf (gethash vreg vreg-liveness-end) (max i (gethash vreg vreg-liveness-end 0)))
             ;; Don't conflict source/destination of move instructions.
             ;; #'linear-scan-allocator specializes this.
             (when (not (and (typep inst 'move-instruction)
                             (or (eql (move-source inst) vreg)
                                 (eql (move-destination inst) vreg))))
               (dolist (preg clobbers)
                 (pushnew preg (gethash vreg vreg-conflicts '()))))
             (when (and (typep inst 'move-instruction)
                        (eql (move-source inst) vreg)
                        (typep (move-destination inst) 'physical-register)
                        (not (member (move-destination inst)
                                     (gethash vreg vreg-conflicts '()))))
               (setf (gethash vreg vreg-move-hint) (move-destination inst)))
             (when (typep inst 'argument-setup-instruction)
               (when (eql (argument-setup-closure inst) vreg)
                 (setf (gethash vreg vreg-move-hint) *funcall-register*))
               (when (eql (argument-setup-fref inst) vreg)
                 (setf (gethash vreg vreg-move-hint) *fref-register*))
               (when (member vreg (argument-setup-required inst))
                 (setf (gethash vreg vreg-move-hint) (nth (position vreg (argument-setup-required inst))
                                                          *argument-registers*)))))))
    (sort (loop
             for vreg being the hash-keys of vreg-liveness-start using (hash-value start)
             for end = (gethash vreg vreg-liveness-end)
             for conflicts = (gethash vreg vreg-conflicts)
             for hint = (gethash vreg vreg-move-hint)
             collect (make-instance 'live-range
                                    :vreg vreg
                                    :start start
                                    :end end
                                    :conflicts conflicts
                                    :hint hint))
          #'<
          :key #'live-range-start)))

(defun linear-scan-allocate (backend-function ordering live-ranges architecture live-in live-out)
  (let* ((remaining-live-ranges live-ranges)
         (active '())
         ;; list of spilled values
         (spilled '())
         ;; vreg => register
         (registers (make-hash-table))
         ;; (inst . vreg) => register
         (instantaneous-registers (make-hash-table :test 'equal))
         (free-registers '(:r8 :r9 :r10 :r11 :r12 :r13 :rbx))
         (mv-flow (multiple-value-flow backend-function architecture)))
    (flet ((expire-old-intervals (i)
             (loop
                (when (endp active)
                  (return))
                (when (>= (live-range-end (first active)) i)
                  (return))
                (let* ((j (pop active))
                       (reg (gethash (live-range-vreg j) registers)))
                  (when (not (member j spilled))
                    (push reg free-registers)))))
           (spill-at-interval (i)
             ;; Select the longest-lived non-conflicting range to spill.
             (let ((spill (first (last (remove-if (lambda (spill-interval)
                                                    (member (gethash (live-range-vreg spill-interval) registers)
                                                            (live-range-conflicts i)))
                                                  active)))))
               #+(or)
               (format t "active ~S~%" active)
               #+(or)
               (format t "Spill ~S ~S~%" spill (if spill (gethash (live-range-vreg spill) registers) nil))
               (cond ((and spill
                           (> (live-range-end spill) (live-range-end i)))
                      (setf (gethash (live-range-vreg i) registers) (gethash (live-range-vreg spill) registers))
                      (push (live-range-vreg spill) spilled)
                      (setf active (remove spill active))
                      (setf active (merge 'list (list i) active #'< :key 'live-range-end)))
                     (t
                      (push (live-range-vreg i) spilled))))))
      (loop
         for instruction-index from 0
         for inst in ordering
         do
               #+(or)
           (print-instruction inst)
           (expire-old-intervals instruction-index)
           (loop
              (when (not (and remaining-live-ranges
                              (eql instruction-index (live-range-start (first remaining-live-ranges)))))
                (return))
              (let* ((interval (pop remaining-live-ranges))
                     (candidates (remove-if (lambda (reg)
                                              (or (member reg (live-range-conflicts interval))
                                                  ;; If the instruction is a move instruction with physical source/destinations,
                                                  ;; then conflict with it unless this interval is a source/dest and ends/begins
                                                  ;; on this instruction.
                                                  (and (typep inst 'move-instruction)
                                                       (or (and (typep (move-source inst) 'physical-register)
                                                                (eql (move-source inst) reg)
                                                                (not (and (eql (move-destination inst) (live-range-vreg interval))
                                                                          (eql instruction-index (live-range-start interval)))))
                                                           (and (typep (move-destination inst) 'physical-register)
                                                                (eql (move-destination inst) reg)
                                                                (not (and (eql (move-source inst) (live-range-vreg interval))
                                                                          (eql instruction-index (live-range-end interval)))))))))
                                            free-registers))
                     (hint (or (live-range-hint interval)
                               (if (and (typep inst 'move-instruction)
                                        (eql (move-destination inst) (live-range-vreg interval))
                                        (typep (move-source inst) 'physical-register)
                                        (not (member (move-source inst) (live-range-conflicts interval))))
                                   (move-source inst)
                                   nil)))
                     (reg (if (member hint candidates)
                              hint
                              (first candidates))))
               #+(or)
                (format t "Interval ~S~%" interval)
               #+(or)
                (format t "Candidates ~S~%" candidates)
               #+(or)
                (format t "hint/reg ~S / ~S~%" hint reg)
                (cond ((and (typep inst 'argument-setup-instruction)
                            (eql instruction-index (live-range-end interval)))
                       ;; Argument setup instruction with an unused argument.
                       ;; Just spill it.
                       (push (live-range-vreg interval) spilled))
                      ((not reg)
                       (spill-at-interval interval))
                      (t
                       (setf free-registers (remove reg free-registers))
                       (setf (gethash (live-range-vreg interval) registers) reg)
                       (setf active (merge 'list (list interval) active #'< :key 'live-range-end))))))
           ;; Now add instantaneous intervals for spilled registers.
           (let* ((vregs (union (remove-duplicates
                                 (remove-if-not (lambda (r)
                                                  (typep r 'virtual-register))
                                                (instruction-inputs inst)))
                                (remove-duplicates
                                 (remove-if-not (lambda (r)
                                                  (typep r 'virtual-register))
                                                (instruction-outputs inst)))))
                  (spilled-vregs (remove-if-not (lambda (vreg)
                                                  (member vreg spilled))
                                                vregs))
                  (used-pregs (instruction-all-clobbers inst architecture mv-flow live-in live-out))
                  ;; Allocations only matter for the specific instruction.
                  ;; Don't update the normal free-registers list.
                  (available-regs (remove-if (lambda (preg)
                                               (member preg used-pregs))
                                             free-registers)))
               #+(or)
             (format t "Instants ~S ~S ~S ~S~%"
                                 (remove-if-not (lambda (r)
                                                  (typep r 'virtual-register))
                                                (instruction-inputs inst))

                                 (remove-if-not (lambda (r)
                                                  (typep r 'virtual-register))
                                                (instruction-outputs inst))
                                 spilled-vregs available-regs)
             (dolist (vreg spilled-vregs)
               (cond ((allow-memory-operand-p inst vreg architecture)
                      ;; Do nothing.
                      (setf (gethash (cons inst vreg) instantaneous-registers) :memory))
                     ((endp available-regs)
                      ;; Look for some register to spill.
                      ;; Select the longest-lived non-conflicting range to spill.
                      (let ((spill (first (last (remove-if (lambda (spill-interval)
                                                             (or
                                                              ;; Don't spill any vregs used by this instruction.
                                                              (member (live-range-vreg spill-interval) vregs)
                                                              ;; Or any pregs.
                                                              (member (gethash (live-range-vreg spill-interval) registers)
                                                                      used-pregs)))
                                                           active)))))
                        (when (not spill)
                          (error "Internal error: Ran out of registers when allocating instant ~S for instruction ~S."
                                 vreg inst))
                        (setf (gethash (cons inst vreg) instantaneous-registers) (gethash (live-range-vreg spill) registers))
                        (push (live-range-vreg spill) spilled)
                        (setf active (remove spill active))
                        (push (gethash (live-range-vreg spill) registers) free-registers)))
                     (t
                      (setf (gethash (cons inst vreg) instantaneous-registers) (pop available-regs))))
               #+(or)
               (format t "Pick ~S for ~S~%" (gethash (cons inst vreg) instantaneous-registers) vreg)))))
    (values registers spilled instantaneous-registers)))

(defgeneric replace-all-registers (instruction substitution-function))

(defmethod replace-all-registers ((instruction label) substitution-function)
  nil)

(defmethod replace-all-registers ((instruction argument-setup-instruction) substitution-function)
  (setf (argument-setup-fref instruction) (funcall substitution-function (argument-setup-fref instruction)))
  (setf (argument-setup-closure instruction) (funcall substitution-function (argument-setup-closure instruction)))
  (setf (argument-setup-count instruction) (funcall substitution-function (argument-setup-count instruction)))
  (setf (argument-setup-required instruction) (mapcar substitution-function (argument-setup-required instruction)))
  (setf (argument-setup-optional instruction) (mapcar substitution-function (argument-setup-optional instruction)))
  (when (argument-setup-rest instruction)
    (setf (argument-setup-rest instruction) (funcall substitution-function (argument-setup-rest instruction)))))

(defmethod replace-all-registers ((instruction bind-local-instruction) substitution-function)
  (setf (bind-local-value instruction) (funcall substitution-function (bind-local-value instruction))))

(defmethod replace-all-registers ((instruction unbind-local-instruction) substitution-function)
  nil)

(defmethod replace-all-registers ((instruction load-local-instruction) substitution-function)
  (setf (load-local-destination instruction) (funcall substitution-function (load-local-destination instruction))))

(defmethod replace-all-registers ((instruction store-local-instruction) substitution-function)
  (setf (store-local-value instruction) (funcall substitution-function (store-local-value instruction))))

(defmethod replace-all-registers ((instruction constant-instruction) substitution-function)
  (setf (constant-destination instruction) (funcall substitution-function (constant-destination instruction))))

(defmethod replace-all-registers ((instruction move-instruction) substitution-function)
  (setf (move-destination instruction) (funcall substitution-function (move-destination instruction)))
  (setf (move-source instruction) (funcall substitution-function (move-source instruction))))

(defmethod replace-all-registers ((instruction branch-instruction) substitution-function)
  (setf (branch-value instruction) (funcall substitution-function (branch-value instruction))))

(defmethod replace-all-registers ((instruction jump-instruction) substitution-function)
  nil)

(defmethod replace-all-registers ((instruction switch-instruction) substitution-function)
  (setf (switch-value instruction) (funcall substitution-function (switch-value instruction))))

(defmethod replace-all-registers ((instruction unreachable-instruction) substitution-function)
  nil)

(defmethod replace-all-registers ((instruction return-instruction) substitution-function)
  (setf (return-value instruction) (funcall substitution-function (return-value instruction))))

(defmethod replace-all-registers ((instruction return-multiple-instruction) substitution-function)
  nil)

(defmethod replace-all-registers ((instruction begin-nlx-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction))))

(defmethod replace-all-registers ((instruction finish-nlx-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction))))

(defmethod replace-all-registers ((instruction nlx-entry-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction)))
  (setf (nlx-entry-value instruction) (funcall substitution-function (nlx-entry-value instruction))))

(defmethod replace-all-registers ((instruction nlx-entry-multiple-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction))))

(defmethod replace-all-registers ((instruction invoke-nlx-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction)))
  (setf (invoke-nlx-value instruction) (funcall substitution-function (invoke-nlx-value instruction))))

(defmethod replace-all-registers ((instruction invoke-nlx-multiple-instruction) substitution-function)
  (setf (nlx-context instruction) (funcall substitution-function (nlx-context instruction))))

(defmethod replace-all-registers ((instruction save-multiple-instruction) substitution-function)
  (setf (save-multiple-context instruction) (funcall substitution-function (save-multiple-context instruction))))

(defmethod replace-all-registers ((instruction restore-multiple-instruction) substitution-function)
  (setf (restore-multiple-context instruction) (funcall substitution-function (restore-multiple-context instruction))))

(defmethod replace-all-registers ((instruction forget-multiple-instruction) substitution-function)
  (setf (forget-multiple-context instruction) (funcall substitution-function (forget-multiple-context instruction))))

(defmethod replace-all-registers ((instruction multiple-value-bind-instruction) substitution-function)
  (setf (multiple-value-bind-values instruction) (mapcar substitution-function (multiple-value-bind-values instruction))))

(defmethod replace-all-registers ((instruction values-instruction) substitution-function)
  (setf (values-values instruction) (mapcar substitution-function (values-values instruction))))

(defmethod replace-all-registers ((instruction call-instruction) substitution-function)
  (setf (call-result instruction) (funcall substitution-function (call-result instruction)))
  (setf (call-arguments instruction) (mapcar substitution-function (call-arguments instruction))))

(defmethod replace-all-registers ((instruction call-multiple-instruction) substitution-function)
  (setf (call-arguments instruction) (mapcar substitution-function (call-arguments instruction))))

(defmethod replace-all-registers ((instruction funcall-instruction) substitution-function)
  (setf (call-result instruction) (funcall substitution-function (call-result instruction)))
  (setf (call-function instruction) (funcall substitution-function (call-function instruction)))
  (setf (call-arguments instruction) (mapcar substitution-function (call-arguments instruction))))

(defmethod replace-all-registers ((instruction funcall-multiple-instruction) substitution-function)
  (setf (call-function instruction) (funcall substitution-function (call-function instruction)))
  (setf (call-arguments instruction) (mapcar substitution-function (call-arguments instruction))))

(defmethod replace-all-registers ((instruction multiple-value-funcall-instruction) substitution-function)
  (setf (call-result instruction) (funcall substitution-function (call-result instruction)))
  (setf (call-function instruction) (funcall substitution-function (call-function instruction))))

(defmethod replace-all-registers ((instruction multiple-value-funcall-multiple-instruction) substitution-function)
  (setf (call-function instruction) (funcall substitution-function (call-function instruction))))

(defmethod replace-all-registers ((instruction object-get-instruction) substitution-function)
  (setf (object-get-destination instruction) (funcall substitution-function (object-get-destination instruction)))
  (setf (object-get-object instruction) (funcall substitution-function (object-get-object instruction)))
  (setf (object-get-index instruction) (funcall substitution-function (object-get-index instruction))))

(defmethod replace-all-registers ((instruction object-set-instruction) substitution-function)
  (setf (object-set-value instruction) (funcall substitution-function (object-set-value instruction)))
  (setf (object-set-object instruction) (funcall substitution-function (object-set-object instruction)))
  (setf (object-set-index instruction) (funcall substitution-function (object-set-index instruction))))

(defmethod replace-all-registers ((instruction eq-instruction) substitution-function)
  (setf (eq-result instruction) (funcall substitution-function (eq-result instruction)))
  (setf (eq-lhs instruction) (funcall substitution-function (eq-lhs instruction)))
  (setf (eq-rhs instruction) (funcall substitution-function (eq-rhs instruction))))

(defmethod replace-all-registers ((instruction fixnum-<-instruction) substitution-function)
  (setf (fixnum-<-result instruction) (funcall substitution-function (fixnum-<-result instruction)))
  (setf (fixnum-<-lhs instruction) (funcall substitution-function (fixnum-<-lhs instruction)))
  (setf (fixnum-<-rhs instruction) (funcall substitution-function (fixnum-<-rhs instruction))))

(defmethod replace-all-registers ((instruction push-special-stack-instruction) substitution-function)
  (setf (push-special-stack-a-value instruction) (funcall substitution-function (push-special-stack-a-value instruction)))
  (setf (push-special-stack-b-value instruction) (funcall substitution-function (push-special-stack-b-value instruction))))

(defmethod replace-all-registers ((instruction flush-binding-cache-entry-instruction) substitution-function)
  (setf (flush-binding-cache-entry-symbol instruction) (funcall substitution-function (flush-binding-cache-entry-symbol instruction))))

(defmethod replace-all-registers ((instruction unbind-instruction) substitution-function)
  nil)

(defmethod replace-all-registers ((instruction disestablish-block-or-tagbody-instruction) substitution-function)
  nil)

(defmethod replace-all-registers ((instruction disestablish-unwind-protect-instruction) substitution-function)
  nil)

(defmethod replace-all-registers ((instruction make-dx-simple-vector-instruction) substitution-function)
  (setf (make-dx-simple-vector-result instruction) (funcall substitution-function (make-dx-simple-vector-result instruction))))

(defmethod replace-all-registers ((instruction make-dx-closure-instruction) substitution-function)
  (setf (make-dx-closure-result instruction) (funcall substitution-function (make-dx-closure-result instruction)))
  (setf (make-dx-closure-function instruction) (funcall substitution-function (make-dx-closure-function instruction)))
  (setf (make-dx-closure-environment instruction) (funcall substitution-function (make-dx-closure-environment instruction))))

(defun rewrite-after-allocation (backend-function registers spilled instantaneous-registers)
  (do* ((inst (first-instruction backend-function) next-inst)
        (next-inst (next-instruction backend-function inst) (if inst (next-instruction backend-function inst))))
       ((null inst))
    (let* ((input-vregs (remove-duplicates
                         (remove-if-not (lambda (r)
                                          (typep r 'virtual-register))
                                        (instruction-inputs inst))))
           (spilled-input-vregs (remove-if-not (lambda (vreg)
                                                 (member vreg spilled))
                                               input-vregs))
           (output-vregs (remove-duplicates
                          (remove-if-not (lambda (r)
                                           (typep r 'virtual-register))
                                         (instruction-outputs inst))))
           (spilled-output-vregs (remove-if-not (lambda (vreg)
                                                  (member vreg spilled))
                                                output-vregs)))
      ;; Load spilled input registers.
      (dolist (spill spilled-input-vregs)
        (let ((reg (or (gethash (cons inst spill) instantaneous-registers)
                       (gethash spill registers)
                       (error "Missing instantaneous register for spill ~S at ~S." spill inst))))
          (when (not (eql reg :memory))
            (insert-before backend-function inst
                           (make-instance 'fill-instruction
                                          :destination reg
                                          :source spill)))))
      ;; Store spilled output registers.
      (dolist (spill spilled-output-vregs)
        (let ((reg (or (gethash (cons inst spill) instantaneous-registers)
                       (gethash spill registers)
                       (error "Missing instantaneous register for spill ~S at ~S." spill inst))))
          (when (not (eql reg :memory))
            (insert-after backend-function inst
                          (make-instance 'spill-instruction
                                         :destination spill
                                         :source reg)))))
      ;; Rewrite the instruction.
      (replace-all-registers inst
                             (lambda (old)
                               (let ((new (or (gethash (cons inst old) instantaneous-registers)
                                              (gethash old registers))))
                                 (if (or (not new)
                                         (eql new :memory))
                                     old
                                     new)))))))

(defun build-use/def-maps (backend-function)
  (let ((uses (make-hash-table))
        (defs (make-hash-table)))
    (do-instructions (inst backend-function)
      (dolist (use (instruction-inputs inst))
        (when (typep use 'virtual-register)
          (pushnew inst (gethash use uses '()))))
      (dolist (def (instruction-outputs inst))
        (when (typep def 'virtual-register)
          (pushnew inst (gethash def defs '())))))
    (values uses defs)))

(defun remove-unused-instructions-1 (backend-function)
  (multiple-value-bind (uses defs)
      (build-use/def-maps backend-function)
    (declare (ignore defs))
    (do* ((n-removed 0)
          (inst (first-instruction backend-function) next-inst)
          (next-inst (next-instruction backend-function inst) (if inst (next-instruction backend-function inst))))
         ((null inst)
          n-removed)
      (when (and (instruction-pure-p inst)
                 (every (lambda (out)
                          (and (typep out 'virtual-register)
                               (endp (gethash out uses))))
                        (instruction-outputs inst)))
        (incf n-removed)
        (remove-instruction backend-function inst)))))

(defun remove-unused-instructions (backend-function)
  (loop
     with total = 0
     do (let ((n (remove-unused-instructions-1 backend-function)))
          (incf total n)
          (when (zerop n)
            (return total)))))
