;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend.register-allocator)

(defgeneric target-argument-registers (target))
(defgeneric target-return-register (target))
(defgeneric target-funcall-register (target))
(defgeneric target-fref-register (target))
(defgeneric target-count-register (target))

(defgeneric architectural-physical-registers (architecture))
(defgeneric valid-physical-registers-for-kind (kind architecture))

(defgeneric instruction-clobbers (instruction architecture)
  (:method (i a) '()))

(defgeneric allow-memory-operand-p (instruction operand architecture)
  (:method (i o a)
    nil))

;; Can foo/bar register kinds be mixed in spill/fill code?
(defgeneric spill/fill-register-kinds-compatible (kind1 kind2 architecture)
  (:method (kind1 kind2 architecture)
    (eql kind1 kind2)))

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
    ;; count/fref are forced to be spilled. rcx (count) and r13 (fref) are
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

(defun instructions-reverse-postorder (backend-function)
  "Return instructions in reverse postorder."
  (let ((visited (make-hash-table :test 'eq :synchronized nil))
        (order '()))
    (labels ((visit (inst)
               (let ((additional '())
                     (block-order '()))
                 (loop
                    (setf (gethash inst visited) t)
                    (push inst block-order)
                    (when (typep inst 'ir:terminator-instruction)
                      (dolist (succ (union (ir::successors backend-function inst)
                                           additional))
                        (when (not (gethash succ visited))
                          (visit succ)))
                      (setf order (append (reverse block-order)
                                          order))
                      (return))
                    (when (typep inst 'ir:begin-nlx-instruction)
                      (setf additional (union additional
                                              (ir:begin-nlx-targets inst))))
                    (setf inst (ir:next-instruction backend-function inst))))))
      (visit (ir:first-instruction backend-function)))
    order))

(defun all-virtual-registers (backend-function)
  (let ((regs '()))
    (ir:do-instructions (inst backend-function)
      (dolist (out (ir::instruction-outputs inst))
        (when (typep out 'ir:virtual-register)
          (pushnew out regs))))
    regs))

(defclass live-range ()
  ((%vreg :initarg :vreg :reader live-range-vreg)
   ;; Start & end (inclusive) of this range, in allocator order.
   (%start :initarg :start :reader live-range-start)
   (%end :initarg :end :reader live-range-end)
   ;; Physical registers this range conflicts with and cannot be allocated in.
   (%conflicts :initarg :conflicts :reader live-range-conflicts)))

(defmethod print-object ((object live-range) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S-~S ~:S"
            (live-range-vreg object)
            (live-range-start object) (live-range-end object)
            (live-range-conflicts object))))

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
  (let ((clobbers '()))
    (dolist (reg (ir::instruction-inputs inst))
      (when (typep reg 'physical-register)
        (pushnew reg clobbers)))
    (dolist (reg (ir::instruction-outputs inst))
      (when (typep reg 'physical-register)
        (pushnew reg clobbers)))
    (setf clobbers (union (instruction-clobbers inst architecture)
                          clobbers))
    (when (eql (gethash inst mv-flow) :multiple)
      (pushnew (target-count-register architecture) clobbers)
      (setf clobbers (union (target-argument-registers architecture)
                            clobbers)))
    (dolist (reg (gethash inst live-in))
      (when (typep reg 'physical-register)
        (pushnew reg clobbers)))
    (dolist (reg (gethash inst live-out))
      (when (typep reg 'physical-register)
        (pushnew reg clobbers)))
    clobbers))

(defun virtual-registers-touched-by-instruction (inst live-in live-out)
  (union (union
          (remove-duplicates
           (remove-if-not (lambda (x) (typep x 'ir:virtual-register))
                          (gethash inst live-in)))
          (remove-duplicates
           (remove-if-not (lambda (x) (typep x 'ir:virtual-register))
                          (gethash inst live-out))))
         ;; If a vreg isn't used, then it won't show up in the liveness maps.
         ;; Scan the instruction's outputs to catch this.
         (remove-duplicates
          (remove-if-not (lambda (x) (typep x 'ir:virtual-register))
                         (ir::instruction-outputs inst)))))

(defclass linear-allocator ()
  ((%function :initarg :function :reader allocator-backend-function)
   (%ordering :initarg :ordering :reader allocator-instruction-ordering)
   (%architecture :initarg :architecture :reader allocator-architecture)
   (%live-in :initarg :live-in :reader allocator-live-in)
   (%live-out :initarg :live-out :reader allocator-live-out)
   (%mv-flow :initarg :mv-flow :reader allocator-mv-flow)
   ;; Stored reversed, with the next range at the end, so the next range can be vector-pop'd off.
   (%ranges :accessor allocator-remaining-ranges)
   (%vreg-ranges :accessor allocator-vreg-ranges)
   (%vreg-hints :accessor allocator-vreg-hints)
   (%active :accessor allocator-active-ranges)
   (%free-registers :accessor allocator-free-registers)
   (%spilled :accessor allocator-spilled-ranges)
   (%allocations :accessor allocator-range-allocations)
   (%instants :accessor allocator-instantaneous-allocations)
   (%cfg-preds :initarg :cfg-preds :reader allocator-cfg-preds)
   (%instruction-clobbers :initarg :instruction-clobbers :reader allocator-instruction-clobbers)))

(defun program-ordering (backend-function)
  (let ((order '()))
    (ir:do-instructions (inst backend-function)
      (push inst order))
    (nreverse order)))

(defun make-linear-allocator (backend-function architecture &key ordering)
  (multiple-value-bind (basic-blocks bb-preds bb-succs)
      (ir::build-cfg backend-function)
    (declare (ignore basic-blocks bb-succs))
    (multiple-value-bind (live-in live-out)
        (ir::compute-liveness backend-function architecture)
      (let ((order (if ordering
                       (funcall ordering backend-function)
                       (instructions-reverse-postorder backend-function)))
            (mv-flow (ir::multiple-value-flow backend-function architecture))
            (clobbers (make-hash-table :test 'eq :synchronized nil)))
        (dolist (inst order)
          (setf (gethash inst clobbers) (instruction-all-clobbers inst architecture mv-flow live-in live-out)))
        (make-instance 'linear-allocator
                       :function backend-function
                       :ordering order
                       :architecture architecture
                       :live-in live-in
                       :live-out live-out
                       :mv-flow mv-flow
                       :cfg-preds bb-preds
                       :instruction-clobbers clobbers)))))

(defun build-live-ranges (allocator)
  (let* ((ranges (make-array 128 :adjustable t :fill-pointer 0))
         (vreg-ranges (make-hash-table :test 'eq :synchronized nil))
         (ordering (allocator-instruction-ordering allocator))
         (live-in (allocator-live-in allocator))
         (live-out (allocator-live-out allocator))
         (active-vregs '())
         (vreg-liveness-start (make-hash-table :test 'eq :synchronized nil))
         (vreg-conflicts (make-hash-table :test 'eq :synchronized nil))
         (vreg-move-hint (make-hash-table :test 'eq :synchronized nil))
         (arch (allocator-architecture allocator))
         (arg-regs (target-argument-registers arch))
         (funcall-reg (target-funcall-register arch))
         (fref-reg (target-fref-register arch)))
    (flet ((add-range (vreg end)
             (setf (gethash vreg vreg-conflicts) (union (gethash vreg vreg-conflicts)
                                                        (set-difference (architectural-physical-registers (allocator-architecture allocator))
                                                                        (valid-physical-registers-for-kind (ir:virtual-register-kind vreg)
                                                                                                           (allocator-architecture allocator)))))
             (let ((range (make-instance 'live-range
                                         :vreg vreg
                                         :start (gethash vreg vreg-liveness-start)
                                         :end end
                                         :conflicts (gethash vreg vreg-conflicts))))
               (when (not ir::*shut-up*)
                 (format t " Add range ~S~%" range))
               (vector-push-extend range ranges)
               (push range (gethash vreg vreg-ranges)))))
      (loop
         for range-start from 0
         for inst in ordering
         do
           (let* ((clobbers (gethash inst (allocator-instruction-clobbers allocator)))
                  (vregs (virtual-registers-touched-by-instruction inst live-in live-out))
                  (newly-live-vregs (set-difference vregs active-vregs))
                  (newly-dead-vregs (set-difference active-vregs vregs)))
             (when (not ir::*shut-up*)
               (format t "~D:" range-start)
               (ir::print-instruction inst)
               (format t "active: ~:S~%" active-vregs)
               (format t "vregs: ~:S~%" vregs)
               (format t "newly live: ~:S~%" newly-live-vregs)
               (format t "newly dead: ~:S~%" newly-dead-vregs))
             ;; Process vregs that have just become live.
             (dolist (vreg newly-live-vregs)
               (setf (gethash vreg vreg-liveness-start) range-start)
               (setf (gethash vreg vreg-conflicts) '()))
             ;; And vregs that have just become dead.
             (let ((range-end (1- range-start)))
               (dolist (vreg newly-dead-vregs)
                 (add-range vreg range-end)))
             (setf active-vregs vregs)
             (dolist (vreg active-vregs)
               ;; Update conflicts for this vreg.
               ;; Don't conflict source/destination of move instructions.
               ;; #'linear-scan-allocator specializes this.
               (when (not (and (typep inst 'ir:move-instruction)
                               (or (eql (ir:move-source inst) vreg)
                                   (eql (ir:move-destination inst) vreg))))
                 (dolist (preg clobbers)
                   (pushnew preg (gethash vreg vreg-conflicts '()))))
               ;; Set the allocation hint.
               (when (and (typep inst 'ir:move-instruction)
                          (eql (ir:move-source inst) vreg)
                          (typep (ir:move-destination inst) 'physical-register)
                          (not (member (ir:move-destination inst)
                                       (gethash vreg vreg-conflicts '()))))
                 (setf (gethash vreg vreg-move-hint) (ir:move-destination inst)))
               (when (typep inst 'ir:argument-setup-instruction)
                 (when (eql (ir:argument-setup-closure inst) vreg)
                   (setf (gethash vreg vreg-move-hint) funcall-reg))
                 (when (eql (ir:argument-setup-fref inst) vreg)
                   (setf (gethash vreg vreg-move-hint) fref-reg))
                 (when (member vreg (ir:argument-setup-required inst))
                   (setf (gethash vreg vreg-move-hint) (nth (position vreg (ir:argument-setup-required inst))
                                                            arg-regs)))))))
      ;; Finish any remaining active ranges.
      (let ((range-end (1- (length ordering))))
        (dolist (vreg active-vregs)
          (add-range vreg range-end))))
    (setf (slot-value allocator '%ranges) (sort ranges #'> :key #'live-range-start)
          (slot-value allocator '%vreg-ranges) vreg-ranges
          (slot-value allocator '%vreg-hints) vreg-move-hint))
  (when (not ir::*shut-up*)
    (format t "Live ranges: ~:S~%" (allocator-remaining-ranges allocator)))
  (values))

(defun expire-old-intervals (allocator current-interval)
  (loop
     until (or (endp (allocator-active-ranges allocator))
               (>= (live-range-end (first (allocator-active-ranges allocator))) current-interval))
     do (let* ((range (pop (allocator-active-ranges allocator)))
               (reg (gethash range (allocator-range-allocations allocator))))
          (when (not (interval-spilled-p allocator range))
            (push reg (allocator-free-registers allocator))))))

(defun mark-interval-spilled (allocator interval)
  (setf (gethash interval (allocator-spilled-ranges allocator)) t)
  (setf (allocator-active-ranges allocator) (remove interval (allocator-active-ranges allocator))))

(defun activate-interval (allocator interval register)
  (let ((vreg (live-range-vreg interval)))
    (setf (gethash interval (allocator-range-allocations allocator)) register)
    ;; Update the hint for this vreg so the allocator tries to allocate multiple intervals in the same register.
    (when (not (gethash vreg (allocator-vreg-hints allocator)))
      (setf (gethash vreg (allocator-vreg-hints allocator)) register))
    (setf (allocator-active-ranges allocator)
          (merge 'list
                 (list interval)
                 (allocator-active-ranges allocator)
                 #'<
                 :key 'live-range-end))))

(defun spill-at-interval (allocator new-interval)
  ;; Select the longest-lived non-conflicting range to spill.
  (let ((spill (first (last (remove-if (lambda (spill-interval)
                                         (member (gethash spill-interval (allocator-range-allocations allocator))
                                                 (live-range-conflicts new-interval)))
                                       (allocator-active-ranges allocator))))))
    (unless ir::*shut-up*
      (format t "active ~S~%" (allocator-active-ranges allocator))
      (format t "Spill ~S ~S~%" spill (if spill (gethash spill (allocator-range-allocations allocator)) nil)))
    (cond ((and spill
                (> (live-range-end spill) (live-range-end new-interval)))
           ;; Found an interval to spill.
           (activate-interval allocator new-interval (gethash spill (allocator-range-allocations allocator)))
           (mark-interval-spilled allocator spill))
          (t
           ;; Nothing to spill, spill the new interval.
           (mark-interval-spilled allocator new-interval)))))

(defun ranges-remaining-p (allocator)
  (not (eql (length (allocator-remaining-ranges allocator)) 0)))

(defun next-remaining-range (allocator)
  (let ((ranges (allocator-remaining-ranges allocator)))
    (aref ranges (1- (length ranges)))))

(defun update-active-intervals (allocator inst instruction-index)
  (loop
     until (or (not (ranges-remaining-p allocator))
               (not (eql instruction-index (live-range-start (next-remaining-range allocator)))))
     do (let* ((interval (vector-pop (allocator-remaining-ranges allocator)))
               (vreg (live-range-vreg interval))
               (candidates (remove-if (lambda (reg)
                                        (or (member reg (live-range-conflicts interval))
                                            ;; If the instruction is a move instruction with physical source/destinations,
                                            ;; then conflict with it unless this interval is a source/dest and ends/begins
                                            ;; on this instruction.
                                            (and (typep inst 'ir:move-instruction)
                                                 (or (and (typep (ir:move-source inst) 'physical-register)
                                                          (eql (ir:move-source inst) reg)
                                                          (not (and (eql (ir:move-destination inst) vreg)
                                                                    (eql instruction-index (live-range-start interval)))))
                                                     (and (typep (ir:move-destination inst) 'physical-register)
                                                          (eql (ir:move-destination inst) reg)
                                                          (not (and (eql (ir:move-source inst) vreg)
                                                                    (eql instruction-index (live-range-end interval)))))))))
                                      (allocator-free-registers allocator)))
               (hint (or (gethash vreg (allocator-vreg-hints allocator))
                         ;; If this is a move from a physical register, then use that physical register as the hint.
                         ;; Move instructions kill physical registers, so this this is safe.
                         (if (and (typep inst 'ir:move-instruction)
                                  (eql (ir:move-destination inst) vreg)
                                  (typep (ir:move-source inst) 'physical-register))
                            (ir:move-source inst)
                            nil)))
               (reg (if (member hint candidates)
                        hint
                        (first candidates))))
          (unless ir::*shut-up*
            (format t "Interval ~S~%" interval)
            (format t "Candidates ~S~%" candidates)
            (format t "hint/reg ~S / ~S~%" hint reg))
          (cond ((and (typep inst 'ir:argument-setup-instruction)
                      (eql instruction-index (live-range-end interval)))
                 ;; Argument setup instruction with an unused argument.
                 ;; Just spill it.
                 (mark-interval-spilled allocator interval))
                ((not reg)
                 (spill-at-interval allocator interval))
                (t
                 (setf (allocator-free-registers allocator) (remove reg (allocator-free-registers allocator)))
                 (activate-interval allocator interval reg))))))

(defun allocate-instants (allocator inst instruction-index)
  (let* ((vregs (union (remove-duplicates
                        (remove-if-not (lambda (r)
                                         (typep r 'ir:virtual-register))
                                       (ir::instruction-inputs inst)))
                       (remove-duplicates
                        (remove-if-not (lambda (r)
                                         (typep r 'ir:virtual-register))
                                       (ir::instruction-outputs inst)))))
         (spilled-vregs (remove-if-not (lambda (vreg)
                                         (spilledp allocator vreg instruction-index))
                                       vregs))
         (used-pregs (gethash inst (allocator-instruction-clobbers allocator)))
         ;; Allocations only matter for the specific instruction.
         ;; Don't update the normal free-registers list.
         (available-regs (remove-if (lambda (preg)
                                      (member preg used-pregs))
                                    (allocator-free-registers allocator))))
    (unless ir::*shut-up*
      (format t "Instants ~:S ~:S ~:S ~:S~%"
              (remove-if-not (lambda (r)
                               (typep r 'ir:virtual-register))
                             (ir::instruction-inputs inst))
              (remove-if-not (lambda (r)
                               (typep r 'ir:virtual-register))
                             (ir::instruction-outputs inst))
              spilled-vregs available-regs))
    (dolist (vreg spilled-vregs)
      (let ((truely-available-regs (intersection available-regs
                                                 (valid-physical-registers-for-kind (ir:virtual-register-kind vreg)
                                                                                    (allocator-architecture allocator)))))
        (cond ((allow-memory-operand-p inst vreg (allocator-architecture allocator))
               ;; Do nothing.
               (setf (gethash (cons instruction-index vreg) (allocator-instantaneous-allocations allocator)) :memory))
              ((endp truely-available-regs)
               ;; Look for some register to spill.
               ;; Select the longest-lived non-conflicting range to spill.
               (let ((spill (first (last (remove-if (lambda (spill-interval)
                                                      (or
                                                       ;; Don't spill any vregs used by this instruction.
                                                       (member (live-range-vreg spill-interval) vregs)
                                                       ;; Or any pregs.
                                                       (member (gethash spill-interval (allocator-range-allocations allocator))
                                                               used-pregs)))
                                                    (allocator-active-ranges allocator))))))
                 (assert spill ()
                         "Internal error: Ran out of registers when allocating instant ~S for instruction ~S."
                         vreg inst)
                 (let ((reg (gethash spill (allocator-range-allocations allocator))))
                   (setf (gethash (cons instruction-index vreg) (allocator-instantaneous-allocations allocator)) reg)
                   (push reg (allocator-free-registers allocator))
                   (mark-interval-spilled allocator spill))))
              (t
               (let ((reg (pop truely-available-regs)))
                 (setf (gethash (cons instruction-index vreg) (allocator-instantaneous-allocations allocator)) reg)
                 (setf available-regs (remove reg available-regs))))))
      (unless ir::*shut-up*
        (format t "Pick ~S for ~S~%" (gethash (cons instruction-index vreg) (allocator-instantaneous-allocations allocator)) vreg)))))

(defun mergable-move-instruction-p (allocator inst instruction-index)
  (let ((src (ir:move-source inst))
        (dst (ir:move-destination inst)))
    (cond ((and (not (eql src dst))
                (typep src 'ir:virtual-register)
                (typep dst 'ir:virtual-register))
           (let* ((src-interval (interval-at allocator src instruction-index))
                  (dst-interval (interval-at allocator dst instruction-index))
                  (first-active-range (first (allocator-active-ranges allocator)))
                  (first-remaining-range (next-remaining-range allocator)))
             (and (not (interval-spilled-p allocator src-interval))
                  (eql first-active-range src-interval)
                  (eql (live-range-end first-active-range) instruction-index)
                  (eql first-remaining-range dst-interval)
                  ;; Must not conflict.
                  (not (member (gethash src-interval (allocator-range-allocations allocator))
                               (live-range-conflicts dst-interval))))))
          (t nil))))

(defun linear-scan-allocate (allocator)
  (setf (allocator-active-ranges allocator) '()
        (allocator-range-allocations allocator) (make-hash-table :test 'eq :synchronized nil)
        (allocator-free-registers allocator) (architectural-physical-registers (allocator-architecture allocator))
        (allocator-spilled-ranges allocator) (make-hash-table :test 'eq :synchronized nil)
        (allocator-instantaneous-allocations allocator) (make-hash-table :test 'equal :synchronized nil))
  (loop
     for inst in (allocator-instruction-ordering allocator)
     for instruction-index from 0
     do
       (expire-old-intervals allocator instruction-index)
       (unless ir::*shut-up*
         (format t "~D:" instruction-index)
         (ir::print-instruction inst)
         (format t "actives ~:S~%" (allocator-active-ranges allocator)))
     ;; If this is a move instruction with a non-spilled source vreg expiring on this instruction
     ;; and a destination vreg starting on this instruction then assign the same register.
       (cond ((and (typep inst 'ir:move-instruction)
                   (mergable-move-instruction-p allocator inst instruction-index))
              (let* ((old-range (pop (allocator-active-ranges allocator)))
                     (new-range (vector-pop (allocator-remaining-ranges allocator)))
                     (reg (gethash old-range (allocator-range-allocations allocator))))
                (assert (eql (live-range-start new-range) instruction-index))
                (unless ir::*shut-up*
                  (format t "Direct move from ~S to ~S using reg ~S~%" old-range new-range reg)
                  (format t "remaining ~:S~%" (allocator-remaining-ranges allocator)))
                (activate-interval allocator new-range reg))
              ;; Shouldn't be any remaining ranges coming live on this instruction.
              (assert (or (not (ranges-remaining-p allocator))
                          (not (eql (live-range-start (next-remaining-range allocator)) instruction-index)))))
             (t
              (update-active-intervals allocator inst instruction-index)))
       (allocate-instants allocator inst instruction-index))
  (expire-old-intervals allocator (length (allocator-instruction-ordering allocator)))
  (assert (endp (allocator-active-ranges allocator))))

(defun interval-at (allocator vreg index)
  (dolist (interval (gethash vreg (allocator-vreg-ranges allocator))
           (error "Missing interval for ~S at index ~S" vreg index))
    (when (<= (live-range-start interval) index (live-range-end interval))
      (return interval))))

(defun interval-spilled-p (allocator interval)
  (gethash interval (allocator-spilled-ranges allocator) nil))

(defun spilledp (allocator vreg index)
  (interval-spilled-p allocator (interval-at allocator vreg index)))

(defun instant-register-at (allocator vreg index)
  (or (gethash (cons index vreg) (allocator-instantaneous-allocations allocator))
      (gethash (interval-at allocator vreg index) (allocator-range-allocations allocator))
      (error "Missing instantaneous allocation for ~S at ~S" vreg index)))

(defun fix-locations-after-control-flow (allocator inst instruction-index target insert-point)
  (let* ((target-index (position target (allocator-instruction-ordering allocator)))
         (active-vregs (remove-if-not (lambda (reg) (typep reg 'ir:virtual-register))
                                      (gethash target (allocator-live-in allocator))))
         (input-intervals (mapcar (lambda (vreg) (interval-at allocator vreg instruction-index))
                                  active-vregs))
         (input-registers (mapcar (lambda (interval)
                                    (if (interval-spilled-p allocator interval)
                                        (live-range-vreg interval)
                                        (or (gethash interval (allocator-range-allocations allocator))
                                            (error "Missing allocation for ~S" interval))))
                                  input-intervals))
         (output-intervals (mapcar (lambda (vreg) (interval-at allocator vreg target-index))
                                   active-vregs))
         (output-registers (mapcar (lambda (interval)
                                     (if (interval-spilled-p allocator interval)
                                         (live-range-vreg interval)
                                         (or (gethash interval (allocator-range-allocations allocator))
                                             (error "Missing allocation for ~S" interval))))
                                   output-intervals))
         (pairs (remove-if (lambda (x) (eql (car x) (cdr x)))
                           (mapcar 'cons input-registers output-registers)))
         (fills '()))
    (flet ((insert (new-inst)
             (ir:insert-after (allocator-backend-function allocator)
                              insert-point
                              new-inst)
             (setf insert-point new-inst)))
      (unless ir::*shut-up*
        (format t "~D:" instruction-index)
        (ir::print-instruction inst)
        (format t "~D:" target-index)
        (ir::print-instruction target)
        (format t "Active vregs at ~S: ~:S~%" inst active-vregs)
        (format t "   Input intervals: ~:S~%" input-intervals)
        (format t "  output intervals: ~:S~%" output-intervals)
        (format t "   Input registers: ~:S~%" input-registers)
        (format t "  output registers: ~:S~%" output-registers)
        (format t "  pairs: ~:S~%" pairs))
      ;; There should be no spill -> spill moves.
      (loop
         for (in . out) in pairs
         do (assert (not (and (typep in 'ir:virtual-register)
                              (typep out 'ir:virtual-register)))))
      ;; Process spills.
      (loop
         for (in . out) in pairs
         when (typep out 'ir:virtual-register)
         do (insert (make-instance 'ir:spill-instruction
                                   :source in
                                   :destination out)))
      (setf pairs (remove-if (lambda (x) (typep (cdr x) 'ir:virtual-register))
                             pairs))
      ;; Remove fills.
      (loop
         for (in . out) in pairs
         when (typep in 'ir:virtual-register)
         do (push (make-instance 'ir:fill-instruction
                                 :source in
                                 :destination out)
                  fills))
      (setf pairs (remove-if (lambda (x) (typep (car x) 'ir:virtual-register))
                             pairs))
      ;; This leaves pairs filled with register -> register assignments.
      (loop until (endp pairs) do
         ;; Peel off any simple moves.
           (loop
              (let ((candidate (find-if (lambda (x)
                                          ;; Destination must not be used by any pending pairs.
                                          (not (find (cdr x) pairs :key #'car)))
                                        pairs)))
                (when (not candidate)
                  (return))
                (setf pairs (remove candidate pairs))
                (insert (make-instance 'ir:move-instruction
                                       :source (car candidate)
                                       :destination (cdr candidate)))))
           (when (endp pairs) (return))
         ;; There are no simple moves left, pick two registers to swap.
           (let* ((p (pop pairs))
                  (r1 (car p))
                  (r2 (cdr p)))
             (when (not (eql r1 r2))
               (insert (make-instance 'ir:swap-instruction
                                      :lhs r1
                                      :rhs r2))
               ;; Fix up the pair list
               (dolist (pair pairs)
                 (cond ((eql (car pair) r1)
                        (setf (car pair) r2))
                       ((eql (car pair) r2)
                        (setf (car pair) r1)))))))
      ;; Finally do fills.
      (dolist (fill fills)
        (insert fill)))))

(defun break-critical-edge (backend-function terminator target)
  "Break the first edge from terminator to target."
  (assert (endp (ir:label-phis target)))
  (let ((l (make-instance 'ir:label :name :broken-critical-edge)))
    (etypecase terminator
      (ir:branch-instruction
       (cond ((eql (ir:next-instruction backend-function terminator) target)
              (ir:insert-after backend-function terminator l))
             (t
              (ir:insert-before backend-function target l)
              (setf (ir:branch-target terminator) l))))
      (ir:switch-instruction
       (do ((i (ir:switch-targets terminator)
               (rest i)))
           ((endp i))
         (when (eql (first i) target)
           (ir:insert-before backend-function target l)
           (setf (first i) l)
           (return))))
      (mezzano.compiler.backend.x86-64::x86-branch-instruction
       (cond ((eql (ir:next-instruction backend-function terminator) target)
              (ir:insert-after backend-function terminator l))
             (t
              (ir:insert-before backend-function target l)
              (setf (mezzano.compiler.backend.x86-64::x86-branch-target terminator) l)))))
    (ir:insert-after backend-function l (make-instance 'ir:jump-instruction :target target :values '()))
    l))

(defun rewrite-ordinary-instruction (allocator backend-function inst instruction-index spilled-input-vregs spilled-output-vregs)
  ;; Load spilled input registers.
  (dolist (spill spilled-input-vregs)
    (let ((reg (instant-register-at allocator spill instruction-index)))
      (when (not (eql reg :memory))
        (ir:insert-before backend-function inst
                          (make-instance 'ir:fill-instruction
                                         :destination reg
                                         :source spill)))))
  ;; Store spilled output registers.
  (dolist (spill spilled-output-vregs)
    (let ((reg (instant-register-at allocator spill instruction-index)))
      (when (not (eql reg :memory))
        (ir:insert-after backend-function inst
                         (make-instance 'ir:spill-instruction
                                        :destination spill
                                        :source reg)))))
  ;; Rewrite the instruction.
  (ir::replace-all-registers inst
                             (lambda (old)
                               (let ((new (cond ((not (typep old 'ir:virtual-register))
                                                 nil)
                                                ((or (member old spilled-input-vregs)
                                                     (member old spilled-output-vregs))
                                                 (instant-register-at allocator old instruction-index))
                                                (t
                                                 (or (gethash (interval-at allocator old instruction-index) (allocator-range-allocations allocator))
                                                     (error "Missing allocation for ~S at ~S" old instruction-index))))))
                                 (if (or (not new)
                                         (eql new :memory))
                                     old
                                     new)))))

(defun rewrite-terminator-instruction (allocator inst instruction-index)
  ;; Insert code to patch up interval differences.
  (let ((successors (ir::successors (allocator-backend-function allocator) inst)))
    (cond ((endp successors)) ; No successors, do nothing.
          ((endp (rest successors))
           ;; Single successor, insert fixups before the instruction.
           (fix-locations-after-control-flow allocator
                                             inst instruction-index
                                             (first successors)
                                             (ir:prev-instruction (allocator-backend-function allocator)
                                                                  inst)))
          (t
           ;; Multiple successors, insert fixups after each branch, breaking critical edges as needed.
           (dolist (succ successors)
             (let ((insert-point succ))
               (when (not (endp (rest (gethash succ (allocator-cfg-preds allocator)))))
                 ;; Critical edge...
                 (unless ir::*shut-up*
                   (format t "Break critical edge ~S -> ~S~%" inst succ))
                 (setf insert-point (break-critical-edge (allocator-backend-function allocator) inst succ)))
               (fix-locations-after-control-flow allocator
                                                 inst instruction-index
                                                 succ
                                                 insert-point)))))))

(defun move-spill/fill-compatible (allocator move)
  (if (and (typep (ir:move-source move) 'ir:virtual-register)
           (typep (ir:move-destination move) 'ir:virtual-register))
      (spill/fill-register-kinds-compatible (ir:virtual-register-kind (ir:move-source move))
                                            (ir:virtual-register-kind (ir:move-destination move))
                                            (allocator-architecture allocator))
      ;; Moves to/from physical registers will have been inserted by the backend.
      ;; Assume it know what it's doing and that they're always compatible.
      t))

(defun rewrite-move-instruction (allocator backend-function inst instruction-index spilled-input-vregs spilled-output-vregs)
  (cond ((and spilled-input-vregs
              spilled-output-vregs
              (move-spill/fill-compatible allocator inst))
         ;; Both the input & the output were spilled.
         ;; Fill into the instant for the input, then spill directly into the output.
         (let ((reg (instant-register-at allocator (ir:move-source inst) instruction-index)))
           (ir:insert-before backend-function inst
                             (make-instance 'ir:fill-instruction
                                            :destination reg
                                            :source (ir:move-source inst)))
           (ir:insert-before backend-function inst
                             (make-instance 'ir:spill-instruction
                                            :destination (ir:move-destination inst)
                                            :source reg))
           (ir:remove-instruction backend-function inst)))
        ((and spilled-input-vregs
              (move-spill/fill-compatible allocator inst))
         ;; Input was spilled, fill directly into the output.
         (let ((reg (if (typep (ir:move-destination inst) 'ir:virtual-register)
                        (gethash (interval-at allocator (ir:move-destination inst) instruction-index) (allocator-range-allocations allocator))
                        (ir:move-destination inst))))
           (ir:insert-before backend-function inst
                             (make-instance 'ir:fill-instruction
                                            :destination reg
                                            :source (ir:move-source inst)))
           (ir:remove-instruction backend-function inst)))
        ((and spilled-output-vregs
              (move-spill/fill-compatible allocator inst))
         ;; Output was spilled, spill directly into the output.
         (let ((reg (if (typep (ir:move-source inst) 'ir:virtual-register)
                        (gethash (interval-at allocator (ir:move-source inst) instruction-index) (allocator-range-allocations allocator))
                        (ir:move-source inst))))
           (ir:insert-before backend-function inst
                             (make-instance 'ir:spill-instruction
                                            :destination (ir:move-destination inst)
                                            :source reg))
           (ir:remove-instruction backend-function inst)))
        ((and (endp spilled-input-vregs)
              (endp spilled-output-vregs)
              (eql (if (typep (ir:move-source inst) 'ir:virtual-register)
                       (gethash (interval-at allocator (ir:move-source inst) instruction-index) (allocator-range-allocations allocator))
                       (ir:move-source inst))
                   (if (typep (ir:move-destination inst) 'ir:virtual-register)
                       (gethash (interval-at allocator (ir:move-destination inst) instruction-index) (allocator-range-allocations allocator))
                       (ir:move-destination inst))))
         ;; Source & destination are the same register (not spilled), eliminate the move.
         (ir:remove-instruction backend-function inst))
        (t
         ;; Just rewrite the instruction.
         (rewrite-ordinary-instruction allocator backend-function inst instruction-index spilled-input-vregs spilled-output-vregs))))

(defun rewrite-after-allocation (allocator)
  (loop
     with backend-function = (allocator-backend-function allocator)
     for instruction-index from 0
     for inst in (allocator-instruction-ordering allocator)
     do
       (let* ((input-vregs (remove-duplicates
                            (remove-if-not (lambda (r)
                                             (typep r 'ir:virtual-register))
                                           (ir::instruction-inputs inst))))
              (spilled-input-vregs (remove-if-not (lambda (vreg)
                                                    (spilledp allocator vreg instruction-index))
                                                  input-vregs))
              (output-vregs (remove-duplicates
                             (remove-if-not (lambda (r)
                                              (typep r 'ir:virtual-register))
                                            (ir::instruction-outputs inst))))
              (spilled-output-vregs (remove-if-not (lambda (vreg)
                                                     (spilledp allocator vreg instruction-index))
                                                   output-vregs)))
         (typecase inst
           (ir:move-instruction
            (rewrite-move-instruction allocator backend-function inst instruction-index spilled-input-vregs spilled-output-vregs))
           (ir:terminator-instruction
            ;; Some terminators may read values (branch/switch/return), so do ordinary processing on them as well.
            (rewrite-ordinary-instruction allocator backend-function inst instruction-index spilled-input-vregs spilled-output-vregs)
            (rewrite-terminator-instruction allocator inst instruction-index))
           (t
            (rewrite-ordinary-instruction allocator backend-function inst instruction-index spilled-input-vregs spilled-output-vregs))))))

(defun allocate-registers (backend-function arch &key ordering)
  (sys.c:with-metering (:backend-register-allocation)
    (let ((allocator (make-linear-allocator backend-function arch :ordering ordering)))
      (build-live-ranges allocator)
      (linear-scan-allocate allocator)
      (rewrite-after-allocation allocator))))
