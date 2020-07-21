;;;; Linear-scan register allocator.

(in-package :mezzano.compiler.backend.register-allocator)

(defvar *log* nil)

(defgeneric target-argument-registers (target))
(defgeneric target-return-register (target))
(defgeneric target-funcall-register (target))
(defgeneric target-count-register (target))

(defgeneric architectural-physical-registers (architecture))
(defgeneric valid-physical-registers-for-kind (kind architecture))

(defgeneric instruction-clobbers (instruction architecture)
  (:method (i a) '()))

(defgeneric instruction-inputs-read-before-outputs-written-p (instruction architecture)
  (:method (i a) nil)
  (:method ((i ir:move-instruction) a) t)
  (:documentation "Can input and output registers share the same physical register?"))

(defgeneric allow-memory-operand-p (instruction operand architecture)
  ;; Instructions don't support memory operands by default.
  (:method (i o a)
    nil))

;; Can foo/bar register kinds be mixed in spill/fill code?
(defgeneric spill/fill-register-kinds-compatible (kind1 kind2 architecture)
  (:method (kind1 kind2 architecture)
    (eql kind1 kind2)))

#|
;; Original recursive implementation.
;; Uses too much stack space when compiling Ironclad.
(defun instructions-reverse-postorder (backend-function)
  "Return instructions in reverse postorder."
  (let ((visited (make-hash-table :test 'eq))
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
|#

(defun instructions-reverse-postorder (backend-function)
  (let ((visited (make-hash-table :test 'eq))
        (order '())
        (stack (make-array 128 :adjustable t :fill-pointer 0)))
    ;; instruction, additional-basic-blocks, block-instruction-order, at-end.
    (vector-push-extend (list (ir:first-instruction backend-function) '() '() nil) stack)
    (setf (gethash (ir:first-instruction backend-function) visited) t)
    (loop
       (when (eql (length stack) 0) (return))
       (let ((current (aref stack (1- (length stack)))))
         (loop
              (let ((inst (first current)))
                (when (fourth current) ; at-end, just finishing up.
                  (setf order (append (reverse (third current))
                                      order))
                  (vector-pop stack)
                  (return))
                (push inst (third current))
                (when (typep inst 'ir:terminator-instruction)
                  (dolist (succ (reverse
                                 (union (ir:successors backend-function inst)
                                        (second current))))
                    (when (not (gethash succ visited))
                      (setf (gethash succ visited) t)
                      (vector-push-extend (list succ '() '() nil) stack)))
                  (setf (fourth current) t)
                  (return))
                (when (typep inst 'ir:begin-nlx-instruction)
                  (setf (second current) (union (second current)
                                                (ir:begin-nlx-targets inst))))
                (setf (first current) (ir:next-instruction backend-function inst))))))
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
   (%vreg-id :initarg :vreg-id :reader live-range-vreg-id)
   ;; Start & end (inclusive) of this range, in allocator order.
   (%start :initarg :start :reader live-range-start)
   (%end :initarg :end :reader live-range-end)
   ;; Physical registers this range conflicts with and cannot be allocated in.
   (%conflicts :initarg :conflicts :reader live-range-conflicts)
   ;; Zombie ranges cover areas where the vreg is effectively dead, but
   ;; kept alive because it is associated with some live variable and needed
   ;; for debugging.
   ;; The register allocator always spills these ranges.
   (%zombie :initarg :zombie :reader live-range-zombie)))

(defmethod print-object ((object live-range) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~S ~S-~S ~:S"
            (live-range-vreg object)
            (live-range-start object) (live-range-end object)
            (live-range-conflicts object))
    (when (live-range-zombie object)
      (format stream " [zombie]"))))

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
  (let ((result '()))
    (flet ((frob (values)
             (dolist (x values)
               (when (typep x 'ir:virtual-register)
                 (pushnew x result :test 'eq)))))
      (frob (gethash inst live-in))
      (frob (gethash inst live-out))
      ;; If a vreg isn't used, then it won't show up in the liveness maps.
      ;; Scan the instruction's outputs to catch this.
      (frob (ir::instruction-outputs inst)))
    result))

(defclass linear-allocator ()
  ((%vreg-to-id-table :initarg :vreg-to-id :reader allocator-vreg-to-id-table)
   (%id-vreg-table :initarg :id-to-vreg :reader allocator-id-to-vreg-table)
   (%function :initarg :function :reader allocator-backend-function)
   (%ordering :initarg :ordering :reader allocator-instruction-ordering)
   (%instruction-to-index-table :initarg :instruction-to-index-table :reader allocator-instruction-to-index-table)
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
   (%instruction-clobbers :initarg :instruction-clobbers :reader allocator-instruction-clobbers)
   (%range-starts :initarg :range-starts :accessor allocator-range-starts)
   (%debug-variable-value-map :initarg :debug-variable-value-map :accessor allocator-debug-variable-value-map)
   (%max-debug-p :initarg :max-debug-p :reader max-debug-p)))

(defun program-ordering (backend-function)
  (let ((order '()))
    (ir:do-instructions (inst backend-function)
      (push inst order))
    (nreverse order)))

(defun number-virtual-registers (backend-function)
  (let ((vreg-to-id (make-hash-table :test 'eq))
        (id-to-vreg (make-array 0 :adjustable t :fill-pointer 0)))
    (flet ((add (reg)
             (when (and (typep reg 'ir:virtual-register)
                        (not (gethash reg vreg-to-id nil)))
               (setf (gethash reg vreg-to-id) (vector-push-extend reg id-to-vreg)))))
      (ir:do-instructions (inst backend-function)
        (mapcar #'add (ir:instruction-inputs inst))
        (mapcar #'add (ir:instruction-outputs inst))))
    (values vreg-to-id id-to-vreg)))

(deftype vreg-id () 'fixnum)

(defun make-linear-allocator (backend-function architecture &key ordering)
  (multiple-value-bind (basic-blocks bb-preds bb-succs)
      (ir::build-cfg backend-function)
    (declare (ignore basic-blocks bb-succs))
    (multiple-value-bind (live-in live-out)
        (ir::compute-liveness backend-function architecture)
      ;; Construct the debug map & remove debug instructions after computing liveness
      ;; so that live ranges extend to the debug instructions (and the beginning of their zombie ranges).
      (let ((debug-map (ir::build-debug-variable-value-map backend-function)))
        (ir::remove-debug-variable-instructions backend-function)
        (let* ((order (if ordering
                          (funcall ordering backend-function)
                          (instructions-reverse-postorder backend-function)))
               (instruction-to-index-table (make-hash-table :test 'eq))
               (mv-flow (ir::multiple-value-flow backend-function architecture))
               (clobbers (make-hash-table :test 'eq)))
          (loop
             for i from 0
             for inst in order
             do
               (setf (gethash inst clobbers) (instruction-all-clobbers inst architecture mv-flow live-in live-out))
               (setf (gethash inst instruction-to-index-table) i))
          (multiple-value-bind (vreg-to-id id-to-vreg)
              (number-virtual-registers backend-function)
            (make-instance 'linear-allocator
                           :function backend-function
                           :ordering order
                           :instruction-to-index-table instruction-to-index-table
                           :architecture architecture
                           :live-in live-in
                           :live-out live-out
                           :mv-flow mv-flow
                           :cfg-preds bb-preds
                           :instruction-clobbers clobbers
                           :debug-variable-value-map debug-map
                           :max-debug-p (= (mezzano.compiler::optimize-quality (ir::ast backend-function) 'debug) 3)
                           :vreg-to-id vreg-to-id
                           :id-to-vreg id-to-vreg)))))))

(defun virtual-registers-used-by-debug-info (allocator inst)
  (if (max-debug-p allocator)
      (mapcar #'second (gethash inst (allocator-debug-variable-value-map allocator)))
      '()))

(defun build-live-ranges (allocator)
  (let* ((vreg-to-id (allocator-vreg-to-id-table allocator))
         (ranges (make-array 128 :adjustable t :fill-pointer 0))
         (vreg-ranges (make-hash-table :test 'eq))
         (ordering (allocator-instruction-ordering allocator))
         (live-in (allocator-live-in allocator))
         (live-out (allocator-live-out allocator))
         (active-vregs '())
         (active-zombie-vregs '())
         (vreg-liveness-start (make-hash-table :test 'eq))
         (vreg-conflicts (make-hash-table :test 'eq))
         (vreg-move-hint (make-hash-table :test 'eq))
         (arch (allocator-architecture allocator))
         (arch-phys-regs (architectural-physical-registers arch))
         (arg-regs (target-argument-registers arch))
         (funcall-reg (target-funcall-register arch))
         (starts (make-hash-table))
         (valid-pregs-cache-kind nil)
         (valid-pregs-cache-regs nil))
    (labels ((valid-pregs (vreg)
               ;; V-P-R-F-K uses multiple dispatch with EQL specializers, slow.
               ;; Use a simple cache to mitigate this.
               (let ((kind (ir:virtual-register-kind vreg)))
                 (cond ((eql kind valid-pregs-cache-kind)
                        valid-pregs-cache-regs)
                       (t
                        (let ((regs (valid-physical-registers-for-kind kind arch)))
                          (setf valid-pregs-cache-kind kind
                                valid-pregs-cache-regs regs)
                          regs)))))
             (add-range (vreg end zombiep)
               (when (not zombiep)
                 (setf (gethash vreg vreg-conflicts) (union (gethash vreg vreg-conflicts)
                                                            (set-difference arch-phys-regs
                                                                            (valid-pregs vreg)))))
               (let* ((start (gethash vreg vreg-liveness-start))
                      (range (make-instance 'live-range
                                            :vreg vreg
                                            :vreg-id (gethash vreg vreg-to-id)
                                            :start start
                                            :end end
                                            :conflicts (if zombiep '() (gethash vreg vreg-conflicts))
                                            :zombie zombiep)))
                 (when (not ir::*shut-up*)
                   (format t " Add range ~S~%" range))
                 (push range (gethash start starts '()))
                 (vector-push-extend range ranges)
                 (push range (gethash vreg vreg-ranges)))))
      (loop
         for range-start from 0
         for inst in ordering
         do
           (let* ((clobbers (gethash inst (allocator-instruction-clobbers allocator)))
                  (vregs (virtual-registers-touched-by-instruction inst live-in live-out))
                  (newly-live-vregs (set-difference vregs active-vregs))
                  (newly-dead-vregs (set-difference active-vregs vregs))
                  (debug-vregs (virtual-registers-used-by-debug-info allocator inst))
                  (zombie-vregs (set-difference debug-vregs vregs))
                  (newly-live-zombie-vregs (set-difference zombie-vregs active-zombie-vregs))
                  (newly-dead-zombie-vregs (set-difference active-zombie-vregs zombie-vregs)))
             (when (not ir::*shut-up*)
               (format t "~D:" range-start)
               (ir::print-instruction inst)
               (format t "active: ~:S~%" active-vregs)
               (format t "vregs: ~:S~%" vregs)
               (format t "newly live: ~:S~%" newly-live-vregs)
               (format t "newly dead: ~:S~%" newly-dead-vregs)
               (format t "debug-vregs: ~:S~%" debug-vregs)
               (format t "zombie-vregs: ~:S~%" zombie-vregs)
               (format t "newly live zombie: ~:S~%" newly-live-zombie-vregs)
               (format t "newly dead zombie: ~:S~%" newly-dead-zombie-vregs))
             ;; Process vregs that have just become dead.
             (let ((range-end (1- range-start)))
               (dolist (vreg newly-dead-vregs)
                 (add-range vreg range-end nil)))
             ;; And zombie vregs that have just become dead.
             (let ((range-end (1- range-start)))
               (dolist (vreg newly-dead-zombie-vregs)
                 (add-range vreg range-end t)))
             ;; Process vregs that have just become live.
             (dolist (vreg newly-live-vregs)
               (setf (gethash vreg vreg-liveness-start) range-start)
               (setf (gethash vreg vreg-conflicts) '()))
             ;; and zombie vregs that have just become live.
             (dolist (vreg newly-live-zombie-vregs)
               (setf (gethash vreg vreg-liveness-start) range-start))
             (setf active-vregs vregs)
             (setf active-zombie-vregs zombie-vregs)
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
                 (when (member vreg (ir:argument-setup-required inst))
                   (setf (gethash vreg vreg-move-hint) (nth (position vreg (ir:argument-setup-required inst))
                                                            arg-regs)))))))
      ;; Finish any remaining active ranges.
      (let ((range-end (1- (length ordering))))
        (dolist (vreg active-vregs)
          (add-range vreg range-end nil))
        (dolist (vreg active-zombie-vregs)
          (add-range vreg range-end t))))
    (setf (slot-value allocator '%ranges) (sort ranges #'> :key #'live-range-start)
          (slot-value allocator '%vreg-ranges) (let ((real-vreg-ranges (make-hash-table :test 'eq)))
                                                 ;; INTERVAL-AT binary searches through this, ranges must be sorted.
                                                 (loop
                                                    for vreg being the hash-keys of vreg-ranges using (hash-value ranges)
                                                    do
                                                      (setf (gethash vreg real-vreg-ranges) (sort (make-array (length ranges)
                                                                                                              :initial-contents ranges)
                                                                                                  #'<
                                                                                                  :key #'live-range-start)))
                                                 real-vreg-ranges)
          (slot-value allocator '%vreg-hints) vreg-move-hint
          (allocator-range-starts allocator) starts)
    (when *log*
      (format *log* ",~D,~D,~D"
              (length (allocator-remaining-ranges allocator))
              (hash-table-count (allocator-vreg-to-id-table allocator))
              (length ordering))))
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
          (cond ((live-range-zombie interval)
                 ;; Zombie range, spill.
                 (mark-interval-spilled allocator interval))
                ((and (typep inst 'ir:argument-setup-instruction)
                      (eql instruction-index (live-range-end interval)))
                 ;; Argument setup instruction with an unused argument.
                 ;; Just spill it.
                 (mark-interval-spilled allocator interval))
                ((not reg)
                 (spill-at-interval allocator interval))
                (t
                 (setf (allocator-free-registers allocator) (remove reg (allocator-free-registers allocator)))
                 (activate-interval allocator interval reg))))))

(defun allocate-instants (allocator inst instruction-index avoid-registers)
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
         (available-regs (set-difference (remove-if (lambda (preg)
                                                      (member preg used-pregs))
                                                    (allocator-free-registers allocator))
                                         avoid-registers)))
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
      (let* ((valid-pregs (valid-physical-registers-for-kind (ir:virtual-register-kind vreg)
                                                             (allocator-architecture allocator)))
             (truely-available-regs (intersection available-regs valid-pregs)))
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
                                                               used-pregs)
                                                       ;; Or anything that's not valid for this kind.
                                                       (not (member (gethash spill-interval (allocator-range-allocations allocator))
                                                                    valid-pregs))))
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

(defun deactivate-interval (allocator interval)
  (setf (allocator-active-ranges allocator) (remove interval
                                                    (allocator-active-ranges allocator))))

(defun linear-scan-allocate (allocator)
  (setf (allocator-active-ranges allocator) '()
        (allocator-range-allocations allocator) (make-hash-table :test 'eq)
        (allocator-free-registers allocator) (architectural-physical-registers (allocator-architecture allocator))
        (allocator-spilled-ranges allocator) (make-hash-table :test 'eq)
        (allocator-instantaneous-allocations allocator) (make-hash-table :test 'equal))
  (loop
     with arch = (allocator-architecture allocator)
     for inst in (allocator-instruction-ordering allocator)
     for instruction-index from 0
     do
       (expire-old-intervals allocator instruction-index)
       (unless ir::*shut-up*
         (format t "~D:" instruction-index)
         (ir::print-instruction inst)
         (format t "actives ~:S~%" (allocator-active-ranges allocator)))
       (let ((instant-avoid-registers '()))
         ;; If this instruction supports having inputs & outputs allocated to the same register,
         ;; then expire inputs ending here to make their registers available before output allocation.
         (when (instruction-inputs-read-before-outputs-written-p inst arch)
           (dolist (input (ir::instruction-inputs inst))
             (when (typep input 'ir:virtual-register)
               (let ((interval (interval-at allocator input instruction-index)))
                 (when (and (not (interval-spilled-p allocator interval))
                            (eql (live-range-end interval) instruction-index)
                            ;; Must not also be an output register.
                            (not (member input (ir::instruction-outputs inst))))
                   (let ((reg (gethash interval (allocator-range-allocations allocator))))
                     (when (not ir::*shut-up*)
                       (format t "Return interval ~S (reg ~S) early.~%" interval reg))
                     (deactivate-interval allocator interval)
                     (push reg (allocator-free-registers allocator))
                     (push reg instant-avoid-registers)))))))
         (update-active-intervals allocator inst instruction-index)
         (allocate-instants allocator inst instruction-index instant-avoid-registers)))
  (expire-old-intervals allocator (length (allocator-instruction-ordering allocator)))
  (assert (endp (allocator-active-ranges allocator))))

(defun interval-at (allocator vreg index)
  ;; IMIN/IMAX are inclusive indicies.
  (do* ((intervals (gethash vreg (allocator-vreg-ranges allocator) #()))
        (imin 0)
        (imax (1- (length intervals))))
      ((< imax imin)
       (error "Missing interval for ~S at index ~S" vreg index))
    (declare (type simple-vector intervals))
    (let* ((imid (ash (+ imin imax) -1))
           (interval (svref intervals imid))
           (start (live-range-start interval)))
      (cond ((<= start index (live-range-end interval))
             (return interval))
            ((< start index)
             (setf imin (1+ imid)))
            (t
             (setf imax (1- imid)))))))

(defun interval-spilled-p (allocator interval)
  (gethash interval (allocator-spilled-ranges allocator) nil))

(defun spilledp (allocator vreg index)
  (interval-spilled-p allocator (interval-at allocator vreg index)))

(defun instant-register-at (allocator vreg index)
  (or (gethash (cons index vreg) (allocator-instantaneous-allocations allocator))
      (gethash (interval-at allocator vreg index) (allocator-range-allocations allocator))
      (error "Missing instantaneous allocation for ~S at ~S" vreg index)))

(defun fix-locations-after-control-flow (allocator inst instruction-index target insert-point)
  (let* ((target-index (gethash target (allocator-instruction-to-index-table allocator)))
         (active-vregs (union (remove-if-not (lambda (reg) (typep reg 'ir:virtual-register))
                                             (gethash target (allocator-live-in allocator)))
                              (virtual-registers-used-by-debug-info allocator inst)))
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
         (spill-pairs '())
         (fill-pairs '())
         (pairs '()))
    (loop
       for in in input-registers
       for out in output-registers
       do
         (cond ((eql in out)) ; nothing.
               ((and (typep in 'ir:virtual-register)
                     (typep out 'ir:virtual-register))
                ;; Impossible.
                (error "Tried to fix spill -> spill ~S ~S?" in out))
               ((typep in 'ir:virtual-register)
                (push (cons in out) fill-pairs))
               ((typep out 'ir:virtual-register)
                (push (cons in out) spill-pairs))
               (t
                (push (cons in out) pairs))))
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
        (format t "  spills: ~:S~%" spill-pairs)
        (format t "   fills: ~:S~%" fill-pairs)
        (format t "   pairs: ~:S~%" pairs))
      ;; Process spills.
      (loop
         for (in . out) in spill-pairs
         do (insert (make-instance 'ir:spill-instruction
                                   :source in
                                   :destination out)))
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
      ;; Process fills.
      (loop
         for (in . out) in fill-pairs
         do (insert (make-instance 'ir:fill-instruction
                                   :source in
                                   :destination out))))))

(defun break-critical-edge (backend-function terminator target)
  "Break the first edge from terminator to target."
  (assert (endp (ir:label-phis target)))
  (let ((l (make-instance 'ir:label :name :broken-critical-edge)))
    (etypecase terminator
      (ir:branch-instruction
       (cond ((eql (ir:branch-true-target terminator) target)
              (setf (ir:branch-true-target terminator) l))
             (t
              (setf (ir:branch-false-target terminator) l)))
       (ir:insert-before backend-function target l))
      (ir:switch-instruction
       (do ((i (ir:switch-targets terminator)
               (rest i)))
           ((endp i))
         (when (eql (first i) target)
           (ir:insert-before backend-function target l)
           (setf (first i) l)
           (return))))
      (mezzano.compiler.backend.x86-64::x86-branch-instruction
       (cond ((eql (mezzano.compiler.backend.x86-64::x86-branch-true-target terminator) target)
              (setf (mezzano.compiler.backend.x86-64::x86-branch-true-target terminator) l))
             (t
              (setf (mezzano.compiler.backend.x86-64::x86-branch-false-target terminator) l)))
       (ir:insert-before backend-function target l))
      (mezzano.compiler.backend.arm64::arm64-branch-instruction
       (cond ((eql (mezzano.compiler.backend.arm64::arm64-branch-true-target terminator) target)
              (setf (mezzano.compiler.backend.arm64::arm64-branch-true-target terminator) l))
             (t
              (setf (mezzano.compiler.backend.arm64::arm64-branch-false-target terminator) l)))
       (ir:insert-before backend-function target l)))
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
         (when (not (typep inst 'ir:terminator-instruction))
           ;; Spill registers that become zombies after the next instruction.
           (dolist (range (gethash (1+ instruction-index)
                                   (allocator-range-starts allocator)))
             (when (and (live-range-zombie range)
                        (not (spilledp allocator (live-range-vreg range) instruction-index)))
               (ir:insert-after backend-function inst
                                (make-instance 'ir:spill-instruction
                                               :destination (live-range-vreg range)
                                               :source (or (gethash (interval-at allocator (live-range-vreg range) instruction-index)
                                                                    (allocator-range-allocations allocator))
                                                           (error "Missing register to spill for new zombie range?")))))))
         (typecase inst
           (ir:move-instruction
            (rewrite-move-instruction allocator backend-function inst instruction-index spilled-input-vregs spilled-output-vregs))
           (ir:terminator-instruction
            ;; Some terminators may read values (branch/switch/return), so do ordinary processing on them as well.
            (rewrite-ordinary-instruction allocator backend-function inst instruction-index spilled-input-vregs spilled-output-vregs)
            (rewrite-terminator-instruction allocator inst instruction-index))
           (t
            (rewrite-ordinary-instruction allocator backend-function inst instruction-index spilled-input-vregs spilled-output-vregs))))))

(defun rebuild-debug-map (allocator)
  (let ((result (make-hash-table))
        (original-debug-map (allocator-debug-variable-value-map allocator)))
    ;; Walk ranges & known instructions to produce a partial debug map.
    ;; Only valid for instructions originally in the function.
    (loop
       with starts = (allocator-range-starts allocator)
       with active-ranges = '()
       with active-vreg-to-range-table = (make-hash-table)
       with range-allocations = (allocator-range-allocations allocator)
       for index from 0
       for inst in (allocator-instruction-ordering allocator)
       do
         (setf active-ranges (remove-if (lambda (range)
                                          (< (live-range-end range) index))
                                        active-ranges))
         (dolist (new-range (gethash index starts))
           (setf (gethash (live-range-vreg new-range) active-vreg-to-range-table) new-range)
           (push new-range active-ranges))
         (loop
            for (variable vreg repr) in (gethash inst original-debug-map)
            for range = (gethash vreg active-vreg-to-range-table)
            for location = (if (interval-spilled-p allocator range)
                               vreg
                               (gethash range range-allocations))
            do
            ;; Mark vregs as used.
              (setf (gethash vreg result) t)
              (push (list variable location repr) (gethash inst result '()))))
    ;; Now loop over all instructions & patch up any that were missed.
    (let ((last-entry '()))
      (ir:do-instructions (inst (allocator-backend-function allocator))
        (multiple-value-bind (existing-debug-entry presentp)
            (gethash inst result)
          (cond (presentp
                 (setf last-entry existing-debug-entry))
                (t
                 (setf (gethash inst result) last-entry))))))
    (setf (allocator-debug-variable-value-map allocator) result)))

(defun unused-spill-p (allocator range)
  ;; Prevents arguments (including count register) from being spilled
  ;; if they are not used.
  (declare (ignore allocator))
  (and (eql (live-range-start range) 0)
       (eql (live-range-end range) 0)))

(defun all-spilled-ranges (allocator)
  "Return a vector of ranges for every spilled variable."
  (let ((result (make-array (hash-table-count (allocator-spilled-ranges allocator))
                            :adjustable t
                            :fill-pointer 0))
        (spilled-variables '()))
    (loop
       for range being the hash-keys in (allocator-spilled-ranges allocator)
       when (not (unused-spill-p allocator range))
       do (pushnew (live-range-vreg range) spilled-variables))
    (dolist (vreg spilled-variables)
      (loop
         for range across (gethash vreg (allocator-vreg-ranges allocator))
         do (vector-push-extend range result)))
    result))

(defun make-vreg-set ()
  (make-array 0))

(defun insert-into-vreg-set (set vreg)
  ;; Search for the position to insert into.
  (cond ((or (null set)
             (eql set vreg))
         vreg)
        ((typep set 'vreg-id)
         (let ((vec (make-array 2)))
           (if (< set vreg)
               (setf (svref vec 0) set
                     (svref vec 1) vreg)
               (setf (svref vec 0) vreg
                     (svref vec 1) set))
           vec))
        (t
         ;; IMIN/IMAX are inclusive indicies.
         (do* ((imin 0)
               (imax (1- (length set))))
              ((< imax imin)
               ;; Inserting at IMIN.
               (let ((new (make-array (1+ (length set)))))
                 (replace new set :end2 imin)
                 (replace new set :start1 (1+ imin) :start2 imin)
                 (setf (svref new imin) vreg)
                 new))
           (declare (type simple-vector set))
           (let* ((imid (ash (+ imin imax) -1))
                  (entry (svref set imid)))
             (cond ((eql entry vreg)
                    ;; Already in the array, return.
                    (return set))
                   ((< entry vreg)
                    (setf imin (1+ imid)))
                   (t
                    (setf imax (1- imid)))))))))

(defun vreg-set-contains (set vreg)
  (when (null set)
    (return-from vreg-set-contains nil))
  (when (typep set 'vreg-id)
    (return-from vreg-set-contains (eql set vreg)))
  (do* ((imin 0)
        (imax (1- (length set))))
       ((< imax imin)
        nil)
    (declare (type (vector t) set))
    (let* ((imid (ash (+ imin imax) -1))
           (entry (svref set imid)))
      (cond ((eql entry vreg)
             (return t))
            ((< entry vreg)
             (setf imin (1+ imid)))
            (t
             (setf imax (1- imid)))))))

(defun build-interference-graph (allocator)
  "Construct the interference graph for spilled virtual registers.
Returns the interference graph and the set of spilled virtual registers."
  (when (not ir::*shut-up*)
    (loop
       for i from 0
       for inst in (allocator-instruction-ordering allocator)
       do (format t "~D: " i) (ir::print-instruction inst)))
  (let ((result (make-array (length (allocator-id-to-vreg-table allocator)) :initial-element nil))
        (spilled-vregs (make-vreg-set))
        (spilled-ranges (sort (all-spilled-ranges allocator)
                              #'<
                              :key #'live-range-start))
        (live '()))
    (loop for range across spilled-ranges do
         (when (not ir::*shut-up*)
           (let ((*print-length* 2))
             (format t "Process range ~S~%" range)
             (dolist (r live)
               (format t "   ~S~%" r))))
         (let ((current-range-start (live-range-start range))
               (current-vreg (live-range-vreg-id range)))
           (setf spilled-vregs (insert-into-vreg-set spilled-vregs current-vreg))
           ;; Expire old ranges.
           (setf live (remove-if (lambda (x)
                                   (< (live-range-end x) current-range-start))
                                 live))
           ;; Add new interference edges.
           (dolist (range live)
             (flet ((insert (vreg other)
                      (setf (svref result vreg) (insert-into-vreg-set (svref result vreg) other))))
               (let ((other-vreg (live-range-vreg-id range)))
                 (when (and (not (eql other-vreg current-vreg))
                            (not (vreg-set-contains (svref result other-vreg) current-vreg)))
                   (insert current-vreg other-vreg)
                   (insert other-vreg current-vreg)))))
           (push range live)))
    (let ((size (length result)))
      (loop for val across result do
           (when (arrayp val)
             (incf size (length val))))
      (when *log*
        (format *log* ",~D,~D"
                size (length spilled-ranges)))
      #+(or)
      (format t "Interference graph has ~:D entries for ~:D spilled ranges~%" size (length spilled-ranges)))
    (values result spilled-vregs)))

(defun assign-stack-slots (allocator interference-graph spilled-vregs)
  (let ((slots (make-array 8 :adjustable t :fill-pointer 0 :initial-element '()))
        (slot-classes (make-array 8 :adjustable t :fill-pointer 0 :initial-element '()))
        (locations (make-hash-table))
        (id-to-vreg-table (allocator-id-to-vreg-table allocator)))
    (loop
       for vreg-id across spilled-vregs
       for vreg = (aref id-to-vreg-table vreg-id)
       do
         (dotimes (i (length slots)
                   (progn
                     ;; SSE slots are 2 wide.
                     ;; TODO: Force 16-byte alignment.
                     (when (eql (ir:virtual-register-kind vreg) :sse)
                       (vector-push-extend (list vreg-id) slots)
                       (vector-push-extend :pad slot-classes))
                     (setf (gethash vreg locations) (length slots))
                     (vector-push-extend (list vreg-id) slots)
                     (vector-push-extend (ir:virtual-register-kind vreg) slot-classes)))
           (when (and (eql (aref slot-classes i) (ir:virtual-register-kind vreg))
                      (not (dolist (entry (aref slots i) nil)
                             (when (vreg-set-contains (svref interference-graph entry) vreg-id)
                               (return t)))))
             ;; Does not interfere with any register in this slot.
             (push vreg-id (aref slots i))
             (setf (gethash vreg locations) i)
             (return))))
    (values locations slot-classes)))

(defun allocate-registers (backend-function arch &key ordering)
  (when *log*
    (format *log* "~S" (format nil "~A" (ir::backend-function-name backend-function))))
  ;(sb-ext:gc :full t)
  (let ((allocator (make-linear-allocator backend-function arch :ordering ordering)))
    (build-live-ranges allocator)
    (linear-scan-allocate allocator)
    (multiple-value-bind (spill-locations stack-layout)
        (multiple-value-bind (interference-graph spilled-vregs)
            (build-interference-graph allocator)
          (assign-stack-slots allocator interference-graph spilled-vregs))
      (rewrite-after-allocation allocator)
      (cond ((= (mezzano.compiler::optimize-quality (ir::ast backend-function) 'debug) 0)
             (setf (allocator-debug-variable-value-map allocator) (make-hash-table)))
            (t
             (rebuild-debug-map allocator)))
      (when *log*
        (terpri *log*)
        (finish-output *log*))
      ;; Return the updated debug map and the stack layout.
      (values (allocator-debug-variable-value-map allocator)
              spill-locations
              stack-layout
              allocator))))
