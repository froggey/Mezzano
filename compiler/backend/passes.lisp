;;;; General optimization passes.

(in-package :mezzano.compiler.backend)

(defun remove-unused-local-variables (backend-function)
  "Remove all locals that are not loaded."
  (let ((loaded-variables (make-hash-table :test 'eq))
        (stores '())
        (binds '())
        (unbinds '()))
    (do-instructions (inst backend-function)
      (typecase inst
        (load-local-instruction
         (setf (gethash (load-local-local inst) loaded-variables) t))
        (store-local-instruction
         (push inst stores))
        (bind-local-instruction
         (push inst binds))
        (unbind-local-instruction
         (push inst unbinds))))
    (dolist (inst stores)
      (when (not (gethash (store-local-local inst) loaded-variables))
        (remove-instruction backend-function inst)))
    (dolist (inst binds)
      (when (not (gethash inst loaded-variables))
        (remove-instruction backend-function inst)))
    (dolist (inst unbinds)
      (when (not (gethash (unbind-local-local inst) loaded-variables))
        (remove-instruction backend-function inst)))
    (length binds)))

(defun replace-all-uses (old-vreg new-vreg uses)
  (dolist (u (gethash old-vreg uses))
    (replace-all-registers u
                           (lambda (reg)
                             (cond ((eql reg old-vreg)
                                    new-vreg)
                                   (t reg))))))

(defun eliminate-redundant-boxing (backend-function)
  "Eliminate nested box/unbox instructions."
  (multiple-value-bind (uses defs)
      (build-use/def-maps backend-function)
    (let ((n 0)
          (remove-me '()))
      (do-instructions (inst backend-function)
        (flet ((frob (dst src)
                 (cond ((eql (virtual-register-kind dst)
                             (virtual-register-kind src))
                        ;; Same kind, replace dst with src.
                        (replace-all-uses dst src uses))
                       (t
                        ;; Different kind. Replace with a move.
                        (insert-before backend-function inst
                                       (make-instance 'move-instruction
                                                      :destination dst
                                                      :source src))))
                 (push inst remove-me)
                 (incf n)))
          (typecase inst
            (unbox-instruction
             (let ((def (first (gethash (unbox-source inst) defs))))
               (loop
                  while (typep def 'move-instruction)
                  do (setf def (first (gethash (move-source def) defs))))
               (when (or (and (typep def 'box-instruction)
                              (eql (box-type inst) (box-type def)))
                         ;; Special case boxing various integer pairs.
                         (and (typep inst 'unbox-unsigned-byte-64-instruction)
                              (or (typep def 'box-fixnum-instruction)
                                  (typep def 'box-signed-byte-64-instruction))))
                 (frob (unbox-destination inst) (box-source def)))))
            (box-instruction
             (let ((def (first (gethash (box-source inst) defs))))
               (loop
                  while (typep def 'move-instruction)
                  do (setf def (first (gethash (move-source def) defs))))
               (when (and (typep def 'unbox-instruction)
                          (eql (box-type inst) (box-type def))
                          ;; Make sure we keep this value around if the box
                          ;; result becomes visible.
                          ;; FIXME: This is only relevant for pairs that have
                          ;; different layouts (f32.4 vs f64.2, etc).
                          (or (not (eql (box-type inst) 'mezzano.simd:simd-pack))
                              (notany (lambda (use)
                                        (typep use '(or base-call-instruction
                                                     return-instruction
                                                     invoke-nlx-instruction
                                                     values-instruction)))
                                      (gethash (box-destination inst) uses))))
                 (frob (box-destination inst)
                       (unbox-source def))))))))
      (dolist (inst remove-me)
        (remove-instruction backend-function inst))
      n)))

(defun basic-block-terminator (backend-function basic-block)
  (loop
     until (typep basic-block 'terminator-instruction)
     do (setf basic-block (next-instruction backend-function basic-block)))
  basic-block)

(defun instruction-in-basic-block-p (backend-function basic-block instruction)
  (do ((inst (next-instruction backend-function basic-block)
             (next-instruction backend-function inst)))
      ((or (not inst)
           (typep inst 'label))
       nil)
    (when (eql inst instruction)
      (return t))))

(defun unbox-phis-phi-is-candidate-p (backend-function uses defs bb bb-preds index phi)
  (declare (ignore uses phi))
  (flet ((value-type (vreg)
           (let ((inst (first (gethash vreg defs))))
             (typecase inst
               (box-single-float-instruction
                'single-float)
               (box-double-float-instruction
                'double-float)
               (box-unsigned-byte-64-instruction
                ':unsigned-byte-64)
               (box-fixnum-instruction
                'fixnum)
               (constant-instruction
                (typecase (constant-value inst)
                  (single-float 'single-float)
                  (double-float 'double-float)
                  (fixnum 'fixnum)
                  ((unsigned-byte 64) ':unsigned-byte-64)
                  (t nil)))
               (t nil))))
         (pred-value (pred)
           (nth index (jump-values (basic-block-terminator backend-function pred)))))
    (let ((type (value-type (pred-value (first (gethash bb bb-preds))))))
      (when (and type
                 (every (lambda (pred)
                          (let ((other-type (value-type (pred-value pred))))
                            ;; Do a bit of finagling to get unsigned-byte-64s unboxed
                            ;; FIXME: This was wrong, fixnum is not a subtype of unsigned-byte-64 (due to negative numbers)
                            (cond ((eql type 'fixnum)
                                   (cond ((eql other-type 'fixnum)
                                          t)
                                         #+nil((eql other-type ':unsigned-byte-64)
                                          (setf type ':unsigned-byte-64)
                                          t)
                                         (t nil)))
                                  ((eql type ':unsigned-byte-64)
                                   (member other-type '(:unsigned-byte-64 #+nil fixnum)))
                                  (t
                                   (eql type other-type)))))
                        (rest (gethash bb bb-preds))))
        type))))

(defun unbox-phis-1 (backend-function basic-blocks bb-preds)
  "Attempt to represent phi nodes unboxed."
  (multiple-value-bind (uses defs)
      (build-use/def-maps backend-function)
    (dolist (bb (rest basic-blocks))
      (loop
         for index from 0
         for phi in (label-phis bb)
         when (eql (virtual-register-kind phi) :value)
         do
         ;; If all definitions of this phi are box or constant instructions
         ;; with the same type, then replace it with a new phi of the
         ;; appropriate type.
           (let ((type (unbox-phis-phi-is-candidate-p backend-function uses defs bb bb-preds index phi)))
             (when (and type
                        ;; Don't bother if it was a fixnum all along.
                        (not (eql type 'fixnum)))
               ;; Insert unbox instructions before every definition of the phi.
               (dolist (pred (gethash bb bb-preds))
                 (let* ((term (basic-block-terminator backend-function pred))
                        (value (nth index (jump-values term)))
                        (temp (make-instance 'virtual-register
                                             :kind (ecase type
                                                     (single-float :single-float)
                                                     (double-float :double-float)
                                                     (:unsigned-byte-64 :integer)))))
                   (insert-before backend-function term
                                  (make-instance (ecase type
                                                   (single-float 'unbox-single-float-instruction)
                                                   (double-float 'unbox-double-float-instruction)
                                                   (:unsigned-byte-64 'unbox-unsigned-byte-64-instruction))
                                                 :source value
                                                 :destination temp))
                   (setf (nth index (jump-values term)) temp)))
               ;; Insert box instructions before every use of the phi.
               ;; Insert before each use to keep new live ranges short.
               (dolist (user (gethash phi uses))
                 (let ((temp (make-instance 'virtual-register)))
                   (insert-before backend-function user
                                  (make-instance (ecase type
                                                   (single-float 'box-single-float-instruction)
                                                   (double-float 'box-double-float-instruction)
                                                   (:unsigned-byte-64 'box-unsigned-byte-64-instruction))
                                                 :source phi
                                                 :destination temp))
                   (replace-all-registers user
                                          (lambda (reg)
                                            (cond ((eql reg phi)
                                                   temp)
                                                  (t reg))))))
               ;; Change the phi kind.
               (setf (slot-value phi '%kind) (ecase type
                                               (single-float :single-float)
                                               (double-float :double-float)
                                               (:unsigned-byte-64 :integer)))
               (return-from unbox-phis-1 t))))))
  nil)

(defun unbox-phis (backend-function)
  "Attempt to represent phi nodes unboxed."
  (multiple-value-bind (basic-blocks bb-preds bb-succs)
      (build-cfg backend-function)
    (declare (ignore bb-succs))
    (let ((total 0))
      ;; UNBOX-PHIS-1 needs to rebuild the use/def map after changing things.
      (loop
         until (not (unbox-phis-1 backend-function basic-blocks bb-preds))
         do (incf total))
      total)))

(defun lower-one-local-variable (backend-function binding)
  (let* ((var (bind-local-ast binding))
         (vreg (make-instance 'virtual-register :name var))
         (to-remove '()))
    ;; Replace loads & stores with move instructions.
    (do-instructions (inst backend-function)
      (typecase inst
        (load-local-instruction
         (when (eql (load-local-local inst) binding)
           (insert-before backend-function inst
                          (make-instance 'move-instruction
                                         :source vreg
                                         :destination (load-local-destination inst)))
           (push inst to-remove)))
        (store-local-instruction
         (when (eql (store-local-local inst) binding)
           (insert-before backend-function inst
                          (make-instance 'move-instruction
                                         :source (store-local-value inst)
                                         :destination vreg))
           (push inst to-remove)))
        (unbind-local-instruction
         (when (eql (unbind-local-local inst) binding)
           (insert-before backend-function inst
                          (make-instance 'debug-unbind-variable-instruction
                                         :variable var))
           (push inst to-remove)))))
    ;; Initial value.
    (insert-before backend-function binding
                   (make-instance 'move-instruction
                                  :source (bind-local-value binding)
                                  :destination vreg))
    (insert-before backend-function binding
                   (make-instance 'debug-bind-variable-instruction
                                  :variable var
                                  :value vreg))
    (remove-instruction backend-function binding)
    (dolist (inst to-remove)
      (remove-instruction backend-function inst))))

(defun lower-local-variables (backend-function)
  "Convert remaining local variables to virtual registers.
Must be performed after SSA conversion."
  (let ((to-convert '()))
    (do-instructions (inst backend-function)
      (when (typep inst 'bind-local-instruction)
        (push inst to-convert)))
    (dolist (binding to-convert)
      (lower-one-local-variable backend-function binding))))

(defun rest-arg-safe-to-make-dx-p (backend-function uses)
  "Return true if the rest arg or it's spine is never captured by something with effectively non-dynamic extent and if the spine is never modified."
  (let ((worklist (list (argument-setup-rest (first-instruction backend-function))))
        (visited '())
        (failed nil))
    (loop until (endp worklist) do
         (let ((value (pop worklist)))
           (when (not (member value visited))
             (push value visited)
             (dolist (use (gethash value uses))
               (typecase use
                 ((or call-instruction call-multiple-instruction)
                  (case (call-function use)
                    ((copy-list)
                     ;; Must be first argument, result must meet checks.
                     (cond ((or (not (eql (length (call-arguments use)) 1))
                                (typep use 'call-multiple-instruction))
                            (setf failed t))
                           (t
                            (push (call-result use) worklist))))
                    ((cons)
                     ;; Must be second argument, result must meet checks.
                     (cond ((or (not (eql (length (call-arguments use)) 2))
                                (eql (first (call-arguments use)) value)
                                (typep use 'call-multiple-instruction))
                            (setf failed t))
                           (t
                            (push (call-result use) worklist))))
                    ((mezzano.runtime::%apply)
                     ;; Must be second argument.
                     (when (or (not (eql (length (call-arguments use)) 2))
                               (eql (first (call-arguments use)) value))
                       (setf failed t)))
                    ((get-properties)
                     ;; Must be first argument.
                     (when (or (not (eql (length (call-arguments use)) 2))
                               (eql (second (call-arguments use)) value))
                       (setf failed t)))
                    (t
                     (setf failed t))))
                 (debug-instruction)
                 (branch-instruction)
                 (t
                  (setf failed t)))))))
    (not failed)))

(defvar *rest-args-converted* 0)
(defvar *rest-args-not-converted* 0)

(defun convert-rest-arg-to-dx (backend-function)
  (let ((rest (argument-setup-rest (first-instruction backend-function))))
    (when rest
      (let ((uses (build-use/def-maps backend-function)))
        (cond ((rest-arg-safe-to-make-dx-p backend-function uses)
               (incf *rest-args-converted*)
               ;; Replace all calls to COPY-LIST involving the rest arg with the rest arg itself.
               (dolist (use (gethash rest uses))
                 (when (and (typep use 'call-instruction)
                            (eql (call-function use) 'copy-list))
                   (replace-all-uses (call-result use) rest uses)
                   (remove-instruction backend-function use))))
              (t
               (incf *rest-args-not-converted*)))))))

(defun remove-unused-instructions-1 (backend-function uses)
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
      (remove-instruction backend-function inst))))

(defun remove-unused-instructions (backend-function)
  (loop
     with total = 0
     do (multiple-value-bind (uses defs)
            (build-use/def-maps backend-function)
          (declare (ignore defs))
          (let ((n (remove-unused-instructions-1 backend-function uses)))
            (incf total n)
            (when (zerop n)
              (return total))))))

(defun localize-constants (backend-function)
  "Copy constants to their point of use."
  (let ((uses (build-use/def-maps backend-function))
        (constants '()))
    (do-instructions (inst backend-function)
      (when (and (typep inst 'constant-instruction)
                 (not (typep (constant-value inst) 'backend-function)))
        (push inst constants)))
    (dolist (inst constants)
      (let ((old-constant (constant-destination inst)))
        (dolist (use (gethash old-constant uses))
          (let ((new-constant (make-instance 'virtual-register)))
            (insert-before backend-function
                           use
                           (make-instance 'constant-instruction
                                          :value (constant-value inst)
                                          :destination new-constant))
            (replace-all-registers use
                                   (lambda (reg)
                                     (cond ((eql reg old-constant)
                                            new-constant)
                                           (t reg)))))))
      (remove-instruction backend-function inst))))

(defparameter *enable-dominance-type-flow* t)

(defgeneric dominance-aware-type-flow-update (function instruction info)
  (:method (f i info) info))

(defparameter *branch-instruction-converted* 0)
(defmethod dominance-aware-type-flow-update (function (instruction branch-instruction) info)
  (loop with value = (branch-value instruction)
        for (known-value data) in info
        when (eql value known-value)
          do (incf *branch-instruction-converted*)
             (cond ((eql data :truthy)
                    ;; Branch is true. Change to a jump to the true branch.
                    (change-class instruction
                                  'jump-instruction
                                  :values '()
                                  :target (branch-true-target instruction))
                    (return))
                   ((eql data :false)
                    ;; Branch is false. Change to a jump to the false branch.
                    (change-class instruction
                                  'jump-instruction
                                  :values '()
                                  :target (branch-false-target instruction))
                    (return))))
  info)

(defparameter *value-has-tag-p-converted* 0)
(defmethod dominance-aware-type-flow-update (function (instruction value-has-tag-p-instruction) info)
  (loop with value = (predicate-value instruction)
        with tag = (value-has-tag-p-tag instruction)
        for (known-value data other-predicate) in info
        when (and (eql value known-value)
                  (member data '(:known-true-predicate :known-false-predicate))
                  (typep other-predicate 'value-has-tag-p-instruction)
                  (eql (value-has-tag-p-tag other-predicate) tag))
          do
             ;; Then this instruction is known to be true or false.
             (incf *value-has-tag-p-converted*)
             (cond ((eql data :known-true-predicate)
                    (push (list (predicate-result instruction) :truthy) info)
                    (change-class instruction
                                  'constant-instruction
                                  :value 't
                                  :destination (predicate-result instruction)))
                   (t ; false
                    (push (list (predicate-result instruction) :falsey) info)
                    (change-class instruction
                                  'constant-instruction
                                  :value 'nil
                                  :destination (predicate-result instruction))))
             (return)
        when (and (eql value known-value)
                  (eql data :known-true-predicate)
                  (or (typep other-predicate 'fixnump-instruction)
                      (and (typep other-predicate 'value-has-tag-p-instruction)
                           (not (eql (value-has-tag-p-tag other-predicate) tag)))
                      (and (not (eql tag sys.int::+tag-immediate+))
                           (typep other-predicate 'value-has-immediate-tag-p-instruction))))
          ;; Other instruction refutes this instruction
          do
             (incf *value-has-tag-p-converted*)
             (push (list (predicate-result instruction) :falsey) info)
             (change-class instruction
                           'constant-instruction
                           :value 'nil
                           :destination (predicate-result instruction))
             (return))
  info)

(defparameter *fixnump-converted* 0)
(defmethod dominance-aware-type-flow-update (function (instruction fixnump-instruction) info)
  (loop with value = (predicate-value instruction)
        for (known-value data other-predicate) in info
        when (and (eql value known-value)
                  (member data '(:known-true-predicate :known-false-predicate))
                  (typep other-predicate 'fixnump-instruction))
          do
             ;; Then this instruction is known to be true or false.
             (incf *fixnump-converted*)
             (cond ((eql data :known-true-predicate)
                    (push (list (predicate-result instruction) :truthy) info)
                    (change-class instruction
                                  'constant-instruction
                                  :value 't
                                  :destination (predicate-result instruction)))
                   (t ; false
                    (push (list (predicate-result instruction) :falsey) info)
                    (change-class instruction
                                  'constant-instruction
                                  :value 'nil
                                  :destination (predicate-result instruction))))
             (return)
        when (and (eql value known-value)
                  (eql data :known-true-predicate)
                  (or (typep other-predicate 'value-has-tag-p-instruction)
                      (typep other-predicate 'value-has-immediate-tag-p-instruction)))
          ;; Other instruction refutes this instruction
          do
             (incf *fixnump-converted*)
             (push (list (predicate-result instruction) :falsey) info)
             (change-class instruction
                           'constant-instruction
                           :value 'nil
                           :destination (predicate-result instruction))
             (return))
  info)

(defparameter *value-has-immediate-tag-p-converted* 0)
(defmethod dominance-aware-type-flow-update (function (instruction value-has-immediate-tag-p-instruction) info)
  (loop with value = (predicate-value instruction)
        with tag = (value-has-immediate-tag-p-tag instruction)
        for (known-value data other-predicate) in info
        when (and (eql value known-value)
                  (member data '(:known-true-predicate :known-false-predicate))
                  (typep other-predicate 'value-has-immediate-tag-p-instruction)
                  (eql (value-has-immediate-tag-p-tag other-predicate) tag))
          do
             ;; Then this instruction is known to be true or false.
             (incf *value-has-immediate-tag-p-converted*)
             (cond ((eql data :known-true-predicate)
                    (push (list (predicate-result instruction) :truthy) info)
                    (change-class instruction
                                  'constant-instruction
                                  :value 't
                                  :destination (predicate-result instruction)))
                   (t ; false
                    (push (list (predicate-result instruction) :falsey) info)
                    (change-class instruction
                                  'constant-instruction
                                  :value 'nil
                                  :destination (predicate-result instruction))))
             (return)
        when (and (eql value known-value)
                  (or (and (eql data :known-true-predicate)
                           (or (typep other-predicate 'fixnump-instruction)
                               (and (typep other-predicate 'value-has-tag-p-instruction)
                                    (not (eql (value-has-tag-p-tag other-predicate) sys.int::+tag-immediate+)))
                               (and (typep other-predicate 'value-has-immediate-tag-p-instruction)
                                    (not (eql (value-has-tag-immediate-p-tag other-predicate) tag)))))
                      (and (eql data :known-false-predicate)
                           (typep other-predicate 'value-has-tag-p-instruction)
                           (eql (value-has-tag-p-tag other-predicate) sys.int::+tag-immediate+))))
          ;; Other instruction refutes this instruction
          do
             (incf *value-has-immediate-tag-p-converted*)
             (push (list (predicate-result instruction) :falsey) info)
             (change-class instruction
                           'constant-instruction
                           :value 'nil
                           :destination (predicate-result instruction))
             (return))
  info)

(defparameter *object-of-type-range-p-converted* 0)
(defmethod dominance-aware-type-flow-update (function (instruction object-of-type-range-p-instruction) info)
  (loop with value = (predicate-value instruction)
        with lo-tag = (object-of-type-range-p-lo-tag instruction)
        with hi-tag = (object-of-type-range-p-hi-tag instruction)
        for (known-value data other-predicate) in info
        when (and (eql value known-value)
                  (member data '(:known-true-predicate :known-false-predicate))
                  (typep other-predicate 'object-of-type-range-p-instruction)
                  (eql (object-of-type-range-p-lo-tag other-predicate) lo-tag)
                  (eql (object-of-type-range-p-hi-tag other-predicate) hi-tag))
          do
             ;; Then this instruction is known to be true or false.
             (incf *object-of-type-range-p-converted*)
             (cond ((eql data :known-true-predicate)
                    (push (list (predicate-result instruction) :truthy) info)
                    (change-class instruction
                                  'constant-instruction
                                  :value 't
                                  :destination (predicate-result instruction)))
                   (t ; false
                    (push (list (predicate-result instruction) :falsey) info)
                    (change-class instruction
                                  'constant-instruction
                                  :value 'nil
                                  :destination (predicate-result instruction))))
             (return))
  info)

(defun dominance-aware-type-flow (function)
  (let ((dom (mezzano.compiler.backend.dominance:compute-dominance function)))
    (multiple-value-bind (uses defs)
        (build-use/def-maps function)
      (declare (ignore uses))
      (labels ((doit (bb info)
                 #++(format t "Visit ~S ~:S~%" bb info)
                 ;; Update instructions based on available information.
                 (loop
                   for inst = bb then (next-instruction function inst)
                   do
                      (setf info (dominance-aware-type-flow-update function inst info))
                      (when (typep inst 'terminator-instruction)
                        (return)))
                 ;; Traverse the dominator tree, adding new information
                 ;; whenever we reach a branch instruction.
                 (let* ((children (mezzano.compiler.backend.dominance:dominator-tree-children
                                   dom bb))
                        (terminator (basic-block-terminator function bb)))
                   #++(when (typep terminator 'branch-instruction)
                     (format t " terminator is ~S ~S ~S ~:S~%" terminator
                             (branch-true-target terminator)
                             (branch-false-target terminator)
                             children))
                   (cond ((and (typep terminator 'branch-instruction)
                               (not (eql (branch-true-target terminator)
                                         (branch-false-target terminator)))
                               (member (branch-true-target terminator) children)
                               (member (branch-false-target terminator) children))
                          (let* ((value (branch-value terminator))
                                 ;; If the source of the value was a predicate value,
                                 ;; we now know something about that.
                                 (value-def (first (gethash value defs)))
                                 (predicate-value (and (typep value-def 'predicate-instruction)
                                                       (predicate-value value-def))))
                            (dolist (child children)
                              (doit child
                                    (cond ((eql child (branch-true-target terminator))
                                           `(,@(when predicate-value
                                                 `((,predicate-value
                                                    :known-true-predicate
                                                    ,value-def)))
                                             (,value :truthy)
                                             ,@info))
                                          ((eql child (branch-false-target terminator))
                                           `(,@(when predicate-value
                                                 `((,predicate-value
                                                    :known-false-predicate
                                                    ,value-def)))
                                             (,value :falsey)
                                             ,@info))
                                          (t
                                           info))))))
                         (t
                          (dolist (child children)
                            (doit child info)))))))
        (doit (first-instruction function) '())))))

;; A values instruction followed immediately by a m-v-b instruction can be
;; turned directly into a set of move instructions, no values needed.
(defun scalarize-values (backend-function)
  (do* ((n-removed 0)
        (inst (first-instruction backend-function) next-inst)
        (next-inst (next-instruction backend-function inst)
                   (if inst (next-instruction backend-function inst))))
       ((null inst)
        n-removed)
    (when (and (typep inst 'multiple-value-bind-instruction)
               (typep (prev-instruction backend-function inst) 'values-instruction))
      (loop with values = (values-values (prev-instruction backend-function inst))
            for var in (multiple-value-bind-values inst)
            do
               (cond (values
                      (insert-after backend-function inst
                                    (make-instance 'move-instruction
                                                   :source (pop values)
                                                   :destination var)))
                     (t
                      (insert-after backend-function inst
                                    (make-instance 'constant-instruction
                                                   :value nil
                                                   :destination var)))))
      (incf n-removed)
      (remove-instruction backend-function (prev-instruction backend-function inst))
      (remove-instruction backend-function inst))))
