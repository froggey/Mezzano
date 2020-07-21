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
                          (eql (box-type inst) (box-type def)))
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
                            (cond ((eql type 'fixnum)
                                   (cond ((eql other-type 'fixnum)
                                          t)
                                         ((eql other-type ':unsigned-byte-64)
                                          (setf type ':unsigned-byte-64)
                                          t)
                                         (t nil)))
                                  ((eql type ':unsigned-byte-64)
                                   (member other-type '(:unsigned-byte-64 fixnum)))
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
