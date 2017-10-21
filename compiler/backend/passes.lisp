;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend)

(defun remove-unused-local-variables (backend-function)
  "Remove all locals that are not loaded."
  (let ((loaded-variables (make-hash-table))
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
               (when (and (typep def 'box-instruction)
                          (eql (box-type inst) (box-type def)))
                 (frob (unbox-destination inst) (box-source def)))))
            (box-instruction
             (let ((def (first (gethash (box-source inst) defs))))
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
           (flet ((value-type (vreg)
                    (let ((inst (first (gethash vreg defs))))
                      (typecase inst
                        (box-single-float-instruction
                         'single-float)
                        (constant-instruction
                         (typecase (constant-value inst)
                           (single-float 'single-float)
                           (t nil)))
                        (t nil))))
                  (pred-value (pred)
                    (nth index (jump-values (basic-block-terminator backend-function pred)))))
             (let ((type (value-type (pred-value (first (gethash bb bb-preds))))))
               (when (and type
                          (every (lambda (pred)
                                   (eql type (value-type (pred-value pred))))
                                 (rest (gethash bb bb-preds))))
                 ;; Insert unbox instructions before every definition of the phi.
                 (dolist (pred (gethash bb bb-preds))
                   (let* ((term (basic-block-terminator backend-function pred))
                          (value (nth index (jump-values term)))
                          (temp (make-instance 'virtual-register
                                               :kind (ecase type
                                                       (single-float :single-float)))))
                     (insert-before backend-function term
                                    (make-instance (ecase type
                                                     (single-float 'unbox-single-float-instruction))
                                                   :source value
                                                   :destination temp))
                     (setf (nth index (jump-values term)) temp)))
                 ;; Insert box instructions before every use of the phi.
                 ;; Insert before each use to keep new live ranges short.
                 (dolist (user (gethash phi uses))
                   (let ((temp (make-instance 'virtual-register)))
                     (insert-before backend-function user
                                    (make-instance (ecase type
                                                     (single-float 'box-single-float-instruction))
                                                   :source phi
                                                   :destination temp))
                     (replace-all-registers user
                                            (lambda (reg)
                                              (cond ((eql reg phi)
                                                     temp)
                                                    (t reg))))))
                 ;; Change the phi kind.
                 (setf (slot-value phi '%kind) (ecase type
                                                 (single-float :single-float)))
                 (return-from unbox-phis-1 t)))))))
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
