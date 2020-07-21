;;;; Debug instruction related functions.

(in-package :mezzano.compiler.backend)

(defun build-debug-variable-value-map (backend-function)
  (let ((result (make-hash-table))
        (worklist (list (list (first-instruction backend-function) '()))))
    (loop
       (when (endp worklist)
         (return))
       (destructuring-bind (bb active-debug-values)
           (pop worklist)
         (cond ((nth-value 1 (gethash bb result))
                (when (not (equal (gethash bb result) active-debug-values))
                  (format *debug-io* "debug map conflict in bb ~S:~%" bb)
                  (format *debug-io* "existing: ~S~%" (gethash bb result))
                  (format *debug-io* "current: ~S~%" active-debug-values)
                  (print-function backend-function)
                  (let ((*print-pretty* t))
                    (format *debug-io* "ast: ~S~%" (mezzano.compiler::unparse-compiler-form (ast backend-function))))
                  (assert (equal (gethash bb result) active-debug-values))))
               (t
                (do ((inst bb (next-instruction backend-function inst)))
                    (nil)
                  (setf (gethash inst result) active-debug-values)
                  ;; Track changed & bound values.
                  (typecase inst
                    (debug-bind-variable-instruction
                     (push (list (debug-variable inst) (debug-value inst) (debug-representation inst)) active-debug-values))
                    (debug-update-variable-instruction
                     (setf active-debug-values
                           (loop
                              for entry in active-debug-values
                              collect (cond ((eql (first entry) (debug-variable inst))
                                             (list (first entry) (debug-value inst) (debug-representation inst)))
                                            (t
                                             entry)))))
                    (debug-unbind-variable-instruction
                     (assert (eql (first (first active-debug-values)) (debug-variable inst)))
                     (pop active-debug-values)))
                  (when (typep inst 'begin-nlx-instruction)
                    (dolist (succ (begin-nlx-targets inst))
                      (push (list succ active-debug-values) worklist)))
                  (when (typep inst 'terminator-instruction)
                    ;; Traverse successors.
                    (dolist (succ (successors backend-function inst))
                      (push (list succ active-debug-values) worklist))
                    (return)))))))
    result))

(defun remove-debug-variable-instructions (backend-function)
  (let ((remove-me '()))
    (do-instructions (inst backend-function)
      (when (typep inst '(or
                          debug-bind-variable-instruction
                          debug-update-variable-instruction
                          debug-unbind-variable-instruction))
        (push inst remove-me)))
    (dolist (inst remove-me)
      (remove-instruction backend-function inst))
    (length remove-me)))

(defun unbox-debug-values (backend-function)
  "Replace (debug-bind var (box type value)) with (debug-bind var value type)"
  (multiple-value-bind (uses defs)
      (build-use/def-maps backend-function)
    (declare (ignore uses))
    (let ((total 0))
      (do-instructions (inst backend-function)
        (when (and (typep inst '(or
                                 debug-bind-variable-instruction
                                 debug-update-variable-instruction))
                   (typep (first (gethash (debug-value inst) defs)) 'box-instruction))
          (let ((box (first (gethash (debug-value inst) defs))))
            (incf total)
            (setf (debug-value inst) (box-source box)
                  (debug-representation inst) (box-type box)))))
      total)))
