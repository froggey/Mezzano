;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.backend)

(defun set-equal (list-1 list-2)
  (and (dolist (elt list-1 t)
         (when (not (member elt list-2))
           (return nil)))
       (dolist (elt list-2 t)
         (when (not (member elt list-1))
           (return nil)))))

(defun compute-liveness (backend-function target)
  (sys.c:with-metering (:backend-compute-liveness)
    (let ((live-in (make-hash-table :test 'eq :synchronized nil))
          (live-in* (make-hash-table :test 'eq :synchronized nil))
          (live-out (make-hash-table :test 'eq :synchronized nil))
          (live-out* (make-hash-table :test 'eq :synchronized nil))
          (actual-successors (make-hash-table :test 'eq :synchronized nil))
          (mv-regs (list* (mezzano.compiler.backend.register-allocator:target-count-register target)
                          (mezzano.compiler.backend.register-allocator:target-argument-registers target))))
      ;; Add control edges from calls/invoke-nlx instructions to all live NLX thunks.
      (let ((additional-successors '())
            (dc (dynamic-contours backend-function)))
        (do-instructions (inst backend-function)
          (when (typep inst '(or base-call-instruction invoke-nlx-instruction invoke-nlx-multiple-instruction))
            (dolist (c (gethash inst dc))
              (when (typep c 'begin-nlx-instruction)
                (setf additional-successors (union additional-successors
                                                   (begin-nlx-targets c))))))
          (when (typep inst 'terminator-instruction)
            (setf (gethash inst actual-successors) (append (successors backend-function inst)
                                                           additional-successors)
                  additional-successors '()))))
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
             (cond ((typep inst 'terminator-instruction)
                    (dolist (succ (gethash inst actual-successors))
                      (setf (gethash inst live-out) (union (gethash inst live-out)
                                                           (gethash succ live-in)))))
                   (t
                    (let ((succ (next-instruction backend-function inst)))
                      (setf (gethash inst live-out) (union (gethash inst live-out)
                                                           (gethash succ live-in))))))
             (setf (gethash inst live-in) (union uses
                                                 (set-difference (gethash inst live-out)
                                                                 defs)))))
         (when (do-reversed-instructions (inst backend-function t)
                 (when (not (and (set-equal (gethash inst live-in) (gethash inst live-in*))
                                 (set-equal (gethash inst live-out) (gethash inst live-out*))))
                   (return nil)))
           (return)))
      (values live-in live-out))))

(defun build-use/def-maps (backend-function)
  (let ((uses (make-hash-table :test 'eq :synchronized nil))
        (defs (make-hash-table :test 'eq :synchronized nil)))
    (do-instructions (inst backend-function)
      (dolist (use (instruction-inputs inst))
        (when (typep use 'virtual-register)
          (pushnew inst (gethash use uses '()))))
      (dolist (def (instruction-outputs inst))
        (when (typep def 'virtual-register)
          (pushnew inst (gethash def defs '())))))
    (values uses defs)))

(defun dynamic-contours (function)
  (let ((contour (make-hash-table :test 'eq :synchronized nil))
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
