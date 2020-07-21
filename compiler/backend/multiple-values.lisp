;;;; Analysis to detect flow of multiple values.
;;;;
;;;; Multiple values are restricted to forward, combining flow only.
;;;; They are produced, then must be consumed immediately after 0 or more jumps.

(in-package :mezzano.compiler.backend)

(defun multiple-value-flow (function architecture)
  (let ((flow (make-hash-table :test 'eq))
        (worklist (list (cons (first-instruction function) nil))))
    ;; DFS over the instructions, following multiple values.
    (loop
       (when (endp worklist)
         (return))
       (let* ((entry (pop worklist))
              (inst (car entry))
              (mv-active-p (cdr entry)))
         (cond (mv-active-p
                ;; Multiple values are active. Only permitted instructions
                ;; are consumers and jumps.
                (cond ((consumes-multiple-p inst)
                       (setf (gethash inst flow) :multiple)
                       ;; M-V-FUNCALL-M consumes and produces multiple values.
                       (setf mv-active-p (produces-multiple-p inst)))
                      ((multiple-value-safe-p inst architecture)
                       (setf (gethash inst flow) :multiple))
                      (t
                       (error "Reached invalid instruction ~S with multiple values active." inst))))
               (t
                ;; Multiple values are active. Must not see a consumer.
                (when (consumes-multiple-p inst)
                  (error "Reached MV consuming instruction ~S without multiple values active." inst))
                (setf (gethash inst flow) :single)
                (setf mv-active-p (produces-multiple-p inst))))
         (dolist (next (successors function inst))
           (let ((next-state (gethash next flow)))
             (ecase next-state
               ((nil)
                (push (cons next mv-active-p) worklist))
               (:single
                (assert (not mv-active-p)))
               (:multiple
                (assert mv-active-p)))))))
    flow))

(defun remove-extraneous-multiple-value-saves (backend-function)
  (let ((to-remove '())
        (n-removed 0))
    (do-instructions (inst backend-function)
      (when (typep inst 'save-multiple-instruction)
        ;; Look forward in this basic block for a matching restore.
        (loop
           for forward = (next-instruction backend-function inst) then (next-instruction backend-function forward)
           until (typep forward 'terminator-instruction)
           do
             (when (and (typep forward 'restore-multiple-instruction)
                        (eql (restore-multiple-context forward) inst))
               (incf n-removed)
               (push inst to-remove)
               (push forward to-remove)
               (loop-finish))
             (when (not (multiple-value-safe-p forward nil))
               (loop-finish)))))
    (dolist (inst to-remove)
      (remove-instruction backend-function inst))
    n-removed))
