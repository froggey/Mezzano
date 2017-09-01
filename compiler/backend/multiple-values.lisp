;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Analysis to detect flow of multiple values.
;;;
;;; Multiple values are restricted to forward, combining flow only.
;;; They are produced, then must be consumed immediately after 0 or more jumps.

(in-package :mezzano.compiler.backend)

(defgeneric successors (function instruction))

(defun skip-symbols (foo)
  (loop
     (when (endp foo)
       (return '()))
     (when (not (symbolp (first foo)))
       (return foo))
     (setf foo (rest foo))))

(defmethod successors (function (instruction backend-instruction))
  (list (first (skip-symbols (rest (member instruction (backend-function-code function)))))))

(defmethod successors (function (instruction jump-instruction))
  (list (first (skip-symbols (member (jump-target instruction) (backend-function-code function))))))

(defmethod successors (function (instruction branch-instruction))
  (list (first (skip-symbols (rest (member instruction (backend-function-code function)))))
        (first (skip-symbols (member (branch-target instruction) (backend-function-code function))))))

(defmethod successors (function (instruction switch-instruction))
  (loop
     for target in (switch-targets instruction)
     collect (first (skip-symbols (member target (backend-function-code function))))))

(defmethod successors (function (instruction return-instruction))
  '())

(defmethod successors (function (instruction return-multiple-instruction))
  '())

(defmethod successors (function (instruction begin-nlx-instruction))
  (append (call-next-method)
          (loop
             for target in (begin-nlx-targets instruction)
             collect (first (skip-symbols (member target (backend-function-code function)))))))

(defmethod successors (function (instruction invoke-nlx-instruction))
  '())

(defmethod successors (function (instruction invoke-nlx-multiple-instruction))
  '())

(defgeneric multiple-value-safe-p (instruction architecture)
  (:method (instruction architecture) nil))

(defmethod multiple-value-safe-p ((instruction move-instruction) architecture)
  t)

(defmethod multiple-value-safe-p ((instruction spill-instruction) architecture)
  t)

(defmethod multiple-value-safe-p ((instruction fill-instruction) architecture)
  t)

(defmethod multiple-value-safe-p ((instruction jump-instruction) architecture)
  t)

(defmethod multiple-value-safe-p ((instruction finish-nlx-instruction) architecture)
  t)

(defmethod multiple-value-safe-p ((instruction forget-multiple-instruction) architecture)
  t)

(defmethod multiple-value-safe-p ((instruction forget-multiple-instruction) architecture)
  t)

(defun multiple-value-flow (function architecture)
  (let ((flow (make-hash-table)))
    ;; DFS over the instructions, following multiple values.
    (labels ((visit (inst mv-active-p)
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
                      (visit next mv-active-p))
                     (:single
                      (assert (not mv-active-p)))
                     (:multiple
                      (assert mv-active-p)))))))
      (visit (first (backend-function-code function))
             nil))
    flow))

(defun dynamic-contours (function)
  (let ((contour (make-hash-table)))
    ;; DFS over the instructions.
    (labels ((visit (inst stack)
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
                  (assert (eql (restore-multiple-context inst)
                               (save-multiple-context (first stack))))
                  (pop stack))
                 (forget-multiple-instruction
                  (assert (eql (forget-multiple-context inst)
                               (save-multiple-context (first stack))))
                  (pop stack)))
               (dolist (next (successors function inst))
                 (multiple-value-bind (next-stack visitedp)
                     (gethash next contour)
                   (cond
                     (visitedp
                      (assert (eql stack next-stack)))
                     (t
                      (visit next stack)))))))
      (visit (first (backend-function-code function))
             '()))
    contour))
