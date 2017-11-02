;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Analysis to detect flow of multiple values.
;;;
;;; Multiple values are restricted to forward, combining flow only.
;;; They are produced, then must be consumed immediately after 0 or more jumps.

(in-package :mezzano.compiler.backend)

(defgeneric multiple-value-safe-p (instruction architecture)
  (:method (instruction architecture) nil))

(defmethod multiple-value-safe-p ((instruction label) architecture)
  t)

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

(defmethod multiple-value-safe-p ((instruction bind-local-instruction) architecture)
  t)

(defmethod multiple-value-safe-p ((instruction unbind-local-instruction) architecture)
  t)

(defmethod multiple-value-safe-p ((instruction load-local-instruction) architecture)
  t)

(defmethod multiple-value-safe-p ((instruction store-local-instruction) architecture)
  t)

(defun multiple-value-flow (function architecture)
  (let ((flow (make-hash-table :test 'eq :synchronized nil))
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
            (assert (eql (restore-multiple-context inst)
                         (save-multiple-context (first stack))))
            (pop stack))
           (forget-multiple-instruction
            (assert (eql (forget-multiple-context inst)
                         (save-multiple-context (first stack))))
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
            (assert (member (store-local-local inst) stack))))
         (dolist (next (successors function inst))
           (multiple-value-bind (next-stack visitedp)
               (gethash next contour)
             (cond
               (visitedp
                (assert (eql stack next-stack)))
               (t
                (push (cons next stack) worklist)))))))
    contour))
