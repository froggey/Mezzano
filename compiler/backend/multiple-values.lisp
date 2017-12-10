;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; Analysis to detect flow of multiple values.
;;;
;;; Multiple values are restricted to forward, combining flow only.
;;; They are produced, then must be consumed immediately after 0 or more jumps.

(in-package :mezzano.compiler.backend)

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
