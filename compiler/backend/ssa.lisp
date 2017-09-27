;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; SSA-related functions.

(in-package :mezzano.compiler.backend)

(defun deconstruct-ssa (backend-function)
  "Deconstruct SSA form, replacing phi nodes with moves."
  (do-instructions (inst backend-function)
    (when (typep inst 'jump-instruction)
      ;; Phi nodes have parallel assignment semantics.
      ;; Try to reduce the number of moves inserted.
      ;; Before:
      ;;   jump foo (a c b x)
      ;;   label foo (a b c d)
      ;; After:
      ;;   move t1 b [temporaries generated for parallel assignment]
      ;;   move t2 c
      ;;   [move from a to a elided]
      ;;   move b t2
      ;;   move c t1
      ;;   move d x
      ;;   jump foo ()
      ;;   label foo ()
      (let* ((conflicts (loop
                           for phi in (label-phis (jump-target inst))
                           for value in (jump-values inst)
                           when (loop
                                   for other-phi in (label-phis (jump-target inst))
                                   when (and (not (eql phi other-phi))
                                             (eql other-phi value))
                                   do (return t)
                                   finally (return nil))
                           collect phi))
             (real-values (loop
                             for value in (jump-values inst)
                             collect (cond ((member value conflicts)
                                            (let ((new-reg (make-instance 'virtual-register)))
                                              (insert-before backend-function inst
                                                             (make-instance 'move-instruction
                                                                            :source value
                                                                            :destination new-reg))
                                              new-reg))
                                           (t
                                            value)))))
        (loop
           for phi in (label-phis (jump-target inst))
           for value in real-values
           do
             (when (not (eql phi value))
               (insert-before backend-function inst
                              (make-instance 'move-instruction
                                             :source value
                                             :destination phi))))
        (setf (jump-values inst) '()))))
  (do-instructions (inst backend-function)
    (when (typep inst 'label)
      (setf (label-phis inst) '()))))

(defun test-deconstruct-function ()
  (let* ((x (make-instance 'virtual-register :name :x))
         (a (make-instance 'virtual-register :name :a))
         (b (make-instance 'virtual-register :name :b))
         (c (make-instance 'virtual-register :name :c))
         (d (make-instance 'virtual-register :name :d))
         (label (make-instance 'label :name :label :phis (list a b c d)))
         (fn (make-instance 'backend-function)))
    (append-instruction fn (make-instance 'argument-setup-instruction
                                          :fref (make-instance 'virtual-register)
                                          :count (make-instance 'virtual-register)
                                          :closure (make-instance 'virtual-register)
                                          :required (list x)
                                          :optional ()
                                          :rest nil))
    (append-instruction fn (make-instance 'jump-instruction
                                          :target label
                                          :values (list x x x x)))
    (append-instruction fn label)
    (append-instruction fn (make-instance 'jump-instruction
                                          :target label
                                          :values (list a c b x)))
    fn))
