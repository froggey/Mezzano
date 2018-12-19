;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; SSA-related functions.

(in-package :mezzano.compiler.backend)

(defun check-definitions-dominate-uses (backend-function)
  "Check that all virtual-register uses are dominated by their definitions.
This only works on functions in SSA form."
  (let ((dom (mezzano.compiler.backend.dominance:compute-dominance backend-function)))
    (labels ((check-defs-dominate-uses (def-stack bb)
               (do ((inst bb (next-instruction backend-function inst)))
                   ((null inst))
                 (dolist (input (instruction-inputs inst))
                   (when (typep input 'virtual-register)
                     (assert (member input def-stack) (inst def-stack)
                             "Instruction ~S uses ~S before definition."
                             inst input)))
                 (dolist (output (instruction-outputs inst))
                   (when (typep output 'virtual-register)
                     (push output def-stack)))
                 (when (typep inst 'terminator-instruction)
                   (return)))
               (dolist (child (mezzano.compiler.backend.dominance:dominator-tree-children dom bb))
                 (check-defs-dominate-uses def-stack child))))
      (check-defs-dominate-uses '() (first-instruction backend-function)))))

(defun check-ssa (backend-function)
  "Verify that BACKEND-FUNCTION is in SSA form.
Virtual registers must be defined exactly once."
  (multiple-value-bind (uses defs)
      (build-use/def-maps backend-function)
    (declare (ignore uses))
    (maphash (lambda (def insts)
               (assert (not (endp insts)) (def)
                       "Virtual register ~S has no definitions?" def)
               (assert (endp (rest insts)) (def insts)
                       "Virtual register ~S defined by multiple instructions ~S"
                       def insts))
             defs))
  (check-definitions-dominate-uses backend-function))

(defun deconstruct-ssa (backend-function)
  "Deconstruct SSA form, replacing phi nodes with moves."
  (check-ssa backend-function)
  (sys.c:with-metering (:backend-deconstruct-ssa)
    (let ((n-moves-inserted 0)
          (n-phis-converted 0)
          (uses (build-use/def-maps backend-function))
          (contours (dynamic-contours backend-function)))
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
          (labels ((need-debug-update (phi use)
                     ;; HACK!
                     ;; Walk backward over update instructions looking for the
                     ;; label. If this label defines the phi, then a debug
                     ;; update should be inserted.
                     ;; This should track debug info instead...
                     (let ((label use))
                       (loop
                          (unless (typep label 'debug-update-variable-instruction)
                            (return))
                          (setf label (prev-instruction backend-function label)))
                       (and (typep label 'label)
                            (member phi (label-phis label)))))
                   (debug-update (phi value)
                     (let ((inserted-variables '()))
                       (dolist (use (gethash phi uses))
                         (when (and (typep use 'debug-update-variable-instruction)
                                    (member (debug-variable use) (gethash inst contours))
                                    (not (member (debug-variable use) inserted-variables)))
                           (push (debug-variable use) inserted-variables)
                           (when (need-debug-update phi use)
                             (insert-before backend-function inst
                                            (make-instance 'debug-update-variable-instruction
                                                           :variable (debug-variable use)
                                                           :value value)))))))
                   (insert-move (phi source dest)
                     ;; Insert a move related to the eventual value of PHI,
                     ;; updating debug info along the way.
                     (insert-before backend-function inst
                                    (make-instance 'move-instruction
                                                   :source source
                                                   :destination dest))
                     (debug-update phi dest)))
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
                                   for phi in (label-phis (jump-target inst))
                                   for value in (jump-values inst)
                                   collect (cond ((member value conflicts)
                                                  (let ((new-reg (make-instance 'virtual-register :kind (virtual-register-kind phi))))
                                                    (incf n-moves-inserted)
                                                    (insert-move phi value new-reg)
                                                    new-reg))
                                                 (t
                                                  value)))))
              (loop
                 for phi in (label-phis (jump-target inst))
                 for value in real-values
                 do
                   (cond ((eql phi value)
                          ;; No change, but insert a debug update anyway.
                          (debug-update phi value))
                         (t
                          (incf n-moves-inserted)
                          (insert-move phi value phi))))
              (setf (jump-values inst) '())))))
      (do-instructions (inst backend-function)
        (when (typep inst 'label)
          (incf n-phis-converted (length (label-phis inst)))
          (setf (label-phis inst) '())))
      (unless *shut-up*
        (format t "Deconstructed ~D phi variables, inserted ~D moves.~%"
                n-phis-converted n-moves-inserted)))))

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

(defun discover-ssa-conversion-candidates (backend-function)
  (let ((simple-transforms '())
        (full-transforms '())
        (rejected-transforms '()))
    ;; Locals that are not stored into can be trivially converted by replacing
    ;; loads with the original binding.
    ;; Locals that are stored into must undergo the full SSA conversion
    ;; algorithm. Additionally, these locals will not be transformed if
    ;; they are live over an NLX region. Phi nodes are not permitted i
    ;; NLX thunks.
    (do-instructions (inst backend-function)
      (when (typep inst 'bind-local-instruction)
        (push inst simple-transforms))
      (when (and (typep inst 'store-local-instruction)
                 (member (store-local-local inst) simple-transforms))
        (setf simple-transforms (remove (store-local-local inst)
                                        simple-transforms))
        (push (store-local-local inst) full-transforms)))
    (unless (endp full-transforms)
      ;; Bail if there are any NLX regions in the function at all.
      (do-instructions (inst backend-function)
        (when (typep inst 'begin-nlx-instruction)
          (setf rejected-transforms full-transforms
                full-transforms '())
          (return)))
      ;; Build dynamic contours and eliminate variables live during NLX regions.
      ;; FIXME: The CFG doesn't quite represent NLX regions correctly, with
      ;; nlx-begin instructions being treated as branches to NLX targets.
      ;; Instead all call instructions within an NLX region should be treated
      ;; as branches to live NLX targets.
      #+(or)
      (let ((contours (dynamic-contours backend-function)))
        (do-instructions (inst backend-function)
          (when (typep inst 'begin-nlx-instruction)
            (let ((reject (intersection (gethash inst contours)
                                        full-transforms)))
              (setf rejected-transforms (append reject rejected-transforms)))
            (setf full-transforms (set-difference full-transforms (gethash inst contours)))))))
    (values simple-transforms
            full-transforms
            rejected-transforms)))

(defun ssa-convert-simple-locals (backend-function candidates debugp)
  "Each candidate has one definition. All loads are replaced with the bound value."
  (let ((remove-me '())
        (n-simple-loads-converted 0))
    (multiple-value-bind (uses defs)
        (build-use/def-maps backend-function)
      (declare (ignore defs))
      (do-instructions (inst backend-function)
        (cond ((and (typep inst 'load-local-instruction)
                   (member (load-local-local inst) candidates))
               (dolist (u (gethash (load-local-destination inst) uses))
                 (replace-all-registers u
                                        (lambda (reg)
                                          (cond ((eql reg (load-local-destination inst))
                                                 (bind-local-value (load-local-local inst)))
                                                (t reg)))))
               (push inst remove-me)
               (incf n-simple-loads-converted))
              ((and (typep inst 'bind-local-instruction)
                    (member inst candidates))
               (when debugp
                 (insert-before backend-function inst
                                (make-instance 'debug-bind-variable-instruction
                                               :variable (bind-local-ast inst)
                                               :value (bind-local-value inst))))
               (push inst remove-me))
              ((and (typep inst 'unbind-local-instruction)
                    (member (unbind-local-local inst) candidates))
               (when debugp
                 (insert-before backend-function inst
                                (make-instance 'debug-unbind-variable-instruction
                                               :variable (bind-local-ast (unbind-local-local inst)))))
               (push inst remove-me)))))
    (dolist (inst remove-me)
      (remove-instruction backend-function inst))
    (unless *shut-up*
      (format t "Converted ~D simple loads.~%" n-simple-loads-converted))
    n-simple-loads-converted))

(defun ssa-convert-one-local (backend-function candidate dom basic-blocks bb-preds bb-succs dynamic-contour debugp)
  (declare (ignore basic-blocks bb-succs))
  (let ((visited (make-hash-table :test 'eq :synchronized nil))
        (phi-sites '())
        (def-sites '())
        (binding-bb nil))
    ;; Locate basic blocks containing the binding & stores.
    (do* ((inst (first-instruction backend-function)
                (next-instruction backend-function inst))
          (current-bb inst))
         ((null inst))
      (when (or (and (typep inst 'store-local-instruction)
                     (eql (store-local-local inst) candidate))
                (eql inst candidate))
        (when (eql inst candidate)
          (setf binding-bb current-bb))
        (unless (gethash current-bb visited)
          (setf (gethash current-bb visited) t)
          (push current-bb def-sites)))
      (when (typep inst 'terminator-instruction)
        (setf current-bb (next-instruction backend-function inst))))
    (unless *shut-up*
      (format t "Def sites for ~S: ~:S~%" candidate def-sites))
    (assert binding-bb)
    (loop
       (when (endp def-sites)
         (return))
       (dolist (frontier (mezzano.compiler.backend.dominance:dominance-frontier dom (pop def-sites)))
         (when (and (not (member frontier phi-sites))
                    ;; Only care about blocks dominated by the binding.
                    (mezzano.compiler.backend.dominance:dominatep dom binding-bb frontier)
                    ;; And block where the variable is live on entry.
                    (member candidate (gethash frontier dynamic-contour)))
           (push frontier phi-sites)
           (unless (gethash frontier visited)
             (setf (gethash frontier visited) t)
             (push frontier def-sites)))))
    (unless *shut-up*
      (format t "Phi sites for ~S: ~:S~%" candidate phi-sites))
    ;; FIXME: Critical edges will prevent phi insertion, need to break them.
    ;; work around this by bailing out whenever a phi site's predecessor is
    ;; terminated by a non-jump.
    (dolist (bb phi-sites)
      (check-type bb label)
      (dolist (pred (gethash bb bb-preds))
        (loop
           (when (typep pred 'terminator-instruction) (return))
           (setf pred (next-instruction backend-function pred)))
        (unless (typep pred 'jump-instruction)
          (unless *shut-up*
            (format t "Bailing out of conversion for ~S due to non-jump ~S.~%"
                    candidate pred))
          (return-from ssa-convert-one-local nil))))
    ;; Insert phi nodes.
    (dolist (bb phi-sites)
      (let ((phi (make-instance 'virtual-register :name `(:phi ,candidate))))
        (push phi (label-phis bb))
        ;; Update each predecessor jump.
        (dolist (pred (gethash bb bb-preds))
          (loop
             (when (typep pred 'terminator-instruction) (return))
             (setf pred (next-instruction backend-function pred)))
          (let ((tmp (make-instance 'virtual-register)))
            (insert-before backend-function pred
                           (make-instance 'load-local-instruction
                                          :local candidate
                                          :destination tmp))
            (push tmp (jump-values pred))))
        ;; And insert stores after each phi.
        (insert-after backend-function bb
                      (make-instance 'store-local-instruction
                                     :local candidate
                                     :value phi))
        (when debugp
          ;; Debug updates too.
          (insert-after backend-function bb
                        (make-instance 'debug-update-variable-instruction
                                       :variable (bind-local-ast candidate)
                                       :value phi)))))
    ;; Now walk the dominator tree to rename values, starting at the binding's basic block.
    (let ((uses (build-use/def-maps backend-function))
          (remove-me '()))
      (labels ((rename (bb stack)
                 (let ((inst bb))
                   (loop
                      (typecase inst
                        (load-local-instruction
                         (when (eql (load-local-local inst) candidate)
                           (let ((new-value (first stack))
                                 (load-value (load-local-destination inst)))
                             ;; Replace all uses with the new value
                             (dolist (u (gethash load-value uses))
                               (replace-all-registers u
                                                      (lambda (reg)
                                                        (cond ((eql reg load-value)
                                                               new-value)
                                                              (t reg)))))
                             (push inst remove-me))))
                        (store-local-instruction
                         (when (eql (store-local-local inst) candidate)
                           (when debugp
                             (insert-after backend-function inst
                                           (make-instance 'debug-update-variable-instruction
                                                          :variable (bind-local-ast candidate)
                                                          :value (store-local-value inst))))
                           (push inst remove-me)
                           (push (store-local-value inst) stack)))
                        (unbind-local-instruction
                         (when (eql (unbind-local-local inst) candidate)
                           (when debugp
                             (insert-after backend-function inst
                                           (make-instance 'debug-unbind-variable-instruction
                                                          :variable (bind-local-ast candidate))))
                           (push inst remove-me)
                           ;; Stop renaming this branch of the dom tree
                           ;; when the variable is unbound.
                           (return-from rename))))
                      (when (typep inst 'terminator-instruction)
                        (return))
                      (setf inst (next-instruction backend-function inst))))
                 (dolist (child (mezzano.compiler.backend.dominance:dominator-tree-children dom bb))
                   (rename child stack))))
        (when debugp
          (insert-after backend-function candidate
                        (make-instance 'debug-bind-variable-instruction
                                       :variable (bind-local-ast candidate)
                                       :value (bind-local-value candidate))))
        (push candidate remove-me)
        (rename binding-bb
                ;; Initial value is whatever value it was bound with.
                (list (bind-local-value candidate)))
        (dolist (inst remove-me)
          (remove-instruction backend-function inst))))
    t))

(defun ssa-convert-locals (backend-function candidates debugp)
  (multiple-value-bind (basic-blocks bb-preds bb-succs)
      (build-cfg backend-function)
    (let ((dom (mezzano.compiler.backend.dominance:compute-dominance backend-function))
          (n-converted 0)
          (converted '())
          (dynamic-contour (dynamic-contours backend-function)))
      (dolist (candidate candidates)
        (when (ssa-convert-one-local backend-function candidate dom basic-blocks bb-preds bb-succs dynamic-contour debugp)
          (push candidate converted)
          (incf n-converted)))
      ;; Walk through and remove any load instructions associated with
      ;; converted bindings.
      (let ((remove-me '()))
        (do-instructions (inst backend-function)
          (when (and (typep inst 'load-local-instruction)
                     (member (load-local-local inst) converted))
            (push inst remove-me)))
        (dolist (inst remove-me)
          (remove-instruction backend-function inst)))
      n-converted)))

(defun construct-ssa (backend-function)
  "Convert locals to SSA registers."
  (sys.c:with-metering (:backend-construct-ssa)
    (multiple-value-bind (simple-transforms full-transforms rejected-transforms)
        (discover-ssa-conversion-candidates backend-function)
      (unless *shut-up*
        (format t "Directly converting ~:S~%" simple-transforms)
        (format t "Fully converting ~:S~%" full-transforms)
        (format t "Rejected converting ~:S~%" rejected-transforms))
      (let ((debugp (/= (sys.c::optimize-quality (ast backend-function) 'debug) 0)))
        (unless (endp simple-transforms)
          (ssa-convert-simple-locals backend-function simple-transforms debugp))
        (unless (endp full-transforms)
          (ssa-convert-locals backend-function full-transforms debugp))))))

(defun remove-unused-phis (backend-function)
  (multiple-value-bind (uses defs)
      (build-use/def-maps backend-function)
    (declare (ignore defs))
    (multiple-value-bind (basic-blocks bb-preds bb-succs)
        (build-cfg backend-function)
      (declare (ignore basic-blocks bb-succs))
      (let ((n-removed 0))
        (do-instructions (inst backend-function)
          (when (typep inst 'label)
            ;; Loop until there are no more unused phis to be removed.
            (let ((did-something nil))
              (loop
                 (loop
                    for index from 0
                    for phi in (label-phis inst)
                    when (endp (gethash phi uses))
                    do
                     ;; Remove this one and start over.
                      (dolist (pred (gethash inst bb-preds))
                        (let ((term (basic-block-terminator backend-function pred)))
                          (cond ((zerop index)
                                 (setf (jump-values term) (rest (jump-values term))))
                                (t
                                 (let ((here (nthcdr (1- index) (jump-values term))))
                                   (setf (cdr here) (cddr here)))))))
                      (cond ((zerop index)
                             (setf (label-phis inst) (rest (label-phis inst))))
                            (t
                             (let ((here (nthcdr (1- index) (label-phis inst))))
                               (setf (cdr here) (cddr here)))))
                      (incf n-removed)
                      (setf did-something t)
                      (return))
                 (cond (did-something
                        (setf did-something nil))
                       (t
                        (return)))))))
        n-removed))))

(defun compute-phi-webs (backend-function)
  (let ((phi-values (make-hash-table))
        (results (make-hash-table))
        (defs (nth-value 1 (build-use/def-maps backend-function))))
    (do-instructions (inst backend-function)
      (when (typep inst 'jump-instruction)
        (loop
           for value in (jump-values inst)
           for phi in (label-phis (jump-target inst))
           do
             (pushnew value (gethash phi phi-values)))))
    (loop
       for phi being the hash-keys of phi-values
       do
         (let ((visited '())
               (worklist (list phi)))
           (setf (gethash phi results) '())
           (loop
              (when (endp worklist)
                (return))
              (let ((other-phi (pop worklist)))
                (unless (member other-phi visited)
                  (push other-phi visited)
                  (dolist (value (gethash other-phi phi-values))
                    (cond ((typep (first (gethash value defs)) 'label)
                           ;; Another phi.
                           (push value worklist))
                          (t
                           (pushnew value (gethash phi results))))))))))
    results))
