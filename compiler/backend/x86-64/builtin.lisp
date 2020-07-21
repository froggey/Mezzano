;;;; x86-64 builtin function support

(in-package :mezzano.compiler.backend.x86-64)

(defmacro define-builtin (name (lambda-list results &key (has-wrapper t)) &body body)
  (when (not (listp results))
    (setf results (list results)))
  (let ((backend-function (gensym))
        (insertion-point (gensym))
        (the-block (gensym))
        (real-lambda-list (loop
                             for arg in lambda-list
                             collect (if (symbolp arg)
                                         arg
                                         (gensym))))
        (defs (gensym)))
    (loop
       for arg in lambda-list
       for real-arg in real-lambda-list
       when (consp arg)
       do
         (assert (not has-wrapper))
         (assert (eql (first arg) :constant))
         (destructuring-bind (name &optional (predicate t))
             (rest arg)
           (setf body `((let ((,name (let ((arg-defs (gethash ,real-arg ,defs)))
                                       (cond ((and arg-defs
                                                   (endp (rest arg-defs))
                                                   (typep (first arg-defs) 'ir:constant-instruction))
                                              (ir:constant-value (first arg-defs)))
                                             (t (give-up))))))
                          (when (not ,predicate)
                            (give-up))
                          ,@body)))))
    `(%defbuiltin ',name
                  ',real-lambda-list
                  ',results
                  (lambda (,backend-function ,insertion-point ,defs ,@real-lambda-list ,@(remove-if #'keywordp results))
                    (declare (ignorable ,defs ,@real-lambda-list ,@(remove-if #'keywordp results)))
                    (block ,the-block
                      (flet ((emit (inst)
                               (ir:insert-before ,backend-function ,insertion-point inst))
                             (give-up ()
                               (return-from ,the-block nil))
                             (finish ()
                               (return-from ,the-block t))
                             (constant-value-p (value &optional (type 't))
                               (and (typep (first (gethash value ,defs)) 'ir:constant-instruction)
                                    (typep (ir:constant-value (first (gethash value ,defs))) type)))
                             (fetch-constant-value (value)
                               (ir:constant-value (first (gethash value ,defs)))))
                        (declare (ignorable #'emit #'give-up #'finish #'constant-value-p #'fetch-constant-value))
                        ,@body
                        t)))
                  ',has-wrapper)))

(defclass builtin ()
  ((%name :initarg :name :reader builtin-name)
   (%lambda-list :initarg :lambda-list :reader builtin-lambda-list)
   (%result-list :initarg :result-list :reader builtin-result-list)
   (%generator :initarg :generator :reader builtin-generator)
   (%has-wrapper :initarg :has-wrapper :reader builtin-has-wrapper)))

(defvar *builtins* (make-hash-table :test 'equal :synchronized t))

;; Produce an alist of symbol names and their associated functions.
(defun generate-builtin-functions ()
  (let ((functions '()))
    (maphash (lambda (name info)
               (when (builtin-has-wrapper info)
                 (push (list name
                             `(lambda ,(builtin-lambda-list info)
                                (declare (sys.int::lambda-name ,name))
                                (funcall #',name ,@(builtin-lambda-list info))))
                       functions)))
             *builtins*)
    functions))

(defun match-builtin (name n-arguments)
  (let ((builtin (gethash name *builtins*)))
    (if (and builtin
             (eql (length (builtin-lambda-list builtin)) n-arguments))
        builtin
        nil)))

(defun %defbuiltin (name lambda-list result-list generator has-wrapper)
  (setf (gethash name *builtins*)
        (make-instance 'builtin
                       :name name
                       :lambda-list lambda-list
                       :result-list result-list
                       :generator generator
                       :has-wrapper has-wrapper))
  name)

(defun consumed-by-p (definition consumer uses defs)
  "Return true if all DEFINITION's outputs are only used by CONSUMER."
  (dolist (out (ir:instruction-outputs definition)
           t)
    (when (typep out 'ir:virtual-register)
      (let ((out-defs (gethash out defs))
            (out-uses (gethash out uses)))
        ;(format t "Out: ~S  defs: ~S  uses: ~S~%" out out-defs out-uses)
        ;; Must have one definition.
        (when (not (and out-defs
                        (eql (first out-defs) definition)
                        (endp (rest out-defs))))
          (return nil))
        ;; Must be used only by the consumer.
        (when (or (endp out-uses)
                  (not (endp (rest out-uses)))
                  (not (eql (first out-uses) consumer)))
          (return nil))))))

(defun reify-predicate (predicate result emitter)
  (let ((tmp (make-instance 'ir:virtual-register)))
    (funcall emitter (make-instance 'ir:constant-instruction
                                    :destination tmp
                                    :value nil))
    (funcall emitter (make-instance 'x86-fake-three-operand-instruction
                                    :opcode (predicate-instruction-cmov-instruction
                                             (predicate-info predicate))
                                    :result result
                                    :lhs tmp
                                    :rhs '(:constant t)))))

;; Lower (branch (call foo ...) target) when FOO produces a predicate result.
(defun lower-predicate-builtin (backend-function inst uses defs)
  (let ((next-inst (ir:next-instruction backend-function inst)))
    (when (and (typep inst 'ir:call-instruction)
               (typep next-inst 'ir:branch-instruction)
               (consumed-by-p inst next-inst uses defs))
      (let ((builtin (match-builtin (ir:call-function inst)
                                    (length (ir:call-arguments inst)))))
        (when (and builtin
                   ;; Predicate result.
                   ;; FIXME: This should work when the result consumed by the branch is a predicate and other results are ignored.
                   (eql (length (builtin-result-list builtin)) 1)
                   (keywordp (first (builtin-result-list builtin))))
          (when (not (apply (builtin-generator builtin)
                            backend-function inst
                            defs
                            (ir:call-arguments inst)))
            (return-from lower-predicate-builtin (values nil nil)))
          (let ((pred (first (builtin-result-list builtin))))
            (ir:insert-before
             backend-function inst
             (make-instance 'x86-branch-instruction
                            :opcode (predicate-instruction-jump-instruction
                                     (predicate-info pred))
                            :true-target (ir:branch-true-target next-inst)
                            :false-target (ir:branch-false-target next-inst)))
            (let ((advance (ir:next-instruction backend-function next-inst)))
              (ir:remove-instruction backend-function inst)
              (ir:remove-instruction backend-function next-inst)
              (values advance t))))))))

(defun lower-builtin (backend-function inst defs)
  (let ((builtin (and (typep inst '(or ir:call-instruction
                                       ir:call-multiple-instruction
                                       ir:tail-call-instruction))
                      (match-builtin (ir:call-function inst)
                                     (length (ir:call-arguments inst))))))
    (when builtin
      (let* ((result-regs (if (typep inst 'ir:call-instruction)
                              (list* (ir:call-result inst)
                                     (loop
                                        for r in (rest (builtin-result-list builtin))
                                        collect (make-instance 'ir:virtual-register)))
                              (loop
                                 for r in (builtin-result-list builtin)
                                 collect (make-instance 'ir:virtual-register))))
             (results (loop
                         for result in (builtin-result-list builtin)
                         for reg in result-regs
                         when (not (keywordp result))
                         collect reg)))
        (when (not (apply (builtin-generator builtin)
                          backend-function inst
                          defs
                          (append (ir:call-arguments inst)
                                  results)))
          (return-from lower-builtin nil))
        (cond ((and result-regs
                    (endp (builtin-result-list builtin)))
               ;; Builtin produces no results, but one value expected.
               (assert (endp (rest result-regs)))
               (when (typep inst 'ir:call-instruction)
                 (ir:insert-before
                  backend-function inst
                  (make-instance 'ir:constant-instruction
                                 :destination (first result-regs)
                                 :value nil))))
              (t
               ;; Convert predicate results to NIL/T.
               (loop
                  for result in (builtin-result-list builtin)
                  for reg in result-regs
                  when (keywordp result)
                  do (reify-predicate result reg
                                      (lambda (new-inst)
                                        (ir:insert-before
                                         backend-function inst new-inst))))))
        ;; Fix up multiple values.
        (when (typep inst '(or ir:call-multiple-instruction ir:tail-call-instruction))
          (ir:insert-before
           backend-function inst
           (make-instance 'ir:values-instruction
                          :values (if (endp (builtin-result-list builtin))
                                      '()
                                      result-regs))))
        (when (typep inst 'ir:tail-call-instruction)
          (ir:insert-before
           backend-function inst
           (make-instance 'ir:return-multiple-instruction)))
        (let ((advance (ir:next-instruction backend-function inst)))
          (ir:remove-instruction backend-function inst)
          advance)))))

(defun lower-builtins (backend-function)
  (multiple-value-bind (uses defs)
      (ir::build-use/def-maps backend-function)
    (do* ((inst (ir:first-instruction backend-function) next-inst)
          (next-inst (ir:next-instruction backend-function inst) (if inst (ir:next-instruction backend-function inst))))
         ((null inst))
      (multiple-value-bind (next did-something)
          (lower-predicate-builtin backend-function inst uses defs)
        (cond (did-something
               (setf next-inst next))
              (t
               (let ((next (lower-builtin backend-function inst defs)))
                 (when next
                   (setf next-inst next)))))))))
