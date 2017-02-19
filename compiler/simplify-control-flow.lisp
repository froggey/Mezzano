;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Simplify complicated control flow in TAGBODY forms.

(in-package sys.c)

(defvar *tagbody-statement-stack* '())

(defun simplify-control-flow (form)
  (simplify-control-flow-1 form '() '() '() nil))

(defun simplify-control-flow-get-replacement-go-tag (go-tag renames)
  (let ((new (assoc go-tag renames)))
    (if new
        (simplify-control-flow-get-replacement-go-tag (second new) renames)
        go-tag)))

(defgeneric simplify-control-flow-1 (form ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody))

(defmethod simplify-control-flow-1 ((form ast-block) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (body form) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
    (declare (ignore control-terminates))
    (setf (body form) new-form))
  (values form nil))

(defmethod simplify-control-flow-1 ((form ast-function) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (declare (ignore ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody))
  (values form nil))

(defmethod simplify-control-flow-1 ((form ast-go) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (setf (target form) (simplify-control-flow-get-replacement-go-tag (target form) renames))
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (info form) ti/tb-mapping permitted-hoist-tagbodys renames nil)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1 (values new-form t)))
    (setf (info form) new-form))
  ;; Find the tagbody holding this tag.
  (let ((tb-ast (cdr (assoc (go-tag-tagbody (target form)) ti/tb-mapping))))
    (assert tb-ast)
    ;; Find the associated statement.
    (let* ((stmt-pair (assoc (target form) (statements tb-ast)))
           (stmt (second stmt-pair)))
      (assert stmt)
      ;; If it's a GO or RETURN-FROM with simple value, then this GO can be replaced.
      (when (or (and (typep stmt 'ast-go)
                     (not (eql (target stmt) (target form))))
                (and (typep stmt 'ast-return-from)
                     (typep (value stmt) 'ast-quote)))
        (change-made)
        (return-from simplify-control-flow-1
          (simplify-control-flow-1 (copy-form stmt) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)))
      ;; If it is a simple value leaving the current tagbody, then replace with that.
      (when (and (eql tb-ast leaving-tagbody)
                 (typep stmt 'ast-quote))
        (change-made)
        (return-from simplify-control-flow-1
          (simplify-control-flow-1 (copy-form stmt) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)))
      ;; If this tagbody is on the permitted list and the go-tag has one use (this go),
      ;; the the entire statement can be moved here.
      ;; The old statement is replaced with 'NIL and a GO to it is inserted at the end
      ;; to maintain proper control flow.
      (when (and (not (typep stmt 'ast-quote))
                 (member tb-ast permitted-hoist-tagbodys)
                 (eql (go-tag-use-count (target form)) 1)
                 (not (member (target form) *tagbody-statement-stack*)))
        (change-made)
        (let ((new-stmt (ast `(progn ,stmt ,form) stmt)))
          (setf (second stmt-pair) (ast `(quote nil) new-stmt))
          (return-from simplify-control-flow-1
            (simplify-control-flow-1 new-stmt ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody))))))
  (values form t))

(defmethod simplify-control-flow-1 ((form ast-if) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (test form) ti/tb-mapping permitted-hoist-tagbodys renames nil)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1 (values new-form t)))
    (setf (test form) new-form))
  (multiple-value-bind (new-then-form then-control-terminates)
      (simplify-control-flow-1 (if-then form) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
    (setf (if-then form) new-then-form)
    (multiple-value-bind (new-else-form else-control-terminates)
        (simplify-control-flow-1 (if-else form) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
      (setf (if-else form) new-else-form)
      (values form
              (and then-control-terminates else-control-terminates)))))

(defmethod simplify-control-flow-1 ((form ast-let) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (let ((new-bindings '()))
    (loop
       for (variable init-form) in (bindings form)
       do (multiple-value-bind (new-form control-terminates)
              (simplify-control-flow-1 init-form
                                       ti/tb-mapping
                                       permitted-hoist-tagbodys
                                       renames
                                       nil)
            (push (list variable new-form) new-bindings)
            (when control-terminates
              (change-made)
              (return-from simplify-control-flow-1
                (values (if (endp (rest new-bindings))
                            (second (first new-bindings))
                            (ast `(let ,(reverse (rest new-bindings))
                                    ,(second (first new-bindings)))
                                 form))
                        t)))))
    (setf (bindings form) (reverse new-bindings)))
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (body form)
                               ti/tb-mapping
                               (if (some (lambda (x) (typep (first x) 'special-variable))
                                         (bindings form))
                                   '()
                                   permitted-hoist-tagbodys)
                               renames
                               leaving-tagbody)
    (setf (body form) new-form)
    (values form control-terminates)))

(defmethod simplify-control-flow-1 ((form ast-multiple-value-bind) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (value-form form)
                               ti/tb-mapping
                               permitted-hoist-tagbodys
                               renames
                               nil)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1
        (values new-form t)))
    (setf (value-form form) new-form))
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (body form) ti/tb-mapping
                               (if (some (lambda (x) (typep x 'special-variable))
                                         (bindings form))
                                   '()
                                   permitted-hoist-tagbodys)
                               renames
                               leaving-tagbody)
    (setf (body form) new-form)
    (values form control-terminates)))

(defmethod simplify-control-flow-1 ((form ast-multiple-value-call) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (function-form form)
                               ti/tb-mapping
                               permitted-hoist-tagbodys
                               renames
                               nil)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1
        (values new-form t)))
    (setf (function-form form) new-form))
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (value-form form)
                               ti/tb-mapping
                               permitted-hoist-tagbodys
                               renames
                               nil)
    (setf (value-form form) new-form)
    (values form control-terminates)))

(defmethod simplify-control-flow-1 ((form ast-multiple-value-prog1) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (value-form form)
                               ti/tb-mapping
                               permitted-hoist-tagbodys
                               renames
                               nil)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1
        (values new-form t)))
    (setf (value-form form) new-form))
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (body form)
                               ti/tb-mapping
                               permitted-hoist-tagbodys
                               renames
                               leaving-tagbody)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1
        (values (ast `(progn ,(value-form form)
                             ,new-form)
                     form)
                t)))
    (setf (body form) new-form))
  (values form nil))

(defmethod simplify-control-flow-1 ((form ast-progn) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (let ((new-subforms '()))
    (loop
       for subform in (forms form)
       do (multiple-value-bind (new-form control-terminates)
              (simplify-control-flow-1 subform ti/tb-mapping permitted-hoist-tagbodys renames
                                       ;; last form
                                       (if (eql (length new-subforms) (1- (length (forms form))))
                                           leaving-tagbody
                                           nil))
            (push new-form new-subforms)
            (when control-terminates
              (when (not (eql (length new-subforms) (length (forms form))))
                (change-made))
              (return-from simplify-control-flow-1
                (values (if (endp (rest new-subforms))
                            (first new-subforms)
                            (ast `(progn ,@(reverse new-subforms))
                                 form))
                        t)))))
    (setf (forms form) (reverse new-subforms)))
  (values form nil))

(defmethod simplify-control-flow-1 ((form ast-quote) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (declare (ignore ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody))
  (values form nil))

(defmethod simplify-control-flow-1 ((form ast-return-from) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (declare (ignore leaving-tagbody))
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (value form) ti/tb-mapping permitted-hoist-tagbodys renames nil)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1
        (values new-form t)))
    (setf (value form) new-form))
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (info form) ti/tb-mapping permitted-hoist-tagbodys renames nil)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1
        (values (ast `(progn ,(value form)
                             ,new-form)
                     form)
                t)))
    (setf (info form) new-form))
  (values form t))

(defmethod simplify-control-flow-1 ((form ast-setq) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (declare (ignore leaving-tagbody))
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (value form) ti/tb-mapping permitted-hoist-tagbodys renames nil)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1 (values new-form t)))
    (setf (value form) new-form))
  (values form nil))

(defmethod simplify-control-flow-1 ((form ast-tagbody) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (declare (ignore leaving-tagbody))
  (push (cons (info form) form) ti/tb-mapping)
  (push form permitted-hoist-tagbodys)
  ;; Snap statements that're GO forms jumping to another statement in this tagbody.
  ;; Make sure to preserve the entry (first) statement.
  (loop
     (let ((victim (find-if (lambda (x)
                              (destructuring-bind (go-tag stmt) x
                                (and (typep stmt 'ast-go)
                                     (eql (go-tag-tagbody (target stmt)) (info form))
                                     ;; Avoid folding infinite loops into themselves.
                                     (not (eql (target stmt) go-tag))
                                     ;; Avoid moving the entry block.
                                     (not (eql (target stmt) (first (first (statements form))))))))
                            (statements form))))
       (when (not victim)
         (return))
       (change-made)
       (let ((target (find (target (second victim)) (statements form) :key #'first)))
         (assert target)
         ;; Replace GO with target's statement.
         (setf (second victim) (second target))
         ;; Rename target go tag to victim.
         ;;(format t "Rename ~S to ~S~%" (first target) (first victim))
         (push (list (first target) (first victim)) renames)
         (incf (go-tag-use-count (first victim)) (go-tag-use-count (first target)))
         (setf (statements form) (remove target (statements form))))
       ;; Update any top-level GO forms with the new name.
       (loop
          for (go-tag statement) in (statements form)
          when (typep statement 'ast-go)
          do (setf (target statement) (simplify-control-flow-get-replacement-go-tag (target statement)
                                                                           renames)))))
  ;; Visit go-tag statements.
  (loop
     for statements in (statements form)
     do (let ((*tagbody-statement-stack* (list* (first statements) *tagbody-statement-stack*)))
          (setf (second statements) (simplify-control-flow-1 (second statements)
                                                             ti/tb-mapping
                                                             permitted-hoist-tagbodys
                                                             renames
                                                             form))))
  form)

(defmethod simplify-control-flow-1 ((form ast-the) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (value form) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1
        (values new-form t)))
    (setf (value form) new-form))
  (values form nil))

(defmethod simplify-control-flow-1 ((form ast-unwind-protect) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (protected-form form) ti/tb-mapping '() renames nil)
    (setf (protected-form form) new-form
          (cleanup-function form) (simplify-control-flow-1 (cleanup-function form)
                                                           ti/tb-mapping
                                                           permitted-hoist-tagbodys
                                                           renames
                                                           nil))
    (values form control-terminates)))

(defmethod simplify-control-flow-1 ((form ast-call) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (let ((new-arguments '()))
    (loop
       for argument in (arguments form)
       do (multiple-value-bind (new-form control-terminates)
              (simplify-control-flow-1 argument ti/tb-mapping permitted-hoist-tagbodys renames nil)
            (push new-form new-arguments)
            (when control-terminates
              (when (not (eql (length new-arguments) (length (arguments form))))
                (change-made))
              (return-from simplify-control-flow-1
                (values (if (endp (rest new-arguments))
                            (first new-arguments)
                            (ast `(progn ,@(reverse new-arguments))
                                 form))
                        t)))))
    (setf (arguments form) (reverse new-arguments)))
  ;; TODO: This is where no-return functions can be handled.
  (values form nil))

(defmethod simplify-control-flow-1 ((form ast-jump-table) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (multiple-value-bind (new-form control-terminates)
      (simplify-control-flow-1 (value form) ti/tb-mapping permitted-hoist-tagbodys renames nil)
    (when control-terminates
      (change-made)
      (return-from simplify-control-flow-1
        (values new-form t)))
    (setf (value form) new-form))
  ;; Fix all the GO forms without letting them turn into RETURN-FROMs or anything else.
  (loop
     for target in (targets form)
     do (setf (target target) (simplify-control-flow-get-replacement-go-tag (target target) renames)))
  (values form t))

(defmethod simplify-control-flow-1 ((form lexical-variable) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (declare (ignore ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody))
  (values form nil))

(defmethod simplify-control-flow-1 ((form lambda-information) ti/tb-mapping permitted-hoist-tagbodys renames leaving-tagbody)
  (let ((*current-lambda* form))
    (loop
       for arg in (lambda-information-optional-args form)
       ;; init-form
       do (setf (second arg) (simplify-control-flow-1 (second arg) ti/tb-mapping '() renames nil)))
    (loop
       for arg in (lambda-information-key-args form)
       ;; init-form
       do (setf (second arg) (simplify-control-flow-1 (second arg) ti/tb-mapping '() renames nil)))
    (setf (lambda-information-body form) (simplify-control-flow-1 (lambda-information-body form)
                                                                  ti/tb-mapping
                                                                  '()
                                                                  renames
                                                                  nil))
    (values form nil)))
