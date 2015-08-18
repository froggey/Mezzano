;;;; Copyright (c) 2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defun ast (form &optional (inherit *current-lambda*))
  "Produce AST nodes from a programmer-friendly representation.
*CURRENT-LAMBDA* must be bound appropriately.
Inherit source locations/etc from INHERIT."
  (declare (ignore inherit))
  (convert-ast-form form '()))

(defun convert-ast-form (form new-variables)
  (when (symbolp form)
    (let ((var (assoc form new-variables)))
      (assert var () "Missing substitution variable ~S" form)
      (return-from convert-ast-form (cdr var))))
  (when (not (consp form))
    (return-from convert-ast-form form))
  (apply (ecase (first form)
           ((block) #'convert-ast-block)
           ((function) #'convert-ast-function)
           ((go) #'convert-ast-go)
           ((if) #'convert-ast-if)
           ((let) #'convert-ast-let)
           ((multiple-value-bind) #'convert-ast-multiple-value-bind)
           ((multiple-value-call) #'convert-ast-multiple-value-call)
           ((multiple-value-prog1) #'convert-ast-multiple-value-prog1)
           ((progn) #'convert-ast-progn)
           ((quote) #'convert-ast-quote)
           ((return-from) #'convert-ast-return-from)
           ((setq) #'convert-ast-setq)
           ((tagbody) #'convert-ast-tagbody)
           ((the) #'convert-ast-the)
           ((unwind-protect) #'convert-ast-unwind-protect)
           ((call) #'convert-ast-call)
           ((jump-table) #'convert-ast-jump-table))
         new-variables (rest form)))

(defun convert-ast-block (new-variables name body)
  (check-type name (or block-information symbol))
  (let ((new-block (if (symbolp name)
                       (make-instance 'block-information
                                      :name name
                                      :definition-point *current-lambda*
                                      :use-count 1)
                       name)))
    (when (symbolp name)
      (push (cons name new-block) new-variables))
    (make-instance 'ast-block
                   :info new-block
                   :body (convert-ast-form body new-variables))))

(defun convert-ast-function (new-variables name)
  (declare (ignore new-variables))
  (make-instance 'ast-function :name name))

(defun convert-ast-go (new-variables target info)
  (make-instance 'ast-go
                 :target (convert-ast-form target new-variables)
                 :info (convert-ast-form info new-variables)))

(defun convert-ast-if (new-variables test then else)
  (make-instance 'ast-if
                 :test (convert-ast-form test new-variables)
                 :then (convert-ast-form then new-variables)
                 :else (convert-ast-form else new-variables)))

(defun convert-ast-let (new-variables bindings body)
  (let ((real-bindings (loop
                          for (variable init-form) in bindings
                          collect (list (etypecase variable
                                          (symbol
                                           (let ((new (make-instance 'lexical-variable
                                                                     :name variable
                                                                     :definition-point *current-lambda*
                                                                     :use-count 1)))
                                             (push (cons variable new) new-variables)
                                             new))
                                          ((or lexical-variable special-variable)
                                           variable))
                                        init-form))))
    (make-instance 'ast-let
                   :bindings (loop
                                for (variable init-form) in real-bindings
                                collect (list variable (convert-ast-form init-form new-variables)))
                   :body (convert-ast-form body new-variables))))

(defun convert-ast-multiple-value-bind (new-variables bindings value-form body)
  (let ((value (convert-ast-form value-form new-variables))
        (real-bindings '()))
    (dolist (binding bindings)
      (cond ((symbolp binding)
             (let ((new (make-instance 'lexical-variable
                                       :name binding
                                       :definition-point *current-lambda*
                                       :use-count 1)))
               (push (cons binding new) new-variables)
               (push new real-bindings)))
            (t (push binding real-bindings))))
    (make-instance 'ast-multiple-value-bind
                   :bindings (reverse real-bindings)
                   :value-form value
                   :body (convert-ast-form body new-variables))))

(defun convert-ast-multiple-value-call (new-variables function value-form)
  (make-instance 'ast-multiple-value-call
                 :function-form (convert-ast-form function new-variables)
                 :value-form (convert-ast-form value-form new-variables)))

(defun convert-ast-multiple-value-prog1 (new-variables value-form body)
  (make-instance 'ast-multiple-value-prog1
                 :value-form (convert-ast-form value-form new-variables)
                 :body (convert-ast-form body new-variables)))

(defun convert-ast-progn (new-variables &rest body)
  (make-instance 'ast-progn
                 :forms (loop
                           for form in body
                           collect (convert-ast-form form new-variables))))

(defun convert-ast-quote (new-variables value)
  (declare (ignore new-variables))
  (make-instance 'ast-quote :value value))

(defun convert-ast-return-from (new-variables target value-form info)
  (make-instance 'ast-return-from
                 :target (convert-ast-form target new-variables)
                 :value (convert-ast-form value-form new-variables)
                 :info (convert-ast-form info new-variables)))

(defun convert-ast-setq (new-variables variable value)
  (check-type variable (or symbol lexical-variable))
  (make-instance 'ast-setq
                 :variable (convert-ast-form variable new-variables)
                 :value (convert-ast-form value new-variables)))

(defun convert-ast-tagbody (new-variables info &rest statements)
  (check-type info (or symbol tagbody-information))
  (let ((tagbody-info (cond ((symbolp info)
                             (make-instance 'tagbody-information
                                     :name info
                                     :definition-point *current-lambda*
                                     :use-count 1))
                            (t info)))
        (new-go-tags '()))
    (when (symbolp info)
      (push (cons info tagbody-info) new-variables))
    (dolist (stmt statements)
      (when (go-tag-p stmt)
        (assert (eql (go-tag-tagbody stmt) tagbody-info)))
      (when (symbolp stmt)
        (let ((tag (make-instance 'go-tag
                                  :name stmt
                                  :tagbody tagbody-info
                                  :use-count 1)))
          (push (cons stmt tag) new-variables)
          (push tag new-go-tags))))
    (setf (tagbody-information-go-tags tagbody-info) (append (reverse new-go-tags)
                                                             (tagbody-information-go-tags tagbody-info)))
    (make-instance 'ast-tagbody
                   :info tagbody-info
                   :statements (loop
                                  for statement in statements
                                  collect (convert-ast-form statement new-variables)))))

(defun convert-ast-the (new-variables type value)
  (make-instance 'ast-the
                 :type type
                 :value (convert-ast-form value new-variables)))

(defun convert-ast-unwind-protect (new-variables protected-form cleanup-function)
  (make-instance 'ast-unwind-protect
                 :protected-form (convert-ast-form protected-form new-variables)
                 :cleanup-function (convert-ast-form cleanup-function new-variables)))

(defun convert-ast-call (new-variables name &rest arguments)
  (make-instance 'ast-call
                 :name name
                 :arguments (loop
                               for form in arguments
                               collect (convert-ast-form form new-variables))))

(defun convert-ast-jump-table (new-variables value &rest targets)
  (make-instance 'ast-jump-table
                 :value (convert-ast-form value new-variables)
                 :targets (loop
                             for form in targets
                             collect (convert-ast-form form new-variables))))
