;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defun ast (form &optional (inherit *current-lambda*))
  "Produce AST nodes from a programmer-friendly representation.
*CURRENT-LAMBDA* must be bound appropriately.
Inherit source locations/etc from INHERIT."
  (convert-ast-form form inherit '()))

(defun convert-ast-form (form inherit new-variables)
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
         inherit
         new-variables
         (rest form)))

(defun convert-ast-block (inherit new-variables name body)
  (check-type name (or block-information symbol))
  (let ((new-block (if (symbolp name)
                       (make-instance 'block-information
                                      :inherit inherit
                                      :name name
                                      :definition-point *current-lambda*
                                      :use-count 1)
                       name)))
    (when (symbolp name)
      (push (cons name new-block) new-variables))
    (make-instance 'ast-block
                   :inherit inherit
                   :info new-block
                   :body (convert-ast-form body inherit new-variables))))

(defun convert-ast-function (inherit new-variables name)
  (declare (ignore new-variables))
  (make-instance 'ast-function
                 :inherit inherit
                 :name name))

(defun convert-ast-go (inherit new-variables target info)
  (make-instance 'ast-go
                 :inherit inherit
                 :target (convert-ast-form target inherit new-variables)
                 :info (convert-ast-form info inherit new-variables)))

(defun convert-ast-if (inherit new-variables test then else)
  (make-instance 'ast-if
                 :inherit inherit
                 :test (convert-ast-form test inherit new-variables)
                 :then (convert-ast-form then inherit new-variables)
                 :else (convert-ast-form else inherit new-variables)))

(defun convert-ast-let (inherit new-variables bindings body)
  (let ((real-bindings (loop
                          for (variable init-form) in bindings
                          collect (list (etypecase variable
                                          (symbol
                                           (let ((new (make-instance 'lexical-variable
                                                                     :inherit inherit
                                                                     :name variable
                                                                     :definition-point *current-lambda*
                                                                     :use-count 1)))
                                             (push (cons variable new) new-variables)
                                             new))
                                          ((or lexical-variable special-variable)
                                           variable))
                                        init-form))))
    (make-instance 'ast-let
                   :inherit inherit
                   :bindings (loop
                                for (variable init-form) in real-bindings
                                collect (list variable (convert-ast-form init-form inherit new-variables)))
                   :body (convert-ast-form body inherit new-variables))))

(defun convert-ast-multiple-value-bind (inherit new-variables bindings value-form body)
  (let ((value (convert-ast-form value-form inherit new-variables))
        (real-bindings '()))
    (dolist (binding bindings)
      (cond ((symbolp binding)
             (let ((new (make-instance 'lexical-variable
                                       :inherit inherit
                                       :name binding
                                       :definition-point *current-lambda*
                                       :use-count 1)))
               (push (cons binding new) new-variables)
               (push new real-bindings)))
            (t (push binding real-bindings))))
    (make-instance 'ast-multiple-value-bind
                   :inherit inherit
                   :bindings (reverse real-bindings)
                   :value-form value
                   :body (convert-ast-form body inherit new-variables))))

(defun convert-ast-multiple-value-call (inherit new-variables function value-form)
  (make-instance 'ast-multiple-value-call
                 :inherit inherit
                 :function-form (convert-ast-form function inherit new-variables)
                 :value-form (convert-ast-form value-form inherit new-variables)))

(defun convert-ast-multiple-value-prog1 (inherit new-variables value-form body)
  (make-instance 'ast-multiple-value-prog1
                 :inherit inherit
                 :value-form (convert-ast-form value-form inherit new-variables)
                 :body (convert-ast-form body inherit new-variables)))

(defun convert-ast-progn (inherit new-variables &rest body)
  (make-instance 'ast-progn
                 :inherit inherit
                 :forms (loop
                           for form in body
                           collect (convert-ast-form form inherit new-variables))))

(defun convert-ast-quote (inherit new-variables value)
  (declare (ignore new-variables))
  (make-instance 'ast-quote
                 :inherit inherit
                 :value value))

(defun convert-ast-return-from (inherit new-variables target value-form info)
  (make-instance 'ast-return-from
                 :inherit inherit
                 :target (convert-ast-form target inherit new-variables)
                 :value (convert-ast-form value-form inherit new-variables)
                 :info (convert-ast-form info inherit new-variables)))

(defun convert-ast-setq (inherit new-variables variable value)
  (check-type variable (or symbol lexical-variable))
  (make-instance 'ast-setq
                 :inherit inherit
                 :variable (convert-ast-form variable inherit new-variables)
                 :value (convert-ast-form value inherit new-variables)))

(defun convert-ast-tagbody (inherit new-variables info &rest statements)
  (check-type info (or symbol tagbody-information))
  (let ((tagbody-info (cond ((symbolp info)
                             (make-instance 'tagbody-information
                                            :inherit inherit
                                            :name info
                                            :definition-point *current-lambda*
                                            :use-count 1))
                            (t info)))
        (new-go-tags '()))
    (when (symbolp info)
      (push (cons info tagbody-info) new-variables))
    (loop
       for (go-tag statement) in statements
       do (etypecase go-tag
            (go-tag
             (assert (eql (go-tag-tagbody go-tag) tagbody-info)))
            (symbol
             (let ((tag (make-instance 'go-tag
                                       :inherit inherit
                                       :name go-tag
                                       :tagbody tagbody-info
                                       :use-count 1)))
               (push (cons go-tag tag) new-variables)
               (push tag new-go-tags)))))
    (setf (tagbody-information-go-tags tagbody-info) (append (reverse new-go-tags)
                                                             (tagbody-information-go-tags tagbody-info)))
    (make-instance 'ast-tagbody
                   :inherit inherit
                   :info tagbody-info
                   :statements (loop
                                  for (go-tag statement) in statements
                                  collect (list (convert-ast-form go-tag inherit new-variables)
                                                (convert-ast-form statement inherit new-variables))))))

(defun convert-ast-the (inherit new-variables type value)
  (make-instance 'ast-the
                 :inherit inherit
                 :type (sys.int::typeexpand type)
                 :value (convert-ast-form value inherit new-variables)))

(defun convert-ast-unwind-protect (inherit new-variables protected-form cleanup-function)
  (make-instance 'ast-unwind-protect
                 :inherit inherit
                 :protected-form (convert-ast-form protected-form inherit new-variables)
                 :cleanup-function (convert-ast-form cleanup-function inherit new-variables)))

(defun convert-ast-call (inherit new-variables name &rest arguments)
  (make-instance 'ast-call
                 :inherit inherit
                 :name name
                 :arguments (loop
                               for form in arguments
                               collect (convert-ast-form form inherit new-variables))))

(defun convert-ast-jump-table (inherit new-variables value &rest targets)
  (make-instance 'ast-jump-table
                 :inherit inherit
                 :value (convert-ast-form value inherit new-variables)
                 :targets (loop
                             for form in targets
                             collect (convert-ast-form form inherit new-variables))))
