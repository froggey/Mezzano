;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.clos)

(defvar *method-combinations* (make-hash-table))

(defclass method-combination ()
  ((%name :initarg :name :reader method-combination-name)
   (%combiner :initarg :combiner :reader method-combination-combiner)))

(defun register-method-combination (name combiner)
  (check-type name symbol)
  (check-type combiner function)
  (assert (not (eql name 'standard)))
  (setf (gethash name *method-combinations*)
        (make-instance 'method-combination :name name :combiner combiner))
  name)

(defun method-combination-object-method-combination (method-combination-object)
  (first method-combination-object))

(defun method-combination-object-arguments (method-combination-object)
  (rest method-combination-object))

(defun resolve-method-combination (name &rest args)
  (cond ((eql name 'standard)
         (assert (endp args) (args)
                 "The STANDARD method combination accepts no arguments.")
         nil)
        (t
         (let ((mc (gethash name *method-combinations*)))
           (when (not mc)
             (error "Unknown method combination ~S." name))
           (list* mc args)))))

(defmacro define-method-combination (name &rest args)
  (cond ((and args
              (listp (first args)))
         `(define-method-combination-long-form ,name ,@args))
        (t
         `(define-method-combination-short-form ,name ,@args))))

(eval-when (:compile-toplevel :load-toplevel :execute)

(defmacro define-method-combination-short-form (name &key documentation identity-with-one-argument (operator name))
  (check-type name symbol)
  (check-type operator symbol)
  (check-type documentation (or string null))
  `(define-method-combination ,name (&optional (order :most-specific-first))
     ((around (:around))
      (primary (,name) :order order :required t))
     ,@(when documentation (list documentation))
     (let ((form (if (or (not ',identity-with-one-argument)
                         (rest primary))
                     (list* ',operator
                            (mapcar (lambda (method)
                                      `(call-method ,method))
                                    primary))
                     `(call-method ,(first primary)))))
       (if around
           `(call-method ,(first around)
                         (,@(rest around)
                            (make-method ,form)))
           form))))

;; To be reinstated when CLOS is usable in the cross-compilation environment.
#+(or)
(defclass method-group-specifier ()
  ((%name :initarg :name :reader method-group-specifier-name)
   (%qualifier-patterns :initarg :qualifier-patterns :reader method-group-specifier-qualifier-patterns)
   (%predicate :initarg :predicate :reader method-group-specifier-predicate)
   (%description :initarg :description :reader method-group-specifier-description)
   (%order-form :initarg :order-form :reader method-group-specifier-order-form)
   (%required :initarg :required :reader method-group-specifier-required)))

(defstruct method-group-specifier
  name
  qualifier-patterns
  predicate
  description
  order-form
  required)

(defun parse-method-group-specifier (specifier)
  (let ((name (first specifier))
        (remaining-specifier (rest specifier))
        (predicate nil)
        (qualifier-patterns '()))
    (check-type name symbol)
    ;; Pull the qualifier patterns or predicate from the specifier.
    (cond ((and (symbolp (first remaining-specifier))
                (not (member (first remaining-specifier) '(* nil))))
           ;; Predicate.
           (setf predicate (pop remaining-specifier)))
          (t
           ;; Eat qualifier patterns.
           (setf qualifier-patterns (loop
                                       while (and (not (endp remaining-specifier))
                                                  (or (eql (first remaining-specifier) '*)
                                                      (listp (first remaining-specifier))))
                                       collect (pop remaining-specifier)))
           (when (endp qualifier-patterns)
             (error "Invalid specifier ~S. No qualifier patterns or predicate found."
                    specifier))
           (dolist (pattern qualifier-patterns)
             (assert (or (eql pattern '*) (listp pattern))))))
    (destructuring-bind (&key description order required)
        remaining-specifier
      (make-method-group-specifier
       :name name
       :qualifier-patterns qualifier-patterns
       :predicate predicate
       :description description
       :order-form order
       :required required))))

(defmacro define-method-combination-long-form (name lambda-list (&rest method-group-specifiers) &body body)
  (check-type name symbol)
  (let ((generic-function-symbol nil)
        (decoded-method-group-specifiers (mapcar #'parse-method-group-specifier
                                                 method-group-specifiers)))
    (when (and body
               (consp (first body))
               (eql (first (first body)) :arguments))
      (error ":ARGUMENTS argument to DEFINE-METHOD-COMBINATION not supported."))
    (when (and body
               (consp (first body))
               (eql (first (first body)) :generic-function))
      (destructuring-bind (_ sym)
          (pop body)
        (declare (ignore _))
        (check-type sym (and symbol (not null)))
        (setf generic-function-symbol sym)))
    (when (not generic-function-symbol)
      (setf generic-function-symbol (gensym "GENERIC-FUNCTION")))
    `(register-method-combination
      ',name
      ,(generate-method-combination-lambda
        name
        generic-function-symbol
        lambda-list
        decoded-method-group-specifiers
        body))))

(defun match-qualifier-pattern (pattern qualifiers)
  (loop
     (cond ((and (null pattern)
                 (null qualifiers))
            (return t))
           ((eql pattern '*)
            (return t))
           ((and pattern qualifiers
                 ;; Hyperspec says that patterns are matched as if by
                 ;; EQUAL, except that * matches anything.
                 ;; However, SBCL compares list elements with EQ...
                 (eq (car pattern) (car qualifiers)))
            (pop pattern)
            (pop qualifiers))
           (t (return nil)))))

(defun generate-method-group-specifier-match (specifier method-symbol)
  `(,(if (method-group-specifier-predicate specifier)
         `(,(method-group-specifier-predicate specifier)
            (method-qualifiers ,method-symbol))
         `(or ,@(loop
                   for pattern in (method-group-specifier-qualifier-patterns specifier)
                   collect `(match-qualifier-pattern ',pattern (method-qualifiers ,method-symbol)))))
     (push ,method-symbol ,(method-group-specifier-name specifier))))

(defun generate-method-combination-lambda (name generic-function-symbol lambda-list method-group-specifiers body)
  (let ((current (gensym "METHOD"))
        (applicable-methods (gensym "APPLICABLE-METHODS")))
    `(lambda (,generic-function-symbol ,applicable-methods ,@lambda-list)
       (declare (ignorable ,generic-function-symbol)
                (sys.int::lambda-name (method-combination ,name)))
       (let (,@(loop
                  for specifier in method-group-specifiers
                  collect (method-group-specifier-name specifier)))
         (dolist (,current ,applicable-methods)
           (cond ,@(loop
                      for specifier in method-group-specifiers
                      collect (generate-method-group-specifier-match specifier current))
                 (t
                  (invalid-method-error ,current "No specifiers matched."))))
         ,@(loop
              for specifier in method-group-specifiers
              when (method-group-specifier-required specifier)
              collect `(when (endp ,(method-group-specifier-name specifier))
                         (error "No ~S methods in generic function ~S." ',(method-group-specifier-name specifier) ,generic-function-symbol)))
         ,@(loop
              for specifier in method-group-specifiers
              collect `(ecase ,(or (method-group-specifier-order-form specifier)
                                   :most-specific-first)
                         (:most-specific-first
                          (setf ,(method-group-specifier-name specifier) (reverse ,(method-group-specifier-name specifier))))
                         (:most-specific-last)))
         ,@body))))

)

(defun invalid-method-error (method format-control &rest format-arguments)
  (apply #'error format-control format-arguments))

;; Built-in method combinations.
;; The STANDARD method combination has special handling in Closette.

(define-method-combination +     :identity-with-one-argument t)
(define-method-combination and   :identity-with-one-argument t)
(define-method-combination append)
(define-method-combination list)
(define-method-combination max   :identity-with-one-argument t)
(define-method-combination min   :identity-with-one-argument t)
(define-method-combination nconc :identity-with-one-argument t)
(define-method-combination or    :identity-with-one-argument t)
(define-method-combination progn :identity-with-one-argument t)
