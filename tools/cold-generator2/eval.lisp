;;;; Basic evaluator used while loading compiled files

(defpackage :mezzano.cold-generator.eval
  (:use :cl)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment))
  (:export #:eval-toplevel #:eval-toplevel-list
           #:*ensure-class-handler*))

(in-package :mezzano.cold-generator.eval)

(defparameter *ensure-class-handler* nil)

(defun fix-up-funcall-function (fn)
  ;; When creating a new deferred funcall form, the function object must
  ;; be wrapped in #' or ' depending on if it is a function name or not.
  (if (or (symbolp fn) (consp fn))
      `#',fn
      `',fn))

(defun apply-toplevel (fn args env)
  (cond ((eql fn (env:translate-symbol env 'mezzano.internals::%defun))
         (apply #'env:%defun env args))
        ((eql fn (env:translate-symbol env 'list))
         args)
        ((and *ensure-class-handler*
              (eql fn (env:translate-symbol env 'mezzano.clos:ensure-class)))
         (apply *ensure-class-handler* env args))
        (t
         ;; Bail on everything else.
         (values nil `(funcall ,(fix-up-funcall-function fn)
                               ,@(mapcar (lambda (x) `',x) args))))))

(defun eval-toplevel (form env)
  "Evaluate FORM in ENV for value.
Only supports the compiled-file subset."
  (when (symbolp form)
    (cond ((or (keywordp form)
               (member form '(nil t)))
           (return-from eval-toplevel (values form nil)))
          (t
           (error "Cannot eval symbol ~S" form))))
  (ecase (first form)
    ((if)
     (multiple-value-bind (value deferred)
         (eval-toplevel (second form) env)
       (cond (deferred
              ;; Unknown test, return the full IF.
              (values nil
                      `(if ,deferred
                           ,(third form)
                           ,(fourth form))))
             (t
              ;; Evaluated test, evaluate body.
              (eval-toplevel (if value
                                 (third form)
                                 (fourth form))
                             env)))))
    ((quote)
     (values (second form) nil))
    ((function)
     (values nil form))
    ((funcall)
     (multiple-value-bind (fn deferred-fn)
         ;; Special-case #'NAME, evaulating directly to the name.
         (cond ((typep (second form) '(cons (eql function) (cons (or symbol (cons symbol (cons symbol null))) null)))
                (second (second form)))
               (t
                (eval-toplevel (second form) env)))
       (let ((args '())
             (unevaluated-args (cddr form)))
         (when deferred-fn
           (return-from eval-toplevel
             `(funcall ,deferred-fn ,@unevaluated-args)))
         (loop
            (when (endp unevaluated-args)
              (return))
            (multiple-value-bind (arg deferred-arg)
                (eval-toplevel (first unevaluated-args) env)
              (when deferred-arg
                ;; Bail out.
                (return-from eval-toplevel
                  (values nil
                          `(funcall ,(fix-up-funcall-function fn)
                                    ,@(loop
                                         for a in (reverse args)
                                         collect `',a)
                                    ,@unevaluated-args))))
              (pop unevaluated-args)
              (push arg args)))
         (apply-toplevel fn (reverse args) env))))))

(defun eval-toplevel-list (environment form-list)
  (let ((deferred-forms '()))
    (dolist (form form-list)
      (multiple-value-bind (value deferred)
          (eval-toplevel form environment)
        (declare (ignore value))
        (when deferred
          (push deferred deferred-forms))))
    (reverse deferred-forms)))
