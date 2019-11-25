;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The fast evaluator, passes most forms to the compiler

(defpackage :mezzano.fast-eval
  (:export #:eval-in-lexenv)
  (:use :cl))

(in-package :mezzano.fast-eval)

(defun eval-compile (form env)
  (let ((mezzano.compiler::*load-time-value-hook* 'mezzano.compiler::eval-load-time-value)
        (*compile-file-pathname* (or *compile-file-pathname*
                                     *load-pathname*)))
    (funcall (mezzano.compiler::compile-lambda `(lambda () (progn ,form))
                                               env))))

(defun eval-progn-body (forms env)
  (do ((itr forms (cdr itr)))
      ((null (cdr itr))
       (eval-in-lexenv (car itr) env))
    (eval-in-lexenv (car itr) env)))

(defun eval-one-setq (var val env)
  (check-type var symbol)
  (multiple-value-bind (expansion expandedp)
      (macroexpand-1 var)
    (declare (ignore expansion))
    (cond (expandedp
           (eval-in-lexenv `(setf ,var ,val) env))
          (t (setf (symbol-value var) (eval-in-lexenv val env))))))

(defun eval-setq (pairs env)
  (destructuring-bind (var val &rest rest)
      pairs
    (cond (rest
           (eval-one-setq var val env)
           (eval-setq rest env))
          (t (eval-one-setq var val env)))))

(defun eval-cons (form env)
  (case (first form)
    ((if) (if (eval-in-lexenv (second form) env)
              (eval-in-lexenv (third form) env)
              (eval-in-lexenv (fourth form) env)))
    ((quote) (second form))
    ((function)
     (cond ((and (consp (second form))
                 (eql (first (second form)) 'lambda))
            (eval-compile form env))
           (t (fdefinition (second form)))))
    ((progn) (eval-progn-body (rest form) env))
    ((eval-when)
     (multiple-value-bind (compile load eval)
         (mezzano.internals::parse-eval-when-situation (second form))
       (declare (ignore compile load))
       (when eval
         (eval-progn-body (cddr form) env))))
    ((multiple-value-call)
     (apply (eval-in-lexenv (second form) env)
            (apply #'append
                   (mapcar (lambda (f) (multiple-value-list (eval-in-lexenv f env)))
                           (cddr form)))))
    ((multiple-value-prog1)
     (multiple-value-prog1
         (eval-in-lexenv (second form) env)
       (eval-progn-body (cddr form) env)))
    ((catch)
     (catch (eval-in-lexenv (second form) env)
       (eval-progn-body (cddr form) env)))
    ((throw)
     (throw (eval-in-lexenv (second form) env)
       (eval-in-lexenv (third form) env)))
    ((progv)
     (progv (eval-in-lexenv (second form) env) (eval-in-lexenv (third form) env)
       (eval-progn-body (cdddr form) env)))
    ;;((macrolet))
    ;;((symbol-macrolet))
    ((setq)
     (when (rest form)
       (eval-setq (rest form) env)))
    ((unwind-protect)
     (unwind-protect
         (eval-in-lexenv (second form) env)
       (eval-progn-body (cddr form) env)))
    ((load-time-value)
     (eval-in-lexenv (second form) env))
    (t (cond ((or (not (symbolp (first form)))
                  (special-operator-p (first form)))
              (eval-compile form env))
             (t (multiple-value-bind (expansion expanded-p)
                    (macroexpand form env)
                  (cond (expanded-p
                         (eval-in-lexenv expansion env))
                        (t
                         (eval-call form env)))))))))

(defun eval-call (form env)
  ;; Don't use FDEFINITION, poke directly in the fref to stop trace wrappers
  ;; from being hidden.
  (let ((fn (mezzano.internals::function-reference-function
             (mezzano.internals::function-reference (first form)))))
    (cond (fn
           (apply fn
                  (mapcar (lambda (f) (eval-in-lexenv f env))
                          (rest form))))
          (t
           ;; Punt if the function is not bound, take advantage
           ;; of the restarts set up by the runtime.
           (eval-compile form env)))))

(defun eval-symbol (form env)
  (let ((expanded (macroexpand form env)))
    (cond ((symbolp expanded)
           (restart-case (symbol-value expanded)
             (use-value (v)
               :interactive (lambda ()
                              (format t "Enter a new value (evaluated): ")
                              (list (eval (read))))
               :report (lambda (s) (format s "Input a value to be used in place of ~S." form))
               v)
             (store-value (v)
               :interactive (lambda ()
                              (format t "Enter a new value (evaluated): ")
                              (list (eval (read))))
               :report (lambda (s) (format s "Input a new value for ~S." form))
               (setf (symbol-value form) v))))
          (t (eval-in-lexenv expanded env)))))

(defun eval-in-lexenv (form env)
  (typecase form
    (cons (eval-cons form env))
    (symbol (eval-symbol form env))
    (t form)))
