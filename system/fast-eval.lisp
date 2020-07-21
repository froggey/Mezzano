;;;; The fast evaluator, passes most forms to the compiler

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
    ((if)
     (destructuring-bind (test-form then-form &optional else-form)
         (rest form)
       (if (eval-in-lexenv test-form env)
           (eval-in-lexenv then-form env)
           (eval-in-lexenv else-form env))))
    ((quote)
     (destructuring-bind (value)
         (rest form)
       value))
    ((function)
     (destructuring-bind (name)
         (rest form)
       (cond ((and (consp name)
                   (eql (first name) 'lambda))
              (eval-compile form env))
             (t (fdefinition name)))))
    ((progn)
     (eval-progn-body (rest form) env))
    ((eval-when)
     (destructuring-bind (situations &body body)
         (rest form)
       (multiple-value-bind (compile load eval)
           (mezzano.internals::parse-eval-when-situation situations)
         (declare (ignore compile load))
         (when eval
           (eval-progn-body body env)))))
    ((multiple-value-call)
     (destructuring-bind (function-form &rest forms)
         (rest form)
       (apply (eval-in-lexenv function-form env)
              (apply #'append
                     (mapcar (lambda (f)
                               (multiple-value-list (eval-in-lexenv f env)))
                             forms)))))
    ((multiple-value-prog1)
     (destructuring-bind (first-form &rest forms)
         (rest form)
       (multiple-value-prog1
           (eval-in-lexenv first-form env)
         (eval-progn-body forms env))))
    ((catch)
     (destructuring-bind (tag &body body)
         (rest form)
       (catch (eval-in-lexenv tag env)
         (eval-progn-body body env))))
    ((throw)
     (destructuring-bind (tag result-form)
         (rest form)
       (throw (eval-in-lexenv tag env)
         (eval-in-lexenv result-form env))))
    ((progv)
     (destructuring-bind (symbols values &body body)
         (rest form)
       (progv (eval-in-lexenv symbols env) (eval-in-lexenv values env)
         (eval-progn-body body env))))
    ;;((macrolet))
    ;;((symbol-macrolet))
    ((setq)
     (when (rest form)
       (eval-setq (rest form) env)))
    ((unwind-protect)
     (destructuring-bind (protected-form &rest cleanup-forms)
         (rest form)
       (unwind-protect
            (eval-in-lexenv protected-form env)
         (eval-progn-body cleanup-forms env))))
    ((load-time-value)
     (destructuring-bind (form &optional read-only-p)
         (rest form)
       (declare (ignore read-only-p))
       (eval-in-lexenv form env)))
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
