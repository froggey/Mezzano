;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defvar *should-inline-functions* t)

(defvar *jump-table-size-min* 4)
(defvar *jump-table-size-max* 64)

(defvar *load-time-value-hook*)

(defun parse-declares (forms)
  "Extract any leading declare forms.
Returns 2 values:
The body, with the declare forms removed.
A list of any declaration-specifiers."
  (do ((declares '())
       (itr forms (cdr itr)))
      ((or (null itr)
	   ;; Stop when (car itr) is not a declare form.
	   (not (and (consp (car itr))
		     (eq 'declare (caar itr)))))
       (values itr (nreverse declares)))
    ;; Dump the bodies of each declare form into a single list.
    (dolist (decl (cdar itr))
      (push decl declares))))

(defun compile-lambda (lambda &optional env)
  (codegen-lambda (compile-lambda-1 lambda env)))

;; Parse lambda and optimize, but do not do codegen.
(defun compile-lambda-1 (lambda &optional env)
  (detect-uses
   (simplify
    (detect-uses
     ;; Make the dynamic environment explicit.
     (lower-special-bindings
      ;; Run a final simplify pass to kill off any useless bindings.
      (detect-uses
       (simplify
        (detect-uses
         ;; Lower closed-over variables.
         (lower-environment
          (detect-uses
           (lower-arguments
            (detect-uses
             (run-optimizers
              (pass1-lambda lambda env))))))))))))))

(defun eval-load-time-value (form read-only-p)
  (declare (ignore read-only-p))
  `(quote ,(eval form)))

(defun compile (name &optional definition)
  (unless definition
    (setf definition (or (when (symbolp name) (macro-function name))
                         (fdefinition name))))
  (when (functionp definition)
    (when (compiled-function-p definition)
      (return-from compile
        (values definition nil nil)))
    (multiple-value-bind (lambda-expression env)
        (function-lambda-expression definition)
      (when (null lambda-expression)
        (error "No source information available for ~S." definition))
      (when env
        (error "TODO: cannot compile functions defined outside the null lexical environment."))
      (setf definition lambda-expression)))
  (multiple-value-bind (fn warnings-p errors-p)
      (let ((*load-time-value-hook* 'eval-load-time-value))
        (compile-lambda definition))
    (cond (name
           (if (and (symbolp name) (macro-function name))
               (setf (macro-function name) fn)
               (setf (fdefinition name) fn))
           (values name warnings-p errors-p))
          (t (values fn warnings-p errors-p)))))

(defvar *current-lambda* nil
  "A lambda-information struct for the lambda currently being translated.")
(defvar *change-count* nil
  "Number of changes made by the optimizer passes.")

(defun change-made ()
  (when (and (boundp '*change-count*)
             *change-count*)
    (incf *change-count*)))

(defun run-optimizers (form)
  (dotimes (i 20 (progn (warn 'sys.int::simple-style-warning
			      :format-control "Possible optimizer infinite loop."
			      :format-arguments '())
			form))
    (let ((*change-count* 0))
      ;; Must be run before lift.
      (setf form (inline-functions (detect-uses form)))
      (setf form (lambda-lift (detect-uses form)))
      ;; Key arg conversion must be performed after lambda-lifting, so as not to
      ;; complicate the lift code.
      (setf form (lower-keyword-arguments form))
      (setf form (constprop (detect-uses form)))
      (setf form (simplify (detect-uses form)))
      (setf form (kill-temporaries (detect-uses form)))
      (setf form (value-aware-lowering (detect-uses form)))
      (setf form (simplify-control-flow (detect-uses form)))
      (detect-uses form)
      (when (eql *change-count* 0)
	(return form)))))
