;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.c)

(defvar *should-inline-functions* t)

(defparameter *perform-tce* nil
  "When true, attempt to eliminate tail calls.")

(defparameter *suppress-builtins* nil
  "When true, the built-in functions will not be used and full calls will
be generated instead.")

(defparameter *enable-branch-tensioner* t)
(defparameter *enable-stack-alignment-checking* nil)
(defparameter *trace-asm* nil)

(defvar *jump-table-size-min* 4)
(defvar *jump-table-size-max* 64)

(defvar *load-time-value-hook*)

(defvar *optimize-restrictions* '())
(defvar *optimize-policy* '(safety 3 debug 3))

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

(defun compile-lambda (lambda &optional env target-architecture)
  (codegen-lambda (compile-lambda-1 lambda env target-architecture) target-architecture))

(defun default-architecture (architecture)
  (or architecture
      #+x86-64 :x86-64
      #+arm64 :arm64))

(defun codegen-lambda (lambda &optional target-architecture)
  (ecase (default-architecture target-architecture)
    (:x86-64
     (mezzano.compiler.codegen.x86-64:codegen-lambda lambda))
    (:arm64
     (mezzano.compiler.codegen.arm64:codegen-lambda lambda))))

;; Parse lambda and optimize, but do not do codegen.
(defun compile-lambda-1 (lambda &optional env target-architecture)
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
              (pass1-lambda lambda env)
              (default-architecture target-architecture))))))))))))))

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

(defun optimize-quality-1 (qualities quality)
  (max (getf *optimize-restrictions* quality 0)
       (or (getf qualities quality nil)
           (getf *optimize-policy* quality 0))))

(defun optimize-quality (ast-node quality)
  (optimize-quality-1 (ast-optimize ast-node) quality))

(defun run-optimizers (form target-architecture)
  (when (eql (optimize-quality form 'compiliation-speed) 3)
    (return-from run-optimizers form))
  (dotimes (i 20 (progn (warn 'sys.int::simple-style-warning
                              :format-control "Possible optimizer infinite loop."
                              :format-arguments '())
                        form))
    (let ((*change-count* 0))
      ;; Must be run before lift.
      (setf form (inline-functions (detect-uses form) target-architecture))
      (setf form (lambda-lift (detect-uses form)))
      ;; Key arg conversion must be performed after lambda-lifting, so as not to
      ;; complicate the lift code.
      (setf form (lower-keyword-arguments form))
      (setf form (constprop (detect-uses form)))
      (setf form (simplify (detect-uses form)))
      (setf form (kill-temporaries (detect-uses form)))
      (setf form (value-aware-lowering (detect-uses form)))
      (setf form (simplify-control-flow (detect-uses form)))
      (setf form (blexit (detect-uses form)))
      (setf form (apply-transforms (detect-uses form) target-architecture))
      (detect-uses form)
      (when (eql *change-count* 0)
        (return form)))))

(defun fixnump (object)
  (typep object '(signed-byte 63)))

(defun error-program-error (format-control &rest format-arguments)
  (error 'sys.int::simple-program-error
         :format-control format-control
         :format-arguments format-arguments))

;; A wrapper around SUBTYPEP that can be traced safely.
(defun compiler-subtypep (type-1 type-2 &optional environment)
  (subtypep type-1 type-2 environment))
