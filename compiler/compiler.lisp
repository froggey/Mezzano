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
(defvar *optimize-policy* '(safety 3 debug 3 speed 1))

(defvar *use-new-compiler* t)

(defvar *meters* (make-hash-table))

(defmacro with-metering ((meter) &body body)
  `(call-with-metering ',meter (lambda () ,@body)))

(defun call-with-metering (meter fn)
  (let ((start-time (get-internal-real-time)))
    (multiple-value-prog1
        (funcall fn)
      (let* ((total-time (- (get-internal-real-time) start-time))
             (seconds (/ total-time internal-time-units-per-second)))
        (incf (gethash meter *meters* 0.0) (float seconds))))))

(defun log-event (event-name)
  (incf (gethash event-name *meters* 0)))

(defun reset-meters ()
  (clrhash *meters*))

(defun compiler-state-bindings ()
  (let ((symbols '(*should-inline-functions*
                   *perform-tce*
                   *suppress-builtins*
                   *enable-branch-tensioner*
                   *enable-stack-alignment-checking*
                   *trace-asm*
                   *jump-table-size-min*
                   *jump-table-size-max*
                   *load-time-value-hook*
                   *optimize-restrictions*
                   *optimize-policy*
                   *verify-special-stack*
                   *constprop-lambda-copy-limit*
                   *use-new-compiler*)))
    (loop
       for sym in symbols
       collect (list sym (symbol-value sym)))))

(defclass target () ())
(defclass x86-64-target (target) ())
(defclass arm64-target (target) ())

(defun canonicalize-target (target)
  (when (typep target 'target)
    (return-from canonicalize-target
      target))
  (ecase (default-architecture target)
    (:x86-64 (make-instance 'x86-64-target))
    (:arm64 (make-instance 'arm64-target))))

(defun default-architecture (architecture)
  (or architecture
      #+x86-64 :x86-64
      #+arm64 :arm64))

(defun compile-lambda (lambda &optional env target-architecture)
  (log-event :compile-lambda)
  (let ((target (canonicalize-target target-architecture)))
    (codegen-lambda (compile-lambda-1 lambda env target) target)))

(defun compile-ast (ast &optional target-architecture)
  (let ((target (canonicalize-target target-architecture)))
    (codegen-lambda (compile-lambda-2 ast target) target)))

(defgeneric codegen-lambda-using-target (lambda target))

(defun codegen-lambda (lambda &optional target-architecture)
  (with-metering (:code-generation)
    (if *use-new-compiler*
        (mezzano.compiler.backend:compile-backend-function
         (mezzano.compiler.backend.ast-convert:convert lambda)
         (canonicalize-target target-architecture))
        (codegen-lambda-using-target
         lambda
         (canonicalize-target target-architecture)))))

(defmethod codegen-lambda-using-target (lambda (target x86-64-target))
  (mezzano.compiler.codegen.x86-64:codegen-lambda lambda))

(defmethod codegen-lambda-using-target (lambda (target arm64-target))
  (mezzano.compiler.codegen.arm64:codegen-lambda lambda))

(defun compile-lambda-1 (lambda &optional env target-architecture)
  (let ((target (canonicalize-target target-architecture)))
    (compile-lambda-2 (pass1-lambda lambda env) target)))

;; Parse lambda and optimize, but do not do codegen.
(defun compile-lambda-2 (form &optional target-architecture)
  ;; Don't optimize at (compiliation-speed 3).
  (let ((run-optimizations (not (eql (optimize-quality form 'compilation-speed) 3)))
        (target (canonicalize-target target-architecture)))
    (when run-optimizations
      (setf form (run-optimizers form target)))
    (unless run-optimizations
      (setf form (lower-keyword-arguments form target)))
    ;; Lower complex lambda lists.
    (setf form (lower-arguments form))
    ;; Lower closed-over variables.
    (setf form (lower-environment form))
    (when run-optimizations
      ;; Run a final simplify pass to kill off any useless bindings.
      (setf form (simplify form target)))
    ;; Make the dynamic environment explicit.
    (setf form (lower-special-bindings form))
    (when run-optimizations
      (setf form (simplify form target)))
    form))

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
      (let ((*load-time-value-hook* 'eval-load-time-value)
            (*gensym-counter* 0))
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

(defvar *max-optimizer-iterations* 20)

(defvar *optimization-passes*
  ;; DX conversion should come early before variables get optimized away & declarations lost.
  '(convert-dynamic-extent
    ;; Inlining must be run before lifting.
    inline-functions
    lambda-lift
    ;; Key arg conversion must be performed after lambda-lifting, so as not to
    ;; complicate the lift code.
    lower-keyword-arguments
    constprop
    simplify
    kill-temporaries
    value-aware-lowering
    simplify-control-flow
    blexit
    apply-transforms))

(defun run-optimizers (form target-architecture)
  (setf target-architecture (canonicalize-target target-architecture))
  (with-metering (:ast-optimize)
    (dotimes (i *max-optimizer-iterations*
              (progn (warn 'sys.int::simple-style-warning
                           :format-control "Possible optimizer infinite loop."
                           :format-arguments '())
                     form))
      (let ((*change-count* 0))
        (dolist (pass *optimization-passes*)
          (setf form (funcall pass form target-architecture)))
        (when (eql *change-count* 0)
          (return form))))))

(defun fixnump (object)
  (typep object '(signed-byte 63)))

(defun error-program-error (format-control &rest format-arguments)
  (error 'sys.int::simple-program-error
         :format-control format-control
         :format-arguments format-arguments))

;; A wrapper around SUBTYPEP that can be traced safely.
(defun compiler-subtypep (type-1 type-2 &optional environment)
  (subtypep type-1 type-2 environment))
