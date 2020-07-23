;;;; Top-level compiler driver

(in-package :mezzano.compiler)

(defvar *should-inline-functions* t)

(defparameter *perform-tce* nil
  "When true, attempt to eliminate tail calls.")

(defparameter *trace-asm* nil)

(defvar *jump-table-size-min* 4)
(defvar *jump-table-size-max* 64)

(defvar *load-time-value-hook*)

(defvar *optimize-restrictions* '())
(defvar *optimize-policy* '(safety 3 debug 3 speed 1))

(defparameter *the-checks-types* nil
  "When true, THE forms will check types at SAFETY 3.
Currently disabled by default as it has a severe performance impact.")

(defun compiler-state-bindings ()
  (let ((symbols '(*should-inline-functions*
                   *perform-tce*
                   *trace-asm*
                   *jump-table-size-min*
                   *jump-table-size-max*
                   *load-time-value-hook*
                   *optimize-restrictions*
                   *optimize-policy*
                   *verify-special-stack*
                   *constprop-lambda-copy-limit*
                   *the-checks-types*
                   *max-optimizer-iterations*)))
    (loop
       for sym in symbols
       collect (list sym (symbol-value sym)))))

(defvar *compilation-unit-active* nil)

(defun call-with-compilation-unit (thunk &key override)
  (if (and (not override) *compilation-unit-active*)
      (funcall thunk)
      (let ((*compilation-unit-active* t))
        (funcall thunk))))

(defmacro with-compilation-unit ((&whole keys &key override) &body body)
  (declare (ignore override))
  `(call-with-compilation-unit (lambda () (progn ,@body)) ,@keys))

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
  (let ((*print-readably* nil))
    (let ((target (canonicalize-target target-architecture)))
      (codegen-lambda (compile-lambda-1 lambda env target) target))))

(defun compile-ast (ast &optional target-architecture)
  (let ((target (canonicalize-target target-architecture)))
    (codegen-lambda (compile-lambda-2 ast target) target)))

(defun codegen-lambda (lambda &optional target-architecture)
  (mezzano.compiler.backend:compile-backend-function
       (mezzano.compiler.backend.ast-convert:convert lambda)
       (canonicalize-target target-architecture)))

(defun compile-lambda-1 (lambda &optional env target-architecture)
  (let ((target (canonicalize-target target-architecture)))
    (compile-lambda-2 (pass1-lambda lambda env) target)))

;; Parse lambda and optimize, but do not do codegen.
(defun compile-lambda-2 (form &optional target-architecture)
  ;; Don't optimize at (compiliation-speed 3).
  (let ((run-optimizations (not (eql (optimize-quality form 'compilation-speed) 3)))
        (target (canonicalize-target target-architecture)))
    (when *the-checks-types*
      (setf form (insert-type-checks form target)))
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
      (let ((*prohibit-tagbody-fusion* t))
        (declare (special *prohibit-tagbody-fusion*))
        (setf form (simplify form target))))
    ;; Make the dynamic environment explicit.
    (setf form (lower-special-bindings form))
    (when run-optimizations
      (let ((*prohibit-tagbody-fusion* t))
        (declare (special *prohibit-tagbody-fusion*))
        (setf form (simplify form target))))
    ;; Lower the complicated DX list functions.
    (setf form (lower-dynamic-extent-list form))
    form))

(defun eval-load-time-value (form read-only-p)
  (declare (ignore read-only-p))
  `(quote ,(eval form)))

(defun compile (name &optional definition)
  (with-compilation-unit ()
    (unless definition
      (setf definition (or (when (symbolp name) (macro-function name))
                           (fdefinition name))))
    (when (functionp definition)
      (when (or (compiled-function-p definition)
                ;; ###: Should generic functions be compiled functions?
                (typep definition 'generic-function))
        (return-from compile
          (values definition nil nil)))
      (multiple-value-bind (lambda-expression env)
          (function-lambda-expression definition)
        (when (null lambda-expression)
          (error "No source information available for ~S." definition))
        (when env
          (error "TODO: cannot compile functions defined outside the null lexical environment."))
        (setf definition lambda-expression)))
    (with-compilation-unit ()
      (multiple-value-bind (fn warnings-p errors-p)
          (let ((*load-time-value-hook* 'eval-load-time-value)
                (*gensym-counter* 0))
            (compile-lambda definition))
        (cond (name
               (if (and (symbolp name) (macro-function name))
                   (setf (macro-function name) fn)
                   (setf (fdefinition name) fn))
               (values name warnings-p errors-p))
              (t (values fn warnings-p errors-p)))))))

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

(defparameter *report-after-optimize-passes* nil)

(defun run-optimizers (form target-architecture)
  (setf target-architecture (canonicalize-target target-architecture))
  (when *report-after-optimize-passes*
    (let ((*report-after-optimize-passes* nil))
      (format t "Before optimizations:~%")
      (write (unparse-compiler-form form) :pretty t)
      (terpri)))
  (dotimes (i *max-optimizer-iterations*
            (progn (warn 'sys.int::simple-style-warning
                         :format-control "Possible optimizer infinite loop."
                         :format-arguments '())
                   form))
    (let ((*change-count* 0))
      (dolist (pass *optimization-passes*)
        (setf form (funcall pass form target-architecture))
        (when *report-after-optimize-passes*
          (let ((*report-after-optimize-passes* nil))
            (format t "After optimize pass ~S:~%" pass)
            (write (unparse-compiler-form form) :pretty t)
            (terpri))))
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

(defun compiler-valid-subtypep (type-1 type-2 &optional environment)
  (multiple-value-bind (successp validp)
      (subtypep type-1 type-2 environment)
    (and successp validp)))

(defun compiler-valid-not-subtypep (type-1 type-2 &optional environment)
  (multiple-value-bind (successp validp)
      (subtypep type-1 type-2 environment)
    (and (not successp) validp)))

(defun compiler-type-equal-p (type-1 type-2 &optional environment)
  (multiple-value-bind (success-1-p valid-1-p)
      (compiler-subtypep type-1 type-2 environment)
    (multiple-value-bind (success-2-p valid-2-p)
        (compiler-subtypep type-2 type-1 environment)
      (values (and success-1-p success-2-p)
              (and valid-1-p valid-2-p)))))

(defun compiler-valid-type-equal-p (type-1 type-2 &optional environment)
  (multiple-value-bind (successp validp)
      (compiler-type-equal-p type-1 type-2 environment)
    (and successp validp)))

(defun compiler-valid-not-type-equal-p (type-1 type-2 &optional environment)
  (multiple-value-bind (successp validp)
      (compiler-type-equal-p type-1 type-2 environment)
    (and (not successp) validp)))

(defun fixnum-to-raw (integer)
  (check-type integer (signed-byte 63))
  (ash integer sys.int::+n-fixnum-bits+))

(defun character-to-raw (character)
  (check-type character character)
  (logior (ash (char-int character)
               (+ (byte-position sys.int::+immediate-tag+)
                  (byte-size sys.int::+immediate-tag+)))
          (dpb sys.int::+immediate-tag-character+
               sys.int::+immediate-tag+
               0)
          sys.int::+tag-immediate+))
