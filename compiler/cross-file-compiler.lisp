(in-package :mezzano.compiler)

(defvar *compile-verbose* t)
(defvar *compile-print* t)

(defvar *compile-file-pathname* nil)
(defvar *compile-file-truename* nil)

(defvar sys.int::*top-level-form-number* nil)

(defvar *output-fasl*)

(defun x-compile-top-level-implicit-progn (forms env mode)
  (dolist (f forms)
    (x-compile-top-level f env mode)))

(defun x-compile-top-level-lms-body (forms env mode)
  "Common code for handling the body of LOCALLY, MACROLET and SYMBOL-MACROLET forms at the top-level."
  (multiple-value-bind (body declares)
      (sys.int::parse-declares forms)
    (x-compile-top-level-implicit-progn
     body (extend-environment env :declarations declares) mode)))

(defun x-compile-top-level (form env &optional (mode :not-compile-time))
  "Cross-compile a top-level form.
3.2.3.1 Processing of Top Level Forms."
  (let ((expansion (sys.int::macroexpand-top-level-form form env)))
    (cond ((consp expansion)
           (case (first expansion)
             ;; 3. If the form is a progn form, each of its body forms is sequentially
             ;;    processed as a top level form in the same processing mode.
             ((progn)
              (x-compile-top-level-implicit-progn (rest expansion) env mode))
             ;; 4. If the form is a locally, macrolet, or symbol-macrolet, compile-file
             ;;    establishes the appropriate bindings and processes the body forms as
             ;;    top level forms with those bindings in effect in the same processing mode.
             ((locally)
              (x-compile-top-level-lms-body (rest expansion) env mode))
             ((macrolet)
              (destructuring-bind (definitions &body body) (rest expansion)
                (x-compile-top-level-lms-body body (sys.int::make-macrolet-env definitions env) mode)))
             ((symbol-macrolet)
              (destructuring-bind (definitions &body body) (rest expansion)
                (x-compile-top-level-lms-body body (sys.int::make-symbol-macrolet-env definitions env) mode)))
             ;; 5. If the form is an eval-when form, it is handled according to figure 3-7.
             ((eval-when)
              (destructuring-bind (situation &body body) (rest expansion)
                (multiple-value-bind (compile load eval)
                    (sys.int::parse-eval-when-situation situation)
                  ;; Figure 3-7. EVAL-WHEN processing
                  (cond
                    ;; Process as compile-time-too.
                    ((or (and compile load)
                         (and (not compile) load eval (eql mode :compile-time-too)))
                     (x-compile-top-level-implicit-progn body env :compile-time-too))
                    ;; Process as not-compile-time.
                    ((or (and (not compile) load eval (eql mode :not-compile-time))
                         (and (not compile) load (not eval)))
                     (x-compile-top-level-implicit-progn body env :not-compile-time))
                    ;; Evaluate.
                    ((or (and compile (not load))
                         (and (not compile) (not load) eval (eql mode :compile-time-too)))
                     (sys.int::x-eval `(progn ,@body) env))
                    ;; Discard.
                    ((or (and (not compile) (not load) eval (eql mode :not-compile-time))
                         (and (not compile) (not load) (not eval)))
                     nil)
                    (t (error "Impossible!"))))))
             ;; 6. Otherwise, the form is a top level form that is not one
             ;;    of the special cases. In compile-time-too mode, the compiler
             ;;    first evaluates the form in the evaluation environment
             ;;    and then minimally compiles it. In not-compile-time mode,
             ;;    the form is simply minimally compiled. All subforms are
             ;;    treated as non-top-level forms.
             (t (when (eql mode :compile-time-too)
                  (sys.int::x-eval expansion env))
                (when *output-fasl*
                  (x-compile expansion env)))))
          (t (when (eql mode :compile-time-too)
               (sys.int::x-eval expansion env))
             (when *output-fasl*
               (x-compile expansion env))))))

(defun sys.int::make-function (tag mc fixups constants gc-data wired)
  (declare (ignore wired))
  (assert (eql tag sys.int::+object-tag-function+))
  (sys.int::make-cross-function :mc mc
                                :constants constants
                                :fixups fixups
                                :gc-info gc-data))

(defvar *failed-fastload-by-symbol* (make-hash-table))

(defun x-compile (form env)
  (cond
    ;; Special case (define-lap-function name (options...) code...)
    ;; Don't macroexpand this, as it expands into a bunch of difficult to
    ;; recognize nonsense.
    ((and (consp form)
          (eql (first form) 'sys.int::define-lap-function)
          (>= (length form) 3))
     (destructuring-bind (name (&optional lambda-list frame-layout environment-vector-offset environment-vector-layout) &body code)
         (cdr form)
       (let ((docstring nil))
         (when (stringp (first code))
           (setf docstring (pop code)))
         (x-compile
          `(sys.int::%defun ',name
                            ',(sys.int::assemble-lap
                               code
                               name
                               (list :debug-info
                                     name
                                     frame-layout
                                     environment-vector-offset
                                     environment-vector-layout
                                     (when *compile-file-pathname*
                                       (princ-to-string *compile-file-pathname*))
                                     sys.int::*top-level-form-number*
                                     lambda-list
                                     docstring
                                     nil)
                               nil
                               *target-architecture*))
          env))))
    (t
     (x-compile-for-value form env)
     (sys.int::add-to-llf sys.int::+llf-drop+))))

(defun x-compile-for-value (form env)
  ;; FIXME: This should probably use compiler-macroexpand.
  (let ((expansion (macroexpand form env)))
    (cond
      ((symbolp expansion)
       (if (or (keywordp expansion) (member expansion '(nil t)))
           (sys.int::add-to-llf nil expansion)
           (sys.int::add-to-llf sys.int::+llf-funcall-n+ expansion 'symbol-value 1)))
      ((not (consp expansion))
       ;; Self-evaluating form.
       (sys.int::add-to-llf nil expansion))
      ((eql (first expansion) 'quote)
       (sys.int::add-to-llf nil (second expansion)))
      ((and (eql (first expansion) 'function)
            (consp (second expansion))
            (eql (first (second expansion)) 'lambda))
       (let* ((fn (compile-lambda (second expansion) env *target-architecture*)))
         (sys.int::add-to-llf nil fn)))
      ((eql (first expansion) 'function)
       (x-compile-for-value `(fdefinition ',(second form)) env))
      ((eql (first expansion) 'setq)
       (x-compile-for-value `(funcall #'(setf symbol-value)
                                      ,(third form)
                                      ',(second form))
                            env))
      ((eql (first expansion) 'if)
       (destructuring-bind (test then &optional else)
           (rest expansion)
         (x-compile-for-value test env)
         (sys.int::add-to-llf sys.int::+llf-if+)
         (x-compile-for-value then env)
         (sys.int::add-to-llf sys.int::+llf-else+)
         (x-compile-for-value else env)
         (sys.int::add-to-llf sys.int::+llf-fi+)))
      ((and (eql (first expansion) 'progn)
            (cdr expansion)
            (endp (cddr expansion)))
       ;; Only (progn foo) supported currently.
       (x-compile-for-value (second expansion) env))
      ((special-operator-p (first expansion))
       ;; Can't convert this, convert it to a zero-argument function and
       ;; call that. PROGN to avoid problems with DECLARE.
       (incf (gethash (first expansion) *failed-fastload-by-symbol* 0))
       (let* ((fn (compile-lambda `(lambda ()
                                     (declare (sys.int::lambda-name
                                               (sys.int::toplevel ,(when *compile-file-pathname*
                                                                         (princ-to-string *compile-file-pathname*))
                                                                  ,sys.int::*top-level-form-number*)))
                                     (progn ,expansion))
                                  env
                                  *target-architecture*)))
         (sys.int::add-to-llf sys.int::+llf-funcall-n+ fn 0)))
      (t
       ;; That should just leave ordinary calls.
       (let ((name (first expansion))
             (args (rest expansion)))
         ;; Unpeel funcall forms.
         (loop
            (cond ((and (eql name 'funcall)
                        (consp args)
                        (sys.int::valid-funcall-function-p (first args)))
                   (setf name (sys.int::funcall-function-name (first args))
                         args (rest args)))
                  (t
                   (return))))
         (dolist (arg args)
           (x-compile-for-value arg env))
         (sys.int::add-to-llf sys.int::+llf-funcall-n+ name (length args)))))))

(defun cross-compile-file (input-file
                           &key
                             (output-file (make-pathname :type "llf" :defaults input-file))
                             (verbose *compile-verbose*)
                             (print *compile-print*)
                             (external-format :default)
                             package)
  (with-open-file (input-stream input-file :external-format external-format)
    (when verbose
      (format t ";; Cross-compiling ~S~%" input-file))
    (let* ((start-time (get-internal-run-time))
           (*package* (or (find-package (or package "CROSS-CL-USER"))
                          (error "Unknown package ~S" package)))
           (*readtable* (copy-readtable *readtable*))
           (cl:*features* *features*)
           (*compile-verbose* verbose)
           (*compile-print* print)
           (sys.int::*compile-parallel* nil)
           (sys.int::*llf-forms* nil)
           (omap (make-hash-table))
           (eof-marker (cons nil nil))
           (*compile-file-pathname* (pathname (merge-pathnames input-file)))
           (*compile-file-truename* (truename *compile-file-pathname*))
           (sys.int::*top-level-form-number* 0)
           (mezzano.compiler::*load-time-value-hook* 'sys.int::compile-file-load-time-value)
           ;; Don't persist optimize proclaimations outside COMPILE-FILE.
           (mezzano.compiler::*optimize-policy* (copy-list mezzano.compiler::*optimize-policy*))
           (*gensym-counter* 0)
           (sys.int::*fixup-table* (make-hash-table))
           (location-stream (make-instance 'sys.int::location-tracking-stream
                                           :stream input-stream
                                           :namestring (ignore-errors (namestring *compile-file-pathname*))))
           (*output-fasl* t)
           (sys.int::*llf-architecture* *target-architecture*))
      (sys.int::with-reader-location-tracking
        (loop
          for form = (read location-stream nil eof-marker)
          until (eql form eof-marker)
          do
             (when *compile-print*
               (let ((*print-length* 3)
                     (*print-level* 2))
                 (format t ";; X-compiling: ~S~%" form)))
             (x-compile-top-level form nil)
             (incf sys.int::*top-level-form-number*)))
      ;; Now write everything to the fasl.
      ;; Do two passes to detect circularity.
      (let ((commands (reverse sys.int::*llf-forms*)))
        (let ((sys.int::*llf-dry-run* t))
          (dolist (cmd commands)
            (dolist (o (cdr cmd))
              (sys.int::save-object o omap (make-broadcast-stream)))))
        (with-open-file (output-stream output-file
                                       :element-type '(unsigned-byte 8)
                                       :if-exists :supersede
                                       :direction :output)
          (sys.int::write-llf-header output-stream input-file)
          (let ((sys.int::*llf-dry-run* nil))
            (dolist (cmd commands)
              (dolist (o (cdr cmd))
                (sys.int::save-object o omap output-stream))
              (when (car cmd)
                (write-byte (car cmd) output-stream))))
          (write-byte sys.int::+llf-end-of-load+ output-stream)
          (when *compile-print*
            (format t ";; Cross-compile-file took ~D seconds.~%"
                    (float (/ (- (get-internal-run-time) start-time)
                              internal-time-units-per-second))))
          (values (truename output-stream) nil nil))))))

(defun load-for-cross-compiler (input-file &key
                           (verbose *compile-verbose*)
                           (print *compile-print*)
                           (external-format :default))
  (with-open-file (input input-file :external-format external-format)
    (let* ((*readtable* (copy-readtable *readtable*))
           (*package* (find-package "CROSS-CL-USER"))
           (*compile-print* print)
           (*compile-verbose* verbose)
           (*compile-file-pathname* (pathname (merge-pathnames input-file)))
           (*compile-file-truename* (truename *compile-file-pathname*))
           (*output-fasl* nil)
           (*gensym-counter* 0))
      (when *compile-verbose*
        (format t ";; Cross-loading ~S~%" input-file))
      (loop for form = (read input nil input) do
           (when (eql form input)
             (return))
           (when *compile-print*
             (let ((*print-length* 3)
                   (*print-level* 2))
               (format t ";; X-loading: ~S~%" form)))
           (x-compile-top-level form nil :not-compile-time))))
  t)

(defun save-custom-compiled-file (path generator)
  (let* ((*readtable* (copy-readtable *readtable*))
         (*package* (find-package "CROSS-CL-USER"))
         (*compile-print* *compile-print*)
         (*compile-verbose* *compile-verbose*)
         (*compile-file-pathname* nil)
         (*compile-file-truename* nil)
         (sys.int::*compile-parallel* nil)
         (sys.int::*llf-forms* nil)
         (omap (make-hash-table))
         (*gensym-counter* 0)
         (sys.int::*top-level-form-number* 0)
         (mezzano.compiler::*load-time-value-hook* 'sys.int::compile-file-load-time-value)
         ;; Don't persist optimize proclaimations outside COMPILE-FILE.
         (mezzano.compiler::*optimize-policy* (copy-list mezzano.compiler::*optimize-policy*))
         (sys.int::*fixup-table* (make-hash-table))
         (*output-fasl* t)
         (sys.int::*llf-architecture* *target-architecture*))
    (loop
      for values = (multiple-value-list (funcall generator))
      for form = (first values)
      do
         (when (not values)
           (return))
         (when *compile-print*
           (let ((*print-length* 3)
                 (*print-level* 2))
             (format t ";; X-compiling: ~S~%" form)))
         (x-compile-top-level form nil))
    ;; Now write everything to the fasl.
    ;; Do two passes to detect circularity.
    (let ((commands (reverse sys.int::*llf-forms*)))
      (let ((sys.int::*llf-dry-run* t))
        (dolist (cmd commands)
          (dolist (o (cdr cmd))
            (sys.int::save-object o omap (make-broadcast-stream)))))
      (with-open-file (output-stream path
                                     :element-type '(unsigned-byte 8)
                                     :if-exists :supersede
                                     :direction :output)
        (sys.int::write-llf-header output-stream path)
        (let ((sys.int::*llf-dry-run* nil))
          (dolist (cmd commands)
            (dolist (o (cdr cmd))
              (sys.int::save-object o omap output-stream))
            (when (car cmd)
              (write-byte (car cmd) output-stream))))
        (write-byte sys.int::+llf-end-of-load+ output-stream)
        (values (truename output-stream) nil nil)))))

(defun save-compiler-builtins (path target-architecture)
  (format t ";; Writing compiler builtins to ~A.~%" path)
  (let* ((builtins (ecase target-architecture
                     (:x86-64 (mezzano.compiler.backend.x86-64::generate-builtin-functions))
                     (:arm64 (mezzano.compiler.backend.arm64::generate-builtin-functions))))
         (*target-architecture* target-architecture))
    (save-custom-compiled-file path
                               (lambda ()
                                 (if builtins
                                     (let ((b (pop builtins)))
                                       `(sys.int::%defun ',(first b) ,(second b)))
                                     (values))))))

(defmethod sys.int::save-one-object ((object sys.int::cross-function) omap stream)
  (let ((constants (sys.int::cross-function-constants object)))
    (dotimes (i (length constants))
      (sys.int::save-object (aref constants i) omap stream))
    (sys.int::save-object (sys.int::cross-function-fixups object) omap stream)
    (write-byte sys.int::+llf-function+ stream)
    (write-byte sys.int::+object-tag-function+ stream) ; tag, normal function.
    (sys.int::save-integer (length (sys.int::cross-function-mc object)) stream)
    (sys.int::save-integer (length constants) stream)
    (sys.int::save-integer (length (sys.int::cross-function-gc-info object)) stream)
    (write-sequence (sys.int::cross-function-mc object) stream)
    (write-sequence (sys.int::cross-function-gc-info object) stream)))

(defmethod sys.int::save-one-object ((object cross-support::cross-short-float) omap stream)
  (write-byte sys.int::+llf-short-float+ stream)
  (sys.int::save-integer (sys.int::%short-float-as-integer object) stream))

(defmethod sys.int::save-one-object ((object cross-support::cross-complex-short-float) omap stream)
  (write-byte sys.int::+llf-complex-short-float+ stream)
  (sys.int::save-integer (sys.int::%short-float-as-integer (cross-support::cross-complex-short-float-realpart object)) stream)
  (sys.int::save-integer (sys.int::%short-float-as-integer (cross-support::cross-complex-short-float-imagpart object)) stream))

;; Override the normal save-one-object method specifically so that the cross-cl package
;; gets translated properly.
(defmethod sys.int::save-one-object ((object symbol) omap stream)
  (cond ((symbol-package object)
         (write-byte sys.int::+llf-symbol+ stream)
         (sys.int::save-integer (length (symbol-name object)) stream)
         (dotimes (i (length (symbol-name object)))
           (sys.int::save-character (char (symbol-name object) i) stream))
         (let ((package (symbol-package object)))
           (when (eql package (find-package :cross-cl))
             (setf package (find-package :cl)))
           (sys.int::save-integer (length (package-name package)) stream)
           (dotimes (i (length (package-name package)))
             (sys.int::save-character (char (package-name package) i) stream))))
        (t
         (sys.int::save-object (symbol-name object) omap stream)
         (write-byte sys.int::+llf-uninterned-symbol+ stream))))

(defmethod sys.int::save-one-object ((object mezzano.runtime::inline-cache) omap stream)
  (sys.int::save-object (cross-support::inline-cache-proxy-name object) omap stream)
  (write-byte sys.int::+llf-inline-cache+ stream))
