;;;; -------------------------------------------------------------------------
;;;; Support to build (compile and load) Lisp files

(uiop/package:define-package :uiop/lisp-build
  (:nicknames :asdf/lisp-build) ;; OBSOLETE, used by slime/contrib/swank-asdf.lisp
  (:use :uiop/common-lisp :uiop/package :uiop/utility
   :uiop/os :uiop/pathname :uiop/filesystem :uiop/stream :uiop/image)
  (:export
   ;; Variables
   #:*compile-file-warnings-behaviour* #:*compile-file-failure-behaviour*
   #:*output-translation-function*
   #:*optimization-settings* #:*previous-optimization-settings*
   #:*base-build-directory*
   #:compile-condition #:compile-file-error #:compile-warned-error #:compile-failed-error
   #:compile-warned-warning #:compile-failed-warning
   #:check-lisp-compile-results #:check-lisp-compile-warnings
   #:*uninteresting-conditions* #:*usual-uninteresting-conditions*
   #:*uninteresting-compiler-conditions* #:*uninteresting-loader-conditions*
   ;; Types
   #+sbcl #:sb-grovel-unknown-constant-condition
   ;; Functions & Macros
   #:get-optimization-settings #:proclaim-optimization-settings #:with-optimization-settings
   #:call-with-muffled-compiler-conditions #:with-muffled-compiler-conditions
   #:call-with-muffled-loader-conditions #:with-muffled-loader-conditions
   #:reify-simple-sexp #:unreify-simple-sexp
   #:reify-deferred-warnings #:unreify-deferred-warnings
   #:reset-deferred-warnings #:save-deferred-warnings #:check-deferred-warnings
   #:with-saved-deferred-warnings #:warnings-file-p #:warnings-file-type #:*warnings-file-type*
   #:enable-deferred-warnings-check #:disable-deferred-warnings-check
   #:current-lisp-file-pathname #:load-pathname
   #:lispize-pathname #:compile-file-type #:call-around-hook
   #:compile-file* #:compile-file-pathname* #:*compile-check*
   #:load* #:load-from-string #:combine-fasls)
  (:intern #:defaults #:failure-p #:warnings-p #:s #:y #:body))
(in-package :uiop/lisp-build)

(with-upgradability ()
  (defvar *compile-file-warnings-behaviour*
    (or #+clisp :ignore :warn)
    "How should ASDF react if it encounters a warning when compiling a file?
Valid values are :error, :warn, and :ignore.")

  (defvar *compile-file-failure-behaviour*
    (or #+(or mkcl sbcl) :error #+clisp :ignore :warn)
    "How should ASDF react if it encounters a failure (per the ANSI spec of COMPILE-FILE)
when compiling a file, which includes any non-style-warning warning.
Valid values are :error, :warn, and :ignore.
Note that ASDF ALWAYS raises an error if it fails to create an output file when compiling.")

  (defvar *base-build-directory* nil
    "When set to a non-null value, it should be an absolute directory pathname,
which will serve as the *DEFAULT-PATHNAME-DEFAULTS* around a COMPILE-FILE,
what more while the input-file is shortened if possible to ENOUGH-PATHNAME relative to it.
This can help you produce more deterministic output for FASLs."))

;;; Optimization settings
(with-upgradability ()
  (defvar *optimization-settings* nil
    "Optimization settings to be used by PROCLAIM-OPTIMIZATION-SETTINGS")
  (defvar *previous-optimization-settings* nil
    "Optimization settings saved by PROCLAIM-OPTIMIZATION-SETTINGS")
  (defparameter +optimization-variables+
    ;; TODO: allegro genera corman mcl
    (or #+(or abcl xcl) '(system::*speed* system::*space* system::*safety* system::*debug*)
        #+clisp '() ;; system::*optimize* is a constant hash-table! (with non-constant contents)
        #+clozure '(ccl::*nx-speed* ccl::*nx-space* ccl::*nx-safety*
                    ccl::*nx-debug* ccl::*nx-cspeed*)
        #+(or cmucl scl) '(c::*default-cookie*)
        #+clasp '()
        #+ecl (unless (use-ecl-byte-compiler-p) '(c::*speed* c::*space* c::*safety* c::*debug*))
        #+gcl '(compiler::*speed* compiler::*space* compiler::*compiler-new-safety* compiler::*debug*)
        #+lispworks '(compiler::*optimization-level*)
        #+mkcl '(si::*speed* si::*space* si::*safety* si::*debug*)
        #+sbcl '(sb-c::*policy*)))
  (defun get-optimization-settings ()
    "Get current compiler optimization settings, ready to PROCLAIM again"
    #-(or abcl allegro clasp clisp clozure cmucl ecl lispworks mkcl sbcl scl xcl)
    (warn "~S does not support ~S. Please help me fix that."
          'get-optimization-settings (implementation-type))
    #+(or abcl allegro clasp clisp clozure cmucl ecl lispworks mkcl sbcl scl xcl)
    (let ((settings '(speed space safety debug compilation-speed #+(or cmucl scl) c::brevity)))
      #.`(loop #+(or allegro clozure)
               ,@'(:with info = #+allegro (sys:declaration-information 'optimize)
                   #+clozure (ccl:declaration-information 'optimize nil))
               :for x :in settings
               ,@(or #+(or abcl clasp ecl gcl mkcl xcl) '(:for v :in +optimization-variables+))
               :for y = (or #+(or allegro clozure) (second (assoc x info)) ; normalize order
                            #+clisp (gethash x system::*optimize* 1)
                            #+(or abcl clasp ecl mkcl xcl) (symbol-value v)
                            #+(or cmucl scl) (slot-value c::*default-cookie*
                                                       (case x (compilation-speed 'c::cspeed)
                                                             (otherwise x)))
                            #+lispworks (slot-value compiler::*optimization-level* x)
                            #+sbcl (sb-c::policy-quality sb-c::*policy* x))
               :when y :collect (list x y))))
  (defun proclaim-optimization-settings ()
    "Proclaim the optimization settings in *OPTIMIZATION-SETTINGS*"
    (proclaim `(optimize ,@*optimization-settings*))
    (let ((settings (get-optimization-settings)))
      (unless (equal *previous-optimization-settings* settings)
        (setf *previous-optimization-settings* settings))))
  (defmacro with-optimization-settings ((&optional (settings *optimization-settings*)) &body body)
    #+(or allegro clisp)
    (let ((previous-settings (gensym "PREVIOUS-SETTINGS")))
      `(let ((,previous-settings (get-optimization-settings)))
         ,@(when settings `((proclaim `(optimize ,@,settings))))
         (unwind-protect (progn ,@body)
           (proclaim `(optimize ,@,previous-settings)))))
    #-(or allegro clisp)
    `(let ,(loop :for v :in +optimization-variables+ :collect `(,v ,v))
       ,@(when settings `((proclaim `(optimize ,@,settings))))
       ,@body)))


;;; Condition control
(with-upgradability ()
  #+sbcl
  (progn
    (defun sb-grovel-unknown-constant-condition-p (c)
      "Detect SB-GROVEL unknown-constant conditions on older versions of SBCL"
      (and (typep c 'sb-int:simple-style-warning)
           (string-enclosed-p
            "Couldn't grovel for "
            (simple-condition-format-control c)
            " (unknown to the C compiler).")))
    (deftype sb-grovel-unknown-constant-condition ()
      '(and style-warning (satisfies sb-grovel-unknown-constant-condition-p))))

  (defvar *usual-uninteresting-conditions*
    (append
     ;;#+clozure '(ccl:compiler-warning)
     #+cmucl '("Deleting unreachable code.")
     #+lispworks '("~S being redefined in ~A (previously in ~A)."
                   "~S defined more than once in ~A.") ;; lispworks gets confused by eval-when.
     #+sbcl
     '(sb-c::simple-compiler-note
       "&OPTIONAL and &KEY found in the same lambda list: ~S"
       #+sb-eval sb-kernel:lexical-environment-too-complex
       sb-kernel:undefined-alien-style-warning
       sb-grovel-unknown-constant-condition ; defined above.
       sb-ext:implicit-generic-function-warning ;; Controversial.
       sb-int:package-at-variance
       sb-kernel:uninteresting-redefinition
       ;; BEWARE: the below four are controversial to include here.
       sb-kernel:redefinition-with-defun
       sb-kernel:redefinition-with-defgeneric
       sb-kernel:redefinition-with-defmethod
       sb-kernel::redefinition-with-defmacro) ; not exported by old SBCLs
     '("No generic function ~S present when encountering macroexpansion of defmethod. Assuming it will be an instance of standard-generic-function.")) ;; from closer2mop
    "A suggested value to which to set or bind *uninteresting-conditions*.")

  (defvar *uninteresting-conditions* '()
    "Conditions that may be skipped while compiling or loading Lisp code.")
  (defvar *uninteresting-compiler-conditions* '()
    "Additional conditions that may be skipped while compiling Lisp code.")
  (defvar *uninteresting-loader-conditions*
    (append
     '("Overwriting already existing readtable ~S." ;; from named-readtables
       #(#:finalizers-off-warning :asdf-finalizers)) ;; from asdf-finalizers
     #+clisp '(clos::simple-gf-replacing-method-warning))
    "Additional conditions that may be skipped while loading Lisp code."))

;;;; ----- Filtering conditions while building -----
(with-upgradability ()
  (defun call-with-muffled-compiler-conditions (thunk)
    "Call given THUNK in a context where uninteresting conditions and compiler conditions are muffled"
    (call-with-muffled-conditions
     thunk (append *uninteresting-conditions* *uninteresting-compiler-conditions*)))
  (defmacro with-muffled-compiler-conditions ((&optional) &body body)
    "Trivial syntax for CALL-WITH-MUFFLED-COMPILER-CONDITIONS"
    `(call-with-muffled-compiler-conditions #'(lambda () ,@body)))
  (defun call-with-muffled-loader-conditions (thunk)
    "Call given THUNK in a context where uninteresting conditions and loader conditions are muffled"
    (call-with-muffled-conditions
     thunk (append *uninteresting-conditions* *uninteresting-loader-conditions*)))
  (defmacro with-muffled-loader-conditions ((&optional) &body body)
    "Trivial syntax for CALL-WITH-MUFFLED-LOADER-CONDITIONS"
    `(call-with-muffled-loader-conditions #'(lambda () ,@body))))


;;;; Handle warnings and failures
(with-upgradability ()
  (define-condition compile-condition (condition)
    ((context-format
      :initform nil :reader compile-condition-context-format :initarg :context-format)
     (context-arguments
      :initform nil :reader compile-condition-context-arguments :initarg :context-arguments)
     (description
      :initform nil :reader compile-condition-description :initarg :description))
    (:report (lambda (c s)
               (format s (compatfmt "~@<~A~@[ while ~?~]~@:>")
                       (or (compile-condition-description c) (type-of c))
                       (compile-condition-context-format c)
                       (compile-condition-context-arguments c)))))
  (define-condition compile-file-error (compile-condition error) ())
  (define-condition compile-warned-warning (compile-condition warning) ())
  (define-condition compile-warned-error (compile-condition error) ())
  (define-condition compile-failed-warning (compile-condition warning) ())
  (define-condition compile-failed-error (compile-condition error) ())

  (defun check-lisp-compile-warnings (warnings-p failure-p
                                                  &optional context-format context-arguments)
    "Given the warnings or failures as resulted from COMPILE-FILE or checking deferred warnings,
raise an error or warning as appropriate"
    (when failure-p
      (case *compile-file-failure-behaviour*
        (:warn (warn 'compile-failed-warning
                     :description "Lisp compilation failed"
                     :context-format context-format
                     :context-arguments context-arguments))
        (:error (error 'compile-failed-error
                       :description "Lisp compilation failed"
                       :context-format context-format
                       :context-arguments context-arguments))
        (:ignore nil)))
    (when warnings-p
      (case *compile-file-warnings-behaviour*
        (:warn (warn 'compile-warned-warning
                     :description "Lisp compilation had style-warnings"
                     :context-format context-format
                     :context-arguments context-arguments))
        (:error (error 'compile-warned-error
                       :description "Lisp compilation had style-warnings"
                       :context-format context-format
                       :context-arguments context-arguments))
        (:ignore nil))))

  (defun check-lisp-compile-results (output warnings-p failure-p
                                             &optional context-format context-arguments)
    "Given the results of COMPILE-FILE, raise an error or warning as appropriate"
    (unless output
      (error 'compile-file-error :context-format context-format :context-arguments context-arguments))
    (check-lisp-compile-warnings warnings-p failure-p context-format context-arguments)))


;;;; Deferred-warnings treatment, originally implemented by Douglas Katzman.
;;;
;;; To support an implementation, three functions must be implemented:
;;; reify-deferred-warnings unreify-deferred-warnings reset-deferred-warnings
;;; See their respective docstrings.
(with-upgradability ()
  (defun reify-simple-sexp (sexp)
    "Given a simple SEXP, return a representation of it as a portable SEXP.
Simple means made of symbols, numbers, characters, simple-strings, pathnames, cons cells."
    (etypecase sexp
      (symbol (reify-symbol sexp))
      ((or number character simple-string pathname) sexp)
      (cons (cons (reify-simple-sexp (car sexp)) (reify-simple-sexp (cdr sexp))))
      (simple-vector (vector (mapcar 'reify-simple-sexp (coerce sexp 'list))))))

  (defun unreify-simple-sexp (sexp)
    "Given the portable output of REIFY-SIMPLE-SEXP, return the simple SEXP it represents"
    (etypecase sexp
      ((or symbol number character simple-string pathname) sexp)
      (cons (cons (unreify-simple-sexp (car sexp)) (unreify-simple-sexp (cdr sexp))))
      ((simple-vector 2) (unreify-symbol sexp))
      ((simple-vector 1) (coerce (mapcar 'unreify-simple-sexp (aref sexp 0)) 'vector))))

  #+clozure
  (progn
    (defun reify-source-note (source-note)
      (when source-note
        (with-accessors ((source ccl::source-note-source) (filename ccl:source-note-filename)
                         (start-pos ccl:source-note-start-pos) (end-pos ccl:source-note-end-pos)) source-note
          (declare (ignorable source))
          (list :filename filename :start-pos start-pos :end-pos end-pos
                #|:source (reify-source-note source)|#))))
    (defun unreify-source-note (source-note)
      (when source-note
        (destructuring-bind (&key filename start-pos end-pos source) source-note
          (ccl::make-source-note :filename filename :start-pos start-pos :end-pos end-pos
                                 :source (unreify-source-note source)))))
    (defun unsymbolify-function-name (name)
      (if-let (setfed (gethash name ccl::%setf-function-name-inverses%))
        `(setf ,setfed)
        name))
    (defun symbolify-function-name (name)
      (if (and (consp name) (eq (first name) 'setf))
          (let ((setfed (second name)))
            (gethash setfed ccl::%setf-function-names%))
          name))
    (defun reify-function-name (function-name)
      (let ((name (or (first function-name) ;; defun: extract the name
                      (let ((sec (second function-name)))
                        (or (and (atom sec) sec) ; scoped method: drop scope
                            (first sec)))))) ; method: keep gf name, drop method specializers
        (list name)))
    (defun unreify-function-name (function-name)
      function-name)
    (defun nullify-non-literals (sexp)
      (typecase sexp
        ((or number character simple-string symbol pathname) sexp)
        (cons (cons (nullify-non-literals (car sexp))
                    (nullify-non-literals (cdr sexp))))
        (t nil)))
    (defun reify-deferred-warning (deferred-warning)
      (with-accessors ((warning-type ccl::compiler-warning-warning-type)
                       (args ccl::compiler-warning-args)
                       (source-note ccl:compiler-warning-source-note)
                       (function-name ccl:compiler-warning-function-name)) deferred-warning
        (list :warning-type warning-type :function-name (reify-function-name function-name)
              :source-note (reify-source-note source-note)
              :args (destructuring-bind (fun &rest more)
                        args
                      (cons (unsymbolify-function-name fun)
                            (nullify-non-literals more))))))
    (defun unreify-deferred-warning (reified-deferred-warning)
      (destructuring-bind (&key warning-type function-name source-note args)
          reified-deferred-warning
        (make-condition (or (cdr (ccl::assq warning-type ccl::*compiler-whining-conditions*))
                            'ccl::compiler-warning)
                        :function-name (unreify-function-name function-name)
                        :source-note (unreify-source-note source-note)
                        :warning-type warning-type
                        :args (destructuring-bind (fun . more) args
                                (cons (symbolify-function-name fun) more))))))
  #+(or cmucl scl)
  (defun reify-undefined-warning (warning)
    ;; Extracting undefined-warnings from the compilation-unit
    ;; To be passed through the above reify/unreify link, it must be a "simple-sexp"
    (list*
     (c::undefined-warning-kind warning)
     (c::undefined-warning-name warning)
     (c::undefined-warning-count warning)
     (mapcar
      #'(lambda (frob)
          ;; the lexenv slot can be ignored for reporting purposes
          `(:enclosing-source ,(c::compiler-error-context-enclosing-source frob)
            :source ,(c::compiler-error-context-source frob)
            :original-source ,(c::compiler-error-context-original-source frob)
            :context ,(c::compiler-error-context-context frob)
            :file-name ,(c::compiler-error-context-file-name frob) ; a pathname
            :file-position ,(c::compiler-error-context-file-position frob) ; an integer
            :original-source-path ,(c::compiler-error-context-original-source-path frob)))
      (c::undefined-warning-warnings warning))))

  #+sbcl
  (defun reify-undefined-warning (warning)
    ;; Extracting undefined-warnings from the compilation-unit
    ;; To be passed through the above reify/unreify link, it must be a "simple-sexp"
    (list*
     (sb-c::undefined-warning-kind warning)
     (sb-c::undefined-warning-name warning)
     (sb-c::undefined-warning-count warning)
     ;; the COMPILER-ERROR-CONTEXT struct has changed in SBCL, which means how we
     ;; handle deferred warnings must change... TODO: when enough time has
     ;; gone by, just assume all versions of SBCL are adequately
     ;; up-to-date, and cut this material.[2018/05/30:rpg]
     (mapcar
      #'(lambda (frob)
          ;; the lexenv slot can be ignored for reporting purposes
          `(
            #- #.(uiop/utility:symbol-test-to-feature-expression '#:compiler-error-context-%source '#:sb-c)
            ,@`(:enclosing-source
                ,(sb-c::compiler-error-context-enclosing-source frob)
                :source
                ,(sb-c::compiler-error-context-source frob)
                :original-source
                ,(sb-c::compiler-error-context-original-source frob))
            #+ #.(uiop/utility:symbol-test-to-feature-expression '#:compiler-error-context-%source '#:sb-c)
            ,@ `(:%enclosing-source
                 ,(sb-c::compiler-error-context-enclosing-source frob)
                 :%source
                 ,(sb-c::compiler-error-context-source frob)
                 :original-form
                 ,(sb-c::compiler-error-context-original-form frob))
            :context ,(sb-c::compiler-error-context-context frob)
            :file-name ,(sb-c::compiler-error-context-file-name frob) ; a pathname
            :file-position ,(sb-c::compiler-error-context-file-position frob) ; an integer
            :original-source-path ,(sb-c::compiler-error-context-original-source-path frob)))
      (sb-c::undefined-warning-warnings warning))))

  (defun reify-deferred-warnings ()
    "return a portable S-expression, portably readable and writeable in any Common Lisp implementation
using READ within a WITH-SAFE-IO-SYNTAX, that represents the warnings currently deferred by
WITH-COMPILATION-UNIT. One of three functions required for deferred-warnings support in ASDF."
    #+allegro
    (list :functions-defined excl::.functions-defined.
          :functions-called excl::.functions-called.)
    #+clozure
    (mapcar 'reify-deferred-warning
            (if-let (dw ccl::*outstanding-deferred-warnings*)
              (let ((mdw (ccl::ensure-merged-deferred-warnings dw)))
                (ccl::deferred-warnings.warnings mdw))))
    #+(or cmucl scl)
    (when lisp::*in-compilation-unit*
      ;; Try to send nothing through the pipe if nothing needs to be accumulated
      `(,@(when c::*undefined-warnings*
            `((c::*undefined-warnings*
               ,@(mapcar #'reify-undefined-warning c::*undefined-warnings*))))
        ,@(loop :for what :in '(c::*compiler-error-count*
                                c::*compiler-warning-count*
                                c::*compiler-note-count*)
                :for value = (symbol-value what)
                :when (plusp value)
                  :collect `(,what . ,value))))
    #+sbcl
    (when sb-c::*in-compilation-unit*
      ;; Try to send nothing through the pipe if nothing needs to be accumulated
      `(,@(when sb-c::*undefined-warnings*
            `((sb-c::*undefined-warnings*
               ,@(mapcar #'reify-undefined-warning sb-c::*undefined-warnings*))))
        ,@(loop :for what :in '(sb-c::*aborted-compilation-unit-count*
                                sb-c::*compiler-error-count*
                                sb-c::*compiler-warning-count*
                                sb-c::*compiler-style-warning-count*
                                sb-c::*compiler-note-count*)
                :for value = (symbol-value what)
                :when (plusp value)
                  :collect `(,what . ,value)))))

  (defun unreify-deferred-warnings (reified-deferred-warnings)
    "given a S-expression created by REIFY-DEFERRED-WARNINGS, reinstantiate the corresponding
deferred warnings as to be handled at the end of the current WITH-COMPILATION-UNIT.
Handle any warning that has been resolved already,
such as an undefined function that has been defined since.
One of three functions required for deferred-warnings support in ASDF."
    (declare (ignorable reified-deferred-warnings))
    #+allegro
    (destructuring-bind (&key functions-defined functions-called)
        reified-deferred-warnings
      (setf excl::.functions-defined.
            (append functions-defined excl::.functions-defined.)
            excl::.functions-called.
            (append functions-called excl::.functions-called.)))
    #+clozure
    (let ((dw (or ccl::*outstanding-deferred-warnings*
                  (setf ccl::*outstanding-deferred-warnings* (ccl::%defer-warnings t)))))
      (appendf (ccl::deferred-warnings.warnings dw)
               (mapcar 'unreify-deferred-warning reified-deferred-warnings)))
    #+(or cmucl scl)
    (dolist (item reified-deferred-warnings)
      ;; Each item is (symbol . adjustment) where the adjustment depends on the symbol.
      ;; For *undefined-warnings*, the adjustment is a list of initargs.
      ;; For everything else, it's an integer.
      (destructuring-bind (symbol . adjustment) item
        (case symbol
          ((c::*undefined-warnings*)
           (setf c::*undefined-warnings*
                 (nconc (mapcan
                         #'(lambda (stuff)
                             (destructuring-bind (kind name count . rest) stuff
                               (unless (case kind (:function (fboundp name)))
                                 (list
                                  (c::make-undefined-warning
                                   :name name
                                   :kind kind
                                   :count count
                                   :warnings
                                   (mapcar #'(lambda (x)
                                               (apply #'c::make-compiler-error-context x))
                                           rest))))))
                         adjustment)
                        c::*undefined-warnings*)))
          (otherwise
           (set symbol (+ (symbol-value symbol) adjustment))))))
    #+sbcl
    (dolist (item reified-deferred-warnings)
      ;; Each item is (symbol . adjustment) where the adjustment depends on the symbol.
      ;; For *undefined-warnings*, the adjustment is a list of initargs.
      ;; For everything else, it's an integer.
      (destructuring-bind (symbol . adjustment) item
        (case symbol
          ((sb-c::*undefined-warnings*)
           (setf sb-c::*undefined-warnings*
                 (nconc (mapcan
                         #'(lambda (stuff)
                             (destructuring-bind (kind name count . rest) stuff
                               (unless (case kind (:function (fboundp name)))
                                 (list
                                  (sb-c::make-undefined-warning
                                   :name name
                                   :kind kind
                                   :count count
                                   :warnings
                                   (mapcar #'(lambda (x)
                                               (apply #'sb-c::make-compiler-error-context x))
                                           rest))))))
                         adjustment)
                        sb-c::*undefined-warnings*)))
          (otherwise
           (set symbol (+ (symbol-value symbol) adjustment)))))))

  (defun reset-deferred-warnings ()
    "Reset the set of deferred warnings to be handled at the end of the current WITH-COMPILATION-UNIT.
One of three functions required for deferred-warnings support in ASDF."
    #+allegro
    (setf excl::.functions-defined. nil
          excl::.functions-called. nil)
    #+clozure
    (if-let (dw ccl::*outstanding-deferred-warnings*)
      (let ((mdw (ccl::ensure-merged-deferred-warnings dw)))
        (setf (ccl::deferred-warnings.warnings mdw) nil)))
    #+(or cmucl scl)
    (when lisp::*in-compilation-unit*
      (setf c::*undefined-warnings* nil
            c::*compiler-error-count* 0
            c::*compiler-warning-count* 0
            c::*compiler-note-count* 0))
    #+sbcl
    (when sb-c::*in-compilation-unit*
      (setf sb-c::*undefined-warnings* nil
            sb-c::*aborted-compilation-unit-count* 0
            sb-c::*compiler-error-count* 0
            sb-c::*compiler-warning-count* 0
            sb-c::*compiler-style-warning-count* 0
            sb-c::*compiler-note-count* 0)))

  (defun save-deferred-warnings (warnings-file)
    "Save forward reference conditions so they may be issued at a latter time,
possibly in a different process."
    (with-open-file (s warnings-file :direction :output :if-exists :supersede
                       :element-type *default-stream-element-type*
                       :external-format *utf-8-external-format*)
      (with-safe-io-syntax ()
        (let ((*read-eval* t))
          (write (reify-deferred-warnings) :stream s :pretty t :readably t))
        (terpri s))))

  (defun warnings-file-type (&optional implementation-type)
    "The pathname type for warnings files on given IMPLEMENTATION-TYPE,
where NIL designates the current one"
    (case (or implementation-type *implementation-type*)
      ((:acl :allegro) "allegro-warnings")
      ;;((:clisp) "clisp-warnings")
      ((:cmu :cmucl) "cmucl-warnings")
      ((:sbcl) "sbcl-warnings")
      ((:clozure :ccl) "ccl-warnings")
      ((:scl) "scl-warnings")))

  (defvar *warnings-file-type* nil
    "Pathname type for warnings files, or NIL if disabled")

  (defun enable-deferred-warnings-check ()
    "Enable the saving of deferred warnings"
    (setf *warnings-file-type* (warnings-file-type)))

  (defun disable-deferred-warnings-check ()
    "Disable the saving of deferred warnings"
    (setf *warnings-file-type* nil))

  (defun warnings-file-p (file &optional implementation-type)
    "Is FILE a saved warnings file for the given IMPLEMENTATION-TYPE?
If that given type is NIL, use the currently configured *WARNINGS-FILE-TYPE* instead."
    (if-let (type (if implementation-type
                      (warnings-file-type implementation-type)
                      *warnings-file-type*))
      (equal (pathname-type file) type)))

  (defun check-deferred-warnings (files &optional context-format context-arguments)
    "Given a list of FILES containing deferred warnings saved by CALL-WITH-SAVED-DEFERRED-WARNINGS,
re-intern and raise any warnings that are still meaningful."
    (let ((file-errors nil)
          (failure-p nil)
          (warnings-p nil))
      (handler-bind
          ((warning #'(lambda (c)
                        (setf warnings-p t)
                        (unless (typep c 'style-warning)
                          (setf failure-p t)))))
        (with-compilation-unit (:override t)
          (reset-deferred-warnings)
          (dolist (file files)
            (unreify-deferred-warnings
             (handler-case
                 (with-safe-io-syntax ()
                   (let ((*read-eval* t))
                     (read-file-form file)))
               (error (c)
                 ;;(delete-file-if-exists file) ;; deleting forces rebuild but prevents debugging
                 (push c file-errors)
                 nil))))))
      (dolist (error file-errors) (error error))
      (check-lisp-compile-warnings
       (or failure-p warnings-p) failure-p context-format context-arguments)))

  #|
  Mini-guide to adding support for deferred warnings on an implementation.

  First, look at what such a warning looks like:

  (describe
  (handler-case
  (and (eval '(lambda () (some-undefined-function))) nil)
  (t (c) c)))

  Then you can grep for the condition type in your compiler sources
  and see how to catch those that have been deferred,
  and/or read, clear and restore the deferred list.

  Also look at
  (macroexpand-1 '(with-compilation-unit () foo))
  |#

  (defun call-with-saved-deferred-warnings (thunk warnings-file &key source-namestring)
    "If WARNINGS-FILE is not nil, record the deferred-warnings around a call to THUNK
and save those warnings to the given file for latter use,
possibly in a different process. Otherwise just call THUNK."
    (declare (ignorable source-namestring))
    (if warnings-file
        (with-compilation-unit (:override t #+sbcl :source-namestring #+sbcl source-namestring)
          (unwind-protect
               (let (#+sbcl (sb-c::*undefined-warnings* nil))
                 (multiple-value-prog1
                     (funcall thunk)
                   (save-deferred-warnings warnings-file)))
            (reset-deferred-warnings)))
        (funcall thunk)))

  (defmacro with-saved-deferred-warnings ((warnings-file &key source-namestring) &body body)
    "Trivial syntax for CALL-WITH-SAVED-DEFERRED-WARNINGS"
    `(call-with-saved-deferred-warnings
      #'(lambda () ,@body) ,warnings-file :source-namestring ,source-namestring)))


;;; from ASDF
(with-upgradability ()
  (defun current-lisp-file-pathname ()
    "Portably return the PATHNAME of the current Lisp source file being compiled or loaded"
    (or *compile-file-pathname* *load-pathname*))

  (defun load-pathname ()
    "Portably return the LOAD-PATHNAME of the current source file or fasl"
    *load-pathname*) ;; magic no longer needed for GCL.

  (defun lispize-pathname (input-file)
    "From a INPUT-FILE pathname, return a corresponding .lisp source pathname"
    (make-pathname :type "lisp" :defaults input-file))

  (defun compile-file-type (&rest keys)
    "pathname TYPE for lisp FASt Loading files"
    (declare (ignorable keys))
    #-(or clasp ecl mkcl) (load-time-value (pathname-type (compile-file-pathname "foo.lisp")))
    #+(or clasp ecl mkcl) (pathname-type (apply 'compile-file-pathname "foo" keys)))

  (defun call-around-hook (hook function)
    "Call a HOOK around the execution of FUNCTION"
    (call-function (or hook 'funcall) function))

  (defun compile-file-pathname* (input-file &rest keys &key output-file &allow-other-keys)
    "Variant of COMPILE-FILE-PATHNAME that works well with COMPILE-FILE*"
    (let* ((keys
             (remove-plist-keys `(#+(or (and allegro (not (version>= 8 2)))) :external-format
                                    ,@(unless output-file '(:output-file))) keys)))
      (if (absolute-pathname-p output-file)
          ;; what cfp should be doing, w/ mp* instead of mp
          (let* ((type (pathname-type (apply 'compile-file-type keys)))
                 (defaults (make-pathname
                            :type type :defaults (merge-pathnames* input-file))))
            (merge-pathnames* output-file defaults))
          (funcall *output-translation-function*
                   (apply 'compile-file-pathname input-file keys)))))

  (defvar *compile-check* nil
    "A hook for user-defined compile-time invariants")

  (defun* (compile-file*) (input-file &rest keys
                                      &key (compile-check *compile-check*) output-file warnings-file
                                      #+clisp lib-file #+(or clasp ecl mkcl) object-file #+sbcl emit-cfasl
                                      &allow-other-keys)
    "This function provides a portable wrapper around COMPILE-FILE.
It ensures that the OUTPUT-FILE value is only returned and
the file only actually created if the compilation was successful,
even though your implementation may not do that. It also checks an optional
user-provided consistency function COMPILE-CHECK to determine success;
it will call this function if not NIL at the end of the compilation
with the arguments sent to COMPILE-FILE*, except with :OUTPUT-FILE TMP-FILE
where TMP-FILE is the name of a temporary output-file.
It also checks two flags (with legacy british spelling from ASDF1),
*COMPILE-FILE-FAILURE-BEHAVIOUR* and *COMPILE-FILE-WARNINGS-BEHAVIOUR*
with appropriate implementation-dependent defaults,
and if a failure (respectively warnings) are reported by COMPILE-FILE,
it will consider that an error unless the respective behaviour flag
is one of :SUCCESS :WARN :IGNORE.
If WARNINGS-FILE is defined, deferred warnings are saved to that file.
On ECL or MKCL, it creates both the linkable object and loadable fasl files.
On implementations that erroneously do not recognize standard keyword arguments,
it will filter them appropriately."
    #+(or clasp ecl)
    (when (and object-file (equal (compile-file-type) (pathname object-file)))
      (format t "Whoa, some funky ASDF upgrade switched ~S calling convention for ~S and ~S~%"
              'compile-file* output-file object-file)
      (rotatef output-file object-file))
    (let* ((keywords (remove-plist-keys
                      `(:output-file :compile-check :warnings-file
                                     #+clisp :lib-file #+(or clasp ecl mkcl) :object-file) keys))
           (output-file
             (or output-file
                 (apply 'compile-file-pathname* input-file :output-file output-file keywords)))
           (physical-output-file (physicalize-pathname output-file))
           #+(or clasp ecl)
           (object-file
             (unless (use-ecl-byte-compiler-p)
               (or object-file
                   #+ecl (compile-file-pathname output-file :type :object)
                   #+clasp (compile-file-pathname output-file :output-type :object))))
           #+mkcl
           (object-file
             (or object-file
                 (compile-file-pathname output-file :fasl-p nil)))
           (tmp-file (tmpize-pathname physical-output-file))
           #+sbcl
           (cfasl-file (etypecase emit-cfasl
                         (null nil)
                         ((eql t) (make-pathname :type "cfasl" :defaults physical-output-file))
                         (string (parse-namestring emit-cfasl))
                         (pathname emit-cfasl)))
           #+sbcl
           (tmp-cfasl (when cfasl-file (make-pathname :type "cfasl" :defaults tmp-file)))
           #+clisp
           (tmp-lib (make-pathname :type "lib" :defaults tmp-file)))
      (multiple-value-bind (output-truename warnings-p failure-p)
          (with-enough-pathname (input-file :defaults *base-build-directory*)
            (with-saved-deferred-warnings (warnings-file :source-namestring (namestring input-file))
              (with-muffled-compiler-conditions ()
                (or #-(or clasp ecl mkcl)
                    (let (#+genera (si:*common-lisp-syntax-is-ansi-common-lisp* t))
                      (apply 'compile-file input-file :output-file tmp-file
                             #+sbcl (if emit-cfasl (list* :emit-cfasl tmp-cfasl keywords) keywords)
                             #-sbcl keywords))
                    #+ecl (apply 'compile-file input-file :output-file
                                (if object-file
                                    (list* object-file :system-p t keywords)
                                    (list* tmp-file keywords)))
                    #+clasp (apply 'compile-file input-file :output-file
                                  (if object-file
                                      (list* object-file :output-type :object #|:system-p t|# keywords)
                                      (list* tmp-file keywords)))
                    #+mkcl (apply 'compile-file input-file
                                  :output-file object-file :fasl-p nil keywords)))))
        (cond
          ((and output-truename
                (flet ((check-flag (flag behaviour)
                         (or (not flag) (member behaviour '(:success :warn :ignore)))))
                  (and (check-flag failure-p *compile-file-failure-behaviour*)
                       (check-flag warnings-p *compile-file-warnings-behaviour*)))
                (progn
                  #+(or clasp ecl mkcl)
                  (when (and #+(or clasp ecl) object-file)
                    (setf output-truename
                          (compiler::build-fasl tmp-file
                           #+(or clasp ecl) :lisp-files #+mkcl :lisp-object-files (list object-file))))
                  (or (not compile-check)
                      (apply compile-check input-file
                             :output-file output-truename
                             keywords))))
           (delete-file-if-exists physical-output-file)
           (when output-truename
             #+clasp (when output-truename (rename-file-overwriting-target tmp-file output-truename))
             ;; see CLISP bug 677
             #+clisp
             (progn
               (setf tmp-lib (make-pathname :type "lib" :defaults output-truename))
               (unless lib-file (setf lib-file (make-pathname :type "lib" :defaults physical-output-file)))
               (rename-file-overwriting-target tmp-lib lib-file))
             #+sbcl (when cfasl-file (rename-file-overwriting-target tmp-cfasl cfasl-file))
             (rename-file-overwriting-target output-truename physical-output-file)
             (setf output-truename (truename physical-output-file)))
           #+clasp (delete-file-if-exists tmp-file)
           #+clisp (progn (delete-file-if-exists tmp-file) ;; this one works around clisp BUG 677
                          (delete-file-if-exists tmp-lib))) ;; this one is "normal" defensive cleanup
          (t ;; error or failed check
           (delete-file-if-exists output-truename)
           #+clisp (delete-file-if-exists tmp-lib)
           #+sbcl (delete-file-if-exists tmp-cfasl)
           (setf output-truename nil)))
        (values output-truename warnings-p failure-p))))

  (defun load* (x &rest keys &key &allow-other-keys)
    "Portable wrapper around LOAD that properly handles loading from a stream."
    (with-muffled-loader-conditions ()
      (let (#+genera (si:*common-lisp-syntax-is-ansi-common-lisp* t))
        (etypecase x
          ((or pathname string #-(or allegro clozure genera) stream #+clozure file-stream)
           (apply 'load x keys))
          ;; Genera can't load from a string-input-stream
          ;; ClozureCL 1.6 can only load from file input stream
          ;; Allegro 5, I don't remember but it must have been broken when I tested.
          #+(or allegro clozure genera)
          (stream ;; make do this way
           (let ((*package* *package*)
                 (*readtable* *readtable*)
                 (*load-pathname* nil)
                 (*load-truename* nil))
             (eval-input x)))))))

  (defun load-from-string (string)
    "Portably read and evaluate forms from a STRING."
    (with-input-from-string (s string) (load* s))))

;;; Links FASLs together
(with-upgradability ()
  (defun combine-fasls (inputs output)
    "Combine a list of FASLs INPUTS into a single FASL OUTPUT"
    #-(or abcl allegro clisp clozure cmucl lispworks sbcl scl xcl)
    (not-implemented-error 'combine-fasls "~%inputs: ~S~%output: ~S" inputs output)
    #+abcl (funcall 'sys::concatenate-fasls inputs output) ; requires ABCL 1.2.0
    #+(or allegro clisp cmucl sbcl scl xcl) (concatenate-files inputs output)
    #+clozure (ccl:fasl-concatenate output inputs :if-exists :supersede)
    #+lispworks
    (let (fasls)
      (unwind-protect
           (progn
             (loop :for i :in inputs
                   :for n :from 1
                   :for f = (add-pathname-suffix
                             output (format nil "-FASL~D" n))
                   :do (copy-file i f)
                       (push f fasls))
             (ignore-errors (lispworks:delete-system :fasls-to-concatenate))
             (eval `(scm:defsystem :fasls-to-concatenate
                      (:default-pathname ,(pathname-directory-pathname output))
                      :members
                      ,(loop :for f :in (reverse fasls)
                             :collect `(,(namestring f) :load-only t))))
             (scm:concatenate-system output :fasls-to-concatenate :force t))
        (loop :for f :in fasls :do (ignore-errors (delete-file f)))
        (ignore-errors (lispworks:delete-system :fasls-to-concatenate))))))
