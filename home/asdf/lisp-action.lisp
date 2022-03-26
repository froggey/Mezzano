;;;; -------------------------------------------------------------------------
;;;; Actions to build Common Lisp software

(uiop/package:define-package :asdf/lisp-action
  (:recycle :asdf/lisp-action :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
   :asdf/component :asdf/system :asdf/operation :asdf/action)
  (:export
   #:try-recompiling
   #:cl-source-file #:cl-source-file.cl #:cl-source-file.lsp
   #:basic-load-op #:basic-compile-op
   #:load-op #:prepare-op #:compile-op #:test-op #:load-source-op #:prepare-source-op
   #:call-with-around-compile-hook
   #:perform-lisp-compilation #:perform-lisp-load-fasl #:perform-lisp-load-source
   #:lisp-compilation-output-files))
(in-package :asdf/lisp-action)


;;;; Component classes
(with-upgradability ()
  (defclass cl-source-file (source-file)
    ((type :initform "lisp"))
    (:documentation "Component class for a Common Lisp source file (using type \"lisp\")"))
  (defclass cl-source-file.cl (cl-source-file)
    ((type :initform "cl"))
    (:documentation "Component class for a Common Lisp source file using type \"cl\""))
  (defclass cl-source-file.lsp (cl-source-file)
    ((type :initform "lsp"))
    (:documentation "Component class for a Common Lisp source file using type \"lsp\"")))


;;;; Operation classes
(with-upgradability ()
  (defclass basic-load-op (operation) ()
    (:documentation "Base class for operations that apply the load-time effects of a file"))
  (defclass basic-compile-op (operation) ()
    (:documentation "Base class for operations that apply the compile-time effects of a file")))


;;; Our default operations: loading into the current lisp image
(with-upgradability ()
  (defclass prepare-op (upward-operation sideway-operation)
    ((sideway-operation :initform 'load-op :allocation :class))
    (:documentation "Load the dependencies for the COMPILE-OP or LOAD-OP of a given COMPONENT."))
  (defclass load-op (basic-load-op downward-operation selfward-operation)
    ;; NB: even though compile-op depends on prepare-op it is not needed-in-image-p,
    ;; so we need to directly depend on prepare-op for its side-effects in the current image.
    ((selfward-operation :initform '(prepare-op compile-op) :allocation :class))
    (:documentation "Operation for loading the compiled FASL for a Lisp file"))
  (defclass compile-op (basic-compile-op downward-operation selfward-operation)
    ((selfward-operation :initform 'prepare-op :allocation :class))
    (:documentation "Operation for compiling a Lisp file to a FASL"))


  (defclass prepare-source-op (upward-operation sideway-operation)
    ((sideway-operation :initform 'load-source-op :allocation :class))
    (:documentation "Operation for loading the dependencies of a Lisp file as source."))
  (defclass load-source-op (basic-load-op downward-operation selfward-operation)
    ((selfward-operation :initform 'prepare-source-op :allocation :class))
    (:documentation "Operation for loading a Lisp file as source."))

  (defclass test-op (selfward-operation)
    ((selfward-operation :initform 'load-op :allocation :class))
    (:documentation "Operation for running the tests for system.
If the tests fail, an error will be signaled.")))


;;;; Methods for prepare-op, compile-op and load-op

;;; prepare-op
(with-upgradability ()
  (defmethod action-description ((o prepare-op) (c component))
    (format nil (compatfmt "~@<loading dependencies of ~3i~_~A~@:>") c))
  (defmethod perform ((o prepare-op) (c component))
    nil)
  (defmethod input-files ((o prepare-op) (s system))
    (if-let (it (system-source-file s)) (list it))))

;;; compile-op
(with-upgradability ()
  (defmethod action-description ((o compile-op) (c component))
    (format nil (compatfmt "~@<compiling ~3i~_~A~@:>") c))
  (defmethod action-description ((o compile-op) (c parent-component))
    (format nil (compatfmt "~@<completing compilation for ~3i~_~A~@:>") c))
  (defgeneric call-with-around-compile-hook (component thunk)
    (:documentation "A method to be called around the PERFORM'ing of actions that apply the
compile-time side-effects of file (i.e., COMPILE-OP or LOAD-SOURCE-OP). This method can be used
to setup readtables and other variables that control reading, macroexpanding, and compiling, etc.
Note that it will NOT be called around the performing of LOAD-OP."))
  (defmethod call-with-around-compile-hook ((c component) function)
    (call-around-hook (around-compile-hook c) function))
  (defun perform-lisp-compilation (o c)
    "Perform the compilation of the Lisp file associated to the specified action (O . C)."
    (let (;; Before 2.26.53, that was unfortunately component-pathname. Now,
          ;; we consult input-files, the first of which should be the one to compile-file
          (input-file (first (input-files o c)))
          ;; On some implementations, there are more than one output-file,
          ;; but the first one should always be the primary fasl that gets loaded.
          (outputs (output-files o c)))
      (multiple-value-bind (output warnings-p failure-p)
          (destructuring-bind
              (output-file
               &optional
                 #+(or clasp ecl mkcl) object-file
                 #+clisp lib-file
                 warnings-file &rest rest) outputs
            ;; Allow for extra outputs that are not of type warnings-file
            ;; The way we do it is kludgy. In ASDF4, output-files shall not be positional.
            (declare (ignore rest))
            (when warnings-file
              (unless (equal (pathname-type warnings-file) (warnings-file-type))
                (setf warnings-file nil)))
            (call-with-around-compile-hook
             c #'(lambda (&rest flags)
                   (apply 'compile-file* input-file
                          :output-file output-file
                          :external-format (component-external-format c)
                          :warnings-file warnings-file
                          (append
                           #+clisp (list :lib-file lib-file)
                           #+(or clasp ecl mkcl) (list :object-file object-file)
                           flags)))))
        (check-lisp-compile-results output warnings-p failure-p
                                    "~/asdf-action::format-action/" (list (cons o c))))))
  (defun report-file-p (f)
    "Is F a build report file containing, e.g., warnings to check?"
    (equalp (pathname-type f) "build-report"))
  (defun perform-lisp-warnings-check (o c)
    "Check the warnings associated with the dependencies of an action."
    (let* ((expected-warnings-files (remove-if-not #'warnings-file-p (input-files o c)))
           (actual-warnings-files (loop :for w :in expected-warnings-files
                                        :when (get-file-stamp w)
                                          :collect w
                                        :else :do (warn "Missing warnings file ~S while ~A"
                                                        w (action-description o c)))))
      (check-deferred-warnings actual-warnings-files)
      (let* ((output (output-files o c))
             (report (find-if #'report-file-p output)))
        (when report
          (with-open-file (s report :direction :output :if-exists :supersede)
            (format s ":success~%"))))))
  (defmethod perform ((o compile-op) (c cl-source-file))
    (perform-lisp-compilation o c))
  (defun lisp-compilation-output-files (o c)
    "Compute the output-files for compiling the Lisp file for the specified action (O . C),
an OPERATION and a COMPONENT."
    (let* ((i (first (input-files o c)))
           (f (compile-file-pathname
               i #+clasp :output-type #+ecl :type #+(or clasp ecl) :fasl
               #+mkcl :fasl-p #+mkcl t)))
      `(,f ;; the fasl is the primary output, in first position
        #+clasp
        ,@(unless nil ;; was (use-ecl-byte-compiler-p)
            `(,(compile-file-pathname i :output-type :object)))
        #+clisp
        ,@`(,(make-pathname :type "lib" :defaults f))
        #+ecl
        ,@(unless (use-ecl-byte-compiler-p)
            `(,(compile-file-pathname i :type :object)))
        #+mkcl
        ,(compile-file-pathname i :fasl-p nil) ;; object file
        ,@(when (and *warnings-file-type* (not (builtin-system-p (component-system c))))
            `(,(make-pathname :type *warnings-file-type* :defaults f))))))
  (defmethod output-files ((o compile-op) (c cl-source-file))
    (lisp-compilation-output-files o c))
  (defmethod perform ((o compile-op) (c static-file))
    nil)

  ;; Performing compile-op on a system will check the deferred warnings for the system
  (defmethod perform ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      (perform-lisp-warnings-check o c)))
  (defmethod input-files ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      ;; The most correct way to do it would be to use:
      ;; (collect-dependencies o c :other-systems nil :keep-operation 'compile-op :keep-component 'cl-source-file)
      ;; but it's expensive and we don't care too much about file order or ASDF extensions.
      (loop :for sub :in (sub-components c :type 'cl-source-file)
            :nconc (remove-if-not 'warnings-file-p (output-files o sub)))))
  (defmethod output-files ((o compile-op) (c system))
    (when (and *warnings-file-type* (not (builtin-system-p c)))
      (if-let ((pathname (component-pathname c)))
        (list (subpathname pathname (coerce-filename c) :type "build-report"))))))

;;; load-op
(with-upgradability ()
  (defmethod action-description ((o load-op) (c cl-source-file))
    (format nil (compatfmt "~@<loading FASL for ~3i~_~A~@:>") c))
  (defmethod action-description ((o load-op) (c parent-component))
    (format nil (compatfmt "~@<completing load for ~3i~_~A~@:>") c))
  (defmethod action-description ((o load-op) (c component))
    (format nil (compatfmt "~@<loading ~3i~_~A~@:>") c))
  (defmethod perform-with-restarts ((o load-op) (c cl-source-file))
    (loop
      (restart-case
          (return (call-next-method))
        (try-recompiling ()
          :report (lambda (s)
                    (format s "Recompile ~a and try loading it again"
                            (component-name c)))
          (perform (find-operation o 'compile-op) c)))))
  (defun perform-lisp-load-fasl (o c)
    "Perform the loading of a FASL associated to specified action (O . C),
an OPERATION and a COMPONENT."
    (if-let (fasl (first (input-files o c)))
      (load* fasl)))
  (defmethod perform ((o load-op) (c cl-source-file))
    (perform-lisp-load-fasl o c))
  (defmethod perform ((o load-op) (c static-file))
    nil))


;;;; prepare-source-op, load-source-op

;;; prepare-source-op
(with-upgradability ()
  (defmethod action-description ((o prepare-source-op) (c component))
    (format nil (compatfmt "~@<loading source for dependencies of ~3i~_~A~@:>") c))
  (defmethod input-files ((o prepare-source-op) (s system))
    (if-let (it (system-source-file s)) (list it)))
  (defmethod perform ((o prepare-source-op) (c component))
    nil))

;;; load-source-op
(with-upgradability ()
  (defmethod action-description ((o load-source-op) (c component))
    (format nil (compatfmt "~@<Loading source of ~3i~_~A~@:>") c))
  (defmethod action-description ((o load-source-op) (c parent-component))
    (format nil (compatfmt "~@<Loaded source of ~3i~_~A~@:>") c))
  (defun perform-lisp-load-source (o c)
    "Perform the loading of a Lisp file as associated to specified action (O . C)"
    (call-with-around-compile-hook
     c #'(lambda ()
           (load* (first (input-files o c))
                  :external-format (component-external-format c)))))

  (defmethod perform ((o load-source-op) (c cl-source-file))
    (perform-lisp-load-source o c))
  (defmethod perform ((o load-source-op) (c static-file))
    nil))


;;;; test-op
(with-upgradability ()
  (defmethod perform ((o test-op) (c component))
    nil)
  (defmethod operation-done-p ((o test-op) (c system))
    "Testing a system is _never_ done."
    nil))
