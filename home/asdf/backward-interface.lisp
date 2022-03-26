;;;; -------------------------------------------------------------------------
;;; Backward-compatible interfaces

(uiop/package:define-package :asdf/backward-interface
  (:recycle :asdf/backward-interface :asdf)
  (:use :uiop/common-lisp :uiop :asdf/upgrade :asdf/session
   :asdf/component :asdf/system :asdf/system-registry :asdf/operation :asdf/action
   :asdf/lisp-action :asdf/plan :asdf/operate
   :asdf/find-system :asdf/parse-defsystem :asdf/output-translations :asdf/bundle)
  (:export
   #:*asdf-verbose*
   #:operation-error #:compile-error #:compile-failed #:compile-warned
   #:error-component #:error-operation #:traverse
   #:component-load-dependencies
   #:enable-asdf-binary-locations-compatibility
   #:operation-on-failure #:operation-on-warnings #:on-failure #:on-warnings
   #:component-property
   #:run-shell-command
   #:system-definition-pathname #:system-registered-p #:require-system
   #:explain
   #+ecl #:make-build))
(in-package :asdf/backward-interface)

;; NB: the warning status of these functions may have to be distinguished later,
;; as some get removed faster than the others in client code.
(with-asdf-deprecation (:style-warning "3.2" :warning "3.4")

  ;; These conditions from ASDF 1 and 2 are used by many packages in Quicklisp;
  ;; but ASDF3 replaced them with somewhat different variants of uiop:compile-condition
  ;; that do not involve ASDF actions.
  ;; TODO: find the offenders and stop them.
  (progn
    (define-condition operation-error (error) ;; Bad, backward-compatible name
      ;; Used by SBCL, cffi-tests, clsql-mysql, clsql-uffi, qt, elephant, uffi-tests, sb-grovel
      ((component :reader error-component :initarg :component)
       (operation :reader error-operation :initarg :operation))
      (:report (lambda (c s)
                 (format s (compatfmt "~@<~A while invoking ~A on ~A~@:>")
                         (type-of c) (error-operation c) (error-component c)))))
    (define-condition compile-error (operation-error) ())
    (define-condition compile-failed (compile-error) ())
    (define-condition compile-warned (compile-error) ()))

  ;; In Quicklisp 2015-05, still used by lisp-executable, staple, repl-utilities, cffi
  (defun component-load-dependencies (component) ;; from ASDF 2.000 to 2.26
    "DEPRECATED. Please use COMPONENT-SIDEWAY-DEPENDENCIES instead; or better,
define your operations with proper use of SIDEWAY-OPERATION, SELFWARD-OPERATION,
or define methods on PREPARE-OP, etc."
    ;; Old deprecated name for the same thing. Please update your software.
    (component-sideway-dependencies component))

  ;; These old interfaces from ASDF1 have never been very meaningful
  ;; but are still used in obscure places.
  ;; In Quicklisp 2015-05, still used by cl-protobufs and clx.
  (defgeneric operation-on-warnings (operation)
    (:documentation "DEPRECATED. Please use UIOP:*COMPILE-FILE-WARNINGS-BEHAVIOUR* instead."))
  (defgeneric operation-on-failure (operation)
    (:documentation "DEPRECATED. Please use UIOP:*COMPILE-FILE-FAILURE-BEHAVIOUR* instead."))
  (defgeneric (setf operation-on-warnings) (x operation)
    (:documentation "DEPRECATED. Please SETF UIOP:*COMPILE-FILE-WARNINGS-BEHAVIOUR* instead."))
  (defgeneric (setf operation-on-failure) (x operation)
    (:documentation "DEPRECATED. Please SETF UIOP:*COMPILE-FILE-FAILURE-BEHAVIOUR* instead."))
  (progn
    (defmethod operation-on-warnings ((o operation))
      *compile-file-warnings-behaviour*)
    (defmethod operation-on-failure ((o operation))
      *compile-file-failure-behaviour*)
    (defmethod (setf operation-on-warnings) (x (o operation))
      (setf *compile-file-warnings-behaviour* x))
    (defmethod (setf operation-on-failure) (x (o operation))
      (setf *compile-file-failure-behaviour* x)))

  ;; Quicklisp 2015-05: Still used by SLIME's swank-asdf (!), common-lisp-stat,
  ;; js-parser, osicat, babel, staple, weblocks, cl-png, plain-odbc, autoproject,
  ;; cl-blapack, com.informatimago, cells-gtk3, asdf-dependency-grovel,
  ;; cl-glfw, cffi, jwacs, montezuma
  (defun system-definition-pathname (x)
    ;; As of 2.014.8, we mean to make this function obsolete,
    ;; but that won't happen until all clients have been updated.
    "DEPRECATED. This function used to expose ASDF internals with subtle
differences with respect to user expectations, that have been refactored
away since. We recommend you use ASDF:SYSTEM-SOURCE-FILE instead for a
mostly compatible replacement that we're supporting, or even
ASDF:SYSTEM-SOURCE-DIRECTORY or ASDF:SYSTEM-RELATIVE-PATHNAME
if that's whay you mean." ;;)
    (system-source-file x))

  ;; TRAVERSE is the function used to compute a plan in ASDF 1 and 2.
  ;; It was never officially exposed but some people still used it.
  (defgeneric traverse (operation component &key &allow-other-keys)
    (:documentation
     "DEPRECATED. Use MAKE-PLAN and PLAN-ACTIONS, or REQUIRED-COMPONENTS,
or some other supported interface instead.

Generate and return a plan for performing OPERATION on COMPONENT.

The plan returned is a list of dotted-pairs. Each pair is the CONS
of ASDF operation object and a COMPONENT object. The pairs will be
processed in order by OPERATE."))
  (progn
    (define-convenience-action-methods traverse (operation component &key)))
  (defmethod traverse ((o operation) (c component) &rest keys &key plan-class &allow-other-keys)
    (plan-actions (apply 'make-plan plan-class o c keys)))


  ;; ASDF-Binary-Locations compatibility
  ;; This remains supported for legacy user, but not recommended for new users.
  ;; We suspect there are no more legacy users in 2016.
  (defun enable-asdf-binary-locations-compatibility
      (&key
         (centralize-lisp-binaries nil)
         (default-toplevel-directory
             ;; Use ".cache/common-lisp/" instead ???
             (subpathname (user-homedir-pathname) ".fasls/"))
         (include-per-user-information nil)
         (map-all-source-files (or #+(or clasp clisp ecl mkcl) t nil))
         (source-to-target-mappings nil)
         (file-types `(,(compile-file-type)
                        "build-report"
                        #+clasp (compile-file-type :output-type :object)
                        #+ecl (compile-file-type :type :object)
                        #+mkcl (compile-file-type :fasl-p nil)
                        #+clisp "lib" #+sbcl "cfasl"
                        #+sbcl "sbcl-warnings" #+clozure "ccl-warnings")))
    "DEPRECATED. Use asdf-output-translations instead."
    #+(or clasp clisp ecl mkcl)
    (when (null map-all-source-files)
      (error "asdf:enable-asdf-binary-locations-compatibility doesn't support :map-all-source-files nil on CLISP, ECL and MKCL"))
    (let* ((patterns (if map-all-source-files (list *wild-file*)
                         (loop :for type :in file-types
                           :collect (make-pathname :type type :defaults *wild-file*))))
           (destination-directory
            (if centralize-lisp-binaries
                `(,default-toplevel-directory
                     ,@(when include-per-user-information
                             (cdr (pathname-directory (user-homedir-pathname))))
                     :implementation ,*wild-inferiors*)
                `(:root ,*wild-inferiors* :implementation))))
      (initialize-output-translations
       `(:output-translations
         ,@source-to-target-mappings
         #+abcl (#p"jar:file:/**/*.jar!/**/*.*" (:function translate-jar-pathname))
         #+abcl (#p"/___jar___file___root___/**/*.*" (,@destination-directory))
         ,@(loop :for pattern :in patterns
             :collect `((:root ,*wild-inferiors* ,pattern)
                        (,@destination-directory ,pattern)))
         (t t)
         :ignore-inherited-configuration))))
  (progn
    (defmethod operate :before (operation-class system &rest args &key &allow-other-keys)
      (declare (ignore operation-class system args))
      (when (find-symbol* '#:output-files-for-system-and-operation :asdf nil)
        (error "ASDF 2 is not compatible with ASDF-BINARY-LOCATIONS, which you are using.
ASDF 2 now achieves the same purpose with its builtin ASDF-OUTPUT-TRANSLATIONS,
which should be easier to configure. Please stop using ASDF-BINARY-LOCATIONS,
and instead use ASDF-OUTPUT-TRANSLATIONS. See the ASDF manual for details.
In case you insist on preserving your previous A-B-L configuration, but
do not know how to achieve the same effect with A-O-T, you may use function
ASDF:ENABLE-ASDF-BINARY-LOCATIONS-COMPATIBILITY as documented in the manual;
call that function where you would otherwise have loaded and configured A-B-L."))))


  ;; run-shell-command from ASDF 2, lightly fixed from ASDF 1, copied from MK-DEFSYSTEM. Die!
  (defun run-shell-command (control-string &rest args)
    "PLEASE DO NOT USE. This function is not just DEPRECATED, but also dysfunctional.
Please use UIOP:RUN-PROGRAM instead."
    #-(and ecl os-windows)
    (let ((command (apply 'format nil control-string args)))
      (asdf-message "; $ ~A~%" command)
      (let ((exit-code
             (ignore-errors
               (nth-value 2 (run-program command :force-shell t :ignore-error-status t
                                         :output *verbose-out*)))))
        (typecase exit-code
          ((integer 0 255) exit-code)
          (t 255))))
    #+(and ecl os-windows)
    (not-implemented-error "run-shell-command" "for ECL on Windows."))

  ;; HOW do we get rid of variables??? With a symbol-macro that issues a warning?
  ;; In Quicklisp 2015-05, cl-protobufs still uses it, but that should be fixed in next version.
  (progn
    (defvar *asdf-verbose* nil)) ;; backward-compatibility with ASDF2 only. Unused.

  ;; Do NOT use in new code. NOT SUPPORTED.
  ;; NB: When this goes away, remove the slot PROPERTY in COMPONENT.
  ;; In Quicklisp 2014-05, it's still used by yaclml, amazon-ecs, blackthorn-engine, cl-tidy.
  ;; See TODO for further cleanups required before to get rid of it.
  (defgeneric component-property (component property))
  (defgeneric (setf component-property) (new-value component property))

  (defmethod component-property ((c component) property)
    (cdr (assoc property (slot-value c 'properties) :test #'equal)))

  (defmethod (setf component-property) (new-value (c component) property)
    (let ((a (assoc property (slot-value c 'properties) :test #'equal)))
      (if a
          (setf (cdr a) new-value)
          (setf (slot-value c 'properties)
                (acons property new-value (slot-value c 'properties)))))
    new-value)


  ;; This method survives from ASDF 1, but really it is superseded by action-description.
  (defgeneric explain (operation component)
    (:documentation "Display a message describing an action.

DEPRECATED. Use ASDF:ACTION-DESCRIPTION and/or ASDF::FORMAT-ACTION instead."))
  (progn
    (define-convenience-action-methods explain (operation component)))
  (defmethod explain ((o operation) (c component))
    (asdf-message (compatfmt "~&~@<; ~@;~A~:>~%") (action-description o c))))

(with-asdf-deprecation (:style-warning "3.3")
  (defun system-registered-p (name)
    "DEPRECATED. Return a generalized boolean that is true if a system of given NAME was registered already.
NAME is a system designator, to be normalized by COERCE-NAME.
The value returned if true is a pair of a timestamp and a system object."
    (if-let (system (registered-system name))
      (cons (if-let (primary-system (registered-system (primary-system-name name)))
              (component-operation-time 'define-op primary-system))
            system)))

  (defun require-system (system &rest keys &key &allow-other-keys)
    "Ensure the specified SYSTEM is loaded, passing the KEYS to OPERATE, but do not update the
system or its dependencies if it has already been loaded."
    (declare (ignore keys))
    (unless (component-loaded-p system)
      (load-system system))))

;;; This function is for backward compatibility with ECL only.
#+ecl
(with-asdf-deprecation (:style-warning "3.2" :warning "9999")
  (defun make-build (system &rest args
                     &key (monolithic nil) (type :fasl) (move-here nil move-here-p)
                       prologue-code epilogue-code no-uiop
                       prefix-lisp-object-files postfix-lisp-object-files extra-object-files
                       &allow-other-keys)
    (let* ((operation (asdf/bundle::select-bundle-operation type monolithic))
           (move-here-path (if (and move-here
                                    (typep move-here '(or pathname string)))
                               (ensure-pathname move-here :namestring :lisp :ensure-directory t)
                               (system-relative-pathname system "asdf-output/")))
           (extra-build-args (remove-plist-keys
                              '(:monolithic :type :move-here
                                :prologue-code :epilogue-code :no-uiop
                                :prefix-lisp-object-files :postfix-lisp-object-files
                                :extra-object-files)
                              args))
           (build-system (if (subtypep operation 'image-op)
                             (eval `(defsystem "asdf.make-build"
                                      :class program-system
                                      :source-file nil
                                      :pathname ,(system-source-directory system)
                                      :build-operation ,operation
                                      :build-pathname ,(subpathname move-here-path
                                                                    (file-namestring (first (output-files operation system))))
                                      :depends-on (,(coerce-name system))
                                      :prologue-code ,prologue-code
                                      :epilogue-code ,epilogue-code
                                      :no-uiop ,no-uiop
                                      :prefix-lisp-object-files ,prefix-lisp-object-files
                                      :postfix-lisp-object-files ,postfix-lisp-object-files
                                      :extra-object-files ,extra-object-files
                                      :extra-build-args ,extra-build-args))
                             system))
           (files (output-files operation build-system)))
      (operate operation build-system)
      (if (or move-here
              (and (null move-here-p) (member operation '(program-op image-op))))
          (loop :with dest-path = (resolve-symlinks* (ensure-directories-exist move-here-path))
            :for f :in files
            :for new-f = (make-pathname :name (pathname-name f)
                                        :type (pathname-type f)
                                        :defaults dest-path)
            :do (rename-file-overwriting-target f new-f)
            :collect new-f)
          files))))
