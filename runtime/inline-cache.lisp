;;;; Inline caches for generic dispatch

(in-package :mezzano.runtime)

;;; An inline cache can be associated with either a named
;;; Function-Reference, or with a specific function.
;;; When associated with a Function-Reference it will register
;;; as a dependent of that and automatically track the fref's
;;; underlying function. If associated with directly with a
;;; specific function, then it will track that function indefinitely.
;;;
;;; Note, for bootstrapping reasons a simplified version of this
;;; definition is part of the cold-generator.
(defclass inline-cache (mezzano.clos:funcallable-standard-object)
  ((%fref :initform nil :initarg :function-reference :reader inline-cache-function-reference)
   (%function :initform nil :initarg :function :accessor inline-cache-function))
  (:metaclass mezzano.clos:funcallable-standard-class))

(defmethod print-object ((object inline-cache) stream)
  (print-unreadable-object (object stream :identity t :type t)
    (format stream "~S" (or (inline-cache-function-reference object)
                            (inline-cache-function object)))))

(defgeneric initialize-inline-cache (inline-cache function))
(defgeneric deinitialize-inline-cache (inline-cache function))

;; Called when the function-reference's function changes.
;; We need to reset the cache and restore the original behaviour.
(defmethod sys.int::update-function-reference-dependent (function-reference (cache inline-cache))
  (deinitialize-inline-cache cache (inline-cache-function cache))
  (let ((new (sys.int::function-reference-function function-reference)))
    (setf (inline-cache-function cache) new)
    (initialize-inline-cache cache new)))

(defmethod initialize-inline-cache ((inline-cache inline-cache) (function function))
  (change-class inline-cache 'inline-cache)
  ;; No caching is possible here, just jump straight to the function.
  (mezzano.clos:set-funcallable-instance-function inline-cache function))

(defmethod initialize-inline-cache ((inline-cache inline-cache) (function null))
  (change-class inline-cache 'inline-cache)
  ;; Need to replicate the behaviour of the undefined-function path.
  (mezzano.clos:set-funcallable-instance-function
   inline-cache
   (lambda (&rest args)
     (sys.int::undefined-function-called
      args (inline-cache-function-reference inline-cache)))))

(defmethod deinitialize-inline-cache ((inline-cache inline-cache) (function null))
  (values))

(defmethod deinitialize-inline-cache ((inline-cache inline-cache) (function function))
  (values))

(defun make-inline-cache (function-or-function-name)
  (if (functionp function-or-function-name)
      (let ((cache (make-instance
                    'inline-cache
                    :function function-or-function-name)))
        (initialize-inline-cache cache function-or-function-name)
        cache)
      (let* ((fref (sys.int::function-reference function-or-function-name))
             (fun (sys.int::function-reference-function fref))
             (cache (make-instance
                     'inline-cache
                     :function-reference fref
                     :function fun)))
        (sys.int::register-function-reference-dependent fref cache)
        (initialize-inline-cache cache fun)
        cache)))

;;;; Generic-Function caching machinery.
;;;;
;;;; This currently specializes on 3 different styles of call:
;;;; 1) 1-effective dispatch.
;;;; 2) Reader methods.
;;;; 3) Writer methods.
;;;;
;;;; There are significant limitations, EQL specializers are not supported.
;;;; This only applies to exactly Standard-Generic-Functions, since subclasses
;;;; may override the dispatch mechanism. Any cache-miss will drop back to
;;;; the ordinary dispatch path, there's no profiling or polymorphic caching.
;;;; Dispatch arguments must also subclasses of Standard-Object
;;;;
;;;; Future directions will be to separate the cache dispatch machinery (as in
;;;; testing for cache hit) from the actual operation (emfun, read, write),
;;;; and compiling general purpose cache methods for various different combinations.
;;;; This will also allow type testing of builtin classes (instead of requring
;;;; instances)

(defun inline-cache-fast-reader (inline-cache gf class slot-definition)
  (let* ((slot-name (mezzano.clos::slot-definition-name slot-definition))
         (effective-slot (find slot-name (mezzano.clos::safe-class-slots class)))
         (known-header (mezzano.runtime::%make-instance-header
                        (mezzano.clos:class-layout class))))
    (cond (effective-slot
           (let ((location (mezzano.clos::safe-slot-definition-location effective-slot))
                 (unbound-value mezzano.clos::*secret-unbound-value*))
             (if (consp location)
                 ;; Class-allocation cell
                 (lambda (instance)
                   (cond ((and (sys.int::%value-has-tag-p instance sys.int::+tag-object+)
                               (sys.int::%fast-instance-layout-eq-p instance known-header))
                          (let ((value (cdr location)))
                            (if (eq unbound-value value)
                                (values (slot-unbound
                                         (class-of instance)
                                         instance
                                         (if (consp location)
                                             (car location)
                                             (mezzano.clos::safe-slot-definition-name
                                              slot-definition))))
                                value)))
                         (t
                          (inline-cache-miss inline-cache instance))))
                 ;; inline cell
                 (let ((index (mezzano.runtime::location-offset-t location)))
                   (lambda (instance)
                     (cond ((and (sys.int::%value-has-tag-p instance sys.int::+tag-object+)
                                 (sys.int::%fast-instance-layout-eq-p instance known-header))
                            (let ((value (sys.int::%object-ref-t instance index)))
                              (if (eq unbound-value value)
                                  (values (slot-unbound
                                           (class-of instance)
                                           instance
                                           (if (consp location)
                                               (car location)
                                               (mezzano.clos::safe-slot-definition-name
                                                slot-definition))))
                                  value)))
                           (t
                            (inline-cache-miss inline-cache gf instance))))))))
          (t
           ;; Slot not present, fall back on SLOT-VALUE.
           (lambda (instance)
             (slot-value instance slot-name))))))

(defun inline-cache-fast-writer (inline-cache gf class slot-definition)
  (let* ((slot-name (mezzano.clos::slot-definition-name slot-definition))
         (effective-slot (find slot-name (mezzano.clos::safe-class-slots class)))
         (typecheck (mezzano.clos::safe-slot-definition-typecheck effective-slot))
         (type (mezzano.clos::safe-slot-definition-type effective-slot))
         (known-header (mezzano.runtime::%make-instance-header
                        (mezzano.clos:class-layout class))))
    (cond (effective-slot
           (let ((location (mezzano.clos::safe-slot-definition-location effective-slot)))
             (if (consp location)
                 ;; Class-allocation cell
                 (lambda (new-value instance)
                   (cond ((and (sys.int::%value-has-tag-p instance sys.int::+tag-object+)
                               (sys.int::%fast-instance-layout-eq-p instance known-header))
                          (when (and typecheck (not (funcall typecheck new-value)))
                            (error 'type-error
                                   :datum new-value
                                   :expected-type type))
                          (setf (cdr location) new-value))
                         (t
                          (inline-cache-miss inline-cache gf new-value instance))))
                 ;; inline cell
                 (let ((index (mezzano.runtime::location-offset-t location)))
                   (lambda (new-value instance)
                     (cond ((and (sys.int::%value-has-tag-p instance sys.int::+tag-object+)
                                 (sys.int::%fast-instance-layout-eq-p instance known-header))
                            (when (and typecheck (not (funcall typecheck new-value)))
                              (error 'type-error
                                     :datum new-value
                                     :expected-type type))
                            (setf (sys.int::%object-ref-t instance index) new-value))
                           (t
                            (inline-cache-miss inline-cache gf new-value instance))))))))
          (t
           ;; Slot not present, fall back on SLOT-VALUE.
           (lambda (new-value instance)
             (setf (slot-value instance slot-name) new-value))))))

(defun inline-cache-fast-1-effective (inline-cache gf emfun argument-offset known-class)
  ;; Generate specialized dispatch functions for various combinations of
  ;; arguments.
  (macrolet ((gen-one (index n-required restp)
               (let ((req-args (loop
                                 for i below n-required
                                 collect (gensym)))
                     (rest-arg (when restp
                                 (gensym))))
                 `(when (and (eql ',index argument-offset)
                             (eql (length (mezzano.clos::gf-required-arglist gf)) ',n-required)
                             (or (and ',restp (or (mezzano.clos::gf-optional-arglist gf)
                                                  (mezzano.clos::gf-rest-arg-p gf)))
                                 (and (not ',restp) (not (or (mezzano.clos::gf-optional-arglist gf)
                                                             (mezzano.clos::gf-rest-arg-p gf))))))
                    (lambda (,@req-args ,@(if rest-arg
                                              `(&rest ,rest-arg)
                                              '()))
                      (declare (sys.int::lambda-name (1-effective-inline-cache ,index ,n-required ,restp))
                               ,@(if rest-arg
                                     `((dynamic-extent ,rest-arg))
                                     `()))
                      (block nil
                        (locally
                            (declare (optimize speed (safety 0))
                                     (type function emfun))
                          (cond ((and (sys.int::%value-has-tag-p ,(nth index req-args) sys.int::+tag-object+)
                                      (sys.int::%fast-instance-layout-eq-p ,(nth index req-args) known-header))
                                 ,(if rest-arg
                                      `(apply emfun ,@req-args ,rest-arg)
                                      `(funcall emfun ,@req-args)))
                                (t
                                 ,(if rest-arg
                                      `(apply #'inline-cache-miss inline-cache gf ,@req-args ,rest-arg)
                                      `(inline-cache-miss inline-cache gf ,@req-args))))))))))
             (gen-all ()
               `(or
                 ,@(loop
                     for idx from 0 below 5
                     appending
                     (loop
                       for req from 1 to 5
                       collect `(gen-one ,idx ,req nil)
                       collect `(gen-one ,idx ,req t))))))
    (let ((known-header (mezzano.runtime::%make-instance-header
                         (mezzano.clos:class-layout known-class))))
      (or (gen-all)
          (lambda (&rest args)
            (declare (dynamic-extent args))
            (let* ((arg (nth argument-offset args)))
              (cond ((eq (class-of arg) known-class)
                     (apply emfun args))
                    (t
                     (apply #'inline-cache-miss inline-cache gf args)))))))))

(defparameter *in-populate-inline-cache* nil)

(defun populate-inline-cache (inline-cache gf &rest args)
  (when *in-populate-inline-cache*
    (return-from populate-inline-cache))
  (let ((*in-populate-inline-cache* t))
    ;; Figure out what kind of type checking to do.
    ;; This basically mirrors the existing dispatch but tries to be more clever
    ;; because we have more information (we know the exact values of the arguments).
    (multiple-value-bind (single-dispatch-p argument-offset)
        (mezzano.clos::generic-function-single-dispatch-p gf)
      (let* ((tracep (mezzano.clos::safe-generic-function-trace-p gf))
             (classes (mapcar #'class-of (mezzano.clos::required-portion gf args)))
             (class (and argument-offset (nth argument-offset classes))))
        (multiple-value-bind (applicable-methods validp)
            (mezzano.clos::std-compute-applicable-methods-using-classes gf classes)
          (let ((emfun (if applicable-methods
                           (mezzano.clos::std-compute-effective-method-function
                            gf applicable-methods)
                           (lambda (&rest args)
                             (apply #'no-applicable-method gf args)))))
            (cond ((and applicable-methods
                        validp
                        (not tracep)
                        single-dispatch-p
                        (every 'mezzano.clos::primary-method-p applicable-methods)
                        (eql (class-of (first applicable-methods))
                             mezzano.clos::*the-class-standard-reader-method*)
                        (mezzano.clos::std-class-instance-p class))
                   ;; Reader method.
                   (mezzano.clos:set-funcallable-instance-function
                    inline-cache
                    (inline-cache-fast-reader
                     inline-cache gf class
                     (mezzano.clos::accessor-method-slot-definition
                      (first applicable-methods)))))
                  ((and applicable-methods
                        validp
                        (not tracep)
                        single-dispatch-p
                        (every 'mezzano.clos::primary-method-p applicable-methods)
                        (eql (class-of (first applicable-methods))
                             mezzano.clos::*the-class-standard-writer-method*)
                        (mezzano.clos::std-class-instance-p class))
                   ;; Writer method, use a fast accessor.
                   (mezzano.clos:set-funcallable-instance-function
                    inline-cache
                    (inline-cache-fast-writer
                     inline-cache gf class
                     (mezzano.clos::accessor-method-slot-definition
                      (first applicable-methods)))))
                  ((and applicable-methods
                        validp
                        (not tracep)
                        single-dispatch-p
                        (mezzano.clos::std-class-instance-p class))
                   ;; 1-effective dispatch
                   (mezzano.clos:set-funcallable-instance-function
                    inline-cache
                    (inline-cache-fast-1-effective
                     inline-cache gf emfun argument-offset class)))
                  (t
                   ;; otherwise punt
                   (mezzano.clos:set-funcallable-instance-function
                    inline-cache gf)))))))))

(defun inline-cache-miss (inline-cache gf &rest args)
  ;; Give up, call the function directly.
  (mezzano.clos:set-funcallable-instance-function inline-cache gf)
  (apply gf args))

(defmethod deinitialize-inline-cache ((inline-cache inline-cache) (function mezzano.clos:standard-generic-function))
  (mezzano.clos:remove-dependent function inline-cache))

;; Called when the generic function is changes, and also once on initialization of the
;; cache. This is where we set up our generic-function inline caching.
(defmethod mezzano.clos:update-dependent ((gf mezzano.clos:standard-generic-function) (dependent inline-cache) &rest initargs)
  (declare (ignore initargs))
  (mezzano.clos:set-funcallable-instance-function
   dependent
   (lambda (&rest args)
     (apply #'populate-inline-cache dependent gf args)
     (apply gf args))))

(defmethod initialize-inline-cache ((inline-cache inline-cache) (gf mezzano.clos:standard-generic-function))
  (cond ((eql (class-of gf) (find-class 'mezzano.clos:standard-generic-function))
         ;; Register as a dependent of the gf here so that we can invalidate
         ;; when the gf has methods added/removed.
         (mezzano.clos:add-dependent gf inline-cache :weak t)
         (mezzano.clos:update-dependent gf inline-cache))
        (t
         ;; Don't enable caching for derived generic functions, since
         ;; they may have different resolution behaviour.
         (call-next-method))))
