(in-package :mezzano.clos)

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

(defparameter *enable-generic-function-inline-caching* t
  "Configure if inline caches attempt to optimize generic dispatch.

This does not control if inline caches are generated or not in compiled
code, it's a runtime flag that turns cache participation on or off.")

(defun inline-cache-fast-reader (inline-cache gf class slot-definition)
  (let* ((slot-name (slot-definition-name slot-definition))
         (effective-slot (find slot-name (safe-class-slots class)
                               :key #'safe-slot-definition-name))
         (known-header (mezzano.runtime::%make-instance-header
                        (class-layout class))))
    (cond (effective-slot
           (let ((location (safe-slot-definition-location effective-slot))
                 (unbound-value *secret-unbound-value*))
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
                                             (safe-slot-definition-name
                                              slot-definition))))
                                value)))
                         (t
                          (inline-cache-miss inline-cache gf instance))))
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
                                               (safe-slot-definition-name
                                                slot-definition))))
                                  value)))
                           (t
                            (inline-cache-miss inline-cache gf instance))))))))
          (t
           ;; Slot not present, fall back on SLOT-VALUE.
           (lambda (instance)
             (slot-value instance slot-name))))))

(defun inline-cache-fast-writer (inline-cache gf class slot-definition)
  (let* ((slot-name (slot-definition-name slot-definition))
         (effective-slot (find slot-name (safe-class-slots class)
                               :key #'safe-slot-definition-name))
         (typecheck (safe-slot-definition-typecheck effective-slot))
         (type (safe-slot-definition-type effective-slot))
         (known-header (mezzano.runtime::%make-instance-header
                        (class-layout class))))
    (cond (effective-slot
           (let ((location (safe-slot-definition-location effective-slot)))
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
                             (eql (length (gf-required-arglist gf)) ',n-required)
                             (or (and ',restp (or (gf-optional-arglist gf)
                                                  (gf-rest-arg-p gf)))
                                 (and (not ',restp) (not (or (gf-optional-arglist gf)
                                                             (gf-rest-arg-p gf))))))
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
                         (class-layout known-class)
                         (if (typep known-class 'funcallable-standard-class)
                             sys.int::+object-tag-funcallable-instance+
                             sys.int::+object-tag-instance+))))
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
        (generic-function-single-dispatch-p gf)
      (let* ((tracep (safe-generic-function-trace-p gf))
             (classes (mapcar #'class-of (required-portion gf args)))
             (class (and argument-offset (nth argument-offset classes))))
        (multiple-value-bind (applicable-methods validp)
            (std-compute-applicable-methods-using-classes gf classes)
          (let ((emfun (if applicable-methods
                           (std-compute-effective-method-function
                            gf applicable-methods)
                           (lambda (&rest args)
                             (apply #'no-applicable-method gf args)))))
            (cond ((and applicable-methods
                        validp
                        (not tracep)
                        single-dispatch-p
                        (every 'primary-method-p applicable-methods)
                        (eql (class-of (first applicable-methods))
                             *the-class-standard-reader-method*)
                        (std-class-instance-p class))
                   ;; Reader method.
                   (set-funcallable-instance-function
                    inline-cache
                    (inline-cache-fast-reader
                     inline-cache gf class
                     (accessor-method-slot-definition
                      (first applicable-methods)))))
                  ((and applicable-methods
                        validp
                        (not tracep)
                        single-dispatch-p
                        (every 'primary-method-p applicable-methods)
                        (eql (class-of (first applicable-methods))
                             *the-class-standard-writer-method*)
                        (std-class-instance-p class))
                   ;; Writer method, use a fast accessor.
                   (set-funcallable-instance-function
                    inline-cache
                    (inline-cache-fast-writer
                     inline-cache gf class
                     (accessor-method-slot-definition
                      (first applicable-methods)))))
                  ((and applicable-methods
                        validp
                        (not tracep)
                        single-dispatch-p
                        (std-class-instance-p class))
                   ;; 1-effective dispatch
                   (set-funcallable-instance-function
                    inline-cache
                    (inline-cache-fast-1-effective
                     inline-cache gf emfun argument-offset class)))
                  (t
                   ;; otherwise punt
                   (set-funcallable-instance-function
                    inline-cache gf)))))))))

(defun inline-cache-miss (inline-cache gf &rest args)
  ;; Give up, call the function directly.
  (set-funcallable-instance-function inline-cache gf)
  (apply gf args))

(defmethod mezzano.runtime::deinitialize-inline-cache ((inline-cache mezzano.runtime::inline-cache) (function standard-generic-function))
  (remove-dependent function inline-cache))

;; Called when the generic function is changes, and also once on initialization of the
;; cache. This is where we set up our generic-function inline caching.
(defmethod update-dependent ((gf standard-generic-function) (dependent mezzano.runtime::inline-cache) &rest initargs)
  (declare (ignore initargs))
  (set-funcallable-instance-function
   dependent
   (lambda (&rest args)
     (apply #'populate-inline-cache dependent gf args)
     (apply gf args))))

(defmethod mezzano.runtime::initialize-inline-cache ((inline-cache mezzano.runtime::inline-cache) (gf standard-generic-function))
  (cond ((and *enable-generic-function-inline-caching*
              (eql (class-of gf) (find-class 'standard-generic-function)))
         ;; Register as a dependent of the gf here so that we can invalidate
         ;; when the gf has methods added/removed.
         (add-dependent gf inline-cache :weak t)
         (update-dependent gf inline-cache))
        (t
         ;; Don't enable caching for derived generic functions, since
         ;; they may have different resolution behaviour.
         (call-next-method))))
