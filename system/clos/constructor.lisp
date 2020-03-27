(in-package :mezzano.clos)

(defmacro with-before-and-after-methods ((applicable-methods &rest apply-arguments) &body body)
  `(progn
     ;; Before methods.
     ,@(loop
          for method in (remove-if-not #'before-method-p applicable-methods)
          collect `(apply ',(method-fast-function method '() '()) ,@apply-arguments))
     (multiple-value-prog1
         (progn ,@body)
       ;; After methods.
       ,@(loop
            for method in (reverse (remove-if-not #'after-method-p applicable-methods))
            collect `(apply ',(method-fast-function method '() '()) ,@apply-arguments)))))

(defun compute-constructor-allocate-form (class initargs-sym)
  (let* ((applicable-methods (compute-applicable-methods
                              (fdefinition 'allocate-instance)
                              (list class)))
         (primary-methods (remove-if-not #'primary-method-p applicable-methods)))
    ;; There must be one primary method, one of the standard ones, and no around methods.
    (when (or (some #'around-method-p applicable-methods)
              (not (eql (length primary-methods) 1)))
      ;; This could bail out & just return the effective method.
      (return-from compute-constructor-allocate-form nil))
    (let* ((specialized-class (first (method-specializers (first primary-methods))))
           (funcallablep (cond ((eql specialized-class (find-class 'standard-class))
                                nil)
                               ((eql specialized-class (find-class 'funcallable-standard-class))
                                t)
                               (t
                                (return-from compute-constructor-allocate-form nil))))
           (instance-sym (gensym "INSTANCE"))
           (unbound-marker-sym (gensym "UNBOUND-MARKER"))
           (layout (safe-class-slot-storage-layout class)))
      (multiple-value-bind (initargs initargs-aok)
          (applicable-methods-initargs applicable-methods)
        (values `(with-before-and-after-methods (,applicable-methods ',class ,initargs-sym)
                   ;; Allocate the instance.
                   (let ((,instance-sym ,(if funcallablep
                                             `(sys.int::%allocate-funcallable-instance
                                               #'uninitialized-standard-funcallable-instance-function
                                               ',layout)
                                             `(sys.int::%allocate-instance ',layout)))
                         (,unbound-marker-sym *secret-unbound-value*))
                     (declare (ignorable ,unbound-marker-sym))
                     ;; Initialize all boxed heap slots to the unbound value.
                     ,@(loop
                          for i from (if funcallablep 2 0) below (sys.int::layout-heap-size layout)
                          when (or (eql (sys.int::layout-heap-layout layout) t)
                                   (and (bit-vector-p (sys.int::layout-heap-layout layout))
                                        (not (zerop (bit (sys.int::layout-heap-layout layout) i)))))
                          collect `(setf (sys.int::%object-ref-t ,instance-sym ',i) ,unbound-marker-sym))
                     ,instance-sym))
                initargs initargs-aok)))))

;; Initargs override already bound slots, but initfunctions do not.
;; Slots can be bound early by :before methods on initialize-instance or shared-initialize.
;; See 7.1.5 Shared-Initialize

(defun compute-constructor-slot-initarg-initialization-form (class slot instance-sym initargs-sym)
  (declare (ignore class))
  (labels ((set-value (value)
             `(setf (sys.int::%object-ref-t ,instance-sym ',(mezzano.runtime::location-offset-t (slot-definition-location slot)))
                    ,value)))
    (cond ((endp (slot-definition-initargs slot))
           nil)
          ((endp (rest (slot-definition-initargs slot)))
           ;; TODO: It would be nice to use the &KEY machinery for this.
           (let ((not-found (gensym "NOT-FOUND"))
                 (initarg-value (gensym "INITARG-VALUE")))
             `(let ((,initarg-value (getf ,initargs-sym ',(first (slot-definition-initargs slot)) ',not-found)))
                (if (not (eql ,initarg-value ',not-found))
                    ,(set-value initarg-value)
                    nil))))
          (t
           ;; Many initargs for this slot.
           (let ((init-key (gensym "INIT-KEY"))
                 (init-value (gensym "INIT-VALUE"))
                 (foundp (gensym "FOUNDP")))
             `(multiple-value-bind (,init-key ,init-value ,foundp)
                  (get-properties ,initargs-sym ',(slot-definition-initargs slot))
                (declare (ignore ,init-key))
                (if ,foundp
                    ,(set-value init-value)
                    nil)))))))

(defun compute-constructor-slot-initfunction-initialization-form (class slot instance-sym initargs-sym)
  (declare (ignore class initargs-sym))
  (when (slot-definition-initfunction slot)
    `(when (eql (sys.int::%object-ref-t ,instance-sym ',(mezzano.runtime::location-offset-t (slot-definition-location slot)))
                *secret-unbound-value*)
       (setf (sys.int::%object-ref-t ,instance-sym ',(mezzano.runtime::location-offset-t (slot-definition-location slot)))
             (funcall ',(slot-definition-initfunction slot))))))

(defun compute-constructor-initialization-form (class instance-sym initargs-sym)
  ;; Compute the form required to initialize an instance of CLASS.
  ;; This combines the call to INITIALIZE-INSTANCE and SHARED-INITIALIZE.
  (let* ((ii-applicable-methods (compute-applicable-methods
                                 (fdefinition 'initialize-instance)
                                 (list (class-prototype class))))
         (ii-primary-methods (remove-if-not #'primary-method-p ii-applicable-methods))
         (si-applicable-methods (compute-applicable-methods
                                 (fdefinition 'shared-initialize)
                                 (list (class-prototype class) 't)))
         (si-primary-methods (remove-if-not #'primary-method-p si-applicable-methods)))
    ;; There must be one primary method, one of the standard ones, and no around methods.
    (when (or (some #'around-method-p ii-applicable-methods)
              (not (eql (length ii-primary-methods) 1))
              (not (eql (first (method-specializers (first ii-primary-methods)))
                        (find-class 'standard-object)))
              (some #'around-method-p si-applicable-methods)
              (not (eql (length si-primary-methods) 1))
              (not (eql (first (method-specializers (first si-primary-methods)))
                        (find-class 'standard-object)))
              (not (eql (second (method-specializers (first si-primary-methods)))
                        (find-class 't))))
      ;; This could bail out & just return the effective method.
      (return-from compute-constructor-initialization-form nil))
    ;; Instance allocated slots only.
    (dolist (slot (class-slots class))
      (when (not (eql (slot-definition-allocation slot) :instance))
        (return-from compute-constructor-initialization-form nil)))
    (multiple-value-bind (ii-initargs ii-initargs-aok)
        (applicable-methods-initargs ii-applicable-methods)
      (multiple-value-bind (si-initargs si-initargs-aok)
          (applicable-methods-initargs si-applicable-methods)
        (values `(with-before-and-after-methods (,ii-applicable-methods ,instance-sym ,initargs-sym)
                   (with-before-and-after-methods (,si-applicable-methods ,instance-sym 't ,initargs-sym)
                     ;; Initialize each slot
                     ,@(loop
                          for slot in (class-slots class)
                          collect (compute-constructor-slot-initarg-initialization-form class slot instance-sym initargs-sym)
                          collect (compute-constructor-slot-initfunction-initialization-form class slot instance-sym initargs-sym))
                     ,instance-sym))
                (union ii-initargs si-initargs)
                (or ii-initargs-aok si-initargs-aok))))))

(defvar *shhh* nil)

(defun property-present-p (place indicator)
  (loop
     for (key value) on place by #'cddr
     when (eql key indicator)
     do (return t)
     finally (return nil)))

(defun compute-constructor-form (class)
  (let ((supplied-initargs (gensym "SUPPLIED-INITARGS"))
        (initargs (gensym "INITARGS"))
        (instance (gensym "INSTANCE")))
    (multiple-value-bind (allocate-form allocate-initargs allocate-aok)
        (compute-constructor-allocate-form class initargs)
      (multiple-value-bind (initialize-form initialize-initargs initialize-aok)
          (compute-constructor-initialization-form class instance initargs)
        (when (or (not allocate-form)
                  (not initialize-form))
          (return-from compute-constructor-form nil))
        (let* ((class-keys (class-slot-initargs class))
               (all-keys (union class-keys (union allocate-initargs initialize-initargs))))
          `(lambda (,supplied-initargs)
             (declare (ignorable ,supplied-initargs)
                      (sys.int::lambda-name (constructor ,class ,all-keys)))
             (let ((,initargs ,supplied-initargs))
               (declare (ignorable ,initargs))
               ;; Default initargs.
               ,@(loop
                    for (initarg form fn) in (class-default-initargs class)
                    collect `(when (not (property-present-p ,supplied-initargs ',initarg))
                               (setf ,initargs (append ,initargs (list ',initarg (funcall ',fn))))))
               ;; Check key validity.
               ,(when (not (or allocate-aok initialize-aok))
                  `(when (not (getf ,initargs :allow-other-keys))
                     (let ((invalid-initargs (loop
                                                for key in ,initargs by #'cddr
                                                when (and (not (eql key :allow-other-keys))
                                                          (not (member key ',all-keys)))
                                                collect key)))
                       (when invalid-initargs
                         (error 'sys.int::simple-program-error
                                :format-control "Invalid initargs ~:S when creating instance of ~S (~S).~%Supplied: ~:S~%valid: ~:S"
                                :format-arguments (list invalid-initargs ',class ',(class-name class) ,initargs ',all-keys))))))
               ;; Allocate & initialize instance.
               (let ((,instance ,allocate-form))
                 ,initialize-form
                 ,instance))))))))

;; A collection of all constructors, used to flush constructors when
;; a method is added to ALLOCATE-INSTANCE.
(sys.int::defglobal *all-constructors* (make-hash-table :synchronized t
                                                        :weakness :key-and-value))

(defclass constructor-cache-flusher ()
  ((%purge :initarg :purge :initform nil :reader constructor-cache-flusher-purge)))

(defun purge-all-constructors ()
  (maphash (lambda (class ctor)
             (declare (ignore ctor))
             (setf (slot-value class 'constructor) nil))
           *all-constructors*)
  (clrhash *all-constructors*))

(defun flush-constructor-cache-recursively (class)
  "Flush constructors from CLASS and all subclasses."
  (setf (slot-value class 'constructor) nil)
  (remhash class *all-constructors*)
  (dolist (subclass (class-direct-subclasses class))
    (flush-constructor-cache-recursively subclass)))

(defmethod update-dependent (metaobject (dependent constructor-cache-flusher) &key ((add-method added-method)) ((remove-method removed-method)) &allow-other-keys)
  (flet ((doit (method)
           (flush-constructor-cache-recursively (first (method-specializers method)))))
    (cond ((constructor-cache-flusher-purge dependent)
           (purge-all-constructors))
          (t
           (when added-method (doit added-method))
           (when removed-method (doit removed-method))))))

(add-dependent #'allocate-instance (make-instance 'constructor-cache-flusher :purge t))
(add-dependent #'initialize-instance (make-instance 'constructor-cache-flusher))
(add-dependent #'shared-initialize (make-instance 'constructor-cache-flusher))

(defvar *compiling-class-constructor* nil)

(sys.int::defglobal *standard-class-constructor-location*)

(defun std-class-constructor (class)
  (let ((ctor (if (standard-class-instance-p class)
                  (standard-instance-access class *standard-class-constructor-location*)
                  (std-slot-value class 'constructor))))
    (cond ((eql ctor :unsupported)
           (return-from std-class-constructor nil))
          (ctor
           (return-from std-class-constructor ctor)))
    (when *compiling-class-constructor*
      (return-from std-class-constructor nil))
    (let ((*compiling-class-constructor* t))
      (ensure-class-finalized class)
      (let ((form (compute-constructor-form class)))
        (cond (form
               (let ((fn (compile nil form)))
                 (setf (gethash class *all-constructors*) fn)
                 (setf (std-slot-value class 'constructor) fn)
                 fn))
              (t
               ;; A constructor can't created for this class.
               (setf (gethash class *all-constructors*) :unsupported)
               (setf (std-slot-value class 'constructor) :unsupported)
               nil))))))

(defgeneric class-constructor (class))

(defmethod class-constructor ((class class))
  nil)

(defmethod class-constructor ((class std-class))
  (std-class-constructor class))

(defun safe-class-constructor (class)
  (if (standard-class-instance-p class)
      (std-class-constructor class)
      (class-constructor class)))
