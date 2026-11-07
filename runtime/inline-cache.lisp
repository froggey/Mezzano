;;;; Inline caches for call-site optimization of function calls.
;;;;
;;;; This is the general-purpose infrastructure for creation and
;;;; management of inline caches.

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

(defun reset-inline-cache (inline-cache)
  (deinitialize-inline-cache inline-cache (inline-cache-function inline-cache))
  (change-class inline-cache 'inline-cache)
  (let ((fref (inline-cache-function-reference inline-cache)))
    (cond (fref
           (let ((new (sys.int::function-reference-function fref)))
             (setf (inline-cache-function inline-cache) new)
             (initialize-inline-cache inline-cache new)))
          (t
           (initialize-inline-cache inline-cache (inline-cache-function inline-cache)))))
  (values))

(defun reset-all-inline-caches ()
  (let ((inline-caches (sys.int::get-all-objects
                        (lambda (x)
                          (typep x 'mezzano.runtime::inline-cache)))))
    (loop for cache across inline-caches
          when (slot-boundp cache '%function)
          do (reset-inline-cache cache)))
  (values))

(defun categorise-inline-caches ()
  (let ((inline-caches (sys.int::get-all-objects
                        (lambda (x)
                          (typep x 'mezzano.runtime::inline-cache))))
        (result (make-hash-table)))
    (loop for cache across inline-caches
          for fn = (sys.int::funcallable-instance-function cache)
          for real-fn = (if (sys.int::closure-p fn)
                            (sys.int::%closure-function fn)
                            fn)
          do (push cache (gethash real-fn result)))
    result))
