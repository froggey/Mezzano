(in-package #:sys.int)

(define-condition unknown-coercion ()
  ((object :initarg :object
           :reader unknown-coercion-object)
   (type :initarg :type
         :reader unknown-coercion-type))
  (:report (lambda (condition stream)
             (format stream "Don't know how to coerce ~S to type ~S."
                     (unknown-coercion-object condition)
                     (unknown-coercion-type condition)))))

(defun coerce (object result-type)
  (when (or (eql result-type 't)
            (typep object result-type))
    (return-from coerce object))
  (cond ((subtypep result-type 'list)
         (map 'list 'identity object))
        ((subtypep result-type 'vector)
         (check-type object sequence)
         (let ((element-type (parse-array-type (typeexpand result-type))))
           (make-array (length object)
                       :element-type element-type
                       :initial-contents object)))
        ((subtypep result-type 'float)
         (float object))
        (t (error 'unknown-coercion :object object :type result-type))))
