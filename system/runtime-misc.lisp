(in-package :sys.int)

(fmakunbound 'funcallable-instance-lambda-expression)
(defgeneric funcallable-instance-lambda-expression (function)
  (:method ((function function))
    (declare (ignore function))
    (values nil t nil)))

(fmakunbound 'funcallable-instance-debug-info)
(defgeneric funcallable-instance-debug-info (function)
  (:method ((function function))
    (declare (ignore function))
    nil))

(defgeneric make-load-form (object &optional environment))
