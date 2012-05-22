(in-package #:sys.int)

(defgeneric funcallable-instance-lambda-expression (function))
(defmethod funcallable-instance-lambda-expression ((function function))
  (values nil t nil))
