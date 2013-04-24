(in-package :sys.int)

(defun describe-symbol (object stream)
  (format stream "~S is a symbol, with address ~X~%" object (lisp-object-address object))
  (if (symbol-package object)
      (format stream "  ~A is in the ~A package~%" object (package-name (symbol-package object))))
  (if (boundp object)
      (format stream "  ~A has the value ~S~%" object (symbol-value object)))
  (if (fboundp object)
      (format stream "  ~A is the function ~S~%" object (symbol-function object)))
  (if (symbol-plist object)
      (format stream "  ~A has the plist ~S~%" object (symbol-plist object)))
  (when (symbol-mode object)
    (format stream "  ~A is declared ~A~%" object (symbol-mode object)))
  (when (symbol-tls-slot object)
    (format stream "  ~A uses the TLS slot ~D~%" (symbol-tls-slot object))))

(defun describe-character (object stream)
  (format stream "~S is a ~S~%" object (type-of object)))

(defun describe-float (object stream)
  (format stream "~D is a single-precision floating-point number.~%" object))

(defun describe-function (object stream)
  (multiple-value-bind (lambda-expression closure-p name)
      (function-lambda-expression object)
    (format stream "~S is a ~A" object (if closure-p "closure" "function"))
    (when name
      (format stream " named ~S" name))
    (format stream "~%")))

(defun describe-array (object stream)
  (format stream "~S is a~A array of ~S, with address ~X~%" object
          (if (typep object 'simple-array) " simple" "n")
          (array-element-type object)
          (lisp-object-address object))
  (format stream "  It is ~D elements long.~%" (array-total-size object))
  (if (array-has-fill-pointer-p object)
      (format stream "  It has a fill-pointer of ~S.~%" (fill-pointer object)))
  (format stream "  It has dimensions ~S.~%" (array-dimensions object)))

(defun describe-bignum (object stream)
  (format stream "~S is a bignum, with address ~X~%"
          object (lisp-object-address object)))

(defun describe-structure (object stream)
  (format stream "~S is a structure of type ~:(~S~), with address ~X~%"
          object (type-of object) (lisp-object-address object))
  (let ((type (%struct-slot object 0)))
    (loop
       for i from 1
       for slot in (structure-slots type) do
         (let ((*print-level* 3)
               (*print-length* 5))
           (format t "  ~S: ~S~%" (first slot) (%struct-slot object i))))))

(defun describe-stack-group (object stream)
  (format stream "~S is a stack-group, with address ~X~%"
          object (lisp-object-address object)))

(defun describe (object &optional (stream *standard-output*))
  (case stream
    ((nil) (setf stream *standard-output*))
    ((t) (setf stream *terminal-io*)))
  (case (logand (lisp-object-address object) 15)
    (#b0000 (format stream "~D is an even fixnum.~%" object))
    ;; TODO: Identify proper/dotted/circular lists.
    (#b0001 (format stream "~S is a list, with address ~X~%" object (lisp-object-address object)))
    (#b0010 (describe-symbol object stream))
    (#b0011 (describe-array object stream)) ; simple array.
    ;; #b0100
    ;; #b0101
    ;; #b0110
    (#b0111 (cond ((structure-object-p object)
                   (describe-structure object stream))
                  ((stack-group-p object)
                   (describe-stack-group object stream))
                  ((std-instance-p object)
                   (describe-object object stream))
                  ((bignump object)
                   (describe-bignum object stream))
                  (t (describe-array object stream))))
    (#b1000 (format stream "~D is an odd fixnum.~%" object))
    ;; #b1001
    (#b1010 (describe-character object stream))
    (#b1011 (describe-float object stream))
    (#b1100 (if (funcallable-std-instance-p object)
                (describe-object object stream)
                (describe-function object stream)))
    ;; #b1101
    (#b1110 (format stream "This is an unbound value marker.~%"))
    (#b1111 (format stream "This is a GC forwarding pointer, pointing to address ~X~%" (logand (lisp-object-address object)
                                                                                               (lognot #xF))))
    (t (format stream "~S is an unknown/invalid object, with address ~X~%" object (lisp-object-address object))))
  (values))
