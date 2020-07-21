;;;; DESCRIBE and DESCRIBE-OBJECT

(in-package :mezzano.internals)

(defgeneric describe-object (object stream))

(defmethod describe-object (object stream)
  (format stream "~S is a ~:(~A~), with address ~X~%"
          object
          (type-of object)
          (lisp-object-address object)))

(defmethod describe-object ((object symbol) stream)
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
    (format stream "  ~A is declared ~A~%" object (symbol-mode object))))

(defmethod describe-object ((object byte) stream)
  (format stream "~S is a ~S.~%" object (type-of object))
  (format stream "  It has size ~:D.~%" (byte-size object))
  (format stream "  It has position ~:D.~%" (byte-position object)))

(defmethod describe-object ((object character) stream)
  (format stream "~S is a ~S.~%" object (type-of object))
  (format stream "  It has the char-code #x~4,'0X~%" (char-code object))
  (when (char-bit object :control)
    (format stream "  It has the control bit set.~%"))
  (when (char-bit object :meta)
    (format stream "  It has the meta bit set.~%"))
  (when (char-bit object :super)
    (format stream "  It has the super bit set.~%"))
  (when (char-bit object :hyper)
    (format stream "  It has the hyper bit set.~%")))

(defmethod describe-object ((object float) stream)
  (etypecase object
    (short-float
     (format stream "~D is a half-precision floating-point number.~%" object)
     (format stream "  It's representation is ~X.~%"
             (%short-float-as-integer object)))
    (single-float
     (format stream "~D is a single-precision floating-point number.~%" object)
     (format stream "  It's representation is ~X.~%"
             (%single-float-as-integer object)))
    (double-float
     (format stream "~D is a double-precision floating-point number, with address ~X.~%" object (lisp-object-address object))
     (format stream "  It's representation is ~X.~%"
             (%double-float-as-integer object)))))

(defmethod describe-object ((object function) stream)
  (multiple-value-bind (lambda-expression closure-p name)
      (function-lambda-expression object)
    (declare (ignore lambda-expression))
    (format stream "~S is a ~A" object
            (cond (closure-p "closure")
                  ((mezzano.delimited-continuations:delimited-continuation-p object)
                   (if (mezzano.delimited-continuations:resumable-p object)
                       "resumable delimited continuation"
                       "consumed delimited continuation"))
                  (t "function")))
    (when name
      (format stream " named ~S" name))
    (format stream "~%")))

(defmethod describe-object ((object array) stream)
  (format stream "~S is a~A array of ~S, with address ~X~%" object
          (if (typep object 'simple-array) " simple" "n")
          (array-element-type object)
          (lisp-object-address object))
  (format stream "  It is ~D elements long.~%" (array-total-size object))
  (if (array-has-fill-pointer-p object)
      (format stream "  It has a fill-pointer of ~S.~%" (fill-pointer object)))
  (format stream "  It has dimensions ~S.~%" (array-dimensions object)))

(defmethod describe-object ((object complex) stream)
  (format stream "~S is a complex number with an element-type of ~A, with address ~X~%"
          object (second (type-of object)) (lisp-object-address object))
  (format stream "  The real part is ~A~%" (realpart object))
  (format stream "  The imaginary part is ~A~%" (imagpart object)))

(defmethod describe-object ((object ratio) stream)
  (format stream "~S is a ratio, with address ~X~%"
          object (lisp-object-address object))
  (format stream "  The numerator is ~A~%" (numerator object))
  (format stream "  The denominator is ~A~%" (denominator object)))

(defmethod describe-object ((object structure-object) stream)
  (format stream "~S is a structure of type ~:(~S~), with address ~X~%"
          object (type-of object) (lisp-object-address object))
  (loop
     with type = (class-of object)
     for slot in (mezzano.clos:class-slots type)
     for slot-name = (mezzano.clos:slot-definition-name slot)
     for fixed-vector = (mezzano.clos:structure-slot-definition-fixed-vector slot)
     do
       (let ((*print-level* 3)
             (*print-length* 5))
         (format stream "  ~S: ~S~%"
                 slot-name
                 (cond (fixed-vector
                        (let ((vec (make-array fixed-vector)))
                          (dotimes (i fixed-vector)
                            (setf (aref vec i) (%struct-vector-slot object type slot-name i)))
                          vec))
                       (t
                        (%struct-slot object type slot-name)))))))

(defmethod describe-object ((object function-reference) stream)
  (format stream "~S is a function reference named ~S, with address ~X~%"
          object (function-reference-name object) (lisp-object-address object))
  (cond ((function-reference-function object)
         (format stream "  It is bound to ~S.~%"
                 (function-reference-function object)))
        (t (format stream "  It is not bound.~%"))))

(defmethod describe-object ((object weak-pointer) stream)
  (format stream "~S is a weak pointer, with address ~X~%"
          object (lisp-object-address object))
  (format stream "  It has weakness ~:(~S~)~%"
          (weak-pointer-weakness object))
  (multiple-value-bind (key value livep)
      (weak-pointer-pair object)
    (cond ((not livep)
           (format stream "  It is dead.~%"))
          ((eql key value)
           (format stream "  It points to the live value ~S.~%" value))
          (t
           (format stream "  It has the key ~S and points to the live value ~S.~%" key value)))))

(defmethod describe-object ((object integer) stream)
  (cond ((fixnump object)
         (format stream "~D is a fixnum.~%" object))
        (t
         (format stream "~S is a bignum, with address ~X~%"
                 object (lisp-object-address object))
         (format stream "  It is ~D fragments long.~%" (%n-bignum-fragments object))
         (dotimes (i (%n-bignum-fragments object))
           (format stream "  ~D: ~16,'0X~%" i (%bignum-fragment object i))))))

(defmethod describe-object ((object cons) stream)
  ;; TODO: Identify proper/dotted/circular lists.
  (format stream "~S is a list, with address ~X~%"
          object (lisp-object-address object)))

(defmethod describe-object ((object standard-object) stream)
  (format stream "A Closette object~%~
             Printed representation: ~A~%~
             Class: ~S~%~
             Structure~%"
          (handler-case
              (format nil "~S" object)
            (error ()
              (with-output-to-string (s)
                (print-unreadable-object (object s :type t :identity t)
                  (format s "<<error printing object>>")))))
          (class-of object))
  (dolist (sn (mapcar #'mezzano.clos:slot-definition-name
                      (mezzano.clos:class-slots (class-of object))))
    (if (slot-boundp object sn)
        (format stream "    ~S <- ~A~%"
                sn
                (let ((value (slot-value object sn)))
                  (handler-case
                      (format nil "~S" value)
                    (error ()
                      (with-output-to-string (s)
                        (print-unreadable-object (value s :type t :identity t)
                          (format s "<<error printing object>>")))))))
        (format stream "    ~S <- not bound~%" sn))))

(defun describe (object &optional stream)
  (describe-object object (case stream
                            ((nil) *standard-output*)
                            ((t) *terminal-io*)
                            (otherwise stream)))
  (values))
