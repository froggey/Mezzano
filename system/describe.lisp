;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defgeneric describe-object (object stream))

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
    (format stream "~S is a ~A" object (if closure-p "closure" "function"))
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
  (let ((type (%struct-slot object 0)))
    (loop
       for i from 1
       for slot in (structure-slots type) do
         (let ((*print-level* 3)
               (*print-length* 5))
           (format stream "  ~S: ~S~%" (structure-slot-name slot) (%struct-slot object i))))))

(defmethod describe-object ((object mezzano.supervisor:thread) stream)
  (format stream "~S is a thread with address ~X~%"
          object (lisp-object-address object))
  (format stream "  It is named ~S~%" (mezzano.supervisor:thread-name object))
  (format stream "  It is in the ~S state~%" (mezzano.supervisor:thread-state object))
  (format stream "  It has priority ~S~%" (mezzano.supervisor:thread-priority object)))

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
  (multiple-value-bind (value livep)
      (weak-pointer-value object)
    (if livep
        (format stream "  It points to the live value ~S.~%" value)
        (format stream "  It is dead.~%"))))

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
             Printed representation: ~S~%~
             Class: ~S~%~
             Structure~%"
          object
          (class-of object))
  (dolist (sn (mapcar #'mezzano.clos:slot-definition-name
                      (mezzano.clos:class-slots (class-of object))))
    (if (slot-boundp object sn)
        (format stream "    ~S <- ~S~%"
                sn
                (slot-value object sn))
        (format stream "    ~S <- not bound~%" sn))))

(defun describe (object &optional stream)
  (describe-object object (case stream
                            ((nil) *standard-output*)
                            ((t) *terminal-io*)
                            (otherwise stream)))
  (values))
