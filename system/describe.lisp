;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

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
    (format stream "  ~A uses the TLS slot ~D~%" object (symbol-tls-slot object))))

(defun describe-byte-specifier (object stream)
  (format stream "~S is a ~S.~%" object (type-of object))
  (format stream "  It has size ~:D.~%" (byte-size object))
  (format stream "  It has position ~:D.~%" (byte-position object)))

(defun describe-character (object stream)
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
          object (lisp-object-address object))
  (format stream "  It is ~D fragments long.~%" (%n-bignum-fragments object))
  (dotimes (i (%n-bignum-fragments object))
    (format stream "  ~D: ~16,'0X~%" i (%bignum-fragment object i))))

(defun describe-complex (object stream)
  (format stream "~S is a complex with an element-type of ~A, with address ~X~%"
          object (second (type-of object)) (lisp-object-address object))
  (format stream "  The real part is ~A~%" (realpart object))
  (format stream "  The imaginary part is ~A~%" (imagpart object)))

(defun describe-ratio (object stream)
  (format stream "~S is a ratio, with address ~X~%"
          object (lisp-object-address object))
  (format stream "  The numerator is ~A~%" (numerator object))
  (format stream "  The denominator is ~A~%" (denominator object)))

(defun describe-structure (object stream)
  (format stream "~S is a structure of type ~:(~S~), with address ~X~%"
          object (type-of object) (lisp-object-address object))
  (let ((type (%struct-slot object 0)))
    (loop
       for i from 1
       for slot in (structure-slots type) do
         (let ((*print-level* 3)
               (*print-length* 5))
           (format t "  ~S: ~S~%" (structure-slot-name slot) (%struct-slot object i))))))

(defun describe-thread (object stream)
  (format stream "~S is a thread with address ~X~%"
          object (lisp-object-address object))
  (format stream "  It is named ~S~%" (mezzano.supervisor:thread-name object))
  (format stream "  It is in the ~S state~%" (mezzano.supervisor:thread-state object))
  (format stream "  It has priority ~S~%" (mezzano.supervisor:thread-priority object)))

(defun describe-function-reference (object stream)
  (format stream "~S is a function reference named ~S, with address ~X~%"
          object (function-reference-name object) (lisp-object-address object))
  (cond ((function-reference-function object)
         (format stream "  It is bound to ~S.~%"
                 (function-reference-function object)))
        (t (format stream "  It is not bound.~%"))))

(defun describe-weak-pointer (object stream)
  (format stream "~S is a weak pointer, with address ~X~%"
          object (lisp-object-address object))
  (multiple-value-bind (value livep)
      (weak-pointer-value object)
    (if livep
        (format stream "  It points to the live value ~S.~%" value)
        (format stream "  It is dead.~%"))))

(defun describe (object &optional (stream *standard-output*))
  (case stream
    ((nil) (setf stream *standard-output*))
    ((t) (setf stream *terminal-io*)))
  (if (fixnump object)
      (format stream "~D is a fixnum.~%" object)
      (case (logand (lisp-object-address object) 15)
        ;; TODO: Identify proper/dotted/circular lists.
        (#b0011 (format stream "~S is a list, with address ~X~%" object (lisp-object-address object)))
        (#b1001
         (let ((otag (%object-tag object)))
           (cond ((or (<= otag +last-simple-1d-array-object-tag+)
                      (<= +first-complex-array-object-tag+
                          otag
                          +last-complex-array-object-tag+))
                  (describe-array object stream))
                 (t (case otag
                      (#.+object-tag-bignum+
                       (describe-bignum object stream))
                      ((#.+object-tag-complex-rational+
                        #.+object-tag-complex-single-float+
                        #.+object-tag-complex-double-float+
                        #.+object-tag-complex-short-float+
                        #.+object-tag-complex-long-float+)
                       (describe-complex object stream))
                      (#.+object-tag-ratio+
                       (describe-ratio object stream))
                      (#.+object-tag-symbol+
                       (describe-symbol object stream))
                      (#.+object-tag-structure-object+
                       (if (bytep object)
                           (describe-byte-specifier object stream)
                           (describe-structure object stream)))
                      ((#.+object-tag-std-instance+
                        #.+object-tag-funcallable-instance+)
                       (describe-object object stream))
                      (#.+object-tag-thread+
                       (describe-thread object stream))
                      (#.+object-tag-unbound-value+
                       (format stream "This is an unbound value marker.~%"))
                      (#.+object-tag-function-reference+
                       (describe-function-reference object stream))
                      (#.+object-tag-weak-pointer+
                       (describe-weak-pointer object stream))
                      ((#.+object-tag-function+
                        #.+object-tag-closure+)
                       (describe-function object stream))
                      (t (format stream "~S is an unknown/invalid object, with address ~X~%" object (lisp-object-address object))))))))
        (#b0111 (describe-byte-specifier object stream))
        (#b1011 (describe-character object stream))
        (#b1101 (describe-float object stream))
        (#b1111 (format stream "This is a GC forwarding pointer, pointing to address ~X~%" (logand (lisp-object-address object)
                                                                                                   (lognot #xF))))
        (t (format stream "~S is an unknown/invalid object, with address ~X~%" object (lisp-object-address object)))))
  (values))
