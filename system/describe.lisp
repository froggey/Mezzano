;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
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

(defun describe-thread (object stream)
  (format stream "~S is a thread with address ~X~%"
          object (lisp-object-address object)))

(defun describe-function-reference (object stream)
  (format stream "~S is a function reference named ~S, with address ~X~%"
          object (function-reference-name object) (lisp-object-address object))
  (cond ((function-reference-function object)
         (format stream "  It is bound to ~S.~%"
                 (function-reference-function object)))
        (t (format stream "  It is not bound.~%"))))

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
                      (#.+object-tag-symbol+
                       (describe-symbol object stream))
                      (#.+object-tag-structure-object+
                       (describe-structure object stream))
                      ((#.+object-tag-std-instance+
                        #.+object-tag-funcallable-instance+)
                       (describe-object object stream))
                      (#.+object-tag-thread+
                       (describe-thread object stream))
                      (#.+object-tag-unbound-value+
                       (format stream "This is an unbound value marker.~%"))
                      (#.+object-tag-function-reference+
                       (describe-function-reference object stream))
                      ((#.+object-tag-function+
                        #.+object-tag-closure+)
                       (describe-function object stream))
                      (t (format stream "~S is an unknown/invalid object, with address ~X~%" object (lisp-object-address object))))))))
        (#b1011 (describe-character object stream))
        (#b1101 (describe-float object stream))
        (#b1111 (format stream "This is a GC forwarding pointer, pointing to address ~X~%" (logand (lisp-object-address object)
                                                                                                   (lognot #xF))))
        (t (format stream "~S is an unknown/invalid object, with address ~X~%" object (lisp-object-address object)))))
  (values))
