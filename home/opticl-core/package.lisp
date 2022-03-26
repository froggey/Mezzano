
(cl:defpackage :opticl-core
  (:use #:cl)
  (:export #:image
           #:pixel
           #:pixel*
           
           #:do-pixels
           #:set-pixels
           
           #:do-region-pixels
           #:set-region-pixels

           #:gray-image
           #:1-bit-gray-image #:make-1-bit-gray-image
           #:2-bit-gray-image #:make-2-bit-gray-image
           #:4-bit-gray-image #:make-4-bit-gray-image
           #:8-bit-gray-image #:make-8-bit-gray-image
           #:16-bit-gray-image #:make-16-bit-gray-image
           #:32-bit-gray-image #:make-32-bit-gray-image
           #:fixnum-gray-image #:make-fixnum-gray-image
           #:single-float-gray-image #:make-single-float-gray-image
           #:double-float-gray-image #:make-double-float-gray-image

           #:gray-alpha-image
           #:1-bit-gray-alpha-image #:make-1-bit-gray-alpha-image
           #:2-bit-gray-alpha-image #:make-2-bit-gray-alpha-image
           #:4-bit-gray-alpha-image #:make-4-bit-gray-alpha-image
           #:8-bit-gray-alpha-image #:make-8-bit-gray-alpha-image
           #:16-bit-gray-alpha-image #:make-16-bit-gray-alpha-image
           #:32-bit-gray-alpha-image #:make-32-bit-gray-image

           #:rgb-image
           #:4-bit-rgb-image #:make-4-bit-rgb-image
           #:8-bit-rgb-image #:make-8-bit-rgb-image
           #:16-bit-rgb-image #:make-16-bit-rgb-image
           #:32-bit-rgb-image #:make-32-bit-rgb-image
           #:fixnum-rgb-image #:make-fixnum-rgb-image
           #:single-float-rgb-image #:make-single-float-rgb-image
           #:double-float-rgb-image #:make-double-float-rgb-image

           #:rgba-image
           #:4-bit-rgba-image #:make-4-bit-rgba-image
           #:8-bit-rgba-image #:make-8-bit-rgba-image
           #:16-bit-rgba-image #:make-16-bit-rgba-image
           #:32-bit-rgba-image #:make-32-bit-rgba-image
           #:fixnum-rgba-image #:make-fixnum-rgba-image
           #:single-float-rgba-image #:make-single-float-rgba-image
           #:double-float-rgba-image #:make-double-float-rgba-image

           #:with-image-bounds

           #:do-pixels
           #:set-pixels
           #:do-region-pixels
           #:set-region-pixels
           #:clear-image
           #:copy-array
           ))
#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-cltl2))

(defpackage :opticl-cltl2
  #+sbcl (:import-from :sb-cltl2 :variable-information)
  #+ccl (:import-from :ccl :variable-information)
  #+(or sbcl ccl allegro) (:export :variable-information))

#+allegro
(defun opticl-cltl2:variable-information (symbol &optional env)
  "A CLTL2-signature-compatible version of VARIABLE-INFORMATION on Allegro Common Lisp."
  (multiple-value-bind (binding-type locative decl localp)
      (sys:variable-information symbol env)
    (declare (ignore locative))
    (values binding-type localp decl)))
