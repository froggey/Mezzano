
(cl:in-package #:opticl-test)

(in-suite :opticl)

(defun test-image (filename)
  (reduce #'merge-pathnames (list filename "test/images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "opticl"))))

(defun output-image (filename)
  (reduce #'merge-pathnames (list filename "test/output/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "opticl"))))

(ensure-directories-exist (output-image ""))

(defun flatten-array (arr)
  (make-array (reduce #'* (array-dimensions arr))
              :displaced-to arr
              :element-type (array-element-type arr)))

(defun array- (arr1 arr2)
  (let ((flat-arr1 (flatten-array arr1))
        (flat-arr2 (flatten-array arr2)))
    (map 'vector #'- flat-arr1 flat-arr2)))

(defun sum-of-element-wise-differences (arr1 arr2)
  (reduce #'+ (array- arr1 arr2)))


;;;;
;;; pixel constructor and accessor tests
(defmacro image-pixel-test (image-constructor
                            &key (height 32)
                                 (width 32)
                                 (initial-element 1))
  `(let ((img (,image-constructor ,height ,width :initial-element ,initial-element))
         (pixel-y (1- ,height))
         (pixel-x (1- ,width)))
    (is (equalp (pixel img pixel-y pixel-x) ,initial-element))
    (is (equalp (pixel* img  pixel-y pixel-x) (multiple-value-list ,initial-element)))))

(defmacro typed-image-pixel-test (image-constructor
                                  image-type
                                  &key (height 32)
                                       (width 32)
                                       (initial-element 1))
  `(let ((img (,image-constructor ,height ,width :initial-element ,initial-element))
         (pixel-y (1- ,height))
         (pixel-x (1- ,width)))
    (declare (type ,image-type img))
    (is (equalp (pixel img pixel-y pixel-x) ,initial-element))
    (is (equalp (pixel* img  pixel-y pixel-x) (multiple-value-list ,initial-element)))))

(defmacro %def-image-test (image-constructor image-type &rest args &key initial-element)
  (declare (ignorable initial-element))
  `(progn
    (test ,(intern (concatenate 'string (symbol-name image-type) "-PIXEL*"))
      (image-pixel-test ,image-constructor ,@args))
    (test ,(intern (concatenate 'string (symbol-name image-type) "-TYPED-PIXEL*"))
      (typed-image-pixel-test ,image-constructor ,image-type ,@args))))

(defmacro def-image-tests (image-test-spec-list)
  `(progn
    ,@(loop for args in image-test-spec-list
         collect
           (destructuring-bind (test-name image-constructor &rest rest-args &key initial-element)
               args
             (declare (ignorable initial-element))
             `(%def-image-test ,test-name ,image-constructor ,@rest-args)))))

(def-image-tests
    ((make-1-bit-gray-image 1-bit-gray-image :initial-element 1)
     (make-2-bit-gray-image 2-bit-gray-image :initial-element 3)
     (make-4-bit-gray-image 4-bit-gray-image :initial-element 15)
     (make-8-bit-gray-image 8-bit-gray-image :initial-element 255)
     (make-16-bit-gray-image 16-bit-gray-image :initial-element #xffff)
     (make-32-bit-gray-image 32-bit-gray-image :initial-element #xffffffff)
     (make-fixnum-gray-image fixnum-gray-image :initial-element 42)
     (make-single-float-gray-image single-float-gray-image :initial-element 1.0s0)
     (make-double-float-gray-image double-float-gray-image :initial-element 1.0d0)))

(def-image-tests
    ((make-4-bit-rgb-image 4-bit-rgb-image :initial-element (values 15 15 15))
     (make-8-bit-rgb-image 8-bit-rgb-image :initial-element (values 255 255 255))
     (make-16-bit-rgb-image 16-bit-rgb-image :initial-element (values #xffff #xffff #xffff))
     (make-32-bit-rgb-image 32-bit-rgb-image :initial-element (values #xffffffff #xffffffff #xffffffff))
     (make-fixnum-rgb-image fixnum-rgb-image :initial-element (values most-positive-fixnum
                                                                      most-positive-fixnum
                                                                      most-positive-fixnum))
     (make-single-float-rgb-image single-float-rgb-image :initial-element (values 1.0s0 1.0s0 1.0s0))
     (make-double-float-rgb-image double-float-rgb-image :initial-element (values 1.0d0 1.0d0 1.0d0))))

(def-image-tests
    ((make-4-bit-rgba-image 4-bit-rgba-image :initial-element (values 15 15 15 15))
     (make-8-bit-rgba-image 8-bit-rgba-image :initial-element (values 255 255 255 255))
     (make-16-bit-rgba-image 16-bit-rgba-image :initial-element (values #xffff #xffff #xffff #xffff))
     (make-32-bit-rgba-image 32-bit-rgba-image :initial-element (values #xffffffff #xffffffff #xffffffff #xffffffff))
     (make-fixnum-rgba-image fixnum-rgba-image :initial-element (values most-positive-fixnum
                                                                        most-positive-fixnum
                                                                        most-positive-fixnum
                                                                        most-positive-fixnum))
     (make-single-float-rgba-image single-float-rgba-image :initial-element (values 1.0s0 1.0s0 1.0s0 1.0s0))
     (make-double-float-rgba-image double-float-rgba-image :initial-element (values 1.0d0 1.0d0 1.0d0 1.0d0))))

