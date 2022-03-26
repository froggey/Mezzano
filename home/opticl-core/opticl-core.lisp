;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl-core)

(deftype image (&key channels element-type)
  `(simple-array ,element-type
                 ,(if (numberp channels)
                      (if (= channels 1)
                          `(* *)
                          `(* * ,channels))
                      channels)))

(deftype gray-image (&key element-type)
  `(simple-array ,element-type (* *)))

(deftype gray-alpha-image (&key element-type)
  `(simple-array ,element-type (* * 2)))

(deftype rgb-image (&key element-type)
  `(simple-array ,element-type (* * 3)))

(deftype rgba-image (&key element-type)
  `(simple-array ,element-type (* * 4)))

(defmacro define-image-type (name &key channels element-type)
  "Defines a new image type. Under the covers, this results in
evaluation of the appropriate deftype and make-my-image-type
constructor functions. Returns the name of the created
type (i.e. name)."
  (let ((type (read-from-string (format nil "~A" name))))
    (let ((ctor-function
           (read-from-string (format nil "make-~A" type))))
      `(progn
         (deftype ,type () ',(list* 'image
                                    (append
                                     (when channels
                                       `(:channels ,channels))
                                     (when element-type
                                       `(:element-type ,element-type)))))
         (defun ,ctor-function
             (height width &key (initial-element nil initial-element-p)
                                (initial-contents nil initial-contents-p))
           (apply #'make-array (append (list height width)
                                       (when ,(and channels
                                                   (> channels 1))
                                         (list ,channels)))
                  :element-type ',element-type
                  (append
                   (when initial-element-p
                     `(:initial-element ,initial-element))
                   (when initial-contents-p
                     `(:initial-contents ,initial-contents)))))
         ',type))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *image-types*
    '((single-float-image :element-type single-float)
      (double-float-image :element-type double-float)
      
      (1-bit-gray-image :channels 1 :element-type (unsigned-byte 1))
      (2-bit-gray-image :channels 1 :element-type (unsigned-byte 2))
      (4-bit-gray-image :channels 1 :element-type (unsigned-byte 4))
      (8-bit-gray-image :channels 1 :element-type (unsigned-byte 8))
      (16-bit-gray-image :channels 1 :element-type (unsigned-byte 16))
      (32-bit-gray-image :channels 1 :element-type (unsigned-byte 32))
      (fixnum-gray-image :channels 1 :element-type fixnum)
      (single-float-gray-image :channels 1 :element-type single-float)
      (double-float-gray-image :channels 1 :element-type double-float)

      (1-bit-gray-alpha-image :channels 2 :element-type (unsigned-byte 1))
      (2-bit-gray-alpha-image :channels 2 :element-type (unsigned-byte 2))
      (4-bit-gray-alpha-image :channels 2 :element-type (unsigned-byte 4))
      (8-bit-gray-alpha-image :channels 2 :element-type (unsigned-byte 8))
      (16-bit-gray-alpha-image :channels 2 :element-type (unsigned-byte 16))
      (32-bit-gray-alpha-image :channels 2 :element-type (unsigned-byte 32))

      (4-bit-rgb-image :channels 3 :element-type (unsigned-byte 4))
      (8-bit-rgb-image :channels 3 :element-type (unsigned-byte 8))
      (16-bit-rgb-image :channels 3 :element-type (unsigned-byte 16))
      (32-bit-rgb-image :channels 3 :element-type (unsigned-byte 32))
      (fixnum-rgb-image :channels 3 :element-type fixnum)
      (single-float-rgb-image :channels 3 :element-type single-float)
      (double-float-rgb-image :channels 3 :element-type double-float)

      (4-bit-rgba-image :channels 4 :element-type (unsigned-byte 4))
      (8-bit-rgba-image :channels 4 :element-type (unsigned-byte 8))
      (16-bit-rgba-image :channels 4 :element-type (unsigned-byte 16))
      (32-bit-rgba-image :channels 4 :element-type (unsigned-byte 32))
      (fixnum-rgba-image :channels 4 :element-type fixnum)
      (single-float-rgba-image :channels 4 :element-type single-float)
      (double-float-rgba-image :channels 4 :element-type double-float)
      )))

;;
;; to define a new image type one could do:
;;
;; (define-image-type rational-gray-image :channels 1 :element-type rational)
;;

(macrolet
    ((frobber ()
       `(progn
          ,@(loop for image-spec in *image-types*
               collect
                 (destructuring-bind (name &key channels element-type)
                     image-spec
                   `(define-image-type ,name
                        ,@(if channels
                              `(:channels ,channels))
                      ,@(if element-type
                            `(:element-type ,element-type))))))))
  (frobber))

;;; support functions/constants for the pixel setf-expander need to
;;; exist at compile time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %get-array-dimensions-from-type-decl (type-decl)
    "Extract the array dimension specifier from type declaration TYPE-DECL."
    #+(or sbcl ccl)
    (and type-decl
	 ;; here we expect e.g. (TYPE SIMPLE-ARRAY (UNSIGNED-BYTE 8) (* * 3))
	 (listp type-decl)
	 (= (length type-decl) 4)
	 (fourth type-decl))
    #+allegro
    (and type-decl
	 ;; here we expect e.g. (TYPE (SIMPLE-ARRAY (INTEGER 0 255) (* * 3)))
	 (listp type-decl)
	 (= (length type-decl) 2)
	 (= (length (second type-decl)) 3)
	 (third (second type-decl))))
  
  (defun %get-image-dimensions (image-var env)
    #+(or sbcl ccl allegro)
    (when (symbolp image-var)
      (multiple-value-bind (binding-type localp declarations)
          (opticl-cltl2:variable-information image-var env)
        (declare (ignore binding-type localp))
        (let ((type-decl (find 'type declarations :key #'car)))
          (%get-array-dimensions-from-type-decl type-decl)))))

  (defconstant +max-image-channels+ 4))

(define-setf-expander pixel (image-var y x &environment env)
  "Sets the (possibly multiple) image intensity value(s) at position
y, x to the provided value(s). For example, to set pixel [0,0] in an
rgb-image to R20, G40, B60, one would do (setf (pixel img 0 0) (values
20 40 60)). With proper type declarations for images, use of this
macro should yield non-consing setting of image intensity data. "
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion image-var env)
    (declare (ignore newval setter))
    (let ((image-dimensions (%get-image-dimensions getter env)))
      (if image-dimensions
          (let ((arity (or (and (= (length image-dimensions) 3)
                                (third image-dimensions))
                           1))
                (temp-y (gensym))
                (temp-x (gensym)))
            (if (= arity 1)
                (let ((store (gensym)))
                  (values `(,@dummies ,temp-y ,temp-x)
                          `(,@vals ,y ,x)
                          `(,store)
                          `(setf (aref ,getter ,temp-y ,temp-x) ,store)
                          `(aref ,getter ,temp-y ,temp-x)))
                (let ((stores (map-into (make-list arity) #'gensym)))
                  (values `(,@dummies ,temp-y ,temp-x)
                          `(,@vals ,y ,x)
                          stores
                          `(progn (setf ,@(loop for i from 0
                                                for store in stores
                                                collect `(aref ,getter ,temp-y ,temp-x ,i)
                                                collect store))
                                  (values ,@stores))
                          `(values ,@(loop for i from 0 below (length stores)
                                           collect `(aref ,getter ,temp-y ,temp-x ,i)))))))
          (let ((syms (map-into (make-list +max-image-channels+) #'gensym)))
            (let ((temp-y (gensym))
                  (temp-x (gensym)))
              (values `(,@dummies ,temp-y ,temp-x)
                      `(,@vals ,y ,x)
                      syms
                      `(ecase (array-rank ,getter)
                         (3 (let ((d (array-dimension ,getter 2)))
                              (case d
                                (1
                                 (values
                                  (setf (aref ,getter ,temp-y ,temp-x 0) ,(elt syms 0))))
                                (2
                                 (values
                                  (setf (aref ,getter ,temp-y ,temp-x 0) ,(elt syms 0))
                                  (setf (aref ,getter ,temp-y ,temp-x 1) ,(elt syms 1))))
                                (3
                                 (values
                                  (setf (aref ,getter ,temp-y ,temp-x 0) ,(elt syms 0))
                                  (setf (aref ,getter ,temp-y ,temp-x 1) ,(elt syms 1))
                                  (setf (aref ,getter ,temp-y ,temp-x 2) ,(elt syms 2))))
                                (4
                                 (values
                                  (setf (aref ,getter ,temp-y ,temp-x 0) ,(elt syms 0))
                                  (setf (aref ,getter ,temp-y ,temp-x 1) ,(elt syms 1))
                                  (setf (aref ,getter ,temp-y ,temp-x 2) ,(elt syms 2))
                                  (setf (aref ,getter ,temp-y ,temp-x 3) ,(elt syms 3))))
                                (t (loop for i below d
                                         collect (setf (aref ,getter ,temp-y ,temp-x i) (elt (list ,@syms) i)))))))
                         (2 (setf (aref ,getter ,temp-y ,temp-x) ,(elt syms 0))))
                      `(ecase (array-rank ,getter)
                         (3
                          (let ((d (array-dimension ,getter 2)))
                            (case d
                              (1
                               (values
                                (aref ,getter ,temp-y ,temp-x 0)))
                              (2
                               (values
                                (aref ,getter ,temp-y ,temp-x 0)
                                (aref ,getter ,temp-y ,temp-x 1)))
                              (3
                               (values
                                (aref ,getter ,temp-y ,temp-x 0)
                                (aref ,getter ,temp-y ,temp-x 1)
                                (aref ,getter ,temp-y ,temp-x 2)))
                              (4
                               (values
                                (aref ,getter ,temp-y ,temp-x 0)
                                (aref ,getter ,temp-y ,temp-x 1)
                                (aref ,getter ,temp-y ,temp-x 2)
                                (aref ,getter ,temp-y ,temp-x 3)))
                              (t (values-list
                                  (loop for i below d
                                        collect (aref ,getter ,temp-y ,temp-x i)))))))
                         (2 (aref ,getter ,temp-y ,temp-x))))))))))

(defmacro pixel (image-var y x &environment env)
  "Returns the (possibly multiple) image intensity value(s) at
position y, x. With proper type declarations for images, use of this
macro should yield non-consing access to image intensity data. "
  (let ((image-dimensions (%get-image-dimensions image-var env)))
    (if image-dimensions
        (progn
          (ecase (length image-dimensions)
            (2 `(aref ,image-var ,y ,x))
            (3 `(values ,@(loop for i below (third image-dimensions)
                                collect `(aref ,image-var ,y ,x ,i))))))
        `(ecase (array-rank ,image-var)
           (2 (aref ,image-var ,y ,x))
           (3 (ecase (array-dimension ,image-var 2)
                (1 (values
                    (aref ,image-var ,y ,x 0)))
                (2 (values
                    (aref ,image-var ,y ,x 0)
                    (aref ,image-var ,y ,x 1)))
                (3 (values
                    (aref ,image-var ,y ,x 0)
                    (aref ,image-var ,y ,x 1)
                    (aref ,image-var ,y ,x 2)))
                (4 (values
                    (aref ,image-var ,y ,x 0)
                    (aref ,image-var ,y ,x 1)
                    (aref ,image-var ,y ,x 2)
                    (aref ,image-var ,y ,x 3)))))))))

(defun pixel* (image y x)
  "pixel* returns the value(s) at position y, x as a list. This
function conses, but occasionally one wants the image intensity values
as a list, rather than as multiple values. This is a convenience
function to provide this, largely to provide symmetry
with (setf (pixel* ...) ...)"
  (multiple-value-list (pixel image y x)))

(defmacro set-pixel* (image y x list)
  `(setf (pixel ,image ,y ,x) (values-list ,list)))

(defsetf pixel* set-pixel*
  "(setf (pixel* img y x) list) sets the values of pixel y, x in img
to the values contained in list. (setf (pixel ...) ...) is the more
efficient way to do this, but if one wants to pass a set of values as
a list instead of as multiple-values (for named colors perhaps), this
function does that.")

;; Note: CLH 2016-08-21
;;
;; We used to gensym a channels variable that could be used without
;; passsing it into with-image-bounds. That was a bad idea and by not
;; setting channels, we don't try to take the array-dimension that the
;; compiler might know does not exist. Also, channels should really be
;; named sosmething like num-channels.
;;
(defmacro with-image-bounds ((ymax-var xmax-var &optional channels) img
                             &body body
                             &environment env)
  (let ((image-dimensions (%get-image-dimensions img env)))
    `(let ((,ymax-var (array-dimension ,img 0))
           (,xmax-var (array-dimension ,img 1))
           ,@(when channels
               `((,channels ,(when (or (not image-dimensions)
                                       (> (length image-dimensions) 2))
                               `(when (= (array-rank ,img) 3)
                                 (array-dimension ,img 2)))))))
      (declare ,@(when channels
                   `((ignorable ,channels)))
       (type fixnum ,ymax-var)
       (type fixnum ,xmax-var))
      ,@body)))

(defmacro do-pixels ((i-var j-var) image &body body)
  (alexandria:with-gensyms (height width)
    `(with-image-bounds (,height ,width)
         ,image
       (loop for ,i-var fixnum below ,height
             do (loop for ,j-var fixnum below ,width
                      do ,@body)))))

(defmacro set-pixels ((i-var j-var) image &body body)
  (alexandria:with-gensyms (height width)
    `(with-image-bounds (,height ,width)
         ,image
       (loop for ,i-var fixnum below ,height
             do (loop for ,j-var fixnum below ,width
                      do (setf (pixel ,image ,i-var ,j-var)
                               (progn ,@body)))))))

(defmacro do-region-pixels ((i-var j-var y1 x1 y2 x2) image &body body)
  (declare (ignorable image))
  `(loop for ,i-var fixnum from ,y1 below ,y2
         do (loop for ,j-var fixnum from ,x1 below ,x2
                  do ,@body)))

(defmacro set-region-pixels ((i-var j-var y1 x1 y2 x2) image &body body)
  (declare (ignorable image))
  `(loop for ,i-var fixnum from ,y1 below ,y2
         do (loop for ,j-var fixnum from ,x1 below ,x2
                  do (setf (pixel ,image ,i-var ,j-var)
                           (progn
                             ,@body)))))

(defun clear-image (image)
  (with-image-bounds (height width channels)
      image
    (declare (ignore height width))
    (if channels
        (ecase channels
          (2 (set-pixels (i j) image (values 0 0)))
          (3 (set-pixels (i j) image (values 0 0 0)))
          (4 (set-pixels (i j) image (values 0 0 0 0))))
        (set-pixels (i j) image 0)))
  image)

(defun copy-array (src &key (element-type (array-element-type src))
                            (fill-pointer (and (array-has-fill-pointer-p src)
                                               (fill-pointer src)))
                            (adjustable (adjustable-array-p src)))
  "Returns an undisplaced copy of ARRAY, with same fill-pointer and
adjustability (if any) as the original, unless overridden by the keyword
arguments."
  (let ((dims (array-dimensions src)))
    ;; Dictionary entry for ADJUST-ARRAY requires adjusting a
    ;; displaced array to a non-displaced one to make a copy.
    (let* ((src-displaced (make-array (reduce #'* dims)
                                      :displaced-to src
                                      :element-type element-type))
           (dest (make-array dims :element-type element-type
                                  :fill-pointer fill-pointer
                                  :adjustable adjustable))
           (dest-displaced (make-array (reduce #'* dims)
                                       :displaced-to dest
                                       :element-type element-type)))
      (replace dest-displaced src-displaced)
      dest)))
