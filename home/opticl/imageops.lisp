;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defun sum (array)
  (let ((acc 0))
    (map-array (lambda (v) (incf acc v)) array)
    acc))

(defun sum-range (array vendr vstartr vendc vstartc)
  (let ((acc 0))
    (loop for i from vstartr below vendr
       do (loop for j from vstartc below vendc
             do (incf acc (aref array i j))))
    acc))

(defmacro make-constrain-fn (min max)
  `(lambda (val)
     (cond ((<= val ,min) ,min)
           ((>= val ,max) ,max)
           (t (round val)))))

(defun constrain (val min max)
  (let ((val (if (< val min) min val)))
    (if (> val max)
        max
        val)))

(defun pixel-in-bounds (img y x)
  (with-image-bounds (ymax xmax)
      img
    (and (>= y 0) (< y ymax)
         (>= x 0) (< x xmax))))

(defmacro when-pixel-in-bounds ((img y x) &body body)
  (let ((ymax (gensym)) (xmax (gensym)))
    `(let ((,ymax (1- (array-dimension ,img 0)))
           (,xmax (1- (array-dimension ,img 1))))
       (if (and (<= 0 ,y ,ymax)
                (<= 0 ,x ,xmax))
           ,@body))))

(defun transpose-image (img)
  (typecase img
    (8-bit-gray-image
     (locally
         (declare (type 8-bit-gray-image img))
       (with-image-bounds (ymax xmax)
           img
         (let ((zimg (make-array (list xmax ymax)
                                 :element-type (array-element-type img))))
           (locally
               (declare (type 8-bit-gray-image zimg))
             (loop for i below ymax
                do (loop for j below xmax
                      do (setf (pixel zimg j i) (pixel img i j)))))
           zimg))))
    (16-bit-gray-image
     (locally
         (declare (type 16-bit-gray-image img))
       (with-image-bounds (ymax xmax)
           img
         (let ((zimg (make-array (list xmax ymax)
                                 :element-type (array-element-type img))))
           (locally
               (declare (type 16-bit-gray-image zimg))
             (loop for i below ymax
                do (loop for j below xmax
                      do (setf (pixel zimg j i) (pixel img i j)))))
           zimg))))
    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image img))
       (with-image-bounds (ymax xmax)
           img
         (let ((zimg (make-array (list xmax ymax 3)
                                 :element-type (array-element-type img))))
           (locally
               (declare (type 8-bit-rgb-image zimg))
             (loop for i below ymax
                do (loop for j below xmax
                      do (setf (pixel zimg j i) (pixel img i j)))))
           zimg))))
    (16-bit-rgb-image
     (locally
         (declare (type 16-bit-rgb-image img))
       (with-image-bounds (ymax xmax)
           img
         (let ((zimg (make-array (list xmax ymax 3)
                                 :element-type (array-element-type img))))
           (locally
               (declare (type 16-bit-rgb-image zimg))
             (loop for i below ymax
                do (loop for j below xmax
                      do (setf (pixel zimg j i) (pixel img i j)))))
           zimg))))
    (8-bit-rgba-image
     (locally
         (declare (type 8-bit-rgba-image img))
       (with-image-bounds (ymax xmax)
           img
         (let ((zimg (make-array (list xmax ymax 4)
                                 :element-type (array-element-type img))))
           (locally
               (declare (type 8-bit-rgba-image zimg))
             (loop for i below ymax
                do (loop for j below xmax
                      do (setf (pixel zimg j i) (pixel img i j)))))
           zimg))))
    (16-bit-rgba-image
     (locally
         (declare (type 16-bit-rgba-image img))
       (with-image-bounds (ymax xmax)
           img
         (let ((zimg (make-array (list xmax ymax 4)
                                 :element-type (array-element-type img))))
           (locally
               (declare (type 16-bit-rgba-image zimg))
             (loop for i below ymax
                do (loop for j below xmax
                      do (setf (pixel zimg j i) (pixel img i j)))))
           zimg))))
    (t
     (with-image-bounds (ymax xmax channels)
         img
       (let ((zimg (make-array
                    (cons xmax (cons ymax (when channels (list channels))))
                    :element-type (array-element-type img))))
         (loop for i below ymax
            do (loop for j below xmax
                  do (setf (pixel zimg j i) (pixel img i j))))
         zimg)))))

(defun horizontal-flip-image (img)
  (with-image-bounds (ymax xmax channels)
      img
    (let ((zimg (make-array
                 (cons ymax (cons xmax (when channels (list channels))))
                 :element-type (array-element-type img))))
      (loop for i below ymax
         do (loop for j below xmax
               for jprime from (1- xmax) downto 0
               do (setf (pixel zimg i j) (pixel img i jprime))))
      zimg)))

(defun vertical-flip-image (img)
  (with-image-bounds (ymax xmax channels)
      img
    (let ((zimg (make-array
                 (cons ymax (cons xmax (when channels (list channels))))
                 :element-type (array-element-type img))))
      (loop for i below ymax
         for iprime from (1- ymax) downto 0
         do (loop for j below xmax
               do (setf (pixel zimg iprime j) (pixel img i j))))
      zimg)))

(defun copy-image (img)
  (with-image-bounds (ymax xmax channels)
      img
    (let ((new-image (make-array
                      (cons ymax (cons xmax (when channels (list channels))))
                      :element-type (array-element-type img))))
      (loop for i below ymax
         do (loop for j below xmax
               do (setf (pixel new-image i j) (pixel img i j))))
      new-image)))

(defun crop-image (img y1 x1 y2 x2)
  (with-image-bounds (ymax xmax channels)
      img
    (declare (ignore ymax xmax))
    (let ((new-rows (- y2 y1))
          (new-cols (- x2 x1)))
      (let ((new-image (make-array
                         (cons new-rows (cons new-cols (when channels (list channels))))
                         :element-type (array-element-type img))))
        (loop for i-src from y1 below y2
           for i-dest below new-rows
           do (loop for j-src from x1 below x2
                 for j-dest below new-cols
                 do 
                 (setf (pixel new-image i-dest j-dest) (pixel img i-src j-src))))
        new-image))))

(defun map-array (fn array &key 
                  (element-type (array-element-type array))
                  (adjustable (adjustable-array-p array))
                  (force-simple t))
  (let ((len (reduce #'* (array-dimensions array))))
    (if force-simple
        (let* ((disp (make-array len
                                 :displaced-to array))
               (vec (map `(vector ,element-type) fn disp))
               (dest (make-array (array-dimensions array)
                                 :adjustable adjustable
                                 :element-type element-type)))
          (loop for i below len 
             do (setf (row-major-aref dest i)
                      (elt vec i)))
          dest)
        (let* ((disp (make-array len :displaced-to array)))
          (make-array (array-dimensions array)
                      :element-type element-type
                      :displaced-to (map `(vector ,element-type) fn disp))))))

(defun trim-image (img y-pixels x-pixels)
  (with-image-bounds (height width)
      img
    (crop-image img y-pixels x-pixels (- height y-pixels) (- width x-pixels))))

