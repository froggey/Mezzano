
(in-package :opticl)

(defun make-gamma-curve-lookup-table (gamma &key (bits 16))
  "Returns an array of length 2^bits of type unsigned-byte of
length bits that contains where the kth element contains the
value (k/2^bits-1)^gamma * 2^bits-1. The resulting curve can be
used by the apply-gamma-curve to apply a gamma curve to an image
using a lookup table of gamma values, rather than computing the
appropriate value for each pixel."
  (let ((curve (make-array `(,(ash 1 bits)) :element-type `(unsigned-byte ,bits)))
        (maxval (1- (ash 1 bits))))
    (let ((maxdouble (float maxval 0d0)))
      (loop for i from 0 to maxval
         do 
         (let ((val (truncate (* (expt (/ i maxdouble) gamma) maxdouble))))
           (setf (aref curve i)
                 (cond ((> 0 val) 0)
                       ((> val maxval) maxval)
                       (t val))))))
    curve))

(defun apply-gamma-curve-lookup-table (image gamma-curve)
  "applys a gamma curve (usually created with
  make-gamma-curve-lookup-table to perform a gamma curve
  operation on an image by looking up the values in a lookup
  table, rather than computing them for eacho pixel"
  (let ((image2 (copy-image image)))
    (with-image-bounds (height width)
        image
      (loop for i from 0 below height
         do (loop for j from 0 below width
               do (let ((vals (multiple-value-list (pixel image i j))))
                    (setf (pixel image2 i j)
                          (values-list
                           (map 'list 
                                (lambda (v)
                                  (aref gamma-curve v))
                                vals)))))))
    image2))

(defun apply-gamma (image gamma)
  (let* ((bits (let ((type (array-element-type image)))
                 (when type (cadr type)))) 
         (gamma-curve
          (make-gamma-curve-lookup-table gamma
                                         :bits (or bits 8))))
    (apply-gamma-curve-lookup-table image gamma-curve)))
