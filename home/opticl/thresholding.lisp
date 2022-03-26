
;;;
;;; Thresholding code contributed by Ivan Chernetsky in 2011.
;;; See COPYRIGHT file for details.

(in-package :opticl)

(defun threshold-image (image threshold)
  "Performs simple thresholding of grayscale image and returns a
binarized image of type 1-bit-gray-image. Before thresholding
threshold is coerced to type of image's elements.

An error of type type-error is signaled if image is not of
gray-image type."
  (etypecase image
    (gray-image
     (with-image-bounds (height width) image
       (let ((binary-image (make-1-bit-gray-image height width :initial-element 0))
             (threshold (coerce threshold (array-element-type image))))
         (declare (type gray-image image)
                  (type 1-bit-gray-image binary-image))
         (do-pixels (i j) image
           (when (>= (pixel image i j) threshold)
             (setf (pixel binary-image i j) 1)))
         binary-image)))))

;; Below is an implementation of minimum error thresholding

(defconstant +8-bit-values-count+ 256
  "Number of different values an element of 8-bit-gray-image
image can be of.")

(defconstant +8-bit-max-value+ 255
  "Maximum value an element of 8-bit-gray-image image can be.")

(defun compute-histogram (image)
  "Computes a normalized histogram of 8-bit-gray-image image,
i.e. an estimate of the probability density function of gray
levels, and returns it."
  (declare (type 8-bit-gray-image image))
  (let ((histogram (make-array +8-bit-values-count+
                               :element-type 'double-float
                               :initial-element 0d0)))
    (do-pixels (i j) image
      (incf (aref histogram (pixel image i j))))
    (with-image-bounds (height width) image
      (let ((size (* height width)))
        (loop for i below +8-bit-values-count+
           do (setf (aref histogram i)
                    (/ (aref histogram i) size)))))
    histogram))

(defconstant +j-fn-max-value+ most-positive-double-float
  "A constant that is used as a default value of J(T) function,
if its value cannot be calculated for a given T.")

(defun sample-j-fn (histogram)
  "Returns an array of values of J(T) for T in [0; 255].
The elements at indexex 0 and 255 are set to +j-fn-max-value+."
  (declare (type (simple-array double-float (256)) histogram))
  (let* ((histlen (length histogram))
         (samples (make-array histlen :element-type 'double-float
                              :initial-element +j-fn-max-value+))
         (apriori-1 (aref histogram 0)))
    (declare (type double-float apriori-1))
    (loop for idx from 1 upto (- histlen 2) do
         (progn
           (incf apriori-1 (aref histogram idx))
           (if (zerop apriori-1)
               (setf (aref samples idx) +j-fn-max-value+)
               (let* ((apriori-2 (- 1d0 apriori-1))
                      (mean-1 (/ (loop for i upto idx
                                    summing (* (aref histogram i) i))
                                 apriori-1))
                      (mean-2 (/ (loop for i from (1+ idx) to (- histlen 1)
                                    summing (* (aref histogram i) i))
                                 apriori-2))
                      (variance-1 (/ (loop for i upto idx
                                        summing (* (expt (- i mean-1) 2)
                                                   (aref histogram i)))
                                     apriori-1))
                      (variance-2 (/ (loop for i from (1+ idx) to (- histlen 1)
                                        summing (* (expt (- i mean-2) 2)
                                                   (aref histogram i)))
                                     apriori-2)))
                 (declare (type double-float apriori-2 mean-1 mean-2 variance-1 variance-2))
                 (if (or (zerop variance-1) (zerop variance-2))
                     (setf (aref samples idx) +j-fn-max-value+)
                     (let ((deviation-1 (sqrt variance-1))
                           (deviation-2 (sqrt variance-2)))
                       (declare (type double-float deviation-1 deviation-2))
                       (setf (aref samples idx)
                             (+ 1
                                (* 2 (+ (* apriori-1 (log deviation-1))
                                        (* apriori-2 (log deviation-2))))
                                (* -2 (+ (* apriori-1 (log apriori-1))
                                         (* apriori-2 (log apriori-2))))))))))))

    samples))

(defun min-error-threshold-image (image)
  "Binarize 8-bit-gray-image image with an automatically guessed
threshold. Returns multiple values: 1-bit-gray-image image and a guessed
threshold.

For further details, please refer 'Minumum error thresholding' by
J. Kittler and J. Illingworth."
  (etypecase image
    (8-bit-gray-image
     (let* ((histogram (compute-histogram image))
            (j-fn-samples (sample-j-fn histogram))
            (min-sample (reduce #'min j-fn-samples))
            (threshold (position min-sample j-fn-samples)))
       (values (the 1-bit-gray-image (threshold-image image (truncate threshold)))
               threshold)))))
