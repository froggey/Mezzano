
(in-package :opticl)

(defun mean (&rest numbers)
  (/ (apply #'+ numbers) (length numbers)))

(defgeneric coerce-image (image type &key &allow-other-keys)
  (:documentation "attempts to coerce a given image into the specified type."))

(defmethod coerce-image (image (type (eql '8-bit-gray-image)) &rest args)
  (declare (ignore args))

  (etypecase image
    (8-bit-gray-image image)
    (1-bit-gray-image 
     (with-image-bounds (y x) image
       (let* ((gray-image (make-8-bit-gray-image y x)))
         (declare (type 8-bit-gray-image gray-image)
                  (type 1-bit-gray-image image))
         (do-pixels (i j) image
           (setf (pixel gray-image i j)
                 (if (plusp (pixel image i j)) 255 0)))
         gray-image)))
    (32-bit-gray-image
     (with-image-bounds (y x) image
       (let ((gray-image (make-8-bit-gray-image y x)))
         (declare (type 8-bit-gray-image gray-image)
                  (type 32-bit-gray-image image))
         (do-pixels (i j)
             image
           (setf (pixel gray-image i j) (pixel image i j)))
         gray-image)))
    (fixnum-gray-image
     (with-image-bounds (y x) image
       (let ((gray-image (make-8-bit-gray-image y x)))
         (declare (type 8-bit-gray-image gray-image)
                  (type fixnum-gray-image image))
         (do-pixels (i j)
             image
           (setf (pixel gray-image i j) (pixel image i j)))
         gray-image)))
    (rgb-image
     (with-image-bounds (y x channels) image
       (let* ((type (array-element-type image))
              (gray-image (make-8-bit-gray-image y x)))
         (declare (type 8-bit-gray-image gray-image)
                  (type rgb-image image))
         (if (subtypep type 'integer)
             (do-pixels (i j)
                 image
               (multiple-value-bind (r g b)
                   (pixel image i j)
                 (setf (pixel gray-image i j)
                       (round (mean r g b)))))
             (do-pixels (i j)
                 image
               (multiple-value-bind (r g b)
                   (pixel image i j)
                 (setf (pixel gray-image i j)
                       (coerce (round (mean r g b)) type)))))
         gray-image)))
    (rgba-image
     (with-image-bounds (y x channels) image
       (let* ((type (array-element-type image))
              (gray-image (make-8-bit-gray-image y x)))
         (declare (type 8-bit-gray-image gray-image)
                  (type rgba-image image))
         (if (subtypep type 'integer)
             (do-pixels (i j)
                 image
               (multiple-value-bind (r g b)
                   (pixel image i j)
                 (setf (pixel gray-image i j)
                       (round (mean r g b)))))
             (do-pixels (i j)
                 image
               (multiple-value-bind (r g b)
                   (pixel image i j)
                 (setf (pixel gray-image i j)
                       (coerce (round (mean r g b)) type)))))
         gray-image)))))

(defmethod coerce-image (image (type (eql 'gray-image)) &key preserve-luminance &allow-other-keys)
  (etypecase image
    (gray-image image)
    (rgb-image
     (with-image-bounds (y x)
         image
       (let* ((type (array-element-type image))
              (gray-image (make-array (list y x) :element-type type)))
         (declare (type gray-image gray-image)
                  (type rgb-image image))
         (if preserve-luminance

             (if (subtypep type 'integer)
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (round 
                            (+ (* r 0.2989)
                               (* g 0.5870)
                               (* b 0.1140))))))
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (coerce (round 
                                    (+ (* r 0.2989)
                                       (* g 0.5870)
                                       (* b 0.1140))) type)))))
             (if (subtypep type 'integer)
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (round (mean r g b)))))
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (coerce (round (mean r g b)) type))))))
         gray-image)))
    (rgba-image
     ;; FIXME! I should do something with the alpha channel instead of
     ;; silently ignoring it!
     (with-image-bounds (y x)
         image
       (let* ((type (array-element-type image))
              (gray-image (make-array (list y x) :element-type type)))
         (declare (type gray-image gray-image)
                  (type rgba-image image))
         (if preserve-luminance

             (if (subtypep type 'integer)
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (round 
                            (+ (* r 0.2989)
                               (* g 0.5870)
                               (* b 0.1140))))))
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (coerce (round 
                                    (+ (* r 0.2989)
                                       (* g 0.5870)
                                       (* b 0.1140))) type)))))
             (if (subtypep type 'integer)
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (round (mean r g b)))))
                 (do-pixels (i j)
                     image
                   (multiple-value-bind (r g b)
                       (pixel image i j)
                     (setf (pixel gray-image i j)
                           (coerce (round (mean r g b)) type))))))
         gray-image)))))

(defmethod coerce-image (image (type (eql 'rgb-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    (gray-image
     (locally
         (declare (type gray-image image))
       (with-image-bounds (y x)
           image
         (let* ((type (array-element-type image))
                (rgb-image (make-array (list y x 3) :element-type type)))
           (declare (type rgb-image rgb-image))
           (do-pixels (i j)
               image
             (let ((val (pixel image i j)))
               (setf (pixel rgb-image i j)
                     (values val val val))))
           rgb-image))))
    (rgb-image image)
    (rgba-image
     (locally
         (declare (type rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((type (array-element-type image))
                (rgb-image (make-array (list y x 3) :element-type type)))
           (do-pixels (i j)
               image
             (setf (pixel rgb-image i j)
                   (pixel image i j)))
           rgb-image))))))

(defmethod coerce-image (image (type (eql '8-bit-rgb-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    (gray-image
     (locally
         (declare (type gray-image image))
       (with-image-bounds (y x)
           image
         (let* ((8-bit-rgb-image (make-8-bit-rgb-image y x)))
           (declare (type 8-bit-rgb-image 8-bit-rgb-image))
           (do-pixels (i j)
               image
             (let ((val (pixel image i j)))
               (setf (pixel 8-bit-rgb-image i j)
                     (values val val val))))
           8-bit-rgb-image))))
    (8-bit-rgb-image image)
    (8-bit-rgba-image
     (locally
         (declare (type 8-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgb-image (make-8-bit-rgb-image y x)))
           (declare (type 8-bit-rgb-image 8-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b a)
                 (pixel image i j)
               (declare (ignore a))
               (setf (pixel 8-bit-rgb-image i j)
                     (values r g b))))
           8-bit-rgb-image))))
    (16-bit-rgb-image
     (locally
         (declare (type 16-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgb-image (make-8-bit-rgb-image y x)))
           (declare (type 8-bit-rgb-image 8-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 8-bit-rgb-image i j)
                     (values (ash r -8) (ash g -8) (ash b -8)))))
           8-bit-rgb-image))))
    (16-bit-rgba-image
     (locally
         (declare (type 16-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgb-image (make-8-bit-rgb-image y x)))
           (declare (type 8-bit-rgb-image 8-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b a)
                 (pixel image i j)
               (declare (ignore a))
               (setf (pixel 8-bit-rgb-image i j)
                     (values (ash r -8) (ash g -8) (ash b -8)))))
           8-bit-rgb-image))))))

(defmethod coerce-image (image (type (eql '8-bit-rgba-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    (gray-image
     (locally
         (declare (type gray-image image))
       (with-image-bounds (y x)
           image
         (let* ((type (array-element-type image))
                (rgba-image (make-array (list y x 4) :element-type type)))
           (declare (type rgba-image rgba-image))
           (do-pixels (i j)
               image
             (let ((val (pixel image i j)))
               (setf (pixel rgba-image i j)
                     (values val val val 255))))
           rgba-image))))
    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgba-image (make-8-bit-rgba-image y x)))
           (declare (type 8-bit-rgba-image 8-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 8-bit-rgba-image i j)
                     (values r g b #xff))))
           8-bit-rgba-image))))
    (8-bit-rgba-image image)
    (16-bit-rgb-image
     (locally
         (declare (type 16-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgba-image (make-8-bit-rgba-image y x)))
           (declare (type 8-bit-rgba-image 8-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 8-bit-rgba-image i j)
                     (values (ash r -8) (ash g -8) (ash b -8) #xff))))
           8-bit-rgba-image))))
    (16-bit-rgba-image
     (locally
         (declare (type 16-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((8-bit-rgba-image (make-8-bit-rgba-image y x)))
           (declare (type 8-bit-rgba-image 8-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b a)
                 (pixel image i j)
               (setf (pixel 8-bit-rgba-image i j)
                     (values (ash r -8) (ash g -8) (ash b -8) (ash a -8)))))
           8-bit-rgba-image))))
    (rgb-image
     (locally
         (declare (type rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((type (array-element-type image))
                (rgba-image (make-array (list y x 4) :element-type type)))
           (declare (type rgba-image rgba-image))
           (do-pixels (i j)
               image
             (setf (pixel* rgba-image i j)
                   (append (multiple-value-list (pixel image i j))
                           (list 255))))
           rgba-image))))
    (rgba-image image)))

(defmethod coerce-image (image (type (eql '16-bit-rgb-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    (gray-image
     (locally
         (declare (type gray-image image))
       (with-image-bounds (y x)
           image
         (let* ((16-bit-rgb-image (make-16-bit-rgb-image y x)))
           (declare (type 16-bit-rgb-image 16-bit-rgb-image))
           (do-pixels (i j)
               image
             (let ((val (ash (pixel image i j) 8)))
               (setf (pixel 16-bit-rgb-image i j)
                     (values val val val))))
           16-bit-rgb-image))))
    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgb-image (make-16-bit-rgb-image y x)))
           (declare (type 16-bit-rgb-image 16-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 16-bit-rgb-image i j)
                     (values (ash r 8) (ash g 8) (ash b 8)))))
           16-bit-rgb-image))))
    (8-bit-rgba-image
     (locally
         (declare (type 8-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgb-image (make-16-bit-rgb-image y x)))
           (declare (type 16-bit-rgb-image 16-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 16-bit-rgb-image i j)
                     (values (ash r 8) (ash g 8) (ash b 8)))))
           16-bit-rgb-image))))
    (16-bit-rgba-image
     (locally
         (declare (type 16-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgb-image (make-16-bit-rgb-image y x)))
           (declare (type 16-bit-rgb-image 16-bit-rgb-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b a)
                 (pixel image i j)
               (declare (ignore a))
               (setf (pixel 16-bit-rgb-image i j)
                     (values r g b))))
           16-bit-rgb-image))))))

(defmethod coerce-image (image (type (eql '16-bit-rgba-image)) &rest args)
  (declare (ignore args))
  (etypecase image
    (gray-image
     (locally
         (declare (type gray-image image))
       (with-image-bounds (y x)
           image
         (let* ((16-bit-rgba-image (make-16-bit-rgba-image y x)))
           (declare (type 16-bit-rgba-image 16-bit-rgba-image))
           (do-pixels (i j)
               image
             (let ((val (ash (pixel image i j) 8)))
               (setf (pixel 16-bit-rgba-image i j)
                     (values val val val #xffff))))
           16-bit-rgba-image))))
    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgba-image (make-16-bit-rgba-image y x)))
           (declare (type 16-bit-rgba-image 16-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 16-bit-rgba-image i j)
                     (values (ash r 8) (ash g 8) (ash b 8) #xffff))))
           16-bit-rgba-image))))
    (8-bit-rgba-image
     (locally
         (declare (type 8-bit-rgba-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgba-image (make-16-bit-rgba-image y x)))
           (declare (type 16-bit-rgba-image 16-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b a)
                 (pixel image i j)
               (setf (pixel 16-bit-rgba-image i j)
                     (values (ash r 8) (ash g 8) (ash b 8) (ash a 8)))))
           16-bit-rgba-image))))
    (16-bit-rgb-image
     (locally
         (declare (type 16-bit-rgb-image image))
       (with-image-bounds (y x channels)
           image
         (let* ((16-bit-rgba-image (make-16-bit-rgba-image y x)))
           (declare (type 16-bit-rgba-image 16-bit-rgba-image))
           (do-pixels (i j)
               image
             (multiple-value-bind (r g b)
                 (pixel image i j)
               (setf (pixel 16-bit-rgba-image i j)
                     (values r g b #xffff))))
           16-bit-rgba-image))))
    (16-bit-rgba-image image)))

(defmethod coerce-image (image (type (eql 'rgba-image)) &rest args)
  (apply #'coerce-image image '8-bit-rgba-image args))

;;;
;;; deprecated convert functions
(defun convert-image-to-8-bit-grayscale (image)
  (coerce image '8-bit-gray-image))

(defun convert-image-to-grayscale (image)
  (coerce-image image 'gray-image))

(defun convert-image-to-grayscale-luminance (image)
  (coerce-image image 'gray-image :preserve-luminance t))

(defun convert-image-to-rgb (image)
  (coerce-image image 'rgb-image))

(defun convert-image-to-rgba (image)
  (coerce-image image 'rgba-image))

