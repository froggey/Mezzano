;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package :opticl)

;; normally we'd take the sqrt here, but since we're just comparing
;; distances, we can compare the squared distances, so we'll elide the
;; sqrt call.
(defun l2-distance (pixel1 pixel2)
  (reduce #'+ (map 'vector (lambda (p q)
                             (let ((d (- p q)))
                               (* d d)))
                   pixel1 pixel2)))

(defun l2-distance-3 (pixel1a pixel1b pixel1c pixel2a pixel2b pixel2c)
  (declare (type fixnum pixel1a pixel1b pixel1c pixel2a pixel2b pixel2c)
           (optimize (speed 3) (safety 0)))
  (let ((d1 (- pixel1a pixel2a))
        (d2 (- pixel1b pixel2b))
        (d3 (- pixel1c pixel2c)))
    (declare (type fixnum d1 d2 d3))
    (the fixnum (+ (the fixnum (* d1 d1))
                   (the fixnum (* d2 d2))
                   (the fixnum (* d3 d3))))))

(declaim (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum) fixnum) l2-distance-3))

(defun k-means-cluster-image-pixels (image k &key (max-iterations 20))
  (declare (type fixnum k))
  (typecase image
    (8-bit-gray-image
     (locally
         (declare (type 8-bit-gray-image image))
       (with-image-bounds (height width channels)
           image
         (let ((means (make-array k :element-type '(unsigned-byte 32) :initial-element 0))
               (counts (make-array k :element-type 'fixnum))
               (z (make-array (list height width) :element-type 'fixnum)))
           (declare (type (simple-array (unsigned-byte 32) (*)) means)
                    (type (simple-array fixnum (*)) counts)
                    (type (simple-array fixnum (* *)) z))
           (flet ((recompute-means ()
                    ;; clear out the old values
                    (declare (optimize (speed 3))
                             (type 8-bit-gray-image image)
                             (type fixnum k))
                    (dotimes (q k)
                      (declare (type fixnum q))
                      (setf (aref means q) 0)
                      (setf (aref counts q) 0))
                  
                    ;; use the means vector first as an accumulator to hold
                    ;; the sums for each channel, later we'll scale by (/
                    ;; num-pixels)
                    (do-pixels (i j) image
                      (let ((m (aref z i j)))
                        (declare (type fixnum m))
                        (setf (aref means m)
                              (+ (pixel image i j) (aref means m)))
                        (let* ((cluster (aref z i j))
                               (cluster-count (aref counts cluster)))
                          (setf (aref counts cluster)
                                (logand most-positive-fixnum (1+ cluster-count))))))
                    (dotimes (q k)
                      (when (plusp (aref counts q))
                        (setf (aref means q)
                              (round (/ (aref means q) (aref counts q)))))))
             
                  (assign-to-means ()
                    (declare (optimize (speed 3))
                             (type 8-bit-gray-image image))
                    (do-pixels (i j) image
                      (setf (aref z i j)
                            (let (min-val nearest-mean)
                              (loop for q below k
                                 do (let ((dist (let ((d (- (pixel image i j) (aref means q))))
                                                  (declare (type (signed-byte 16) d))
                                                  (the fixnum (* d d)))))
                                      (when (or (null min-val) (< dist min-val))
                                        (setf min-val dist
                                              nearest-mean q))))
                              nearest-mean)))))
        
             ;; randomly assign pixel values to the k means
             (loop for i below k
                for y = (random height)
                for x = (random width)
                do (setf (aref means i)
                         (pixel image y x)))

             (loop for iter below max-iterations
                with stop = nil
                with old-means
                until stop
                do
                  (assign-to-means)
                  (recompute-means)
                  (when (and old-means (equalp old-means means))
                    (setf stop t))
                  (setf old-means (copy-array means)))
             (values means z))))))
    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image image))
       (with-image-bounds (height width channels)
           image
         (let ((means (make-array (list k 1 3)
                                  :element-type '(unsigned-byte 32)))
               (counts (make-array k :element-type 'fixnum))
               (z (make-array (list height width) :element-type 'fixnum)))
           (declare (type (simple-array fixnum (* *)) z)
                    (type 32-bit-rgb-image means))
           (flet ((recompute-means ()
                    (declare (type (simple-array fixnum (* *)) z)
                             (type (simple-array fixnum (*)) counts)
                             (optimize (speed 3)))
                    ;; clear out the old values
                    (dotimes (q k)
                      (setf (pixel means q 0) (values 0 0 0))
                      (setf (aref counts q) 0))
                  
                    ;; use the means vector first as an accumulator to hold
                    ;; the sums for each channel, later we'll scale by (/
                    ;; num-pixels)
                    (do-pixels (i j) image
                      (let ((m (aref z i j)))
                        (multiple-value-bind (v1 v2 v3)
                            (pixel image i j)
                          (multiple-value-bind (m1 m2 m3)
                              (pixel means m 0)
                             (setf (pixel means m 0)
                                  (values
                                   (+ v1 m1)
                                   (+ v2 m2)
                                   (+ v3 m3)))))
                        (let* ((cluster (aref z i j))
                               (cluster-count (aref counts cluster)))
                          (setf (aref counts cluster)
                                (logand #xffffffff (1+ cluster-count))))))
                    (dotimes (q k)
                      (when (plusp (aref counts q))
                        (multiple-value-bind (m1 m2 m3)
                            (pixel means q 0)
                          (let ((factor (aref counts q)))
                            (setf (pixel means q 0)
                                  (values (truncate (/ m1 factor))
                                          (truncate (/ m2 factor))
                                          (truncate (/ m3 factor))))))))
                    (let ((new-means-list
                           (loop for count across counts
                              for i below k
                              collect (list count (pixel* means i 0)))))
                      (declare (type list new-means-list))
                      (loop for i fixnum below k
                         for (count mean) in (sort new-means-list #'> :key #'first)
                         do
                           (setf (pixel* means i 0) mean)
                           (setf (aref counts i) count))))
                  (assign-to-means ()
                    (declare (type 8-bit-rgb-image image)
                             (optimize (speed 3)))
                    (do-pixels (i j) image
                      (setf (aref z i j)
                            (let (min-val nearest-mean)
                              (loop for q below k
                                 do (let ((dist (multiple-value-call #'l2-distance-3
                                                  (pixel image i j)
                                                  (pixel means q 0))))
                                      (when (or (null min-val) (< dist min-val))
                                        (setf min-val dist
                                              nearest-mean q))))
                              nearest-mean)))))
        
             ;; randomly assign pixel values to the k means
             (loop for i below k
                for y = (random height)
                for x = (random width)
                do (setf (pixel means i 0)
                         (pixel image y x)))

             (loop for iter below max-iterations
                with stop = nil
                with oldz
                until stop
                do
                  (assign-to-means)
                  (recompute-means)
                  (when (and oldz (equalp oldz z))
                    (setf stop t))
                  (setf oldz (copy-array z)))
             (values means z))))))
    (t
     (with-image-bounds (height width channels)
         image
       (let ((means (make-array (append (list k 1) (if channels (list channels)))
                                :element-type '(unsigned-byte 32)))
             (counts (make-array k :element-type 'fixnum))
             (z (make-array (list height width) :element-type '(unsigned-byte 32))))
         (declare (type (simple-array (unsigned-byte 32) (* *)) z))
         (flet ((recompute-means ()
                  ;; clear out the old values
                  (let ((zero (make-list (or channels 1) :initial-element 0)))
                    (dotimes (q k)
                      (setf (pixel* means q 0) zero)
                      (setf (aref counts q) 0)))
                 
                  ;; use the means vector first as an accumulator to hold
                  ;; the sums for each channel, later we'll scale by (/
                  ;; num-pixels)
                  (do-pixels (i j) image
                    (let ((m (aref z i j)))
                      (setf (pixel* means m 0)
                            (mapcar #'+ 
                                    (multiple-value-list (pixel image i j))
                                    (pixel* means m 0)))
                      (incf (aref counts (aref z i j)))))
                  (dotimes (q k)
                    (when (plusp (aref counts q))
                      (setf (pixel* means q 0)
                            (mapcar (lambda (x) (round (/ x (aref counts q))))
                                    (pixel* means q 0) )))))
             
                (assign-to-means ()
                  (typecase image
                    (gray-image
                     (locally
                         (declare (type gray-image image))
                       (do-pixels (i j) image
                         (setf (aref z i j)
                               (let (min-val nearest-mean)
                                 (loop for q below k
                                    do (let ((dist (let ((d (- (pixel image i j)
                                                               (pixel means q 0))))
                                                     (* d d))))
                                         (when (or (null min-val) (< dist min-val))
                                           (setf min-val dist
                                                 nearest-mean q))))
                                 nearest-mean)))))
                    (rgb-image
                     (locally
                         (declare (type rgb-image image))
                       (do-pixels (i j) image
                         (setf (aref z i j)
                               (let (min-val nearest-mean)
                                 (loop for q below k
                                    do (let ((dist (multiple-value-call #'l2-distance-3
                                                     (pixel image i j)
                                                     (pixel means q 0))))
                                         (when (or (null min-val) (< dist min-val))
                                           (setf min-val dist
                                                 nearest-mean q))))
                                 nearest-mean)))))
                    (t (do-pixels (i j) image
                         (setf (aref z i j)
                               (let (min-val nearest-mean)
                                 (loop for q below k
                                    do (let ((dist (l2-distance (pixel* image i j)
                                                                (pixel* means q 0))))
                                         (when (or (null min-val) (< dist min-val))
                                           (setf min-val dist
                                                 nearest-mean q))))
                                 nearest-mean)))))))
        
           ;; randomly assign pixel values to the k means
           (loop for i below k
              for y = (random height)
              for x = (random width)
              do (setf (pixel means i 0)
                       (pixel image y x)))

           (loop for iter below max-iterations
              with stop = nil
              with oldz
              until stop
              do
              (assign-to-means)
              (recompute-means)
              (when (and oldz (equalp oldz z))
                (setf stop t))
              (setf oldz (copy-array z)))
           (values means z)))))))

