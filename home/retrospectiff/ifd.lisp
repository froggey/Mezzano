
(in-package :retrospectiff.ifd)

(defun get-ifd-values (ifd key)
  (let ((field (find key ifd :key 'tag :test '=)))
    (when field
      (data field))))

(defun get-ifd-value (ifd key)
  (let ((values (get-ifd-values ifd key)))
    (when values (elt values 0))))

(defun add-ifd-entry (ifd entry)
  (push entry (entries ifd))
  (incf (entry-count ifd))
  ifd)

(defun vectorize (data)
  (etypecase data
    (vector data)
    (list (apply #'vector data))
    (nil nil)
    (atom (vector data))))

(defun make-ifd-entry-short (tag data)
  (let ((data (vectorize data)))
    (make-instance 'short-ifd-entry
                   :tag tag
                   :field-type +field-type-short+
                   :data data
                   :value-count (length data))))

(defun make-ifd-entry-long (tag data)
  (let ((data (vectorize data)))
    (make-instance 'long-ifd-entry
                   :tag tag
                   :field-type +field-type-long+
                   :data data
                   :value-count (length data))))

(defun make-ifd-entry-rational (tag data)
  (let ((data (vectorize data)))
    (make-instance 'rational-ifd-entry
                   :tag tag
                   :field-type +field-type-rational+
                   :data (map 'vector
                              (lambda (x)
                                (make-instance 'rational :numerator (car x)
                                               :denominator (cdr x)))
                              data)
                   :value-count (length data))))


(defun ifd-entry-out-of-line-bytes (entry)
  (let ((bytes (entry-bytes entry)))
    (if (> bytes 4) bytes 0)))

;; we should return the number of strips (and possibly the length of
;; each strip (uncompressed), but not yet)..
(defun compute-rows-per-strip (image-length
                               bytes-per-row
                               &key (strip-size #x40000))
  (let ((strip-rows (truncate strip-size bytes-per-row)))
    (min image-length strip-rows)))


(defun make-tiff-fields (image)
  (with-accessors
        ((image-width tiff-image-width)
         (image-length tiff-image-length)
         (image-data tiff-image-data)
         (bits-per-sample tiff-image-bits-per-sample)
         (samples-per-pixel tiff-image-samples-per-pixel))
      image
    (let* ((num-bits-per-sample (if (typep bits-per-sample 'sequence)
                                   (elt bits-per-sample 0)
                                   bits-per-sample))
           (bytes-per-row (ceiling (* image-width (* samples-per-pixel num-bits-per-sample)) 8))
           (rows-per-strip (compute-rows-per-strip image-length bytes-per-row))
           (fields (make-instance 'tiff-fields
                                  :byte-order *byte-order*
                                  :magic 42
                                  :ifd-list nil))
           (ifd (make-instance 'ifd
                               :entry-count 0
                               :entries nil
                               :next-ifd-offset 0)))

      (destructuring-bind (strip-offsets strip-byte-counts)
          (apply #'mapcar #'list
                 (loop for i below image-length by rows-per-strip
                    for byte-offset from i by (* rows-per-strip
                                                 bytes-per-row) 
                    collect (list byte-offset 
                                  (* bytes-per-row
                                     (- (min (+ i rows-per-strip)
                                             image-length) i)))))
        (reduce #'add-ifd-entry 
                (list (make-ifd-entry-long +image-length-tag+ image-length)
                      (make-ifd-entry-long +image-width-tag+ image-width)
                      (make-ifd-entry-short +bits-per-sample-tag+ bits-per-sample)
                      (make-ifd-entry-short +samples-per-pixel-tag+ samples-per-pixel)
                      (make-ifd-entry-rational +x-resolution-tag+ (vector (cons 72 1)))
                      (make-ifd-entry-rational +y-resolution-tag+ (vector (cons 72 1)))
                      (make-ifd-entry-short +resolution-unit-tag+ 2))
                :initial-value ifd)
        (cond
          ((= samples-per-pixel 1)
           (add-ifd-entry 
            ifd
            (make-ifd-entry-short +photometric-interpretation-tag+
                                  +photometric-interpretation-black-is-zero+)))
          ((member samples-per-pixel '(3 4))
           (add-ifd-entry 
            ifd
            (make-ifd-entry-short +photometric-interpretation-tag+
                                  +photometric-interpretation-rgb+))))

        (add-ifd-entry ifd
                       (make-ifd-entry-long +rows-per-strip-tag+ rows-per-strip))
        (add-ifd-entry ifd
                       (make-ifd-entry-long +strip-byte-counts-tag+ strip-byte-counts))
        
        (setf (ifd-list fields) (list ifd))

        (incf *tiff-file-offset* 8)
        (setf (ifd-offset fields) *tiff-file-offset*)
        
        (let ((num-entries (entry-count ifd)))
          (incf *tiff-file-offset* (+ 2 (* num-entries 12))))
        
        (let ((out-of-line-data-size 
               (* 4 (length strip-offsets))))
          (loop for entry in (entries ifd)
             do (incf out-of-line-data-size 
                      (ifd-entry-out-of-line-bytes entry)))

          ;; skip one more ifd-entry
          (incf *tiff-file-offset* 12)
          (incf *tiff-file-offset* 4)

          ;; *file-offset* to the strip-offsets
          (add-ifd-entry
           ifd
           (make-ifd-entry-long
            +strip-offsets-tag+
            (map 'vector
                 (lambda (x) (+ x
                                *tiff-file-offset*
                                out-of-line-data-size))
                 strip-offsets)))

          (values fields out-of-line-data-size strip-offsets strip-byte-counts))))))
