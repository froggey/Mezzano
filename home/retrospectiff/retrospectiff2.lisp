
(in-package :retrospectiff2)

(defun read-grayscale-strip (stream
                             image-info
                             array
                             start-row
                             strip-offset
                             strip-byte-count
                             image-width
                             bits-per-sample
                             compression)
  (file-position stream strip-offset)
  (let ((compressed-bytes (read-bytes stream strip-byte-count)))
    (let ((decompressed-bytes (apply (find-compression-decoder compression) compressed-bytes
                                     (when image-info
                                       (list image-info)))))
      (ecase bits-per-sample
        (1 (let ((bytes-per-row (1+ (ash (1- image-width) -3))))
             (let ((strip-length (ceiling (length decompressed-bytes) bytes-per-row)))
               (let ((end-row (+ start-row strip-length)))
                 (loop for i from start-row below end-row
                    for strip-row-offset from 0 
                    do
                      (loop for j below image-width by 8
                         for byte-index from 0
                         do
                           (let ((current-byte (aref decompressed-bytes
                                                     (+ (* strip-row-offset bytes-per-row) byte-index))))
                             ;; FIXME! Easy enough to support
                             ;; lsb-to-msb here, so do that too please
                             (loop for bit from 7 downto 0
                                for jprime from j
                                do (setf (pixel array i jprime)
                                         (ldb (byte 1 bit) current-byte))))))))))

        (4
         (let ((bytes-per-row (1+ (ash (1- image-width) -1))))
           (let ((strip-length (ceiling (length decompressed-bytes) bytes-per-row)))
             (let ((end-row (+ start-row strip-length)))
               (loop for i from start-row below end-row
                  for strip-row-offset from 0 
                  do
                    (loop for j below image-width by 2
                       for byte-index from 0
                       do
                         (let ((current-byte (aref decompressed-bytes
                                                   (+ (* strip-row-offset bytes-per-row) byte-index))))
                           (loop for bit below 8 by 4
                              for jprime from j
                              do (setf (pixel array i jprime)
                                       (ldb (byte 4 (- 4 bit)) current-byte))))))))))
        
        (8
         (let ((bytes-per-row image-width))
           (let ((strip-length (ceiling (length decompressed-bytes) bytes-per-row)))
             (let ((end-row (+ start-row strip-length)))
               (loop for i from start-row below end-row
                  for strip-row-offset from 0 
                  do
                    (loop for j below image-width
                       for byte-index from 0
                       do
                         (let ((current-byte (aref decompressed-bytes
                                                   (+ (* strip-row-offset bytes-per-row) byte-index))))
                           (setf (pixel array i j) current-byte))))))))

        (16
         (let ((bytes-per-row (ash image-width 1)))
           (let ((strip-length (ceiling (length decompressed-bytes) bytes-per-row)))
             (let ((end-row (+ start-row strip-length)))
               (loop for i from start-row below end-row
                  for strip-row-offset from 0 
                  do
                    (loop for j below image-width
                       for byte-index from 0 by 2
                       do
                         (let ((current-byte1 (aref decompressed-bytes
                                                    (+ (* strip-row-offset bytes-per-row) byte-index)))
                               (current-byte2 (aref decompressed-bytes
                                                    (+ (* strip-row-offset bytes-per-row) (1+ byte-index)))))
                           (setf (pixel array i j)
                                 (ecase *byte-order*
                                   (:little-endian
                                    (+ (ash current-byte2 8)
                                       current-byte1))
                                   (:big-endian
                                    (+ (ash current-byte1 8)
                                       current-byte2)))))))))))))))

(defun grayscale-horizontal-difference-depredict (image max-value)
  (destructuring-bind (image-length image-width)
      (array-dimensions image)
    (loop for i below image-length
       do
         (loop for j from 1 below image-width
            do
              (setf (pixel image i j)
                    (logand
                     (+ (pixel image i j)
                        (pixel image i (1- j)))
                     max-value))))))

(defun read-grayscale-image (stream ifd)
  (let ((image-width (get-ifd-value ifd +image-width-tag+))
        (image-length (get-ifd-value ifd +image-length-tag+))
        (bits-per-sample (or (get-ifd-value ifd +bits-per-sample-tag+) 1))
        (compression (get-ifd-value ifd +compression-tag+))
        (photometric-interpretation (get-ifd-value ifd +photometric-interpretation-tag+))
        (strip-offsets (get-ifd-values ifd +strip-offsets-tag+))
        (rows-per-strip (get-ifd-value ifd +rows-per-strip-tag+))
        (strip-byte-counts (get-ifd-values ifd +strip-byte-counts-tag+))
        (predictor (get-ifd-value ifd +predictor-tag+))
        image-info
        (jpeg-tables (get-ifd-values ifd +jpeg-tables+)))
    (when jpeg-tables
      (setf image-info (make-instance 'jpeg-image-info :jpeg-tables jpeg-tables)))
    (case bits-per-sample
      (1
       (let ((data (make-1-bit-gray-image image-length image-width)))
         (loop for strip-offset across strip-offsets
            for strip-byte-count across strip-byte-counts
            for row-offset = 0 then (+ row-offset rows-per-strip)
            do (read-grayscale-strip stream image-info data row-offset
                                     strip-offset strip-byte-count
                                     image-width
                                     bits-per-sample
                                     compression))
         (make-instance 'tiff-image
                        :length image-length :width image-width
                        :bits-per-sample bits-per-sample
                        :samples-per-pixel 1 :data data
                        :byte-order *byte-order*
                        :min-is-white (= photometric-interpretation
                                         +photometric-interpretation-white-is-zero+))))
      (4
       (let ((data (make-4-bit-gray-image image-length image-width)))
         (loop for strip-offset across strip-offsets
            for strip-byte-count across strip-byte-counts
            for row-offset = 0 then (+ row-offset rows-per-strip)
            do (read-grayscale-strip stream image-info data row-offset
                                     strip-offset strip-byte-count
                                     image-width
                                     bits-per-sample
                                     compression))
         (make-instance 'tiff-image
                        :length image-length :width image-width
                        :bits-per-sample bits-per-sample
                        :samples-per-pixel 1 :data data
                        :byte-order *byte-order*)))

      (8
       (let ((data (make-8-bit-gray-image image-length image-width)))
         (loop for strip-offset across strip-offsets
            for strip-byte-count across strip-byte-counts
            for row-offset = 0 then (+ row-offset rows-per-strip)
            do (read-grayscale-strip stream image-info data row-offset
                                     strip-offset strip-byte-count
                                     image-width
                                     bits-per-sample
                                     compression))
         (case predictor
           (#.+horizontal-differencing+
            (grayscale-horizontal-difference-depredict data #xff)))

         (make-instance 'tiff-image
                        :length image-length :width image-width
                        :bits-per-sample bits-per-sample
                        :samples-per-pixel 1 :data data
                        :byte-order *byte-order*)))
      (16
       (let ((data (make-16-bit-gray-image image-length image-width)))
         (loop for strip-offset across strip-offsets
            for strip-byte-count across strip-byte-counts
            for row-offset = 0 then (+ row-offset rows-per-strip)
            do (read-grayscale-strip stream image-info data row-offset
                                     strip-offset strip-byte-count
                                     image-width
                                     bits-per-sample
                                     compression))

         (case predictor
           (#.+horizontal-differencing+
            (grayscale-horizontal-difference-depredict data #xffff)))
         
         (make-instance 'tiff-image
                        :length image-length :width image-width
                        :bits-per-sample bits-per-sample
                        :samples-per-pixel 1 :data data
                        :byte-order *byte-order*)))
      (t
       (error "Unsupported grayscale bit depth: ~A" bits-per-sample)))))

(defun rgb-horizontal-difference-depredict (image max-value)
  (destructuring-bind (image-length image-width channels)
      (array-dimensions image)
    (declare (ignore channels))
    (loop for i below image-length
       do
         (loop for j from 1 below image-width
            do
              (multiple-value-bind (oldr oldg oldb)
                  (pixel image i (1- j))
                (multiple-value-bind (newr newg newb)
                    (pixel image i i)
                  (setf (pixel image i j)
                        (values (logand (+ oldr newr) max-value)
                                (logand (+ oldg newg) max-value)
                                (logand (+ oldb newb) max-value)))))))))

(defun read-rgb-strip (stream image-info array start-row strip-offset
                       strip-byte-count width bits-per-sample samples-per-pixel
                       bytes-per-pixel compression)
  (file-position stream strip-offset)
  (let ((compressed-bytes (read-bytes stream strip-byte-count)))
    (let ((decompressed-bytes (apply (find-compression-decoder compression) compressed-bytes
                                     (when image-info
                                       (list image-info)))))
      (let ((decoded-offset 0))
        (let ((strip-length (/ (length decompressed-bytes) width bytes-per-pixel))
              (max-bits-per-sample (reduce #'max bits-per-sample)))
          (ecase max-bits-per-sample
            (8
             (loop for i from start-row below (+ start-row strip-length)
                do
                  (loop for j below width
                     do
                       (setf (pixel* array i j)
                             (loop for k below samples-per-pixel
                                for bits across bits-per-sample
                                collect
                                  (prog1
                                      (aref decompressed-bytes decoded-offset)
                                    (incf decoded-offset)))))))
            (16
             (loop for i from start-row below (+ start-row strip-length)
                do
                  (loop for j below width
                     do
                       (setf (pixel* array i j)
                             (loop for k below samples-per-pixel
                                for bits across bits-per-sample
                                collect
                                  (prog1
                                      (ecase *byte-order*
                                        (:big-endian
                                         (+ (ash (aref decompressed-bytes decoded-offset) 8)
                                            (aref decompressed-bytes (1+ decoded-offset))))
                                        (:little-endian
                                         (+ (ash (aref decompressed-bytes (1+ decoded-offset)) 8)
                                            (aref decompressed-bytes decoded-offset))))
                                    (incf decoded-offset 2)))))))))))))

(defun read-planar-rgb-strip (stream image-info array start-row strip-offset
                              strip-byte-count width plane-bits-per-sample samples-per-pixel
                              bytes-per-pixel compression plane)
  (file-position stream strip-offset)
  (let ((compressed-bytes (read-bytes stream strip-byte-count)))
    (let* ((decompressed-bytes (apply (find-compression-decoder compression) compressed-bytes
                                      (when image-info
                                        (list image-info))))
           (decoded-offset 0)
           (bytes-per-sample (/ bytes-per-pixel samples-per-pixel))
           (strip-length (/ (length decompressed-bytes) width bytes-per-sample)))
      (ecase plane-bits-per-sample
        (8
         (loop for i from start-row below (+ start-row strip-length)
            do
              (loop for j below width
                 do
                   (setf (aref array i j plane)
                         (aref decompressed-bytes decoded-offset))
                   (incf decoded-offset))))

        (16
         (loop for i from start-row below (+ start-row strip-length)
            do
              (loop for j below width
                 do
                   (setf (aref array i j plane)
                         (ecase *byte-order*
                           (:big-endian
                            (+ (ash (aref decompressed-bytes decoded-offset) 8)
                               (aref decompressed-bytes (1+ decoded-offset))))
                           (:little-endian
                            (+ (ash (aref decompressed-bytes (1+ decoded-offset)) 8)
                               (aref decompressed-bytes decoded-offset)))))
                   (incf decoded-offset 2))))))))

(defun read-rgb-image (stream ifd)
  (let ((image-width (get-ifd-value ifd +image-width-tag+))
        (image-length (get-ifd-value ifd +image-length-tag+))
        (samples-per-pixel (get-ifd-value ifd +samples-per-pixel-tag+))
        (bits-per-sample (get-ifd-values ifd +bits-per-sample-tag+))
        (rows-per-strip (get-ifd-value ifd +rows-per-strip-tag+))
        (strip-offsets (get-ifd-values ifd +strip-offsets-tag+))
        (strip-byte-counts (get-ifd-values ifd +strip-byte-counts-tag+))
        (compression (get-ifd-value ifd +compression-tag+))
        ;; default planar-configuration is +planar-configuration-chunky+
        (planar-configuration (or (get-ifd-value ifd +planar-configuration-tag+)
                                  +planar-configuration-chunky+))
        (predictor (get-ifd-value ifd +predictor-tag+))
        image-info
        (jpeg-tables (get-ifd-values ifd +jpeg-tables+)))

    (when jpeg-tables
      (setf image-info (make-instance 'jpeg-image-info :jpeg-tables jpeg-tables)))
    ;; FIXME
    ;; 1. we need to support predictors for lzw encoded images.
    ;; 2. Presumably we'll want planar images as well at some point.

    
    (let* ((max-bits-per-sample (reduce #'max bits-per-sample))
           (bytes-per-pixel
            (* samples-per-pixel (1+ (ash (1- max-bits-per-sample) -3)))))
      (let ((data
             (ecase max-bits-per-sample
               (8 (make-8-bit-rgb-image image-length image-width))
               (16 (make-16-bit-rgb-image image-length image-width)))))

        (case planar-configuration
          (#.+planar-configuration-chunky+
           (loop for strip-offset across strip-offsets
              for strip-byte-count across strip-byte-counts
              for row-offset = 0 then (+ row-offset rows-per-strip)
              do (read-rgb-strip stream
                                 image-info
                                 data
                                 row-offset
                                 strip-offset
                                 strip-byte-count
                                 image-width
                                 bits-per-sample
                                 samples-per-pixel
                                 bytes-per-pixel
                                 compression))
           
           (case predictor
             (#.+horizontal-differencing+
              (rgb-horizontal-difference-depredict data (1- (ash 1 max-bits-per-sample)))))

           (make-instance 'tiff-image
                          :length image-length :width image-width
                          :bits-per-sample bits-per-sample
                          :samples-per-pixel samples-per-pixel
                          :data data :byte-order *byte-order*))

          (#.+planar-configuration-planar+
           (let* ((strips-per-image
                   (floor (+ image-length rows-per-strip -1) rows-per-strip)))
             (loop for plane below samples-per-pixel
                do
                  (let ((plane-bits-per-sample (elt bits-per-sample plane)))
                    (loop for strip-offset across (subseq strip-offsets
                                                          (* plane strips-per-image)
                                                          (* (1+ plane) strips-per-image))
                       for strip-byte-count across (subseq strip-byte-counts
                                                           (* plane strips-per-image)
                                                           (* (1+ plane) strips-per-image))
                       for row-offset = 0 then (+ row-offset rows-per-strip)
                       do (read-planar-rgb-strip stream
                                                 image-info
                                                 data
                                                 row-offset
                                                 strip-offset
                                                 strip-byte-count
                                                 image-width
                                                 plane-bits-per-sample
                                                 samples-per-pixel
                                                 bytes-per-pixel
                                                 compression
                                                 plane))))
             
             (case predictor
               (rgb-horizontal-difference-depredict data (1- (ash 1 max-bits-per-sample))))

             (make-instance 'tiff-image
                            :length image-length :width image-width
                            :bits-per-sample bits-per-sample
                            :samples-per-pixel samples-per-pixel
                            :data data :byte-order *byte-order*)))
          (t
           (error "Planar Configuration ~A not supported." planar-configuration)))))))

(defun read-indexed-image (stream ifd)
  (let ((image-width (get-ifd-value ifd +image-width-tag+))
        (image-length (get-ifd-value ifd +image-length-tag+))
        (bits-per-sample (get-ifd-value ifd +bits-per-sample-tag+))
        (rows-per-strip (get-ifd-value ifd +rows-per-strip-tag+))
        (strip-offsets (get-ifd-values ifd +strip-offsets-tag+))
        (strip-byte-counts (get-ifd-values ifd +strip-byte-counts-tag+))
        (compression (get-ifd-value ifd +compression-tag+))
        (predictor (get-ifd-value ifd +predictor-tag+))
        image-info
        (jpeg-tables (get-ifd-values ifd +jpeg-tables+))
        (color-map (get-ifd-values ifd +color-map-tag+)))
    (when jpeg-tables
      (setf image-info (make-instance 'jpeg-image-info :jpeg-tables jpeg-tables)))
    (let* ((k (expt 2 bits-per-sample))
           (color-index (make-array k)))
      (loop for i below k
         do (setf (aref color-index i)
                  (list (aref color-map i)
                        (aref color-map (+ k i))
                        (aref color-map (+ (ash k 1) i)))))
      ;; FIXME
      ;; 1. we need to support predictors for lzw encoded images.
      (ecase bits-per-sample
        (8
         (let ((data (make-8-bit-gray-image image-length image-width)))
           (loop for strip-offset across strip-offsets
              for strip-byte-count across strip-byte-counts
              for row-offset = 0 then (+ row-offset rows-per-strip)
              do (read-grayscale-strip stream
                                       image-info
                                       data
                                       row-offset
                                       strip-offset
                                       strip-byte-count
                                       image-width
                                       bits-per-sample
                                       compression))
           (case predictor
             (#.+horizontal-differencing+
              (grayscale-horizontal-difference-depredict data #xff)))
           (make-instance 'tiff-image
                          :length image-length :width image-width
                          :bits-per-sample bits-per-sample
                          :data data :byte-order *byte-order*
                          :color-map color-index)))
        (16
         (let ((data (make-16-bit-gray-image image-length image-width)))
           (loop for strip-offset across strip-offsets
              for strip-byte-count across strip-byte-counts
              for row-offset = 0 then (+ row-offset rows-per-strip)
              do (read-grayscale-strip stream
                                       image-info
                                       data
                                       row-offset
                                       strip-offset
                                       strip-byte-count
                                       image-width
                                       bits-per-sample
                                       compression))
           (case predictor
             (#.+horizontal-differencing+
              (grayscale-horizontal-difference-depredict data #xffff)))
           (make-instance 'tiff-image
                          :length image-length :width image-width
                          :bits-per-sample bits-per-sample
                          :data data :byte-order *byte-order*
                          :color-map color-index)))))))

(defun read-tiff-stream (stream)
  (let* ((fields (read-value 'tiff-fields stream))
         (ifd (entries (first (ifd-list fields)))))
    (let ((photometric-interpretation 
         (get-ifd-value ifd +photometric-interpretation-tag+)))
    (ecase photometric-interpretation
      (#.+photometric-interpretation-white-is-zero+
       ;; FIXME! This image should be inverted
       (read-grayscale-image stream ifd))
      (#.+photometric-interpretation-black-is-zero+
       (read-grayscale-image stream ifd))
      (#.+photometric-interpretation-rgb+
       (read-rgb-image stream ifd))
      (#.+photometric-interpretation-palette-color+
       (read-indexed-image stream ifd))))))

(defun read-tiff-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-tiff-stream stream)))


(defun make-tiff-image-for-output (image)
  (typecase image

    (16-bit-gray-image
     (locally
         (declare (type 16-bit-gray-image image))
       (destructuring-bind (height width)
           (array-dimensions image)
         (let ((tiff-image (make-instance 'tiff:tiff-image
                                          :width width
                                          :length height
                                          :bits-per-sample 16
                                          :samples-per-pixel 1
                                          :data (make-array (* width height 2)))))
           (with-accessors ((image-data tiff:tiff-image-data))
               tiff-image
             (let ((pixoff 0))
               (loop for i below height
                  do
                    (loop for j below width
                       do
                         (let ((pixval (pixel image i j)))
                           (ecase *byte-order*
                             (:little-endian
                              (setf (aref image-data pixoff) (logand pixval #xff))
                              (incf pixoff)
                              (setf (aref image-data pixoff) (ash pixval -8))
                              (incf pixoff))
                             (:big-endian
                              (setf (aref image-data pixoff) (ash pixval -8))
                              (incf pixoff)
                              (setf (aref image-data pixoff) (logand pixval #xff))
                              (incf pixoff))))))))
           tiff-image))))

    (16-bit-rgb-image
     (locally
         (declare (type 16-bit-rgb-image image))
       (destructuring-bind (height width channels)
           (array-dimensions image)
         (declare (ignore channels))
         (let ((tiff-image (make-instance 'tiff:tiff-image
                                          :width width
                                          :length height
                                          :bits-per-sample '(16 16 16)
                                          :samples-per-pixel 3
                                          :data (make-array (* width height 3 2)))))
           (with-accessors ((image-data tiff:tiff-image-data))
               tiff-image
             (loop for i below height
                do
                  (loop for j below width
                     do
                       (let ((pixoff (* 3 2 (+ (* i width) j))))
                         (multiple-value-bind
                               (r g b)
                             (pixel image i j)

                           (ecase *byte-order*
                             (:little-endian
                              (setf (aref image-data pixoff) (logand r #xff)
                                    (aref image-data (incf pixoff)) (ash r -8)

                                    (aref image-data (incf pixoff)) (logand g #xff)
                                    (aref image-data (incf pixoff)) (ash g -8)

                                    (aref image-data (incf pixoff)) (logand b #xff)
                                    (aref image-data (incf pixoff)) (ash b -8)))
                             (:big-endian
                              (setf (aref image-data pixoff) (ash r -8)
                                    (aref image-data (incf pixoff)) (logand r #xff)

                                    (aref image-data (incf pixoff)) (ash g -8)
                                    (aref image-data (incf pixoff)) (logand g #xff)

                                    (aref image-data (incf pixoff)) (ash b -8)
                                    (aref image-data (incf pixoff)) (logand b #xff)))))))))
           tiff-image))))

    (16-bit-rgba-image
     (locally
         (declare (type 16-bit-rgba-image image))
       (destructuring-bind (height width channels)
           (array-dimensions image)
         (declare (ignore channels))
         (let ((tiff-image (make-instance 'tiff:tiff-image
                                          :width width
                                          :length height
                                          :bits-per-sample '(16 16 16 16)
                                          :samples-per-pixel 4
                                          :data (make-array (* width height 4 2)))))
           (with-accessors ((image-data tiff:tiff-image-data))
               tiff-image
             (loop for i below height
                do
                  (loop for j below width
                     do
                       (let ((pixoff (* 4 2 (+ (* i width) j))))
                         (multiple-value-bind
                               (r g b a)
                             (pixel image i j)
                           (setf (aref image-data pixoff) (ash r -8)
                                 (aref image-data (incf pixoff)) (logand r #xff)

                                 (aref image-data (incf pixoff)) (ash g -8)
                                 (aref image-data (incf pixoff)) (logand g #xff)

                                 (aref image-data (incf pixoff)) (ash b -8)
                                 (aref image-data (incf pixoff)) (logand b #xff)

                                 (aref image-data (incf pixoff)) (ash a -8)
                                 (aref image-data (incf pixoff)) (logand a #xff)))))))
           tiff-image))))

    (8-bit-gray-image
     (locally
         (declare (type 8-bit-gray-image image))
       (destructuring-bind (height width)
           (array-dimensions image)
         (let ((tiff-image (make-instance 'tiff:tiff-image
                                          :width width
                                          :length height
                                          :bits-per-sample 8
                                          :samples-per-pixel 1
                                          :data (make-array (* width height)
                                                            :initial-element 255))))
           (with-accessors ((image-data tiff:tiff-image-data))
               tiff-image
             (let ((pixoff 0))
               (loop for i below height
                  do
                    (loop for j below width
                       do
                         (setf (aref image-data pixoff) (pixel image i j))
                         (incf pixoff)))))
           tiff-image))))

    (8-bit-rgb-image
     (locally
         (declare (type 8-bit-rgb-image image))
       (destructuring-bind (height width channels)
           (array-dimensions image)
         (declare (ignore channels))
         (let ((tiff-image (make-instance 'tiff:tiff-image
                                          :width width
                                          :length height
                                          :bits-per-sample '(8 8 8)
                                          :samples-per-pixel 3
                                          :data (make-array (* width height 3)))))
           (with-accessors ((image-data tiff:tiff-image-data))
               tiff-image
             (loop for i below height
                do
                  (loop for j below width
                     do
                       (let ((pixoff (* 3 (+ (* i width) j))))
                         (multiple-value-bind
                               (r g b)
                             (pixel image i j)
                           (setf (aref image-data pixoff) r
                                 (aref image-data (incf pixoff)) g
                                 (aref image-data (incf pixoff)) b))))))
           tiff-image))))

    (8-bit-rgba-image
     (locally
         (declare (type 8-bit-rgba-image image))
       (destructuring-bind (height width channels)
           (array-dimensions image)
         (declare (ignore channels))
         (let ((tiff-image (make-instance 'tiff:tiff-image
                                          :width width
                                          :length height
                                          :bits-per-sample '(8 8 8 8)
                                          :samples-per-pixel 4
                                          :data (make-array (* width height 4)))))
           (with-accessors ((image-data tiff:tiff-image-data))
               tiff-image
             (loop for i below height
                do
                  (loop for j below width
                     do
                       (let ((pixoff (* 4 (+ (* i width) j))))
                         (multiple-value-bind
                               (r g b a)
                             (pixel image i j)
                           (setf (aref image-data pixoff) r
                                 (aref image-data (incf pixoff)) g
                                 (aref image-data (incf pixoff)) b
                                 (aref image-data (incf pixoff)) a))))))
           tiff-image))))

    (4-bit-gray-image
     (locally
         (declare (type 4-bit-gray-image image))
       (destructuring-bind (height width)
           (array-dimensions image)
         (let ((tiff-image (make-instance 'tiff:tiff-image
                                          :width width
                                          :length height
                                          :bits-per-sample 4
                                          :samples-per-pixel 1
                                          :data (make-array (* width height)
                                                            :initial-element 15))))
           (with-accessors ((image-data tiff:tiff-image-data))
               tiff-image
             (let ((byte-offset 0)
                   (nibble 0))
               (loop for i below height
                  do
                    (loop for j below width
                       do
                         (if (zerop nibble)
                             (progn
                               (setf (ldb (byte 4 4)
                                          (aref image-data byte-offset))
                                     (pixel image i j))
                               (incf nibble))
                             (progn
                               (setf (ldb (byte 4 0)
                                          (aref image-data byte-offset))
                                     (pixel image i j))
                               (setf nibble 0)
                               (incf byte-offset)))))))
           tiff-image))))

    (1-bit-gray-image
     (locally
         (declare (type 1-bit-gray-image image))
       (destructuring-bind (height width)
           (array-dimensions image)
         (let ((tiff-image (make-instance 'tiff:tiff-image
                                          :width width
                                          :length height
                                          :bits-per-sample 1
                                          :samples-per-pixel 1
                                          :data (make-array (* width height)
                                                            :initial-element 1))))
           (with-accessors ((image-data tiff:tiff-image-data))
               tiff-image
             (let ((byte-offset 0)
                   (bit-offset 0))
               (loop for i below height
                  do
                    (loop for j below width
                       do
                         (if (< bit-offset 7)
                             (progn
                               (setf (ldb (byte 1 (- 7 bit-offset))
                                          (aref image-data byte-offset))
                                     (pixel image i j))
                               (incf bit-offset))
                             (progn
                               (setf (ldb (byte 1 0)
                                          (aref image-data byte-offset))
                                     (pixel image i j))
                               (setf bit-offset 0)
                               (incf byte-offset)))))))
           tiff-image))))

    (t (error "Cannot write a TIFF image from ~A" (type-of image)))))

;;;
;;; The general strategy here is to:
;;;
;;; 1. make the TIFF Image File Directory (we're only going to deal
;;; with single images per TIFF file for the moment)
;;;
;;; 2. Compute the offsets of the first (and only IFD -- probably 8)
;;;
;;; 3. Compute the offset of the various IFD arrays that aren't
;;;    represented inline -- starting at the offset of the IFD + (2 +
;;;    number of directory entries * 12)
;;; 
;;; 4. Compute the offset of the strip/sample data
;;;
;;; 5. Write the TIFF Header
;;;
;;; 6. Write the IFD directory entries (inline portions), then write
;;; the non-inline values
;;;
;;; 7. Write the sample (strip) data
;;;
;;; FIXME! Writing indexed tiff images is broken!
(defun write-tiff-stream (stream image &key byte-order)
  (let ((*byte-order* (or byte-order *byte-order*))
        (*tiff-file-offset* 0))
    ;; FIXME! The typecase below is a hack to get around the fact that
    ;; we need to handle both a tiff-image and an opticl-core:image
    ;; here in order to keep opticl happy. I think we need two
    ;; write-tiff-{file,stream} flavors, one for opticl-core:image and
    ;; one for tiff-image.
    (let ((obj (etypecase
                   image
                 (opticl-core:image (make-tiff-image-for-output image))
                 (t (make-tiff-image-for-output (tiff-image-data image))))))
      (multiple-value-bind (fields out-of-line-data-size strip-offsets strip-byte-counts)
          (make-tiff-fields obj)
        (write-value 'tiff-fields stream fields)
        (file-position stream (+ (file-position stream) out-of-line-data-size))
        (with-accessors
              ((image-width tiff-image-width)
               (image-length tiff-image-length)
               (image-data tiff-image-data))
            obj
          ;; need to convert our opticl-core image into proper TIFF strippable-data
          (loop for start in strip-offsets
             for count in strip-byte-counts
             do
               (write-sequence (subseq image-data start
                                       (+ start count))
                               stream)))))))

(defun write-tiff-file (pathname image &rest args &key (if-exists :error) &allow-other-keys)
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists if-exists)
    (apply #'write-tiff-stream stream image (remove-keyword-args :if-exists args))
    pathname))

