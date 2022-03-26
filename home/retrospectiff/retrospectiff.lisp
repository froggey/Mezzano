
(in-package :retrospectiff)

(defun read-grayscale-strip (stream
                             image-info
                             array
                             start-row
                             strip-offset
                             strip-byte-count
                             width
                             bits-per-sample
                             compression)
  (file-position stream strip-offset)
  (let ((compressed-bytes (read-bytes stream strip-byte-count)))
    (let ((decompressed-bytes (apply (find-compression-decoder compression) compressed-bytes
                                     (when image-info
                                       (list image-info)))))
      (let ((stream (flexi-streams:make-in-memory-input-stream decompressed-bytes)))
        (let ((bytes-per-row
               (ecase bits-per-sample
                 (1 (1+ (ash (1- width) -3)))
                 (4 (1+ (ash (1- width) -1)))
                 (8 width)
                 (16 (ash width 1)))))
          (let ((strip-length (ceiling (length decompressed-bytes) bytes-per-row)))
            (let ((end-row (+ start-row strip-length)))
              (loop for i from start-row below end-row
                 do
                   (let ((rowoff (* i bytes-per-row)))
                     (read-sequence array stream
                                    :start rowoff
                                    :end (+ rowoff bytes-per-row)))))))))))

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
       (let* ((bytes-per-row (1+ (ash (1- image-width) -3)))
              (data (make-array (* bytes-per-row image-length))))
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
       (let ((data (make-array (ceiling (* image-width image-length) 2))))
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
       (let* ((bytes-per-pixel 1)
              (data (make-array (* image-width image-length bytes-per-pixel))))
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
            (loop for i below image-length
               do
                 (loop for j from 1 below image-width
                    do
                      (let ((offset (+ (* i image-width) j)))
                        (setf (aref data offset)
                         (logand
                          (+ (aref data offset)
                             (aref data (1- offset)))
                          #xff)))))))

         (make-instance 'tiff-image
                        :length image-length :width image-width
                        :bits-per-sample bits-per-sample
                        :samples-per-pixel 1 :data data
                        :byte-order *byte-order*)))
      (16
       (let* ((bytes-per-pixel 2)
              (data (make-array (* image-width image-length bytes-per-pixel))))
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
            (loop for i below image-length
               do
                 (loop for j from 1 below image-width
                    do
                      (let ((offset (+ (* i image-width bytes-per-pixel)
                                       (* bytes-per-pixel j))))
                        (ecase *byte-order*
                          (:little-endian
                           (let ((old (+ (aref data (- offset 2))
                                         (ash (aref data (- offset 1)) 8))))
                             (let ((diff (+ (aref data offset)
                                            (ash (aref data (+ offset 1)) 8))))
                               (let ((new (logand
                                           (+ old diff)
                                           #xffff)))
                                 (setf (aref data offset)
                                       (ldb (byte 8 0) new))
                                 (setf (aref data (1+ offset))
                                       (ldb (byte 8 8) new))))))
                          (:big-endian
                           (let ((old (+ (aref data (- offset 1))
                                         (ash (aref data (- offset 2)) 8))))
                             (let ((diff (+ (aref data (1+ offset))
                                            (ash (aref data offset) 8))))
                               (let ((new (logand
                                           (+ old diff)
                                           #xffff)))
                                 (setf (aref data offset)
                                       (ldb (byte 8 8) new))
                                 (setf (aref data (1+ offset))
                                       (ldb (byte 8 0) new))))))))))))

         (make-instance 'tiff-image
                        :length image-length :width image-width
                        :bits-per-sample bits-per-sample
                        :samples-per-pixel 1 :data data
                        :byte-order *byte-order*)))
      (t
       (error "Unsupported grayscale bit depth: ~A" bits-per-sample)))))

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
              (bytes-per-sample (/ bytes-per-pixel samples-per-pixel)))
          (loop for i from start-row below (+ start-row strip-length)
             do
               (let ((rowoff (* i width bytes-per-pixel)))
                 (loop for j below width
                    do
                      (let ((pixoff (+ rowoff (* bytes-per-pixel j))))
                        (loop for k below samples-per-pixel
                           for bits across bits-per-sample
                           do
                             (case bits
                               (8
                                (setf (aref array (+ pixoff (* k bytes-per-sample)))
                                      (aref decompressed-bytes decoded-offset))
                                (incf decoded-offset))
                               (16
                                (let ((data-offset (+ pixoff (ash k 1))))
                                  (ecase *byte-order*
                                    (:big-endian
                                     (setf (aref array data-offset)
                                           (aref decompressed-bytes decoded-offset)
                                           (aref array (1+ data-offset))
                                           (aref decompressed-bytes (1+ decoded-offset))))
                                    (:little-endian
                                     (setf (aref array (1+ data-offset))
                                           (aref decompressed-bytes decoded-offset)
                                           (aref array data-offset)
                                           (aref decompressed-bytes (1+ decoded-offset)))))
                                  (incf decoded-offset 2))))))))))))))

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
      (loop for i from start-row below (+ start-row strip-length)
         do
           (let ((rowoff (* i width bytes-per-pixel)))
             (loop for j below width
                do
                  (let ((pixoff (+ rowoff (* bytes-per-pixel j)))
                        (k plane))
                    (case plane-bits-per-sample
                      (8
                       (setf (aref array (+ pixoff k))
                             (aref decompressed-bytes decoded-offset))
                       (incf decoded-offset))
                      (16
                       (let ((data-offset (+ pixoff (ash k 1))))
                         (ecase *byte-order*
                           (:big-endian
                            (setf (aref array data-offset)
                                  (aref decompressed-bytes decoded-offset)
                                  (aref array (1+ data-offset))
                                  (aref decompressed-bytes (1+ decoded-offset))))
                           (:little-endian
                            (setf (aref array (1+ data-offset))
                                  (aref decompressed-bytes decoded-offset)
                                  (aref array data-offset)
                                  (aref decompressed-bytes (1+ decoded-offset)))))
                         (incf decoded-offset 2)))))))))))

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
    (case planar-configuration
      (#.+planar-configuration-chunky+
       (let* ((bytes-per-pixel
               (* samples-per-pixel
                  (1+ (ash (1- (apply #'max
                                      (map 'list #'identity
                                           bits-per-sample)))
                           -3))))
              (data (make-array (* image-width image-length bytes-per-pixel))))
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
            (loop for i below image-length
               do 
                 (loop for j from 1 below image-width
                    do 
                      (let ((offset (+ (* i image-width samples-per-pixel)
                                       (* samples-per-pixel j))))
                        (loop for k below samples-per-pixel
                           do (setf (aref data (+ offset k))
                                    (logand
                                     (+ (aref data (+ offset k))
                                        (aref data (- (+ offset k) samples-per-pixel)))
                                     #xff))))))))
         (make-instance 'tiff-image
                        :length image-length :width image-width
                        :bits-per-sample bits-per-sample
                        :samples-per-pixel samples-per-pixel
                        :data data :byte-order *byte-order*)))

      (#.+planar-configuration-planar+
       (let* ((bytes-per-pixel
               (* samples-per-pixel
                  (1+ (ash (1- (reduce #'max bits-per-sample))
                           -3))))
              (strips-per-image
               (floor (+ image-length rows-per-strip -1) rows-per-strip))
              (data (make-array (* image-width image-length bytes-per-pixel))))
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
           (#.+horizontal-differencing+
            (loop for i below image-length
               do 
                 (loop for j from 1 below image-width
                    do 
                      (let ((offset (+ (* i image-width samples-per-pixel)
                                       (* samples-per-pixel j))))
                        (loop for k below samples-per-pixel
                           do (setf (aref data (+ offset k))
                                    (logand
                                     (+ (aref data (+ offset k))
                                        (aref data (- (+ offset k) samples-per-pixel)))
                                     #xff))))))))
         (make-instance 'tiff-image
                        :length image-length :width image-width
                        :bits-per-sample bits-per-sample
                        :samples-per-pixel samples-per-pixel
                        :data data :byte-order *byte-order*)))
      (t
       (error "Planar Configuration ~A not supported." planar-configuration)))))

(defun read-indexed-strip (stream array start-row strip-offset
                           strip-byte-count width bits-per-sample
                           bytes-per-pixel compression)
  (file-position stream strip-offset)
  (let ((compressed (read-bytes stream strip-byte-count)))
    (let ((decoded (funcall (find-compression-decoder compression) compressed))
	  (decoded-offset 0))
      (let ((strip-length (/ (length decoded) width)))
	(loop for i from start-row below (+ start-row strip-length)
	   do
	   (let ((rowoff (* i width bytes-per-pixel)))
	     (loop for j below width
		do
		(let ((pixoff (+ rowoff (* bytes-per-pixel j))))
		  (case bits-per-sample
		    (8
		     (setf (aref array pixoff)
			   (aref decoded decoded-offset))
		     (incf decoded-offset))
		    (16
		     (let ((data-offset pixoff))
		       (ecase *byte-order*
			 (:big-endian
			  (setf (aref array data-offset)
				(aref decoded decoded-offset)
				(aref array (1+ data-offset))
				(aref decoded (1+ decoded-offset))))
			 (:little-endian
			  (setf (aref array (1+ data-offset))
				(aref decoded decoded-offset)
				(aref array data-offset)
				(aref decoded (1+ decoded-offset)))))
		     (incf decoded-offset 2))))))))))))

(defun read-indexed-image (stream ifd)
  (let ((image-width (get-ifd-value ifd +image-width-tag+))
        (image-length (get-ifd-value ifd +image-length-tag+))
        (bits-per-sample (get-ifd-value ifd +bits-per-sample-tag+))
        (rows-per-strip (get-ifd-value ifd +rows-per-strip-tag+))
        (strip-offsets (get-ifd-values ifd +strip-offsets-tag+))
        (strip-byte-counts (get-ifd-values ifd +strip-byte-counts-tag+))
        (compression (get-ifd-value ifd +compression-tag+))
        (predictor (get-ifd-value ifd +predictor-tag+))
        (color-map (get-ifd-values ifd +color-map-tag+)))
    (let* ((k (expt 2 bits-per-sample))
           (color-index (make-array k)))
      (loop for i below k
         do (setf (aref color-index i)
                  (list (aref color-map i)
                        (aref color-map (+ k i))
                        (aref color-map (+ (ash k 1) i)))))
      ;; FIXME
      ;; 1. we need to support predictors for lzw encoded images.
      (let* ((bytes-per-pixel
              (1+ (ash (1- bits-per-sample)
                       -3)))
             (data (make-array (* image-width image-length bytes-per-pixel))))
        (loop for strip-offset across strip-offsets
           for strip-byte-count across strip-byte-counts
           for row-offset = 0 then (+ row-offset rows-per-strip)
           do (read-indexed-strip stream
                                  data
                                  row-offset
                                  strip-offset
                                  strip-byte-count
                                  image-width
                                  bits-per-sample
                                  bytes-per-pixel
                                  compression))
        (case predictor
          (#.+horizontal-differencing+
           (loop for i below image-length
              do 
              (loop for j from 1 below image-width
                 do 
                 (let ((offset (+ (* i image-width) j)))
                   (setf (aref data offset)
                         (logand
                          (+ (aref data offset)
                             (aref data (1- offset)))
                          #xff)))))))
        (make-instance 'tiff-image
                       :length image-length :width image-width
                       :bits-per-sample bits-per-sample
                       :samples-per-pixel 1
                       :data data :byte-order *byte-order*
                       :color-map color-index)))))

(defun read-tiff-stream (stream)
  (let* ((fields (read-value 'tiff-fields stream))
         (ifd (entries (first (ifd-list fields)))))
    (let ((photometric-interpretation 
         (get-ifd-value ifd +photometric-interpretation-tag+)))
    (ecase photometric-interpretation
      (#.+photometric-interpretation-white-is-zero+
       (read-grayscale-image stream ifd))
      (#.+photometric-interpretation-black-is-zero+
       ;;; FIXME! This image should be inverted
       (read-grayscale-image stream ifd))
      (#.+photometric-interpretation-rgb+
       (read-rgb-image stream ifd))
      (#.+photometric-interpretation-palette-color+
       (read-indexed-image stream ifd))))))

(defun read-tiff-file (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (read-tiff-stream stream)))


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
(defun write-tiff-stream (stream obj &key byte-order)
  (let ((*byte-order* (or byte-order *byte-order*))
        (*tiff-file-offset* 0))
    (multiple-value-bind (fields out-of-line-data-size strip-offsets strip-byte-counts)
        (make-tiff-fields obj)
      (write-value 'tiff-fields stream fields)
      (file-position stream (+ (file-position stream) out-of-line-data-size))
      (with-accessors
            ((image-width tiff-image-width)
             (image-length tiff-image-length)
             (image-data tiff-image-data))
          obj
        (loop for start in strip-offsets
           for count in strip-byte-counts
           do
             (write-sequence (subseq image-data start
                                     (+ start count))
                             stream))))))

(defun write-tiff-file (pathname image &rest args &key (if-exists :error) &allow-other-keys)
  (with-open-file (stream pathname
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-exists if-exists)
    (apply #'write-tiff-stream stream image (remove-keyword-args :if-exists args))
    pathname))

