(in-package :png-read)

(defgeneric parse-critical-chunk (chunk-type chunk-data)
  (:method (chunk-type chunk-data)
    (error "Unknown critical chunk ~a." chunk-type)))

(defmethod parse-critical-chunk ((chunk-type (eql '|IHDR|)) chunk-data)
  (let ((width (big-endian-vector-to-integer (subseq chunk-data 0 4)))
        (height (big-endian-vector-to-integer (subseq chunk-data 4 8)))
        (bit-depth (aref chunk-data 8))
        (colour-type (aref chunk-data 9))
        (compression (aref chunk-data 10))
        (filter-method (aref chunk-data 11))
        (interlace-method (aref chunk-data 12)))
    (setf (width *png-state*) width
          (height *png-state*) height
          (bit-depth *png-state*) bit-depth
          (colour-type *png-state*) (ecase colour-type
                                      (0 :greyscale)
                                      (2 :truecolor)
                                      (3 :indexed-colour)
                                      (4 :greyscale-alpha)
                                      (6 :truecolor-alpha))
          (compression *png-state*) (ecase compression
                                      (0 :zlib))
          (filter-method *png-state*) (ecase filter-method
                                        (0 :standard-filter))
          (interlace-method *png-state*) (ecase interlace-method
                                           (0 :no-interlace)
                                           (1 :adam7-interlace)))))

(defmethod parse-critical-chunk ((chunk-type (eql '|PLTE|)) chunk-data)
  (unless (zerop (mod (length chunk-data) 3))
    (error "Corrupted pallete data."))
  (let ((data-length (/ (length chunk-data) 3)))
   (let ((pallete-array (make-array (list data-length 3) :element-type '(unsigned-byte 8))))
     (iter (for i from 0 below data-length)
           (setf (aref pallete-array i 0) (aref chunk-data (* i 3))
                 (aref pallete-array i 1) (aref chunk-data (+ 1 (* i 3)))
                 (aref pallete-array i 2) (aref chunk-data (+ 2 (* i 3)))))
     (setf (pallete *png-state*) pallete-array))))

(defmethod parse-critical-chunk ((chunk-type (eql '|IDAT|)) chunk-data)
  (when (zerop (length chunk-data))
    (cerror "Ignore this chunk." "Empty IDAT chunk."))
  (push chunk-data (datastream *png-state*)))

(defmethod parse-critical-chunk ((chunk-type (eql '|IEND|)) chunk-data)
  (let* ((l (reduce #'+ (datastream *png-state*) :key #'length))
         (v (make-array (list l) :element-type '(unsigned-byte 8))))
    (loop
      for start = 0 then (+ start (length chunk))
      for chunk of-type (simple-array (unsigned-byte 8) (*)) in (reverse (datastream *png-state*))
      do (replace v chunk :start1 start))
    (setf (datastream *png-state*) v))
  (finish-decoding *png-state*)
  (dolist (tk (postprocess-ancillaries *png-state*))
    (funcall tk *png-state*))
  (setf (finished *png-state*) t))
