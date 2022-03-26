
(in-package :retrospectiff.compression)

(defun packbits-encode (raw-vector)
  (declare (ignore raw-vector))
  (error "packbits-encode doesn't work yet")
  #+nil
  (typecase raw-vector
    (string
     (packbits-encode
      (map 'vector #'char-code raw-vector)))
    (vector
     (let ((output (make-array 256
                               :element-type '(unsigned-byte 8)
                               :fill-pointer 0
                               :adjustable t)))
       #+nil
       (loop for i below (length vector)
            )
       output))))

(defun packbits-decode (compressed-vector &key stream (bits 8))
  (declare (optimize (debug 3)))
  (let ((output (make-array 256
                            :element-type `(unsigned-byte ,bits)
                            :fill-pointer 0
                            :adjustable t))
        (input-byte-offset 0)
        (input-length (length compressed-vector))
        (output-byte-offset 0))
    (flet ((output-byte (byte)
             (if stream
                 (write-byte byte stream)
                 (progn
                   (ensure-array-size-and-set-fill-pointer
                    output output-byte-offset)
                   (setf (aref output output-byte-offset) byte)
                   (setf (fill-pointer output)
                         (incf output-byte-offset)))))
           (next-input-byte ()
             (when (< input-byte-offset input-length)
               (prog1
                   (aref compressed-vector input-byte-offset)
                 (incf input-byte-offset)))))
      (loop for byte = (next-input-byte)
         while byte
         do 
           (let ((byte (convert-to-signed-integer byte bits)))
             (cond ((equal -128 byte))
                   ((minusp byte)
                    (let ((output (next-input-byte)))
                      (loop for i to (- byte)
                         do
                         (output-byte output))))
                   (t
                    (loop for i to byte
                       do
                       (output-byte (next-input-byte))))))))
    output))
