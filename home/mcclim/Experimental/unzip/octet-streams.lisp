(in-package :unzip)

;;;

(defclass octet-input-stream (fundamental-binary-input-stream)
  ((data     :initarg :data     :accessor data-of)
   (data-end :initarg :data-end :accessor data-end-of)
   (data-ptr :initarg :data-ptr :accessor data-ptr-of)))

(defmethod stream-read-byte ((stream octet-input-stream))
  (cond ((>= (data-ptr-of stream)
             (data-end-of stream))
         :eof)
        (t
         (prog1 (aref (data-of stream) (data-ptr-of stream))
           (incf (data-ptr-of stream))))))

(defun make-octet-input-stream (octets &key (start 0) (end (length octets)))
  (assert (and (integerp start) (<= 0 start (length octets))))
  (assert (and (integerp end) (<= 0 end (length octets))))
  (assert (<= start end))
  (assert (typep octets 'vector))
  (make-instance 'octet-input-stream
    :data octets
    :data-end end
    :data-ptr start))

;;;;

(defclass octet-output-stream (fundamental-binary-output-stream)
  ((buffer :initarg :buffer :accessor buffer-of)))

(defmethod stream-write-byte ((stream octet-output-stream) byte)
  (vector-push-extend byte (buffer-of stream)))

(defun make-octet-output-stream ()
  (make-instance 'octet-output-stream
    :buffer (make-array 0 :element-type '(unsigned-byte 8)
                          :fill-pointer 0
                          :adjustable t)))

(defun get-output-stream-octet-vector (stream)
  (buffer-of stream))

(defmacro with-output-to-octet-vector ((var) &body body)
  `(let ((,var (make-octet-output-stream)))
     (locally ,@body)
     (get-output-stream-octet-vector ,var)))

;;;;
