(in-package :fast-io)

 ;; fast-output-stream

(defclass fast-output-stream (fundamental-output-stream)
  ((buffer :type output-buffer)))

(defmethod initialize-instance ((self fast-output-stream) &key stream
                                buffer-size &allow-other-keys)
  (call-next-method)
  (let ((*default-output-buffer-size* (or buffer-size *default-output-buffer-size*)))
    (with-slots (buffer) self
      (setf buffer (make-output-buffer :output stream)))))

(defmethod stream-write-byte ((stream fast-output-stream) byte)
  (with-slots (buffer) stream
    (fast-write-byte byte buffer)))

(defmethod stream-write-sequence ((stream fast-output-stream) sequence start end
                                  &key &allow-other-keys)
  (with-slots (buffer) stream
    (fast-write-sequence sequence buffer start end))
  sequence)

(defun finish-output-stream (stream)
  (with-slots (buffer) stream
    (if (streamp (output-buffer-output buffer))
        (flush buffer)
        (finish-output-buffer buffer))))

 ;; fast-input-stream

(defclass fast-input-stream (fundamental-input-stream)
  ((buffer :type input-buffer)))

(defmethod initialize-instance ((self fast-input-stream) &key stream
                                vector &allow-other-keys)
  (call-next-method)
  (with-slots (buffer) self
    (setf buffer (make-input-buffer :vector vector :stream stream))))

(defmethod stream-read-sequence ((stream fast-input-stream) sequence start end
                                 &key &allow-other-keys)
  (with-slots (buffer) stream
    (fast-read-sequence sequence buffer start end)))
