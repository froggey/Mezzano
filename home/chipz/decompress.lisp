(in-package :chipz)

;;; We provide several convenience functions for decompression:
;;;
;;; * decompress a buffer to a newly-consed buffer;
;;; * decompress a stream to a newly-consed buffer;
;;; * decompress a buffer to a user-specified buffer;
;;; * decompress a buffer to a stream;
;;; * decompress a stream to a stream.
;;;
;;; We do not provide stream->buffer decompression, as we have no way of
;;; knowing how much to read from the stream to fill the buffer, no way
;;; of determining what to do with possible state left in the
;;; INFLATE-STATE that we used, etc.  Application-specific logic will
;;; have to handle those bits.

(defgeneric decompress (output state input &key &allow-other-keys)
  (:method (output format input &rest keys)
    (error 'invalid-format-error :format format))
  (:method (output (format (eql :deflate)) input &rest keys)
    (%decompress output format input keys))
  (:method (output (format (eql 'deflate)) input &rest keys)
    (%decompress output format input keys))
  (:method (output (format (eql :zlib)) input &rest keys)
    (%decompress output format input keys))
  (:method (output (format (eql 'zlib)) input &rest keys)
    (%decompress output format input keys))
  (:method (output (format (eql :gzip)) input &rest keys)
    (%decompress output format input keys))
  (:method (output (format (eql 'gzip)) input &rest keys)
    (%decompress output format input keys))
  #-mezzano (:method (output (format (eql :bzip2)) input &rest keys)
    (%decompress output format input keys))
  #-mezzano (:method (output (format (eql 'bzip2)) input &rest keys)
    (%decompress output format input keys)))

(defun %decompress (output format input keys)
  (let ((state (make-dstate format)))
    (multiple-value-prog1 (apply #'decompress output state input keys)
      (finish-dstate state))))

;;; SUBSEQ is specified to always make a copy.  But we don't want an
;;; exact copy of a freshly-consed vector; that'd be wasteful.
(defun maybe-subseq (v end)
  (if (= end (length v))
      v
      (subseq v 0 end)))

(defun decompress-fun-for-state (state)
  (typecase state
    (inflate-state #'%inflate)
    (bzip2-state #'%bzip2-decompress)))

(defun %decompress/null-vector (state input fun
                                input-start input-end buffer-size)
  (declare (type function fun))
  (loop
     with output = (make-array buffer-size :element-type '(unsigned-byte 8))
     with output-start = 0
     do (cond
          ((= output-start (length output))
           ;; Reallocate the output buffer.
           (let ((new (make-array (* 2 (length output))
                                  :element-type '(unsigned-byte 8))))
             (setf output (replace new output))))
          (t
           (multiple-value-bind (consumed produced)
               (funcall fun state input output
                        :input-start input-start :input-end input-end
                        :output-start output-start :output-end (length output))
             (incf input-start consumed)
             (incf output-start produced)
             (when (or (dstate-done state)
                       (and (or (>= input-start input-end)
                                (zerop consumed))
                            (zerop produced)))
               (return-from %decompress/null-vector (maybe-subseq output output-start))))))))

(defmethod decompress ((output null) (state decompression-state) (input vector)
                       &key (input-start 0) input-end buffer-size
                       &allow-other-keys)
  (%decompress/null-vector state input
                           (decompress-fun-for-state state)
                           input-start (or input-end (length input))
                           (or buffer-size +default-buffer-size+)))

(defun %decompress/null-stream (state input fun buffer-size)
  (declare (type function fun))
  (let ((input-buffer (make-array 8192 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent input-buffer))
    (loop
       with input-start = 0
       with input-end = 0
       with output = (make-array buffer-size :element-type '(unsigned-byte 8))
       with output-start = 0
       initially (setf input-end (read-sequence input-buffer input))
       do (cond
            ((= output-start (length output))
             ;; Reallocate the output buffer.
             (let ((new (make-array (* 2 (length output))
                                    :element-type '(unsigned-byte 8))))
               (setf output (replace new output))))
            (t
             (multiple-value-bind (consumed produced)
                 (funcall fun state input-buffer output
                          :input-start input-start :input-end input-end
                          :output-start output-start)
               (incf input-start consumed)
               (incf output-start produced)
               (let ((input-consumed-p (>= input-start input-end)))
                 ;; Get more input if possible.
                 (when input-consumed-p
                   (setf input-start 0
                         input-end (read-sequence input-buffer input)))
                 (when (or (dstate-done state)
                           (and (or (and input-consumed-p (zerop input-end))
                                    (zerop consumed))
                                (zerop produced)))
                   (return-from %decompress/null-stream (maybe-subseq output output-start))))))))))

(defmethod decompress ((output null) (state decompression-state) (input stream)
                       &key buffer-size &allow-other-keys)
  (%decompress/null-stream state input
                           (decompress-fun-for-state state)
                           (or buffer-size +default-buffer-size+)))

(defun %decompress/vector-vector (output state input fun
                                  input-start input-end
                                  output-start output-end)
  (declare (type simple-octet-vector input output))
  (declare (type function fun))
  (loop
     with n-bytes-consumed = 0 and n-bytes-produced = 0
     do (multiple-value-bind (consumed produced)
            (funcall fun state input output
                     :input-start input-start :input-end input-end
                     :output-start output-start :output-end output-end)
          (incf input-start consumed)
          (incf output-start produced)
          (incf n-bytes-consumed consumed)
          (incf n-bytes-produced produced)
          (when (and (or (>= input-start input-end)
                         (zerop consumed))
                     (or (>= output-start output-end)
                         (zerop produced)))
            (return-from %decompress/vector-vector 
              (values n-bytes-consumed n-bytes-produced))))))

(defmethod decompress ((output vector) (state decompression-state) (input vector)
                       &key (input-start 0) input-end
                       (output-start 0) output-end &allow-other-keys)
  (%decompress/vector-vector output state input
                             (decompress-fun-for-state state)
                             input-start (or input-end (length input))
                             output-start (or output-end (length output))))

(defun %decompress/stream-vector (output state input fun input-start input-end)
  (declare (type function fun))
  (let ((buffer (make-array 8192 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent buffer))
    (loop (multiple-value-bind (consumed produced)
              (funcall fun state input buffer
                       :input-start input-start :input-end input-end)
            (incf input-start consumed)
            (write-sequence buffer output :end produced)
            (when (or (dstate-done state)
                      (and (or (>= input-start input-end)
                               (zerop consumed))
                           (zerop produced)))
              (return-from %decompress/stream-vector output))))))

(defmethod decompress ((output stream) (state decompression-state) (input vector)
                       &key (input-start 0) input-end &allow-other-keys)
  (%decompress/stream-vector output state input
                             (decompress-fun-for-state state)
                             input-start (or input-end (length input))))

(defun %decompress/stream-stream (output state input fun)
  (declare (type function fun))
  (let ((input-buffer (make-array 8192 :element-type '(unsigned-byte 8)))
        (output-buffer (make-array 8192 :element-type '(unsigned-byte 8))))
    (declare (dynamic-extent input-buffer output-buffer))
    (loop
       with input-start = 0
       with input-end = 0
       initially (setf input-end (read-sequence input-buffer input))
       do (multiple-value-bind (consumed produced)
              (funcall fun state input-buffer output-buffer
                       :input-start input-start :input-end input-end)
            (incf input-start consumed)
            (write-sequence output-buffer output :end produced)
            (let ((input-consumed-p (>= input-start input-end)))
              (when input-consumed-p
                (setf input-start 0
                      input-end (read-sequence input-buffer input)))
              (when (or (dstate-done state)
                        (and (or (and input-consumed-p (zerop input-end))
                                 (zerop consumed))
                             (zerop produced)))
                (return-from %decompress/stream-stream output)))))))

(defmethod decompress ((output stream) (state decompression-state) (input stream)
                       &key &allow-other-keys)
  (%decompress/stream-stream output state input
                             (decompress-fun-for-state state)))
