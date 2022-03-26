;;;; stream.lisp -- gray stream wrappers for INFLATE

(in-package :chipz)


;;; portability definitions

#+cmu
(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :gray-streams))

;;; TRIVIAL-GRAY-STREAMS has it, we might as well, too...
#+allegro
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (fboundp 'stream:stream-write-string)
    (require "streamc.fasl")))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defvar *binary-input-stream-class*
  (quote
   #+lispworks stream:fundamental-binary-input-stream
   #+sbcl sb-gray:fundamental-binary-input-stream
   #+openmcl gray:fundamental-binary-input-stream
   #+cmu ext:fundamental-binary-input-stream
   #+allegro excl:fundamental-binary-input-stream
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *stream-read-byte-function*
  (quote
   #+lispworks stream:stream-read-byte
   #+sbcl sb-gray:stream-read-byte
   #+openmcl gray:stream-read-byte
   #+cmu ext:stream-read-byte
   #+allegro excl:stream-read-byte
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))

(defvar *stream-read-sequence-function*
  (quote
   #+lispworks stream:stream-read-sequence
   #+sbcl sb-gray:stream-read-sequence
   #+openmcl ccl:stream-read-vector
   #+cmu ext:stream-read-sequence
   #+allegro excl:stream-read-sequence
   #-(or lispworks sbcl openmcl cmu allegro)
   (error "octet streams not supported in this implementation")))
) ; EVAL-WHEN

;;; READ-SEQUENCE

(defmacro define-stream-read-sequence (specializer &body body)
  #+sbcl
  `(defmethod sb-gray:stream-read-sequence ((stream ,specializer) seq &optional (start 0) end)
     (cond
       ((not (typep seq 'simple-octet-vector))
        (call-next-method))
       (t
        (let ((end (or end (length seq))))
          ,@body))))
  #+cmu
  `(defmethod ext:stream-read-sequence ((stream ,specializer) seq &optional (start 0) end)
     (cond
       ((not (typep seq 'simple-octet-vector))
        (call-next-method))
       (t
        (let ((end (or end (length seq))))
          ,@body))))
  #+allegro
  `(defmethod excl:stream-read-sequence ((stream ,specializer) seq &optional (start 0) end)
     (cond
       ((not (typep seq 'simple-octet-vector))
        (call-next-method))
       (t
        (let ((end (or end (length seq))))
          ,@body))))
  #+openmcl
  `(defmethod ccl:stream-read-vector ((stream ,specializer) seq start end)
     (cond
       ((not (typep seq 'simple-octet-vector))
        (call-next-method))
       (t
        ,@body)))
  #+lispworks
  `(defmethod stream:stream-read-sequence ((stream ,specializer) seq start end)
     (cond
       ((not (typep seq 'simple-octet-vector))
        (call-next-method))
       (t
        ,@body))))


;;; class definition

(defclass decompressing-stream (#.*binary-input-stream-class*)
  ((wrapped-stream :initarg :stream :reader wrapped-stream)
   (dstate :initarg :dstate :reader dstate)
   (dfun :initarg :dfun :reader dfun)
   (input-buffer :initform (make-array 4096 :element-type '(unsigned-byte 8))
                 :reader input-buffer)
   (input-buffer-index :initform 0 :accessor input-buffer-index)
   (input-buffer-n-bytes :initform 0 :accessor input-buffer-n-bytes)
   (output-buffer :initform (make-array 4096 :element-type '(unsigned-byte 8))
                  :reader output-buffer)
   (output-buffer-index :initform 0 :accessor output-buffer-index)
   (output-buffer-n-bytes :initform 0 :accessor output-buffer-n-bytes)))

;;; constructors
(defun make-decompressing-stream (format stream)
  (multiple-value-bind (state dfun)
      (ecase format
        ((:deflate :zlib :gzip deflate zlib gzip)
         (values (make-inflate-state format) #'%inflate))
        ((:bzip2 bzip2)
         (values (make-bzip2-state) #'%bzip2-decompress)))
    (make-instance 'decompressing-stream
                   :stream stream
                   :dstate state
                   :dfun dfun)))


;;; stream management

(defun output-available-p (stream)
  (/= (output-buffer-index stream) (output-buffer-n-bytes stream)))

(defun input-available-p (stream)
  (/= (input-buffer-index stream) (input-buffer-n-bytes stream)))

(defun refill-stream-input-buffer (stream)
  (with-slots (input-buffer wrapped-stream
                            input-buffer-index input-buffer-n-bytes)
      stream
    (let ((n-bytes-read (read-sequence input-buffer wrapped-stream)))
      (setf input-buffer-index 0 input-buffer-n-bytes n-bytes-read)
      #+nil
      (format *trace-output* "index: ~D | n-bytes ~D~%"
              input-buffer-index input-buffer-n-bytes)
      (values))))

(defun refill-stream-output-buffer (stream)
  (unless (input-available-p stream)
    (refill-stream-input-buffer stream))
  (multiple-value-bind (bytes-read bytes-output)
      (funcall (the function (dfun stream))
               (dstate stream)
               (input-buffer stream)
               (output-buffer stream)
                :input-start (input-buffer-index stream)
                :input-end (input-buffer-n-bytes stream))
    (setf (output-buffer-index stream) 0
          (output-buffer-n-bytes stream) bytes-output
          (input-buffer-index stream) (+ (input-buffer-index stream) bytes-read))
    (assert (<= (input-buffer-index stream) (input-buffer-n-bytes stream)))))


;;; methods

(defun read-and-decompress-byte (stream)
  (unless (output-available-p stream)
    (refill-stream-output-buffer stream))
  ;; FIXME: should we cache this, so we don't try to refill all the time?
  (cond
    ((output-available-p stream)
     (prog1 (aref (output-buffer stream) (output-buffer-index stream))
       (incf (output-buffer-index stream))))
    (t
     (finish-dstate (dstate stream))
     :eof)))

(defun copy-existing-output (stream seq start end)
  (declare (type simple-octet-vector seq))
  (let ((amount (min (- start end)
                     (- (output-buffer-n-bytes stream)
                        (output-buffer-index stream)))))
    (replace seq (output-buffer stream)
             :start1 start :end1 end
             :start2 (output-buffer-index stream)
             :end2 (output-buffer-n-bytes stream))
    (+ start amount)))

(define-stream-read-sequence decompressing-stream
  (unless (typep seq 'simple-octet-vector)
    (return-from #.*stream-read-sequence-function* (call-next-method)))
  (loop initially (when (output-available-p stream)
                    (setf start (copy-existing-output stream seq
                                                      start end)))
     while (< start end)
     do (unless (input-available-p stream)
          (refill-stream-input-buffer stream))
       ;; If we didn't refill, then we must be all done.
       (unless (input-available-p stream)
         (finish-dstate (dstate stream))
         (loop-finish))
       ;; Decompress directly into the user-provided buffer.
       (multiple-value-bind (bytes-read bytes-output)
           (funcall (the function (dfun stream))
                    (dstate stream)
                    (input-buffer stream)
                    seq
                    :input-start (input-buffer-index stream)
                    :input-end (input-buffer-n-bytes stream)
                    :output-start start
                    :output-end end)
         (incf (input-buffer-index stream) bytes-read)
         (incf start bytes-output))
     finally (return start)))

(defmethod #.*stream-read-byte-function* ((stream decompressing-stream))
  (read-and-decompress-byte stream))
