;;;; Copyright (c) 2019 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.file-system-cache
  (:use :cl)
  (:export #:file-cache-stream
           #:file-cache-character-stream
           #:file-%direction
           #:file-%buffer
           #:file-%position
           #:file-%length))

(in-package :mezzano.file-system-cache)

(defclass file-cache-stream (file-stream)
  ((%direction :initarg :direction :reader file-%direction)
   (%buffer :initarg :buffer :accessor file-%buffer)
   (%position :initarg :position :accessor file-%position)
   (%length :initarg :length :accessor file-%length))
  (:default-initargs :position 0 :length 0))

(defclass file-cache-character-stream (mezzano.internals::external-format-mixin
                                       file-cache-stream)
  ())

(defmethod mezzano.gray:stream-element-type ((stream file-cache-stream))
  (declare (ignore stream))
  '(unsigned-byte 8))

(defmethod mezzano.gray:stream-element-type ((stream file-cache-character-stream))
  (declare (ignore stream))
  'character)

(defmethod mezzano.gray:stream-external-format ((stream file-cache-stream))
  (declare (ignore stream))
  :default)

(defmethod input-stream-p ((stream file-cache-stream))
  (member (file-%direction stream) '(:input :io)))

(defmethod output-stream-p ((stream file-cache-stream))
  (member (file-%direction stream) '(:output :io)))

(defmethod mezzano.gray:stream-write-byte ((stream file-cache-stream) byte)
  (assert (member (file-%direction stream) '(:output :io)))
  (when (>= (file-%position stream) (file-%length stream))
    (setf (file-%length stream) (1+ (file-%position stream)))
    (let ((array-length (array-dimension (file-%buffer stream) 0)))
      (when (>= (file-%position stream) array-length)
        (setf (file-%buffer stream) (adjust-array (file-%buffer stream) (+ array-length 8192) :initial-element 0)))))
  (setf (aref (file-%buffer stream) (file-%position stream)) byte)
  (incf (file-%position stream)))

(defmethod mezzano.gray:stream-read-byte ((stream file-cache-stream))
  (assert (member (file-%direction stream) '(:input :io)))
  (prog1 (if (>= (file-%position stream) (file-%length stream))
             :eof
             (aref (file-%buffer stream) (file-%position stream)))
    (incf (file-%position stream))))

(defmethod mezzano.gray:stream-write-sequence ((stream file-cache-stream) sequence &optional (start 0) end)
  (assert (member (file-%direction stream) '(:output :io)))
  (let* ((end (or end (file-%length stream)))
         (end2 (min end (length sequence))))
    (when (> end2 (file-%length stream))
      (setf (file-%buffer stream) (adjust-array (file-%buffer stream) end2 :initial-element 0)))
    (replace (file-%buffer stream) sequence
             :start1 (file-%position stream)
             :end1 (+ (file-%position stream) (- end start))
             :start2 start
             :end2 end2)
    (incf (file-%position stream) (- end2 start))))

(defmethod mezzano.gray:stream-read-sequence ((stream file-cache-stream) sequence &optional (start 0) end)
  (assert (member (file-%direction stream) '(:input :io)))
  (let* ((end (or end (length sequence)))
         (end2 (min end (file-%length stream))))
    (replace sequence (file-%buffer stream) :start1 start :end1 end :start2 0 :end2 end2)
    end2))

(defmethod mezzano.gray:stream-file-position ((stream file-cache-stream) &optional (position-spec nil position-specp))
  (if position-specp
      (setf (file-%position stream) (case position-spec
                                      (:start 0)
                                      (:end (file-%length stream))
                                      (t position-spec)))
      (file-%position stream)))

(defmethod mezzano.gray:stream-file-length ((stream file-cache-stream))
  (file-%length stream))
