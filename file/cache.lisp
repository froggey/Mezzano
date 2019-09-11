;;;; Copyright (c) 2019 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.file-system-cache
  (:use :cl)
  (:export #:file-cache-stream
           #:file-cache-character-stream
           #:direction
           #:buffer
           #:file-position*
           #:file-length*))

(in-package :mezzano.file-system-cache)

(defclass file-cache-stream (file-stream)
  ((%direction :initarg :direction :reader direction)
   (%buffer :initarg :buffer :accessor buffer)
   (%position :initarg :position :accessor file-position*)
   (%length :initarg :length :accessor file-length*))
  (:default-initargs :position 0 :length 0))

(defclass file-cache-character-stream (file-cache-stream)
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

(defmethod mezzano.gray:stream-external-format ((stream file-cache-character-stream))
  (declare (ignore stream))
  :utf-8)

(defmethod input-stream-p ((stream file-cache-stream))
  (member (direction stream) '(:input :io)))

(defmethod output-stream-p ((stream file-cache-stream))
  (member (direction stream) '(:output :io)))

(defmethod mezzano.gray:stream-write-byte ((stream file-cache-stream) byte)
  (assert (member (direction stream) '(:output :io)))
  (when (>= (file-position* stream) (file-length* stream))
    (setf (file-length* stream) (1+ (file-position* stream)))
    (let ((array-length (array-dimension (buffer stream) 0)))
      (when (>= (file-position* stream) array-length)
        (setf (buffer stream) (adjust-array (buffer stream) (+ array-length 8192) :initial-element 0)))))
  (setf (aref (buffer stream) (file-position* stream)) byte)
  (incf (file-position* stream)))

(defmethod mezzano.gray:stream-read-byte ((stream file-cache-stream))
  (assert (member (direction stream) '(:input :io)))
  (let ((offset (file-position* stream)))
    (incf (file-position* stream))
    (if (>= offset (file-length* stream))
        :eof
        (aref (buffer stream) offset))))

(defmethod mezzano.gray:stream-write-char ((stream file-cache-character-stream) char)
  (assert (member (direction stream) '(:output :io)))
  (when (>= (file-position* stream) (file-length* stream))
    (setf (file-length* stream) (1+ (file-position* stream)))
    (let ((array-length (array-dimension (buffer stream) 0)))
      (when (>= (file-position* stream) array-length)
        (setf (buffer stream) (adjust-array (buffer stream) (+ array-length 8192) :initial-element 0)))))
  (setf (aref (buffer stream) (file-position* stream)) (char-code char))
  (incf (file-position* stream)))

(defmethod mezzano.gray:stream-read-char ((stream file-cache-character-stream))
  (assert (member (direction stream) '(:input :io)))
  (let ((offset (file-position* stream)))
    (incf (file-position* stream))
    (if (>= offset (file-length* stream))
        :eof
        (code-char (aref (buffer stream) offset)))))

(defmethod mezzano.gray:stream-write-sequence ((stream file-cache-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:output :io)))
  (unless end (setf end (file-length* stream)))
  (let ((end2 (min end (length sequence))))
    (when (> end2 (file-length* stream))
      (setf (buffer stream) (adjust-array (buffer stream) end2 :initial-element 0)))
    (replace (buffer stream) sequence :start1 0 :end1 end :start2 start :end2 end2)
    end2))

(defmethod mezzano.gray:stream-read-sequence ((stream file-cache-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (unless end (setf end (length sequence)))
  (let ((end2 (min end (file-length* stream))))
    (replace sequence (buffer stream) :start1 start :end1 end :start2 0 :end2 end2)
    end2))

(defmethod mezzano.gray:stream-write-sequence ((stream file-cache-character-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:output :io)))
  (unless end (setf end (file-length* stream)))
  (let ((end2 (min end (length sequence))))
    (when (> end2 (file-length* stream))
      (setf (buffer stream) (adjust-array (buffer stream) end2 :initial-element 0)))
    (loop :for n1 :to (1- end2)
          :for n2 :from start :to (1- end)
          :do (setf (aref (buffer stream) n1)
                    (char-code (aref sequence n2))))
    end2))

(defmethod mezzano.gray:stream-read-sequence ((stream file-cache-character-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (unless end (setf end (length sequence)))
  (let ((end2 (min end (file-length* stream))))
    (loop :for n1 :from start :to (1- end)
          :for n2 :to (1- end2)
          :do (setf (aref sequence n1)
                    (code-char (aref (buffer stream) n2))))
    end2))

(defmethod mezzano.gray:stream-file-position ((stream file-cache-stream) &optional (position-spec nil position-specp))
  (if position-specp
      (setf (file-position* stream) (case position-spec
                                      (:start 0)
                                      (:end (file-length* stream))
                                      (t position-spec)))
      (t (file-position* stream))))

(defmethod mezzano.gray:stream-file-length ((stream file-cache-stream))
  (file-length* stream))
