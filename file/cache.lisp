;;;; Copyright (c) 2019 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.file-system-cache
  (:use :cl)
  (:export #:file-cache-stream
           #:file-cache-character-stream
           #:direction
           #:buffer
           #:buffer-offset
           #:buffer-size))

(in-package :mezzano.file-system-cache)

(defclass file-cache-stream (file-stream)
  ((direction :initarg :direction :reader direction)
   (buffer :initarg :buffer
           :accessor buffer)
   (buffer-offset :initarg :buffer-offset
                  :accessor buffer-offset)
   (buffer-size :initarg :buffer-size
                :initform 0
                :accessor buffer-size)))

(defclass file-cache-character-stream (file-cache-stream)
  ())

(defmethod sys.gray:stream-element-type ((stream file-cache-stream))
  (declare (ignore stream))
  '(unsigned-byte 8))

(defmethod sys.gray:stream-element-type ((stream file-cache-character-stream))
  (declare (ignore stream))
  'character)

(defmethod sys.gray:stream-external-format ((stream file-cache-stream))
  (declare (ignore stream))
  :default)

(defmethod sys.gray:stream-external-format ((stream file-cache-character-stream))
  (declare (ignore stream))
  :utf-8)

(defmethod input-stream-p ((stream file-cache-stream))
  (member (direction stream) '(:input :io)))

(defmethod output-stream-p ((stream file-cache-stream))
  (member (direction stream) '(:output :io)))

(defmethod sys.gray:stream-write-byte ((stream file-cache-stream) byte)
  (assert (member (direction stream) '(:output :io)))
  (when (> (buffer-offset stream) (buffer-size stream))
    (setf (buffer-size stream) (buffer-offset stream))
    (let ((array-size (array-dimension (buffer stream) 0)))
      (when (>= (buffer-offset stream) array-size)
        (setf (buffer stream) (adjust-array (buffer stream) (+ array-size 8192) :initial-element 0)))))
  (setf (aref (buffer stream) (buffer-offset stream)) byte)
  (incf (buffer-offset stream)))

(defmethod sys.gray:stream-read-byte ((stream file-cache-stream))
  (assert (member (direction stream) '(:input :io)))
  (let ((offset (buffer-offset stream)))
    (incf (buffer-offset stream))
    (if (>= offset (buffer-size stream))
        :eof
        (aref (buffer stream) offset))))

(defmethod sys.gray:stream-write-char ((stream file-cache-character-stream) char)
  (assert (member (direction stream) '(:output :io)))
  (when (> (buffer-offset stream) (buffer-size stream))
    (setf (buffer-size stream) (buffer-offset stream))
    (let ((array-size (array-dimension (buffer stream) 0)))
      (when (>= (buffer-offset stream) array-size)
        (setf (buffer stream) (adjust-array (buffer stream) (+ array-size 8192) :initial-element 0)))))
  (setf (aref (buffer stream) (buffer-offset stream)) (char-code char))
  (incf (buffer-offset stream)))

(defmethod sys.gray:stream-read-char ((stream file-cache-character-stream))
  (assert (member (direction stream) '(:input :io)))
  (let ((offset (buffer-offset stream)))
    (incf (buffer-offset stream))
    (if (>= offset (buffer-size stream))
        :eof
        (code-char (aref (buffer stream) offset)))))

(defmethod sys.gray:stream-write-sequence ((stream file-cache-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:output :io)))
  (unless end (setf end (buffer-size stream)))
  (let ((end2 (min end (length sequence))))
    (when (> end2 (buffer-size stream))
      (setf (buffer stream) (adjust-array (buffer stream) end2 :initial-element 0)))
    (replace (buffer stream) sequence :start1 0 :end1 end :start2 start :end2 end2)
    end2))

(defmethod sys.gray:stream-read-sequence ((stream file-cache-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (unless end (setf end (length sequence)))
  (let ((end2 (min end (buffer-size stream))))
    (replace sequence (buffer stream) :start1 start :end1 end :start2 0 :end2 end2)
    end2))

(defmethod sys.gray:stream-write-sequence ((stream file-cache-character-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:output :io)))
  (unless end (setf end (buffer-size stream)))
  (let ((end2 (min end (length sequence))))
    (when (> end2 (buffer-size stream))
      (setf (buffer stream) (adjust-array (buffer stream) end2 :initial-element 0)))
    (loop :for n1 :to (1- end2)
          :for n2 :from start :to (1- end)
          :do (setf (aref (buffer stream) n1)
                    (char-code (aref sequence n2))))
    end2))

(defmethod sys.gray:stream-read-sequence ((stream file-cache-character-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (unless end (setf end (length sequence)))
  (let ((end2 (min end (buffer-size stream))))
    (loop :for n1 :from start :to (1- end)
          :for n2 :to (1- end2)
          :do (setf (aref sequence n1)
                    (code-char (aref (buffer stream) n2))))
    end2))

(defmethod sys.gray:stream-file-position ((stream file-cache-stream) &optional (position-spec nil position-specp))
  (if position-specp
      (setf (buffer-offset stream) (case position-spec
                                     (:start 0)
                                     (:end (buffer-size stream))
                                     (t position-spec)))
      (t (buffer-offset stream))))

(defmethod sys.gray:stream-file-length ((stream file-cache-stream))
  (buffer-size stream))
