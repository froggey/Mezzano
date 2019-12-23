;;;; Copyright (c) 2019 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.file-system-cache
  (:use :cl)
  (:export #:file-cache-stream
           #:file-cache-character-stream
           #:fs-read-block
           #:fs-write-block
           #:direction
           #:buffer
           #:dirty-block
           #:dirty-block-n
           #:file-position*
           #:file-length*))

(in-package :mezzano.file-system-cache)

(defclass file-cache-stream (file-stream)
  ((%direction :initarg :direction :reader direction)
   (%buffer :initarg :buffer :accessor buffer)
   (%dirty-block :initarg :dirty-block :accessor dirty-block)
   (%dirty-block-n :initarg :dirty-block-n :accessor dirty-block-n)
   (%position :initarg :position :accessor file-position*)
   (%length :initarg :length :accessor file-length*)
   (%block-length :initarg :block-length :accessor block-length))
  (:default-initargs :buffer (make-hash-table :weakness :value)
                     :dirty-block nil
                     :dirty-block-n nil
                     :position 0
                     :length 0))

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
  (member (direction stream) '(:input :io)))

(defmethod output-stream-p ((stream file-cache-stream))
  (member (direction stream) '(:output :io)))

(defgeneric fs-read-block (stream block-n)
  (:documentation "Read block-n from file"))

(defgeneric fs-write-block (stream)
  (:documentation "Write block-n to file"))

;; TODO: Give error when out of space
(defun make-block (stream)
  (make-array (block-length stream)
              :element-type '(unsigned-byte 8)))

(defmethod mezzano.gray:stream-write-byte ((stream file-cache-stream) byte)
  (assert (member (direction stream) '(:output :io)))
  (when (>= (file-position* stream) (file-length* stream))
    (setf (file-length* stream) (1+ (file-position* stream))))
  (multiple-value-bind (buffer-n offset)
      (truncate (file-position* stream) (block-length stream))
    (let ((buffer (or (gethash buffer-n (buffer stream))
                      (setf (gethash buffer-n (buffer stream))
                            (or (fs-read-block stream buffer-n)
                                (make-block stream))))))
      (setf (aref buffer offset) byte)
      (unless (eql buffer-n (dirty-block-n stream))
        (fs-write-block stream)
        (setf (dirty-block stream) buffer
              (dirty-block-n stream) buffer-n))))
  (incf (file-position* stream)))

(defmethod mezzano.gray:stream-read-byte ((stream file-cache-stream))
  (assert (member (direction stream) '(:input :io)))
  (prog1 (if (>= (file-position* stream) (file-length* stream))
             :eof
             (multiple-value-bind (buffer-n offset)
                 (truncate (file-position* stream) (block-length stream))
               (aref (or (gethash buffer-n (buffer stream))
                         (setf (gethash buffer-n (buffer stream))
                               (fs-read-block stream buffer-n)))
                     offset)))
    (incf (file-position* stream))))

#+nil
(defmethod mezzano.gray:stream-write-sequence ((stream file-cache-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:output :io)))
  (let* ((end (or end (file-length* stream)))
         (end2 (min end (length sequence))))
    (when (> end2 (file-length* stream))
      (setf (buffer stream) (adjust-array (buffer stream) end2 :initial-element 0)))
    (replace (buffer stream) sequence
             :start1 (file-position* stream)
             :end1 (+ (file-position* stream) (- end start))
             :start2 start
             :end2 end2)
    (incf (file-position* stream) (- end2 start))))

#+nil
(defmethod mezzano.gray:stream-read-sequence ((stream file-cache-stream) sequence &optional (start 0) end)
  (assert (member (direction stream) '(:input :io)))
  (let* ((end (or end (length sequence)))
         (end2 (min end (file-length* stream))))
    (replace sequence (buffer stream) :start1 start :end1 end :start2 0 :end2 end2)
    end2))

(defmethod mezzano.gray:stream-file-position ((stream file-cache-stream) &optional (position-spec nil position-specp))
  (if position-specp
      (setf (file-position* stream) (case position-spec
                                      (:start 0)
                                      (:end (file-length* stream))
                                      (t position-spec)))
      (file-position* stream)))

(defmethod mezzano.gray:stream-file-length ((stream file-cache-stream))
  (file-length* stream))
