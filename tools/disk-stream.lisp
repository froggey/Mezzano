;;; A stream class that wraps a disk.
;;; Quite dangerous, especially when used on the paging disk!
;;;
;;; To use:
;;; 1) Find a disk object, try #'MEZZANO-SUPERVISOR:ALL-DISKS.
;;; 2) (make-instance 'disk-stream :disk THE-DISK-OBJECT)
;;; 3) Read or write using READ-/WRITE-SEQUENCE, and seek with FILE-POSITION
;;; Accesses must be aligned to the disk sector size.
;;; Accesses are unbuffered and will occur immediately, no flushing currently required.

(in-package :mezzano.internals)

(defclass disk-stream (mezzano.gray:fundamental-binary-input-stream
                       mezzano.gray:fundamental-binary-output-stream
                       file-stream)
  ((%disk :initarg :disk :reader disk-stream-disk)
   (%fpos :initarg :position))
  (:default-initargs :position 0))

(defmethod mezzano.gray:stream-element-type ((stream disk-stream))
  '(unsigned-byte 8))

(defmethod mezzano.gray:stream-file-position ((stream disk-stream) &optional (position-spec nil position-specp))
  (with-slots (%fpos) stream
    (cond (position-specp
           (setf %fpos (case position-spec
                         (:start 0)
                         (:end (stream-file-length stream))
                         (t position-spec))))
          (t %fpos))))

(defmethod mezzano.gray:stream-file-length ((stream disk-stream))
  (let ((disk (disk-stream-disk stream)))
    (* (mezzano.supervisor:disk-n-sectors disk)
       (mezzano.supervisor:disk-sector-size disk))))

;;; FIXME: Should limit these to the size of the disk.
(defmethod mezzano.gray:stream-read-sequence ((stream disk-stream) seq &optional start end)
  (let* ((disk (disk-stream-disk stream))
         (sector-size (mezzano.supervisor:disk-sector-size disk))
         (n-bytes (- (or end (length seq)) start))
         (buffer (make-array sector-size :element-type '(unsigned-byte 8) :area :wired))
         (fpos (file-position stream)))
    (assert (zerop (rem fpos sector-size)))
    (assert (zerop (rem n-bytes sector-size)))
    (dotimes (i (truncate n-bytes sector-size))
      (multiple-value-bind (successp error)
          (mezzano.supervisor:disk-read disk
                                        (truncate fpos sector-size)
                                        1
                                        buffer)
        (when (not successp)
          (error "Disk read error: ~S" error))
        (setf (subseq seq (+ start (* i sector-size))) buffer)
        (incf fpos sector-size)
        (file-position stream fpos)))))

(defmethod mezzano.gray:stream-write-sequence ((stream disk-stream) seq &optional start end)
  (let* ((disk (disk-stream-disk stream))
         (sector-size (mezzano.supervisor:disk-sector-size disk))
         (n-bytes (- (or end (length seq)) start))
         (buffer (make-array sector-size :element-type '(unsigned-byte 8) :area :wired))
         (fpos (file-position stream)))
    (assert (zerop (rem fpos sector-size)))
    (assert (zerop (rem (length buffer) sector-size)))
    (dotimes (i (truncate n-bytes sector-size))
      (replace buffer seq :start2 (+ start (* i sector-size)))
      (multiple-value-bind (successp error)
          (mezzano.supervisor:disk-write disk
                                         (truncate fpos sector-size)
                                         1
                                         buffer)
        (when (not successp)
          (error "Disk read error: ~S" error))
        (incf fpos sector-size)
        (file-position stream fpos)))))
