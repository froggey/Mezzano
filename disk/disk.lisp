;;;; Copyright (c) 2019 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.disk)

;;======================================================================
;;    Helper functions
;;======================================================================

(defun read-sector (disk lba n-sectors wired-buffer)
  (multiple-value-bind (success-p error-reason)
      (sup:disk-read disk lba n-sectors wired-buffer)
    (unless success-p
      (error "Disk read error: ~A" error-reason))))

(defun write-sector (disk lba n-sectors wired-buffer)
  (multiple-value-bind (success-p error-reason)
      (sup:disk-write disk lba n-sectors wired-buffer)
    (unless success-p
      (error "Disk write error: ~A" error-reason))))

(defun do-block (disk fn lba n-sectors buffer &optional (offset 0))
  (let* ((sector-size (sup:disk-sector-size disk))
         (sectors-per-4K (/ 4096 sector-size))
         (n-bytes (* sector-size n-sectors))
         (wired-buffer (make-array (min 4096 n-bytes)
                                   :element-type '(unsigned-byte 8)
                                   :area :wired)))
    (multiple-value-bind (full-transfers partial-transfer)
        (truncate n-sectors sectors-per-4K)
      (loop :for addr := lba :then (+ addr sectors-per-4k)
            :for base-offset := offset :then (+ base-offset 4096)
            :repeat full-transfers
            :do (funcall fn disk addr sectors-per-4K wired-buffer buffer base-offset)
            :finally
            (when (/= partial-transfer 0)
              (funcall fn disk addr partial-transfer wired-buffer buffer base-offset
                       (+ base-offset (* partial-transfer sector-size))))))))

;;======================================================================
;;    API
;;======================================================================

(defgeneric block-device-sector-size (disk))

(defgeneric block-device-n-sectors (disk))

(defgeneric block-device-read (device lba n-sectors buffer &key offset))

(defgeneric block-device-write (device lba n-sectors buffer &key offset))

(defgeneric block-device-flush (device))

(defgeneric block-device-read-sector (disk start-sector n-sectors))

(defgeneric block-device-write-sector (disk start-sector array n-sectors))

;;======================================================================
;;    Supervisor disk API implementation
;;======================================================================

(defmethod block-device-sector-size ((disk sup:disk))
  (sup:disk-sector-size disk))

(defmethod block-device-n-sectors ((disk sup:disk))
  (sup:disk-n-sectors disk))

(defmethod block-device-read ((disk sup:disk) lba n-sectors buffer &key (offset 0))
  (flet ((read-sector (disk addr n-sectors wired-buffer buffer base-offset &optional (end1 nil))
           (read-sector disk addr n-sectors wired-buffer)
           (replace buffer wired-buffer :start1 base-offset :end1 end1)))
    (do-block disk #'read-sector lba n-sectors buffer offset)))

(defmethod block-device-write ((disk sup:disk) lba n-sectors buffer &key (offset 0))
  (flet ((write-sector (disk addr n-sectors wired-buffer buffer base-offset &optional (end2 nil))
           (replace wired-buffer buffer :start2 base-offset :end2 end2)
           (write-sector disk addr n-sectors wired-buffer)))
    (do-block disk #'write-sector lba n-sectors buffer offset)))

(defmethod block-device-flush ((disk sup:disk))
  (sup:disk-flush disk))

(defmethod block-device-read-sector ((disk sup:disk) start-sector n-sectors)
  (let ((result (make-array (* (block-device-sector-size disk) n-sectors)
                            :element-type '(unsigned-byte 8))))
    (block-device-read disk start-sector n-sectors result)
    result))

(defmethod block-device-write-sector ((disk sup:disk) start-sector array n-sectors)
  (block-device-write disk start-sector n-sectors array))

;;======================================================================
;;    Cache API implementation
;;======================================================================

(defclass page-cache ()
  ((%partition :initarg :partition :reader partition)
   (%cache :initarg :cache :reader cache))
  (:default-initargs :cache (make-hash-table :weakness :value)))

(defmethod block-device-sector-size ((disk page-cache))
  (block-device-sector-size (partition disk)))

(defmethod block-device-n-sectors ((disk page-cache))
  (block-device-n-sectors (partition disk)))

(defmethod block-device-flush ((disk page-cache))
  (block-device-flush (partition disk)))

(defmethod block-device-read-sector ((disk page-cache) start-sector n-sectors)
  (or (gethash start-sector (cache disk))
      (setf (gethash start-sector (cache disk))
            (block-device-read-sector (partition disk) start-sector n-sectors))))

(defmethod block-device-write-sector ((disk page-cache) start-sector block n-sectors)
  (block-device-write-sector (partition disk) start-sector block n-sectors))
