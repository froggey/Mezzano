;;;; Copyright (c) 2019 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.disk)

(defgeneric block-device-sector-size (disk))

(defmethod block-device-sector-size ((disk sup:disk))
  (sup:disk-sector-size disk))

(defgeneric block-device-n-sectors (disk))

(defmethod block-device-n-sectors ((disk sup:disk))
  (sup:disk-n-sectors disk))

(defgeneric block-device-read (device lba n-sectors buffer &key offset))

(defun read-sector (disk addr n-sectors wired-buffer buffer base-offset &optional (end1 nil))
  (multiple-value-bind (success-p error-reason)
      (sup:disk-read disk addr n-sectors wired-buffer)
    (unless success-p
      (error "Disk read error: ~A" error-reason)))
  (replace buffer wired-buffer :start1 base-offset :end1 end1))

(defmethod block-device-read ((disk sup:disk) lba n-sectors buffer &key (offset 0))
  (let* ((sector-size (sup:disk-sector-size disk))
         (sectors-per-4K (/ 4096 sector-size))
         (n-bytes (* sector-size n-sectors))
         (wired-buffer (make-array (min 4096 n-bytes)
                                   :element-type '(unsigned-byte 8)
                                   :area :wired)))
    (multiple-value-bind (full-transfers partial-transfer)
        (truncate n-sectors sectors-per-4K)
      (loop
         for addr = lba then (+ addr sectors-per-4k)
         for base-offset = offset then (+ base-offset 4096)
         repeat full-transfers
         do (read-sector disk addr sectors-per-4K wired-buffer buffer base-offset)
         finally
         (when (/= partial-transfer 0)
           (read-sector disk addr partial-transfer wired-buffer buffer base-offset
                        (+ base-offset (* partial-transfer sector-size))))))))

(defgeneric block-device-write (device lba n-sectors buffer &key offset))

(defun write-sector (disk addr n-sectors wired-buffer buffer base-offset &optional (end2 nil))
  (replace wired-buffer buffer :start2 base-offset :end2 end2)
  (multiple-value-bind (success-p error-reason)
      (sup:disk-write disk addr n-sectors wired-buffer)
    (unless success-p
      (error "Disk write error: ~A" error-reason))))

(defmethod block-device-write ((disk sup:disk) lba n-sectors buffer &key (offset 0))
  (let* ((sector-size (sup:disk-sector-size disk))
         (sectors-per-4K (/ 4096 sector-size))
         (n-bytes (* sector-size n-sectors))
         (wired-buffer (make-array (min 4096 n-bytes)
                                   :element-type '(unsigned-byte 8)
                                   :area :wired)))
    (multiple-value-bind (full-transfers partial-transfer)
        (truncate n-sectors sectors-per-4K)
      (loop
         for addr = lba then (+ addr sectors-per-4k)
         for base-offset = offset then (+ base-offset 4096)
         repeat full-transfers
         do (write-sector disk addr sectors-per-4K wired-buffer buffer base-offset)
         finally
         (when (/= partial-transfer 0)
           (write-sector disk addr partial-transfer wired-buffer buffer base-offset
                         (+ base-offset (* partial-transfer sector-size))))))))

(defgeneric block-device-flush (device))

(defmethod block-device-flush ((disk sup:disk))
  (sup:disk-flush disk))

(defun block-device-read-sector (disk start-sector n-sectors)
  "Read n sectors from disk"
  (let ((result (make-array (* (block-device-sector-size disk) n-sectors)
                            :element-type '(unsigned-byte 8))))
    (block-device-read disk start-sector n-sectors result)
    result))

(defun block-device-write-sector (disk start-sector array n-sectors)
  "Write n sectors to disk"
  (block-device-write disk start-sector n-sectors array))
