;;;; Copyright (c) 2019 Bruno Cichon <ebrasca@librepanther.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.disk)

;;======================================================================
;; Helper functions
;;======================================================================

(defun read-sector (disk lba n-sectors wired-buffer buffer base-offset &optional (end1 nil))
  (multiple-value-bind (success-p error-reason)
      (sup:disk-read disk lba n-sectors wired-buffer)
    (if success-p
        (replace buffer wired-buffer :start1 base-offset :end1 end1)
        (error "Disk read error: ~A" error-reason))))

(defun write-sector (disk lba n-sectors wired-buffer buffer base-offset &optional (end2 nil))
  (replace wired-buffer buffer :start2 base-offset :end2 end2)
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
;; API
;;======================================================================

(defgeneric block-device-sector-size (disk))

(defgeneric block-device-n-sectors (disk))

(defgeneric block-device-read (device lba n-sectors buffer &key offset))

(defgeneric block-device-write (device lba n-sectors buffer &key offset))

(defgeneric block-device-flush (device))

;;======================================================================
;; Supervisor disk API implementation
;;======================================================================

(defmethod block-device-sector-size ((disk sup:disk))
  (sup:disk-sector-size disk))

(defmethod block-device-n-sectors ((disk sup:disk))
  (sup:disk-n-sectors disk))

(defmethod block-device-read ((disk sup:disk) lba n-sectors buffer &key (offset 0))
  (do-block disk #'read-sector lba n-sectors buffer offset))

(defmethod block-device-write ((disk sup:disk) lba n-sectors buffer &key (offset 0))
  (do-block disk #'write-sector lba n-sectors buffer offset))

(defmethod block-device-flush ((disk sup:disk))
  (multiple-value-bind (success-p error-reason) (sup:disk-flush disk)
    (or success-p (error "Disk flush error: ~A" error-reason))))

(defun block-device-read-sector (disk start-sector n-sectors)
  (let ((result (make-array (* (block-device-sector-size disk) n-sectors)
                            :element-type '(unsigned-byte 8))))
    (block-device-read disk start-sector n-sectors result)
    result))

(defun block-device-write-sector (disk start-sector array n-sectors)
  (block-device-write disk start-sector n-sectors array))

;;======================================================================
;; Code to maintain a list of devices that support the block device
;; interface. All of the disks and partition that use the supervisor
;; driver support the block device interface, because that interface
;; is defined above.
;;======================================================================

(defvar *block-devices-lock* (sup:make-mutex "Lock for *block-devices*"))
(defvar *block-devices* NIL)

(defun all-block-devices ()
  (sup:with-snapshot-inhibited ()
    (sup:with-mutex (*block-devices-lock*)
      (copy-list *block-devices*))))

(defun register-block-device (device)
  (sup:with-snapshot-inhibited ()
    (sup:with-mutex (*block-devices-lock*)
      (push device *block-devices*)))
  (mezzano.file-system:mount-block-device device))

(defun unregister-block-device (device)
  (sup:with-snapshot-inhibited ()
    (sup:with-mutex (*block-devices-lock*)
      (setf *block-devices* (delete device *block-devices*))))
  (mezzano.file-system:unmount-block-device device))

(defvar *previous-sup-disks* NIL)

(defun register-supervisor-disks ()
  ;; This function is also called from ipl.lisp for cold boot
  ;; un-register supervisor disks from last boot
  (loop
     for part in *previous-sup-disks* do
       (unregister-block-device part))
  ;; register supervisor disks from this boot
  (setf *previous-sup-disks* NIL)
  (loop
     for part in (sup:all-disks) do
       (push part *previous-sup-disks*)
       (register-block-device part)))

(mezzano.supervisor:add-boot-hook 'register-supervisor-disks :early)
