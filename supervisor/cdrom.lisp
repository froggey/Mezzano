;;;; A very basic driver for SCSI-attached CD-ROM drives.

;;; Due to deficiencies in the disk layer, this driver does not support
;;; changing disks.

(defpackage :mezzano.supervisor.cdrom
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sys.int :mezzano.internals))
  (:export #:cdrom-initialize-device))

(in-package :mezzano.supervisor.cdrom)

(defconstant +scsi-command-inquiry+ #x12)
(defconstant +scsi-command-read-capacity+ #x25)
(defconstant +scsi-command-read+ #x28)

(defstruct (cdrom-device
             (:area :wired))
  device
  cdb
  buffer
  command-fn)

(defun cdrom-initialize-device (device cdb-size command-fn)
  (sup:ensure (>= cdb-size 12))
  (let* ((cdb (sys.int::make-simple-vector cdb-size :wired))
         (buffer-frame (sup::allocate-physical-pages 1
                                                     :mandatory-p "CD-ROM result buffer"))
         (buffer (sup::convert-to-pmap-address (ash buffer-frame 12)))
         (cdrom (make-cdrom-device :device device
                                   :cdb cdb
                                   :buffer buffer
                                   :command-fn command-fn)))
    (sup:debug-print-line "Hello CD " cdrom)
    ;; Inquiry
    (setf (svref cdb 0) +scsi-command-inquiry+
          (svref cdb 1) 0
          (svref cdb 2) 0
          (svref cdb 3) 0
          (svref cdb 4) 128
          (svref cdb 5) 0)
    (sup:debug-print-line "Issue inquiry command...")
    (let ((result (funcall command-fn device cdb buffer 512)))
      (when (not result)
        (sup:debug-print-line "Inquiry command failed.")
        (return-from cdrom-initialize-device))
      (dotimes (i result)
        (sup:debug-print-line i " " (sys.int::memref-unsigned-byte-8 buffer i))))
    ;; FIXME: Assumes that the CD has a single data-track spanning the entire disc.
    ;; Read capacity.
    (sup:debug-print-line "Issue read capacity command...")
    (setf (svref cdb 0) +scsi-command-read-capacity+
          (svref cdb 1) 0
          (svref cdb 2) 0
          (svref cdb 3) 0
          (svref cdb 4) 0
          (svref cdb 5) 0
          (svref cdb 6) 0
          (svref cdb 7) 0
          (svref cdb 8) 0
          (svref cdb 9) 0)
    (let ((result (funcall command-fn device cdb buffer 8)))
      (when (not result)
        (sup:debug-print-line "Read capacity failed, no medium?")
        (return-from cdrom-initialize-device))
      (let ((max-lba (logior (ash (sys.int::memref-unsigned-byte-8 buffer 0) 24)
                             (ash (sys.int::memref-unsigned-byte-8 buffer 1) 16)
                             (ash (sys.int::memref-unsigned-byte-8 buffer 2) 8)
                             (sys.int::memref-unsigned-byte-8 buffer 3)))
            (block-size (logior (ash (sys.int::memref-unsigned-byte-8 buffer 4) 24)
                                (ash (sys.int::memref-unsigned-byte-8 buffer 5) 16)
                                (ash (sys.int::memref-unsigned-byte-8 buffer 6) 8)
                                (sys.int::memref-unsigned-byte-8 buffer 7))))
        (when (not (eql block-size 2048))
          (sup:debug-print-line "Device has unusual block size " block-size ", ignoring.")
          (return-from cdrom-initialize-device))
        (sup:debug-print-line " Max LBA: " max-lba " Block sise: " block-size)
        (sup:register-disk cdrom nil
                           max-lba block-size #xFFFF
                           'cdrom-read nil nil
                           nil)))))

(defun cdrom-read (cdrom lba count mem-addr)
  (let ((cdb (cdrom-device-cdb cdrom))
        (device (cdrom-device-device cdrom))
        (command-fn (cdrom-device-command-fn cdrom)))
    (setf (svref cdb 0) +scsi-command-read+
          (svref cdb 1) 0
          (svref cdb 2) (ldb (byte 8 24) lba)
          (svref cdb 3) (ldb (byte 8 16) lba)
          (svref cdb 4) (ldb (byte 8 8) lba)
          (svref cdb 5) (ldb (byte 8 0) lba)
          (svref cdb 6) 0
          (svref cdb 7) (ldb (byte 8 8) count)
          (svref cdb 8) (ldb (byte 8 0) count)
          (svref cdb 9) 0)
    (let ((result (funcall command-fn device cdb mem-addr (* count 2048))))
      (if result
          t
          (values nil :device-error)))))
