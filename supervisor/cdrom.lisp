;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defstruct (cdrom-device
             (:area :wired))
  device
  cdb
  buffer)

(defun cdrom-initialize-device (device cdb-size command-fn)
  (ensure (>= cdb-size 12))
  (let* ((cdb (sys.int::make-simple-vector cdb-size :wired))
         (buffer-frame (allocate-physical-pages 1
                                                :mandatory-p "CD-ROM result buffer"))
         (buffer (convert-to-pmap-address (ash buffer-frame 12)))
         (cdrom (make-cdrom-device :device device
                                   :cdb cdb
                                   :buffer buffer)))
    (debug-print-line "Hello CD " cdrom)
    ;; Inquiry
    (setf (svref cdb 0) #x12
          (svref cdb 1) 0
          (svref cdb 2) 0
          (svref cdb 3) 0
          (svref cdb 4) 128
          (svref cdb 5) 0)
    (debug-print-line "Issue inquiry command...")
    (let ((result (funcall command-fn device cdb buffer 512)))
      (debug-print-line "PACKET result: " result)
      (dotimes (i result)
        (debug-print-line i " " (sys.int::memref-unsigned-byte-8 buffer i)))
    )))
