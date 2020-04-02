;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

;;======================================================================
;; Mass Storage Class Driver
;;======================================================================

(defpackage :mezzano.driver.usb.mass
  (:use :cl :mezzano.driver.usb :mezzano.disk)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sync :mezzano.sync)
                    (:sys.int :mezzano.internals)))

(in-package :mezzano.driver.usb.mass)

(defvar *mass-storage* nil)    ;; for debug

(defvar *trace-stream* sys.int::*cold-stream*)
(defvar *trace* 0)

(defmacro with-trace-level ((trace-level) &body body)
  `(when (>= *trace* ,trace-level)
     ,@body))

(declaim (inline enter-function))

(defun enter-function (name)
  (with-trace-level (1)
    (sup:debug-print-line name)))

;;======================================================================
;; From USB Mass Storage Class Specification Overview Rev 1.4 Feb. 19, 2020
;;======================================================================

(defconstant +mass-subclass-de-faco-scsi    0)
(defconstant +mass-subclass-rbc+            1)
(defconstant +mass-subclass-atapi+          2)
(defconstant +mass-subclass-obsolete-3+     3)
(defconstant +mass-subclass-ufi+            4)
(defconstant +mass-subclass-obsolete-5+     5)
(defconstant +mass-subclass-scsi+           6)
(defconstant +mass-subclass-lsd-fs+         7)
(defconstant +mass-subclass-ieee-1667+      8)

;;======================================================================
;; Command Block Wrapper Definitions
;; from USB Mass Storage Class Bulk-Only Transport Rev 1.0 Sept. 31 1999
;;======================================================================

(defconstant +cbw-signature+         0)
(defconstant +cbw-tag+               4)
(defconstant +cbw-transfer-length+   8)
(defconstant +cbw-flags+            12)
(defconstant +cbw-lun+              13)
(defconstant +cbw-cb-length+        14)
(defconstant +cbw-control-block+    15)

;; CBWs are always 31 bytes, but only bytes 0 - (14 + +cbw-cb-length+) are valid
(defconstant +cbw-buffer-size+      31)

(defconstant +cbw-signature-value+  #x43425355)

;; Only the MSB (bit 7) of the flags is used, so just define the two values
(defconstant +cbw-flags-data-out+ #x00)
(defconstant +cbw-flags-data-in+  #x80)

;;======================================================================
;; Command Status Wrapper Definitions
;; from USB Mass Storage Class Bulk-Only Transport Rev 1.0 Sept. 31 1999
;;======================================================================

(defconstant +csw-signature+         0)
(defconstant +csw-tag+               4)
(defconstant +csw-data-residue+      8)
(defconstant +cs2-status+           12)

;; CSWs are always 13 bytes
(defconstant +csw-buffer-size+      13)

(defconstant +csw-signature-value+  #x53425355)

(defconstant +csw-status-success+     0)
(defconstant +csw-status-cmd-failed+  1)
(defconstant +csw-phase-error+        2)
;; 3 - 4 Reserved - obsolete
;; 5 - #xFF Reserved

#+nil
(define-constant +scsi-operation-code+  0)
#+nil
(define-constant +scsi-flags+           1)

(defconstant +scsi-code-inquiry+    #x12)
(defconstant +scsi-code-read/10+    #x28)
(defconstant +scsi-code-write/10+   #x2A)
(defconstant +scsi-code-read/16+    #x88)
(defconstant +scsi-code-write/16+   #x8A)
(defconstant +scsi-service-action+  #x9E)

(defconstant +scsi-code-read-capacity+ #x10)

;;======================================================================
;; Notes


;; Reset Recovery section 5.3.4
;; 1. A bulk-only mass storage reset
;; 2. A Clear Feature HALT to bulk-in endpoint
;; 3. A Clear Feature HALT to bulk-out endpont
;; I think these are all control endpoint messages first one is a class message?

;;======================================================================
;;======================================================================

(defstruct mass-storage
  usbd
  device
  interface
  event
  bulk-in-endpt-num
  bulk-out-endpt-num
  lock
  cbw-tag
  status
  ;; info from the inquiry command
  vendor
  product
  revision
  ;; info from read capacity command
  num-blocks
  block-size
  ;;
  disk
  partitions
  )

(defun next-cbw-tag (driver)
  (sup:with-mutex ((mass-storage-lock driver))
    (incf (mass-storage-cbw-tag driver))))

(defun timed-wait (event timeout)
  (sup:with-timer (timer :relative timeout :name "Mass Storage timed wait")
    (sync:wait-for-objects timer event)
    (if (and (not (sup:event-state event)) (sup:timer-expired-p timer))
        :timeout
        :complete)))

;;======================================================================
;;======================================================================
(define-condition timeout-retry ()
  ((%endpoint        :initarg :endpoint        :reader timeout-endpoint)
   (%enqueued-buf    :initarg :enqueued-buf    :reader timeout-enqueued-buf)))

(defun retry-operation (c)
  (invoke-restart 'retry-operation
                  (timeout-endpoint c)
                  (timeout-enqueued-buf c)))

;;======================================================================
;;======================================================================
(defclass usb-ms-partition (disk-partition-mixin)
  ((%mass-storage    :initarg :mass-storage    :accessor dp-mass-storage)))

(defmethod block-device-sector-size ((partition usb-ms-partition))
  (mass-storage-block-size (dp-mass-storage partition)))

(defmethod block-device-n-sectors ((partition usb-ms-partition))
  (mass-storage-num-blocks (dp-mass-storage partition)))

(defclass usb-ms-disk (disk-mixin disk-pt-mixin)
  ((%mass-storage    :initarg :mass-storage    :accessor disk-mass-storage)))

(defmethod block-device-sector-size ((disk usb-ms-disk))
  (mass-storage-block-size (disk-mass-storage disk)))

(defmethod block-device-n-sectors ((disk usb-ms-disk))
  (mass-storage-num-blocks (disk-mass-storage disk)))

;;======================================================================
;;======================================================================

(defun encode-cbw (driver buf offset length in-p logical-unit)
  (setf (get-unsigned-word/32 buf (+ offset +cbw-signature+))
        +cbw-signature-value+
        (get-unsigned-word/32 buf (+ offset +cbw-tag+))
        (next-cbw-tag driver)
        (get-unsigned-word/32 buf (+ offset +cbw-transfer-length+))
        length
        (aref buf (+ offset +cbw-flags+))
        (if in-p +cbw-flags-data-in+ +cbw-flags-data-out+)
        (aref buf (+ offset +cbw-lun+))
        logical-unit))

(defun encode-scsi-inquiry (buf offset vital-p page length)
  (let ((cbw-offset (+ offset +cbw-control-block+)))
    (setf (aref buf (+ offset +cbw-cb-length+)) 6
          (aref buf cbw-offset) +scsi-code-inquiry+
          (aref buf (+ cbw-offset 1)) (if vital-p 1 0)
          (aref buf (+ cbw-offset 2)) page
          (get-unsigned-word/16 buf (+ cbw-offset 3)) length
          (aref buf (+ cbw-offset 5)) 0))) ;; TODO what about the NACA bit

(defun encode-scsi-read-capacity/10 (buf offset)
  (let ((cbw-offset (+ offset +cbw-control-block+)))
    (setf (aref buf (+ offset +cbw-cb-length+)) 10
          (aref buf cbw-offset) #x25)
    (dotimes (i 9)
      (setf (aref buf (+ cbw-offset i 1)) 0))))

(defun encode-scsi-read-capacity/16 (buf offset length)
  (let ((cbw-offset (+ offset +cbw-control-block+)))
    (setf (aref buf (+ offset +cbw-cb-length+)) 16
          (aref buf cbw-offset) +scsi-service-action+
          (aref buf (+ cbw-offset 1)) +scsi-code-read-capacity+
          (get-unsigned-word/32 buf (+ cbw-offset 2)) 0
          (get-unsigned-word/32 buf (+ cbw-offset 6)) 0
          (get-be-unsigned-word/32 buf (+ cbw-offset 10)) length
          (aref buf (+ cbw-offset 14)) 0
          (aref buf (+ cbw-offset 15)) 0)))

(defun encode-scsi-read/10 (buf offset lba n-blocks)
  (let ((cbw-offset (+ offset +cbw-control-block+)))
    (setf (aref buf (+ offset +cbw-cb-length+)) 10
          (aref buf cbw-offset) +scsi-code-read/10+
          (aref buf (+ cbw-offset 1)) 0
          (get-be-unsigned-word/32 buf (+ cbw-offset 2)) lba
          (aref buf (+ cbw-offset 6)) 0
          (get-be-unsigned-word/16 buf (+ cbw-offset 7)) n-blocks
          (aref buf (+ cbw-offset 9)) 0)))

(defun encode-scsi-write/10 (buf offset lba n-blocks)
  (let ((cbw-offset (+ offset +cbw-control-block+)))
    (setf (aref buf (+ offset +cbw-cb-length+)) 10
          (aref buf cbw-offset) +scsi-code-write/10+
          (aref buf (+ cbw-offset 1)) 0
          (get-be-unsigned-word/32 buf (+ cbw-offset 2)) lba
          (aref buf (+ cbw-offset 6)) 0
          (get-be-unsigned-word/16 buf (+ cbw-offset 7)) n-blocks
          (aref buf (+ cbw-offset 9)) 0)))

;;======================================================================
;;======================================================================

(defun send-buf (usbd device mass-storage endpoint buf length)
  (let((event (mass-storage-event mass-storage)))
    (setf (sup:event-state event) nil)
    (bulk-enqueue-buf usbd device endpoint buf length)
    (when (eq (timed-wait event 0.500) :timeout)
      (sup:debug-print-line "send-buf timeout")
      (signal 'timeout-retry :endpoint endpoint :enqueued-buf buf))))

(defun %read-sector (usbd device mass-storage lba buf offset)
  (enter-function "%read-sector")
  (let ((data-length (mass-storage-block-size mass-storage))
        (out-endpoint (mass-storage-bulk-out-endpt-num mass-storage))
        (in-endpoint (mass-storage-bulk-in-endpt-num mass-storage)))
    (with-buffers ((buf-pool usbd) ((cmd-buf /8 31)
                                    (data-buf /8 data-length)
                                    (status-buf /8 13)))
      (encode-cbw mass-storage cmd-buf 0 data-length T 0)
      (encode-scsi-read/10 cmd-buf 0 lba 1)
      (send-buf usbd device mass-storage out-endpoint cmd-buf 31)

      (with-trace-level (4)
        (sup:debug-print-line "%read-sector command status "
                              (mass-storage-status mass-storage)))

      (send-buf usbd device mass-storage in-endpoint data-buf data-length)

      (with-trace-level (3)
        (sup:debug-print-line "read sector data buffer:")
        (print-buffer sys.int::*cold-stream* data-buf :indent "  ")
        (sup:debug-print-line "read sector data status "
                              (mass-storage-status mass-storage)))

      (send-buf usbd device mass-storage in-endpoint status-buf 13)

      (with-trace-level (4)
        (sup:debug-print-line "%read-sector status buffer:")
        (print-buffer sys.int::*cold-stream* status-buf :indent "  ")
        (sup:debug-print-line "%read-sector status status "
                              (mass-storage-status mass-storage)))

      (cond ((= (aref status-buf 12) 0)
             ;; Copy buffer
             (dotimes (i data-length)
               (setf (aref buf (+ offset i)) (aref data-buf i))))
            ((= (aref status-buf 12) 1)
             ;; Command failed
             (error "%read-sector failed"))
            ((= (aref status-buf 12) 2)
             (error "%read-sector failed with phase error"))
            (T
             (error "%read-sector failed with unknown error ~D"
                    (aref status-buf 12)))))))

(defun %block-device-read (mass-storage lba n-sectors buf offset)
  (enter-function "%block-device-read")
  (handler-bind
      ((timeout-retry #'retry-operation))
    (loop
       with usbd = (mass-storage-usbd mass-storage)
       with device = (mass-storage-device mass-storage)
       with sector-size = (mass-storage-block-size mass-storage)
       with retry-count = 0
       do
         (restart-case
             (progn
               (when (<= n-sectors 0)
                 (return))
               (%read-sector usbd device mass-storage lba buf offset)
               (incf lba)
               (incf offset sector-size)
               (decf n-sectors))
           (retry-operation (endpoint enqueued-buf)
             (sup:debug-print-line "timeout on read lba: " lba)
             ;; clean up
             (bulk-dequeue-buf usbd device endpoint enqueued-buf)
             (reset-recovery usbd device mass-storage)
             ;; if too many retries, give up
             (incf retry-count)
             (when (= retry-count 5)
               (error "Mass storage timeout on read")))))))

(defmethod block-device-read
    ((disk usb-ms-disk) lba n-sectors buf &key (offset 0))
  (enter-function "block-device-read (mass-storage)")
  (%block-device-read (disk-mass-storage disk) lba n-sectors buf offset))

(defmethod block-device-read
    ((partition usb-ms-partition) lba n-sectors buf &key (offset 0))
  (enter-function "block-device-read (partition)")

  (when (> (+ lba n-sectors) (dp-size partition))
    (error "Attempt to read past end of partition, ~
            LBA: ~D, read size ~D, partition size ~D"
           lba n-sectors (dp-size partition)))

  (%block-device-read (dp-mass-storage partition)
                      (+ lba (dp-start-lba partition))
                      n-sectors
                      buf
                      offset))

;;======================================================================
;;======================================================================

(defun %write-sector (usbd device mass-storage lba buf offset)
  (enter-function "%write-sector")
  (let ((data-length (mass-storage-block-size mass-storage))
        (out-endpoint (mass-storage-bulk-out-endpt-num mass-storage))
        (in-endpoint (mass-storage-bulk-in-endpt-num mass-storage)))
    (with-buffers ((buf-pool usbd) ((cmd-buf /8 31)
                                    (data-buf /8 data-length)
                                    (status-buf /8 13)))
      (encode-cbw mass-storage cmd-buf 0 data-length NIL 0)
      (encode-scsi-write/10 cmd-buf 0 lba 1)
      (send-buf usbd device mass-storage out-endpoint cmd-buf 31)

      ;; Copy buffer
      (dotimes (i data-length)
        (setf (aref data-buf i) (aref buf (+ offset i))))

      (with-trace-level (4)
        (sup:debug-print-line "%write-sector command status "
                              (mass-storage-status mass-storage)))

      (send-buf usbd device mass-storage out-endpoint data-buf data-length)

      (with-trace-level (3)
        (sup:debug-print-line "write sector data buffer:")
        (print-buffer sys.int::*cold-stream* data-buf :indent "  ")
        (sup:debug-print-line "write sector data status "
                              (mass-storage-status mass-storage)))

      (send-buf usbd device mass-storage in-endpoint status-buf 13)

      (with-trace-level (4)
        (sup:debug-print-line "%write-sector status buffer:")
        (print-buffer sys.int::*cold-stream* status-buf :indent "  ")
        (sup:debug-print-line "%write-sector status status "
                              (mass-storage-status mass-storage)))

      (cond ((= (aref status-buf 12) 0)
             ;; Success - nothing else to do
             )
            ((= (aref status-buf 12) 1)
             ;; Command failed
             (error "%read-sector failed"))
            ((= (aref status-buf 12) 2)
             (error "%write-sector failed with phase error"))
            (T
             (error "%write-sector failed with unknown error ~D"
                    (aref status-buf 12)))))))

(defun %block-device-write (mass-storage lba n-sectors buf offset)
  (enter-function "block-device-write")
  (handler-bind
      ((timeout-retry #'retry-operation))
    (loop
       with usbd = (mass-storage-usbd mass-storage)
       with device = (mass-storage-device mass-storage)
       with sector-size = (mass-storage-block-size mass-storage)
       with retry-count = 0
       do
         (restart-case
             (progn
               (when (<= n-sectors 0)
                 (return))
               (%write-sector usbd device mass-storage lba buf offset)
               (incf lba)
               (incf offset sector-size)
               (decf n-sectors))
           (retry-operation (endpoint enqueued-buf)
             (sup:debug-print-line "timeout on write lba: " lba)
             ;; clean up
             (bulk-dequeue-buf usbd device endpoint enqueued-buf)
             (reset-recovery usbd device mass-storage)
             ;; if too many retries, give up
             (incf retry-count)
             (when (= retry-count 5)
               (error "Mass storage timeout on write")))))))

(defmethod block-device-write
    ((disk usb-ms-disk) lba n-sectors buf &key (offset 0))
  (enter-function "block-device-write (disk)")
  (%block-device-write (disk-mass-storage disk) lba n-sectors buf offset))

(defmethod block-device-write
    ((partition usb-ms-partition) lba n-sectors buf &key (offset 0))
  (enter-function "block-device-write (partition)")

  (when (> (+ lba n-sectors) (dp-size partition))
    (error "Attempt to write past end of partition, ~
            LBA: ~D, write size ~D, partition size ~D"
           lba n-sectors (dp-size partition)))

  (%block-device-write (dp-mass-storage partition)
                       (+ lba (dp-start-lba partition))
                       n-sectors
                       buf
                       offset))

;;======================================================================
;;======================================================================

(defmethod block-device-flush  ((mass-storage mass-storage))
  (enter-function "block-device-flush")
  )

;;======================================================================
;;======================================================================

(defun parse-endpt-descriptor (usbd device mass-storage endpt-desc)
  (let* ((address (aref endpt-desc +ed-address+))
         (endpt-in (ldb-test +ed-direction-field+ address))
         (endpt-num (ldb +ed-endpt-num-field+ address)))
    (when (/= (aref endpt-desc +ed-attributes+) +ed-attr-bulk+)
      (sup:debug-print-line "Mass Storage Probe failed because "
                            "endpoint type is "
                            (aref endpt-desc +ed-attributes+)
                            " instead of a bulk endpoint")
      (throw :probe-failed nil))

    (create-bulk-endpt usbd
                       device
                       mass-storage
                       endpt-num
                       endpt-in
                       'mass-storage-int-callback)
    (if endpt-in
        (setf (mass-storage-bulk-in-endpt-num mass-storage) endpt-num)
        (setf (mass-storage-bulk-out-endpt-num mass-storage) endpt-num))))


(defmethod delete-device ((mass-storage mass-storage) device)
  ;; Device has disconnected - clean up
  ;; terminate and transfers in progress
  ;; unmount device - deregister-disk? It's already gone so no operations allowed
  (unregister-block-device (mass-storage-disk mass-storage))
  (dolist (part (mass-storage-partitions mass-storage))
    (unregister-block-device part)))

(defun parse-inquiry (usbd device mass-storage)
  (enter-function "parse-inquiry")

  (let ((data-length 36)
        (event (mass-storage-event mass-storage)))
    (with-buffers ((buf-pool usbd) ((cmd-buf /8 31)
                                    (data-buf /8 data-length)
                                    (status-buf /8 13)))
      ;; send inquiry command
      (encode-cbw mass-storage cmd-buf 0 data-length T 0)
      (encode-scsi-inquiry cmd-buf 0 NIL 0 data-length)
      (setf (sup:event-state event) nil)
      (bulk-enqueue-buf usbd
                        device
                        (mass-storage-bulk-out-endpt-num mass-storage)
                        cmd-buf
                        31)

      (with-trace-level (4)
        (sup:debug-print-line "inquiry command buffer:")
        (print-buffer sys.int::*cold-stream* cmd-buf :indent "  "))

      (when (eq (timed-wait event 1.0) :timeout)
        ;; TODO need better error message here
        (error "Mass storage inquiry command timeout"))

      (with-trace-level (4)
        (sup:debug-print-line "inquiry command status"
                              (mass-storage-status mass-storage)))

      (setf (sup:event-state event) nil)
      (bulk-enqueue-buf usbd
                        device
                        (mass-storage-bulk-in-endpt-num mass-storage)
                        data-buf
                        data-length)

      (when (eq (timed-wait event 1.0) :timeout)
        ;; TODO need better error message here
        (error "Mass storage inquiry data timeout"))

      (with-trace-level (3)
        (sup:debug-print-line "inquiry data buffer:")
        (print-buffer sys.int::*cold-stream* data-buf :indent "  ")
        (sup:debug-print-line "inquiry data status "
                              (mass-storage-status mass-storage)))

      (setf (sup:event-state event) nil)
      (bulk-enqueue-buf usbd
                        device
                        (mass-storage-bulk-in-endpt-num mass-storage)
                        status-buf
                        13)

      (when (eq (timed-wait event 1.0) :timeout)
        ;; TODO need better error message here
        (error "Mass storage inquiry status timeout"))

      (with-trace-level (4)
        (sup:debug-print-line "inquiry status buffer:")
        (print-buffer sys.int::*cold-stream* status-buf :indent "  ")
        (sup:debug-print-line "inquiry status status "
                              (mass-storage-status mass-storage)))

      (cond ((= (aref status-buf 12) 0)
             ;; success - process data-buffer
             (setf (mass-storage-vendor mass-storage)
                   (string-right-trim " " (get-ascii-string data-buf 8 8))
                   (mass-storage-product mass-storage)
                   (string-right-trim " " (get-ascii-string data-buf 16 16))
                   (mass-storage-revision mass-storage)
                   (string-right-trim " " (get-ascii-string data-buf 32 4))))
            ((= (aref status-buf 12) 1)
             ;;command failed
             (sup:debug-print-line "Mass Storage Probe failed because "
                                   "Inquiry command failed")
             (throw :probe-failed nil))
            ((= (aref status-buf 12) 2)
             (sup:debug-print-line "Mass Storage Probe failed because "
                                   "Inquiry command got phase error")
             (throw :probe-failed nil))
            (T
             (sup:debug-print-line "Mass Storage Probe failed because "
                                   "Inquiry failed with unkown error "
                                   (aref status-buf 12))
             (throw :probe-failed nil))))))

(defun parse-read-capacity (usbd device mass-storage cap/10-p)
  (enter-function "parse-read-capacity")

  (let ((data-length (if cap/10-p 8 32))
        (event (mass-storage-event mass-storage)))
    (with-buffers ((buf-pool usbd) ((cmd-buf /8 31)
                                    (data-buf /8 data-length)
                                    (status-buf /8 13)))
      ;; send read capacity command
      (encode-cbw mass-storage cmd-buf 0 data-length T 0)
      (if cap/10-p
          (encode-scsi-read-capacity/10 cmd-buf 0)
          (encode-scsi-read-capacity/16 cmd-buf 0 data-length))
      (setf (sup:event-state event) nil)
      (bulk-enqueue-buf usbd
                        device
                        (mass-storage-bulk-out-endpt-num mass-storage)
                        cmd-buf
                        31)

      (with-trace-level (4)
        (sup:debug-print-line "read capacity command buffer:")
        (print-buffer sys.int::*cold-stream* cmd-buf :indent "  "))

      (when (eq (timed-wait event 1.0) :timeout)
        ;; TODO need better error message here
        (error "Mass storage read capacity command timeout"))

      (with-trace-level (4)
        (sup:debug-print-line "read capacity command status "
                              (mass-storage-status mass-storage)))

      (setf (sup:event-state event) nil)
      (bulk-enqueue-buf usbd
                        device
                        (mass-storage-bulk-in-endpt-num mass-storage)
                        data-buf
                        data-length)

      (when (eq (timed-wait event 1.0) :timeout)
        ;; TODO need better error message here
        (error "Mass storage read capacity data timeout"))

      (with-trace-level (3)
        (sup:debug-print-line "read capacity data buffer:")
        (print-buffer sys.int::*cold-stream* data-buf :indent "  ")
        (sup:debug-print-line "read capacity data status "
                              (mass-storage-status mass-storage)))

      (setf (sup:event-state event) nil)
      (bulk-enqueue-buf usbd
                        device
                        (mass-storage-bulk-in-endpt-num mass-storage)
                        status-buf
                        13)

      (when (eq (timed-wait event 1.0) :timeout)
        ;; TODO need better error message here
        (error "Mass storage read capacity status timeout"))

      (with-trace-level (4)
        (sup:debug-print-line "read capacity status buffer:")
        (print-buffer sys.int::*cold-stream* status-buf :indent "  ")
        (sup:debug-print-line "read capacity status status "
                              (mass-storage-status mass-storage)))

      (cond ((= (aref status-buf 12) 0)
             ;; success - process data-buffer
             (if cap/10-p
                 (let ((block-addr (get-be-unsigned-word/32 data-buf 0)))
                   (cond ((= block-addr #xFFFFFFFF)
                          (parse-read-capacity usbd device mass-storage NIL))
                         (T
                          (setf (mass-storage-num-blocks mass-storage)
                                block-addr
                                (mass-storage-block-size mass-storage)
                                (get-be-unsigned-word/32 data-buf 4)))))

                 (setf (mass-storage-num-blocks mass-storage)
                       (get-be-unsigned-word/64 data-buf 0)
                       (mass-storage-block-size mass-storage)
                       (get-be-unsigned-word/32 data-buf 8))))
            ((= (aref status-buf 12) 1)
             ;;command failed
             (sup:debug-print-line "Mass Storage Probe failed because "
                                   "read capacity command failed")
             (throw :probe-failed nil))
            ((= (aref status-buf 12) 2)
             (sup:debug-print-line "Mass Storage Probe failed because "
                                   "read capacity command got phase error")
             (throw :probe-failed nil))
            (T
             (sup:debug-print-line "Mass Storage Probe failed because "
                                   "read capacity failed with unkown error "
                                   (aref status-buf 12))
             (throw :probe-failed nil))))))

(defun probe-mass-storage-scsi (usbd device iface-desc configs)
  (when (/= (aref iface-desc +id-num-endpoints+) 2)
    (sup:debug-print-line "Mass Storage Probe failed because "
                          "interface descriptor has "
                          (aref iface-desc +id-num-endpoints+)
                          " endpoints, Only exactly 2 supported.")
    (throw :probe-failed nil))

  (let ((mass-storage (make-mass-storage
                       :usbd usbd
                       :device device
                       :interface (aref iface-desc +id-number+)
                       :event  (sup:make-event
                                :name "Mass Storage Event")
                       :bulk-in-endpt-num nil
                       :bulk-out-endpt-num nil
                       :lock (sup:make-mutex "USB Mass Storage Lock")
                       :cbw-tag #x100)))
    (dotimes (i 2)
      (let ((endpt-desc (pop configs)))
        (when (or (null endpt-desc)
                  (/= (aref endpt-desc +ed-type+) +desc-type-endpoint+))
          (sup:debug-print-line "Mass Storage probe failed because "
                                "found descriptor type "
                                (aref endpt-desc +ed-type+)
                                " instead of endpoint descriptor.")
          (throw :probe-failed nil))
        (parse-endpt-descriptor usbd device mass-storage endpt-desc)))

    (when (or (null (mass-storage-bulk-in-endpt-num mass-storage))
              (null (mass-storage-bulk-out-endpt-num mass-storage)))
      (sup:debug-print-line "Mass Storage probe failed because "
                            "did not have both in and out bulk endpoints")
      (throw :probe-failed nil))

    (parse-inquiry usbd device mass-storage)
    (parse-read-capacity usbd device mass-storage T)

    (with-trace-level (2)
      (sup:debug-print-line "vendor: \""
                            (mass-storage-vendor mass-storage)
                            "\", product: \""
                            (mass-storage-product mass-storage)
                            "\", revision: \""
                            (mass-storage-revision mass-storage)
                            "\"")
      (sup:debug-print-line "num blocks: #x"
                            (mass-storage-num-blocks mass-storage)
                            ", block size: #x"
                            (mass-storage-block-size mass-storage)))

    (setf (mass-storage-disk mass-storage)
          (make-instance 'usb-ms-disk
                         :mass-storage mass-storage
                         ;; TODO really determine writable
                         :writable-p T
                         :n-sectors (mass-storage-num-blocks mass-storage)
                         :sector-size (mass-storage-block-size mass-storage))
          (mass-storage-partitions mass-storage)
          (mapcar #'(lambda (part-info)
                      (apply #'make-instance 'usb-ms-partition
                             :mass-storage mass-storage
                             part-info))
                  (parse-partition-table (mass-storage-disk mass-storage))))

    (register-block-device (mass-storage-disk mass-storage))
    (dolist (part (mass-storage-partitions mass-storage))
      (register-block-device part))

    (setf *mass-storage* mass-storage)

    (values configs mass-storage)))

(define-usb-class-driver "Mass Storage" 'probe-mass-storage-scsi
  '((#.+id-class-mass-storage+ #.+mass-subclass-scsi+ #.+id-protocol-bulk-only+)))

(defun mass-storage-int-callback (mass-storage endpoint-num status length buf)
  (setf (mass-storage-status mass-storage) status
        (sup:event-state (mass-storage-event mass-storage)) T))


(defun reset-recovery (usbd device mass-storage)
  (with-trace-level (0)
    (sup:debug-print-line "reset-recovery"))
  (with-buffers ((buf-pool usbd) (buf /8 1)) ;; don't want 0 length buf
    (control-receive-data usbd
                          device
                          #b00100001   ;; Class, Interface, host to device
                          #xFF
                          0
                          (mass-storage-interface mass-storage)
                          0
                          buf)
    (control-receive-data usbd
                          device
                          (encode-request-type +rt-dir-host-to-device+
                                               +rt-type-standard+
                                               +rt-rec-endpoint+)
                          +dev-req-clear-feature+
                          +feature-endpoint-halt+
                          (mass-storage-bulk-in-endpt-num mass-storage)
                          0
                          buf)
    (control-receive-data usbd
                          device
                          (encode-request-type +rt-dir-host-to-device+
                                               +rt-type-standard+
                                               +rt-rec-endpoint+)
                          +dev-req-clear-feature+
                          +feature-endpoint-halt+
                          (mass-storage-bulk-out-endpt-num mass-storage)
                          0
                          buf)))
