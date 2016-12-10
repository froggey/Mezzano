;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant +virtio-block-capacity+ 0)
(defconstant +virtio-block-size-max+ 8)
(defconstant +virtio-block-seg-max+ 12)
(defconstant +virtio-block-blk-size+ 20)

(defconstant +virtio-block-req-type+ 0)
(defconstant +virtio-block-req-ioprio+ 4)
(defconstant +virtio-block-req-sector+ 8)
(defconstant +virtio-block-req-status+ 16)

(defconstant +virtio-block-t-in+ 0)
(defconstant +virtio-block-t-out+ 1)
(defconstant +virtio-block-t-flush+ 4)
(defconstant +virtio-block-t-flush-out+ 5)

(defconstant +virtio-block-s-ok+ 0)
(defconstant +virtio-block-s-ioerr+ 1)
(defconstant +virtio-block-s-unsup+ 2)

(defstruct (virtio-block
             (:area :wired))
  virtio-device
  irq-handler-function
  (irq-latch (make-latch "Virtio-Block IRQ Notifier"))
  request-phys
  request-virt)

(defun virtio-block-rw (device lba count mem-addr rw)
  (let* ((req (virtio-block-request-virt device))
         (dev (virtio-block-virtio-device device))
         (vq (virtio-virtqueue dev 0))
         (req-desc (virtio-ring-alloc-descriptor vq))
         (buf-desc (virtio-ring-alloc-descriptor vq))
         (stat-desc (virtio-ring-alloc-descriptor vq)))
    ;; Set up the block request.
    (setf (sys.int::memref-unsigned-byte-32 (+ req +virtio-block-req-type+) 0)
          (ecase rw
            (:read +virtio-block-t-in+)
            (:write +virtio-block-t-out+)))
    (setf (sys.int::memref-unsigned-byte-32 (+ req +virtio-block-req-ioprio+) 0)
          0)
    (setf (sys.int::memref-unsigned-byte-64 (+ req +virtio-block-req-sector+) 0)
          lba)
    (setf (sys.int::memref-unsigned-byte-32 (+ req +virtio-block-req-status+) 0)
          0)
    (ensure req-desc)
    (ensure buf-desc)
    (ensure stat-desc)
    ;; Request descriptor.
    (setf (virtio-ring-desc-address vq req-desc) (virtio-block-request-phys device)
          (virtio-ring-desc-length vq req-desc) +virtio-block-req-status+
          (virtio-ring-desc-flags vq req-desc) (ash 1 +virtio-ring-desc-f-next+)
          (virtio-ring-desc-next vq req-desc) buf-desc)
    ;; Buffer descriptor.
    (setf (virtio-ring-desc-address vq buf-desc) (- mem-addr +physical-map-base+)
          (virtio-ring-desc-length vq buf-desc) (* count 512)
          (virtio-ring-desc-flags vq buf-desc) (logior (ash 1 +virtio-ring-desc-f-next+)
                                                       (ecase rw
                                                         (:read (ash 1 +virtio-ring-desc-f-write+))
                                                         (:write 0)))
          (virtio-ring-desc-next vq buf-desc) stat-desc)
    ;; Status descriptor.
    (setf (virtio-ring-desc-address vq stat-desc) (+ (virtio-block-request-phys device)
                                                     +virtio-block-req-status+)
          (virtio-ring-desc-length vq stat-desc) 1
          (virtio-ring-desc-flags vq stat-desc) (ash 1 +virtio-ring-desc-f-write+))
    (virtio-ring-add-to-avail-ring vq req-desc)
    (virtio-kick dev 0)
    (latch-wait (virtio-block-irq-latch device))
    (latch-reset (virtio-block-irq-latch device))
    ;; Release the descriptors.
    (virtio-ring-free-descriptor vq req-desc)
    (virtio-ring-free-descriptor vq buf-desc)
    (virtio-ring-free-descriptor vq stat-desc)
    ;; Check status.
    (let ((status (sys.int::memref-unsigned-byte-8 (+ req +virtio-block-req-status+) 0)))
      (values (eql status +virtio-block-s-ok+)
              (case status
                (#.+virtio-block-s-ok+ :no-error)
                (#.+virtio-block-s-ioerr+ :io-error)
                (#.+virtio-block-s-unsup+ :unsupported)
                (t :unknown))))))

(defun virtio-block-read (device lba count mem-addr)
  (virtio-block-rw device lba count mem-addr :read))

(defun virtio-block-write (device lba count mem-addr)
  (virtio-block-rw device lba count mem-addr :write))

(defun virtio-block-irq-handler (blk)
  (latch-trigger (virtio-block-irq-latch blk)))

(defun virtio-block-register (device)
  ;; Wired allocation required for the IRQ handler closure.
  (declare (sys.c::closure-allocation :wired))
  (debug-print-line "Detected virtio block device " device)
  (let* ((blk (make-virtio-block :virtio-device device))
         (irq-handler (lambda (interrupt-frame irq)
                        (declare (ignore interrupt-frame irq))
                        (virtio-block-irq-handler blk)))
         ;; Allocate some memory for the request header & footer.
         (frame (or (allocate-physical-pages 1)
                    (panic "Unable to allocate memory for virtio block request")))
         (phys (* frame +4k-page-size+))
         (virt (convert-to-pmap-address phys)))
    (debug-print-line "Virtio-Block request data at " phys)
    (setf (virtio-block-request-phys blk) phys
          (virtio-block-request-virt blk) virt)
    (setf (virtio-block-irq-handler-function blk) irq-handler)
    (virtio-attach-irq device irq-handler)
    ;; Set the driver bit in the status field.
    (setf (virtio-device-status device) (logior +virtio-status-acknowledge+
                                                +virtio-status-driver+))
    ;; Allocate virtqueue.
    (when (not (virtio-configure-virtqueues device 1))
      (setf (virtio-device-status device) +virtio-status-failed+)
      (return-from virtio-block-register nil))
    ;; Read data from the config area.
    ;; TODO: Set the BLK-SIZE feature and use non-512 byte blocks.
    (let ((capacity (virtio-device-specific-header/64 device +virtio-block-capacity+)))
      (register-disk blk t capacity 512 256 'virtio-block-read 'virtio-block-write))
    ;; Enable IRQ handler.
    (setf (virtio-irq-mask device) nil)
    ;; Configuration complete, go to OK mode.
    (setf (virtio-device-status device) (logior +virtio-status-acknowledge+
                                                +virtio-status-driver+
                                                +virtio-status-ok+))
    t))
