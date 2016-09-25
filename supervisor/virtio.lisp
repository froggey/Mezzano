;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;;; Subsystem IDs
(defconstant +virtio-dev-id-invalid+       #x00)
(defconstant +virtio-dev-id-net+           #x01)
(defconstant +virtio-dev-id-block+         #x02)
(defconstant +virtio-dev-id-console+       #x03)
(defconstant +virtio-dev-id-entropy-src+   #x04)
(defconstant +virtio-dev-id-mem-balloon+   #x05)
(defconstant +virtio-dev-id-io-memory+     #x06)
(defconstant +virtio-dev-id-rpmsg+         #x07)
(defconstant +virtio-dev-id-scsi-host+     #x08)
(defconstant +virtio-dev-id-9p-transport+  #x09)
(defconstant +virtio-dev-id-mac80211-wlan+ #x0A)
(defconstant +virtio-dev-id-rproc-serial+  #x0B)
(defconstant +virtio-dev-id-caif+          #x0C)
(defconstant +virtio-dev-id-gpu+           #x10)
(defconstant +virtio-dev-id-input+         #x12)

;; Bits for the device status field.
(defconstant +virtio-status-reset+ #x00)
(defconstant +virtio-status-acknowledge+ #x01
  "The guest has found the device and recognized it as a valid virtio device.")
(defconstant +virtio-status-driver+ #x02
  "The guest knows how to drive the device.")
(defconstant +virtio-status-ok+ #x04
  "The driver is set up and ready to drive the device.")
(defconstant +virtio-status-failed+ #x80
 "The guest has given up on the device.")

(defconstant +virtio-ring-desc-size+ (+ 8 4 2 2))
(defconstant +virtio-ring-used-elem-size+ (+ 4 4))

;; vring flags.
(defconstant +virtio-ring-desc-f-next+ 0
  "Buffer continues via the next field.")
(defconstant +virtio-ring-desc-f-write+ 1
  "Buffer is write-only (otherwise read-only).")
(defconstant +virtio-ring-desc-f-indirect+ 2
  "Buffer contains a list of buffer descriptors.")

(defconstant +virtio-ring-avail-f-no-interrupt+ 1)

(defconstant +virtio-ring-desc-address-offset+ 0)
(defconstant +virtio-ring-desc-length-offset+ 8)
(defconstant +virtio-ring-desc-flags-offset+ 12)
(defconstant +virtio-ring-desc-next-offset+ 14)

(defconstant +virtio-ring-avail-flags-offset+ 0)
(defconstant +virtio-ring-avail-idx-offset+ 2)
(defconstant +virtio-ring-avail-ring-offset+ 4)

(defconstant +virtio-ring-used-flags-offset+ 0)
(defconstant +virtio-ring-used-idx-offset+ 2)
(defconstant +virtio-ring-used-ring-offset+ 4)

(defconstant +virtio-ring-used-elem-id-offset+ 0)
(defconstant +virtio-ring-used-elem-len-offset+ 4)

(defstruct (virtio-device
             (:area :wired))
  ;; Access through the PCI config space.
  pci-device
  header
  ;; Access through MMIO.
  mmio
  mmio-irq
  virtqueues)

(defstruct (virtqueue
             (:area :wired))
  index
  virtual
  physical
  size
  avail-offset
  used-offset
  next-free-descriptor
  last-seen-used)

(defun virtio-ring-size (queue-size)
  "Compute the actual size of a vring from the queue-size field."
  (+ (align-up (+ (* +virtio-ring-desc-size+ queue-size) (* 2 (+ 2 queue-size))) 4096)
     (align-up (* +virtio-ring-used-elem-size+ queue-size) 4096)))

(defun virtio-virtqueue (device virtqueue)
  (svref (virtio-device-virtqueues device) virtqueue))

(defun virtio-device-specific-header/8 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (if (virtio-device-mmio device)
      (virtio-mmio-device-specific-header/8 device offset)
      (virtio-pci-device-specific-header/8 device offset)))

(defun (setf virtio-device-specific-header/8) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (if (virtio-device-mmio device)
      (setf (virtio-mmio-device-specific-header/8 device offset) value)
      (setf (virtio-pci-device-specific-header/8 device offset) value)))

(defun virtio-device-specific-header/16 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (if (virtio-device-mmio device)
      (virtio-mmio-device-specific-header/16 device offset)
      (virtio-pci-device-specific-header/16 device offset)))

(defun (setf virtio-device-specific-header/16) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (if (virtio-device-mmio device)
      (setf (virtio-mmio-device-specific-header/16 device offset) value)
      (setf (virtio-pci-device-specific-header/16 device offset) value)))

(defun virtio-device-specific-header/32 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (if (virtio-device-mmio device)
      (virtio-mmio-device-specific-header/32 device offset)
      (virtio-pci-device-specific-header/32 device offset)))

(defun (setf virtio-device-specific-header/32) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (if (virtio-device-mmio device)
      (setf (virtio-mmio-device-specific-header/32 device offset) value)
      (setf (virtio-pci-device-specific-header/32 device offset) value)))

(defun virtio-device-specific-header/64 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (logior (virtio-device-specific-header/32 device offset)
          (ash (virtio-device-specific-header/32 device (+ offset 4)) 32)))

(defun (setf virtio-device-specific-header/64) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (virtio-device-specific-header/32 device offset) (ldb (byte 32 0) value)
        (virtio-device-specific-header/32 device (+ offset 4)) (ldb (byte 32 32) value)))

;;; Accessors for the virtqueue descriptors.
(macrolet ((def (name offset accessor)
             `(progn
                (defun ,name (vq idx)
                  #+(or)(debug-print-line "read field " ',name " idx " idx " @ " (+ (virtqueue-virtual vq) (* idx +virtio-ring-desc-size+) ,offset))
                  (assert (< idx (virtqueue-size vq)))
                  (,accessor (+ (virtqueue-virtual vq) (* idx +virtio-ring-desc-size+) ,offset) 0))
                (defun (setf ,name) (value vq idx)
                  #+(or)(debug-print-line "write field " ',name " idx " idx " @ " (+ (virtqueue-virtual vq) (* idx +virtio-ring-desc-size+) ,offset) " value " value)
                  (assert (< idx (virtqueue-size vq)))
                  (setf (,accessor (+ (virtqueue-virtual vq) (* idx +virtio-ring-desc-size+) ,offset) 0) value)))))
  (def virtio-ring-desc-address +virtio-ring-desc-address-offset+ sys.int::memref-unsigned-byte-64)
  (def virtio-ring-desc-length +virtio-ring-desc-length-offset+ sys.int::memref-unsigned-byte-32)
  (def virtio-ring-desc-flags +virtio-ring-desc-flags-offset+ sys.int::memref-unsigned-byte-16)
  (def virtio-ring-desc-next +virtio-ring-desc-next-offset+ sys.int::memref-unsigned-byte-16))

(defun virtio-ring-avail-flags (vq)
  (sys.int::memref-unsigned-byte-16 (+ (virtqueue-virtual vq)
                                       (virtqueue-avail-offset vq)
                                       +virtio-ring-avail-flags-offset+)
                                    0))
(defun virtio-ring-avail-idx (vq)
  (sys.int::memref-unsigned-byte-16 (+ (virtqueue-virtual vq)
                                       (virtqueue-avail-offset vq)
                                       +virtio-ring-avail-idx-offset+)
                                    0))
(defun (setf virtio-ring-avail-flags) (value vq)
  (setf (sys.int::memref-unsigned-byte-16 (+ (virtqueue-virtual vq)
                                             (virtqueue-avail-offset vq)
                                             +virtio-ring-avail-flags-offset+)
                                          0)
        value))
(defun (setf virtio-ring-avail-idx) (value vq)
  (setf (sys.int::memref-unsigned-byte-16 (+ (virtqueue-virtual vq)
                                             (virtqueue-avail-offset vq)
                                             +virtio-ring-avail-idx-offset+)
                                          0)
        value))

(defun virtio-ring-avail-ring (vq idx)
  (assert (< idx (virtqueue-size vq)))
  (sys.int::memref-unsigned-byte-16 (+ (virtqueue-virtual vq)
                                       (virtqueue-avail-offset vq)
                                       +virtio-ring-avail-ring-offset+
                                       (* idx 2))
                                    0))
(defun (setf virtio-ring-avail-ring) (value vq idx)
  (assert (< idx (virtqueue-size vq)))
  (setf (sys.int::memref-unsigned-byte-16 (+ (virtqueue-virtual vq)
                                             (virtqueue-avail-offset vq)
                                             +virtio-ring-avail-ring-offset+
                                             (* idx 2))
                                          0)
        value))

(defun virtio-ring-used-flags (vq)
  (sys.int::memref-unsigned-byte-16 (+ (virtqueue-virtual vq)
                                       (virtqueue-used-offset vq)
                                       +virtio-ring-used-flags-offset+)
                                    0))
(defun virtio-ring-used-idx (vq)
  (sys.int::memref-unsigned-byte-16 (+ (virtqueue-virtual vq)
                                       (virtqueue-used-offset vq)
                                       +virtio-ring-used-idx-offset+)
                                    0))
(defun (setf virtio-ring-used-flags) (value vq)
  (setf (sys.int::memref-unsigned-byte-16 (+ (virtqueue-virtual vq)
                                             (virtqueue-used-offset vq)
                                             +virtio-ring-used-flags-offset+)
                                          0)
        value))
(defun (setf virtio-ring-used-idx) (value vq)
  (setf (sys.int::memref-unsigned-byte-16 (+ (virtqueue-virtual vq)
                                             (virtqueue-used-offset vq)
                                             +virtio-ring-used-idx-offset+)
                                          0)
        value))

(defun virtio-ring-used-elem-id (vq idx)
  (assert (< idx (virtqueue-size vq)))
  (sys.int::memref-unsigned-byte-32 (+ (virtqueue-virtual vq)
                                       (virtqueue-used-offset vq)
                                       +virtio-ring-used-ring-offset+
                                       (* idx +virtio-ring-used-elem-size+)
                                       +virtio-ring-used-elem-id-offset+)
                                    0))
(defun virtio-ring-used-elem-len (vq idx)
  (assert (< idx (virtqueue-size vq)))
  (sys.int::memref-unsigned-byte-32 (+ (virtqueue-virtual vq)
                                       (virtqueue-used-offset vq)
                                       +virtio-ring-used-ring-offset+
                                       (* idx +virtio-ring-used-elem-size+)
                                       +virtio-ring-used-elem-len-offset+)
                                    0))
(defun (setf virtio-ring-used-elem-id) (value vq idx)
  (assert (< idx (virtqueue-size vq)))
  (setf (sys.int::memref-unsigned-byte-32 (+ (virtqueue-virtual vq)
                                             (virtqueue-used-offset vq)
                                             +virtio-ring-used-ring-offset+
                                             (* idx +virtio-ring-used-elem-size+)
                                             +virtio-ring-used-elem-id-offset+)
                                          0)
        value))
(defun (setf virtio-ring-used-elem-len) (value vq idx)
  (assert (< idx (virtqueue-size vq)))
  (setf (sys.int::memref-unsigned-byte-32 (+ (virtqueue-virtual vq)
                                             (virtqueue-used-offset vq)
                                             +virtio-ring-used-ring-offset+
                                             (* idx +virtio-ring-used-elem-size+)
                                             +virtio-ring-used-elem-len-offset+)
                                          0)
        value))

(defun virtio-ring-alloc-descriptor (vq)
  (let ((id (virtqueue-next-free-descriptor vq)))
    (when id
      (setf (virtqueue-next-free-descriptor vq)
            (if (logbitp +virtio-ring-desc-f-next+ (virtio-ring-desc-flags vq id))
                (virtio-ring-desc-next vq id)
                nil)))
    id))

(defun virtio-ring-add-to-avail-ring (vq desc)
  "Add a descriptor to the available ring."
  ;; Update the available ring.
  (setf (virtio-ring-avail-ring vq (rem (virtio-ring-avail-idx vq)
                                        (virtqueue-size vq)))
        desc)
  ;; FIXME: memory barrier here.
  ;; Update the index field.
  (setf (virtio-ring-avail-idx vq) (ldb (byte 16 0)
                                        (1+ (virtio-ring-avail-idx vq)))))

(defun virtio-kick (dev vq-id)
  "Notify the device that new buffers have been added to VQ-ID."
  (if (virtio-device-mmio dev)
      (virtio-mmio-kick dev vq-id)
      (virtio-pci-kick dev vq-id)))

(defun virtio-ring-disable-interrupts (vq)
  (setf (virtio-ring-avail-flags vq) (logior (virtio-ring-avail-flags vq)
                                             (ash 1 +virtio-ring-avail-f-no-interrupt+))))

(defun virtio-ring-enable-interrupts (vq)
  (setf (virtio-ring-avail-flags vq) (logand (virtio-ring-avail-flags vq)
                                             (lognot (ash 1 +virtio-ring-avail-f-no-interrupt+)))))

(defun virtio-device-status (dev)
  (if (virtio-device-mmio dev)
      (virtio-mmio-status dev)
      (virtio-pci-device-status dev)))

(defun (setf virtio-device-status) (value dev)
  (if (virtio-device-mmio dev)
      (setf (virtio-mmio-status dev) value)
      (setf (virtio-pci-device-status dev) value)))

(defun virtio-device-register (dev did)
  ;; Reset device.
  (setf (virtio-device-status dev) +virtio-status-reset+)
  ;; Acknowledge the device.
  (setf (virtio-device-status dev) +virtio-status-acknowledge+)
  (case did
    (#.+virtio-dev-id-net+
     (virtio-net-register dev))
    (#.+virtio-dev-id-block+
     (virtio-block-register dev))
    (t
     (debug-print-line "Unknown virtio device type " did)
     (setf (virtio-device-status dev) +virtio-status-failed+))))

(defun virtio-isr-status (device)
  (if (virtio-device-mmio device)
      (virtio-mmio-interrupt-status device)
      (virtio-pci-isr-status device)))

(defun virtio-device-irq (device)
  (if (virtio-device-mmio device)
      (virtio-mmio-device-irq device)
      (virtio-pci-device-irq device)))

(defun virtio-attach-irq (device handler)
  (platform-attach-irq (virtio-device-irq device) handler))

(defun (setf virtio-irq-mask) (value device)
  (if value
      (platform-mask-irq (virtio-device-irq device))
      (platform-unmask-irq (virtio-device-irq device))))

(defun virtio-configure-virtqueues (device n-queues)
  (if (virtio-device-mmio device)
      (virtio-mmio-configure-virtqueues device n-queues)
      (virtio-pci-configure-virtqueues device n-queues)))
