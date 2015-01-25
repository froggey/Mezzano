(in-package :mezzano.supervisor)

;;; Subsystem IDs
(defconstant +virtio-pci-subsystem-network-device+ 1)

;;; Registes in the virtio header.
(defconstant +virtio-pci-device-features+ 0
  "Device features, 32 bits.")
(defconstant +virtio-pci-guest-features+ 4
  "Guest features, 32 bits.")
(defconstant +virtio-pci-queue-address+ 8
  "Queue address, 32 bits.")
(defconstant +virtio-pci-queue-size+ 12
  "Queue size, 16 bits.")
(defconstant +virtio-pci-queue-select+ 14
  "Queue select, 16 bits.")
(defconstant +virtio-pci-queue-notify+ 16
  "Queue notify, 16 bits.")
(defconstant +virtio-pci-device-status+ 18
  "Device status, 8 bits.")
(defconstant +virtio-pci-isr-status+ 19
  "ISR status, 8 bits.")
(defconstant +virtio-pci-device-specific+ 20
  "Start of device-specific fields.")

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
  pci-device
  header
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

(defun initialize-virtio ()
  ;; Nothing.
  )

(macrolet ((accessor (name offset how)
             `(progn
                (defun ,name (device)
                  (,how (virtio-device-header device) ,offset))
                (defun (setf ,name) (value device)
                  (setf (,how (virtio-device-header device) ,offset) value)))))
  (accessor virtio-device-features +virtio-pci-device-features+ pci-io-region/32)
  (accessor virtio-guest-features +virtio-pci-guest-features+ pci-io-region/32)
  (accessor virtio-queue-address +virtio-pci-queue-address+ pci-io-region/32)
  (accessor virtio-queue-size +virtio-pci-queue-size+ pci-io-region/16)
  (accessor virtio-queue-select +virtio-pci-queue-select+ pci-io-region/16)
  (accessor virtio-queue-notify +virtio-pci-queue-notify+ pci-io-region/16)
  (accessor virtio-device-status +virtio-pci-device-status+ pci-io-region/8)
  (accessor virtio-isr-status +virtio-pci-isr-status+ pci-io-region/8))

(defun virtio-device-specific-header/8 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci-io-region/8 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)))

(defun (setf virtio-device-specific-header/8) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci-io-region/8 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)) value))

(defun virtio-device-specific-header/16 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci-io-region/16 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)))

(defun (setf virtio-device-specific-header/16) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci-io-region/16 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)) value))

(defun virtio-device-specific-header/32 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci-io-region/32 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)))

(defun (setf virtio-device-specific-header/32) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci-io-region/32 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)) value))

(defun virtio-pci-register (location)
  (let* ((header (pci-bar location 0))
         (dev (make-virtio-device :pci-device location :header header)))
    ;; Reset device.
    (setf (pci-io-region/8 header +virtio-pci-device-status+) +virtio-status-reset+)
    ;; Acknowledge the device.
    (setf (pci-io-region/8 header +virtio-pci-device-status+) +virtio-status-acknowledge+)
    ;; Enable PCI bus master bit, just in case it wasn't set and the emulator
    ;; is really picky.
    (setf (pci-config/16 location +pci-config-command+) (logior (pci-config/16 location +pci-config-command+)
                                                                ;; Bit 2 is Bus Master bit.
                                                                (ash 1 2)))
    (case (pci-config/16 location +pci-config-subdeviceid+)
      (#.+virtio-pci-subsystem-network-device+
       (virtio-net-register dev)))))

(defun virtio-ring-size (queue-size)
  "Compute the actual size of a vring from the queue-size field."
  (+ (align-up (+ (* +virtio-ring-desc-size+ queue-size) (* 2 (+ 2 queue-size))) 4096)
     (align-up (* +virtio-ring-used-elem-size+ queue-size) 4096)))

(defun virtio-virtqueue (device virtqueue)
  (svref (virtio-device-virtqueues device) virtqueue))

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
  (setf (virtio-queue-notify dev) vq-id))

(defun virtio-ring-disable-interrupts (vq)
  (setf (virtio-ring-avail-flags vq) (logior (virtio-ring-avail-flags vq)
                                             (ash 1 +virtio-ring-avail-f-no-interrupt+))))

(defun virtio-ring-enable-interrupts (vq)
  (setf (virtio-ring-avail-flags vq) (logand (virtio-ring-avail-flags vq)
                                             (lognot (ash 1 +virtio-ring-avail-f-no-interrupt+)))))

(defun virtio-configure-virtqueues (device n-queues)
  (setf (virtio-device-virtqueues device) (sys.int::make-simple-vector n-queues :wired))
  (dotimes (queue n-queues)
    ;; 1. Write the virtqueue index to the queue select field.
    (setf (virtio-queue-select device) queue)
    ;; Read the virtqueue size from the queue size field.
    (let* ((queue-size (virtio-queue-size device))
           (size (virtio-ring-size queue-size)))
      (debug-print-line "Virtqueue " queue " has size " queue-size ". Computed size is " size)
      (when (not (zerop queue-size))
        ;; Allocate and clear the virtqueue.
        ;; Must be 4k aligned and contiguous in physical memory.
        (let* ((frame (or (allocate-physical-pages (ceiling size +4k-page-size+))
                          (progn (debug-print-line "Virtqueue allocation failed")
                                 (return-from virtio-configure-virtqueues nil))))
               (phys (* frame +4k-page-size+))
               (virt (+ +physical-map-base+ phys)))
          (debug-print-line "Virtqueue allocated at " phys " (" (ceiling size +4k-page-size+) ")")
          (dotimes (i size)
            (setf (sys.int::memref-unsigned-byte-8 virt i) 0))
          ;; Write the address to the the queue address field.
          ;; This is a page number, not an actual address.
          (setf (virtio-queue-address device) frame)
          (let ((vq (make-virtqueue :index queue
                                    :virtual virt
                                    :physical phys
                                    :size queue-size
                                    :avail-offset (* queue-size +virtio-ring-desc-size+)
                                    :used-offset (align-up (+ (* queue-size +virtio-ring-desc-size+)
                                                              4
                                                              (* queue-size 2))
                                                           4096)
                                    :last-seen-used 0)))
            (setf (svref (virtio-device-virtqueues device) queue) vq)
            ;; Initialize the free descriptor list.
            (dotimes (i (1- queue-size))
              (setf (virtio-ring-desc-next vq i) (1+ i)
                    (virtio-ring-desc-flags vq i) (ash 1 +virtio-ring-desc-f-next+)))
            (setf (virtqueue-next-free-descriptor vq) 0))))))
  t)

(defun virtio-attach-irq (device handler)
  (i8259-hook-irq (virtio-device-irq device) handler))

(defun virtio-device-irq (device)
  (pci-config/8 (virtio-device-pci-device device) +pci-config-intr-line+))

(defun (setf virtio-irq-mask) (value device)
  (if value
      (i8259-mask-irq (virtio-device-irq device))
      (i8259-unmask-irq (virtio-device-irq device))))
