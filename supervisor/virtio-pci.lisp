;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; PCI transport for virtio devices.

(in-package :mezzano.supervisor)

;;;; Standard v1.0 PCI transport.

(define-virtio-transport virtio-pci-transport)

(defstruct (virtio-pci-device
             (:include virtio-device)
             (:area :wired))
  pci-device
  common-cfg
  notify-cfg
  notify-off-multiplier
  isr-cfg
  device-cfg
  pci-cfg)

;;; Virtio PCI capabilities.
(defconstant +virtio-pci-cap-common-cfg+ 1)
(defconstant +virtio-pci-cap-notify-cfg+ 2)
(defconstant +virtio-pci-cap-isr-cfg+ 3)
(defconstant +virtio-pci-cap-device-cfg+ 4)
(defconstant +virtio-pci-cap-pci-cfg+ 5)

(defconstant +virtio-pci-capability-bar+ 4)
(defconstant +virtio-pci-capability-offset+ 8)
(defconstant +virtio-pci-capability-length+ 12)
(defconstant +virtio-pci-capability-notify-off-multiplier+ 16)

(defun virtio-pci-read-cap (pci-device cap-id)
  ;; Read the BAR, offset & length fields for a given capability.
  ;; Returns an object suitable for use with ....
  (let* ((cap (pci-get-vendor-capability pci-device cap-id))
         (bar (pci-config/8 pci-device (+ cap +virtio-pci-capability-bar+)))
         (offset (pci-config/32 pci-device (+ cap +virtio-pci-capability-offset+)))
         (length (pci-config/32 pci-device (+ cap +virtio-pci-capability-length+)))
         (region (pci-io-region pci-device bar (+ offset length))))
    (sys.int::cons-in-area region offset :wired)))

(defun virtio-pci-access (cap offset)
  (values (car cap) (+ (cdr cap) offset)))

;;; Common configuration structure layout.
(macrolet ((def (name offset how)
             `(progn
                (defun ,name (device)
                  (multiple-value-bind (loc real-offset)
                      (virtio-pci-access (virtio-pci-device-common-cfg device) ,offset)
                    (,how loc real-offset)))
                (defun (setf ,name) (value device)
                  (multiple-value-bind (loc real-offset)
                      (virtio-pci-access (virtio-pci-device-common-cfg device) ,offset)
                    (setf (,how loc real-offset) value))))))
  (def virtio-pci-common-cfg-device-feature-select 0 pci-io-region/le32)
  (def virtio-pci-common-cfg-device-feature        4 pci-io-region/le32)
  (def virtio-pci-common-cfg-driver-feature-select 8 pci-io-region/le32)
  (def virtio-pci-common-cfg-driver-feature       12 pci-io-region/le32)
  (def virtio-pci-common-cfg-msix-config          16 pci-io-region/le16)
  (def virtio-pci-common-cfg-num-queues           18 pci-io-region/le16)
  (def virtio-pci-common-cfg-device-status        20 pci-io-region/8)
  (def virtio-pci-common-cfg-config-generation    21 pci-io-region/8)
  (def virtio-pci-common-cfg-queue-select         22 pci-io-region/le16)
  (def virtio-pci-common-cfg-queue-size           24 pci-io-region/le16)
  (def virtio-pci-common-cfg-queue-msix-vector    26 pci-io-region/le16)
  (def virtio-pci-common-cfg-queue-enable         28 pci-io-region/le16)
  (def virtio-pci-common-cfg-queue-notify-off     30 pci-io-region/le16)
  (def virtio-pci-common-cfg-queue-desc           32 pci-io-region/le64)
  (def virtio-pci-common-cfg-queue-avail          40 pci-io-region/le64)
  (def virtio-pci-common-cfg-queue-used           48 pci-io-region/le64))

(defun dump-virtio-pci-device (device)
  (debug-print-line "Virtio PCI device " device)
  (debug-print-line "  Common CFG @ " (virtio-pci-device-common-cfg device))
  (debug-print-line "    device-feature-select: " (virtio-pci-common-cfg-device-feature-select device))
  (debug-print-line "    device-feature: " (virtio-pci-common-cfg-device-feature device))
  (debug-print-line "    driver-feature-select: " (virtio-pci-common-cfg-driver-feature-select device))
  (debug-print-line "    driver-feature: " (virtio-pci-common-cfg-driver-feature device))
  (debug-print-line "    msix-config: " (virtio-pci-common-cfg-msix-config device))
  (debug-print-line "    num-queues: " (virtio-pci-common-cfg-num-queues device))
  (debug-print-line "    device-status: " (virtio-pci-common-cfg-device-status device))
  (debug-print-line "    config-generation: " (virtio-pci-common-cfg-config-generation device))
  (dotimes (i (virtio-pci-common-cfg-num-queues device))
    (setf (virtio-pci-common-cfg-queue-select device) i)
    (debug-print-line "   Queue " i)
    (debug-print-line "    queue-select: " (virtio-pci-common-cfg-queue-select device))
    (debug-print-line "    queue-size: " (virtio-pci-common-cfg-queue-size device))
    (debug-print-line "    queue-msix-vector: " (virtio-pci-common-cfg-queue-msix-vector device))
    (debug-print-line "    queue-enable: " (virtio-pci-common-cfg-queue-enable device))
    (debug-print-line "    queue-notify-off: " (virtio-pci-common-cfg-queue-notify-off device))
    (debug-print-line "    queue-desc: " (virtio-pci-common-cfg-queue-desc device))
    (debug-print-line "    queue-avail: " (virtio-pci-common-cfg-queue-avail device))
    (debug-print-line "    queue-used: " (virtio-pci-common-cfg-queue-used device)))
  (debug-print-line "  Notify CFG @ " (virtio-pci-device-notify-cfg device))
  (debug-print-line "    notify-off-multiplier: " (virtio-pci-device-notify-off-multiplier device)))

(defun virtio-pci-transport-device-status (device)
  (virtio-pci-common-cfg-device-status device))

(defun (setf virtio-pci-transport-device-status) (value device)
  (setf (virtio-pci-common-cfg-device-status device) value))

(defun virtio-pci-transport-kick (device vq-id)
  "Notify the device that new buffers have been added to VQ-ID."
  (setf (virtio-pci-common-cfg-queue-select device) vq-id)
  (multiple-value-bind (loc real-offset)
      (virtio-pci-access (virtio-pci-device-notify-cfg device)
                         (* (virtio-pci-device-notify-off-multiplier device)
                            (virtio-pci-common-cfg-queue-notify-off device)))
    (setf (pci-io-region/le16 loc real-offset) vq-id)))

(defun virtio-pci-transport-configure-virtqueues (device n-queues)
  (setf (virtio-device-virtqueues device) (sys.int::make-simple-vector n-queues :wired))
  (dotimes (queue n-queues)
    ;; 1. Write the virtqueue index to the queue select field.
    (setf (virtio-pci-common-cfg-queue-select device) queue)
    ;; Read the virtqueue size from the queue size field.
    ;; TODO: This is the maximum size, could be reduced.
    (let* ((queue-size (virtio-pci-common-cfg-queue-size device))
           (size (virtio-ring-size queue-size)))
      (debug-print-line "Virtqueue " queue " has size " queue-size ". Computed size is " size)
      (when (not (zerop queue-size))
        ;; Allocate and clear the virtqueue.
        ;; Must be 4k aligned and contiguous in physical memory.
        (let* ((frame (or (allocate-physical-pages (ceiling size +4k-page-size+))
                          (progn (debug-print-line "Virtqueue allocation failed")
                                 (return-from virtio-pci-transport-configure-virtqueues nil))))
               (phys (* frame +4k-page-size+))
               (virt (convert-to-pmap-address phys)))
          (debug-print-line "Virtqueue allocated at " phys " (" (ceiling size +4k-page-size+) ")")
          (dotimes (i size)
            (setf (sys.int::memref-unsigned-byte-8 virt i) 0))
          ;; Write the address to the the queue address field.
          (setf (virtio-pci-common-cfg-queue-desc device) phys)
          (setf (virtio-pci-common-cfg-queue-avail device) (+ phys (* queue-size +virtio-ring-desc-size+)))
          (setf (virtio-pci-common-cfg-queue-used device) (+ phys (align-up (+ (* queue-size +virtio-ring-desc-size+)
                                                                              4
                                                                              (* queue-size 2))
                                                                           4096)))
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
  ;; Enable virtqueues.
  (dotimes (queue n-queues)
    (setf (virtio-pci-common-cfg-queue-select device) queue)
    (setf (virtio-pci-common-cfg-queue-enable device) 1))
  t)

;;;; Legacy PCI transport.

(define-virtio-transport virtio-legacy-pci-transport)

(defstruct (virtio-legacy-pci-device
             (:include virtio-device)
             (:area :wired))
  pci-device
  header)

;;; Registers in the PCI virtio header.
(defconstant +virtio-legacy-pci-device-features+ 0
  "Device features, 32 bits.")
(defconstant +virtio-legacy-pci-guest-features+ 4
  "Guest features, 32 bits.")
(defconstant +virtio-legacy-pci-queue-address+ 8
  "Queue address, 32 bits.")
(defconstant +virtio-legacy-pci-queue-size+ 12
  "Queue size, 16 bits.")
(defconstant +virtio-legacy-pci-queue-select+ 14
  "Queue select, 16 bits.")
(defconstant +virtio-legacy-pci-queue-notify+ 16
  "Queue notify, 16 bits.")
(defconstant +virtio-legacy-pci-device-status+ 18
  "Device status, 8 bits.")
(defconstant +virtio-legacy-pci-isr-status+ 19
  "ISR status, 8 bits.")
(defconstant +virtio-legacy-pci-device-specific+ 20
  "Start of device-specific fields.")

(macrolet ((accessor (name offset how)
             `(progn
                (defun ,name (device)
                  (,how (virtio-legacy-pci-device-header device) ,offset))
                (defun (setf ,name) (value device)
                  (setf (,how (virtio-legacy-pci-device-header device) ,offset) value)))))
  (accessor virtio-legacy-pci-transport-device-features +virtio-legacy-pci-device-features+ pci-io-region/32)
  (accessor virtio-legacy-pci-transport-guest-features +virtio-legacy-pci-guest-features+ pci-io-region/32)
  (accessor virtio-legacy-pci-transport-queue-address +virtio-legacy-pci-queue-address+ pci-io-region/32)
  (accessor virtio-legacy-pci-transport-queue-size +virtio-legacy-pci-queue-size+ pci-io-region/16)
  (accessor virtio-legacy-pci-transport-queue-select +virtio-legacy-pci-queue-select+ pci-io-region/16)
  (accessor virtio-legacy-pci-transport-queue-notify +virtio-legacy-pci-queue-notify+ pci-io-region/16)
  (accessor virtio-legacy-pci-transport-device-status +virtio-legacy-pci-device-status+ pci-io-region/8)
  (accessor virtio-legacy-pci-transport-isr-status +virtio-legacy-pci-isr-status+ pci-io-region/8))

(defun virtio-legacy-pci-transport-device-specific-header/8 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci-io-region/8 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)))

(defun (setf virtio-legacy-pci-transport-specific-header/8) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci-io-region/8 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)) value))

(defun virtio-legacy-pci-transport-specific-header/16 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci-io-region/16 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)))

(defun (setf virtio-legacy-pci-transport-specific-header/16) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci-io-region/16 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)) value))

(defun virtio-legacy-pci-transport-specific-header/32 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci-io-region/32 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)))

(defun (setf virtio-legacy-pci-transport-specific-header/32) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci-io-region/32 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)) value))

(defun virtio-legacy-pci-transport-kick (dev vq-id)
  "Notify the device that new buffers have been added to VQ-ID."
  (setf (virtio-legacy-pci-transport-queue-notify dev) vq-id))

(defun virtio-legacy-pci-transport-configure-virtqueues (device n-queues)
  (setf (virtio-device-virtqueues device) (sys.int::make-simple-vector n-queues :wired))
  (dotimes (queue n-queues)
    ;; 1. Write the virtqueue index to the queue select field.
    (setf (virtio-legacy-pci-transport-queue-select device) queue)
    ;; Read the virtqueue size from the queue size field.
    (let* ((queue-size (virtio-legacy-pci-transport-queue-size device))
           (size (virtio-ring-size queue-size)))
      (debug-print-line "Virtqueue " queue " has size " queue-size ". Computed size is " size)
      (when (not (zerop queue-size))
        ;; Allocate and clear the virtqueue.
        ;; Must be 4k aligned and contiguous in physical memory.
        (let* ((frame (or (allocate-physical-pages (ceiling size +4k-page-size+))
                          (progn (debug-print-line "Virtqueue allocation failed")
                                 (return-from virtio-legacy-pci-transport-configure-virtqueues nil))))
               (phys (* frame +4k-page-size+))
               (virt (convert-to-pmap-address phys)))
          (debug-print-line "Virtqueue allocated at " phys " (" (ceiling size +4k-page-size+) ")")
          (dotimes (i size)
            (setf (sys.int::memref-unsigned-byte-8 virt i) 0))
          ;; Write the address to the the queue address field.
          ;; This is a page number, not an actual address.
          (setf (virtio-legacy-pci-transport-queue-address device) frame)
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

(defun virtio-legacy-pci-transport-device-irq (device)
  (pci-config/8 (virtio-legacy-pci-device-pci-device device) +pci-config-intr-line+))

(defun virtio-legacy-pci-transport-ack-irq (device status)
  (declare (ignore device status))
  nil)

(defun virtio-pci-register (location)
  (cond ((<= #x1000 (pci-config/16 location +pci-config-deviceid+) #x103F)
         ;; This is a legacy or transitional device
         ;; TODO: Operate transitional devices in normal mode.
         ;; TODO: Test the VIRTIO_F_VERSION_1 feature.
         (let* ((legacy-header (pci-bar location 0))
                (dev (make-virtio-legacy-pci-device
                      :pci-device location
                      :header legacy-header
                      :transport 'virtio-legacy-pci-transport
                      :did (pci-config/16 location +pci-config-subdeviceid+)
                      :boot-id (current-boot-id))))
           ;; Enable PCI bus master bit, just in case it wasn't set and
           ;; the emulator is really picky.
           (setf (pci-bus-master-enabled location) t)
           (virtio-device-register dev)))
        (t
         ;; This is a standard v1 device.
         (let* ((common-cfg (virtio-pci-read-cap location +virtio-pci-cap-common-cfg+))
                (notify-cfg (virtio-pci-read-cap location +virtio-pci-cap-notify-cfg+))
                (isr-cfg (virtio-pci-read-cap location +virtio-pci-cap-isr-cfg+))
                (device-cfg (virtio-pci-read-cap location +virtio-pci-cap-device-cfg+))
                (pci-cfg (virtio-pci-read-cap location +virtio-pci-cap-pci-cfg+))
                (dev (make-virtio-pci-device
                      :pci-device location
                      :common-cfg common-cfg
                      :notify-cfg notify-cfg
                      :isr-cfg isr-cfg
                      :device-cfg device-cfg
                      :pci-cfg pci-cfg
                      :notify-off-multiplier (virtio-pci-read-notify-off-multiplier location)
                      :transport 'virtio-pci-transport
                      :did (- (pci-config/16 location +pci-config-deviceid+) #x1040)
                      :boot-id (current-boot-id))))
           (dump-virtio-pci-device dev)
           ;; Enable PCI bus master bit, just in case it wasn't set and
           ;; the emulator is really picky.
           (setf (pci-bus-master-enabled location) t)
           (virtio-device-register dev)))))

(defun virtio-pci-read-notify-off-multiplier (pci-device)
  "Read the NOTIFY-OFF-MULTIPLIER from PCI config space."
  (let ((cap (pci-get-vendor-capability pci-device +virtio-pci-cap-notify-cfg+)))
    (pci-config/32 pci-device (+ cap +virtio-pci-capability-notify-off-multiplier+))))
