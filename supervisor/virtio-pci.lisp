;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; PCI transport for virtio devices.

(in-package :mezzano.supervisor)

;;; Registers in the PCI virtio header.
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

(macrolet ((accessor (name offset how)
             `(progn
                (defun ,name (device)
                  (,how (virtio-device-header device) ,offset))
                (defun (setf ,name) (value device)
                  (setf (,how (virtio-device-header device) ,offset) value)))))
  (accessor virtio-pci-device-features +virtio-pci-device-features+ pci-io-region/32)
  (accessor virtio-pci-guest-features +virtio-pci-guest-features+ pci-io-region/32)
  (accessor virtio-pci-queue-address +virtio-pci-queue-address+ pci-io-region/32)
  (accessor virtio-pci-queue-size +virtio-pci-queue-size+ pci-io-region/16)
  (accessor virtio-pci-queue-select +virtio-pci-queue-select+ pci-io-region/16)
  (accessor virtio-pci-queue-notify +virtio-pci-queue-notify+ pci-io-region/16)
  (accessor virtio-pci-device-status +virtio-pci-device-status+ pci-io-region/8)
  (accessor virtio-pci-isr-status +virtio-pci-isr-status+ pci-io-region/8))

(defun virtio-pci-device-specific-header/8 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci-io-region/8 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)))

(defun (setf virtio-pci-device-specific-header/8) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci-io-region/8 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)) value))

(defun virtio-pci-device-specific-header/16 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci-io-region/16 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)))

(defun (setf virtio-pci-device-specific-header/16) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci-io-region/16 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)) value))

(defun virtio-pci-device-specific-header/32 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci-io-region/32 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)))

(defun (setf virtio-pci-device-specific-header/32) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci-io-region/32 (virtio-device-header device) (+ +virtio-pci-device-specific+ offset)) value))

(defun virtio-pci-register (location)
  (let* ((header (pci-bar location 0))
         (dev (make-virtio-device :pci-device location
                                  :header header
                                  :did (pci-config/16 location +pci-config-subdeviceid+)
                                  :boot-id (current-boot-id))))
    ;; Enable PCI bus master bit, just in case it wasn't set and the emulator
    ;; is really picky.
    (setf (pci-bus-master-enabled location) t)
    (virtio-device-register dev)))

(defun virtio-pci-kick (dev vq-id)
  "Notify the device that new buffers have been added to VQ-ID."
  (setf (virtio-pci-queue-notify dev) vq-id))

(defun virtio-pci-configure-virtqueues (device n-queues)
  (setf (virtio-device-virtqueues device) (sys.int::make-simple-vector n-queues :wired))
  (dotimes (queue n-queues)
    ;; 1. Write the virtqueue index to the queue select field.
    (setf (virtio-pci-queue-select device) queue)
    ;; Read the virtqueue size from the queue size field.
    (let* ((queue-size (virtio-pci-queue-size device))
           (size (virtio-ring-size queue-size)))
      (debug-print-line "Virtqueue " queue " has size " queue-size ". Computed size is " size)
      (when (not (zerop queue-size))
        ;; Allocate and clear the virtqueue.
        ;; Must be 4k aligned and contiguous in physical memory.
        (let* ((frame (or (allocate-physical-pages (ceiling size +4k-page-size+))
                          (progn (debug-print-line "Virtqueue allocation failed")
                                 (return-from virtio-pci-configure-virtqueues nil))))
               (phys (* frame +4k-page-size+))
               (virt (convert-to-pmap-address phys)))
          (debug-print-line "Virtqueue allocated at " phys " (" (ceiling size +4k-page-size+) ")")
          (dotimes (i size)
            (setf (sys.int::memref-unsigned-byte-8 virt i) 0))
          ;; Write the address to the the queue address field.
          ;; This is a page number, not an actual address.
          (setf (virtio-pci-queue-address device) frame)
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

(defun virtio-pci-device-irq (device)
  (pci-config/8 (virtio-device-pci-device device) +pci-config-intr-line+))
