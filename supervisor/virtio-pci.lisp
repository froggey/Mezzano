;;;; PCI transport for virtio devices.

(defpackage :mezzano.supervisor.virtio-pci-transport
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:pci :mezzano.supervisor.pci)
                    (:virtio :mezzano.supervisor.virtio)
                    (:sys.int :mezzano.internals)))

(in-package :mezzano.supervisor.virtio-pci-transport)

;;;; Standard v1.0 PCI transport.

(virtio:define-virtio-transport virtio-pci-transport)

(defstruct (virtio-pci-device
             (:include virtio:virtio-device)
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
  (let* ((cap (pci:pci-get-vendor-capability pci-device cap-id))
         (bar (pci:pci-config/8 pci-device (+ cap +virtio-pci-capability-bar+)))
         (offset (pci:pci-config/32 pci-device (+ cap +virtio-pci-capability-offset+)))
         (length (pci:pci-config/32 pci-device (+ cap +virtio-pci-capability-length+)))
         (region (pci:pci-io-region pci-device bar)))
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
  (def virtio-pci-common-cfg-device-feature-select 0 pci:pci-io-region/le32)
  (def virtio-pci-common-cfg-device-feature        4 pci:pci-io-region/le32)
  (def virtio-pci-common-cfg-driver-feature-select 8 pci:pci-io-region/le32)
  (def virtio-pci-common-cfg-driver-feature       12 pci:pci-io-region/le32)
  (def virtio-pci-common-cfg-msix-config          16 pci:pci-io-region/le16)
  (def virtio-pci-common-cfg-num-queues           18 pci:pci-io-region/le16)
  (def virtio-pci-common-cfg-device-status        20 pci:pci-io-region/8)
  (def virtio-pci-common-cfg-config-generation    21 pci:pci-io-region/8)
  (def virtio-pci-common-cfg-queue-select         22 pci:pci-io-region/le16)
  (def virtio-pci-common-cfg-queue-size           24 pci:pci-io-region/le16)
  (def virtio-pci-common-cfg-queue-msix-vector    26 pci:pci-io-region/le16)
  (def virtio-pci-common-cfg-queue-enable         28 pci:pci-io-region/le16)
  (def virtio-pci-common-cfg-queue-notify-off     30 pci:pci-io-region/le16)
  (def virtio-pci-common-cfg-queue-desc           32 pci:pci-io-region/le64)
  (def virtio-pci-common-cfg-queue-avail          40 pci:pci-io-region/le64)
  (def virtio-pci-common-cfg-queue-used           48 pci:pci-io-region/le64))

(defun dump-virtio-pci-device (device)
  (sup:debug-print-line "Virtio PCI device " device)
  (sup:debug-print-line "  Common CFG @ " (virtio-pci-device-common-cfg device))
  (sup:debug-print-line "    device-feature-select: " (virtio-pci-common-cfg-device-feature-select device))
  (sup:debug-print-line "    device-feature: " (virtio-pci-common-cfg-device-feature device))
  (sup:debug-print-line "    driver-feature-select: " (virtio-pci-common-cfg-driver-feature-select device))
  (sup:debug-print-line "    driver-feature: " (virtio-pci-common-cfg-driver-feature device))
  (sup:debug-print-line "    msix-config: " (virtio-pci-common-cfg-msix-config device))
  (sup:debug-print-line "    num-queues: " (virtio-pci-common-cfg-num-queues device))
  (sup:debug-print-line "    device-status: " (virtio-pci-common-cfg-device-status device))
  (sup:debug-print-line "    config-generation: " (virtio-pci-common-cfg-config-generation device))
  (dotimes (i (virtio-pci-common-cfg-num-queues device))
    (setf (virtio-pci-common-cfg-queue-select device) i)
    (sup:debug-print-line "   Queue " i)
    (sup:debug-print-line "    queue-select: " (virtio-pci-common-cfg-queue-select device))
    (sup:debug-print-line "    queue-size: " (virtio-pci-common-cfg-queue-size device))
    (sup:debug-print-line "    queue-msix-vector: " (virtio-pci-common-cfg-queue-msix-vector device))
    (sup:debug-print-line "    queue-enable: " (virtio-pci-common-cfg-queue-enable device))
    (sup:debug-print-line "    queue-notify-off: " (virtio-pci-common-cfg-queue-notify-off device))
    (sup:debug-print-line "    queue-desc: " (virtio-pci-common-cfg-queue-desc device))
    (sup:debug-print-line "    queue-avail: " (virtio-pci-common-cfg-queue-avail device))
    (sup:debug-print-line "    queue-used: " (virtio-pci-common-cfg-queue-used device)))
  (sup:debug-print-line "  Notify CFG @ " (virtio-pci-device-notify-cfg device))
  (sup:debug-print-line "    notify-off-multiplier: " (virtio-pci-device-notify-off-multiplier device)))

(defun virtio-pci-transport-device-specific-header/8 (device offset)
  (multiple-value-bind (loc real-offset)
      (virtio-pci-access (virtio-pci-device-device-cfg device) offset)
    (pci:pci-io-region/8 loc real-offset)))

(defun (setf virtio-pci-transport-device-specific-header/8) (value device offset)
  (multiple-value-bind (loc real-offset)
      (virtio-pci-access (virtio-pci-device-device-cfg device) offset)
    (setf (pci:pci-io-region/8 loc real-offset) value)))

(defun virtio-pci-transport-device-specific-header/16 (device offset)
  (multiple-value-bind (loc real-offset)
      (virtio-pci-access (virtio-pci-device-device-cfg device) offset)
    (pci:pci-io-region/16 loc real-offset)))

(defun (setf virtio-pci-transport-device-specific-header/16) (value device offset)
  (multiple-value-bind (loc real-offset)
      (virtio-pci-access (virtio-pci-device-device-cfg device) offset)
    (setf (pci:pci-io-region/16 loc real-offset) value)))

(defun virtio-pci-transport-device-specific-header/32 (device offset)
  (multiple-value-bind (loc real-offset)
      (virtio-pci-access (virtio-pci-device-device-cfg device) offset)
    (pci:pci-io-region/32 loc real-offset)))

(defun (setf virtio-pci-transport-device-specific-header/32) (value device offset)
  (multiple-value-bind (loc real-offset)
      (virtio-pci-access (virtio-pci-device-device-cfg device) offset)
    (setf (pci:pci-io-region/32 loc real-offset) value)))

(defun virtio-pci-transport-device-status (device)
  (virtio-pci-common-cfg-device-status device))

(defun (setf virtio-pci-transport-device-status) (value device)
  (setf (virtio-pci-common-cfg-device-status device) value))

(defun virtio-pci-transport-device-feature (device bit)
  (multiple-value-bind (leaf leaf-bit)
      (truncate bit 32)
    (setf (virtio-pci-common-cfg-device-feature-select device) leaf)
    (logbitp leaf-bit (virtio-pci-common-cfg-device-feature device))))

(defun virtio-pci-transport-driver-feature (device bit)
  (multiple-value-bind (leaf leaf-bit)
      (truncate bit 32)
    (setf (virtio-pci-common-cfg-driver-feature-select device) leaf)
    (logbitp leaf-bit (virtio-pci-common-cfg-driver-feature device))))

(defun (setf virtio-pci-transport-driver-feature) (value device bit)
  (multiple-value-bind (leaf leaf-bit)
      (truncate bit 32)
    (setf (virtio-pci-common-cfg-driver-feature-select device) leaf)
    (setf (ldb (byte 1 leaf-bit) (virtio-pci-common-cfg-driver-feature device))
          (if value 1 0))
    value))

(defun virtio-pci-transport-kick (device vq-id)
  "Notify the device that new buffers have been added to VQ-ID."
  (setf (virtio-pci-common-cfg-queue-select device) vq-id)
  (multiple-value-bind (loc real-offset)
      (virtio-pci-access (virtio-pci-device-notify-cfg device)
                         (* (virtio-pci-device-notify-off-multiplier device)
                            (virtio-pci-common-cfg-queue-notify-off device)))
    (setf (pci:pci-io-region/le16 loc real-offset) vq-id)))

(defun virtio-pci-transport-queue-address (device)
  (virtio-pci-common-cfg-queue-desc device))

(defun (setf virtio-pci-transport-queue-address) (address device)
  (let ((queue-size (virtio-pci-common-cfg-queue-size device)))
    ;; Write the address to the the queue address fields.
    (setf (virtio-pci-common-cfg-queue-desc device) address)
    (setf (virtio-pci-common-cfg-queue-avail device)
          (+ address
             (* queue-size virtio:+virtio-ring-desc-size+)))
    (setf (virtio-pci-common-cfg-queue-used device)
          (+ address
           (sup::align-up
            (+ (* queue-size virtio:+virtio-ring-desc-size+)
               4
               (* queue-size 2))
            4096))))
  address)

(defun virtio-pci-transport-queue-size (device)
  (virtio-pci-common-cfg-queue-size device))

(defun virtio-pci-transport-enable-queue (device queue)
  (setf (virtio-pci-common-cfg-queue-select device) queue)
  (setf (virtio-pci-common-cfg-queue-enable device) 1))

(defun (setf virtio-pci-transport-queue-select) (queue device)
  (setf (virtio-pci-common-cfg-queue-select device) queue))

;;;; Legacy PCI transport.

(virtio:define-virtio-transport virtio-legacy-pci-transport)

(defstruct (virtio-legacy-pci-device
             (:include virtio:virtio-device)
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
  (accessor virtio-legacy-pci-transport-device-features +virtio-legacy-pci-device-features+ pci:pci-io-region/32)
  (accessor virtio-legacy-pci-transport-guest-features  +virtio-legacy-pci-guest-features+  pci:pci-io-region/32)
  (accessor virtio-legacy-pci-transport-queue-pfn       +virtio-legacy-pci-queue-address+   pci:pci-io-region/32)
  (accessor virtio-legacy-pci-transport-queue-size      +virtio-legacy-pci-queue-size+      pci:pci-io-region/16)
  (accessor virtio-legacy-pci-transport-queue-select    +virtio-legacy-pci-queue-select+    pci:pci-io-region/16)
  (accessor virtio-legacy-pci-transport-queue-notify    +virtio-legacy-pci-queue-notify+    pci:pci-io-region/16)
  (accessor virtio-legacy-pci-transport-device-status   +virtio-legacy-pci-device-status+   pci:pci-io-region/8)
  (accessor virtio-legacy-pci-transport-isr-status      +virtio-legacy-pci-isr-status+      pci:pci-io-region/8))

(defun virtio-legacy-pci-transport-device-feature (device bit)
  (cond ((< bit 32)
         (logbitp bit (virtio-legacy-pci-transport-device-features device)))
        (t nil)))

(defun virtio-legacy-pci-transport-driver-feature (device bit)
  (cond ((< bit 32)
         (logbitp bit (virtio-legacy-pci-transport-guest-features device)))
        (t nil)))

(defun (setf virtio-legacy-pci-transport-driver-feature) (value device bit)
  (cond ((< bit 32)
         (setf (ldb (byte 1 bit) (virtio-legacy-pci-transport-guest-features device))
               (if value 1 0))
         value)
        (t (error "feature bit ~D out of range" bit))))

(defun virtio-legacy-pci-transport-device-specific-header/8 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci:pci-io-region/8 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)))

(defun (setf virtio-legacy-pci-transport-specific-header/8) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci:pci-io-region/8 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)) value))

(defun virtio-legacy-pci-transport-specific-header/16 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci:pci-io-region/16 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)))

(defun (setf virtio-legacy-pci-transport-specific-header/16) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci:pci-io-region/16 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)) value))

(defun virtio-legacy-pci-transport-specific-header/32 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (pci:pci-io-region/32 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)))

(defun (setf virtio-legacy-pci-transport-specific-header/32) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required."
  (setf (pci:pci-io-region/32 (virtio-legacy-pci-device-header device) (+ +virtio-legacy-pci-device-specific+ offset)) value))

(defun virtio-legacy-pci-transport-kick (dev vq-id)
  "Notify the device that new buffers have been added to VQ-ID."
  (setf (virtio-legacy-pci-transport-queue-notify dev) vq-id))

(defun virtio-legacy-pci-transport-device-irq (device)
  (pci:pci-intr-line (virtio-legacy-pci-device-pci-device device)))

(defun virtio-legacy-pci-transport-ack-irq (device status)
  (declare (ignore device status))
  nil)

(defun virtio-legacy-pci-transport-queue-address (device)
  (* (virtio-legacy-pci-transport-queue-pfn device) #x1000))

(defun (setf virtio-legacy-pci-transport-queue-address) (address device)
  (setf (virtio-legacy-pci-transport-queue-pfn device) (truncate address #x1000))
  address)

(defun virtio-legacy-pci-transport-enable-queue (device queue)
  (declare (ignore device queue))
  nil)

(defun pci::virtio-pci-register (location)
  (cond ((<= #x1000 (pci:pci-config/16 location pci:+pci-config-deviceid+) #x103F)
         ;; This is a legacy or transitional device
         ;; TODO: Operate transitional devices in normal mode.
         ;; TODO: Test the VIRTIO_F_VERSION_1 feature.
         (let* ((legacy-header (pci:pci-bar location 0))
                (dev (make-virtio-legacy-pci-device
                      :pci-device location
                      :header legacy-header
                      :transport 'virtio-legacy-pci-transport
                      :did (pci:pci-config/16 location pci:+pci-config-subdeviceid+)
                      :boot-id (pci:pci-device-boot-id location))))
           ;; Enable PCI bus master bit, just in case it wasn't set and
           ;; the emulator is really picky.
           (setf (pci:pci-bus-master-enabled location) t)
           (virtio:virtio-device-register dev)))
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
                      :did (- (pci:pci-config/16 location pci:+pci-config-deviceid+) #x1040)
                      :boot-id (pci:pci-device-boot-id location))))
           (dump-virtio-pci-device dev)
           ;; Enable PCI bus master bit, just in case it wasn't set and
           ;; the emulator is really picky.
           (setf (pci:pci-bus-master-enabled location) t)
           (virtio:virtio-device-register dev)))))

(defun virtio-pci-read-notify-off-multiplier (pci-device)
  "Read the NOTIFY-OFF-MULTIPLIER from PCI config space."
  (let ((cap (pci:pci-get-vendor-capability pci-device +virtio-pci-cap-notify-cfg+)))
    (pci:pci-config/32 pci-device (+ cap +virtio-pci-capability-notify-off-multiplier+))))
