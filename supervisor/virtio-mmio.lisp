;;;; MMIO transport for virtio devices.

(defpackage :mezzano.supervisor.virtio-mmio-transport
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:virtio :mezzano.supervisor.virtio)
                    (:sys.int :mezzano.internals)))

(in-package :mezzano.supervisor.virtio-mmio-transport)

(virtio:define-virtio-transport virtio-legacy-mmio-transport)

(defstruct (virtio-legacy-mmio-device
             (:include virtio:virtio-device)
             (:area :wired))
  mmio
  mmio-irq)

(defmacro define-virtio-mmio-register (name index)
  (let ((accessor (intern (format nil "VIRTIO-MMIO-~A" name)
                          (symbol-package name))))
    `(progn
       (defun ,accessor (device)
         (sup::physical-memref-unsigned-byte-32 (+ (virtio-legacy-mmio-device-mmio device)
                                                   ',index)))
       (defun (setf ,accessor) (value device)
         (setf (sup::physical-memref-unsigned-byte-32 (+ (virtio-legacy-mmio-device-mmio device)
                                                         ',index))
               value)))))

;;; Registers in the MMIO virtio header.
(define-virtio-mmio-register magic              #x00)
(define-virtio-mmio-register version            #x04)
(define-virtio-mmio-register device-id          #x08)
(define-virtio-mmio-register vendor-id          #x0C)
(define-virtio-mmio-register host-features      #x10)
(define-virtio-mmio-register host-features-sel  #x14)
(define-virtio-mmio-register guest-features     #x20)
(define-virtio-mmio-register guest-features-sel #x24)
(define-virtio-mmio-register guest-page-size    #x28)
(define-virtio-mmio-register queue-sel          #x30)
(define-virtio-mmio-register queue-num-max      #x34)
(define-virtio-mmio-register queue-num          #x38)
(define-virtio-mmio-register queue-align        #x3C)
(define-virtio-mmio-register queue-pfn          #x40)
(define-virtio-mmio-register queue-notify       #x50)
(define-virtio-mmio-register interrupt-status   #x60)
(define-virtio-mmio-register interrupt-ack      #x64)
(define-virtio-mmio-register status             #x70)
(defconstant +virtio-mmio-config0+            #x100)

(defconstant +virtio-mmio-magic-value+ #x74726976)

(defun virtio-legacy-mmio-transport-device-feature (device bit)
  (setf (virtio-mmio-host-features-sel device) (truncate bit 32))
  (logbitp (rem bit 32) (virtio-mmio-host-features device)))

(defun virtio-legacy-mmio-transport-driver-feature (device bit)
  (setf (virtio-mmio-guest-features-sel device) (truncate bit 32))
  (logbitp (rem bit 32) (virtio-mmio-guest-features device)))

(defun (setf virtio-legacy-mmio-transport-driver-feature) (value device bit)
  (setf (virtio-mmio-guest-features-sel device) (truncate bit 32))
  (setf (ldb (byte 1 (rem bit 32)) (virtio-mmio-guest-features device))
        (if value 1 0))
  value)

(defun virtio-legacy-mmio-transport-device-specific-header/8 (device offset)
  (sup::physical-memref-unsigned-byte-8 (+ (virtio-legacy-mmio-device-mmio device)
                                           +virtio-mmio-config0+
                                           offset)))

(defun (setf virtio-legacy-mmio-transport-device-specific-header/8) (value device offset)
  (setf (sup::physical-memref-unsigned-byte-8 (+ (virtio-legacy-mmio-device-mmio device)
                                                 +virtio-mmio-config0+
                                                 offset))
        value))

(defun virtio-legacy-mmio-transport-device-specific-header/16 (device offset)
  (sup::physical-memref-unsigned-byte-16 (+ (virtio-legacy-mmio-device-mmio device)
                                            +virtio-mmio-config0+
                                            offset)))

(defun (setf virtio-legacy-mmio-transport-device-specific-header/16) (value device offset)
  (setf (sup::physical-memref-unsigned-byte-16 (+ (virtio-legacy-mmio-device-mmio device)
                                                  +virtio-mmio-config0+
                                                  offset))
        value))

(defun virtio-legacy-mmio-transport-device-specific-header/32 (device offset)
  (sup::physical-memref-unsigned-byte-32 (+ (virtio-legacy-mmio-device-mmio device)
                                            +virtio-mmio-config0+
                                            offset)))

(defun (setf virtio-legacy-mmio-transport-device-specific-header/32) (value device offset)
  (setf (sup::physical-memref-unsigned-byte-32 (+ (virtio-legacy-mmio-device-mmio device)
                                                  +virtio-mmio-config0+
                                                  offset))
        value))

(defun virtio-legacy-mmio-transport-isr-status (device)
  (virtio-mmio-interrupt-status device))

(defun virtio-legacy-mmio-transport-ack-irq (device status)
  (setf (virtio-mmio-interrupt-ack device) status))

(defun virtio-legacy-mmio-transport-device-status (device)
  (virtio-mmio-status device))

(defun (setf virtio-legacy-mmio-transport-device-status) (value device)
  (setf (virtio-mmio-status device) value))

(defun virtio-legacy-mmio-transport-device-irq (device)
  (virtio-legacy-mmio-device-mmio-irq device))

(defun virtio-mmio-register (address irq)
  (let* ((dev (make-virtio-legacy-mmio-device
               :mmio address
               :mmio-irq irq
               :transport 'virtio-legacy-mmio-transport
               :boot-id (sup:current-boot-id)))
         (magic (virtio-mmio-magic dev))
         (version (virtio-mmio-version dev))
         (did (virtio-mmio-device-id dev))
         (vid (virtio-mmio-vendor-id dev)))
    (setf (virtio:virtio-device-did dev) did)
    (when (not (and (eql magic +virtio-mmio-magic-value+)
                    (eql version 1)
                    (not (eql did virtio:+virtio-dev-id-invalid+))))
      (return-from virtio-mmio-register nil))
    (sup:debug-print-line "mmio virtio device at " address " did: " did " vid: " vid)
    (virtio:virtio-device-register dev)))

(defun sup::virtio-mmio-fdt-register (fdt-node address-cells size-cells)
  (declare (ignore size-cells))
  (let* ((reg (sup::fdt-get-property fdt-node "reg"))
         (address (sup::fdt-read-integer reg address-cells 0))
         (interrupts (sup::fdt-get-property fdt-node "interrupts"))
         (irq (sup::fdt-read-integer interrupts 1 1)))
    ;; FIXME: IRQ routing.
    (virtio-mmio-register address (+ 32 irq))))

(defun virtio-legacy-mmio-transport-kick (dev vq-id)
  "Notify the device that new buffers have been added to VQ-ID."
  (setf (virtio-mmio-queue-notify dev) vq-id))

(defun virtio-legacy-mmio-transport-queue-select (device)
  (virtio-mmio-queue-sel device))

(defun (setf virtio-legacy-mmio-transport-queue-select) (queue device)
  (setf (virtio-mmio-queue-sel device) queue))

(defun virtio-legacy-mmio-transport-queue-size (device)
  (virtio-mmio-queue-num-max device))

(defun virtio-legacy-mmio-transport-queue-address (device)
  (* (virtio-mmio-queue-pfn device) sup::+4k-page-size+))

(defun (setf virtio-legacy-mmio-transport-queue-address) (address device)
  ;; This is a page number, not an actual address.
  (setf (virtio-mmio-guest-page-size device) sup::+4k-page-size+)
  (setf (virtio-mmio-queue-pfn device) (truncate address sup::+4k-page-size+)))

(defun virtio-legacy-mmio-transport-enable-queue (device queue)
  (declare (ignore device queue))
  nil)

(defun virtio-mmio-device-irq (device)
  (virtio-legacy-mmio-device-mmio-irq device))
