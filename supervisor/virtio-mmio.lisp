;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; MMIO transport for virtio devices.

(in-package :mezzano.supervisor)

(defmacro define-virtio-mmio-register (name index)
  (let ((accessor (intern (format nil "VIRTIO-MMIO-~A" name)
                          (symbol-package name))))
    `(progn
       (defun ,accessor (device)
         (physical-memref-unsigned-byte-32 (+ (virtio-device-mmio device)
                                              ',index)))
       (defun (setf ,accessor) (value device)
         (setf (physical-memref-unsigned-byte-32 (+ (virtio-device-mmio device)
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

(defun virtio-mmio-device-specific-header/8 (device offset)
  (physical-memref-unsigned-byte-8 (+ (virtio-device-mmio device)
                                      +virtio-mmio-config0+
                                      offset)))

(defun (setf virtio-mmio-device-specific-header/8) (value device offset)
  (setf (physical-memref-unsigned-byte-8 (+ (virtio-device-mmio device)
                                            +virtio-mmio-config0+
                                            offset))
        value))

(defun virtio-mmio-device-specific-header/16 (device offset)
  (physical-memref-unsigned-byte-16 (+ (virtio-device-mmio device)
                                       +virtio-mmio-config0+
                                       offset)))

(defun (setf virtio-mmio-device-specific-header/16) (value device offset)
  (setf (physical-memref-unsigned-byte-16 (+ (virtio-device-mmio device)
                                             +virtio-mmio-config0+
                                             offset))
        value))

(defun virtio-mmio-device-specific-header/32 (device offset)
  (physical-memref-unsigned-byte-32 (+ (virtio-device-mmio device)
                                       +virtio-mmio-config0+
                                       offset)))

(defun (setf virtio-mmio-device-specific-header/32) (value device offset)
  (setf (physical-memref-unsigned-byte-32 (+ (virtio-device-mmio device)
                                             +virtio-mmio-config0+
                                             offset))
        value))

(defun virtio-mmio-register (address irq)
  (let* ((dev (make-virtio-device :mmio address
                                  :mmio-irq irq
                                  :boot-id (current-boot-id)))
         (magic (virtio-mmio-magic dev))
         (version (virtio-mmio-version dev))
         (did (virtio-mmio-device-id dev))
         (vid (virtio-mmio-vendor-id dev)))
    (setf (virtio-device-did dev) did)
    (when (not (and (eql magic +virtio-mmio-magic-value+)
                    (eql version 1)
                    (not (eql did +virtio-dev-id-invalid+))))
      (return-from virtio-mmio-register nil))
    (debug-print-line "mmio virtio device at " address " did: " did " vid: " vid)
    (virtio-device-register dev did)))

(defun virtio-mmio-kick (dev vq-id)
  "Notify the device that new buffers have been added to VQ-ID."
  (setf (virtio-mmio-queue-notify dev) vq-id))

(defun virtio-mmio-configure-virtqueues (device n-queues)
  (setf (virtio-device-virtqueues device) (sys.int::make-simple-vector n-queues :wired))
  (dotimes (queue n-queues)
    ;; 1. Write the virtqueue index to the queue select field.
    (setf (virtio-mmio-queue-sel device) queue)
    ;; Read the virtqueue size from the queue size field.
    (let* ((queue-size (virtio-mmio-queue-num-max device))
           (size (virtio-ring-size queue-size)))
      (debug-print-line "Virtqueue " queue " has size " queue-size ". Computed size is " size)
      (when (not (zerop queue-size))
        ;; Allocate and clear the virtqueue.
        ;; Must be 4k aligned and contiguous in physical memory.
        (let* ((frame (or (allocate-physical-pages (ceiling size +4k-page-size+))
                          (progn (debug-print-line "Virtqueue allocation failed")
                                 (return-from virtio-mmio-configure-virtqueues nil))))
               (phys (* frame +4k-page-size+))
               (virt (convert-to-pmap-address phys)))
          (debug-print-line "Virtqueue allocated at " phys " (" (ceiling size +4k-page-size+) ")")
          (setf (virtio-mmio-queue-num device) queue-size)
          (dotimes (i size)
            (setf (sys.int::memref-unsigned-byte-8 virt i) 0))
          ;; Write the address to the the queue address field.
          ;; This is a page number, not an actual address.
          (setf (virtio-mmio-guest-page-size device) +4k-page-size+)
          (setf (virtio-mmio-queue-pfn device) frame)
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

(defun virtio-mmio-device-irq (device)
  (virtio-device-mmio-irq device))
