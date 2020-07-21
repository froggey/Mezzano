;;;; Generic Virtio device support.

(defpackage :mezzano.supervisor.virtio
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sys.int :mezzano.internals))
  (:export #:+virtio-dev-id-invalid+
           #:+virtio-dev-id-net+
           #:+virtio-dev-id-block+
           #:+virtio-dev-id-console+
           #:+virtio-dev-id-entropy-src+
           #:+virtio-dev-id-mem-balloon+
           #:+virtio-dev-id-io-memory+
           #:+virtio-dev-id-rpmsg+
           #:+virtio-dev-id-scsi-host+
           #:+virtio-dev-id-9p-transport+
           #:+virtio-dev-id-mac80211-wlan+
           #:+virtio-dev-id-rproc-serial+
           #:+virtio-dev-id-caif+
           #:+virtio-dev-id-gpu+
           #:+virtio-dev-id-input+

           #:+virtio-status-reset+
           #:+virtio-status-acknowledge+
           #:+virtio-status-driver+
           #:+virtio-status-ok+
           #:+virtio-status-failed+

           #:+virtio-ring-desc-size+
           #:+virtio-ring-desc-f-next+
           #:+virtio-ring-desc-f-write+
           #:+virtio-ring-desc-f-indirect+

           #:define-virtio-transport
           #:virtio-device
           #:virtqueue
           #:virtio-ring-size
           #:virtio-virtqueue
           #:virtio-device-virtqueues
           #:virtio-device-boot-id
           #:virtio-device-did
           #:virtio-device-register
           #:make-virtqueue
           #:virtqueue-index
           #:virtqueue-size
           #:virtqueue-avail-offset
           #:virtqueue-used-offset
           #:virtqueue-next-free-descriptor
           #:virtqueue-last-seen-used
           #:virtio-device-specific-header/8
           #:virtio-device-specific-header/16
           #:virtio-device-specific-header/32
           #:virtio-device-specific-header/64
           #:virtio-ring-desc-address
           #:virtio-ring-desc-length
           #:virtio-ring-desc-flags
           #:virtio-ring-desc-next
           #:virtio-ring-avail-flags
           #:virtio-ring-avail-idx
           #:virtio-ring-avail-ring
           #:virtio-ring-used-flags
           #:virtio-ring-used-idx
           #:virtio-ring-used-elem-id
           #:virtio-ring-used-elem-len
           #:virtio-ring-alloc-descriptor
           #:virtio-ring-free-descriptor
           #:virtio-ring-add-to-avail-ring
           #:virtio-pop-used-ring
           #:virtio-kick
           #:virtio-ring-disable-interrupts
           #:virtio-ring-enable-interrupts
           #:virtio-device-status
           #:virtio-device-feature
           #:virtio-driver-feature
           #:virtio-isr-status
           #:virtio-device-irq
           #:virtio-attach-irq
           #:virtio-ack-irq
           #:virtio-queue-select
           #:virtio-queue-size
           #:virtio-queue-address
           #:virtio-enable-queue
           #:virtio-irq-mask
           #:virtio-configure-virtqueues
           #:virtio-driver-detached
           #:define-virtio-driver))

(in-package :mezzano.supervisor.virtio)

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
(defconstant +virtio-status-device-needs-reset+ #x40
  "The device has experienced an error from which it can't recover.")

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

(defmacro define-virtio-transport (name)
  (let ((transport-functions '(device-specific-header/8
                               (setf-device-specific-header/8 device-specific-header/8)
                               device-specific-header/16
                               (setf-device-specific-header/16 device-specific-header/16)
                               device-specific-header/32
                               (setf-device-specific-header/32 device-specific-header/32)
                               kick
                               device-status
                               (setf-device-status device-status)
                               device-feature
                               driver-feature
                               (setf-driver-feature driver-feature)
                               isr-status
                               device-irq
                               ack-irq
                               queue-select
                               (setf-queue-select queue-select)
                               queue-size
                               (setf-queue-size queue-size)
                               queue-address
                               (setf-queue-address queue-address)
                               enable-queue)))
    `(defun ,name (function)
       (ecase function
         ,@(loop
              for fn in transport-functions
              collect (if (consp fn) ; setf function.
                          (list (first fn) `#'(setf ,(intern (format nil "~A-~A" name (second fn)))))
                          (list fn `',(intern (format nil "~A-~A" name fn)))))))))

(defstruct (virtio-device
             (:area :wired))
  transport
  virtqueues
  did
  claimed
  boot-id)

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
  (+ (sup::align-up (+ (* +virtio-ring-desc-size+ queue-size) (* 2 (+ 2 queue-size))) 4096)
     (sup::align-up (+ (* 2 3) (* +virtio-ring-used-elem-size+ queue-size)) 4096)))

(defun virtio-virtqueue (device virtqueue)
  (svref (virtio-device-virtqueues device) virtqueue))

(defmacro define-virtio-transport-function (name lambda-list &optional docstring)
  "Define a new transport function. The lambda list must contain a DEVICE entry."
  `(defun ,(if (consp name)
               `(setf ,(intern (format nil "VIRTIO-~A" (second name))))
               (intern (format nil "VIRTIO-~A" name)))
       ,lambda-list
     ,docstring
     (funcall (funcall (virtio-device-transport device)
                       ',(if (consp name)
                             (intern (format nil "SETF-~A" (second name)))
                             name))
              ,@lambda-list)))

(define-virtio-transport-function device-specific-header/8 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required.")
(define-virtio-transport-function (setf device-specific-header/8) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required.")
(define-virtio-transport-function device-specific-header/16 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required.")
(define-virtio-transport-function (setf device-specific-header/16) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required.")
(define-virtio-transport-function device-specific-header/32 (device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required.")
(define-virtio-transport-function (setf device-specific-header/32) (value device offset)
  "Access the device-specific portion of the header, skpping the MSI-X fields if required.")

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
                  #+(or)(sup:debug-print-line "read field " ',name " idx " idx " @ " (+ (virtqueue-virtual vq) (* idx +virtio-ring-desc-size+) ,offset))
                  (assert (< idx (virtqueue-size vq)))
                  (,accessor (+ (virtqueue-virtual vq) (* idx +virtio-ring-desc-size+) ,offset) 0))
                (defun (setf ,name) (value vq idx)
                  #+(or)(sup:debug-print-line "write field " ',name " idx " idx " @ " (+ (virtqueue-virtual vq) (* idx +virtio-ring-desc-size+) ,offset) " value " value)
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

(defun virtio-ring-free-descriptor (vq id)
  (let ((next (virtqueue-next-free-descriptor vq)))
    (cond (next
           (setf (virtio-ring-desc-flags vq id) (ash 1 +virtio-ring-desc-f-next+)
                 (virtio-ring-desc-next vq id) next))
          (t
           (setf (virtio-ring-desc-flags vq id) 0)))
    (setf (virtqueue-next-free-descriptor vq) id)))

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

(defun virtio-pop-used-ring (vq)
  (cond ((eql (virtio-ring-used-idx vq)
              (virtqueue-last-seen-used vq))
         nil)
        (t
         (let* ((ring-entry (rem (virtqueue-last-seen-used vq)
                                 (virtqueue-size vq)))
                (desc (virtio-ring-used-elem-id vq ring-entry)))
           (setf (virtqueue-last-seen-used vq)
                 (ldb (byte 16 0) (1+ (virtqueue-last-seen-used vq))))
           desc))))

(define-virtio-transport-function kick (device vq-id)
  "Notify the device that new buffers have been added to VQ-ID.")

(defun virtio-ring-disable-interrupts (vq)
  (setf (virtio-ring-avail-flags vq) (logior (virtio-ring-avail-flags vq)
                                             (ash 1 +virtio-ring-avail-f-no-interrupt+))))

(defun virtio-ring-enable-interrupts (vq)
  (setf (virtio-ring-avail-flags vq) (logand (virtio-ring-avail-flags vq)
                                             (lognot (ash 1 +virtio-ring-avail-f-no-interrupt+)))))

(define-virtio-transport-function device-status (device))
(define-virtio-transport-function (setf device-status) (value device))

;; Currently no lock required here, this is only modified at boot time
;; during device detection.
(sys.int::defglobal *virtio-devices*)
(sys.int::defglobal *virtio-late-probe-devices*)

(defun virtio-device-register (dev)
  (sup::push-wired dev *virtio-devices*)
  ;; Reset device.
  (setf (virtio-device-status dev) +virtio-status-reset+)
  ;; Acknowledge the device.
  (setf (virtio-device-status dev) +virtio-status-acknowledge+)
  (case (virtio-device-did dev)
    (#.+virtio-dev-id-block+
     (virtio-block-register dev)
     (setf (virtio-device-claimed dev) :block))
    (#.+virtio-dev-id-gpu+
     (virtio-gpu-register dev)
     (setf (virtio-device-claimed dev) :gpu))
    (#.+virtio-dev-id-input+
     (virtio-input-register dev)
     (setf (virtio-device-claimed dev) :input))
    (t
     (sup::push-wired dev *virtio-late-probe-devices*))))

;; These devices must be probed late because their drivers may not be in wired memory.
(defun virtio-late-probe ()
  (dolist (dev *virtio-late-probe-devices*)
    (dolist (drv *virtio-drivers*
             (progn
               (sup:debug-print-line "Unknown virtio device type " (virtio-device-did dev))
               (setf (virtio-device-status dev) +virtio-status-failed+)))
      (when (and (eql (virtio-device-did dev) (virtio-driver-dev-id drv))
                 (sys.int::log-and-ignore-errors
                  (funcall (virtio-driver-probe drv) dev)))
        (setf (virtio-device-claimed dev) drv)
        (return)))))

(define-virtio-transport-function device-feature (device bit))
(define-virtio-transport-function driver-feature (device bit))
(define-virtio-transport-function (setf driver-feature) (value device bit))
(define-virtio-transport-function isr-status (device))
(define-virtio-transport-function device-irq (device))
(define-virtio-transport-function ack-irq (device status))
(define-virtio-transport-function queue-select (device))
(define-virtio-transport-function (setf queue-select) (queue device))
(define-virtio-transport-function queue-size (device))
(define-virtio-transport-function queue-address (device))
(define-virtio-transport-function (setf queue-address) (address device))
(define-virtio-transport-function enable-queue (device queue))

(defun virtio-configure-1-virtqueue (device queue)
  ;; Select this queue.
  (setf (virtio-queue-select device) queue)
  ;; Read the virtqueue size from the queue size field,
  ;; and calculate the total ring size.
  (let* ((queue-size (virtio-queue-size device))
         (size (virtio-ring-size queue-size)))
    (sup:debug-print-line "Virtqueue " queue " has size " queue-size ". Computed size is " size)
    (when (zerop queue-size)
      (setf (svref (virtio-device-virtqueues device) queue) nil)
      (return-from virtio-configure-1-virtqueue nil))
    ;; Allocate and clear the virtqueue.
    ;; Must be 4k aligned and contiguous in physical memory.
    (let* ((frame (or (sup::allocate-physical-pages (ceiling size sup::+4k-page-size+))
                      (progn (sup:debug-print-line "Virtqueue allocation failed")
                             (return-from virtio-configure-1-virtqueue nil))))
           (phys (* frame sup::+4k-page-size+))
           (virt (sup::convert-to-pmap-address phys)))
      (sup:debug-print-line "Virtqueue allocated at " phys " (" (ceiling size sup::+4k-page-size+) ")")
      (dotimes (i size)
        (setf (sys.int::memref-unsigned-byte-8 virt i) 0))
      ;; Write the address to the the queue address field.
      (setf (virtio-queue-address device) phys)
      (let ((vq (make-virtqueue :index queue
                                :virtual virt
                                :physical phys
                                :size queue-size
                                :avail-offset (* queue-size +virtio-ring-desc-size+)
                                :used-offset (sup::align-up (+ (* queue-size +virtio-ring-desc-size+)
                                                               4
                                                               (* queue-size 2))
                                                            4096)
                                :last-seen-used 0)))
        (setf (svref (virtio-device-virtqueues device) queue) vq)
        ;; Initialize the free descriptor list.
        (dotimes (i (1- queue-size))
          (setf (virtio-ring-desc-next vq i) (1+ i)
                (virtio-ring-desc-flags vq i) (ash 1 +virtio-ring-desc-f-next+)))
        (setf (virtqueue-next-free-descriptor vq) 0)))))

(defun virtio-deconfigure-1-virtqueue (device queue)
  "Release all system resources associated with QUEUE."
  (let ((vq (svref (virtio-device-virtqueues device) queue)))
    (when vq
      (let* ((queue-size (virtqueue-size vq))
             (size (virtio-ring-size queue-size)))
        (sup::release-physical-pages (truncate (virtqueue-physical vq) sup::+4k-page-size+)
                                     (ceiling size sup::+4k-page-size+))))))

(defun virtio-configure-virtqueues (device n-queues)
  (setf (virtio-device-virtqueues device)
        (sys.int::make-simple-vector n-queues :wired))
  (dotimes (queue n-queues)
    (when (not (virtio-configure-1-virtqueue device queue))
      ;; Configuration of this queue failed.
      ;; Release any allocated memory for earlier queues and bail out.
      (dotimes (configured-queue (1- queue))
        (virtio-deconfigure-1-virtqueue device configured-queue))
      (setf (virtio-device-status device) +virtio-status-failed+)
      (return-from virtio-configure-virtqueues nil)))
  (dotimes (queue n-queues)
    (virtio-enable-queue device queue))
  t)

(defun virtio-attach-irq (device handler)
  (declare (mezzano.compiler::closure-allocation :wired))
  (sup:irq-attach (sup:platform-irq (virtio-device-irq device))
                  (lambda (interrupt-frame irq)
                    (let ((status (virtio-isr-status device)))
                      (when (logbitp 0 status)
                        (funcall handler interrupt-frame irq))
                      (virtio-ack-irq device status))
                    :completed)
                  device))

;; FIXME: Access to this needs to be protected.
;; I'm not sure a mutex will cut it, it needs to be accessible
;; during boot-time device probing.
(sys.int::defglobal *virtio-drivers* '())

(defstruct (virtio-driver
             (:area :wired))
  name
  probe
  dev-id)

(defmacro define-virtio-driver (name probe-function dev-id)
  `(register-virtio-driver ',name ',probe-function ,dev-id))

(defun register-virtio-driver (name probe-function dev-id)
  (dolist (drv *virtio-drivers*)
    (when (eql (virtio-driver-name drv) name)
      (when (not (and (eql (virtio-driver-probe drv) probe-function)
                      (eql (virtio-driver-dev-id drv) dev-id)))
        (error "Incompatible redefinition of virtio driver ~S." name))
      ;; TODO: Maybe detach current driver and reprobe?
      (return-from register-virtio-driver name)))
  (let ((driver (make-virtio-driver
                 :name name
                 :probe probe-function
                 :dev-id dev-id)))
    (sup:debug-print-line "Registered new virtio driver " name " for device-id " dev-id)
    (sup::push-wired driver *virtio-drivers*)
    ;; Probe devices.
    (dolist (dev *virtio-devices*)
      (when (and (not (virtio-device-claimed dev))
                 (eql (virtio-device-did dev) dev-id)
                 (sys.int::log-and-ignore-errors
                  (funcall probe-function dev)))
        (setf (virtio-device-claimed dev) driver)))
    name))

(defun sup::initialize-virtio ()
  (when (not (boundp '*virtio-drivers*))
    (setf *virtio-drivers* '()))
  ;; TODO: This should notify drivers that devices are gone.
  (setf *virtio-devices* '()
        *virtio-late-probe-devices* '())
  (sup::add-deferred-boot-action 'virtio-late-probe))

(defun virtio-driver-detached (device)
  "Call when a driver is done with a device."
  (setf (virtio-device-claimed device) nil)
  (sup:with-device-access ((virtio-device-boot-id device) nil)
    (setf (virtio-device-status device) +virtio-status-failed+)
    ;; TODO: Maybe reprobe the device?
    ;; Free the virtqueues.
    (dotimes (queue (length (virtio-device-virtqueues device)))
      (virtio-deconfigure-1-virtqueue device queue))))
