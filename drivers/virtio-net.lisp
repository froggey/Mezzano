;;;; A driver for the virtio-net network device.

(defpackage :virtio-net
  (:use :cl)
  (:import-from :sys.net
                :ub16ref/le
                :ub32ref/le
                :ub64ref/le))

(in-package :virtio-net)

;;; IO offsets.
(defconstant +device-features-low+ 0
  "Low bits of device features, 32 bits.")
(defconstant +guest-features-low+ 4
  "Low bits of guest features, 32 bits.")
(defconstant +queue-address+ 8
  "Queue address, 32 bits.")
(defconstant +queue-size+ 12
  "Queue size, 16 bits.")
(defconstant +queue-select+ 14
  "Queue select, 16 bits.")
(defconstant +queue-notify+ 16
  "Queue notify, 16 bits.")
(defconstant +device-status+ 18
  "Device status, 8 bits.")
(defconstant +ISR-status+ 19
  "ISR status, 8 bits.")
;;; There are more registers after this, but their offsets
;;; are not fixed and depend on feature bits.

;;; Subsystem IDs
(defconstant +virtio-subsystem-network-device+ 1)

;;; Device-independent feature bits
(defconstant +f-notify-on-empty+ 24
  "Can interrupt when the device runs out of available descriptors on a virtqueue.")
(defconstant +f-ring-indirect-desc+ 28
  "The device supports indirect descriptors.")
(defconstant +f-ring-event-idx+ 29
  "Enables the used_event and avail_event fields.")
(defconstant +f-bad-feature+ 30
  "Set by buggy drivers.")
(defconstant +f-features-high+ 31
  "The device supports the high feature bits.")

;;; Network device feature bits.
(defconstant +net-f-csum+ 0
  "Device handles packets with partial checksum.")
(defconstant +net-f-guest-csum+ 1
  "Guest handles packets with partial checksum.")
(defconstant +net-f-mac+ 5
  "Device has given MAC address.")
(defconstant +net-f-gso+ 6
  "Device handles packets with any GSO type. (Deprecated)")
(defconstant +net-f-guest-tso4+ 7
  "Guest can receive TSOv4.")
(defconstant +net-f-guest-tso6+ 8
  "Guest can receive TSOv6.")
(defconstant +net-f-guest-ecn+ 9
  "Guest can receive TSO with ECN.")
(defconstant +net-f-guest-ufo+ 10
  "Guest can receive UFO.")
(defconstant +net-f-host-tso4+ 11
  "Device can receive TSOv4.")
(defconstant +net-f-host-tso6+ 12
  "Device can receive TSOv6.")
(defconstant +net-f-host-ecn+ 13
  "Device can receive TSO with ECN.")
(defconstant +net-f-host-ufo+ 14
  "Device can receive UFO.")
(defconstant +net-f-mrg-rxbuf+ 15
  "Guest can merge receive buffers.")
(defconstant +net-f-status+ 16
  "Configuration status field is available.")
(defconstant +net-f-ctrl-vq+ 17
  "Control channel is available.")
(defconstant +net-f-ctrl-rx+ 18
  "Control channel RX mode support.")
(defconstant +net-f-ctrl-vlan+ 19
  "Control channel VLAN filtering.")

;;; Bits for the network device status field.
(defconstant +net-s-link-up+ 1)

;; Bits for the device status field.
(defconstant +config-s-acknowledge+ 0
  "The guest has found the device and recognized it as a valid virtio device.")
(defconstant +config-s-driver+ 1
  "The guest knows how to drive the device.")
(defconstant +config-s-driver-ok+ 2
  "The driver is set up and ready to drive the device.")
(defconstant +config-s-failed+ 7
 "The guest has given up on the device.")

(defconstant +vring-desc-size+ (+ 8 4 2 2))
(defconstant +vring-used-elem-size+ (+ 4 4))

(defconstant +vring-desc-f-next+ 0
  "Buffer continues via the next field.")
(defconstant +vring-desc-f-write+ 1
  "Buffer is write-only (otherwise read-only).")
(defconstant +vring-desc-f-indirect+ 2
  "Buffer contains a list of buffer descriptors.")

(defconstant +vring-avail-f-no-interrupt+ 0
  "Interrupt not required after the device consumes the descriptor.")

(defconstant +vring-used-f-no-notify+ 0)

(defconstant +net-hdr-f-needs-csum+ 0)
(defconstant +net-hdr-gso-none+ 0)
(defconstant +net-hdr-gso-tcpv4+ 1)
(defconstant +net-hdr-gso-udp+ 3)
(defconstant +net-hdr-gso-tcpv6+ 4)
(defconstant +net-hdr-gso-ecn+ #x80)

;; Virtio devices are distinguished by their subsystem device ID,
;; not by the PCI device ID.
;; Any PCI device with Vendor ID 1AF4 and Device ID 1000 through 103F
;; inclusive is a virtio device.
(defparameter *virtio-pci-ids*
  (loop for i to #x3F
     collect (list #x1AF4 (+ #x1000 i))))

(defstruct (virtio-device (:constructor nil))
  pci-device
  mmiop
  io-base
  virtqueues)

(defstruct virtqueue
  index
  array
  physical
  size
  avail-offset
  used-offset
  (n-free-descriptors (sys.int::make-semaphore :name "Virt-Queue n-free descriptors")
                      :read-only t)
  next-free-descriptor
  last-seen-used)

(defstruct (virtio-net
             (:include virtio-device))
  (mac (make-array 6 :element-type '(unsigned-byte 8)))
  rx-physical
  rx-array
  tx-physical
  tx-array
  (n-free-tx-buffers (sys.int::make-semaphore :name "Virtio-Net free tx buffers" :area :static)
                     :read-only t)
  free-tx-buffers
  process
  (irq-semaphore (sys.int::make-semaphore :name "Virtio-Net interrupt" :area :static)
                 :read-only t))

(defmethod print-object ((object virtio-net) stream)
  (print-unreadable-object (object stream :type t)
    (format t "~/SYS.NET::FORMAT-MAC/"
            (virtio-net-mac object))))

(defmethod sys.net:ethernet-mac ((nic virtio-net))
  (virtio-net-mac nic))

(defmethod sys.net:transmit-packet ((nic virtio-net) packet-descriptor)
  (transmit-one nic packet-descriptor))

(defun make-virtio-device (constructor pci-device)
  (let ((dev (funcall constructor :pci-device pci-device))
        (bar0 (sys.int::pci-bar pci-device 0)))
    (setf (virtio-device-mmiop dev) (zerop (logand bar0 1))
          (virtio-device-io-base dev) (logand bar0 (lognot 3)))
    dev))

(macrolet ((def (name mmio-accessor io-accessor)
             `(progn
                (defun ,name (dev offset)
                  (if (virtio-device-mmiop dev)
                      (,mmio-accessor (+ (virtio-device-io-base dev) offset) 0)
                      (,io-accessor (+ (virtio-device-io-base dev) offset))))
                (defun (setf ,name) (value dev offset)
                  (if (virtio-device-mmiop dev)
                      (setf (,mmio-accessor (+ (virtio-device-io-base dev) offset) 0) value)
                      (setf (,io-accessor (+ (virtio-device-io-base dev) offset)) value))))))
  (def reg/8 sys.int::memref-unsigned-byte-8 system:io-port/8)
  (def reg/16 sys.int::memref-unsigned-byte-16 system:io-port/16)
  (def reg/32 sys.int::memref-unsigned-byte-32 system:io-port/32))

(defun virtio-reset (dev)
  "Reset a virtio device."
  ;; Clear device-status to perform a reset.
  (setf (reg/8 dev +device-status+) 0))

(defun virtio-device-features (dev)
  (reg/32 dev +device-features-low+))

(defun virtio-device-device-specific-offset (dev)
  "Return the offset of the device-specific part of the virtio header."
  ;; MSI-X and the high feature bits aren't used, so this is easy.
  (declare (ignore dev))
  20)

(defun vring-size (queue-size)
  "Compute the actual size of a vring from the queue-size field."
  (flet ((align (x)
           (logand (+ x 4095)
                   (lognot 4095))))
    (+ (align (+ (* +vring-desc-size+ queue-size) (* 2 (+ 2 queue-size))))
       (align (* +vring-used-elem-size+ queue-size)))))

(defconstant +vring-desc-address-offset+ 0)
(defconstant +vring-desc-length-offset+ 8)
(defconstant +vring-desc-flags-offset+ 12)
(defconstant +vring-desc-next-offset+ 14)

(defconstant +vring-avail-flags-offset+ 0)
(defconstant +vring-avail-idx-offset+ 2)
(defconstant +vring-avail-ring-offset+ 4)

(defconstant +vring-used-flags-offset+ 0)
(defconstant +vring-used-idx-offset+ 2)
(defconstant +vring-used-ring-offset+ 4)

(defconstant +vring-used-elem-id-offset+ 0)
(defconstant +vring-used-elem-len-offset+ 4)

;;; Accessors for the virtqueue descriptors.
;;; FIXME: virtqueues are native endian, not little endian!
(macrolet ((def (name offset accessor)
             `(progn
                (defun ,name (vq idx)
                  (assert (< idx (virtqueue-size vq)))
                  (,accessor (virtqueue-array vq) (+ (* idx +vring-desc-size+) ,offset)))
                (defun (setf ,name) (value vq idx)
                  (assert (< idx (virtqueue-size vq)))
                  (setf (,accessor (virtqueue-array vq) (+ (* idx +vring-desc-size+) ,offset)) value)))))
  (def vring-desc-address +vring-desc-address-offset+ ub64ref/le)
  (def vring-desc-length +vring-desc-length-offset+ ub32ref/le)
  (def vring-desc-flags +vring-desc-flags-offset+ ub16ref/le)
  (def vring-desc-next +vring-desc-next-offset+ ub16ref/le))

(defun vring-avail-flags (vq)
  (ub16ref/le (virtqueue-array vq) (+ (virtqueue-avail-offset vq)
                                      +vring-avail-flags-offset+)))
(defun vring-avail-idx (vq)
  (ub16ref/le (virtqueue-array vq) (+ (virtqueue-avail-offset vq)
                                      +vring-avail-idx-offset+)))
(defun (setf vring-avail-flags) (value vq)
  (setf (ub16ref/le (virtqueue-array vq) (+ (virtqueue-avail-offset vq)
                                            +vring-avail-flags-offset+))
        value))
(defun (setf vring-avail-idx) (value vq)
  (setf (ub16ref/le (virtqueue-array vq) (+ (virtqueue-avail-offset vq)
                                            +vring-avail-idx-offset+))
        value))

(defun vring-avail-ring (vq idx)
  (assert (< idx (virtqueue-size vq)))
  (ub16ref/le (virtqueue-array vq) (+ (virtqueue-avail-offset vq)
                                      +vring-avail-ring-offset+
                                      (* idx 2))))
(defun (setf vring-avail-ring) (value vq idx)
  (assert (< idx (virtqueue-size vq)))
  (setf (ub16ref/le (virtqueue-array vq) (+ (virtqueue-avail-offset vq)
                                            +vring-avail-ring-offset+
                                            (* idx 2)))
        value))

(defun vring-used-flags (vq)
  (ub16ref/le (virtqueue-array vq) (+ (virtqueue-used-offset vq)
                                      +vring-used-flags-offset+)))
(defun vring-used-idx (vq)
  (ub16ref/le (virtqueue-array vq) (+ (virtqueue-used-offset vq)
                                      +vring-used-idx-offset+)))
(defun (setf vring-used-flags) (value vq)
  (setf (ub16ref/le (virtqueue-array vq) (+ (virtqueue-used-offset vq)
                                            +vring-used-flags-offset+))
        value))
(defun (setf vring-used-idx) (value vq)
  (setf (ub16ref/le (virtqueue-array vq) (+ (virtqueue-used-offset vq)
                                            +vring-used-idx-offset+))
        value))

(defun vring-used-elem-id (vq idx)
  (assert (< idx (virtqueue-size vq)))
  (ub32ref/le (virtqueue-array vq) (+ (virtqueue-used-offset vq)
                                      +vring-used-ring-offset+
                                      (* idx +vring-used-elem-size+)
                                      +vring-used-elem-id-offset+)))
(defun vring-used-elem-len (vq idx)
  (assert (< idx (virtqueue-size vq)))
  (ub32ref/le (virtqueue-array vq) (+ (virtqueue-used-offset vq)
                                      +vring-used-ring-offset+
                                      (* idx +vring-used-elem-size+)
                                      +vring-used-elem-len-offset+)))
(defun (setf vring-used-elem-id) (value vq idx)
  (assert (< idx (virtqueue-size vq)))
  (setf (ub32ref/le (virtqueue-array vq) (+ (virtqueue-used-offset vq)
                                            +vring-used-ring-offset+
                                            (* idx +vring-used-elem-size+)
                                            +vring-used-elem-id-offset+))
        value))
(defun (setf vring-used-elem-len) (value vq idx)
  (assert (< idx (virtqueue-size vq)))
  (setf (ub32ref/le (virtqueue-array vq) (+ (virtqueue-used-offset vq)
                                            +vring-used-ring-offset+
                                            (* idx +vring-used-elem-size+)
                                            +vring-used-elem-len-offset+))
        value))

(defun virtio-configure-virtqueues (dev n-queues)
  (setf (virtio-device-virtqueues dev) (make-array n-queues
                                                   :initial-element nil))
  (dotimes (queue n-queues)
    ;; 1. Write the virtqueue index to the queue select field.
    (setf (reg/16 dev +queue-select+) queue)
    ;; Read the virtqueue size from the queue size field.
    (let* ((queue-size (reg/16 dev +queue-size+))
           (size (vring-size queue-size)))
      (format t "Virtqueue ~D has size ~D. Computed size is ~D~%" queue queue-size size)
      (when (not (zerop queue-size))
        ;; Allocate and clear the virtqueue.
        ;; Must be 4k aligned and contiguous in physical memory.
        (multiple-value-bind (array phys)
            (sys.int::allocate-dma-buffer size)
          (format t "Virtqueue allocated at ~X~%" phys)
          (fill array 0)
          ;; Write the address to the the queue address field.
          (setf (reg/32 dev +queue-address+) (truncate phys 4096))
          (let ((vq (make-virtqueue :index queue
                                    :array array
                                    :physical phys
                                    :size queue-size
                                    :avail-offset (* queue-size +vring-desc-size+)
                                    :used-offset (logand (+ (* queue-size +vring-desc-size+)
                                                            4
                                                            (* queue-size 2)
                                                            4095)
                                                         (lognot 4095))
                                    :last-seen-used 0)))
            (setf (aref (virtio-device-virtqueues dev) queue) vq)
            ;; Initialize the free descriptor list.
            (dotimes (i (1- queue-size))
              (setf (vring-desc-next vq i) (1+ i)
                    (vring-desc-flags vq i) (ash 1 +vring-desc-f-next+)))
            (setf (sys.int::semaphore-count (virtqueue-n-free-descriptors vq)) queue-size
                  (virtqueue-next-free-descriptor vq) 0)))))))

(defun vring-alloc-descriptor (vq)
  (sys.int::wait-on-semaphore (virtqueue-n-free-descriptors vq))
  (let ((id (virtqueue-next-free-descriptor vq)))
    (setf (virtqueue-next-free-descriptor vq)
          (if (logbitp +vring-desc-f-next+ (vring-desc-flags vq id))
              (vring-desc-next vq id)
              nil))
    id))

(defun vring-add-to-avail-ring (vq desc)
  "Add a descriptor to the available ring."
  ;; Update the available ring.
  (setf (vring-avail-ring vq (rem (vring-avail-idx vq)
                                  (virtqueue-size vq)))
        desc)
  ;; FIXME: memory barrier here.
  ;; Update the index field.
  (setf (vring-avail-idx vq) (ldb (byte 16 0)
                                  (1+ (vring-avail-idx vq)))))

(defun vring-add-buffer (vq buffer-list)
  "Add a buffer to the vring VQ. Doesn't kick the device."
  (let ((descriptors (loop for i below (length buffer-list)
                        collect (vring-alloc-descriptor vq))))
    (do ((desc descriptors (cdr desc))
         (buf buffer-list (cdr buf)))
        ((null desc))
      (destructuring-bind (phys len writable)
          (car buf)
        ;; Configure the descriptor.
        (setf (vring-desc-address vq (first desc)) phys
              (vring-desc-length vq (first desc)) len
              (vring-desc-flags vq (first desc)) (logior (if writable
                                                             (ash 1 +vring-desc-f-write+)
                                                             0)
                                                         (if (cdr desc)
                                                             (ash 1 +vring-desc-f-next+)
                                                             0)))
        (when (cdr desc)
          (setf (vring-desc-next vq (first desc)) (cadr desc)))))
    (vring-add-to-avail-ring vq (first descriptors))))

(defun virtio-kick (dev vq-id)
  "Notify the device that new buffers have been added to VQ-ID."
  (setf (reg/16 dev +queue-notify+) vq-id))

(defun vring-disable-interrupts (vq)
  (setf (vring-avail-flags vq) (logior (vring-avail-flags vq)
                                       (ash 1 +vring-avail-f-no-interrupt+))))
(defun vring-enable-interrupts (vq)
  (setf (vring-avail-flags vq) (logand (vring-avail-flags vq)
                                       (lognot (ash 1 +vring-avail-f-no-interrupt+)))))

(defconstant +net-receiveq+ 0
  "The RX virtqueue index.")
(defconstant +net-transmitq+ 1
  "The TX virtqueue index.")

(defconstant +virtio-net-hdr-size+ 10)
(defconstant +rx-buffer-size+ #x600
  "Size of one RX packet buffer. As large as one ethernet
packet (1514 bytes), plus a virtio-net header (10 or 12 bytes)
and then some alignment.")
(defconstant +n-rx-buffers+ 32)
(defconstant +tx-buffer-size+ #x600
  "Size of one TX packet buffer. As large as one ethernet
packet (1514 bytes), plus a virtio-net header (10 or 12 bytes)
and then some alignment.")
(defconstant +n-tx-buffers+ 32)

(defvar *cards* '())

(defun worker (dev)
  (loop
     (sys.int::wait-on-semaphore (virtio-net-irq-semaphore dev))
     #+nil(format t "Network interrupt! RX used idx: ~D  TX used idx: ~D~%"
             (vring-used-idx (aref (virtio-device-virtqueues dev) +net-receiveq+))
             (vring-used-idx (aref (virtio-device-virtqueues dev) +net-transmitq+)))
     (let ((rx-queue (aref (virtio-device-virtqueues dev) +net-receiveq+)))
       (loop
          (when (eql (vring-used-idx rx-queue)
                     (virtqueue-last-seen-used rx-queue))
            (virtio-kick dev +net-receiveq+)
            (return))
          (let* ((ring-entry (rem (virtqueue-last-seen-used rx-queue)
                                  (virtqueue-size rx-queue)))
                 (id (vring-used-elem-id rx-queue ring-entry))
                 (len (vring-used-elem-len rx-queue ring-entry)))
            #+nil(format t "RX ring entry: ~D  buffer: ~D  len ~D~%" ring-entry id len)
            ;; Cheat slightly, RX descriptor IDs can be converted directly
            ;; to offsets in the RX buffer.
            ;; Extract the packet!
            (let* ((rx-offset (+ (* (truncate id 2) +rx-buffer-size+) +virtio-net-hdr-size+))
                   (packet (subseq (virtio-net-rx-array dev) rx-offset (+ rx-offset
                                                                          (- len +virtio-net-hdr-size+)))))
              ;; Re-add the descriptor to the avail ring.
              (vring-add-to-avail-ring rx-queue id)
              ;; Dispatch!
              (sys.net:receive-packet dev packet)))
          (setf (virtqueue-last-seen-used rx-queue)
                (ldb (byte 16 0) (1+ (virtqueue-last-seen-used rx-queue))))))
     (let ((tx-queue (aref (virtio-device-virtqueues dev) +net-transmitq+)))
       (loop
          (when (eql (vring-used-idx tx-queue)
                     (virtqueue-last-seen-used tx-queue))
            (return))
          (let* ((ring-entry (rem (virtqueue-last-seen-used tx-queue)
                                  (virtqueue-size tx-queue)))
                 (id (vring-used-elem-id tx-queue ring-entry))
                 (len (vring-used-elem-len tx-queue ring-entry)))
            #+nil(format t "TX ring entry: ~D  buffer: ~D  len ~D~%" ring-entry id len)
            ;; Packet sent. Add the descriptor back to the freelist.
            (push id (virtio-net-free-tx-buffers dev))
            (sys.int::signal-semaphore (virtio-net-n-free-tx-buffers dev)))
          (setf (virtqueue-last-seen-used tx-queue)
                (ldb (byte 16 0) (1+ (virtqueue-last-seen-used tx-queue))))))))

(defun transmit-one (dev packet)
  ;; Allocate a free TX descriptor.
  (sys.int::wait-on-semaphore (virtio-net-n-free-tx-buffers dev))
  (let* ((tx-queue (aref (virtio-device-virtqueues dev) +net-transmitq+))
         (hdr-desc (pop (virtio-net-free-tx-buffers dev)))
         ;; Bleah. Do a terrible phys->offset conversion.
         (offset (- (vring-desc-address tx-queue hdr-desc) (virtio-net-tx-physical dev))))
    ;; Copy packet into the descriptor.
    (sys.net:copy-packet (virtio-net-tx-array dev) packet (+ offset +virtio-net-hdr-size+))
    ;; Clear the header.
    (fill (virtio-net-tx-array dev) 0
          :start offset
          :end (+ offset +virtio-net-hdr-size+))
    ;; Add to the avail ring and notify the card.
    (vring-add-to-avail-ring tx-queue hdr-desc)
    (virtio-kick dev +net-transmitq+)))

(sys.int::define-interrupt-handler virtio-interrupt (io-base sem)
  (let ((status (system:io-port/8 (+ io-base +isr-status+))))
    (when (logbitp 0 status)
      (sys.int::signal-semaphore sem))))

(defun probe (pci-device)
  (when (eql (sys.int::pci-config/16 pci-device sys.int::+pci-config-subdeviceid+)
             +virtio-subsystem-network-device+)
    (let ((dev (make-virtio-device #'make-virtio-net pci-device)))
      ;; Reset the device.
      (virtio-reset dev)
      ;; Set status to ACKNOWLEDGE|DRIVER.
      (setf (reg/8 dev +device-status+) (logior (ash 1 +config-s-acknowledge+)
                                                (ash 1 +config-s-driver+)))
      (let ((features (virtio-device-features dev)))
        (format t "Virtio-net device at ~S has features ~8,'0X~%"
                pci-device features)
        ;; Allocate virtqueues.
        ;; Network devices have two.
        (virtio-configure-virtqueues dev 2)
        ;; Negotiate feature bits.
        ;; This driver only cares about the MAC feature.
        (when (not (logbitp +net-f-mac+ features))
          (format t "MAC feature bit not set. Bailing out.~%")
          (setf (reg/8 dev +device-status+) (ash 1 +config-s-failed+))
          (return-from probe))
        (setf (reg/32 dev +guest-features-low+) (ash 1 +net-f-mac+))
        ;; Features negotiated, switch to driver-ok mode.
        (setf (reg/8 dev +device-status+) (logior (ash 1 +config-s-acknowledge+)
                                                  (ash 1 +config-s-driver+)
                                                  (ash 1 +config-s-driver-ok+)))
        (push dev *cards*)
        ;; Read the mac address.
        (dotimes (i 6)
          (setf (aref (virtio-net-mac dev) i)
                (reg/8 dev (+ (virtio-device-device-specific-offset dev) i))))
        (format t "Mac is ~/SYS.NET::FORMAT-MAC/~%" (virtio-net-mac dev))
        ;; Populate the receive virtqueue.
        ;; Throw in 32 buffers, just because.
        (let* ((rx-queue (aref (virtio-device-virtqueues dev) +net-receiveq+))
               (n-rx-buffers (min +n-rx-buffers+ (truncate (virtqueue-size rx-queue) 2))))
          ;; Allocate as a single large chunk.
          (multiple-value-bind (array phys)
              (sys.int::allocate-dma-buffer (* n-rx-buffers +rx-buffer-size+))
            (format t "RX buffer at ~8,'0X~%" phys)
            (setf (virtio-net-rx-array dev) array
                  (virtio-net-rx-physical dev) phys)
            ;; Now create actual virtio buffers in the ring.
            (dotimes (i n-rx-buffers)
              (vring-add-buffer rx-queue
                                (list (list (+ phys (* i +rx-buffer-size+))
                                            +virtio-net-hdr-size+
                                            t)
                                      (list (+ phys (* i +rx-buffer-size+) +virtio-net-hdr-size+)
                                            (- +rx-buffer-size+ +virtio-net-hdr-size+)
                                            t))))
            (virtio-kick dev +net-receiveq+)))
        ;; Allocate a bunch of bounce-buffers for TX.
        (let* ((tx-queue (aref (virtio-device-virtqueues dev) +net-transmitq+))
               (n-tx-buffers (min +n-tx-buffers+ (truncate (virtqueue-size tx-queue) 2))))
          (multiple-value-bind (array phys)
              (sys.int::allocate-dma-buffer (* n-tx-buffers +tx-buffer-size+))
            (format t "TX buffer at ~8,'0X~%" phys)
            (setf (virtio-net-tx-array dev) array
                  (virtio-net-tx-physical dev) phys)
            ;; Allocate descriptors, but don't put them in the avail ring.
            (dotimes (i +n-tx-buffers+)
              (let ((hdr-desc (vring-alloc-descriptor tx-queue))
                    (pkt-desc (vring-alloc-descriptor tx-queue)))
                (setf (vring-desc-address tx-queue hdr-desc) (+ phys (* i +tx-buffer-size+))
                      (vring-desc-length tx-queue hdr-desc) +virtio-net-hdr-size+
                      (vring-desc-flags tx-queue hdr-desc) (ash 1 +vring-desc-f-next+)
                      (vring-desc-next tx-queue hdr-desc) pkt-desc)
                (setf (vring-desc-address tx-queue pkt-desc) (+ phys (* i +tx-buffer-size+) +virtio-net-hdr-size+)
                      (vring-desc-length tx-queue pkt-desc) (- +tx-buffer-size+ +virtio-net-hdr-size+)
                      (vring-desc-flags tx-queue pkt-desc) 0)
                (push hdr-desc (virtio-net-free-tx-buffers dev))
                (sys.int::signal-semaphore (virtio-net-n-free-tx-buffers dev))))))
        (sys.net:register-nic dev)
        ;; Unmask the IRQ.
        (setf (sys.int::isa-pic-interrupt-handler (sys.int::pci-irq-line pci-device))
              (sys.int::make-interrupt-handler 'virtio-interrupt
                                               (virtio-net-io-base dev)
                                               (virtio-net-irq-semaphore dev)))
        (setf (sys.int::isa-pic-irq-mask (sys.int::pci-irq-line pci-device)) nil)
        ;; Create worker process.
        (setf (virtio-net-process dev) (sys.int::make-process "Virtio-Net worker"))
        (sys.int::process-preset (virtio-net-process dev)
                                 #'worker dev)
        (sys.int::process-enable (virtio-net-process dev))))))

(sys.int::pci-register-driver *virtio-pci-ids* 'probe)
