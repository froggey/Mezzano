;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;;; Network device feature bits.
(defconstant +virtio-net-f-csum+ 0
  "Device handles packets with partial checksum.")
(defconstant +virtio-net-f-guest-csum+ 1
  "Guest handles packets with partial checksum.")
(defconstant +virtio-net-f-mac+ 5
  "Device has given MAC address.")
(defconstant +virtio-net-f-gso+ 6
  "Device handles packets with any GSO type. (Deprecated)")
(defconstant +virtio-net-f-guest-tso4+ 7
  "Guest can receive TSOv4.")
(defconstant +virtio-net-f-guest-tso6+ 8
  "Guest can receive TSOv6.")
(defconstant +virtio-net-f-guest-ecn+ 9
  "Guest can receive TSO with ECN.")
(defconstant +virtio-net-f-guest-ufo+ 10
  "Guest can receive UFO.")
(defconstant +virtio-net-f-host-tso4+ 11
  "Device can receive TSOv4.")
(defconstant +virtio-net-f-host-tso6+ 12
  "Device can receive TSOv6.")
(defconstant +virtio-net-f-host-ecn+ 13
  "Device can receive TSO with ECN.")
(defconstant +virtio-net-f-host-ufo+ 14
  "Device can receive UFO.")
(defconstant +virtio-net-f-mrg-rxbuf+ 15
  "Guest can merge receive buffers.")
(defconstant +virtio-net-f-status+ 16
  "Configuration status field is available.")
(defconstant +virtio-net-f-ctrl-vq+ 17
  "Control channel is available.")
(defconstant +virtio-net-f-ctrl-rx+ 18
  "Control channel RX mode support.")
(defconstant +virtio-net-f-ctrl-vlan+ 19
  "Control channel VLAN filtering.")

(defconstant +virtio-net-mac+ 0
  "MAC address field, 6 octets.")

(defconstant +virtio-net-receiveq+ 0
  "The RX virtqueue index.")
(defconstant +virtio-net-transmitq+ 1
  "The TX virtqueue index.")

(defconstant +virtio-net-mtu+ 1514)

(defconstant +virtio-net-hdr-size+ 10)
(defconstant +virtio-net-rx-buffer-size+ #x600
  "Size of one RX packet buffer. As large as one ethernet
packet (1514 bytes), plus a virtio-net header (10 or 12 bytes)
and then some alignment.")
(defconstant +virtio-net-n-rx-buffers+ 32)
(defconstant +virtio-net-tx-buffer-size+ #x600
  "Size of one TX packet buffer. As large as one ethernet
packet (1514 bytes), plus a virtio-net header (10 or 12 bytes)
and then some alignment.")
(defconstant +virtio-net-n-tx-buffers+ 32)

(defstruct (virtio-net
             (:area :wired))
  mac
  virtio-device
  boot-id
  (lock (make-mutex "Virtio-Net NIC lock"))
  (irq-latch (make-latch "Virtio-Net NIC IRQ latch"))
  worker
  tx-virt
  tx-phys
  (free-tx-buffers nil)
  ;; Preallocated wired conses usable in the PA TX processing.
  (tx-buffer-freelist nil)
  ;; Packets waiting to be transmitted.
  (tx-pending '())
  (real-tx-pending '())
  rx-virt
  rx-phys
  (rx-buffer-count 0)
  (rx-buffers '())
  (received-packets '())
  (total-rx-bytes 0)
  (total-tx-bytes 0)
  (total-rx-packets 0)
  (total-tx-packets 0))

(defvar *virtio-net-cards*)

(defun virtio-net-update-nic (nic))

(defun virtio-net-do-receive-processing (nic)
  (let* ((dev (virtio-net-virtio-device nic))
         (rx-queue (virtio-virtqueue dev +virtio-net-receiveq+)))
    (loop
       #+(or)(debug-print-line "Process RX... " (virtio-ring-used-idx rx-queue) " " (virtqueue-last-seen-used rx-queue))
       (when (eql (virtio-ring-used-idx rx-queue)
                  (virtqueue-last-seen-used rx-queue))
         ;; All packets have been processed, notify the device that buffers are available.
         (virtio-kick dev +virtio-net-receiveq+)
         (return))
       (let* ((ring-entry (rem (virtqueue-last-seen-used rx-queue)
                               (virtqueue-size rx-queue)))
              (id (virtio-ring-used-elem-id rx-queue ring-entry))
              ;; Cheat slightly, RX descriptor IDs can be converted directly
              ;; to offsets in the RX buffer.
              (rx-offset (+ (* (truncate id 2) +virtio-net-rx-buffer-size+) +virtio-net-hdr-size+))
              (packet-cons (or (virtio-net-rx-buffers nic)
                               (panic "Where are my RX buffers?")))
              (packet (car packet-cons)))
         (setf (virtio-net-rx-buffers nic) (cdr packet-cons))
         (decf (virtio-net-rx-buffer-count nic))
         #+nil(format t "RX ring entry: ~D  buffer: ~D  len ~D~%" ring-entry id len)
         ;; Extract the packet!
         ;; FIXME: Just how long are the packets supposed to be?
         (dotimes (i +virtio-net-mtu+)
           (setf (aref packet i) (sys.int::memref-unsigned-byte-8 (virtio-net-rx-virt nic) (+ rx-offset i))))
         (incf (virtio-net-total-rx-bytes nic) +virtio-net-mtu+)
         (incf (virtio-net-total-rx-packets nic))
         ;; Re-add the descriptor to the avail ring.
         (virtio-ring-add-to-avail-ring rx-queue id)
         ;; Dispatch!
         (setf (cdr packet-cons) (virtio-net-received-packets nic)
               (virtio-net-received-packets nic) packet-cons))
       (setf (virtqueue-last-seen-used rx-queue)
             (ldb (byte 16 0) (1+ (virtqueue-last-seen-used rx-queue)))))))

(defun virtio-net-do-transmit-processing (nic)
  (let* ((dev (virtio-net-virtio-device nic))
         (tx-queue (virtio-virtqueue dev +virtio-net-transmitq+)))
    (loop
       #+(or)(debug-print-line "Process TX... " (virtio-ring-used-idx tx-queue) " " (virtqueue-last-seen-used tx-queue))
       (when (eql (virtio-ring-used-idx tx-queue)
                  (virtqueue-last-seen-used tx-queue))
         (return))
       (let* ((ring-entry (rem (virtqueue-last-seen-used tx-queue)
                               (virtqueue-size tx-queue)))
              (id (virtio-ring-used-elem-id tx-queue ring-entry))
              (len (virtio-ring-used-elem-len tx-queue ring-entry))
              (cons (virtio-net-tx-buffer-freelist nic)))
         (setf (virtio-net-tx-buffer-freelist nic) (cdr cons))
         ;; Packet sent. Add the descriptor back to the freelist.
         (setf (car cons) id
               (cdr cons) (virtio-net-free-tx-buffers nic)
               (virtio-net-free-tx-buffers nic) cons))
       (setf (virtqueue-last-seen-used tx-queue)
             (ldb (byte 16 0) (1+ (virtqueue-last-seen-used tx-queue)))))))

(defun virtio-net-transmit-real (nic packet)
  (let* ((dev (virtio-net-virtio-device nic))
         (tx-queue (virtio-virtqueue dev +virtio-net-transmitq+))
         (hdr-desc-cons (virtio-net-free-tx-buffers nic))
         (hdr-desc (pop (virtio-net-free-tx-buffers nic)))
         ;; Cheat slightly, TX descriptor IDs can be converted directly
         ;; to offsets in the TX buffer.
         (tx-addr (+ (virtio-net-tx-virt nic) (* (truncate hdr-desc 2) +virtio-net-tx-buffer-size+))))
    #+(or)(debug-print-line "Transmit buffer at " (virtio-net-tx-virt nic) " + "
                      (* (truncate hdr-desc 2) +virtio-net-tx-buffer-size+)
                      "  old cons is " hdr-desc-cons)
    (setf (cdr hdr-desc-cons) (virtio-net-tx-buffer-freelist nic)
          (virtio-net-tx-buffer-freelist nic) hdr-desc-cons)
    (incf (virtio-net-total-tx-bytes nic) (length packet))
    (incf (virtio-net-total-tx-packets nic))
    ;; Copy packet into the descriptor.
    (dotimes (i (length packet))
      (setf (sys.int::memref-unsigned-byte-8 (+ tx-addr +virtio-net-hdr-size+) i) (aref packet i)))
    ;; Clear the header.
    (dotimes (i +virtio-net-hdr-size+)
      (setf (sys.int::memref-unsigned-byte-8 tx-addr i) 0))
    (let ((pkt-desc (virtio-ring-desc-next tx-queue hdr-desc)))
      (setf (virtio-ring-desc-length tx-queue pkt-desc) (length packet)))
    ;; Add to the avail ring and notify the card.
    (virtio-ring-add-to-avail-ring tx-queue hdr-desc)
    (virtio-kick dev +virtio-net-transmitq+)))

(defun virtio-net-worker (nic)
  (loop
     ;; Wait for something to happen.
     (latch-wait (virtio-net-irq-latch nic))
     (latch-reset (virtio-net-irq-latch nic))
     (when (not (eql (virtio-net-boot-id nic) *boot-id*))
       ;; Reboot occurred, card no longer exists.
       (return-from virtio-net-worker))
     (with-mutex ((virtio-net-lock nic))
       (when (virtio-net-tx-pending nic)
         ;; Add pending packets to the real transmit queue, in proper order.
         (setf (virtio-net-real-tx-pending nic) (append (virtio-net-real-tx-pending nic)
                                                        (nreverse (virtio-net-tx-pending nic)))
               (virtio-net-tx-pending nic) '())))
     ;; Refill receive buffers.
     (dotimes (i (- +virtio-net-n-rx-buffers+ (virtio-net-rx-buffer-count nic)))
       (incf (virtio-net-rx-buffer-count nic))
       (push (make-array +virtio-net-mtu+ :element-type '(unsigned-byte 8))
             (virtio-net-rx-buffers nic)))
     ;; Do all access pseudo atomically, preventing mid-access snapshotting.
     ;; This is the only thread directly accessing the card, so no more sync is required.
     (with-pseudo-atomic
       (virtio-net-do-receive-processing nic)
       (virtio-net-do-transmit-processing nic)
       ;; Send any pending packets.
       (loop
          #+(or)(debug-print-line "Real TX " (virtio-net-free-tx-buffers nic) " " (virtio-net-real-tx-pending nic))
          (when (not (and (virtio-net-free-tx-buffers nic)
                          (virtio-net-real-tx-pending nic)))
            (return))
          (virtio-net-transmit-real nic (pop (virtio-net-real-tx-pending nic)))))
     ;; Dispatch received packets.
     (dolist (pkt (nreverse (virtio-net-received-packets nic)))
       (nic-received-packet nic pkt))
     (setf (virtio-net-received-packets nic) '())))

(defun virtio-net-irq-handler (nic)
  (when (logbitp 0 (virtio-isr-status (virtio-net-virtio-device nic)))
    (latch-trigger (virtio-net-irq-latch nic))))

(defun initialize-virtio-net ()
  (when (not (boundp '*virtio-net-cards*))
    (setf *virtio-net-cards* '()))
  ;; Flush worker threads and cards.
  (dolist (nic *virtio-net-cards*)
    ;; Wake each worker thread, so they can notice (boot-id nic) != current-boot-id.
    (latch-trigger (virtio-net-irq-latch nic)))
  (setf *virtio-net-cards* '()))

(defun virtio-net-transmit (nic packet)
  (let* ((len (loop for elt in packet
                 summing (length elt)))
         (data (make-array len
                           :element-type '(unsigned-byte 8)
                           :area :wired))
         (cons (cons data nil)))
    (when (> len +virtio-net-mtu+)
      (error "Packet exceeds MTU."))
    ;; Copy packet into temp buffer.
    (let ((offset 0))
      (dolist (p packet)
        (dotimes (i (length p))
          (setf (aref data offset) (aref p i))
          (incf offset))))
    (with-mutex ((virtio-net-lock nic))
      (setf (cdr cons) (virtio-net-tx-pending nic)
            (virtio-net-tx-pending nic) cons))
    (latch-trigger (virtio-net-irq-latch nic))))

(defun virtio-net-stats (nic)
  (values (virtio-net-total-rx-bytes nic)
          (virtio-net-total-rx-packets nic)
          0
          (virtio-net-total-tx-bytes nic)
          (virtio-net-total-tx-packets nic)
          0
          0))

(defun virtio-net-register (device)
  ;; Wired allocation required for the IRQ handler closure.
  (declare (sys.c::closure-allocation :wired))
  ;; Set the driver bit in the status field.
  (setf (virtio-device-status device) (logior +virtio-status-acknowledge+
                                              +virtio-status-driver+))
  ;; Feature negotiation, we only have eyes for the MAC feature.
  (when (not (logbitp +virtio-net-f-mac+ (virtio-device-features device)))
    (setf (virtio-device-status device) +virtio-status-failed+)
    (return-from virtio-net-register))
  ;; Enable MAC feature.
  (setf (ldb (byte 1 +virtio-net-f-mac+) (virtio-guest-features device)) 1)
  ;; Allocate virtqueues.
  (when (not (virtio-configure-virtqueues device 2))
    (setf (virtio-device-status device) +virtio-status-failed+)
    (return-from virtio-net-register))
  ;; Read the MAC address.
  (let ((mac 0))
    (dotimes (i 6)
      (setf (ldb (byte 8 (* i 8)) mac) (virtio-device-specific-header/8 device (+ +virtio-net-mac+ i))))
    ;; Create card.
    (let* ((nic (make-virtio-net :mac mac
                                 :virtio-device device
                                 :boot-id *boot-id*)))
      (when (not (virtio-net-allocate-tx-descriptors nic))
        (debug-print-line "Unable to allocate TX buffers")
        (setf (virtio-device-status device) +virtio-status-failed+)
        (return-from virtio-net-register))
      (when (not (virtio-net-allocate-rx-descriptors nic))
        (debug-print-line "Unable to allocate RX buffers")
        (setf (virtio-device-status device) +virtio-status-failed+)
        (return-from virtio-net-register))
      (push-wired nic *virtio-net-cards*)
      (register-nic nic mac 'virtio-net-transmit 'virtio-net-stats +virtio-net-mtu+)
      ;; Attach IRQ handler.
      (virtio-attach-irq device (lambda (interrupt-frame irq)
                                  (declare (ignore interrupt-frame irq))
                                  (virtio-net-irq-handler nic)))
      (setf (virtio-irq-mask device) nil)
      ;; Cook up a worker thread.
      (add-deferred-boot-action
       (lambda ()
         (setf (virtio-net-worker nic) (make-thread (lambda () (virtio-net-worker nic))
                                                    :name "Virtio-Net NIC worker"))))
      ;; Configuration complete, go to OK mode.
      (setf (virtio-device-status device) (logior +virtio-status-acknowledge+
                                                  +virtio-status-driver+
                                                  +virtio-status-ok+))
      (virtio-kick device +virtio-net-receiveq+))))

(defun virtio-net-allocate-tx-descriptors (nic)
  ;; Allocate a bunch of bounce-buffers for TX.
  (let* ((dev (virtio-net-virtio-device nic))
         (tx-queue (virtio-virtqueue dev +virtio-net-transmitq+))
         (n-tx-buffers (min +virtio-net-n-tx-buffers+ (truncate (virtqueue-size tx-queue) 2)))
         (frame (or (allocate-physical-pages (ceiling (* n-tx-buffers +virtio-net-tx-buffer-size+) +4k-page-size+))
                    (return-from virtio-net-allocate-tx-descriptors nil)))
         (phys (* frame +4k-page-size+))
         (virt (+ +physical-map-base+ phys)))
    (debug-print-line "Virtio-Net TX buffer at " phys)
    (setf (virtio-net-tx-virt nic) virt
          (virtio-net-tx-phys nic) phys)
    ;; Allocate descriptors, but don't put them in the avail ring.
    (dotimes (i n-tx-buffers)
      (let ((hdr-desc (virtio-ring-alloc-descriptor tx-queue))
            (pkt-desc (virtio-ring-alloc-descriptor tx-queue)))
        (setf (virtio-ring-desc-address tx-queue hdr-desc) (+ phys (* i +virtio-net-tx-buffer-size+))
              (virtio-ring-desc-length tx-queue hdr-desc) +virtio-net-hdr-size+
              (virtio-ring-desc-flags tx-queue hdr-desc) (ash 1 +virtio-ring-desc-f-next+)
              (virtio-ring-desc-next tx-queue hdr-desc) pkt-desc)
        (setf (virtio-ring-desc-address tx-queue pkt-desc) (+ phys (* i +virtio-net-tx-buffer-size+) +virtio-net-hdr-size+)
              (virtio-ring-desc-length tx-queue pkt-desc) (- +virtio-net-tx-buffer-size+ +virtio-net-hdr-size+)
              (virtio-ring-desc-flags tx-queue pkt-desc) 0)
        (push-wired hdr-desc (virtio-net-free-tx-buffers nic)))))
  t)

(defun virtio-net-allocate-rx-descriptors (nic)
  ;; Populate the receive virtqueue.
  ;; Throw in 32 buffers, just because.
  (let* ((dev (virtio-net-virtio-device nic))
         (rx-queue (virtio-virtqueue (virtio-net-virtio-device nic) +virtio-net-receiveq+))
         (n-rx-buffers (min +virtio-net-n-rx-buffers+ (truncate (virtqueue-size rx-queue) 2)))
         ;; Allocate as a single large chunk.
         (frame (or (allocate-physical-pages (ceiling (* n-rx-buffers +virtio-net-rx-buffer-size+) +4k-page-size+))
                    (return-from virtio-net-allocate-rx-descriptors nil)))
         (phys (* frame +4k-page-size+))
         (virt (+ +physical-map-base+ phys)))
    (debug-print-line "Virtio-Net RX buffer at " phys)
    (setf (virtio-net-rx-virt nic) virt
          (virtio-net-rx-phys nic) phys)
    ;; Now create actual virtio buffers in the ring.
    (dotimes (i n-rx-buffers)
      (let ((hdr-desc (virtio-ring-alloc-descriptor rx-queue))
            (pkt-desc (virtio-ring-alloc-descriptor rx-queue)))
        (setf (virtio-ring-desc-address rx-queue hdr-desc) (+ phys (* i +virtio-net-rx-buffer-size+))
              (virtio-ring-desc-length rx-queue hdr-desc) +virtio-net-hdr-size+
              (virtio-ring-desc-flags rx-queue hdr-desc) (logior (ash 1 +virtio-ring-desc-f-next+)
                                                           (ash 1 +virtio-ring-desc-f-write+))
              (virtio-ring-desc-next rx-queue hdr-desc) pkt-desc)
        (setf (virtio-ring-desc-address rx-queue pkt-desc) (+ phys (* i +virtio-net-rx-buffer-size+) +virtio-net-hdr-size+)
              (virtio-ring-desc-length rx-queue pkt-desc) (- +virtio-net-rx-buffer-size+ +virtio-net-hdr-size+)
              (virtio-ring-desc-flags rx-queue pkt-desc) (ash 1 +virtio-ring-desc-f-write+))
        (virtio-ring-add-to-avail-ring rx-queue hdr-desc))))
  t)
