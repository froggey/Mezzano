;;;; Virtio-net network card driver.

(defpackage :mezzano.driver.virtio-net
  (:use :cl)
  (:local-nicknames (:nic :mezzano.driver.network-card)
                    (:sup :mezzano.supervisor)
                    (:virtio :mezzano.supervisor.virtio)
                    (:sync :mezzano.sync)
                    (:sys.int :mezzano.internals)))

(in-package :mezzano.driver.virtio-net)

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

(defclass virtio-net (nic:network-card)
  ((%mac :accessor virtio-net-mac :reader nic:mac-address)
   (%virtio-device :accessor virtio-net-virtio-device :initarg :virtio-device)
   (%tx-mailbox :accessor virtio-net-tx-mailbox)
   (%irq-handler :accessor virtio-net-irq-handler)
   (%worker-thread :accessor virtio-net-worker-thread)
   (%tx-virt :accessor virtio-net-tx-virt)
   (%tx-phys :accessor virtio-net-tx-phys)
   (%free-tx-buffers :accessor virtio-net-free-tx-buffers :initform nil)
   ;; Preallocated wired conses usable in the PA TX processing.
   (%tx-buffer-freelist :accessor virtio-net-tx-buffer-freelist :initform nil)
   ;; Packets waiting to be transmitted.
   (%real-tx-pending :accessor virtio-net-real-tx-pending :initform '())
   (%rx-virt :accessor virtio-net-rx-virt)
   (%rx-phys :accessor virtio-net-rx-phys)
   (%total-rx-bytes :accessor virtio-net-total-rx-bytes :initform 0)
   (%total-tx-bytes :accessor virtio-net-total-tx-bytes :initform 0)
   (%total-rx-packets :accessor virtio-net-total-rx-packets :initform 0)
   (%total-tx-packets :accessor virtio-net-total-tx-packets :initform 0)))

(defun virtio-net-boot-id (nic)
  (virtio:virtio-device-boot-id (virtio-net-virtio-device nic)))

(defmacro with-virito-net-access ((nic) &body body)
  `(sup:with-device-access ((virtio-net-boot-id ,nic) (throw 'nic-detached nil))
     ,@body))

(defun virtio-net-receive-processing (nic)
  (let* ((dev (virtio-net-virtio-device nic))
         (rx-queue (virtio:virtio-virtqueue dev +virtio-net-receiveq+)))
    (loop
       ;; Receive outstanding packets one by one.
       (with-virito-net-access (nic)
         (when (eql (virtio:virtio-ring-used-idx rx-queue)
                    (virtio:virtqueue-last-seen-used rx-queue))
           ;; All packets have been processed, notify the device that buffers are available.
           (virtio:virtio-kick dev +virtio-net-receiveq+)
           (return)))
       ;; Allocate a buffer. Try to minimize the amount of work done in a device-access region, hence the dropping in and out.
       ;; TODO: Get the packet size correct.
       (let ((packet (make-array +virtio-net-mtu+ :element-type '(unsigned-byte 8))))
         (with-virito-net-access (nic)
           (let* ((ring-entry (rem (virtio:virtqueue-last-seen-used rx-queue)
                                   (virtio:virtqueue-size rx-queue)))
                  (id (virtio:virtio-ring-used-elem-id rx-queue ring-entry))
                  (len (virtio:virtio-ring-used-elem-len rx-queue ring-entry))
                  ;; Cheat slightly, RX descriptor IDs can be converted directly
                  ;; to offsets in the RX buffer.
                  (rx-offset (+ (* (truncate id 2) +virtio-net-rx-buffer-size+) +virtio-net-hdr-size+)))
             ;;(format t "RX ring entry: ~D  buffer: ~D  len ~D~%" ring-entry id len)
             ;; Extract the packet!
             (dotimes (i (- len +virtio-net-hdr-size+))
               (setf (aref packet i) (sys.int::memref-unsigned-byte-8 (virtio-net-rx-virt nic)
                                                                      (+ rx-offset i))))
             (incf (virtio-net-total-rx-bytes nic) +virtio-net-mtu+)
             (incf (virtio-net-total-rx-packets nic))
             ;; Re-add the descriptor to the avail ring.
             (virtio:virtio-ring-add-to-avail-ring rx-queue id)
             (setf (virtio:virtqueue-last-seen-used rx-queue)
                   (ldb (byte 16 0) (1+ (virtio:virtqueue-last-seen-used rx-queue))))))
         (nic:device-received-packet nic packet)))))

(defun virtio-net-do-transmit-processing (nic)
  (let* ((dev (virtio-net-virtio-device nic))
         (tx-queue (virtio:virtio-virtqueue dev +virtio-net-transmitq+)))
    (loop
       #+(or)(debug-print-line "Process TX... " (virtio-ring-used-idx tx-queue) " " (virtqueue-last-seen-used tx-queue))
       (when (eql (virtio:virtio-ring-used-idx tx-queue)
                  (virtio:virtqueue-last-seen-used tx-queue))
         (return))
       (let* ((ring-entry (rem (virtio:virtqueue-last-seen-used tx-queue)
                               (virtio:virtqueue-size tx-queue)))
              (id (virtio:virtio-ring-used-elem-id tx-queue ring-entry))
              (cons (virtio-net-tx-buffer-freelist nic)))
         (setf (virtio-net-tx-buffer-freelist nic) (cdr cons))
         ;; Packet sent. Add the descriptor back to the freelist.
         (setf (car cons) id
               (cdr cons) (virtio-net-free-tx-buffers nic)
               (virtio-net-free-tx-buffers nic) cons))
       (setf (virtio:virtqueue-last-seen-used tx-queue)
             (ldb (byte 16 0) (1+ (virtio:virtqueue-last-seen-used tx-queue)))))))

(defun virtio-net-transmit-real (nic packet)
  (let* ((dev (virtio-net-virtio-device nic))
         (tx-queue (virtio:virtio-virtqueue dev +virtio-net-transmitq+))
         (hdr-desc-cons (virtio-net-free-tx-buffers nic))
         (hdr-desc (pop (virtio-net-free-tx-buffers nic)))
         ;; Cheat slightly, TX descriptor IDs can be converted directly
         ;; to offsets in the TX buffer.
         (tx-addr (+ (virtio-net-tx-virt nic) (* (truncate hdr-desc 2) +virtio-net-tx-buffer-size+))))
    #+(or)(sup:debug-print-line "Transmit buffer at " (virtio-net-tx-virt nic) " + "
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
    (let ((pkt-desc (virtio:virtio-ring-desc-next tx-queue hdr-desc)))
      (setf (virtio:virtio-ring-desc-length tx-queue pkt-desc) (length packet)))
    ;; Add to the avail ring and notify the card.
    (virtio:virtio-ring-add-to-avail-ring tx-queue hdr-desc)
    (virtio:virtio-kick dev +virtio-net-transmitq+)))

(defun virtio-net-transmit-processing (nic)
  ;; Add pending packets to the real transmit queue, in proper order.
  (loop
     (let ((packet (sync:mailbox-receive (virtio-net-tx-mailbox nic) :wait-p nil)))
       (when (not packet)
         (return))
       (setf (virtio-net-real-tx-pending nic) (append (virtio-net-real-tx-pending nic)
                                                      (list packet)))))
  (with-virito-net-access (nic)
    (virtio-net-do-transmit-processing nic)
    ;; Send any pending packets.
    (loop
       #+(or)(debug-print-line "Real TX " (virtio-net-free-tx-buffers nic) " " (virtio-net-real-tx-pending nic))
       (when (not (and (virtio-net-free-tx-buffers nic)
                       (virtio-net-real-tx-pending nic)))
         (return))
       (virtio-net-transmit-real nic (pop (virtio-net-real-tx-pending nic))))))

(defmethod nic:transmit-packet ((nic virtio-net) packet)
  (let* ((len (loop for elt in packet
                 summing (length elt)))
         (data (make-array len
                           :element-type '(unsigned-byte 8))))
    (when (> len +virtio-net-mtu+)
      (error "Packet exceeds MTU."))
    ;; Copy packet into temp buffer.
    (let ((offset 0))
      (dolist (p packet)
        (replace data p :start1 offset)
        (incf offset (length p))))
    (sync:mailbox-send data (virtio-net-tx-mailbox nic))))

(defmethod nic:statistics ((nic virtio-net))
  (values (virtio-net-total-rx-bytes nic)
          (virtio-net-total-rx-packets nic)
          0
          (virtio-net-total-tx-bytes nic)
          (virtio-net-total-tx-packets nic)
          0
          0))

(defun virtio-net-allocate-tx-descriptors (nic)
  ;; Allocate a bunch of bounce-buffers for TX.
  (let* ((dev (virtio-net-virtio-device nic))
         (tx-queue (virtio:virtio-virtqueue dev +virtio-net-transmitq+))
         (n-tx-buffers (min +virtio-net-n-tx-buffers+ (truncate (virtio:virtqueue-size tx-queue) 2)))
         (frame (or (sup::allocate-physical-pages (ceiling (* n-tx-buffers +virtio-net-tx-buffer-size+) sup::+4k-page-size+))
                    (return-from virtio-net-allocate-tx-descriptors nil)))
         (phys (* frame sup::+4k-page-size+))
         (virt (sup::convert-to-pmap-address phys)))
    (sup:debug-print-line "Virtio-Net TX buffer at " phys)
    (setf (virtio-net-tx-virt nic) virt
          (virtio-net-tx-phys nic) phys)
    ;; Allocate descriptors, but don't put them in the avail ring.
    (dotimes (i n-tx-buffers)
      (let ((hdr-desc (virtio:virtio-ring-alloc-descriptor tx-queue))
            (pkt-desc (virtio:virtio-ring-alloc-descriptor tx-queue)))
        (setf (virtio:virtio-ring-desc-address tx-queue hdr-desc) (+ phys (* i +virtio-net-tx-buffer-size+))
              (virtio:virtio-ring-desc-length tx-queue hdr-desc) +virtio-net-hdr-size+
              (virtio:virtio-ring-desc-flags tx-queue hdr-desc) (ash 1 virtio:+virtio-ring-desc-f-next+)
              (virtio:virtio-ring-desc-next tx-queue hdr-desc) pkt-desc)
        (setf (virtio:virtio-ring-desc-address tx-queue pkt-desc) (+ phys (* i +virtio-net-tx-buffer-size+) +virtio-net-hdr-size+)
              (virtio:virtio-ring-desc-length tx-queue pkt-desc) (- +virtio-net-tx-buffer-size+ +virtio-net-hdr-size+)
              (virtio:virtio-ring-desc-flags tx-queue pkt-desc) 0)
        (push hdr-desc (virtio-net-free-tx-buffers nic)))))
  t)

(defun virtio-net-allocate-rx-descriptors (nic)
  ;; Populate the receive virtqueue.
  ;; Throw in 32 buffers, just because.
  (let* ((rx-queue (virtio:virtio-virtqueue (virtio-net-virtio-device nic) +virtio-net-receiveq+))
         (n-rx-buffers (min +virtio-net-n-rx-buffers+ (truncate (virtio:virtqueue-size rx-queue) 2)))
         ;; Allocate as a single large chunk.
         (frame (or (sup::allocate-physical-pages (ceiling (* n-rx-buffers +virtio-net-rx-buffer-size+) sup::+4k-page-size+))
                    (return-from virtio-net-allocate-rx-descriptors nil)))
         (phys (* frame sup::+4k-page-size+))
         (virt (sup::convert-to-pmap-address phys)))
    (sup:debug-print-line "Virtio-Net RX buffer at " phys)
    (setf (virtio-net-rx-virt nic) virt
          (virtio-net-rx-phys nic) phys)
    ;; Now create actual virtio buffers in the ring.
    (dotimes (i n-rx-buffers)
      (let ((hdr-desc (virtio:virtio-ring-alloc-descriptor rx-queue))
            (pkt-desc (virtio:virtio-ring-alloc-descriptor rx-queue)))
        (setf (virtio:virtio-ring-desc-address rx-queue hdr-desc) (+ phys (* i +virtio-net-rx-buffer-size+))
              (virtio:virtio-ring-desc-length rx-queue hdr-desc) +virtio-net-hdr-size+
              (virtio:virtio-ring-desc-flags rx-queue hdr-desc) (logior (ash 1 virtio:+virtio-ring-desc-f-next+)
                                                                        (ash 1 virtio:+virtio-ring-desc-f-write+))
              (virtio:virtio-ring-desc-next rx-queue hdr-desc) pkt-desc)
        (setf (virtio:virtio-ring-desc-address rx-queue pkt-desc) (+ phys (* i +virtio-net-rx-buffer-size+) +virtio-net-hdr-size+)
              (virtio:virtio-ring-desc-length rx-queue pkt-desc) (- +virtio-net-rx-buffer-size+ +virtio-net-hdr-size+)
              (virtio:virtio-ring-desc-flags rx-queue pkt-desc) (ash 1 virtio:+virtio-ring-desc-f-write+))
        (virtio:virtio-ring-add-to-avail-ring rx-queue hdr-desc))))
  t)

(defun virtio-net-register (device)
  (let* ((nic (make-instance 'virtio-net
                             :virtio-device device))
         (irq-handler (sup:make-simple-irq (virtio:virtio-device-irq device))))
    (sup:debug-print-line "Detected virtio net device " device " with irq " irq-handler)
    (setf (virtio-net-irq-handler nic) irq-handler)
    (setf (virtio-net-tx-mailbox nic) (sync:make-mailbox :name `(tx-mailbox ,nic)))
    (setf (virtio-net-worker-thread nic)
          (sup:make-thread (lambda ()
                             (virtio-net-worker nic))
                           :name "Virtio-Net NIC worker")))
  t)

(defun virtio-net-worker (nic)
  (when (not (virtio-net-initialize nic))
    (virtio:virtio-driver-detached (virtio-net-virtio-device nic))
    (return-from virtio-net-worker))
  (unwind-protect
       (catch 'nic-detached
         (loop
            ;; Wait for something to happen.
            ;; Either an interrupt, a request to send, or the device's boot epoch expiring.
            (sync:wait-for-objects (virtio-net-irq-handler nic)
                                   (virtio-net-tx-mailbox nic)
                                   (virtio-net-boot-id nic))
            (let* ((dev (virtio-net-virtio-device nic))
                   (status (virtio:virtio-isr-status dev)))
              ;; The IRQ (if any) must be acknowledged before doing RX/TX processing.
              ;; Doing it the other way around can result in IRQs being lost between
              ;; RX processing and IRQ acknowledgment.
              (virtio:virtio-ack-irq dev status)
              (sup:simple-irq-unmask (virtio-net-irq-handler nic))
              (virtio-net-receive-processing nic)
              (virtio-net-transmit-processing nic))))
    (nic:unregister-network-card nic)
    (virtio:virtio-driver-detached (virtio-net-virtio-device nic))))

(defun virtio-net-initialize (nic)
  (sup:with-device-access ((virtio-net-boot-id nic)
                           nil)
    (let ((device (virtio-net-virtio-device nic)))
      ;; Set the driver bit in the status field.
      (setf (virtio:virtio-device-status device) (logior virtio:+virtio-status-acknowledge+
                                                         virtio:+virtio-status-driver+))
      ;; Feature negotiation, we only have eyes for the MAC feature.
      (when (not (virtio:virtio-device-feature device +virtio-net-f-mac+))
        (sup:debug-print-line "Virtio feature mismatch")
        (setf (virtio:virtio-device-status device) virtio:+virtio-status-failed+)
        (return-from virtio-net-initialize nil))
      ;; Enable MAC feature.
      (setf (virtio:virtio-driver-feature device +virtio-net-f-mac+) t)
      ;; Allocate virtqueues.
      (when (not (virtio:virtio-configure-virtqueues device 2))
        (sup:debug-print-line "Unable to configure virtqueues")
        (setf (virtio:virtio-device-status device) virtio:+virtio-status-failed+)
        (return-from virtio-net-initialize nil))
      ;; Read the MAC address.
      (let ((mac 0))
        (dotimes (i 6)
          (setf (ldb (byte 8 (* i 8)) mac) (virtio:virtio-device-specific-header/8 device (+ +virtio-net-mac+ i))))
        (setf (virtio-net-mac nic) mac)
        (when (not (virtio-net-allocate-tx-descriptors nic))
          (sup:debug-print-line "Unable to allocate TX buffers")
          (setf (virtio:virtio-device-status device) virtio:+virtio-status-failed+)
          (return-from virtio-net-initialize nil))
        (when (not (virtio-net-allocate-rx-descriptors nic))
          (sup:debug-print-line "Unable to allocate RX buffers")
          (setf (virtio:virtio-device-status device) virtio:+virtio-status-failed+)
          (return-from virtio-net-initialize nil))
        ;; Configuration complete, go to OK mode.
        (sup:simple-irq-attach (virtio-net-irq-handler nic))
        (sup:simple-irq-unmask (virtio-net-irq-handler nic))
        (setf (virtio:virtio-device-status device) (logior virtio:+virtio-status-acknowledge+
                                                           virtio:+virtio-status-driver+
                                                           virtio:+virtio-status-ok+))
        (virtio:virtio-kick device +virtio-net-receiveq+)))
    (nic:register-network-card nic))
  t)

(defmethod nic:mtu ((nic virtio-net))
  +virtio-net-mtu+)

(virtio:define-virtio-driver virtio-net virtio-net-register virtio:+virtio-dev-id-net+)
