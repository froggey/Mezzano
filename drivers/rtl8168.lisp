;;;; Copyright (c) 2015-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.driver.rtl8168
  (:use :cl :mezzano.supervisor)
  (:local-nicknames (:nic :mezzano.driver.network-card)))

(in-package :mezzano.driver.rtl8168)

;;; http://www.iitg.ernet.in/asahu/cs421/RealTek.pdf

;;; Registers.
(defconstant +rtl8168-register-ID0+       #x00 "ID (MAC) Register 0")
(defconstant +rtl8168-register-MAR0+      #x08 "Multicast Register 0")
(defconstant +rtl8168-register-DTCCR+     #x10 "Dump Tally Counter Command Register")
(defconstant +rtl8168-register-TNPDS+     #x20 "Transmit Normal Priority Descriptors")
(defconstant +rtl8168-register-THPDS+     #x28 "Transmit High Priority Descriptors")
(defconstant +rtl8168-register-FLASH+     #x30 "Flash memory read/write register")
(defconstant +rtl8168-register-ERBCR+     #x34 "Early Receive (Rx) Byte Count Register")
(defconstant +rtl8168-register-ERSR+      #x36 "Early Rx Status Register")
(defconstant +rtl8168-register-CR+        #x37 "Command Register")
(defconstant +rtl8168-register-TPPoll+    #x38 "Transmit Priority Polling register")
(defconstant +rtl8168-register-IMR+       #x3C "Interrupt Mask Register")
(defconstant +rtl8168-register-ISR+       #x3E "Interrupt Status Register")
(defconstant +rtl8168-register-TCR+       #x40 "Transmit (Tx) Configuration Register")
(defconstant +rtl8168-register-RCR+       #x44 "Receive (Rx) Configuration Register")
(defconstant +rtl8168-register-TCTR+      #x48 "Timer CounT Register")
(defconstant +rtl8168-register-MPC+       #x4C "Missed Packed Counter")
(defconstant +rtl8168-register-9346CR+    #x50 "93C46 (93C56) Command Register")
(defconstant +rtl8168-register-CONFIG0+   #x51 "Configuration Register 0")
(defconstant +rtl8168-register-CONFIG1+   #x52 "Configuration Register 1")
(defconstant +rtl8168-register-CONFIG2+   #x53 "Configuration Register 2")
(defconstant +rtl8168-register-CONFIG3+   #x54 "Configuration Register 3")
(defconstant +rtl8168-register-CONFIG4+   #x55 "Configuration Register 4")
(defconstant +rtl8168-register-CONFIG5+   #x56 "Configuration Register 5")
(defconstant +rtl8168-register-TimerInt+  #x58 "Timer Interrupt Register")
(defconstant +rtl8168-register-MULINT+    #x5C "Multiple Interrupt Select")
(defconstant +rtl8168-register-PHYAR+     #x60 "PHY Access Register")
(defconstant +rtl8168-register-TBICSR0+   #x64 "TBI Control and Status Register")
(defconstant +rtl8168-register-TBI_ANAR+  #x68 "TBI Auto-Negotiation Advertisement Register")
(defconstant +rtl8168-register-TBI_LPAR+  #x6A "TBI Auto-Negotiation Link Partner Ability Register")
(defconstant +rtl8168-register-PHYStatus+ #x6C "PHY (GMII, MII, or TBI) Status Register.")
(defconstant +rtl8168-register-Wakeup0+   #x84 "Power Management wakeup frame0 (64bit)")
(defconstant +rtl8168-register-Wakeup1+   #x8C "Power Management wakeup frame1 (64bit)")
(defconstant +rtl8168-register-Wakeup2LD+ #x94 "Power Management wakeup frame2 (128bit), low D-Word")
(defconstant +rtl8168-register-Wakeup2HD+ #x9C "Power Management wakeup frame2 (128bit), high D-Word")
(defconstant +rtl8168-register-Wakeup3LD+ #xA4 "Power Management wakeup frame3 (128bit), low D-Word")
(defconstant +rtl8168-register-Wakeup3HD+ #xAC "Power Management wakeup frame3 (128bit), high D-Word")
(defconstant +rtl8168-register-Wakeup4LD+ #xB4 "Power Management wakeup frame4 (128bit), low D-Word")
(defconstant +rtl8168-register-Wakeup4HD+ #xBC "Power Management wakeup frame4 (128bit), high D-Word")
(defconstant +rtl8168-register-CRC0+      #xC4 "16-bit CRC of wakeup frame 0")
(defconstant +rtl8168-register-CRC1+      #xC6 "16-bit CRC of wakeup frame 1")
(defconstant +rtl8168-register-CRC2+      #xC8 "16-bit CRC of wakeup frame 2")
(defconstant +rtl8168-register-CRC3+      #xCA "16-bit CRC of wakeup frame 3")
(defconstant +rtl8168-register-CRC4+      #xCC "16-bit CRC of wakeup frame 4")
(defconstant +rtl8168-register-RMS+       #xDA "Rx packet Maximum Size")
(defconstant +rtl8168-register-C+CR+      #xE0 "C+ Command Register")
(defconstant +rtl8168-register-RDSAR+     #xE4 "Receive Descriptor Start Address Register (256-byte alignment)")
(defconstant +rtl8168-register-ETThR+     #xEC "Early Transmit Threshold Register")

;;; Bits in CR.
(defconstant +rtl8168-CR-RST+ 4 "Reset")
(defconstant +rtl8168-CR-RE+  3 "Receiver Enable")
(defconstant +rtl8168-CR-TE+  2 "Transmit Enable")

;;; Bit and fields in RCR.
(defconstant +rtl8168-RCR-MulERINT+ 24 "Multiple Early Interrupt Select")
(defconstant +rtl8168-RCR-RER8+ 16)
(defconstant +rtl8168-RCR-RXFTH+ 13 "Rx FIFO Threshold")
(defconstant +rtl8168-RCR-MXDMA+ 8 "Max DMA Burst Size per Rx DMA Burst")
(defconstant +rtl8168-RCR-9356SEL+ 6)
(defconstant +rtl8168-RCR-AER+ 5 "Accept Error Packet")
(defconstant +rtl8168-RCR-AR+ 4 "Accept Runt Packets")
(defconstant +rtl8168-RCR-AB+ 3 "Accept Broadcast Packets")
(defconstant +rtl8168-RCR-AM+ 2 "Accept Multicast Packets")
(defconstant +rtl8168-RCR-APM+ 1 "Accept Physical Match Packets")
(defconstant +rtl8168-RCR-AAP+ 0 "Accept All Packets With Destination Address")

;;; Bits and fields in TCR.
(defconstant +rtl8168-TCR-IFG+ 24 "InterFrameGap Time")
(defconstant +rtl8168-TCR-IFG2+ 19)
(defconstant +rtl8168-TCR-LBK+ 17 "LoopBack Test")
(defconstant +rtl8168-TCR-CRC+ 16 "Append CRC (active low)")
(defconstant +rtl8168-TCR-MXDMA+ 8 "Max DMA Burst Size per Tx DMA Burst")

;;; Bits in TPPoll.
(defconstant +rtl8168-TPPoll-HPQ+ 7 "High Priority Queue polling")
(defconstant +rtl8168-TPPoll-NPQ+ 6 "Normal Priority Queue polling")
(defconstant +rtl8168-TPPoll-FSWInt+ 0 "Forced Software Interrupt")

(defconstant +rtl8168-mtu+ 1536)
(defconstant +rtl8168-descriptor-size+ 16)
(defconstant +rtl8168-n-tx-descriptors+ 128)
(defconstant +rtl8168-n-rx-descriptors+ 128)

;;; Descriptor flag bits.
(defconstant +rtl8168-descriptor-OWN+ 31 "Ownership")
(defconstant +rtl8168-descriptor-EOR+ 30 "End of descriptor ring")
(defconstant +rtl8168-descriptor-FS+ 29 "First segment descriptor")
(defconstant +rtl8168-descriptor-LS+ 28 "Last segment descriptor")
(defconstant +rtl8168-descriptor-LGSEN+ 27 "Large Send")
(defconstant +rtl8168-descriptor-IPCS+ 18 "IP checksum offload")
(defconstant +rtl8168-descriptor-UDPCS+ 17 "UDP checksum offload")
(defconstant +rtl8168-descriptor-TCPCS+ 16 "TCP checksum offload")
(defconstant +rtl8168-descriptor-Frame_Length+ 0)

(defstruct (rtl8168
             (:area :wired))
  pci-location
  mac
  boot-id
  (lock (make-mutex "RTL8168 NIC lock"))
  (irq-latch (make-latch "RTL8168 NIC IRQ latch"))
  worker-thread
  irq
  irq-handler
  io-base

  tx-ring-phys
  tx-ring-virt
  tx-bounce-phys
  tx-bounce-virt
  ;; Number of outstanding packets (passed to card, but not yet transmitted).
  tx-used-count
  tx-current
  tx-tail
  ;; List of pending packets.
  tx-pending

  rx-ring-phys
  rx-ring-virt
  rx-bounce-phys
  rx-bounce-virt
  rx-current

  wrapper
)

;; FIXME: This should be merged with virtio-net.
;; Only exists for compatibility.
(defclass rtl8168-network-card (nic:network-card)
  ((nic :initarg :nic)))

(defun rtl8168-pci-register (location)
  (let ((nic (make-rtl8168 :pci-location location
                           :io-base (pci-io-region location 2 256)
                           :boot-id (current-boot-id)
                           :irq (pci-intr-line location))))
    (setf (rtl8168-worker-thread nic)
          (make-thread (lambda () (rtl8168-worker nic))
                       :name "RTL8168 NIC worker")))
  t)

;;; Register access.

(defun rtl8168-reg/8 (nic reg)
  (pci-io-region/8 (rtl8168-io-base nic) reg))
(defun rtl8168-reg/16 (nic reg)
  (pci-io-region/16 (rtl8168-io-base nic) reg))
(defun rtl8168-reg/32 (nic reg)
  (pci-io-region/32 (rtl8168-io-base nic) reg))

(defun (setf rtl8168-reg/8) (value nic reg)
  (setf (pci-io-region/8 (rtl8168-io-base nic) reg) value))
(defun (setf rtl8168-reg/16) (value nic reg)
  (setf (pci-io-region/16 (rtl8168-io-base nic) reg) value))
(defun (setf rtl8168-reg/32) (value nic reg)
  (setf (pci-io-region/32 (rtl8168-io-base nic) reg) value))

;;; Descriptor field access.

(defun rtl8168-descriptor-flags (base descriptor)
  (sys.int::memref-unsigned-byte-32 (+ base (* descriptor +rtl8168-descriptor-size+) 0) 0))
(defun rtl8168-descriptor-vlan (base descriptor)
  (sys.int::memref-unsigned-byte-32 (+ base (* descriptor +rtl8168-descriptor-size+) 4) 0))
(defun rtl8168-descriptor-address (base descriptor)
  (sys.int::memref-unsigned-byte-64 (+ base (* descriptor +rtl8168-descriptor-size+) 8) 0))

(defun (setf rtl8168-descriptor-flags) (value base descriptor)
  (setf (sys.int::memref-unsigned-byte-32 (+ base (* descriptor +rtl8168-descriptor-size+) 0) 0) value))
(defun (setf rtl8168-descriptor-vlan) (value base descriptor)
  (setf (sys.int::memref-unsigned-byte-32 (+ base (* descriptor +rtl8168-descriptor-size+) 4) 0) value))
(defun (setf rtl8168-descriptor-address) (value base descriptor)
  (setf (sys.int::memref-unsigned-byte-64 (+ base (* descriptor +rtl8168-descriptor-size+) 8) 0) value))

(defun rtl8168-rx-desc-flags (nic descriptor)
  (rtl8168-descriptor-flags (rtl8168-rx-ring-virt nic) descriptor))
(defun rtl8168-rx-desc-vlan (nic descriptor)
  (rtl8168-descriptor-vlan (rtl8168-rx-ring-virt nic) descriptor))
(defun rtl8168-rx-desc-address (nic descriptor)
  (rtl8168-descriptor-address (rtl8168-rx-ring-virt nic) descriptor))

(defun (setf rtl8168-rx-desc-flags) (value nic descriptor)
  (setf (rtl8168-descriptor-flags (rtl8168-rx-ring-virt nic) descriptor) value))
(defun (setf rtl8168-rx-desc-vlan) (value nic descriptor)
  (setf (rtl8168-descriptor-vlan (rtl8168-rx-ring-virt nic) descriptor) value))
(defun (setf rtl8168-rx-desc-address) (value nic descriptor)
  (setf (rtl8168-descriptor-address (rtl8168-rx-ring-virt nic) descriptor) value))

(defun rtl8168-tx-desc-flags (nic descriptor)
  (rtl8168-descriptor-flags (rtl8168-tx-ring-virt nic) descriptor))
(defun rtl8168-tx-desc-vlan (nic descriptor)
  (rtl8168-descriptor-vlan (rtl8168-tx-ring-virt nic) descriptor))
(defun rtl8168-tx-desc-address (nic descriptor)
  (rtl8168-descriptor-address (rtl8168-tx-ring-virt nic) descriptor))

(defun (setf rtl8168-tx-desc-flags) (value nic descriptor)
  (setf (rtl8168-descriptor-flags (rtl8168-tx-ring-virt nic) descriptor) value))
(defun (setf rtl8168-tx-desc-vlan) (value nic descriptor)
  (setf (rtl8168-descriptor-vlan (rtl8168-tx-ring-virt nic) descriptor) value))
(defun (setf rtl8168-tx-desc-address) (value nic descriptor)
  (setf (rtl8168-descriptor-address (rtl8168-tx-ring-virt nic) descriptor) value))

(defun rtl8168-initialize (nic)
  (setf (rtl8168-irq-handler nic) (make-simple-irq (rtl8168-irq nic) (rtl8168-irq-latch nic)))
  (with-pseudo-atomic
    ;; Initialize the device.
    (when (not (eql (rtl8168-boot-id nic) (current-boot-id)))
      ;; Reboot occurred, card no longer exists.
      (return-from rtl8168-initialize))
    (debug-print-line "Initializing RTL8168 at " (rtl8168-pci-location nic) ". IO base " (rtl8168-io-base nic))
    (simple-irq-attach (rtl8168-irq-handler nic))
    (setf (pci-bus-master-enabled (rtl8168-pci-location nic)) t)
    ;; Mask and ack interrupts.
    (setf (rtl8168-reg/16 nic +rtl8168-register-IMR+) #x0000
          (rtl8168-reg/16 nic +rtl8168-register-ISR+) #xFFFF)
    ;; Soft reset NIC by setting the reset bit.
    (setf (rtl8168-reg/8 nic +rtl8168-register-CR+) (ash 1 +rtl8168-CR-RST+))
    ;; Check that the chip has finished the reset. It will automatically clear the reset bit.
    (loop
       repeat 100
       do
         (when (not (logbitp +rtl8168-CR-RST+ (rtl8168-reg/8 nic +rtl8168-register-CR+)))
           (return))
         (sleep 0.01)
       finally
         (debug-print-line "RTL8168 reset timed out!")
         (return-from rtl8168-initialize))
    ;; Read the MAC.
    (loop
       with mac = 0
       for i below 6
       do (setf (ldb (byte 8 (* i 8)) mac) (rtl8168-reg/8 nic (+ +rtl8168-register-ID0+ i)))
       finally
         (setf (rtl8168-mac nic) mac))
    (debug-print-line "RTL8168 MAC address is " (rtl8168-mac nic))
    ;; Allocate TX & RX descriptor rings.
    (setf (values (rtl8168-tx-ring-phys nic)
                  (rtl8168-tx-ring-virt nic))
          (rtl8168-allocate-ring "TX" +rtl8168-n-tx-descriptors+))
    (setf (values (rtl8168-rx-ring-phys nic)
                  (rtl8168-rx-ring-virt nic))
          (rtl8168-allocate-ring "RX" +rtl8168-n-rx-descriptors+))
    ;; And TX & RX bounce buffers.
    (setf (values (rtl8168-tx-bounce-phys nic)
                  (rtl8168-tx-bounce-virt nic))
          (rtl8168-allocate-bounce "TX" +rtl8168-n-tx-descriptors+))
    (setf (values (rtl8168-rx-bounce-phys nic)
                  (rtl8168-rx-bounce-virt nic))
          (rtl8168-allocate-bounce "RX" +rtl8168-n-rx-descriptors+))
    ;; FIXME: Verify that all buffers were allocated.
    ;; Reset the TX ring.
    (dotimes (i +rtl8168-n-tx-descriptors+)
      (setf (rtl8168-tx-desc-flags nic i) 0
            (rtl8168-tx-desc-vlan nic i) 0
            (rtl8168-tx-desc-address nic i) (+ (rtl8168-tx-bounce-phys nic)
                                               (* i (mezzano.supervisor::align-up +rtl8168-mtu+ 128)))))
    (setf (rtl8168-tx-used-count nic) 0
          (rtl8168-tx-current nic) 0
          (rtl8168-tx-tail nic) 0
          (rtl8168-tx-pending nic) '())
    ;; Reset the RX ring.
    (dotimes (i +rtl8168-n-rx-descriptors+)
      (setf (rtl8168-rx-desc-flags nic i) (logior (ash +rtl8168-mtu+ +rtl8168-descriptor-Frame_Length+)
                                                  (ash 1 +rtl8168-descriptor-OWN+)
                                                  (if (eql i (1- +rtl8168-n-rx-descriptors+))
                                                      (ash 1 +rtl8168-descriptor-EOR+)
                                                      0))
            (rtl8168-rx-desc-vlan nic i) 0
            (rtl8168-rx-desc-address nic i) (+ (rtl8168-rx-bounce-phys nic)
                                               (* i (mezzano.supervisor::align-up +rtl8168-mtu+ 128)))))
    (setf (rtl8168-rx-current nic) 0)
    ;; Configure Rx.
    (setf (rtl8168-reg/32 nic +rtl8168-register-RCR+)
          (logior (ash #b111 +rtl8168-RCR-RXFTH+) ; No Rx FIFO threshold. This seems like the safest option to me.
                  (ash #b111 +rtl8168-RCR-MXDMA+) ; Unlimited maximum DMA burst size. PCI related, I don't know anything about this.
                  ;; Accept broadcast packets, packets addressed to us and all packets with a destination address (promiscious mode).
                  (ash 1 +rtl8168-RCR-AB+)
                  (ash 1 +rtl8168-RCR-APM+)
                  (ash 1 +rtl8168-RCR-AAP+)))
    (setf (rtl8168-reg/16 nic +rtl8168-register-RMS+) +rtl8168-mtu+) ; ### not clear if this includes the trailing CRC or not.
    (setf (rtl8168-reg/32 nic +rtl8168-register-RDSAR+) (ldb (byte 32 0) (rtl8168-rx-ring-phys nic))
          (rtl8168-reg/32 nic (+ +rtl8168-register-RDSAR+ 4)) (ldb (byte 32 32) (rtl8168-rx-ring-phys nic)))
    ;; Configure Tx.
    (setf (rtl8168-reg/32 nic +rtl8168-register-TCR+)
          (logior (ash #b11 +rtl8168-TCR-IFG+) ; Standard interframe gap time.
                  (ash #b0 +rtl8168-TCR-IFG2+)
                  (ash #b111 +rtl8168-TCR-MXDMA+))) ; Unlimited maximum DMA burst size. PCI related, I don't know anything about this.
    (setf (rtl8168-reg/8 nic +rtl8168-register-ETThR+) 48) ; I have no idea what this should be. (ceiling mtu 32)
    (setf (rtl8168-reg/32 nic +rtl8168-register-TNPDS+) (ldb (byte 32 0) (rtl8168-tx-ring-phys nic))
          (rtl8168-reg/32 nic (+ +rtl8168-register-TNPDS+ 4)) (ldb (byte 32 32) (rtl8168-tx-ring-phys nic)))
    ;; Enable TX & RX.
    (setf (rtl8168-reg/8 nic +rtl8168-register-CR+) (logior (ash 1 +rtl8168-CR-RE+)
                                                            (ash 1 +rtl8168-CR-TE+)))
    ;; Unmask IRQs.
    (setf (rtl8168-reg/16 nic +rtl8168-register-IMR+) #b1111) ; enable tx error, tx ok, rx error and rx ok.
    (simple-irq-unmask (rtl8168-irq-handler nic)))
  ;; FIXME: Make sure this happens in the same boot. Can't be done in with-pseudo-atomic because
  ;; of allocation.
  (let ((wrapper (make-instance 'rtl8168-network-card :nic nic)))
    (setf (rtl8168-wrapper nic) wrapper)
    (nic:register-network-card wrapper))
  t)

(defun rtl8168-transmit (nic packet)
  (let* ((len (loop for elt in packet
                 summing (length elt)))
         (data (make-array len
                           :element-type '(unsigned-byte 8)))
         (cons (cons data nil)))
    (when (> len +rtl8168-mtu+)
      (error "Packet exceeds MTU."))
    ;; Copy packet into temp buffer.
    (let ((offset 0))
      (dolist (p packet)
        (dotimes (i (length p))
          (setf (aref data offset) (aref p i))
          (incf offset))))
    (with-mutex ((rtl8168-lock nic))
      (setf (cdr cons) (rtl8168-tx-pending nic)
            (rtl8168-tx-pending nic) cons))
    (latch-trigger (rtl8168-irq-latch nic))))

(defun rtl8168-stats (nic)
  (values 0
          0
          0
          0
          0
          0
          0))

(defun rtl8168-worker (nic)
  (when (not (rtl8168-initialize nic))
    (return-from rtl8168-worker))
  (flet ((check-boot ()
           (when (not (eql (rtl8168-boot-id nic) (current-boot-id)))
             (return-from rtl8168-worker))))
    (loop
       ;; Wait for something to happen.
       (simple-irq-unmask (rtl8168-irq-handler nic))
       (latch-wait (rtl8168-irq-latch nic))
       (latch-reset (rtl8168-irq-latch nic))
       ;; Perform receive handling. Remove packets from the RX ring
       ;; until there are none left.
       (loop
          (let ((current (rtl8168-rx-current nic)))
            (with-pseudo-atomic ()
              (check-boot)
              ;; Acknowledge IRQ.
              (setf (rtl8168-reg/16 nic +rtl8168-register-ISR+) #b1111)
              (when (logbitp +rtl8168-descriptor-OWN+ (rtl8168-rx-desc-flags nic current))
                ;; Current descriptor owned by the NIC, no more packets to receive.
                ;; Break out of the RX loop.
                (return)))
            #+(or)(debug-print-line "RTL8168 receive on " current)
            ;; Receiving one packet.
            ;; Allocate a buffer. Can't be done while pseudo-atomic, hence the dropping in & out.
            (let ((buffer (make-array +rtl8168-mtu+ :element-type '(unsigned-byte 8))))
              (with-pseudo-atomic ()
                (check-boot)
                ;; Copy the packet into the receive buffer.
                (let ((address (+ (rtl8168-rx-bounce-virt nic)
                                  (* current (mezzano.supervisor::align-up +rtl8168-mtu+ 128)))))
                  (dotimes (i (min +rtl8168-mtu+
                                   (ldb (byte 16 +rtl8168-descriptor-Frame_Length+)
                                        (rtl8168-rx-desc-flags nic current))))
                    (setf (aref buffer i) (sys.int::memref-unsigned-byte-8 address i))))
                ;; Reset the descriptor.
                (setf (rtl8168-rx-desc-flags nic current) (logior (ash +rtl8168-mtu+ +rtl8168-descriptor-Frame_Length+)
                                                                  (ash 1 +rtl8168-descriptor-OWN+)
                                                                  (if (eql current (1- +rtl8168-n-rx-descriptors+))
                                                                      (ash 1 +rtl8168-descriptor-EOR+)
                                                                      0))
                      (rtl8168-rx-desc-vlan nic current) 0))
              (nic:device-received-packet (rtl8168-wrapper nic) buffer))
            ;; Advance Current.
            (setf (rtl8168-rx-current nic) (rem (1+ current) +rtl8168-n-rx-descriptors+))))
       ;; Transmit handling.
       ;; Recover free descriptors.
       (with-pseudo-atomic ()
         (check-boot)
         (loop
            (when (eql (rtl8168-tx-current nic) (rtl8168-tx-tail nic))
              ;; All pending descriptors processed.
              (return))
            (when (logbitp +rtl8168-descriptor-OWN+ (rtl8168-tx-desc-flags nic (rtl8168-tx-tail nic)))
              ;; Reached the descriptor that the card is processing.
              #+(or)(debug-print-line "RTL8168 still processing " (rtl8168-tx-tail nic))
              (return))
            (decf (rtl8168-tx-used-count nic))
            (setf (rtl8168-tx-tail nic) (rem (1+ (rtl8168-tx-tail nic)) +rtl8168-n-tx-descriptors+))
            #+(or)(debug-print-line "RTL8168 transmitted packet on descriptor " (rtl8168-tx-tail nic))))
       ;; Send pending packets.
       (loop
          (when (eql (rtl8168-tx-used-count nic) +rtl8168-n-tx-descriptors+)
            ;; Don't transmit when there are no free descriptors.
            #+(or)(debug-print-line "RTL8168 not transmitting - no free descriptors.")
            (return))
          (let ((to-send (with-mutex ((rtl8168-lock nic))
                           (pop (rtl8168-tx-pending nic))))
                (current (rtl8168-tx-current nic)))
            (when (not to-send)
              (return))
            #+(or)(debug-print-line "RTL8168 transmitting packet " to-send " on descriptor " current)
            (with-pseudo-atomic ()
              (check-boot)
              ;; Copy packet to buffer.
              (let ((address (+ (rtl8168-tx-bounce-virt nic)
                                (* current (mezzano.supervisor::align-up +rtl8168-mtu+ 128)))))
                (dotimes (i (min +rtl8168-mtu+
                                 (length to-send)))
                  (setf (sys.int::memref-unsigned-byte-8 address i) (aref to-send i))))
              ;; Configure the descriptor.
              (setf (rtl8168-tx-desc-flags nic current) (logior (ash (min +rtl8168-mtu+ (length to-send))
                                                                     +rtl8168-descriptor-Frame_Length+)
                                                                (ash 1 +rtl8168-descriptor-OWN+)
                                                                (if (eql current (1- +rtl8168-n-tx-descriptors+))
                                                                    (ash 1 +rtl8168-descriptor-EOR+)
                                                                    0)
                                                                ;; This is the first & last segment.
                                                                (ash 1 +rtl8168-descriptor-FS+)
                                                                (ash 1 +rtl8168-descriptor-LS+))
                    (rtl8168-tx-desc-vlan nic current) 0)
              ;; Notify the card.
              (setf (rtl8168-reg/8 nic +rtl8168-register-TPPoll+) (ash 1 +rtl8168-TPPoll-NPQ+)))
            ;; One used.
            (incf (rtl8168-tx-used-count nic))
            ;; Advance current.
            (setf (rtl8168-tx-current nic) (rem (1+ current) +rtl8168-n-tx-descriptors+)))))))

(defun rtl8168-allocate-ring (name size)
  (let* ((descriptor-frame (or (mezzano.supervisor::allocate-physical-pages
                                (ceiling (* size +rtl8168-descriptor-size+)
                                         mezzano.supervisor::+4k-page-size+))
                               (return-from rtl8168-allocate-ring
                                 (progn
                                   (debug-print-line "Unable to allocate memory for RTL8168 " name " descriptor ring.")
                                   nil))))
         (phys (* descriptor-frame mezzano.supervisor::+4k-page-size+))
         (virt (mezzano.supervisor::convert-to-pmap-address phys)))
    (dotimes (i (* +rtl8168-n-tx-descriptors+ +rtl8168-descriptor-size+))
      (setf (sys.int::memref-unsigned-byte-8 virt i) 0))
    (debug-print-line "RTL8168 " name " ring at " phys)
    (values phys virt)))

(defun rtl8168-allocate-bounce (name size)
  (let* ((descriptor-frame (or (mezzano.supervisor::allocate-physical-pages
                                (ceiling (* size (mezzano.supervisor::align-up +rtl8168-mtu+ 128))
                                         mezzano.supervisor::+4k-page-size+))
                               (return-from rtl8168-allocate-bounce
                                 (progn
                                   (debug-print-line "Unable to allocate memory for RTL8168 " name " bounce buffer.")
                                   nil))))
         (phys (* descriptor-frame mezzano.supervisor::+4k-page-size+))
         (virt (mezzano.supervisor::convert-to-pmap-address phys)))
    (debug-print-line "RTL8168 " name " bounce buffer at " phys)
    (values phys virt)))

(defmethod nic:mac-address ((nic rtl8168-network-card))
  (rtl8168-mac (slot-value nic 'nic)))

(defmethod nic:statistics ((nic rtl8168-network-card))
  (rtl8168-stats (slot-value nic 'nic)))

(defmethod nic:mtu ((nic rtl8168-network-card))
  +rtl8168-mtu+)

(defmethod nic:device-transmit-packet ((nic rtl8168-network-card) packet)
  (rtl8168-transmit (slot-value nic 'nic) packet))

(define-pci-driver rtl8168 rtl8168-pci-register
  ((#x10EC #x8168))
  ())
