;;; RTL8139 driver.

(defpackage #:system.rtl8139
  (:use #:cl #:sys.net #:system))

(in-package #:system.rtl8139)

(defconstant +rtl8139-id0+        #x00 "ID/MAC register (0 of 6).")
(defconstant +rtl8139-multicast0+ #x08 "Multicast register (0 of 8).")
(defconstant +rtl8139-tsd0+       #x10 "Transmit status (0 of 4).")
(defconstant +rtl8139-tdad0+      #x20 "Transmit start address (0 of 4).")
(defconstant +rtl8139-rbstart+    #x30 "Rx buffer start address.")
(defconstant +rtl8139-erbcr+      #x34 "Early Rx byte count register.")
(defconstant +rtl8139-ersr+       #x36 "Early Rx status register.")
(defconstant +rtl8139-command+    #x37 "Command register.")
(defconstant +rtl8139-capr+       #x38 "Current address of packet read (16 bytes behind).")
(defconstant +rtl8139-cbr+        #x3A "Current buffer address.")
(defconstant +rtl8139-imr+        #x3C "Interrupt mask register.")
(defconstant +rtl8139-isr+        #x3E "Interrupt status register.")
(defconstant +rtl8139-tcr+        #x40 "Tx configuration register.")
(defconstant +rtl8139-rcr+        #x44 "Rx configuration register.")
(defconstant +rtl8139-tctr+       #x48 "Timer count register.")
(defconstant +rtl8139-mpc+        #x4C "Missed packet counter.")
(defconstant +rtl8139-9346cr+     #x50 "93C46 (93C56) command register.")
(defconstant +rtl8139-config0+    #x51)
(defconstant +rtl8139-config1+    #x52)
(defconstant +rtl8139-timerint+   #x54 "Timer interrupt register.")
(defconstant +rtl8139-msr+        #x58 "Media status register.")
(defconstant +rtl8139-config3+    #x59)
(defconstant +rtl8139-config4+    #x5A)
(defconstant +rtl8139-mulint+     #x5C)
(defconstant +rtl8139-rerid+      #x5E "PCI revision ID.")
(defconstant +rtl8139-tsad+       #x50 "Transmit staus of all descriptors.")
(defconstant +rtl8139-bmcr+       #x62 "Basic mode control register.")
(defconstant +rtl8139-bmsr+       #x64 "Basic mode status register.")
(defconstant +rtl8139-anar+       #x66 "Auto-negotiation advertisment register.")
(defconstant +rtl8139-anlpar+     #x68 "Auto-negotiation link partner register.")
(defconstant +rtl8139-aner+       #x6A "Auto-negotiation expansion register.")
(defconstant +rtl8139-dis+        #x6C "Disconnect counter.")
(defconstant +rtl8139-fcsc+       #x6E "False carrier sense counter.")
(defconstant +rtl8139-nwaytr+     #x70 "N-way test register.")
(defconstant +rtl8139-rec+        #x72 "Rx ER counter.")
(defconstant +rtl8139-cscr+       #x74 "CS configuration register.")
(defconstant +rtl8139-phy1-parm+  #x78)
(defconstant +rtl8139-tw-parm+    #x7C)
(defconstant +rtl8139-phy2-parm+  #x80)
(defconstant +rtl8139-tdokladdr+  #x82)
(defconstant +rtl8139-crc0+       #x84)
(defconstant +rtl8139-wakeup0+    #x8C)
(defconstant +rtl8139-lsbcrc0+    #xCC)
(defconstant +rtl8139-flash+      #xD4)
(defconstant +rtl8139-config5+    #xD8)
(defconstant +rtl8139-miir+       #xFC)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun bit* (x) (ash 1 x)))

(defconstant +header-mar+  (bit* 15) "Multicast address received.")
(defconstant +header-pam+  (bit* 14) "Physical address received.")
(defconstant +header-bar+  (bit* 13) "Broadcast address received.")
(defconstant +header-ise+  (bit* 5) "Invalid symbol error.")
(defconstant +header-runt+ (bit* 4) "Runt packet.")
(defconstant +header-long+ (bit* 3) "Long packet.")
(defconstant +header-crc+  (bit* 2) "CRC error.")
(defconstant +header-faet+ (bit* 1) "Frame alignment error.")
(defconstant +header-rok+  (bit* 0) "Receive OK.")

(defconstant +tsd-crs+     (bit* 31) "Carrier sense lost.")
(defconstant +tsd-tabt+    (bit* 30) "Transmit abort.")
(defconstant +tsd-owc+     (bit* 29) "Out of window collision.")
(defconstant +tsd-cdh+     (bit* 28) "CD heart beat.")
(defconstant +tsd-ncc0+    (bit* 27) "Number of collisions.")
(defconstant +tsd-ertxth0+ (bit* 21) "Early Tx threshold.")
(defconstant +tsd-tok+     (bit* 15) "Transmit OK.")
(defconstant +tsd-tun+     (bit* 14) "Transmit FIFO underrun.")
(defconstant +tsd-own+     (bit* 13) "Set when Tx DMA completes.")
(defparameter +tsd-size+ (byte 13 0) "Descriptor size (octets).")

(defconstant +command-rst+  (bit* 4) "Reset.")
(defconstant +command-re+   (bit* 3) "Receiver enable.")
(defconstant +command-te+   (bit* 2) "Transmitter enable.")
(defconstant +command-bufe+ (bit* 0) "Buffer empty.")

(defconstant +imr-serr+    (bit* 15) "System error.")
(defconstant +imr-timeout+ (bit* 14))
(defconstant +imr-lenchg+  (bit* 13) "Cable length change.")
(defconstant +imr-fovw+    (bit* 6) "Rx FIFO overflow.")
(defconstant +imr-pun/linkchg+ (bit* 5) "Packet underrun/link change.")
(defconstant +imr-rbo+ (bit* 4) "Rx buffer overflow.")
(defconstant +imr-ter+ (bit* 3) "Tx error.")
(defconstant +imr-tok+ (bit* 2) "Tx OK.")
(defconstant +imr-rer+ (bit* 1) "Rx error.")
(defconstant +imr-rok+ (bit* 0) "Rx OK.")

(defconstant +rcr-rer8+ (bit* 16) "Receive error packets larger than 8 bytes.")
(defparameter +rcr-rblen+ (byte 2 11) "Rx ring buffer size.")
(defconstant +rcr-8k-buffer+ #b00)
(defconstant +rcr-16k-buffer+ #b01)
(defconstant +rcr-32k-buffer+ #b10)
(defconstant +rcr-64k-buffer+ #b11)
(defconstant +rcr-wrap+ (bit* 7) "Rx ring buffer wrapping.
When set, the Rx buffer must be 1.5k larger. Invalid when using a 64k buffer size.")
(defconstant +rcr-9356sel+ (bit* 6) "EEPROM type.")
(defconstant +rcr-aer+  (bit* 5) "Accept error packets.")
(defconstant +rcr-ar+   (bit* 4) "Accept runt packets.")
(defconstant +rcr-ab+   (bit* 3) "Accept broadcast packets.")
(defconstant +rcr-am+   (bit* 2) "Accept multicast packets.")
(defconstant +rcr-apm+  (bit* 1) "Accept physical match packets.")
(defconstant +rcr-aap+  (bit* 0) "Accept physical address packets.")

(defparameter +config0-bs+ (byte 3 0))
(defconstant +config0-no-boot-rom+   #b000)
(defconstant +config0-8k-boot-rom+   #b001)
(defconstant +config0-16k-boot-rom+  #b010)
(defconstant +config0-32k-boot-rom+  #b011)
(defconstant +config0-64k-boot-rom+  #b100)
(defconstant +config0-128k-boot-rom+ #b101)

(defconstant +config1-drvload+ (bit* 5) "Driver load.")
(defconstant +config1-lwact+   (bit* 4) "LWAKE active mode.")
(defconstant +config1-memmap+  (bit* 3) "Registers are mapped into PCI memory space.")
(defconstant +config1-iomap+   (bit* 2) "Registers are mapped into PCI I/O space.")
(defconstant +config1-vpd+     (bit* 1) "Enable vital product data.")
(defconstant +config1-pmen+    (bit* 0) "Power management enable.")

(defconstant +msr-txfce+      (bit* 7) "Tx flow control enable.")
(defconstant +msr-rxfce+      (bit* 6) "Rx flow control enable.")
(defconstant +msr-aux-status+ (bit* 4) "Aux. power present status.")
(defconstant +msr-speed-10+   (bit* 3) "Media speed.")
(defconstant +msr-linkb+      (bit* 2) "Inverse of link status.")
(defconstant +msr-txpf+       (bit* 1) "Transmit pause flag.")
(defconstant +msr-rxpf+       (bit* 0) "Receive pause flag.")

(defconstant +rtl8139-tx-descriptors+ 4)
(defconstant +rtl8139-tx-buffer-size+ #x700 "Maximum Tx packet size (1.75k).")
(defconstant +rtl8139-rx-buffer-size+ (+ #x2000 16) "Rx buffer size.")

(defparameter *rtl8139-pciids*
  '((#x10EC #x8139)))

(defclass rtl8139 ()
  ((pci-device :initarg :pci-device)
   (tx-buffer)
   (tx-address)
   (last-tx-descriptor :initform 0)
   (next-tx-descriptor :initform 0)
   (free-tx-descriptors :initform +rtl8139-tx-descriptors+)
   (rx-buffer)
   (rx-address)
   (rx-offset :initform 0)
   (rx-process)
   (signal-cons)
   (isr)
   (ioar)
   (memar)
   (mac)))

(defmethod print-object ((object rtl8139) stream)
  (let ((mac (slot-value object 'mac)))
    (print-unreadable-object (object stream :type t :identity (not mac))
      (format t "~2,'0X:~2,'0X:~2,'0X:~2,'0X:~2,'0X:~2,'0X"
              (aref mac 0) (aref mac 1) (aref mac 2) (aref mac 3) (aref mac 4) (aref mac 5)))))

(defmethod ethernet-mac ((nic rtl8139))
  (slot-value nic 'mac))

;;; TODO: replace with some kind of IO register layout thing.
(defun rtl8139-reg/8 (card reg)
  (io-port/8 (+ (slot-value card 'ioar) reg)))
(defun rtl8139-reg/16 (card reg)
  (io-port/16 (+ (slot-value card 'ioar) reg)))
(defun rtl8139-reg/32 (card reg)
  (io-port/32 (+ (slot-value card 'ioar) reg)))

(defun (setf rtl8139-reg/8) (value card reg)
  (setf (io-port/8 (+ (slot-value card 'ioar) reg)) value))
(defun (setf rtl8139-reg/16) (value card reg)
  (setf (io-port/16 (+ (slot-value card 'ioar) reg)) value))
(defun (setf rtl8139-reg/32) (value card reg)
  (setf (io-port/32 (+ (slot-value card 'ioar) reg)) value))

(defun init-rtl8139 (card)
  "Initialize the RTL8139 hardware."
  ;; Enable the card by clearing CONFIG1.
  (setf (rtl8139-reg/8 card +rtl8139-config1+) 0)
  ;; Issue a reset command.
  (setf (rtl8139-reg/8 card +rtl8139-command+) +command-rst+)
  ;; Wait for the reset to complete.
  ;; TODO: Use process-wait-with-timeout
  (do () ((not (logtest (rtl8139-reg/8 card +rtl8139-command+) +command-rst+))))
  ;; Read the MAC address.
  (dotimes (i 6)
    (setf (aref (slot-value card 'mac) i) (rtl8139-reg/8 card (+ +rtl8139-id0+ i))))
  ;; Set the RX buffer start address.
  (setf (rtl8139-reg/32 card +rtl8139-rbstart+) (slot-value card 'rx-address))
  ;; Enable the ROK, RER, TOK and TER interrupts.
  (setf (rtl8139-reg/16 card +rtl8139-imr+) (logior +imr-rok+ +imr-rer+ +imr-tok+ +imr-ter+))
  ;; Configure the RX buffer (RCR).
  ;; Accept: Packets to us (0x01),
  ;;         Packets partially matched to us (0x02),
  ;;         Multicast packets (0x04),
  ;;         Broadcast packets (0x08).
  ;; Don't accept runt or broken packets.
  ;; WRAP is disabled (bit 7).
  ;; RX buffer length is 8KB+16.
  (setf (rtl8139-reg/32 card +rtl8139-rcr+) (logior +rcr-aap+ +rcr-apm+
                                                    +rcr-am+ +rcr-ab+
                                                    (dpb +rcr-8k-buffer+ +rcr-rblen+ 0)))
  ;; Enable TX and RX.
  (setf (rtl8139-reg/8 card +rtl8139-command+) (logior +command-re+ +command-te+)))

;; FIXME: This needs better locking.
(defun rtl8139-tx (card packet-descriptor)
  ;; Wait for a free TX descriptor.
  #+nil(format t "Transmitting packet... ~S descriptors free.~%" (slot-value card 'free-tx-descriptors))
  (sys.int::process-wait "RTL8139 TX" (lambda () (/= (slot-value card 'free-tx-descriptors) 0)))
  ;; Copy the packet into the TX buffer.
  (unless (= (slot-value card 'free-tx-descriptors) 0)
    (let ((tx-descriptor (slot-value card 'next-tx-descriptor))
	  (tx-buffer (slot-value card 'tx-buffer))
	  (tx-address (slot-value card 'tx-address))
	  (packet-size (packet-length packet-descriptor)))
      #+nil(format t "Transmitting ~D byte packet using descriptor ~S (phys ~X)~%"
              packet-size tx-descriptor (+ tx-address (* tx-descriptor +rtl8139-tx-buffer-size+)))
      (when (>= packet-size +rtl8139-tx-buffer-size+)
	(error "RTL8139: packet too large. ~S" packet-descriptor))
      (copy-packet tx-buffer packet-descriptor (* tx-descriptor +rtl8139-tx-buffer-size+))
      ;; Avoid sending runt packets (does the RTL8139 do this automatically?)
      (when (< packet-size 64)
        (dotimes (i (- 64 packet-size))
          (setf (aref tx-buffer (+ (* tx-descriptor +rtl8139-tx-buffer-size+) packet-size i)) 0))
        (setf packet-size 64))
      ;; Transmit!
      (setf (rtl8139-reg/32 card (+ +rtl8139-tdad0+ (* tx-descriptor 4)))
	    (+ tx-address (* tx-descriptor +rtl8139-tx-buffer-size+))
	    (rtl8139-reg/32 card (+ +rtl8139-tsd0+ (* tx-descriptor 4))) packet-size)
      (decf (slot-value card 'free-tx-descriptors))
      (if (= tx-descriptor (1- +rtl8139-tx-descriptors+))
	  (setf (slot-value card 'next-tx-descriptor) 0)
	  (incf (slot-value card 'next-tx-descriptor))))))

(defmethod sys.net:transmit-packet ((interface rtl8139) packet-descriptor)
  (rtl8139-tx interface packet-descriptor))

(sys.int::define-interrupt-handler rtl8139-interrupt (io-base signal-cons)
  ;; Copy the current ISR state into the signal cons.
  (setf (car signal-cons) (io-port/16 (+ io-base +rtl8139-isr+)))
  ;; Write #xFFFF to ISR to clear the interrupt state.
  (setf (io-port/16 (+ io-base +rtl8139-isr+)) #xFFFF))

(defun ub16ref/le (vector index)
  (logior (aref vector index)
	  (ash (aref vector (1+ index)) 8)))

(defun rtl8139-process (card)
  (let ((signal-cons (slot-value card 'signal-cons)))
    (loop (sys.int::process-wait "RTL8139 interrupt"
                                 (lambda ()
                                   (car signal-cons)))
       (setf (car signal-cons) nil)
       #+nil(format t "In RTL8139 handler...~%")
       (do ((tx (slot-value card 'last-tx-descriptor)))
           ((or (eql (slot-value card 'free-tx-descriptors) +rtl8139-tx-descriptors+)
                (let ((tsd (rtl8139-reg/32 card (+ +rtl8139-tsd0+ (* tx 4)))))
                  (not (and (logtest tsd +tsd-own+)
                            (logtest tsd +tsd-tok+))))))
         ;; Release the buffer, both OWN and TOK set.
         #+nil(format t "TX descriptor ~S reports successful send.~%" tx)
         (if (= tx (1- +rtl8139-tx-descriptors+))
             (setf tx 0)
             (incf tx))
         (setf (slot-value card 'last-tx-descriptor) tx)
         (incf (slot-value card 'free-tx-descriptors)))
       #+nil(format t "Finished TX work.~%")
       (do ()
           ;; Test the buffer-empty bit in the command register.
           ((logtest (rtl8139-reg/8 card +rtl8139-command+) +command-bufe+))
         (let* ((rx-buffer (slot-value card 'rx-buffer))
                (rx-offset (slot-value card 'rx-offset))
                (header (ub16ref/le rx-buffer (+ rx-offset 0)))
                ;; Includes the trailing CRC32.
                (total-length (ub16ref/le rx-buffer (+ rx-offset 2)))
                (length (- total-length 4)))
           ;; Check the ROK bit in the header and make sure that the packet isn't
           ;; longer than the maximum ethernet length.
           (when (or (zerop (logand header +header-rok+))
                     (> length 1514))
             (warn "RTL8139: RX failure. Header: ~4,'0X  Length: ~D" header total-length)
             ;; FIXME: should reset the card.
             (setf (rtl8139-reg/16 card +rtl8139-capr+) #xFFF0)
             (return))
           (let ((packet (make-array length :element-type '(unsigned-byte 8))))
             (cond ((>= (+ rx-offset 4 length) (- +rtl8139-rx-buffer-size+ 16))
                    ;; Packet wraps, fall back on the slow code.
                    (do ((i 0 (1+ i))
                         (j (+ rx-offset 4) (1+ j)))
                        ((= i length))
                      (when (>= j (- +rtl8139-rx-buffer-size+ 16))
                        (setf j 0))
                      (setf (aref packet i) (aref rx-buffer j))))
                   (t (sys.int::%fast-copy (1+ (sys.int::lisp-object-address packet))
                                           (+ (slot-value card 'rx-address) (+ rx-offset 4) #x8000000000)
                                           length)))
             ;; Update the RX buffer offset, must be aligned to 4 bytes.
             (setf rx-offset (+ rx-offset
                                (if (logtest total-length 3)
                                    (logand (+ total-length 4) -4)
                                    total-length)
                                4))
             (when (>= rx-offset (- +rtl8139-rx-buffer-size+ 16))
               (decf rx-offset (- +rtl8139-rx-buffer-size+ 16)))
             (setf (slot-value card 'rx-offset) rx-offset
                   ;; CAPR is 16 bytes behind, for some reason.
                   ;; Negative CAPRs?
                   (rtl8139-reg/16 card +rtl8139-capr+) (logand (- rx-offset 16) #xFFFF))
             #+nil(format t "Received ~S byte packet. rx-offset is ~X (CAPR is ~X)~%" (length packet) rx-offset (- rx-offset 16))
             (sys.net:receive-packet card packet))))
       #+nil(format t "Finished RX work.~%"))
    (error "NO NO NO.")))

(defmethod initialize-instance :after ((card rtl8139) &key pci-device)
  (multiple-value-bind (tx-buffer tx-address)
      (sys.int::allocate-dma-buffer (* +rtl8139-tx-buffer-size+ +rtl8139-tx-descriptors+))
    (multiple-value-bind (rx-buffer rx-address)
	(sys.int::allocate-dma-buffer +rtl8139-rx-buffer-size+)
      (format t "Tx buffer base: ~X  Rx buffer base: ~X~%" tx-address rx-address)
      (setf (slot-value card 'rx-process) (sys.int::make-process "RTL8139 receive process")
            (slot-value card 'signal-cons) (sys.int::cons-in-area nil nil :static)
            (slot-value card 'tx-buffer) tx-buffer
	    (slot-value card 'tx-address) tx-address
	    (slot-value card 'rx-buffer) rx-buffer
	    (slot-value card 'rx-address) rx-address
	    (slot-value card 'ioar) (logand (sys.int::pci-bar pci-device 0) -4)
	    (slot-value card 'memar) (logand (sys.int::pci-bar pci-device 1) -4)
            (slot-value card 'isr) (sys.int::make-interrupt-handler 'rtl8139-interrupt
                                                                    (slot-value card 'ioar)
                                                                    (slot-value card 'signal-cons))
            (slot-value card 'mac) (make-array 6 :element-type '(unsigned-byte 8)))
      (sys.int::process-preset (slot-value card 'rx-process)
                               #'rtl8139-process card)
      (sys.int::process-enable (slot-value card 'rx-process))
      (init-rtl8139 card)
      (setf (sys.int::isa-pic-interrupt-handler (sys.int::pci-irq-line pci-device)) (slot-value card 'isr)
            (sys.int::isa-pic-irq-mask (sys.int::pci-irq-line pci-device)) nil))))

(defun rtl8139-probe (pci-device)
  (format t "Detected RTL8139 at ~2,'0X:~X:~X~%"
          (sys.int::pci-device-bus pci-device)
          (sys.int::pci-device-device pci-device)
          (sys.int::pci-device-function pci-device))
  (format t "IOAR: ~8,'0X  MEMAR: ~8,'0X  IRQ:~D~%"
	  (sys.int::pci-bar pci-device 0) (sys.int::pci-bar pci-device 1) (sys.int::pci-irq-line pci-device))
  (let* ((card (make-instance 'rtl8139 :pci-device pci-device))
         (mac (slot-value card 'mac)))
    (format t "MAC address is: ~2,'0X:~2,'0X:~2,'0X:~2,'0X:~2,'0X:~2,'0X~%"
            (aref mac 0) (aref mac 1) (aref mac 2) (aref mac 3) (aref mac 4) (aref mac 5))
    (sys.net:register-nic card)))

(defun rtl8139-test (card)
  (format t "Transmitting test packet on card ~S~%" card)
  (let* ((mac (slot-value card 'mac))
         (packet (vector #xFF #xFF #xFF #xFF #xFF #xFF ; destination, broadcast
                         (aref mac 0) (aref mac 1) (aref mac 2) (aref mac 3) (aref mac 4) (aref mac 5) ; source
                         #x08 #x06 ; ethertype arp
                         #x00 #x00 ; hardware type ethernet
                         #x08 #x00 ; protocol type ip
                         #x06 ; hardware size
                         #x04 ; protocol size
                         #x00 #x01 ; request
                         (aref mac 0) (aref mac 1) (aref mac 2) (aref mac 3) (aref mac 4) (aref mac 5) ; sender mac
                         #x0A #x00 #x02 #x0F ; sender ip (qemu user client)
                         #x00 #x00 #x00 #x00 #x00 #x00 ; target mac
                         #x0A #x00 #x02 #x02)) ; target ip (qemu gateway)
         (packet-descriptor (list packet)))
    (rtl8139-tx card packet-descriptor)))

(sys.int::pci-register-driver *rtl8139-pciids* 'rtl8139-probe)
