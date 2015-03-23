;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;; Layout of CONFIG-ADDRESS register:
;; 31|30      24|23      16|15      11|10      8|7      2|1|0|
;; |E| Reserved |    Bus   |  Device  |Function |Register|0|0|
;; E = Enable bit

(defconstant +pci-config-address+ #x0CF8)
(defconstant +pci-config-data+    #x0CFC)

(defconstant +pci-config-vendorid+      #x00)
(defconstant +pci-config-deviceid+      #x02)
(defconstant +pci-config-command+       #x04)
(defconstant +pci-config-status+        #x06)
(defconstant +pci-config-revid+         #x08)
(defconstant +pci-config-classcode+     #x09)
(defconstant +pci-config-cachelinesz+   #x0C)
(defconstant +pci-config-latency-timer+ #x0D)
(defconstant +pci-config-hdr-type+      #x0E)
(defconstant +pci-config-bist+          #x0F)
(defconstant +pci-config-bar-start+     #x10)
(defconstant +pci-config-cardbus-cis+   #x28)
(defconstant +pci-config-subvendorid+   #x2C)
(defconstant +pci-config-subdeviceid+   #x2E)
(defconstant +pci-config-ex-rom-base+   #x30)
(defconstant +pci-config-capabilities+  #x34)
(defconstant +pci-config-intr-line+     #x3C)
(defconstant +pci-config-intr-pin+      #x3D)
(defconstant +pci-config-min-gnt+       #x3E)
(defconstant +pci-config-max-lat+       #x3F)

(defconstant +pci-bridge-htype+             #x01)
(defconstant +pci-bridge-primary-bus+       #x18)
(defconstant +pci-bridge-secondary-bus+     #x19)
(defconstant +pci-bridge-subordinate-bus+   #x1A)
(defconstant +pci-bridge-secondary-latency+ #x1B)
(defconstant +pci-bridge-io-base+           #x1C)
(defconstant +pci-bridge-io-limit+          #x1D)
(defconstant +pci-bridge-secondary-status+  #x1E)

(defconstant +pci-command-bus-master+ 2)

(defvar *pci-config-lock*)

(defun make-pci-location (bus device function)
  (declare (type (integer 0 255) bus register)
	   (type (integer 0 31) device)
	   (type (integer 0 7) function))
  (logior #x80000000
	  (ash bus 16)
	  (ash device 11)
	  (ash function 8)))

(defun pci-set-config-address (location register)
  (setf (system:io-port/32 +pci-config-address+) (logior location
                                                  (logand register #b11111100))))

(defun pci-config/8 (location register)
  (without-interrupts
    (with-symbol-spinlock (*pci-config-lock*)
      (pci-set-config-address location register)
      (system:io-port/8 (+ +pci-config-data+ (logand register #b11))))))

(defun pci-config/16 (location register)
  (when (logtest register #b01)
    (error "Misaligned PCI register ~S." register))
  (without-interrupts
    (with-symbol-spinlock (*pci-config-lock*)
      (pci-set-config-address location register)
      (system:io-port/16 (+ +pci-config-data+ (logand register #b10))))))

(defun pci-config/32 (location register)
  (when (logtest register #b11)
    (error "Misaligned PCI register ~S." register))
  (without-interrupts
    (with-symbol-spinlock (*pci-config-lock*)
      (pci-set-config-address location register)
      (system:io-port/32 +pci-config-data+))))

(defun (setf pci-config/8) (value location register)
  (without-interrupts
    (with-symbol-spinlock (*pci-config-lock*)
      (pci-set-config-address location register)
      (setf (system:io-port/8 (+ +pci-config-data+ (logand register #b11))) value))))

(defun (setf pci-config/16) (value location register)
  (when (logtest register #b01)
    (error "Misaligned PCI register ~S." register))
  (without-interrupts
    (with-symbol-spinlock (*pci-config-lock*)
      (pci-set-config-address location register)
      (setf (system:io-port/16 (+ +pci-config-data+ (logand register #b10))) value))))

(defun (setf pci-config/32) (value location register)
  (when (logtest register #b11)
    (error "Misaligned PCI register ~S." register))
  (without-interrupts
    (with-symbol-spinlock (*pci-config-lock*)
      (pci-set-config-address location register)
      (setf (system:io-port/32 +pci-config-data+) value))))

(defun pci-base-class (location)
  (ldb (byte 8 24) (pci-config/32 location +pci-config-revid+)))

(defun pci-sub-class (location)
  (ldb (byte 8 16) (pci-config/32 location +pci-config-revid+)))

(defun pci-programming-interface (location)
  (ldb (byte 8 8) (pci-config/32 location +pci-config-revid+)))

(defun pci-bar (location bar)
  (pci-config/32 location (+ +pci-config-bar-start+ (* bar 4))))

(defun pci-io-region (location bar size)
  (let ((address (pci-bar location bar)))
    (when (not (logbitp 0 location))
      (map-physical-memory (logand address (lognot #b1111)) size "PCI MMIO"))
    address))

(defun pci-intr-line (location)
  (pci-config/8 location +pci-config-intr-line+))

(defun pci-io-region/8 (location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (system:io-port/8 (+ (logand location (lognot #b11)) offset))
      ;; MMIO.
      (sys.int::memref-unsigned-byte-8 (+ +physical-map-base+
                                          (logand location (lognot #b1111))
                                          offset)
                                       0)))

(defun pci-io-region/16 (location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (system:io-port/16 (+ (logand location (lognot #b11)) offset))
      ;; MMIO.
      (sys.int::memref-unsigned-byte-16 (+ +physical-map-base+
                                           (logand location (lognot #b1111))
                                           offset)
                                        0)))

(defun pci-io-region/32 (location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (system:io-port/32 (+ (logand location (lognot #b11)) offset))
      ;; MMIO.
      (sys.int::memref-unsigned-byte-32 (+ +physical-map-base+
                                           (logand location (lognot #b1111))
                                           offset)
                                       0)))

(defun (setf pci-io-region/8) (value location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (setf (system:io-port/8 (+ (logand location (lognot #b11)) offset)) value)
      ;; MMIO.
      (setf (sys.int::memref-unsigned-byte-8 (+ +physical-map-base+
                                                (logand location (lognot #b1111))
                                                offset)
                                             0)
            value)))

(defun (setf pci-io-region/16) (value location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (setf (system:io-port/16 (+ (logand location (lognot #b11)) offset)) value)
      ;; MMIO.
      (setf (sys.int::memref-unsigned-byte-16 (+ +physical-map-base+
                                                 (logand location (lognot #b1111))
                                                 offset)
                                              0)
            value)))

(defun (setf pci-io-region/32) (value location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (setf (system:io-port/32 (+ (logand location (lognot #b11)) offset)) value)
      ;; MMIO.
      (setf (sys.int::memref-unsigned-byte-32 (+ +physical-map-base+
                                                 (logand location (lognot #b1111))
                                                 offset)
                                              0)
            value)))

(defun pci-scan (bus)
  (dotimes (device 32)
    ;; High bit of the header type specifies if a device is multifunction.
    (let ((multifunction (eql (logand (pci-config/8 (make-pci-location bus device 0) +pci-config-hdr-type+) #x80) #x80)))
      (dotimes (function (if multifunction 8 1))
	(let* ((location (make-pci-location bus device function))
               (vendor-id (pci-config/16 location +pci-config-vendorid+))
               (device-id (pci-config/16 location +pci-config-deviceid+))
               (base-class-code (pci-base-class location))
               (sub-class-code (pci-sub-class location))
               (programming-interface (pci-programming-interface location)))
	  (unless (or (eql vendor-id #xFFFF) (eql vendor-id 0))
            (debug-print-line "PCI:" bus ":" device ":" function " " vendor-id ":" device-id " " base-class-code ":" sub-class-code ":" programming-interface " "
                              (pci-config/8 location +pci-config-revid+))
            (cond ((and (eql vendor-id #x1AF4)
                        (<= #x1000 device-id #x103F))
                   ;; Some kind of virtio-device.
                   (virtio-pci-register location))
                  ((and (eql base-class-code #x01)
                        (eql sub-class-code #x01))
                   ;; A PATA controller.
                   (ata-pci-register location))
                  ((or
                    ;; SATA controller implementing AHCI 1.0.
                    (and (eql base-class-code #x01)
                         (eql sub-class-code #x06)
                         (eql programming-interface #x01))
                    ;; The RAID controller in my test machine.
                    (and (eql vendor-id #x8086)
                         (eql device-id #x2822)))
                   ;; An AHCI controller.
                   (ahci-pci-register location))
                  ((eql (pci-config/8 location +pci-config-hdr-type+) +pci-bridge-htype+)
                   ;; Bridge device, scan the other side.
                   (pci-scan (pci-config/8 location +pci-bridge-secondary-bus+))))))))))

(defun initialize-pci ()
  (setf *pci-config-lock* :unlocked)
  (setf (system:io-port/32 +pci-config-address+) #x80000000)
  (when (eql (system:io-port/32 +pci-config-address+) #x80000000)
    (debug-print-line "Begin PCI scan.")
    (pci-scan 0)))
