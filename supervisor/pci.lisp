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

(defvar *pci-devices*)

(defstruct (pci-device
             (:area :wired))
  address
  boot-id)

(defun make-pci-address (bus device function)
  (declare (type (integer 0 255) bus register)
	   (type (integer 0 31) device)
	   (type (integer 0 7) function))
  (logior #x80000000
	  (ash bus 16)
	  (ash device 11)
	  (ash function 8)))

(defun pci-set-config-address (address register)
  (setf (system:io-port/32 +pci-config-address+) (logior address
                                                  (logand register #b11111100))))

(defun pci-device-location (device)
  (let ((address (pci-device-address device)))
    (values (ldb (byte 8 16) address) ; bus
            (ldb (byte 5 11) address) ; device
            (ldb (byte 3 8) address)))) ; function

(defun pci-config/8 (device register)
  (safe-without-interrupts (device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (system:io-port/8 (+ +pci-config-data+ (logand register #b11)))))))

(defun pci-config/16 (device register)
  (when (logtest register #b01)
    (error "Misaligned PCI register ~S." register))
  (safe-without-interrupts (device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (system:io-port/16 (+ +pci-config-data+ (logand register #b10)))))))

(defun pci-config/32 (device register)
  (when (logtest register #b11)
    (error "Misaligned PCI register ~S." register))
  (safe-without-interrupts (device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (system:io-port/32 +pci-config-data+)))))

(defun (setf pci-config/8) (value device register)
  (safe-without-interrupts (value device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (setf (system:io-port/8 (+ +pci-config-data+ (logand register #b11))) value)))))

(defun (setf pci-config/16) (value device register)
  (when (logtest register #b01)
    (error "Misaligned PCI register ~S." register))
  (safe-without-interrupts (value device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (setf (system:io-port/16 (+ +pci-config-data+ (logand register #b10))) value)))))

(defun (setf pci-config/32) (value device register)
  (when (logtest register #b11)
    (error "Misaligned PCI register ~S." register))
  (safe-without-interrupts (value device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (setf (system:io-port/32 +pci-config-data+) value)))))

(defun pci-base-class (device)
  (ldb (byte 8 24) (pci-config/32 device +pci-config-revid+)))

(defun pci-sub-class (device)
  (ldb (byte 8 16) (pci-config/32 device +pci-config-revid+)))

(defun pci-programming-interface (device)
  (ldb (byte 8 8) (pci-config/32 device +pci-config-revid+)))

(defun pci-bar (device bar)
  (pci-config/32 device (+ +pci-config-bar-start+ (* bar 4))))

(defun pci-io-region (device bar size)
  ;; TODO: I think the size can be determined from the BAR?
  ;; Would be better to do that in the future...
  (let ((address (pci-bar device bar)))
    (when (not (logbitp 0 address))
      (let* ((base (logand address (lognot #b1111)))
             (end (align-up (+ base size) #x1000))
             (aligned-base (logand base (lognot #xFFF)))
             (aligned-size (- end aligned-base)))
        (map-physical-memory aligned-base
                             aligned-size
                             "PCI MMIO")))
    address))

(defun pci-intr-line (device)
  (pci-config/8 device +pci-config-intr-line+))

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

(defun map-pci-devices (fn)
  (dolist (device *pci-devices*)
    (funcall fn device)))

(defun pci-probe (callback vid-did-pairs)
  (map-pci-devices (dx-lambda (device)
                     (let ((vendor-id (pci-config/16 device +pci-config-vendorid+))
                           (device-id (pci-config/16 device +pci-config-deviceid+)))
                       (loop
                          for (vid did) in vid-did-pairs
                          when (and (eql vid vendor-id)
                                    (eql did device-id))
                          do (funcall callback device))))))

(defun initialize-pci ()
  (setf *pci-config-lock* :unlocked)
  (setf (system:io-port/32 +pci-config-address+) #x80000000)
  (setf *pci-devices* '())
  (when (eql (system:io-port/32 +pci-config-address+) #x80000000)
    (debug-print-line "Begin PCI scan.")
    (labels ((scan-bus (bus)
               (dotimes (device-nr 32)
                 ;; High bit of the header type specifies if a device is multifunction.
                 (let ((multifunction (logbitp 7 (pci-config/8
                                                  (make-pci-device :address (make-pci-address bus device-nr 0)
                                                                   :boot-id *boot-id*)
                                                  +pci-config-hdr-type+))))
                   (dotimes (function (if multifunction 8 1))
                     (let* ((address (make-pci-address bus device-nr function))
                            (device (make-pci-device :address address :boot-id *boot-id*))
                            (vendor-id (pci-config/16 device +pci-config-vendorid+))
                            (header-type (ldb (byte 7 0) (pci-config/8 device +pci-config-hdr-type+))))
                       (debug-print-line bus ":" device-nr ":" function " " vendor-id)
                       (unless (or (eql vendor-id #xFFFF) (eql vendor-id 0))
                         (push-wired device *pci-devices*)
                         (when (eql header-type +pci-bridge-htype+)
                           ;; Bridge device, scan the other side.
                           (scan-bus (pci-config/8 device +pci-bridge-secondary-bus+))))))))))
      (declare (dynamic-extent #'scan-bus))
      (scan-bus 0))
    (map-pci-devices
     (dx-lambda (device)
       (let* ((vendor-id (pci-config/16 device +pci-config-vendorid+))
              (device-id (pci-config/16 device +pci-config-deviceid+))
              (base-class-code (pci-base-class device))
              (sub-class-code (pci-sub-class device))
              (programming-interface (pci-programming-interface device))
              (header-type (ldb (byte 7 0) (pci-config/8 device +pci-config-hdr-type+))))
         (multiple-value-bind (bus device-nr function)
             (pci-device-location device)
           (debug-print-line "PCI:" bus ":" device-nr ":" function
                             " " vendor-id ":" device-id
                             " " base-class-code ":" sub-class-code ":" programming-interface
                             " " (pci-config/8 device +pci-config-revid+)
                             " " header-type))
         (cond ((and (eql vendor-id #x1AF4)
                     (<= #x1000 device-id #x103F))
                ;; Some kind of virtio-device.
                (virtio-pci-register device))
               ((and (eql base-class-code #x01)
                     (eql sub-class-code #x01))
                ;; A PATA controller.
                (ata-pci-register device))
               ((or
                 ;; SATA controller implementing AHCI 1.0.
                 (and (eql base-class-code #x01)
                      (eql sub-class-code #x06)
                      (eql programming-interface #x01))
                 ;; The RAID controller in my test machine.
                 (and (eql vendor-id #x8086)
                      (eql device-id #x2822)))
                ;; An AHCI controller.
                (ahci-pci-register device))
               ((and (eql vendor-id #x10EC)
                     (eql device-id #x8168))
                ;; The NIC in my test machine.
                (rtl8168-pci-register device))))))))
