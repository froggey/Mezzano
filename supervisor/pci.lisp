;;;; PCI bus driver

(defpackage :mezzano.supervisor.pci
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sys.int :mezzano.internals))
  (:export #:pci-device
           #:pci-device-location
           #:pci-device-boot-id
           #:pci-config/8
           #:pci-config/16
           #:pci-config/32
           #:pci-base-class
           #:pci-sub-class
           #:pci-programming-interface
           #:pci-bar
           #:pci-bar-size
           #:pci-io-region
           #:pci-io-region/8
           #:pci-io-region/16
           #:pci-io-region/32
           #:pci-io-region/64
           #:pci-io-region/le16
           #:pci-io-region/le32
           #:pci-io-region/le64
           #:pci-intr-line
           #:pci-bus-master-enabled
           #:pci-get-vendor-capability
           #:map-pci-devices
           #:define-pci-driver

           #:+pci-config-vendorid+
           #:+pci-config-deviceid+
           #:+pci-config-command+
           #:+pci-config-status+
           #:+pci-config-revid+
           #:+pci-config-classcode+
           #:+pci-config-cachelinesz+
           #:+pci-config-latency-timer+
           #:+pci-config-hdr-type+
           #:+pci-config-bist+
           #:+pci-config-bar-start+
           #:+pci-config-cardbus-cis+
           #:+pci-config-subvendorid+
           #:+pci-config-subdeviceid+
           #:+pci-config-ex-rom-base+
           #:+pci-config-capabilities+
           #:+pci-config-intr-line+
           #:+pci-config-intr-pin+
           #:+pci-config-min-gnt+
           #:+pci-config-max-lat+

           #:+pci-bridge-htype+
           #:+pci-bridge-primary-bus+
           #:+pci-bridge-secondary-bus+
           #:+pci-bridge-subordinate-bus+
           #:+pci-bridge-secondary-latency+
           #:+pci-bridge-io-base+
           #:+pci-bridge-io-limit+
           #:+pci-bridge-secondary-status+

           #:+pci-command-bus-master+
           #:+pci-command-interrupt-disable+

           #:+pci-status-interrupt-status+
           #:+pci-status-capabilities-list+

           #:+pci-capability-vendor+
           #:+pci-capability-next+
           #:+pci-capability-length+
           #:+pci-capability-vendor-type+

           #:+pci-capability-id-msi+
           #:+pci-capability-id-vendor+))

(in-package :mezzano.supervisor.pci)

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

(defconstant +pci-standard-htype+           #x00)

(defconstant +pci-bridge-htype+             #x01)
(defconstant +pci-bridge-primary-bus+       #x18)
(defconstant +pci-bridge-secondary-bus+     #x19)
(defconstant +pci-bridge-subordinate-bus+   #x1A)
(defconstant +pci-bridge-secondary-latency+ #x1B)
(defconstant +pci-bridge-io-base+           #x1C)
(defconstant +pci-bridge-io-limit+          #x1D)
(defconstant +pci-bridge-secondary-status+  #x1E)

(defconstant +pci-command-bus-master+ 2)
(defconstant +pci-command-interrupt-disable+ 10)

(defconstant +pci-status-interrupt-status+ 3)
(defconstant +pci-status-capabilities-list+ 4)

(defconstant +pci-capability-vendor+ 0)
(defconstant +pci-capability-next+ 1)
(defconstant +pci-capability-length+ 2)
(defconstant +pci-capability-vendor-type+ 3)

(defconstant +pci-capability-id-msi+ #x05)
(defconstant +pci-capability-id-vendor+ #x09)

(sys.int::defglobal *pci-config-lock*)

(sys.int::defglobal *pci-devices*)
(sys.int::defglobal *pci-late-probe-devices*)

(defstruct (pci-device
             (:area :wired))
  address
  vendor-id
  device-id
  boot-id
  claimed)

(defun make-pci-address (bus device function)
  (declare (type (integer 0 255) bus)
           (type (integer 0 31) device)
           (type (integer 0 7) function))
  (logior #x80000000
          (ash bus 16)
          (ash device 11)
          (ash function 8)))

(defun pci-set-config-address (address register)
  (setf (sys.int::io-port/32 +pci-config-address+)
        (logior address (logand register #b11111100))))

(defun pci-device-location (device)
  (let ((address (pci-device-address device)))
    (values (ldb (byte 8 16) address) ; bus
            (ldb (byte 5 11) address) ; device
            (ldb (byte 3 8) address)))) ; function

(defun pci-config/8 (device register)
  (sup:safe-without-interrupts (device register)
    (when (eql (pci-device-boot-id device) (sup:current-boot-id))
      (sup:with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (sys.int::io-port/8 (+ +pci-config-data+ (logand register #b11)))))))

(defun pci-config/16 (device register)
  (when (logtest register #b01)
    (error "Misaligned PCI register ~S." register))
  (sup:safe-without-interrupts (device register)
    (when (eql (pci-device-boot-id device) (sup:current-boot-id))
      (sup:with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (sys.int::io-port/16 (+ +pci-config-data+ (logand register #b10)))))))

(defun pci-config/32 (device register)
  (when (logtest register #b11)
    (error "Misaligned PCI register ~S." register))
  (sup:safe-without-interrupts (device register)
    (when (eql (pci-device-boot-id device) (sup:current-boot-id))
      (sup:with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (sys.int::io-port/32 +pci-config-data+)))))

(defun (setf pci-config/8) (value device register)
  (sup:safe-without-interrupts (value device register)
    (when (eql (pci-device-boot-id device) (sup:current-boot-id))
      (sup:with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (setf (sys.int::io-port/8 (+ +pci-config-data+ (logand register #b11))) value)))))

(defun (setf pci-config/16) (value device register)
  (when (logtest register #b01)
    (error "Misaligned PCI register ~S." register))
  (sup:safe-without-interrupts (value device register)
    (when (eql (pci-device-boot-id device) (sup:current-boot-id))
      (sup:with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (setf (sys.int::io-port/16 (+ +pci-config-data+ (logand register #b10))) value)))))

(defun (setf pci-config/32) (value device register)
  (when (logtest register #b11)
    (error "Misaligned PCI register ~S." register))
  (sup:safe-without-interrupts (value device register)
    (when (eql (pci-device-boot-id device) (sup:current-boot-id))
      (sup:with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (setf (sys.int::io-port/32 +pci-config-data+) value)))))

(defun pci-base-class (device)
  (ldb (byte 8 24) (pci-config/32 device +pci-config-revid+)))

(defun pci-sub-class (device)
  (ldb (byte 8 16) (pci-config/32 device +pci-config-revid+)))

(defun pci-programming-interface (device)
  (ldb (byte 8 8) (pci-config/32 device +pci-config-revid+)))

(defun pci-bar (device bar)
  (pci-config/32 device (+ +pci-config-bar-start+ (* bar 4))))

(defun (setf pci-bar) (value device bar)
  (setf (pci-config/32 device (+ +pci-config-bar-start+ (* bar 4))) value))

(defun decode-pci-bar-type (bar-data)
  "Returns the basic type of the BAR (IO/MMIO + address limits) and prefetchablility."
  (cond ((zerop bar-data)
         (values nil nil))
        ((logbitp 0 bar-data)
         (values :io nil))
        (t
         (values (case (ldb (byte 2 1) bar-data)
                   (0 :mmio-32)
                   (2 :mmio-64))
                 (logbitp 3 bar-data)))))

(defun pci-bar-size (device bar)
  "Calculate the size of the specified BAR.
Caution: This disables & reenables address decoding to perform the calcuation.
Returns NIL if the BAR has an unknown type."
  (sup:safe-without-interrupts (device bar)
    (when (eql (pci-device-boot-id device) (sup:current-boot-id))
      (let ((old-command (pci-config/16 device +pci-config-command+))
            (bar-data (pci-bar device bar))
            (size nil))
        ;; Disable address decoding as we're changing the addresses in the BAR.
        (setf (pci-config/16 device +pci-config-command+) 0)
        (case (decode-pci-bar-type bar-data)
          (:io
           ;; IO memory.
           (setf (pci-bar device bar) #xFFFFFFFF)
           (let ((lo (logxor (logand (pci-bar device bar) #xFFFE) #xFFFF)))
             (unless (eql lo #xFFFF)
               (setf size (1+ lo)))))
          (:mmio-32
           ;; 32-bit memory.
           (setf (pci-bar device bar) #xFFFFFFFF)
           (let ((lo (logxor (logand (pci-bar device bar) #xFFFFFFF0) #xFFFFFFFF)))
             (unless (eql lo #xFFFFFFFF)
               (setf size (1+ lo)))))
          (:mmio-64
           ;; 64-bit memory.
           (let ((bar-data-hi (pci-bar device (1+ bar))))
             (setf (pci-bar device bar) #xFFFFFFFF)
             (setf (pci-bar device (1+ bar)) #xFFFFFFFF)
             ;; Be careful and avoid constructing a bignum here, stick
             ;; with the separate 32-bit halves as long as possible.
             (let ((lo (logxor (logand (pci-bar device bar) #xFFFFFFF0) #xFFFFFFFF))
                   (hi (logxor (pci-bar device (1+ bar)) #xFFFFFFFF)))
               (unless (and (eql lo #xFFFFFFFF)
                            (eql hi #xFFFFFFFF))
                 (setf size (1+ (logior lo (ash hi 32))))))
             (setf (pci-bar device (1+ bar)) bar-data-hi))))
        ;; Restore old bar value & reenable decoding.
        (setf (pci-bar device bar) bar-data
              (pci-config/16 device +pci-config-command+) old-command)
        size))))

(defun pci-io-region (device bar)
  (let ((address (pci-bar device bar)))
    (when (not (decode-pci-bar-type address))
      (error "PCI device ~S BAR ~D has unknown type ~8,'0X" device bar address))
    address))

(defun pci-intr-line (device)
  (pci-config/8 device +pci-config-intr-line+))

(defun pci-bus-master-enabled (device)
  (logbitp +pci-command-bus-master+
           (pci-config/16 device +pci-config-command+)))

(defun (setf pci-bus-master-enabled) (value device)
  (let ((prev (pci-config/16 device +pci-config-command+)))
    (setf (pci-config/16 device +pci-config-command+)
          (if value
              (logior prev (ash 1 +pci-command-bus-master+))
              (logand prev (lognot (ash 1 +pci-command-bus-master+)))))
    value))

(defun pci-io-region/8 (location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (sys.int::io-port/8 (+ (logand location (lognot #b11)) offset))
      ;; MMIO.
      (sup::physical-memref-unsigned-byte-8 (+ (logand location (lognot #b1111))
                                               offset))))

(defun pci-io-region/16 (location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (sys.int::io-port/16 (+ (logand location (lognot #b11)) offset))
      ;; MMIO.
      (sup::physical-memref-unsigned-byte-16 (+ (logand location (lognot #b1111))
                                                offset))))

(defun pci-io-region/32 (location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (sys.int::io-port/32 (+ (logand location (lognot #b11)) offset))
      ;; MMIO.
      (sup::physical-memref-unsigned-byte-32 (+ (logand location (lognot #b1111))
                                                offset))))

(defun (setf pci-io-region/8) (value location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (setf (sys.int::io-port/8 (+ (logand location (lognot #b11)) offset)) value)
      ;; MMIO.
      (setf (sup::physical-memref-unsigned-byte-8 (+ (logand location (lognot #b1111))
                                                     offset))
            value)))

(defun (setf pci-io-region/16) (value location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (setf (sys.int::io-port/16 (+ (logand location (lognot #b11)) offset)) value)
      ;; MMIO.
      (setf (sup::physical-memref-unsigned-byte-16 (+ (logand location (lognot #b1111))
                                                      offset))
            value)))

(defun (setf pci-io-region/32) (value location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (setf (sys.int::io-port/32 (+ (logand location (lognot #b11)) offset)) value)
      ;; MMIO.
      (setf (sup::physical-memref-unsigned-byte-32 (+ (logand location (lognot #b1111))
                                                      offset))
            value)))

;; Little-endian accessors.
(defun pci-io-region/le16 (location offset)
  (pci-io-region/16 location offset))

(defun pci-io-region/le32 (location offset)
  (pci-io-region/32 location offset))

(defun pci-io-region/le64 (location offset)
  (logior (pci-io-region/32 location offset)
          (ash (pci-io-region/32 location (+ offset 4)) 32)))

(defun (setf pci-io-region/le16) (value location offset)
  (setf (pci-io-region/16 location offset) value))

(defun (setf pci-io-region/le32) (value location offset)
  (setf (pci-io-region/32 location offset) value))

(defun (setf pci-io-region/le64) (value location offset)
  (setf (pci-io-region/32 location offset) (ldb (byte 32 0) value))
  (setf (pci-io-region/32 location (+ offset 4)) (ldb (byte 32 32) value))
  value)

(defun map-pci-devices (fn)
  (dolist (device *pci-devices*)
    (funcall fn device)))

(defun sup::initialize-pci ()
  (when (not (boundp '*pci-drivers*))
    (setf *pci-drivers* '()))
  (setf *pci-config-lock* :unlocked)
  (setf *pci-devices* '()
        *pci-late-probe-devices* '())
  (sup:add-deferred-boot-action 'pci-late-probe))

(defun dump-pci-device-config-space (device)
  (let* ((vendor-id (pci-config/16 device +pci-config-vendorid+))
         (device-id (pci-config/16 device +pci-config-deviceid+))
         (base-class-code (pci-base-class device))
         (sub-class-code (pci-sub-class device))
         (programming-interface (pci-programming-interface device))
         (header-type (ldb (byte 7 0) (pci-config/8 device +pci-config-hdr-type+))))
    (multiple-value-bind (bus device-nr function)
        (pci-device-location device)
      (multiple-value-bind (vendor-name device-name)
          (pci-find-device-name vendor-id device-id)
        (sup:debug-print-line "PCI:" bus ":" device-nr ":" function
                              " " vendor-id ":" device-id
                              " " vendor-name " - " device-name
                              " " base-class-code ":" sub-class-code ":" programming-interface
                              " rid: " (pci-config/8 device +pci-config-revid+)
                              " hdr: " header-type
                              " intr: " (pci-intr-line device))))
    (when (logbitp +pci-status-capabilities-list+
                   (pci-config/16 device +pci-config-status+))
      (loop
         with cap = (pci-config/8 device +pci-config-capabilities+)
         do
           (setf cap (logand cap (lognot 3))) ; Bottom 2 bits must be masked off.
           (when (zerop cap)
             (return))
           (let ((id (pci-config/8 device (+ cap +pci-capability-vendor+))))
             (case id
               (#.+pci-capability-id-msi+
                (sup:debug-print-line "    " cap ": MSI " (pci-config/16 device (+ cap +pci-capability-length+))))
               (#.+pci-capability-id-vendor+
                (let ((subid (pci-config/8 device (+ cap +pci-capability-vendor-type+))))
                  (cond ((and (eql vendor-id #x1AF4)
                              (eql subid 1))
                         (sup:debug-print-line "    " cap ": Virtio common config"))
                        ((and (eql vendor-id #x1AF4)
                              (eql subid 2))
                         (sup:debug-print-line "    " cap ": Virtio notify config"))
                        ((and (eql vendor-id #x1AF4)
                              (eql subid 3))
                         (sup:debug-print-line "    " cap ": Virtio ISR config"))
                        ((and (eql vendor-id #x1AF4)
                              (eql subid 4))
                         (sup:debug-print-line "    " cap ": Virtio device config"))
                        ((and (eql vendor-id #x1AF4)
                              (eql subid 5))
                         (sup:debug-print-line "    " cap ": Virtio PCI config"))
                        (t
                         (sup:debug-print-line "    " cap ": Vendor-specific " subid)))))
               (t
                (sup:debug-print-line "    " cap ": Unknown capability " id)))
             (setf cap (pci-config/8 device (+ cap +pci-capability-next+))))))))

(defun pci-scan-bus (bus)
  (dotimes (device-nr 32)
    ;; High bit of the header type specifies if a device is multifunction.
    (let ((multifunction (logbitp 7 (pci-config/8
                                     (make-pci-device :address (make-pci-address bus device-nr 0)
                                                      :boot-id (sup:current-boot-id))
                                     +pci-config-hdr-type+))))
      (dotimes (function (if multifunction 8 1))
        (let* ((address (make-pci-address bus device-nr function))
               (device (make-pci-device :address address :boot-id (sup:current-boot-id)))
               (vendor-id (pci-config/16 device +pci-config-vendorid+))
               (device-id (pci-config/16 device +pci-config-deviceid+))
               (header-type (ldb (byte 7 0) (pci-config/8 device +pci-config-hdr-type+))))
          (unless (or (eql vendor-id #xFFFF) (eql vendor-id 0))
            (setf (pci-device-vendor-id device) vendor-id
                  (pci-device-device-id device) device-id)
            (sup::push-wired device *pci-devices*)
            ;; Ensure memory associated with BARs is mapped.
            (do ((n-bars (case header-type
                           (#.+pci-standard-htype+ 6)
                           (#.+pci-bridge-htype+ 2)
                           (t 0)))
                 (bar 0 (1+ bar)))
                ((>= bar n-bars))
              (let* ((address (pci-bar device bar))
                     (type (decode-pci-bar-type address))
                     (size (pci-bar-size device bar)))
                (when (and size (member type '(:mmio-32 :mmio-64)))
                  (let* ((base (logand address (lognot #b1111)))
                         (end (sup::align-up (+ base size) #x1000))
                         (aligned-base (logand base (lognot #xFFF)))
                         (aligned-size (- end aligned-base)))
                    (sup:map-physical-memory-early aligned-base
                                                   aligned-size
                                                   "PCI MMIO"))
                  (when (eql type :mmio-64)
                    (incf bar)))))
            (when (eql header-type +pci-bridge-htype+)
              ;; Bridge device, scan the other side.
              (pci-scan-bus (pci-config/8 device +pci-bridge-secondary-bus+)))))))))

(defun sup::pci-detect ()
  (setf (sys.int::io-port/32 +pci-config-address+) #x80000000)
  (when (eql (sys.int::io-port/32 +pci-config-address+) #x80000000)
    (sup:debug-print-line "Begin PCI scan.")
    (pci-scan-bus 0)
    (map-pci-devices
     (sup::dx-lambda (device)
       (let* ((vendor-id (pci-config/16 device +pci-config-vendorid+))
              (device-id (pci-config/16 device +pci-config-deviceid+))
              (base-class-code (pci-base-class device))
              (sub-class-code (pci-sub-class device))
              (programming-interface (pci-programming-interface device)))
         (dump-pci-device-config-space device)
         (cond ((and (eql vendor-id #x1AF4)
                     (<= #x1000 device-id #x107F))
                ;; Some kind of virtio-device.
                (virtio-pci-register device)
                (setf (pci-device-claimed device) :virtio-pci))
               ((and (eql base-class-code #x01)
                     (eql sub-class-code #x01))
                ;; A PATA controller.
                (ata-pci-register device)
                (setf (pci-device-claimed device) :ata))
               ((or
                 ;; SATA controller implementing AHCI 1.0.
                 (and (eql base-class-code #x01)
                      (eql sub-class-code #x06)
                      (eql programming-interface #x01))
                 ;; The RAID controller in my test machine.
                 (and (eql vendor-id #x8086)
                      (eql device-id #x2822)))
                ;; An AHCI controller.
                (ahci-pci-register device)
                (setf (pci-device-claimed device) :ahci))
               ((and (eql vendor-id #x80EE)
                     (eql device-id #xCAFE))
                ;; VirtualBox Guest Service.
                (setf (pci-device-claimed device)
                      (virtualbox-guest-register device)))
               ((and (eql vendor-id #x80EE)
                     (eql device-id #xBEEF))
                ;; VirtualBox Graphics Adapter.
                (setf (pci-device-claimed device)
                      (virtualbox-graphics-register device)))
               (t
                (sup::push-wired device *pci-late-probe-devices*))))))))

;; These devices must be probed late because their drivers may not be in wired memory.
(defun pci-late-probe ()
  (dolist (dev *pci-late-probe-devices*)
    (dolist (drv *pci-drivers*
             (sup::debug-print-line "PCI device " (pci-config/16 dev +pci-config-vendorid+) ":" (pci-config/16 dev +pci-config-deviceid+) " not supported."))
      (when (and (not (pci-device-claimed dev))
                 (pci-driver-compatible-p drv dev)
                 (sys.int::log-and-ignore-errors
                  (funcall (pci-driver-probe drv) dev)))
        (setf (pci-device-claimed dev) drv)
        (return)))))

;; FIXME: Access to this needs to be protected.
;; I'm not sure a mutex will cut it, it needs to be accessible
;; during boot-time device probing.
(sys.int::defglobal *pci-drivers* '())

(defstruct (pci-driver
             (:area :wired))
  name
  probe
  pci-ids
  classes)

(defun pci-driver-compatible-p (driver device)
  (or
   (loop
      for (base-class sub-class prog-interface) in (pci-driver-classes driver)
      do
        (when (and (or (not base-class)
                       (eql (pci-base-class device) base-class))
                   (or (not sub-class)
                       (eql (pci-sub-class device) sub-class))
                   (or (not prog-interface)
                       (eql (pci-programming-interface device) prog-interface)))
          (return t)))
   (loop
      for (vid did) in (pci-driver-pci-ids driver)
      do
        (when (and (eql (pci-config/16 device +pci-config-vendorid+) vid)
                   (eql (pci-config/16 device +pci-config-deviceid+) did))
          (return t)))))

(defun probe-pci-driver (driver)
  (dolist (dev *pci-devices*)
    (when (and (not (pci-device-claimed dev))
               (pci-driver-compatible-p driver dev)
               (sys.int::log-and-ignore-errors
                (funcall (pci-driver-probe driver) dev)))
      (setf (pci-device-claimed dev) driver))))

(defmacro define-pci-driver (name probe-function pci-ids classes)
  `(register-pci-driver ',name ',probe-function ',pci-ids ',classes))

(defun register-pci-driver (name probe-function pci-ids classes)
  (dolist (drv *pci-drivers*)
    (when (eql (pci-driver-name drv) name)
      (when (not (eql (pci-driver-probe drv) probe-function))
        ;; TODO: Detach current driver and reprobe?
        (error "Incompatible redefinition of virtio driver ~S." name))
      (probe-pci-driver drv)
      (return-from register-pci-driver name)))
  (let ((driver (make-pci-driver
                 :name name
                 :probe probe-function
                 :pci-ids pci-ids
                 :classes classes)))
    (sup:debug-print-line "Registered new pci driver " name)
    (sup::push-wired driver *pci-drivers*)
    (probe-pci-driver driver)
    name))

(declaim (special sys.int::*pci-ids*))

(defun pci-find-vendor-name (id &optional (ids sys.int::*pci-ids*))
  (let ((position (sup::bsearch id ids :stride 3)))
    (when position
      (values (svref ids (+ position 1))
              (svref ids (+ position 2))))))

(defun pci-find-device-name (vid did &optional (ids sys.int::*pci-ids*))
  (multiple-value-bind (vname devices)
      (pci-find-vendor-name vid ids)
    (when (and vname devices)
      (let ((position (sup::bsearch did devices :stride 3)))
        (when position
          (values vname
                  (svref devices (+ position 1))
                  (svref devices (+ position 2))))))))

(defun pci-find-subsystem-name (vid did svid sdid &optional (ids sys.int::*pci-ids*))
  (multiple-value-bind (vname dname subsystems)
      (pci-find-device-name vid did ids)
    (when (and vname subsystems)
      (let ((position (sup::bsearch (logior (ash svid 16) sdid) subsystems
                                    :stride 2)))
        (if position
            (values vname dname (svref subsystems (1+ position)))
            (values vname dname nil))))))

(defun pci-device-vendor-name (device)
  (pci-find-vendor-name (pci-device-vendor-id device)))

(defun pci-device-device-name (device)
  (nth-value 1 (pci-find-device-name (pci-device-vendor-id device)
                                     (pci-device-device-id device))))

(defun pci-get-vendor-capability (device subid)
  (when (logbitp +pci-status-capabilities-list+
                 (pci-config/16 device +pci-config-status+))
    (loop
       with cap = (pci-config/8 device +pci-config-capabilities+)
       do
         (setf cap (logand cap (lognot 3))) ; Bottom 2 bits must be masked off.
         (when (zerop cap)
           (return nil))
         (when (and (eql (pci-config/8 device (+ cap +pci-capability-vendor+))
                         +pci-capability-id-vendor+)
                    (eql (pci-config/8 device (+ cap +pci-capability-vendor-type+))
                         subid))
           (return cap))
         (setf cap (pci-config/8 device (+ cap +pci-capability-next+))))))
