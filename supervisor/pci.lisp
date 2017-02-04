;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
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

(sys.int::defglobal *pci-config-lock*)

(sys.int::defglobal *pci-devices*)
(sys.int::defglobal *pci-late-probe-devices*)

(defstruct (pci-device
             (:area :wired))
  address
  vendor-id
  device-id
  vendor-name
  device-name
  boot-id
  claimed)

(defun make-pci-address (bus device function)
  (declare (type (integer 0 255) bus register)
           (type (integer 0 31) device)
           (type (integer 0 7) function))
  (logior #x80000000
          (ash bus 16)
          (ash device 11)
          (ash function 8)))

(defun pci-set-config-address (address register)
  (setf (sys.int::io-port/32 +pci-config-address+) (logior address
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
        (sys.int::io-port/8 (+ +pci-config-data+ (logand register #b11)))))))

(defun pci-config/16 (device register)
  (when (logtest register #b01)
    (error "Misaligned PCI register ~S." register))
  (safe-without-interrupts (device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (sys.int::io-port/16 (+ +pci-config-data+ (logand register #b10)))))))

(defun pci-config/32 (device register)
  (when (logtest register #b11)
    (error "Misaligned PCI register ~S." register))
  (safe-without-interrupts (device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (sys.int::io-port/32 +pci-config-data+)))))

(defun (setf pci-config/8) (value device register)
  (safe-without-interrupts (value device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (setf (sys.int::io-port/8 (+ +pci-config-data+ (logand register #b11))) value)))))

(defun (setf pci-config/16) (value device register)
  (when (logtest register #b01)
    (error "Misaligned PCI register ~S." register))
  (safe-without-interrupts (value device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
        (pci-set-config-address (pci-device-address device) register)
        (setf (sys.int::io-port/16 (+ +pci-config-data+ (logand register #b10))) value)))))

(defun (setf pci-config/32) (value device register)
  (when (logtest register #b11)
    (error "Misaligned PCI register ~S." register))
  (safe-without-interrupts (value device register)
    (when (eql (pci-device-boot-id device) *boot-id*)
      (with-symbol-spinlock (*pci-config-lock*)
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

(defconstant +pci-command-bus-master-enable+ (ash 1 2))

(defun pci-bus-master-enabled (device)
  (logtest (pci-config/16 device +pci-config-command+)
           +pci-command-bus-master-enable+))

(defun (setf pci-bus-master-enabled) (value device)
  (let ((prev (pci-config/16 device +pci-config-command+)))
    (setf (pci-config/16 device +pci-config-command+)
          (if value
              (logior prev +pci-command-bus-master-enable+)
              (logand prev (lognot +pci-command-bus-master-enable+))))
    value))

(defun pci-io-region/8 (location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (sys.int::io-port/8 (+ (logand location (lognot #b11)) offset))
      ;; MMIO.
      (physical-memref-unsigned-byte-8 (+ (logand location (lognot #b1111))
                                          offset))))

(defun pci-io-region/16 (location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (sys.int::io-port/16 (+ (logand location (lognot #b11)) offset))
      ;; MMIO.
      (physical-memref-unsigned-byte-16 (+ (logand location (lognot #b1111))
                                           offset))))

(defun pci-io-region/32 (location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (sys.int::io-port/32 (+ (logand location (lognot #b11)) offset))
      ;; MMIO.
      (physical-memref-unsigned-byte-32 (+ (logand location (lognot #b1111))
                                           offset))))

(defun (setf pci-io-region/8) (value location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (setf (sys.int::io-port/8 (+ (logand location (lognot #b11)) offset)) value)
      ;; MMIO.
      (setf (physical-memref-unsigned-byte-8 (+ (logand location (lognot #b1111))
                                                offset))
            value)))

(defun (setf pci-io-region/16) (value location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (setf (sys.int::io-port/16 (+ (logand location (lognot #b11)) offset)) value)
      ;; MMIO.
      (setf (physical-memref-unsigned-byte-16 (+ (logand location (lognot #b1111))
                                                 offset))
            value)))

(defun (setf pci-io-region/32) (value location offset)
  (if (logbitp 0 location)
      ;; Port IO.
      (setf (sys.int::io-port/32 (+ (logand location (lognot #b11)) offset)) value)
      ;; MMIO.
      (setf (physical-memref-unsigned-byte-32 (+ (logand location (lognot #b1111))
                                                 offset))
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
  (when (not (boundp '*pci-drivers*))
    (setf *pci-drivers* '()))
  (setf *pci-config-lock* :unlocked)
  (setf *pci-devices* '()
        *pci-late-probe-devices* '())
  (add-deferred-boot-action 'pci-late-probe))

(defun pci-detect ()
  (setf (sys.int::io-port/32 +pci-config-address+) #x80000000)
  (when (eql (sys.int::io-port/32 +pci-config-address+) #x80000000)
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
                            (device-id (pci-config/16 device +pci-config-deviceid+))
                            (header-type (ldb (byte 7 0) (pci-config/8 device +pci-config-hdr-type+))))
                       (unless (or (eql vendor-id #xFFFF) (eql vendor-id 0))
                         (setf (pci-device-vendor-id device) vendor-id
                               (pci-device-device-id device) device-id)
                         (multiple-value-bind (vendor-name device-name)
                             (pci-find-device-name vendor-id device-id)
                           (setf (pci-device-vendor-name device) vendor-name
                                 (pci-device-device-name device) device-name)
                           (debug-print-line bus ":" device-nr ":" function " " vendor-id ":" device-id " " vendor-name " - " device-name))
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
                (push-wired device *pci-late-probe-devices*))))))))

;; These devices must be probed late because their drivers may not be in wired memory.
(defun pci-late-probe ()
  (dolist (dev *pci-late-probe-devices*)
    (dolist (drv *pci-drivers*
             (debug-print-line "PCI device " (pci-config/16 dev +pci-config-vendorid+) ":" (pci-config/16 dev +pci-config-deviceid+) " not supported."))
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
      for (base-class sub-class pi) in (pci-driver-classes driver)
      do
        (when (and (or (not base-class)
                       (eql (pci-base-class device) base-class))
                   (or (not sub-class)
                       (eql (pci-sub-class device) sub-class))
                   (or (not pi)
                       (eql (pci-programming-interface device) pi)))
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
    (debug-print-line "Registered new pci driver " name)
    (push-wired driver *pci-drivers*)
    (probe-pci-driver driver)
    name))

(declaim (special sys.int::*pci-ids*))

(defun bsearch (item vector &key (start 0) end (stride 1))
  "Locate ITEM using a binary search through VECTOR."
  ;; IMIN/IMAX are inclusive indicies.
  (do ((imin start)
       (imax (1- (truncate (or end (sys.int::simple-vector-length vector)) stride))))
      ((< imax imin)
       nil)
    (let* ((imid (truncate (+ imin imax) 2))
           (elt (svref vector (* imid stride))))
      (cond ((< elt item) (setf imin (1+ imid)))
            ((> elt item) (setf imax (1- imid)))
            (t (return (* imid stride)))))))

(defun pci-find-vendor-name (id &optional (ids sys.int::*pci-ids*))
  (let ((position (bsearch id ids :stride 3)))
    (when position
      (values (svref ids (+ position 1))
              (svref ids (+ position 2))))))

(defun pci-find-device-name (vid did &optional (ids sys.int::*pci-ids*))
  (multiple-value-bind (vname devices)
      (pci-find-vendor-name vid ids)
    (when (and vname devices)
      (let ((position (bsearch did devices :stride 3)))
        (when position
          (values vname
                  (svref devices (+ position 1))
                  (svref devices (+ position 2))))))))

(defun pci-find-subsystem-name (vid did svid sdid &optional (ids sys.int::*pci-ids*))
  (multiple-value-bind (vname dname subsystems)
      (pci-find-device-name vid did ids)
    (when (and vname subsystems)
      (let ((position (bsearch (logior (ash svid 16) sdid) subsystems
                               :stride 2)))
        (if position
            (values vname dname (svref subsystems (1+ position)))
            (values vname dname nil))))))
