;;; PCI scanning and device management.

;; Layout of CONFIG-ADDRESS register:
;; 31|30      24|23      16|15      11|10      8|7      2|1|0|
;; |E| Reserved |    Bus   |  Device  |Function |Register|0|0|
;; E = Enable bit

(in-package #:sys.int)

(declaim (special *pci-ids*))

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

(defvar *pci-devices* '()
  "A list of all detected PCI devices.")

(defvar *pci-drivers* '())

(defstruct pci-device
  bus
  device
  function
  vendor-id
  device-id)

(defun make-pci-config-address (bus device function register)
  (declare (type (integer 0 255) bus register)
	   (type (integer 0 31) device)
	   (type (integer 0 7) function))
  (logior #x80000000
	  (ash bus 16)
	  (ash device 11)
	  (ash function 8)
	  (logand register #b11111100)))

(defun %pci-config/8 (bus device function register)
  (setf (io-port/32 +pci-config-address+) (make-pci-config-address bus device function register))
  (io-port/8 (+ +pci-config-data+ (logand register #b11))))
(defun %pci-config/16 (bus device function register)
  (when (logtest register #b01)
    (error "Misaligned PCI register ~S." register))
  (setf (io-port/32 +pci-config-address+) (make-pci-config-address bus device function register))
  (io-port/16 (+ +pci-config-data+ (logand register #b10))))
(defun %pci-config/32 (bus device function register)
  (when (logtest register #b11)
    (error "Misaligned PCI register ~S." register))
  (setf (io-port/32 +pci-config-address+) (make-pci-config-address bus device function register))
  (io-port/32 +pci-config-data+))

(defun pci-config/8 (pci-device register)
  (%pci-config/8 (pci-device-bus pci-device) (pci-device-device pci-device) (pci-device-function pci-device) register))
(defun pci-config/16 (pci-device register)
  (%pci-config/16 (pci-device-bus pci-device) (pci-device-device pci-device) (pci-device-function pci-device) register))
(defun pci-config/32 (pci-device register)
  (%pci-config/32 (pci-device-bus pci-device) (pci-device-device pci-device) (pci-device-function pci-device) register))

(defun (setf %pci-config/8) (value bus device function register)
  (setf (io-port/32 +pci-config-address+) (make-pci-config-address bus device function register))
  (setf (io-port/8 (+ +pci-config-data+ (logand register #b11))) value))
(defun (setf %pci-config/16) (value bus device function register)
  (when (logtest register #b01)
    (error "Misaligned PCI register ~S." register))
  (setf (io-port/32 +pci-config-address+) (make-pci-config-address bus device function register))
  (setf (io-port/16 (+ +pci-config-data+ (logand register #b10))) value))
(defun (setf %pci-config/32) (value bus device function register)
  (unless (logtest register #b11)
    (error "Misaligned PCI register ~S." register))
  (setf (io-port/32 +pci-config-address+) (make-pci-config-address bus device function register))
  (setf (io-port/32 +pci-config-data+) value))

(defun (setf pci-config/8) (value pci-device register)
  (setf (%pci-config/8 (pci-device-bus pci-device) (pci-device-device pci-device) (pci-device-function pci-device) register) value))
(defun (setf pci-config/16) (value pci-device register)
  (setf (%pci-config/16 (pci-device-bus pci-device) (pci-device-device pci-device) (pci-device-function pci-device) register) value))
(defun (setf pci-config/32) (value pci-device register)
  (setf (%pci-config/32 (pci-device-bus pci-device) (pci-device-device pci-device) (pci-device-function pci-device) register) value))

(defun pci-hdr-type (pci-device)
  (logand (pci-config/8 pci-device +pci-config-hdr-type+) #x7F))

(defun pci-irq-line (pci-device)
  (pci-config/8 pci-device +pci-config-intr-line+))

(defun pci-base-class (pci-device)
  (pci-config/8 pci-device (+ +pci-config-classcode+ 2)))

(defun pci-sub-class (pci-device)
  (pci-config/8 pci-device (+ +pci-config-classcode+ 1)))

(defun pci-programming-interface (pci-device)
  (pci-config/8 pci-device +pci-config-classcode+))

(defun pci-bar (pci-device n)
  (declare (type (integer 0 3) n))
  (pci-config/32 pci-device (+ +pci-config-bar-start+ (* n 4))))

(defun pci-scan (bus)
  (dotimes (device 32)
    ;; High bit of the header type specifies if a device is multifunction.
    (let ((multifunction (eql (logand (%pci-config/8 bus device 0 +pci-config-hdr-type+) #x80) #x80)))
      (dotimes (function (if multifunction 8 1))
	(let ((vendor (%pci-config/16 bus device function +pci-config-vendorid+)))
	  (unless (or (eql vendor #xFFFF) (eql vendor 0))
	    (let* ((device-id (%pci-config/16 bus device function +pci-config-deviceid+))
		   (info (make-pci-device :bus bus
                                          :device device
                                          :function function
                                          :vendor-id vendor
                                          :device-id device-id)))
	      (push info *pci-devices*)
              (multiple-value-bind (vname dname)
                  (pci-find-device vendor device-id)
                (format t "~2,'0X:~X:~X: ~4,'0X ~4,'0X ~S ~S~%"
                        bus device function
                        vendor device-id
                        vname dname))
	      (when (eql (pci-hdr-type info) +pci-bridge-htype+)
		(pci-scan (pci-config/8 info +pci-bridge-secondary-bus+))))))))))

(defun pci-init ()
  "Detect PCI and scan the buses."
  (setf (io-port/32 +pci-config-address+) #x80000000)
  (setf *pci-devices* '())
  (when (eql (io-port/32 +pci-config-address+) #x80000000)
    (format t "Scanning PCI bus...~%")
    (pci-scan 0)))

(add-hook '*early-initialize-hook* 'pci-init)

(defun pci-probe (device-ids fn)
  (dolist (dev *pci-devices*)
    (dolist (id device-ids)
      (when (and (eql (pci-device-vendor-id dev) (first id))
		 (eql (pci-device-device-id dev) (second id)))
	(funcall fn dev)))))

(defun pci-register-driver (ids function)
  (push (list function ids) *pci-drivers*)
  (when *pci-devices*
    (pci-probe ids function)))

(defun pci-device-scan ()
  (format t "Registering drivers...~%")
  (dolist (driver *pci-drivers*)
    (pci-probe (second driver) (first driver))))

(defun bsearch (item vector &optional (stride 1))
  "Locate ITEM using a binary search through VECTOR."
  ;; IMIN/IMAX are inclusive indicies.
  (do ((imin 0)
       (imax (1- (truncate (length vector) stride))))
      ((< imax imin)
       nil)
    (let* ((imid (truncate (+ imin imax) 2))
           (elt (aref vector (* imid stride))))
      (cond ((< elt item) (setf imin (1+ imid)))
            ((> elt item) (setf imax (1- imid)))
            (t (return (* imid stride)))))))

(defun pci-find-vendor (id &optional (ids *pci-ids*))
  (let ((position (bsearch id ids 3)))
    (when position
      (values (aref ids (+ position 1))
              (aref ids (+ position 2))))))

(defun pci-find-device (vid did &optional (ids *pci-ids*))
  (multiple-value-bind (vname devices)
      (pci-find-vendor vid ids)
    (when (and vname devices)
      (let ((position (bsearch did devices 3)))
        (when position
          (values vname
                  (aref devices (+ position 1))
                  (aref devices (+ position 2))))))))

(defun pci-find-subsystem (vid did svid sdid &optional (ids *pci-ids*))
  (multiple-value-bind (vname dname subsystems)
      (pci-find-device vid did ids)
    (when (and vname subsystems)
      (let ((position (bsearch (logior (ash svid 16) sdid) subsystems 2)))
        (if position
            (values vname dname (aref subsystems (1+ position)))
            (values vname dname nil))))))
