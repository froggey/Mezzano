;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant +ata-compat-primary-command+ #x1F0)
(defconstant +ata-compat-primary-control+ #x3F0)
(defconstant +ata-compat-primary-irq+ 14)
(defconstant +ata-compat-secondary-command+ #x170)
(defconstant +ata-compat-secondary-control+ #x370)
(defconstant +ata-compat-secondary-irq+ 15)

(defconstant +ata-register-data+ 0) ; read/write
(defconstant +ata-register-error+ 1) ; read
(defconstant +ata-register-features+ 1) ; write
(defconstant +ata-register-count+ 2) ; read/write
(defconstant +ata-register-lba-low+ 3) ; read/write
(defconstant +ata-register-lba-mid+ 4) ; read/write
(defconstant +ata-register-lba-high+ 5) ; read/write
(defconstant +ata-register-device+ 6) ; read/write
(defconstant +ata-register-status+ 7) ; read
(defconstant +ata-register-command+ 7) ; write

(defconstant +ata-register-alt-status+ 6) ; read
(defconstant +ata-register-device-control+ 6) ; write

(defconstant +ata-bmr-command+ 0) ; write
(defconstant +ata-bmr-status+ 2) ; write
(defconstant +ata-bmr-prdt-address+ 4) ; write

(defconstant +ata-bmr-command-start+ #x01)
(defconstant +ata-bmr-direction-read/write+ #x08) ; set to read, clear to write.

(defconstant +ata-bmr-status-active+ #x01)
(defconstant +ata-bmr-status-error+ #x02)
(defconstant +ata-bmr-status-interrupt+ #x04)
(defconstant +ata-bmr-status-drive-0-dma-capable+ #x20)
(defconstant +ata-bmr-status-drive-1-dma-capable+ #x40)
(defconstant +ata-bmr-status-simplex+ #x80)

;; Device bits.
(defconstant +ata-dev+  #x10 "Select device 0 when clear, device 1 when set.")
(defconstant +ata-lba+  #x40 "Set when using LBA.")

;; Status bits.
(defconstant +ata-err+  #x01 "An error occured during command execution.")
(defconstant +ata-drq+  #x08 "Device is ready to transfer data.")
(defconstant +ata-df+   #x20 "Device fault.")
(defconstant +ata-drdy+ #x40 "Device is ready to accept commands.")
(defconstant +ata-bsy+  #x80 "Device is busy.")

;; Device Control bits.
(defconstant +ata-nien+ #x02 "Mask interrupts.")
(defconstant +ata-srst+ #x04 "Initiate a software reset.")
(defconstant +ata-hob+  #x80 "Read LBA48 high-order bytes.")

;; Error bits.
(defconstant +ata-abrt+ #x04 "Command aborted by device.")

;; Commands.
(defconstant +ata-command-read-sectors+ #x20)
(defconstant +ata-command-read-sectors-ext+ #x24)
(defconstant +ata-command-read-dma+ #xC8)
(defconstant +ata-command-read-dma-ext+ #x25)
(defconstant +ata-command-write-sectors+ #x30)
(defconstant +ata-command-write-sectors-ext+ #x3A)
(defconstant +ata-command-write-dma+ #xCA)
(defconstant +ata-command-write-dma-ext+ #x35)
(defconstant +ata-command-identify+ #xEC)
(defconstant +ata-command-identify-packet+ #xA1)
(defconstant +ata-command-packet+ #xA0)

(defstruct (ata-controller
             (:area :wired))
  command
  control
  bus-master-register
  prdt-phys
  irq
  current-channel
  (irq-latch (make-latch "ATA IRQ Notifier"))
  bounce-buffer)

(defstruct (ata-device
             (:area :wired))
  controller
  channel
  block-size
  sector-count
  lba48-capable)

(defstruct (atapi-device
             (:area :wired))
  controller
  channel
  cdb-size
  initialized-p)

(defun ata-error (controller)
  "Read the error register."
  (sys.int::io-port/8 (+ (ata-controller-command controller)
                         +ata-register-error+)))

(defun ata-status (controller)
  "Read the status register."
  (sys.int::io-port/8 (+ (ata-controller-command controller)
                         +ata-register-status+)))

(defun ata-alt-status (controller)
  "Read the alternate status register."
  (sys.int::io-port/8 (+ (ata-controller-control controller)
                         +ata-register-alt-status+)))

(defun ata-wait-for-controller (controller mask value timeout)
  "Wait for the bits in the alt-status register masked by MASK to become equal to VALUE.
Returns true when the bits are equal, false when the timeout expires or if the device sets ERR."
  (loop
     (let ((status (ata-alt-status controller)))
       (when (logtest status +ata-err+)
         (return nil))
       (when (eql (logand status mask) value)
         (return t)))
     (when (<= timeout 0)
       (return nil))
     ;; FIXME: Depends on the granularity of the timer.
     (sleep 0.01)
     (decf timeout 0.01)))

(defun ata-select-device (controller channel)
  ;; select-device should never be called with a command in progress on the controller.
  (when (logtest (logior +ata-bsy+ +ata-drq+)
                 (ata-alt-status controller))
    (debug-print-line "ATA-SELECT-DEVICE called with command in progress.")
    (return-from ata-select-device nil))
  (when (not (eql (ata-controller-current-channel controller) channel))
    (assert (or (eql channel :master) (eql channel :slave)))
    (setf (sys.int::io-port/8 (+ (ata-controller-command controller)
                                 +ata-register-device+))
          (ecase channel
            (:master 0)
            (:slave +ata-dev+)))
    ;; Again, neither BSY nor DRQ should be set.
    (when (logtest (logior +ata-bsy+ +ata-drq+)
                   (ata-alt-status controller))
      (debug-print-line "ATA-SELECT-DEVICE called with command in progress.")
      (return-from ata-select-device nil))
    (setf (ata-controller-current-channel controller) channel))
  t)

(defun ata-detect-packet-device (controller channel)
  (let ((buf (sys.int::make-simple-vector 256 :wired)))
    ;; Select the device.
    (when (not (ata-select-device controller channel))
      (debug-print-line "Could not select ata device when probing.")
      (return-from ata-detect-packet-device nil))
    ;; Issue IDENTIFY.
    (setf (sys.int::io-port/8 (+ (ata-controller-command controller)
                                 +ata-register-command+))
          +ata-command-identify-packet+)
    ;; Delay 400ns after writing command.
    (ata-alt-status controller)
    ;; Wait for BSY to clear and DRQ to go high.
    ;; Use a 1 second timeout.
    ;; I don't know if there's a standard timeout for this, but
    ;; I figure that the device should respond to IDENTIFY quickly.
    ;; Wrong blah! ata-wait-for-controller is nonsense.
    ;; if bsy = 0 & drq = 0, then there was an error.
    (let ((success (ata-wait-for-controller controller (logior +ata-bsy+ +ata-drq+) +ata-drq+ 1)))
      ;; Read the status register to clear the pending interrupt flag.
      (sys.int::io-port/8 (+ (ata-controller-command controller)
                             +ata-register-status+))
      ;; Check ERR before checking for timeout.
      ;; ATAPI devices will abort, and wait-for-controller will time out.
      (when (logtest (ata-alt-status controller) +ata-err+)
        (debug-print-line "IDENTIFY PACKET aborted by device.")
        (return-from ata-detect-packet-device))
      (when (not success)
        (debug-print-line "Timeout while waiting for DRQ during IDENTIFY PACKET.")
        (return-from ata-detect-packet-device)))
    ;; Read data.
    (dotimes (i 256)
      (setf (svref buf i) (sys.int::io-port/16 (+ (ata-controller-command controller)
                                                  +ata-register-data+))))
    (let* ((general-config (svref buf 0))
           (cdb-size (case (ldb (byte 2 0) general-config)
                      (0 12)
                      (1 16))))
      (debug-print-line "PACKET device:")
      (debug-print-line " General configuration: " general-config)
      (debug-print-line " Specific configuration: " (svref buf 2))
      (debug-print-line " Capabilities: " (svref buf 49) " " (svref buf 50))
      (debug-print-line " Field validity: " (svref buf 53))
      (debug-print-line " DMADIR: " (svref buf 62))
      (debug-print-line " Multiword DMA transfer: " (svref buf 63))
      (debug-print-line " PIO transfer mode supported: " (svref buf 64))
      (debug-print-line " Features: " (svref buf 82) " " (svref buf 83) " " (svref buf 84) " " (svref buf 85) " " (svref buf 86) " " (svref buf 87))
      (debug-print-line " Removable Media Notification: " (svref buf 127))
      (when (or (not (logbitp 15 general-config))
                (logbitp 14 general-config))
        (debug-print-line "Device does not implement the PACKET command set.")
        (return-from ata-detect-packet-device))
      (when (not (eql (ldb (byte 5 8) general-config) 5))
        (debug-print-line "PACKET device is not a CD-ROM drive.")
        (return-from ata-detect-packet-device))
      (when (not cdb-size)
        (debug-print-line "PACKET device has unsupported CDB size " (ldb (byte 2 0) (svref buf 0)))
        (return-from ata-detect-packet-device))
      (let ((device (make-atapi-device :controller controller
                                       :channel channel
                                       :cdb-size cdb-size
                                       :initialized-p nil)))
        (cdrom-initialize-device device cdb-size 'ata-issue-packet-command)
        (setf (atapi-device-initialized-p device) t)))))

(defun ata-detect-drive (controller channel)
  (let ((buf (sys.int::make-simple-vector 256 :wired)))
    ;; Select the device.
    (when (not (ata-select-device controller channel))
      (debug-print-line "Could not select ata device when probing.")
      (return-from ata-detect-drive nil))
    ;; Issue IDENTIFY.
    (setf (sys.int::io-port/8 (+ (ata-controller-command controller)
                                 +ata-register-command+))
          +ata-command-identify+)
    ;; Delay 400ns after writing command.
    (ata-alt-status controller)
    ;; Wait for BSY to clear and DRQ to go high.
    ;; Use a 1 second timeout.
    ;; I don't know if there's a standard timeout for this, but
    ;; I figure that the device should respond to IDENTIFY quickly.
    ;; Wrong blah! ata-wait-for-controller is nonsense.
    ;; if bsy = 0 & drq = 0, then there was an error.
    (let ((success (ata-wait-for-controller controller (logior +ata-bsy+ +ata-drq+) +ata-drq+ 1)))
      ;; Read the status register to clear the pending interrupt flag.
      (sys.int::io-port/8 (+ (ata-controller-command controller)
                             +ata-register-status+))
      ;; Check ERR before checking for timeout.
      ;; ATAPI devices will abort, and wait-for-controller will time out.
      (when (logtest (ata-alt-status controller) +ata-err+)
        (if (and (logtest (sys.int::io-port/8 (+ (ata-controller-command controller)
                                                   +ata-register-error+))
                            +ata-abrt+)
                   ;; The LBA low and interrupt reason registers should both be set to #x01,
                   ;; but some hardware is broken and doesn't do this.
                   (eql (sys.int::io-port/8 (+ (ata-controller-command controller)
                                               +ata-register-lba-mid+))
                        #x14)
                   (eql (sys.int::io-port/8 (+ (ata-controller-command controller)
                                               +ata-register-lba-high+))
                        #xEB))
            (return-from ata-detect-drive
              (ata-detect-packet-device controller channel))
            (debug-print-line "IDENTIFY aborted by device."))
        (return-from ata-detect-drive))
      (when (not success)
        (debug-print-line "Timeout while waiting for DRQ during IDENTIFY.")
        (return-from ata-detect-drive)))
    ;; Read data.
    (dotimes (i 256)
      (setf (svref buf i) (sys.int::io-port/16 (+ (ata-controller-command controller)
                                                  +ata-register-data+))))
    (let* ((supported-command-sets (svref buf 83))
           (lba48-capable (logbitp 10 supported-command-sets))
           (sector-size (if (and (logbitp 14 (svref buf 106))
                                 (not (logbitp 13 (svref buf 106))))
                            ;; Data in logical sector size field valid.
                            (logior (svref buf 117)
                                    (ash (svref buf 118) 16))
                            ;; Not valid, use 512.
                            512))
           (sector-count (if lba48-capable
                             (logior (svref buf 100)
                                     (ash (svref buf 101) 16)
                                     (ash (svref buf 102) 32)
                                     (ash (svref buf 103) 48))
                             (logior (svref buf 60)
                                     (ash (svref buf 61) 16))))
           (device (make-ata-device :controller controller
                                    :channel channel
                                    :block-size sector-size
                                    :sector-count sector-count
                                    :lba48-capable lba48-capable)))
      (debug-print-line "Features (83): " supported-command-sets)
      (debug-print-line "Sector size: " sector-size)
      (debug-print-line "Sector count: " sector-count)
      (register-disk device t (ata-device-sector-count device) (ata-device-block-size device) 256 'ata-read 'ata-write))))

(defun ata-issue-lba28-command (device lba count command)
  (let ((controller (ata-device-controller device)))
    ;; Select the device.
    (when (not (ata-select-device controller (ata-device-channel device)))
      (debug-print-line "Could not select ata device.")
      (return-from ata-issue-lba28-command nil))
    (latch-reset (ata-controller-irq-latch controller))
    ;; HI3: Write_parameters
    (setf (sys.int::io-port/8 (+ (ata-controller-command controller)
                                 +ata-register-count+))
          (if (eql count 256)
              0
              count))
    (setf (sys.int::io-port/8 (+ (ata-controller-command controller)
                                 +ata-register-lba-low+))
          (ldb (byte 8 0) lba))
    (setf (sys.int::io-port/8 (+ (ata-controller-command controller)
                                 +ata-register-lba-mid+))
          (ldb (byte 8 8) lba))
    (setf (sys.int::io-port/8 (+ (ata-controller-command controller)
                                 +ata-register-lba-high+))
          (ldb (byte 8 16) lba))
    (setf (sys.int::io-port/8 (+ (ata-controller-command controller)
                                 +ata-register-device+))
          (logior (ecase (ata-device-channel device)
                    (:master 0)
                    (:slave +ata-dev+))
                  +ata-lba+
                  (ldb (byte 4 24) lba)))
    ;; HI4: Write_command
    (setf (sys.int::io-port/8 (+ (ata-controller-command controller)
                                 +ata-register-command+))
          command))
  t)

(defun ata-issue-lba48-command (device lba count command)
  (let* ((controller (ata-device-controller device))
         (command-base (ata-controller-command controller)))
    ;; Select the device.
    (when (not (ata-select-device controller (ata-device-channel device)))
      (debug-print-line "Could not select ata device.")
      (return-from ata-issue-lba48-command nil))
    (latch-reset (ata-controller-irq-latch controller))
    ;; HI3: Write_parameters
    (when (eql count 65536)
      (setf count 0))
    (flet ((wr (reg val)
             (setf (sys.int::io-port/8 (+ command-base reg)) val)))
      (declare (dynamic-extent #'wr))
      (wr +ata-register-count+    (ldb (byte 8 8) count))
      (wr +ata-register-count+    (ldb (byte 8 0) count))
      (wr +ata-register-lba-high+ (ldb (byte 8 40) lba))
      (wr +ata-register-lba-mid+  (ldb (byte 8 32) lba))
      (wr +ata-register-lba-low+  (ldb (byte 8 24) lba))
      (wr +ata-register-lba-high+ (ldb (byte 8 16) lba))
      (wr +ata-register-lba-mid+  (ldb (byte 8 8) lba))
      (wr +ata-register-lba-low+  (ldb (byte 8 0) lba))
      (wr +ata-register-device+ (logior (ecase (ata-device-channel device)
                                          (:master 0)
                                          (:slave +ata-dev+))
                                        +ata-lba+))
      ;; HI4: Write_command
      (wr +ata-register-command+ command)))
  t)

(defun ata-check-status (controller &optional (timeout 30))
  "Wait until BSY clears, then return two values.
First is true if DRQ is set, false if DRQ is clear or timeout.
Second is true if the timeout expired.
This is used to implement the Check_Status states of the various command protocols."
  ;; Sample the alt-status register for the required delay.
  (ata-alt-status controller)
  (loop
     (let ((status (ata-alt-status controller)))
       (when (not (logtest status +ata-bsy+))
         (return (values (logtest status +ata-drq+)
                         nil)))
       ;; Stay in Check_Status.
       (when (<= timeout 0)
         (return (values nil t)))
       (sleep 0.001)
       (decf timeout 0.001))))

(defun ata-intrq-wait (controller &optional (timeout 30))
  "Wait for a interrupt from the device.
This is used to implement the INTRQ_Wait state."
  (declare (ignore timeout))
  ;; FIXME: Timeouts.
  (latch-wait (ata-controller-irq-latch controller))
  (latch-reset (ata-controller-irq-latch controller)))

(defun ata-pio-data-in (device count mem-addr)
  "Implement the PIO data-in protocol."
  (let ((controller (ata-device-controller device)))
    (loop
       ;; HPIOI0: INTRQ_wait
       (ata-intrq-wait controller)
       ;; HPIOI1: Check_Status
       (multiple-value-bind (drq timed-out)
           (ata-check-status controller)
         (when timed-out
           ;; FIXME: Should reset the device here.
           (debug-print-line "Device timeout during PIO data in.")
           (return-from ata-pio-data-in nil))
         (when (not drq)
           ;; FIXME: Should reset the device here.
           (debug-print-line "Device error during PIO data in.")
           (return-from ata-pio-data-in nil)))
       ;; HPIOI2: Transfer_Data
       (dotimes (i 256) ; FIXME: non-512 byte sectors, non 2-byte words.
         (setf (sys.int::memref-unsigned-byte-16 mem-addr 0)
               (sys.int::io-port/16 (+ (ata-controller-command controller)
                                       +ata-register-data+)))
         (incf mem-addr 2))
       ;; If there are no more blocks to transfer, transition back to host idle,
       ;; otherwise return to HPIOI0.
       (when (zerop (decf count))
         (return t)))))

(defun ata-pio-data-out (device count mem-addr)
  "Implement the PIO data-out protocol."
  (let ((controller (ata-device-controller device)))
    (loop
       ;; HPIOO0: Check_Status
       (multiple-value-bind (drq timed-out)
           (ata-check-status controller)
         (when timed-out
           ;; FIXME: Should reset the device here.
           (debug-print-line "Device timeout during PIO data out.")
           (return-from ata-pio-data-out nil))
         (when (not drq)
           (cond ((zerop count)
                  ;; All data transfered successfully.
                  (return-from ata-pio-data-out t))
                 (t ;; Error?
                  ;; FIXME: Should reset the device here.
                  (debug-print-line "Device error during PIO data out.")
                  (return-from ata-pio-data-out nil)))))
       ;; HPIOO1: Transfer_Data
       (dotimes (i 256) ; FIXME: non-512 byte sectors, non 2-byte words.
         (setf (sys.int::io-port/16 (+ (ata-controller-command controller)
                                       +ata-register-data+))
               (sys.int::memref-unsigned-byte-16 mem-addr 0))
         (incf mem-addr 2))
       ;; HPIOO2: INTRQ_Wait
       (ata-intrq-wait controller)
       ;; Return to HPIOO0.
       (decf count))))

(defun ata-configure-prdt (controller phys-addr n-octets direction)
  (let* ((prdt (ata-controller-prdt-phys controller))
         (prdt-virt (convert-to-pmap-address prdt)))
    (do ((offset 0))
        ((<= n-octets #x10000)
         ;; Write final chunk.
         (setf (sys.int::memref-unsigned-byte-32 prdt-virt offset) phys-addr
               (sys.int::memref-unsigned-byte-32 prdt-virt (1+ offset)) (logior #x80000000
                                                                                ;; 0 = 64k
                                                                                (logand n-octets #xFFFF))))
      ;; Write 64k chunks.
      (setf (sys.int::memref-unsigned-byte-32 prdt-virt offset) phys-addr
            (sys.int::memref-unsigned-byte-32 prdt-virt (1+ offset)) 0)
      (incf phys-addr #x10000))
    ;; Write the PRDT location.
    (setf (pci-io-region/32 (ata-controller-bus-master-register controller) +ata-bmr-prdt-address+) prdt
          ;; Clear DMA status. Yup. You have to write 1 to clear bits.
          (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+) (logior +ata-bmr-status-error+ +ata-bmr-status-interrupt+)
          ;; Set direction.
          (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) (ecase direction
                                                                                                (:read +ata-bmr-direction-read/write+)
                                                                                                (:write 0)))))

(defun ata-read-write (device lba count mem-addr what dma-fn pio-fn)
  (let ((controller (ata-device-controller device)))
    (ensure (>= lba 0))
    (ensure (>= count 0))
    (ensure (<= (+ lba count) (ata-device-sector-count device)))
    (cond
      ((ata-device-lba48-capable device)
       (when (> count 65536)
         (debug-print-line "Can't do " what " of more than 65,536 sectors.")
         (return-from ata-read-write (values nil :too-many-sectors))))
      (t
       (when (> count 256)
         (debug-print-line "Can't do " what " of more than 256 sectors.")
         (return-from ata-read-write (values nil :too-many-sectors)))))
    (when (eql count 0)
      (return-from ata-read-write t))
    (cond ((and (<= +physical-map-base+ mem-addr)
                ;; 4GB limit.
                (< mem-addr (+ +physical-map-base+ (* 4 1024 1024 1024))))
           (funcall dma-fn controller device lba count (- mem-addr +physical-map-base+)))
          ((eql (* count (ata-device-block-size device)) +4k-page-size+)
           ;; Transfer is small enough that the bounce page can be used.
           (let* ((bounce-frame (ata-controller-bounce-buffer controller))
                  (bounce-phys (ash bounce-frame 12))
                  (bounce-virt (convert-to-pmap-address bounce-phys)))
             ;; FIXME: Don't copy a whole page
             (when (eql what :write)
               (%fast-page-copy bounce-virt mem-addr))
             (funcall dma-fn controller device lba count bounce-phys)
             (when (eql what :read)
               (%fast-page-copy mem-addr bounce-virt))))
          (t ;; Give up and do a slow PIO transfer.
           (funcall pio-fn controller device lba count mem-addr))))
  t)

(defun ata-issue-lba-command (device lba count command28 command48)
  (if (ata-device-lba48-capable device)
      (ata-issue-lba48-command device lba count command48)
      (ata-issue-lba28-command device lba count command28)))

(defun ata-read-pio (controller device lba count mem-addr)
  (when (not (ata-issue-lba-command device lba count
                                    +ata-command-read-sectors+
                                    +ata-command-read-sectors-ext+))
    (return-from ata-read-pio (values nil :device-error)))
  (when (not (ata-pio-data-in device count mem-addr))
    (return-from ata-read-pio (values nil :device-error))))

(defun ata-read-dma (controller device lba count phys-addr)
  (ata-configure-prdt controller phys-addr (* count 512) :read)
  (when (not (ata-issue-lba-command device lba count
                                    +ata-command-read-dma+
                                    +ata-command-read-dma-ext+))
    (return-from ata-read-dma (values nil :device-error)))
  ;; Start DMA.
  ;; FIXME: Bochs has absurd timing requirements here. Needs to be *immediately* (tens of instructions)
  ;; after the command write.
  (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) (logior +ata-bmr-command-start+
                                                                                                    +ata-bmr-direction-read/write+))
  ;; Wait for completion.
  (ata-intrq-wait controller)
  (let ((status (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+)))
    ;; Stop the transfer.
    (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) +ata-bmr-direction-read/write+)
    ;; Clear error bit.
    (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+) (logior status +ata-bmr-status-error+ +ata-bmr-status-interrupt+))
    (if (logtest status +ata-bmr-status-error+)
        (values nil :device-error)
        t)))

(defun ata-read (device lba count mem-addr)
  (ata-read-write device lba count mem-addr
                  :read #'ata-read-dma #'ata-read-pio))

(defun ata-write-pio (controller device lba count mem-addr)
  (when (not (ata-issue-lba-command device lba count
                                    +ata-command-write-sectors+
                                    +ata-command-write-sectors-ext+))
    (return-from ata-write-pio (values nil :device-error)))
  (when (not (ata-pio-data-out device count mem-addr))
    (return-from ata-write-pio (values nil :device-error))))

(defun ata-write-dma (controller device lba count phys-addr)
  (ata-configure-prdt controller phys-addr (* count 512) :write)
  (when (not (ata-issue-lba-command device lba count
                                    +ata-command-write-dma+
                                    +ata-command-write-dma-ext+))
    (return-from ata-write-dma (values nil :device-error)))
  ;; Start DMA.
  ;; FIXME: Bochs has absurd timing requirements here. Needs to be *immediately* (tens of instructions)
  ;; after the command write.
  (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) +ata-bmr-command-start+)
  ;; Wait for completion.
  (ata-intrq-wait controller)
  (let ((status (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+)))
    ;; Stop the transfer.
    (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) 0)
    ;; Clear error bit.
    (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+) (logior status +ata-bmr-status-error+ +ata-bmr-status-interrupt+))
    (if (logtest status +ata-bmr-status-error+)
        (values nil :device-error)
        t)))

(defun ata-write (device lba count mem-addr)
  (ata-read-write device lba count mem-addr
                  :write #'ata-write-dma #'ata-write-pio))

(defun ata-submit-packet-command (device cdb result-len dma dmadir)
  (let* ((controller (atapi-device-controller device))
         (command-base (ata-controller-command controller)))
    ;; Select the device.
    (when (not (ata-select-device controller (atapi-device-channel device)))
      (debug-print-line "Could not select ata device.")
      (return-from ata-submit-packet-command nil))
    (latch-reset (ata-controller-irq-latch controller))
    ;; HI3: Write_parameters
    (flet ((wr (reg val)
             (setf (sys.int::io-port/8 (+ command-base reg)) val)))
      (declare (dynamic-extent #'wr))
      (wr +ata-register-features+ (if dma
                                      (logior #b0001
                                              (ecase dmadir
                                                (:device-to-host #b0100)
                                                (:host-to-device #b0000)))
                                      #b0000))
      ;; Set maximum transfer size.
      (cond ((eql result-len 0)
             ;; Some devices use 0 to mean some other value, use a tiny result instead.
             (wr +ata-register-lba-mid+  2)
             (wr +ata-register-lba-high+ 0))
            (t
             ;; Some devices use 0 to mean some other value, use a tiny result instead.
             (wr +ata-register-lba-mid+  (ldb (byte 8 0) result-len))
             (wr +ata-register-lba-high+ (ldb (byte 8 8) result-len))))
      ;; HI4: Write_command
      (wr +ata-register-command+ +ata-command-packet+))
    ;; Delay 400ns after writing command.
    (ata-alt-status controller)
    ;; HP0: Check_Status_A
    (multiple-value-bind (drq timed-out)
        (ata-check-status controller)
      (when timed-out
        ;; FIXME: Should reset the device here.
        (debug-print-line "Device timeout during PACKET Check_Status_A.")
        (return-from ata-submit-packet-command nil))
      (when (not drq)
        ;; FIXME: Should reset the device here.
        (debug-print-line "Device error " (ata-error controller) " during PACKET Check_Status_A.")
        (return-from ata-submit-packet-command nil)))
    ;; HP1: Send_Packet
    ;; Send the command a word at a time.
    (loop
       for i from 0 by 2 below (atapi-device-cdb-size device)
       for lo = (svref cdb i)
       for hi = (svref cdb (1+ i))
       for word = (logior lo (ash hi 8))
       do (setf (sys.int::io-port/16 (+ command-base +ata-register-data+)) word)))
  t)

(defun ata-issue-pio-packet-command (device cdb result-buffer result-len)
  (let* ((controller (atapi-device-controller device))
         (command-base (ata-controller-command controller)))
    (when (not (ata-submit-packet-command device cdb result-len nil nil))
      (return-from ata-issue-pio-packet-command nil))
    ;; HP3: INTRQ_wait
    (when (atapi-device-initialized-p device)
      (ata-intrq-wait controller))
    ;; HP2: Check_Status_B
    (multiple-value-bind (drq timed-out)
        (ata-check-status controller)
      (when timed-out
        ;; FIXME: Should reset the device here.
        (debug-print-line "Device timeout during PACKET Check_Status_B.")
        (return-from ata-issue-pio-packet-command nil))
      (when (logtest +ata-err+ (ata-alt-status controller))
        ;; FIXME: Should reset the device here.
        (debug-print-line "Device error " (ata-error controller) " during PACKET Check_Status_B.")
        (return-from ata-issue-pio-packet-command nil))
      (cond (drq
             ;; Data ready for transfer.
             ;; HP4: Transfer_Data
             (let ((len (logior (sys.int::io-port/8 (+ command-base +ata-register-lba-mid+))
                                (ash (sys.int::io-port/8 (+ command-base +ata-register-lba-high+)) 8))))
               (loop
                  for i from 0 by 2 below (min len result-len)
                  do
                    (setf (sys.int::memref-unsigned-byte-16 (+ result-buffer i))
                          (sys.int::io-port/16 (+ (ata-controller-command controller)
                                                  +ata-register-data+))))
               ;; Read any remaining data.
               (loop
                  for i from 0 by 2 below (- len result-len)
                  do (sys.int::io-port/16 (+ (ata-controller-command controller)
                                             +ata-register-data+)))
               len))
            (t
             ;; No data to transfer.
             0)))))

(defun ata-issue-dma-packet-command (device cdb result-buffer-phys result-len)
  (let* ((controller (atapi-device-controller device))
         (command-base (ata-controller-command controller)))
    (ata-configure-prdt controller result-buffer-phys result-len :read)
    (when (not (ata-submit-packet-command device cdb result-len t :device-to-host))
      (return-from ata-issue-dma-packet-command nil))
    ;; Start DMA.
    (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) (logior +ata-bmr-command-start+
                                                                                                      +ata-bmr-direction-read/write+))
    ;; Wait for completion.
    (ata-intrq-wait controller)
    ;; FIXME: Should go to Check_Status_B here.
    (let ((status (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+)))
      ;; Stop the transfer.
      (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) +ata-bmr-direction-read/write+)
      ;; Clear error bit.
      (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+) (logior status +ata-bmr-status-error+ +ata-bmr-status-interrupt+))
      (if (logtest status +ata-bmr-status-error+)
          (values nil :device-error)
          t))))

(defun ata-issue-packet-command (device cdb result-buffer result-len)
  (ensure (eql (sys.int::simple-vector-length cdb)
               (atapi-device-cdb-size device)))
  (ensure (eql (rem result-len 4) 0))
  (cond ((not (atapi-device-initialized-p device))
         (ata-issue-pio-packet-command device cdb result-buffer result-len))
        ((or (null result-buffer)
             (and (<= +physical-map-base+ result-buffer)
                  ;; 4GB limit.
                  (< result-buffer (+ +physical-map-base+ (* 4 1024 1024 1024)))))
         (ata-issue-dma-packet-command device
                                       cdb
                                       (if result-buffer
                                           (- result-buffer +physical-map-base+)
                                           0)
                                       result-len))
        ((eql result-len +4k-page-size+)
         ;; Transfer is small enough that the bounce page can be used.
         (let* ((controller (atapi-device-controller device))
                (bounce-frame (ata-controller-bounce-buffer controller))
                (bounce-phys (ash bounce-frame 12))
                (bounce-virt (convert-to-pmap-address bounce-phys)))
           (ata-issue-dma-packet-command device
                                         cdb
                                         bounce-phys
                                         result-len)
           ;; FIXME: Don't copy a whole page.
           (%fast-page-copy result-buffer bounce-virt)))
        (t ;; Give up and do a slow PIO transfer.
         (ata-issue-pio-packet-command device cdb result-buffer result-len))))

(defun ata-irq-handler (controller)
  ;; Read the status register to clear the interrupt pending state.
  (sys.int::io-port/8 (+ (ata-controller-command controller)
                         +ata-register-status+))
  (latch-trigger (ata-controller-irq-latch controller)))

(defun init-ata-controller (command-base control-base bus-master-register prdt-phys irq)
  (declare (sys.c::closure-allocation :wired))
  (debug-print-line "New controller at " command-base " " control-base " " bus-master-register " " irq)
  (let* ((dma32-bounce-buffer (allocate-physical-pages 1
                                                       :mandatory-p "ATA DMA bounce buffer"
                                                       :32-bit-only t))
         (controller (make-ata-controller :command command-base
                                          :control control-base
                                          :bus-master-register bus-master-register
                                          :prdt-phys prdt-phys
                                          :irq irq
                                          :bounce-buffer dma32-bounce-buffer)))
    ;; Disable IRQs on the controller and reset both drives.
    (setf (sys.int::io-port/8 (+ control-base +ata-register-device-control+))
          (logior +ata-srst+ +ata-nien+))
    (sleep 0.000005) ; Hold SRST high for 5μs.
    (setf (sys.int::io-port/8 (+ control-base +ata-register-device-control+))
          +ata-nien+)
    (sleep 0.002) ; Hold SRST low for 2ms before probing for drives.
    ;; Now wait for BSY to clear. It may take up to 31 seconds for the
    ;; reset to finish, which is a bit silly...
    (when (not (ata-wait-for-controller controller +ata-bsy+ 0 2))
      ;; BSY did not go low, no devices on this controller.
      (debug-print-line "No devices on ata controller.")
      (return-from init-ata-controller))
    (debug-print-line "Probing ata controller.")
    ;; Attach interrupt handler.
    (i8259-hook-irq irq (lambda (interrupt-frame irq)
                          (declare (ignore interrupt-frame irq))
                          (ata-irq-handler controller)))
    (i8259-unmask-irq irq)
    ;; Probe drives.
    (ata-detect-drive controller :master)
    (ata-detect-drive controller :slave)
    ;; Enable controller interrupts.
    (setf (sys.int::io-port/8 (+ control-base +ata-register-device-control+)) 0)))

(defun ata-pci-register (location)
  (let* ((prdt-page (allocate-physical-pages 1
                                             :mandatory-p "ATA PRDT page"
                                             :32-bit-only t)))
    ;; Make sure to enable PCI bus mastering for this device.
    (setf (pci-config/16 location +pci-config-command+) (logior (pci-config/16 location +pci-config-command+)
                                                                (ash 1 +pci-command-bus-master+)))
    ;; Ignore controllers not in compatibility mode, they share IRQs.
    ;; It's not a problem for the ATA driver, but the supervisor's IRQ handling
    ;; doesn't deal with shared IRQs at all.
    (when (not (logbitp 0 (pci-programming-interface location)))
      (init-ata-controller +ata-compat-primary-command+
                           +ata-compat-primary-control+
                           (pci-bar location 4)
                           (* prdt-page +4k-page-size+)
                           +ata-compat-primary-irq+))
    (when (not (logbitp 2 (pci-programming-interface location)))
      (init-ata-controller +ata-compat-secondary-command+
                           +ata-compat-secondary-control+
                           (+ (pci-bar location 4) 8)
                           (+ (* prdt-page +4k-page-size+) 2048)
                           +ata-compat-secondary-irq+))))
