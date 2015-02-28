;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
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

;; Commands.
(defconstant +ata-command-read-sectors+ #x20)
(defconstant +ata-command-read-dma+ #xC8)
(defconstant +ata-command-write-sectors+ #x30)
(defconstant +ata-command-write-dma+ #xCA)
(defconstant +ata-command-identify+ #xEC)

(defvar *ata-devices*)

(defstruct (ata-controller
             (:area :wired))
  ;; Taken when accessing the controller.
  (access-lock (make-mutex "ATA Access Lock" :spin))
  ;; Taken while there's a command in progress.
  (command-lock (make-mutex "ATA Command Lock"))
  command
  control
  bus-master-register
  prdt-phys
  irq
  current-channel
  (irq-cvar (make-condition-variable "ATA IRQ Notifier"))
  irq-delivered
  hotunplug)

(defstruct (ata-device
             (:area :wired))
  controller
  channel
  block-size
  sector-count)

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
     (sleep 0.001)
     (decf timeout 0.001)))

(defun ata-select-device (controller channel)
  ;; select-device should never be called with a command in progress on the controller.
  (when (logtest (logior +ata-bsy+ +ata-drq+)
                 (ata-alt-status controller))
    (debug-write-line "ATA-SELECT-DEVICE called with command in progress.")
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
      (debug-write-line "ATA-SELECT-DEVICE called with command in progress.")
      (return-from ata-select-device nil))
    (setf (ata-controller-current-channel controller) channel))
  t)

(defun ata-detect-drive (controller channel)
  (let ((buf (sys.int::make-simple-vector 256 :wired)))
    (with-mutex ((ata-controller-access-lock controller))
      ;; Select the device.
      (when (not (ata-select-device controller channel))
        (debug-write-line "Could not select ata device when probing.")
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
        ;; Check ERR before checking for timeout.
        ;; ATAPI devices will abort, and wait-for-controller will time out.
        (when (logtest (ata-alt-status controller) +ata-err+)
          (debug-write-line "IDENTIFY aborted by device.")
          (return-from ata-detect-drive))
        (when (not success)
          (debug-write-line "Timeout while waiting for DRQ during IDENTIFY.")
          (return-from ata-detect-drive)))
      ;; Read the status register to clear the pending interrupt flag.
      (sys.int::io-port/8 (+ (ata-controller-command controller)
                             +ata-register-status+))
      ;; Read data.
      (dotimes (i 256)
        (setf (svref buf i) (sys.int::io-port/16 (+ (ata-controller-command controller)
                                                    +ata-register-data+)))))
    (let ((device (make-ata-device :controller controller
                                   :channel channel
                                   ;; Check for large sector drives.
                                   :block-size (if (and (logbitp 14 (svref buf 106))
                                                        (not (logbitp 13 (svref buf 106))))
                                                   (logior (ash (svref buf 118) 16)
                                                           (svref buf 117))
                                                   512)
                                   ;; TODO: LBA48 support.
                                   :sector-count (logior (ash (svref buf 61) 16)
                                                         (svref buf 60)))))
      (setf *ata-devices*
            (sys.int::cons-in-area
             device
             *ata-devices*
             :wired))
      (register-disk device (ata-device-sector-count device) (ata-device-block-size device) 256 'ata-read 'ata-write))))

(defun ata-issue-lba28-command (device lba count command)
  (let ((controller (ata-device-controller device)))
    ;; Select the device.
    (when (not (ata-select-device controller (ata-device-channel device)))
      (debug-write-line "Could not select ata device.")
      (return-from ata-issue-lba28-command nil))
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

(defun ata-check-status (device &optional (timeout 30))
  "Wait until BSY clears, then return two values.
First is true if DRQ is set, false if DRQ is clear or timeout.
Second is true if the timeout expired.
This is used to implement the Check_Status states of the various command protocols."
  (let ((controller (ata-device-controller device)))
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
         (decf timeout 0.001)))))

(defun ata-intrq-wait (device &optional (timeout 30))
  "Wait for a interrupt from the device.
This is used to implement the INTRQ_Wait state."
  (declare (ignore timeout))
  ;; FIXME: Timeouts.
  (let ((controller (ata-device-controller device)))
    (setf (ata-controller-irq-delivered controller) nil)
    (loop
       (condition-wait (ata-controller-irq-cvar controller)
                       (ata-controller-access-lock controller))
       (when (or (ata-controller-irq-delivered controller)
                 (ata-controller-hotunplug controller))
         (return)))))

(defun ata-pio-data-in (device count mem-addr)
  "Implement the PIO data-in protocol."
  (let ((controller (ata-device-controller device)))
    (loop
       ;; HPIOI0: INTRQ_wait
       (ata-intrq-wait device)
       (when (ata-controller-hotunplug controller)
         (debug-write-line "Pending PIO read aborted due to device hot-unplug.")
         (return-from ata-pio-data-in nil))
       ;; HPIOI1: Check_Status
       (multiple-value-bind (drq timed-out)
           (ata-check-status device)
         (when timed-out
           ;; FIXME: Should reset the device here.
           (debug-write-line "Device timeout during PIO data in.")
           (return-from ata-pio-data-in nil))
         (when (not drq)
           ;; FIXME: Should reset the device here.
           (debug-write-line "Device error during PIO data in.")
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
           (ata-check-status device)
         (when timed-out
           ;; FIXME: Should reset the device here.
           (debug-write-line "Device timeout during PIO data out.")
           (return-from ata-pio-data-out nil))
         (when (not drq)
           (cond ((zerop count)
                  ;; All data transfered successfully.
                  (return-from ata-pio-data-out t))
                 (t ;; Error?
                  ;; FIXME: Should reset the device here.
                  (debug-write-line "Device error during PIO data out.")
                  (return-from ata-pio-data-out nil)))))
       ;; HPIOO1: Transfer_Data
       (dotimes (i 256) ; FIXME: non-512 byte sectors, non 2-byte words.
         (setf (sys.int::io-port/16 (+ (ata-controller-command controller)
                                       +ata-register-data+))
               (sys.int::memref-unsigned-byte-16 mem-addr 0))
         (incf mem-addr 2))
       ;; HPIOO2: INTRQ_Wait
       (ata-intrq-wait device)
       (when (ata-controller-hotunplug controller)
         (debug-write-line "Pending PIO read aborted due to device hot-unplug.")
         (return-from ata-pio-data-out nil))
       ;; Return to HPIOO0.
       (decf count))))

(defun ata-configure-prdt (controller phys-addr n-octets direction)
  (let* ((prdt (ata-controller-prdt-phys controller))
         (prdt-virt (+ +physical-map-base+ prdt)))
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

(defun ata-read-pio (controller device lba count mem-addr)
  (when (not (ata-issue-lba28-command device lba count +ata-command-read-sectors+))
    (return-from ata-read-pio (values nil :device-error)))
  (when (not (ata-pio-data-in device count mem-addr))
    (return-from ata-read-pio (values nil :device-error))))

(defun ata-read-dma (controller device lba count phys-addr)
  (ata-configure-prdt controller phys-addr (* count 512) :read)
  (when (not (ata-issue-lba28-command device lba count +ata-command-read-dma+))
    (return-from ata-read-dma (values nil :device-error)))
  ;; Start DMA.
  ;; FIXME: Bochs has absurd timing requirements here. Needs to be *immediately* (tens of instructions)
  ;; after the command write.
  (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) (logior +ata-bmr-command-start+
                                                                                                    +ata-bmr-direction-read/write+))
  ;; Wait for completion.
  (ata-intrq-wait device)
  (let ((status (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+)))
    ;; Stop the transfer.
    (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) +ata-bmr-direction-read/write+)
    ;; Clear error bit.
    (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+) (logior status +ata-bmr-status-error+ +ata-bmr-status-interrupt+))
    (if (logtest status +ata-bmr-status-error+)
        (values nil :device-error)
        t)))

(defun ata-read (device lba count mem-addr)
  (let ((controller (ata-device-controller device)))
    (assert (>= lba 0))
    (assert (>= count 0))
    (assert (< (+ lba count) (ata-device-sector-count device)))
    (when (> count 256)
      (debug-write-line "Can't do reads of more than 256 sectors.")
      (return-from ata-read (values nil :too-many-sectors)))
    (when (eql count 0)
      (return-from ata-read t))
    (with-mutex ((ata-controller-command-lock controller))
      (with-mutex ((ata-controller-access-lock controller))
        (if (<= +physical-map-base+
                mem-addr
                ;; 4GB limit.
                (+ +physical-map-base+ (* 4 1024 1024 1024)))
            (ata-read-dma controller device lba count (- mem-addr +physical-map-base+))
            (ata-read-pio controller device lba count mem-addr)))))
  t)

(defun ata-write-pio (controller device lba count mem-addr)
  (when (not (ata-issue-lba28-command device lba count +ata-command-write-sectors+))
    (return-from ata-write-pio (values nil :device-error)))
  (when (not (ata-pio-data-out device count mem-addr))
    (return-from ata-write-pio (values nil :device-error))))

(defun ata-write-dma (controller device lba count phys-addr)
  (ata-configure-prdt controller phys-addr (* count 512) :write)
  (when (not (ata-issue-lba28-command device lba count +ata-command-write-dma+))
    (return-from ata-write-dma (values nil :device-error)))
  ;; Start DMA.
  ;; FIXME: Bochs has absurd timing requirements here. Needs to be *immediately* (tens of instructions)
  ;; after the command write.
  (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) +ata-bmr-command-start+)
  ;; Wait for completion.
  (ata-intrq-wait device)
  (let ((status (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+)))
    ;; Stop the transfer.
    (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-command+) 0)
    ;; Clear error bit.
    (setf (pci-io-region/8 (ata-controller-bus-master-register controller) +ata-bmr-status+) (logior status +ata-bmr-status-error+ +ata-bmr-status-interrupt+))
    (if (logtest status +ata-bmr-status-error+)
        (values nil :device-error)
        t)))

(defun ata-write (device lba count mem-addr)
  (let ((controller (ata-device-controller device)))
    (assert (>= lba 0))
    (assert (>= count 0))
    (assert (< (+ lba count) (ata-device-sector-count device)))
    (when (> count 256)
      (debug-write-line "Can't do writes of more than 256 sectors.")
      (return-from ata-write (values nil :too-many-sectors)))
    (when (eql count 0)
      (return-from ata-write t))
    (with-mutex ((ata-controller-command-lock controller))
      (with-mutex ((ata-controller-access-lock controller))
        (if (<= +physical-map-base+
                mem-addr
                ;; 4GB limit.
                (+ +physical-map-base+ (* 4 1024 1024 1024)))
            (ata-write-dma controller device lba count (- mem-addr +physical-map-base+))
            (ata-write-pio controller device lba count mem-addr)))))
  t)

(defun ata-irq-handler (irq)
  (dolist (drive *ata-devices*)
    (let ((controller (ata-device-controller drive)))
      (when (eql (ata-controller-irq controller) irq)
        (with-mutex ((ata-controller-access-lock controller))
          ;; Read the status register to clear the interrupt pending state.
          (sys.int::io-port/8 (+ (ata-controller-command controller)
                                 +ata-register-status+))
          (setf (ata-controller-irq-delivered controller) t)
          (condition-notify (ata-controller-irq-cvar controller)))))))

(defun init-ata-controller (command-base control-base bus-master-register prdt-phys irq)
  (debug-print-line "New controller at " command-base " " control-base " " bus-master-register " " irq)
  (let ((controller (make-ata-controller :command command-base
                                         :control control-base
                                         :bus-master-register bus-master-register
                                         :prdt-phys prdt-phys
                                         :irq irq)))
    ;; Disable IRQs on the controller and reset both drives.
    (setf (sys.int::io-port/8 (+ control-base +ata-register-device-control+))
          (logior +ata-srst+ +ata-nien+))
    (sleep 0.000005) ; Hold SRST high for 5μs.
    (setf (sys.int::io-port/8 (+ control-base +ata-register-device-control+))
          +ata-nien+)
    (sleep 0.002) ; Hold SRST low for 2ms before probing for drives.
    ;; Now wait for BSY to clear. It may take up to 31 seconds for the
    ;; reset to finish, which is a bit silly...
    (when (not (ata-wait-for-controller controller +ata-bsy+ 0 31))
      ;; BSY did not go low, no devices on this controller.
      (debug-write-line "No devices on ata controller.")
      (return-from init-ata-controller))
    (debug-write-line "Probing ata controller.")
    ;; Attach interrupt handler.
    (i8259-hook-irq irq 'ata-irq-handler) ; fixme: should clear pending irqs?
    (i8259-unmask-irq irq)
    ;; Probe drives.
    (ata-detect-drive controller :master)
    (ata-detect-drive controller :slave)
    ;; Enable controller interrupts.
    (setf (sys.int::io-port/8 (+ control-base +ata-register-device-control+)) 0)))

(defun initialize-ata ()
  (setf *ata-devices* '()))

(defun ata-pci-register (location)
  (let* ((prdt-page (allocate-physical-pages 1 "ATA PRDT page")))
    ;; Make sure to enable PCI bus mastering for this device.
    (setf (pci-config/16 location +pci-config-command+) (logior (pci-config/16 location +pci-config-command+)
                                                                ;; Bit 2 is Bus Master bit.
                                                                (ash 1 2)))
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
