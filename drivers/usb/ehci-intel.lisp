;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

;======================================================================
;;
;; This file is the Hardware Abstraction Layer (HAL) for the Intel
;; EHCI controller. In the USB 2.0 spec it's know as the Host
;; Controller Driver (HCD).
;;
;; It includes register definitions, the ehci-intel class and ehci
;; setup code. Any code that refereces registers in the Intel EHCI
;; controller should be in this file.
;;
;;======================================================================

(in-package :mezzano.driver.usb.ehci.intel)

(defvar *ehci*)                 ;; for debug
(defvar *error-qtd* nil)        ;; for debug
(defvar *trace* 0)              ;; for debug higher number -> more tracing

(defmacro with-trace-level ((trace-level) &body body)
  `(when (>= *trace* ,trace-level)
     ,@body))

(declaim (inline enter-function))

(defun enter-function (name)
  (with-trace-level (1)
    (sup:debug-print-line name)))

;;======================================================================
;; support routines
;;======================================================================

;;======================================================================
;;
;;======================================================================

(defconstant +ehci-pci-sbrn+          #x60)
(defconstant +ehci-pci-fladj+         #x61)
(defconstant +ehci-pci-portwakecap+   #x62)

;; Capability Register Offsets
(defconstant +ehci-cap-length+        #x00)
(defconstant +ehci-cap-hciversion+    #x02)
(defconstant +ehci-cap-hcsparams+     #x04)
(defconstant +ehci-cap-hccparams+     #x08)
(defconstant +ehci-cap-hcsp-portroute #x0C)

;; Operational Register Offsets
(defconstant +ehci-op-command+        #x00)
(defconstant +ehci-op-status+         #x04)
(defconstant +ehci-op-int-enable+     #x08)
(defconstant +ehci-op-frame-index+    #x0c)
(defconstant +ehci-op-4g-segment+     #x10)
(defconstant +ehci-op-frame-list+     #x14)
(defconstant +ehci-op-async-list+     #x18)
(defconstant +ehci-op-config-flag+    #x40)
(defconstant +ehci-op-port-status+    #x44)

(defconstant +ehci-status-halted-bit+ 12)

;; Port Status and Control Masks
(defconstant +portsc-mask-disconnect-enable+   (ash 1 21))
(defconstant +portsc-mask-connect-enable+      (ash 1 20))
(defconstant +portsc-mask-release-port+        (ash 1 13))
(defconstant +portsc-mask-power-on+            (ash 1 12))
(defconstant +portsc-mask-reset-port+          (ash 1  8))
(defconstant +portsc-mask-suspend+             (ash 1  7))
(defconstant +portsc-mask-resume+              (ash 1  6))
(defconstant +portsc-mask-clear-o/c-change+    (ash 1  5))
(defconstant +portsc-mask-clear-e/d-change+    (ash 1  3))
(defconstant +portsc-mask-enabled+             (ash 1  2))
(defconstant +portsc-mask-clear-status-change+ (ash 1  1))

(defconstant +ehci-portsc-connect-status+  0)
(defconstant +ehci-portsc-status-change+   1)

;; USB Status Register and USB Interrupt Enable Register bit definitions
(defconstant +intr-bit-transfer-done+     0)
(defconstant +intr-bit-usb-error+         1)
(defconstant +intr-bit-hub-status-change+ 2)
(defconstant +intr-bit-frame-rollover+    3)
(defconstant +intr-bit-pci-error+         4)
(defconstant +intr-bit-async-advance+     5)

;; USB Status Register and USB Interrupt Enable Register Masks
(defconstant +intr-mask-transfer-done+     #x01)
(defconstant +intr-mask-usb-error+         #x02)
(defconstant +intr-mask-hub-status-change+ #x04)
(defconstant +intr-mask-frame-rollover+    #x08)
(defconstant +intr-mask-pci-error+         #x10)
(defconstant +intr-mask-async-advance+     #x20)

;;======================================================================
;; Queue Element Transfer Descriptor (qTD) EHCI Controller Spec section 3.5
;;======================================================================

(defconstant +pid-out-token+   #b00)
(defconstant +pid-in-token+    #b01)
(defconstant +pid-setup-token+ #b10)
;; Reserved                    #b11

(defconstant +qtd-status-mask+ #xF8)

(defconstant +qtd-error-codes+
  '(#b00001000 "Transaction Error"
    #b00010000 "Babble Detected"
    #b00100000 "Data Buffer Error"
    #b01000000 "Halted"))

(defun alloc-qtd (ehci &key event-type endpoint buf-size buf)
  (let ((qtd (alloc-buffer/32 (buf-pool ehci) 8)))
    (dotimes (idx 8)
      (setf (aref qtd idx) 0))
    (setf (gethash qtd (qtd->xfer-info ehci))
          (make-xfer-info :event-type event-type
                          :endpoint endpoint
                          :buf-size buf-size
                          :buf buf))
    qtd))

(defun free-qtd (ehci qtd)
  (remhash qtd (qtd->xfer-info ehci))
  (free-buffer qtd))

(defun qtd-token (tqd)
  (aref tqd 2))

(defun encode-qtd (qtd dt num-bytes ioc c_page cerr pid buf)
  (setf (aref qtd 0) #x00000001         ;; no next qTD
        (aref qtd 1) #x00000001         ;; no alt next qTD
        (aref qtd 2) (logior (dpb dt        (byte  1 31) 0)
                             (dpb num-bytes (byte 15 16) 0)
                             (dpb ioc       (byte  1 15) 0)
                             (dpb c_page    (byte  3 12) 0)
                             (dpb cerr      (byte  2 10) 0)
                             (dpb pid       (byte  2  8) 0)
                             (dpb 1         (byte  1  7) 0)) ;;Active flag
        (aref qtd 3) (array->phys-addr buf)))

;;======================================================================
;; Queue Head EHCI Controller Spec section 3.6
;;======================================================================

(defconstant +qh-type-iTD+  #b00)
(defconstant +qh-type-QH+   #b01)
(defconstant +qh-type-siTD  #b10)
(defconstant +qh-type-FSTN+ #b11)

(defconstant +qh-eps-full-speed+ #b00)
(defconstant +qh-eps-low-speed+  #b01)
(defconstant +qh-eps-high-speed+ #b10)

(defconstant +qh-header-address-field+ (byte 7 0))

(defun alloc-qh (ehci)
  (let ((qh (alloc-buffer/32 (buf-pool ehci) 12)))
    (dotimes (idx 12)
      (setf (aref qh idx) 0))
    qh))

(defun free-qh (ehci qh)
  ;; TODO free any assocaited qtds
  (declare (ignore ehci))
  (free-buffer qh))

(defun qh-next-qh (qh)
  (logandc2 (aref qh 0) #x1F))

(defun (setf qh-next-qh) (value qh type)
  (when (logtest #x1F value)
    (error "QH Link address not 32-byte aligned ~16,'0X" value))
  (setf (aref qh 0) (logior (ash type 1) value)))

(defun qh-next-qtd (qh)
  (logandc2 (aref qh 4) #x1F))

(defun (setf qh-next-qtd) (value qh)
  (setf (aref qh 4) value))

(defun encode-qh (qh control-ep max-packet toggle speed ep-num addr s-mask)
  (setf (aref qh 1) (logior (if (and control-ep (eq speed :full)) #x08000000 0)
                             (ash max-packet 16)
                             (ash toggle 14)
                             (getf '(:full 0 :low #x00001000 :high #x00002000)
                                   speed)
                             (ash ep-num 8)
                             addr)
        (aref qh 2) (logior #x40000000  ;; Pipe Multiplier #b01
                             s-mask)
        (aref qh 4) #x00000001          ;; No valid qTDs.
        (aref qh 5) #x00000001          ;; Alt qTD not valid
        ))

(defun qh-header (qh)
  (aref qh 1))

(defun (setf qh-header) (value qh)
  (setf (aref qh 1) value))

(defun qh-next-qtd (qh)
  (aref qh 4))

(defun (setf qh-next-qtd) (value qh)
  (setf (aref qh 4) value))

(defun qh-token (qh)
  (aref qh 6))

(defun (setf qh-token) (value qh)
  (setf (aref qh 6) value))

;;======================================================================
;; EHCI Endpoint Structure
;;======================================================================

(defstruct (ehci-endpoint (:include endpoint))
  qh
  (pid NIL)    ;; Only used by bulk endpoints
  )

(declaim (inline device-control-qh))

(defun device-control-qh (device)
  (ehci-endpoint-qh (aref (usb-device-endpoints device) 0)))

;;======================================================================
;; EHCI Device Class
;;======================================================================

(defclass ehci-device (usb-device)
  ((%addr          :initarg  :addr         :accessor device-addr)
   (%speed         :initarg  :speed        :accessor device-speed)
   ;; TODO - move this to usb-device?
   (%control-event :initarg  :control-event :accessor device-control-event)))

(defmethod (setf usb-device-max-packet) :after (max-packet (device ehci-device))
  (let ((qh (device-control-qh device)))
    (setf (qh-header qh) (logior (ash max-packet 16)
                                 (logandc2 (qh-header qh) #x07FF0000)))))

;;======================================================================
;; EHCI Driver Class
;;======================================================================

(defclass ehci-intel (usbd)
  (
   ;; base address of Capability Registers
   (%cap-regs         :initarg :cap-regs         :accessor cap-regs)
   ;; base address of the Operational Registers
   (%op-regs          :initarg :op-regs          :accessor op-regs)
   ;; HCSPARAMS fields
   (%power-control    :initarg :power-control    :accessor power-control)
   (%routing-rules    :initarg :routing-rules    :accessor routing-rules)
   (%n-pcc            :initarg :n-pcc            :accessor n-pcc)
   (%n-cc             :initarg :n-cc             :accessor n-cc)
   (%p-indicator      :initarg :p-indicator      :accessor p-indicator)
   (%debug-port       :initarg :debug-port       :accessor debug-port)
   ;; HCCPARAMS fields
   (%64-bit-cap       :initarg :64-bit-cap       :accessor 64-bit-cap)
   (%frame-list-flag  :initarg :frame-list-flag  :accessor frame-list-flag)
   (%async-park-cap   :initarg :async-park-cap   :accessor async-park-cap)
   (%isoch-threshold  :initarg :isoch-threshold  :accessor isoch-threshold)
   (%extended-cap     :initarg :extended-cap     :accessor extended-cap)

   ;; physical address of memory used for buffers
   (%phys-addr        :initarg :phys-addr        :accessor phys-addr)
   (%phys-addr-free   :initarg :phys-addr-free   :accessor phys-free-free)
   (%phys-addr-end    :initarg :phys-addr-end    :accessor phys-phys-end)
   (%phys-addr-high   :initarg :phys-addr-high   :accessor phys-addr-high)

   ;; Periodic Frame List
   (%pfl-phys-addr    :initarg :pfl-phys-addr    :accessor pfl-phys-addr)
   (%pfl-table        :initarg :pfl-table        :accessor pfl-table)

   (%async-qh        :initarg :async-qh         :accessor async-qh)
   (%pending-qtds    :initform NIL              :accessor pending-qtds)
   (%qtd->xfer-info  :initarg :qtd->xfer-info   :accessor qtd->xfer-info)
   ))

(defmethod delete-controller ((ehci ehci-intel))
  (sup:debug-print-line "Deleting EHCI (intel) controller")
  (sup:terminate-thread (interrupt-thread ehci)))

;; Physical memory allocation
(defconstant +ehci-phys-memory-pages+ 8)

(defun wait-for-async-doorbell (ehci)
  ;; TODO - waiting holding the mutex is rude, find a better method
  (sup:with-mutex ((usbd-lock ehci))
    (setf (command-reg ehci) (logior #x00000040 (command-reg ehci)))
    (loop
       for time = 0.0 then (+ time 0.01)
       when (logbitp 5 (status-reg ehci)) do
         (setf (status-reg ehci) #x00000020)
         (return)
       when (> time 1.0) do
         (error "timeout waiting for the async doorbell ~D" time)
       do
         (sleep 0.01))))

(defun ehci-addr->array (ehci phys-addr)
  (phys-addr->array (logior (phys-addr-high ehci) phys-addr)))

;;======================================================================
;; Register access routines
;;======================================================================

(defun command-reg (ehci)
  (pci:pci-io-region/32 (op-regs ehci) +ehci-op-command+))

(defun (setf command-reg) (value ehci)
  (setf (pci:pci-io-region/32 (op-regs ehci) +ehci-op-command+) value))

(defun status-reg (ehci)
  (pci:pci-io-region/32 (op-regs ehci) +ehci-op-status+))

(defun (setf status-reg) (value ehci)
  (setf (pci:pci-io-region/32 (op-regs ehci) +ehci-op-status+) value))

(defun interrupt-enable-reg (ehci)
  (pci:pci-io-region/32 (op-regs ehci) +ehci-op-int-enable+))

(defun (setf interrupt-enable-reg) (value ehci)
  (setf (pci:pci-io-region/32 (op-regs ehci) +ehci-op-int-enable+) value))

(defun portsc-reg (ehci port-num)
  (pci:pci-io-region/32 (op-regs ehci)
                        (+ +ehci-op-port-status+ (* 4 port-num))))

(defun (setf portsc-reg) (value ehci port-num)
  (setf (pci:pci-io-region/32 (op-regs ehci)
                              (+ +ehci-op-port-status+ (* 4 port-num)))
        value))

;;======================================================================
;; HCD public interface methods
;;======================================================================

(defmethod get-buffer-memory ((ehci ehci-intel) num-bytes)
  (with-slots (%phys-addr-free %phys-addr-end) ehci
    ;; force 32-byte alignment for all buffers
    (setf phy-addr-free (logandc2 (+ %phys-addr-free 31) #x1F))
    (let* ((result %phys-addr-free)
           (next-free (+ %phys-addr-free num-bytes)))
      (when (> next-free %phys-addr-end)
        (error "Out of memory ~D ~D ~D" result num-bytes %phys-addr-end))
      (setf %phys-addr-free next-free)
      result)))

(defmethod create-device ((ehci ehci-intel) port-num)
  (enter-function "create-device")
  (with-hcd-access (ehci)
    (let* ((qh (alloc-qh ehci))
           (device (make-instance 'ehci-device
                                  :hcd ehci
                                  :port-num port-num
                                  :addr 0
                                  :speed :high
                                  :control-event (sup:make-event
                                                  :name "EHCI Control Event"))))
      (setf (aref (usb-device-endpoints device) 0)
            (make-ehci-endpoint :type :control
                                :device device
                                :driver NIL
                                :num 0
                                :event-type (device-control-event device)
                                :qh qh))

      (encode-qh qh T #x40 1 :high 0 0 0)

      ;; Add qh to async list
      (sup:with-mutex ((usbd-lock ehci))
        (setf (qh-next-qh qh +qh-type-QH+) (qh-next-qh (async-qh ehci)))
        (sys.int::dma-write-barrier)
        (setf (qh-next-qh (async-qh ehci) +qh-type-QH+) (array->phys-addr qh)))

      device)))

(defmethod delete-device ((ehci ehci-intel) device)
  (enter-function "delete-device")

  (with-hcd-access (ehci)
    ;; Delete the control QH
    (let* ((qh (device-control-qh device))
           (qh-phys-addr (array->phys-addr qh)))
      (sup:with-mutex ((usbd-lock ehci))
        (loop
           for prev-qh = (async-qh ehci) then
             (ehci-addr->array ehci next-phys-addr)
           for next-phys-addr = (logandc2 (qh-next-qh prev-qh) #x1F)
           when (eql next-phys-addr qh-phys-addr) do
             (setf (qh-next-qh prev-qh +qh-type-QH+) (qh-next-qh qh))
             (return)))
      (wait-for-async-doorbell ehci)
      (free-qh ehci qh))

    ;; Delete remaining QHs
    (loop
       with endpoints = (usb-device-endpoints device)
       for endpt-num from 1 to 31
       for endpoint = (aref endpoints endpt-num)
       when endpoint do
         (ecase (ehci-endpoint-type endpoint)
           (:bulk
            (delete-bulk-endpt ehci device endpt-num))))

    ;; Free any pending QTDs
    (let ((qtds NIL))
      (sup:with-mutex ((usbd-lock ehci))
        (setf (pending-qtds ehci)
              (remove-if #'(lambda (qtd)
                             (if (eq (ehci-endpoint-device
                                      (xfer-info-endpoint
                                       (gethash qtd (qtd->xfer-info ehci))))
                                     device)
                                 (progn (push qtd qtds) T)
                                 NIL))
                         (pending-qtds ehci))))
      (dolist (qtd qtds)
        (free-qtd ehci qtd)))))

(defmethod debounce-port ((ehci ehci-intel) port-num)
  ;; TODO - define this routine
  )

;; When the EHCI Driver receives the request to reset and enable the
;; port, it first checks the value reported by the LineStatus bits in the
;; PORTSC register. If they indicate the attached device is a full-speed
;; device (e.g. D+ is asserted), then the EHCI Driver sets the PortReset
;; control bit to a one (and sets the PortEnable bit to a zero) which
;; begins the reset-process. Software times the duration of the reset,
;; then terminates reset signaling by writing a zero to the port reset
;; bit. The reset process is actually complete when software reads a zero
;; in the PortReset bit. The EHCI Driver checks the PortEnable bit in the
;; PORTSC register. If set to a one, the connected device is a high-speed
;; device and EHCI Driver
;; (root hub emulator) issues a change report to the hub driver and the
;; hub driver continues to enumerate the attached device.

;; • At the time
;; the EHCI Driver receives the port reset and enable request the
;; LineStatus bits might indicate a low-speed device. Additionally, when
;; the port reset process is complete, the PortEnable field may indicate
;; that a full-speed device is attached. In either case the EHCI driver
;; sets the PortOwner bit in the PORTSC register to a one to release port
;; ownership to a companion host controller.

(defmethod reset-port ((ehci ehci-intel) port-num)
  (let ((status (portsc-reg ehci port-num)))
    (when (= (ldb (byte 2 10) status) #b01)
      ;; low speed device
      (setf (portsc-reg ehci port-num) (logior +portsc-mask-release-port+
                                               +portsc-mask-power-on+))
      (sup:debug-print-line "low-speed device not supported by EHCI")
      (error "low-speed device - not supported by EHCI"))

    #+nil
    (if (logbitp 11 status)
        ;; full-speed device
        ???
        ???)
    (setf (portsc-reg ehci port-num) (logior +portsc-mask-disconnect-enable+
                                             +portsc-mask-connect-enable+
                                             +portsc-mask-power-on+
                                             +portsc-mask-reset-port+
                                             +portsc-mask-clear-status-change+))

    (sleep 0.010)

    (loop
       for value = (portsc-reg ehci port-num) then (portsc-reg ehci port-num)
       for time = 0.010 then (+ time 0.010)
       when (not (logtest +portsc-mask-reset-port+ value)) do
         ;; port reset complete
         (cond ((logtest +portsc-mask-enabled+ value)
                (return T))
               (T
                ;; full speed device - release device
                (setf (portsc-reg ehci port-num)
                      (logior +portsc-mask-release-port+
                              +portsc-mask-power-on+))
                (sup:debug-print-line "full-speed device not supported by EHCI")
                (error "full-speed device - not supported by EHCI")))
       when (>= time 1.0) do
         (error "Reset timed out on port ~D" port-num)
       do (sleep 0.010))))

(defmethod set-device-address ((ehci ehci-intel) device address)
  (enter-function "set-device-address")
  (with-buffers ((buf-pool ehci) (buf /8 1))     ;; don't want 0 length buf
    (control-receive-data ehci
                          device
                          (encode-request-type +rt-dir-host-to-device+
                                               +rt-type-standard+
                                               +rt-rec-device+)
                          +dev-req-set-address+
                          address
                          0
                          0
                          buf))


  ;; Update ed and hcd-device to use the new address
  (let* ((qh (device-control-qh device)))
    (with-hcd-access (ehci)
      (setf (qh-header qh) (dpb address +qh-header-address-field+ (qh-header qh))
            (device-addr device) address)))

  ;; According to microsoft doc, need to wait 10ms after
  ;; set-address before get-descriptor
  (sleep 0.03))

;;======================================================================
;; Interrupt Endpoint code
;;======================================================================

(defmethod create-interrupt-endpt
    ((ehci ehci-intel)
     device driver endpt-num num-bufs buf-size event-type interval)
  (enter-function "create-interrupt-endpt")
  )

(defmethod delete-interrupt-endpt ((ehci ehci-intel) device endpt-num)
  (enter-function "delete-interrupt-endpt")
  )

(defun handle-interrupt-endpt (ehci xfer-info qtd)
  (enter-function "handle-interrupt-endpt")
  )

;;======================================================================
;; Bulk Endpoint code
;;======================================================================

(defmethod create-bulk-endpt
    ((ehci ehci-intel) device driver endpt-num in-p event-type)
  (enter-function "create-bulk-endpt")
  (with-hcd-access (ehci)
    (when (aref (usb-device-endpoints device) endpt-num)
      (error "Endpoint ~D already defined. Type is ~S"
             endpt-num
             (ehci-endpoint-type
              (aref (usb-device-endpoints device) endpt-num))))
    (let* ((qh (alloc-qh ehci))
           (endpoint (make-ehci-endpoint :type :bulk
                                         :device device
                                         :driver driver
                                         :num endpt-num
                                         :event-type event-type
                                         :qh qh
                                         :pid (if in-p +pid-in-token+
                                                  +pid-out-token+))))
      (encode-qh qh
                 NIL
                 16 #+nil(usb-device-max-packet device)
                 1
                 :high
                 endpt-num
                 (device-addr device)
                 0)
      (setf (aref (usb-device-endpoints device) endpt-num) endpoint)

      ;; Add qh to async list
      (sup:with-mutex ((usbd-lock ehci))
        (setf (qh-next-qh qh +qh-type-QH+) (qh-next-qh (async-qh ehci)))
        (sys.int::dma-write-barrier)
        (setf (qh-next-qh (async-qh ehci) +qh-type-QH+)
              (array->phys-addr qh))))))

(defmethod delete-bulk-endpt ((ehci ehci-intel) device endpt-num)
  (enter-function "delete-bulk-endpt")
  (with-hcd-access (ehci)
    (let ((endpoint (aref (usb-device-endpoints device) endpt-num)))
      (when (null endpoint)
        (error "Endpoint ~D does not exist" endpt-num))

      (when (not (eq (ehci-endpoint-type endpoint) :bulk))
        (error "Endpoint ~D is type ~S not :bulk"
               endpt-num
               (ehci-endpoint-type endpoint)))

      ;; remove bulk qh from async list
      (let* ((qh (ehci-endpoint-qh endpoint))
             (qh-phys-addr (array->phys-addr qh)))
        (sup:with-mutex ((usbd-lock ehci))
          (loop
             for prev-qh-phys-addr = (qh-next-qh (async-qh ehci)) then
               (qh-next-qh prev-qh)
             for prev-qh = (ehci-addr->array ehci prev-qh-phys-addr) then
               (ehci-addr->array ehci prev-qh-phys-addr)
             when (eql (qh-next-qh prev-qh) qh-phys-addr) do
               (setf (qh-next-qh prev-qh +qh-type-QH+) (qh-next-qh qh))
               (return)))

        (wait-for-async-doorbell ehci)
        (free-qh ehci qh)))))

(defmethod bulk-enqueue-buf ((ehci ehci-intel) device endpt-num buf num-bytes)
  (enter-function "bulk-enqueue-buf")
  (when (> num-bytes (length buf))
    (error "Invalid arguments num-bytes (~D) > length of buffer (~D)"
           num-bytes (length buf)))

  (let* ((endpoint (aref (usb-device-endpoints device) endpt-num))
         (qh (ehci-endpoint-qh endpoint))
         (msg-qtd (alloc-qtd ehci
                             :event-type (ehci-endpoint-event-type endpoint)
                             :endpoint endpoint
                             :buf-size num-bytes
                             :buf buf)))
    (encode-qtd msg-qtd
                  1         ;; data toggle
                  num-bytes ;; num-bytes
                  1         ;; ioc
                  0         ;; c_page
                  3         ;; cerr
                  (ehci-endpoint-pid endpoint)
                  buf)

    (sup:with-mutex ((usbd-lock ehci))
      (push msg-qtd (pending-qtds ehci)))

    (sys.int::dma-write-barrier)
    (setf (qh-next-qtd qh) (array->phys-addr msg-qtd))
    (values)))

(defmethod bulk-dequeue-buf ((ehci ehci-intel) device endpt-num buf)
  (enter-function "bulk-dequeue-buf")
  (let* ((buf-phys-addr (array->phys-addr buf))
         (endpoint (aref (usb-device-endpoints device) endpt-num))
         (qh (ehci-endpoint-qh endpoint)))
    (when (not (logbitp 0 (qh-next-qtd qh)))
      ;; TODO stop this queue - see spec on how to remove qtd
      (loop
         for prev-qtd = NIL then qtd
         for qtd = (ehci-addr->array ehci (qh-next-qtd qh)) then
           (ehci-addr->array ehci (aref qtd 0))
         when (= (aref qtd 3) buf-phys-addr) do
         ;; remove qtd for list
           (if prev-qtd
               (setf (aref prev-qtd  0) (aref qtd 0))
               (setf (qh-next-qtd qh) (aref qtd 0)))
           (sup:with-mutex ((usbd-lock ehci))
             (setf (pending-qtds ehci) (delete qtd (pending-qtds ehci))))
           (free-qtd ehci qtd)
           (return T)
         when (not (logbit 0(aref qtd 0))) do
         ;; qtd not found
           (return nil)))))

;; TODO move this routine to usd-defs.lisp, delete here and in ohci.lisp
(defun transfer-complete (driver event-type endpt-num device status length buf)
  ;; Signal driver a transfer is complete - based oon the event type
  (cond ((typep event-type 'keyword)
         ;; enqueue an event with this type
         (let ((event (make-usb-event
                       :type event-type
                       :dest driver
                       :device device)))
           (setf (usb-event-plist-value event :endpoint-num) endpt-num
                 (usb-event-plist-value event :status) status
                 (usb-event-plist-value event :length) length
                 (usb-event-plist-value event :buf) buf)
           (enqueue-event event)))
        ((typep event-type 'sup:event)
         ;; this means some thread is waiting on this interrupt
         ;; which may not be a good idea
         (setf (sup:event-state event-type) t))
        ((typep event-type 'sync:semaphore)
         ;; this means some thread is waiting on this interrupt
         ;; which may not be a good idea
         (sync:semaphore-up event-type))
        (T
         (funcall event-type driver endpt-num status length buf))))

(defun handle-bulk-endpt (ehci xfer-info qtd)
  (enter-function "handle-bulk-endpt")
  (with-hcd-access (ehci)
    (unwind-protect
         (let ((status (logand (qtd-token qtd) +qtd-status-mask+)))

           ;; TODO check condition code - handle errors
           (let* ((endpoint (xfer-info-endpoint xfer-info)))
             (transfer-complete (ehci-endpoint-driver endpoint)
                                (xfer-info-event-type xfer-info)
                                (ehci-endpoint-num endpoint)
                                (ehci-endpoint-device endpoint)
                                status
                                (- (xfer-info-buf-size xfer-info)
                                   (ldb (byte 15 16) (qtd-token qtd)))
                                (xfer-info-buf xfer-info))))
      (free-qtd ehci qtd))))

;;======================================================================
;; set-device-address
;;======================================================================

;;======================================================================
;;
;; control-receive-data
;;
;; Primary function for receiving data on the control endpoint.
;;
;; TODO is this comment still valid:
;;
;; It is also used for USB commands that don't actually receive any
;; data, such as, set address and set configuration. In these cases,
;; the length should be 0 and the buf can be any length buffer,
;; although the convention is to use a buffer of length 1.
;;
;;======================================================================

(defun control-timed-wait (event timeout)
  (sup:with-timer (timer :relative timeout :name "Control timed wait")
    (sync:wait-for-objects timer event)
    (when (and (not (sup:event-state event))
               (sup:timer-expired-p timer))
      (error "control endpoint operation timer expired"))))

(defmethod control-receive-data
    ((ehci ehci-intel) device request-type request value index length buf)
  (enter-function "control-receive-data")
  (with-hcd-access (ehci)
    (let* ((endpoint (aref (usb-device-endpoints device) 0))
           (event-type (ehci-endpoint-event-type endpoint))
           (qh (ehci-endpoint-qh endpoint))
           (msg-buf (alloc-buffer/8 (buf-pool ehci) 8))
           (msg-qtd (alloc-qtd ehci
                               :event-type event-type
                               :endpoint endpoint
                               :buf-size 8
                               :buf msg-buf))
           (msg-xfer-info (gethash msg-qtd (qtd->xfer-info ehci))))

      (encode-qtd msg-qtd
                  0    ;; data toggle
                  8    ;; num-bytes
                  1    ;; ioc
                  0    ;; c_page
                  3    ;; cerr
                  +pid-setup-token+
                  msg-buf)

      (encode-request msg-buf
                      request-type
                      request
                      value
                      index
                      length)

      (sup:with-mutex ((usbd-lock ehci))
        (push msg-qtd (pending-qtds ehci)))
      (setf (sup:event-state event-type) nil)
      (sys.int::dma-write-barrier)
      (setf (qh-next-qtd qh) (array->phys-addr msg-qtd))

      (control-timed-wait event-type 1.0)

      (setf (xfer-info-buf msg-xfer-info) NIL)
      (free-buffer msg-buf)

      (when (/= (logand (qtd-token msg-qtd) +qtd-status-mask+) 0)
        ;; non-zero means error
        (when *error-qtd*
          (free-qtd ehci *error-qtd*)
          (setf *error-qtd* msg-qtd)
          (let ((status (logand (qtd-token msg-qtd) #xFF)))
            (error
             "control-receive-data: Send command failed with error ~A ~
              (#b~8,'0B)."
             (getf +qtd-error-codes+ status)
             status))))

      ;; re-use msg-qtd
      (encode-qtd msg-qtd
                  1      ;; data toggle
                  length ;; num-bytes
                  1      ;; ioc
                  0      ;; c_page
                  3      ;; cerr
                  +pid-in-token+
                  buf)

      (sup:with-mutex ((usbd-lock ehci))
        (push msg-qtd (pending-qtds ehci)))
      (setf (sup:event-state event-type) nil)
      (sys.int::dma-write-barrier)
      (setf (qh-next-qtd qh) (array->phys-addr msg-qtd)
            (xfer-info-buf msg-xfer-info) buf)

      (control-timed-wait event-type 1.0)

      (when (/= (logand (qtd-token msg-qtd) +qtd-status-mask+) 0)
        ;; non-zero means error
        (when *error-qtd*
          (free-qtd ehci *error-qtd*)
          (setf *error-qtd* msg-qtd)
          (let ((status (logand (qtd-token msg-qtd) #xFF)))
            (error
             "control-receive-data: Receive buffer failed with error ~A ~
              (#b~8,'0B)."
             (getf +qtd-error-codes+ status)
             status))))

      (with-trace-level (3)
        (sup:debug-print-line "control-receive-data - length: "
                              (- length (ldb (byte 15 16) (qtd-token msg-qtd))))
        (print-buffer sys.int::*cold-stream* buf :indent "    "))

      (prog1
          (- length (ldb (byte 15 16) (qtd-token msg-qtd)))
        (free-qtd ehci msg-qtd)))))

;;======================================================================
;;
;; control-send-data
;;
;; Primary function for sending data buffers on the control endpoint.
;;
;;======================================================================

(defmethod control-send-data
    ((ehci ehci-intel) device request-type request value index length buf)
  (enter-function "control-send-data")
  (with-hcd-access (ehci)
    (let* ((endpoint (aref (usb-device-endpoints device) 0))
           (event-type (ehci-endpoint-event-type endpoint))
           (qh (ehci-endpoint-qh endpoint))
           (msg-buf (alloc-buffer/8 (buf-pool ehci) 8))
           (msg-qtd (alloc-qtd ehci
                               :event-type event-type
                               :endpoint endpoint
                               :buf-size 8
                               :buf msg-buf))
           (msg-xfer-info (gethash msg-qtd (qtd->xfer-info ehci))))

      (encode-qtd msg-qtd
                  0    ;; dt
                  8    ;; num-bytes
                  1    ;; ioc
                  0    ;; c_page
                  3    ;; cerr
                  +pid-setup-token+
                  msg-buf)

      (encode-request msg-buf
                      request-type
                      request
                      value
                      index
                      length)

      (sup:with-mutex ((usbd-lock ehci))
        (push msg-qtd (pending-qtds ehci)))
      (sys.int::dma-write-barrier)
      (setf (qh-next-qtd qh) (array->phys-addr msg-qtd))

      (control-timed-wait event-type 1.0)

      (setf (xfer-info-buf msg-xfer-info) NIL)
      (free-buffer msg-buf)

      (when (/= (logand (qtd-token msg-qtd) +qtd-status-mask+) 0)
        ;; non-zero means error
        (when *error-qtd*
          (free-qtd ehci *error-qtd*)
          (setf *error-qtd* msg-qtd)
          (let ((status (logand (qtd-token msg-qtd) #xFF)))
            (error
             "control-send-data: Send command failed with error ~A ~
              (#b~8,'0B)."
             (getf +qtd-error-codes+ status)
             status))))

      ;; re-use msg-qtd
      (encode-qtd msg-qtd
                  1      ;; data toggle
                  length ;; num-bytes
                  1      ;; ioc
                  0      ;; c_page
                  3      ;; cerr
                  +pid-out-token+
                  buf)

      (sup:with-mutex ((usbd-lock ehci))
        (push msg-qtd (pending-qtds ehci)))
      (setf (sup:event-state event-type) nil)
      (sys.int::dma-write-barrier)
      (setf (qh-next-qtd qh) (array->phys-addr msg-qtd)
            (xfer-info-buf msg-xfer-info) buf)

      (control-timed-wait event-type 1.0)

      (when (/= (logand (qtd-token msg-qtd) +qtd-status-mask+) 0)
        ;; non-zero means error
        (when *error-qtd*
          (free-qtd ehci *error-qtd*)
          (setf *error-qtd* msg-qtd)
          (let ((status (logand (qtd-token msg-qtd) #xFF)))
            (error
             "control-send-data: Receive buffer failed with error ~A ~
              (#b~8,'0B)."
             (getf +qtd-error-codes+ status)
             status))))

      (prog1
          (- length (ldb (byte 15 16) (qtd-token msg-qtd)))
        (free-qtd ehci msg-qtd)))))

;;======================================================================
;; EHCI interrupt event handlers
;;======================================================================

(defmethod handle-interrupt-event (type (ehci ehci-intel) event)
  (declare (ignore ehci event))
  ;; Default method for EHCI events - call for any event which is not
  ;; handled above, or handled by usbd.
  (error "EHCI interrupt event ~A not handled. Error signaled, but ignored."
         type))

(defmethod handle-interrupt-event
    ((type (eql :hub-status-change)) (ehci ehci-intel) event)
  (enter-function "handle-interrupt-event :hub-status-change")
  (with-hcd-access (ehci)
    (unwind-protect
         (dotimes (port-num (num-ports ehci))
           (let ((status (portsc-reg ehci port-num)))
             (when (logbitp +ehci-portsc-status-change+ status)
               (let ((type (if (logbitp +ehci-portsc-connect-status+ status)
                               :port-connect
                               :port-disconnect))
                     (event))
                 (unwind-protect
                      (progn (setf event (alloc-interrupt-event ehci)
                                   (interrupt-event-type event) type
                                   (interrupt-event-port-num event) port-num)
                             (handle-interrupt-event type ehci event))
                   (when (and event
                              (not (eq (interrupt-event-type event) :free)))
                     (free-interrupt-event (interrupt-event-hcd event) event))
                   (setf (portsc-reg ehci port-num)
                         (ash 1 +ehci-portsc-status-change+)))))))
      (setf (interrupt-enable-reg ehci)
            (logior (interrupt-enable-reg ehci)
                    +intr-mask-hub-status-change+)))))

(defmethod handle-interrupt-event
    ((type (eql :transfer-done)) (ehci ehci-intel) event)
  (enter-function "handle-interrupt-event :transfer-done")
  (with-hcd-access (ehci)
    (unwind-protect
         ;; examine outstanding qTDs for ones that are complete
         (let ((done-qtds NIL))
           (sup:with-mutex ((usbd-lock ehci))
             (setf (pending-qtds ehci)
                   (remove-if #'(lambda (qtd)
                                  (if (logbitp 7 (qtd-token qtd))
                                      NIL
                                      (progn (push qtd done-qtds) T)))
                              (pending-qtds ehci))))

           (dolist (qtd done-qtds)
             (let* ((xfer-info (gethash qtd (qtd->xfer-info ehci)))
                    (endpoint (xfer-info-endpoint xfer-info)))
               (case (ehci-endpoint-type endpoint)
                 (:control
                  (setf (sup:event-state (ehci-endpoint-event-type endpoint)) t))
                 (:interrupt
                  (handle-interrupt-endpt ehci xfer-info qtd))
                 (:bulk
                  (handle-bulk-endpt ehci xfer-info qtd))
                 (:isochronous
                  (sup:debug-print-line "ERROR: ~
                     isochronous endpoint not implemented"))))))

      ;; enable :transfer-done interrupt
      (setf (interrupt-enable-reg ehci)
            (logior (interrupt-enable-reg ehci) #x00000001)))))

(defmethod handle-interrupt-event
    ((type (eql :usb-error)) (ehci ehci-intel) event)
  (let ((error-qtds NIL))
    ;; Search for QTDs whose associated QH has an error status
    (sup:with-mutex ((usbd-lock ehci))
      (setf (pending-qtds ehci)
            (remove-if
             #'(lambda (qtd)
                 (let* ((xfer-info (gethash qtd (qtd->xfer-info ehci)))
                        (endpoint (xfer-info-endpoint xfer-info)))
                   (when (logtest #x7C (qh-token (ehci-endpoint-qh endpoint)))
                     (push qtd error-qtds)
                     T)))
             (pending-qtds ehci))))

    ;; "Transfer is complete" for the error QTDs
    (dolist (qtd error-qtds)
      (let* ((xfer-info (gethash qtd (qtd->xfer-info ehci)))
             (endpoint (xfer-info-endpoint xfer-info))
             (qh (ehci-endpoint-qh endpoint)))
        ;; transfer status from QH to QTD
        (setf (qtd-token qtd)
              (dpb (logand #x7C (qh-token qh)) (byte 8 0) (qtd-token qtd)))
        (case (ehci-endpoint-type endpoint)
          (:control
           (setf (sup:event-state (ehci-endpoint-event-type endpoint)) t))
          (:interrupt
           (handle-interrupt-endpt ehci xfer-info qtd))
          (:bulk
           (handle-bulk-endpt ehci xfer-info qtd))
          (:isochronous
           (sup:debug-print-line "ERROR: ~
                     isochronous endpoint not implemented")))))))

(defmethod handle-interrupt-event
    ((type (eql :controller-disconnect)) (ehci ehci-intel) event)
  (enter-function "handle-interrupt-event :controller-disconnect")
  (delete-controller ehci))

;;======================================================================
;; Worker thread which handles PCIe interrupts
;;
;; While the interrupt thread could be part of usb-driver and call a
;; method to enqueue the interrupt events, having the thread direcly
;; create and enqueue the events saves the overhead of a method call
;; in the critical path of interrupt handling.
;;
;;======================================================================

(defun interrupt-thread-main (ehci)
  (let ((pci-irq (pci-irq ehci)))
    (loop
       (block :process-event
         (handler-bind
             ((error
               (lambda (c)
                 (ignore-errors
                   (let ((*standard-output* *error-output*))
                     (format *error-output* "~&Error ~A.~%" c)
                     (sys.int::backtrace)))
                 (return-from :process-event (values nil c))))
              (controller-disconnect
               (lambda (c)
                 (let* ((ehci (disconnect-hcd c))
                        (event (alloc-interrupt-event ehci)))
                   (setf (interrupt-event-type event) :controller-disconnect)
                   (enqueue-event event))
                 ;; should never return - thread should be killed
                 (sleep 60)
                 ;; if we return, exit
                 (return-from interrupt-thread-main))))
           (sync:wait-for-objects
            pci-irq
            (pci:pci-device-boot-id (pci-device ehci)))
           (with-hcd-access (ehci)
             (let ((interrupts (logand (status-reg ehci)
                                       (interrupt-enable-reg ehci))))
               ;; disable pending interrupts, and clear them.
               (setf (interrupt-enable-reg ehci)
                     (logandc2 (interrupt-enable-reg ehci) interrupts)
                     (status-reg ehci) interrupts)

               ;; Re-enable PCI interrupts
               (sup:simple-irq-unmask pci-irq)
               ;; Create and enqueue an interrupt event for each pending
               ;; interrupt. Worker threads then handle the interrupt events.
               (macrolet ((%intr-event (bit type)
                            (let ((event (gensym "event-")))
                              `(when (logbitp ,bit interrupts)
                                 (let ((,event (alloc-interrupt-event ehci)))
                                   (setf (interrupt-event-type ,event) ,type)
                                   (enqueue-event ,event))))))
                 (%intr-event +intr-bit-transfer-done+ :transfer-done)
                 (%intr-event +intr-bit-usb-error+ :usb-error)
                 (%intr-event +intr-bit-hub-status-change+ :hub-status-change)
                 (%intr-event +intr-bit-frame-rollover+ :frame-rollover)
                 (%intr-event +intr-bit-pci-error+ :pci-error)
                 (%intr-event +intr-bit-async-advance+ :async-advance)))))))))

;;======================================================================
;; Periodic Frame List (PFL) (AKA Interrupt Table) Routines
;;======================================================================

(defun init-periodic-frame-list (ehci)
  (setf (pci:pci-io-region/32 (op-regs ehci) +ehci-op-frame-list+)
        (logand (pfl-phys-addr ehci) #xFFFFFFFF))

  ;; TODO - create real periodic frame table

  ;; Setup periodic frame list with no entries enabled (T (bit 0) = 1)
  (let ((pfl-table (pfl-table ehci)))
    (dotimes (i 1024)
      (setf (aref pfl-table i) #x00000001))))

;;======================================================================
;; EHCI Init code
;;======================================================================

(define-condition ehci-bad-reset-state (error)
  ((register    :initarg :register    :reader brs-register)
   (expected    :initarg :expected    :reader brs-expected)
   (actual      :initarg :actual      :reader brs-actual))
  (:report
   (lambda (condition stream)
     (format stream
             "Invalid reset state for ~A register. Expected: ~8,'0X actual: ~8,'0X"
             (brs-register condition)
             (brs-expected condition)
             (brs-actual condition)))))

(defun init-memory (ehci)
  (let ((op-regs (op-regs ehci)))
    ;; Allocate all of the physical memory required by ehci at once
    ;; so that if ehci is 64-bit capable, the 64-bit base address
    ;; works for all of the physical memory.
    (with-slots (%64-bit-cap %phys-addr %phys-addr-free %phys-addr-end) ehci
      (setf %phys-addr (* (or (sup::allocate-physical-pages
                               +ehci-phys-memory-pages+
                               :32-bit-only (not %64-bit-cap))
                              (error "Unable to allocate physical buffer!"))
                          sup::+4k-page-size+)
            %phys-addr-free %phys-addr
            %phys-addr-end (+ %phys-addr (* +ehci-phys-memory-pages+
                                            sup::+4k-page-size+))
            (phys-addr-high ehci) (logandc2 %phys-addr #xFFFFFFFF))
      (when %64-bit-cap
        (setf (pci:pci-io-region/32 op-regs +ehci-op-4g-segment+)
              (ldb (byte 32 32) %phys-addr)))

      ;; Allocate Periodic Frame List must be on 4KB aligned
      (setf %phys-addr-free (logandc2 (+ %phys-addr-free #xFFF) #xFFF)
            (pfl-phys-addr ehci) %phys-addr-free
            (pfl-table ehci) (make-array 1024
                                         :element-type '(unsigned-byte 32)
                                         :physical-memory %phys-addr-free))

      (incf %phys-addr-free (* 1024 4))
      ;; minimum buffer size 32 since we force 32-byte alignment for all buffers
      (setf (buf-pool ehci)
            (create-buffer-pool ehci "EHCI (intel) Driver"
                                '(32 128 1024 4096))))))

(defun  ehci-validate-reset(ehci)
  (let ((op-regs (op-regs ehci)))
    (flet ((check-register (name offset expected)
             (let ((value (pci:pci-io-region/32 op-regs offset)))
               (when (/= value expected)
                 (error 'ehci-bad-reset-state
                        :register name
                        :expected expected
                        :actual value)))))
      (check-register "Command" +ehci-op-command+
                      (if (async-park-cap ehci) #x00080B00 #x00080000))
      (check-register "Status" +ehci-op-status+ #x00001000)
      (check-register "Interrupt Enable" +ehci-op-int-enable+ #x0)
      (check-register "Frame Index" +ehci-op-frame-index+ #x0)
      (check-register "4G Segment"+ehci-op-4g-segment+ #x0)
      (check-register "Config Flag" +ehci-op-config-flag+ #x0)
      (dotimes (i (num-ports ehci))
        (check-register "Port SC" (+ +ehci-op-port-status+ (* 4 i))
                        (if (power-control ehci) #x00002000 #x00003000))))))

(defun ehci-intel-probe (device)
  (sup:debug-print-line "Probing EHCI (intel)")
  (sup:with-device-access ((pci:pci-device-boot-id device)
                           (return-from ehci-intel-probe))
    (let* ((cap-regs (pci:pci-io-region device 0))
           (cap-len (logand (pci:pci-io-region/32 cap-regs +ehci-cap-length+)
                            #x000000FF))
           (op-regs (+ cap-regs cap-len))
           (hcsparams (pci:pci-io-region/32 cap-regs +ehci-cap-hcsparams+))
           (hccparams (pci:pci-io-region/32 cap-regs +ehci-cap-hccparams+))
           (ehci (make-instance 'ehci-intel
                                :pci-device device
                                :pci-irq (sup:make-simple-irq
                                          (pci:pci-intr-line device))
                                :num-ports (ldb (byte 4 0) hcsparams)
                                :lock (sup:make-mutex "EHCI Lock")
                                :cap-regs cap-regs
                                :op-regs op-regs
                                :power-control (logbitp 4 hcsparams)
                                :routing-rules (logbitp 7 hcsparams)
                                :n-pcc (ldb (byte 4 8) hcsparams)
                                :n-cc (ldb (byte 4 12) hcsparams)
                                :p-indicator (logbitp 16 hcsparams)
                                :debug-port (ldb (byte 4 20) hcsparams)
                                :64-bit-cap (logbitp 0 hccparams)
                                :frame-list-flag (logbitp 1 hccparams)
                                :async-park-cap (logbitp 2 hccparams)
                                :isoch-threshold (ldb (byte 4 4) hccparams)
                                :extended-cap (ldb (byte 4 8) hccparams)
                                ;; ### Does this need to be synchronized?
                                :qtd->xfer-info (make-hash-table :synchronized t))))
      (setf (pci:pci-bus-master-enabled device) T)
      (setf *ehci* ehci)

      ;; halt EHCI (should already be halted, but check anyway)
      (when (not (logbitp +ehci-status-halted-bit+ (status-reg ehci)))
        ;; EHCI is running - stop it
        (setf (command-reg ehci) 0)
        (loop
           for time = 0 then (+ time 0.010)
           when (logbitp +ehci-status-halted-bit+ (status-reg ehci)) do
             (return)
           when (> time 1.0) do
             (sup:debug-print-line "EHCI halt timeout " time)
             (return-from ehci-intel-probe NIL)
           do
             (sleep 0.010)))

      ;; reset EHCI
      (setf (command-reg ehci) #x00000002)
      (loop
         for time = 0 then (+ time 0.010)
         when (not (logbitp 1 (command-reg ehci))) do
           (return)
         when (> time 1.0) do
           (sup:debug-print-line "EHCI reset timeout " time)
           (return-from ehci-intel-probe NIL)
         do
           (sleep 0.010))

      (ehci-validate-reset ehci)

      (init-memory ehci)
      (init-periodic-frame-list ehci)

      (setf (interrupt-thread ehci) (sup:make-thread
                                     (lambda () (interrupt-thread-main ehci))
                                     :name "EHCI (intel) interrupt-thread"))
      (sup:simple-irq-unmask (pci-irq ehci))

      ;; Route all ports to this ehci
      (setf (pci:pci-io-region/32 op-regs +ehci-op-config-flag+) #x00000001)

      ;; Configure Port Control
      (dotimes (port-num (num-ports ehci))
        (setf (portsc-reg ehci port-num)
              (logior +portsc-mask-disconnect-enable+
                      +portsc-mask-connect-enable+
                      +portsc-mask-power-on+)))

      ;; create dummy qh as the head of the async list
      (let* ((qh (alloc-qh ehci))
             (qh-phys-addr (array->phys-addr qh)))
        (encode-qh qh T #x40 1 :high 0 0 0)
        (setf
         (async-qh ehci) qh
         ;; link pointer to self (only qh in list)
         (qh-next-qh qh +qh-type-QH+) qh-phys-addr
         ;; set head bit
         (qh-header qh) (logior (qh-header qh) (ash 1 15))
         ;;
         (pci:pci-io-region/32 op-regs +ehci-op-async-list+) qh-phys-addr))

      ;; Enable ehci
      (sys.int::dma-write-barrier)

      (setf (command-reg ehci)
            (logior (dpb 8 (byte 8 16) 0)   ;; Interrupt interval 1ms
                    (dpb 1 (byte 1 11) 0)   ;; Async Schedule Park Mode Enable
                    (dpb 3 (byte 2 8)  0)   ;; Async Shecdule Park Mode Count
                    (ash 1 5)               ;; Async Schedule Enable
                    (dpb 1 (byte 1 4)  0)   ;; Periodic Schedule Enable
                    (dpb 0 (byte 2 2)  0)   ;; Frame list size 1024 elements
                    #x00000001))            ;; Run/Stop - run

      (sup:debug-print-line "EHCI Running")

      ;; Enable interrupts
      (setf (interrupt-enable-reg ehci)
            (logior +intr-mask-hub-status-change+
                    +intr-mask-usb-error+
                    +intr-mask-transfer-done+)))))

(pci:define-pci-driver EHCI-intel ehci-intel-probe () ((#x0c #x03 #x20)))
