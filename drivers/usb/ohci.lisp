;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

;;======================================================================
;;
;; Host Controller Driver (HCD) for OpenHCI based on the Open Host
;; Controller Interface Specification for USB.
;;
;;======================================================================

(in-package :mezzano.driver.usb.ohci)

(defvar *ohci*)                     ;; for debug
(defvar *error-td* nil)             ;; for debug
(defvar *trace* 0)                  ;; for debug higher number => more tracing

;;======================================================================
;; support routines
;;======================================================================

(defmacro validate-address/16 (buf phys)
  `(when (/= (logand ,phys #x0F) 0)
     (error ,(concatenate 'string "Invalid "
                          (symbol-name buf)
                          " physical address: ~8,'0X")
            ,phys)))

(defmacro array-total-bytes (buf)
  `(let ((elements (array-total-size ,buf))
         (type (array-element-type ,buf)))
     (when (or (not (listp type))
               (not (member (car type) '(unsigned-byte signed-byte))))
       (error ,(concatenate 'string "Unable to determine bytes in "
                            (symbol-name buf)
                            ": ~A") (type-of ,buf)))
     (* elements (/ (cadr type) 8))))

(defun compute-fsmps(fminterval)
  (logand #x7fff (round (/ (* 6 (- fminterval 210)) 7))))

(defmacro with-trace-level ((trace-level) &body body)
  `(when (>= *trace* ,trace-level)
     ,@body))

;;======================================================================
;; Operational Registers - defined in Chapter 7
;;======================================================================

(defconstant +ohci-revision+                #x00)
(defconstant +ohci-control+                 #x04)
(defconstant +ohci-command-status+          #x08)
(defconstant +ohci-interrupt-status+        #x0c)
(defconstant +ohci-interrupt-enable+        #x10)
(defconstant +ohci-interrupt-disable+       #x14)
(defconstant +ohci-hcca+                    #x18)
(defconstant +ohci-current-period-endpt+    #x1C)
(defconstant +ohci-control-head-pointer+    #x20)
(defconstant +ohci-current-control-pointer+ #x24)
(defconstant +ohci-bulk-head-pointer+       #x28)
(defconstant +ohci-current-bulk-pointer+    #x2C)
(defconstant +ohci-done-head-pointer+       #x30)
(defconstant +ohci-frame-interval+          #x34)
(defconstant +ohci-frame-remaining+         #x38)
(defconstant +ohci-frame-number+            #x3C)
(defconstant +ohci-periodic-start+          #x40)
(defconstant +ohci-low-speed-threshold+     #x44)
(defconstant +ohci-root-hub-descriptor-a+   #x48)
(defconstant +ohci-root-hub-descriptor-b+   #x4C)
(defconstant +ohci-root-hub-status+         #x50)
(defconstant +ohci-root-hub-port-status+    #x54)

;; Field definitions in the control register

(defconstant +control-bulk-service-ratio+      (byte 0  2))
(defconstant +control-periodic-list-enable+    (byte 1  2))
(defconstant +control-isochronous-enable+      (byte 1  3))
(defconstant +control-control-list-enable+     (byte 1  4))
(defconstant +control-bulk-list-enable+        (byte 1  5))
(defconstant +control-functional-state+        (byte 2  6))
(defconstant +control-interrupt-routing+       (byte 1  8))
(defconstant +control-remote-wakeup-connected+ (byte 1  9))
(defconstant +control-remote-wakeup-enable+    (byte 1 10))

(defconstant +functional-state-reset+       #b00)
(defconstant +functional-state-resume+      #b01)
(defconstant +functional-state-operational+ #b10)
(defconstant +functional-state-suspend+     #b11)

;; Field definitions in the command status register

(defconstant +command-controller-reset+    (byte 1 0))
(defconstant +command-control-list-filled+ (byte 1 1))
(defconstant +command-bulk-list-filled+    (byte 1 2))
(defconstant +command-ownership-change+    (byte 1 3))
(defconstant +command-schedule-overrun+    (byte 2 16))

;; Field definitions in the Interrupt Status Register, Interrupt
;; Enable Register and Interrupt Disable Register

(defconstant +interrupt-scheduling-overrun+    (byte 1  0))
(defconstant +interrupt-done-head+             (byte 1  1))
(defconstant +interrupt-start-of-frame+        (byte 1  2))
(defconstant +interrupt-resume-detect+         (byte 1  3))
(defconstant +interrupt-unrecoverable-error+   (byte 1  4))
(defconstant +interrupt-frame-number-overflow+ (byte 1  5))
(defconstant +interrupt-root-hub-change+       (byte 1  6))
(defconstant +interrupt-ownership-change+      (byte 1 30))
(defconstant +interrupt-master-enable+         (byte 1 31))

;; Field definitions in the Frame Interval Register

(defconstant +frame-interval-interval+        (byte 14  0))
(defconstant +frame-interval-max-data-packet+ (byte 15 16))
(defconstant +frame-interval-toggle+          (byte  1 31))

;; Bit definitions of the Port Status Register
(defconstant +ps-connect-status+           (byte 1 0))
(defconstant +ps-reset-status+             (byte 1 4))
(defconstant +ps-low-speed-device+         (byte 1 9))
(defconstant +ps-connect-status-change+    (byte 1 16))
(defconstant +ps-reset-status-change+      (byte 1 20))

;;======================================================================
;; Endpoint Descriptor - defined in section 4.2
;;======================================================================

;; Endpoint Descriptor word indexes

(defconstant +endpt-header+   0)
(defconstant +endpt-tdq-tail+ 1)
(defconstant +endpt-tdq-head+ 2)
(defconstant +endpt-next-ed+  3)

;; Endpoint Descriptor header fields and field values

(defconstant +endpt-function-addr-field+ (byte  7  0))
(defconstant +endpt-number+              (byte  4  7))
(defconstant +endpt-direction+           (byte  2 11))
(defconstant +endpt-speed+               (byte  1 13))
(defconstant +endpt-skip+                (byte  1 14))
(defconstant +endpt-format+              (byte  1 15))
(defconstant +endpt-max-packet-size+     (byte 11 16))

(defconstant +endpt-direction-td+  #b00)
(defconstant +endpt-direction-out+ #b01)
(defconstant +endpt-direction-in+  #b10)
(defconstant +endpt-direction-td3+ #b11)

(defconstant +endpt-full-speed+ #b0)
(defconstant +endpt-low-speed+  #b1)

(defconstant +endpt-active+   #b0)
(defconstant +endpt-inactive+ #b1)

(defconstant +endpt-general-tds+     #b0)
(defconstant +endpt-isochronous-tds+ #b1)

;; Endpoint Descriptor TD Queue Head Pointer fields and field values

(defconstant +endpt-halted+          (byte 1 0))
(defconstant +endpt-toggle-carry+    (byte 1 1))

(defun ed-header (ed)
  (aref ed +endpt-header+))

(defun (setf ed-header) (value ed)
  (setf (aref ed +endpt-header+) value))

(defun ed-tdq-tail (ed)
  (aref ed +endpt-tdq-tail+))

(defun (setf ed-tdq-tail) (phys-addr ed)
  (validate-address/16 tdq-tail phys-addr)
  (setf (aref ed +endpt-tdq-tail+) phys-addr))

(defun ed-tdq-head (ed)
  (aref ed +endpt-tdq-head+))

(defun ed-tdq-head-tdq-addr (ed)
  (logandc2 (aref ed +endpt-tdq-head+) #x0F))

(defun (setf ed-tdq-head) (phys-addr ed)
  ;; clears the toggle carry and halted bits.
  (validate-address/16 tdq-head phys-addr)
  (setf (aref ed +endpt-tdq-head+) phys-addr))

(defun ed-next-ed (ed)
  (aref ed +endpt-next-ed+))

(defun (setf ed-next-ed) (phys-addr ed)
  (validate-address/16 next-ed phys-addr)
  (setf (aref ed +endpt-next-ed+) phys-addr))

(defun alloc-disabled-ed (ohci)
  (let ((ed (alloc-buffer/32 (buf-pool ohci) 4)))
    ;; set the skip bit
    (setf (aref ed 0) (dpb 1 +endpt-skip+ 0))
    ed))

(defun alloc-ed (ohci)
  (alloc-buffer/32 (buf-pool ohci) 4))

(defun free-ed (ohci ed)
  (loop
     with td->xfer-info = (td->xfer-info ohci)
     with td-tail-phys-addr = (ed-tdq-tail ed)
     for td-phys-addr = (ed-tdq-head-tdq-addr ed) then next-td-phys-addr
     for td = (phys-addr->array td-phys-addr)
     for xfer-info = (gethash td td->xfer-info)
     for next-td-phys-addr = (td-next-td td)
     do
       (when (xfer-info-buf xfer-info)
         (free-buffer (xfer-info-buf xfer-info)))
       (free-td ohci td)
     until (= td-phys-addr td-tail-phys-addr))
  (free-buffer ed))

(defun encode-ed-header (port-num endpt-num dir speed skip format size)
  (logior (dpb port-num +endpt-function-addr-field+ 0)
          (dpb endpt-num +endpt-number+ 0)        ;; endpoint number
          (dpb dir +endpt-direction+ 0)     ;; default to td
          (dpb speed +endpt-speed+ 0)         ;; full speed
          (dpb skip +endpt-skip+ 0)          ;; Don't skip (ie valid ed)
          (dpb format +endpt-format+ 0)        ;; Not isochronous ed
          (dpb size +endpt-max-packet-size+ 0))) ;; max packet size

;;======================================================================
;; Transfer Descriptor - defined in section 4.3
;;======================================================================

;; Transfer Descriptor word indexes

(defconstant +td-header+         0)
(defconstant +td-buffer-pointer+ 1)
(defconstant +td-next-td+        2)
(defconstant +td-buffer-end+     3)

(defconstant +td-buffer-rounding+ (byte 1 18))
(defconstant +td-direction-pid+   (byte 2 19))
(defconstant +td-delay-interrupt+ (byte 3 21))
(defconstant +td-data-toggle+     (byte 2 24))
(defconstant +td-error-count+     (byte 2 26))
(defconstant +td-condition-code+  (byte 4 28))

(defconstant +td-full-buffer+    #b0)
(defconstant +td-partial-buffer+ #b1)

(defconstant +pid-setup-token+  #b00)
(defconstant +pid-out-token+    #b01)
(defconstant +pid-in-token+     #b10)

(defconstant +td-ed-toggle+  #b00)
(defconstant +td-toggle-0+   #b10)
(defconstant +td-toggle-1+   #b11)

(defconstant +condition-codes+
  #(:success
    :crc-error
    :bitstuffing-error
    :data-toggle-mismatch
    :stall
    :device-not-responding
    :pid-check-failure
    :unexpected-pid
    :data-overrun
    :data-underrun
    :10-reserved
    :11-reserved
    :buffer-overrun
    :buffer-underrun
    :14-buffer-not-accessed
    :15-buffer-not-accessed))

(defun td-header (td)
  (aref td +td-header+))

(defun (setf td-header) (value td)
  (setf (aref td +td-header+) value))

(defun td-buffer-pointer (td)
  (aref td +td-buffer-pointer+))

(defun (setf td-buffer-pointer) (phys-addr td)
  (setf (aref td +td-buffer-pointer+) phys-addr))

(defun td-next-td (td)
  (aref td +td-next-td+))

(defun (setf td-next-td) (phys-addr td)
  (validate-address/16 next-td phys-addr)
  (setf (aref td +td-next-td+) phys-addr))

(defun td-buffer-end (td)
  (aref td +td-buffer-end+))

(defun (setf td-buffer-end) (phys-addr td)
  (setf (aref td +td-buffer-end+) phys-addr))

(defun alloc-td (ohci &key event-type endpoint buf-size buf)
  (let ((td (alloc-buffer/32 (buf-pool ohci) 4)))
    (setf (gethash td (td->xfer-info ohci)) (make-xfer-info :event-type event-type
                                                        :endpoint endpoint
                                                        :buf-size buf-size
                                                        :buf buf))
    td))

(defun free-td (ohci td)
  (remhash td (td->xfer-info ohci))
  (free-buffer td))

(defun encode-td-header (partial-buf pid int-delay toggle)
  (logior (dpb partial-buf +td-buffer-rounding+ 0)  ;; parital buffer is OK
          (dpb pid +td-direction-pid+ 0)
          (dpb int-delay +td-delay-interrupt+ 0)
          (dpb toggle +td-data-toggle+ 0)
          (dpb 0 +td-error-count+ 0)      ;; no errors
          (dpb #xF +td-condition-code+ 0)))  ;; status - not accessed

(defun encode-td (td partial-buf pid int-delay toggle buf next-td)
  (let ((buf-phys-addr (array->phys-addr buf)))
    (setf (td-header td) (encode-td-header partial-buf pid int-delay toggle)
          (td-buffer-pointer td) buf-phys-addr
          (td-next-td td) (array->phys-addr next-td)
          (td-buffer-end td) (+ buf-phys-addr (array-total-bytes buf) -1))))

(defun td-buf-info (td buf-size)
  ;; TODO this code won't work for non-contiguous buffers
  (let ((buf-pointer (td-buffer-pointer td))
        (buf-start (- (1+ (td-buffer-end td)) buf-size)))
    (values
     (phys-addr->array buf-start)
     (if (= buf-pointer 0) buf-size (- buf-pointer buf-start)))))

(defun td-xfer-bytes (td buf-size)
  ;; TODO this code won't work for non-contiguous buffers
  (let ((buf-pointer (td-buffer-pointer td))
        (buf-start (- (1+ (td-buffer-end td)) buf-size)))
    (if (= buf-pointer 0) buf-size (- buf-pointer buf-start))))

;;======================================================================
;; HCCA (Host Controller Communication Area) - defined in section 4.4
;;======================================================================

;; HCCA byte offsets

(defconstant +hcca-interrupt-table+     0)
(defconstant +hcca-frame-number+     #x80)
(defconstant +hcca-done-head+        #x84)

(defun hcca-frame-number (ohci)
  (sup::physical-memref-unsigned-byte-16 (+ (hcca-phys-addr ohci)
                                            +hcca-frame-number+)))

(defun hcca-done-head (ohci)
  (sup::physical-memref-unsigned-byte-32 (+ (hcca-phys-addr ohci)
                                            +hcca-done-head+)))

;;======================================================================
;;
;;======================================================================

(defun wait-for-next-sof (ohci)
  ;; TODO wait for 1 ms when sleep has better resolution
  ;; wait for sof - frame time is 1ms
  ;; clear sof
  (setf (get-interrupt-status ohci) (dpb 1 +interrupt-start-of-frame+ 0))

  (loop for timeout = 0 then (+ timeout 0.010)
     do
       (sleep  0.010)
       (cond ((ldb-test +interrupt-start-of-frame+ (get-interrupt-status ohci))
              ;; see sof interrupt
              (return))
             ((> timeout 1.0)
              ;; timed out
              (error "wait-for-next-sof did not see SOF interrupt")))))

(defun clear-current-pointer (ohci current-pointer-offset enable-mask)
  (let ((bar0 (bar ohci)))
    ;; disable list
    (setf (pci:pci-io-region/32 bar0 +ohci-control+)
          (logandc2 (pci:pci-io-region/32 bar0 +ohci-control+) enable-mask))

    (wait-for-next-sof ohci)

    ;; clear current point
    (setf (pci:pci-io-region/32 bar0 current-pointer-offset) 0)

    ;; enable list
    (setf (pci:pci-io-region/32 bar0 +ohci-control+)
          (logior (pci:pci-io-region/32 bar0 +ohci-control+) enable-mask))))

;;======================================================================
;;
;;======================================================================

(defconstant +ohci-phys-memory-pages+ 8)

(defclass ohci (usbd)
  (
   ;; base address register - address of controller regs
   (%bar              :initarg :bar               :accessor bar)

   ;; physical address of memory used for buffers
   (phys-addr         :initarg :phys-addr         :accessor phys-addr)
   (phys-addr-free    :initarg :phys-addr-free    :accessor phys-addr-free)
   (phys-addr-end     :initarg :phys-addr-end     :accessor phys-addr-end)

   ;;
   (hcca-phys-addr    :initarg :hcca-phys-addr    :accessor hcca-phys-addr)
   (interrupt-table   :initarg :interrupt-table   :accessor interrupt-table)
   (32ms-interrupts   :initarg :32ms-interrupts   :accessor 32ms-interrupts)
   (16ms-interrupts   :initarg :16ms-interrupts   :accessor 16ms-interrupts)
   (8ms-interrupts    :initarg :8ms-interrupts    :accessor 8ms-interrupts)
   (4ms-interrupts    :initarg :4ms-interrupts    :accessor 4ms-interrupts)
   (2ms-interrupts    :initarg :2ms-interrupts    :accessor 2ms-interrupts)
   (1ms-interrupts    :initarg :1ms-interrupts    :accessor 1ms-interrupts)

   (td->xfer-info       :initarg :td->xfer-info       :accessor td->xfer-info)
   ))

(defmethod delete-controller ((ohci ohci))
  (sup:debug-print-line "Deleting OHCI controller")
  (sup:terminate-thread (interrupt-thread ohci)))

;;======================================================================
;; Register access routines
;;======================================================================

(defun get-interrupt-status (ohci)
  (pci:pci-io-region/32 (bar ohci) +ohci-interrupt-status+))

(defun (setf get-interrupt-status) (value ohci)
  (setf (pci:pci-io-region/32 (bar ohci) +ohci-interrupt-status+) value))

(defun get-interrupt-enable (ohci)
  (pci:pci-io-region/32 (bar ohci) +ohci-interrupt-enable+))

(defun (setf get-interrupt-enable) (value ohci)
  (setf (pci:pci-io-region/32 (bar ohci) +ohci-interrupt-enable+) value))

(defun (setf get-interrupt-disable) (value ohci)
  (setf (pci:pci-io-region/32 (bar ohci) +ohci-interrupt-disable+) value))

(defun get-control-head-pointer (ohci)
  (pci:pci-io-region/32 (bar ohci) +ohci-control-head-pointer+))

(defun (setf get-control-head-pointer) (value ohci)
  (setf (pci:pci-io-region/32 (bar ohci) +ohci-control-head-pointer+) value))

(defun get-bulk-head-pointer (ohci)
  (pci:pci-io-region/32 (bar ohci) +ohci-bulk-head-pointer+))

(defun (setf get-bulk-head-pointer) (value ohci)
  (setf (pci:pci-io-region/32 (bar ohci) +ohci-bulk-head-pointer+) value))

(defun get-hub-status (ohci)
  (pci:pci-io-region/32 (bar ohci) +ohci-root-hub-status+))

(defun (setf get-hub-status) (value ohci)
  (setf (pci:pci-io-region/32 (bar ohci) +ohci-root-hub-status+) value))


(defun get-port-status (ohci port-num)
  (pci:pci-io-region/32 (bar ohci)
                        (+ +ohci-root-hub-port-status+ (* 4 port-num))))

(defun (setf get-port-status) (value ohci port-num)
  (setf (pci:pci-io-region/32 (bar ohci)
                              (+ +ohci-root-hub-port-status+ (* 4 port-num)))
        value))

(defun get-port-speed (ohci port-num)
  ;; speed = 1 => low speed
  (ldb +ps-low-speed-device+ (get-port-status ohci port-num)))

;;======================================================================
;;======================================================================

;;======================================================================
;; HCD public interface methods
;;======================================================================

(defmethod get-buffer-memory ((ohci ohci) num-bytes)
  (with-slots (phys-addr-free phys-addr-end) ohci
    ;; force 16-byte and 32-byte alignment for 16-byte buffers and
    ;; 32-byte buffers
    (cond ((= num-bytes 16)
           (setf phys-addr-free (logandc2 (+ phys-addr-free 15) #x0F)))
          ((= num-bytes 32)
           (setf phys-addr-free (logandc2 (+ phys-addr-free 31) #x1F))))
    ;; allocate the memory
    (let ((result phys-addr-free)
          (next-free (+ phys-addr-free num-bytes)))
      (when (> next-free phys-addr-end)
        (error "OHCI: Out of memory ~D ~D ~D" result num-bytes phys-addr-end))
      (setf phys-addr-free next-free)
      result)))

(defmethod debounce-port ((ohci ohci) port-num)
  (with-hcd-access (ohci)
    (loop
       with total-time = 0
       and stable-time = 0
       for status = (get-port-status ohci port-num) then
         (get-port-status ohci port-num)
       with connect-state = 0 #+nil (ldb +ps-connect-status+ status)
       do
         (sleep 0.03)
         (incf total-time 0.03)
         (incf stable-time 0.03)
         (cond ((>= total-time 1.500)
                ;; Didn't see status stable for 100ms for 1.5 seconds - give up
                (error "Debounce timeout on port ~D for OHCI" port-num))
               ((ldb-test +ps-connect-status-change+ status)
                ;; saw connect status change
                (setf (get-port-status ohci port-num)
                      (dpb 1 +ps-connect-status-change+ 0)
                      connect-state
                      (ldb +ps-connect-status+ status)
                      stable-time
                      0))
               ((/= connect-state (ldb +ps-connect-status+ status))
                ;; saw current connect state changed
                (setf connect-state
                      (ldb +ps-connect-status+ status )
                      stable-time
                      0))
               ((>= stable-time 0.100)
                (return))
               (T
                ;; no state change, no timeout, so wait some more
                )))))

(defmethod reset-port ((ohci ohci) port-num)
  (with-trace-level (1)
    (sup:debug-print-line "reset-port " port-num))

  ;; Reset port
  (with-hcd-access (ohci)
    (setf (get-port-status ohci port-num) (dpb 1 +ps-reset-status+ 0))

    (loop
       with reset-time = 0
       do (let ((status (get-port-status ohci port-num)))
            (cond ((ldb-test +ps-reset-status-change+ status)
                   ;; reset complete - wait for reset "rest" time
                   (when (ldb-test +ps-reset-status+
                                   (get-port-status ohci port-num))
                     (error "Reset signal still set on port ~D" port-num))
                   (with-trace-level (3)
                     (format sys.int::*cold-stream*
                             "    reset-time: ~5,3F~%" reset-time))
                   (setf (get-port-status ohci port-num)
                         (dpb 1 +ps-reset-status-change+ 0))
                   (sleep 0.02)
                   ;; reset "rest" complete
                   (return))
                  ((>=  reset-time 1.0)
                   ;; reset timed out
                   (error "Reset timed out on port ~D" port-num))
                  (T
                   ;; port still in reset, so wait some more
                   (sleep 0.02)
                   (incf reset-time 0.02)))))))

;;======================================================================
;;
;; Define OHCI specific endpoint
;;
;;======================================================================

(defstruct (ohci-endpoint (:include endpoint))
  ed
  buf-size
  interval
  header)

(declaim (inline device-control-ed))

(defun device-control-ed (hcd-device)
  (ohci-endpoint-ed (aref (usb-device-endpoints hcd-device) 0)))

;;======================================================================
;;
;; hcd-device
;;
;; Data structure that contains all of the device related
;; info. Includes usb-device so that the fields and methods required
;; by usb-driver are also available.
;;
;; Uses an :after method so that changes to the maximum packet size
;; get propogated to the control ed. The max packet size is normally
;; updated during device enumeration.
;;
;;
;; Creates a control endpoint descriptor and adds it to the control list.
;;
;;======================================================================

(defclass hcd-device (usb-device)
  ((%addr          :initarg  :addr         :accessor device-addr)
   (%speed         :initarg  :speed        :accessor device-speed)
   (%semaphore     :initarg  :semaphore    :accessor device-semaphore)))

(defmethod (setf usb-device-max-packet) :after
    (max-packet (hcd-device hcd-device))
  (let ((ed (device-control-ed hcd-device)))
    (setf (ed-header ed)
          (dpb max-packet +endpt-max-packet-size+ (ed-header ed)))))

(defmethod create-device ((ohci ohci) port-num)
  (with-hcd-access (ohci)
    (let* ((ed (alloc-ed ohci))
           (td (alloc-td ohci))
           (td-phys-addr (array->phys-addr td))
           (speed (get-port-speed ohci port-num))
           (device (make-instance 'hcd-device
                                  :hcd ohci
                                  :port-num port-num
                                  :addr 0
                                  :speed speed
                                  :semaphore (sync:make-semaphore
                                              :name "OHCI SEMAPHORE"))))
      (setf (aref (usb-device-endpoints device) 0)
            (make-ohci-endpoint :type :control
                                :device device
                                :driver NIL
                                :num 0
                                :event-type (device-semaphore device)
                                :ed ed
                                :buf-size nil
                                :interval nil
                                :header nil))
      (setf (ed-header ed) (encode-ed-header 0
                                             0
                                             +endpt-direction-td+
                                             speed
                                             +endpt-active+
                                             +endpt-general-tds+
                                             (if (= speed 1) 8 64))
            (ed-tdq-tail ed) td-phys-addr
            (ed-tdq-head ed) td-phys-addr)

      ;; add ed to control list
      (setf (ed-next-ed ed) (get-control-head-pointer ohci)
            (get-control-head-pointer ohci) (array->phys-addr ed))
      device)))

(defmethod delete-device ((ohci ohci) device)
  (with-trace-level (1)
    (sup:debug-print-line "delete device"))
  (with-hcd-access (ohci)
    (let* ((ed (device-control-ed device))
           (ed-phys-addr (array->phys-addr ed)))

      ;; remove control-ed from list
      (cond ((eql (get-control-head-pointer ohci) ed-phys-addr)
             (setf (get-control-head-pointer ohci) (ed-next-ed ed)))
            (T
             (loop
                for prev-ed-phys-addr = (get-control-head-pointer ohci) then
                  (ed-next-ed prev-ed)
                for prev-ed = (phys-addr->array prev-ed-phys-addr) then
                  (phys-addr->array prev-ed-phys-addr)
                when (eql (ed-next-ed prev-ed) ed-phys-addr)
                do (progn (setf (ed-next-ed prev-ed) (ed-next-ed ed))
                          (return)))))

      ;; make sure controller is not using the ed
      (clear-current-pointer ohci
                             +ohci-current-control-pointer+
                             (dpb 1 +control-control-list-enable+ 0))

      ;; free ed and all associated td's
      (free-ed ohci ed)
      (setf (aref (usb-device-endpoints device) 0) nil)

      (loop
         with endpoints = (usb-device-endpoints device)
         for endpt-num from 1 to 31
         for endpoint = (aref endpoints endpt-num) then
           (aref endpoints endpt-num)
         when endpoint do
           (ecase (ohci-endpoint-type endpoint)
             (:interrupt
              (delete-interrupt-endpt ohci device endpt-num))
             (:bulk
              (delete-bulk-endpt ohci device endpt-num))
             (:isochronous
              (delete-isochronous-endpt ohci device endpt-num)))))))

;;======================================================================
;;
;;======================================================================

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
        ((typep event-type 'sync:semaphore)
         ;; this means some thread is waiting on this interrupt
         ;; which may not be a good idea
         (sync:semaphore-up event-type))
        (T
         (funcall event-type driver endpt-num status length buf))))

;;======================================================================
;; Interrupt Endpoint code
;;======================================================================

(defun add-td (ohci ed header event-type endpoint buf-size)
  (let* ((msg-td (phys-addr->array (ed-tdq-tail ed)))
         (msg-xfer-info (gethash msg-td (td->xfer-info ohci)))
         (dummy-td (alloc-td ohci
                             :event-type event-type
                             :endpoint endpoint
                             :buf-size buf-size))
         (buf (alloc-buffer/8 (buf-pool ohci) buf-size))
         (buf-phys-addr (array->phys-addr buf)))
    (setf
     ;; initialize TD
     (td-header msg-td) header
     (td-buffer-pointer msg-td) buf-phys-addr
     (td-next-td msg-td) (array->phys-addr dummy-td)
     (td-buffer-end msg-td) (+ buf-phys-addr buf-size -1)
     (xfer-info-buf msg-xfer-info) buf
     ;; update queue tail pointer to new dummy td
     (ed-tdq-tail ed) (array->phys-addr dummy-td))
    dummy-td))

(defmethod create-interrupt-endpt
    ((ohci ohci) device driver endpt-num num-bufs buf-size event-type interval)
  (with-trace-level (1)
    (sup:debug-print-line "create-interrupt-endpt"))

  (with-hcd-access (ohci)
    (when (aref (usb-device-endpoints device) endpt-num)
      (error "Endpoint ~D already defined. Type is ~S"
             endpt-num
             (ohci-endpoint-type
              (aref (usb-device-endpoints device) endpt-num))))
    (let* ((ed (alloc-ed ohci))
           (td-header (encode-td-header +td-partial-buffer+
                                        +pid-in-token+
                                        1
                                        +td-ed-toggle+))
           (endpoint (make-ohci-endpoint :type :interrupt
                                         :device device
                                         :driver driver
                                         :num endpt-num
                                         :event-type event-type
                                         :ed ed
                                         :buf-size buf-size
                                         :interval interval
                                         :header td-header))
           (td (alloc-td ohci
                         :event-type event-type
                         :endpoint endpoint
                         :buf-size buf-size))
           (td-phys-addr (array->phys-addr td)))
      (setf
       ;; save endpoint object
       (aref (usb-device-endpoints device) endpt-num) endpoint
       ;; Setup header
       (ed-header ed) (encode-ed-header
                       (device-addr device)
                       endpt-num
                       +endpt-direction-td+
                       (device-speed device)
                       +endpt-active+
                       +endpt-general-tds+
                       (usb-device-max-packet device))
       (ed-tdq-tail ed) td-phys-addr
       (ed-tdq-head ed) td-phys-addr)

      ;; Add more td's
      (dotimes (i num-bufs)
        (add-td ohci ed td-header event-type endpoint buf-size))
      (add-interrupt-ed ohci ed buf-size interval)
      endpt-num)))

(defmethod delete-interrupt-endpt ((ohci ohci) device endpt-num)
  (with-trace-level (1)
    (sup:debug-print-line "delete-interrupt-endpt"))

  (with-hcd-access (ohci)
    (let* ((endpoint (aref (usb-device-endpoints device) endpt-num)))
      (when (null endpoint)
        (error "Endpoint ~D does not exist" endpt-num))

      (when (not (eq (ohci-endpoint-type endpoint) :interrupt))
        (error "Endpoint ~D is type ~S not :interrupt"
               endpt-num
               (ohci-endpoint-type endpoint)))

      (let ((ed (ohci-endpoint-ed endpoint)))
        (remove-interrupt-ed ohci
                             ed
                             (ohci-endpoint-buf-size endpoint)
                             (ohci-endpoint-interval endpoint))

        (wait-for-next-sof ohci)

        (setf (aref (usb-device-endpoints device) endpt-num) nil)

        (free-ed ohci ed)))))

(defun handle-interrupt-endpt (ohci xfer-info td)
  (with-trace-level (1)
    (sup:debug-print-line "handle-interrupt-endpt"))

  ;; first save the current interrupt info in data... variables
  ;; then add a new td to the interrupt ed
  ;; then call the driver interrupt handler

  (with-hcd-access (ohci)
    (let ((data-status (aref +condition-codes+
                             (ldb +td-condition-code+ (td-header td)))))

      ;; TODO check condition code - handle errors

      ;; Reuse td as "dummy" td and put back in queue
      (let* ((endpoint (xfer-info-endpoint xfer-info))
             (buf-size (ohci-endpoint-buf-size endpoint))
             (data-buf (xfer-info-buf xfer-info))
             (data-length (td-xfer-bytes td buf-size))
             (dummy-td td)
             (ed (ohci-endpoint-ed endpoint))
             (msg-td (phys-addr->array (ed-tdq-tail ed)))
             (buf (alloc-buffer/8 (buf-pool ohci) buf-size))
             (buf-phys-addr (array->phys-addr buf)))
        (setf
         ;; clean up dummy-td
         (td-header dummy-td) 0
         (td-buffer-pointer dummy-td) 0
         (td-next-td dummy-td) 0
         (td-buffer-end dummy-td) 0
         ;; update xfer-info
         (xfer-info-buf xfer-info) NIL
         (xfer-info-buf (gethash msg-td (td->xfer-info ohci))) buf
         ;; initialize TD
         (td-header msg-td) (ohci-endpoint-header endpoint)
         (td-buffer-pointer msg-td) buf-phys-addr
         (td-next-td msg-td) (array->phys-addr dummy-td)
         (td-buffer-end msg-td) (+ buf-phys-addr buf-size -1)
         ;; update queue tail pointer to new dummy td
         (ed-tdq-tail ed) (array->phys-addr dummy-td))

        ;; Signal driver that an interrupt transfer is complete
        (transfer-complete (ohci-endpoint-driver endpoint)
                           (xfer-info-event-type xfer-info)
                           (ohci-endpoint-num endpoint)
                           (ohci-endpoint-device endpoint)
                           data-status
                           data-length
                           data-buf)))))

;;======================================================================
;; Bulk Endpoint code
;;======================================================================

(defmethod create-bulk-endpt
    ((ohci ohci) device driver endpt-num in-p event-type)
  (with-trace-level (1)
    (sup:debug-print-line "create-bulk-endpt"))

  (with-hcd-access (ohci)
    (when (aref (usb-device-endpoints device) endpt-num)
      (error "Endpoint ~D already defined. Type is ~S"
             endpt-num
             (ohci-endpoint-type
              (aref (usb-device-endpoints device) endpt-num))))
    (let* ((ed (alloc-ed ohci))
           (td-header (encode-td-header
                       +td-partial-buffer+
                       (if in-p +pid-in-token+ +pid-out-token+)
                       1
                       +td-ed-toggle+))
           (endpoint (make-ohci-endpoint :type :bulk
                                         :device device
                                         :driver driver
                                         :num endpt-num
                                         :event-type event-type
                                         :ed ed
                                         :buf-size :n/a
                                         :interval :n/a
                                         :header td-header))
           (td (alloc-td ohci
                         :event-type event-type
                         :endpoint endpoint))
           (td-phys-addr (array->phys-addr td)))
      (setf
       ;; save endpoint object
       (aref (usb-device-endpoints device) endpt-num) endpoint
       (ed-header ed) (encode-ed-header
                       (device-addr device)
                       endpt-num
                       (if in-p +endpt-direction-in+ +endpt-direction-out+)
                       (device-speed device)
                       +endpt-active+
                       +endpt-general-tds+
                       (usb-device-max-packet device))
       (ed-tdq-tail ed) td-phys-addr
       (ed-tdq-head ed) td-phys-addr)

      ;; Add bulk endpoint to controller
      (let ((bar0 (bar ohci)))
        (setf (ed-next-ed ed)
              (pci:pci-io-region/32 bar0 +ohci-bulk-head-pointer+)
              (pci:pci-io-region/32 bar0 +ohci-bulk-head-pointer+)
              (array->phys-addr ed))))))

(defmethod delete-bulk-endpt ((ohci ohci) device endpt-num)
  (with-trace-level (1)
    (sup:debug-print-line "delete-bulk-endpt"))

  (with-hcd-access (ohci)
    (let* ((endpoint (aref (usb-device-endpoints device) endpt-num)))
      (when (null endpoint)
        (error "Endpoint ~D does not exist" endpt-num))

      (when (not (eq (ohci-endpoint-type endpoint) :bulk))
        (error "Endpoint ~D is type ~S not :bulk"
               endpt-num
               (ohci-endpoint-type endpoint)))

      ;; remove bulk-ed from list
      (let* ((ed (ohci-endpoint-ed endpoint))
             (ed-phys-addr (array->phys-addr ed)))

        ;; remove bulk-ed from list
        (cond ((eql (get-bulk-head-pointer ohci) ed-phys-addr)
               (setf (get-bulk-head-pointer ohci) (ed-next-ed ed)))
              (T
               (loop
                  for prev-ed-phys-addr = (get-bulk-head-pointer ohci) then
                    (ed-next-ed prev-ed)
                  for prev-ed = (phys-addr->array prev-ed-phys-addr) then
                    (phys-addr->array prev-ed-phys-addr)
                  when (eql (ed-next-ed prev-ed) ed-phys-addr)
                  do (progn (setf (ed-next-ed prev-ed) (ed-next-ed ed))
                            (return)))))

        ;; make sure controller is not using the ed
        (clear-current-pointer ohci
                               +ohci-current-bulk-pointer+
                               (dpb 1 +control-bulk-list-enable+ 0))

        ;; free ed and all associated td's
        (free-ed ohci ed)))))

(defmethod bulk-enqueue-buf ((ohci ohci) device endpt-num buf num-bytes)
  (with-trace-level (1)
    (sup:debug-print-line "bulk-enqueue-buf"))

  (when (> num-bytes (length buf))
    (error "Invalid arguments num-bytes (~D) > length of buffer (~D)"
           num-bytes (length buf)))

  (let* ((endpoint (aref (usb-device-endpoints device) endpt-num))
         (ed (ohci-endpoint-ed endpoint))
         (msg-td (phys-addr->array (ed-tdq-tail ed)))
         (msg-xfer-info (gethash msg-td (td->xfer-info ohci)))
         (dummy-td (alloc-td ohci
                             :event-type (ohci-endpoint-event-type endpoint)
                             :endpoint endpoint))
         (dummy-td-phys-addr (array->phys-addr dummy-td))
         (buf-phys-addr (array->phys-addr buf)))
    (setf
     (td-header msg-td) (ohci-endpoint-header endpoint)
     (td-buffer-pointer msg-td) buf-phys-addr
     (td-next-td msg-td) dummy-td-phys-addr
     (td-buffer-end msg-td) (+ buf-phys-addr num-bytes -1)
     (xfer-info-buf-size msg-xfer-info) num-bytes
     (xfer-info-buf msg-xfer-info) buf
     ;; update queue tail pointer to new dummy td
     (ed-tdq-tail ed) dummy-td-phys-addr)

    ;; These two lines attempt to force the ED to be up-to-date before
    ;; the controller reads it.
    (sys.int::dma-write-barrier)
    (pci:pci-io-region/32 (bar ohci) +ohci-command-status+)

    (setf (pci:pci-io-region/32 (bar ohci) +ohci-command-status+)
          (dpb 1 +command-bulk-list-filled+ 0))
    (values)))

(defmethod bulk-dequeue-buf ((ohci ohci) device endpt-num buf)
  (with-trace-level (1)
    (sup:debug-print-line "bulk-dequeue-buf"))
  ;; TODO stop this queue ...
  (let* ((endpoint (aref (usb-device-endpoints device) endpt-num))
         (ed (ohci-endpoint-ed endpoint))
         (td->xfer-info (td->xfer-info ohci)))
    (loop
       for prev-td = NIL then td
       for td-phys-addr = (ed-tdq-head-tdq-addr ed) then (td-next-td td)
       for td = (phys-addr->array td-phys-addr)
       for msg-xfer-info = (gethash td td->xfer-info)
       when (eq buf (xfer-info-buf msg-xfer-info)) do
       ;; remove td from list
         (if prev-td
             (setf (td-next-td prev-td) (td-next-td td))
             (setf (ed-tdq-head ed) (td-next-td td)))
         (free-td ohci td)
         (return T)
       when (= td-phys-addr (ed-tdq-tail ed)) do
         ;; td not found
         (return NIL))))

(defun handle-bulk-endpt (ohci xfer-info td)
  (with-trace-level (1)
    (sup:debug-print-line "handle-bulk-endpt"))

  (with-hcd-access (ohci)
    (unwind-protect
         (let ((status (aref +condition-codes+
                             (ldb +td-condition-code+ (td-header td)))))

           ;; TODO check condition code - handle errors
           (let* ((endpoint (xfer-info-endpoint xfer-info)))
             (transfer-complete (ohci-endpoint-driver endpoint)
                                (xfer-info-event-type xfer-info)
                                (ohci-endpoint-num endpoint)
                                (ohci-endpoint-device endpoint)
                                status
                                (td-xfer-bytes td (xfer-info-buf-size xfer-info))
                                (xfer-info-buf xfer-info))))
      (free-td ohci td))))

;;======================================================================
;; set-device-address
;;======================================================================

(defmethod set-device-address ((ohci ohci) device address)
  (with-buffers ((buf-pool ohci) (buf /8 1))     ;; don't want 0 length buf
    (control-receive-data ohci
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
  (let* ((ed (device-control-ed device)))
    (with-hcd-access (ohci)
      (setf (ed-header ed)
            (dpb address +endpt-function-addr-field+ (ed-header ed))
            (device-addr device)
            address)))

  ;; According to microsoft doc, need to wait 10ms after
  ;; set-address before get-descriptor
  (sleep 0.03))

;;======================================================================
;;
;; control-receive-data
;;
;; Primary function for receiving data on the control endpoint.
;;
;; It is also used for USB commands that don't actually receive any
;; data, such as, set address and set configuration. In these cases,
;; the length should be 0 and the buf can be any length buffer,
;; although the convention is to use a buffer of length 1.
;;
;;======================================================================

(defmethod control-receive-data
    ((ohci ohci) device request-type request value index length buf)
  (with-trace-level (1)
    (sup:debug-print-line  "control-receive-data"))

  (with-hcd-access (ohci)
    (let* ((endpoint (aref (usb-device-endpoints device) 0))
           (event-type (ohci-endpoint-event-type endpoint))
           (ed (ohci-endpoint-ed endpoint))
           (msg-td (phys-addr->array (ed-tdq-head-tdq-addr ed)))
           (msg-xfer-info (gethash msg-td (td->xfer-info ohci)))
           (msg-buf (alloc-buffer/8 (buf-pool ohci) 8))
           (dummy-td (alloc-td ohci))
           (semaphore (device-semaphore device)))

      (encode-td msg-td +td-partial-buffer+ +pid-setup-token+ 3 +td-toggle-0+
                 msg-buf
                 dummy-td)

      (encode-request msg-buf
                      request-type
                      request
                      value
                      index
                      length)
      (setf (xfer-info-event-type msg-xfer-info) event-type
            (xfer-info-endpoint msg-xfer-info) endpoint
            (xfer-info-buf-size msg-xfer-info) 8
            (xfer-info-buf msg-xfer-info) msg-buf
            (ed-tdq-tail ed) (array->phys-addr dummy-td))

      (with-trace-level (3)
        (print-ed sys.int::*cold-stream* ed :indent "    " :buffers t))

      (sys.int::dma-write-barrier)

      (setf (pci:pci-io-region/32 (bar ohci) +ohci-command-status+)
            (dpb 1 +command-control-list-filled+ 0))

      ;; TODO wait with timeout?
      (sync:semaphore-down semaphore)

      (setf (xfer-info-buf msg-xfer-info) NIL)
      (free-buffer msg-buf)

      (when (/= (ldb +td-condition-code+ (td-header msg-td)) 0)
        ;; non-zero means error
        (when *error-td*
          (free-td ohci *error-td*))
        (setf *error-td* msg-td)
        (let ((condition-code (ldb +td-condition-code+ (td-header msg-td))))
          (error
           "control-receive-data: Send command failed with error ~A (#x~2,'0X)."
           (aref +condition-codes+ condition-code)
           condition-code)))

      (setf dummy-td msg-td ;; re-use previous td as "dummy" td
            msg-td (phys-addr->array (ed-tdq-head-tdq-addr ed))
            msg-xfer-info (gethash msg-td (td->xfer-info ohci))

            ;; clean up dummy-td
            (td-header dummy-td) 0
            (td-buffer-pointer dummy-td) 0
            (td-next-td dummy-td) 0
            (td-buffer-end dummy-td) 0)

      (encode-td
       msg-td +td-partial-buffer+ +pid-in-token+ 3 +td-toggle-1+
       buf
       dummy-td)

      (setf (xfer-info-event-type msg-xfer-info) event-type
            (xfer-info-endpoint msg-xfer-info) endpoint
            (xfer-info-buf-size msg-xfer-info) length
            (xfer-info-buf msg-xfer-info) buf
            (ed-tdq-tail ed) (array->phys-addr dummy-td))

      (with-trace-level (3)
        (print-ed sys.int::*cold-stream* ed :indent "    " :buffers t))

      (sys.int::dma-write-barrier)

      ;; tell HC that the control list has work
      (setf (pci:pci-io-region/32 (bar ohci) +ohci-command-status+)
            (dpb 1 +command-control-list-filled+ 0))

      ;; TODO wait with timeout?
      (sync:semaphore-down semaphore)

      (when (/= (ldb +td-condition-code+ (td-header msg-td)) 0)
        ;; non-zero means error
        (when *error-td*
          (free-td ohci *error-td*))
        (setf *error-td* msg-td)
        (let ((condition-code (ldb +td-condition-code+ (td-header msg-td))))
          (error
           "control-receive-data: Receive buffer failed with error ~A (#x~2,'0X)."
           (aref +condition-codes+ condition-code)
           condition-code)))
      (prog1
          (td-xfer-bytes msg-td length)
        (free-td ohci msg-td)))))

;;======================================================================
;;
;; control-send-data
;;
;; Primary function for sending data buffers on the control endpoint.
;;
;;======================================================================

(defmethod control-send-data
    ((ohci ohci) device request-type request value index length buf)
  (with-trace-level (1)
    (sup:debug-print-line  "control-send-data"))

  (with-hcd-access (ohci)
    (let* ((endpoint (aref (usb-device-endpoints device) 0))
           (event-type (ohci-endpoint-event-type endpoint))
           (ed (ohci-endpoint-ed endpoint))
           (msg-td (phys-addr->array (ed-tdq-head-tdq-addr ed)))
           (msg-xfer-info (gethash msg-td (td->xfer-info ohci)))
           (msg-buf (alloc-buffer/8 (buf-pool ohci) 8))
           (dummy-td (alloc-td ohci))
           (semaphore (device-semaphore device)))

      (encode-td msg-td +td-partial-buffer+ +pid-setup-token+ 3 +td-toggle-0+
                 msg-buf
                 dummy-td)

      (encode-request msg-buf
                      request-type
                      request
                      value
                      index
                      length)

      (setf (xfer-info-event-type msg-xfer-info) event-type
            (xfer-info-endpoint msg-xfer-info) endpoint
            (xfer-info-buf-size msg-xfer-info) 8
            (xfer-info-buf msg-xfer-info) msg-buf
            (ed-tdq-tail ed) (array->phys-addr dummy-td))

      (with-trace-level (3)
        (print-ed sys.int::*cold-stream* ed :indent "    " :buffers t))

      (sys.int::dma-write-barrier)

      ;; tell HC that the control list has work
      (setf (pci:pci-io-region/32 (bar ohci) +ohci-command-status+)
            (dpb 1 +command-control-list-filled+ 0))

      ;; TODO wait with timeout?
      (sync:semaphore-down semaphore)

      (setf (xfer-info-buf msg-xfer-info) NIL)
      (free-buffer msg-buf)

      (when (/= (ldb +td-condition-code+ (td-header msg-td)) 0)
        ;; non-zero means error
        (when *error-td*
          (free-td ohci *error-td*))
        (setf *error-td* msg-td)
        (let ((condition-code (ldb +td-condition-code+ (td-header msg-td))))
          (error
           "control-receive-data: Send command failed with error ~A (#x~2,'0X)."
           (aref +condition-codes+ condition-code)
           condition-code)))

      (setf dummy-td msg-td ;; re-use previous td as "dummy" td
            msg-td (phys-addr->array (ed-tdq-head-tdq-addr ed))
            msg-xfer-info (gethash msg-td (td->xfer-info ohci))


            ;; clean up dummy-td
            (td-header dummy-td) 0
            (td-buffer-pointer dummy-td) 0
            (td-next-td dummy-td) 0
            (td-buffer-end dummy-td) 0)

      (encode-td
       msg-td +td-partial-buffer+ +pid-out-token+ 3 +td-toggle-1+
       buf
       dummy-td)

      (setf (xfer-info-event-type msg-xfer-info) event-type
            (xfer-info-endpoint msg-xfer-info) endpoint
            (xfer-info-buf-size msg-xfer-info) length
            (xfer-info-buf msg-xfer-info) buf
            (ed-tdq-tail ed) (array->phys-addr dummy-td))

      (with-trace-level (3)
        (print-ed sys.int::*cold-stream* ed :indent "    " :buffers t))

      (sys.int::dma-write-barrier)

      ;; tell HC that the control list has work
      (setf (pci:pci-io-region/32 (bar ohci) +ohci-command-status+)
            (dpb 1 +command-control-list-filled+ 0))

      ;; TODO wait with timeout?
      (sync:semaphore-down semaphore)

      (when (/= (ldb +td-condition-code+ (td-header msg-td)) 0)
        (when *error-td*
          (free-td ohci *error-td*))
        (setf *error-td* msg-td)
        (let ((condition-code (ldb +td-condition-code+ (td-header msg-td))))
          (error
           "control-receive-data: Receive command failed with error ~A (#x~2,'0X)."
           (aref +condition-codes+ condition-code)
           condition-code)))

      (prog1
          (td-xfer-bytes msg-td length)
        (free-td ohci msg-td)))))

;;======================================================================
;; OHCI interrupt event handlers
;;======================================================================

(defmethod handle-interrupt-event (type (ohci ohci) event)
  (declare (ignore ohci event))
  ;; Default method for OHCI events - call for any event which is not
  ;; handled above, or handled by usbd.
  (error "OHCI interrupt event ~A not handled. Error signaled, but ignored."
         type))

(defmacro enqueue-interrupt-event (usbd type &body fields-values)
  (let ((event (gensym "event-")))
    `(let ((,event (alloc-interrupt-event ,usbd)))
       (setf (interrupt-event-type ,event) ,type)
       ,@(loop for (field value) on fields-values by #'cddr
            collect `(setf (,field ,event) ,value))
       (enqueue-event ,event))))

(defmethod handle-interrupt-event
    ((type (eql :hub-status-change)) (ohci ohci) event)
  (with-trace-level (1)
    (sup:debug-print-line "handle-interrupt-event - :hub-status-change"))
  (with-hcd-access (ohci)
    (let ((status (get-hub-status ohci)))
      (when (and (logbitp 17 status) (logbitp 1 status))
        ;; Over Current Indicator Change and Over Current Indicator
        (error "USB OHCI reports over current ~A" ohci)))
    (unwind-protect
         (dotimes (port-num (num-ports ohci))
           (let ((status (get-port-status ohci port-num)))
             (when (ldb-test +ps-connect-status-change+ status)
               (let ((type (if (ldb-test +ps-connect-status+ status)
                               :port-connect
                               :port-disconnect))
                     (event))
                 (unwind-protect
                      (progn
                        (setf event (alloc-interrupt-event ohci)
                              (interrupt-event-type event) type
                              (interrupt-event-port-num event) port-num)
                        (handle-interrupt-event type ohci event))
                   (when (and event
                              (not (eq (interrupt-event-type event) :free)))
                     (free-interrupt-event (interrupt-event-hcd event) event))
                   (setf (get-port-status ohci port-num)
                         (dpb 1 +ps-connect-status-change+ 0)))))))
      ;; Finished handling the interrupt, so enable the root hub change interrupt
      (setf (get-interrupt-enable ohci)
            (dpb 1 +interrupt-root-hub-change+ 0)))))

(defvar *done-heads* NIL) ;; for debug

(defmethod handle-interrupt-event
    ((type (eql :writeback-done)) (ohci ohci) event)
  (with-hcd-access (ohci)
    (let ((done-head (logandc2 (hcca-done-head ohci) #x0F)))
      (setf (get-interrupt-status ohci) (dpb 1 +interrupt-done-head+ 0))
      (setf (get-interrupt-enable ohci) (dpb 1 +interrupt-done-head+ 0))

      ;; TODO - need to reverse the list first
      (loop
         for td = (phys-addr->array done-head) then (phys-addr->array next-td)
         for next-td = (td-next-td td) then (td-next-td td)
         for xfer-info = (gethash td (td->xfer-info ohci)) then
           (gethash td (td->xfer-info ohci))
         do (with-trace-level (6)
              (format sys.int::*cold-stream*
                      "td done: ~S~%" (type-of (xfer-info-endpoint xfer-info))))
           (cond ((null xfer-info)
                  ;; internal error no mapping from td to xfer-info - log it
                  (format sys.int::*cold-stream*
                          "No xfer-info for TD ~A~%" td))
                 (T
                  (let ((endpoint (xfer-info-endpoint xfer-info)))
                    (ecase (ohci-endpoint-type endpoint)
                      (:control
                       (sync:semaphore-up (ohci-endpoint-event-type endpoint)))
                      (:interrupt
                       (handle-interrupt-endpt ohci xfer-info td))
                      (:bulk
                       (handle-bulk-endpt ohci xfer-info td))
                      (:isochronous
                       (error "isochronous endpoint not implemented"))))))

         when (= next-td 0) do (return)))))

(defmethod handle-interrupt-event
    ((type (eql :controller-disconnect)) (ohci ohci) event)
  (delete-controller ohci))

;;======================================================================
;; Worker thread which handles PCIe interrupts
;;
;; While the interrupt thread could be part of usb-driver and call a
;; method to enqueue the interrupt events, having the thread direcly
;; create and enqueue the events saves the overhead of a method call
;; in the critical path of interrupt handling.
;;
;;======================================================================

(defun interrupt-thread-main (ohci)
  (let ((pci-irq (pci-irq ohci))
        (done-head-mask (dpb 1 +interrupt-done-head+ 0)))
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
                 (let* ((ohci (disconnect-hcd c))
                        (event (alloc-interrupt-event ohci)))
                   (setf (interrupt-event-type event) :controller-disconnect)
                   (enqueue-event event))
                 ;; should never return - thread should be killed
                 (sleep 60)
                 ;; if we return, exit
                 (return-from interrupt-thread-main))))
           (sync:wait-for-objects
            pci-irq
            (pci:pci-device-boot-id (pci-device ohci)))
           (with-hcd-access (ohci)
             (let ((interrupts (logand (get-interrupt-status ohci)
                                       (get-interrupt-enable ohci))))
               ;; disable pending interrupts, clear them,
               ;; then enable the master interrupt
               ;; Can't clear done head interrupt until list is processed
               (setf
                (get-interrupt-disable ohci) interrupts
                (get-interrupt-status ohci) (logandc2 interrupts
                                                      done-head-mask)
                (get-interrupt-enable ohci) #x80000000)
               ;; Re-enable PCI interrupts
               (sup:simple-irq-unmask pci-irq)
               ;; Create and enqueue an interrupt event for each pending
               ;; interrupt. Worker threads then handle the interrupt events.
               (macrolet ((%handle-interrupt-event (bit type &body fields-values)
                            (let ((event (gensym "event-")))
                              `(when (logbitp ,bit interrupts)
                                 (let ((,event (alloc-interrupt-event ohci)))
                                   (setf (interrupt-event-type ,event) ,type)
                                   ,@(loop for (field value) on fields-values by #'cddr
                                        collect `(setf (,field ,event) ,value))
                                   (enqueue-event ,event))))))
                 ;; enqueue each interrupt individually
                 (%handle-interrupt-event 0 :overrun)
                 (%handle-interrupt-event 1 :writeback-done)
                 (%handle-interrupt-event 2 :start-of-frame)
                 (%handle-interrupt-event 3 :resume-detected)
                 (%handle-interrupt-event 4 :unrecoverable-error)
                 (%handle-interrupt-event 5 :frame-rollover)
                 (%handle-interrupt-event 6 :hub-status-change)
                 (%handle-interrupt-event 30 :ownership-change)))))))))

;;======================================================================
;; Interrupt Table Routines
;;======================================================================

(defstruct (int-node (:print-function int-node-print))
  (bandwidth 0) ;; bandwith consumed by this node and all previous levels
  ed     ;; disabled endpoint descriptor
  next   ;; next level int-node - used for propagating bandwidth to next levels
  prev1  ;; previous level int-nodes - used for propagating bandwidth
  prev2  ;;     to previous levels
  idx)   ;; index of next (next level int-node) for printing and debug

(defun int-node-print (int-node stream level)
  (declare (ignore level))
  (format stream "<int-node lbw: ~2D, ed: ~8,'0X/~8,'0X[~2D]>"
          (round (log (1+ (int-node-bandwidth int-node)) 10))
          (array->phys-addr (int-node-ed int-node))
          (ed-next-ed (int-node-ed int-node))
          (int-node-idx int-node)))

;; bit reverse <num> with width <bits>
(defun bit-reverse (bits num)
  (let ((result 0))
    (dotimes (i bits result)
      (when (logbitp i num)
        (setf result (dpb 1 (byte 1 (- bits i 1)) result))))))

(defun reorder-interrupt-level (int-level temp)
  (let ((num-elements (array-dimension int-level 0)))
    (loop
       with bits = (round (log num-elements 2))
       for idx1 from 0 to (1- num-elements)
       for idx2 = (bit-reverse bits idx1) then (bit-reverse bits idx1)
       do (setf (aref temp idx2) (aref int-level idx1)))
    (loop
       for idx from 0 to (1- num-elements)
       do (setf (aref int-level idx) (aref temp idx)))))

(defun init-interrupt-level (ohci prev-level cur-level)
  (declare (ignore unused))
  (loop
     with num-entries = (array-dimension cur-level 0)
     for idx from 0 to (1- num-entries)
     for idx1 = idx                  then idx
     for idx2 = (+ idx1 num-entries) then (+ idx1 num-entries)
     for ed = (alloc-disabled-ed ohci) then (alloc-disabled-ed ohci)
     for int-node1 = (aref prev-level idx1) then (aref prev-level idx1)
     for int-node2 = (aref prev-level idx2) then (aref prev-level idx2)
     for new-int-node = (make-int-node :ed ed) then (make-int-node :ed ed)
     do (setf (aref cur-level idx) new-int-node
              (int-node-prev1 new-int-node) int-node1
              (int-node-prev2 new-int-node) int-node2
              (int-node-next int-node1) new-int-node
              (int-node-idx int-node1) idx
              (ed-next-ed (int-node-ed int-node1)) (array->phys-addr ed)
              (int-node-next int-node2) new-int-node
              (int-node-idx int-node2) idx
              (ed-next-ed (int-node-ed int-node2)) (array->phys-addr ed))))

(defun init-interrupt-table (ohci)
  (with-slots (interrupt-table 32ms-interrupts 16ms-interrupts 8ms-interrupts
                               4ms-interrupts 2ms-interrupts 1ms-interrupts) ohci
    (setf 32ms-interrupts (make-array 32)
          16ms-interrupts (make-array 16)
          8ms-interrupts  (make-array  8)
          4ms-interrupts  (make-array  4)
          2ms-interrupts  (make-array  2)
          1ms-interrupts  (make-array  1))

    (loop for i from 0 to 31
       for ed = (alloc-disabled-ed ohci) then (alloc-disabled-ed ohci)
       do (setf (aref 32ms-interrupts i) (make-int-node
                                          :ed ed
                                          :prev1 nil
                                          :prev2 nil)
                (aref interrupt-table i) (array->phys-addr ed)))

    (init-interrupt-level ohci 32ms-interrupts 16ms-interrupts)
    (init-interrupt-level ohci 16ms-interrupts  8ms-interrupts)
    (init-interrupt-level ohci  8ms-interrupts  4ms-interrupts)
    (init-interrupt-level ohci  4ms-interrupts  2ms-interrupts)
    (init-interrupt-level ohci  2ms-interrupts  1ms-interrupts)

    (let ((int-node (aref 1ms-interrupts 0)))
      (setf (int-node-next int-node) nil
            (int-node-idx int-node) -1
            (ed-next-ed (int-node-ed int-node)) 0))

    (let ((temp (make-array 32)))
      (reorder-interrupt-level 32ms-interrupts temp)
      (reorder-interrupt-level 16ms-interrupts temp)
      (reorder-interrupt-level  8ms-interrupts temp)
      (reorder-interrupt-level  4ms-interrupts temp))
    (values)))

(defun find-minimum-int-node (int-level)
  (loop for int-node across int-level
     with minimum = most-positive-fixnum
     with result = nil
     when (= (int-node-bandwidth int-node) 0) do
       (return int-node)
     when (< (int-node-bandwidth int-node) minimum) do
       (setf minimum (int-node-bandwidth int-node)
             result int-node)
       finally (return result)))

(defun propagate-bandwidth-to-previous (int-node bandwidth)
  (let ((prev1 (int-node-prev1 int-node))
        (prev2 (int-node-prev2 int-node)))
    (when prev1
      (incf (int-node-bandwidth prev1) bandwidth)
      (propagate-bandwidth-to-previous prev1 bandwidth))

    (when prev2
      (incf (int-node-bandwidth prev2) bandwidth)
      (propagate-bandwidth-to-previous prev2 bandwidth))))

(defun propagate-bandwidth-to-next (int-node bandwidth)
  (loop
     for next-int-node = (int-node-next int-node) then
       (int-node-next next-int-node)
     when (null next-int-node) do (return (values))
     do (incf (int-node-bandwidth next-int-node) bandwidth)))

(defconstant +interval->int-level+
  '((32 32ms-interrupts)
    (16 16ms-interrupts)
    ( 8  8ms-interrupts)
    ( 4  4ms-interrupts)
    ( 2  2ms-interrupts)
    ( 1  1ms-interrupts)))

(defun %add-interrupt-ed (int-level ed bandwidth)
  (let ((int-node (find-minimum-int-node int-level)))
    (incf (int-node-bandwidth int-node) bandwidth)
    (setf (ed-next-ed ed) (ed-next-ed (int-node-ed int-node))
          (ed-next-ed (int-node-ed int-node)) (array->phys-addr ed))

    ;; propagate to next and previous levels
    (propagate-bandwidth-to-next int-node bandwidth)
    (propagate-bandwidth-to-previous int-node bandwidth)))

(defun add-interrupt-ed (ohci ed buf-size interval)
  (loop for (step int-level-func) in +interval->int-level+
     when (<= step interval)
     do (with-hcd-access (ohci)
          (%add-interrupt-ed
           (funcall int-level-func ohci)
           ed
           ;; TODO is this reasonable for bandwidth calculation?
           (/ (* buf-size 1000) step)))
       (return)))

(defun %remove-interrupt-ed (int-level rem-ed bandwidth)
  (loop
     for int-node across int-level
     with rem-ed-phys-addr = (array->phys-addr rem-ed)
     when (loop
             for ed = (int-node-ed int-node) then
               (phys-addr->array (ed-next-ed ed))
             when (= (ed-next-ed ed) 0) do (return NIL)
             when (= (ed-next-ed ed) rem-ed-phys-addr) do
               (setf (ed-next-ed ed) (ed-next-ed rem-ed))
               (return T)) do
       (incf (int-node-bandwidth int-node) bandwidth)
     ;; propagate to next and previous levels
       (propagate-bandwidth-to-next int-node bandwidth)
       (propagate-bandwidth-to-previous int-node bandwidth)
       (return-from %remove-interrupt-ed))
  (error "Unable to find interrupt ed in interrupt table"))

(defun remove-interrupt-ed (ohci ed buf-size interval)
  (loop for (step int-level-func) in +interval->int-level+
     when (<= step interval)
     do (with-hcd-access (ohci)
          (%remove-interrupt-ed
           (funcall int-level-func ohci)
           ed
           ;; TODO is this reasonable for bandwidth calculation?
           (- (/ (* buf-size 1000) step))))
       (return)))

;;======================================================================
;; Init Routines
;;======================================================================

(defun init-memory (ohci)
  ;; Allocate physical memory
  (with-slots (phys-addr phys-addr-free phys-addr-end) ohci
    (setf phys-addr (* (or (sup::allocate-physical-pages
                            +ohci-phys-memory-pages+ :32-bit-only T)
                           (error "Unable to allocate physical memory!"))
                       sup::+4k-page-size+)
          phys-addr-free phys-addr
          phys-addr-end  (+ phys-addr (* +ohci-phys-memory-pages+
                                         sup::+4k-page-size+)))
    ;; Allocate HCCA
    (setf (hcca-phys-addr ohci) phys-addr-free
          (interrupt-table ohci) (make-array
                                  32
                                  :element-type '(unsigned-byte 32)
                                  :physical-memory phys-addr-free))
    (incf phys-addr-free 256)

    ;; skip frame number and writeback done head
    (incf phys-addr-free 8)

    (setf (buf-pool ohci)
          (create-buffer-pool ohci "OHCI Driver" '(16 32 128 1024 4096)))))

(defun ohci-probe (device)
  (sup:debug-print-line "Probing OHCI")
  (sup:with-device-access ((pci:pci-device-boot-id device)
                           (return-from ohci-probe))
    ;; map bar0
    (let* ((bar0 (pci:pci-io-region device 0))
           (rh-a (pci:pci-io-region/32 bar0 +ohci-root-hub-descriptor-a+))
           (ohci (make-instance 'ohci
                                :pci-device device
                                :pci-irq (sup:make-simple-irq
                                          (pci:pci-intr-line device))
                                :num-ports (ldb (byte 8 0) rh-a)
                                :lock (sup:make-mutex "OHCI Lock")
                                :bar bar0
                                :hcca-phys-addr 0 ;; dummy value for print-ohci
                                ;; ### Does this need to be synchronized?
                                :td->xfer-info (make-hash-table :synchronized t))))
      (setf (pci:pci-bus-master-enabled device) T)
      (setf *ohci* ohci)

      (init-memory ohci)
      (init-interrupt-table ohci)

      (setf (interrupt-thread ohci) (sup:make-thread
                                     (lambda () (interrupt-thread-main ohci))
                                     :name "OHCI interrupt thread"))
      (sup:simple-irq-unmask (pci-irq ohci))

      ;; take control of the HC - sections 5.1.1.3.3 - 5.1.1.3.5
      (let ((control (pci:pci-io-region/32 bar0 +ohci-control+)))
        (cond ((ldb-test +control-interrupt-routing+ control)
               ;; System Management Mode - change ownership to this driver
               (setf (pci:pci-io-region/32 bar0 +ohci-command-status+)
                     (dpb 1 +command-ownership-change+ 0))
               ;; Wait for ownership to change
               (loop
                  do (sleep 0.010)
                  when (not (ldb-test +control-interrupt-routing+ control))
                  do (return nil)))
              ((ldb-test +control-functional-state+ control)
               ;; state not reset => BIOS Active
               (when (/= (ldb +control-functional-state+ control)
                         +functional-state-operational+)
                 (setf (pci:pci-io-region/32 bar0 +ohci-control+)
                       (dpb +functional-state-resume+
                            +control-functional-state+
                            0))
                 (sleep 0.021))) ;; >= 20 ms
              (T
               ;; this driver has control - wait for reset time
               (sleep 0.003))))  ;; >= 2.5 ms

      ;; Setup the host controller - section 5.1.1.4 - 5.1.1.5
      (let ((frame-interval (pci:pci-io-region/32 bar0 +ohci-frame-interval+)))
        (setf (pci:pci-io-region/32 bar0 +ohci-command-status+)
              (dpb 1 +command-controller-reset+ 0))
        (sleep 0.000015) ;; >= 10 usec

        ;; Init registers here must take <= 2ms before setting state to
        ;; operational
        (setf (pci:pci-io-region/32 bar0 +ohci-interrupt-enable+)
              (logior
               (dpb 1 +interrupt-scheduling-overrun+ 0)
               (dpb 1 +interrupt-done-head+ 0)
               (dpb 0 +interrupt-start-of-frame+ 0)
               (dpb 1 +interrupt-resume-detect+ 0)
               (dpb 1 +interrupt-unrecoverable-error+ 0)
               (dpb 0 +interrupt-frame-number-overflow+ 0)
               (dpb 0 +interrupt-root-hub-change+ 0)
               (dpb 1 +interrupt-ownership-change+ 0)
               (dpb 1 +interrupt-master-enable+ 0))
              )

        (setf (pci:pci-io-region/32 bar0 +ohci-hcca+)
              (hcca-phys-addr ohci))

        (setf  (pci:pci-io-region/32 bar0 +ohci-control-head-pointer+) 0
               (pci:pci-io-region/32 bar0 +ohci-current-control-pointer+) 0
               (pci:pci-io-region/32 bar0 +ohci-bulk-head-pointer+) 0
               (pci:pci-io-region/32 bar0 +ohci-current-bulk-pointer+) 0)

        (setf  (pci:pci-io-region/32 bar0 +ohci-frame-interval+)
               (logior
                (dpb #x2edf +frame-interval-interval+ 0)
                (dpb (compute-fsmps #x2edf) +frame-interval-max-data-packet+ 0)

                (logxor (logand (pci:pci-io-region/32 bar0 +ohci-frame-interval+)
                                #x80000000)
                        #x80000000)))

        (setf  (pci:pci-io-region/32 bar0 +ohci-low-speed-threshold+) #x0628)

        (setf (pci:pci-io-region/32 bar0 +ohci-control+)
              (logior
               (dpb 0 +control-bulk-service-ratio+ 0)
               (dpb 1 +control-periodic-list-enable+ 0)
               (dpb 0 +control-isochronous-enable+ 0)
               (dpb 1 +control-control-list-enable+ 0)
               (dpb 1 +control-bulk-list-enable+ 0)
               (dpb +functional-state-operational+ +control-functional-state+ 0)
               (dpb 0 +control-interrupt-routing+ 0)
               (dpb 0 +control-remote-wakeup-enable+ 0))
              )

        )

      ;; generate :hub-status-change event so that devices already
      ;; connected are serviced
      (let ((event (alloc-interrupt-event ohci)))
        (setf (interrupt-event-type event) :hub-status-change)
        (enqueue-event event)))))

(pci:define-pci-driver OHCI-driver ohci-probe () ((#x0c #x03 #x10)))
