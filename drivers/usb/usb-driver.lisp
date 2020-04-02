;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

;;======================================================================
;;
;; This file contains the USB Driver which is the layer between the
;; Hardware Abstraction Layer (HAL) AKA Hardware Controller Driver
;; (HCD) and the USB device drivers.
;;
;;======================================================================

(in-package :mezzano.driver.usb)

;;======================================================================
;;
;; with-hcd-access must used around references to hardware registers
;; and to physical memory (either directly or via a buffer array)
;;
;; TODO - add with-hcd-access as required
;;
;;======================================================================

(define-condition controller-disconnect ()
  ((%hcd :initarg :hcd :reader disconnect-hcd)))

(defmacro with-hcd-access ((hcd) &body body)
  `(sup:with-device-access
       ((pci:pci-device-boot-id (pci-device ,hcd))
        (signal 'controller-disconnect :hcd ,hcd))
     ,@body))

;;======================================================================
;;======================================================================

(defvar *trace* 0)

(defmacro with-trace-level ((trace-level) &body body)
  `(when (>= *trace* ,trace-level)
     ,@body))

;;======================================================================
;;======================================================================

(defclass usbd ()
  (;; These fields must be initialized by the HCD via initarg because
   ;; they are used by the method initialize-instance :after below
   (%num-ports             :initarg :num-ports  :accessor num-ports)

   ;; These fields must be initialized by the HCD
   (%pci-device            :initarg :pci-device :accessor pci-device)
   (%pci-irq               :initarg :pci-irq    :accessor pci-irq)
   (%interrupt-thread                           :accessor interrupt-thread)
   (%lock                  :initarg :lock       :accessor usbd-lock)
   (%buf-pool              :initarg :buf-pool   :accessor buf-pool)

   ;; These fields are initialized in the method initialize-instance below
   (%port->device                               :accessor port->device)
   (%interrupt-event-pool                       :accessor interrupt-event-pool)

   ;; These fields are initialized by the initform
   (%next-device-address   :initform 1          :accessor next-device-address)
   (%free-addresses        :initform NIL        :accessor free-addresses)))

(defmethod initialize-instance :after ((usbd usbd) &key &allow-other-keys)
  (setf (port->device usbd) (make-array (num-ports usbd) :initial-element nil)
        (interrupt-event-pool usbd) (init-interrupt-event-pool usbd))
  ;; Ensure thread pool exists
  (create-thread-pool))

(defgeneric delete-controller (usbd))

(defmethod delete-controller :after ((usbd usbd))
  ;; Tell drivers that the device is gone
  (loop for device across (port->device usbd)
     when device do
       (dolist (driver (usb-device-drivers device))
         (delete-device driver device)))

  ;; Delete the buffer pool
  (delete-buffer-pool (buf-pool usbd)))

;;======================================================================
;;======================================================================

(defun alloc-device-address (usbd)
  (sup:with-mutex ((usbd-lock usbd))
    (if (free-addresses usbd)
        (pop (free-addresses usbd))
        (prog1
            (next-device-address usbd)
          (incf (next-device-address usbd))))))

(defun free-device-address (usbd address)
  (sup:with-mutex ((usbd-lock usbd))
    (if (member address (free-addresses usbd))
        (sup:debug-print-line "Attempt to free already free device address "
                              address
                              ". Free operation ignored.")
        (push address (free-addresses usbd)))))

;;======================================================================
;;======================================================================

(defclass usb-device ()
  (;; These fields must be initialized by the HCD via initarg because
   ;; they are used by the method initialize-instance :after below
   (%hcd            :initarg  :hcd         :accessor usb-device-hcd)
   (%port-num       :initarg  :port-num    :accessor usb-device-port-num)

   ;; These fields are initialized in the method below
   ;; Lock for device - used to serialize creation and teardown of the
   ;; device
   (%endpoints     :initarg  :endpoints    :accessor usb-device-endpoints)
   (%%lock         :initarg  :lock         :accessor usb-device-lock)

   ;; These fields are initialized during device enumerateion
   (%drivers        :initform NIL          :accessor usb-device-drivers)
   (%max-packet                            :accessor usb-device-max-packet)
   (%dev-desc-size                         :accessor usb-device-desc-size)
   (%device-address :initform NIL          :accessor usb-device-address)
   ;; These slots are for informational/debug purposes and are
   ;; otherwise unused
   (vendor-id  :initarg :vendor-id)
   (product-id :initarg :product-id)
   (class      :initarg :class)
   (subclass   :initarg :subclass)
   ;; for debug
   (%configuration  :initform NIL          :accessor usb-device-configuration)))

(defmethod initialize-instance :after ((device usb-device)
                                       &key &allow-other-keys)
  (let ((usbd (usb-device-hcd device)))
    (setf (usb-device-lock device) (sup:make-mutex "USB Device lock"))
    ;; lock device so that enumeration/driver acceptance can occur before
    ;; any operations or disconnect can occur.
    (sup:acquire-mutex (usb-device-lock device))

    (setf (usb-device-endpoints device) (make-array 32 :initial-element nil)
          (aref (port->device usbd) (usb-device-port-num device)) device)))

;;=========================
;; Creates an HCD device and returns the device
;;=========================

(defgeneric create-device (hcd port-num)
  (:method (hcd port-num)
    (error
     "create-device not defined for ~A. Error signaled, but ignored."
     hcd)))

(defgeneric delete-device (hcd/driver device)
  (:method (hcd/driver device)
    (error
     "delete-device not defined for ~A. Error signaled, but ignored."
     hcd/driver)))

(defmethod delete-device :after ((usbd usbd) device)
  (let ((drivers (usb-device-drivers device))
        (device-address (usb-device-address device)))
    (setf (aref (port->device usbd) (usb-device-port-num device)) nil)
    (when device-address
      (free-device-address usbd device-address))
    ;; Tell drivers that the device has disconnected
    (dolist (driver drivers)
      (delete-device driver device))))

;;======================================================================
;;
;; Interrupt event structure - This structure and the related handler,
;; handler-interrupt-event is only used for passing interrupt events
;; between the interrupt thread and the pool worker threads.
;;
;; A pool of these structures is preallocated so that the interrupt
;; thread does not need to create a structure while in interrupt
;; context.
;;
;;======================================================================

(defstruct interrupt-event
  hcd
  type
  port-num
  phys-addr
  ;; TBD - fields to be added as required for interrupt context
  )

;;=========================
;; Interrupt event pool functions
;;=========================

(defun init-interrupt-event-pool (usbd)
  (let ((queue (sup:make-fifo 8)))
    (dotimes (i 5)
      (sup:fifo-push (make-interrupt-event :hcd usbd :type :free) queue))
    queue))

(defun alloc-interrupt-event (usbd)
  (let ((event (sup:fifo-pop (interrupt-event-pool usbd) nil)))
    (when (null event)
      (error "No free interrupt events ~A" usbd))
    (setf (interrupt-event-type event) :undefined
          (interrupt-event-port-num event) -1
          (interrupt-event-phys-addr event) 0)
    event))

(defun free-interrupt-event (usbd event)
  (when (eq (interrupt-event-type event) :free)
    (error "Attempt to free an event that is already free ~A" event))
  (setf (interrupt-event-type event) :free)
  (when (not (sup:fifo-push event (interrupt-event-pool usbd) nil))
    (error "Unable to free interrupt event - no space in pool ~A" usbd)))

;;=========================
;; USBD interrupt event handlers
;;=========================

(defgeneric handle-interrupt-event (type usbd event)
  (:method (type usbd event)
    (error "USBD interrupt event ~A not handled. Error signaled, but ignored."
           type)))

(defmethod handle-interrupt-event ((type (eql :frame-rollover)) usbd event)
  (declare (ignore type usbd event))
  (sup:debug-print-line "USBD Interrupt Event: frame-rollover - ignored"))

(defmethod handle-interrupt-event ((type (eql :port-connect)) usbd event)
  ;; Disconnect existing device on the port, if any
  (let ((device (aref (port->device usbd) (interrupt-event-port-num event))))
    (when device
      (sup:with-mutex ((usb-device-lock device))
        (delete-device usbd device))))

  ;; Enumerate Device
  (handler-case
      (let ((port-num (interrupt-event-port-num event))
            (device))
        (debounce-port usbd port-num)
        (reset-port usbd port-num)

        (unwind-protect
             (progn
               ;; The device is created leaving the device mutex held
               (setf device (create-device usbd port-num))

               ;; get device descriptor size and max packet size
               (with-buffers ((buf-pool usbd) (buf /8 64))
                 (let ((num-bytes (get-descriptor usbd device
                                                  +desc-type-device+ 0
                                                  64 buf)))
                   (when (< num-bytes +dd-max-packet-size+)
                     (error "get-descriptor returned too few bytes ~D"
                            num-bytes))

                   (setf (usb-device-desc-size device)
                         (aref buf +dd-length+)
                         (usb-device-max-packet device)
                         (aref buf +dd-max-packet-size+))))

               (reset-port usbd port-num)
               (setf (usb-device-address device) (alloc-device-address usbd))
               (set-device-address usbd device (usb-device-address device))

               (let ((desc-length (usb-device-desc-size device)))
                 (with-buffers ((buf-pool usbd) (buf /8 desc-length))
                   (let ((num-bytes (get-descriptor usbd device
                                                    +desc-type-device+ 0
                                                    desc-length
                                                    buf)))

                     (when (/= num-bytes desc-length)
                       (error "get-descriptor failed expected ~D bytes got ~D"
                              desc-length num-bytes))

                     (sup:debug-print-line "Enumeration complete - success")
                     (if (probe-usb-driver usbd device buf)
                         (sup:debug-print-line "Driver accepted device")
                         (sup:debug-print-line "No driver found"))))))

          (when (and device
                     (sup:mutex-held-p (usb-device-lock device)))
            (sup:release-mutex (usb-device-lock device)))))
    ;; TODO - add error handling
    ))

(defmethod handle-interrupt-event ((type (eql :port-disconnect)) usbd event)
  (handler-case
      (let* ((port-num (interrupt-event-port-num event))
             (device (aref (port->device usbd) port-num)))
        (unwind-protect
             (when device
               (sup:with-mutex ((usb-device-lock device))
                 (delete-device usbd device))))
        (sup:debug-print-line "Disconnect complete - success"))
    ;; TODO - add error handling
    ))

;;======================================================================
;;
;; USB event structure - This structure is used to pass events from
;; one worker thread to another. The plist field is used to
;; communicate event specific information.
;;
;; For example, when an interrupt buffer arrives, the HCD will send
;; the buffer to the appropriate driver using a usb event. In this
;; case, usb-event-type was specified by the driver when the interrupt
;; endpoint was created; the destination is the driver object, the
;; data buffer is provided using :buf as an indicator in the plist,
;; and the number of bytes in the buffer is provided using :num-bytes
;; as an indicator in the plist.
;;
;;======================================================================

(defstruct usb-event
  type
  dest
  device
  ;; TBD - common general fields to be added as required
  plist)

(defun usb-event-plist-value (event indicator)
  (getf (usb-event-plist event) indicator))

(defun (setf usb-event-plist-value) (value event indicator)
  (setf (getf (usb-event-plist event) indicator) value))

;;=========================
;; USBD usb event handlers
;;=========================

(defgeneric handle-usb-event (type dest event)
  (:method (type dest event)
    (error "USBD usb event ~A not handled, Error signaled, but ignored." type)))

;;======================================================================
;; Code for USB device driver registration
;;
;; classes: list of (class subclass) pairs
;; products: list of (<vendor id> <product id>) pairs
;;
;;======================================================================

(defvar *drivers-lock* (sup:make-mutex "USB Drivers lock"))
(defvar *vendor-drivers* NIL)
(defvar *class-drivers* NIL)

(defstruct driver
  name
  probe
  products
  classes)

(defun define-usb-driver (name probe-function products classes)
  (let ((driver (make-driver :name name
                             :probe probe-function
                             :products products
                             :classes classes)))
    (sup:with-mutex (*drivers-lock*)
      (pushnew driver *vendor-drivers* :test #'equalp))))

(defun define-usb-class-driver (name probe-function classes)
  (let ((driver (make-driver :name name
                             :probe probe-function
                             :products nil
                             :classes classes)))
    (sup::with-mutex (*drivers-lock*)
      (pushnew driver *class-drivers* :test #'equalp ))))

(defun find-usb-driver (vendor-id product-id class subclass)
  ;; give vendor/product drivers priority over class/subclass drivers
  (sup:with-mutex (*drivers-lock*)
    (loop for driver in *vendor-drivers*
       do (loop for (driver-vendor driver-product) in (driver-products driver)
             when (and (= vendor-id driver-vendor)
                       (= product-id driver-product)) do
               (return-from find-usb-driver driver)))
    (loop for driver in *vendor-drivers*
       do (loop for (driver-class driver-subclass) in (driver-classes driver)
             when (and (= class driver-class)
                       (= subclass driver-subclass)) do
               (return-from find-usb-driver driver)))))

(defun find-usb-class-driver (class subclass protocol)
  (sup:with-mutex (*drivers-lock*)
    (loop for driver in *class-drivers*
       do (loop for (driver-class driver-subclass driver-protocol) in
               (driver-classes driver)
             when (and (= class driver-class)
                       (= subclass driver-subclass)
                       (= protocol driver-protocol)) do
               (return-from find-usb-class-driver driver)))))

(defun probe-usb-driver (usbd device buf)
  (let* ((vendor-id (dpb (aref buf +dd-vendor-id-high+)
                         (byte 8 8)
                         (aref buf +dd-vendor-id-low+)))
         (product-id (dpb (aref buf +dd-product-id-high+)
                          (byte 8 8)
                          (aref buf +dd-product-id-low+)))
         (class (aref buf +dd-device-class+))
         (subclass (aref buf +dd-device-sub-class+))
         (driver (find-usb-driver vendor-id product-id class subclass)))

    (setf (slot-value device 'vendor-id) vendor-id
          (slot-value device 'product-id) product-id
          (slot-value device 'class) class
          (slot-value device 'subclass) subclass)

    (sup:debug-print-line "vendor id " vendor-id " product id " product-id)

    (when driver
      (let ((probe-result (funcall (driver-probe driver) usbd device)))
        (when probe-result
          (push probe-result (usb-device-drivers device))
          (return-from probe-usb-driver T))))

    ;; Either there was no driver, or the device specific driver didn't
    ;; accept the device. Try the class drivers.
    (catch :probe-failed
      (let ((configuration (get-configuration usbd device)))
        (setf (usb-device-configuration device) configuration)
        (do ((desc (car configuration) (car configs))
             (configs (cdr configuration) (cdr configs))
             (config-set-p NIL))
            ((null desc))

          ;; Some devices seem to require that the configuration be
          ;; set before getting the report descriptor, so set the
          ;; configuration here. But, then what to do if there are
          ;; multiple configurations? Error for now.
          (when (= (aref desc +dd-type+) +desc-type-configuration+)
            (when config-set-p
              (sup:debug-print-line
               "Probe failed because "
               "multiple configuration descriptors are not supported")
              (delete-device usbd device)
              (return-from probe-usb-driver NIL))
            (setf config-set-p T)
            (set-configuration usbd device (aref desc +cd-config-value+)))

          (when (= (aref desc +dd-type+) +desc-type-interface+)
            (let ((driver (find-usb-class-driver (aref desc +id-class+)
                                                 (aref desc +id-sub-class+)
                                                 (aref desc +id-protocol+))))
              (when driver
                (multiple-value-bind (remaining-configs probe-result)
                    (funcall (driver-probe driver) usbd device desc configs)
                  (when probe-result
                    (push probe-result (usb-device-drivers device))
                    (setf configs remaining-configs))))))))

      ;; Loop exited normally
      (cond ((null (usb-device-drivers device))
             ;; No driver accepted the device, delete it.
             (delete-device usbd device)
             (return-from probe-usb-driver NIL))
            (T
             ;; One or more drivers accepted the device, keep it
             (return-from probe-usb-driver T))))

    ;; Loop exited via the catch - probe failed
    (delete-device usbd device)
    NIL))

(defun %get-configuration (usbd device length buf)
  (let ((num-bytes (get-descriptor usbd device
                                   +desc-type-configuration+ 0
                                   length buf)))
    (when (/= num-bytes length)
      ;; Unable to get configuration descriptor
      (sup:debug-print-line "HID Probe failed because "
                            "unable to get config descriptor, only got "
                            num-bytes
                            " bytes instead of "
                            length
                            ".")
      (throw :probe-failed nil))))

(defun get-configuration (usbd device)
  (with-buffers ((buf-pool usbd) (buf /8 9))
    ;; Get first configuration descriptor - need full descriptor length
    (%get-configuration usbd device 9 buf)

    (let ((length (aref buf 2)))
      (with-buffers ((buf-pool usbd) (config-buf /8 length))
        ;; Get full descriptor
        (%get-configuration usbd device length config-buf)

        (with-trace-level (3)
          (print-descriptor mezzano.internals::*cold-stream* config-buf))

        ;; split configuration descriptor into separate descriptors
        (do* ((offset 0 (+ offset (aref config-buf offset)))
              (result nil))
             ((>= offset length) (nreverse result))
          (let ((size (aref config-buf offset)))
            (when (= size 0)
              (sup:debug-print-line "HID Probe failed because "
                                    "of an invalid descriptor with size = 0.")
              (throw :probe-failed nil))
            (push (loop
                     for idx from 0 to (1- size)
                     with desc = (make-array size)
                     do (setf (aref desc idx)
                              (aref config-buf (+ offset idx)))
                     finally (return desc)) result)))))))

;;======================================================================
;;
;;======================================================================

(defgeneric debounce-port (hcd port-num)
  (:method (hcd port-num)
    (error "debounce-port not defined for ~A. Error signaled, but ignored."
           hcd)))

(defgeneric reset-port (hcd port-num)
  (:method (hcd port-num)
    (error "reset-port not defined for ~A. Error signaled, but ignored." hcd)))

(defgeneric set-device-address (hcd device address)
  (:method (hcd device address)
    (error
     "set-device-address not defined for ~A. Error ignored." hcd)))

(defgeneric create-interrupt-endpt
    (hcd device driver endpt-num num-bufs buf-size event-type interval)
  (:method (hcd device driver endpt-num num-bufs buf-size event-type interval)
     (error "create-interrupt-endpt not defined for ~A. Error ignored."
     hcd)))

(defgeneric delete-interrupt-endpt (hcd device endpt-num)
  (:method (hcd device endpt-num)
     (error "delete-interrupt-endpt not defined for ~A. Error ignored."
     hcd)))

(defgeneric create-bulk-endpt
    (hcd device driver endpt-num in-p event-type)
  (:method (hcd device driver endpt-num in-p event-type)
    (error "create-bulk-endpt not defined for ~A. Error ignored" hcd)))

(defgeneric delete-bulk-endpt (hcd device endpt-num)
  (:method (hcd device endpt-num)
    (error "delete-bulk-endpt not defined for ~A. Error ignored" hcd)))

(defgeneric bulk-enqueue-buf (hcd device endpt-num buf num-bytes)
  (:method (hcd device endpt-num buf num-bytes)
    (error "bulk-enqueue-buf not defined for ~A. Error ignored" hcd)))

(defgeneric bulk-dequeue-buf (hcd device endpt-num buf)
  (:method (hcd device endpt=num buf)
    (error "bulk-dequeue-buf not defined for ~A. Error ignored" hcd)))

(defgeneric create-isochronous-endpt
    (hcd device driver endpt-num in-p event-type)
  (:method (hcd device driver endpt-num in-p event-type)
    (error "create-isochronous-endpt not defined for ~A. Error ignored" hcd)))

(defgeneric delete-isochronous-endpt (hcd device endpt-num)
  (:method (hcd device endpt-num)
    (error "delete-isochronous-endpt not defined for ~A. Error ignored" hcd)))
