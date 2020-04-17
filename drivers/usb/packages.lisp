;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.driver.usb
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:pci :mezzano.supervisor.pci)
                    (:sync :mezzano.sync))
  (:export
   ;; USB driver (USBD) Interface functions for device drivers
   :define-usb-driver
   :define-usb-class-driver

   ;; USB Driver/HCD validation and teardown interface
   :with-hcd-access
   :controller-disconnect
   :disconnect-hcd

   ;; USBD Class
   :usbd
   :delete-controller
   :num-ports
   :pci-device
   :pci-irq
   :interrupt-thread
   :usbd-lock
   :pci-device
   :buf-pool
   :port->device

   ;; USBD Device Class
   :usb-device
   :usb-device-endpoints
   :usb-device-drivers
   :usb-device-max-packet
   :usb-device-desc-size

   :create-device
   :delete-device

   ;; USB Driver (USBD) Interface functions for HALs/HCDs
   :alloc-interrupt-event
   :free-interrupt-event

   ;; HAL/HCD Interface (generic) functions
   :get-buffer-memory

   :debounce-port
   :reset-port
   :set-device-address

   :create-interrupt-endpt
   :delete-interrupt-endpt
   :create-bulk-endpt
   :delete-bulk-endpt
   :bulk-enqueue-buf
   :bulk-dequeue-buf
   :create-isochronous-endpt
   :delete-isochronous-endpt

   ;; USB Driver interrupt event structure
   :interrupt-event
   :interrupt-event-hcd
   :interrupt-event-type
   :interrupt-event-port-num
   :interrupt-event-phys-addr
   :handle-interrupt-event

   ;; USB Driver usb event structure
   :usb-event
   :make-usb-event
   :usb-event-type
   :usb-event-dest
   :usb-event-device
   :usb-event-plist-value
   :handle-usb-event

   ;; Buffer management interface - provided by buffers.lisp
   :create-buffer-pool
   :delete-buffer-pool
   :alloc-buffer/8
   :alloc-buffer/16
   :alloc-buffer/32
   :free-buffer
   :array->phys-addr
   :phys-addr->array
   :with-buffers
   :/8
   :/16
   :/32

   ;; Thread pool interface - provided by threads.lisp
   :create-thread-pool
   :enqueue-event

   ;; USB Debug routines
   :pci-print/4
   :pci-print/2
   :pci-print/1
   :global-print/32
   :global-print/16
   :global-print/8
   :global-print/4
   :global-print/2
   :global-print/1
   :increase-indent
   :print-buffer
   :print-descriptor
   ))

(defpackage :mezzano.driver.usb.buffers
  (:use :cl :mezzano.driver.usb)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sys.int :mezzano.internals)))

(defpackage :mezzano.driver.usb.threads
  (:use :cl :mezzano.driver.usb)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sync :mezzano.sync)))

(defpackage :mezzano.driver.usb.ohci
  (:use :cl :mezzano.driver.usb)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:pci :mezzano.supervisor.pci)
                    (:sync :mezzano.sync)
                    (:sys.int :mezzano.internals)))

(defpackage :mezzano.driver.usb.ehci.intel
  (:use :cl :mezzano.driver.usb)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:pci :mezzano.supervisor.pci)
                    (:sync :mezzano.sync)
                    (:sys.int :mezzano.internals)))
