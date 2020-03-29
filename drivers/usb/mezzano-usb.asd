;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

;;======================================================================

;; The USB driver has three layers: the middle layer, the USB Driver
;; (USBD) layer, contains the code that is common across host
;; controllers and devices; the bottom layer, the Hardware Abstraction
;; Layer (HAL) or Host Controller Driver (HCD) layer, contains code
;; that is specific to a host controller; and, the top layer consists
;; of drivers for specific devices or classes of devices.

;; The middle layer consists of services such as buffers and worker
;; threads and code that only depends on the USB specs. In addition,
;; it defines the interfaces that are provided by the bottom layer and
;; used by the top layer. In order to prevent the middle layer from
;; being a collection of pass through functions, serveral classes are
;; defined which are used as super classes by the bottom layer
;; classes. This allows the middle layer to provide common
;; functionality with lower overhead. The middle layer is defined by
;; the "mezzano-usb" system.

;; The bottom layer consists of drivers for each of the supported host
;; controlles. Curently (Aug. 12, 2019) only the Open Host Controller
;; Interface (OHCI) controller is supported. OHCI only supports USB
;; 1.1. Drivers for additional host controllers should be added over
;; time. In particular, there should be a driver for the Enhanced Host
;; Controller Interface (EHCI) which supports USB 2.0 and the
;; eXtensible Host Controller Interface (xHCI) which supports USB 3.

;; The top layer consists of vendor specific device drivers and class
;; device drivers. For example, logitech.lisp contains a hand coded
;; device specific driver for the Logitech Trackman Wheel pointing
;; device (mouse). It is not included in the system because that mouse,
;; along with several other mice, are handled by the Human Interface
;; Device (HID) driver. The HID driver is a class driver that (should)
;; handle all HIDs. Currently (Aug. 12, 2019), the HID driver only
;; supports mice.

;; Debug support is provided in separate files and consists mostly of
;; functions that print various data structures in human readable
;; format.

;;======================================================================

(in-package :asdf)

(defsystem "mezzano-usb"
  :description "USB Driver AKA USBD"
  :serial t
  :components ((:file "packages")
               (:file "usb-defs")
               (:file "buffers")
               (:file "threads")
               (:file "usb-driver")))

(defsystem "mezzano-usb/ohci"
  :description "OHCI Driver"
  :depends-on ("mezzano-usb")
  :serial t
  :components ((:file "ohci")))

(defsystem "mezzano-usb/ehci"
  :description "EHCI Drivers"
  :depends-on ("mezzano-usb")
  :components ((:file "ehci-intel")))

#+nil ;; currently no vendor specific drivers
(defsystem "mezzano-usb/vendor-drivers"
  :description "Vendor specific drivers"
  :depends-on ("mezzano-usb" "mezzano-usb/ohci")
  :components (#+nil (:file "logitech") ;; replaced by hid class driver
                     ))

(defsystem "mezzano-usb/class-drivers"
  :description "Class drivers"
  :depends-on ("mezzano-usb")
  :components ((:file "hid")
               (:file "mass-storage")))

(defsystem "mezzano-usb/debug"
  :description "Debug support for USB drivers"
  :depends-on ("mezzano-usb" "mezzano-usb/ohci" "mezzano-usb/ehci")
  :serial t
  :components ((:file "usb-debug")
               (:file "ohci-debug")
               (:file "ohci-test")
               (:file "ehci-intel-debug")))

(defsystem "mezzano-usb/hid/debug"
  :description "Debug and test support for HID driver"
  :depends-on ("mezzano-usb" "mezzano-usb/class-drivers" "mezzano-usb/debug")
  :components ((:file "hid-debug")
               (:file "hid-test")))

(defsystem "mezzano-usb/mass-storage/debug"
  :description "Debug support for mass storage driver"
  :depends-on ("mezzano-usb" "mezzano-usb/class-drivers" "mezzano-usb/debug")
  :components ((:file "mass-storage-debug")))
