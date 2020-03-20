;;;; Copyright (c) 2019 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

;;======================================================================
;;
;; This file contains global USB definitions. That is definitions that
;; are not specific to a host controller or USB device.
;;
;; For example, this file includes things like offsets in a device
;; descriptor buffer but not things like bit field definitions for
;; host controller registers.
;;
;; All of the symbols defined here are (should be) external to the
;; mezzano.driver.usb package so that they are accessible from HCDs
;; and device drivers.
;;
;;======================================================================

(in-package :mezzano.driver.usb)

;;======================================================================
;; Support macros that define and export these symbols
;;======================================================================

(defmacro define-constant (name value)
  `(progn
     (export ',name)
     (defconstant ,name ,value)))

(defmacro define-function (name lambda-list &body body)
  `(progn
     (export ',name)
     (defun ,name ,lambda-list ,@body)))

(defmacro define-generic (name lambda-list &body body)
  `(progn
     (export ',name)
     (defgeneric ,name ,lambda-list ,@body)))

(defmacro define-macro (name lambda-list &body body)
  `(progn
     (export ',name)
     (defmacro ,name ,lambda-list ,@body)))

(defmacro define-structure (name &body fields)
  ;; simplistic macro for defining a structure
  (when (or (not (symbolp name))
            (find-if #'(lambda (field) (not (symbolp field))) fields))
    (error  "define-structure not implemented for ~A definition" name))
  (let* ((name-string (symbol-name name))
         (make (intern (concatenate 'string "MAKE" "-" name-string)))
         (copy (intern (concatenate 'string "COPY" "-" name-string)))
         (predicate (intern (concatenate 'string name-string "-P"))))
  `(progn
     (export ',name)
     (export ',make)
     (export ',copy)
     (export ',predicate)
     ,@(mapcar #'(lambda (field)
                   (let ((accessor (intern (concatenate 'string
                                                        name-string
                                                        "-"
                                                        (symbol-name field)))))
                     `(export ',accessor)))
               fields)
     (defstruct ,name ,@fields))))

;;======================================================================
;; Standard Reequest Type from USB Spec 1.1 Sec 9.4
;;======================================================================

(define-constant +rt-dir-host-to-device+ 0)
(define-constant +rt-dir-device-to-host+ 1)

(define-constant +rt-type-standard+ 0)
(define-constant +rt-type-class+    1)
(define-constant +rt-type-vendor+   2)
(define-constant +rt-type-reseverd+ 3)

(define-constant +rt-rec-device+    0)
(define-constant +rt-rec-interface+ 1)
(define-constant +rt-rec-endpoint+  2)
(define-constant +rt-rec-other+     3)
;; 4-31 reserved

(define-function encode-request-type (dir type recipient)
  (logior (dpb dir  (byte 1 7) 0)
          (dpb type (byte 2 5) 0)
          recipient))

;;======================================================================
;; Standard Reequest Codes from USB Spec 1.1 Sec 9.4
;;======================================================================

(define-constant +dev-req-get-status+         0)
(define-constant +dev-req-clear-feature+      1)
(define-constant +dev-req-reserved-2+         2)
(define-constant +dev-req-set-feature+        3)
(define-constant +dev-req-reserved-4+         4)
(define-constant +dev-req-set-address+        5)
(define-constant +dev-req-get-descriptor+     6)
(define-constant +dev-req-set-descriptor+     7)
(define-constant +dev-req-get-configuration+  8)
(define-constant +dev-req-set-configuration+  9)
(define-constant +dev-req-get-interface+     10)
(define-constant +dev-req-set-interface+     12)

;;======================================================================
;; Standard Feature Selectors from USB Spec 1.1 Sec 9.6
;;======================================================================

(define-constant +feature-device-remote-wakeup+    1)

(define-constant +feature-endpoint-halt+           0)

;;======================================================================
;; Function for encoding a standard request
;;======================================================================

(define-function encode-request (buf request-type request value index length)
  (setf (aref buf 0) request-type
        (aref buf 1) request
        (aref buf 2) (ldb (byte 8 0) value)
        (aref buf 3) (ldb (byte 8 8) value)
        (aref buf 4) (ldb (byte 8 0) index)
        (aref buf 5) (ldb (byte 8 8) index)
        (aref buf 6) (ldb (byte 8 0) length)
        (aref buf 7) (ldb (byte 8 8) length)))

;;======================================================================
;; Standard Descriptor Types from USB Spec 1.1 Sec 9.4 and USB Spec
;; 2.0 table 9-5
;;======================================================================

;; (byte 2 5) = 0 => standard descriptor
(define-constant +desc-type-device+                    1)
(define-constant +desc-type-configuration+             2)
(define-constant +desc-type-string+                    3)
(define-constant +desc-type-interface+                 4)
(define-constant +desc-type-endpoint+                  5)
(define-constant +desc-type-device-qualifier+          6)
(define-constant +desc-type-other-speed-configuration+ 7)
(define-constant +desc-type-interface-power+           8)

;; (byte 2 5) = 1 => class descriptor
(define-constant +desc-type-hid+                    #x21)
(define-constant +desc-type-report+                 #x22)
(define-constant +desc-type-physical+               #x23)

;; (byte 2 5) = 2 => Vendor descriptor

;; (byte 2 5) = 3 => Reserved

;;======================================================================
;; Standard Device Descriptor byte offsets from USB Spec 1.1 Sec 9.4
;;======================================================================

(define-constant +dd-length+               0)
(define-constant +dd-type+                 1)
(define-constant +dd-usb-low+              2)
(define-constant +dd-usb-high+             3)
(define-constant +dd-device-class+         4)
(define-constant +dd-device-sub-class+     5)
(define-constant +dd-device-protocol+      6)
(define-constant +dd-max-packet-size+      7)
(define-constant +dd-vendor-id-low+        8)
(define-constant +dd-vendor-id-high+       9)
(define-constant +dd-product-id-low+      10)
(define-constant +dd-product-id-high+     11)
(define-constant +dd-device-release-low+  12)
(define-constant +dd-device-release-high+ 13)
(define-constant +dd-manufacturer-idx+    14)
(define-constant +dd-product-idx+         15)
(define-constant +dd-serial-number-idx+   16)
(define-constant +dd-num-configurations+  17)

;;======================================================================
;; Standard Configuration Descriptor byte offsets from USB Spec 1.1 Sec 9.6
;;======================================================================

(define-constant +cd-length+               0)
(define-constant +cd-type+                 1)
(define-constant +cd-total-length+         2)
(define-constant +cd-num-interfaces+       4)
(define-constant +cd-config-value+         5)
(define-constant +cd-config-idx+           6)
(define-constant +cd-attributes+           7)
(define-constant +cd-max-power+            8)

;;======================================================================
;; Standard Interface Descriptor byte offsets from USB Spec 1.1 Sec 9.6
;;======================================================================

(define-constant +id-length+               0)
(define-constant +id-type+                 1)
(define-constant +id-number+               2)
(define-constant +id-alt-setting+          3)
(define-constant +id-num-endpoints+        4)
(define-constant +id-class+                5)
(define-constant +id-sub-class+            6)
(define-constant +id-protocol+             7)
(define-constant +id-interface-idx+        8)

(define-constant +id-class-hid+            3)
(define-constant +id-class-mass-storage+   8)

(define-constant +id-protocol-keyboard+    #x01)
(define-constant +id-protocol-mouse+       #x02)

(define-constant +id-protocol-bulk-only+   #x50)

;;======================================================================
;; Standard Endpoint Descriptor byte offsets from USB Spec 1.1 Sec 9.6
;;======================================================================

(define-constant +ed-length+               0)
(define-constant +ed-type+                 1)
(define-constant +ed-address+              2)
(define-constant +ed-attributes+           3)
(define-constant +ed-max-packet+           4)
(define-constant +ed-interval+             6)

(define-constant +ed-endpt-num-field+      (byte 4 0))
(define-constant +ed-direction-field+      (byte 1 7))

(define-constant +ed-attr-control+         0)
(define-constant +ed-attr-isochronous+     1)
(define-constant +ed-attr-bulk+            2)
(define-constant +ed-attr-interrupt+       3)

;;======================================================================
;; Standard String Descriptor byte offsets from USB Spec 1.1 Sec 9.6
;;======================================================================

;; For string index 0 - each Language ID takes 2 bytes

(define-constant +sd-length+               0)
(define-constant +sd-type+                 1)
(define-constant +sd-language-id-0+        2)

;; For string index /= 0 - unicode string

;; (define-constant +sd-length+               0)
;; (define-constant +sd-type+                 1)
(define-constant +sd-string+                2)

;;======================================================================
;; Definitions from Device Class Definition for Human Interface Devices (HID)
;;======================================================================

;;======================================================================
;; Standard HID Descriptor byte offsets from USB Spec 1.1 Sec 9.6
;;======================================================================

(define-constant +hd-length+                0)
(define-constant +hd-type+                  1)
(define-constant +hd-hid-release+           2)
(define-constant +hd-country-code+          4)
(define-constant +hd-num-descriptors+       5)
;; offsets for first desciptor
(define-constant +hd-descriptor-type+       6) ;; +desc-type-...+
(define-constant +hd-descriptor-length+     7)

;;======================================================================
;; HIB Subclass codes
;;======================================================================

(define-constant +hid-subclass-none+ 0)
(define-constant +hid-subclass-boot+ 1)
;; 2 - 255 Reserved

;;======================================================================
;; HIB Protocl codes - only valid for +hid-subclass-boot+ interfaces
;;======================================================================

(define-constant +hib-protocol-none+      0)
(define-constant +hib-protocol-keyboard+  1)
(define-constant +hib-protocol-mouse+     2)
;; 3 - 255 Reserved

;;======================================================================
;; HIB Class Requests
;;======================================================================

(define-constant +hib-class-req-get-report+    1)
(define-constant +hib-class-req-get-idle+      2)
(define-constant +hib-class-req-get-protocol+  3)
;; 4 - 8 reserved
(define-constant +hib-class-req-set-report+    9)
(define-constant +hib-class-req-set-idle+     10)
(define-constant +hib-class-req-set-protocol+ 11)

;;======================================================================
;;
;;======================================================================

(declaim (inline get-unsigned-word/16 get-unsigned-word/32))

(define-function get-unsigned-word/16 (buf offset)
  (dpb (aref buf (1+ offset)) (byte 8 8) (aref buf offset)))

(define-function (setf get-unsigned-word/16) (value buf offset)
  (setf (aref buf offset)       (ldb (byte 8  0) value)
        (aref buf (+ offset 1)) (ldb (byte 8  8) value)))

(define-function get-unsigned-word/32 (buf offset)
  (dpb (aref buf (+ offset 3))
       (byte 8 24)
       (dpb (aref buf (+ offset 2))
            (byte 8 16)
            (dpb (aref buf (+ offset 1))
                 (byte 8 8)
                 (aref buf offset)))))

(define-function (setf get-unsigned-word/32) (value buf offset)
  (setf (aref buf offset)       (ldb (byte 8  0) value)
        (aref buf (+ offset 1)) (ldb (byte 8  8) value)
        (aref buf (+ offset 2)) (ldb (byte 8 16) value)
        (aref buf (+ offset 3)) (ldb (byte 8 24) value)))

(define-function get-be-unsigned-word/16 (buf offset)
  (dpb (aref buf offset) (byte 8 8) (aref buf (1+ offset))))

(define-function (setf get-be-unsigned-word/16) (value buf offset)
  (setf (aref buf (+ offset 1)) (ldb (byte 8  0) value)
        (aref buf offset)       (ldb (byte 8  8) value)))

(define-function get-be-unsigned-word/32 (buf offset)
  (dpb (aref buf offset)
       (byte 8 24)
       (dpb (aref buf (+ offset 1))
            (byte 8 16)
            (dpb (aref buf (+ offset 2))
                 (byte 8 8)
                 (aref buf (+ offset 3))))))

(define-function (setf get-be-unsigned-word/32) (value buf offset)
  (setf (aref buf (+ offset 3)) (ldb (byte 8  0) value)
        (aref buf (+ offset 2)) (ldb (byte 8  8) value)
        (aref buf (+ offset 1)) (ldb (byte 8 16) value)
        (aref buf offset)       (ldb (byte 8 24) value)))

(define-function get-ascii-string (buf offset length)
  (let ((result (make-string length)))
    (dotimes (i length)
      (setf (char result i) (code-char (aref buf (+ offset i)))))
    result))

;;======================================================================
;;
;;======================================================================

(define-generic control-send-data
    (usbd device request-type request value index length buf))

(define-generic control-receive-data
    (usbd device request-type request value index length buf))

(define-generic bulk-send-data (usbd device length buf))

(define-generic bulk-receive-data (usbd device length buf))

;;======================================================================
;;
;;======================================================================

(declaim (inline get-descriptor))

(define-function get-descriptor (usbd device descriptor-type index length buf)
  (control-receive-data usbd
                        device
                        (encode-request-type  +rt-dir-device-to-host+
                                              +rt-type-standard+
                                              +rt-rec-device+)
                        +dev-req-get-descriptor+
                        (dpb descriptor-type
                             (byte 8 8)
                             index)
                        0 ;; ignoring language ID
                        length
                        buf))

;;======================================================================
;;
;;======================================================================

(declaim (inline set-configuration))

(define-function set-configuration (usbd device configuration)
  (let ((buf))
    (unwind-protect
         (progn
           ;; don't want 0 length buf
           (setf buf (alloc-buffer/8 (buf-pool usbd) 1))
           (control-receive-data usbd
                                 device
                                 (encode-request-type +rt-dir-host-to-device+
                                                      +rt-type-standard+
                                                      +rt-rec-device+)
                                 +dev-req-set-configuration+
                                 configuration
                                 0
                                 0
                                 buf))
      (when buf
        (free-buffer buf)))))


;;======================================================================
;;
;; xfer-info holds information used when a transfer is completed by
;; a controller.
;;
;; event-type:
;;    if keyword, generate a usb-event with this value as the type
;;    if semaphore, signal the semaphore
;;    otherwise, it must be a function that is called with:
;;        driver object
;;        endpoint-num
;;        transaction status
;;        actual length transfer
;;        data buffer
;;
;;======================================================================

(define-structure xfer-info
  event-type
  endpoint
  buf-size
  buf)

;;======================================================================
;; endpoint holds the information associated with an endpoint.
;;======================================================================

(define-structure endpoint
  type          ;; endpoint type (:control :interrupt :bulk :isochronous)
  device        ;; device associated with the endpoint
  driver        ;; driver associated with the device
  num           ;; endpoint number
  event-type)   ;; event type - used to initialize xfer-info
