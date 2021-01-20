;;;; Copyright (c) 2019, 2020 Philip Mueller (phil.mueller@fittestbits.com)
;;;; This code is licensed under the MIT license.

(in-package :mezzano.driver.usb.hid)

;;======================================================================
;; HID Keyboard Driver
;;======================================================================

(defun generate-keyboard-buf-code (state)
  ;; TODO create generate-keyboard-buf-code
  (values nil nil))


(defun probe-hid-keyboard (usbd device configs state)
  (let ((endpoint (make-hid-endpt :type :keyboard)))
    (let ((endpt-desc (pop configs)))
      (when (or (null endpt-desc)
                (/= (aref endpt-desc +ed-type+) +desc-type-endpoint+))
        (sup:debug-print-line "HID probe failed because "
                              "found descriptor type "
                              (aref endpt-desc +ed-type+)
                              " instead of endpoint descriptor.")
        (throw :probe-failed :failed))
      ;; TODO write keyboard-int-callback
      #+nil
      (let ((endpt-num (parse-endpt-descriptor usbd
                                               endpoint
                                               device
                                               endpt-desc
                                               'keyboard-int-callback)))
        (setf (aref (hid-driver-endpoints driver) endpt-num) endpoint))))
  ;; Return NIL - this driver not implemented
  (values configs nil))

(register-hid-device-function :keyboard 'probe-hid-keyboard)
