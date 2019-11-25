(defpackage :mezzano.driver.usb.logitech
  (:use :cl :mezzano.driver.usb)
  (:local-nicknames (:sup :mezzano.supervisor)))

(in-package :mezzano.driver.usb.logitech)

;;======================================================================
;; Hard coded device driver for TrackMan Wheel pointing device (mouse)
;; which has the following descriptors:
;;
;; Configuration Descriptor
;;   Length: 34, Interfaces: 1, Config Number: 1, String Index: 0,
;;   Remote Wakeup, Power: 100
;;
;; Interface Descriptor
;;   Iface Number: 0, Alternate: 0, Num Endpts: 1, Class: 3, SubClass: 1.
;;   Protocol: 2, String Index: 0
;;
;; HID Descriptor
;;   HID Rel: 1.10, Country Code: 0, Num Descriptors: 1, Type: 34,
;;   Report size: 103
;;
;; End Point Descriptor
;;   Address: 1, In, Interrupt, Max Packet: 8, Interval: 10
;;
;; Summary of report descriptor:
;;     byte 0:
;;         bit 0: button 1
;;         bit 1: button 2
;;         bit 2: button 3
;;         bits 3-7: Unused
;;     byte 1:
;;         bits 0-7: x (-127 -> 127)
;;     byte 2:
;;         bits 0-7: y (-127 -> 127)
;;     byte 3:
;;         bits 0-7: wheel (-127 -> 127)
;;
;;======================================================================

(defstruct trackman-wheel
  usbd
  device
  submit-mouse)

(define-usb-driver
    "Logitech TrackMan Wheel"
    'probe-trackman '((#x046D #xC404)) '())

(defun probe-trackman (usbd device)
  ;; Configuration number from configuration descriptor
  (set-configuration usbd device 1)

  (let ((driver (make-trackman-wheel
                 :usbd usbd
                 :device device
                 :submit-mouse (intern "SUBMIT-MOUSE"
                                       :mezzano.gui.compositor))))

    (create-interrupt-endpt usbd
                            device
                            driver
                            1             ; from endpoint descriptor
                            3             ; should be enough?
                            4             ; interrupt report message size
                            'mouse-int-callback
                            10)           ; from endpoint descriptor
    driver))

(declaim (inline adjust-motion))

(defconstant +threshold+ 50)
(defconstant +multiplier+ 3)

(defun adjust-motion (motion)
  (cond ((>= motion +threshold+)
         (- (* +multiplier+ motion) (* (1- +multiplier+) +threshold+)))
        ((>= motion (- +threshold+))
         motion)
        (T
         (+ (* +multiplier+ motion) (* (1- +multiplier+) +threshold+)))))

(defun mouse-int-callback (driver endpoint-num status length buf)
  (declare (ignore length))
  (unwind-protect
       (cond ((eq status :success)
              (let ((buttons (logand (aref buf 0) #b111))
                    (x-motion (if (logbitp 7 (aref buf 1))
                                  (- (1+ (logxor #xFF (aref buf 1))))
                                  (aref buf 1)))
                    (y-motion (if (logbitp 7 (aref buf 2))
                                  (- (1+ (logxor #xFF (aref buf 2))))
                                  (aref buf 2)))
                    (wheel (aref buf 3)))
                (funcall (trackman-wheel-submit-mouse driver)
                         (logior buttons
                                 (if (> #x80 wheel 0)
                                     #b00001000
                                     0)
                                 (if (>= wheel #x80)
                                     #b00010000
                                     0))
                         (adjust-motion x-motion)
                         (adjust-motion y-motion))
                (when (/= wheel 0)
                  ;; generate button up for buttons 4 or 5
                  (funcall (trackman-wheel-submit-mouse driver)
                           buttons
                           (adjust-motion x-motion)
                           (adjust-motion y-motion)))))
             (T
              (format mezzano.internals::*cold-stream*
                      "Interrupt error ~A on endpoint number ~D~%"
                      status endpoint-num)))
    #+nil (push (format nil "~D: ~A" length buf) *ints*)
    (free-buffer buf)))

(defmethod delete-device ((driver trackman-wheel) device)
  ;; Device has disconnected - clean up any resources allocated here
  ;; Nothing to do here?
  )
