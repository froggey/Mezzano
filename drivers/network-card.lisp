;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The interface that all network card drivers must conform to.

(defpackage :mezzano.driver.network-card
  (:use :cl)
  (:import-from :mezzano.supervisor
                #:debug-print-line)
  (:export #:network-card
           #:register-network-card
           #:mac-address
           #:statistics
           #:mtu
           #:receive-packet
           #:transmit-packet
           #:device-transmit-packet
           #:device-received-packet))

(in-package :mezzano.driver.network-card)

(sys.int::defglobal *nics* '())
(sys.int::defglobal *received-packets* (mezzano.supervisor:make-fifo 50))

(defclass network-card () ())

(defmethod print-object ((object network-card) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~/mezzano.network.ethernet:format-mac-address/"
            (mac-address object))))

(defun register-network-card (device)
  (debug-print-line "Registered NIC " device " with MAC " (mac-address device))
  (push device *nics*))

(defgeneric mac-address (nic))
(defgeneric statistics (nic))
(defgeneric mtu (nic))

(defun transmit-packet (nic packet)
  (mezzano.supervisor::set-network-light t)
  (mezzano.supervisor::set-network-light nil)
  (device-transmit-packet nic packet))

(defun receive-packet ()
  "Wait for a packet to arrive.
Returns two values, the packet data and the receiving NIC."
  (let ((info (mezzano.supervisor:fifo-pop *received-packets*)))
    (mezzano.supervisor::set-network-light t)
    (mezzano.supervisor::set-network-light nil)
    (values (cdr info) (car info))))

(defgeneric device-transmit-packet (nic packet))

(defun device-received-packet (nic packet)
  (mezzano.supervisor:fifo-push (cons nic packet) *received-packets*))

(defun boot-hook ()
  ;; TODO: Clear out old network cards.
  ;; Can't do this in an early boot-hook as device detection happens earlier.
  (mezzano.supervisor:fifo-reset *received-packets*))
(mezzano.supervisor:add-boot-hook 'boot-hook :early)
