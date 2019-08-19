;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The interface that all network card drivers must conform to.

(defpackage :mezzano.driver.network-card
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sync :mezzano.sync))
  (:export #:network-card
           #:register-network-card
           #:unregister-network-card
           #:mac-address
           #:statistics
           #:mtu
           #:transmit-packet
           #:receive-mailbox
           #:device-received-packet

           #:add-nic-hooks))

(in-package :mezzano.driver.network-card)

(sys.int::defglobal *nics-lock* (sup:make-mutex "*NICS* lock"))
(sys.int::defglobal *nics* '()
  "List of registered NICs.")
(sys.int::defglobal *nic-registration-hooks* '()
  "A list of hooks to call when a NIC is registered.")
(sys.int::defglobal *nic-unregistration-hooks* '()
  "A list of hooks to call when a NIC is unregistered.")
(sys.int::defglobal *nic-receive-buffer-capacity* 50
  "The number of recevied packets that can be buffered by a NIC by default.")

(sys.int::defglobal *netmangler-receive-drop-probability* nil
  "Probability that the network stack will drop a received packet.
Should be NIL to disable dropping or a (REAL 0 1) to enable, with higher values
corresponding to higher chances of a packet being dropped.")
(sys.int::defglobal *netmangler-receive-drop-count* 0)
(sys.int::defglobal *netmangler-transmit-drop-probability* nil
  "Probability that the network stack will drop a transmitted packet.")
(sys.int::defglobal *netmangler-transmit-drop-count* 0)
(declaim (type fixnum
               *netmangler-receive-drop-count*
               *netmangler-transmit-drop-count*)
         (type (or null (real 0 1))
               *netmangler-receive-drop-probability*
               *netmangler-transmit-drop-probability*))

(defclass network-card ()
  ((%received-packets :reader receive-mailbox))
  (:documentation "Base class of all network cards."))

(defmethod initialize-instance :after ((nic network-card) &key)
  (setf (slot-value nic '%received-packets)
        (sync:make-mailbox :name `(receive-mailbox ,nic)
                           :capacity *nic-receive-buffer-capacity*)))

(defmethod print-object ((object network-card) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~/mezzano.network.ethernet:format-mac-address/"
            (mac-address object))))

(defun register-network-card (device)
  "Register a new NIC."
  (check-type device network-card)
  (sup:debug-print-line "Registered NIC " device " with MAC " (mac-address device))
  (sup:with-mutex (*nics-lock*)
    (push device *nics*)
    (dolist (hook *nic-registration-hooks*)
      (funcall hook device))))

(defun unregister-network-card (device)
  "Unregister a NIC."
  (sup:debug-print-line "Unregistered NIC " device " with MAC " (mac-address device))
  (sup:with-mutex (*nics-lock*)
    (setf *nics* (remove device *nics*))
    (dolist (hook *nic-unregistration-hooks*)
      (funcall hook device))))

(defun add-nic-hooks (registration-hook unregistration-hook)
  "Add hooks to be called on NIC registration & unregistration.
The registration hook will immediately be called with all currently registered NICs."
  (check-type registration-hook sys.int::function-designator)
  (check-type unregistration-hook sys.int::function-designator)
  (sup:with-mutex (*nics-lock*)
    (push registration-hook *nic-registration-hooks*)
    (push unregistration-hook *nic-unregistration-hooks*)
    (dolist (nic *nics*)
      (funcall registration-hook nic))))

(defgeneric mac-address (nic)
  (:documentation "Return the MAC address of NIC."))
(defgeneric statistics (nic)
  (:documentation "Return various bits of information about NIC.
1st value: total number of octets received.
2nd value: total number of packets received.
3rd value: total number of receive errors.
4th value: total number of octets transmitted.
5th value: total number of packets transmitted.
6th value: total number of transmit errors.
7th value: total number of collisions detected."))
(defgeneric mtu (nic)
  (:documentation "Return the MTU of the NIC."))

(defgeneric transmit-packet (nic packet)
  (:documentation "Transmit a packet on NIC."))

(defmethod transmit-packet :around (nic packet)
  (let ((prob *netmangler-transmit-drop-probability*))
    (when (and prob (< (random 1.0) prob))
      (sys.int::atomic-incf *netmangler-transmit-drop-count*)
      (return-from transmit-packet)))
  (mezzano.supervisor::set-network-light t)
  (unwind-protect
       (call-next-method)
    (mezzano.supervisor::set-network-light nil)))

(defun device-received-packet (nic packet)
  "Helper function that should be called by NIC's driver when a packet is received."
  (let ((prob *netmangler-receive-drop-probability*))
    (when (and prob (< (random 1.0) prob))
      (sys.int::atomic-incf *netmangler-receive-drop-count*)
      (return-from device-received-packet)))
  (sync:mailbox-send packet (receive-mailbox nic)))
