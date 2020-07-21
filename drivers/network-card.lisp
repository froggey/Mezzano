;;;; The interface that all network card drivers must conform to.

(defpackage :mezzano.driver.network-card
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:sync :mezzano.sync)
                    (:sys.int :mezzano.internals))
  (:export #:network-card
           #:register-network-card
           #:unregister-network-card
           #:mac-address
           #:statistics
           #:mtu
           #:transmit-packet
           #:receive-mailbox
           #:device-received-packet

           #:add-nic-watchers))

(in-package :mezzano.driver.network-card)

(sys.int::defglobal *nics* (sync:make-watchable-set :name "NICs")
  "List of registered NICs.")
(sys.int::defglobal *nic-receive-buffer-capacity* 50
  "The number of recevied packets that can be buffered by a NIC by default.")

(sys.int::defglobal *netmangler-receive-duplicate* nil
  "If true, then received packets will be duplicated.")
(sys.int::defglobal *netmangler-receive-drop-probability* nil
  "Probability that the network stack will drop a received packet.
Should be NIL to disable dropping or a (REAL 0 1) to enable, with higher values
corresponding to higher chances of a packet being dropped.")
(sys.int::defglobal *netmangler-receive-drop-count* 0)
(sys.int::defglobal *netmangler-transmit-duplicate* nil
  "If true, then transmitted packets will be duplicated.")
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
  (sync:watchable-set-add-item device *nics*))

(defun unregister-network-card (device)
  "Unregister a NIC."
  (sup:debug-print-line "Unregistered NIC " device " with MAC " (mac-address device))
  (sync:watchable-set-rem-item device *nics*))

(defun add-nic-watchers (add-mbox rem-mbox)
  "Add mailboxes to watch for NIC registration & unregistration.
All currently registered NICs will immediately be sent to ADD-MBOX."
  (sync:watchable-set-add-watcher add-mbox rem-mbox *nics*))

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
       (progn
         (call-next-method)
         (when *netmangler-transmit-duplicate*
           (call-next-method)))
    (mezzano.supervisor::set-network-light nil)))

(defun device-received-packet (nic packet)
  "Helper function that should be called by NIC's driver when a packet is received."
  (mezzano.supervisor::set-network-light t)
  (mezzano.supervisor::set-network-light nil)
  (let ((prob *netmangler-receive-drop-probability*))
    (when (and prob (< (random 1.0) prob))
      (sys.int::atomic-incf *netmangler-receive-drop-count*)
      (return-from device-received-packet)))
  (sync:mailbox-send packet (receive-mailbox nic))
  (when *netmangler-receive-duplicate*
    (sync:mailbox-send packet (receive-mailbox nic))))
