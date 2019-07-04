;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.net)

(defvar *cards* '())

;;; Hardcode the qemu & virtualbox network layout for now.
(defun net-setup (&key
                    (local-ip "10.0.2.15")
                    ;; Use a prefix-length of 24 instead of 8, so people
                    ;; running the file-server on non-10.0.2.0 10/8 networks
                    ;; don't run into trouble.
                    (prefix-length 24)
                    (gateway "10.0.2.2")
                    (interface (first *cards*))
                    ;; Use Google DNS, as Virtualbox does not provide a DNS server within the NAT.
                    (dns-server "8.8.8.8"))
  (let ((loopback-interface (make-instance 'sys.net::loopback-interface))
        (local-ip (mezzano.network.ip:make-ipv4-address local-ip))
        (gateway (mezzano.network.ip:make-ipv4-address gateway))
        (loopback-ip (mezzano.network.ip:make-ipv4-address "127.0.0.1"))
        (loopback-network (mezzano.network.ip:make-ipv4-address "127.0.0.0")))
    ;; Flush existing route info.
    (setf mezzano.network.ip::*ipv4-interfaces* nil
          mezzano.network.ip::*routing-table* nil
          mezzano.network.dns:*dns-servers* '())
    ;; Bring interfaces up.
    (mezzano.network.ip::ifup interface local-ip)
    (mezzano.network.ip::ifup loopback-interface loopback-ip)
    ;; Set up default routing table.
    ;; Default route.
    (mezzano.network.ip:add-route
     (mezzano.network.ip:make-ipv4-address 0)
     0
     gateway)
    ;; Local network.
    (mezzano.network.ip:add-route
     (mezzano.network.ip:address-network local-ip prefix-length)
     prefix-length
     interface)
    ;; Loopback network.
    (mezzano.network.ip:add-route loopback-network 8 loopback-interface)
    (push (mezzano.network.ip:make-ipv4-address dns-server)
          mezzano.network.dns:*dns-servers*))
  t)

;; Everything in the network stack uses a single serial queue for the moment...
(defvar *network-serial-queue*)

(defvar *receive-sources* (make-hash-table))

;; TODO: This is where DHCP could be done.
(defun nic-added (nic)
  (push nic *cards*)
  (setf mezzano.network.ip::*routing-table* '()
        mezzano.network.ip::*ipv4-interfaces* '()
        mezzano.network.ip::*outstanding-sends* '()
        mezzano.network.arp::*arp-table* '()
        *hosts* `(("localhost" ,(mezzano.network.ip:make-ipv4-address '(127 0 0 1)))))
  (net-setup)
  (format t "Interfaces: ~S~%" mezzano.network.ip::*ipv4-interfaces*)
  ;; Do receive work for this nic.
  (let ((source (mezzano.sync.dispatch:make-source
                 (mezzano.driver.network-card:receive-mailbox nic)
                 (lambda ()
                   (sys.int::log-and-ignore-errors
                     (let ((packet (mezzano.sync:mailbox-receive
                                    (mezzano.driver.network-card:receive-mailbox nic))))
                       (mezzano.network.ethernet::receive-ethernet-packet
                        nic packet))))
                 :target *network-serial-queue*)))
    (setf (gethash nic *receive-sources*) source)))

(defun nic-removed (nic)
  ;; Stop trying to receive packets on this interface.
  (mezzano.sync.dispatch:cancel (gethash nic *receive-sources*))
  (remhash nic *receive-sources*)
  (setf *cards* (remove nic *cards*)))

;; TODO: Integrate ARP expiration into this.
(defun initialize-network-stack ()
  (setf *network-serial-queue* (mezzano.sync.dispatch:make-queue
                                :name "Main network stack queue"
                                :concurrent nil
                                ;; Start suspended so tasks aren't run
                                ;; during initialization.
                                :suspended t))
  ;; Create sources for NIC addition/removal.
  (let ((nic-add-mailbox (mezzano.sync:make-mailbox :name "NIC add mailbox"))
        (nic-rem-mailbox (mezzano.sync:make-mailbox :name "NIC rem mailbox")))
    (mezzano.sync.dispatch:make-source
     nic-add-mailbox
     (lambda ()
       (nic-added (mezzano.sync:mailbox-receive nic-add-mailbox)))
     :target *network-serial-queue*)
    (mezzano.sync.dispatch:make-source
     nic-rem-mailbox
     (lambda ()
       (nic-removed (mezzano.sync:mailbox-receive nic-rem-mailbox)))
     :target *network-serial-queue*)
    (mezzano.driver.network-card:add-nic-hooks
     (lambda (nic) (mezzano.sync:mailbox-send nic nic-add-mailbox))
     (lambda (nic) (mezzano.sync:mailbox-send nic nic-rem-mailbox))))
  ;; All initialzation work complete, now safe to run tasks.
  (mezzano.sync.dispatch:resume *network-serial-queue*))

(defvar *network-dispatch-context*
  (mezzano.sync.dispatch:make-dispatch-context
   :initial-work #'initialize-network-stack
   :name "Network stack"))
