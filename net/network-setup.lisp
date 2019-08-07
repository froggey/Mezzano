;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.net)

;; Everything in the network stack uses a single serial queue for the moment...
(defvar *network-serial-queue*)

(defvar *receive-sources* (make-hash-table))
(defvar *interface-config* (make-hash-table))

(defvar *static-configurations*
  '((t ; match all interfaces
     :static
     :local-ip "10.0.2.15"
     ;; Use a prefix-length of 24 instead of 8, so people
     ;; running the file-server on non-10.0.2.0 10/8 networks
     :prefix-length 24
     :gateway "10.0.2.2"
     ;; Use Google DNS, as Virtualbox does not provide a DNS
     ;; server within the NAT.
     :dns-servers ("8.8.8.8")))
  "A list of known INTERFACEs and configurations to use with them.")

(defun match-static-configuration (interface)
  (dolist (conf *static-configurations* nil)
    (when (or (eql (first conf) t) ; wildcard configuration
              (equal (first conf) (mezzano.driver.network-card:mac-address interface)))
      (return (rest conf)))))

(defgeneric configure-interface (interface configuration-type &key)
  (:documentation "Begin acquiring an IP address/routes/etc for INTERFACE."))
(defgeneric deconfigure-interface (interface configuration-type &key)
  (:documentation "Remove IP addresses/routes/etc associated with INTERFACE.
This is called with the same options CONFIGURE-INTERFACE was originally called with."))

(defmethod configure-interface (interface (configuration-type (eql :static)) &key local-ip prefix-length gateway dns-servers)
  (let ((local-ip (mezzano.network.ip:make-ipv4-address local-ip)))
    ;; Bring interfaces up.
    (mezzano.network.ip::ifup interface local-ip)
    ;; Add routes.
    ;; Local network.
    (mezzano.network.ip:add-route
     (mezzano.network.ip:address-network local-ip prefix-length)
     prefix-length
     interface
     interface)
    ;; Default route.
    (when gateway
      (mezzano.network.ip:add-route
       "0.0.0.0" 0
       (mezzano.network.ip:make-ipv4-address gateway)
       interface))
    (dolist (dns-server dns-servers)
      (mezzano.network.dns:add-dns-server dns-server interface))))

(defmethod deconfigure-interface (interface (configuration-type (eql :static)) &key local-ip prefix-length gateway dns-servers)
  (let ((local-ip (mezzano.network.ip:make-ipv4-address local-ip)))
    (mezzano.network.ip:remove-route
     (mezzano.network.ip:address-network local-ip prefix-length)
     prefix-length
     interface)
    (when gateway
      (mezzano.network.ip:remove-route "0.0.0.0" 0 interface))
    (dolist (dns-server dns-servers)
      (mezzano.network.dns:remove-dns-server dns-server interface))
    (mezzano.network.ip::ifdown interface)))

;; TODO: This is where DHCP could be done.
(defun nic-added (nic)
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
    (setf (gethash nic *receive-sources*) source))
  ;; Bring this interface up.
  (let ((conf (match-static-configuration nic)))
    (cond (conf
           (setf (gethash nic *interface-config*) conf)
           (apply #'configure-interface nic (first conf) (rest conf)))
          (t
           (format t "No static configuration available for interface ~A~%" nic)))))

(defun nic-removed (nic)
  (let ((conf (gethash nic *interface-config*)))
    (when conf
      (apply #'deconfigure-interface nic (first conf) (rest conf))
      (remhash nic *interface-config*)))
  ;; Stop trying to receive packets on this interface.
  (mezzano.sync.dispatch:cancel (gethash nic *receive-sources*))
  (remhash nic *receive-sources*))

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
  (let ((loopback-interface (make-instance 'sys.net::loopback-interface)))
    (mezzano.network.ip::ifup loopback-interface "127.0.0.1")
    (mezzano.network.ip:add-route "127.0.0.0" 8 loopback-interface))
  (setf mezzano.network.ip::*routing-table* '()
        mezzano.network.ip::*ipv4-interfaces* '()
        mezzano.network.ip::*outstanding-sends* '()
        mezzano.network.arp::*arp-table* '()
        *hosts* `(("localhost" "127.0.0.1")))
  ;; All initialzation work complete, now safe to run tasks.
  (mezzano.sync.dispatch:resume *network-serial-queue*))

(defvar *network-dispatch-context*
  (mezzano.sync.dispatch:make-dispatch-context
   :initial-work #'initialize-network-stack
   :name "Network stack"))
