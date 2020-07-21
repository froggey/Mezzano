;;;; Network initialization and link management

(in-package :mezzano.network)

;; Everything in the network stack uses a single serial queue for the moment...
(defvar *network-serial-queue*)

;; These three variables are only accessed from the network serial queue
;; and don't need additional synchronization.
(defvar *receive-sources* (make-hash-table))
(defvar *boot-source* nil)
(defvar *interface-config* (make-hash-table))

(defvar *static-configurations*
  '((t :dhcp) ; match all interfaces
    ;; This is the old static IP configuration, for use in VirtualBox/qemu.
    #+(or)
    (t ; match all interfaces
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

(defmethod configure-interface (interface (configuration-type (eql :static)) &key local-ip prefix-length gateway dns-servers)
  (let ((local-ip (mezzano.network.ip:make-ipv4-address local-ip)))
    ;; Bring interfaces up.
    (mezzano.network.ip::ifup interface local-ip prefix-length)
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

(defun nic-added (nic)
  ;; Do receive work for this nic.
  (let ((source (mezzano.sync.dispatch:make-source
                 (mezzano.driver.network-card:receive-mailbox nic)
                 (lambda ()
                   (sys.int::log-and-ignore-errors
                     (let ((packet (mezzano.sync:mailbox-receive
                                    (mezzano.driver.network-card:receive-mailbox nic)
                                    :wait-p nil)))
                       (when packet
                         (mezzano.network.ethernet::receive-ethernet-packet
                          nic packet)))))
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

(defun network-boot-handler ()
  (mezzano.supervisor:with-snapshot-inhibited ()
    (mezzano.network.tcp::flush-stale-connections)
    (mezzano.sync.dispatch:cancel *boot-source*)
    (setf *boot-source* (mezzano.sync.dispatch:make-source
                         (mezzano.supervisor:current-boot-id)
                         'network-boot-handler
                         :target *network-serial-queue*))))

;; ARP expiration disabled.
;; It's causing the IP layer to drop packets which breaks long-running TCP connections.
(defun initialize-network-stack ()
  (setf *network-serial-queue* (mezzano.sync.dispatch:make-queue
                                :name "Main network stack queue"
                                :concurrent nil
                                ;; Start suspended so tasks aren't run
                                ;; during initialization.
                                :suspended t))
  ;; Create sources for NIC addition/removal.
  (let ((nic-add-mailbox (mezzano.sync:make-mailbox :name "NIC add mailbox"))
        (nic-rem-mailbox (mezzano.sync:make-mailbox :name "NIC rem mailbox"))
        #+(or)
        (arp-expiration-timer (mezzano.supervisor:make-timer :name "ARP expiration timer")))
    #+(or)
    (setf mezzano.network.arp::*arp-expiration-timer* arp-expiration-timer)
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
    #+(or)
    (mezzano.sync.dispatch:make-source
     arp-expiration-timer
     (lambda ()
       (mezzano.network.arp::arp-expiration))
     :target *network-serial-queue*)
    (mezzano.driver.network-card:add-nic-watchers
     nic-add-mailbox nic-rem-mailbox))
  (setf *boot-source* (mezzano.sync.dispatch:make-source
                       (mezzano.supervisor:current-boot-id)
                       'network-boot-handler
                       :target *network-serial-queue*))
  (setf mezzano.network.ip::*routing-table* '()
        mezzano.network.ip::*ipv4-interfaces* '()
        mezzano.network.ip::*outstanding-sends* '()
        mezzano.network.arp::*arp-table* '()
        *hosts* `(("localhost" "127.0.0.1")))
  (let ((loopback-interface (make-instance 'loopback-interface)))
    (mezzano.network.ip::ifup loopback-interface "127.0.0.1" 8)
    (mezzano.network.ip:add-route "127.0.0.0" 8 loopback-interface))
  ;; All initialzation work complete, now safe to run tasks.
  (mezzano.sync.dispatch:resume *network-serial-queue*))

(defvar *network-dispatch-context*
  (mezzano.sync.dispatch:make-dispatch-context
   :initial-work #'initialize-network-stack
   :name "Network stack"))
