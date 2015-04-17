(in-package :sys.net)

;;; Hardcode the qemu & virtualbox network layout for now.
(defun net-setup (&key
                  (local-ip (mezzano.network.ip:make-ipv4-address '(10 0 2 15)))
                  (prefix-length 8)
                  (gateway (mezzano.network.ip:make-ipv4-address '(10 0 2 2)))
                  (interface (first mezzano.network.ethernet::*cards*)))
  (let ((loopback-interface (make-instance 'sys.net::loopback-interface)))
    ;; Flush existing route info.
    (setf mezzano.network.ip::*ipv4-interfaces* nil
          mezzano.network.ip::*routing-table* nil
          mezzano.network.dns:*dns-servers* '())
    ;; Bring interfaces up.
    (mezzano.network.ip::ifup interface local-ip)
    (mezzano.network.ip::ifup loopback-interface
                              (mezzano.network.ip:make-ipv4-address '(127 0 0 1)))
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
    (mezzano.network.ip:add-route
     (mezzano.network.ip:make-ipv4-address '(127 0 0 0))
     8
     loopback-interface)
    ;; Use Google DNS, as Virtualbox does not provide a DNS server within the NAT.
    (push (mezzano.network.ip:make-ipv4-address '(8 8 8 8)) mezzano.network.dns:*dns-servers*))
  t)

(defun ethernet-boot-hook ()
  (setf mezzano.network.ethernet::*cards* (copy-list mezzano.supervisor:*nics*)
        mezzano.network.ip::*routing-table* '()
        mezzano.network.ip::*ipv4-interfaces* '()
        mezzano.network.arp::*arp-table* '()
        *hosts* `(("localhost" ,(mezzano.network.ip:make-ipv4-address '(127 0 0 1)))))
  (net-setup)
  (format t "Interfaces: ~S~%" mezzano.network.ip::*ipv4-interfaces*))
(ethernet-boot-hook)
(mezzano.supervisor:add-boot-hook 'ethernet-boot-hook)

;; Don't start the ethernet worker until the whole stack has been loaded.
(when (not mezzano.network.ethernet::*ethernet-thread*)
  (setf mezzano.network.ethernet::*ethernet-thread*
        (mezzano.supervisor:make-thread 'mezzano.network.ethernet::ethernet-thread
                                        :name "Ethernet thread")))
