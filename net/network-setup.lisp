(in-package :sys.net)

;;; Hardcode the qemu & virtualbox network layout for now.
(defun net-setup (&key
                  (local-ip (mezzano.network.ip:make-ipv4-address '(10 0 2 15)))
                  (prefix-length 8)
                  (gateway (mezzano.network.ip:make-ipv4-address '(10 0 2 2)))
                  (interface (first mezzano.network.ethernet::*cards*)))
  ;; Flush existing route info.
  (setf mezzano.network.ip::*ipv4-interfaces* nil
        mezzano.network.ip::*routing-table* nil
        mezzano.network.dns:*dns-servers* '())
  (mezzano.network.ip::ifup interface local-ip)
  ;; Default route.
  (mezzano.network.ip:add-route
   (mezzano.network.ip:make-ipv4-address 0)
   0
   gateway
   interface)
  ;; Local network.
  (mezzano.network.ip:add-route
   (mezzano.network.ip:address-network local-ip prefix-length)
   prefix-length
   nil
   interface)
  ;; Use Google DNS, as Virtualbox does not provide a DNS server within the NAT.
  (push (mezzano.network.ip:make-ipv4-address '(8 8 8 8)) mezzano.network.dns:*dns-servers*)
  t)

(defun ethernet-boot-hook ()
  (setf mezzano.network.ethernet::*cards* (copy-list mezzano.supervisor:*nics*)
        mezzano.network.ip::*routing-table* '()
        mezzano.network.ip::*ipv4-interfaces* '()
        mezzano.network.arp::*arp-table* '()
        ;; FIXME: need a loopback route.
        *hosts* `(("localhost" ,(mezzano.network.ip:make-ipv4-address '(10 0 2 15)))))
  (net-setup)
  (format t "Interfaces: ~S~%" mezzano.network.ip::*ipv4-interfaces*))
(ethernet-boot-hook)
(mezzano.supervisor:add-boot-hook 'ethernet-boot-hook)
