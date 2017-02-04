;;;; Copyright (c) 2015-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.net)

;;; Hardcode the qemu & virtualbox network layout for now.
(defun net-setup (&key
                    (local-ip "10.0.2.15")
                    ;; Use a prefix-length of 24 instead of 8, so people
                    ;; running the file-server on non-10.0.2.0 10/8 networks
                    ;; don't run into trouble.
                    (prefix-length 24)
                    (gateway "10.0.2.2")
                    (interface (first mezzano.network.ethernet::*cards*))
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

(defun ethernet-boot-hook ()
  ;; delay a little to allow any NIC drivers to complete.
  ;; TODO: Need a notification mechanism to detect when NICs are attached/detached.
  (sleep 1)
  (setf mezzano.network.ethernet::*cards* (copy-list mezzano.driver.network-card::*nics*)
        mezzano.network.ip::*routing-table* '()
        mezzano.network.ip::*ipv4-interfaces* '()
        mezzano.network.ip::*outstanding-sends* '()
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
