;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :system.networking
  (:use :cl)
  (:nicknames :sys.net :mezzano.network)
  (:export #:copy-packet #:packet-length
           #:buffered-format
           #:send #:receive
           #:local-endpoint
           #:remote-endpoint
           #:disconnect
           #:resolve-address
           #:octet
           #:network-error))

(defpackage :mezzano.network.ethernet
  (:use :cl)
  (:import-from :sys.int
                #:ub16ref/be #:ub16ref/le
                #:ub32ref/be #:ub32ref/le
                #:ub64ref/be #:ub64ref/le)
  (:export #:ethernet-mac
           #:format-mac-address
           #:*ethernet-broadcast*
           #:transmit-packet
           #:transmit-ethernet-packet
           #:ethernet-receive
           #:+ethertype-ipv4+
           #:+ethertype-arp+
           #:+ethertype-ipv6+))

(defpackage :mezzano.network.arp
  (:use :cl)
  (:import-from :sys.int
                #:ub16ref/be #:ub16ref/le
                #:ub32ref/be #:ub32ref/le
                #:ub64ref/be #:ub64ref/le)
  (:export #:arp-lookup
           #:start-arp-expiration))

(defpackage :mezzano.network.ip
  (:use :cl)
  (:import-from :sys.int
                #:ub16ref/be #:ub16ref/le
                #:ub32ref/be #:ub32ref/le
                #:ub64ref/be #:ub64ref/le)
  (:import-from :sys.net
                #:octet)
  (:export #:make-ipv4-address
           #:format-ipv4-address
           #:ipv4-address-to-string
           #:invalid-ipv4-address
           #:parse-ipv4-address
           #:ipv4-interface-address
           #:address-network
           #:address-host
           #:address-equal
           #:compute-ip-checksum
           #:compute-ip-partial-checksum
           #:no-route-to-host
           #:ipv4-route
           #:add-route
           #:remove-route
           #:transmit-ipv4-packet
           #:transmit-ipv4-packet-on-interface
           #:+ip-protocol-icmp+
           #:+ip-protocol-igmp+
           #:+ip-protocol-tcp+
           #:+ip-protocol-udp+
           #:ping-host
           #:+ipv4-broadcast-source+
           #:+ipv4-broadcast-local-network+
           #:+ipv4-multicast-network+
           #:ipv4-receive))

(defpackage :mezzano.network.tcp
  (:use :cl)
  (:import-from :sys.int
                #:ub16ref/be #:ub16ref/le
                #:ub32ref/be #:ub32ref/le
                #:ub64ref/be #:ub64ref/le)
  (:local-nicknames (:gray :mezzano.gray))
  (:export #:tcp-stream-connect
           #:tcp-listen
           #:tcp-accept
           #:close-tcp-listener
           #:tcp-listener-local-port
           #:tcp-listener-local-ip
           #:tcp-connection-local-port
           #:tcp-connection-local-ip
           #:tcp-connection-remote-port
           #:tcp-connection-remote-ip
           #:tcp-stream-connection
           #:network-error
           #:connection-error
           #:connection-error-host
           #:connection-error-port
           #:connection-aborted
           #:connection-timed-out
           #:connection-stale))

(defpackage :mezzano.network.udp
  (:use :cl)
  (:import-from :sys.int
                #:ub16ref/be #:ub16ref/le
                #:ub32ref/be #:ub32ref/le
                #:ub64ref/be #:ub64ref/le)
  (:import-from :sys.net
                #:send
                #:receive
                #:disconnect)
  (:export #:with-udp-connection))

(defpackage :mezzano.network.dns
  (:use :cl)
  (:import-from :sys.int
                #:ub16ref/be #:ub16ref/le
                #:ub32ref/be #:ub32ref/le
                #:ub64ref/be #:ub64ref/le)
  (:export #:resolve-address
           #:*dns-servers*
           #:add-dns-server
           #:remove-dns-server))

(defpackage :mezzano.network.dhcp
  (:use :cl)
  (:import-from :sys.int
                #:ub16ref/be #:ub16ref/le
                #:ub32ref/be #:ub32ref/le
                #:ub64ref/be #:ub64ref/le)
  (:export #:acquire-lease #:renew-lease #:dhcp-lease #:start-dhcp-interaction
	   #:dhcp-invalid-option #:dhcp-error #:ip-address #:netmask #:gateway #:interface #:ntp-servers
	   #:dns-servers #:dhcp-server #:lease-timeout #:lease-timestamp #:mezzano-server))
