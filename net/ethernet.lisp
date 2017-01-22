;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.network.ethernet)

(defvar *cards* '())

(defconstant +ethertype-ipv4+ #x0800)
(defconstant +ethertype-arp+  #x0806)
(defconstant +ethertype-ipv6+ #x86DD)

(defparameter *ethernet-broadcast* (make-array 6 :element-type '(unsigned-byte 8)
                                               :initial-element #xFF))

(defvar *ethernet-thread* nil)

(defun ethernet-mac (nic)
  (let ((mac (make-array 6 :element-type '(unsigned-byte 8)))
        (mac-int (mezzano.driver.network-card:mac-address nic)))
    (dotimes (i 6)
      (setf (aref mac i) (ldb (byte 8 (* i 8)) mac-int)))
    mac))

(defun format-mac-address (stream mac &optional colon-p at-sign-p)
  (format stream "~2,'0X:~2,'0X:~2,'0X:~2,'0X:~2,'0X:~2,'0X"
          (ldb (byte 8 0) mac)
          (ldb (byte 8 8) mac)
          (ldb (byte 8 16) mac)
          (ldb (byte 8 24) mac)
          (ldb (byte 8 32) mac)
          (ldb (byte 8 40) mac)))

(defun receive-ethernet-packet (interface packet)
  (let ((ethertype (ub16ref/be packet 12)))
    (cond
      ((eql ethertype +ethertype-arp+)
       (mezzano.network.arp::arp-receive interface packet))
      ((eql ethertype +ethertype-ipv4+)
       (mezzano.network.ip::ipv4-receive interface packet 14 (length packet)))
      (t (format t "Unknown ethertype ~X ~X.~%" ethertype packet)))))

(defun ethernet-thread ()
  (loop
     (sys.int::log-and-ignore-errors
      (multiple-value-bind (packet nic)
          (mezzano.driver.network-card:receive-packet)
        (receive-ethernet-packet nic packet)))))

(defun ethernet-loopback (interface packet)
  ;; This is a bit hacky...
  (mezzano.driver.network-card::device-received-packet
   interface
   (let ((loopback-packet (make-array (sys.net::packet-length packet)
                                      :element-type '(unsigned-byte 8))))
     (sys.net::copy-packet loopback-packet packet)
     loopback-packet)))

(defun transmit-ethernet-packet (interface destination ethertype packet)
  (let* ((ethernet-header (make-array 14 :element-type '(unsigned-byte 8)))
         (packet (cons ethernet-header packet))
         (source (ethernet-mac interface)))
    (dotimes (i 6)
      (setf (aref ethernet-header i) (aref destination i)
            (aref ethernet-header (+ i 6)) (aref source i)))
    (setf (ub16ref/be ethernet-header 12) ethertype)
    (cond ((equalp destination source)
           ;; Loopback, don't hit the wire.
           (ethernet-loopback interface packet))
          ((equalp destination *ethernet-broadcast*)
           ;; Broadcast, loopback and send over the wire.
           (ethernet-loopback interface packet)
           (transmit-packet interface packet))
          (t ;; Somewhere else.
           (transmit-packet interface packet)))))

(defun transmit-packet (nic packet)
  (mezzano.driver.network-card:transmit-packet nic packet))
