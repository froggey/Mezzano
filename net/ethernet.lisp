;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.network.ethernet)

(defvar *cards* '())

(defconstant +ethertype-ipv4+ #x0800)
(defconstant +ethertype-arp+  #x0806)
(defconstant +ethertype-ipv6+ #x86DD)

(defparameter *ethernet-broadcast* (make-array 6 :element-type '(unsigned-byte 8)
                                               :initial-element #xFF))

(defvar *ethernet-thread* nil)

(defvar *raw-packet-hooks* nil)

(define-condition drop-packet () ())

(defmacro with-raw-packet-hook (function &body body)
  (let ((old-value (gensym)))
    `(let ((,old-value *raw-packet-hooks*))
       (unwind-protect (progn (setf *raw-packet-hooks* (cons ,function *raw-packet-hooks*))
                              ,@body)
         (setf *raw-packet-hooks* ,old-value)))))

(defun ethernet-mac (nic)
  (let ((mac (make-array 6 :element-type '(unsigned-byte 8)))
        (mac-int (mezzano.supervisor:nic-mac nic)))
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
  (dolist (hook *raw-packet-hooks*)
    (funcall hook interface packet))
  (let ((ethertype (ub16ref/be packet 12)))
    (cond
      ((eql ethertype +ethertype-arp+)
       (mezzano.network.arp::arp-receive interface packet))
      ((eql ethertype +ethertype-ipv4+)
       ;; Should check the IP header checksum here...
       (let ((header-length (* (ldb (byte 4 0) (aref packet 14)) 4))
             (total-length (ub16ref/be packet 16))
             (protocol (aref packet (+ 14 9))))
         (cond
           ((eql protocol mezzano.network.ip:+ip-protocol-tcp+)
            (mezzano.network.tcp::%tcp4-receive packet (ub32ref/be packet (+ 14 12)) (+ 14 header-length) (+ 14 total-length)))
           ((eql protocol mezzano.network.ip:+ip-protocol-udp+)
            (mezzano.network.udp::%udp4-receive packet (ub32ref/be packet (+ 14 12)) (+ 14 header-length) (+ 14 total-length)))
           (t (format t "Unknown IPv4 protocol ~S ~S.~%" protocol packet)))))
      (t (format t "Unknown ethertype ~S ~S.~%" ethertype packet)))))

(defun ethernet-thread ()
  (loop
     (sys.int::log-and-ignore-errors
      (multiple-value-bind (packet nic)
          (mezzano.supervisor:net-receive-packet)
        (receive-ethernet-packet nic packet)))))

(defun transmit-ethernet-packet (interface destination ethertype packet)
  (let* ((ethernet-header (make-array 14 :element-type '(unsigned-byte 8)))
	 (packet (cons ethernet-header packet))
	 (source (ethernet-mac interface)))
    (dotimes (i 6)
      (setf (aref ethernet-header i) (aref destination i)
	    (aref ethernet-header (+ i 6)) (aref source i)))
    (setf (ub16ref/be ethernet-header 12) ethertype)
    (transmit-packet interface packet)))

(defun transmit-packet (nic packet)
  (mezzano.supervisor:net-transmit-packet nic packet))

(when (not *ethernet-thread*)
  (setf *ethernet-thread* (mezzano.supervisor:make-thread 'ethernet-thread
                                                          :name "Ethernet thread")))
