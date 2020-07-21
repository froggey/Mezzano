;;;; Ethernet packet handling

(in-package :mezzano.network.ethernet)

(defconstant +ethertype-ipv4+ #x0800)
(defconstant +ethertype-arp+  #x0806)
(defconstant +ethertype-ipv6+ #x86DD)

(defparameter *ethernet-broadcast* (make-array 6 :element-type '(unsigned-byte 8)
                                               :initial-element #xFF))

(defun ethernet-mac (nic)
  (let ((mac (make-array 6 :element-type '(unsigned-byte 8)))
        (mac-int (mezzano.driver.network-card:mac-address nic)))
    (dotimes (i 6)
      (setf (aref mac i) (ldb (byte 8 (* i 8)) mac-int)))
    mac))

(defun format-mac-address (stream mac &optional colon-p at-sign-p)
  (declare (ignore colon-p at-sign-p))
  (format stream "~2,'0X:~2,'0X:~2,'0X:~2,'0X:~2,'0X:~2,'0X"
          (ldb (byte 8 0) mac)
          (ldb (byte 8 8) mac)
          (ldb (byte 8 16) mac)
          (ldb (byte 8 24) mac)
          (ldb (byte 8 32) mac)
          (ldb (byte 8 40) mac)))

(defgeneric ethernet-receive (ethertype interface packet start end))

(defmethod ethernet-receive (ethertype interface packet start end)
  nil)

(defun receive-ethernet-packet (interface packet)
  (ethernet-receive (ub16ref/be packet 12) interface packet 14 (length packet)))

(defun ethernet-loopback (interface packet)
  ;; This is a bit hacky... (less than it was before!)
  (let ((loopback-packet (make-array (net::packet-length packet)
                                     :element-type '(unsigned-byte 8))))
    (net::copy-packet loopback-packet packet)
    (mezzano.sync.dispatch:dispatch-async
     (lambda ()
       (sys.int::log-and-ignore-errors
         (receive-ethernet-packet interface loopback-packet)))
     net::*network-serial-queue*)))

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
