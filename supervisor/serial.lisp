;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;;; Serial registers.
(defconstant +serial-RBR+ 0 "Receive buffer, R/O, DLAB=0.")
(defconstant +serial-THR+ 0 "Transmitter holding, W/O, DLAB=0.")
(defconstant +serial-IER+ 1 "Interrupt enable, R/W, DLAB=0.")
(defconstant +serial-IIR+ 2 "Interrupt identification, R/O.")
(defconstant +serial-FCR+ 2 "FIFO control, W/O.")
(defconstant +serial-LCR+ 3 "Line control, R/W.")
(defconstant +serial-MCR+ 4 "Modem control, R/W.")
(defconstant +serial-LSR+ 5 "Line status, R/O.")
(defconstant +serial-MSR+ 6 "Modem status, R/O.")
(defconstant +serial-SCR+ 7 "Scratch, R/W.")
(defconstant +serial-DLL+ 0 "Divisor latch LSB, R/W, DLAB=1.")
(defconstant +serial-DLH+ 1 "Divisor latch MSB, R/W, DLAB=1.")

;;; Interrupt enable bits.
(defconstant +serial-ier-received-data-available+ #x01)
(defconstant +serial-ier-transmitter-holding-register-empty+ #x02)
(defconstant +serial-ier-receiver-line-status-register-change+ #x04)
(defconstant +serial-ier-modem-status-register-change+ #x08)
(defconstant +serial-ier-sleep-mode+ #x10)
(defconstant +serial-ier-low-power-mode+ #x20)

;;; Interrupt identification bits.
(defconstant +serial-iir-interrupt-not-pending+ #x01)
(defconstant +serial-iir-interrupt+ #b00001110)
(defconstant +serial-iir-interrupt-modem-status-change+ #b00000000)
(defconstant +serial-iir-interrupt-transmitter-holding-register-empty+ #b00000010)
(defconstant +serial-iir-interrupt-received-data-available+ #b00000100)
(defconstant +serial-iir-interrupt-line-status-change+ #b00000110)
(defconstant +serial-iir-interrupt-character-timeout+ #b00001100)
(defconstant +serial-iir-64-byte-fifo-enabled+ #x20)
(defconstant +serial-iir-fifo-status+ #b11000000)
(defconstant +serial-iir-no-fifo+ #b00000000)
(defconstant +serial-iir-unusable-fifo+ #b10000000)
(defconstant +serial-iir-fifo-enabled+ #b11000000)

;;; FIFO control bits.
(defconstant +serial-fcr-enable+ #x01)
(defconstant +serial-fcr-clear-rx+ #x02)
(defconstant +serial-fcr-clear-tx+ #x04)
(defconstant +serial-fcr-dma-mode-0+ #x00)
(defconstant +serial-fcr-dma-mode-1+ #x08)
(defconstant +serial-fcr-enable-64-byte-fifo+ #x20)
(defconstant +serial-fcr-rx-trigger-level+ #b11000000)
(defconstant +serial-rx-trigger-1-byte+ #b00000000)
(defconstant +serial-rx-trigger-4-bytes+ #b01000000)
(defconstant +serial-rx-trigger-8-bytes+ #b10000000)
(defconstant +serial-rx-trigger-14-bytes+ #b11000000)

;;; Line control bits.
(defconstant +serial-lcr-data+ #b00000011
  "Data size bits in LCR.")
(defconstant +serial-lcr-data-5+ 0 "5 data bits.")
(defconstant +serial-lcr-data-6+ 1 "6 data bits.")
(defconstant +serial-lcr-data-7+ 2 "7 data bits.")
(defconstant +serial-lcr-data-8+ 3 "8 data bits.")
(defconstant +serial-lcr-stop-2+ #x04
  "Use 2 stop bits (or 1.5 with 5 bit words).")
(defconstant +serial-lcr-parity+ #b00111000
  "Parity bits in LCR.")
(defconstant +serial-lcr-no-parity+   #b00000000)
(defconstant +serial-lcr-odd-parity+  #b00001000)
(defconstant +serial-lcr-even-parity+ #b00011000)
(defconstant +serial-lcr-high-parity+ #b00101000)
(defconstant +serial-lcr-low-parity+  #b00111000)
(defconstant +serial-lcr-break-signal-enable+ #x40)
(defconstant +serial-lcr-dlab+ #x80
  "Enable access to config registers.")

;;; Modem control bits.
(defconstant +serial-mcr-data-terminal-ready+ #x01)
(defconstant +serial-mcr-request-to-send+ #x02)
(defconstant +serial-mcr-auxiliary-output-1+ #x04)
(defconstant +serial-mcr-auxiliary-output-2+ #x08)
(defconstant +serial-mcr-loopback-mode+ #x10)
(defconstant +serial-mcr-autoflow-control+ #x20)

;;; Line status bits.
(defconstant +serial-lsr-data-available+ 0)
(defconstant +serial-lsr-overrun-error+ 1)
(defconstant +serial-lsr-parity-error+ 2)
(defconstant +serial-lsr-framing-error+ 3)
(defconstant +serial-lsr-break-signal+ 4)
(defconstant +serial-lsr-thr-empty+ 5)
(defconstant +serial-lsr-thr-empty-line-idle+ 6)
(defconstant +serial-lsr-bad-fifo-data+ 7)

;;; Modem status bits.
(defconstant +serial-msr-clear-to-send-change+ 0)
(defconstant +serial-msr-data-set-ready-change+ 1)
(defconstant +serial-msr-ring-indicator-trailing-edge+ 2)
(defconstant +serial-msr-carrier-detect-change+ 3)
(defconstant +serial-msr-clear-to-send+ 4)
(defconstant +serial-msr-data-set-ready+ 5)
(defconstant +serial-msr-ring-indicator+ 6)
(defconstant +serial-msr-carrier-detect+ 7)

(defconstant +bochs-log-port+ #xE9)

(defconstant +debug-serial-tx-fifo-size+ 16)

(sys.int::defglobal *debug-serial-io-port*)
(sys.int::defglobal *debug-serial-lock*)
(sys.int::defglobal *serial-at-line-start*)

;; Low-level byte functions.

(defun debug-serial-write-byte (byte)
  (setf (sys.int::io-port/8 +bochs-log-port+) byte)
  (safe-without-interrupts (byte)
    (with-symbol-spinlock (*debug-serial-lock*)
      ;; Wait for the TX FIFO to empty.
      (loop
         until (logbitp +serial-lsr-thr-empty+
                        (sys.int::io-port/8 (+ *debug-serial-io-port*
                                               +serial-LSR+))))
      ;; Write byte.
      (setf (sys.int::io-port/8 (+ *debug-serial-io-port* +serial-THR+))
            byte))))

;; High-level character functions. These assume that whatever is on the other
;; end of the port uses UTF-8 with CRLF newlines.

(defun debug-serial-write-char (char)
  (setf *serial-at-line-start* nil)
  ;; FIXME: Should write all the bytes to the buffer in one go.
  ;; Other processes may interfere.
  (cond ((eql char #\Newline)
         (setf *serial-at-line-start* t)
         ;; Turn #\Newline into CRLF
         (debug-serial-write-byte #x0D)
         (debug-serial-write-byte #x0A))
        (t
         (with-utf-8-bytes (char byte)
           (debug-serial-write-byte byte)))))

(defun debug-serial-write-string (string)
  (dotimes (i (string-length string))
    (debug-serial-write-char (char string i))))

(defun debug-serial-stream (op &optional arg)
  (ecase op
    (:read-char (panic "Serial read char not implemented."))
    (:clear-input)
    (:write-char (debug-serial-write-char arg))
    (:write-string (debug-serial-write-string arg))
    (:force-output)
    (:start-line-p *serial-at-line-start*)))

(defun initialize-debug-serial (io-port irq baud)
  (declare (ignore irq))
  (setf *debug-serial-io-port* io-port
        *debug-serial-lock* :unlocked
        *serial-at-line-start* t)
  ;; Initialize port.
  (let ((divisor (truncate 115200 baud)))
    (setf
     ;; Turn interrupts off.
     (sys.int::io-port/8 (+ io-port +serial-IER+)) #x00
     ;; DLAB on.
     (sys.int::io-port/8 (+ io-port +serial-LCR+)) +serial-lcr-dlab+
     ;; Set divisor low/high bytes.
     (sys.int::io-port/8 (+ io-port +serial-DLL+)) (ldb (byte 8 0) divisor)
     (sys.int::io-port/8 (+ io-port +serial-DLH+)) (ldb (byte 8 8) divisor)
     ;; 8N1, DLAB off.
     (sys.int::io-port/8 (+ io-port +serial-LCR+)) (logior +serial-lcr-data-8+
                                                           +serial-lcr-no-parity+)
     ;; Enable FIFOs, clear them and use the 14-byte threshold.
     (sys.int::io-port/8 (+ io-port +serial-FCR+)) (logior +serial-fcr-enable+
                                                           +serial-fcr-clear-rx+
                                                           +serial-fcr-clear-tx+
                                                           +serial-rx-trigger-14-bytes+)
     ;; Enable RTS, DTR, and enable aux output 2, required for IRQs.
     (sys.int::io-port/8 (+ io-port +serial-MCR+)) (logior +serial-mcr-data-terminal-ready+
                                                           +serial-mcr-request-to-send+
                                                           +serial-mcr-auxiliary-output-2+)
     ;; Enable RX interrupts.
     (sys.int::io-port/8 (+ io-port +serial-IER+)) +serial-ier-received-data-available+))
  (debug-set-output-pseudostream 'debug-serial-stream))
