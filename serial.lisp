(in-package #:sys.int)

(defclass serial-stream (sys.gray:fundamental-binary-input-stream
                         sys.gray:fundamental-binary-output-stream)
  ((base :initarg :base :reader serial-stream-io-base)
   (irq :initarg :irq :reader serial-stream-irq)
   (speed :initarg :speed :reader serial-stream-speed))
  (:default-initargs :speed 38400))

(defmethod initialize-instance :after ((instance serial-stream) &key &allow-other-keys)
  (check-type (serial-stream-speed instance) (integer 50 115200))
  (reset-serial-port instance))

(defun reinit-serial-ports ()
  (reset-serial-port *serial-one*)
  (reset-serial-port *serial-two*))
(add-hook '*early-initialize-hook* 'reinit-serial-ports)

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
(defparameter *serial-iir-interrupt* (byte 3 1))
(defconstant +serial-iir-interrupt-modem-status-change+ #b000)
(defconstant +serial-iir-interrupt-transmitter-holding-register-empty+ #b001)
(defconstant +serial-iir-interrupt-received-data-available+ #b010)
(defconstant +serial-iir-interrupt-line-status-change+ #b011)
(defconstant +serial-iir-interrupt-character-timeout+ #b110)
(defconstant +serial-iir-64-byte-fifo-enabled+ #x20)
(defparameter *serial-iir-fifo-status* (byte 2 6))
(defconstant +serial-iir-no-fifo+ #b00)
(defconstant +serial-iir-unusable-fifo+ #b10)
(defconstant +serial-iir-fifo-enabled+ #b11)

;;; FIFO control bits.
(defconstant +serial-fcr-enable+ #x01)
(defconstant +serial-fcr-clear-rx+ #x02)
(defconstant +serial-fcr-clear-tx+ #x04)
(defconstant +serial-fcr-dma-mode-0+ #x00)
(defconstant +serial-fcr-dma-mode-1+ #x08)
(defconstant +serial-fcr-enable-64-byte-fifo+ #x20)
(defparameter *serial-fcr-rx-trigger-level* (byte 2 6))
(defconstant +serial-rx-trigger-1-byte+ #b00)
(defconstant +serial-rx-trigger-4-bytes+ #b01)
(defconstant +serial-rx-trigger-8-bytes+ #b10)
(defconstant +serial-rx-trigger-14-bytes+ #b11)

;;; Line control bits.
(defparameter *serial-lcr-data* (byte 2 0)
  "Data size bits in LCR.")
(defconstant +serial-lcr-data-5+ 0 "5 data bits.")
(defconstant +serial-lcr-data-6+ 1 "6 data bits.")
(defconstant +serial-lcr-data-7+ 2 "7 data bits.")
(defconstant +serial-lcr-data-8+ 3 "8 data bits.")
(defconstant +serial-lcr-stop-2+ #x04
  "Use 2 stop bits (or 1.5 with 5 bit words).")
(defparameter *serial-lcr-parity* (byte 3 3)
  "Parity bits in LCR.")
(defconstant +serial-lcr-no-parity+ 0)
(defconstant +serial-lcr-odd-parity+ 1)
(defconstant +serial-lcr-even-parity+ 3)
(defconstant +serial-lcr-high-parity+ 5)
(defconstant +serial-lcr-low-parity+ 7)
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

(define-interrupt-handler serial-interrupt ()
  't)

(defun reset-serial-port (port &optional speed)
  (check-type speed (or null (integer 50 115200)))
  (if speed
      (setf (slot-value port 'speed) speed)
      (setf speed (serial-stream-speed port)))
  (let ((base (serial-stream-io-base port))
        (irq (serial-stream-irq port))
        (divisor (truncate 115200 speed)))
    (format t "Initializing serial port ~X (irq ~D), speed ~D (~D)~%" base irq speed divisor)
    (setf (isa-pic-irq-mask irq) t)
    (when (zerop divisor) (setf divisor 1))
    ;; Configure the port.
    (setf (io-port/8 (+ base +serial-IER+)) #x00 ; Turn off interrupts.
          (io-port/8 (+ base +serial-LCR+)) +serial-lcr-dlab+ ; DLAB on.
          ;; Set baud rate.
          (io-port/8 (+ base +serial-DLL+)) (ldb (byte 8 0) divisor) ; Divisor Latch Low Byte.
          (io-port/8 (+ base +serial-DLH+)) (ldb (byte 8 8) divisor)) ; Divisor Latch High Byte.
    ;; LCR. Set 8N1, no break, DLAB off.
    (setf (io-port/8 (+ base +serial-LCR+)) (logior (dpb +serial-lcr-data-8+ *serial-lcr-data* 0)
                                                    (dpb +serial-lcr-no-parity+ *serial-lcr-parity* 0)))
    ;; FIFO control.
    (setf (io-port/8 (+ base +serial-FCR+)) (logior (dpb +serial-rx-trigger-14-bytes+ *serial-fcr-rx-trigger-level* 0)
                                                    +serial-fcr-enable+
                                                    +serial-fcr-clear-rx+
                                                    +serial-fcr-clear-tx+))
    ;; Modem control.
    (setf (io-port/8 (+ base +serial-MCR+)) (logior +serial-mcr-data-terminal-ready+
                                                    +serial-mcr-request-to-send+
                                                    +serial-mcr-auxiliary-output-2+))
    ;; Turn RX interrupts on.
    (setf (io-port/8 (+ base +serial-IER+)) +serial-ier-received-data-available+)
    (setf (isa-pic-interrupt-handler irq) (make-interrupt-handler 'serial-interrupt)
          (isa-pic-irq-mask irq) nil)))

(defvar *serial-one* (make-instance 'serial-stream :base #x3F8 :irq 4))
(defvar *serial-two* (make-instance 'serial-stream :base #x2F8 :irq 3))

(defmethod sys.gray:stream-write-byte ((stream serial-stream) integer)
  (check-type integer (unsigned-byte 8))
  (let ((base (serial-stream-io-base stream)))
    (loop (when (logbitp +serial-lsr-thr-empty+
                         (io-port/8 (+ base +serial-LSR+)))
            (setf (io-port/8 (+ base +serial-THR+)) integer)
            (return))
       ;; Wait for FIFO to clear.
       #+nil(process-wait "Serial write"
                     (lambda ()
                       (logbitp +serial-lsr-thr-empty+
                                (io-port/8 (+ base +serial-LSR+))))))))

(defmethod sys.gray:stream-read-byte ((stream serial-stream))
  (let ((base (serial-stream-io-base stream)))
    (loop (when (logbitp +serial-lsr-data-available+
                         (io-port/8 (+ base +serial-LSR+)))
            (return (io-port/8 (+ base +serial-RBR+))))
       (process-wait "Serial read"
                     (lambda ()
                       (logbitp +serial-lsr-data-available+
                                (io-port/8 (+ base +serial-LSR+))))))))

(defclass serial-character-stream (sys.gray:fundamental-character-input-stream
                                   sys.gray:fundamental-character-output-stream)
  ((stream :initarg :stream :reader serial-stream-stream)))

(defvar *serial-one-char* (make-instance 'serial-character-stream :stream *serial-one*))
(defvar *serial-two-char* (make-instance 'serial-character-stream :stream *serial-two*))

(defmethod sys.gray:stream-read-char ((stream serial-character-stream))
  (code-char (read-byte (serial-stream-stream stream))))

(defmethod sys.gray:stream-write-char ((stream serial-character-stream) character)
  (write-byte (char-code character) (serial-stream-stream stream))
  (when (eql character #\Newline)
    (write-byte #x0D (serial-stream-stream stream))))
