;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
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

(defconstant +debug-serial-buffer-size+ 256)

(defvar *debug-serial-io-port*)
(defvar *debug-serial-irq*)
(defvar *debug-serial-tx-buffer*)
(defvar *debug-serial-tx-buffer-head*)
(defvar *debug-serial-tx-buffer-tail*)
(defvar *debug-serial-rx-fifo*)
(defvar *debug-serial-lock*)
(defvar *serial-at-line-start*)

(defmacro debug-fifo-push (value buffer buffer-size head tail)
  `(let ((next (1+ ,tail)))
     (when (>= next ,buffer-size)
       (setf next 0))
     (cond ((= next ,head)
            ;; When next reaches head, the buffer is full.
            nil)
           (t (setf (svref ,buffer ,tail) ,value
                    ,tail next)
              t))))

(defmacro debug-fifo-pop (buffer buffer-size head tail)
  `(if (eql ,head ,tail)
       (values nil nil)
       (values (prog1 (svref ,buffer ,head)
                 (incf ,head)
                 (when (>= ,head ,buffer-size)
                   (setf ,head 0)))
               t)))

(defmacro debug-fifo-emptyp (buffer buffer-size head tail)
  (declare (ignore buffer buffer-size))
  `(eql ,head ,tail))

(defmacro debug-fifo-fullp (buffer buffer-size head tail)
  (declare (ignore buffer))
  `(let ((next (1+ ,tail)))
     (when (>= next ,buffer-size)
       (setf next 0))
     (eql next ,head)))

(defun debug-serial-irq-handler (irq)
  (declare (ignore irq))
  (with-symbol-spinlock (*debug-serial-lock*)
    (let ((iir (sys.int::io-port/8 (+ *debug-serial-io-port* +serial-IIR+))))
      ;; Read any received data.
      (do ()
          ((not (logbitp +serial-lsr-data-available+
                         (sys.int::io-port/8 (+ *debug-serial-io-port* +serial-LSR+)))))
        (fifo-push (sys.int::io-port/8 (+ *debug-serial-io-port* +serial-RBR+))
                   *debug-serial-rx-fifo*
                   nil))
      ;; Refill the TX FIFO when it becomes empty.
      (when (logbitp +serial-lsr-thr-empty+
                     (sys.int::io-port/8 (+ *debug-serial-io-port* +serial-LSR+)))
        (loop
           ;; Push at most 16 bytes into the FIFO.
           for i below 16
           while (not (debug-fifo-emptyp *debug-serial-tx-buffer*
                                         +debug-serial-buffer-size+
                                         *debug-serial-tx-buffer-head*
                                         *debug-serial-tx-buffer-tail*))
           do
             (setf (sys.int::io-port/8 (+ *debug-serial-io-port* +serial-THR+))
                   (debug-fifo-pop *debug-serial-tx-buffer*
                                   +debug-serial-buffer-size+
                                   *debug-serial-tx-buffer-head*
                                   *debug-serial-tx-buffer-tail*))))
      ;; Turn the TX interrupt off when the TX buffer is empty.
      (when (debug-fifo-emptyp *debug-serial-tx-buffer*
                               +debug-serial-buffer-size+
                               *debug-serial-tx-buffer-head*
                               *debug-serial-tx-buffer-tail*)
        (setf (sys.int::io-port/8 (+ *debug-serial-io-port* +serial-IER+))
              +serial-ier-received-data-available+)))))

;; Low-level byte functions.

(defun debug-serial-write-byte (byte)
  (setf (sys.int::io-port/8 +bochs-log-port+) byte)
  (without-interrupts
    (loop
       (with-symbol-spinlock (*debug-serial-lock*)
         ;; Try to write directly to the serial port if the TX buffer is empty.
         (when (and (debug-fifo-emptyp *debug-serial-tx-buffer*
                                       +debug-serial-buffer-size+
                                       *debug-serial-tx-buffer-head*
                                       *debug-serial-tx-buffer-tail*)
                    (logbitp +serial-lsr-thr-empty+
                             (sys.int::io-port/8 (+ *debug-serial-io-port* +serial-LSR+))))
           (setf (sys.int::io-port/8 (+ *debug-serial-io-port* +serial-THR+))
                 byte)
           (return))
         ;; Write into the buffer, if not full.
         (when (not (debug-fifo-fullp *debug-serial-tx-buffer*
                                      +debug-serial-buffer-size+
                                      *debug-serial-tx-buffer-head*
                                      *debug-serial-tx-buffer-tail*))
           ;; If the buffer was empty, then the TX interrupt will have been
           ;; turned off. Turn it back on.
           (when (debug-fifo-emptyp *debug-serial-tx-buffer*
                                    +debug-serial-buffer-size+
                                    *debug-serial-tx-buffer-head*
                                    *debug-serial-tx-buffer-tail*)
             (setf (sys.int::io-port/8 (+ *debug-serial-io-port* +serial-IER+))
                   (logior +serial-ier-received-data-available+
                           +serial-ier-transmitter-holding-register-empty+)))
           (debug-fifo-push byte
                            *debug-serial-tx-buffer*
                            +debug-serial-buffer-size+
                            *debug-serial-tx-buffer-head*
                            *debug-serial-tx-buffer-tail*)
           (return)))
       ;; Delay until an interrupt, the serial port may have drained part of
       ;; the TX buffer after.
       ;; Use without-interrupts & stihlt/cli to avoid race conditions on
       ;; the local cpu (taking an interrupt between testing the buffer and
       ;; going to sleep). This is still not SMP-safe.
       (sys.int::%stihlt)
       (sys.int::%cli))))

(defun debug-serial-read-byte (&optional (waitp t))
  (fifo-pop *debug-serial-rx-fifo* waitp))

;; High-level character functions. These assume that whatever is on the other
;; end of the port uses UTF-8 with CRLF newlines.

(defun debug-serial-read-char ()
  (loop
     (let* ((leader (debug-serial-read-byte))
            (extra-bytes (cond ((eql (logand leader #b10000000) #b00000000) 0)
                               ((eql (logand leader #b11100000) #b11000000) 1)
                               ((eql (logand leader #b11110000) #b11100000) 2)
                               ((eql (logand leader #b11111000) #b11110000) 3)
                               ;; Reject 5 and 6 byte encodings, and continuation bytes
                               (t (return #\REPLACEMENT_CHARACTER)))))
       (dotimes (i extra-bytes)
         ;; FIXME: this should avoid consuming non-continuation bytes.
         ;; Need to add a peek-byte function, but also prevent other threads
         ;; reading characters between the two calls.
         (let ((byte (debug-serial-read-byte)))
           (unless (eql (logand byte #b11000000) #b10000000)
             (return #\REPLACEMENT_CHARACTER))
           (setf leader (logior (ash leader 6)
                                (logand byte #b00111111)))))
       ;; Reject overlong forms and non-unileader values.
       (when (or (cond ((< leader #x80) (not (eql extra-bytes 0)))
                       ((< leader #x800) (not (eql extra-bytes 1)))
                       ((< leader #x10000) (not (eql extra-bytes 2))))
                 (<= #xD800 leader #xDFFF)
                 (< #x10FFFF leader))
         (return #\REPLACEMENT_CHARACTER))
       ;; Ignore CR characters.
       (unless (eql leader #x0D)
         (return (code-char leader))))))

(defun debug-serial-write-char (char)
  (let ((code (char-code char)))
    (setf *serial-at-line-start* nil)
    ;; FIXME: Should write all the bytes to the buffer in one go.
    ;; Other processes may interfere.
    (cond ((eql char #\Newline)
           (setf *serial-at-line-start* t)
           ;; Turn #\Newline into CRLF
           (debug-serial-write-byte #x0D)
           (debug-serial-write-byte #x0A))
          ;; Encode as UTF-8, ignore any flag bits.
          ((< code #x80)
           (debug-serial-write-byte code))
          ((< code #x800)
           (debug-serial-write-byte (logior #b11000000 (ldb (byte 5 6) code)))
           (debug-serial-write-byte (logior #b10000000 (ldb (byte 6 0) code))))
          ((< code #x10000)
           (debug-serial-write-byte (logior #b11100000 (ldb (byte 4 12) code)))
           (debug-serial-write-byte (logior #b10000000 (ldb (byte 6 6) code)))
           (debug-serial-write-byte (logior #b10000000 (ldb (byte 6 0) code))))
          (t
           (debug-serial-write-byte (logior #b11110000 (ldb (byte 3 18) code)))
           (debug-serial-write-byte (logior #b10000000 (ldb (byte 6 12) code)))
           (debug-serial-write-byte (logior #b10000000 (ldb (byte 6 6) code)))
           (debug-serial-write-byte (logior #b10000000 (ldb (byte 6 0) code)))))))

(defun debug-serial-write-string (string)
  (dotimes (i (string-length string))
    (debug-serial-write-char (char string i))))

(defun debug-serial-stream (op &optional arg)
  (ecase op
    (:read-char (debug-serial-read-char))
    (:clear-input (fifo-reset *debug-serial-rx-fifo*))
    (:write-char (debug-serial-write-char arg))
    (:write-string (debug-serial-write-string arg))
    (:force-output)
    (:start-line-p *serial-at-line-start*)))

(defun initialize-debug-serial (io-port irq baud)
  (setf *debug-serial-io-port* io-port
        *debug-serial-irq* irq
        *debug-serial-lock* :unlocked
        *serial-at-line-start* t
        *debug-serial-tx-buffer* (sys.int::make-simple-vector +debug-serial-buffer-size+ :wired)
        *debug-serial-tx-buffer-head* 0
        *debug-serial-tx-buffer-tail* 0)
  (when (not (boundp '*debug-serial-rx-fifo*))
    (setf *debug-serial-rx-fifo* (make-fifo 50 :element-type '(unsigned-byte 8))))
  (fifo-reset *debug-serial-rx-fifo*)
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
  (debug-set-output-pseudostream 'debug-serial-stream)
  (i8259-hook-irq irq 'debug-serial-irq-handler)
  (i8259-unmask-irq irq))

(defvar *debug-early-serial-io-port*)

(defun debug-early-serial-write-byte (byte)
  (setf (sys.int::io-port/8 +bochs-log-port+) byte)
  ;; Wait for the TX buffer to drain.
  (loop
     (when (logbitp +serial-lsr-thr-empty+
                    (sys.int::io-port/8 (+ *debug-early-serial-io-port* +serial-LSR+)))
       (return)))
  ;; Write byte.
  (setf (sys.int::io-port/8 (+ *debug-early-serial-io-port* +serial-THR+)) byte))

(defun debug-early-serial-write-char (char)
  (let ((code (ldb (byte 8 0) (char-code char))))
    (cond ((eql char #\Newline)
           (setf *serial-at-line-start* t)
           ;; Turn #\Newline into CRLF
           (debug-early-serial-write-byte #x0D)
           (debug-early-serial-write-byte #x0A))
          (t (debug-early-serial-write-byte code)))))

(defun debug-early-serial-stream (op &optional arg)
  (ecase op
    (:write-char
     (debug-early-serial-write-char arg))
    (:write-string
     (dotimes (i (string-length arg))
       (debug-early-serial-write-char (char arg i))))
    (:force-output)
    (:start-line-p nil)))

(defun initialize-early-debug-serial (io-port)
  (setf *debug-early-serial-io-port* io-port)
  (debug-set-output-pseudostream 'debug-early-serial-stream))
