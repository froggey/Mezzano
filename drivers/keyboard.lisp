(in-package :sys.int)

(defstruct (ps/2-fifo (:area :static))
  (head 0 :type fixnum)
  (tail 0 :type fixnum)
  (buffer (make-array 500 :element-type '(unsigned-byte 8) :area :static)
          :read-only t)
  (semaphore (make-semaphore :name "PS/2 FIFO count" :area :static)
             :read-only t)
  (lock (make-spinlock :name "PS/2 FIFO lock" :area :static)
        :read-only t))

(defconstant +ps/2-data-port+ #x60)
(defconstant +ps/2-control-port+ #x64)
(defconstant +ps/2-key-irq+ 1)
(defconstant +ps/2-aux-irq+ 12)

(define-interrupt-handler ps/2-interrupt (fifo break-state &aux data x)
  (setf data (io-port/8 +ps/2-data-port+))
  (when break-state
    ;; Signal a break in the current stack-group when C-Esc is pressed.
    (case (car break-state)
      (nil) ; off
      (0 ; looking for #x1D (left ctrl)
       (if (eql data #x1D)
           (incf (car break-state))
           (setf (car break-state) 0)))
      (1 ; looking for #x01 (escape)
       (setf (car break-state) 0)
       (when (eql data #x01)
         (signal-break-from-interrupt
          *isa-pic-interrupted-stack-group*)))))
  (with-spinlock-held (ps/2-fifo-lock fifo)
    (setf x (1+ (ps/2-fifo-tail fifo)))
    (when (>= x (length (ps/2-fifo-buffer fifo)))
      (setf x 0))
    ;; When next reaches head, the buffer is full.
    (unless (= x (ps/2-fifo-head fifo))
      (setf (aref (ps/2-fifo-buffer fifo) (ps/2-fifo-tail fifo)) data
            (ps/2-fifo-tail fifo) x)
      (signal-semaphore (ps/2-fifo-semaphore fifo))))
  't)

(defvar *ps/2-key-fifo*)
(defvar *ps/2-aux-fifo*)
(defvar *ps/2-key-port-working* nil)
(defvar *ps/2-aux-port-working* nil)
(defvar *ps/2-break-control* nil)

(defun ps/2-fifo-empty (fifo)
  (with-interrupts-disabled ()
    (with-spinlock-held (ps/2-fifo-lock fifo)
      (eql (ps/2-fifo-head fifo) (ps/2-fifo-tail fifo)))))

(defun ps/2-pop-fifo (fifo)
  "Pop a byte from FIFO. Returns NIL if FIFO is empty!"
  (with-interrupts-disabled ()
    (with-spinlock-held (ps/2-fifo-lock fifo)
      (when (try-semaphore (ps/2-fifo-semaphore fifo))
        (prog1 (aref (ps/2-fifo-buffer fifo) (ps/2-fifo-head fifo))
          (incf (ps/2-fifo-head fifo))
          (when (>= (ps/2-fifo-head fifo) (length (ps/2-fifo-buffer fifo)))
            (setf (ps/2-fifo-head fifo) 0)))))))

(defun ps/2-read-fifo (fifo &optional timeout)
  (when (wait-on-semaphore (ps/2-fifo-semaphore fifo) :timeout timeout)
    ;; There's one byte in the FIFO for us now.
    (with-interrupts-disabled ()
      (with-spinlock-held (ps/2-fifo-lock fifo)
        (assert (not (eql (ps/2-fifo-head fifo) (ps/2-fifo-tail fifo))) (fifo)
                "FIFO empty after sem down?")
        (prog1 (aref (ps/2-fifo-buffer fifo) (ps/2-fifo-head fifo))
          (incf (ps/2-fifo-head fifo))
          (when (>= (ps/2-fifo-head fifo) (length (ps/2-fifo-buffer fifo)))
            (setf (ps/2-fifo-head fifo) 0)))))))

(defclass ps/2-keyboard-stream (sys.gray:fundamental-character-input-stream sys.gray:unread-char-mixin) ())

(defconstant +extended-scan-code+ #xE0)

;; (extended-scancode normal-key [shifted-key])
(defvar *extended-key-alist*
  '((#x5B :left-super)
    (#x1D :right-control)
    (#x5C :right-super)
    (#x38 :right-meta)
    (#x5D #\u0010401B) ; menu
    (#x52 #\u00104010) ; insert
    (#x47 #\u00104012) ; home
    (#x49 #\u00104014) ; page up
    (#x53 #\u00104011) ; delete
    (#x4F #\u00104013) ; end
    (#x51 #\u00104015) ; page down
    (#x48 #\u00104018) ; up arrow
    (#x4B #\u00104016) ; left arrow
    (#x50 #\u00104019) ; down arrow
    (#x4D #\u00104017) ; right arrow
    (#x35 #\u001040FB) ; KP divide
    (#x1C #\u001040FF) ; KP enter
    ))

(defvar *ps/2-keyboard-extended-key* nil)
(defvar *ps/2-keyboard-shifted* nil)
(defvar *ps/2-keyboard-ctrled* nil)
(defvar *ps/2-keyboard-metaed* nil)
(defvar *ps/2-keyboard-supered* nil)
(defvar *ps/2-keyboard-hypered* nil)

(defun ps/2-translate-scancode (scancode)
  (let ((key (svref (if *ps/2-keyboard-shifted*
                        *gb-keymap-high*
                        *gb-keymap-low*)
                    (logand scancode #x7F))))
    (when *ps/2-keyboard-extended-key*
      (setf *ps/2-keyboard-extended-key* nil)
      (let ((extended-key (assoc (logand scancode #x7F) *extended-key-alist*)))
        (cond ((null extended-key)
               (setf key nil))
              ((and *ps/2-keyboard-shifted* (third extended-key))
               (setf key (third extended-key)))
              (t (setf key (second extended-key))))))
    (cond ((= scancode +extended-scan-code+)
           (setf *ps/2-keyboard-extended-key* t)
           nil)
          ((= (logand scancode #x80) 0)
           ;; Key press.
           (cond ((member key '(:shift :left-shift :right-shift))
                  (setf *ps/2-keyboard-shifted* t)
                  nil)
                 ((member key '(:control :left-control :right-control))
                  (setf *ps/2-keyboard-ctrled* t)
                  nil)
                 ((member key '(:meta :left-meta :right-meta))
                  (setf *ps/2-keyboard-metaed* t)
                  nil)
                 ((member key '(:super :left-super :right-super))
                  (setf *ps/2-keyboard-supered* t)
                  nil)
                 ((member key '(:hyper :left-hyper :right-hyper))
                  (setf *ps/2-keyboard-hypered* t)
                  nil)
                 ((characterp key)
                  (when *ps/2-keyboard-ctrled*
                    (setf (char-bit key :control) t))
                  (when *ps/2-keyboard-metaed*
                    (setf (char-bit key :meta) t))
                  (when *ps/2-keyboard-supered*
                    (setf (char-bit key :super) t))
                  (when *ps/2-keyboard-hypered*
                    (setf (char-bit key :hyper) t))
                  key)
                 ((null key)
                  (write-string "Unknown keycode #x")
                  (sys.int::write-integer scancode 16)
                  (write-char #\/)
                  (sys.int::write-integer scancode)
                  nil)))
          (t ;; Key release.
           (case key
             ((:shift :left-shift :right-shift) (setf *ps/2-keyboard-shifted* nil))
             ((:control :left-control :right-control) (setf *ps/2-keyboard-ctrled* nil))
             ((:meta :left-meta :right-meta) (setf *ps/2-keyboard-metaed* nil))
             ((:super :left-super :right-super) (setf *ps/2-keyboard-supered* nil))
             ((:hyper :left-hyper :right-hyper) (setf *ps/2-keyboard-hypered* nil)))
           nil))))

(defun ps/2-read-char (fifo)
  (loop
     (let ((key (ps/2-translate-scancode (ps/2-read-fifo fifo))))
       (when key (return key)))))

(defmethod sys.gray:stream-read-char ((stream ps/2-keyboard-stream))
  (ps/2-read-char *ps/2-key-fifo*))

(defun keyboard-listen (fifo)
  (not (ps/2-fifo-empty fifo)))

(defmethod sys.gray:stream-listen ((stream ps/2-keyboard-stream))
  (keyboard-listen *ps/2-key-fifo*))

;;; PS/2 controller commands.
(defconstant +ps/2-read-config-byte+ #x20)
(defconstant +ps/2-write-config-byte+ #x60)
(defconstant +ps/2-disable-aux-port+ #xA7)
(defconstant +ps/2-enable-aux-port+ #xA8)
(defconstant +ps/2-test-aux-port+ #xA9)
(defconstant +ps/2-test-controller+ #xAA)
(defconstant +ps/2-test-key-port+ #xAB)
(defconstant +ps/2-disable-key-port+ #xAD)
(defconstant +ps/2-enable-key-port+ #xAE)
(defconstant +ps/2-read-controller-output-port+ #xD0)
(defconstant +ps/2-write-controller-output-port+ #xD1)
(defconstant +ps/2-write-key-port+ #xD2)
(defconstant +ps/2-write-aux-port+ #xD4)

;;; PS/2 status register bits.
(defconstant +ps/2-status-output-buffer-status+ (ash 1 0))
(defconstant +ps/2-status-input-buffer-status+ (ash 1 1))
(defconstant +ps/2-status-command/data+ (ash 1 3))
(defconstant +ps/2-status-timeout-error+ (ash 1 6))
(defconstant +ps/2-status-parity-error+ (ash 1 7))

;;; PS/2 config byte bits.
(defconstant +ps/2-config-key-interrupt+ (ash 1 0))
(defconstant +ps/2-config-aux-interrupt+ (ash 1 1))
(defconstant +ps/2-config-key-clock+ (ash 1 4))
(defconstant +ps/2-config-aux-clock+ (ash 1 5))
(defconstant +ps/2-config-key-translation+ (ash 1 6))

;;; PS/2 device commands and responses.
;;; Not directly related to the PS/2 controller, but close enough.
(defconstant +ps/2-reset+ #xFF)
(defconstant +ps/2-disable-scanning+ #xF5)
(defconstant +ps/2-identify+ #xF2)
(defconstant +ps/2-ack+ #xFA)
(defconstant +ps/2-resend+ #xFE)
(defconstant +ps/2-self-test-passed+ #xAA)

(defun ps/2-input-wait (&optional (what "data"))
  "Wait for space in the input buffer."
  ;; FIXME: Wait 1ms or something instead of this.
  (dotimes (i 100000
            (warn "PS/2: Timeout waiting for ~S." what))
    (when (zerop (logand (io-port/8 +ps/2-control-port+)
                         +ps/2-status-input-buffer-status+))
      (return t))))

(defun ps/2-output-wait (&optional (what "data"))
  "Wait for data to become available in the output buffer."
  ;; FIXME: Wait 1ms or something instead of this.
  (dotimes (i 100000
            (warn "PS/2: Timeout waiting for ~S." what))
    (when (not (zerop (logand (io-port/8 +ps/2-control-port+)
                              +ps/2-status-output-buffer-status+)))
      (return t))))

;;; FIXME: should return timeout status.
(defun ps/2-send-key-byte (byte)
  "Send a byte to the device attached to the KEY port."
  (ps/2-input-wait "KEY port")
  (setf (io-port/8 +ps/2-data-port+) byte))
(defun ps/2-send-aux-byte (byte)
  "Send a byte to the device attached to the AUX port."
  (ps/2-input-wait "AUX port command")
  (setf (io-port/8 +ps/2-control-port+) +ps/2-write-aux-port+)
  (ps/2-input-wait "AUX port")
  (setf (io-port/8 +ps/2-data-port+) byte))

(defun ps/2-read-config ()
  "Read the PS/2 configuration byte."
  (ps/2-input-wait "config byte command")
  (setf (io-port/8 +ps/2-control-port+) +ps/2-read-config-byte+)
  (ps/2-output-wait "config byte")
  (io-port/8 +ps/2-data-port+))

(defun ps/2-write-config (byte)
  "Write the PS/2 configuration byte."
  (ps/2-input-wait "config byte command")
  (setf (io-port/8 +ps/2-control-port+) +ps/2-write-config-byte+)
  (ps/2-input-wait "config byte")
  (setf (io-port/8 +ps/2-data-port+) byte))

(defun reset-ps/2 ()
  ;; Before we begin, mask the KEY and AUX IRQs.
  (setf (isa-pic-irq-mask +ps/2-key-irq+) t
        (isa-pic-irq-mask +ps/2-aux-irq+) t)
  ;; Disable devices.
  (ps/2-input-wait "disable key port")
  (setf (io-port/8 +ps/2-control-port+) +ps/2-disable-key-port+)
  (ps/2-input-wait "disable aux port")
  (setf (io-port/8 +ps/2-control-port+) +ps/2-disable-aux-port+)
  ;; Flush the output buffer.
  (do () ((not (logtest (io-port/8 +ps/2-control-port+)
                        +ps/2-status-output-buffer-status+)))
    (io-port/8 +ps/2-data-port+))
  (let ((config-byte (ps/2-read-config))
        (single-channelp nil))
    ;; If the aux clock bit is clear, then it can't be a dual-channel controller.
    (unless (logtest config-byte +ps/2-config-aux-clock+)
      (setf single-channelp t))
    ;; Disable the key and aux interrupts and key translation.
    (ps/2-write-config (logand config-byte
                               (lognot (logior +ps/2-config-key-interrupt+
                                               +ps/2-config-aux-interrupt+
                                               +ps/2-config-key-translation+))))
    ;; Perform a self-test.
    (ps/2-input-wait "self-test")
    (setf (io-port/8 +ps/2-control-port+) +ps/2-test-controller+)
    (ps/2-output-wait "self-test")
    (let ((result (io-port/8 +ps/2-data-port+)))
      (unless (eql result #x55)
        (warn "PS/2: Self test failed: ~2,'0X! Blazing ahead anyway..." result)))
    ;; Look for the second channel, unless we already know there isn't one.
    (unless single-channelp
      (ps/2-input-wait "enable-aux")
      (setf (io-port/8 +ps/2-control-port+) +ps/2-enable-aux-port+)
      ;; This bit should be clear now, for a dual-channel controller.
      (setf config-byte (ps/2-read-config))
      (when (logtest config-byte +ps/2-config-aux-clock+)
        (setf single-channelp t))
      (ps/2-input-wait "disable-aux")
      (setf (io-port/8 +ps/2-control-port+) +ps/2-disable-aux-port+))
    ;; Test the ports.
    (ps/2-input-wait "key test")
    (setf (io-port/8 +ps/2-control-port+) +ps/2-test-key-port+)
    (ps/2-output-wait "KEY port test")
    (let ((result (io-port/8 +ps/2-data-port+)))
      (cond ((eql result #x00)
             (setf *ps/2-key-port-working* t))
            (t (setf *ps/2-key-port-working* nil)
               (warn "PS/2: KEY port test failed: ~2,'0X!" result))))
    (setf *ps/2-aux-port-working* (not single-channelp))
    (unless single-channelp
      (ps/2-input-wait "aux test")
      (setf (io-port/8 +ps/2-control-port+) +ps/2-test-aux-port+)
      (ps/2-output-wait "AUX port test")
      (let ((result (io-port/8 +ps/2-data-port+)))
        (cond ((eql result #x00)
               (setf *ps/2-aux-port-working* t))
              (t (setf *ps/2-aux-port-working* nil)
                 (warn "PS/2: AUX port test failed: ~2,'0X!" result)))))
    ;; Ports have been detected, enable them and their IRQs.
    (ps/2-write-config (logior (ps/2-read-config)
                               (if *ps/2-key-port-working* +ps/2-config-key-interrupt+ 0)
                               (if *ps/2-aux-port-working* +ps/2-config-aux-interrupt+ 0)))
    (setf *ps/2-key-port-working* t
          *ps/2-aux-port-working* t)
    ;; Enable ports.
    (when *ps/2-key-port-working*
      (ps/2-input-wait "enable key")
      (setf (io-port/8 +ps/2-control-port+) +ps/2-enable-key-port+)
      (setf (isa-pic-irq-mask +ps/2-key-irq+) nil))
    (when *ps/2-aux-port-working*
      (ps/2-input-wait "enable aux")
      (setf (io-port/8 +ps/2-control-port+) +ps/2-enable-aux-port+)
      (setf (isa-pic-irq-mask +ps/2-aux-irq+) nil))
    ;; Flush data FIFOs.
    (ps/2-flush-input :key)
    (ps/2-flush-input :aux)))

(defun ps/2-write (port byte)
  (ecase port
    (:key (ps/2-send-key-byte byte))
    (:aux (ps/2-send-aux-byte byte))))

(defun ps/2-read (port &optional timeout)
  (ps/2-read-fifo (ecase port
                    (:key *ps/2-key-fifo*)
                    (:aux *ps/2-aux-fifo*))
                  (if (eql timeout t) 10000 timeout)))

(defun ps/2-flush-input (port)
  (ecase port
    (:key (loop (unless (ps/2-pop-fifo *ps/2-key-fifo*) (return))))
    (:aux (loop (unless (ps/2-pop-fifo *ps/2-aux-fifo*) (return))))))

(defun ps/2-send-command (port byte)
  "Send a PS/2 command and respond to resend replies."
  (let ((attempts 10))
    (loop
       (when (<= (decf attempts) 0)
         (warn "PS/2 send command #x~2,'0X failed, too many resends." byte)
         (return (values nil :resend-limit-exceeded)))
       (ps/2-write port byte)
       (let ((reply (ps/2-read port t)))
         (case reply
           (#.+ps/2-ack+ (return t))
           (#.+ps/2-resend+)
           ((nil) (return (values nil :timeout)))
           (t (warn "PS/2 send command #x~2,'0X got unknown response #x~2,'0X." byte reply)
              (return (values nil byte))))))))

(defun ps/2-identify (port)
  (multiple-value-bind (successp reason)
      (ps/2-send-command port +ps/2-disable-scanning+)
    (unless successp
      (warn "PS/2 identify failed: ~2,'0X~%" reason)
      (return-from ps/2-identify nil)))
  (multiple-value-bind (successp reason)
      (ps/2-send-command port +ps/2-identify+)
    (unless successp
      (warn "PS/2 identify failed: ~2,'0X~%" reason)
      (return-from ps/2-identify nil)))
  ;; Read up to two bytes.
  (let ((byte-one (ps/2-read port t)))
    (case byte-one
      (#x00 :standard-mouse)
      (#x03 :scroll-mouse)
      (#x04 :5-button-mouse)
      (#xAB (let ((byte-two (ps/2-read port t)))
              (case byte-two
                ((#x41 #xC1) :mf2-keyboard-translated)
                ((#x83) :mf2-keyboard)
                ((nil) byte-one)
                (t (list byte-one byte-two)))))
      ((nil) :at-keyboard)
      (t byte-one))))

(defun init-keyboard (port)
  (ps/2-flush-input port)
  ;; Expecting ACK followed by Self-Test-Passed.
  (unless (ps/2-send-command port +ps/2-reset+)
    (warn "PS/2 keyboard reset failed.~%")
    (return-from init-keyboard))
  (unless (eql (ps/2-read port t) +ps/2-self-test-passed+)
    (warn "PS/2 keyboard reset failed.~%")
    (return-from init-keyboard))
  (format t "Detected ~S on PS/2 ~S port.~%" (ps/2-identify port) port)
  ;; Switch to scancode set 1.
  (unless (ps/2-send-command port #xF0)
    (warn "PS/2 keyboard scancode change failed (1).~%")
    (return-from init-keyboard))
  (unless (ps/2-send-command port 1)
    (warn "PS/2 keyboard scancode change failed (2).~%")
    (return-from init-keyboard))
  (unless (ps/2-send-command port #xF4)
    (warn "PS/2 keyboard start reporting failed.~%")
    (return-from init-keyboard)))

(defun ps/2-mouse-command (port command)
  (multiple-value-bind (successp result)
      (ps/2-send-command port command)
    (unless successp
      (warn "PS/2: Error controlling mouse, expected ACK(FA) got ~2,'0X~%" result))))

(defun init-mouse (port)
  #+nil(progn
  (ps/2-flush-input port)
  (ps/2-write port +ps/2-reset+)
  (unless (eql (ps/2-read port t) +ps/2-ack+)
    (warn "PS/2 mouse reset change failed (1).~%")))
  #+nil(unless (eql (ps/2-read port t) #xAA)
    (warn "PS/2 mouse reset change failed (2).~%"))
  #+nil(unless (eql (ps/2-read port t) 0)
    (warn "PS/2 mouse reset change failed (3).~%"))
  (ps/2-flush-input port)
  (format t "Detected ~S on PS/2 ~S port.~%" (ps/2-identify port) port)
  ;; Set mouse defaults.
  (ps/2-mouse-command port #xF6)
  ;; Enable mouse data reporting.
  (ps/2-mouse-command port #xF4))

(defun init-ps/2 ()
  ;; Install IRQ handlers.
  (setf *ps/2-break-control* (cons-in-area 0 nil :static)
        *ps/2-key-fifo* (make-ps/2-fifo)
        (isa-pic-interrupt-handler +ps/2-key-irq+) (make-interrupt-handler 'ps/2-interrupt
                                                                           *ps/2-key-fifo*
                                                                           *ps/2-break-control*)
        *ps/2-aux-fifo* (make-ps/2-fifo)
        (isa-pic-interrupt-handler +ps/2-aux-irq+) (make-interrupt-handler 'ps/2-interrupt
                                                                           *ps/2-aux-fifo*
                                                                           nil))
  ;; Reset keyboard state.
  (setf *ps/2-keyboard-extended-key* nil
        *ps/2-keyboard-shifted* nil
        *ps/2-keyboard-ctrled* nil
        *ps/2-keyboard-metaed* nil
        *ps/2-keyboard-supered* nil
        *ps/2-keyboard-hypered* nil)
  ;; Reset the controller.
  (reset-ps/2)
  ;; Initialize devices.
  (when *ps/2-key-port-working*
    (init-keyboard :key))
  (when *ps/2-aux-port-working*
    (init-mouse :aux)))

(add-hook '*initialize-hook* 'init-ps/2)
