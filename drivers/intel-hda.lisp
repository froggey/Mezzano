(cl:defpackage :intel-hda
  (:use #:cl))

(cl:in-package :intel-hda)

(defmacro define-register (name (size position) &rest slots-and-docstring)
  (let ((docstring (when (stringp (first slots-and-docstring))
                     (pop slots-and-docstring)))
        (reg-offset-constant (intern (format nil "+~A+" name))))
    `(progn
       (defconstant ,reg-offset-constant ,position ,@(when docstring (list docstring)))
       (defun ,(intern (format nil "PRINT-~A" name)) (register)
         (format t "~A: ~X~%" ',name register)
         ,@(loop for (slot-name (size position access docstring) . values) in slots-and-docstring
              collect `(let ((value (ldb (byte ,size ,position) register)))
                         (format t "  ~A: ~D (~X)" ',slot-name value value)
                         (case value
                           ,@(loop for (value value-name docstring) in values
                                collect `(,value (format t " ~A" ',value-name))))
                         (terpri))))
       ,@(loop for (slot-name (size position access docstring) . values) in slots-and-docstring
            for accessor-name = (intern (format nil "~A-~A" name slot-name))
            collect `(defparameter ,(intern (format nil "+~A-~A+" name slot-name)) (byte ,size ,position)
                       ,@(when docstring (list docstring)))
            collect `(defmacro ,accessor-name (place)
                       `(ldb (byte ,',size ,',position) ,place))
            append (loop for (value value-name docstring) in values
                      collect `(defconstant ,(intern (format nil "+~A-~A-~A+" name slot-name value-name))
                                 ,value ,@(when docstring (list docstring))))))))

(define-register gcap (2 #x00)
  "Global Capabilities."
  (oss  (4 12 :ro "Number of Output Streams Supported."))
  (iss  (4  8 :ro "Number of Input Streams Supported."))
  (bss  (5  3 :ro "Number of Bidirectional Streams Supported."))
  (nsdo (2  1 :ro "Number of Serial Data Out Signals."))
  (64ok (1  0 :ro "64 Bit Address Supported.")))

(define-register vmin (1 #x02) "Minor Version.")
(define-register vmaj (1 #x03) "Major Version.")

(define-register outpay (2 #x04) "Output Payload Capability.")
(define-register inpay  (2 #x06) "Input Payload Capability.")

(define-register gctl (4 #x08)
  "Global Control."
  (unsol  (1 8 :rw "Accept Unsolicited Response Enable."))
  (fcntrl (1 1 :rw "Flush Control."))
  (crst   (1 0 :rw "Controller Reset.")))

(define-register wakeen (2 #x0C) "Wake Enable.")
(define-register statest (2 #x0E) "Wake/State Change Status.")

(define-register gsts (2 #x10)
  "Global Status."
  (fsts (1 1 :rw "Flush Status.")))

(define-register outstrmpay (2 #x18) "Output Stream Payload Capability.")
(define-register instrmpay  (2 #x1A) "Input Stream Payload Capability.")

(define-register intctl (4 #x20)
  "Interrupt Control."
  (gie (1 31 :rw "Global Interrupt Enable."))
  (cie (1 30 :rw "Controller Interrupt Enable."))
  (sie (30 0 :rw "Stream Interrupt Enable.")))
(define-register intsts (4 #x24)
  "Interrupt Status."
  (gis (1 31 :ro "Global Interrupt State."))
  (cis (1 30 :ro "Controller Interrupt State."))
  (sis (30 0 :ro "Stream Interrupt State.")))

(define-register walclk (4 #x30) "Wall Clock Counter.")
(define-register ssync (4 #x38) "Stream Synchronization.")

(define-register corblbase (4 #x40) "CORB Lower Base Address.")
(define-register corbubase (4 #x44) "CORB Upper Base Address.")
(define-register corbwp (2 #x48) "CORB Write Pointer.")
(define-register corbrp (2 #x4A)
  "CORB Read Pointer."
  (corbrprst (1 15 :rw "CORB Read Pointer Reset."))
  (corbrp    (8 0 :ro "CORB Read Pointer.")))
(define-register corbctl (1 #x4C)
  "CORB Control."
  (corbrun (1 1 :rw "Enable CORB DMA Engine."))
  (cmeie   (1 0 :rw "CORB Memory Error Interrupt Enable.")))
(define-register corbsts (1 #x4D)
  "CORB Status."
  (cmei (1 0 :rw "CORB Memory Error Indication.")))
(define-register corbsize (1 #x4E)
  "CORB Size."
  (corbszcap (4 4 :ro "CORB Size Capability.")
    (#b0001 \8b "8 bytes, 2 entries.")
    (#b0010 \64b "64 bytes, 16 entries.")
    (#b0100 \1024b "1024 bytes, 256 entries."))
  (corbsize (2 0 :rw "CORB Size.")
    (#b00 \8b)
    (#b01 \64b)
    (#b10 \1024b)))

(define-register rirblbase (4 #x50) "RIRB Lower Base Address.")
(define-register rirbubase (4 #x54) "RIRB Upper Base Address.")
(define-register rirbwp (2 #x58)
  "RIRB Write Pointer."
  (rirbwprst (1 15 :wo "RIRB Write Pointer Reset."))
  (rirbwp    (8 0 :ro "RIRB Write Pointer.")))
(define-register rintcnt (2 #x5A) "Response Interrupt Count.")
(define-register rirbctl (1 #x5C)
  "RIRB Control."
  (rirboic   (1 2 :rw "Response Overrun Interrupt Control."))
  (rirbdmaen (1 1 :rw "RIRB DMA Enable."))
  (rintctl   (1 0 :rw "Response Interrupt Control.")))
(define-register rirbsts (1 #x5D)
  "RIRB Status."
  (rirbois   (1 2 :rw "Response Overrun Interrupt Status."))
  (rintfl    (1 0 :rw "Response Interrupt.")))
(define-register rirbsize (1 #x5E)
  "RIRB Size."
  (rirbszcap (4 4 :ro "RIRB Size Capability.")
    (#b0001 \16b "16 bytes, 2 entries.")
    (#b0010 \128b "128 bytes, 16 entries.")
    (#b0100 \2048b "2048 bytes, 256 entries."))
  (rirbsize (2 0 :rw "RIRB Size.")
    (#b00 \16b)
    (#b01 \128b)
    (#b10 \2048b)))

(define-register icoi (4 #x60) "Immediate Command Output Interface.")
(define-register icii (4 #x64) "Immediate Command Input Interface.")
(define-register icis (2 #x68)
  "Immediate Command Status."
  (irradd   (4 4 :ro "Immediate Response Result Address."))
  (irrunsol (1 3 :ro "Immediate Response Result Unsolicited."))
  (icv      (1 2 :ro "Immediate Command Version."))
  (irv      (1 1 :rw "Immediate Result Value."))
  (icb      (1 0 :rw "Immediate Command Busy.")))

(define-register dplbase (4 #x70)
  "DMA Position Buffer Lower Base."
  (enable (1 0 :rw "DMA Position Buffer Enable.")))
(define-register dpubase (4 #x74) "DMA Position Buffer Upper Base.")

(define-register sdnctlsts (4 #x0)
  "Stream Descriptor n Control and Status."
  (fifordy (1 29 :ro "FIFO Ready."))
  (dese    (1 28 :rw "Descriptor Error. Write 1 to clear."))
  (fifoe   (1 27 :rw "FIFO Error. Write 1 to clear."))
  (bcis    (1 26 :rw "Buffer Completion Interrupt Status. Write 1 to clear."))
  (strm    (4 20 :rw "Stream Number.")
    (0 unused))
  (dir     (1 19 :rw "Bidirectional Direction Control.")
    (0 input)
    (1 output))
  (tp      (1 18 "Traffic Priority."))
  (stripe  (2 16 :rw "Stripe Control. Ouput/bidi output only. 0 for input."))
  (deie    (1 4 :rw "Descriptor Error Interrupt Enable."))
  (feie    (1 3 :rw "FIFO Error Interrupt Enable."))
  (ioce    (1 2 :rw "Interrupt On Completion Enable."))
  (run     (1 1 :rw "Stream Run."))
  (srst    (1 0 :rw "Stream Reset.")))
(define-register sdnlpib (4 #x4) "SDn Link Position in Current Buffer.")
(define-register sdncbl (4 #x8) "SDn Cyclic Buffer Length.")
(define-register sdnlvi (2 #xC) "SDn Last Valid Index.")
(define-register sdnfifos (2 #x10) "SDn FIFO Size.")
(define-register sdnfmt (2 #x12)
  "SDn Format."
  (base (1 14 :rw "Sample Base Rate.")
    (0 \48khz)
    (1 \44.1khz))
  (mult (3 11 :rw "Sample Base Rate Multiple.")
    (#b000 x1)
    (#b001 x2)
    (#b010 x3)
    (#b011 x4))
  (div (3 8 :rw "Sample Base Rate Divisor."))
  (bits (3 4 :rw "Bits per Sample.")
    (#b000 \8bit)
    (#b001 \16bit)
    (#b010 \20bit)
    (#b011 \24bit)
    (#b100 \32bit))
  (chan (4 0 :rw "Number of Channels. Off by 1.")))
(define-register sdnbdpl (4 #x18) "SDn Buffer Descriptor List Pointer - Lower.")
(define-register sdnbdpu (4 #x1C) "SDn Buffer Descriptor List Pointer - Upper.")

(defstruct (bar (:area :static))
  address
  mmio)

;; FIXME: Some kind of limit?
(defun wrap-bar (pci-device bar)
  (let* ((bits (sys.int::pci-bar pci-device bar))
         (mmiop (not (logbitp 0 bits)))
         (address (logand bits (if mmiop
                                   (lognot 7)
                                   (lognot 3)))))
    (when mmiop (incf address #x8000000000))
    (make-bar :address address
              :mmio mmiop)))

;; Create REG/n accessors for BARs
(macrolet ((def (bitsize)
             (let ((place-name (intern (format nil "REG/~D" bitsize)))
                   (memref-name (intern (format nil "MEMREF-UNSIGNED-BYTE-~D" bitsize) "SYS.INT"))
                   (io-name (intern (format nil "IO-PORT/~D" bitsize) "SYS.INT")))
               `(progn
                  (defun ,place-name (dev offset)
                    (if (bar-mmio dev)
                        (,memref-name (+ (bar-address dev) offset) 0)
                        (,io-name (+ (bar-address dev) offset))))
                  (defun (setf ,place-name) (value dev offset)
                    (if (bar-mmio dev)
                        (setf (,memref-name (+ (bar-address dev) offset) 0) value)
                        (setf (,io-name (+ (bar-address dev) offset)) value)))))))
  (def 8) (def 16) (def 32) (def 64))

(defstruct (hda (:area :static))
  pci-device
  register-set
  ;; CORB, RIRB & DMA Position in Current Buffer share the same DMA allocation.
  ;; CORB starts at 0, RIRB starts at 1k and position starts at 3k.
  corb/rirb/dmap
  corb/rirb/dmap-physical
  corbsize
  rirbsize
  rirb-read-pointer
  )

(defconstant +maximum-n-streams+ 16)

(defconstant +corb-max-size+ 1024)
(defconstant +rirb-max-size+ 2048)
(defconstant +dmap-entry-size+ 4)
(defconstant +bdl-entry-size+ 16)
(defconstant +max-bdl-entries+ 256)

(defconstant +corb-offset+ 0)
(defconstant +rirb-offset+ (+ +corb-offset+ +corb-max-size+))
(defconstant +dmap-offset+ (+ +rirb-offset+ +rirb-max-size+))
(defconstant +bdl-offset+ (+ +dmap-offset+ (logand (+ (* +maximum-n-streams+ +dmap-entry-size+) 127)
                                                   (lognot 127))))
(defconstant +io-buffer-size+ (+ +bdl-offset+ (* +maximum-n-streams+ +bdl-entry-size+ +max-bdl-entries+)))

(defun n-input-streams (hda)
  (gcap-iss (reg/16 (hda-register-set hda) +gcap+)))

(defun n-output-streams (hda)
  (gcap-oss (reg/16 (hda-register-set hda) +gcap+)))

(defun n-bidirectional-streams (hda)
  (gcap-bss (reg/16 (hda-register-set hda) +gcap+)))

(defun input-stream-base (hda n)
  "Return the register offset for input stream N."
  (check-type n (integer 0))
  (assert (< n (n-input-streams hda)))
  (+ #x80 (* n #x20)))

(defun output-stream-base (hda n)
  "Return the register offset for output stream N."
  (check-type n (integer 0))
  (assert (< n (n-output-streams hda)))
  (+ #x80 (* (n-input-streams hda) #x20) (* n #x20)))

(defun bidirectional-stream-base (hda n)
  "Return the register offset for bidirectional stream N."
  (check-type n (integer 0))
  (assert (< n (n-bidirectional-streams hda)))
  (+ #x80 (* (n-input-streams hda) #x20) (* (n-output-streams hda) #x20) (* n #x20)))

(defun initialize-corb (hda)
  (let ((regs (hda-register-set hda)))
    ;; Stop the CORB if it's running and disable memory error interrupt.
    (setf (reg/8 regs +corbctl+) 0)
    ;; Configure the CORB size.
    (let ((corbsize (reg/8 regs +corbsize+)))
      (multiple-value-bind (thing buffer-size)
          (cond ((logtest (corbsize-corbszcap corbsize)
                          +corbsize-corbszcap-1024b+)
                 (values +corbsize-corbsize-1024b+ 1024))
                ((logtest (corbsize-corbszcap corbsize)
                          +corbsize-corbszcap-64b+)
                 (values +corbsize-corbsize-64b+ 64))
                ((logtest (corbsize-corbszcap corbsize)
                          +corbsize-corbszcap-8b+)
                 (values +corbsize-corbsize-8b+ 8))
                (t (format t "Unknown CORB size?~%")
                   (print-corbsize corbsize)
                   (error "Givin' up (CORBSIZE).")))
        (format t "Using CORBSIZE ~D (~D Bytes).~%" thing buffer-size)
        (setf (corbsize-corbsize corbsize) thing
              (reg/8 regs +corbsize+) corbsize)
        (setf (hda-corbsize hda) buffer-size)))
    ;; Initialize pointer. TODO: DMA64.
    (setf (reg/32 regs +corbubase+) 0
          (reg/32 regs +corblbase+) (+ (hda-corb/rirb/dmap-physical hda) +corb-offset+))
    ;; Reset CORB read pointer.
    (setf (corbrp-corbrprst (reg/16 regs +corbrp+)) 1)
    (setf (reg/16 regs +corbrp+) 0)
    ;; Reset CORB write pointer.
    (setf (reg/16 regs +corbwp+) 0)
    ;; TODO: Enable CMEIE.
    ;; Start the DMA engine.
    (setf (corbctl-corbrun (reg/8 regs +corbctl+)) 1)))

(defun initialize-rirb (hda)
  (let ((regs (hda-register-set hda)))
    ;; Stop the RIRB if it's running and disable memory error interrupt.
    (setf (reg/8 regs +rirbctl+) 0)
    ;; Configure RIRBSIZE.
    (let ((rirbsize (reg/8 regs +rirbsize+)))
      (multiple-value-bind (thing buffer-size)
          (cond ((logtest (rirbsize-rirbszcap rirbsize)
                          +rirbsize-rirbszcap-2048b+)
                 (values +rirbsize-rirbsize-2048b+ 2048))
                ((logtest (rirbsize-rirbszcap rirbsize)
                          +rirbsize-rirbszcap-128b+)
                 (values +rirbsize-rirbsize-128b+ 128))
                ((logtest (rirbsize-rirbszcap rirbsize)
                          +rirbsize-rirbszcap-16b+)
                 (values +rirbsize-rirbsize-16b+ 16))
                (t (format t "Unknown RIRB size?~%")
                   (print-rirbsize rirbsize)
                   (error "Givin' up (RIRBSIZE).")))
        (format t "Using RIRBSIZE ~D (~D Bytes).~%" thing buffer-size)
        (setf (rirbsize-rirbsize rirbsize) thing
              (reg/8 regs +rirbsize+) rirbsize)
        (setf (hda-rirbsize hda) buffer-size)))
    ;; Initialize pointer. TODO: DMA64.
    (setf (reg/32 regs +rirbubase+) 0
          (reg/32 regs +rirblbase+) (+ (hda-corb/rirb/dmap-physical hda) +rirb-offset+))
    ;; Reset RIRB write pointer.
    (setf (rirbwp-rirbwprst (reg/16 regs +rirbwp+)) 1)
    (setf (reg/16 regs +rirbwp+) 0)
    ;; And the software read pointer.
    (setf (hda-rirb-read-pointer hda) 0)
    ;; TODO: Enable interrupts.
    ;; Start the DMA engine.
    (setf (rirbctl-rirbdmaen (reg/8 regs +rirbctl+)) 1)))

(defconstant +corb-entry-size+ 4)
(defconstant +rirb-entry-size+ 8)

(defun read-rirb-entry (hda offset)
  (let* ((data (hda-corb/rirb/dmap hda))
         (base (+ (truncate +rirb-offset+ 4) (* offset 2))))
    (values (aref data base)
            (aref data (1+ base)))))

(defun poll-rirb (hda)
  (let* ((regs (hda-register-set hda))
         (rirbsize (rirbsize-rirbsize (reg/8 regs +rirbsize+)))
         (rirbwp (reg/16 regs +rirbwp+))
         (read-pointer (hda-rirb-read-pointer hda)))
    (when (not (eql read-pointer rirbwp))
      ;; Bits available!
      ;; Update the read pointer, then read the entry.
      (setf read-pointer (rem (1+ (hda-rirb-read-pointer hda))
                              (ecase rirbsize
                                (#.+rirbsize-rirbsize-16b+ 2)
                                (#.+rirbsize-rirbsize-128b+ 16)
                                (#.+rirbsize-rirbsize-2048b+ 256)))
            (hda-rirb-read-pointer hda) read-pointer)
      (read-rirb-entry hda read-pointer))))

(defun write-corb-entry (hda offset value)
  (let* ((data (hda-corb/rirb/dmap hda))
         (base (+ (truncate +corb-offset+ 4) offset)))
    (setf (aref data base) value)))

(defun send-corb (hda command)
  (let* ((regs (hda-register-set hda))
         (corbsize (corbsize-corbsize (reg/8 regs +corbsize+)))
         (corbrp (reg/16 regs +corbrp+))
         (corbwp (reg/16 regs +corbwp+))
         (next (rem (1+ corbwp)
                    (ecase corbsize
                      (#.+corbsize-corbsize-8b+ 2)
                      (#.+corbsize-corbsize-64b+ 16)
                      (#.+corbsize-corbsize-1024b+ 256)))))
    (when (eql corbrp next)
      ;; CORB is full, wait a bit.
      (return-from send-corb nil))
    (write-corb-entry hda next command)
    ;; Update write pointer.
    (setf (reg/16 regs +corbwp+) next)
    t))

(defun make-command (cad nid verb)
  (check-type cad (unsigned-byte 4))
  (check-type nid (unsigned-byte 8))
  (check-type verb (unsigned-byte 20))
  (logior (ash cad 28)
          (ash nid 20)
          verb))

(defun make-parameter (parameter)
  (check-type parameter (unsigned-byte 8))
  (logior #xF0000 parameter))

(define-register parameter-viddid (4 0)
  "Codec Vendor ID & Device ID."
  (vendor (16 16 :ro))
  (device (16 0 :ro)))

(define-register parameter-revision (4 2)
  "Codec Revision ID."
  (stepping (8 0 :ro "Optional vendor stepping number within the given Revision ID."))
  (vendor (8 8 :ro "Vendor's revision number for this Device ID."))
  (minor (4 16 :ro "Minor revision number."))
  (major (4 20 :ro "Major revision number.")))

(define-register parameter-subordinates (4 4)
  "Subordinate Node Count."
  (starting-node (8 16 :ro "Starting Node Number."))
  (total (8 0 :ro "Total Number of Nodes.")))

(define-register parameter-function-group (4 5) ; 5? It's what the spec says...
  "Function Group Type."
  (unsol-capable (1 8 :ro "Node is capable of generating unsolicited responses."))
  (node-type (8 0 :ro)
             (1 audio)
             (2 vendor-defined-modem)))

(define-register parameter-audio-function-group-capabilities (4 8)
  "Audio Function Group Capabilities."
  (output-delay (4 0 :ro))
  (input-delay (4 8 :ro))
  (beep-gen (1 16 :ro "A beep generator is present.")))

(define-register parameter-audio-widget-capabilities (4 9)
  "Audio Widget Capabilities."
  (chan-count-lsb (1 0 :ro))
  (in-amp-present (1 1 :ro))
  (out-amp-present (1 2 :ro))
  (amp-param-override (1 3 :ro))
  (format-override (1 4 :ro))
  (stripe (1 5 :ro))
  (proc-widget (1 6 :ro))
  (unsol-capable (1 7 :ro))
  (conn-list (1 8 :ro))
  (digital (1 9 :ro))
  (power-cntrl (1 10 :ro))
  (l-r-swap (1 11 :ro))
  (cp-caps (1 12 :ro))
  (chan-count-ext (3 13 :ro))
  (delay (4 16 :ro))
  (type (4 20 :ro)
        (0 output)
        (1 input)
        (2 mixer)
        (3 selector)
        (4 pin-complex)
        (5 power)
        (6 volume-knob)
        (7 beep-generator)
        (15 vendor-defined)))

(define-register parameter-pcm-size-rate (4 10)
  (bit-depth (5 16 :ro)
             (#b00001 b8)
             (#b00010 b16)
             (#b00100 b20)
             (#b01000 b24)
             (#b10000 b32))
  (rate (12 0 :ro)
        (#b000000000001  r1 "  8.0kHz   1/6 * 48")
        (#b000000000010  r2 " 11.025kHz 1/4 * 44.1")
        (#b000000000100  r3 " 16.0kHz   1/3 * 48")
        (#b000000001000  r4 " 22.05kHz  1/2 * 44.1")
        (#b000000010000  r5 " 32.0kHz   2/3 * 48")
        (#b000000100000  r6 " 44.1kHz")
        (#b000001000000  r7 " 48.0kHz")
        (#b000010000000  r8 " 88.2kHz   2/1 * 48.1")
        (#b000100000000  r9 " 96.0kHz   2/1 * 48")
        (#b001000000000 r10 "176.4kHz   4/1 * 48.1")
        (#b010000000000 r11 "192.0kHz   4/1 * 48")
        (#b100000000000 r12 "384.0kHz   8/1 * 48")))

(define-register parameter-stream-formats (4 11)
  (pcm (1 0 :ro))
  (float32 (1 1 :ro))
  (ac3 (1 2 :ro)))

(define-register parameter-pin-capabilities (4 12)
  (impedance-sense-capable (1 0 :ro))
  (trigger-reqd (1 1 :ro))
  (presence-detect-capable (1 2 :ro))
  (headphone-drive-capable (1 3 :ro))
  (output-capable (1 4 :ro))
  (input-capable (1 5 :ro))
  (balanced-io-pins (1 6 :ro))
  (hdmi (1 7 :ro))
  (vref-control (8 8 :ro))
  (eapd-capable (1 16 :ro))
  (dp (1 24 :ro "Display Port."))
  (hbr (1 27 :ro "High Bit Rate.")))

(define-register parameter-input-amplifier-capabilities (4 13)
  (offset (7 0 :ro))
  (num-steps (7 8 :ro))
  (step-size (7 16 :ro))
  (mute-capable (1 31 :ro)))

(define-register parameter-output-amplifier-capabilities (4 18)
  (offset (7 0 :ro))
  (num-steps (7 8 :ro))
  (step-size (7 16 :ro))
  (mute-capable (1 31 :ro)))

(define-register parameter-connection-list-length (4 14)
  (length (7 0 :ro))
  (long-form (1 7 :ro)))

(define-register parameter-power-states (4 15)
  (d0-sup (1 0 :ro))
  (d1-sup (1 1 :ro))
  (d2-sup (1 2 :ro))
  (d3-sup (1 3 :ro))
  (d3-cold-sup (1 4 :ro))
  (s3d3-cold-sup (1 29 :ro))
  (clkstop (1 30 :ro))
  (epss (1 31 :ro)))

(define-register parameter-processing-capabilities (4 16)
  (benign (1 0 :ro))
  (num-coeff (8 8 :ro)))

(define-register parameter-gpio-count (4 17)
  (num-gpios (8 0 :ro))
  (num-gpos (8 8 :ro))
  (num-gpis (8 16 :ro))
  (gpi-unsol (1 30 :ro))
  (gpi-wake (1 31 :ro)))

(define-register parameter-volume-knob-capabilities (4 19)
  (num-steps (7 0 :ro))
  (delta (1 7 :ro)))

(defconstant +root-nid+ 0)

(defun command (hda cad nid command)
  (send-corb hda (make-command cad nid command))
  (poll-rirb hda))

(defclass node ()
  ((nid :initarg :nid :reader nid)
   (cad :initarg :cad :reader cad)
   (hda :initarg :hda :reader hda)))

(defclass root-node (node)
  ((vendor-id :initarg :vendor-id :reader vendor-id)
   (device-id :initarg :device-id :reader device-id)
   (revision-id :initarg :revision-id :reader revision-id)
   (function-groups :initarg :function-groups :reader function-groups)))

(defclass function-group (node)
  (widgets :initarg :widgets :reader widgets))

(defclass audio-function-group (function-group)
  ())

(defclass widget (node)
  (function-group :initarg :function-group :reader function-group))

(defclass widget-connections-mixin ()
  (connections :initarg :connections :reader connections))

(defclass output-amp-mixin () ())
(defclass input-amp-mixin () ())

(defclass audio-widget (widget) ())

(defclass audio-output-converter (audio-widget output-amp-mixin)
  ())
(defclass audio-input-converter (audio-widget widget-connections-mixin input-amp-mixin)
  ())
(defclass audio-pin-complex (audio-widget widget-connections-mixin output-amp-mixin input-amp-mixin)
  ((pin-location :initarg :pin-location :reader pin-location)
   (pin-default-device :initarg :pin-default-device :reader pin-default-device)
   (pin-connection-type :initarg :pin-connection-type :reader pin-connection-type)
   (pin-colour :initarg :pin-colour :reader pin-colour)
  ))
(defclass audio-mixer (audio-widget widget-connections-mixin output-amp-mixin input-amp-mixin)
  ())
(defclass audio-selector (audio-widget widget-connections-mixin output-amp-mixin)
  ())
(defclass audio-power-control (audio-widget)
  ())
(defclass audio-volume-knob (audio-widget widget-connections-mixin)
  ())
(defclass audio-beep-generator (audio-widget)
  ())
(defclass audio-vendor-defined (audio-widget)
  ())

(defgeneric function-group-class (type))
(defmethod function-group-class (type)
  'function-group)

(defmethod function-group-class ((type (eql #.+parameter-function-group-node-type-audio+)))
  'audio-function-group)

(defgeneric widget-class (function-group type))
(defmethod widget-class (function-group type)
  'widget)

(defmethod widget-class ((function-group audio-widget)
                         (type (eql #.+parameter-audio-widget-capabilities-type-output+)))
  'audio-output-converter)
(defmethod widget-class ((function-group audio-widget)
                         (type (eql #.+parameter-audio-widget-capabilities-type-input+)))
  'audio-input-converter)
(defmethod widget-class ((function-group audio-widget)
                         (type (eql #.+parameter-audio-widget-capabilities-type-mixer+)))
  'audio-mixer)
(defmethod widget-class ((function-group audio-widget)
                         (type (eql #.+parameter-audio-widget-capabilities-type-selector+)))
  'audio-selector)
(defmethod widget-class ((function-group audio-widget)
                         (type (eql #.+parameter-audio-widget-capabilities-type-pin-complex+)))
  'audio-pin-complex)
(defmethod widget-class ((function-group audio-widget)
                         (type (eql #.+parameter-audio-widget-capabilities-type-power+)))
  'audio-power-control)
(defmethod widget-class ((function-group audio-widget)
                         (type (eql #.+parameter-audio-widget-capabilities-type-volume-knob+)))
  'audio-volume-knob)
(defmethod widget-class ((function-group audio-widget)
                         (type (eql #.+parameter-audio-widget-capabilities-type-beep-generator+)))
  'audio-beep-generator)
(defmethod widget-class ((function-group audio-widget)
                         (type (eql #.+parameter-audio-widget-capabilities-type-vendor-defined+)))
  'audio-vendor-defined)

(defun pin-location (location)
  "Decode a pin location byte."
  (case location
    (#x00 "External")
    (#x01 "Rear")
    (#x02 "Front")
    (#x03 "Left")
    (#x04 "Right")
    (#x05 "Top")
    (#x06 "Bottom")
    (#x07 "Rear panel")
    (#x08 "Drive bay")
    (#x10 "Internal")
    (#x17 "Riser")
    (#x18 "Digital display")
    (#x19 "ATAPI")
    (#x20 "External (seperate chassis)")
    (#x21 "Rear (seperate chassis)")
    (#x22 "Front (seperate chassis)")
    (#x23 "Left (seperate chassis)")
    (#x24 "Right (seperate chassis)")
    (#x25 "Top (seperate chassis)")
    (#x26 "Bottom (seperate chassis)")
    (#x30 "Other")
    (#x36 "Other bottom")
    (#x37 "Mobile Lid (inside)")
    (#x38 "Mobile Lid (outside)")
    (t (format nil "Unknown ~2,'0X" location))))

(defparameter *pin-default-device*
  #("Line Out" "Speaker" "HP Out" "CD" "SPDIF Out" "Digital Other Out" "Modem Line Side" "Modem Handset Side"
    "Line In" "AUX" "Mic In" "Telephony" "SPDIF In" "Digital Other In" "Reserved" "Other"))

(defparameter *pin-connection-type*
  #("Unknown"
    "1/8\" stereo/mono"
    "1/4\" stereo/mono"
    "ATAPI internal"
    "RCA"
    "Optical"
    "Other Digital"
    "Other Analog"
    "Multichannel Analog (DIN)"
    "XLR/Professional"
    "RJ-11 (Modem)"
    "Combination"
    "Unknown (12)"
    "Unknown (13)"
    "Unknown (14)"
    "Other"))

(defparameter *pin-colour*
  #("Unknown"
    "Black"
    "Grey"
    "Blue"
    "Green"
    "Red"
    "Orange"
    "Yellow"
    "Purple"
    "Pink"
    "Reserved (10)"
    "Reserved (11)"
    "Reserved (12)"
    "Reserved (13)"
    "White"
    "Other"))

(defmethod initialize-instance :after ((node root-node) &rest initargs)
  (let* ((subs (parameter node +parameter-subordinates+))
         (viddid (parameter node +parameter-viddid+))
         (revision (parameter node +parameter-revision+)))
    (setf (slot-value node 'vendor-id) (parameter-viddid-vendor viddid)
          (slot-value node 'device-id) (parameter-viddid-device viddid)
          (slot-value node 'revision-id) (list (parameter-revision-major revision)
                                               (parameter-revision-minor revision)
                                               (parameter-revision-vendor revision)
                                               (parameter-revision-stepping revision)))))

(defun enumerate-codec (hda cad)
  (let ((root-subs (command hda cad +root-nid+ (make-parameter +parameter-subordinates+)))
        (viddid (command hda cad +root-nid+ (make-parameter +parameter-viddid+))))
    (format t "ROOT ~D: ~4,'0X ~4,'0X~%" cad
            (parameter-viddid-vendor viddid)
            (parameter-viddid-device viddid))
    (loop for function-nid from (parameter-subordinates-starting-node root-subs)
       below (+ (parameter-subordinates-starting-node root-subs)
                (parameter-subordinates-total root-subs))
       do (let ((subs (command hda cad function-nid (make-parameter +parameter-subordinates+)))
                (type (command hda cad function-nid (make-parameter +parameter-function-group+))))
            (format t "  ~D ~A:~%"
                    function-nid
                    (case (parameter-function-group-node-type type)
                      (#.+parameter-function-group-node-type-audio+ 'audio)
                      (#.+parameter-function-group-node-type-vendor-defined-modem+ 'modem)
                      (t (parameter-function-group-node-type type))))
            (when (eql (parameter-function-group-node-type type) +parameter-function-group-node-type-audio+)
              (loop for widget-nid from (parameter-subordinates-starting-node subs)
                 below (+ (parameter-subordinates-starting-node subs)
                          (parameter-subordinates-total subs))
                 do (let ((caps (command hda cad widget-nid (make-parameter +parameter-audio-widget-capabilities+))))
                      (format t "    ~D ~A. Connected to: ~S~%" widget-nid
                              (case (parameter-audio-widget-capabilities-type caps)
                                (#.+parameter-audio-widget-capabilities-type-output+ 'output)
                                (#.+parameter-audio-widget-capabilities-type-input+ 'input)
                                (#.+parameter-audio-widget-capabilities-type-mixer+ 'mixer)
                                (#.+parameter-audio-widget-capabilities-type-selector+ 'selector)
                                (#.+parameter-audio-widget-capabilities-type-pin-complex+ 'pin-complex)
                                (#.+parameter-audio-widget-capabilities-type-power+ 'power)
                                (#.+parameter-audio-widget-capabilities-type-volume-knob+ 'volume-knob)
                                (#.+parameter-audio-widget-capabilities-type-beep-generator+ 'beep-generator)
                                (#.+parameter-audio-widget-capabilities-type-vendor-defined+ 'vendor-defined)
                                (t (parameter-audio-widget-capabilities-type caps)))
                              (get-connections hda cad widget-nid)))))))))

(defun get-connections (hda cad nid)
  (let* ((len (command hda cad nid (make-parameter +parameter-connection-list-length+)))
         (n-entries (parameter-connection-list-length-length len))
         (entries (if (not (zerop (parameter-connection-list-length-long-form len)))
                      (loop for i from 0 below (ceiling n-entries 2)
                         append (let ((x (command hda cad nid (logior #xF0200 (* i 2)))))
                                  (list (ldb (byte 16 0) x)
                                        (ldb (byte 16 16) x))))
                      (loop for i from 0 below (ceiling n-entries 4)
                         append (let ((x (command hda cad nid (logior #xF0200 (* i 4)))))
                                  (list (ldb (byte 8 0) x)
                                        (ldb (byte 8 8) x)
                                        (ldb (byte 8 16) x)
                                        (ldb (byte 8 24) x)))))))
    (sort (subseq entries 0 n-entries) #'<)))

(defun graph-connections (hda cad)
  (format t "digraph G {~%")
  (let ((root-subs (command hda cad +root-nid+ (make-parameter +parameter-subordinates+))))
    (loop for function-nid from (parameter-subordinates-starting-node root-subs)
       below (+ (parameter-subordinates-starting-node root-subs)
                (parameter-subordinates-total root-subs))
       do (let ((subs (command hda cad function-nid (make-parameter +parameter-subordinates+)))
                (type (command hda cad function-nid (make-parameter +parameter-function-group+))))
            (when (eql (parameter-function-group-node-type type) +parameter-function-group-node-type-audio+)
              (loop for widget-nid from (parameter-subordinates-starting-node subs)
                 below (+ (parameter-subordinates-starting-node subs)
                          (parameter-subordinates-total subs))
                 do (let* ((caps (command hda cad widget-nid (make-parameter +parameter-audio-widget-capabilities+)))
                           (connections (get-connections hda cad widget-nid))
                           (type (parameter-audio-widget-capabilities-type caps)))
                      (if (eql type +parameter-audio-widget-capabilities-type-pin-complex+)
                          (let ((config (command hda cad widget-nid #xF1C00)))
                            (format t "  ~D [label=\"Pin-Complex\\n~A\\n~A\\n~A\\n~A\"];~%"
                                    widget-nid
                                    (pin-location (ldb (byte 6 24) config))
                                    (aref *pin-default-device* (ldb (byte 4 20) config))
                                    (aref *pin-connection-type* (ldb (byte 4 16) config))
                                    (aref *pin-colour* (ldb (byte 4 12) config))))
                          (format t "  ~D [label=\"~:(~A~)\"];~%" widget-nid
                                  (case type
                                    (#.+parameter-audio-widget-capabilities-type-output+ 'output)
                                    (#.+parameter-audio-widget-capabilities-type-input+ 'input)
                                    (#.+parameter-audio-widget-capabilities-type-mixer+ 'mixer)
                                    (#.+parameter-audio-widget-capabilities-type-selector+ 'selector)
                                    (#.+parameter-audio-widget-capabilities-type-pin-complex+ 'pin-complex)
                                    (#.+parameter-audio-widget-capabilities-type-power+ 'power)
                                    (#.+parameter-audio-widget-capabilities-type-volume-knob+ 'volume-knob)
                                    (#.+parameter-audio-widget-capabilities-type-beep-generator+ 'beep-generator)
                                    (#.+parameter-audio-widget-capabilities-type-vendor-defined+ 'vendor-defined)
                                    (t (parameter-audio-widget-capabilities-type caps)))))
                      (if (eql type +parameter-audio-widget-capabilities-type-volume-knob+)
                          (dolist (input connections)
                            (format t "  ~D -> ~D;~%" widget-nid input))
                          (dolist (input connections)
                            (format t "  ~D -> ~D;~%" input widget-nid)))))))))
    (format t "}~%"))

(defvar *cards* '())
(setf *cards* '())

(defun probe (pci-device)
  (when (member pci-device *cards* :key #'hda-pci-device)
    (return-from probe))
  (let* ((bar0 (wrap-bar pci-device 0))
         (hda (make-hda :pci-device pci-device :register-set bar0)))
    (format t "Found Intel HDA controller at ~S.~%" pci-device)
    ;; Perform a controller reset by pulsing crst to 0.
    (setf (reg/32 bar0 +gctl+) 0
    ;; Wait for it to read back 0.
          (reg/32 bar0 +gctl+) (mask-field +gctl-crst+ -1))
    ;; Wait for it to read 1. FIXME: Timeouts...
    (loop while (zerop (gctl-crst (reg/32 bar0 +gctl+))))
    ;; Wait for the codecs to report in. 521µs.
    (loop while (< (reg/32 bar0 +walclk+) 14000))
    (format t "HDA version ~D.~D~%" (reg/8 bar0 +vmaj+) (reg/8 bar0 +vmin+))
    (print-gcap (reg/16 bar0 +gcap+))
    (format t "OUTPAY: ~D  INPAY: ~D~%"
            (reg/16 bar0 +outpay+) (reg/16 bar0 +inpay+))
    (format t "OUTSTRMPAY: ~D  INSTRMPAY: ~D~%"
            (reg/16 bar0 +outstrmpay+) (reg/16 bar0 +instrmpay+))
    (format t "STATEST: ~D~%" (reg/16 bar0 +statest+))
    (format t "Walclk1 ~D   Walclk1 ~D~%"
            (reg/32 bar0 +walclk+) (reg/32 bar0 +walclk+))
    (multiple-value-bind (array phys)
        (sys.int::allocate-dma-buffer +io-buffer-size+ 32)
      (setf (hda-corb/rirb/dmap hda) array
            (hda-corb/rirb/dmap-physical hda) phys))
    (print-icis (reg/16 bar0 +icis+))
    (initialize-corb hda)
    (initialize-rirb hda)
    ;; TODO: DMA64
    #+nil(setf (reg/32 bar0 +dpubase+) 0
          (reg/32 bar0 +dplbase+) (logior (+ (hda-corb/rirb/dmap-physical hda) +dmap-offset+)
                                          (mask-field +dplbase-enable+ -1)))
    (push hda *cards*)
    (poll-rirb hda)
    ))

(defparameter *hda-pci-ids*
  '((#x8086 #x2668) ; ICH6 HDA
    (#x8086 #x27D8))) ; ICH7 HDA

(sys.int::pci-register-driver *hda-pci-ids* 'probe)
(setf c (first *cards*))
