;;;; Copyright (c) 2015-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.driver.intel-hda
  (:use :cl))

(in-package :mezzano.driver.intel-hda)

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

(defconstant +maximum-n-streams+ 16)
(defconstant +maximum-n-codecs+ 15)

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

(defclass hda (mezzano.driver.sound:sound-card)
  ((pci-device :initarg :pci-device :accessor hda-pci-device)
   (register-set :initarg :register-set :accessor hda-register-set)
   ;; CORB, RIRB & DMA Position in Current Buffer share the same DMA allocation.
   ;; CORB starts at 0, RIRB starts at 1k and position starts at 3k.
   (corb/rirb/dmap :initarg :corb/rirb/dmap :accessor hda-corb/rirb/dmap)
   (corb/rirb/dmap-physical :initarg :corb/rirb/dmap-physical :accessor hda-corb/rirb/dmap-physical)
   (corbsize :initarg :corbsize :accessor hda-corbsize)
   (rirbsize :initarg :rirbsize :accessor hda-rirbsize)
   (rirb-read-pointer :initarg :rirb-read-pointer :accessor hda-rirb-read-pointer)
   (codecs :initarg :codecs :accessor hda-codecs)
   (interrupt-latch :initarg :interrupt-latch :accessor hda-interrupt-latch)
   (interrupt-handler :initarg :interrupt-handler :accessor hda-interrupt-handler)
   (dma-buffer-phys :initarg :dma-buffer-phys :accessor hda-dma-buffer-phys)
   (dma-buffer-virt :initarg :dma-buffer-virt :accessor hda-dma-buffer-virt)
   (dma-buffer-size :initarg :dma-buffer-size :accessor hda-dma-buffer-size))
  (:default-initargs :codecs (make-array 15 :initial-element nil)))

(define-condition device-disconnect () ())

(defun check-hda-presence (hda)
  (when (not (eql (mezzano.supervisor::pci-device-boot-id (hda-pci-device hda))
                  (mezzano.supervisor:current-boot-id)))
    (signal 'device-disconnect)))

(defmacro with-hda-access ((hda) &body body)
  `(mezzano.supervisor:with-snapshot-inhibited ()
     (check-hda-presence ,hda)
     ,@body))

(defun global-reg/8 (hda reg)
  (mezzano.supervisor:pci-io-region/8 (hda-register-set hda) reg))
(defun global-reg/16 (hda reg)
  (mezzano.supervisor:pci-io-region/16 (hda-register-set hda) reg))
(defun global-reg/32 (hda reg)
  (mezzano.supervisor:pci-io-region/32 (hda-register-set hda) reg))
(defun (setf global-reg/8) (value hda reg)
  (setf (mezzano.supervisor:pci-io-region/8 (hda-register-set hda) reg) value))
(defun (setf global-reg/16) (value hda reg)
  (setf (mezzano.supervisor:pci-io-region/16 (hda-register-set hda) reg) value))
(defun (setf global-reg/32) (value hda reg)
  (setf (mezzano.supervisor:pci-io-region/32 (hda-register-set hda) reg) value))

(defun sd-reg/8 (hda sd reg)
  (mezzano.supervisor:pci-io-region/8 (hda-register-set hda) (+ #x80 (* sd #x20) reg)))
(defun sd-reg/16 (hda sd reg)
  (mezzano.supervisor:pci-io-region/16 (hda-register-set hda) (+ #x80 (* sd #x20) reg)))
(defun sd-reg/32 (hda sd reg)
  (mezzano.supervisor:pci-io-region/32 (hda-register-set hda) (+ #x80 (* sd #x20) reg)))
(defun (setf sd-reg/8) (value hda sd reg)
  (setf (mezzano.supervisor:pci-io-region/8 (hda-register-set hda) (+ #x80 (* sd #x20) reg)) value))
(defun (setf sd-reg/16) (value hda sd reg)
  (setf (mezzano.supervisor:pci-io-region/16 (hda-register-set hda) (+ #x80 (* sd #x20) reg)) value))
(defun (setf sd-reg/32) (value hda sd reg)
  (setf (mezzano.supervisor:pci-io-region/32 (hda-register-set hda) (+ #x80 (* sd #x20) reg)) value))

(defun initialize-corb (hda)
  ;; Stop the CORB if it's running and disable memory error interrupt.
  (setf (global-reg/8 hda +corbctl+) 0)
  ;; Configure the CORB size.
  (let ((corbsize (global-reg/8 hda +corbsize+)))
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
            (global-reg/8 hda +corbsize+) corbsize)
      (setf (hda-corbsize hda) buffer-size)))
  ;; Initialize pointer.
  (let ((corb-address (+ (hda-corb/rirb/dmap-physical hda) +corb-offset+)))
    (setf (global-reg/32 hda +corbubase+) (ldb (byte 32 32) corb-address)
          (global-reg/32 hda +corblbase+) (ldb (byte 32 0) corb-address)))
  ;; Reset CORB read pointer.
  (setf (corbrp-corbrprst (global-reg/16 hda +corbrp+)) 1)
  (setf (global-reg/16 hda +corbrp+) 0)
  ;; Reset CORB write pointer.
  (setf (global-reg/16 hda +corbwp+) 0)
  ;; TODO: Enable CMEIE.
  ;; Start the DMA engine.
  (setf (corbctl-corbrun (global-reg/8 hda +corbctl+)) 1))

(defun initialize-rirb (hda)
  ;; Stop the RIRB if it's running and disable memory error interrupt.
  (setf (global-reg/8 hda +rirbctl+) 0)
  ;; Configure RIRBSIZE.
  (let ((rirbsize (global-reg/8 hda +rirbsize+)))
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
            (global-reg/8 hda +rirbsize+) rirbsize)
      (setf (hda-rirbsize hda) buffer-size)))
  ;; Initialize pointer. TODO: DMA64.
  (let ((rirb-address (+ (hda-corb/rirb/dmap-physical hda) +rirb-offset+)))
    (setf (global-reg/32 hda +rirbubase+) (ldb (byte 32 32) rirb-address)
          (global-reg/32 hda +rirblbase+) (ldb (byte 32 0) rirb-address)))
  ;; Reset RIRB write pointer.
  (setf (rirbwp-rirbwprst (global-reg/16 hda +rirbwp+)) 1)
  (setf (global-reg/16 hda +rirbwp+) 0)
  ;; And the software read pointer.
  (setf (hda-rirb-read-pointer hda) 0)
  ;; HACK: qemu requires this for RIBR responses to work, even though RIBR
  ;; interrupts aren't currently used.
  (setf (global-reg/16 hda +rintcnt+) #xFF)
  ;; TODO: Enable interrupts.
  ;; Start the DMA engine.
  (setf (rirbctl-rirbdmaen (global-reg/8 hda +rirbctl+)) 1))

(defconstant +corb-entry-size+ 4)
(defconstant +rirb-entry-size+ 8)

(defun read-rirb-entry (hda offset)
  (let* ((data (hda-corb/rirb/dmap hda))
         (base (+ (truncate +rirb-offset+ 4) (* offset 2))))
    (values (sys.int::memref-unsigned-byte-32 data base)
            (sys.int::memref-unsigned-byte-32 data (1+ base)))))

(defun poll-rirb (hda)
  (let* ((rirbsize (rirbsize-rirbsize (global-reg/8 hda +rirbsize+)))
         (rirbwp (global-reg/16 hda +rirbwp+))
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
    (setf (sys.int::memref-unsigned-byte-32 data base) value)))

(defun send-corb (hda command)
  (let* ((corbsize (corbsize-corbsize (global-reg/8 hda +corbsize+)))
         (corbrp (global-reg/16 hda +corbrp+))
         (corbwp (global-reg/16 hda +corbwp+))
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
    (setf (global-reg/16 hda +corbwp+) next)
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

(define-register parameter-function-group (4 5)
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
  (loop
     with time = (+ (get-universal-time) 2)
     until (> (get-universal-time) time)
     do (multiple-value-bind (x y)
            (poll-rirb hda)
          (when x
            (return (values x y))))
       finally (error "Timeout executing command. hda: ~S  cad: ~S  nid: ~S command: ~S." hda cad nid command)))

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
  ((root :initarg :root :reader root)
   (widgets :initarg :widgets :reader widgets)))

(defclass audio-function-group (function-group)
  ((outputs :initarg :outputs :reader outputs)
   (inputs :initarg :inputs :reader inputs))
  (:default-initargs :outputs '() :inputs '()))

(defclass widget (node)
  ((function-group :initarg :function-group :reader function-group)))

(defclass widget-connections-mixin ()
  ((connections :initarg :connections :reader connections)))
(defgeneric widget-connection-direction (widget)
  (:documentation "Directionality of this widget's connections.
One of :SINK, :SOURCE, :BIDIRECTIONAL, or :UNDIRECTED."))

(defclass output-amp-mixin () ())
(defclass input-amp-mixin () ())

(defclass audio-widget (widget) ())

;; Most audio widgets are sinks.
(defmethod widget-connection-direction (audio-widget)
  (declare (ignore widget))
  :sink)

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
(defmethod widget-connection-direction ((widget audio-volume-knob))
  (declare (ignore widget))
  :undirected)
(defclass audio-beep-generator (audio-widget)
  ())
(defclass audio-vendor-defined (audio-widget)
  ())

(defgeneric function-group-class (type))
(defmethod function-group-class (type)
  'function-group)

(defmethod function-group-class ((type (eql #.+parameter-function-group-node-type-audio+)))
  'audio-function-group)

(defgeneric widget-class (function-group nid))
(defmethod widget-class (function-group nid)
  'widget)

(defmethod widget-class ((function-group audio-function-group) nid)
  (let ((caps (command (hda function-group) (cad function-group) nid
                       (make-parameter +parameter-audio-widget-capabilities+))))
    (case (parameter-audio-widget-capabilities-type caps)
      (#.+parameter-audio-widget-capabilities-type-output+
       'audio-output-converter)
      (#.+parameter-audio-widget-capabilities-type-input+
       'audio-input-converter)
      (#.+parameter-audio-widget-capabilities-type-mixer+
       'audio-mixer)
      (#.+parameter-audio-widget-capabilities-type-selector+
       'audio-selector)
      (#.+parameter-audio-widget-capabilities-type-pin-complex+
       'audio-pin-complex)
      (#.+parameter-audio-widget-capabilities-type-power+
       'audio-power-control)
      (#.+parameter-audio-widget-capabilities-type-volume-knob+
       'audio-volume-knob)
      (#.+parameter-audio-widget-capabilities-type-beep-generator+
       'audio-beep-generator)
      (#.+parameter-audio-widget-capabilities-type-vendor-defined+
       'audio-vendor-defined)
      (t (call-next-method)))))

(defun decode-pin-location (location)
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
  #(:line-out :speaker :headphones-out :cd :spdif-out :digital-other-out :modem-line-side :modem-handset-side
    :line-in :aux :mic-in :telephony :spdif-in :digital-other-in :reserved :other))

(defparameter *pin-connection-type*
  #("Unknown"
    "1/8\'' stereo/mono"
    "1/4\'' stereo/mono"
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
  #(:unknown
    :black
    :grey
    :blue
    :green
    :red
    :orange
    :yellow
    :purple
    :pink
    :reserved-10
    :reserved-11
    :reserved-12
    :reserved-13
    :white
    :other))

(defun parameter (node parameter)
  (command (hda node) (cad node) (nid node) (make-parameter parameter)))

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


(defgeneric enumerate-widget (widget))

(defmethod enumerate-widget ((widget widget)))

(defmethod enumerate-widget :after ((widget widget-connections-mixin))
  (setf (slot-value widget 'connections)
        (loop for c in (get-connections widget)
           collect (find c (widgets (function-group widget)) :key #'nid))))

(defmethod enumerate-widget :after ((widget audio-pin-complex))
  (let ((config (command (hda widget) (cad widget) (nid widget) #xF1C00)))
    (setf (slot-value widget 'pin-location) (decode-pin-location (ldb (byte 6 24) config))
          (slot-value widget 'pin-default-device) (aref *pin-default-device* (ldb (byte 4 20) config))
          (slot-value widget 'pin-connection-type) (aref *pin-connection-type* (ldb (byte 4 16) config))
          (slot-value widget 'pin-colour) (aref *pin-colour* (ldb (byte 4 12) config)))))

(defun get-connections (widget)
  (let* ((hda (hda widget))
         (cad (cad widget))
         (nid (nid widget))
         (len (parameter widget +parameter-connection-list-length+))
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
    (subseq entries 0 n-entries)))

(defgeneric enumerate-function-group (function-group))

(defmethod enumerate-function-group ((function-group function-group))
  (let ((subs (parameter function-group +parameter-subordinates+)))
    (setf (slot-value function-group 'widgets)
          (loop for widget-nid from (parameter-subordinates-starting-node subs)
             below (+ (parameter-subordinates-starting-node subs)
                      (parameter-subordinates-total subs))
             collect (make-instance (widget-class function-group widget-nid)
                                    :nid widget-nid
                                    :cad (cad function-group)
                                    :hda (hda function-group)
                                    :function-group function-group))))
  (dolist (widget (widgets function-group))
    (enumerate-widget widget))
  function-group)

(defmethod enumerate-function-group :after ((function-group audio-function-group))
  ;; Discover interesting pins.
  ;; "Interesting" is entirely arbitrary and consists of headphones,
  ;; line-in, line-out and mic pins.
  (dolist (widget (widgets function-group))
    (when (typep widget 'audio-pin-complex)
      (case (pin-default-device widget)
        ((:line-out :headphones-out)
         (push widget (slot-value function-group 'outputs)))
        ((:line-in :mic-in)
         (push widget (slot-value function-group 'inputs)))))))

(defun enumerate-codec (hda cad)
  (let* ((root (make-instance 'root-node :nid +root-nid+ :cad cad :hda hda))
         (root-subs (parameter root +parameter-subordinates+)))
    (setf (slot-value root 'function-groups)
          (loop for function-nid from (parameter-subordinates-starting-node root-subs)
             below (+ (parameter-subordinates-starting-node root-subs)
                      (parameter-subordinates-total root-subs))
             collect (let* ((type (command hda cad function-nid (make-parameter +parameter-function-group+))))
                       (make-instance (function-group-class (parameter-function-group-node-type type))
                                      :nid function-nid
                                      :cad cad
                                      :hda hda
                                      :root root))))
    (dolist (fgroup (function-groups root))
      (enumerate-function-group fgroup))
    root))

(defgeneric graph-widget (widget))

(defmethod graph-widget ((widget widget))
  (format t "~D [label=\"~:(~A~) (~D)\"];~%"
          (nid widget) (type-of widget) (nid widget)))

(defmethod graph-widget :after ((widget widget-connections-mixin))
  (dolist (connection (connections widget))
    (ecase (widget-connection-direction widget)
      (:bidirectional
       (format t "~D -> ~D;~%" (nid connection) (nid widget))
       (format t "~D -> ~D;~%" (nid widget) (nid connection)))
      (:sink (format t "~D -> ~D;~%" (nid connection) (nid widget)))
      (:source (format t "~D -> ~D;~%" (nid widget) (nid connection)))
      (:undirected (format t "~D -> ~D [dir=\"none\"];~%" (nid widget) (nid connection))))))

(defmethod graph-widget ((widget audio-pin-complex))
  (format t "~D [label=\"~:(~A~) (~D)\\n~A\\n~:(~A~)\\n~A\\n~A\"];~%"
          (nid widget) (type-of widget) (nid widget)
          (pin-location widget)
          (pin-default-device widget)
          (pin-connection-type widget)
          (pin-colour widget)))

(defun graph-connections (root-node)
  (format t "digraph G {~%")
  (dolist (function-group (function-groups root-node))
    (dolist (widget (widgets function-group))
      (graph-widget widget)))
  (format t "}~%"))

(defun intel-hda-probe (device)
  ;; FIXME: Flush old cards first.
  (let* ((bar0 (mezzano.supervisor:pci-io-region device 0 #x3000))
         (hda (make-instance 'hda :pci-device device :register-set bar0)))
    (setf (hda-interrupt-latch hda) (mezzano.supervisor:make-latch "HDA IRQ latch"))
    (setf (hda-interrupt-handler hda) (mezzano.supervisor:make-simple-irq (mezzano.supervisor:pci-intr-line device) (hda-interrupt-latch hda)))
    (format t "Found Intel HDA controller at ~S.~%" device)
    (setf (mezzano.supervisor:pci-bus-master-enabled device) t)
    ;; Perform a controller reset by pulsing crst to 0.
    (format t "Begin reset.~%")
    (setf (global-reg/32 hda +gctl+) 0)
    ;; Wait for it to read back 0.
    (loop while (not (zerop (gctl-crst (global-reg/32 hda +gctl+)))))
    (format t "Leaving reset.~%")
    (setf (global-reg/32 hda +gctl+) (mask-field +gctl-crst+ -1))
    ;; Wait for it to read 1. FIXME: Timeouts...
    (loop while (zerop (gctl-crst (global-reg/32 hda +gctl+))))
    (format t "Waiting for codecs.~%")
    ;; Wait for the codecs to report in. 521µs.
    (loop while (< (global-reg/32 hda +walclk+) 14000))
    (format t "HDA version ~D.~D~%" (global-reg/8 hda +vmaj+) (global-reg/8 hda +vmin+))
    (print-gcap (global-reg/16 hda +gcap+))
    (format t "OUTPAY: ~D  INPAY: ~D~%"
            (global-reg/16 hda +outpay+) (global-reg/16 hda +inpay+))
    (format t "OUTSTRMPAY: ~D  INSTRMPAY: ~D~%"
            (global-reg/16 hda +outstrmpay+) (global-reg/16 hda +instrmpay+))
    (format t "STATEST: ~D~%" (global-reg/16 hda +statest+))
    (format t "Walclk1 ~D   Walclk1 ~D~%"
            (global-reg/32 hda +walclk+) (global-reg/32 hda +walclk+))
    (print-icis (global-reg/16 hda +icis+))
    (let* ((dma-phys (* (or (mezzano.supervisor::allocate-physical-pages
                             (ceiling +io-buffer-size+ mezzano.supervisor::+4k-page-size+)
                             :32-bit-only (zerop (gcap-64ok (global-reg/16 hda +gcap+))))
                            (error "Unable to allocate DMA buffer!"))
                        mezzano.supervisor::+4k-page-size+))
           (dma-virt (mezzano.supervisor::convert-to-pmap-address dma-phys)))
      (setf (hda-corb/rirb/dmap hda) dma-virt
            (hda-corb/rirb/dmap-physical hda) dma-phys))
    (let* ((buf-size #x20000)
           (buf-phys (* (or (mezzano.supervisor::allocate-physical-pages
                             (ceiling buf-size mezzano.supervisor::+4k-page-size+)
                             :32-bit-only (zerop (gcap-64ok (global-reg/16 hda +gcap+))))
                            (error "Unable to allocate stream buffer!"))
                        mezzano.supervisor::+4k-page-size+))
           (buf-virt (mezzano.supervisor::convert-to-pmap-address buf-phys)))
      (setf (hda-dma-buffer-size hda) buf-size
            (hda-dma-buffer-phys hda) buf-phys
            (hda-dma-buffer-virt hda) buf-virt))
    (mezzano.supervisor:simple-irq-attach (hda-interrupt-handler hda))
    (initialize-corb hda)
    (initialize-rirb hda)
    (let ((dmap-address (logior (+ (hda-corb/rirb/dmap-physical hda) +dmap-offset+)
                                (mask-field +dplbase-enable+ -1))))
      (setf (global-reg/32 hda +dpubase+) (ldb (byte 32 32) dmap-address)
            (global-reg/32 hda +dplbase+) (ldb (byte 32 0) dmap-address)))
    (loop
       with statest = (global-reg/16 hda +statest+)
       for i from 0 below +maximum-n-codecs+
       when (logbitp i statest)
       do (setf (aref (hda-codecs hda) i) (enumerate-codec hda i)))
    (mezzano.driver.sound:register-sound-card hda))
  t)

(mezzano.supervisor:define-pci-driver intel-hda intel-hda-probe
  ((#x8086 #x2668) ; ICH6 HDA
   (#x8086 #x27D8)) ; ICH7 HDA
  ())

(defun write-bdl (hda entry base length)
  (let ((array (hda-corb/rirb/dmap hda))
        (offset (+ (/ +bdl-offset+ 4) (* entry 4))))
    (setf (sys.int::memref-unsigned-byte-32 array (+ offset 0)) (ldb (byte 32 0) base)
          (sys.int::memref-unsigned-byte-32 array (+ offset 1)) (ldb (byte 32 32) base)
          (sys.int::memref-unsigned-byte-32 array (+ offset 2)) length
          ;; IOC
          (sys.int::memref-unsigned-byte-32 array (+ offset 3)) 1)))

(defun read-bdl (hda entry)
  (let ((array (hda-corb/rirb/dmap hda))
        (offset (+ (/ +bdl-offset+ 4) (* entry 4))))
    (values (logior (sys.int::memref-unsigned-byte-32 array (+ offset 0))
                    (ash (sys.int::memref-unsigned-byte-32 array (+ offset 0)) 32))
            (sys.int::memref-unsigned-byte-32 array (+ offset 2))
            (sys.int::memref-unsigned-byte-32 array (+ offset 3)))))

(defun dma-position (hda stream)
  (with-hda-access (hda)
    (let* ((phys (+ (hda-corb/rirb/dmap-physical hda) +dmap-offset+))
           (virt (+ mezzano.supervisor::+physical-map-base+ phys)))
      (sys.int::memref-unsigned-byte-32 virt (* stream 2)))))

(defun (setf dma-position) (value hda stream)
  (let* ((phys (+ (hda-corb/rirb/dmap-physical hda) +dmap-offset+))
         (virt (+ mezzano.supervisor::+physical-map-base+ phys)))
    (setf (sys.int::memref-unsigned-byte-32 virt (* stream 2)) value)))

(defun prep-stream (hda stream-id bdl-base bdl-length cb-length)
  (let ((bdl (+ (hda-corb/rirb/dmap-physical hda) +bdl-offset+ (* bdl-base 16))))
    (setf (sd-reg/32 hda stream-id +sdnbdpl+) (ldb (byte 32 0) bdl)
          (sd-reg/32 hda stream-id +sdnbdpu+) (ldb (byte 32 32) bdl))
    (setf (sd-reg/16 hda stream-id +sdnfmt+) #x4011
          (sd-reg/16 hda stream-id +sdnlvi+) bdl-length
          (sd-reg/32 hda stream-id +sdncbl+) cb-length)))

(defun stream-reset (hda stream-id)
  ;; Clear the run bit before doing anything.
  (setf (sd-reg/32 hda stream-id +sdnctlsts+)
        (logand (sd-reg/32 hda stream-id +sdnctlsts+)
                (lognot 2)))
  ;; Reset the stream.
  (setf (sd-reg/32 hda stream-id +sdnctlsts+) 1)
  (loop
     (when (logtest (sd-reg/32 hda stream-id +sdnctlsts+) 1)
       (return)))
  (setf (sd-reg/32 hda stream-id +sdnctlsts+) 0)
  (setf (global-reg/32 hda +intctl+) 0))

(defun stream-go (hda stream-id)
  ;; run, IoC enabled, stream number (tag) 1.
  (setf (sd-reg/32 hda stream-id +sdnctlsts+) #x00100006))

(defun first-input-stream (hda)
  (declare (ignore hda))
  0)

(defun first-output-stream (hda)
  (with-hda-access (hda)
    (gcap-iss (global-reg/16 hda +gcap+))))

(defun start-playback (hda buffer buffer-size codec dac pin &optional mixer)
  (with-hda-access (hda)
    (stream-reset hda (first-output-stream hda))
    (write-bdl hda 0 buffer (truncate buffer-size 2))
    (write-bdl hda 1 (+ buffer (truncate buffer-size 2)) (truncate buffer-size 2))
    (prep-stream hda (first-output-stream hda) 0 1 buffer-size)
    (when mixer
      (command hda codec mixer #x3F07F)) ; unmute L/R out/in
    (command hda codec dac #x70610) ; stream=1
    (command hda codec dac #x24011) ; format
    (command hda codec dac #x3b07f) ; unmute L/R out
    (command hda codec pin #x3b07f) ; unmute L/R out
    (command hda codec pin #x70740) ; enable output
    ;; Enable global and stream interrupts.
    (setf (global-reg/32 hda +intctl+) (logior #x80000000 (ash 1 (first-output-stream hda))))
    (stream-go hda (first-output-stream hda))))

(defun wait-for-buffer-interrupt (hda)
  (let ((latch (hda-interrupt-latch hda))
        (stream (first-output-stream hda)))
    (loop
       (with-hda-access (hda)
         (mezzano.supervisor:simple-irq-unmask (hda-interrupt-handler hda)))
       (mezzano.supervisor:latch-wait latch)
       (mezzano.supervisor:latch-reset latch)
       (with-hda-access (hda)
         (when (logbitp stream (global-reg/32 hda +intsts+))
           ;; qemu is picky and requires a write to the 8-bit status part of
           ;; the register.
           (setf (sd-reg/8 hda stream (+ +sdnctlsts+ 3)) (ash 1 2)) ; bcis
           (return))))))

;; Return a list of all pin widgets.
(defun pin-widgets (hda)
  (let ((result '()))
    (loop
       for codec-id below (length (hda-codecs hda))
       for codec = (aref (hda-codecs hda) codec-id)
       when codec
       do (dolist (function-group (function-groups codec))
            (dolist (widget (widgets function-group))
              (when (typep widget 'audio-pin-complex)
                (push widget result)))))
    result))

(defun pin-output-priority (pin)
  (case (pin-default-device pin)
    (:line-out
     (case (pin-colour pin)
       (:green 4)
       (t 2)))
    (:speaker
     (case (pin-colour pin)
       (:green 4)
       (t 0)))
    (:headphones-out
     (case (pin-colour pin)
       (:green 3)
       (t 1)))
    (t 0)))

(defun default-output-pin (hda)
  (first (sort (remove-if-not #'output-path (pin-widgets hda)) #'>
               :key #'pin-output-priority)))

(defun output-path (pin)
  "Return two values, an output converter and an optional mixer, for this pin.
Returns NIL if there is no output path."
  (let ((direct-converters (remove-if-not (lambda (x) (typep x 'audio-output-converter))
                                          (connections pin)))
        (direct-mixers (remove-if-not (lambda (x) (typep x 'audio-mixer))
                                      (connections pin))))
    (cond ((endp direct-converters)
           (dolist (mixer direct-mixers
                    (values nil nil))
             (let ((indirect-converters (remove-if-not (lambda (x) (typep x 'audio-output-converter))
                                                       (connections mixer))))
               (when indirect-converters
                 (return (values (first indirect-converters) mixer))))))
          (t
           (values (first direct-converters) nil)))))

;; TODO: This should stream to anything that looks vaugely output-like, instead
;; of a single pin.
(defmethod mezzano.driver.sound:sound-card-run ((hda hda) buffer-fill-callback)
  (handler-case
      (let* ((buffer (hda-dma-buffer-phys hda))
             (buf-len #x2000);(hda-dma-buffer-size hda))
             (half-buf-len (truncate buf-len 2))
             (n-samples (truncate half-buf-len 2)) ; buf-len is in bytes (2 per sample) and we want to fill only half the buffer at once
             (float-sample-buffer (make-array n-samples :element-type 'single-float))
             (output-pin (default-output-pin hda))
             (output-stream (first-output-stream hda))
             (buffer-offset 0)
             (stop-countdown nil))
        (labels ((store-sample (sample offset)
                   ;; Clamp to limits, don't wrap.
                   (let* ((sample-clamped (max (min sample 1.0f0) -1.0f0))
                          (sample-rescaled (if (< sample-clamped 0.0f0)
                                               (* sample-clamped 32768.0f0)
                                               (* sample-clamped 32767.0f0)))
                          (sample-16bit (truncate sample-rescaled)))
                     (declare (optimize speed (safety 0))
                              (type single-float sample-clamped sample-rescaled)
                              (type fixnum sample-16bit))
                     (setf (mezzano.supervisor::physical-memref-unsigned-byte-8 buffer (+ buffer-offset offset offset)) (ldb (byte 8 0) sample-16bit)
                           (mezzano.supervisor::physical-memref-unsigned-byte-8 buffer (+ buffer-offset offset offset 1)) (ldb (byte 8 8) sample-16bit))))
                 (refill-fifo ()
                   (with-hda-access (hda)
                     (locally
                         (declare (optimize speed (safety 0))
                                  (type (simple-array single-float (*)) float-sample-buffer)
                                  (type fixnum n-samples))
                       (dotimes (i n-samples)
                         (store-sample (aref float-sample-buffer i) i))))
                   (cond ((eql buffer-offset 0)
                          (setf buffer-offset half-buf-len))
                         (t
                          (setf buffer-offset 0)))
                   (cond ((funcall buffer-fill-callback float-sample-buffer 0 n-samples)
                          (setf stop-countdown nil))
                         ((not stop-countdown)
                          (setf stop-countdown 4)))))
          ;; Prepopulate the initial buffer.
          (funcall buffer-fill-callback float-sample-buffer 0 n-samples)
          ;; Clear the whole buffer.
          (with-hda-access (hda)
            (dotimes (i buf-len)
              (setf (mezzano.supervisor::physical-memref-unsigned-byte-8 buffer i) 0)))
          ;; Begin playback.
          (multiple-value-bind (converter mixer)
              (output-path output-pin)
            (start-playback hda buffer buf-len (cad output-pin) (nid converter) (nid output-pin) (and mixer (nid mixer))))
          (unwind-protect
               (loop
                  ;; Wait for the dma position to move from the
                  ;; current buffer to the other buffer.
                  (let* ((dmap (dma-position hda output-stream))
                         (current-offset (truncate dmap half-buf-len)))
                    (when (not (eql current-offset (truncate buffer-offset half-buf-len)))
                      (when stop-countdown
                        (when (zerop stop-countdown)
                          (return))
                        (decf stop-countdown))
                      ;; Refill buffer.
                      (refill-fifo)))
                  (wait-for-buffer-interrupt hda))
            (with-hda-access (hda)
              (stream-reset hda (first-output-stream hda))))))
    (device-disconnect ()
      (format t "HDA ~S disconnected.~%" hda)
      (throw 'mezzano.supervisor:terminate-thread nil))))
