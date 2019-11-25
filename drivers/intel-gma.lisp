;;;; Intel Graphics Media Accelerator driver.
;;;; Targeting generation 3 devices, specifically the GMA 950.

;;; "Intel® 945G/945GZ/945GC/945P/945PL Express Chipset Family"
;;; https://www.intel.com/Assets/PDF/datasheet/307502.pdf
;;;
;;; There is no documentation available for the GMA 950, but the GMA 3000 is
;;; similar enough to be compatible.
;;;
;;; "Intel® 965 Express Chipset Family and Intel® G35 Express Chipset Graphics Controller PRM"
;;; "Volume 1: Graphics Core"
;;; https://01.org/sites/default/files/documentation/965_g35_vol_1_graphics_core.pdf
;;; "Volume 2: 3D/Media"
;;; https://01.org/sites/default/files/documentation/965_g35_vol_2_3d_media_web_updated_0.pdf
;;; "Volume 3: Display Registers"
;;; https://01.org/sites/default/files/documentation/965_g35_vol_3_display_registers_updated_0.pdf
;;; "Volume 4: Subsystem and Cores"
;;; https://01.org/sites/default/files/documentation/965_g35_vol_4_subsystem_core_0.pdf
;;;
;;; Pipe = Generates display timing signals
;;;
;;; PIPE{A,B}CONF.PipeState don't appear to be present in GMA 950.
;;; What selects between FPA0 and FPA1?
;;; Changing DSPACNTR doesn't seem to do anything.
;;; Changing DSPBCNTR doesn't do anything good.
;;;
;;; Documentation seems to lie about how Display register double buffering works.
;;; It claims that writeback is triggered when the DSP{A,B}SURF register is
;;; written, but it seems that it's actually the DSP{A,B}LINOFF register that
;;; causes this to happen.

(defpackage :mezzano.driver.intel-gma
  (:use :cl)
  (:local-nicknames (:pci :mezzano.supervisor.pci)
                    (:sup :mezzano.supervisor)
                    (:sync :mezzano.sync)))

(in-package :mezzano.driver.intel-gma)

(defvar *card*) ; for testing.

(defclass intel-gma ()
  ((%device :initarg :device :reader gma-device)
   (%registers :initarg :registers :reader gma-registers)
   (%gm :initarg :gm :reader gma-gm)
   (%gtt :initarg :gtt :reader gma-gtt)
   (%edid :initarg :edid :reader gma-edid)
   (%lock :reader gma-lock)))

(defmethod initialize-instance :after ((instance intel-gma) &key)
  (setf (slot-value instance '%lock) (sup:make-mutex instance)))

;; PCI BAR indices.
(defconstant +pci-bar-mmadr+ 0)
(defconstant +pci-bar-iobar+ 1)
(defconstant +pci-bar-gmadr+ 2)
(defconstant +pci-bar-gttadr+ 3)

;; Additional PCI config registers.
(defconstant +pci-config-mggc+ #x52 "GMCH Graphics Control")
(defconstant +mggc-gms+ (byte 3 4) "Graphics Mode Select")
(defconstant +mggc-gms-none+ #b000)
(defconstant +mggc-gms-1mb+ #b001)
(defconstant +mggc-gms-8mb+ #b011)
(defconstant +mggc-ivd+ (byte 1 1) "IGD VGA Disable")
(defconstant +pci-config-mdeven+ #x54 "Device Enable")
(defconstant +pci-config-bsm+ #x5C "Base of Stolen Memory")

;;; Clock Control and Power Management Registers
(defconstant +vga0+              #x06000)
(defconstant +vga1+              #x06004)
(defconstant +vga-pd+            #x06010)
(defconstant +dplla-ctrl+        #x06014)
(defconstant +dpllb-ctrl+        #x06018)
(defconstant +dpllamd+           #x0601C)
(defconstant +dpllbmd+           #x06020)
(defconstant +fpa0+              #x06040)
(defconstant +fpa1+              #x06044)
(defconstant +fpb0+              #x06048)
(defconstant +fpb1+              #x0604C)
(defconstant +dpll-test+         #x0606C)
(defconstant +d-state+           #x06104)
(defconstant +dspclk-gate-d+     #x06200)
(defconstant +renclk-gate-d1+    #x06204)
(defconstant +renclk-gate-d2+    #x06208)
(defconstant +ramclk-gate-d+     #x06210)
(defconstant +deuc+              #x06214)

;;; I/O Control Registers
(defconstant +gpio-ctl0+         #x05010)
(defconstant +gpio-ctl1+         #x05014)
(defconstant +gpio-ctl2+         #x05018)
(defconstant +gpio-ctl3+         #x0501C)
(defconstant +gpio-ctl4+         #x05020)
(defconstant +gpio-ctl5+         #x05024)
(defconstant +gpio-ctl6+         #x05028)
(defconstant +gpio-ctl7+         #x0502C)
(defconstant +gpio-gmbus0+       #x05100)
(defconstant +gpio-gmbus1+       #x05104)
(defconstant +gpio-gmbus2+       #x05108)
(defconstant +gpio-gmbus3+       #x0510C)
(defconstant +gpio-gmbus4+       #x05110)
(defconstant +gpio-gmbus5+       #x05120)

;;; Display Engine Pipeline Registers
;; Display Pipeline A
(defconstant +htotal-a+          #x60000)
(defconstant +hblank-a+          #x60004)
(defconstant +hsync-a+           #x60008)
(defconstant +vtotal-a+          #x6000C)
(defconstant +vblank-a+          #x60010)
(defconstant +vsync-a+           #x60014)
(defconstant +pipeasrc+          #x6001C)
(defconstant +bclrpat-a+         #x60020)
(defconstant +vsyncshift-a+      #x60028)
(defconstant +crcctrlreda+       #x60050)
(defconstant +crcctrlgreena+     #x60054)
(defconstant +crcctrlbluea+      #x60058)
(defconstant +crcctrlresa+       #x6005C)
(defconstant +crcresreda+        #x60060)
(defconstant +crcresgreena+      #x60064)
(defconstant +crcresbluea+       #x60068)
(defconstant +crcresresa+        #x6006C)
;; Display Pipeline B
(defconstant +htotal-b+          #x61000)
(defconstant +hblank-b+          #x61004)
(defconstant +hsync-b+           #x61008)
(defconstant +vtotal-b+          #x6100C)
(defconstant +vblank-b+          #x61010)
(defconstant +vsync-b+           #x61014)
(defconstant +pipebsrc+          #x6101C)
(defconstant +bclrpat-b+         #x61020)
(defconstant +vsyncshift-b+      #x61028)
(defconstant +crcctrlredb+       #x61050)
(defconstant +crcctrlgreenb+     #x61054)
(defconstant +crcctrlblueb+      #x61058)
(defconstant +crcctrlresb+       #x6105C)
(defconstant +crcresredb+        #x61060)
(defconstant +crcresgreenb+      #x61064)
(defconstant +crcresblueb+       #x61068)
(defconstant +crcresresb+        #x6106C)
;; Display Port Control
(defconstant +adpa+              #x61100)
(defconstant +port-hotplu-en+    #x61110)
(defconstant +port-hotplu-stat+  #x61114)
(defconstant +sdvo/hdmib+        #x61140)
(defconstant +sdvo/dp+           #x61150)
(defconstant +sdvo/dp2+          #x61154)
(defconstant +sdvo/dmic+         #x61160)
(defconstant +video-dip-ctl+     #x61170)
(defconstant +video-dip-data+    #x61178)

;;; Display and Cursor Control Registers
;; Display Pipeline A Control
(defconstant +pipea-dsl+         #x70000)
(defconstant +pipea-slc+         #x70004)
(defconstant +pipeaconf+         #x70008)
(defconstant +pipeagcmaxred+     #x70010)
(defconstant +pipeagcmaxgrn+     #x70014)
(defconstant +pipeagcmaxblu+     #x70018)
(defconstant +pipeastat+         #x70024)
(defconstant +dsparb+            #x70030)
(defconstant +fw1+               #x70034)
(defconstant +fw2+               #x70038)
(defconstant +fw3+               #x7003C)
(defconstant +pipeaframeh+       #x70040)
(defconstant +pipeaframepix+     #x70044)
;; Cursor A and B
(defconstant +curacntr+          #x70080)
(defconstant +curabase+          #x70084)
(defconstant +curapos+           #x70088)
(defconstant +curapalet0+        #x70090)
(defconstant +curapalet1+        #x70094)
(defconstant +curapalet2+        #x70098)
(defconstant +curapalet4+        #x7009C)
(defconstant +curbcntr+          #x700C0)
(defconstant +curbbase+          #x700C4)
(defconstant +curbpos+           #x700C8)
(defconstant +curbpalet0+        #x700D0)
(defconstant +curbpalet1+        #x700D4)
(defconstant +curbpalet2+        #x700D8)
(defconstant +curbpalet4+        #x700DC)
;; Display A Control
(defconstant +dspacntr+          #x70180)
(defconstant +dspalinoff+        #x70184)
(defconstant +dspastride+        #x70188)
(defconstant +dspakeyval+        #x70194)
(defconstant +dspakeymsk+        #x70198)
(defconstant +dspasurf+          #x7019C)
(defconstant +dspatileoff+       #x701A4)
(defconstant +dspaflpqstat+      #x70200)
;; Display Pipeline B Control
(defconstant +pipeb-dsl+         #x71000)
(defconstant +pipeb-slc+         #x71004)
(defconstant +pipebconf+         #x71008)
(defconstant +pipebgcmaxred+     #x71010)
(defconstant +pipebgcmaxgrn+     #x71014)
(defconstant +pipebgcmaxblu+     #x71018)
(defconstant +pipebstat+         #x71024)
(defconstant +pipebframeh+       #x71040)
(defconstant +pipebframepix+     #x71044)
;; Display B / Sprite Control
(defconstant +dspbcntr+          #x71180)
(defconstant +dspblinoff+        #x71184)
(defconstant +dspbstride+        #x71188)
(defconstant +dspbkeyval+        #x71194)
(defconstant +dspbkeymsk+        #x71198)
(defconstant +dspbsurf+          #x7119C)
(defconstant +dspbtileoff+       #x711A4)
(defconstant +dspbflpqstat+      #x71200)
;; Video BIOS Registers
(defconstant +vgacntrl+          #x71400)

;; Fields in ADPA.
(defconstant +adpa-enable+ (byte 1 31))
(defconstant +adpa-pipe-select+ (byte 1 30))
(defconstant +adpa-vga-sync-select+ (byte 1 15))
(defconstant +adpa-dpms+ (byte 2 10))
(defconstant +adpa-vsync-polarity+ (byte 1 4))
(defconstant +adpa-hsync-polarity+ (byte 1 3))
;; Fields in the DPLL{A,B}_CTRL registers.
(defconstant +dpll-vco-enable+ (byte 1 31))
(defconstant +dpll-ctrl-p1+ (byte 8 16))
(defconstant +dpll-ctrl-p2+ (byte 2 24))
;; Fields in the FP{A,B}{0,1} registers.
(defconstant +pll-n-divisor+ (byte 6 16))
(defconstant +pll-m1-divisor+ (byte 6 8))
(defconstant +pll-m2-divisor+ (byte 6 0))
;; Fields in the HTOTAL_{A,B} registers.
(defconstant +htotal-active+ (byte 12 0))
(defconstant +htotal-total+ (byte 13 16))
;; Fields in the HBLANK_{A,B} registers.
(defconstant +hblank-start+ (byte 13 0))
(defconstant +hblank-end+ (byte 13 16))
;; Fields in the HSYNC_{A,B} registers.
(defconstant +hsync-start+ (byte 13 0))
(defconstant +hsync-end+ (byte 13 16))
;; Fields in the VTOTAL_{A,B} registers.
(defconstant +vtotal-active+ (byte 12 0))
(defconstant +vtotal-total+ (byte 13 16))
;; Fields in the VBLANK_{A,B} registers.
(defconstant +vblank-start+ (byte 13 0))
(defconstant +vblank-end+ (byte 13 16))
;; Fields in the VSYNC_{A,B} registers.
(defconstant +vsync-start+ (byte 13 0))
(defconstant +vsync-end+ (byte 13 16))
;; Fields in the PIPE{A,B}SRC registers.
(defconstant +pipesrc-horz+ (byte 12 16))
(defconstant +pipesrc-vert+ (byte 12 0))
;; Fields in the PIPE{A,B}CONF registers.
(defconstant +pipeconf-enable+ (byte 1 31))
(defconstant +pipeconf-force-border+ (byte 1 25))
(defconstant +pipeconf-display-disable+ (byte 1 19))
(defconstant +pipeconf-cursor-disable+ (byte 1 18))
;; Fields in the DSP{A,B]CNTR registers.
(defconstant +dspcntr-plane-enable+ (byte 1 31))
(defconstant +dspcntr-gamma-enable+ (byte 1 30))
(defconstant +dspcntr-format+ (byte 4 26))
(defconstant +dspcntr-pipe-select+ (byte 2 24))
(defconstant +dspcntr-key-window-enable+ (byte 1 23))
(defconstant +dspcntr-key-enable+ (byte 1 22))
(defconstant +dspcntr-pixel-multiply+ (byte 2 20))
(defconstant +dspcntr-180-rotation+ (byte 1 15))
(defconstant +dspcntr-tiled+ (byte 1 10))

(defstruct timing
  pixel-clock
  horz-left-border
  horz-active ; width
  horz-right-border
  horz-front-porch
  horz-sync
  horz-back-porch
  horz-image-size
  vert-top-border
  vert-active ; height
  vert-bottom-border
  vert-front-porch
  vert-sync
  vert-back-porch
  vert-image-size
  interlaced
  stereo
  ;; FIXME: Figure this out.
  sync-config)

(defun timing-refresh-rate (timing)
  (round (timing-pixel-clock timing)
         (* (+ (timing-horz-sync timing)
               (timing-horz-back-porch timing)
               (timing-horz-front-porch timing))
            (+ (timing-vert-sync timing)
               (timing-vert-back-porch timing)
               (timing-vert-front-porch timing)))))

(defmacro with-gma-access ((device) &body body)
  `(sup:with-device-access ((pci:pci-device-boot-id (gma-device ,device))
                            (error "~S is stale" ,device))
     (sup:with-mutex ((gma-lock ,device))
       ,@body)))

(defun framebuffer-address (device)
  (logand (pci:pci-io-region (gma-device device) +pci-bar-gmadr+) (lognot #xF)))

(defconstant +reference-frequency+ 96000000)

(defun pll-parameters-to-dot-clock (n m1 m2 p1 p2 &key (refclk +reference-frequency+))
  (let* ((p1 (ecase p1
               (#b00000001 1)
               (#b00000010 2)
               (#b00000100 3)
               (#b00001000 4)
               (#b00010000 5)
               (#b00100000 6)
               (#b01000000 7)
               (#b10000000 8)))
         (p2 (ecase p2
               (#b00 10)
               (#b01 5)))
         (m (+ (* 5 (+ m1 2)) (+ m2 2)))
         (p-div (* p1 p2)))
    (truncate (/ (* refclk m) (+ n 2) p-div))))

(defun compute-pll-parameters (target-clock)
  "Calculate the best PLL paramters for TARGET-CLOCK.
Returns the N, M1, M2, P1, P2, and the actual dot clock frequency."
  (let ((best-clock 0)
        (best-n 0) (best-m1 0) (best-m2 0)
        ;; p1 raw value, not register value
        (best-p1 0)
        ;; p2 raw value, not register value.
        (p2 10))
    ;; Cycle through all PLL values looking for the closest frequency.
    (loop
       for n from 3 to 8
       do (loop
             for m1 from 10 to 20
             do (loop
                   for m2 from 5 to 9
                   for m = (+ (* 5 (+ m1 2)) (+ m2 2))
                   when (<= 70 m 120)
                   do
                     (loop
                        for p1 from 1 to 8
                        for p-div = (* p1 p2)
                        when (<= 5 p-div 80)
                        do
                          (let* ((vco (/ (* +reference-frequency+ m) (+ n 2)))
                                 (clock (/ vco p-div)))
                            (when (and
                                   ;; Must be in range.
                                   (<= 1400000000 vco 2800000000)
                                   ;; Must be better.
                                   (< (abs (- target-clock clock))
                                      (abs (- target-clock best-clock))))
                              (setf best-clock clock
                                    best-n n
                                    best-m1 m1
                                    best-m2 m2
                                    best-p1 p1)))))))
    (let ((error (float (abs (- 1 (/ target-clock best-clock))))))
      ;; TODO: This should bail if the error is too great. It's not clear
      ;; what "too great" is though.
      (values best-n best-m1 best-m2
              (ash 1 (1- best-p1)) ; Convert to register value.
              #b00 ; p2 register value
              (truncate best-clock)
              error))))

(defun save-current-mode-parameters (device)
  "Return a MODE-PARAMETERS object for the device's currently configured mode."
  (with-gma-access (device)
    (let ((mmio (gma-registers device)))
      (flet ((reg (idx)
               (pci:pci-io-region/32 mmio idx)))
        (let* ((dplla-ctrl (reg +dplla-ctrl+))
               (fpa0 (reg +fpa0+))
               (htotal-a (reg +htotal-a+))
               (hblank-a (reg +hblank-a+))
               (hsync-a (reg +hsync-a+))
               (vtotal-a (reg +vtotal-a+))
               (vblank-a (reg +vblank-a+))
               (vsync-a (reg +vsync-a+)))
          (make-timing
           :pixel-clock (pll-parameters-to-dot-clock
                         (ldb +pll-n-divisor+ fpa0)
                         (ldb +pll-m1-divisor+ fpa0)
                         (ldb +pll-m2-divisor+ fpa0)
                         (ldb +dpll-ctrl-p1+ dplla-ctrl)
                         (ldb +dpll-ctrl-p2+ dplla-ctrl))
           :horz-left-border (- (1+ (ldb +htotal-total+ htotal-a))
                                (1+ (ldb +hblank-end+ hblank-a)))
           :horz-active (1+ (ldb +htotal-active+ htotal-a))
           :horz-right-border (- (1+ (ldb +htotal-active+ htotal-a))
                                 (1+ (ldb +hblank-start+ hblank-a)))
           :horz-front-porch (- (1+ (ldb +hsync-start+ hsync-a))
                                (1+ (ldb +hblank-start+ hblank-a)))
           :horz-sync (- (1+ (ldb +hsync-end+ hsync-a))
                         (1+ (ldb +hsync-start+ hsync-a)))
           :horz-back-porch (- (1+ (ldb +hblank-end+ hblank-a))
                               (1+ (ldb +hsync-end+ hsync-a)))
           :horz-image-size (truncate (* (/ (1+ (ldb +htotal-active+ htotal-a)) 72) 25.4)) ; Assume 72dpi
           :vert-top-border (- (1+ (ldb +vtotal-total+ vtotal-a))
                               (1+ (ldb +vblank-end+ vblank-a)))
           :vert-active (1+ (ldb +vtotal-active+ vtotal-a))
           :vert-bottom-border (- (1+ (ldb +vtotal-active+ vtotal-a))
                                  (1+ (ldb +vblank-start+ vblank-a)))
           :vert-front-porch (- (1+ (ldb +vsync-start+ vsync-a))
                                (1+ (ldb +vblank-start+ vblank-a)))
           :vert-sync (- (1+ (ldb +vsync-end+ vsync-a))
                         (1+ (ldb +vsync-start+ vsync-a)))
           :vert-back-porch (- (1+ (ldb +vblank-end+ vblank-a))
                               (1+ (ldb +vsync-end+ vsync-a)))
           :vert-image-size (truncate (* (/ (1+ (ldb +vtotal-active+ vtotal-a)) 72) 25.4)) ; Assume 72dpi
           ;; TODO: Pick up interlaced modes
           :interlaced nil
           ;; TODO: Pick up stereo modes
           :stereo nil
           ;; TODO: Sync...
           :sync-config 15)))))) ; Digital, serrated +vsync, +hsync.

(defun mode-switch (device mode)
  (with-gma-access (device)
    ;; Update the video framebuffer.
    ;; Do this first so that the compositor updates display before
    ;; the new mode is set and nothing is displayed with the wrong stride.
    (sup::video-set-framebuffer
     (framebuffer-address device)
     (timing-horz-active mode)
     (timing-vert-active mode)
     (* (timing-horz-active mode) 4)
     :x8r8g8b8
     :device device)
    (mezzano.gui.compositor:force-redisplay t)
    ;; Set mode.
    (let ((mmio (gma-registers device)))
      (flet ((reg (idx)
               (pci:pci-io-region/32 mmio idx))
             ((setf reg) (value idx)
               (setf (pci:pci-io-region/32 mmio idx) value)))
        ;; Full modesetting sequence. Ez.
        ;; Disable ports [physical output port]
        (setf (ldb +adpa-enable+ (reg +adpa+)) 0)
        ;; Disable planes [display pane]
        (setf (ldb +dspcntr-plane-enable+ (reg +dspacntr+)) 1
              (reg +dspasurf+) 0
              (reg +dspalinoff+) 0
              (ldb +dspcntr-plane-enable+ (reg +dspacntr+)) 0
              (reg +dspasurf+) 0
              (reg +dspalinoff+) 0)
        ;; Disable pipe [pipeline, timings]
        (setf (ldb +pipeconf-enable+ (reg +pipeaconf+)) 0)
        ;; Disable VGA display [vgacntrl]
        (setf (ldb (byte 1 31) (reg +vgacntrl+)) 1)
        ;; FIXME: There should be a delay between disabling the pipe and disabling
        ;; the DPLL. Wait for a VBLANK or two.
        (sleep 0.1)
        ;; Disable DPLL
        (setf (ldb +dpll-vco-enable+ (reg +dplla-ctrl+)) 0)
        ;; Program new mode and enable.
        ;; Program DPLL
        (multiple-value-bind (n m1 m2 p1 p2)
            (compute-pll-parameters (timing-pixel-clock mode))
          (setf (ldb +dpll-ctrl-p1+ (reg +dplla-ctrl+)) p1
                (ldb +dpll-ctrl-p2+ (reg +dplla-ctrl+)) p2
                (ldb +pll-n-divisor+ (reg +fpa0+)) n
                (ldb +pll-m1-divisor+ (reg +fpa0+)) m1
                (ldb +pll-m2-divisor+ (reg +fpa0+)) m2))
        ;; Enable DPLL
        (setf (ldb +dpll-vco-enable+ (reg +dplla-ctrl+)) 1)
        ;; Wait for DPLL warmup (150us)
        (sleep 0.00015)
        ;; TODO: Set sync polarity, etc
        ;; Program pipe timings (can be done before DPLL setup)
        (setf (ldb +htotal-active+ (reg +htotal-a+))
              (1- (timing-horz-active mode)))
        (setf (ldb +htotal-total+ (reg +htotal-a+))
              (1- (+ (timing-horz-left-border mode)
                     (timing-horz-active mode)
                     (timing-horz-right-border mode)
                     (timing-horz-front-porch mode)
                     (timing-horz-sync mode)
                     (timing-horz-back-porch mode))))
        (setf (ldb +hblank-start+ (reg +hblank-a+))
              (1- (+ (timing-horz-active mode)
                     (timing-horz-right-border mode))))
        (setf (ldb +hblank-end+ (reg +hblank-a+))
              (1- (+ (timing-horz-active mode)
                     (timing-horz-right-border mode)
                     (timing-horz-front-porch mode)
                     (timing-horz-sync mode)
                     (timing-horz-back-porch mode))))
        (setf (ldb +hsync-start+ (reg +hsync-a+))
              (1- (+ (timing-horz-active mode)
                     (timing-horz-right-border mode)
                     (timing-horz-front-porch mode))))
        (setf (ldb +hsync-end+ (reg +hsync-a+))
              (1- (+ (timing-horz-active mode)
                     (timing-horz-right-border mode)
                     (timing-horz-front-porch mode)
                     (timing-horz-sync mode))))
        (setf (ldb +vtotal-active+ (reg +vtotal-a+))
              (1- (timing-vert-active mode)))
        (setf (ldb +vtotal-total+ (reg +vtotal-a+))
              (1- (+ (timing-vert-bottom-border mode)
                     (timing-vert-active mode)
                     (timing-vert-top-border mode)
                     (timing-vert-front-porch mode)
                     (timing-vert-sync mode)
                     (timing-vert-back-porch mode))))
        (setf (ldb +vblank-start+ (reg +vblank-a+))
              (1- (+ (timing-vert-active mode)
                     (timing-vert-top-border mode))))
        (setf (ldb +vblank-end+ (reg +vblank-a+))
              (1- (+ (timing-vert-active mode)
                     (timing-vert-top-border mode)
                     (timing-vert-front-porch mode)
                     (timing-vert-sync mode)
                     (timing-vert-back-porch mode))))
        (setf (ldb +vsync-start+ (reg +vsync-a+))
              (1- (+ (timing-vert-active mode)
                     (timing-vert-top-border mode)
                     (timing-vert-front-porch mode))))
        (setf (ldb +vsync-end+ (reg +vsync-a+))
              (1- (+ (timing-vert-active mode)
                     (timing-vert-top-border mode)
                     (timing-vert-front-porch mode)
                     (timing-vert-sync mode))))
        ;; Enable pipe
        (setf (ldb +pipesrc-horz+ (reg +pipeasrc+)) (1- (timing-horz-active mode))
              (ldb +pipesrc-vert+ (reg +pipeasrc+)) (1- (timing-vert-active mode)))
        (setf (ldb +pipeconf-enable+ (reg +pipeaconf+)) 1)
        ;; Enable planes
        (setf (reg +dspastride+) (* (timing-horz-active mode) 4)
              (reg +dspasurf+) 0
              (ldb +dspcntr-plane-enable+ (reg +dspacntr+)) 1
              (reg +dspalinoff+) 0)
        ;; Enable ports
        (setf (ldb +adpa-enable+ (reg +adpa+)) 1))))
  (values))

(defun dump-display-state (device)
  "Dump the clock, pipe, and display registers."
  (with-gma-access (device)
    (let ((mmio (gma-registers device)))
      (flet ((reg (idx)
               (pci:pci-io-region/32 mmio idx)))
        (format t "Clock Control and Power Management Registers:~%")
        (let ((reg (reg +vga0+)))
          (format t "  VGA0              : ~8,'0X~%" reg)
          (format t "    N: ~D  M1: ~D  M2: ~D~%"
                  (ldb (byte 6 16) reg) (ldb (byte 6 8) reg) (ldb (byte 6 0) reg)))
        (let ((reg (reg +vga1+)))
          (format t "  VGA1              : ~8,'0X~%" reg)
          (format t "    N: ~D  M1: ~D  M2: ~D~%"
                  (ldb (byte 6 16) reg) (ldb (byte 6 8) reg) (ldb (byte 6 0) reg)))
        (let ((reg (reg +vga-pd+)))
          (format t "  VGA_PD            : ~8,'0X~%" reg)
          (format t "    VGA0P1: ~8,'0B  VGA0P2: ~2,'0B  VGA1P1: ~8,'0B  VGA1P2: ~2,'0B~%"
                  (ldb (byte 8 0) reg) (ldb (byte 2 8) reg)
                  (ldb (byte 8 16) reg) (ldb (byte 2 24) reg)))
        (let ((reg (reg +dplla-ctrl+)))
          (format t "  DPLLA_CTRL        : ~8,'0X~%" reg)
          (format t "    Phase: ~4,'0B  Input: ~2,'0B~%"
                  (ldb (byte 4 9) reg) (ldb (byte 2 13) reg))
          (format t "    P1: ~8,'0B  P2: ~2,'0B  Mode: ~2,'0B~%"
                  (ldb +dpll-ctrl-p1+ reg) (ldb +dpll-ctrl-p2+ reg) (ldb (byte 2 26) reg))
          (format t "    VGADis: ~D  HiSpeed: ~D  VCOEn: ~D~%"
                  (ldb (byte 1 28) reg) (ldb (byte 1 30) reg) (ldb (byte 1 31) reg)))
        (let ((reg (reg +dpllb-ctrl+)))
          (format t "  DPLLB_CTRL        : ~8,'0X~%" reg)
          (format t "    Phase: ~4,'0B  Input: ~2,'0B~%"
                  (ldb (byte 4 9) reg) (ldb (byte 2 13) reg))
          (format t "    P1: ~8,'0B  P2: ~2,'0B  Mode: ~2,'0B~%"
                  (ldb +dpll-ctrl-p1+ reg) (ldb +dpll-ctrl-p2+ reg) (ldb (byte 2 26) reg))
          (format t "    VGADis: ~D  HiSpeed: ~D  VCOEn: ~D~%"
                  (ldb (byte 1 28) reg) (ldb (byte 1 30) reg) (ldb (byte 1 31) reg)))
        (format t "  DPLLAMD           : ~8,'0X~%" (reg +dpllamd+))
        (format t "  DPLLBMD           : ~8,'0X~%" (reg +dpllbmd+))
        (let ((reg (reg +fpa0+)))
          (format t "  FPA0              : ~8,'0X~%" reg)
          (format t "    N: ~D  M1: ~D  M2: ~D~%"
                  (ldb +pll-n-divisor+ reg) (ldb +pll-m1-divisor+ reg) (ldb +pll-m2-divisor+ reg)))
        (let ((reg (reg +fpa1+)))
          (format t "  FPA1              : ~8,'0X~%" reg)
          (format t "    N: ~D  M1: ~D  M2: ~D~%"
                  (ldb +pll-n-divisor+ reg) (ldb +pll-m1-divisor+ reg) (ldb +pll-m2-divisor+ reg)))
        (let ((reg (reg +fpb0+)))
          (format t "  FPB0              : ~8,'0X~%" reg)
          (format t "    N: ~D  M1: ~D  M2: ~D~%"
                  (ldb +pll-n-divisor+ reg) (ldb +pll-m1-divisor+ reg) (ldb +pll-m2-divisor+ reg)))
        (let ((reg (reg +fpb1+)))
          (format t "  FPB1              : ~8,'0X~%" reg)
          (format t "    N: ~D  M1: ~D  M2: ~D~%"
                  (ldb +pll-n-divisor+ reg) (ldb +pll-m1-divisor+ reg) (ldb +pll-m2-divisor+ reg)))
        (format t "  DPLL_TEST         : ~8,'0X~%" (reg +dpll-test+))
        (format t "  D_STATE           : ~8,'0X~%" (reg +d-state+))
        (format t "  DSPCLK_GATE_D     : ~8,'0X~%" (reg +dspclk-gate-d+))
        (format t "  RENCLK_GATE_D1    : ~8,'0X~%" (reg +renclk-gate-d1+))
        (format t "  RENCLK_GATE_D2    : ~8,'0X~%" (reg +renclk-gate-d2+))
        (format t "  RAMCLK_GATE_D     : ~8,'0X~%" (reg +ramclk-gate-d+))
        (format t "  DEUC              : ~8,'0X~%" (reg +deuc+))
        (format t "Display Pipeline A~%")
        (let ((reg (reg +htotal-a+)))
          (format t "  HTOTAL_A          : ~8,'0X~%" reg)
          (format t "    Active: ~D  Total: ~D~%"
                  (ldb +htotal-active+ reg) (ldb +htotal-total+ reg)))
        (let ((reg (reg +hblank-a+)))
          (format t "  HBLANK_A          : ~8,'0X~%" reg)
          (format t "    Start: ~D  End: ~D~%"
                  (ldb +hblank-start+ reg) (ldb +hblank-end+ reg)))
        (let ((reg (reg +hsync-a+)))
          (format t "  HSYNC_A           : ~8,'0X~%" reg)
          (format t "    Start: ~D  End: ~D~%"
                  (ldb +hsync-start+ reg) (ldb +hsync-end+ reg)))
        (let ((reg (reg +vtotal-a+)))
          (format t "  VTOTAL_A          : ~8,'0X~%" reg)
          (format t "    Active: ~D  Total: ~D~%"
                  (ldb +vtotal-active+ reg) (ldb +vtotal-total+ reg)))
        (let ((reg (reg +vblank-a+)))
          (format t "  VBLANK_A          : ~8,'0X~%" reg)
          (format t "    Start: ~D  End: ~D~%"
                  (ldb +vblank-start+ reg) (ldb +vblank-end+ reg)))
        (let ((reg (reg +vsync-a+)))
          (format t "  VSYNC_A           : ~8,'0X~%" reg)
          (format t "    Start: ~D  End: ~D~%"
                  (ldb +vsync-start+ reg) (ldb +vsync-end+ reg)))
        (let ((reg (reg +pipeasrc+)))
          (format t "  PIPEASRC          : ~8,'0X~%" reg)
          (format t "    Horz: ~D  Vert: ~D~%"
                  (ldb +pipesrc-horz+ reg) (ldb +pipesrc-vert+ reg)))
        (format t "  BCLRPAT_A         : ~8,'0X~%" (reg +bclrpat-a+))
        (format t "  VSYNCSHIFT_A      : ~8,'0X~%" (reg +vsyncshift-a+))
        (format t "  CRCCTRLREDA       : ~8,'0X~%" (reg +crcctrlreda+))
        (format t "  CRCCTRLGREENA     : ~8,'0X~%" (reg +crcctrlgreena+))
        (format t "  CRCCTRLBLUEA      : ~8,'0X~%" (reg +crcctrlbluea+))
        (format t "  CRCCTRLRESA       : ~8,'0X~%" (reg +crcctrlresa+))
        (format t "  CRCRESREDA        : ~8,'0X~%" (reg +crcresreda+))
        (format t "  CRCRESGREENA      : ~8,'0X~%" (reg +crcresgreena+))
        (format t "  CRCRESBLUEA       : ~8,'0X~%" (reg +crcresbluea+))
        (format t "  CRCRESRESA        : ~8,'0X~%" (reg +crcresresa+))
        (format t "Display Pipeline B~%")
        (let ((reg (reg +htotal-b+)))
          (format t "  HTOTAL_B          : ~8,'0X~%" reg)
          (format t "    Active: ~D  Total: ~D~%"
                  (ldb +htotal-active+ reg) (ldb +htotal-total+ reg)))
        (let ((reg (reg +hblank-b+)))
          (format t "  HBLANK_B          : ~8,'0X~%" reg)
          (format t "    Start: ~D  End: ~D~%"
                  (ldb +hblank-start+ reg) (ldb +hblank-end+ reg)))
        (let ((reg (reg +hsync-b+)))
          (format t "  HSYNC_B           : ~8,'0X~%" reg)
          (format t "    Start: ~D  End: ~D~%"
                  (ldb +hsync-start+ reg) (ldb +hsync-end+ reg)))
        (let ((reg (reg +vtotal-b+)))
          (format t "  VTOTAL_B          : ~8,'0X~%" reg)
          (format t "    Active: ~D  Total: ~D~%"
                  (ldb +vtotal-active+ reg) (ldb +vtotal-total+ reg)))
        (let ((reg (reg +vblank-b+)))
          (format t "  VBLANK_B          : ~8,'0X~%" reg)
          (format t "    Start: ~D  End: ~D~%"
                  (ldb +vblank-start+ reg) (ldb +vblank-end+ reg)))
        (let ((reg (reg +vsync-b+)))
          (format t "  VSYNC_B           : ~8,'0X~%" reg)
          (format t "    Start: ~D  End: ~D~%"
                  (ldb +vsync-start+ reg) (ldb +vsync-end+ reg)))
        (let ((reg (reg +pipebsrc+)))
          (format t "  PIPEBSRC          : ~8,'0X~%" reg)
          (format t "    Horz: ~D  Vert: ~D~%"
                  (ldb +pipesrc-horz+ reg) (ldb +pipesrc-vert+ reg)))
        (format t "  BCLRPAT_B         : ~8,'0X~%" (reg +bclrpat-b+))
        (format t "  VSYNCSHIFT_B      : ~8,'0X~%" (reg +vsyncshift-b+))
        (format t "  CRCCTRLREDB       : ~8,'0X~%" (reg +crcctrlredb+))
        (format t "  CRCCTRLGREENB     : ~8,'0X~%" (reg +crcctrlgreenb+))
        (format t "  CRCCTRLBLUEB      : ~8,'0X~%" (reg +crcctrlblueb+))
        (format t "  CRCCTRLRESB       : ~8,'0X~%" (reg +crcctrlresb+))
        (format t "  CRCRESREDB        : ~8,'0X~%" (reg +crcresredb+))
        (format t "  CRCRESGREENB      : ~8,'0X~%" (reg +crcresgreenb+))
        (format t "  CRCRESBLUEB       : ~8,'0X~%" (reg +crcresblueb+))
        (format t "  CRCRESRESB        : ~8,'0X~%" (reg +crcresresb+))
        (format t "Display Port Control~%")
        (let ((reg (reg +adpa+)))
          (format t "  ADPA              : ~8,'0X~%" reg)
          (format t "    HsyncPol: ~D  VSyncPol: ~D  MonitorDPMS: ~2,'0B~%"
                  (ldb +adpa-hsync-polarity+ reg)
                  (ldb +adpa-vsync-polarity+ reg)
                  (ldb +adpa-dpms+ reg))
          (format t "    VgaSync: ~D  PipeSelect: ~D  Enable: ~D~%"
                  (ldb +adpa-vga-sync-select+ reg)
                  (ldb +adpa-pipe-select+ reg)
                  (ldb +adpa-enable+ reg)))
        (format t "  PORT_HOTPLU_EN    : ~8,'0X~%" (reg +port-hotplu-en+))
        (format t "  PORT_HOTPLU_STAT  : ~8,'0X~%" (reg +port-hotplu-stat+))
        (format t "  SDVO/HDMIB        : ~8,'0X~%" (reg +sdvo/hdmib+))
        (format t "  SDVO/DP           : ~8,'0X~%" (reg +sdvo/dp+))
        (format t "  SDVO/DP2          : ~8,'0X~%" (reg +sdvo/dp2+))
        (format t "  SDVO/DMIC         : ~8,'0X~%" (reg +sdvo/dmic+))
        (format t "  VIDEO_DIP_CTL     : ~8,'0X~%" (reg +video-dip-ctl+))
        (format t "  VIDEO_DIP_DATA    : ~8,'0X~%" (reg +video-dip-data+))
        (format t "Pipe FIFO Control~%")
        (let ((reg (reg +dsparb+)))
          (format t "  DSPARB            : ~8,'0X~%" reg)
          (format t "    Bstart: ~D  CStart: ~D~%"
                  (ldb (byte 7 0) reg) (ldb (byte 7 7) reg)))
        (let ((reg (reg +fw1+)))
          (format t "  FW1               : ~8,'0X~%" reg)
          (format t "    PlaneA: ~D  PlaneB: ~D  CursorB: ~D  Refresh: ~D~%"
                  (ldb (byte 7 0) reg) (ldb (byte 7 8) reg)
                  (ldb (byte 6 16) reg) (ldb (byte 9 23) reg)))
        (let ((reg (reg +fw2+)))
          (format t "  FW2               : ~8,'0X~%" reg)
          (format t "    PlaneC: ~D  CursorA: ~D~%"
                  (ldb (byte 7 0) reg) (ldb (byte 6 8) reg)))
        (let ((reg (reg +fw3+)))
          (format t "  FW3               : ~8,'0X~%" reg)
          (format t "    Display: ~D  Cursor: ~D  CursorFIFO: ~D  HPLLen: ~D~%"
                  (ldb (byte 9 0) reg) (ldb (byte 6 16) reg)
                  (ldb (byte 6 24) reg) (logbitp 31 reg)))
        (format t "Display Pipeline A Control~%")
        (format t "  PIPEA_DSL         : ~8,'0X~%" (reg +pipea-dsl+))
        (format t "  PIPEA_SLC         : ~8,'0X~%" (reg +pipea-slc+))
        (format t "  PIPEACONF         : ~8,'0X~%" (reg +pipeaconf+))
        (format t "  PIPEAGCMAXRED     : ~8,'0X~%" (reg +pipeagcmaxred+))
        (format t "  PIPEAGCMAXGRN     : ~8,'0X~%" (reg +pipeagcmaxgrn+))
        (format t "  PIPEAGCMAXBLU     : ~8,'0X~%" (reg +pipeagcmaxblu+))
        (format t "  PIPEASTAT         : ~8,'0X~%" (reg +pipeastat+))
        (format t "  PIPEAFRAMEH       : ~8,'0X~%" (reg +pipeaframeh+))
        (format t "  PIPEAFRAMEPIX     : ~8,'0X~%" (reg +pipeaframepix+))
        (format t "Display A Control~%")
        (let ((reg (reg +dspacntr+)))
          (format t "  DSPACNTR          : ~8,'0X~%" reg)
          (format t "    Tiled: ~D  Rot180: ~D  PixelMultiply: ~2,'0B~%"
                  (ldb (byte 1 10) reg) (ldb (byte 1 15) reg) (ldb (byte 2 20) reg))
          (format t "    KeyEn: ~D  KeyWinEn: ~D  Pipe: ~2,'0B~%"
                  (ldb (byte 1 22) reg) (ldb (byte 1 23) reg) (ldb (byte 2 24) reg))
          (format t "    SrcFmt: ~4,'0B  GammaEn: ~D  PlaneEn: ~D~%"
                  (ldb (byte 4 26) reg) (ldb (byte 1 30) reg) (ldb (byte 1 31) reg)))
        (format t "  DSPALINOFF        : ~8,'0X~%" (reg +dspalinoff+))
        (format t "  DSPASTRIDE        : ~8,'0X~%" (reg +dspastride+))
        (format t "  DSPAKEYVAL        : ~8,'0X~%" (reg +dspakeyval+))
        (format t "  DSPAKEYMSK        : ~8,'0X~%" (reg +dspakeymsk+))
        (format t "  DSPASURF          : ~8,'0X~%" (reg +dspasurf+))
        (format t "  DSPATILEOFF       : ~8,'0X~%" (reg +dspatileoff+))
        (format t "  DSPAFLPQSTAT      : ~8,'0X~%" (reg +dspaflpqstat+))
        (format t "Display Pipeline B Control~%")
        (format t "  PIPEB_DSL         : ~8,'0X~%" (reg +pipeb-dsl+))
        (format t "  PIPEB_SLC         : ~8,'0X~%" (reg +pipeb-slc+))
        (format t "  PIPEBCONF         : ~8,'0X~%" (reg +pipebconf+))
        (format t "  PIPEBGCMAXRED     : ~8,'0X~%" (reg +pipebgcmaxred+))
        (format t "  PIPEBGCMAXGRN     : ~8,'0X~%" (reg +pipebgcmaxgrn+))
        (format t "  PIPEBGCMAXBLU     : ~8,'0X~%" (reg +pipebgcmaxblu+))
        (format t "  PIPEBSTAT         : ~8,'0X~%" (reg +pipebstat+))
        (format t "  PIPEBFRAMEH       : ~8,'0X~%" (reg +pipebframeh+))
        (format t "  PIPEBFRAMEPIX     : ~8,'0X~%" (reg +pipebframepix+))
        (format t "Display B / Sprite Control~%")
        (let ((reg (reg +dspbcntr+)))
          (format t "  DSPBCNTR          : ~8,'0X~%" reg)
          (format t "    Tiled: ~D  Rot180: ~D  PixelMultiply: ~2,'0B~%"
                  (ldb (byte 1 10) reg) (ldb (byte 1 15) reg) (ldb (byte 2 20) reg))
          (format t "    KeyEn: ~D  KeyWinEn: ~D  Pipe: ~2,'0B~%"
                  (ldb (byte 1 22) reg) (ldb (byte 1 23) reg) (ldb (byte 2 24) reg))
          (format t "    SrcFmt: ~4,'0B  GammaEn: ~D  PlaneEn: ~D~%"
                  (ldb (byte 4 26) reg) (ldb (byte 1 30) reg) (ldb (byte 1 31) reg)))
        (format t "  DSPBLINOFF        : ~8,'0X~%" (reg +dspblinoff+))
        (format t "  DSPBSTRIDE        : ~8,'0X~%" (reg +dspbstride+))
        (format t "  DSPBKEYVAL        : ~8,'0X~%" (reg +dspbkeyval+))
        (format t "  DSPBKEYMSK        : ~8,'0X~%" (reg +dspbkeymsk+))
        (format t "  DSPBSURF          : ~8,'0X~%" (reg +dspbsurf+))
        (format t "  DSPBTILEOFF       : ~8,'0X~%" (reg +dspbtileoff+))
        (format t "  DSPBFLPQSTAT      : ~8,'0X~%" (reg +dspbflpqstat+))
        (format t "Cursor A and B~%")
        (format t "  CURACNTR          : ~8,'0X~%" (reg +curacntr+))
        (format t "  CURABASE          : ~8,'0X~%" (reg +curabase+))
        (format t "  CURAPOS           : ~8,'0X~%" (reg +curapos+))
        (format t "  CURAPALET0        : ~8,'0X~%" (reg +curapalet0+))
        (format t "  CURAPALET1        : ~8,'0X~%" (reg +curapalet1+))
        (format t "  CURAPALET2        : ~8,'0X~%" (reg +curapalet2+))
        (format t "  CURAPALET4        : ~8,'0X~%" (reg +curapalet4+))
        (format t "  CURBCNTR          : ~8,'0X~%" (reg +curbcntr+))
        (format t "  CURBBASE          : ~8,'0X~%" (reg +curbbase+))
        (format t "  CURBPOS           : ~8,'0X~%" (reg +curbpos+))
        (format t "  CURBPALET0        : ~8,'0X~%" (reg +curbpalet0+))
        (format t "  CURBPALET1        : ~8,'0X~%" (reg +curbpalet1+))
        (format t "  CURBPALET2        : ~8,'0X~%" (reg +curbpalet2+))
        (format t "  CURBPALET4        : ~8,'0X~%" (reg +curbpalet4+))
        (format t "Video BIOS Registers~%")
        (format t "  VGACNTRL          : ~8,'0X~%" (reg +vgacntrl+))))))

;; Fields in GMBUS0 (Clock/Port Select)
(defconstant +gmbus-rate-select+ (byte 2 8))
(defconstant +gmbus-rate-100khz+ #b00)
(defconstant +gmbus-rate-50khz+ #b01)
(defconstant +gmbus-rate-400khz+ #b10)
(defconstant +gmbus-rate-1mhz+ #b11)
(defconstant +gmbus-hold-time-extension+ (byte 1 7))
(defconstant +gmbus-pin-pair+ (byte 3 0))
;; Fields in GMBUS1 (Command/Status)
(defconstant +gmbus-sw-clr-int+ (byte 1 31))
(defconstant +gmbus-sw-rdy+ (byte 1 30))
(defconstant +gmbus-ent+ (byte 1 29))
(defconstant +gmbus-bus-cycle-select+ (byte 3 25))
(defconstant +gmbus-cycle-index-wait+ #b011)
(defconstant +gmbus-cycle-all-stop+ #b100)
(defconstant +gmbus-total-byte-count+ (byte 9 16))
(defconstant +gmbus-index+ (byte 8 8))
(defconstant +gmbus-saddr+ (byte 7 1))
(defconstant +gmbus-direction+ (byte 1 0))
(defconstant +gmbus-direction-read+ 1)
(defconstant +gmbus-direction-write+ 0)
;; Fields in GMBUS2 (Status)
(defconstant +gmbus-inuse+ (byte 1 15))
(defconstant +gmbus-hw-wait-phase+ (byte 1 14))
(defconstant +gmbus-stall-timeout-error+ (byte 1 13))
(defconstant +gmbus-interrupt-status+ (byte 1 12))
(defconstant +gmbus-hw-rdy+ (byte 1 11))
(defconstant +gmbus-nak-indicator+ (byte 1 10))
(defconstant +gmbus-ga+ (byte 1 9))
(defconstant +gmbus-current-byte-count+ (byte 9 0))

(defun dump-gpio-state (device)
  "Dump the GPIO registers."
  (with-gma-access (device)
    (let ((mmio (gma-registers device)))
      (flet ((reg (idx)
               (pci:pci-io-region/32 mmio idx)))
        (dotimes (i 8)
          (format t "  GPIOCTL_~D        : ~8,'0X~%" i (reg (+ +gpio-ctl0+ (* i 4)))))
        (let ((reg (reg +gpio-gmbus0+)))
          (format t "  GMBUS0           : ~8,'0X~%" reg)
          (format t "    Rate: ~2,'0B  HoldTime: ~D  PinPair: ~3,'0B~%"
                  (ldb +gmbus-rate-select+ reg)
                  (ldb +gmbus-hold-time-extension+ reg)
                  (ldb +gmbus-pin-pair+ reg)))
        (let ((reg (reg +gpio-gmbus1+)))
          (format t "  GMBUS1           : ~8,'0X~%" reg)
          (format t "    SW_CLR_INT: ~D  SW_RDY: ~D  ENT: ~D~%"
                  (ldb +gmbus-sw-clr-int+ reg)
                  (ldb +gmbus-sw-rdy+ reg)
                  (ldb +gmbus-ent+ reg))
          (format t "    BusCycle: ~3,'0B  ByteCount: ~D  Index: ~2,'0X~%"
                  (ldb +gmbus-bus-cycle-select+ reg)
                  (ldb +gmbus-total-byte-count+ reg)
                  (ldb +gmbus-index+ reg))
          (format t "    SADDR: ~2,'0X  Dir: ~D~%"
                  (ldb +gmbus-saddr+ reg)
                  (ldb +gmbus-direction+ reg)))
        (let ((reg (reg +gpio-gmbus2+)))
          (format t "  GMBUS2           : ~8,'0X~%" reg)
          (format t "    INUSE: ~D  HW_WAIT_PHASE: ~D~%"
                  (ldb +gmbus-inuse+ reg)
                  (ldb +gmbus-hw-wait-phase+ reg))
          (format t "    StallTimeout: ~D  InterruptStatus: ~D~%"
                  (ldb +gmbus-stall-timeout-error+ reg)
                  (ldb +gmbus-interrupt-status+ reg))
          (format t "    HW_RDY: ~D  NAKi: ~D  GA: ~D~%"
                  (ldb +gmbus-hw-rdy+ reg)
                  (ldb +gmbus-nak-indicator+ reg)
                  (ldb +gmbus-ga+ reg))
          (format t "    CurrentByteCount: ~D~%"
                  (ldb +gmbus-current-byte-count+ reg)))
        (format t "  GMBUS3           : ~8,'0X~%" (reg +gpio-gmbus3+))
        (format t "  GMBUS4           : ~8,'0X~%" (reg +gpio-gmbus4+))
        (format t "  GMBUS5           : ~8,'0X~%" (reg +gpio-gmbus5+))))))

(defun read-raw-edid (device)
  (let ((buffer (make-array 128 :element-type '(unsigned-byte 8))))
    (with-gma-access (device)
      (let ((mmio (gma-registers device)))
        (flet ((reg (idx)
                 (pci:pci-io-region/32 mmio idx))
               ((setf reg) (value idx)
                 (setf (pci:pci-io-region/32 mmio idx) value)))
          ;; Reset the GMBUS.
          (setf (reg +gpio-gmbus1+) 0
                (ldb +gmbus-sw-clr-int+ (reg +gpio-gmbus1+)) 1
                (ldb +gmbus-sw-clr-int+ (reg +gpio-gmbus1+)) 0)
          ;; Start the access.
          (setf (reg +gpio-gmbus0+)
                (logior
                 ;; Standard i2c transfer rate
                 (dpb +gmbus-rate-100khz+ +gmbus-rate-select+ 0)
                 ;; No hold time, don't know what this is...
                 (dpb 0 +gmbus-hold-time-extension+ 0)
                 ;; Select the Analog Monitor DDC Pins
                 (dpb #b010 +gmbus-pin-pair+ 0)))
          (setf (reg +gpio-gmbus1+)
                (logior
                 (dpb 0 +gmbus-sw-clr-int+ 0)
                 (dpb 1 +gmbus-sw-rdy+ 0)
                 (dpb 0 +gmbus-ent+ 0)
                 (dpb +gmbus-cycle-index-wait+ +gmbus-bus-cycle-select+ 0)
                 ;; Reading 128 bytes from the start of the EDID ROM
                 (dpb 128 +gmbus-total-byte-count+ 0)
                 (dpb 0 +gmbus-index+ 0)
                 ;; This is (byte 7 1) of the EDID address #xA0
                 (dpb #x50 +gmbus-saddr+ 0)
                 (dpb +gmbus-direction-READ+ +gmbus-direction+ 0)))
          ;; Reading 128 bytes in 32 4-byte chunks.
          (dotimes (i 32)
            ;; Wait for hardware.
            (loop while (zerop (ldb +gmbus-hw-rdy+ (reg +gpio-gmbus2+))))
            (let ((data (reg +gpio-gmbus3+)))
              (setf (aref buffer (+ (* i 4) 0)) (ldb (byte 8  0) data)
                    (aref buffer (+ (* i 4) 1)) (ldb (byte 8  8) data)
                    (aref buffer (+ (* i 4) 2)) (ldb (byte 8 16) data)
                    (aref buffer (+ (* i 4) 3)) (ldb (byte 8 24) data))))
          ;; Stop the transaction.
          (setf (reg +gpio-gmbus1+)
                (logior
                 (dpb 1 +gmbus-sw-rdy+ 0)
                 (dpb +gmbus-cycle-all-stop+ +gmbus-bus-cycle-select+ 0))))))
    buffer))

(defstruct chromacticity
  red-x red-y
  green-x green-y
  blue-x blue-y
  white-x white-y)

(defstruct edid-data
  manufacturer-id
  product-code
  serial-number
  week-of-manufacture
  year-of-manufacture
  horizontal-screen-size
  vertical-screen-size
  display-gamma
  display-type
  srgb-colour-space
  native-format
  chromacticity
  timing-modes)

(defun decode-edid-established-timings (edid)
  (let ((timings '()))
    (flet ((add (byte bit width height pixel-clock hsync hbporch hactive hfporch vsync vbporch vactive vfporch)
             (when (logbitp bit (aref edid byte))
               (push (make-timing
                      :pixel-clock pixel-clock
                      :horz-left-border (floor (- hactive width) 2)
                      :horz-active width
                      :horz-right-border (ceiling (- hactive width) 2)
                      :horz-back-porch hbporch
                      :horz-sync hsync
                      :horz-front-porch hfporch
                      :horz-image-size (truncate (* (/ width 72) 25.4)) ; Assume 72dpi
                      :vert-top-border (floor (- vactive height) 2)
                      :vert-active height
                      :vert-bottom-border (ceiling (- vactive height) 2)
                      :vert-back-porch vbporch
                      :vert-sync vsync
                      :vert-front-porch vfporch
                      :vert-image-size (truncate (* (/ height 72) 25.4))
                      :stereo nil
                      ;; Digital, serrated +vsync, +hsync.
                      :sync-config 15)
                     timings))))
      ;; TODO: Double check these timings, make sure sync polarity is right, etc.
      (add 35 7  720  400  28322000  108  51  726 15  2 32 404 11) ; (VGA) 720x400@70
      ;;(add 35 6  720  400 ...) ; (XGA) 720x400@88
      (add 35 5  640  480  25175000   96  48  640 16  2 31 480 11) ; (VGA) 640x480@60
      (add 35 4  640  480  30240000   64  93  646 61  3 37 484  1) ; (Apple Macintosh II) 640x480@66
      (add 35 3  640  480  31500000   40 128  640 24  3 28 480  9) ; 640x480@72
      (add 35 2  640  480  31500000   96  48  640 16  2 32 480 11) ; 640x480@75
      (add 35 1  800  600  38100000  128 128  800 32  4 14 600  1) ; 800x600@56
      (add 35 0  800  600  40000000  128  88  800 40  4 23 600  1) ; 800x600@60
      (add 36 7  800  600  50000000  120  64  800 56  6 23 600 37) ; 600x600@72
      (add 36 6  800  600  49500000   80 160  800 16  2 21 600  1) ; 800x600@75
      ;;(add 36 5  832  624 ...) ; (Apple Macintosh II) 832x642@75
      ;;(add 36 4 1024  768 ...) ; (1024×768i) 1024x768@87 interlaced
      (add 36 3 1024  768  65000000  136 160 1024 24  6 29 768 3) ; 1024x768@60
      ;;(add 36 2 1024  768 ...) ; 1024x768@70 [this uses -h-v sync]
      (add 36 1 1024  768  78750000   96 176 1024 16  3 28 768 1) ; 1024x768@75
      (add 36 0 1280 1024 135000000  144 248 1280 16  3 38 1024 1) ; 1280x1024@75
      ;;(add 37 7 1152  870 ...) ; (Apple Macintosh II) 1152x870@75
      timings)))

(defun decode-edid-standard-timings (edid)
  ;; TODO: Decoding these timing requires using the VESA GTF
  ;; to produce detailed timing information.
  ;; Just ignore them for now.
  (declare (ignore edid))
  '()
  #+(or)
  (loop
     for i below 8
     for byte1 = (aref edid (+ 38 (* i 2)))
     for byte2 = (aref edid (+ 38 (* i 2) 1))
     unless (and (eql byte1 #x01) (eql byte2 #x01))
     collect (let* ((width (* (+ byte1 31) 8))
                    (height (truncate
                             width
                             ;; Decode aspect ratio
                             (ecase (ldb (byte 2 6) byte2)
                               (#b00 16/10)
                               (#b01 4/3)
                               (#b10 5/4)
                               (#b11 16/9)))))
               (make-timing :width width
                            :height height
                            :refresh-rate (+ (ldb (byte 6 0) byte2) 60)))))

(defun decode-edid-detailed-timing (edid offset)
  (let ((pixel-clock (* (mezzano.internals::ub16ref/le edid (+ offset 0)) 10000))
        (hactive (logior (aref edid (+ offset 2))
                         (ash (ldb (byte 4 4) (aref edid (+ offset 4))) 8)))
        (hblank (logior (aref edid (+ offset 3))
                        (ash (ldb (byte 4 0) (aref edid (+ offset 4))) 8)))
        (hfporch (logior (aref edid (+ offset 8))
                         (ash (ldb (byte 2 6) (aref edid (+ offset 11))) 8)))
        (hsync (logior (aref edid (+ offset 9))
                       (ash (ldb (byte 2 4) (aref edid (+ offset 11))) 8)))
        (hsize (logior (aref edid (+ offset 12))
                       (ash (ldb (byte 4 4) (aref edid (+ offset 14))) 8)))
        (hborder (aref edid (+ offset 15)))
        (vactive (logior (aref edid (+ offset 5))
                         (ash (ldb (byte 4 4) (aref edid (+ offset 7))) 8)))
        (vblank (logior (aref edid (+ offset 6))
                        (ash (ldb (byte 4 0) (aref edid (+ offset 7))) 8)))
        (vfporch (logior (ldb (byte 4 4) (aref edid (+ offset 10)))
                         (ash (ldb (byte 2 2) (aref edid (+ offset 11))) 8)))
        (vsync (logior (ldb (byte 4 0) (aref edid (+ offset 10)))
                       (ash (ldb (byte 2 0) (aref edid (+ offset 11))) 8)))
        (vsize (logior (aref edid (+ offset 13))
                       (ash (ldb (byte 4 0) (aref edid (+ offset 14))) 8)))
        (vborder (aref edid (+ offset 16)))
        (flags (aref edid (+ offset 17))))
    (make-timing
     :pixel-clock pixel-clock
     :horz-left-border hborder
     :horz-active hactive
     :horz-right-border hborder
     :horz-back-porch (- hblank hfporch hsync)
     :horz-sync hsync
     :horz-front-porch hfporch
     :horz-image-size hsize
     :vert-top-border vborder
     :vert-active vactive
     :vert-bottom-border vborder
     :vert-back-porch (- vblank vfporch vsync)
     :vert-sync vsync
     :vert-front-porch vfporch
     :vert-image-size vsize
     :interlaced (logbitp 7 flags)
     :stereo (if (zerop (ldb (byte 2 5) flags))
                 nil
                 (ecase (logior (ash (ldb (byte 2 5) flags) 1)
                                (ldb (byte 1 0) flags))
                   (#b010 :field-sequential-right)
                   (#b110 :field-sequential-left)
                   (#b011 :2-way-interleaved-right)
                   (#b101 :2-way-interleaved-left)
                   (#b110 :4-way-interleaved)
                   (#b111 :side-by-side-interleaved)))
     :sync-config (ldb (byte 4 1) flags))))

(defun decode-edid-detailed-timings (edid)
  ;; This needs to keep ordering correct.
  ;; They're in priority order and entry 0 may be the prefered resolution.
  (loop
     for i below 4
     for offset = (+ 54 (* i 18))
     when (not (and (zerop (aref edid offset))
                         (zerop (aref edid (+ offset 1)))))
     collect (decode-edid-detailed-timing edid offset)))

(defun decode-raw-edid (edid)
  ;; Verify header and checksum.
  (assert (and (eql (aref edid 0) #x00)
               (eql (aref edid 1) #xFF)
               (eql (aref edid 2) #xFF)
               (eql (aref edid 3) #xFF)
               (eql (aref edid 4) #xFF)
               (eql (aref edid 5) #xFF)
               (eql (aref edid 6) #xFF)
               (eql (aref edid 7) #x00))
          () "EDID has invalid header")
  (assert (zerop (logand #xFF (loop for byte across edid summing byte)))
          () "EDID has invalid checksum")
  (when (not (and (eql (aref edid 18) 1)
                  (member (aref edid 19) '(3 4))))
    (warn "Unsupported EDID version ~D.~D"
          (aref edid 18) (aref edid 19)))
  (let ((detailed-timings (decode-edid-detailed-timings edid)))
    (make-edid-data
     :manufacturer-id (mezzano.internals::ub16ref/be edid 8)
     :product-code (mezzano.internals::ub16ref/le edid 10)
     :serial-number (mezzano.internals::ub32ref/le edid 12)
     :week-of-manufacture (aref edid 16)
     :year-of-manufacture (+ 1990 (aref edid 17))
     ;; TODO: These two can be zero indicating an aspect ratio.
     :horizontal-screen-size (aref edid 21)
     :vertical-screen-size (aref edid 22)
     :display-gamma (/ (+ (aref edid 23) 100) 100.0)
     :display-type (if (logbitp 7 (aref edid 20))
                       ;; Digital display
                       (case (ldb (byte 2 3) (aref edid 24))
                         (#b00 :rgb444)
                         (#b01 :rgb444+ycrcb444)
                         (#b10 :rgb444+ycrcb422)
                         (#b11 :rgb444+ycrcb444+ycrcb422))
                       ;; Analog display
                       (case (ldb (byte 2 3) (aref edid 24))
                         (#b00 :greyscale)
                         (#b01 :rgb-colour)
                         (#b10 :colour)
                         (#b11 :analog)))
     :native-format (if (logbitp 1 (aref edid 24))
                        (first detailed-timings)
                        nil)
     :srgb-colour-space (logbitp 2 (aref edid 24))
     :chromacticity (flet ((read (msb lsb-byte lsb-field)
                             (let ((raw (logior (ash (aref edid msb) 2)
                                                (ldb (byte 2 lsb-field)
                                                     (aref edid lsb-byte)))))
                               (/ raw 1024.0))))
                      (make-chromacticity
                       :red-x (read 27 25 6)
                       :red-y (read 28 25 4)
                       :green-x (read 29 25 2)
                       :green-y (read 30 25 0)
                       :blue-x (read 31 25 6)
                       :blue-y (read 32 25 4)
                       :white-x (read 33 25 2)
                       :white-y (read 34 25 0)))
     :timing-modes (append
                    detailed-timings
                    (decode-edid-established-timings edid)
                    (decode-edid-standard-timings edid)))))

(defun intel-gma-probe (pci-device)
  (format t "Detected intel-gma at ~S~%" pci-device)
  (let* ((card (make-instance 'intel-gma
                              :device pci-device
                              :registers (pci:pci-io-region pci-device +pci-bar-mmadr+)
                              :gm (pci:pci-io-region pci-device +pci-bar-gmadr+)
                              :gtt (pci:pci-io-region pci-device +pci-bar-gttadr+)))
         (mode (save-current-mode-parameters card))
         (framebuffer (framebuffer-address card)))
    (pci:pci-io-region pci-device +pci-bar-iobar+)
    (setf *card* card)
    ;; Take ownership of the compositor framebuffer.
    (when (and (eql (sup::framebuffer-layout (sup:current-framebuffer)) :x8r8g8b8)
               (eql (framebuffer-address card)
                    (sup::framebuffer-base-address (sup:current-framebuffer))))
      (sup::video-set-framebuffer
       framebuffer
       (timing-horz-active mode)
       (timing-vert-active mode)
       (* (timing-horz-active mode) 4)
       :x8r8g8b8
       :device card)
      (mezzano.gui.compositor:force-redisplay t))
    (setf (slot-value card '%edid)
          (ignore-errors (decode-raw-edid (read-raw-edid card))))
    card))

;; Scary warning:
;;   Be careful adding new IDs here.
;;   Some chipsets (especially laptops) are very sensitive and can
;;   be bricked by bugs.
(pci:define-pci-driver intel-gma intel-gma-probe
  ((#x8086 #x2772))
  ())
