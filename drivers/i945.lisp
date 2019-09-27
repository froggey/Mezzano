;;;; Intel 82945G/GZ Integrated Graphics Controller driver

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

(defpackage :mezzano.driver.i945
  (:use :cl)
  (:local-nicknames (:pci :mezzano.supervisor.pci)
                    (:sup :mezzano.supervisor)
                    (:sync :mezzano.sync)))

(in-package :mezzano.driver.i945)

(defvar *card*) ; for testing.

(defclass i945 ()
  ((%device :initarg :device :reader i945-device)
   (%registers :initarg :registers :reader i945-registers)
   (%lock :reader i945-lock)))

(defmethod initialize-instance :after ((instance i945) &key)
  (setf (slot-value instance '%lock) (sup:make-mutex instance)))

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

(defstruct mode-parameters
  width
  height
  format
  ;; PLL parameters
  n
  m1
  m2
  p1
  p2
  ;; Display timings
  htotal-active
  htotal-total
  hblank-start
  hblank-end
  hsync-start
  hsync-end
  vtotal-active
  vtotal-total
  vblank-start
  vblank-end
  vsync-start
  vsync-end)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod make-load-form ((object mode-parameters) &optional environment)
    (make-load-form-saving-slots object :environment environment)))

(defparameter *modes*
  '(#S(mode-parameters
       :width 1600 :height 1200 :format :x8r8g8b8
       :n 4 :m1 16 :m2 9 :p1 1 :p2 0
       :htotal-active 1599 :htotal-total 2159
       :hblank-start 1599 :hblank-end 2159
       :hsync-start 1663 :hsync-end 1855
       :vtotal-active 1199 :vtotal-total 1249
       :vblank-start 1199 :vblank-end 1249
       :vsync-start 1200 :vsync-end 1203)
    #S(mode-parameters
       :width 1280 :height 1024 :format :x8r8g8b8
       :n 2 :m1 14 :m2 8 :p1 2 :p2 0
       :htotal-active 1279 :htotal-total 1687
       :hblank-start 1279 :hblank-end 1687
       :hsync-start 1327 :hsync-end 1439
       :vtotal-active 1023 :vtotal-total 1065
       :vblank-start 1023 :vblank-end 1065
       :vsync-start 1024 :vsync-end 1027)))

(defmacro with-i945-access ((device) &body body)
  `(sup:with-device-access ((pci:pci-device-boot-id (i945-device ,device))
                            (error "~S is stale" ,device))
     (sup:with-mutex ((i945-lock ,device))
       ,@body)))

(defun save-current-mode-parameters (device)
  "Return a MODE-PARAMETERS object for the device's currently configured mode."
  (with-i945-access (device)
    (let ((mmio (i945-registers device)))
      (flet ((reg (idx)
               (pci:pci-io-region/32 mmio idx)))
        (let* ((dplla-ctrl (reg +dplla-ctrl+))
               (fpa0 (reg +fpa0+))
               (htotal-a (reg +htotal-a+))
               (hblank-a (reg +hblank-a+))
               (hsync-a (reg +hsync-a+))
               (vtotal-a (reg +vtotal-a+))
               (vblank-a (reg +vblank-a+))
               (vsync-a (reg +vsync-a+))
               (pipeasrc (reg +pipeasrc+))
               (dspacntr (reg +dspacntr+)))
          (make-mode-parameters
           :width (1+ (ldb +pipesrc-horz+ pipeasrc))
           :height (1+ (ldb +pipesrc-vert+ pipeasrc))
           :format (case (ldb +dspcntr-format+ dspacntr)
                     (#b0110 :x8r8g8b8)
                     (t (ldb +dspcntr-format+ dspacntr)))
           :n (ldb +pll-n-divisor+ fpa0)
           :m1 (ldb +pll-m1-divisor+ fpa0)
           :m2 (ldb +pll-m2-divisor+ fpa0)
           :p1 (ldb +dpll-ctrl-p1+ dplla-ctrl)
           :p2 (ldb +dpll-ctrl-p2+ dplla-ctrl)
           :htotal-active (ldb +htotal-active+ htotal-a)
           :htotal-total (ldb +htotal-total+ htotal-a)
           :hblank-start (ldb +hblank-start+ hblank-a)
           :hblank-end (ldb +hblank-end+ hblank-a)
           :hsync-start (ldb +hsync-start+ hsync-a)
           :hsync-end (ldb +hsync-end+ hsync-a)
           :vtotal-active (ldb +vtotal-active+ vtotal-a)
           :vtotal-total (ldb +vtotal-total+ vtotal-a)
           :vblank-start (ldb +vblank-start+ vblank-a)
           :vblank-end (ldb +vblank-end+ vblank-a)
           :vsync-start (ldb +vsync-start+ vsync-a)
           :vsync-end (ldb +vsync-end+ vsync-a)))))))

(defun framebuffer-address (device)
  (logand (pci:pci-io-region (i945-device device) 2) (lognot #xF)))

(defun mode-switch (device mode)
  (when (not (eql (mode-parameters-format mode) :x8r8g8b8))
    (error "Mode ~S has unsupported format." mode))
  (with-i945-access (device)
    ;; Update the video framebuffer.
    ;; Do this first so that the compositor updates display before
    ;; the new mode is set and nothing is displayed with the wrong stride.
    (sup::video-set-framebuffer
     (framebuffer-address device)
     (mode-parameters-width mode)
     (mode-parameters-height mode)
     (* (mode-parameters-width mode) 4)
     :x8r8g8b8
     :device device)
    (mezzano.gui.compositor:force-redisplay t)
    ;; Set mode.
    (let ((mmio (i945-registers device)))
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
        (setf (ldb +dpll-ctrl-p1+ (reg +dplla-ctrl+)) (mode-parameters-p1 mode)
              (ldb +dpll-ctrl-p2+ (reg +dplla-ctrl+)) (mode-parameters-p2 mode)
              (ldb +pll-n-divisor+ (reg +fpa0+)) (mode-parameters-n mode)
              (ldb +pll-m1-divisor+ (reg +fpa0+)) (mode-parameters-m1 mode)
              (ldb +pll-m2-divisor+ (reg +fpa0+)) (mode-parameters-m2 mode))
        ;; Enable DPLL
        (setf (ldb +dpll-vco-enable+ (reg +dplla-ctrl+)) 1)
        ;; Wait for DPLL warmup (150us)
        (sleep 0.00015)
        ;; Program pipe timings (can be done before DPLL setup)
        (setf (ldb +htotal-active+ (reg +htotal-a+)) (mode-parameters-htotal-active mode)
              (ldb +htotal-total+ (reg +htotal-a+)) (mode-parameters-htotal-total mode)
              (ldb +hblank-start+ (reg +hblank-a+)) (mode-parameters-hblank-start mode)
              (ldb +hblank-end+ (reg +hblank-a+)) (mode-parameters-hblank-end mode)
              (ldb +hsync-start+ (reg +hsync-a+)) (mode-parameters-hsync-start mode)
              (ldb +hsync-end+ (reg +hsync-a+)) (mode-parameters-hsync-end mode)
              (ldb +vtotal-active+ (reg +vtotal-a+)) (mode-parameters-vtotal-active mode)
              (ldb +vtotal-total+ (reg +vtotal-a+)) (mode-parameters-vtotal-total mode)
              (ldb +vblank-start+ (reg +vblank-a+)) (mode-parameters-vblank-start mode)
              (ldb +vblank-end+ (reg +vblank-a+)) (mode-parameters-vblank-end mode)
              (ldb +vsync-start+ (reg +vsync-a+)) (mode-parameters-vsync-start mode)
              (ldb +vsync-end+ (reg +vsync-a+)) (mode-parameters-vsync-end mode))
        ;; Enable pipe
        (setf (ldb +pipesrc-horz+ (reg +pipeasrc+)) (1- (mode-parameters-width mode))
              (ldb +pipesrc-vert+ (reg +pipeasrc+)) (1- (mode-parameters-height mode)))
        (setf (ldb +pipeconf-enable+ (reg +pipeaconf+)) 1)
        ;; Enable planes
        (setf (reg +dspastride+) (* (mode-parameters-width mode) 4)
              (reg +dspasurf+) 0
              (ldb +dspcntr-plane-enable+ (reg +dspacntr+)) 1
              (reg +dspalinoff+) 0)
        ;; Enable ports
        (setf (ldb +adpa-enable+ (reg +adpa+)) 1))))
  (values))

(defun dump-display-state (device)
  "Dump the clock, pipe, and display registers."
  (with-i945-access (device)
    (let ((mmio (i945-registers device)))
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

(defun i945-probe (pci-device)
  (let ((registers (pci:pci-io-region pci-device 0)))
    (pci:pci-io-region pci-device 1)
    (pci:pci-io-region pci-device 2)
    (pci:pci-io-region pci-device 3)
    (let ((card (make-instance 'i945
                               :device pci-device
                               :registers registers)))
      (setf *card* card)
      (format t "Detected i945 at ~S~%" pci-device)
      (dump-display-state card)
      (let ((mode (save-current-mode-parameters card))
            (framebuffer (framebuffer-address card)))
        ;; Take ownership of the compositor framebuffer.
        (when (and (eql (mode-parameters-format mode) :x8r8g8b8)
                   (eql (framebuffer-address card)
                        (sup::framebuffer-base-address (sup:current-framebuffer))))
          (sup::video-set-framebuffer
           framebuffer
           (mode-parameters-width mode)
           (mode-parameters-height mode)
           (* (mode-parameters-width mode) 4)
           :x8r8g8b8
           :device card)
          (mezzano.gui.compositor:force-redisplay t)))
      card)))

(pci:define-pci-driver i945 i945-probe
  ((#x8086 #x2772))
  ())
