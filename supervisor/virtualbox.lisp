;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;;; Support for VirtualBox window resizing and mouse integration.
;;; http://wiki.osdev.org/VirtualBox_Guest_Additions

(defconstant +vbox-vmmdev-version+         #x00010003)
(defconstant +vbox-request-header-version+ #x00010001)

(defmacro define-vbox-packet (name request-id fields)
  (let ((issue (intern (format nil "VBOX-ISSUE-~A" name)
                       (symbol-package name)))
        (vbox (gensym "VBOX"))
        (phys (gensym "PHYS")))
    `(defun ,issue (,vbox ,@(loop
                               for (field direction) in fields
                               when (eql direction :input)
                               collect field))
       (let ((,phys (virtualbox-guest-device-request-page ,vbox)))
         ;; Header.
         (setf (physical-memref-unsigned-byte-32 ,phys 0) ,(+ 24 (* (length fields) 4)) ; total size.
               (physical-memref-unsigned-byte-32 ,phys 1) +vbox-request-header-version+
               (physical-memref-unsigned-byte-32 ,phys 2) ,request-id
               (physical-memref-unsigned-byte-32 ,phys 3) 0 ; return code
               (physical-memref-unsigned-byte-32 ,phys 4) 0 ; reserved
               (physical-memref-unsigned-byte-32 ,phys 5) 0) ; reserved
         ,@(loop
              for i from 6
              for (field direction) in fields
              collect `(setf (physical-memref-unsigned-byte-32 ,phys ,i) ,(if (eql direction :input)
                                                                              field
                                                                              0)))
         (setf (pci-io-region/32 (virtualbox-guest-device-kick-port ,vbox) 0) ,phys)
         (values
          (physical-memref-unsigned-byte-32 ,phys 3)
          ,@(loop
               for i from 6
               for (field direction) in fields
               when (eql direction :output)
               collect `(physical-memref-unsigned-byte-32 ,phys ,i)))))))

(define-vbox-packet guest-info 50
  ((version :input)
   (ostype :input)))

(define-vbox-packet set-guest-caps 55
  ((caps :input)))

(define-vbox-packet ack-events 41
  ((events :input)))

(define-vbox-packet get-display-change 51
  ((xres :output)
   (yres :output)
   (bpp :output)
   (eventack :input)))

(define-vbox-packet get-mouse 1
  ((features :input)
   (x :output)
   (y :output)))

(define-vbox-packet set-mouse 2
  ((features :input)
   (x :input)
   (y :input)))

(sys.int::defglobal *vbox-event-fifo*)

(defstruct (virtualbox-guest-device
             (:area :wired))
  pci-device
  kick-port
  irq-handler-function
  request-page)

(defconstant +vbox-mouse-features+
  ;; bit 0 says "guest supports (and wants) absolute mouse"
  ;; bit 4 says we'll query absolute positions on interrupts
  (logior (ash 1 0) (ash 1 4)))

(sys.int::defglobal *vbox-screen-xres*)
(sys.int::defglobal *vbox-screen-yres*)

(defun virtualbox-irq-handler (vbox)
  (multiple-value-bind (rc xres yres bpp)
      (vbox-issue-get-display-change vbox 1)
    (declare (ignore rc))
    (when (not (or (zerop xres)
                   (zerop yres)
                   (zerop bpp)
                   (and (eql *vbox-screen-xres* xres)
                        (eql *vbox-screen-yres* yres))))
      ;; Screen size has changed. Resize the display and send an event
      ;; to the listener thread to update the video framebuffer.
      (debug-print-line "New screen geometry is " xres "x" yres "x" bpp)
      (setf *vbox-screen-xres* xres
            *vbox-screen-yres* yres)
      (bochs-vbe-switch-resolution xres yres)
      (irq-fifo-push :screen-geometry-changed
                     *vbox-event-fifo*))
    (multiple-value-bind (rc x y)
        (vbox-issue-get-mouse vbox +vbox-mouse-features+)
      (declare (ignore rc))
      ;; Provided mouse coordinates are scaled to [0,#xFFFF], scale them
      ;; back to the screen geometry.
      (let ((scaled-x (truncate (* x *vbox-screen-xres*) #xFFFF))
            (scaled-y (truncate (* y *vbox-screen-yres*) #xFFFF)))
        (irq-fifo-push (logior (logand scaled-x #xFFFF)
                               (ash (logand scaled-y #xFFFF) 16))
                       *vbox-event-fifo*))))
  (vbox-issue-ack-events vbox 0))

(defun vbox-ensure-fifo-exists ()
  (when (not (boundp '*vbox-event-fifo*))
    ;; Create a new fifo in the unlikely event that this image has never
    ;; been booted in virtualbox.
    (let ((new-fifo (make-irq-fifo 50 :name "VirtualBox event FIFO")))
      (safe-without-interrupts (new-fifo)
        (when (not (boundp '*vbox-event-fifo*))
          (setf *vbox-event-fifo* new-fifo))))))

(defun virtualbox-read-event (&optional (wait-p t))
  (vbox-ensure-fifo-exists)
  (let ((value (irq-fifo-pop *vbox-event-fifo* wait-p)))
    (if (integerp value)
        ;; Decode mouse position
        (values (ldb (byte 16 0) value)
                (ldb (byte 16 16) value))
        ;; Otherwise leave event intact.
        value)))

(defun virtualbox-guest-register (device)
  (declare (sys.c::closure-allocation :wired))
  (vbox-ensure-fifo-exists)
  (irq-fifo-reset *vbox-event-fifo*)
  (let* ((irq (pci-intr-line device))
         (kick-port (pci-bar device 0))
         (state-region (pci-bar device 1))
         (request-page (allocate-physical-pages 1
                                                :mandatory-p "VirtualBox Guest request page"
                                                :32-bit-only t))
         (vbox (make-virtualbox-guest-device :pci-device device
                                             :kick-port kick-port
                                             :request-page (ash request-page 12)))
         (handler (lambda (interrupt-frame irq)
                    (declare (ignore interrupt-frame irq))
                    (virtualbox-irq-handler vbox))))
    (setf (virtualbox-guest-device-irq-handler-function vbox) handler)
    (pci-io-region device 0 4)
    (pci-io-region device 1 32)
    (debug-print-line "VirtualBox Guest Device detected at " device " using IRQ " irq)
    (i8259-hook-irq irq handler)
    (i8259-unmask-irq irq)
    ;; Take the initial screen dimensions from the bootloader's framebuffer.
    (setf *vbox-screen-xres* (boot-field +boot-information-framebuffer-width+)
          *vbox-screen-yres* (boot-field +boot-information-framebuffer-height+))
    ;; This is an Unknown 64-bit OS.
    (vbox-issue-guest-info vbox +vbox-vmmdev-version+ #x100)
    ;; Support auto-resize guest display.
    (vbox-issue-set-guest-caps vbox (ash 1 2))
    ;; Enable mouse integration.
    (vbox-issue-set-mouse vbox +vbox-mouse-features+ 0 0)
    ;; Enable interrupts for these capabilities.
    (setf (pci-io-region/32 state-region 12) #xFFFFFFFF)
    vbox))

(defconstant +bochs-vbe-index-port+ #x01CE)
(defconstant +bochs-vbe-data-port+  #x01CF)

(defun bochs-vbe-write (index value)
  (setf (sys.int::io-port/16 +bochs-vbe-index-port+) index
        (sys.int::io-port/16 +bochs-vbe-data-port+) value))

(defun bochs-vbe-read (index)
  (setf (sys.int::io-port/16 +bochs-vbe-index-port+) index)
  (sys.int::io-port/16 +bochs-vbe-data-port+))

(defun bochs-vbe-switch-resolution (xres yres)
  ;; Disable.
  (bochs-vbe-write 4 0)
  ;; Set width/height and bpp.
  (bochs-vbe-write 1 xres)
  (bochs-vbe-write 2 yres)
  (bochs-vbe-write 3 32)
  ;; Enable display & LFB.
  (bochs-vbe-write 4 #x41)
  ;; Clear y offset.
  (bochs-vbe-write 9 0))

(sys.int::defglobal *virtualbox-graphics-boot-id* nil)
(sys.int::defglobal *virtualbox-graphics-fb-address* nil)

(defun virtualbox-graphics-register (device)
  (setf *virtualbox-graphics-boot-id* (current-boot-id)
        *virtualbox-graphics-fb-address* (logand (pci-bar device 0)
                                                 #xFFFFF000))
  :virtualbox-graphics)

(defun virtualbox-graphics-update-framebuffer ()
  (incf *snapshot-inhibit*)
  (when (eql *virtualbox-graphics-boot-id* (current-boot-id))
    (map-physical-memory *virtualbox-graphics-fb-address*
                         (align-up (* *vbox-screen-xres* *vbox-screen-yres* 4)
                                   #x1000)
                         "Framebuffer")
    (video-set-framebuffer *virtualbox-graphics-fb-address*
                           *vbox-screen-xres*
                           *vbox-screen-yres*
                           (* *vbox-screen-xres* 4)
                           :x8r8g8b8
                           nil))
  (decf *snapshot-inhibit*))
