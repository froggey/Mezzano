(in-package #:sys.int)

(defconstant +vbe-dispi-max-xres+              1600)
(defconstant +vbe-dispi-max-yres+              1200)
(defconstant +vbe-dispi-max-bpp+               32)

(defconstant +vbe-dispi-ioport-index+          #x01CE)
(defconstant +vbe-dispi-ioport-data+           #x01CF)

(defconstant +vbe-dispi-index-id+              0) ; id0
(defconstant +vbe-dispi-index-xres+            1) ; id0
(defconstant +vbe-dispi-index-yres+            2) ; id0
(defconstant +vbe-dispi-index-bpp+             3) ; id0
(defconstant +vbe-dispi-index-enable+          4) ; id0
(defconstant +vbe-dispi-index-bank+            5) ; id0
(defconstant +vbe-dispi-index-virt-width+      6) ; id1
(defconstant +vbe-dispi-index-virt-height+     7) ; id1
(defconstant +vbe-dispi-index-x-offset+        8) ; id1
(defconstant +vbe-dispi-index-y-offset+        9) ; id1
(defconstant +vbe-dispi-index-video-memory+    10) ; id5

(defconstant +vbe-dispi-id0+                   #xB0C0)
(defconstant +vbe-dispi-id1+                   #xB0C1)
(defconstant +vbe-dispi-id2+                   #xB0C2)
(defconstant +vbe-dispi-id3+                   #xB0C3)
(defconstant +vbe-dispi-id4+                   #xB0C4)
(defconstant +vbe-dispi-id5+                   #xB0C5)

(defconstant +vbe-dispi-disabled+              #x00) ; id0
(defconstant +vbe-dispi-enabled+               #x01) ; id0
(defconstant +vbe-dispi-getcaps+               #x02) ; id3
(defconstant +vbe-dispi-8bit-dac+              #x20) ; id3
(defconstant +vbe-dispi-lfb-enabled+           #x40) ; id2
(defconstant +vbe-dispi-noclearmem+            #x80) ; id2

(defconstant +vbe-dispi-lfb-physical-address+  #xE0000000)
(defvar *bochs-vbe-version* nil)
(defvar *bochs-max-x-res* nil)
(defvar *bochs-max-y-res* nil)
(defvar *bochs-vbe-framebuffer-address* nil)
(defvar *bochs-vram-size* nil)

(defvar *bochs-flip-mode* nil ; is the greatest
  "When true, *bochs-back-buffer* refers to the first buffer in video memory (y offset zero) and
the front buffer refers to the second buffer.")
(defvar *bochs-framebuffer* nil)
(defvar *bochs-back-buffer* nil)

(defun write-vbe-reg (index value)
  (setf (io-port/16 +vbe-dispi-ioport-index+) index
	(io-port/16 +vbe-dispi-ioport-data+) value))

(defun read-vbe-reg (index)
  (setf (io-port/16 +vbe-dispi-ioport-index+) index)
  (io-port/16 +vbe-dispi-ioport-data+))

(defun probe-bochs-vbe ()
  (write-vbe-reg +vbe-dispi-index-id+ +vbe-dispi-id0+)
  (write-vbe-reg +vbe-dispi-index-id+ +vbe-dispi-id1+)
  (write-vbe-reg +vbe-dispi-index-id+ +vbe-dispi-id2+)
  (write-vbe-reg +vbe-dispi-index-id+ +vbe-dispi-id3+)
  (write-vbe-reg +vbe-dispi-index-id+ +vbe-dispi-id4+)
  (write-vbe-reg +vbe-dispi-index-id+ +vbe-dispi-id5+)
  (let ((id (read-vbe-reg +vbe-dispi-index-id+)))
    (when (= (logand id #xfff0) #xb0c0)
      (setf *bochs-vbe-version* (logand id #xF))
      (format t "Bochs VBE adaptor present. Version ~S~%" *bochs-vbe-version*)
      (when (< id +vbe-dispi-id2+)
        (format t " Adapter too old, doesn't support LFB.~%")
        (setf *bochs-vbe-version* nil)
        (return-from probe-bochs-vbe))
      (cond ((>= id +vbe-dispi-id3+)
             (write-vbe-reg +vbe-dispi-index-enable+ +vbe-dispi-getcaps+)
             (setf *bochs-max-x-res* (read-vbe-reg +vbe-dispi-index-xres+)
                   *bochs-max-y-res* (read-vbe-reg +vbe-dispi-index-yres+)))
            (t
             (setf *bochs-max-x-res* +vbe-dispi-max-xres+
                   *bochs-max-y-res* +vbe-dispi-max-yres+)))
      (format t " Maximum resolution: ~Sx~Sx~S~%"
              *bochs-max-x-res*
              *bochs-max-y-res*
              (read-vbe-reg +vbe-dispi-index-bpp+))
      (cond ((>= id +vbe-dispi-id5+)
             (setf *bochs-vram-size* (* (read-vbe-reg +vbe-dispi-index-video-memory+) 64 1024)))
            ((>= id +vbe-dispi-id4+)
             (setf *bochs-vram-size* (* 8 1024 1024)))
            ;; I have no idea what this should really be.
            (t (setf *bochs-vram-size* (* 4 1024 1024))))
      (format t " ~DMB of display memory available.~%"
              (/ *bochs-vram-size* 1024 1024))
      (let ((framebuffer (pci-get-lfb-addr #x1234 #x1111)))
	(if framebuffer
            (setf *bochs-vbe-framebuffer-address* framebuffer)
            (setf *bochs-vbe-framebuffer-address* +vbe-dispi-lfb-physical-address+)))
      (format t " Framebuffer at #x~X~%" *bochs-vbe-framebuffer-address*)
      t)))

(defun set-bochs-vbe-mode (xres yres bpp)
  (write-vbe-reg +vbe-dispi-index-enable+ +vbe-dispi-disabled+)
  (write-vbe-reg +vbe-dispi-index-xres+ xres)
  (write-vbe-reg +vbe-dispi-index-yres+ yres)
  (write-vbe-reg +vbe-dispi-index-bpp+ bpp)
  (write-vbe-reg +vbe-dispi-index-enable+ (logior +vbe-dispi-enabled+ +vbe-dispi-lfb-enabled+))
  (write-vbe-reg +vbe-dispi-index-y-offset+ 0))

(defun pci-get-lfb-addr (vendor-id device-id)
  (dolist (dev *pci-devices*)
    (when (and (eql (pci-device-vendor-id dev) vendor-id)
	       (eql (pci-device-device-id dev) device-id))
      (let ((data (pci-bar dev 0)))
	(when (not (eql (logand data #xFFF1) 0))
	  (return nil))
	(return (logand data #xFFFF0000))))))

(defun set-gc-light ()
  (when (and (boundp '*bochs-framebuffer*)
             *bochs-framebuffer*)
    (dotimes (i 8)
      (setf (aref *bochs-framebuffer* 0 i) (ldb (byte 24 0) (lognot (aref *bochs-framebuffer* 0 i)))))))

(defun clear-gc-light ()
  (when (and (boundp '*bochs-framebuffer*)
             *bochs-framebuffer*)
    (dotimes (i 8)
      (setf (aref *bochs-framebuffer* 0 i) (ldb (byte 24 0) (lognot (aref *bochs-framebuffer* 0 i)))))))

(defun bochs-vbe-early-initialize ()
  (setf *bochs-framebuffer* nil
        *bochs-vbe-version* nil))
(add-hook '*early-initialize-hook* 'bochs-vbe-early-initialize)

(defun bochs-change-res (width height)
  (when (null *bochs-vbe-version*)
    (error "Bochs VBE device not present."))
  (when (or (> width *bochs-max-x-res*)
            (> height *bochs-max-y-res*))
    (error "Requested size ~Dx~D exceeds maximum ~Dx~D~%"
           width height
           *bochs-max-x-res* *bochs-max-y-res*))
  (when (> (* width height 4 2) *bochs-vram-size*)
    (error "Requested size ~Dx~D does not fit in video memory. Wanted ~DMB, ~DMB available."
           (/ (* width height 4 2) 1024 1024)
           (/ *bochs-vram-size* 1024 1024)))
  (set-bochs-vbe-mode width height 32)
  (setf *bochs-framebuffer* (make-array (list height width)
                                        :element-type '(unsigned-byte 32)
                                        :memory (+ #x8000000000 *bochs-vbe-framebuffer-address*))
        *bochs-back-buffer* (make-array (list height width)
                                        :element-type '(unsigned-byte 32)
                                        :memory (+ #x8000000000 *bochs-vbe-framebuffer-address* (* width height 4)))
        *bochs-flip-mode* nil)
  (setf *terminal-io* (make-instance 'framebuffer-stream
                                          :framebuffer *bochs-framebuffer*)))

(defun bochs-flip-buffer ()
  (rotatef *bochs-framebuffer* *bochs-back-buffer*)
  (setf *bochs-flip-mode* (not *bochs-flip-mode*))
  (write-vbe-reg +vbe-dispi-index-y-offset+
                 (if *bochs-flip-mode*
                     (array-dimension *bochs-framebuffer* 0)
                     0))
  (write-vbe-reg +vbe-dispi-index-virt-width+ (array-dimension *bochs-framebuffer* 1)))

(defun bochs-vbe-initialize ()
  (when (probe-bochs-vbe)
    (bochs-change-res 1024 768)
    #+nil(sys.graphics:register-screen "Bochs" *bochs-framebuffer*)))
(add-hook '*initialize-hook* 'bochs-vbe-initialize)
