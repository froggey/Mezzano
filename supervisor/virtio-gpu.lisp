;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant +virtio-gpu-controlq+ 0)
(defconstant +virtio-gpu-cursorq+  1)

(defconstant +virtio-gpu-config-events-read+  0)
(defconstant +virtio-gpu-config-events-clear+ 4)
(defconstant +virtio-gpu-config-num-scanouts+ 8)

;;; 2d commands
(defconstant +virtio-gpu-cmd-get-display-info+         #x0100)
(defconstant +virtio-gpu-cmd-resource-create-2d+       #x0101)
(defconstant +virtio-gpu-cmd-resource-unref+           #x0102)
(defconstant +virtio-gpu-cmd-set-scanout+              #x0103)
(defconstant +virtio-gpu-cmd-resource-flush+           #x0104)
(defconstant +virtio-gpu-cmd-transfer-to-host-2d+      #x0105)
(defconstant +virtio-gpu-cmd-resource-attach-backing+  #x0106)
(defconstant +virtio-gpu-cmd-resource-detach-backing+  #x0107)

;; cursor commands
(defconstant +virtio-gpu-cmd-update-cursor+            #x0300)
(defconstant +virtio-gpu-cmd-move-cursor+              #x0301)

;; success responses
(defconstant +virtio-gpu-resp-ok-nodata+               #x1100)
(defconstant +virtio-gpu-resp-ok-display-info+         #x1101)

;; error responses
(defconstant +virtio-gpu-resp-err-unspec+              #x1200)
(defconstant +virtio-gpu-resp-err-out-of-memory+       #x1201)
(defconstant +virtio-gpu-resp-err-invalid-scanout-id+  #x1202)
(defconstant +virtio-gpu-resp-err-invalid-resource-id+ #x1203)
(defconstant +virtio-gpu-resp-err-invalid-context-id+  #x1204)
(defconstant +virtio-gpu-resp-err-invalid-parameter+   #x1205)

(defconstant +virtio-gpu-flag-fence+ (ash 1 0))

(defconstant +virtio-gpu-ctrl-hdr-type+     0)
(defconstant +virtio-gpu-ctrl-hdr-flags+    4)
(defconstant +virtio-gpu-ctrl-hdr-fence-id+ 8)
(defconstant +virtio-gpu-ctrl-hdr-ctx-id+   16)

(defconstant +virtio-gpu-ctrl-hdr-size+     24)

(defconstant +virtio-gpu-max-scanouts+    16)

(defconstant +virtio-gpu-display-x+        0)
(defconstant +virtio-gpu-display-y+        4)
(defconstant +virtio-gpu-display-width+    8)
(defconstant +virtio-gpu-display-height+  12)
(defconstant +virtio-gpu-display-enabled+ 16)
(defconstant +virtio-gpu-display-flags+   20)

(defconstant +virtio-gpu-display-size+    24)

(defconstant +virtio-gpu-format-b8g8r8a8-unorm+ 1)
(defconstant +virtio-gpu-format-b8g8r8x8-unorm+ 2)
(defconstant +virtio-gpu-format-a8r8g8b8-unorm+ 3)
(defconstant +virtio-gpu-format-x8r8g8b8-unorm+ 4)
(defconstant +virtio-gpu-format-r8g8b8a8-unorm+ 67)
(defconstant +virtio-gpu-format-x8b8g8r8-unorm+ 68)
(defconstant +virtio-gpu-format-a8b8g8r8-unorm+ 121)
(defconstant +virtio-gpu-format-r8g8b8x8-unorm+ 134)

(defconstant +virtio-gpu-resource-create-2d-resource-id+ 0)
(defconstant +virtio-gpu-resource-create-2d-format+      4)
(defconstant +virtio-gpu-resource-create-2d-width+       8)
(defconstant +virtio-gpu-resource-create-2d-height+     12)

(defconstant +virtio-gpu-resource-create-2d-size+       16)

(defconstant +virtio-gpu-resource-unref-resource-id+ 0)

(defconstant +virtio-gpu-resource-unref-size+ 8)

(defconstant +virtio-gpu-set-scanout-x+ 0)
(defconstant +virtio-gpu-set-scanout-y+ 4)
(defconstant +virtio-gpu-set-scanout-width+ 8)
(defconstant +virtio-gpu-set-scanout-height+ 12)
(defconstant +virtio-gpu-set-scanout-scanout-id+ 16)
(defconstant +virtio-gpu-set-scanout-resource-id+ 20)

(defconstant +virtio-gpu-set-scanout-size+ 24)

(defconstant +virtio-gpu-resource-flush-x+ 0)
(defconstant +virtio-gpu-resource-flush-y+ 4)
(defconstant +virtio-gpu-resource-flush-width+ 8)
(defconstant +virtio-gpu-resource-flush-height+ 12)
(defconstant +virtio-gpu-resource-flush-resource-id+ 16)

(defconstant +virtio-gpu-resource-flush-size+ 24)

(defconstant +virtio-gpu-transfer-to-host-2d-x+ 0)
(defconstant +virtio-gpu-transfer-to-host-2d-y+ 4)
(defconstant +virtio-gpu-transfer-to-host-2d-width+ 8)
(defconstant +virtio-gpu-transfer-to-host-2d-height+ 12)
(defconstant +virtio-gpu-transfer-to-host-2d-offset+ 16)
(defconstant +virtio-gpu-transfer-to-host-2d-resource-id+ 24)

(defconstant +virtio-gpu-transfer-to-host-2d-size+ 32)

(defconstant +virtio-gpu-resource-attach-backing-resource-id+ 0)
(defconstant +virtio-gpu-resource-attach-backing-nr-entries+ 4)
(defconstant +virtio-gpu-resource-attach-backing-entry0+ 8)

(defconstant +virtio-gpu-mem-entry-addr+   0)
(defconstant +virtio-gpu-mem-entry-length+ 8)

(defconstant +virtio-gpu-mem-entry-size+ 16)

(defconstant +virtio-gpu-resource-detach-backing-resource-id+ 0)

(defconstant +virtio-gpu-resource-detach-backing-size+ 8)

(defconstant +virtio-gpu-update-cursor-scanout-id+ 0)
(defconstant +virtio-gpu-update-cursor-x+ 4)
(defconstant +virtio-gpu-update-cursor-y+ 8)
(defconstant +virtio-gpu-update-cursor-resource-id+ 16)
(defconstant +virtio-gpu-update-cursor-hot-x+ 20)
(defconstant +virtio-gpu-update-cursor-hot-y+ 24)

(defconstant +virtio-gpu-update-cursor-size+ 32)

(defconstant +virtio-gpu-framebuffer-resource-id+ 123)

(defstruct (virtio-gpu
             (:area :wired))
  virtio-device
  request-phys
  request-virt
  scanout
  width
  height)

(defun virtio-gpu-command-address (gpu &optional (offset 0))
  (+ (virtio-gpu-request-virt gpu)
     +virtio-gpu-ctrl-hdr-size+
     offset))

(defun virtio-gpu-response-address (gpu &optional (offset 0))
  (+ (virtio-gpu-request-virt gpu)
     2048
     +virtio-gpu-ctrl-hdr-size+
     offset))

(defun virtio-gpu-configure-command (gpu type flags fence-id ctx-id)
  (let ((addr (virtio-gpu-request-virt gpu)))
    (setf (sys.int::memref-unsigned-byte-32 (+ addr +virtio-gpu-ctrl-hdr-type+) 0) type
          (sys.int::memref-unsigned-byte-32 (+ addr +virtio-gpu-ctrl-hdr-flags+) 0) flags
          (sys.int::memref-unsigned-byte-32 (+ addr +virtio-gpu-ctrl-hdr-fence-id+) 0) fence-id
          (sys.int::memref-unsigned-byte-32 (+ addr +virtio-gpu-ctrl-hdr-ctx-id+) 0) ctx-id)))

(defun virtio-gpu-issue-command (gpu command-length response-length)
  (let* ((req-phys (virtio-gpu-request-phys gpu))
         (dev (virtio-gpu-virtio-device gpu))
         (vq (virtio-virtqueue dev +virtio-gpu-controlq+))
         (cmd-desc (virtio-ring-alloc-descriptor vq))
         (rsp-desc (virtio-ring-alloc-descriptor vq)))
    (ensure cmd-desc)
    (ensure rsp-desc)
    #+(or)
    (debug-print-line "dev:" dev " clen:" command-length " rlen:" response-length " cmdd:" cmd-desc " rspd:" rsp-desc)
    ;; Command descriptor.
    (setf (virtio-ring-desc-address vq cmd-desc) req-phys
          (virtio-ring-desc-length vq cmd-desc) (+ +virtio-gpu-ctrl-hdr-size+ command-length)
          (virtio-ring-desc-flags vq cmd-desc) (ash 1 +virtio-ring-desc-f-next+)
          (virtio-ring-desc-next vq cmd-desc) rsp-desc)
    ;; Response descriptor.
    (setf (virtio-ring-desc-address vq rsp-desc) (+ req-phys 2048)
          (virtio-ring-desc-length vq rsp-desc) (+ +virtio-gpu-ctrl-hdr-size+ response-length)
          (virtio-ring-desc-flags vq rsp-desc) (ash 1 +virtio-ring-desc-f-write+)
          (virtio-ring-desc-next vq rsp-desc) 0)
    ;; Issue command & await completion.
    (let ((last-used (virtio-ring-used-idx vq)))
      (virtio-ring-add-to-avail-ring vq cmd-desc)
      (virtio-kick dev +virtio-gpu-controlq+)
      ;; Spin waiting for the command to complete.
      (loop
         (when (not (eql last-used (virtio-ring-used-idx vq)))
           (return))))
    ;; Release descriptors
    (virtio-ring-free-descriptor vq cmd-desc)
    (virtio-ring-free-descriptor vq rsp-desc)
    ;; Return response header.
    (values (physical-memref-unsigned-byte-32 (+ req-phys 2048 +virtio-gpu-ctrl-hdr-type+))
            (physical-memref-unsigned-byte-32 (+ req-phys 2048 +virtio-gpu-ctrl-hdr-flags+))
            (physical-memref-unsigned-byte-32 (+ req-phys 2048 +virtio-gpu-ctrl-hdr-fence-id+))
            (physical-memref-unsigned-byte-32 (+ req-phys 2048 +virtio-gpu-ctrl-hdr-ctx-id+)))))

(defun virtio-gpu-get-display-info (gpu)
  (virtio-gpu-configure-command gpu
                                +virtio-gpu-cmd-get-display-info+
                                0
                                0
                                0)
  (let ((resp-type (virtio-gpu-issue-command gpu
                                             0
                                             (* +virtio-gpu-max-scanouts+
                                                +virtio-gpu-display-size+)))
        (base (virtio-gpu-response-address gpu))
        (pmode nil)
        (pmode-width nil)
        (pmode-height nil))
    (when (not (eql resp-type +virtio-gpu-resp-ok-display-info+))
      (debug-print-line "virtio-gpu: Invalid response during get-display-info: " resp-type)
      (return-from virtio-gpu-get-display-info nil))
    (dotimes (i +virtio-gpu-max-scanouts+)
      (let* ((offset (* i +virtio-gpu-display-size+))
             (x (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-x+) 0))
             (y (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-y+) 0))
             (width (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-width+) 0))
             (height (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-height+) 0))
             (enabled (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-enabled+) 0))
             (flags (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-flags+) 0)))
        (debug-print-line "Display " i ": x:" x " y:" y " w: " width " h:" height " en:" enabled " flg:" flags)
        (when (not pmode)
          (setf pmode i
                pmode-width width
                pmode-height height))))
    (values pmode pmode-width pmode-height)))

(defun virtio-gpu-resource-create-2d (gpu resource-id width height format)
  (virtio-gpu-configure-command gpu
                                +virtio-gpu-cmd-resource-create-2d+
                                0
                                0
                                0)
  (setf (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-create-2d-resource-id+) 0) resource-id
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-create-2d-format+) 0) format
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-create-2d-width+) 0) width
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-create-2d-height+) 0) height)
  (let ((resp-type (virtio-gpu-issue-command gpu
                                             +virtio-gpu-resource-create-2d-size+
                                             0)))
    (cond ((eql resp-type +virtio-gpu-resp-ok-nodata+)
           (values t nil))
          (t
           (values nil resp-type)))))

(defun virtio-gpu-attach-backing (gpu resource-id address size)
  (virtio-gpu-configure-command gpu
                                +virtio-gpu-cmd-resource-attach-backing+
                                0
                                0
                                0)
  (setf (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-attach-backing-resource-id+) 0) resource-id
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-attach-backing-nr-entries+) 0) 1
        (sys.int::memref-unsigned-byte-64 (virtio-gpu-command-address gpu (+ +virtio-gpu-resource-attach-backing-entry0+ +virtio-gpu-mem-entry-addr+)) 0) address
        (sys.int::memref-unsigned-byte-64 (virtio-gpu-command-address gpu (+ +virtio-gpu-resource-attach-backing-entry0+ +virtio-gpu-mem-entry-length+)) 0) size)
  (let ((resp-type (virtio-gpu-issue-command gpu
                                             (+ +virtio-gpu-resource-attach-backing-entry0+ +virtio-gpu-mem-entry-size+)
                                             0)))
    (cond ((eql resp-type +virtio-gpu-resp-ok-nodata+)
           (values t nil))
          (t
           (values nil resp-type)))))

(defun virtio-gpu-set-scanout (gpu x y width height scanout-id resource-id)
  (virtio-gpu-configure-command gpu
                                +virtio-gpu-cmd-set-scanout+
                                0
                                0
                                0)
  (setf (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-set-scanout-x+) 0) x
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-set-scanout-y+) 0) y
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-set-scanout-width+) 0) width
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-set-scanout-height+) 0) height
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-set-scanout-scanout-id+) 0) scanout-id
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-set-scanout-resource-id+) 0) resource-id)
  (let ((resp-type (virtio-gpu-issue-command gpu
                                             +virtio-gpu-set-scanout-size+
                                             0)))
    (cond ((eql resp-type +virtio-gpu-resp-ok-nodata+)
           (values t nil))
          (t
           (values nil resp-type)))))

(defun virtio-gpu-transfer-to-host-2d (gpu x y width height offset resource-id)
  (virtio-gpu-configure-command gpu
                                +virtio-gpu-cmd-transfer-to-host-2d+
                                0
                                0
                                0)
  (setf (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-transfer-to-host-2d-x+) 0) x
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-transfer-to-host-2d-y+) 0) y
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-transfer-to-host-2d-width+) 0) width
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-transfer-to-host-2d-height+) 0) height
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-transfer-to-host-2d-offset+) 0) offset
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-transfer-to-host-2d-resource-id+) 0) resource-id)
  (let ((resp-type (virtio-gpu-issue-command gpu
                                             +virtio-gpu-transfer-to-host-2d-size+
                                             0)))
    (cond ((eql resp-type +virtio-gpu-resp-ok-nodata+)
           (values t nil))
          (t
           (values nil resp-type)))))

(defun virtio-gpu-resource-flush (gpu x y width height resource-id)
  (virtio-gpu-configure-command gpu
                                +virtio-gpu-cmd-resource-flush+
                                0
                                0
                                0)
  (setf (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-flush-x+) 0) x
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-flush-y+) 0) y
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-flush-width+) 0) width
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-flush-height+) 0) height
        (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-resource-flush-resource-id+) 0) resource-id)
  (let ((resp-type (virtio-gpu-issue-command gpu +virtio-gpu-resource-flush-size+ 0)))
    (cond ((eql resp-type +virtio-gpu-resp-ok-nodata+)
           (values t nil))
          (t
           (values nil resp-type)))))

(defun virtio-gpu-dirty (gpu x y w h in-unsafe-context-p)
  (declare (ignore in-unsafe-context-p))
  (let ((xy (logior (ash x 32) y))
        (wh (logior (ash w 32) h)))
    (safe-without-interrupts (gpu xy wh)
      ;; Issue commands to update the framebuffer and to flush the scanout.
      (let ((x (ldb (byte 32 32) xy))
            (y (ldb (byte 32 0) xy))
            (w (ldb (byte 32 32) wh))
            (h (ldb (byte 32 0) wh)))
        (virtio-gpu-transfer-to-host-2d gpu
                                        x y w h
                                        (* (+ x (* y (virtio-gpu-width gpu))) 4)
                                        +virtio-gpu-framebuffer-resource-id+)
        (virtio-gpu-resource-flush gpu
                                   x y w h
                                   +virtio-gpu-framebuffer-resource-id+)))))

(defun virtio-gpu-register (device)
  (declare (sys.c::closure-allocation :wired))
  (debug-print-line "Detected virtio GPU device " device)
  (let* ((gpu (make-virtio-gpu :virtio-device device))
         ;; Allocate some memory for the request header & footer.
         (frame (or (allocate-physical-pages 1)
                    (panic "Unable to allocate memory for virtio gpu request")))
         (phys (* frame +4k-page-size+))
         (virt (convert-to-pmap-address phys)))
    (debug-print-line "Virtio-GPU request data at " phys)
    (setf (virtio-gpu-request-phys gpu) phys
          (virtio-gpu-request-virt gpu) virt)
    ;; Set the driver bit in the status field.
    (setf (virtio-device-status device) (logior +virtio-status-acknowledge+
                                                +virtio-status-driver+))
    ;; Allocate virtqueues.
    (when (not (virtio-configure-virtqueues device 2))
      (setf (virtio-device-status device) +virtio-status-failed+)
      (return-from virtio-gpu-register nil))
    ;; Configuration complete, go to OK mode.
    (setf (virtio-device-status device) (logior +virtio-status-acknowledge+
                                                +virtio-status-driver+
                                                +virtio-status-ok+))
    ;; Initialize the GPU & framebuffer.
    (multiple-value-bind (pmode width height)
        (virtio-gpu-get-display-info gpu)
      (when (not pmode)
        (debug-print-line "virtio-gpu: No enabled display found")
        (return-from virtio-gpu-register nil))
      (debug-print-line "virtio-gpu: Using pmode " pmode " " width "x" height)
      (multiple-value-bind (successp error)
          (virtio-gpu-resource-create-2d gpu
                                         +virtio-gpu-framebuffer-resource-id+
                                         width
                                         height
                                         +virtio-gpu-format-b8g8r8a8-unorm+)
        (when (not successp)
          (debug-print-line "virtio-gpu: Unable to create framebuffer resource: " error)
          (return-from virtio-gpu-register nil)))
      (let* ((framebuffer-size (* width height 4))
             (n-framebuffer-pages (ceiling framebuffer-size +4k-page-size+))
             (framebuffer-frames (allocate-physical-pages n-framebuffer-pages)))
        (when (not framebuffer-frames)
          (debug-print-line "virtio-gpu: Unable to allocate framebuffer memory")
          (return-from virtio-gpu-register nil))
        (let ((framebuffer-phys (* framebuffer-frames +4k-page-size+)))
          (debug-print-line "virtio-gpu: Framebuffer at " framebuffer-phys)
          ;; Clear framebuffer.
          (sys.int::%fill-words (convert-to-pmap-address framebuffer-phys) 0 (truncate framebuffer-size 2))
          ;; Attach backing store to framebuffer resource.
          (multiple-value-bind (successp error)
              (virtio-gpu-attach-backing gpu +virtio-gpu-framebuffer-resource-id+ framebuffer-phys framebuffer-size)
            (when (not successp)
              (debug-print-line "virtio-gpu: Unable to attach framebuffer memory: " error)
            (return-from virtio-gpu-register nil)))
          ;; Attach framebuffer resource to scanout.
          (multiple-value-bind (successp error)
              (virtio-gpu-set-scanout gpu 0 0 width height pmode +virtio-gpu-framebuffer-resource-id+)
            (when (not successp)
              (debug-print-line "virtio-gpu: Unable to set scanout: " error)
            (return-from virtio-gpu-register nil)))
          (setf (virtio-gpu-scanout gpu) pmode
                (virtio-gpu-width gpu) width
                (virtio-gpu-height gpu) height)
          (video-set-framebuffer framebuffer-phys width height
                                 (* width 4) :x8r8g8b8
                                 (lambda (x y w h in-unsafe-context-p)
                                   (virtio-gpu-dirty gpu x y w h in-unsafe-context-p))))))
    t))
