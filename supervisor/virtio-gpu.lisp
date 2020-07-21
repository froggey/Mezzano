;;;; Virtio-gpu driver

(defpackage :mezzano.supervisor.virtio-gpu
  (:use :cl)
  (:local-nicknames (:sup :mezzano.supervisor)
                    (:virtio :mezzano.supervisor.virtio)
                    (:sys.int :mezzano.internals))
  (:export #:+virtio-gpu-resp-err-unspec+
           #:+virtio-gpu-resp-err-out-of-memory+
           #:+virtio-gpu-resp-err-invalid-scanout-id+
           #:+virtio-gpu-resp-err-invalid-resource-id+
           #:+virtio-gpu-resp-err-invalid-context-id+
           #:+virtio-gpu-resp-err-invalid-parameter+

           ;; 2d/display commands
           #:+virtio-gpu-cmd-get-display-info+
           #:+virtio-gpu-cmd-resource-create-2d+
           #:+virtio-gpu-cmd-resource-unref+
           #:+virtio-gpu-cmd-set-scanout+
           #:+virtio-gpu-cmd-resource-flush+
           #:+virtio-gpu-cmd-transfer-to-host-2d+
           #:+virtio-gpu-cmd-resource-attach-backing+
           #:+virtio-gpu-cmd-resource-detach-backing+
           #:+virtio-gpu-cmd-get-capset-info+
           #:+virtio-gpu-cmd-get-capset+

           ;; 3d commands
           #:+virtio-gpu-cmd-ctx-create+
           #:+virtio-gpu-cmd-ctx-destroy+
           #:+virtio-gpu-cmd-ctx-attach-resource+
           #:+virtio-gpu-cmd-ctx-detach-resource+
           #:+virtio-gpu-cmd-resource-create-3d+
           #:+virtio-gpu-cmd-transfer-to-host-3d+
           #:+virtio-gpu-cmd-transfer-from-host-3d+
           #:+virtio-gpu-cmd-submit-3d+

           ;; cursor commands
           #:+virtio-gpu-cmd-update-cursor+
           #:+virtio-gpu-cmd-move-cursor+

           #:+virtio-gpu-flag-fence+
           #:+virtio-gpu-max-scanouts+

           #:+virtio-gpu-ctrl-hdr-type+
           #:+virtio-gpu-ctrl-hdr-flags+
           #:+virtio-gpu-ctrl-hdr-fence-id+
           #:+virtio-gpu-ctrl-hdr-ctx-id+

           #:+virtio-gpu-format-b8g8r8a8-unorm+
           #:+virtio-gpu-format-b8g8r8x8-unorm+
           #:+virtio-gpu-format-a8r8g8b8-unorm+
           #:+virtio-gpu-format-x8r8g8b8-unorm+
           #:+virtio-gpu-format-r8g8b8a8-unorm+
           #:+virtio-gpu-format-x8b8g8r8-unorm+
           #:+virtio-gpu-format-a8b8g8r8-unorm+
           #:+virtio-gpu-format-r8g8b8x8-unorm+

           #:+virtio-gpu-framebuffer-resource-id+
           #:+virtio-gpu-internal-resource-max+

           #:virtio-device
           #:virtio-gpu
           #:virtio-gpu-n-capsets
           #:virtio-gpu-n-scanouts
           #:virtio-gpu-scanout
           #:virtio-gpu-width
           #:virtio-gpu-height
           #:virtio-gpu-framebuffer
           #:virtio-gpu-framebuffer-format
           #:virtio-gpu-virgl-p
           #:virtio-gpu-virgl-data

           #:virtio-gpu-get-display-info
           #:virtio-gpu-resource-create-2d
           #:virtio-gpu-resource-unref
           #:virtio-gpu-resource-attach-backing
           #:virtio-gpu-set-scanout
           #:virtio-gpu-transfer-to-host-2d
           #:virtio-gpu-resource-flush
           #:virtio-gpu-resource-flush
           #:virtio-gpu-get-capset-info
           #:virtio-gpu-get-capset
           #:virtio-gpu-ctx-create
           #:virtio-gpu-ctx-destroy
           #:virtio-gpu-attach-resource
           #:virtio-gpu-detach-resource
           #:virtio-gpu-resource-create-3d
           #:virtio-gpu-transfer-to-host-3d
           #:virtio-gpu-transfer-from-host-3d
           #:virtio-gpu-submit-3d
           ))

(in-package :mezzano.supervisor.virtio-gpu)

(defconstant +virtio-gpu-controlq+ 0)
(defconstant +virtio-gpu-cursorq+  1)

(defconstant +virtio-gpu-f-virgl+ 0 "Feature bit for virgl support.")

;; Offsets in the device-specific space.
(defconstant +virtio-gpu-config-events-read+  0)
(defconstant +virtio-gpu-config-events-clear+ 4)
(defconstant +virtio-gpu-config-num-scanouts+ 8)
(defconstant +virtio-gpu-config-num-capsets+ 12)

;; 2d commands
(defconstant +virtio-gpu-cmd-get-display-info+         #x0100)
(defconstant +virtio-gpu-cmd-resource-create-2d+       #x0101)
(defconstant +virtio-gpu-cmd-resource-unref+           #x0102)
(defconstant +virtio-gpu-cmd-set-scanout+              #x0103)
(defconstant +virtio-gpu-cmd-resource-flush+           #x0104)
(defconstant +virtio-gpu-cmd-transfer-to-host-2d+      #x0105)
(defconstant +virtio-gpu-cmd-resource-attach-backing+  #x0106)
(defconstant +virtio-gpu-cmd-resource-detach-backing+  #x0107)
(defconstant +virtio-gpu-cmd-get-capset-info+          #x0108)
(defconstant +virtio-gpu-cmd-get-capset+               #x0109)

;; 3d commands
(defconstant +virtio-gpu-cmd-ctx-create+               #x0200)
(defconstant +virtio-gpu-cmd-ctx-destroy+              #x0201)
(defconstant +virtio-gpu-cmd-ctx-attach-resource+      #x0202)
(defconstant +virtio-gpu-cmd-ctx-detach-resource+      #x0203)
(defconstant +virtio-gpu-cmd-resource-create-3d+       #x0204)
(defconstant +virtio-gpu-cmd-transfer-to-host-3d+      #x0205)
(defconstant +virtio-gpu-cmd-transfer-from-host-3d+    #x0206)
(defconstant +virtio-gpu-cmd-submit-3d+                #x0207)

;; cursor commands
(defconstant +virtio-gpu-cmd-update-cursor+            #x0300)
(defconstant +virtio-gpu-cmd-move-cursor+              #x0301)

;; success responses
(defconstant +virtio-gpu-resp-ok-nodata+               #x1100)
(defconstant +virtio-gpu-resp-ok-display-info+         #x1101)
(defconstant +virtio-gpu-resp-ok-capset-info+          #x1102)
(defconstant +virtio-gpu-resp-ok-capset+               #x1103)

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

(defconstant +virtio-gpu-get-capset-info-capset-index+ 0)
(defconstant +virtio-gpu-get-capset-info-size+ 8)

(defconstant +virtio-gpu-resp-get-capset-info-capset-id+ 0)
(defconstant +virtio-gpu-resp-get-capset-info-capset-max-version+ 4)
(defconstant +virtio-gpu-resp-get-capset-info-capset-max-size+ 8)
(defconstant +virtio-gpu-resp-get-capset-info-size+ 16)

(defconstant +virtio-gpu-get-capset-capset-id+ 0)
(defconstant +virtio-gpu-get-capset-capset-version+ 4)
(defconstant +virtio-gpu-get-capset-size+ 8)

(defconstant +virtio-gpu-format-b8g8r8a8-unorm+ 1)
(defconstant +virtio-gpu-format-b8g8r8x8-unorm+ 2)
(defconstant +virtio-gpu-format-a8r8g8b8-unorm+ 3)
(defconstant +virtio-gpu-format-x8r8g8b8-unorm+ 4)
(defconstant +virtio-gpu-format-r8g8b8a8-unorm+ 67)
(defconstant +virtio-gpu-format-x8b8g8r8-unorm+ 68)
(defconstant +virtio-gpu-format-a8b8g8r8-unorm+ 121)
(defconstant +virtio-gpu-format-r8g8b8x8-unorm+ 134)

(defconstant +virtio-gpu-update-cursor-scanout-id+ 0)
(defconstant +virtio-gpu-update-cursor-x+ 4)
(defconstant +virtio-gpu-update-cursor-y+ 8)
(defconstant +virtio-gpu-update-cursor-resource-id+ 16)
(defconstant +virtio-gpu-update-cursor-hot-x+ 20)
(defconstant +virtio-gpu-update-cursor-hot-y+ 24)

(defconstant +virtio-gpu-update-cursor-size+ 32)

(defconstant +virtio-gpu-framebuffer-resource-id+ 123)
(defconstant +virtio-gpu-internal-resource-max+ 128
  "Resource IDs below this are used internally by the GPU driver and
must not be allocated by virgl.")

(defstruct (virtio-gpu
             (:area :wired))
  virtio-device
  request-phys
  request-virt
  command-lock
  scanout
  width
  height
  framebuffer
  virgl-p
  virgl-data)

(defun virtio-gpu-framebuffer-format (gpu)
  (declare (ignore gpu))
  :b8g8r8a8-unorm)

(defun virtio-gpu-n-scanouts (gpu)
  (virtio:virtio-device-specific-header/32
   (virtio-gpu-virtio-device gpu) +virtio-gpu-config-num-scanouts+))

(defun virtio-gpu-n-capsets (gpu)
  (virtio:virtio-device-specific-header/32
   (virtio-gpu-virtio-device gpu) +virtio-gpu-config-num-capsets+))

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
          (sys.int::memref-unsigned-byte-64 (+ addr +virtio-gpu-ctrl-hdr-fence-id+) 0) fence-id
          (sys.int::memref-unsigned-byte-32 (+ addr +virtio-gpu-ctrl-hdr-ctx-id+) 0) ctx-id)))

(defun virtio-gpu-issue-command (gpu command-length response-length)
  (let* ((req-phys (virtio-gpu-request-phys gpu))
         (dev (virtio-gpu-virtio-device gpu))
         (vq (virtio:virtio-virtqueue dev +virtio-gpu-controlq+))
         (cmd-desc (virtio:virtio-ring-alloc-descriptor vq))
         (rsp-desc (virtio:virtio-ring-alloc-descriptor vq)))
    (sup:ensure cmd-desc)
    (sup:ensure rsp-desc)
    #+(or)
    (sup:debug-print-line "dev:" dev " clen:" command-length " rlen:" response-length " cmdd:" cmd-desc " rspd:" rsp-desc)
    ;; Command descriptor.
    (setf (virtio:virtio-ring-desc-address vq cmd-desc) req-phys
          (virtio:virtio-ring-desc-length vq cmd-desc) (+ +virtio-gpu-ctrl-hdr-size+ command-length)
          (virtio:virtio-ring-desc-flags vq cmd-desc) (ash 1 virtio:+virtio-ring-desc-f-next+)
          (virtio:virtio-ring-desc-next vq cmd-desc) rsp-desc)
    ;; Response descriptor.
    (setf (virtio:virtio-ring-desc-address vq rsp-desc) (+ req-phys 2048)
          (virtio:virtio-ring-desc-length vq rsp-desc) (+ +virtio-gpu-ctrl-hdr-size+ response-length)
          (virtio:virtio-ring-desc-flags vq rsp-desc) (ash 1 virtio:+virtio-ring-desc-f-write+)
          (virtio:virtio-ring-desc-next vq rsp-desc) 0)
    ;; Issue command & await completion.
    (let ((last-used (virtio:virtio-ring-used-idx vq)))
      (virtio:virtio-ring-add-to-avail-ring vq cmd-desc)
      (virtio:virtio-kick dev +virtio-gpu-controlq+)
      ;; Spin waiting for the command to complete.
      (loop
         (when (not (eql last-used (virtio:virtio-ring-used-idx vq)))
           (return))))
    ;; Release descriptors
    (virtio:virtio-ring-free-descriptor vq cmd-desc)
    (virtio:virtio-ring-free-descriptor vq rsp-desc)
    ;; Return response header.
    (values (sup::physical-memref-unsigned-byte-32 (+ req-phys 2048 +virtio-gpu-ctrl-hdr-type+))
            (sup::physical-memref-unsigned-byte-32 (+ req-phys 2048 +virtio-gpu-ctrl-hdr-flags+))
            (sup::physical-memref-unsigned-byte-32 (+ req-phys 2048 +virtio-gpu-ctrl-hdr-fence-id+))
            (sup::physical-memref-unsigned-byte-32 (+ req-phys 2048 +virtio-gpu-ctrl-hdr-ctx-id+)))))

(defmacro define-virtio-gpu-command (name &key command command-fields command-size)
  `(defun ,name (gpu ,@(mapcar #'first command-fields) &key (context 0) (fence 0) (flags 0))
     (sup:with-mutex ((virtio-gpu-command-lock gpu))
       (virtio-gpu-configure-command gpu ,command flags fence context)
       ,@(loop
            for (name offset accessor) in command-fields
            collect `(setf (,(ecase accessor
                               (:ub8 'sys.int::memref-unsigned-byte-8)
                               (:ub16/le 'sys.int::memref-unsigned-byte-16)
                               (:ub32/le 'sys.int::memref-unsigned-byte-32)
                               (:ub64/le 'sys.int::memref-unsigned-byte-64))
                             (virtio-gpu-command-address gpu ,offset))
                           ,name))
       (let ((resp-type (virtio-gpu-issue-command gpu ,command-size 0)))
         (cond ((eql resp-type +virtio-gpu-resp-ok-nodata+)
                (values t nil))
               (t
                (values nil resp-type)))))))

;;; 2d commands

(defun virtio-gpu-get-display-info (gpu)
  (sup:with-mutex ((virtio-gpu-command-lock gpu))
    (virtio-gpu-configure-command gpu +virtio-gpu-cmd-get-display-info+ 0 0 0)
    (let ((resp-type (virtio-gpu-issue-command gpu 0 (* +virtio-gpu-max-scanouts+ +virtio-gpu-display-size+)))
          (base (virtio-gpu-response-address gpu))
          (pmode nil)
          (pmode-width nil)
          (pmode-height nil))
      (when (not (eql resp-type +virtio-gpu-resp-ok-display-info+))
        (sup:debug-print-line "virtio-gpu: Invalid response during get-display-info: " resp-type)
        (return-from virtio-gpu-get-display-info nil))
      (dotimes (i +virtio-gpu-max-scanouts+)
        (let* ((offset (* i +virtio-gpu-display-size+))
               (x (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-x+) 0))
               (y (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-y+) 0))
               (width (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-width+) 0))
               (height (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-height+) 0))
               (enabled (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-enabled+) 0))
               (flags (sys.int::memref-unsigned-byte-32 (+ base offset +virtio-gpu-display-flags+) 0)))
          (sup:debug-print-line "Display " i ": x:" x " y:" y " w: " width " h:" height " en:" enabled " flg:" flags)
          (when (not pmode)
            (setf pmode i
                  pmode-width width
                  pmode-height height))))
      (values pmode pmode-width pmode-height))))

(define-virtio-gpu-command virtio-gpu-resource-create-2d
    :command +virtio-gpu-cmd-resource-create-2d+
    :command-fields ((resource-id 0 :ub32/le)
                     (format 4 :ub32/le)
                     (width 8 :ub32/le)
                     (height 12 :ub32/le))
    :command-size 16)

(define-virtio-gpu-command virtio-gpu-resource-unref
    :command +virtio-gpu-cmd-resource-unref+
    :command-fields ((resource-id 0 :ub32/le))
    :command-size 8)

(define-virtio-gpu-command virtio-gpu-resource-attach-backing
    :command +virtio-gpu-cmd-resource-attach-backing+
    :command-fields ((resource-id 0 :ub32/le)
                     (nr-entries 4 :ub32/le) ; must be 1, this is actually an array of entries.
                     (entry0-addr 8 :ub64/le)
                     (entry0-length 16 :ub64/le))
    :command-size 24)

(define-virtio-gpu-command virtio-gpu-set-scanout
    :command +virtio-gpu-cmd-set-scanout+
    :command-fields ((x 0 :ub32/le)
                     (y 4 :ub32/le)
                     (width 8 :ub32/le)
                     (height 12 :ub32/le)
                     (scanout-id 16 :ub32/le)
                     (resource-id 20 :ub32/le))
    :command-size 24)

(define-virtio-gpu-command virtio-gpu-transfer-to-host-2d
    :command +virtio-gpu-cmd-transfer-to-host-2d+
    :command-fields ((x 0 :ub32/le)
                     (y 4 :ub32/le)
                     (width 8 :ub32/le)
                     (height 12 :ub32/le)
                     (offset 16 :ub64/le)
                     (resource-id 24 :ub32/le))
    :command-size 32)

(define-virtio-gpu-command virtio-gpu-resource-flush
    :command +virtio-gpu-cmd-resource-flush+
    :command-fields ((x 0 :ub32/le)
                     (y 4 :ub32/le)
                     (width 8 :ub32/le)
                     (height 12 :ub32/le)
                     (resource-id 16 :ub32/le))
    :command-size 24)

(define-virtio-gpu-command virtio-gpu-resource-flush
    :command +virtio-gpu-cmd-resource-flush+
    :command-fields ((x 0 :ub32/le)
                     (y 4 :ub32/le)
                     (width 8 :ub32/le)
                     (height 12 :ub32/le)
                     (resource-id 16 :ub32/le))
    :command-size 24)

(defun virtio-gpu-get-capset-info (gpu capset-index)
  (sup:with-mutex ((virtio-gpu-command-lock gpu))
    (virtio-gpu-configure-command gpu +virtio-gpu-cmd-get-capset-info+ 0 0 0)
    (setf (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-get-capset-info-capset-index+)) capset-index)
    (let ((resp-type (virtio-gpu-issue-command gpu +virtio-gpu-get-capset-info-size+ +virtio-gpu-resp-get-capset-info-size+)))
      (when (not (eql resp-type +virtio-gpu-resp-ok-capset-info+))
        (sup:debug-print-line "virtio-gpu: Invalid response during get-capset-info: " resp-type)
        (return-from virtio-gpu-get-capset-info nil))
      (values (sys.int::memref-unsigned-byte-32 (virtio-gpu-response-address gpu +virtio-gpu-resp-get-capset-info-capset-id+))
              (sys.int::memref-unsigned-byte-32 (virtio-gpu-response-address gpu +virtio-gpu-resp-get-capset-info-capset-max-version+))
              (sys.int::memref-unsigned-byte-32 (virtio-gpu-response-address gpu +virtio-gpu-resp-get-capset-info-capset-max-size+))))))

;; CAPSET-DATA must be a correctly-sized array for the result.
(defun virtio-gpu-get-capset (gpu capset-id capset-version capset-data)
  (assert (< (length capset-data) 2000)) ; limited by size of response buffer.
  (sup:with-mutex ((virtio-gpu-command-lock gpu))
    (virtio-gpu-configure-command gpu +virtio-gpu-cmd-get-capset+ 0 0 0)
    (setf (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-get-capset-capset-id+)) capset-id)
    (setf (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu +virtio-gpu-get-capset-capset-version+)) capset-version)
    (let ((resp-type (virtio-gpu-issue-command gpu +virtio-gpu-get-capset-size+ (length capset-data))))
      (when (not (eql resp-type +virtio-gpu-resp-ok-capset+))
        (sup:debug-print-line "virtio-gpu: Invalid response during get-capset: " resp-type)
        (return-from virtio-gpu-get-capset nil))
      (dotimes (i (length capset-data))
        (setf (aref capset-data i) (sys.int::memref-unsigned-byte-8 (virtio-gpu-response-address gpu i))))
      capset-data)))

;;; 3d commands

;; Cares about context
(defun virtio-gpu-ctx-create (gpu debug-name &key (context 0) (fence 0) (flags 0))
  (let* ((debug-name-bytes (mezzano.internals::encode-utf-8-string debug-name))
         (nlen (min 64 (length debug-name-bytes))))
    (mezzano.supervisor:with-mutex ((virtio-gpu-command-lock gpu))
      (virtio-gpu-configure-command gpu
                                    +virtio-gpu-cmd-ctx-create+
                                    flags
                                    fence
                                    context)
      (setf (mezzano.internals::memref-unsigned-byte-32
             (virtio-gpu-command-address gpu 0))
            nlen)
      (dotimes (i nlen)
        (setf (mezzano.internals::memref-unsigned-byte-8
               (virtio-gpu-command-address gpu (+ 8 i)))
              (aref debug-name-bytes i)))
      (let ((resp-type (virtio-gpu-issue-command gpu 72 0)))
        (cond ((eql resp-type +virtio-gpu-resp-ok-nodata+)
               (values t nil))
              (t
               (values nil resp-type)))))))

;; Cares about context
(define-virtio-gpu-command virtio-gpu-ctx-destroy
    :command +virtio-gpu-cmd-ctx-destroy+
    :command-fields ()
    :command-size 0)

;; Cares about context
(define-virtio-gpu-command virtio-gpu-attach-resource
    :command +virtio-gpu-cmd-ctx-attach-resource+
    :command-fields ((resource-id 0 :ub32/le))
    :command-size 8)

;; Cares about context
(define-virtio-gpu-command virtio-gpu-detach-resource
    :command +virtio-gpu-cmd-ctx-detach-resource+
    :command-fields ((resource-id 0 :ub32/le))
    :command-size 8)

(define-virtio-gpu-command virtio-gpu-resource-create-3d
    :command +virtio-gpu-cmd-resource-create-3d+
    :command-fields ((resource-id 0 :ub32/le)
                     (target 4 :ub32/le)
                     (format 8 :ub32/le)
                     (bind 12 :ub32/le)
                     (width 16 :ub32/le)
                     (height 20 :ub32/le)
                     (depth 24 :ub32/le)
                     (array-size 28 :ub32/le)
                     (last-level 32 :ub32/le)
                     (nr-samples 36 :ub32/le)
                     (cmd-flags 40 :ub32/le))
    :command-size 48)

;; Cares about context
(define-virtio-gpu-command virtio-gpu-transfer-to-host-3d
    :command +virtio-gpu-cmd-transfer-to-host-3d+
    :command-fields ((x 0 :ub32/le)
                     (y 4 :ub32/le)
                     (z 8 :ub32/le)
                     (w 12 :ub32/le)
                     (h 16 :ub32/le)
                     (d 20 :ub32/le)
                     (offset 24 :ub64/le)
                     (resource-id 32 :ub32/le)
                     (level 36 :ub32/le)
                     (stride 40 :ub32/le)
                     (layer-size 44 :ub32/le))
    :command-size 48)

;; Cares about context
(define-virtio-gpu-command virtio-gpu-transfer-from-host-3d
    :command +virtio-gpu-cmd-transfer-from-host-3d+
    :command-fields ((x 0 :ub32/le)
                     (y 4 :ub32/le)
                     (z 8 :ub32/le)
                     (w 12 :ub32/le)
                     (h 16 :ub32/le)
                     (d 20 :ub32/le)
                     (offset 24 :ub64/le)
                     (resource-id 32 :ub32/le)
                     (level 36 :ub32/le)
                     (stride 40 :ub32/le)
                     (layer-size 44 :ub32/le))
    :command-size 48)

;; Cares about context
(defun virtio-gpu-submit-3d (gpu command-data &key (context 0) (fence 0) (flags 0))
  (check-type command-data (array (unsigned-byte 8) (*)))
  (assert (< (length command-data) 2000)) ; limited by size of command buffer.
  (sup:with-mutex ((virtio-gpu-command-lock gpu))
    (virtio-gpu-configure-command gpu +virtio-gpu-cmd-submit-3d+ flags fence context)
    (setf (sys.int::memref-unsigned-byte-32 (virtio-gpu-command-address gpu 0)) (length command-data))
    (loop
       for i from 8
       for elt across command-data
       do (setf (sys.int::memref-unsigned-byte-8 (virtio-gpu-command-address gpu i)) elt))
    (let ((resp-type (virtio-gpu-issue-command gpu (+ 8 (length command-data)) 0)))
      (when (not (eql resp-type +virtio-gpu-resp-ok-nodata+))
        (sup:debug-print-line "virtio-gpu: Invalid response during submit-3d: " resp-type)
        (return-from virtio-gpu-submit-3d (values nil resp-type)))
      t)))

(defun virtio-gpu-dirty (gpu x y w h in-unsafe-context-p)
  (when (not in-unsafe-context-p)
    ;; Don't issue transfer-to-host-2d if the width or height is zero,
    ;; this causes qemu to assert.
    (when (not (or (zerop w) (zerop h)))
      ;; Issue commands to update the framebuffer and to flush the scanout.
      (virtio-gpu-transfer-to-host-2d gpu
                                      x y w h
                                      (* (+ x (* y (virtio-gpu-width gpu))) 4)
                                      +virtio-gpu-framebuffer-resource-id+)
      (virtio-gpu-resource-flush gpu
                                 x y w h
                                 +virtio-gpu-framebuffer-resource-id+))))

;; In the virtio package for backwards compatiblity reasons.
(defun virtio::virtio-gpu-register (device)
  (declare (mezzano.compiler::closure-allocation :wired))
  (sup:debug-print-line "Detected virtio GPU device " device)
  (let ((gpu (make-virtio-gpu :virtio-device device
                              :command-lock (sup:make-mutex "Virtio GPU command lock"))))
    ;; Allocate some memory for the request header & footer.
    (let* ((frame (or (sup::allocate-physical-pages 1)
                      (panic "Unable to allocate memory for virtio gpu request")))
           (phys (* frame sup::+4k-page-size+))
           (virt (sup::convert-to-pmap-address phys)))
      (sup:debug-print-line "Virtio-GPU request data at " phys)
      (setf (virtio-gpu-request-phys gpu) phys
            (virtio-gpu-request-virt gpu) virt))
    ;; Set the driver bit in the status field.
    (setf (virtio:virtio-device-status device) (logior virtio:+virtio-status-acknowledge+
                                                       virtio:+virtio-status-driver+))
    (sup:debug-print-line "virtio-gpu: " (virtio:virtio-device-specific-header/32 device +virtio-gpu-config-num-scanouts+) " scanouts")
    (sup:debug-print-line "virtio-gpu: " (virtio:virtio-device-specific-header/32 device +virtio-gpu-config-num-capsets+) " capsets")
    ;; Enable virgl, if present.
    (when (virtio:virtio-device-feature device +virtio-gpu-f-virgl+)
      (sup:debug-print-line "virtio-gpu: virgl enabled")
      (setf (virtio:virtio-driver-feature device +virtio-gpu-f-virgl+) t)
      (setf (virtio-gpu-virgl-p gpu) t))
    ;; Allocate virtqueues.
    (when (not (virtio:virtio-configure-virtqueues device 2))
      (setf (virtio:virtio-device-status device) virtio:+virtio-status-failed+)
      (return-from virtio::virtio-gpu-register nil))
    ;; Configuration complete, go to OK mode.
    (setf (virtio:virtio-device-status device) (logior virtio:+virtio-status-acknowledge+
                                                       virtio:+virtio-status-driver+
                                                       virtio:+virtio-status-ok+))
    ;; Initialize the GPU & framebuffer.
    (multiple-value-bind (pmode width height)
        (virtio-gpu-get-display-info gpu)
      (when (not pmode)
        (sup:debug-print-line "virtio-gpu: No enabled display found")
        (return-from virtio::virtio-gpu-register nil))
      (sup:debug-print-line "virtio-gpu: Using pmode " pmode " " width "x" height)
      (multiple-value-bind (successp error)
          (virtio-gpu-resource-create-2d gpu
                                         +virtio-gpu-framebuffer-resource-id+
                                         +virtio-gpu-format-b8g8r8a8-unorm+
                                         width
                                         height)
        (when (not successp)
          (sup:debug-print-line "virtio-gpu: Unable to create framebuffer resource: " error)
          (return-from virtio::virtio-gpu-register nil)))
      ;; TODO: Support discontigious framebuffer.
      (let* ((framebuffer-size (* width height 4))
             (framebuffer-dma-buffer (sup:make-dma-buffer framebuffer-size
                                                          :name gpu
                                                          :contiguous t
                                                          :errorp nil)))
        (when (not framebuffer-dma-buffer)
          (sup:debug-print-line "virtio-gpu: Unable to allocate framebuffer memory")
          (return-from virtio::virtio-gpu-register nil))
        (let ((framebuffer-phys (sup:dma-buffer-physical-address framebuffer-dma-buffer)))
          (sup:debug-print-line "virtio-gpu: Framebuffer at " framebuffer-phys)
          ;; Clear framebuffer.
          (sys.int::%fill-words (sup:dma-buffer-virtual-address framebuffer-dma-buffer)
                                0
                                (truncate framebuffer-size 8))
          ;; Attach backing store to framebuffer resource.
          (multiple-value-bind (successp error)
              (virtio-gpu-resource-attach-backing gpu +virtio-gpu-framebuffer-resource-id+ 1 framebuffer-phys framebuffer-size)
            (when (not successp)
              (sup:debug-print-line "virtio-gpu: Unable to attach framebuffer memory: " error)
            (return-from virtio::virtio-gpu-register nil)))
          ;; Attach framebuffer resource to scanout.
          (multiple-value-bind (successp error)
              (virtio-gpu-set-scanout gpu 0 0 width height pmode +virtio-gpu-framebuffer-resource-id+)
            (when (not successp)
              (sup:debug-print-line "virtio-gpu: Unable to set scanout: " error)
              (return-from virtio::virtio-gpu-register nil)))
          (setf (virtio-gpu-scanout gpu) pmode
                (virtio-gpu-width gpu) width
                (virtio-gpu-height gpu) height
                (virtio-gpu-framebuffer gpu) framebuffer-dma-buffer)
          (sup::video-set-framebuffer framebuffer-phys width height
                                      (* width 4) :x8r8g8b8
                                      :damage-fn
                                      (lambda (x y w h in-unsafe-context-p)
                                        (virtio-gpu-dirty gpu x y w h in-unsafe-context-p))
                                      :device gpu))))
    t))
