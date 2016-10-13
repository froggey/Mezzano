;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant +virtio-input-cfg-unset+     #x00)
(defconstant +virtio-input-cfg-id-name+   #x01)
(defconstant +virtio-input-cfg-id-serial+ #x02)
(defconstant +virtio-input-cfg-id-devids+ #x03)
(defconstant +virtio-input-cfg-prop-bits+ #x10)
(defconstant +virtio-input-cfg-ev-bits+   #x11)
(defconstant +virtio-input-cfg-abs-info+  #x12)

(defconstant +virtio-input-absinfo-min+   0)
(defconstant +virtio-input-absinfo-max+   4)
(defconstant +virtio-input-absinfo-fuzz+ 12)
(defconstant +virtio-input-absinfo-flat+ 16)
(defconstant +virtio-input-absinfo-res+  20)

(defconstant +virtio-input-devids-bustype+ 0)
(defconstant +virtio-input-devids-vendor+  2)
(defconstant +virtio-input-devids-product+ 4)
(defconstant +virtio-input-devids-version+ 6)

(defconstant +virtio-input-config-select+ 0)
(defconstant +virtio-input-config-subsel+ 1)
(defconstant +virtio-input-config-size+   2)
(defconstant +virtio-input-config-data+   8)

(defconstant +virtio-input-event-type+  0)
(defconstant +virtio-input-event-code+  2)
(defconstant +virtio-input-event-value+ 4)
(defconstant +virtio-input-event-total-size+ 8)

(sys.int::defglobal *virtio-input-devices* '())

(defstruct (virtio-input
             (:area :wired))
  virtio-device
  irq-handler-function
  event-phys
  event-virt
  (debug-dump-state 0)
  (fifo (make-irq-fifo 50 :name "Virtio-Input fifo")))

(defun virtio-input-event-processing (input)
  (let* ((dev (virtio-input-virtio-device input))
         (vq (virtio-virtqueue dev 0)))
    (loop
       (let ((desc (virtio-pop-used-ring vq)))
         (when (not desc)
           (return))
         (let* ((phys-addr (virtio-ring-desc-address vq desc))
                (type (physical-memref-unsigned-byte-16 (+ phys-addr +virtio-input-event-type+)))
                (code (physical-memref-unsigned-byte-16 (+ phys-addr +virtio-input-event-code+)))
                (value (physical-memref-unsigned-byte-32 (+ phys-addr +virtio-input-event-value+))))
           ;; Check for magic key press.
           (when (and (eql type #x01)
                      (eql value 1))
             (case (virtio-input-debug-dump-state input)
               ;; Start. Expect left-meta.
               (0 (cond ((eql code #x38) ; left-meta
                         (setf (virtio-input-debug-dump-state input) 1))))
               ;; Saw left-meta press
               (1 (cond ((eql code #x38) ; left-meta
                         ;; Stay in this state.
                         )
                        ((eql code #x57) ; F11
                         (debug-dump-threads)
                         (setf (virtio-input-debug-dump-state input) 0))
                        (t
                         (setf (virtio-input-debug-dump-state input) 0))))
               (t
                (setf (virtio-input-debug-dump-state input) 0))))
           ;; Stuff the event into the fifo.
           (let ((packed-value (logior (ash type 48)
                                       (ash code 32)
                                       value)))
             (irq-fifo-push packed-value (virtio-input-fifo input))))
         ;; Re-add the descriptor to the avail ring.
         (virtio-ring-add-to-avail-ring vq desc)))
    (virtio-kick dev 0)))

(defun virtio-input-status-processing (input)
  (let* ((dev (virtio-input-virtio-device input))
         (vq (virtio-virtqueue dev 1)))
    (loop
       (let ((desc (virtio-pop-used-ring vq)))
         (when (not desc)
           (return))
         (virtio-ring-free-descriptor vq desc)))))

(defun virtio-input-irq-handler (input)
  (virtio-input-event-processing input)
  (virtio-input-status-processing input))

(defun virtio-input-register (device)
  ;; Wired allocation required for the IRQ handler closure.
  (declare (sys.c::closure-allocation :wired))
  (debug-print-line "Detected virtio input device " device)
  (let* ((input (make-virtio-input :virtio-device device))
         (irq-handler (lambda (interrupt-frame irq)
                        (declare (ignore interrupt-frame irq))
                        (virtio-input-irq-handler input)))
         ;; Allocate memory for the event receive buffer.
         (frame (or (allocate-physical-pages 1)
                    (panic "Unable to allocate memory for virtio input buffer.")))
         (phys (* frame +4k-page-size+))
         (virt (convert-to-pmap-address phys)))
    (setf (virtio-input-event-phys input) phys
          (virtio-input-event-virt input) virt)
    ;; Set the driver bit in the status field.
    (setf (virtio-device-status device) (logior +virtio-status-acknowledge+
                                                +virtio-status-driver+))
    ;; Allocate virtqueues.
    (when (not (virtio-configure-virtqueues device 2))
      (setf (virtio-device-status device) +virtio-status-failed+)
      (panic "Unable to initialize virtqueues."))
    ;; Enable IRQ handler.
    (setf (virtio-input-irq-handler-function input) irq-handler)
    (virtio-attach-irq device irq-handler)
    (setf (virtio-irq-mask device) nil)
    ;; Configuration complete, go to OK mode.
    (setf (virtio-device-status device) (logior +virtio-status-acknowledge+
                                                +virtio-status-driver+
                                                +virtio-status-ok+))
    ;; Submit event buffers.
    (let ((event-vq (virtio-virtqueue device 0)))
      (dotimes (i (truncate +4k-page-size+ +virtio-input-event-total-size+))
        (let ((addr (+ phys (* i +virtio-input-event-total-size+)))
              (desc (virtio-ring-alloc-descriptor event-vq)))
          (when (null desc)
            (return))
          (setf (virtio-ring-desc-address event-vq desc) addr
                (virtio-ring-desc-length event-vq desc) +virtio-input-event-total-size+
                (virtio-ring-desc-flags event-vq desc) (ash 1 +virtio-ring-desc-f-write+))
          (virtio-ring-add-to-avail-ring event-vq desc))))
    (virtio-kick device 0)
    (when (not (boundp '*virtio-input-devices*))
      (setf *virtio-input-devices* '()))
    (push-wired input *virtio-input-devices*)))

(defun read-virtio-input-device (device &optional (wait-p t))
  (let ((packed-value (irq-fifo-pop (virtio-input-fifo device) wait-p)))
    (when packed-value
      (values (ldb (byte 12 48) packed-value) ; type
              (ldb (byte 16 32) packed-value) ; code
              (ldb (byte 32 0) packed-value))))) ; value
