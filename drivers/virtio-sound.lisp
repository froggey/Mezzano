;;;; Virtio-sound sound card driver.

;;; "Virtual I/O Device (VIRTIO) Version 1.2" section 5.14.

(defpackage :mezzano.driver.virtio-sound
  (:use :cl)
  (:local-nicknames (:virtio :mezzano.supervisor.virtio)
                    (:sup :mezzano.supervisor)
                    (:sync :mezzano.sync)
                    (:int :mezzano.internals)
                    (:sound :mezzano.driver.sound))
  (:export #:virtio-sound))

(in-package :mezzano.driver.virtio-sound)

(defconstant +virtio-dev-id-sound+ #x19)

;;; Virtqueue indices.
(defconstant +controlq+ 0)
(defconstant +eventq+ 1)
(defconstant +txq+ 2)
(defconstant +rxq+ 3)

;;; Control message types.
(defconstant +jack-info+ 1)
(defconstant +jack-remap+ 2)
(defconstant +r-pcm-info+ #x100)
(defconstant +r-pcm-set-params+ #x101)
(defconstant +r-pcm-prepare+ #x102)
(defconstant +r-pcm-release+ #x103)
(defconstant +r-pcm-start+ #x104)
(defconstant +r-pcm-stop+ #x105)
(defconstant +r-chmap-info+ #x200)

;;; Status codes.
(defconstant +s-ok+ #x8000)
(defconstant +s-bad-msg+ #x8001)
(defconstant +s-not-supp+ #x8002)
(defconstant +s-io-err+ #x8003)

;;; Event types.
(defconstant +evt-pcm-period-elapsed+ #x1100)
(defconstant +evt-pcm-xrun+ #x1101)

;;; Stream directions.
(defconstant +d-output+ 0)
(defconstant +d-input+ 1)

;;; PCM format & rate indices (bit positions in the info bitmaps).
(defconstant +pcm-fmt-s16+ 5)
(defconstant +pcm-rate-44100+ 6)

;;; Sizes of the structs used by this driver.
(defconstant +event-size+ 8)
(defconstant +pcm-info-size+ 32)
(defconstant +pcm-xfer-size+ 4)
(defconstant +pcm-status-size+ 8)

;;; Offsets within struct virtio_snd_pcm_info.
(defconstant +pcm-info-features-offset+ 4)
(defconstant +pcm-info-formats-offset+ 8)
(defconstant +pcm-info-rates-offset+ 16)
(defconstant +pcm-info-direction-offset+ 24)
(defconstant +pcm-info-channels-min-offset+ 25)
(defconstant +pcm-info-channels-max-offset+ 26)

;;; Offsets within struct virtio_snd_pcm_set_params.
(defconstant +pcm-set-params-stream-id-offset+ 4)
(defconstant +pcm-set-params-buffer-bytes-offset+ 8)
(defconstant +pcm-set-params-period-bytes-offset+ 12)
(defconstant +pcm-set-params-features-offset+ 16)
(defconstant +pcm-set-params-channels-offset+ 20)
(defconstant +pcm-set-params-format-offset+ 21)
(defconstant +pcm-set-params-rate-offset+ 22)
(defconstant +pcm-set-params-padding-offset+ 23)

;;; Device configuration space offsets.
(defconstant +config-streams-offset+ 4)

;;; Driver tunables.
(defconstant +n-periods+ 4
  "Number of period buffers in the TX ring.")
(defconstant +period-bytes+ 2048
  "Size of one period in bytes. Must divide buffer size evenly.")
(defconstant +buffer-bytes+ (* +n-periods+ +period-bytes+))
(defconstant +period-stride+ (+ +pcm-xfer-size+
                                +period-bytes+
                                +pcm-status-size+)
  "Bytes of DMA per period: header + data + status.")
(defconstant +idle-periods+ 10
  "Number of silent periods after the sink drains before stopping the stream.")

;;; The control response area sits one page beyond the request area.
(defconstant +control-response-offset+ 4096)

(defclass virtio-sound (sound:sound-card)
  ((virtio-device :initarg :virtio-device :accessor virtio-sound-virtio-device)
   (irq-handler :initform nil :accessor virtio-sound-irq-handler)
   (stream-id :initform nil :accessor virtio-sound-stream-id)
   ;; Control command request/response DMA areas.
   (control-phys :initform nil :accessor virtio-sound-control-phys)
   (control-virt :initform nil :accessor virtio-sound-control-virt)
   ;; Event queue DMA area.
   (event-phys :initform nil :accessor virtio-sound-event-phys)
   (event-virt :initform nil :accessor virtio-sound-event-virt)
   ;; TX period buffers and their descriptor ids.
   (tx-phys :initform nil :accessor virtio-sound-tx-phys)
   (tx-virt :initform nil :accessor virtio-sound-tx-virt)
   (period-desc-ids :initform nil :accessor virtio-sound-period-desc-ids)
   (n-periods :initarg :n-periods :initform +n-periods+ :accessor virtio-sound-n-periods)
   (period-bytes :initarg :period-bytes :initform +period-bytes+ :accessor virtio-sound-period-bytes)))

(defun virtio-sound-boot-id (card)
  (virtio:virtio-device-boot-id (virtio-sound-virtio-device card)))

(define-condition device-disconnect () ())

(defmacro with-virtio-sound-access ((card) &body body)
  `(sup:with-device-access ((virtio-sound-boot-id ,card)
                            (signal 'device-disconnect))
     ,@body))

(defun issue-command (card code request-size response-size)
  "Send a control command on the control queue and wait for the response.
The request payload (excluding the code field) must already be placed in
the control request area. Returns the response status code."
  (let* ((dev (virtio-sound-virtio-device card))
         (vq (virtio:virtio-virtqueue dev +controlq+))
         (req-phys (virtio-sound-control-phys card))
         (req-desc (virtio:virtio-ring-alloc-descriptor vq))
         (resp-desc (virtio:virtio-ring-alloc-descriptor vq)))
    (assert req-desc (card vq) "Unable to allocate descriptor")
    (assert resp-desc (card vq) "Unable to allocate descriptor")
    (setf (int::memref-unsigned-byte-32 (virtio-sound-control-virt card)) code
          (virtio:virtio-ring-desc-address vq req-desc) req-phys
          (virtio:virtio-ring-desc-length vq req-desc) request-size
          (virtio:virtio-ring-desc-flags vq req-desc) (ash 1 virtio:+virtio-ring-desc-f-next+)
          (virtio:virtio-ring-desc-next vq req-desc) resp-desc
          (virtio:virtio-ring-desc-address vq resp-desc) (+ req-phys +control-response-offset+)
          (virtio:virtio-ring-desc-length vq resp-desc) response-size
          (virtio:virtio-ring-desc-flags vq resp-desc) (ash 1 virtio:+virtio-ring-desc-f-write+)
          (virtio:virtio-ring-desc-next vq resp-desc) 0)
    (int::dma-write-barrier)
    (let ((last-used (virtio:virtio-ring-used-idx vq)))
      (virtio:virtio-ring-add-to-avail-ring vq req-desc)
      (virtio:virtio-kick dev +controlq+)
      ;; Spin for the response.
      ;; TODO: timeouts and maybe wait for an interrupt instead.
      (loop
        (when (not (eql last-used (virtio:virtio-ring-used-idx vq)))
          (return))))
    (int::dma-write-barrier)
    ;(int::hexdump (virtio-sound-control-virt card) (ceiling (* 2 +control-response-offset+) sup::+4k-page-size+) :memory :virtual)
    (virtio:virtio-ring-free-descriptor vq req-desc)
    (virtio:virtio-ring-free-descriptor vq resp-desc)
    (int::memref-unsigned-byte-32 (+ (virtio-sound-control-virt card)
                                     +control-response-offset+))))

(defun pcm-info (card start-id count)
  "Query info for COUNT PCM streams starting at START_ID.
The stream info is left in the control response area; returns the status code."
  (let ((virt (virtio-sound-control-virt card)))
    (setf (int::memref-unsigned-byte-32 virt 1) start-id
          (int::memref-unsigned-byte-32 virt 2) count
          (int::memref-unsigned-byte-32 virt 3) +pcm-info-size+))
  (issue-command card +r-pcm-info+ 16 (+ 4 (* count +pcm-info-size+))))

(defun pcm-command (card code)
  "Send a PCM command addressed to the card's stream."
  (setf (int::memref-unsigned-byte-32 (virtio-sound-control-virt card) 1)
        (virtio-sound-stream-id card))
  (issue-command card code 8 4))

(defun set-params (card)
  (let ((virt (virtio-sound-control-virt card)))
    (setf (int::memref-unsigned-byte-32 (+ virt +pcm-set-params-stream-id-offset+)) (virtio-sound-stream-id card)
          (int::memref-unsigned-byte-32 (+ virt +pcm-set-params-buffer-bytes-offset+)) +buffer-bytes+
          (int::memref-unsigned-byte-32 (+ virt +pcm-set-params-period-bytes-offset+)) +period-bytes+
          ;; No feature bits (MSG transport).
          (int::memref-unsigned-byte-32 (+ virt +pcm-set-params-features-offset+)) 0
          (int::memref-unsigned-byte-8 (+ virt +pcm-set-params-channels-offset+)) 2
          (int::memref-unsigned-byte-8 (+ virt +pcm-set-params-format-offset+)) +pcm-fmt-s16+
          (int::memref-unsigned-byte-8 (+ virt +pcm-set-params-rate-offset+)) +pcm-rate-44100+
          (int::memref-unsigned-byte-8 (+ virt +pcm-set-params-padding-offset+)) 0))
  (issue-command card +r-pcm-set-params+ 24 4))

(defun prepare (card)
  (pcm-command card +r-pcm-prepare+))

(defun start (card)
  (pcm-command card +r-pcm-start+))

(defun stop (card)
  (pcm-command card +r-pcm-stop+))

(defun release (card)
  (pcm-command card +r-pcm-release+))

(defun pcm-info-field (info-address offset)
  (int::memref-unsigned-byte-32 (+ info-address offset)))

(defun find-output-stream (card streams)
  "Find a stream that can do stereo 44.1kHz s16 output.
The response area holds the PCM_INFO results. Returns the stream id or nil."
  (let ((response (+ (virtio-sound-control-virt card) +control-response-offset+)))
    (dotimes (i streams nil)
      (let* ((info (+ response 4 (* i +pcm-info-size+)))
             (direction (int::memref-unsigned-byte-8 (+ info +pcm-info-direction-offset+)))
             (channels (int::memref-unsigned-byte-8 (+ info +pcm-info-channels-max-offset+)))
             (formats (pcm-info-field info +pcm-info-formats-offset+))
             (rates (pcm-info-field info +pcm-info-rates-offset+)))
        (format t "Stream ~A: ~X ~X ~X ~X~%"
                i direction channels formats rates)
        (when (and (eql direction +d-output+)
                   (>= channels 2)
                   (logbitp +pcm-fmt-s16+ formats)
                   (logbitp +pcm-rate-44100+ rates))
          (return i))))))

(defun populate-event-queue (card)
  "Populate the event queue with a single writable buffer."
  (let* ((dev (virtio-sound-virtio-device card))
         (vq (virtio:virtio-virtqueue dev +eventq+))
         (frame (or (sup::allocate-physical-pages 1)
                    (return-from populate-event-queue nil)))
         (phys (* frame sup::+4k-page-size+))
         (virt (sup::convert-to-pmap-address phys)))
    (setf (virtio-sound-event-phys card) phys
          (virtio-sound-event-virt card) virt)
    (let ((desc (or (virtio:virtio-ring-alloc-descriptor vq)
                    (return-from populate-event-queue nil))))
      (setf (virtio:virtio-ring-desc-address vq desc) phys
            (virtio:virtio-ring-desc-length vq desc) +event-size+
            (virtio:virtio-ring-desc-flags vq desc) (ash 1 virtio:+virtio-ring-desc-f-write+)
            (virtio:virtio-ring-desc-next vq desc) 0)
      (virtio:virtio-ring-add-to-avail-ring vq desc)
      (virtio:virtio-kick dev +eventq+))))

(defun allocate-tx-buffers (card)
  "Allocate the TX period buffers and pre-link the three descriptors per period.
Period I uses descriptor head id 3*I."
  (let* ((dev (virtio-sound-virtio-device card))
         (txq (virtio:virtio-virtqueue dev +txq+))
         (n-periods (virtio-sound-n-periods card))
         (total-bytes (* n-periods +period-stride+))
         (frame (or (sup::allocate-physical-pages (ceiling total-bytes sup::+4k-page-size+))
                    (return-from allocate-tx-buffers nil)))
         (phys (* frame sup::+4k-page-size+))
         (virt (sup::convert-to-pmap-address phys)))
    (format t "Virtio-sound TX buffer at ~X~%" phys)
    (setf (virtio-sound-tx-phys card) phys
          (virtio-sound-tx-virt card) virt
          (virtio-sound-period-desc-ids card) (make-array n-periods))
    (dotimes (i n-periods)
      (let* ((base (+ phys (* i +period-stride+)))
             (hdr-desc (virtio:virtio-ring-alloc-descriptor txq))
             (data-desc (virtio:virtio-ring-alloc-descriptor txq))
             (status-desc (virtio:virtio-ring-alloc-descriptor txq)))
        (setf (aref (virtio-sound-period-desc-ids card) i) hdr-desc)
        ;; Write the PCM transfer header (stream ID) for this period.
        (setf (int::memref-unsigned-byte-32 (+ virt (* i +period-stride+)))
              (virtio-sound-stream-id card))
        ;; Header descriptor (read-only).
        (setf (virtio:virtio-ring-desc-address txq hdr-desc) base
              (virtio:virtio-ring-desc-length txq hdr-desc) +pcm-xfer-size+
              (virtio:virtio-ring-desc-flags txq hdr-desc) (ash 1 virtio:+virtio-ring-desc-f-next+)
              (virtio:virtio-ring-desc-next txq hdr-desc) data-desc)
        ;; Data descriptor (read-only).
        (setf (virtio:virtio-ring-desc-address txq data-desc) (+ base +pcm-xfer-size+)
              (virtio:virtio-ring-desc-length txq data-desc) (virtio-sound-period-bytes card)
              (virtio:virtio-ring-desc-flags txq data-desc) (ash 1 virtio:+virtio-ring-desc-f-next+)
              (virtio:virtio-ring-desc-next txq data-desc) status-desc)
        ;; Status descriptor (write-only).
        (setf (virtio:virtio-ring-desc-address txq status-desc) (+ base +pcm-xfer-size+ (virtio-sound-period-bytes card))
              (virtio:virtio-ring-desc-length txq status-desc) +pcm-status-size+
              (virtio:virtio-ring-desc-flags txq status-desc) (ash 1 virtio:+virtio-ring-desc-f-write+)
              (virtio:virtio-ring-desc-next txq status-desc) 0)))
    t))

(defun fill-dma (card data buf)
  "Convert single-float samples to little-endian s16 and write them to the
DMA data area at virtual address DATA."
  (declare (type (simple-array single-float (*)) buf)
           (ignore card))
  (let ((n-samples (length buf)))
    (dotimes (i n-samples)
      (let* ((s (aref buf i))
             (sc (max (min s 1.0f0) -1.0f0))
             (sr (if (< sc 0.0f0) (* sc 32768.0) (* sc 32767.0)))
             (si (truncate sr)))
        (setf (int::memref-unsigned-byte-8 (+ data (* 2 i))) (ldb (byte 8 0) si)
              (int::memref-unsigned-byte-8 (+ data (1+ (* 2 i)))) (ldb (byte 8 8) si))))
    (int::dma-write-barrier)))

(defun fill-period (card i buffer-fill-callback buf)
  "Fill period I via BUFFER-FILL-CALLBACK and write it to the DMA area.
Returns true if the callback produced audio, nil if it ran silent."
  (let ((result (funcall buffer-fill-callback buf 0 (length buf))))
    (unless result
      (fill buf 0.0))
    (fill-dma card
              (+ (virtio-sound-tx-virt card)
                 (* i +period-stride+)
                 +pcm-xfer-size+)
              buf)
    result))

(defun submit-period (card i)
  "Re-add period I's descriptor chain to the TX available ring and kick."
  (let* ((dev (virtio-sound-virtio-device card))
         (vq (virtio:virtio-virtqueue dev +txq+)))
    (virtio:virtio-ring-add-to-avail-ring vq (aref (virtio-sound-period-desc-ids card) i))
    (virtio:virtio-kick dev +txq+)))

(defun virtio-sound-register (device)
  (let* ((card (make-instance 'virtio-sound :virtio-device device)))
    (format t "Detected virtio sound device ~S~%" device)
    (setf (virtio:virtio-device-status device) virtio:+virtio-status-reset+)
    ;; Allocate the control request/response area.
    (let ((frame (or (sup::allocate-physical-pages (ceiling (* 2 +control-response-offset+) sup::+4k-page-size+))
                     (progn (format t "virtio-sound: Unable to allocate control buffer~%")
                            (return-from virtio-sound-register nil)))))
      (let ((phys (* frame sup::+4k-page-size+)))
        (setf (virtio-sound-control-phys card) phys
              (virtio-sound-control-virt card) (sup::convert-to-pmap-address phys))))
    ;; Set the driver bit in the status field.
    (setf (virtio:virtio-device-status device) (logior virtio:+virtio-status-acknowledge+
                                                       virtio:+virtio-status-driver+))
    ;; Allocate virtqueues.
    (when (not (virtio:virtio-configure-virtqueues device 4))
      (format t "virtio-sound: Unable to configure virtqueues~%")
      (setf (virtio:virtio-device-status device) virtio:+virtio-status-failed+)
      (return-from virtio-sound-register nil))
    ;; Configuration complete, go to OK mode.
    (setf (virtio:virtio-device-status device) (logior virtio:+virtio-status-acknowledge+
                                                       virtio:+virtio-status-driver+
                                                       virtio:+virtio-status-ok+))
    ;; Populate the event queue, as the spec requires.
    (populate-event-queue card)
    ;; Find a suitable output stream.
    (let* ((streams (virtio:virtio-device-specific-header/32 device +config-streams-offset+))
           (status (pcm-info card 0 streams))
           (stream-id (when (eql status +s-ok+)
                        (find-output-stream card streams))))
      (unless stream-id
        (format t "virtio-sound: No suitable output stream found (~S streams, status ~S)~%" streams status)
        (setf (virtio:virtio-device-status device) virtio:+virtio-status-failed+)
        (return-from virtio-sound-register nil))
      (format t "virtio-sound: Using output stream ~S of ~S~%" stream-id streams)
      (setf (virtio-sound-stream-id card) stream-id))
    ;; Allocate the TX period buffers.
    (when (not (allocate-tx-buffers card))
      (format t "virtio-sound: Unable to allocate TX buffers~%")
      (setf (virtio:virtio-device-status device) virtio:+virtio-status-failed+)
      (return-from virtio-sound-register nil))
    ;; Since we did some work during setup there are pending IRQs, ack
    ;; them before attaching otherwise we'll get an IRQ flood.
    (virtio:virtio-ack-irq device (virtio:virtio-isr-status device))
    (setf (virtio-sound-irq-handler card) (sup:make-simple-irq (virtio:virtio-device-irq device)))
    (sound:register-sound-card card))
  t)

(defmethod sound:sound-card-presence-event ((card virtio-sound))
  (virtio-sound-boot-id card))

(defmethod sound:sound-card-run ((card virtio-sound) buffer-fill-callback)
  (handler-case
      (let* ((dev (virtio-sound-virtio-device card))
             (txq (virtio:virtio-virtqueue dev +txq+))
             (irq (virtio-sound-irq-handler card))
             (n-periods (virtio-sound-n-periods card))
             (period-bytes (virtio-sound-period-bytes card))
             (n-samples (truncate period-bytes 2))
             (buf (make-array n-samples :element-type 'single-float))
             (idle-periods 0))
        (unwind-protect
             (progn
               ;; Arm the IRQ before START so the first period-completion
               ;; interrupt is not rejected while the handler is masked.
               (sup:simple-irq-unmask irq)
               ;; Negotiate stream parameters and prepare the stream.
               (with-virtio-sound-access (card)
                 (unless (eql (set-params card) +s-ok+)
                   (error "Unable to configure parameters"))
                 (unless (eql (prepare card) +s-ok+)
                   (error "Unable to prepare card"))
                 ;; Pre-buffer the entire hardware buffer and start.
                 (dotimes (i n-periods)
                   (fill-period card i buffer-fill-callback buf)
                   (submit-period card i))
                 (unless (eql (start card) +s-ok+)
                   (error "Unable to start playback")))
               ;; Playback loop.
               (loop
                 (sync:wait-for-objects irq (virtio-sound-boot-id card))
                 (with-virtio-sound-access (card)
                   ;; Acknowledge the interrupt before processing so that
                   ;; interrupts are not lost.
                   (virtio:virtio-ack-irq dev (virtio:virtio-isr-status dev))
                   (sup:simple-irq-unmask irq)
                   ;; Drain completed periods from the TX used ring.
                   (let ((refilled nil))
                     (loop
                       (when (eql (virtio:virtio-ring-used-idx txq)
                                  (virtio:virtqueue-last-seen-used txq))
                         (return))
                       (let* ((ring-entry (rem (virtio:virtqueue-last-seen-used txq)
                                               (virtio:virtqueue-size txq)))
                              (id (virtio:virtio-ring-used-elem-id txq ring-entry))
                              (i (truncate id 3)))
                         (setf (virtio:virtqueue-last-seen-used txq)
                               (ldb (byte 16 0) (1+ (virtio:virtqueue-last-seen-used txq))))
                         (when (fill-period card i buffer-fill-callback buf)
                           (setf refilled t))
                         (submit-period card i)))
                     (if refilled
                         (setf idle-periods 0)
                         (incf idle-periods))
                     (when (>= idle-periods +idle-periods+)
                       (return)))))
               ;; Idle: stop & release the stream, then drain any remaining
               ;; buffers. RELEASE completes all pending I/O.
               (with-virtio-sound-access (card)
                 (stop card)
                 (release card)
                 (loop
                   (when (eql (virtio:virtio-ring-used-idx txq)
                              (virtio:virtqueue-last-seen-used txq))
                     (int::dma-write-barrier)
                     (return))
                   (setf (virtio:virtqueue-last-seen-used txq)
                         (ldb (byte 16 0) (1+ (virtio:virtqueue-last-seen-used txq)))))))
          (virtio:virtio-ack-irq dev (virtio:virtio-isr-status dev))
          (sup:simple-irq-mask irq)))
        (device-disconnect ()
                           (format t "Card ~S disconnected.~%" card)
                           (throw 'mezzano.supervisor:terminate-thread nil))))

(virtio:define-virtio-driver virtio-sound virtio-sound-register +virtio-dev-id-sound+)
