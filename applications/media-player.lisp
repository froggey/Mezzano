;;;; A fancy all-purpose media player

(defpackage :mezzano.app.media-player
  (:use :cl)
  (:export #:spawn))

(in-package :mezzano.app.media-player)

(defclass media-player ()
  ((%event-mailbox :initarg :event-mailbox :reader event-mailbox)
   (%window :initarg :window :reader window)
   (%font :initarg :font :reader font)
   (%frame :initarg :frame :reader frame)
   (%audio-sink :initarg :audio-sink :reader audio-sink)

   (%container :initarg :container :reader container)

   (%demux-thread :initform nil :accessor demux-thread)

   (%video-decode-thread :initform nil :accessor video-decode-thread)
   (%video-packet-mailbox :reader video-packet-mailbox)
   (%video-frame-mailbox :reader video-frame-mailbox)

   (%audio-decode-thread :initform nil :accessor audio-decode-thread)
   (%audio-packet-mailbox :reader audio-packet-mailbox)
   (%audio-frame-mailbox :reader audio-frame-mailbox)))

(defgeneric dispatch-event (viewer event)
  (:method (viewer event) nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame window)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame window)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:mouse-event))
  (mezzano.gui.widgets:frame-mouse-event (frame window) event))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-close-event))
  (throw 'mezzano.supervisor:terminate-thread nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:quit-event))
  (throw 'mezzano.supervisor:terminate-thread nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:key-event))
  (declare (ignore window event)))

(defclass worker-exited () ())

(defclass demux-worker-exited (worker-exited) ())

(defmethod dispatch-event (window (event demux-worker-exited))
  (setf (demux-thread window) nil))

(defclass video-worker-exited (worker-exited) ())

(defmethod dispatch-event (window (event video-worker-exited))
  (setf (video-decode-thread window) nil))

(defclass audio-worker-exited (worker-exited) ())

(defmethod dispatch-event (window (event audio-worker-exited))
  (setf (audio-decode-thread window) nil))

(defun demux-worker (player tracks)
  (unwind-protect
       (let ((container (container player)))
         (loop
           (multiple-value-bind (track presentation-timestamp data start end)
               (orchard:read-packet container)
             (when (not track)
               (return))
             (loop for (target-track mailbox) in tracks
                   when (eql target-track track)
                     do (mezzano.sync:mailbox-send
                         (list track presentation-timestamp data start end)
                         mailbox)))))
    (mezzano.sync:mailbox-send
     (make-instance 'demux-worker-exited) (event-mailbox player))))

(declaim (inline conv-yuv))
(defun conv-yuv (y u v)
  (let ((y (float (the fixnum (- y 16)) 0.0))
        (u (float (the fixnum (- u 128)) 0.0))
        (v (float (the fixnum (- v 128)) 0.0)))
    (declare (type single-float y u v))
    (flet ((rc (n)
             (declare (type single-float n))
             (min 255 (max 0 (the fixnum (truncate n))))))
      (declare (inline rc))
      (values
       (rc (+ (* 1.164 y)              (*  1.596 v)))
       (rc (+ (* 1.164 y) (* -0.392 u) (* -0.813 v)))
       (rc (+ (* 1.164 y) (*  2.017 u)))))))

(defun convert-vp8-frame (vp8-frame surface)
  (declare #.orchard.utils:*optimize-settings*
           (type orchard.vp8:vp8-frame vp8-frame)
           (type mezzano.gui:surface surface))
  (let* ((luma (orchard.vp8:vp8-frame-y vp8-frame))
         (u (orchard.vp8:vp8-frame-u vp8-frame))
         (v (orchard.vp8:vp8-frame-v vp8-frame))
         (pixels (mezzano.gui:surface-pixels surface)))
    (declare (type (simple-array (unsigned-byte 32) (* *)) pixels))
    (loop for y of-type fixnum below (orchard.vp8:vp8-frame-height vp8-frame) do
      (loop for x of-type fixnum below (orchard.vp8:vp8-frame-width vp8-frame) do
        (multiple-value-bind (r g b)
            (conv-yuv (aref luma y x)
                      (aref u (the fixnum (ash y -1)) (the fixnum (ash x -1)))
                      (aref v (the fixnum (ash y -1)) (the fixnum (ash x -1))))
          (declare (type (unsigned-byte 8) r g b))
          (setf (aref pixels y x)
                (logior (the fixnum (ash #xFF (byte-position mezzano.gui:+colour-alpha-bits+)))
                        (the fixnum (ash r (byte-position mezzano.gui:+colour-red-bits+)))
                        (the fixnum (ash g (byte-position mezzano.gui:+colour-green-bits+)))
                        (the fixnum (ash b (byte-position mezzano.gui:+colour-blue-bits+))))))))))

(defun video-worker (player video-track)
  (declare (ignore video-track))
  (unwind-protect
       (let ((packet-mbox (video-packet-mailbox player))
             (frame-mbox (video-frame-mailbox player)))
         (loop
           (destructuring-bind (track presentation-timestamp data start end)
               (mezzano.sync:mailbox-receive packet-mbox)
             (when (null presentation-timestamp)
               (return))
             ;; Note - since the VP8 decoder has a pool of buffers and re-uses them,
             ;; we need to make sure we copy the frame in some way (in this case by
             ;; doing the yuv->rgb conversion) before decoding the next.
             (let ((frame (orchard:decode-packet track data start end)))
               (when frame
                 (let* ((surface (mezzano.gui:make-surface (orchard.vp8:vp8-frame-width frame)
                                                           (orchard.vp8:vp8-frame-height frame))))
                   (convert-vp8-frame frame surface)
                   (mezzano.sync:mailbox-send (list presentation-timestamp surface) frame-mbox)))))))
    (mezzano.sync:mailbox-send
     (make-instance 'video-worker-exited) (event-mailbox player))))

(defun find-video-track (container)
  (loop for track in (orchard:container-tracks container)
        when (typep track 'orchard:video-track)
          do (return (values track
                             (orchard:video-track-width track)
                             (orchard:video-track-height track)))))

(defun conv-pcm-data (data)
  (declare #.orchard.utils:*optimize-settings*
           (type (simple-array single-float (* 2)) data))
  (let* ((pcm (make-array (* (array-total-size data) 2) :element-type '(unsigned-byte 8))))
    (declare (type (simple-array (unsigned-byte 8) (*)) pcm))
    (loop for i of-type fixnum below (array-dimension data 0)
          for f0 of-type single-float = (aref data i 0)
          for f1 of-type single-float = (aref data i 1)
          for pcm0 of-type (signed-byte 16)
            = (orchard.utils:fclamp
               (the fixnum (truncate (* f0 32768.0f0)))
               -32768
               32767)
          for pcm1 of-type (signed-byte 16)
            = (orchard.utils:fclamp
               (the fixnum (truncate (* f1 32768.0f0)))
               -32768
               32767)
          do
             (setf (nibbles:sb16ref/le pcm (the fixnum (* i 4))) pcm0
                   (nibbles:sb16ref/le pcm (the fixnum (+ (the fixnum (* i 4)) 2))) pcm1))
    pcm))

(defun audio-worker (player audio-track)
  (declare (ignore audio-track))
  (unwind-protect
       (let ((packet-mbox (audio-packet-mailbox player))
             (frame-mbox (audio-frame-mailbox player)))
         (loop
           (destructuring-bind (track presentation-timestamp data start end)
               (mezzano.sync:mailbox-receive packet-mbox)
             (when (null presentation-timestamp)
               (return))
             (let ((frame (orchard:decode-packet track data start end)))
               (when frame
                 (let ((pcm (conv-pcm-data frame)))
                   (mezzano.sync:mailbox-send (list presentation-timestamp pcm) frame-mbox)))))))
    (mezzano.sync:mailbox-send
     (make-instance 'audio-worker-exited) (event-mailbox player))))

(defun find-audio-track (container)
  (loop for track in (orchard:container-tracks container)
        when (typep track 'orchard:audio-track)
          do (return track)))

(defun compute-window-size (content-width content-height)
  ;; Make a fake frame to get the frame size.
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size (make-instance 'mezzano.gui.widgets:frame))
    (values (+ left (max 32 content-width) right)
            (+ top (max 32 content-height) bottom))))

(defun make-thread-with-streams (name terminal-io function &rest arguments)
  (mezzano.supervisor:make-thread
   (lambda () (apply function arguments))
   :name name
   :initial-bindings `((*terminal-io* ,terminal-io)
                       (*standard-input* ,(make-synonym-stream '*terminal-io*))
                       (*standard-output* ,(make-synonym-stream '*terminal-io*))
                       (*error-output* ,(make-synonym-stream '*terminal-io*))
                       (*trace-output* ,(make-synonym-stream '*terminal-io*))
                       (*debug-io* ,(make-synonym-stream '*terminal-io*))
                       (*query-io* ,(make-synonym-stream '*terminal-io*)))))

(defun main-loop (player)
  (let* ((window (window player))
         (framebuffer (mezzano.gui.compositor:window-buffer window))
         (mbox (event-mailbox player))
         (audio-sink (audio-sink player))
         (audio-frame-mailbox (audio-frame-mailbox player))
         (video-frame-mailbox (video-frame-mailbox player))
         audio-sink-tag
         audio-frame-tag
         video-frame-tag
         timer-tag
         ;; When we have audio to sink, this is a list of
         ;; (OFFSET PRESENTATION-TIMESTAMP DATA)
         ;; Offset gets updated as we gradually sink data into
         ;; the output.
         (pending-audio nil)
         ;; This is our current presentation time, roughly paced by
         ;; audio output. If we don't have an audio stream, then a
         ;; timer is used instead.
         (current-time 0.0d0)
         (pacing-timer (mezzano.supervisor:make-timer :name player))
         ;; Presentation time of the last video frame displayed.
         (last-video-time 0.0d0))
    ;; Use the underlying WATCHER interface, rather than WAIT-FOR-OBJECTS
    ;; to reduce allocation.
    (mezzano.supervisor:with-watcher (watcher :name player)
      (mezzano.supervisor:watcher-add-object mbox watcher)
      (when audio-sink
        (setf audio-sink-tag (mezzano.supervisor:watcher-add-object
                              audio-sink watcher)))
      (setf audio-frame-tag (mezzano.supervisor:watcher-add-object
                             audio-frame-mailbox watcher))
      (setf video-frame-tag (mezzano.supervisor:watcher-add-object
                             video-frame-mailbox watcher))
      (setf timer-tag (mezzano.supervisor:watcher-add-object pacing-timer watcher))
      (unless audio-sink
        (mezzano.supervisor:timer-arm 0.0d0 pacing-timer)
        (setf (mezzano.supervisor:watcher-event-enabled-p timer-tag watcher) t))
      (loop
        (handler-case
            (progn
              ;; Reset the tags.
              (when audio-sink
                (setf (mezzano.supervisor:watcher-event-enabled-p audio-sink-tag watcher) nil)
                (setf (mezzano.supervisor:watcher-event-enabled-p audio-frame-tag watcher) nil)
                (setf (mezzano.supervisor:watcher-event-enabled-p video-frame-tag watcher) nil))
              ;; If we have audio pending to stream, then listen to
              ;; the audio sink, otherwise to the audio mailbox.
              ;; Keep track of the returned tag since we'll want to remove
              ;; it later.
              ;; Audio is used to time output here, we pace video output
              ;; on it and use backpressure in the mailboxes to prevent
              ;; the decoders from racing ahead.
              (when audio-sink
                (if pending-audio
                    (setf (mezzano.supervisor:watcher-event-enabled-p audio-sink-tag watcher) t)
                    (setf (mezzano.supervisor:watcher-event-enabled-p audio-frame-tag watcher) t))
                ;; We only care about video if it's time for a new frame.
                (when (< last-video-time current-time)
                  (setf (mezzano.supervisor:watcher-event-enabled-p video-frame-tag watcher) t)))
              ;; Wait for objects...
              (let ((objects (mezzano.supervisor:watcher-wait watcher)))
                ;; Main message queue.
                (when (member mbox objects)
                  (let ((obj (mezzano.sync:mailbox-receive mbox :wait-p nil)))
                    (when obj
                      (dispatch-event player obj))))
                ;; Audio data, ready to be pushed to the stream.
                (when (member audio-frame-mailbox objects)
                  (let ((obj (mezzano.sync:mailbox-receive audio-frame-mailbox :wait-p t)))
                    (when obj
                      (setf pending-audio (list* 0 obj))
                      (setf current-time (first obj)))))
                ;; We have audio data, and the sink isn't full.
                (when (member audio-sink objects)
                  (let* ((pcm (third pending-audio))
                         (new-offset (mezzano.driver.sound:output-sound pcm audio-sink :start (first pending-audio) :wait-p nil)))
                    (if (>= new-offset (length pcm))
                        (setf pending-audio nil)
                        (setf (first pending-audio) new-offset))))
                ;; New video frame. Since we only enable this when the
                ;; current frame is old, we know this frame is ready to
                ;; display immediately.
                (when (member video-frame-mailbox objects)
                  (let ((obj (mezzano.sync:mailbox-receive video-frame-mailbox :wait-p nil)))
                    (when obj
                      (let ((surface (second obj)))
                        (setf last-video-time (first obj))
                        (unless audio-sink
                          (mezzano.supervisor:timer-arm (- last-video-time current-time) pacing-timer)
                          (setf (mezzano.supervisor:watcher-event-enabled-p timer-tag watcher) t)
                          (setf (mezzano.supervisor:watcher-event-enabled-p video-frame-tag watcher) nil))
                        (multiple-value-bind (left right top bottom)
                            (mezzano.gui.widgets:frame-size
                             (frame player))
                          (declare (ignore right bottom))
                          (mezzano.gui:bitblt
                           :set
                           (mezzano.gui:surface-width surface)
                           (mezzano.gui:surface-height surface)
                           surface 0 0
                           framebuffer left top)
                          (mezzano.gui.compositor:damage-window
                           window
                           left top
                           (mezzano.gui:surface-width surface)
                           (mezzano.gui:surface-height surface)))))))
                (when (member pacing-timer objects)
                  (setf current-time last-video-time)
                  (setf (mezzano.supervisor:watcher-event-enabled-p timer-tag watcher) nil)
                  (setf (mezzano.supervisor:watcher-event-enabled-p video-frame-tag watcher) t))))
          (error (c)
            (ignore-errors
             (format t "Error: ~A~%" c)))
          ;; Exit when the close button is clicked.
          (mezzano.gui.widgets:close-button-clicked ()
            (return)))))))

(defmethod initialize-instance :after ((player media-player) &key &allow-other-keys)
  (let ((video-frame-mailbox (mezzano.sync:make-mailbox
                              :name (list 'video-frames player)
                              :capacity 10))
        (video-packet-mailbox (mezzano.sync:make-mailbox
                               :name (list 'video-packets player)
                               :capacity 5))
        (audio-frame-mailbox (mezzano.sync:make-mailbox
                              :name (list 'audio-frames player)
                              :capacity 10))
        (audio-packet-mailbox (mezzano.sync:make-mailbox
                               :name (list 'audio-packets player)
                               :capacity 5)))
    (setf (slot-value player '%video-packet-mailbox) video-packet-mailbox
          (slot-value player '%video-frame-mailbox) video-frame-mailbox)
    (setf (slot-value player '%audio-packet-mailbox) audio-packet-mailbox
          (slot-value player '%audio-frame-mailbox) audio-frame-mailbox)))

(defun main (path)
  (with-simple-restart (abort "Close media player")
    (let ((font (mezzano.gui.font:open-font
                 mezzano.gui.font:*default-monospace-font*
                 mezzano.gui.font:*default-monospace-font-size*))
          (mbox (mezzano.sync:make-mailbox :capacity 50))
          (container (orchard:open-container path)))
      (unwind-protect
           (multiple-value-bind (video-track video-width video-height)
               (find-video-track container)
             (multiple-value-bind (width height)
                 (compute-window-size (or video-width 200) (or video-height 50))
               (mezzano.gui.compositor:with-window (window mbox width height)
                 (let* ((audio-track (find-audio-track container))
                        (audio-sink (when audio-track
                                      (mezzano.driver.sound:make-sound-output-sink
                                       :buffer-duration 0.05
                                       :format :pcm-s16le
                                       :sample-rate (orchard:audio-track-sample-rate audio-track))))
                        (framebuffer (mezzano.gui.compositor:window-buffer window))
                        (frame (make-instance 'mezzano.gui.widgets:frame
                                              :framebuffer framebuffer
                                              :title (file-namestring path)
                                              :close-button-p t
                                              :damage-function (mezzano.gui.widgets:default-damage-function window)))
                        (player (make-instance 'media-player
                                               :event-mailbox mbox
                                               :window window
                                               :font font
                                               :frame frame
                                               :container container
                                               :audio-sink audio-sink)))
                   (setf (mezzano.gui.compositor:name window) player)
                   (unwind-protect
                        (progn
                          ;; Start worker threads
                          (setf (slot-value player '%demux-thread)
                                (make-thread-with-streams
                                 (list 'demux player) *terminal-io*
                                 #'demux-worker player
                                 (append
                                  (when audio-track
                                    (list
                                     (list audio-track (audio-packet-mailbox player))))
                                  (when video-track
                                    (list
                                     (list video-track (video-packet-mailbox player)))))))
                          (setf (slot-value player '%video-decode-thread)
                                (when video-track
                                  (make-thread-with-streams
                                   (list 'video-decode player) *terminal-io*
                                   #'video-worker player video-track)))
                          (setf (slot-value player '%audio-decode-thread)
                                (when audio-track
                                  (make-thread-with-streams
                                   (list 'audio-decode player) *terminal-io*
                                   #'audio-worker player audio-track)))
                          ;; Initial draw, the frame & background.
                          (multiple-value-bind (left right top bottom)
                              (mezzano.gui.widgets:frame-size frame)
                            (mezzano.gui:bitset :set
                                                (- width left right) (- height top bottom)
                                                mezzano.gui.theme:*background*
                                                framebuffer left top)
                            (mezzano.gui.widgets:draw-frame frame)
                            (mezzano.gui.compositor:damage-window window
                                                                  0 0
                                                                  width height))
                          ;; Main loop.
                          (main-loop player))
                     (when audio-sink
                       (mezzano.driver.sound:flush-sink audio-sink))
                     ;; Make sure to terminate worker threads.
                     (when (demux-thread player)
                       (mezzano.supervisor:terminate-thread (demux-thread player)))
                     (when (audio-decode-thread player)
                       (mezzano.supervisor:terminate-thread (audio-decode-thread player)))
                     (when (video-decode-thread player)
                       (mezzano.supervisor:terminate-thread (video-decode-thread player))))))))
        (orchard:close-container container)))))

(defun spawn (path)
  (setf path (merge-pathnames path))
  (make-thread-with-streams
   (format nil "Media Player - ~S" path)
   (make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                  :title "Media player console")
   #'main path))
