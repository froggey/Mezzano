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

   (%container :initarg :container :reader container)

   (%video-decode-thread :initarg :video-decode-thread :accessor video-decode-thread)
   (%video-frame-mailbox :initarg :video-frame-mailbox :reader video-frame-mailbox)
   (%video-decode-mailbox :initarg :video-decode-mailbox :reader video-decode-mailbox)

   (%audio-decode-thread :initarg :audio-decode-thread :accessor audio-decode-thread)
   (%audio-frame-mailbox :initarg :audio-frame-mailbox :reader audio-frame-mailbox)
   (%audio-decode-mailbox :initarg :audio-decode-mailbox :reader audio-decode-mailbox)))

(defgeneric dispatch-event (viewer event)
  (:method (viewer event) nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame window)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame window)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:mouse-event))
  (mezzano.gui.widgets:frame-mouse-event (frame window) event))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-close-event))
  (throw 'mezzano.supervisor::terminate-thread nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:quit-event))
  (throw 'mezzano.supervisor::terminate-thread nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:key-event))
  (declare (ignore window event)))

(defclass worker-exited () ())

(defclass video-worker-exited (worker-exited) ())

(defmethod dispatch-event (window (event video-worker-exited))
  (setf (video-decode-thread window) nil))

(defclass audio-worker-exited (worker-exited) ())

(defmethod dispatch-event (window (event audio-worker-exited))
  (setf (video-decode-thread window) nil))

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
                      (aref u (ash y -1) (ash x -1))
                      (aref v (ash y -1) (ash x -1)))
          (declare (type (unsigned-byte 8) r g b))
          (setf (aref pixels y x)
                (logior (the fixnum (ash #xFF (byte-position mezzano.gui:+colour-alpha-bits+)))
                        (the fixnum (ash r (byte-position mezzano.gui:+colour-red-bits+)))
                        (the fixnum (ash g (byte-position mezzano.gui:+colour-green-bits+)))
                        (the fixnum (ash b (byte-position mezzano.gui:+colour-blue-bits+))))))))))

(defun video-worker (player video-track)
  (unwind-protect
       (let ((frame-mbox (video-frame-mailbox player)))
         (loop
           (multiple-value-bind (presentation-timestamp frame)
               (orchard.matroska::decode-frame video-track)
             (when (null presentation-timestamp)
               (return))
             (let* ((surface (mezzano.gui:make-surface (orchard.vp8:vp8-frame-width frame)
                                                       (orchard.vp8:vp8-frame-height frame))))
               (convert-vp8-frame frame surface)
               (mezzano.sync:mailbox-send (list presentation-timestamp surface) frame-mbox)))))
    (mezzano.sync:mailbox-send
     (make-instance 'video-worker-exited) (event-mailbox player))))

(defun find-video-track (container)
  (loop for track in (orchard.matroska::container-tracks container)
        when (typep track 'orchard.matroska::video-track)
          do (return (values track
                             (orchard.matroska::video-track-width track)
                             (orchard.matroska::video-track-height track)))))

(defun conv-planar-float (data)
  (declare #.orchard.utils:*optimize-settings*
           (type (simple-vector 2) data))
  (let* ((ch0 (aref data 0))
         (ch1 (aref data 1))
         (pcm (make-array (* (length ch0) 4) :element-type '(unsigned-byte 8))))
    (declare (type (simple-array double-float (*)) ch0 ch1)
             (type (simple-array (unsigned-byte 8) (*)) pcm))
    (loop for i of-type fixnum below (length ch0)
          for f0 of-type double-float = (aref ch0 i)
          for f1 of-type double-float = (aref ch1 i)
          for pcm0 of-type (signed-byte 16)
            = (max -32768 (min 32767 (the fixnum (truncate (* f0 32767.0d0)))))
          for pcm1 of-type (signed-byte 16)
            = (max -32768 (min 32767 (the fixnum (truncate (* f1 32767.0d0)))))
          do
             (setf (nibbles:sb16ref/le pcm (the fixnum (* i 4))) pcm0
                   (nibbles:sb16ref/le pcm (the fixnum (+ (the fixnum (* i 4)) 2))) pcm1))
    pcm))

(defun audio-worker (player audio-track)
  (unwind-protect
       (let ((frame-mbox (audio-frame-mailbox player)))
         (loop
           (multiple-value-bind (presentation-timestamp frame)
               (orchard.matroska::decode-frame audio-track)
             (when (null presentation-timestamp)
               (return))
             (when frame
               (let ((pcm (conv-planar-float frame)))
                 (mezzano.sync:mailbox-send (list presentation-timestamp pcm) frame-mbox))))))
    (mezzano.sync:mailbox-send
     (make-instance 'audio-worker-exited) (event-mailbox player))))

(defun find-audio-track (container)
  (loop for track in (orchard.matroska::container-tracks container)
        when (typep track 'orchard.matroska::audio-track)
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

(defun main (path)
  (with-simple-restart (abort "Close media player")
    (let ((font (mezzano.gui.font:open-font
                 mezzano.gui.font:*default-monospace-font*
                 mezzano.gui.font:*default-monospace-font-size*))
          (mbox (mezzano.sync:make-mailbox :capacity 50))
          (container (orchard.matroska::open-matroska-file path))
          (audio-sink (mezzano.driver.sound:make-sound-output-sink
                       :buffer-duration 0.1
                       :format :pcm-s16le)))
      (unwind-protect
           (multiple-value-bind (video-track video-width video-height)
               (find-video-track container)
             (multiple-value-bind (width height)
                 (compute-window-size video-width video-height)
               (mezzano.gui.compositor:with-window (window mbox width height)
                 (let* ((audio-track (find-audio-track container))
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
                                               :container container))
                        (video-frame-mailbox (mezzano.sync:make-mailbox
                                              :name (list 'video-frames player)
                                              :capacity 10))
                        (video-decode-mailbox (mezzano.sync:make-mailbox
                                               :name (list 'video-decode player)))
                        (audio-frame-mailbox (mezzano.sync:make-mailbox
                                              :name (list 'audio-frames player)
                                              :capacity 10))
                        (audio-decode-mailbox (mezzano.sync:make-mailbox
                                               :name (list 'audio-decode player))))
                   (setf (slot-value player '%video-frame-mailbox) video-frame-mailbox
                         (slot-value player '%video-decode-mailbox) video-decode-mailbox)
                   (setf (slot-value player '%audio-frame-mailbox) audio-frame-mailbox
                         (slot-value player '%audio-decode-mailbox) audio-decode-mailbox)
                   (setf (mezzano.gui.compositor:name window) player)
                   (unwind-protect
                        (progn
                          (setf (slot-value player '%video-decode-thread)
                                (make-thread-with-streams
                                 (list 'video-decode player) *terminal-io* #'video-worker player video-track))
                          (setf (slot-value player '%audio-decode-thread)
                                (make-thread-with-streams
                                 (list 'audio-decode player) *terminal-io* #'audio-worker player audio-track))
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
                          (loop
                            (handler-case
                                (let ((objects (mezzano.sync:wait-for-objects
                                                mbox
                                                video-frame-mailbox
                                                audio-frame-mailbox)))
                                  (when (member mbox objects)
                                    (let ((obj (mezzano.sync:mailbox-receive mbox :wait-p nil)))
                                      (when obj
                                        (dispatch-event player obj))))
                                  (when (member video-frame-mailbox objects)
                                    (let ((obj (mezzano.sync:mailbox-receive video-frame-mailbox :wait-p nil)))
                                      (when obj
                                        (let ((surface (second obj)))
                                          (multiple-value-bind (left right top bottom)
                                              (mezzano.gui.widgets:frame-size frame)
                                            (declare (ignore right bottom))
                                            (mezzano.gui:bitblt
                                             :set
                                             (mezzano.gui:surface-width surface)
                                             (mezzano.gui:surface-height surface)
                                             surface 0 0
                                             framebuffer left top)
                                            (mezzano.gui.compositor:damage-window window
                                                                                  0 0
                                                                                  width height))))))
                                  (when (member audio-frame-mailbox objects)
                                    (let ((obj (mezzano.sync:mailbox-receive audio-frame-mailbox :wait-p nil)))
                                      (when obj
                                        (let ((pcm (second obj)))
                                          (mezzano.driver.sound:output-sound pcm audio-sink))))))
                              (error (c)
                                (ignore-errors
                                 (format t "Error: ~A~%" c)))
                              ;; Exit when the close button is clicked.
                              (mezzano.gui.widgets:close-button-clicked ()
                                (return-from main)))))
                     (when (video-decode-thread player)
                       (mezzano.supervisor:terminate-thread (video-decode-thread player))))))))
        (orchard.matroska::close-container container)
        (mezzano.driver.sound:flush-sink audio-sink)))))

(defun spawn (path)
  (setf path (merge-pathnames path))
  (make-thread-with-streams
   (format nil "Media Player - ~S" path)
   (make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                  :title "Media player console")
   #'main path))
