;;;; A simple WAV player

(defpackage :mezzano.gui.music-player
  (:use :cl)
  (:export #:spawn))

(in-package :mezzano.gui.music-player)

(defclass music-player ()
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%audio-thread :initarg :audio-thread :accessor audio-thread)
   (%reader-thread :initarg :reader-thread :accessor reader-thread)
   (%font :initarg :font :reader font)
   (%frame :initarg :frame :reader frame)
   (%stream :initarg :stream :reader player-stream)
   (%state :initarg :state :accessor playback-state)
   (%worker-comm-lock :initarg :comm-lock :reader worker-comm-lock)
   (%worker-comm-cvar :initarg :comm-cvar :reader worker-comm-cvar)))

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

(defmethod dispatch-event (window (event worker-exited))
  (setf (audio-thread window) nil))

(defun wrap-data-chunk-data-samples-reader ()
  (let ((next (wav:wrap-format-chunk-data-reader)))
    (lambda (stream chunk-id chunk-data-size)
      (if (string= chunk-id "data")
          (prog1
              (file-position stream)
            (file-position stream (+ (file-position stream)
                                     chunk-data-size)))
          (funcall next stream chunk-id chunk-data-size)))))

(defun worker (player)
  (unwind-protect
       (let* ((stream (player-stream player))
              (riff-chunks (wav:read-wav-file stream
                                              :chunk-data-reader (wrap-data-chunk-data-samples-reader)))
              (riff (find "RIFF" riff-chunks
                          :key (lambda (e) (getf e :chunk-id))
                          :test #'string=))
              (fmt (find "fmt " riff-chunks
                         :key (lambda (e) (getf e :chunk-id))
                         :test #'string=))
              (data (find "data" riff-chunks
                          :key (lambda (e) (getf e :chunk-id))
                          :test #'string=)))
         (when (not (and riff
                         (string= (getf riff :file-type) "WAVE")
                         fmt
                         data))
           (format t "Invalid wav file, missing chunks.~%")
           (return-from worker))
         (let* ((fmt* (getf fmt :chunk-data))
                (compression (getf fmt* :compression-code))
                (channels (getf fmt* :number-of-channels))
                (sample-rate (getf fmt* :sample-rate))
                (sample-size (getf fmt* :significant-bits-per-sample))
                (data-size (getf data :chunk-data-size))
                (data-file-position (getf data :chunk-data)))
           (when (not (and (eql compression 1) ; uncompressed
                           (eql channels 2)
                           (eql sample-rate 44100)
                           (member sample-size '(8 16))))
             (format t "Unsupported wav file. ~S ~S ~S ~S ~S~%" fmt compression channels sample-rate sample-size)
             (return-from worker))
               (file-position stream data-file-position)
               (let* ((chunk-size #x4000)
                      (audio-sink (mezzano.driver.sound:make-sound-output-sink
                                   :buffer-duration 0.01
                                   :format (ecase sample-size
                                             (8 :pcm-u8)
                                             (16 :pcm-s16le))))
                      (current-position 0)
                      (chunk-fifo (mezzano.supervisor:make-fifo 4)))
                 ;; Pre-fill: push first chunks so the sink has data
                 ;; immediately when the sound worker starts playback.
                 (dotimes (i 4)
                   (when (>= current-position data-size) (return))
                   (let* ((n-bytes (min chunk-size (- data-size current-position)))
                          (buf (make-array n-bytes :element-type '(unsigned-byte 8))))
                     (read-sequence buf stream)
                     (mezzano.driver.sound:output-sound buf audio-sink :end n-bytes)
                     (incf current-position n-bytes)))
                 ;; Reader thread: reads remaining chunks into FIFO while
                 ;; the main thread pushes them to the sink. Reading and
                 ;; playback overlap, so a slow read never drains the sink.
                 (setf (reader-thread player)
                       (mezzano.supervisor:make-thread
                         (lambda ()
                           (unwind-protect
                                (loop
                                 (when (>= current-position data-size) (return))
                                 (let* ((n-bytes (min chunk-size (- data-size current-position)))
                                        (buf (make-array n-bytes :element-type '(unsigned-byte 8))))
                                   (read-sequence buf stream)
                                   (incf current-position n-bytes)
                                   (mezzano.supervisor:fifo-push buf chunk-fifo)))
                             (mezzano.supervisor:fifo-push nil chunk-fifo)))
                        :name (format nil "Audio reader for ~S" player)))
                 (unwind-protect
                      (loop
                         (let ((buf (mezzano.supervisor:fifo-pop chunk-fifo)))
                           (when (null buf) (return))
                           (mezzano.driver.sound:output-sound buf audio-sink
                                                              :end (length buf))))
                   (mezzano.driver.sound:flush-sink audio-sink)))))
     (mezzano.supervisor:fifo-push (make-instance 'worker-exited)
                                   (fifo player))))

(defun main (path)
  (with-simple-restart (abort "Close music player")
    (let ((font (mezzano.gui.font:open-font
                 mezzano.gui.font:*default-monospace-font*
                 mezzano.gui.font:*default-monospace-font-size*))
          (fifo (mezzano.supervisor:make-fifo 50))
          (width 400)
          (height 100))
      (with-open-file (audio-stream path :element-type '(unsigned-byte 8))
        (mezzano.gui.compositor:with-window (window fifo width height)
          (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
                 (frame (make-instance 'mezzano.gui.widgets:frame
                                       :framebuffer framebuffer
                                       :title (file-namestring path)
                                       :close-button-p t
                                       :damage-function (mezzano.gui.widgets:default-damage-function window)))
                 (player (make-instance 'music-player
                                        :fifo fifo
                                        :window window
                                        :font font
                                        :frame frame
                                        :stream audio-stream
                                        :comm-lock (mezzano.supervisor:make-mutex "Music player lock")
                                        :comm-cvar (mezzano.supervisor:make-condition-variable "Music player cvar")
                                        :state :playing)))
            (setf (mezzano.gui.compositor:name window) player)
            (unwind-protect
                 (progn
                   (setf (slot-value player '%audio-thread)
                         (mezzano.supervisor:make-thread
                          (lambda () (worker player))
                          :name (format nil "Audio worker for ~S" player)
                          :initial-bindings `((*terminal-io* ,*terminal-io*)
                                              (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                              (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                              (*error-output* ,(make-synonym-stream '*terminal-io*))
                                              (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                              (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                              (*query-io* ,(make-synonym-stream '*terminal-io*)))))
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
                          (dispatch-event player (mezzano.supervisor:fifo-pop fifo))
                        (error (c)
                          (ignore-errors
                            (format t "Error: ~A~%" c)))
                        ;; Exit when the close button is clicked.
                        (mezzano.gui.widgets:close-button-clicked ()
                          (return-from main)))))
              (when (audio-thread player)
                (mezzano.supervisor:terminate-thread (audio-thread player))
                (mezzano.supervisor:terminate-thread (reader-thread player)))
              (mezzano.supervisor:with-mutex ((worker-comm-lock player))
                (setf (playback-state player) :exit)
                (mezzano.supervisor:condition-notify (worker-comm-cvar player))))))))))

(defun spawn (path)
  (setf path (merge-pathnames path))
  (mezzano.supervisor:make-thread (lambda () (main path))
                                  :name (format nil "Music Player - ~S" path)
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Music player console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
