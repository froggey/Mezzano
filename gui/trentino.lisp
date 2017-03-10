;;; Trentino video player, makes use of CL-VIDEO library
;;; 2017 Eugene Zaikonnikov <eugene@funcall.org>
;;;; This code is licensed under the MIT license.

(defpackage mezzano.gui.trentino
  (:use #:cl)
  (:export #:spawn))

(in-package #:mezzano.gui.trentino)

(defclass media-player ()
  ((%fifo :initarg :fifo :reader fifo)
   (%window :initarg :window :reader window)
   (%thread :initarg :thread :reader thread)
   (%font :initarg :font :reader font)
   (%frame :initarg :frame :reader frame)))

(defgeneric dispatch-event (viewer event))

(defmethod dispatch-event (window (event null)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame window)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame window)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:mouse-event))
  (mezzano.gui.widgets:frame-mouse-event (frame window) event))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-close-event))
  (declare (ignore window event))
  (throw 'mezzano.supervisor::terminate-thread nil))

(define-condition pause-event () ())

(defmethod dispatch-event (window (event mezzano.gui.compositor:key-event))
  (when (not (mezzano.gui.compositor:key-releasep event))
    (let* ((ch (mezzano.gui.compositor:key-key event)))
      (cond ((char= ch #\Space)
             (signal 'pause-event))))))

;;; we speccialize own class & method to resample stream as necessary for Intel HDA
;;; but we still want to run decode in another thread
(defclass hda-pcm-stream-record (cl-video:audio-stream-record)
  ())

(defmethod shared-initialize :after ((rec hda-pcm-stream-record) slots &key &allow-other-keys)
  (declare (ignorable slots))
  (setf  (cl-video:buffer rec) (make-array (cl-video:suggested-buffer-size rec) :element-type '(unsigned-byte 8)))
  (cl-video:initialize-ring rec 16 (cl-video:suggested-buffer-size rec) '(signed-byte 16)))

(defun resample (from to sample)
  (let ((ratio (/ to from)))
    (decimate (upsample sample (numerator ratio)) (denominator ratio))))

(defmethod cl-video:decode-media-stream ((rec hda-pcm-stream-record) fsize input-stream)
  (let* ((chunk (pop (cl-video:wcursor rec)))
	 (cur-lock (cl-video:vacancy-lock chunk))
	 (new-chunk (car (cl-video:wcursor rec))))
    (mezzano.supervisor:acquire-mutex (cl-video:vacancy-lock new-chunk))
    (read-sequence (cl-video:buffer rec) input-stream :end fsize)
    (flexi-streams:with-input-from-sequence (is (cl-video:buffer rec))
      (let ((sample-size (/ (cl-video:block-align rec) (cl-video:number-of-channels rec))))
	(cl-video::debug-log (format nil "sbps ~D " (cl-video:significant-bits-per-sample rec)))
	(loop for i from 0 below fsize do
	     (setf (aref (cl-video:frame chunk) i) (if (= sample-size 1)
						       (case (cl-video:significant-bits-per-sample rec)
							 (8 (ash (- (read-byte is) 128) 8))
							 (16 (read-byte is))
							 (24 (floor (read-byte is) 256))
							 (otherwise (error "Unsupported sample size")))
					    (error "No support for stereo sound yet"))))))
    (mezzano.supervisor:release-mutex cur-lock)))

(defmethod find-hda-pcm-stream-record ((container cl-video:av-container))
  (find-if #'(lambda (x) (and (eql (type-of x) 'hda-pcm-stream-record)
			      (eql (cl-video:compression-code x) cl-video:+pcmi-uncompressed+))) (cl-video:stream-records container)))

(defmethod play-audio-stream ((container cl-video:av-container))
  (let ((audio-rec (cl-video:find-pcm-stream-record container)))
    (when audio-rec
      (mezzano.supervisor:make-thread
       #'(lambda ()
	   (cl-video:stream-playback-start audio-rec)
	   (unwind-protect
		(loop until (cl-video:finish container)
		   for cur = (if (cl-video:pause container) cur (cl-video:pop-chunk-rcursor audio-rec))
		   for src = (cl-video:frame cur) do
		   ;; pause synching protocol w/video stream
		     (mezzano.supervisor:acquire-mutex (cl-video:pause-lock container))
		   ;; send the audio frame
		     (mezzano.driver.intel-hda::play-sound src (first mezzano.driver.intel-hda::*cards*))
		     (loop while (cl-video:pause container) do (sleep 0.2))
		     (mezzano.supervisor:release-mutex (cl-video:pause-lock container))
		   ;; advance the cursor lock
		     (mezzano.supervisor:acquire-mutex (cl-video:vacancy-lock (car (cl-video:rcursor audio-rec))))
		     (mezzano.supervisor:release-mutex (cl-video:vacancy-lock cur))
		     (when (eql cur (cl-video:final audio-rec))
		       (return))
		     (sleep (cl-video:frame-delay audio-rec)))
	     (cl-video:stream-playback-stop audio-rec)))
       :name "Trentino audio worker"))))

(defun compute-window-size (container)
  ;; Make a fake frame to get the frame size.
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size (make-instance 'mezzano.gui.widgets:frame))
    (values (+ left (max 32 (cl-video:width container)) right)
            (+ top (max 32 (cl-video:height container)) bottom))))

(defmethod play-video-stream ((container cl-video:av-container))
  (with-simple-restart (abort "Close media player")
    (let ((font (mezzano.gui.font:open-font
                 mezzano.gui.font:*default-monospace-font*
                 mezzano.gui.font:*default-monospace-font-size*))
          (fifo (mezzano.supervisor:make-fifo 50))
	  (should-pause nil))
      (mezzano.supervisor:make-thread
	   #'(lambda ()
	       ;; Window needs to be slightly larger than the video to account for the frame.
	       (multiple-value-bind (window-width window-height)
		   (compute-window-size container)
		 (mezzano.gui.compositor:with-window (window fifo window-width window-height)
		   (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
			  (frame (make-instance 'mezzano.gui.widgets:frame
						:framebuffer framebuffer
						:title (pathname-name (cl-video:filename container))
						:close-button-p t
						:damage-function (mezzano.gui.widgets:default-damage-function window)))
			  (viewer (make-instance 'media-player
						 :fifo fifo
						 :window window
						 :thread (mezzano.supervisor:current-thread)
						 :font font
						 :frame frame))
			  (rec (cl-video:find-video-stream-record container))
			  (video-width (cl-video:width container))
			  (video-height (cl-video:height container))
			  (quit nil))

		     ;; Handling the window events in own thread
		     (mezzano.supervisor:make-thread
		      #'(lambda ()
			  (loop until quit do
			       (handler-case
				   ;; Don't block when reading the fifo.
				   (dispatch-event viewer (mezzano.supervisor:fifo-pop fifo))
				 (error (c)
				   (ignore-errors
				     (format t "Error: ~A~%" c)))
				 (mezzano.gui.widgets:close-button-clicked ()
				   (setf (cl-video:finish container) t)
				   (setf quit t))
				 (pause-event ()
				   (setf should-pause t)))))
              :name "Trentino event worker")
		     (cl-video:stream-playback-start rec)
		     (unwind-protect
			  (loop until quit
			     for cur = (if (cl-video:pause container) cur (cl-video:pop-chunk-rcursor rec))
			     for src = (cl-video:frame cur) do
			       (multiple-value-bind (left right top bottom)
				   (mezzano.gui.widgets:frame-size frame)
				 (declare (ignore right bottom))
				 (mezzano.gui.image:transcode-cl-jpeg-buffer
				  framebuffer left top
				  src video-width video-height 3)
				 (mezzano.gui.widgets:draw-frame frame)
				 (mezzano.gui.compositor:damage-window window
								       0 0
								       (cl-video:width container) (cl-video:height container)))
			       (unless (cl-video:pause container)
				 (mezzano.supervisor:acquire-mutex (cl-video:vacancy-lock (car (cl-video:rcursor rec))))
				 (mezzano.supervisor:release-mutex (cl-video:vacancy-lock cur)))
			       (when (eql cur (cl-video:final rec))
				 (return))
			       (sleep (cl-video:frame-delay rec))
			       (when should-pause
				 (setf (cl-video:pause container) (not (cl-video:pause container)))
				 (mezzano.supervisor:acquire-mutex (cl-video:pause-lock container))
				 (mezzano.supervisor:release-mutex (cl-video:pause-lock container))))
		       (cl-video:stream-playback-stop rec))))))
       :name "Trentino video worker"))))

(defun decode-file (pathname &key player-callback)
  (let ((container (make-instance (cond  ((string-equal "avi" (pathname-type pathname)) 'cl-video:avi-mjpeg-container)
					 ((string-equal "gif" (pathname-type pathname)) 'cl-video:gif-container)
					 (t (error 'unrecognized-file-format))) :filename pathname :player-callback player-callback)))
    (cl-video:decode container)))

(defun main (path)
  (decode-file path :player-callback #'(lambda (video)
						  (let ((a (cl-video:find-pcm-stream-record video)))
						    (when a (change-class a 'hda-pcm-stream-record)))
						  (cl-video:prime-all-streams video)
						  (play-audio-stream video) (play-video-stream video))))

(defun spawn (path)
  (setf path (merge-pathnames path))
  (mezzano.supervisor:make-thread (lambda () (main path))
                                  :name (format nil "Trentino Media Player - ~S" (pathname-name path))
                                  :initial-bindings `((*terminal-io* ,(make-instance 'mezzano.gui.popup-io-stream:popup-io-stream
                                                                                     :title "Media Player console"))
                                                      (*standard-input* ,(make-synonym-stream '*terminal-io*))
                                                      (*standard-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*error-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*trace-output* ,(make-synonym-stream '*terminal-io*))
                                                      (*debug-io* ,(make-synonym-stream '*terminal-io*))
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))
                                  :priority :low))
