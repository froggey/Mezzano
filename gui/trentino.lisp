;;; Trentino video player, makes use of CL-VIDEO library
;;; 2017 Eugene Zaikonnikov <eugene@funcall.org>

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

(defmethod decode-media-stream ((rec hda-pcm-stream-record) fsize input-stream)
  (let* ((chunk (pop (cl-video:wcursor rec)))
	 (cur-lock (cl-video:vacancy-lock chunk))
	 (new-chunk (car (cl-video:wcursor rec))))
    (mezzano.supervisor:acquire-mutex (cl-video:vacancy-lock new-chunk))
    (read-sequence (cl-video:buffer rec) input-stream :end fsize)
    (flexi-streams:with-input-from-sequence (is (cl-video:buffer rec))
      (let ((sample-size (/ (cl-video:block-align rec) (cl-video:number-of-channels rec))))
	(loop for i from 0 below fsize do
	     (setf (aref (cl-video:frame chunk) i) (if (= sample-size 1)
						       (case (cl-video:significant-bits-per-sample rec)
							 (8 (ash (- (read-byte is) 128) 8))
							 (16 (read-byte is))
							 (24 (floor (read-byte is) 256))
							 (otherwise (error "Unsupported sample size")))
					    (error "No support for stereo sound yet"))))))
    (mezzano.supervisor:release-mutex cur-lock)))

(defmethod find-hda-pcm-stream-record ((avi cl-video:avi-mjpeg-stream))
  (find-if #'(lambda (x) (and (eql (type-of x) 'hda-pcm-stream-record)
			      (eql (cl-video:compression-code x) cl-video:+pcmi-uncompressed+))) (cl-video:stream-records avi)))

(defmethod play-audio-stream ((avi cl-video:avi-mjpeg-stream))
  (let ((audio-rec (cl-video:find-pcm-stream-record avi)))
    (when audio-rec
      (mezzano.supervisor:make-thread
       #'(lambda ()
	   (cl-video:stream-playback-start audio-rec)
	   (sleep (* (cl-video:start audio-rec) (/ (cl-video:scale audio-rec) (cl-video:rate audio-rec))))
	   (unwind-protect
		(loop for cur = (if (cl-video:pause avi) cur (pop (cl-video:rcursor audio-rec)))
		   for src = (cl-video:frame cur)
		   until (cl-video:finish avi) do
		   ;; pause synching protocol w/video stream
		     (mezzano.supervisor:acquire-mutex (cl-video:pause-lock avi))
		   ;; send the audio frame
		     (mezzano.driver.intel-hda::play-sound src (first mezzano.driver.intel-hda::*cards*))
		     (loop while (cl-video:pause avi) do (sleep 0.2))
		     (mezzano.supervisor:release-mutex (cl-video:pause-lock avi))
		   ;; advance the cursor lock
		     (mezzano.supervisor:acquire-mutex (cl-video:vacancy-lock (car (cl-video:rcursor audio-rec))))
		     (mezzano.supervisor:release-mutex (cl-video:vacancy-lock cur))
		     (when (eql cur (cl-video:final audio-rec))
		       (return))
		     (sleep (/ (cl-video:scale audio-rec) (cl-video:rate audio-rec))))
	     (cl-video:stream-playback-stop audio-rec)))
       :name "Trentino audio worker"))))

(defun compute-window-size (avi)
  ;; Make a fake frame to get the frame size.
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size (make-instance 'mezzano.gui.widgets:frame))
    (values (+ left (max 32 (cl-video:width avi)) right)
            (+ top (max 32 (cl-video:height avi)) bottom))))

(defmethod play-video-stream ((avi cl-video:avi-mjpeg-stream))
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
		   (compute-window-size avi)
		 (mezzano.gui.compositor:with-window (window fifo window-width window-height)
		   (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
			  (frame (make-instance 'mezzano.gui.widgets:frame
						:framebuffer framebuffer
						:title (pathname-name (cl-video:filename avi))
						:close-button-p t
						:damage-function (mezzano.gui.widgets:default-damage-function window)))
			  (viewer (make-instance 'media-player
						 :fifo fifo
						 :window window
						 :thread (mezzano.supervisor:current-thread)
						 :font font
						 :frame frame))
			  (rec (cl-video:find-mjpeg-stream-record avi))
			  (avi-width (cl-video:width avi))
			  (avi-height (cl-video:height avi))
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
				   (setf (cl-video:finish avi) t)
				   (setf quit t))
				 (pause-event ()
				   (setf should-pause t)))))
              :name "Trentino event worker")

		     (sleep (* (cl-video:start rec) (/ (cl-video:scale rec) (cl-video:rate rec)))) ;stream delay, if any
		     (cl-video:stream-playback-start rec)
		     (unwind-protect
			  (loop until quit
			     for cur = (if (cl-video:pause avi) cur (pop (cl-video:rcursor rec)))
			     for src = (cl-video:frame cur) do
			       (multiple-value-bind (left right top bottom)
				   (mezzano.gui.widgets:frame-size frame)
				 (declare (ignore right bottom))
				 (mezzano.gui.image:transcode-cl-jpeg-buffer
				  framebuffer left top
				  src avi-width avi-height 3)
				 (mezzano.gui.widgets:draw-frame frame)
				 (mezzano.gui.compositor:damage-window window
								       0 0
								       (cl-video:width avi) (cl-video:height avi)))
			       (unless (cl-video:pause avi)
				 (mezzano.supervisor:acquire-mutex (cl-video:vacancy-lock (car (cl-video:rcursor rec))))
				 (mezzano.supervisor:release-mutex (cl-video:vacancy-lock cur)))
			       (when (eql cur (cl-video:final rec))
				 (return))
			       (sleep (/ (cl-video:scale rec) (cl-video:rate rec)))
			       (when should-pause
				 (setf (cl-video:pause avi) (not (cl-video:pause avi)))
				 (mezzano.supervisor:acquire-mutex (cl-video:pause-lock avi))
				 (mezzano.supervisor:release-mutex (cl-video:pause-lock avi))))
		       (cl-video:stream-playback-stop rec))))))
       :name "Trentino video worker"))))

(defun main (path)
  (cl-video:decode-file path :player-callback #'(lambda (avi)
						  (let ((a (find-pcm-stream-record avi)))
						    (when a (change-class a 'hda-pcm-stream-record)))
						  (play-audio-stream avi) (play-video-stream avi))))

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
