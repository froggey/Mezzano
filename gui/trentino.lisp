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

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-activation-event))
  (setf (mezzano.gui.widgets:activep (frame window)) (mezzano.gui.compositor:state event))
  (mezzano.gui.widgets:draw-frame (frame window)))

(defmethod dispatch-event (window (event mezzano.gui.compositor:mouse-event))
  (mezzano.gui.widgets:frame-mouse-event (frame window) event))

(defmethod dispatch-event (window (event mezzano.gui.compositor:window-close-event))
  (declare (ignore window event))
  (throw 'mezzano.supervisor::terminate-thread nil))

(defmethod dispatch-event (window (event mezzano.gui.compositor:key-event))
  (declare (ignore window event)))

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
	     (cl-video:stream-playback-stop audio-rec)))))))

(defmethod play-video-stream ((avi cl-video:avi-mjpeg-stream))
  (with-simple-restart (abort "Close media player")
    (let ((font (mezzano.gui.font:open-font
                 mezzano.gui.font:*default-monospace-font*
                 mezzano.gui.font:*default-monospace-font-size*))
          (fifo (mezzano.supervisor:make-fifo 50)))
      (mezzano.supervisor:make-trhread
       #'(lambda (avi)
	   (mezzano.gui.compositor:with-window (window fifo (cl-video:width avi) (cl-video:height avi))
	     (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
		    (frame (make-instance 'mezzano.gui.widgets:frame
					  :framebuffer framebuffer
					  :title (pathname-name path)
					  :close-button-p t
					  :damage-function (mezzano.gui.widgets:default-damage-function window)))
		    (viewer (make-instance 'media-player
					   :fifo fifo
					   :window window
					   :thread (mezzano.supervisor:current-thread)
					   :font font
					   :frame frame))
		    (rec (cl-video:find-mjpeg-stream-record avi))
		    (buffer (make-array (list (cl-video:height avi) (cl-video:width avi)) :element-type '(unsigned-byte 32))))
	       (sleep (* (cl-video:start rec) (/ (cl-video:scale rec) (cl-video:rate rec)))) ;stream delay, if any
	       (cl-video:stream-playback-start rec)
	       (unwind-protect
		    (loop for cur = (if (cl-video:pause avi) cur (pop (cl-video:rcursor rec)))
		       for src = (cl-video:frame cur)
		       with quit = nil until quit do
			 (loop for i from 0 below height do
			      (loop for j from 0 below width
				 for spos = (* 3 (+ j (* width i))) do
				   (setf (aref buffer i j)
					 (logior #xff000000 (ash (aref src (+ 2 spos)) 16) (ash (aref src (1+ spos)) 8) (aref src spos)))))
			 (multiple-value-bind (left right top bottom)
			     (mezzano.gui.widgets:frame-size frame)
			   (mezzano.gui:bitblt :set
					       (mezzano.gui:surface-width buffer) (mezzano.gui:surface-height buffer)
					       buffer 0 0
					       framebuffer
					       (+ left (- (truncate (- width left right) 2) (truncate (mezzano.gui:surface-width buffer) 2)))
					       (+ top (- (truncate (- height top bottom) 2) (truncate (mezzano.gui:surface-height buffer) 2))))
			   (mezzano.gui.widgets:draw-frame frame)
			   (mezzano.gui.compositor:damage-window window
								 0 0
								 width height))
			 (unless (cl-video:pause avi)
			   (mezzano.supervisor:acquire-mutex (cl-video:vacancy-lock (car (cl-video:rcursor rec))))
			   (mezzano.supervisor:release-mutex (cl-video:vacancy-lock cur)))
			 (when (eql cur (cl-video:final rec))
			   (return))
			 (sleep (/ (cl-video:scale rec) (cl-video:rate rec)))
		    
			 (handler-case
			     (dispatch-event viewer (mezzano.supervisor:fifo-pop fifo))
			   (error (c)
			     (ignore-errors
			       (format t "Error: ~A~%" c)))
			   ;; Exit when the close button is clicked.
			   (mezzano.gui.widgets:close-button-clicked ()
			     (setf (cl-video:pause avi) (not (cl-video:pause avi)))
			     (mezzano.supervisor:acquire-mutex (cl-video:pause-lock avi))
			     (mezzano.supervisor:release-mutex (cl-video:pause-lock avi))
			     (return-from main))))
		 (stream-playback-stop rec)))))))))

(defun main (path)
  (decode-file pathname :player-callback #'(lambda (avi)
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
                                                      (*query-io* ,(make-synonym-stream '*terminal-io*)))))
