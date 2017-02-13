(defpackage faux-bordeaux
  (:nicknames #:bt)
  (:import-from #:mezzano.supervisor #:make-thread #:make-lock #:acquire-lock #:release-lock)
  (:export #:make-thread #:make-lock #:acquire-lock #:release-lock))

(defpackage mezzano.gui.trentino
  (:use #:cl #:mezzano.gui.font #:cl-video)
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

(defun compute-window-size (image)
  ;; Make a fake frame to get the frame size.
  (multiple-value-bind (left right top bottom)
      (mezzano.gui.widgets:frame-size (make-instance 'mezzano.gui.widgets:frame))
    (values (+ left (max 32 (mezzano.gui:surface-width image)) right)
            (+ top (max 32 (mezzano.gui:surface-height image)) bottom))))

(defmethod play-audio-stream ((avi avi-mjpeg-stream))
  (let ((audio-rec (find-pcm-stream-record avi))
	astream)
    (when audio-rec
      (bt:make-thread
       #'(lambda ()
	   (portaudio:initialize)
	   (stream-playback-start audio-rec)
	   (setf astream
		 (portaudio:open-default-stream 0 (number-of-channels audio-rec) :float (coerce (sample-rate audio-rec) 'double-float)
						(/ (/ (rate audio-rec) (scale audio-rec)) (number-of-channels audio-rec))))
	   (sleep (* (start audio-rec) (/ (scale audio-rec) (rate audio-rec))))
	   (unwind-protect
		(loop for cur = (if (pause avi) cur (pop (rcursor audio-rec)))
		   for src = (frame cur)
		   until (finish avi) do
		   ;; pause synching protocol w/video stream
		     (bt:acquire-lock (pause-lock avi))
		   ;; send the audio frame
		     (mezzano.driver.intel-hda::play-sound src (first mezzano.driver.intel-hda::*cards*))
		     (loop while (pause avi) do (sleep 0.2))
		     (bt:release-lock (pause-lock avi))
		   ;; advance the cursor lock
		     (bt:acquire-lock (vacancy-lock (car (rcursor audio-rec))))
		     (bt:release-lock (vacancy-lock cur))
		     (when (eql cur (final audio-rec))
		       (return))
		     (sleep (/ (scale audio-rec) (rate audio-rec))))
	     (stream-playback-stop audio-rec)))))))

(defmethod play-video-stream ((avi avi-mjpeg-stream))
  (with-simple-restart (abort "Close media player")
    (let ((font (mezzano.gui.font:open-font
                 mezzano.gui.font:*default-monospace-font*
                 mezzano.gui.font:*default-monospace-font-size*))
          (fifo (mezzano.supervisor:make-fifo 50)))
      (bt:make-trhread
       #'(lambda (avi)
	   (mezzano.gui.compositor:with-window (window fifo (width avi) (height avi))
	     (let* ((framebuffer (mezzano.gui.compositor:window-buffer window))
		    (frame (make-instance 'mezzano.gui.widgets:frame
					  :framebuffer framebuffer
					  :title (namestring path)
					  :close-button-p t
					  :damage-function (mezzano.gui.widgets:default-damage-function window)))
		    (viewer (make-instance 'media-player
					   :fifo fifo
					   :window window
					   :thread (mezzano.supervisor:current-thread)
					   :font font
					   :frame frame))
		    (rec (find-mjpeg-stream-record avi)))
	       (sleep (* (start rec) (/ (scale rec) (rate rec)))) ;stream delay, if any
	       (stream-playback-start rec)
	       
	       (loop for cur = (if (pause avi) cur (pop (rcursor rec)))
		  for src = (frame cur)
		  with quit = nil until quit do
		    (loop for i from 0 below height do
			 (loop for j from 0 below width
			    for spos = (* 3 (+ j (* width i))) do
			      (setf (aref buffer i j)
				    (logior (ash (aref src (+ 2 spos)) 16) (ash (aref src (1+ spos)) 8) (aref src spos)))))
		    (multiple-value-bind (left right top bottom)
			(mezzano.gui.widgets:frame-size frame)
		      (mezzano.gui:bitblt :set
					  (mezzano.gui:surface-width image) (mezzano.gui:surface-height image)
					  image 0 0
					  framebuffer
					  (+ left (- (truncate (- width left right) 2) (truncate (mezzano.gui:surface-width image) 2)))
					  (+ top (- (truncate (- height top bottom) 2) (truncate (mezzano.gui:surface-height image) 2))))
		      (mezzano.gui.widgets:draw-frame frame)
		      (mezzano.gui.compositor:damage-window window
							    0 0
							    width height))
		    (unless (pause avi)
		      (bt:acquire-lock (vacancy-lock (car (rcursor rec))))
		      (bt:release-lock (vacancy-lock cur)))
		    (when (eql cur (final rec))
		      (return))
		    (sleep (/ (scale rec) (rate rec)))
		    
		    (handler-case
			(dispatch-event viewer (mezzano.supervisor:fifo-pop fifo))
		      (error (c)
			(ignore-errors
			  (format t "Error: ~A~%" c)))
		      ;; Exit when the close button is clicked.
		      (mezzano.gui.widgets:close-button-clicked ()
			(setf (pause avi) (not (pause avi)))
			(bt:acquire-lock (pause-lock avi))
			(bt:release-lock (pause-lock avi))
			(return-from main)))))))))))

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
