(in-package :cl-video)

(defmethod play-audio-stream ((container av-container))
  (let* ((aout (audio-out container))
	 (audio-rec (audio-rec aout)))
    (when audio-rec
      (bt:make-thread
       #'(lambda ()
	   (stream-playback-start audio-rec)
	   (initialize-sink aout)
	   (unwind-protect
		(loop until (finish container)
		   for cur = (if (pause container) cur (pop-chunk-rcursor audio-rec))
		   for src = (frame cur) do
		   ;; pause synching protocol w/video stream
		     (bt:acquire-lock (pause-lock container))
		   ;; send the audio frame
		     (sink-frame aout src)
		     (loop while (pause container) do (sleep 0.2))
		     (bt:release-lock (pause-lock container))
		   ;; advance the cursor lock
		     (bt:acquire-lock (vacancy-lock (car (rcursor audio-rec))))
		     (bt:release-lock (vacancy-lock cur))
		     (when (eql cur (final audio-rec))
		       (return))
		     (sleep (frame-delay audio-rec)))
	     (stream-playback-stop audio-rec)
	     (close-sink aout)))
       :name "Audio stream playback"))))
    
(defmethod play-video-stream ((container av-container))
  (bt:make-thread 
   #'(lambda ()
       (with-slots (height width) container
	 (let* ((display (xlib:open-default-display))
		(window (xlib:create-window :parent (xlib:screen-root (xlib:display-default-screen display))
					    :x 0 :y 0 :width width :height height
					    :event-mask '(:exposure :key-press #+nil :resize-redirect)))
		(gc (xlib:create-gcontext :drawable window))
		(pixmap (xlib:create-pixmap :width width :height height :depth 24 :drawable window))
		(pixmap-gc (xlib:create-gcontext :drawable pixmap))
		(buffer (make-array `(,height ,width) :element-type 'xlib:pixel))
		(image (xlib:create-image  :data buffer :depth 24
					   :height height :width width))
		(rec (find-video-stream-record container)))
	   (unwind-protect
		(progn
		  (setf (xlib:wm-name window) (pathname-name (filename container)))
		  (xlib:map-window window)
		  (stream-playback-start rec)
		  (loop with quit = nil until quit
		     for cur = (if (pause container) cur (pop-chunk-rcursor rec))
		     for src = (frame cur) do
		       (loop for i from 0 below height do
			    (loop for j from 0 below width
			       for spos = (* 3 (+ j (* width i))) do
				 (setf (aref buffer i j)
				       (logior (ash (aref src (+ 2 spos)) 16) (ash (aref src (1+ spos)) 8) (aref src spos)))))
		       (xlib:put-image pixmap pixmap-gc image :width width :height height :x 0 :y 0)
		       (xlib:copy-area pixmap gc 0 0 width height window 0 0)
		       (xlib:display-force-output display)
		       (unless (pause container)
			 (bt:acquire-lock (vacancy-lock (car (rcursor rec))))
			 (bt:release-lock (vacancy-lock cur)))
		       (when (eql cur (final rec))
			 (return))
		       (sleep (frame-delay rec))
		       (xlib:event-case (display :timeout 0)
			 (:resize-request ()
					  t)
			 (:exposure ()
				    t)
			 (:key-press (window code)
				     (case (xlib:keysym->character
					    display
					    (xlib:keycode->keysym display code 0))
				       (#\q
					(setf (finish container) t)
					(setf quit t))
				       ((#\Space #\p)
					(setf (pause container) (not (pause container)))
					(bt:acquire-lock (pause-lock container))
					(bt:release-lock (pause-lock container))))
				     t))))
	     (stream-playback-stop rec)
	     (xlib:free-pixmap pixmap)
	     (xlib:free-gcontext gc)
	     (xlib:close-display display)))))
   :name "Video stream playback"))

(defun decode-file (pathname &key player-callback)
  (let ((container (make-instance (cond  ((string-equal "avi" (pathname-type pathname)) 'avi-mjpeg-container)
					 ((string-equal "gif" (pathname-type pathname)) 'gif-container)
					 ((string-equal "wav" (pathname-type pathname)) 'wav-container)
					 (t (error 'unrecognized-file-format)))
				  :filename pathname :player-callback player-callback
				  :audio-out (make-instance 'portaudio-pcm-output))))
    (decode container)))

(defun play (pathname)
  (decode-file pathname :player-callback
	       #'(lambda (video)
		   ;;has to use our specific decode for audio
		   (let ((a (find-pcm-stream-record video)))
		     (when a (setf (audio-rec (audio-out video)) a)))
		   (prime-all-streams video)
		   (play-audio-stream video) (play-video-stream video))))
