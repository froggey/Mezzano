(in-package :cl-video)

(define-constant  +pcmi-uncompressed+ 1)

(define-constant +frame-duration-seconds+ 5)
(define-constant +chunk-granularity-scale+ 8) ;8 per second

(define-condition unsupported-wav-file-format (media-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Unsupported WAV file format"))))

(define-condition malformed-wav-file-format (media-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Malformed WAV file format"))))

(defclass wav-stream-record (stream-record)
  ((compression-code :accessor compression-code)
   (number-of-channels :accessor number-of-channels)
   (sample-rate :accessor sample-rate)
   (average-bytes-per-second :accessor average-bytes-per-second)
   (block-align :accessor block-align)
   (significant-bits-per-sample :accessor significant-bits-per-sample)
   (extra-format-bytes :accessor extra-format-bytes)
   (extra-bytes :accessor extra-bytes)))

(defmethod stream-playback-start ((rec wav-stream-record))
  (call-next-method))

(defmethod frame-size ((rec wav-stream-record))
  ;;TODO: check it aligns the blocks well without skips/pauses
  (/ (* (sample-rate rec) (block-align rec)) +chunk-granularity-scale+))

(defmethod frame-delay ((rec wav-stream-record))
  0)

(defmethod read-audio-stream-header ((rec wav-stream-record) stream)
  (let ((chunk (riff:read-riff-chunk stream)))
    (flexi-streams:with-input-from-sequence (is (riff:riff-chunk-data chunk))
      (setf (compression-code rec) (riff:read-u2 is)
	    (number-of-channels rec) (riff:read-u2 is)
	    (sample-rate rec) (riff:read-u4 is)
	    (average-bytes-per-second rec) (riff:read-u4 is)
	    (block-align rec) (riff:read-u2 is)
	    (significant-bits-per-sample rec) (riff:read-u2 is))
      (debug-log (format nil "Audio stream with sample rate ~D at ~D bits per sample" (sample-rate rec) (significant-bits-per-sample rec)))
      (unless (eql (compression-code rec) +pcmi-uncompressed+)
	(setf (extra-format-bytes rec) (riff:read-u2 is)
	      (extra-bytes rec) (make-array (extra-format-bytes rec) :element-type (stream-element-type is)))
	(read-sequence (extra-bytes rec) is))
      (initialize-ring rec (* +frame-duration-seconds+ +chunk-granularity-scale+)
		       (/ (frame-size rec) (/ (significant-bits-per-sample rec) 8))
		       (sink-frame-element-type (audio-out (container rec)))))))

(defmethod decode-media-stream ((rec wav-stream-record) fsize input-stream)
  (setf (buffer rec) (make-array (frame-size rec) :element-type '(unsigned-byte 8)))
  (loop with frame-size = (frame-size rec)
     repeat (ceiling fsize frame-size) ; break down single large data frame into chunks
     summing frame-size into thus-far
     do (let* ((chunk (pop (wcursor rec)))
	       (cur-lock (vacancy-lock chunk))
	       (new-chunk (car (wcursor rec))))
	  (bt:acquire-lock (vacancy-lock new-chunk))
	  (read-sequence (buffer rec) input-stream
			 :end (1- (if (>= (- fsize thus-far) frame-size)
				      frame-size
				      (rem fsize frame-size))))
	  (translate-source-frame (audio-out (container rec)) (frame chunk))
	  (bt:release-lock cur-lock))))

(defclass wav-container (av-container)
  ((chunk-decoder :accessor chunk-decoder)))

(defmethod initialize-instance :after ((s wav-container) &key &allow-other-keys)
  (push (make-instance 'static-stream-record :container s) (stream-records s))
  (setf (chunk-decoder s) #'(lambda (stream id size)
			      (declare (ignorable id))
			      (decode-media-stream (car (stream-records s)) size stream))))

(defmethod read-wav-stream-info ((wav wav-container) stream)
  (let ((rec (make-instance 'wav-stream-record :container wav)))
    (read-audio-stream-header rec stream)
    rec))

(defmethod read-wav ((wav wav-container) stream)
  (push (read-wav-stream-info wav stream)
	(stream-records wav)))

(defmethod find-pcm-stream-record ((wav wav-container))
  ;; only one stream in wav container
  (car (stream-records wav)))

(defmethod decode ((wav wav-container))
  (with-open-file (stream (filename wav) :direction :input :element-type '(unsigned-byte 8))
    ;; read WAV header first
    (let* ((chunk (riff:read-riff-chunk stream))
	   (id (getf chunk :chunk-id))
	   (fourcc (getf chunk :file-type)))
      (unless (string-equal id "riff")
	(error 'unrecognized-file-format))
      (cond ((string-equal fourcc "wave") (read-wav wav stream))
	    (t (error 'unsupported-wav-file-format))))
    (when (player-callback wav)
      (funcall (player-callback wav) wav))
    (loop for chunk = (riff:read-riff-chunk stream :chunk-data-reader (chunk-decoder wav))
       while (and chunk (not (finish wav))))
    (let ((rec (car (stream-records wav))))
      (setf (final rec) (car (wcursor rec)))
      (bt:release-lock (vacancy-lock (car (wcursor rec)))))
    (let ((vrec (find-video-stream-record wav)))
      (bt:release-lock (vacancy-lock (car (wcursor vrec)))))))
