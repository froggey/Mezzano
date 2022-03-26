;;;; Video decoder implementation
;;;; Supports MJPEG in AVI container
;;;; (c) 2017 Eugene Zaikonnikov <eugene@fucall.org>

(in-package :cl-video)

(define-constant +avif-must-use-index+ #x20) 

(define-constant  +pcmi-uncompressed+ 1)

(define-constant +avi-dht+
    #(#x01 #xA2 
      #x00 #x00 #x01 #x05 #x01 #x01 #x01 #x01 #x01 #x01 #x00 #x00 #x00 #x00 #x00 
      #x00 #x00 #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x01 
      #x00 #x03 #x01 #x01 #x01 #x01 #x01 #x01 #x01 #x01 #x01 #x00 #x00 #x00 #x00 
 
      #x00 #x00 #x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08 #x09 #x0A #x0B #x10 #x00 
      #x02 #x01 #x03 #x03 #x02 #x04 #x03 #x05 #x05 #x04 #x04 #x00 #x00 #x01 #x7D 
      #x01 #x02 #x03 #x00 #x04 #x11 #x05 #x12 #x21 #x31 #x41 #x06 #x13 #x51 #x61 
      #x07 #x22 #x71 #x14 #x32 #x81 #x91 #xA1 #x08 #x23 #x42 #xB1 #xC1 #x15 #x52 
      #xD1 #xF0 #x24 #x33 #x62 #x72 #x82 #x09 #x0A #x16 #x17 #x18 #x19 #x1A #x25 
      #x26 #x27 #x28 #x29 #x2A #x34 #x35 #x36 #x37 #x38 #x39 #x3A #x43 #x44 #x45 
      #x46 #x47 #x48 #x49 #x4A #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5A #x63 #x64 
 
      #x65 #x66 #x67 #x68 #x69 #x6A #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7A #x83 
      #x84 #x85 #x86 #x87 #x88 #x89 #x8A #x92 #x93 #x94 #x95 #x96 #x97 #x98 #x99 
      #x9A #xA2 #xA3 #xA4 #xA5 #xA6 #xA7 #xA8 #xA9 #xAA #xB2 #xB3 #xB4 #xB5 #xB6 
      #xB7 #xB8 #xB9 #xBA #xC2 #xC3 #xC4 #xC5 #xC6 #xC7 #xC8 #xC9 #xCA #xD2 #xD3 
      #xD4 #xD5 #xD6 #xD7 #xD8 #xD9 #xDA #xE1 #xE2 #xE3 #xE4 #xE5 #xE6 #xE7 #xE8 
      #xE9 #xEA #xF1 #xF2 #xF3 #xF4 #xF5 #xF6 #xF7 #xF8 #xF9 #xFA #x11 #x00 #x02 
      #x01 #x02 #x04 #x04 #x03 #x04 #x07 #x05 #x04 #x04 #x00 #x01 #x02 #x77 #x00 
 
      #x01 #x02 #x03 #x11 #x04 #x05 #x21 #x31 #x06 #x12 #x41 #x51 #x07 #x61 #x71 
      #x13 #x22 #x32 #x81 #x08 #x14 #x42 #x91 #xA1 #xB1 #xC1 #x09 #x23 #x33 #x52 
      #xF0 #x15 #x62 #x72 #xD1 #x0A #x16 #x24 #x34 #xE1 #x25 #xF1 #x17 #x18 #x19 
      #x1A #x26 #x27 #x28 #x29 #x2A #x35 #x36 #x37 #x38 #x39 #x3A #x43 #x44 #x45 
      #x46 #x47 #x48 #x49 #x4A #x53 #x54 #x55 #x56 #x57 #x58 #x59 #x5A #x63 #x64 
      #x65 #x66 #x67 #x68 #x69 #x6A #x73 #x74 #x75 #x76 #x77 #x78 #x79 #x7A #x82 
      #x83 #x84 #x85 #x86 #x87 #x88 #x89 #x8A #x92 #x93 #x94 #x95 #x96 #x97 #x98 
 
      #x99 #x9A #xA2 #xA3 #xA4 #xA5 #xA6 #xA7 #xA8 #xA9 #xAA #xB2 #xB3 #xB4 #xB5 
      #xB6 #xB7 #xB8 #xB9 #xBA #xC2 #xC3 #xC4 #xC5 #xC6 #xC7 #xC8 #xC9 #xCA #xD2 
      #xD3 #xD4 #xD5 #xD6 #xD7 #xD8 #xD9 #xDA #xE2 #xE3 #xE4 #xE5 #xE6 #xE7 #xE8 
      #xE9 #xEA #xF2 #xF3 #xF4 #xF5 #xF6 #xF7 #xF8 #xF9 #xFA)
  :test #'equalp)

(define-condition unsupported-avi-file-format (media-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Unsupported AVI file format"))))

(define-condition malformed-avi-file-format (media-decoder-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignorable condition))
	     (format stream "Malformed AVI file format"))))

(defclass avi-stream-record (stream-record)
  ((fcc-type :accessor fcc-type)
   (fcc-handler :accessor fcc-handler)
   (flags :accessor flags)
   (priority :accessor priority)
   (language :accessor language)
   (initial-frames :accessor initial-frames)
   (scale :accessor scale)
   (rate :accessor rate)
   (start :accessor start)
   (stream-length :accessor stream-length)
   (quality :accessor quality)
   (sample-size :accessor sample-size)
   (frame :accessor frame)
   (frame-delay :accessor frame-delay :initarg :frame-delay)
   (index :accessor index)))

(defclass mjpeg-stream-record (avi-stream-record video-stream-record)
  ((jpeg-descriptor :accessor jpeg-descriptor :initform (cl-jpeg::make-descriptor))))

(defmethod stream-playback-start ((rec avi-stream-record))
  (sleep (* (start rec) (/ (scale rec) (rate rec)))) ;stream delay, if any
  (call-next-method))

(defmethod shared-initialize :after ((rec mjpeg-stream-record) slots &key &allow-other-keys)
  (declare (ignorable slots))
  (flexi-streams:with-input-from-sequence (is +avi-dht+)
    (setf (jpeg::descriptor-byte-reader (jpeg-descriptor rec)) #'(lambda () (read-byte is)))
    (jpeg:read-dht (jpeg-descriptor rec)))
  (setf (buffer rec) (make-array (suggested-buffer-size rec) :element-type '(unsigned-byte 8))
	(jpeg:descriptor-source-cache (jpeg-descriptor rec)) (buffer rec))
  (initialize-ring rec (max 5 (* 1 (floor (rate rec) (scale rec)))) ;at least 5 chunks to prevent cursor deadlocks
		   (* (height (container rec)) (width (container rec)) 3) 'cl-jpeg::uint8))

(defclass audio-stream-record (avi-stream-record)
  ((compression-code :accessor compression-code)
   (number-of-channels :accessor number-of-channels)
   (sample-rate :accessor sample-rate)
   (average-bytes-per-second :accessor average-bytes-per-second)
   (block-align :accessor block-align)
   (significant-bits-per-sample :accessor significant-bits-per-sample)
   (extra-format-bytes :accessor extra-format-bytes)
   (extra-bytes :accessor extra-bytes)))

#+nil(defmethod shared-initialize :after ((rec audio-stream-record) slots &key &allow-other-keys)
  (declare (ignorable slots))
  (setf  (buffer rec) (make-array (suggested-buffer-size rec) :element-type '(unsigned-byte 8))))

(defmethod frame-size ((rec audio-stream-record))
  (suggested-buffer-size rec))

(defmethod read-audio-stream-header ((rec audio-stream-record) stream)
  (loop for chunk = (riff:read-riff-chunk stream)
     when (string-equal (riff:riff-chunk-id chunk) "strf") do
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
	   (read-sequence (extra-bytes rec) is)))
       (setf (buffer rec) (make-array (suggested-buffer-size rec) :element-type '(unsigned-byte 8)))
       (initialize-ring rec 16 (suggested-buffer-size rec) (sink-frame-element-type (audio-out (container rec))))
       (return)))

(defclass avi-mjpeg-container (av-container)
  ((chunk-decoder :accessor chunk-decoder)
   (padding :accessor padding :initform 0)
   (flags :accessor flags)
   (nstreams :accessor nstreams)))

(defmethod decode-media-stream ((rec audio-stream-record) fsize input-stream)
  (let* ((chunk (pop (wcursor rec)))
	 (cur-lock (vacancy-lock chunk))
	 (new-chunk (car (wcursor rec))))
    (when (> fsize (length (buffer rec)))
      (setf (buffer rec) (make-array fsize :element-type '(unsigned-byte 8))))
    (bt:acquire-lock (vacancy-lock new-chunk))
    (read-sequence (buffer rec) input-stream :end fsize)
    (translate-source-frame (audio-out (container rec)) (frame chunk))
    (bt:release-lock cur-lock)))

(defmethod decode-media-stream ((rec mjpeg-stream-record) fsize input-stream)
  (let* ((chunk (pop (wcursor rec)))
	 (cur-lock (vacancy-lock chunk))
	 (new-chunk (car (wcursor rec))))
    (when (> fsize (length (jpeg:descriptor-source-cache (jpeg-descriptor rec))))
      (setf (jpeg:descriptor-source-cache (jpeg-descriptor rec)) (make-array fsize :element-type '(unsigned-byte 8))))
    (bt:acquire-lock (vacancy-lock new-chunk))
    (read-sequence (jpeg:descriptor-source-cache (jpeg-descriptor rec)) input-stream :end fsize)
    (jpeg:decode-stream nil :buffer (frame chunk)
			:descriptor (jpeg-descriptor rec)
			:cached-source-p t)
    (bt:release-lock cur-lock)))

(defmethod initialize-instance :after ((s avi-mjpeg-container) &key &allow-other-keys)
  (setf (chunk-decoder s) #'(lambda (stream id size)
			      (if (member (subseq id 2) '("dc" "wb") :test #'string-equal)
				  (progn (decode-media-stream (elt (stream-records s) (parse-integer (subseq id 0 2))) size stream)
					 (when (plusp (padding s)) (loop repeat (rem size (padding s)) do (read-byte s))))
				  (read-sequence (make-array size :element-type '(unsigned-byte 8)) stream)))))

(defmethod read-avi-stream-info ((avi avi-mjpeg-container) stream)
  (loop for chunk = (riff:read-riff-chunk stream)
     with rec = (make-instance 'avi-stream-record :container avi)
     when (string-equal (riff:riff-chunk-id chunk) "strh") do
       (flexi-streams:with-input-from-sequence (is (riff:riff-chunk-data chunk))
	 (setf (fcc-type rec) (riff::read-fourcc is)
	       (fcc-handler rec) (riff::read-fourcc is)
	       (flags rec) (riff:read-u4 is)
	       (priority rec) (riff:read-u2 is)
	       (language rec) (riff:read-u2 is)
	       (initial-frames rec) (riff:read-u4 is)
	       (scale rec) (riff:read-u4 is)
	       (rate rec) (riff:read-u4 is)
	       (start rec) (riff:read-u4 is)
	       (stream-length rec) (riff:read-u4 is)
	       (suggested-buffer-size rec) (riff:read-u4 is)
	       (quality rec) (riff:read-u4 is)
	       (sample-size rec) (riff:read-u4 is)
	       (frame rec) (list (riff:read-u2 is) (riff:read-u2 is) (riff:read-u2 is) (riff:read-u2 is))))
       (when (and (string-equal (fcc-type rec) "vids") (not (member (fcc-handler rec) '("mjpg") :test #'string-equal)))
	 (error 'unsupported-avi-file-format))
       (cond ((and (string-equal (fcc-type rec) "vids") (string-equal (fcc-handler rec) "mjpg"))
	      (change-class rec 'mjpeg-stream-record) (setf (frame-delay rec) (/ (scale rec) (rate rec))))
	     ((string-equal (fcc-type rec) "auds")
	      (change-class rec 'audio-stream-record) (read-audio-stream-header rec stream) (setf (frame-delay rec) (/ (scale rec) (rate rec)))))
       (return-from read-avi-stream-info rec))
  (error 'malformed-avi-file-format))

(defmethod read-avi-header ((avi avi-mjpeg-container) stream)
  (loop for chunk = (riff:read-riff-chunk stream)
     when (string-equal (riff:riff-chunk-id chunk) "avih") do
       (flexi-streams:with-input-from-sequence (is (riff:riff-chunk-data chunk))
	 (riff:read-u4 is)   ;;dwMicroSecPerFrame
	 (riff:read-u4 is)   ;;dwMaxBytesPerSec
	 (setf (padding avi) (riff:read-u4 is)
	       (flags avi) (riff:read-u4 is))
	 (unless (zerop (logand (flags avi) +avif-must-use-index+))
	   (debug-log (format nil "CL-VIDEO WARNING: must use index flag is set, frame order is possibly incorrect~%")))
	 (riff:read-u4 is)   ;;dwTotalFrames
	 (riff:read-u4 is)   ;;dwInitialFrames
	 (setf (nstreams avi) (riff:read-u4 is))
	 (riff:read-u4 is) ;;dwSuggestedBufferSize
	 (setf (width avi) (riff:read-u4 is)
	       (height avi) (riff:read-u4 is))
	 (riff:read-u4 is) (riff:read-u4 is) (riff:read-u4 is) (riff:read-u4 is))
       (setf (stream-records avi)
	     (loop repeat (nstreams avi) collecting (read-avi-stream-info avi stream)))
       (return)))

(defmethod find-pcm-stream-record ((container av-container))
  nil)

(defmethod find-pcm-stream-record ((avi avi-mjpeg-container))
  (find-if #'(lambda (x) (and (eql (type-of x) 'audio-stream-record)
			      (eql (compression-code x) +pcmi-uncompressed+))) (stream-records avi)))

(defmethod decode ((avi avi-mjpeg-container))
  (with-open-file (stream (filename avi) :direction :input :element-type '(unsigned-byte 8))
    ;; read AVI header first
    (let* ((chunk (riff:read-riff-chunk stream))
	   (id (getf chunk :chunk-id))
	   (fourcc (getf chunk :file-type)))
      (unless (string-equal id "riff")
	(error 'unrecognized-file-format))
      (cond ((string-equal fourcc "avi ") (read-avi-header avi stream))
	    (t (error 'unsupported-avi-file-format))))
    (when (player-callback avi)
      (funcall (player-callback avi) avi))
    (loop for chunk = (riff:read-riff-chunk stream :chunk-data-reader (chunk-decoder avi))
       while (and chunk (not (finish avi))))
    (loop for rec in (stream-records avi) do 
	 (setf (final rec) (car (wcursor rec)))
	 (bt:release-lock (vacancy-lock (car (wcursor rec)))))))

(defun show-file-chunks (pathname)
  (with-open-file (stream pathname :direction :input :element-type '(unsigned-byte 8))
    (loop for chunk = (riff:read-riff-chunk stream)
	 while chunk do (format t "~A size ~D~%" (riff:riff-chunk-id chunk) (riff:riff-chunk-data-size chunk)))))
