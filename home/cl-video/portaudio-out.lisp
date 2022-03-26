(in-package :cl-video)

;;; a class & method to process the audio stream into floats as portaudio wants
;;; but we still want to run decode in another thread to avoid the skips
(defclass portaudio-pcm-output (audio-output)
  ((astream :accessor astream)))

(defmethod sink-frame-element-type ((aout portaudio-pcm-output))
  'float)

(defmethod initialize-sink ((aout portaudio-pcm-output))
  (portaudio:initialize)
  (with-slots (audio-rec astream) aout
    (setf astream
	  (portaudio:open-default-stream 0 (number-of-channels audio-rec) :float (coerce (sample-rate audio-rec) 'double-float)
					 (/ (frame-size audio-rec) (/ (significant-bits-per-sample audio-rec) 8))))
    (portaudio:start-stream astream)))

(defmethod sink-frame ((aout portaudio-pcm-output) frame)
  (portaudio:write-stream (astream aout) frame))

(defmethod close-sink ((aout portaudio-pcm-output))
  (portaudio:close-stream (astream aout))
  (portaudio:terminate))

(defmethod translate-source-frame ((aout portaudio-pcm-output) frame)
  (with-slots (audio-rec) aout
    (flexi-streams:with-input-from-sequence (is (buffer audio-rec))
      (let ((sample-size (/ (block-align audio-rec) (number-of-channels audio-rec))))
	(loop for i from 0 below (length frame) do
	     (setf (aref frame i) (if (= sample-size 1)
				      (float (/ (- (read-byte is) 128) 128))
				      (float (/ (let ((u2 (riff:read-u2 is)))
						  (if (> u2 32767)
						      (- u2 65536)
						      u2))
						32768)))))))))
  
