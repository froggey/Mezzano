(in-package #:cl-video)

(defclass static-stream-record (video-stream-record)
  ())

(defmethod initialize-instance :after ((rec static-stream-record) &key &allow-other-keys)
  (initialize-ring rec 1 (* 640 480 3) '(unsigned-byte 8)))

(defmethod stream-playback-start ((rec static-stream-record))
  (bt:release-lock (vacancy-lock (car (wcursor rec))))
  nil)

(defmethod stream-playback-stop ((rec static-stream-record))
  nil)

(defmethod frame-delay ((rec static-stream-record))
  ;; small enough for responsiveness sake
  0.2)
