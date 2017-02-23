;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The interface that all network card drivers must conform to.

(defpackage :mezzano.driver.sound
  (:use :cl)
  (:export #:sound-card
           #:register-sound-card
           #:sound-card-run
           #:make-sound-output-sink
           #:output-sound))

(in-package :mezzano.driver.sound)

(defclass sound-card () ())

(defclass sound-output-sink ()
  ((%buffer :initarg :buffer :reader buffer)
   (%buffer-empty :initarg :buffer-empty :accessor buffer-empty)
   (%buffer-head :initarg :buffer-head :accessor buffer-head)
   (%buffer-tail :initarg :buffer-tail :accessor buffer-tail)
   (%volume :initarg :volume :accessor sink-volume)))

(defvar *cards* '())

(defvar *sinks* '())
(defvar *sink-lock* (mezzano.supervisor:make-mutex "Sound output sink lock"))
(defvar *sink-cvar* (mezzano.supervisor:make-condition-variable "Sound output sink cvar"))

(defvar *master-volume* 1.0)

(defun mix (dest source volume &key (start1 0) end1 (start2 0) end2)
  (unless end1 (setf end1 (length dest)))
  (unless end2 (setf end2 (length source)))
  (when (eql dest source)
    (setf source (subseq source start2 end2)
          end2 (- end2 start2)
          start2 0))
  (assert (<= 0 start1 end1 (length dest)))
  (assert (<= 0 start2 end2 (length source)))
  (check-type dest (simple-array single-float (*)))
  (check-type source (simple-array single-float (*)))
  (check-type volume single-float)
  (locally
      (declare (optimize speed (safety 0))
               (type (simple-array single-float (*)) dest source)
               (type fixnum start1 end1 start2 end2)
               (type single-float volume))
    (loop
       for i fixnum from start1 below end1
       for j fixnum from start2 below end2
       do
         (incf (aref dest i) (* (aref source j) volume))))
  dest)

(defun mix-out-of-sink (sink samples start end)
  (let* ((buffer (buffer sink))
         (buffer-size (length buffer))
         (total-copied 0))
    (when (buffer-empty sink)
      (return-from mix-out-of-sink 0))
    (when (eql (- end start) 0)
      (return-from mix-out-of-sink 0))
    (when (<= (buffer-tail sink) (buffer-head sink))
      ;; Tail behind head, copy from head to end of buffer.
      (let ((n-to-copy (min (- end start)
                            (- buffer-size (buffer-head sink)))))
        (unless (eql n-to-copy 0)
          (mix samples buffer
               (* *master-volume* (sink-volume sink))
               :start1 start
               :start2 (buffer-head sink)
               :end2 (+ (buffer-head sink) n-to-copy))
          (incf start n-to-copy)
          (incf total-copied n-to-copy)
          (setf (buffer-head sink) (rem (+ (buffer-head sink) n-to-copy)
                                        buffer-size)))))
    (when (> (buffer-tail sink) (buffer-head sink))
      ;; Head behind tail, copy from head to tail.
      (let ((n-to-copy (min (- end start)
                            (- (buffer-tail sink) (buffer-head sink)))))
        (unless (eql n-to-copy 0)
          (mix samples buffer
               (* *master-volume* (sink-volume sink))
               :start1 start
               :start2 (buffer-head sink)
               :end2 (+ (buffer-head sink) n-to-copy))
          (incf start n-to-copy)
          (incf total-copied n-to-copy)
          (setf (buffer-head sink) (rem (+ (buffer-head sink) n-to-copy)
                                        buffer-size)))))
    (setf (buffer-empty sink) (eql (buffer-head sink) (buffer-tail sink)))
    total-copied))

(defgeneric sound-card-run (card buffer-fill-callback))

(defun refill-sound-output-buffer (buffer start end)
  (check-type buffer (simple-array single-float (*)))
  (assert (<= 0 start end (length buffer)))
  (locally
      (declare (optimize speed (safety 0))
               (type (simple-array single-float (*)) buffer)
               (type fixnum start end))
    (fill buffer 0.0 :start start :end end))
  (prog1
      (mezzano.supervisor:with-mutex (*sink-lock*)
        (cond ((endp *sinks*)
               ;; No more sinks, stop the card.
               nil)
              (t
               ;; Fill buffer from sinks as much as possible.
               (dolist (sink *sinks*)
                 (mix-out-of-sink sink buffer start end))
               ;; Remove all empty sinks.
               (setf *sinks* (delete-if
                              (lambda (sink)
                                (buffer-empty sink))
                              *sinks*))
               ;; Samples have been consumed, wake fillers.
               (mezzano.supervisor:condition-notify *sink-cvar* t)
               ;; Leave the card running.
               t)))))

(defun sound-worker (device)
  (loop
     (mezzano.supervisor:with-mutex (*sink-lock*)
       ;; Wait for at least one sink.
       (loop
          (when (not (endp *sinks*))
            (return))
          (mezzano.supervisor:condition-wait *sink-cvar* *sink-lock*)))
     ;; There are sinks. Start the card.
     (format t "Starting playback on ~S~%" device)
     (sound-card-run device 'refill-sound-output-buffer)
     (format t "Finished playback on ~S~%" device)))

(defun register-sound-card (device)
  (format t "Registered sound card ~S~%" device)
  (push device *cards*)
  (mezzano.supervisor:make-thread
   (lambda () (sound-worker device))
   :name (format nil "Sound worker for ~S" device)))

(defun make-sound-output-sink (&key (buffer-duration 0.1) (volume 1.0))
  (make-instance 'sound-output-sink
                 :volume volume
                 ;; 100ms at 44.1khz, 2 channels.
                 :buffer (make-array (* (truncate (* 44100 buffer-duration)) 2)
                                     :element-type 'single-float)
                 :buffer-empty t
                 :buffer-head 0
                 :buffer-tail 0))

(defun copy-into-sink (sink samples start end)
  (let* ((buffer (buffer sink))
         (buffer-size (length buffer))
         (total-copied 0))
    (when (and (not (buffer-empty sink))
               (eql (buffer-head sink) (buffer-tail sink)))
      ;; Buffer is full.
      (return-from copy-into-sink 0))
    (when (>= (buffer-tail sink) (buffer-head sink))
      ;; Head behind tail, copy to end of sink buffer.
      (let ((n-to-copy (min (- end start)
                            (- buffer-size (buffer-tail sink)))))
        (unless (eql n-to-copy 0)
          (replace buffer samples
                   :start1 (buffer-tail sink)
                   :end1 (+ (buffer-tail sink) n-to-copy)
                   :start2 start)
          (incf start n-to-copy)
          (incf total-copied n-to-copy)
          (setf (buffer-tail sink) (rem (+ (buffer-tail sink) n-to-copy)
                                        buffer-size))
          (setf (buffer-empty sink) nil))))
    (when (< (buffer-tail sink) (buffer-head sink))
      ;; Tail behind head, copy to head.
      (let ((n-to-copy (min (- end start)
                            (- (buffer-head sink) (buffer-tail sink)))))
        (unless (eql n-to-copy 0)
          (replace buffer samples
                   :start1 (buffer-tail sink)
                   :end1 (+ (buffer-tail sink) n-to-copy)
                   :start2 start)
          (incf start n-to-copy)
          (incf total-copied n-to-copy)
          (setf (buffer-tail sink) (rem (+ (buffer-tail sink) n-to-copy)
                                        buffer-size))
          (setf (buffer-empty sink) nil))))
    total-copied))

(defun output-sound (samples sink)
  (check-type sink sound-output-sink)
  (check-type samples (array single-float (*)))
  (assert (zerop (rem (length samples) 2))
          (samples sink)
          "SAMPLES must be a multiple of the number of channels.")
  (when (eql (length samples) 0)
    (return-from output-sound))
  (mezzano.supervisor:with-mutex (*sink-lock*)
    (let ((offset 0))
      (loop
         (when (buffer-empty sink)
           ;; Buffer is currently empty, sink is not live.
           (assert (not (member sink *sinks*)))
           (push sink *sinks*))
         ;; Fill the buffer as much as possible.
         (let ((total-copied (copy-into-sink sink samples offset (length samples))))
           (when (not (eql total-copied 0))
             (incf offset total-copied)
             (mezzano.supervisor:condition-notify *sink-cvar* t))
           (when (>= offset (length samples))
             (return))
           ;; Buffer must be full now.
           (assert (eql (buffer-tail sink) (buffer-head sink)))
           ;; Wait for the buffer to be consumed.
           (mezzano.supervisor:condition-wait *sink-cvar* *sink-lock*))))))
