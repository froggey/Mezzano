;;;; Copyright (c) 2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;; The interface that all sound device drivers must conform to.

(defpackage :mezzano.driver.sound
  (:use :cl)
  (:export #:sound-card
           #:register-sound-card
           #:sound-card-run
           #:master-volume
           #:make-sound-output-sink
           #:sink-volume
           #:output-sound
           #:flush-sink
           #:sound-format-error))

(in-package :mezzano.driver.sound)

(defclass sound-card () ())

(defclass sound-output-sink ()
  ((%buffer :initarg :buffer :reader buffer)
   (%buffer-empty :initarg :buffer-empty :accessor buffer-empty)
   (%buffer-head :initarg :buffer-head :accessor buffer-head)
   (%buffer-tail :initarg :buffer-tail :accessor buffer-tail)
   (%format :initarg :format :accessor sink-format)
   (%volume :initarg :volume :accessor sink-volume)))

(defmethod (setf sink-volume) :around (value (sink sound-output-sink))
  (check-type value single-float)
  (call-next-method))

(define-condition sound-format-error (simple-error)
  ((%format :initarg :format
            :reader sound-format-error-format)
   (%channels :initarg :channels
              :reader sound-format-error-channels)
   (%sample-rate :initarg :sample-rate
                 :reader sound-format-error-sample-rate)))

(defvar *cards* '())

(defvar *sinks* '())
(defvar *sink-lock* (mezzano.supervisor:make-mutex "Sound output sink lock"))
(defvar *sink-cvar* (mezzano.supervisor:make-condition-variable "Sound output sink cvar"))

(defvar *master-volume* 1.0)

(defun master-volume ()
  *master-volume*)

(defun (setf master-volume) (value)
  (check-type value single-float)
  (setf *master-volume* value))

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
   :name (format nil "Sound worker for ~S" device)
   :priority :high))

(defgeneric transcode (audio-format audio-buffer data-buffer &key (start1 0) end1 (start2 0) end2))
(defgeneric elements-per-sample (audio-format))

(defmacro define-audio-transcoder (name (input-elements element-type) &body body)
  `(progn
     (defmethod transcode ((audio-format (eql ',name)) audio-buffer data-buffer &key (start1 0) end1 (start2 0) end2)
       (check-type data-buffer (array ,element-type (*)))
       (check-type audio-buffer (array single-float (*)))
       (setf end1 (or end1 (length audio-buffer)))
       (setf end2 (or end2 (length data-buffer)))
       (assert (<= 0 start1 end1 (length audio-buffer)))
       (assert (<= 0 start2 end2 (length data-buffer)))
       (let ((n-samples 0))
         (flet ((read-element ()
                  (assert (< start2 end2))
                  (prog1
                      (aref data-buffer start2)
                    (incf start2))))
           (loop
              (when (>= start1 end1)
                (return))
              (when (>= start2 end2)
                (return))
              (setf (aref audio-buffer start1)
                    (let ,(loop
                             for elt in input-elements
                             collect `(,elt (read-element)))
                      ,@body))
              (incf n-samples)
              (incf start1)))
         (values n-samples
                 (* n-samples ',(length input-elements)))))
     (defmethod elements-per-sample ((audio-format (eql ',name)))
       ',(length input-elements))))

(define-audio-transcoder :pcm-single-float ((sample) single-float)
  sample)

(define-audio-transcoder :pcm-u8 ((octet) (unsigned-byte 8))
  (/ (float (- octet 128)) 128.0))

(define-audio-transcoder :pcm-s16le ((octet-1 octet-2) (unsigned-byte 8))
  (let* ((raw-sample (logior octet-1
                             (ash octet-2 8))))
    (/ (if (> raw-sample 32767)
           (logior raw-sample (ash -1 16))
           raw-sample)
       32768.0)))

(defun make-sound-output-sink (&key
                                 (buffer-duration 0.2)
                                 buffer-size-in-samples
                                 (volume 1.0)
                                 (format :pcm-single-float)
                                 (channels 2)
                                 (sample-rate 44100))
  (unless (eql channels 2)
    (error 'sound-format-error
           :format format
           :channels channels
           :sample-rate sample-rate
           :format-control "Non-stereo sound formats not supported."))
  (unless (eql sample-rate 44100)
    (error 'sound-format-error
           :format format
           :channels channels
           :sample-rate sample-rate
           :format-control "Non-44.1kHz sound formats not supported."))
  (make-instance 'sound-output-sink
                 :volume volume
                 :format format
                 :buffer (make-array (or buffer-size-in-samples
                                         (* (truncate (* 44100 buffer-duration)) 2))
                                     :element-type 'single-float)
                 :buffer-empty t
                 :buffer-head 0
                 :buffer-tail 0))

(defun copy-into-sink (sink samples start end)
  (let* ((buffer (buffer sink))
         (buffer-size (length buffer))
         (total-copied 0)
         (elements-consumed 0))
    (when (and (not (buffer-empty sink))
               (eql (buffer-head sink) (buffer-tail sink)))
      ;; Buffer is full.
      (return-from copy-into-sink 0))
    (when (>= (buffer-tail sink) (buffer-head sink))
      ;; Head behind tail, copy to end of sink buffer.
      (let ((n-to-copy (min (/ (- end start) (elements-per-sample (sink-format sink)))
                            (- buffer-size (buffer-tail sink)))))
        (unless (eql n-to-copy 0)
          (multiple-value-bind (n-samples-sourced
                                n-elements-consumed)
              (transcode (sink-format sink)
                         buffer samples
                         :start1 (buffer-tail sink)
                         :end1 (+ (buffer-tail sink) n-to-copy)
                         :start2 start)
            (incf start n-elements-consumed)
            (incf total-copied n-samples-sourced)
            (incf elements-consumed n-elements-consumed)
            (setf (buffer-tail sink) (rem (+ (buffer-tail sink) n-samples-sourced)
                                          buffer-size))
            (unless (eql n-samples-sourced 0)
              (setf (buffer-empty sink) nil))))))
    (when (< (buffer-tail sink) (buffer-head sink))
      ;; Tail behind head, copy to head.
      (let ((n-to-copy (min (/ (- end start) (elements-per-sample (sink-format sink)))
                            (- (buffer-head sink) (buffer-tail sink)))))
        (unless (eql n-to-copy 0)
          (multiple-value-bind (n-samples-sourced
                                n-elements-consumed)
              (transcode (sink-format sink)
                         buffer samples
                         :start1 (buffer-tail sink)
                         :end1 (+ (buffer-tail sink) n-to-copy)
                         :start2 start)
            (incf start n-elements-consumed)
            (incf total-copied n-samples-sourced)
            (incf elements-consumed n-elements-consumed)
            (setf (buffer-tail sink) (rem (+ (buffer-tail sink) n-samples-sourced)
                                          buffer-size))
            (unless (eql n-samples-sourced 0)
              (setf (buffer-empty sink) nil))))))
    (values total-copied
            elements-consumed)))

(defun output-sound (samples sink &key (start 0) end)
  (check-type sink sound-output-sink)
  (check-type samples (array * (*)))
  (setf end (or end (length samples)))
  (assert (<= 0 start end (length samples)))
  (when (eql (- end start) 0)
    (return-from output-sound))
  (mezzano.supervisor:with-mutex (*sink-lock*)
    (let ((offset start))
      (loop
         (when (buffer-empty sink)
           ;; Buffer is currently empty, sink is not live.
           (assert (not (member sink *sinks*)))
           (push sink *sinks*))
         ;; Fill the buffer as much as possible.
         (multiple-value-bind (total-samples-copied
                               total-elements-consumed)
             (copy-into-sink sink samples offset end)
           (incf offset total-elements-consumed)
           (when (not (eql total-samples-copied 0))
             (mezzano.supervisor:condition-notify *sink-cvar* t))
           (when (>= offset end)
             (return))
           ;; Buffer must be full now.
           (assert (eql (buffer-tail sink) (buffer-head sink)))
           ;; Wait for the buffer to be consumed.
           (mezzano.supervisor:condition-wait *sink-cvar* *sink-lock*))))))

(defun flush-sink (sink)
  "Drop all samples in SINK's internal buffer, returning the number of samples dropped."
  (check-type sink sound-output-sink)
  (mezzano.supervisor:with-mutex (*sink-lock*)
    (if (buffer-empty sink)
        0
        (let* ((head (buffer-head sink))
               (tail (buffer-tail sink))
               (size (length (buffer sink)))
               (n-samples (if (>= head tail)
                              (+ (- size head) tail)
                              (- tail head))))
          (setf (buffer-head sink) 0
                (buffer-tail sink) 0
                (buffer-empty sink) t)
          n-samples))))
