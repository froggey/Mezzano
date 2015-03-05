;;;; Copyright (c) 2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; The low-level part of the profiler.
;;;; Deals with setting up/tearing down the profile buffer and actually
;;;; taking samples.

(in-package :mezzano.supervisor)

(defvar *enable-profiling* nil)

;; The profile buffer must be a wired simple-vector.
;; It is treated as a circular buffer, when it fill up newer entries
;; will replace older entries.
;; Assuming an average of 32 entries per sample (current thread plus backtrace),
;; and a sample frequency of 18Hz (the default PIT tick rate) a 1MB buffer
;; will last for just under 4 minutes, and a 16MB buffer will last for about
;; one hour.
(defvar *profile-buffer*)
(defvar *profile-buffer-head*)
(defvar *profile-buffer-tail*)

(defun profile-sample (interrupt-frame)
  (when (and (boundp '*enable-profiling*)
             *enable-profiling*)
    ;; If a thread's stack is not mapped in, then this may take a page-fault.
    ;; Produce a truncated sample if that happens.
    (let ((*pagefault-hook* (dx-lambda (interrupt-frame info fault-addr)
                              (declare (ignore interrupt-frame info fault-addr))
                              (return-from profile-sample))))
      (flet ((append-one (thing)
               (let ((buffer-len (sys.int::%array-like-length *profile-buffer*))
                     (place *profile-buffer-head*))
                 (incf *profile-buffer-head*)
                 (when (eql *profile-buffer-head* buffer-len)
                   (setf *profile-buffer-head* 0))
                 (when (eql *profile-buffer-head* *profile-buffer-tail*)
                   (incf *profile-buffer-tail*)
                   (when (eql *profile-buffer-tail* buffer-len)
                     (setf *profile-buffer-tail* 0)))
                 (setf (svref *profile-buffer* place) thing))))
        (append-one (current-thread))
        (append-one (interrupt-frame-raw-register interrupt-frame :rip))
        (do ((fp (interrupt-frame-raw-register interrupt-frame :rbp)
                 (sys.int::memref-unsigned-byte-64 fp 0)))
            ((eql fp 0))
          (append-one (sys.int::memref-unsigned-byte-64 fp 1)))))))

(defun start-profiling (&optional (buffer-size (* 1024 1024)))
  "Set up a profile sample buffer and enable sampling."
  (assert (not *enable-profiling*) ()
          "Profiling already started.")
  (when (or (not (boundp '*profile-buffer*))
            (not *profile-buffer*)
            (not (eql (length *profile-buffer*) buffer-size)))
    (setf *profile-buffer* (make-array buffer-size :area :wired)))
  (setf *profile-buffer-head* 0
        *profile-buffer-tail* 0)
  (setf *enable-profiling* t))

(defun stop-profiling ()
  "Stop sampling and retrieve the sample buffer."
  (assert *enable-profiling* ()
          "Profiling not started.")
  (setf *enable-profiling* nil)
  (let* ((n-entries (if (< *profile-buffer-head* *profile-buffer-tail*)
                        (+ (- (length *profile-buffer*) *profile-buffer-tail*)
                           *profile-buffer-head*)
                        *profile-buffer-head*))
         (buffer (make-array n-entries)))
    (cond ((< *profile-buffer-head* *profile-buffer-tail*)
           (replace buffer *profile-buffer*
                    :start2 *profile-buffer-tail*)
           (replace buffer *profile-buffer*
                    :start1 (- (length *profile-buffer*) *profile-buffer-tail*)
                    :end2 *profile-buffer-head*))
          (t (replace buffer *profile-buffer*
                    :end2 *profile-buffer-head*)))
    buffer))
