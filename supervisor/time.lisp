;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant internal-time-units-per-second 1000000)

(sys.int::defglobal *heartbeat-wait-queue*)
(sys.int::defglobal *run-time*)
(sys.int::defglobal *run-time-at-boot*)
(sys.int::defglobal *boot-time*)

(defun initialize-time ()
  (when (not (boundp '*run-time*))
    (setf *heartbeat-wait-queue* (make-wait-queue :name "Heartbeat wait queue"))
    (setf *run-time* 0))
  (setf *run-time-at-boot* *run-time*
        *boot-time* (get-universal-time)))

(defun beat-heartbeat (run-time-advance)
  (incf *run-time* run-time-advance)
  (with-wait-queue-lock (*heartbeat-wait-queue*)
    (do ()
        ((null (wait-queue-head *heartbeat-wait-queue*)))
      (wake-thread (pop-wait-queue *heartbeat-wait-queue*))))
  (decay-lights run-time-advance))

(defun wait-for-heartbeat ()
  (ensure-interrupts-enabled)
  (%call-on-wired-stack-without-interrupts
   (lambda (sp fp)
     (let ((self (current-thread)))
       (lock-wait-queue *heartbeat-wait-queue*)
       (push-wait-queue self *heartbeat-wait-queue*)
       (%lock-thread self)
       (unlock-wait-queue *heartbeat-wait-queue*)
       (setf (thread-wait-item self) *heartbeat-wait-queue*
             (thread-state self) :sleeping)
       (%reschedule-via-wired-stack sp fp)))
   nil))

(defun get-internal-real-time ()
  (+ (- *run-time* *run-time-at-boot*)
     (* *boot-time* internal-time-units-per-second)))

(defun get-internal-run-time ()
  *run-time*)

(defun sleep (seconds)
  (let ((wake-internal-time (+ *run-time*
                               (ceiling (* seconds internal-time-units-per-second)))))
     (loop
        (when (>= *run-time* wake-internal-time)
          (return))
        (wait-for-heartbeat))
     nil))
