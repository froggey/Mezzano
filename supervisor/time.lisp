;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defglobal *rtc-is-utc* t "True if the RTC holds UTC, not local time.")
(defglobal *time-zone* 0 "CL time zone.")

(defglobal *days-before-month*)

(in-package :mezzano.supervisor)

(defconstant internal-time-units-per-second 1000000)

(sys.int::defglobal *heartbeat-wait-queue*)
(sys.int::defglobal *run-time*)
(sys.int::defglobal *run-time-at-boot*)
(sys.int::defglobal *boot-time*)

(defun initialize-time ()
  (when (not (boundp '*run-time*))
    (setf *heartbeat-wait-queue* (make-wait-queue :name "Heartbeat wait queue"))
    (setf *run-time* 0)
    (setf sys.int::*rtc-is-utc* t
          sys.int::*time-zone* 0)
    (setf sys.int::*days-before-month*
          #.(let ((reversed-result nil)
                  (sum 0))
              (push nil reversed-result)
              (dolist (days-in-month '(31 28 31 30 31 30 31 31 30 31 30 31))
                (push sum reversed-result)
                (incf sum days-in-month))
              (coerce (nreverse reversed-result) 'simple-vector))))
  (setf *run-time-at-boot* *run-time*
        *boot-time* 0))

(defun initialize-time-late ()
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

;;; Derived from SBCL 1.0.55's time code.
(defun leap-years-before (year)
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
          (truncate years 100))
       (truncate (+ years 300) 400))))

(defun encode-universal-time (second minute hour date month year &optional time-zone)
  (check-type second (mod 60))
  (check-type minute (mod 60))
  (check-type hour (mod 24))
  (check-type date (integer 1 31))
  (check-type month (integer 1 12))
  (check-type year (integer 1900))
  (check-type time-zone (or null rational))
  (unless time-zone (setf time-zone sys.int::*time-zone*))
  (let* ((days (+ (1- date)
                  (svref sys.int::*days-before-month* month)
                  (if (> month 2)
                      (leap-years-before (1+ year))
                      (leap-years-before year))
                  (* (- year 1900) 365)))
         (hours (+ hour (* days 24)))
         (time (+ second (* (+ minute (* (+ hours time-zone) 60)) 60))))
    time))

(defun get-universal-time ()
  (multiple-value-bind (second minute hour day month year)
      (mezzano.supervisor:read-rtc-time)
    (encode-universal-time second minute hour day month year
                           (if sys.int::*rtc-is-utc* 0 sys.int::*time-zone*))))
