;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(defvar *rtc-is-utc* t "True if the RTC holds UTC, not local time.")
(defvar *time-zone* 0 "CL time zone.")

;;; Derived from SBCL 1.0.55's time code.
(defun leap-years-before (year)
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
          (truncate years 100))
       (truncate (+ years 300) 400))))

(defvar *days-before-month*
  #.(let ((reversed-result nil)
          (sum 0))
      (push nil reversed-result)
      (dolist (days-in-month '(31 28 31 30 31 30 31 31 30 31 30 31))
        (push sum reversed-result)
        (incf sum days-in-month))
      (coerce (nreverse reversed-result) 'simple-vector)))

(defun encode-universal-time (second minute hour date month year &optional time-zone)
  (check-type second (mod 60))
  (check-type minute (mod 60))
  (check-type hour (mod 24))
  (check-type date (integer 1 31))
  (check-type month (integer 1 12))
  (check-type year (integer 1900))
  (check-type time-zone (or null rational))
  (unless time-zone (setf time-zone *time-zone*))
  (let* ((days (+ (1- date)
                  (aref *days-before-month* month)
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
                           (if *rtc-is-utc* 0 *time-zone*))))

(defmacro time (form)
  `(%time (lambda () (progn ,form))))

(defun %time (fn)
  (let ((start-time (get-universal-time))
        (start-cycle (tsc))
        (start-gc-time *gc-time*))
    (multiple-value-prog1 (funcall fn)
      (let ((finish-cycle (tsc))
            (finish-time (get-universal-time)))
        (format *trace-output* "; Execution took ~:D seconds (~:D seconds of GC time).~%" (- finish-time start-time) (- *gc-time* start-gc-time))
        (format *trace-output* "; Execution took ~:D cycles.~%" (- finish-cycle start-cycle))))))
