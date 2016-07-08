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

(defconstant +seconds-in-week+ (* 60 60 24 7))
(defconstant +weeks-offset+ 2145)
(defconstant +seconds-offset+ 432000)
(defconstant +minutes-per-day+ (* 24 60))
(defconstant +quarter-days-per-year+ (1+ (* 365 4)))
(defconstant +quarter-days-per-century+ 146097)
(defconstant +november-17-1858+ 678882)
(defconstant +weekday-november-17-1858+ 2)

(defun decode-universal-time (universal-time &optional time-zone)
  "Converts a universal-time to decoded time format returning the following
   nine values: second, minute, hour, date, month, year, day of week (0 =
   Monday), T (daylight savings time) or NIL (standard time), and timezone.
   Completely ignores daylight-savings-time when time-zone is supplied."
  (multiple-value-bind (daylight seconds-west)
      (if time-zone
          (values nil (* time-zone 60 60))
          ;; TODO: DST!
          (values nil (* *time-zone* 60 60)))
    (multiple-value-bind (weeks secs)
        (truncate (+ (- universal-time seconds-west) +seconds-offset+)
                  +seconds-in-week+)
      (let ((weeks (+ weeks +weeks-offset+)))
        (multiple-value-bind (t1 second)
            (truncate secs 60)
          (let ((tday (truncate t1 +minutes-per-day+)))
            (multiple-value-bind (hour minute)
                (truncate (- t1 (* tday +minutes-per-day+)) 60)
              (let* ((t2 (1- (* (+ (* weeks 7) tday +november-17-1858+) 4)))
                     (tcent (truncate t2 +quarter-days-per-century+)))
                (setq t2 (mod t2 +quarter-days-per-century+))
                (setq t2 (+ (- t2 (mod t2 4)) 3))
                (let* ((year (+ (* tcent 100)
                                (truncate t2 +quarter-days-per-year+)))
                       (days-since-mar0
                        (1+ (truncate (mod t2 +quarter-days-per-year+) 4)))
                       (day (mod (+ tday +weekday-november-17-1858+) 7))
                       (t3 (+ (* days-since-mar0 5) 456)))
                  (cond ((>= t3 1989)
                         (setq t3 (- t3 1836))
                         (setq year (1+ year))))
                  (multiple-value-bind (month t3)
                      (truncate t3 153)
                    (let ((date (1+ (truncate t3 5))))
                      (values second minute hour date month year day
                              daylight
                              (if daylight
                                  (1+ (/ seconds-west 60 60))
                                  (/ seconds-west 60 60))))))))))))))

(defun get-decoded-time ()
  (decode-universal-time (get-universal-time)))

(defmacro time (form)
  `(%time (lambda () (progn ,form))))

(defun %time (fn)
  (let ((start-time (get-universal-time))
        (start-cycle (tsc))
        (start-gc-time *gc-time*))
    (multiple-value-prog1 (funcall fn)
      (let ((finish-cycle (tsc))
            (finish-time (get-universal-time)))
        (fresh-line *trace-output*)
        (format *trace-output* "; Execution took ~:D seconds (~:D seconds of GC time).~%" (- finish-time start-time) (- *gc-time* start-gc-time))
        (format *trace-output* "; Execution took ~:D cycles.~%" (- finish-cycle start-cycle))))))
