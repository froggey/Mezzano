(in-package #:sys.int)

;; RTC IO ports.
(defconstant +rtc-index-io-reg+ #x70)
(defconstant +rtc-data-io-reg+ #x71)
(defconstant +rtc-nmi-disable+ #x80)

;; RTC/CMOS registers.
(defconstant +rtc-second+   #x00)
(defconstant +rtc-minute+   #x02)
(defconstant +rtc-hour+     #x04)
(defconstant +rtc-hour-pm+ #x80
  "When the RTC uses 12-hour mode, this bit specifies PM.")
(defconstant +rtc-weekday+  #x06)
(defconstant +rtc-day+      #x07)
(defconstant +rtc-month+    #x08)
(defconstant +rtc-year+     #x09)
(defconstant +rtc-century+  #x32)
(defconstant +rtc-status-a+ #x0A)
(defconstant +rtc-status-a-update-in-progress+ #x80)
(defconstant +rtc-status-b+ #x0B)
(defconstant +rtc-status-b-24-hour-mode+ #x02)
(defconstant +rtc-status-b-binary-mode+ #x04)

(defun rtc (register)
  ;; Setting the high bit of the index register
  ;; will disable NMIs.
  (check-type register (unsigned-byte 7))
  (setf (io-port/8 +rtc-index-io-reg+) register)
  (io-port/8 +rtc-data-io-reg+))

(defun (setf rtc) (value register)
  ;; Setting the high bit of the index register
  ;; will disable NMIs.
  (check-type register (unsigned-byte 7))
  (setf (io-port/8 +rtc-index-io-reg+) register
        (io-port/8 +rtc-data-io-reg+) value))

(defun read-rtc-time (&optional (century-boundary 2012))
  "Read the time values from the RTC.
CENTURY-BOUNDARY is used to calculate the full year.
It should be the current year, or earlier."
  ;; http://wiki.osdev.org/CMOS
  ;; This repeatedly reads the RTC until the values match.
  (flet ((wait-for-rtc ()
           "Wait for the update-in-progress flag to clear. Returns false if the RTC times out."
           (dotimes (i 10000 nil)
             (when (zerop (logand (rtc +rtc-status-a+) +rtc-status-a-update-in-progress+))
               (return t))))
         (conv-bcd (v)
           "Convert a BCD byte to binary."
           (+ (ldb (byte 4 0) v)
              (* (ldb (byte 4 4) v) 10))))
    ;; Don't care about failure here.
    (wait-for-rtc)
    (let ((second (rtc +rtc-second+))
          (minute (rtc +rtc-minute+))
          (hour (rtc +rtc-hour+))
          (day (rtc +rtc-day+))
          (month (rtc +rtc-month+))
          (year (rtc +rtc-year+))
          (status-b (rtc +rtc-status-b+)))
      (do ((last-second nil (prog1 second (setf second (rtc +rtc-second+))))
           (last-minute nil (prog1 minute (setf minute (rtc +rtc-minute+))))
           (last-hour   nil (prog1 hour   (setf hour   (rtc +rtc-hour+))))
           (last-day    nil (prog1 day    (setf day    (rtc +rtc-day+))))
           (last-month  nil (prog1 month  (setf month  (rtc +rtc-month+))))
           (last-year   nil (prog1 year   (setf year   (rtc +rtc-year+)))))
          ((and (eql second last-second)
                (eql minute last-minute)
                (eql hour   last-hour)
                (eql day    last-day)
                (eql month  last-month)
                (eql year   last-year)))
        (when (not (wait-for-rtc))
          ;; RTC timed out, use these values anyway.
          (return)))
      (when (zerop (logand status-b +rtc-status-b-binary-mode+))
        ;; RTC is running in BCD mode.
        (setf second (conv-bcd second)
              minute (conv-bcd minute)
              ;; Preserve the 12-hour AM/PM bit.
              hour (logior (conv-bcd hour) (logand hour +rtc-hour-pm+))
              day (conv-bcd day)
              month (conv-bcd month)
              year (conv-bcd year)))
      (when (and (not (logtest status-b +rtc-status-b-24-hour-mode+))
                 (logtest hour +rtc-hour-pm+))
        ;; RTC in 12-hour mode and it's PM.
        (setf hour (rem (+ (logand hour (lognot +rtc-hour-pm+)) 12) 24)))
      ;; Calculate the full year.
      (let ((century (* (truncate century-boundary 100) 100)))
        (incf year century)
        (when (< year century-boundary) (incf year 100)))
      (values second minute hour day month year))))

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
      (read-rtc-time)
    (encode-universal-time second minute hour day month year
                           (if *rtc-is-utc* 0 *timeszone*))))

(defmacro time (form)
  `(%time (lambda () (progn ,form))))

(defun %time (fn)
  (let ((start-time (get-universal-time)))
    (multiple-value-prog1 (funcall fn)
      (format *trace-output* "; Execution took ~D seconds.~%" (- (get-universal-time) start-time)))))
