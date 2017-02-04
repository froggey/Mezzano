;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant +pit-irq+ 0)

(sys.int::defglobal *rtc-lock*)

(sys.int::defglobal *pit-tick-rate* 0)

(sys.int::defglobal *run-time-advance*)

(sys.int::defglobal *cpu-speed*)

;; http://wiki.osdev.org/Programmable_Interval_Timer
(defun configure-pit-tick-rate (hz)
  (let ((divisor (min #x10000 (truncate 1193180 hz))))
    (setf *pit-tick-rate* (/ 1.0 hz)
          *run-time-advance* (truncate (* *pit-tick-rate*
                                          internal-time-units-per-second)))
    ;; Channel 0. lobyte/hibyte. Square wave generator. Not BCD.
    (setf (sys.int::io-port/8 #x43) #x36)
    ;; Write low & high bytes to channel 0.
    (setf (sys.int::io-port/8 #x40) (ldb (byte 8 0) divisor)
          (sys.int::io-port/8 #x40) (ldb (byte 8 8) divisor))))

(defun calibrate-tsc-1 ()
  "Return the number of cycles per second, approximately."
  (let ((initial-time (get-internal-run-time))
        (start-time nil)
        (end-time nil)
        (start-cycle nil)
        (end-cycle nil))
    ;; Wait for the start of this tick.
    (loop
       (setf start-time (get-internal-run-time))
       (when (not (eq start-time initial-time))
         (return)))
    (setf start-cycle (sys.int::tsc))
    (loop
       (setf end-time (get-internal-run-time))
       (when (not (eq end-time start-time))
         (return)))
    (setf end-cycle (sys.int::tsc))
    (let* ((cycles (- end-cycle start-cycle))
           (time (/ (float (- end-time start-time)) internal-time-units-per-second))
           (cycles-per-second (/ cycles time)))
      cycles-per-second)))

(defun calibrate-tsc ()
  (let ((n (calibrate-tsc-1)))
    (dotimes (i 5)
      (setf n (/ (+ n (calibrate-tsc-1)) 2)))
    (setf *cpu-speed* n)))

(defun initialize-platform-time ()
  (when (not (boundp '*rtc-lock*))
    (setf *rtc-lock* (place-spinlock-initializer)))
  (configure-pit-tick-rate 100)
  (i8259-hook-irq +pit-irq+ 'pit-irq-handler)
  (i8259-unmask-irq +pit-irq+)
  (calibrate-tsc))

(defun pit-irq-handler (interrupt-frame irq)
  (declare (ignore irq))
  (beat-heartbeat *run-time-advance*)
  (profile-sample interrupt-frame))

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
  ;; will disable some types of NMI.
  (check-type register (unsigned-byte 7))
  (setf (sys.int::io-port/8 +rtc-index-io-reg+) register)
  (sys.int::io-port/8 +rtc-data-io-reg+))

(defun (setf rtc) (value register)
  ;; Setting the high bit of the index register
  ;; will disable some types of NMI.
  (check-type register (unsigned-byte 7))
  (setf (sys.int::io-port/8 +rtc-index-io-reg+) register
        (sys.int::io-port/8 +rtc-data-io-reg+) value))

(defun wait-for-rtc ()
  "Wait for the update-in-progress flag to clear. Returns false if the RTC times out."
  (dotimes (i 10000 nil)
    (when (zerop (logand (rtc +rtc-status-a+) +rtc-status-a-update-in-progress+))
      (return t))))

(defun read-rtc-time-1 ()
  "Read raw values from the RTC."
  ;; http://wiki.osdev.org/CMOS
  ;; This repeatedly reads the RTC until the values match.
  ;; Prevents reading a strange value when the RTC ticks.
  (safe-without-interrupts ()
    (with-symbol-spinlock (*rtc-lock*)
      ;; Don't care about failure here.
      (wait-for-rtc)
      (let ((second (rtc +rtc-second+))
            (minute (rtc +rtc-minute+))
            (hour (rtc +rtc-hour+))
            (day (rtc +rtc-day+))
            (month (rtc +rtc-month+))
            (year (rtc +rtc-year+))
            (status-b (rtc +rtc-status-b+)))
        ;; Keep rereading until we get two consistent times.
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
        (values second minute hour day month year status-b)))))

(defun read-rtc-time (&optional (century-boundary 2012))
  "Read the time values from the RTC.
CENTURY-BOUNDARY is used to calculate the full year.
It should be the current year, or earlier."
  (flet ((conv-bcd (v)
               "Convert a BCD byte to binary."
               (+ (ldb (byte 4 0) v)
                  (* (ldb (byte 4 4) v) 10))))
    (multiple-value-bind (second minute hour day month year status-b)
        (read-rtc-time-1)
      (when (zerop (logand status-b +rtc-status-b-binary-mode+))
        ;; RTC is running in BCD mode.
        (setf second (conv-bcd second)
              minute (conv-bcd minute)
              ;; Preserve the 12-hour AM/PM bit.
              hour (logior (conv-bcd (logand hour (lognot +rtc-hour-pm+)))
                           (logand hour +rtc-hour-pm+))
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
