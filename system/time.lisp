(in-package :mezzano.internals)

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

(defun format-time (stream object &optional colon-p at-sign-p)
  (declare (ignore at-sign-p))
  (when (and colon-p
             (>= object 60))
    (format stream "~:D second~:P or " object))
  (multiple-value-bind (minutes actual-seconds)
      (truncate object 60)
    (multiple-value-bind (hours actual-minutes)
        (truncate minutes 60)
      (when (not (zerop hours))
        (format stream "~:D hour~:P, " hours))
      (when (or (not (zerop hours))
                (not (zerop actual-minutes)))
        (format stream "~:D minute~:P, " actual-minutes))
      (format stream "~:D second~:P" actual-seconds))))


(defun %time (fn)
  (let* ((self (mezzano.supervisor:current-thread))
         (start-bytes-consed (mezzano.supervisor:thread-bytes-consed self))
         (start-time (get-universal-time))
         (start-run-time (mezzano.supervisor:thread-run-time self))
         (start-gc-time *gc-time*)
         (start-alloc-time (mezzano.supervisor:thread-allocation-time self))
         (start-pager (mezzano.supervisor:thread-run-time *pager-thread*))
         (start-cycle (tsc)))
    (multiple-value-prog1 (funcall fn)
      (let* ((finish-cycle (- (tsc) start-cycle))
             (finish-time (- (get-universal-time) start-time))
             (finish-pager (float (/ (- (mezzano.supervisor:thread-run-time *pager-thread*) start-pager)
                                     internal-time-units-per-second)))
             (finish-bytes-consed (- (mezzano.supervisor:thread-bytes-consed self) start-bytes-consed))
             (finish-gc-time (- *gc-time* start-gc-time))
             (finish-alloc-time (float (/ (- (mezzano.supervisor:thread-allocation-time self) start-alloc-time)
                                          internal-time-units-per-second)))
             (finish-run-time (float (/ (- (mezzano.supervisor:thread-run-time self) start-run-time)
                                        internal-time-units-per-second))))
        (fresh-line *trace-output*)
        (format *trace-output* "; Execution took ~:/mezzano.internals::format-time/.~%" finish-time)
        (format *trace-output* "; ~:D seconds of run time, ~:D seconds of GC time, ~:D allocation time.~%"
                finish-run-time finish-gc-time finish-alloc-time)
        (when (plusp finish-time)
          (format *trace-output* "; Pager ran for ~:D seconds.~%" finish-pager))
        (format *trace-output* "; Execution took ~:D cycles (~:D seconds).~%"
                finish-cycle (float (/ (mezzano.supervisor:high-precision-time-units-to-internal-time-units finish-cycle) internal-time-units-per-second)))
        (format *trace-output* "; ~:D bytes consed.~%" finish-bytes-consed)))))
