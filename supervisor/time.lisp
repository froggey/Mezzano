;;;; Time management

(in-package :mezzano.internals)

(defglobal *rtc-is-utc* t "True if the RTC holds UTC, not local time.")
(defglobal sys.int::*time-zone* 0 "CL time zone.")

(defglobal *days-before-month*)

(in-package :mezzano.supervisor)

(defconstant internal-time-units-per-second 1000000)

(sys.int::defglobal *heartbeat-wait-queue*)
;; Total time the system has run for, internal time units. Not reset on boot.
(sys.int::defglobal *run-time*)
;; Value of *RUN-TIME* when the system was last booted.
(sys.int::defglobal *run-time-at-boot*)
;; Universal time when the system was last booted.
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
              (coerce (nreverse reversed-result) 'simple-vector)))
    (setf *active-timers* (make-timer-queue)))
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
  ;; Walk the timer list, waking timers that have expired.
  (with-place-spinlock ((timer-queue-lock *active-timers*))
    (loop
       (when (or (timer-list-empty-p *active-timers*)
                 (> (timer-%deadline (timer-queue-head *active-timers*)) *run-time*))
         (return))
       (let ((timer (timer-list-pop-front *active-timers*)))
         (when (timer-cvar timer)
           (condition-notify (timer-cvar timer) t))
         (setf (event-state (timer-event timer)) t))))
  (decay-lights run-time-advance))

(defun wait-for-heartbeat-unsleep-helper (arg)
  (declare (ignore arg))
  nil)

(defun wait-for-heartbeat ()
  (ensure-interrupts-enabled)
  (%call-on-wired-stack-without-interrupts
   (lambda (sp fp)
     (let ((self (current-thread)))
       (lock-wait-queue *heartbeat-wait-queue*)
       (push-wait-queue self *heartbeat-wait-queue*)
       (acquire-global-thread-lock)
       (unlock-wait-queue *heartbeat-wait-queue*)
       (setf (thread-wait-item self) *heartbeat-wait-queue*
             (thread-state self) :sleeping
             (thread-unsleep-helper self) #'wait-for-heartbeat-unsleep-helper)
       (%reschedule-via-wired-stack sp fp)))
   nil))

(defun get-internal-real-time ()
  (+ (- *run-time* *run-time-at-boot*)
     (* *boot-time* internal-time-units-per-second)))

(defun get-internal-run-time ()
  *run-time*)

(defun safe-sleep (seconds)
  "Sleep in a supervisor-safe way, no allocations."
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

(defstruct (timer-queue
             (:area :wired))
  head
  tail
  (lock (place-spinlock-initializer)))

(defstruct (timer
             (:constructor %make-timer)
             (:area :wired))
  name
  (next :unlinked)
  (prev :unlinked)
  %deadline
  event
  cvar)

(sys.int::defglobal *active-timers*)

(define-doubly-linked-list-helpers timer-list
    timer-next timer-prev
    timer-queue-head timer-queue-tail)

(defun dump-active-timers ()
  (debug-print-line "Active timers: (current time is " *run-time* ")")
  (do-timer-list (timer *active-timers*)
    (with-page-fault-hook
        (()
         (debug-print-line "<truncated>")
         (abandon-page-fault))
      (debug-print-line "  " timer "/" (timer-name timer) " @ " (timer-%deadline timer)))))

(defun make-timer (&key name relative deadline)
  (when (and relative deadline)
    (error "Relative and deadline time specified"))
  (let ((timer (%make-timer :name name)))
    (setf (timer-event timer) (make-event :name timer))
    (when relative
      (timer-arm relative timer))
    (when deadline
      (timer-arm-absolute deadline timer))
    timer))

(defun timer-arm (seconds timer)
  "Arm TIMER to go off in approximately SECONDS seconds from now.
SECONDS must be a REAL.
This is a convienence function wrapping TIMER-ARM-ABSOLUTE."
  (timer-arm-absolute (truncate
                             (+ (* seconds internal-time-units-per-second)
                                *run-time*))
                      timer))

(defun timer-disarm-1 (timer)
  (when (timer-%deadline timer)
    (setf (event-state (timer-event timer)) nil)
    (when (timer-list-linked-p timer)
      (timer-list-remove timer *active-timers*))
    (setf (timer-%deadline timer) nil)))

(defun timer-arm-absolute (internal-run-time timer)
  "Configure TIMER to go off at INTERNAL-RUN-TIME.
This is an absolute time specified in integer internal time units.
The current internal run time can be fetched with GET-INTERNAL-RUN-TIME."
  (check-type internal-run-time integer)
  (check-type timer timer)
  (safe-without-interrupts (internal-run-time timer)
    (with-place-spinlock ((timer-queue-lock *active-timers*))
      (timer-disarm-1 timer)
      (setf (timer-%deadline timer) internal-run-time)
      (cond ((> internal-run-time *run-time*)
             ;; Timer has not yet expired.
             ;; Insert it into the list at the appropriate time.
             (do-timer-list (other *active-timers*
                                   (timer-list-push-back timer *active-timers*))
               (when (<= internal-run-time (timer-%deadline other))
                 (timer-list-insert-before timer other *active-timers*)
                 (return))))
            (t
             ;; Timer has already expired.
             (setf (event-state (timer-event timer)) t)))))
  (values))

(defun convert-deadline-to-remaining-time (deadline)
  (/ (min 0 (- deadline (get-internal-run-time)))
     internal-time-units-per-second))

(defun timer-disarm-absolute (timer)
  "Reset TIMER back to its a disarmed state.
Returns the TIMER's old deadline if was armed or NIL if it was disarmed."
  (check-type timer timer)
  ;; Disarm the timer and return the previous deadline.
  (safe-without-interrupts (timer)
    (with-place-spinlock ((timer-queue-lock *active-timers*))
      (prog1 (timer-%deadline timer)
        (timer-disarm-1 timer)))))

(defun timer-disarm (timer)
  "Reset TIMER back to its a disarmed state.
Returns the number of seconds remaining if was armed or NIL if it was disarmed.
This function may cons."
  (let ((deadline (timer-disarm-absolute timer)))
    (when deadline
      (convert-deadline-to-remaining-time deadline))))

(defun timer-remaining (timer)
  "Returns the number of seconds remaining if TIMER is armed or NIL if it is disarmed."
  (let ((deadline (timer-deadline timer)))
    (when deadline
      (convert-deadline-to-remaining-time deadline))))

(defun timer-deadline (timer)
  "Returns TIMER's internal run time deadline if is armed or NIL if it is disarmed."
  (timer-%deadline timer))

(defun timer-wait (timer)
  "Wait until TIMER's deadline has passed.
Will wait forever if TIMER has not been armed."
  (event-wait (timer-event timer)))

(defun timer-expired-p (timer)
  (event-state (timer-event timer)))

(sys.int::defglobal *timer-pool-lock* (make-mutex "Timer pool"))
(sys.int::defglobal *timer-pool* nil)
(sys.int::defglobal *timer-pool-size* 0)
(sys.int::defglobal *timer-pool-limit* 1000)
(sys.int::defglobal *timer-pool-hit-count* 0)
(sys.int::defglobal *timer-pool-miss-count* 0)

(defun push-timer-pool (timer)
  (timer-disarm-absolute timer) ; don't cons
  (setf (timer-name timer) 'pooled-timer)
  (with-mutex (*timer-pool-lock*)
    (when (< *timer-pool-size* *timer-pool-limit*)
      (setf (timer-next timer) *timer-pool*
            *timer-pool* timer)
      (incf *timer-pool-size*))))

(defun pop-timer-pool ()
  (flet ((alloc ()
           (with-mutex (*timer-pool-lock*)
             (let ((timer *timer-pool*))
               (cond (timer
                      (setf *timer-pool* (timer-next timer)
                            (timer-next timer) :unlinked)
                      (incf *timer-pool-hit-count*)
                      (decf *timer-pool-size*)
                      timer)
                     (t
                      (incf *timer-pool-miss-count*)
                      nil))))))
    (or (alloc)
        (make-timer :name 'pooled-timer))))

(defmacro with-timer ((timer &key relative absolute name) &body body)
  "Allocate & arm a timer from the timer pool."
  (let ((timer-actual (gensym "TIMER")))
    `(let ((,timer-actual (pop-timer-pool)))
       (setf (timer-name ,timer-actual) ,(or name `',timer))
       (unwind-protect
            (progn
              ,(when relative
                 `(timer-arm ,relative ,timer-actual))
              ,(when absolute
                 `(timer-arm-absolute ,absolute ,timer-actual))
              (let ((,timer ,timer-actual)) ,@body))
         (push-timer-pool ,timer-actual)))))

(defun timer-sleep (timer seconds)
  "Like SLEEP, but uses a pre-allocated timer."
  (check-type seconds (real 0))
  (unwind-protect
       (progn
         (timer-arm seconds timer)
         (timer-wait timer))
    (timer-disarm-absolute timer)) ; don't cons
  nil)

(defun sleep (seconds)
  (check-type seconds (real 0))
  (with-timer (sleep :relative seconds)
    (timer-wait sleep))
  nil)
