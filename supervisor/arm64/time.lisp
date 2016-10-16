;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(sys.int::define-lap-function %cntfrq (())
  (mezzano.lap.arm64:mrs :x9 :cntfrq-el0)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %cntpct (())
  (mezzano.lap.arm64:mrs :x9 :cntpct-el0)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %cntp-tval (())
  (mezzano.lap.arm64:mrs :x9 :cntp-tval-el0)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function (setf %cntp-tval) ((value))
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:msr :cntp-tval-el0 :x9)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %cntp-ctl (())
  (mezzano.lap.arm64:mrs :x9 :cntp-ctl-el0)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function (setf %cntp-ctl) ((value))
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:msr :cntp-ctl-el0 :x9)
  (mezzano.lap.arm64:ret))

(sys.int::defglobal *generic-timer-rate*)
(sys.int::defglobal *generic-timer-reset-value*)
(sys.int::defglobal *run-time-advance*)

(sys.int::defglobal *rtc-adjust*)

(defun generic-timer-irq-handler (interrupt-frame irq)
  (declare (ignore irq))
  (setf (%cntp-tval) *generic-timer-reset-value*)
  (beat-heartbeat *run-time-advance*)
  (profile-sample interrupt-frame))

(defun initialize-platform-time (irq)
  (let ((timer-rate (%cntfrq))
        (tick-rate 100))
    (debug-print-line "Timer frequency: " timer-rate " Hz")
    (setf *generic-timer-rate* timer-rate)
    (setf *generic-timer-reset-value* (truncate timer-rate tick-rate))
    (setf *run-time-advance* (truncate internal-time-units-per-second tick-rate))
    (debug-print-line "Timer reset: " *generic-timer-reset-value*)
    (debug-print-line "Timer advance: " *run-time-advance*)
    (platform-attach-irq irq 'generic-timer-irq-handler)
    (platform-unmask-irq irq)
    (when (not (boundp '*rtc-adjust*))
      (setf *rtc-adjust* 0))
    ;; Set countdown value.
    (setf (%cntp-tval) 0)
    ;; Enable the timer.
    (setf (%cntp-ctl) 1)))

(defun read-rtc-time ()
  (let ((seconds (+ *rtc-adjust* (truncate (%cntpct) *generic-timer-rate*))))
    (decode-universal-time seconds)))

(defun sys.int::tsc ()
  ;; This isn't the cycle counter, but it's close enough for now.
  (%cntpct))
