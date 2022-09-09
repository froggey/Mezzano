(in-package :mezzano.supervisor)

(sys.int::define-lap-function %cntfrq (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:mrs :x9 :cntfrq-el0)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %cntpct (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:mrs :x9 :cntpct-el0)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %cntp-tval (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:mrs :x9 :cntp-tval-el0)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function (setf %cntp-tval) ((value))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:msr :cntp-tval-el0 :x9)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %cntp-ctl (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:mrs :x9 :cntp-ctl-el0)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function (setf %cntp-ctl) ((value))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:msr :cntp-ctl-el0 :x9)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %cntvct (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:mrs :x9 :cntvct-el0)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %cntv-tval (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:mrs :x9 :cntv-tval-el0)
  (mezzano.lap.arm64:isb)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function (setf %cntv-tval) ((value))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:msr :cntv-tval-el0 :x9)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function %cntv-ctl (())
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:mrs :x9 :cntv-ctl-el0)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function (setf %cntv-ctl) ((value))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x9 :xzr :x0 :asr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:msr :cntv-ctl-el0 :x9)
  (mezzano.lap.arm64:ret))

(sys.int::defglobal *generic-timer-rate*)
(sys.int::defglobal *generic-timer-reset-value*)
(sys.int::defglobal *run-time-advance*)

(sys.int::defglobal *rtc-adjust*)

(defun generic-timer-irq-handler (interrupt-frame irq)
  (declare (ignore irq))
  (setf (%cntv-tval) *generic-timer-reset-value*)
  (beat-heartbeat *run-time-advance*)
  (profile-sample interrupt-frame)
  :completed)

(defun initialize-platform-time (fdt-node)
  (let* ((fdt-interrupt (fdt-get-property fdt-node "interrupts"))
         ;; FIXME: need to deal with PPI vs SPI. 16 is PPI offset.
         ;; Read the IRQ for the virtual timer
         (irq (+ 16 (fdt-read-u32 fdt-interrupt 7)))
         (timer-rate (%cntfrq))
         (tick-rate 100))
    (debug-print-line "Timer irq: " irq)
    (debug-print-line "Timer frequency: " timer-rate " Hz")
    (setf *generic-timer-rate* timer-rate)
    (setf *generic-timer-reset-value* (truncate timer-rate tick-rate))
    (setf *run-time-advance* (truncate internal-time-units-per-second tick-rate))
    (debug-print-line "Timer reset: " *generic-timer-reset-value*)
    (debug-print-line "Timer advance: " *run-time-advance*)
    (when (not (boundp '*rtc-adjust*))
      (setf *rtc-adjust* 0))
    (irq-attach (platform-irq irq)
                'generic-timer-irq-handler
                fdt-node
                :exclusive t)
    ;; Set countdown value.
    ;; ### why is this 0 and not *generic-timer-reset-value*?
    (setf (%cntv-tval) 0)
    (%isb)
    ;; Enable the timer.
    (setf (%cntv-ctl) 1)
    (%isb)))

(defun get-universal-time ()
  (+ *rtc-adjust* (truncate (%cntvct) *generic-timer-rate*)))

(defun sys.int::tsc ()
  ;; This isn't the cycle counter, but it's close enough for now.
  (prog1
      (%cntvct)
    (%isb)))

(defun get-high-precision-timer ()
  ;; TODO
  (prog1
      (%cntvct)
    (%isb)))

(defun high-precision-time-units-to-internal-time-units (hp-time)
  ;; TODO
  hp-time)
