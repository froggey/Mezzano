;;;; x86-64 float support functions

(in-package :mezzano.runtime)

(sys.int::define-lap-function sys.int::%%make-double-float-rax ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push 0)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:mov64 :rcx #.(ash 4 sys.int::+n-fixnum-bits+)) ; fixnum 4
  ;; Tag.
  (sys.lap-x86:mov64 :r8 #.(ash sys.int::+object-tag-double-float+
                                sys.int::+n-fixnum-bits+))
  ;; Header data.
  (sys.lap-x86:xor64 :r9 :r9)
  ;; Words.
  (sys.lap-x86:mov64 :r10 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Area
  (sys.lap-x86:mov64 :r11 nil)
  ;; Allocate object.
  (sys.lap-x86:call (:named-call %allocate-object))
  ;; Set data.
  (sys.lap-x86:pop (:object :r8 0))
  ;; Single-value return.
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))

(sys.int::define-lap-function %mxcsr (())
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:sub64 :rsp 8)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:stmxcsr (:rsp))
  (sys.lap-x86:mov32 :eax (:rsp))
  (sys.lap-x86:lea64 :r8 (:rax :rax)) ; fixnumize
  ;; Single-value return.
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:add64 :rsp 8)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))

(sys.int::define-lap-function (setf %mxcsr) ((value))
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:sub64 :rsp 8)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 (:rsp) :r8)
  (sys.lap-x86:sar64 (:rsp) #.sys.int::+n-fixnum-bits+) ; unfixnum
  (sys.lap-x86:ldmxcsr (:rsp))
  ;; Return whatever.
  (sys.lap-x86:add64 :rsp 8)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))

(defconstant +mxcsr-rounding-mode+ (byte 2 13))
(defconstant +mxcsr-rounding-mode-nearest+ #b00)
(defconstant +mxcsr-rounding-mode-negative-infinity+ #b01)
(defconstant +mxcsr-rounding-mode-positive-infinity+ #b10)
(defconstant +mxcsr-rounding-mode-zero+ #b11)
(defconstant +mxcsr-flush-to-zero+ 15)
(defconstant +mxcsr-precision-mask+ 12)
(defconstant +mxcsr-underflow-mask+ 11)
(defconstant +mxcsr-overflow-mask+ 10)
(defconstant +mxcsr-divide-by-zero-mask+ 9)
(defconstant +mxcsr-denormal-operand-mask+ 8)
(defconstant +mxcsr-invalid-operation-mask+ 7)
(defconstant +mxcsr-denormals-are-zero+ 6)
(defconstant +mxcsr-precision-flag+ 5)
(defconstant +mxcsr-underflow-flag+ 4)
(defconstant +mxcsr-overflow-flag+ 3)
(defconstant +mxcsr-divide-by-zero-flag+ 2)
(defconstant +mxcsr-denormal-operand-flag+ 1)
(defconstant +mxcsr-invalid-operation-flag+ 0)

(defun decode-mxcsr-rounding-mode (mode)
  (ecase mode
    (#.+mxcsr-rounding-mode-nearest+ :nearest)
    (#.+mxcsr-rounding-mode-negative-infinity+ :negative-infinity)
    (#.+mxcsr-rounding-mode-positive-infinity+ :positive-infinity)
    (#.+mxcsr-rounding-mode-zero+ :zero)))

(defun encode-mxcsr-rounding-mode (mode)
  (ecase mode
    (:nearest +mxcsr-rounding-mode-nearest+)
    (:negative-infinity +mxcsr-rounding-mode-negative-infinity+)
    (:positive-infinity +mxcsr-rounding-mode-positive-infinity+)
    (:zero +mxcsr-rounding-mode-zero+)))

;; TODO: Figure out the operation and operands.
(defun mezzano.runtime::%raise-simd-exception (arg)
  (declare (ignore arg))
  (let ((mxcsr (%mxcsr)))
    (unwind-protect
         (flet ((test (mask-bit flag-bit exception-name)
                  (when (and (not (logbitp mask-bit mxcsr))
                             (logbitp flag-bit mxcsr))
                    ;; Clear flag bit. Actual MXCSR will be updated
                    ;; by the UNWIND-PROTECT.
                    (setf mxcsr (logandc2 mxcsr (ash 1 flag-bit)))
                    ;; And signal exception.
                    (error exception-name))))
           ;; For the sake of sanity, mask all exceptions before trying to
           ;; do anything else. The hash-table code gets particularly unhappy
           ;; with unmasked exceptions.
           (setf (%mxcsr) (logior mxcsr
                                  (ash 1 +mxcsr-invalid-operation-mask+)
                                  (ash 1 +mxcsr-denormal-operand-mask+)
                                  (ash 1 +mxcsr-overflow-mask+)
                                  (ash 1 +mxcsr-underflow-mask+)
                                  (ash 1 +mxcsr-divide-by-zero-mask+)
                                  (ash 1 +mxcsr-precision-mask+)))
           (test +mxcsr-invalid-operation-mask+ +mxcsr-invalid-operation-flag+
                 'floating-point-invalid-operation)
           (test +mxcsr-denormal-operand-mask+ +mxcsr-denormal-operand-flag+
                 'mezzano.extensions:floating-point-denormal-operand)
           (test +mxcsr-overflow-mask+ +mxcsr-overflow-flag+
                 'floating-point-overflow)
           (test +mxcsr-underflow-mask+ +mxcsr-underflow-flag+
                 'floating-point-underflow)
           (test +mxcsr-divide-by-zero-mask+ +mxcsr-divide-by-zero-flag+
                 'division-by-zero)
           (test +mxcsr-precision-mask+ +mxcsr-precision-flag+
                 'floating-point-inexact)
           (error "Unknown #XM exception. MXCSR: ~8,'0X" mxcsr))
      ;; Restore the old exception mask after we fiddled with it.
      (setf (%mxcsr) mxcsr))))

(defun get-fpu-mode ()
  (let ((mxcsr (%mxcsr)))
    (list :rounding (decode-mxcsr-rounding-mode (ldb +mxcsr-rounding-mode+ mxcsr))
          :flush-to-zero (logbitp +mxcsr-flush-to-zero+ mxcsr)
          :denormals-are-zero (logbitp +mxcsr-denormals-are-zero+ mxcsr)
          :precision (logbitp +mxcsr-precision-mask+ mxcsr)
          :underflow (logbitp +mxcsr-underflow-mask+ mxcsr)
          :overflow (logbitp +mxcsr-overflow-mask+ mxcsr)
          :divide-by-zero (logbitp +mxcsr-divide-by-zero-mask+ mxcsr)
          :denormal-operand (logbitp +mxcsr-denormal-operand-mask+ mxcsr)
          :invalid-operation (logbitp +mxcsr-invalid-operation-mask+ mxcsr))))

(defun set-fpu-mode (&key
                       (rounding :preserve)
                       (flush-to-zero :preserve)
                       (denormals-are-zero :preserve)
                       (precision :preserve)
                       (underflow :preserve)
                       (overflow :preserve)
                       (divide-by-zero :preserve)
                       (denormal-operand :preserve)
                       (invalid-operation :preserve))
  (let ((mxcsr (%mxcsr)))
    (unless (eql rounding :preserve)
      (setf (ldb +mxcsr-rounding-mode+ mxcsr) (encode-mxcsr-rounding-mode rounding)))
    (flet ((dobit (value mask-bit flag-bit)
             (unless (eql value :preserve)
               (cond (value
                      (setf mxcsr (logior mxcsr (ash 1 mask-bit))))
                     (t
                      ;; If the exception is being unmasked, then clear
                      ;; the matching flag bit so the handler has some idea
                      ;; of what exception occured.
                      (when flag-bit
                        (setf mxcsr (logandc2 mxcsr (ash 1 flag-bit))))
                      (setf mxcsr (logandc2 mxcsr (ash 1 mask-bit))))))))
      (dobit flush-to-zero +mxcsr-flush-to-zero+ nil)
      (dobit denormals-are-zero +mxcsr-denormals-are-zero+ nil)
      (dobit precision +mxcsr-precision-mask+ +mxcsr-precision-flag+)
      (dobit underflow +mxcsr-underflow-mask+ +mxcsr-underflow-flag+)
      (dobit overflow +mxcsr-overflow-mask+ +mxcsr-overflow-flag+)
      (dobit divide-by-zero +mxcsr-divide-by-zero-mask+ +mxcsr-divide-by-zero-flag+)
      (dobit denormal-operand +mxcsr-denormal-operand-mask+ +mxcsr-denormal-operand-flag+)
      (dobit invalid-operation +mxcsr-invalid-operation-mask+ +mxcsr-invalid-operation-flag+))
    (setf (%mxcsr) mxcsr)))
