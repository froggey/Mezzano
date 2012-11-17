(in-package #:sys.int)

(defvar *isa-pic-shadow-mask* #xFFFF)

(defun isa-pic-irq-mask (irq)
  (check-type irq (integer 0 16))
  (logtest (ash 1 irq) *isa-pic-shadow-mask*))

(defun (setf isa-pic-irq-mask) (value irq)
  (check-type irq (integer 0 16))
  (setf (ldb (byte 1 irq) *isa-pic-shadow-mask*)
        (if value 1 0))
  (if (< irq 8)
      ;; Master PIC.
      (setf (io-port/8 #x21) (ldb (byte 8 0) *isa-pic-shadow-mask*))
      ;; Slave PIC.
      (setf (io-port/8 #xA1) (ldb (byte 8 8) *isa-pic-shadow-mask*)))
  value)

(defvar *isa-pic-handlers* (make-array 16 :initial-element nil :area :static))
(defvar *isa-pic-base-handlers* (make-array 16 :initial-element nil))

(macrolet ((doit ()
             (let ((forms '(progn)))
               (dotimes (i 16)
                 (push `(irq-handler ,i) forms))
               (nreverse forms)))
           (irq-handler (n)
             (let ((sym (intern (format nil "%%IRQ~D-thunk" n))))
               `(progn
                  (define-lap-function ,sym ()
                    (sys.lap-x86:push :rbp)
                    (sys.lap-x86:mov64 :rbp :rsp)
                    (sys.lap-x86:push :rax)
                    (sys.lap-x86:push :rcx)
                    (sys.lap-x86:push :rdx)
                    (sys.lap-x86:push :rsi)
                    (sys.lap-x86:push :rdi)
                    (sys.lap-x86:mov64 :rax (:constant *isa-pic-handlers*))
                    (sys.lap-x86:mov64 :rax (:symbol-value :rax))
                    (sys.lap-x86:cmp64 (:rax ,(+ (- +tag-array-like+) 8 (* n 8))) nil)
                    (sys.lap-x86:je over)
                    (sys.lap-x86:call (:rax ,(+ (- +tag-array-like+) 8 (* n 8))))
                    over
                    (sys.lap-x86:mov8 :al #x20)
                    (sys.lap-x86:out8 #x20)
                    ,@(when (>= n 8)
                            `((sys.lap-x86:mov8 :al #x20)
                              (sys.lap-x86:out8 #xA0)))
                    (sys.lap-x86:pop :rdi)
                    (sys.lap-x86:pop :rsi)
                    (sys.lap-x86:pop :rdx)
                    (sys.lap-x86:pop :rcx)
                    (sys.lap-x86:pop :rax)
                    (sys.lap-x86:pop :rbp)
                    (sys.lap-x86:iret))
                  (setf (aref *isa-pic-base-handlers* ,n) #',sym)))))
  (doit))

(defun isa-pic-interrupt-handler (irq)
  (aref *isa-pic-handlers* irq))

(defun (setf isa-pic-interrupt-handler) (value irq)
  (check-type value (or null function))
  (setf (aref *isa-pic-handlers* irq) value))

(defconstant +isa-pic-interrupt-base+ #x30)

(defun set-idt-entry (entry &key (offset 0) (segment #x0008)
                      (present t) (dpl 0) (ist nil)
                      (interrupt-gate-p t))
  (check-type entry (unsigned-byte 8))
  (check-type offset (signed-byte 64))
  (check-type segment (unsigned-byte 16))
  (check-type dpl (unsigned-byte 2))
  (check-type ist (or null (unsigned-byte 3)))
  (let ((value 0))
    (setf (ldb (byte 16 48) value) (ldb (byte 16 16) offset)
          (ldb (byte 1 47) value) (if present 1 0)
          (ldb (byte 2 45) value) dpl
          (ldb (byte 4 40) value) (if interrupt-gate-p
                                      #b1110
                                      #b1111)
          (ldb (byte 3 16) value) (or ist 0)
          (ldb (byte 16 16) value) segment
          (ldb (byte 16 0) value) (ldb (byte 16 0) offset))
    (setf (aref *idt* (* entry 2)) value
          (aref *idt* (1+ (* entry 2))) (ldb (byte 32 32) offset))))

(defun init-isa-pic ()
  ;; Hook into the IDT.
  (dotimes (i 16)
    (set-idt-entry (+ +isa-pic-interrupt-base+ i)
                   :offset (lisp-object-address (aref *isa-pic-base-handlers* i))))
  ;; Initialize the ISA PIC.
  (setf (io-port/8 #x20) #x11
        (io-port/8 #xA0) #x11
        (io-port/8 #x21) +isa-pic-interrupt-base+
        (io-port/8 #xA1) (+ +isa-pic-interrupt-base+ 8)
        (io-port/8 #x21) #x04
        (io-port/8 #xA1) #x02
        (io-port/8 #x21) #x01
        (io-port/8 #xA1) #x01
        ;; Mask all IRQs except for the cascade IRQ (2).
        (io-port/8 #x21) #xFF
        (io-port/8 #xA1) #xFF
        *isa-pic-shadow-mask* #xFFFF
        (isa-pic-irq-mask 2) nil))

;;; Must be run each boot, but also do it really early here in case
;;; anything turns interrupts on during cold initialization.
#+nil(add-hook '*early-initialize-hook* 'init-isa-pic)
(init-isa-pic)

(defun ldb-exception (stack-frame)
  (mumble-string "In LDB.")
  (dotimes (i 32)
    (mumble-string " ")
    (mumble-hex (memref-unsigned-byte-64 stack-frame i)))
  (mumble-string ". Halted.")
  (loop (%hlt)))

(defvar *exception-base-handlers* (make-array 32 :initial-element nil))
(define-lap-function %%exception ()
  ;; RAX already pushed.
  (sys.lap-x86:push :rbx)
  (sys.lap-x86:push :rcx)
  (sys.lap-x86:push :rdx)
  (sys.lap-x86:push :rbp)
  (sys.lap-x86:push :rsi)
  (sys.lap-x86:push :rdi)
  (sys.lap-x86:push :r8)
  (sys.lap-x86:push :r9)
  (sys.lap-x86:push :r10)
  (sys.lap-x86:push :r11)
  (sys.lap-x86:push :r12)
  (sys.lap-x86:push :r13)
  (sys.lap-x86:push :r14)
  (sys.lap-x86:push :r15)
  (sys.lap-x86:mov64 :r8 :rsp)
  (sys.lap-x86:shl64 :r8 3)
  (sys.lap-x86:test64 :rsp #b1000)
  (sys.lap-x86:jz already-aligned)
  (sys.lap-x86:push 0)
  already-aligned
  (sys.lap-x86:mov32 :ecx 8)
  ;; FIXME: Should switch to a secondary data stack.
  (sys.lap-x86:mov64 :r13 (:constant ldb-exception))
  (sys.lap-x86:call (:symbol-function :r13))
  (sys.lap-x86:mov64 :rsp :r8)
  (sys.lap-x86:pop :r15)
  (sys.lap-x86:pop :r14)
  (sys.lap-x86:pop :r13)
  (sys.lap-x86:pop :r12)
  (sys.lap-x86:pop :r11)
  (sys.lap-x86:pop :r10)
  (sys.lap-x86:pop :r9)
  (sys.lap-x86:pop :r8)
  (sys.lap-x86:pop :rdi)
  (sys.lap-x86:pop :rsi)
  (sys.lap-x86:pop :rbp)
  (sys.lap-x86:pop :rdx)
  (sys.lap-x86:pop :rcx)
  (sys.lap-x86:pop :rbx)
  (sys.lap-x86:pop :rax)
  (sys.lap-x86:add64 :rsp 16)
  (sys.lap-x86:iret))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *exception-names*
    #("Divide-Error"
      "Debug"
      "NMI"
      "Breakpoint"
      "Overflow"
      "BOUND-Range-Exceeded"
      "Invalid-Opcode"
      "Device-Not-Available"
      "Double-Fault"
      "Coprocessor-Segment-Overrun"
      "Invalid-TSS"
      "Segment-Not-Present"
      "Stack-Segment-Fault"
      "General-Protection-Fault"
      "Page-Fault"
      "Exception-15"
      "Math-Fault"
      "Alignment-Check"
      "Machine-Check"
      "SIMD-Floating-Point-Exception"
      "Exception-20"
      "Exception-21"
      "Exception-22"
      "Exception-23"
      "Exception-24"
      "Exception-25"
      "Exception-26"
      "Exception-27"
      "Exception-28"
      "Exception-29"
      "Exception-30"
      "Exception-31")))

(macrolet ((doit ()
             (let ((forms '(progn)))
               (dotimes (i 32)
                 (push `(exception-handler ,i) forms))
               (nreverse forms)))
           (exception-handler (n)
             (let ((sym (intern (format nil "%%~A-thunk" (aref *exception-names* n)))))
               `(progn
                  (define-lap-function ,sym ()
                    ;; Some exceptions do not push an error code.
                    ,@(unless (member n '(8 10 11 12 13 14 17))
                              `((sys.lap-x86:push 0)))
                    (sys.lap-x86:push ,n)
                    (sys.lap-x86:push :rax)
                    (sys.lap-x86:mov64 :rax (:constant %%exception))
                    (sys.lap-x86:jmp (:symbol-function :rax)))
                  (setf (aref *exception-base-handlers* ,n) #',sym)
                  (set-idt-entry ,n :offset (lisp-object-address #',sym))))))
  (doit))
