(in-package :sys.int)

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

(defvar *isa-pic-handlers* (make-array 16 :initial-element nil))
(defvar *isa-pic-base-handlers* (make-array 16 :initial-element nil))

(defvar *isa-pic-interrupted-stack-group*)

(defun isa-pic-common (irq)
  (let ((handler (svref *isa-pic-handlers* irq))
        (*isa-pic-interrupted-stack-group* (current-stack-group)))
    (when handler
      (funcall handler)))
  (setf (io-port/8 #x20) #x20)
  (when (>= irq 8)
    (setf (io-port/8 #xA0) #x20)))

(macrolet ((doit ()
             (let ((forms '(progn)))
               (dotimes (i 16)
                 (push `(irq-handler ,i) forms))
               (nreverse forms)))
           (irq-handler (n)
             (let ((sym (intern (format nil "%%IRQ~D-ISR" n))))
               `(progn
                  (define-lap-function ,sym ()
                    (sys.lap-x86:push :rbp) ;  0  (0)
                    (sys.lap-x86:mov64 :rbp :rsp)
                    (:gc :frame :interrupt t)
                    ;; Save the current state.
                    (sys.lap-x86:push :rax) ; -8  (-1)
                    (sys.lap-x86:push :rcx) ; -16 (-2)
                    (sys.lap-x86:push :rdx) ; -24 (-3)
                    (sys.lap-x86:push :rsi) ; -32 (-4)
                    (sys.lap-x86:push :rdi) ; -40 (-5)
                    (sys.lap-x86:push :rbx) ; -48 (-6)
                    (sys.lap-x86:push :r13) ; -56 (-7)
                    (sys.lap-x86:push :r12) ; -64 (-8)
                    (sys.lap-x86:push :r11) ; -72 (-9)
                    (sys.lap-x86:push :r10) ; -80 (-10)
                    (sys.lap-x86:push :r9)  ; -88 (-11)
                    (sys.lap-x86:push :r8)  ; -96 (-12)
                    ;; Realign the stack.
                    (sys.lap-x86:and64 :rsp ,(lognot 15))
                    ;; Call the handler.
                    (sys.lap-x86:mov32 :r8d ,(ash n +n-fixnum-bits+))
                    (sys.lap-x86:mov32 :ecx ,(ash 1 +n-fixnum-bits+))
                    (sys.lap-x86:mov64 :r13 (:function isa-pic-common))
                    (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
                    (sys.lap-x86:xor32 :ecx :ecx)
                    (sys.lap-x86:mov64 :r13 (:function %maybe-preempt-from-interrupt-frame))
                    (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
                    (sys.lap-x86:cmp64 :r8 nil)
                    (sys.lap-x86:je no-preempt)
                    ;; Mark current stack-group as interrupted.
                    (sys.lap-x86:gs)
                    (sys.lap-x86:or64 (,(+ (- +tag-object+)
                                           (* (1+ +stack-group-offset-flags+) 8)))
                                      ,(ash +stack-group-interrupted+
                                            (+ +stack-group-state-position+
                                               +n-fixnum-bits+)))
                    (sys.lap-x86:mov32 :ecx ,(ash 1 +n-fixnum-bits+))
                    (sys.lap-x86:mov64 :r13 (:function %%switch-to-stack-group))
                    (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
                    no-preempt
                    (sys.lap-x86:mov64 :r8 (:rbp -96))
                    (sys.lap-x86:mov64 :r9 (:rbp -88))
                    (sys.lap-x86:mov64 :r10 (:rbp -80))
                    (sys.lap-x86:mov64 :r11 (:rbp -72))
                    (sys.lap-x86:mov64 :r12 (:rbp -64))
                    (sys.lap-x86:mov64 :r13 (:rbp -56))
                    (sys.lap-x86:mov64 :rbx (:rbp -48))
                    (sys.lap-x86:mov64 :rdi (:rbp -40))
                    (sys.lap-x86:mov64 :rsi (:rbp -32))
                    (sys.lap-x86:mov64 :rdx (:rbp -24))
                    (sys.lap-x86:mov64 :rcx (:rbp -16))
                    (sys.lap-x86:mov64 :rax (:rbp -8))
                    (sys.lap-x86:leave)
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
                   :offset (%array-like-ref-unsigned-byte-64 (aref *isa-pic-base-handlers* i) 0)))
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

(defun interrupt-frame-register-offset (register)
  (ecase register
    (:ss   5)
    (:rsp  4)
    (:rflags 3)
    (:cs   2)
    (:rip  1)
    (:rbp  0)
    (:rax -1)
    (:rcx -2)
    (:rdx -3)
    (:rbx -4)
    (:rsi -5)
    (:rdi -6)
    (:r8  -7)
    (:r9  -8)
    (:r10 -9)
    (:r11 -10)
    (:r12 -11)
    (:r13 -12)
    (:r14 -13)
    (:r15 -14)))

(defun interrupt-frame-pointer (frame)
  (%array-like-ref-t frame 0))

(defun interrupt-frame-raw-register (frame register)
  (memref-unsigned-byte-64 (interrupt-frame-pointer frame)
                            (interrupt-frame-register-offset register)))

(defun (setf interrupt-frame-raw-register) (value frame register)
  (setf (memref-unsigned-byte-64 (interrupt-frame-pointer frame)
                                  (interrupt-frame-register-offset register))
        value))

(defun interrupt-frame-value-register (frame register)
  (memref-t (interrupt-frame-pointer frame)
             (interrupt-frame-register-offset register)))

(defun (setf interrupt-frame-value-register) (value frame register)
  (setf (memref-t (interrupt-frame-pointer frame)
                   (interrupt-frame-register-offset register))
        value))

(defmacro define-interrupt-handler (name lambda-list &body body)
  `(progn (setf (get ',name 'interrupt-handler)
                (lambda ,lambda-list
                  (declare (system:lambda-name (interrupt-handler ,name)))
                  ,@body))
          ',name))

(defun make-interrupt-handler (name &rest arguments)
  (setf argument (copy-list-in-area arguments :static))
  (let* ((fn (get name 'interrupt-handler))
         (thunk (lambda () (apply fn arguments))))
    ;; Grovel inside the closure and move the environment
    ;; object to static space.
    (let ((the-lambda (function-pool-object thunk 0))
          (the-env (function-pool-object thunk 1)))
      (make-closure the-lambda
                    (make-array (length the-env)
                                :initial-contents the-env
                                :area :static)))))

(define-lap-function %%interrupt-break-thunk ()
  ;; Control will return to the interrupt code.
  ;; All registers can be smashed here, aside from the stack regs.
  (:gc :no-frame)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  ;; Align the control stack.
  (sys.lap-x86:and64 :rsp #.(lognot 15))
  ;; Call.
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:mov64 :r13 (:function break))
  (sys.lap-x86:call (:r13 #.(+ (- sys.int::+tag-object+) 8 (* sys.int::+fref-entry-point+ 8))))
  ;; Done.
  (sys.lap-x86:leave)
  (:gc :no-frame)
  (sys.lap-x86:ret))

(defun signal-break-from-interrupt (stack-group)
  "Configure STACK-GROUP so it will call BREAK when resumed."
  (when (and (stack-group-interruptable-p stack-group)
             ;; Only works with an interrupted stack layout. :(
             (eql (stack-group-state stack-group) :interrupted))
    ;; STACK-GROUP's RSP points to an interrupt frame.
    (let* ((rsp (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-control-stack-pointer+))
           (original-rsp rsp)
           (return-address (memref-unsigned-byte-64 rsp 15))
           (fn-address (base-address-of-internal-pointer return-address))
           (fn-offset (- return-address fn-address))
           (fn (%%assemble-value fn-address +tag-object+)))
      (multiple-value-bind (framep interruptp pushed-values pushed-values-register
                                   layout-address layout-length
                                   multiple-values incoming-arguments block-or-tagbody-thunk)
          (gc-info-for-function-offset fn fn-offset)
        ;; Don't try if there are active multiple-values.
        (when multiple-values
          (return-from signal-break-from-interrupt))
        ;; STACK-GROUP's control stack looks like:
        ;;  +0 RBP
        ;;  +8 RFlags (with IF cleared due to the interrupt)
        ;; +16 RIP (to the interrupt handler)
        ;; 16 byte alignment not guaranteed.
        ;; Rewrite it so it looks like:
        ;;  +0 RBP
        ;;  +8 RFlags (with IF set)
        ;; +16 break-thunk
        ;; +24 RIP
        ;; break-thunk will align the stack before invoking BREAK.
        (decf rsp 8)
        (setf (memref-unsigned-byte-64 rsp 0) (memref-unsigned-byte-64 original-rsp 0)) ; RBP
        (setf (memref-unsigned-byte-64 rsp 1) (logior (memref-unsigned-byte-64 original-rsp 1) #x200)) ; RFlags
        (setf (memref-t rsp 2) #'%%interrupt-break-thunk) ; thunk
        (setf (%array-like-ref-unsigned-byte-64 stack-group +stack-group-offset-control-stack-pointer+) rsp)))))
