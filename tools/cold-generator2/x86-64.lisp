;;;; x86-64 target support

(defpackage :mezzano.cold-generator.x86-64
  (:use :cl)
  (:import-from #:mezzano.cold-generator
                #:configure-system-for-target)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:lap #:mezzano.lap.x86)
                    (#:sys.int #:mezzano.internals)))

(in-package :mezzano.cold-generator.x86-64)

(defparameter *funcallable-instance-trampoline*
  `((:gc :no-frame :layout #*0 :incoming-arguments :rcx)
    ;; Load the real function from the funcallable-instance.
    (lap:mov64 :rbx (:object :rbx ,sys.int::+funcallable-instance-function+))
    ;; Invoke the real function via the FUNCTION calling convention.
    (lap:jmp (:object :rbx ,sys.int::+function-entry-point+)))
  "Trampoline used for calling a closure or funcallable-instance via a funcallable-instance.")

;; Handlers for the defined CPU exceptions, a bool indicating if they take
;; an error code or not.
(defparameter *cpu-exception-info*
  '((sys.int::%divide-error-handler nil)           ; 0
    (sys.int::%debug-exception-handler nil)        ; 1
    (sys.int::%nonmaskable-interrupt-handler nil)  ; 2
    (sys.int::%breakpoint-handler nil)             ; 3
    (sys.int::%overflow-handler nil)               ; 4
    (sys.int::%bound-exception-handler nil)        ; 5
    (sys.int::%invalid-opcode-handler nil)         ; 6
    (sys.int::%device-not-available-handler nil)   ; 7
    (sys.int::%double-fault-handler t)             ; 8
    nil                                            ; 9
    (sys.int::%invalid-tss-handler t)              ; 10
    (sys.int::%segment-not-present-handler t)      ; 11
    (sys.int::%stack-segment-fault-handler t)      ; 12
    (sys.int::%general-protection-fault-handler t) ; 13
    (sys.int::%page-fault-handler t)               ; 14
    nil                                            ; 15
    (sys.int::%math-fault-handler nil)             ; 16
    (sys.int::%alignment-check-handler t)          ; 17
    (sys.int::%machine-check-handler nil)          ; 18
    (sys.int::%simd-exception-handler nil)         ; 19
    nil                                            ; 20
    nil                                            ; 21
    nil                                            ; 22
    nil                                            ; 23
    nil                                            ; 24
    nil                                            ; 25
    nil                                            ; 26
    nil                                            ; 27
    nil                                            ; 28
    nil                                            ; 29
    nil                                            ; 30
    nil))                                          ; 31

(defparameter *common-interrupt-code*
  `(;; Common code for all interrupts.
    ;; There's an interrupt frame set up, a per-interrupt value in r9, and an fref for the high-level handler in r13.
    ;; Generate a DX interrupt frame object, then call the handler with it.
    ;; Realign the stack.
    (lap:and64 :rsp ,(lognot 15))
    (lap:sub64 :rsp 16) ; 2 elements.
    (lap:mov64 (:rsp) ,(ash sys.int::+object-tag-interrupt-frame+ sys.int::+object-type-shift+)) ; header
    (lap:lea64 :rax (:rbp :rbp)) ; Convert frame pointer to fixnum.
    (lap:mov64 (:rsp 8) :rax) ; 2nd element.
    (lap:lea64 :r8 (:rsp ,sys.int::+tag-object+))
    (lap:mov32 :ecx ,(ash 2 sys.int::+n-fixnum-bits+)) ; 2 args.
    (:gc :frame :interrupt t)
    ;; TODO: Turn this into a direct named call.
    (lap:lea64 :rax (:object :r13 ,sys.int::+fref-code+))
    (lap:call :rax)
    ;; Restore registers, then return.
    (lap:mov64 :r15 (:rbp -112))
    (lap:mov64 :r14 (:rbp -104))
    (lap:mov64 :r13 (:rbp -96))
    (lap:mov64 :r12 (:rbp -88))
    (lap:mov64 :r11 (:rbp -80))
    (lap:mov64 :r10 (:rbp -72))
    (lap:mov64 :r9 (:rbp -64))
    (lap:mov64 :r8 (:rbp -56))
    (lap:mov64 :rdi (:rbp -48))
    (lap:mov64 :rsi (:rbp -40))
    (lap:mov64 :rbx (:rbp -32))
    (lap:mov64 :rdx (:rbp -24))
    (lap:mov64 :rcx (:rbp -16))
    (lap:mov64 :rax (:rbp -8))
    (lap:leave)
    (lap:iret)))

(defparameter *common-user-interrupt-code*
  `(;; Common code for interrupts 32+.
    ;; There's an incomplete interrupt frame set up and an interrupt number in rax (fixnum).
    ;; Save registers to fill in the rest of the interrupt frame.
    (lap:push :rcx) ; -16 (-2)
    (lap:push :rdx) ; -24 (-3)
    (lap:push :rbx) ; -32 (-4)
    (lap:push :rsi) ; -40 (-5)
    (lap:push :rdi) ; -48 (-6)
    (lap:push :r8)  ; -56 (-7)
    (lap:push :r9)  ; -64 (-8)
    (lap:push :r10) ; -72 (-9)
    (lap:push :r11) ; -80 (-10)
    (lap:push :r12) ; -88 (-11)
    (lap:push :r13) ; -96 (-12)
    (lap:push :r14) ; -104 (-13)
    (lap:push :r15) ; -112 (-14)
    ;; Fall into the common interrupt code.
    (lap:mov64 :r9 :rax)
    (lap:mov64 :r13 (:function sys.int::%user-interrupt-handler))
    (lap:jmp interrupt-common)))

(defun create-exception-isr (handler error-code-p)
  (append
   (if error-code-p
       ;; Create interrupt frame and pull error code off the stack.
       ;; There aren't any free registers yet, so we have to shuffle things around.
       ;; Don't use xchg, because that has an implicit lock (slow).
       '((lap:push :rax)
         (lap:mov64 :rax (:rsp 8))  ; load error code into rax, freeing up the fp location
         (lap:mov64 (:rsp 8) :rbp)  ; really create the stack frame.
         (lap:lea64 :rbp (:rsp 8))) ; rsp is slightly offset because of the push rax.
       ;; Create interrupt frame. No error code, so no shuffling needed.
       '((lap:push :rbp)
         (lap:mov64 :rbp :rsp)
         (lap:push :rax)))
   ;; Frame looks like:
   ;; +40 SS
   ;; +32 RSP
   ;; +24 RFlags
   ;; +16 CS
   ;; +8  RIP
   ;; +0  RBP
   ;; -8  RAX
   ;; Save registers to fill in the rest of the interrupt frame.
   ;; RAX holds the saved error code (if any).
   `((lap:push :rcx) ; -16 (-2)
     (lap:push :rdx) ; -24 (-3)
     (lap:push :rbx) ; -32 (-4)
     (lap:push :rsi) ; -40 (-5)
     (lap:push :rdi) ; -48 (-6)
     (lap:push :r8)  ; -56 (-7)
     (lap:push :r9)  ; -64 (-8)
     (lap:push :r10) ; -72 (-9)
     (lap:push :r11) ; -80 (-10)
     (lap:push :r12) ; -88 (-11)
     (lap:push :r13) ; -96 (-12)
     (lap:push :r14) ; -104 (-13)
     (lap:push :r15) ; -112 (-14)
     ;; Jump to the common exception code.
     (lap:mov64 :r13 (:function ,handler)))
   (if error-code-p
       '((lap:lea64 :r9 (:rax :rax))) ; Convert error code to fixnum.
       '((lap:mov32 :r9d nil))) ; Nothing interesting
   '((lap:jmp interrupt-common))))

(defun create-user-interrupt-isr (index)
  ;; Create interrupt frame. No error code, so no shuffling needed.
  `((lap:push :rbp)
    (lap:mov64 :rbp :rsp)
    ;; Frame looks like:
    ;; +40 SS
    ;; +32 RSP
    ;; +24 RFlags
    ;; +16 CS
    ;; +8  RIP
    ;; +0  RBP
    ;; Save RAX, then jump to common code with the interrupt number.
    (lap:push :rax) ; -8 (-1)
    (lap:mov32 :eax ,(ash index sys.int::+n-fixnum-bits+))
    (lap:jmp user-interrupt-common)))

(defun create-low-level-interrupt-support (environment)
  ;; For maximum flexibility, create ISRs for all the IDT entries.
  (let* ((idt-size 256)
         (isr-table (env:make-array environment 256 :initial-element nil :area :wired))
         (syms (loop
                  for i below idt-size
                  collect (make-symbol (format nil "ISR-~D" i))))
         (isr-code
          (append
           (loop
              for sym in syms
              for (handler error-code-p) in *cpu-exception-info*
              when handler
              append (list* '(:align 16)
                            sym
                            (create-exception-isr handler error-code-p)))
           (loop
              for i from 32 below idt-size
              for sym in (nthcdr 32 syms)
              append (list* '(:align 16)
                            sym
                            (create-user-interrupt-isr i)))
           (list '(:align 16)
                 'user-interrupt-common)
           *common-user-interrupt-code*
           (list '(:align 16)
                 'interrupt-common)
           *common-interrupt-code*)))
    (multiple-value-bind (fn symbols)
        (env:compile-lap environment isr-code
                         :area :wired-function
                         :name (env:translate-symbol environment 'sys.int::%%interrupt-service-routines))
      (loop
         for (name . value) in symbols
         for pos = (position name syms)
         when pos
         do (setf (aref isr-table pos) (- value 16))) ; symbols are from the start of the function, not from the entry point.
      (env:%defun environment (env:translate-symbol environment 'sys.int::%%interrupt-service-routines) fn)
      (setf (env:cross-symbol-value environment 'sys.int::*interrupt-service-routines*) isr-table)
      (values))))

(defmethod configure-system-for-target (environment (target (eql :x86-64)))
  (setf (env:cross-symbol-value environment 'sys.int::*funcallable-instance-trampoline*)
        (env:compile-lap environment
                         *funcallable-instance-trampoline*
                         :area :wired-function
                         :name (env:translate-symbol environment 'sys.int::%%funcallable-instance-trampoline%%)))
  (create-low-level-interrupt-support environment))
