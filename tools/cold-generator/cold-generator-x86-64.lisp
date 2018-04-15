;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :cold-generator.x86-64)

(defparameter *undefined-function-thunk*
  `((:gc :no-frame :layout #*0 :incoming-arguments :rcx)
    ;; Call helper using the function calling convention, leaving fref intact.
    (sys.lap-x86:mov64 :rbx (:function sys.int::raise-undefined-function))
    (sys.lap-x86:mov64 :rbx (:object :rbx ,sys.int::+fref-function+))
    (sys.lap-x86:jmp (:object :rbx ,sys.int::+function-entry-point+)))
  "Code for the undefined function thunk.")

(defparameter *closure-trampoline*
  `((:gc :no-frame :layout #*0 :incoming-arguments :rcx)
    ;; Load the real function from the fref.
    (sys.lap-x86:mov64 :rbx (:object :r13 ,sys.int::+fref-function+))
    ;; Invoke the real function via the FUNCTION calling convention.
    ;; This will work even if the fref was altered or made funbound.
    ;; (SETF FUNCTION-REFERENCE-FUNCTION) will set the function to the
    ;; undefined-function thunk in that case, so everything works fine.
    (sys.lap-x86:jmp (:object :rbx ,sys.int::+function-entry-point+)))
  "Trampoline used for calling a closure or funcallable-instance via an fref.")

(defparameter *funcallable-instance-trampoline*
  `((:gc :no-frame :layout #*0 :incoming-arguments :rcx)
    ;; Load the real function from the funcallable-instance.
    (sys.lap-x86:mov64 :rbx (:object :rbx ,sys.int::+funcallable-instance-function+))
    ;; Invoke the real function via the FUNCTION calling convention.
    (sys.lap-x86:jmp (:object :rbx ,sys.int::+function-entry-point+)))
  "Trampoline used for calling a closure or funcallable-instance via a funcallable-instance.")

;; Handlers for the defined CPU exceptions, a bool indicating if they take
;; an error code or not and the IST to use.
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
    (sys.int::%page-fault-handler t 1)             ; 14
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
    (sys.lap-x86:and64 :rsp ,(lognot 15))
    (sys.lap-x86:sub64 :rsp 16) ; 2 elements.
    (sys.lap-x86:mov64 (:rsp) ,(ash sys.int::+object-tag-interrupt-frame+ sys.int::+object-type-shift+)) ; header
    (sys.lap-x86:lea64 :rax (:rbp :rbp)) ; Convert frame pointer to fixnum.
    (sys.lap-x86:mov64 (:rsp 8) :rax) ; 2nd element.
    (sys.lap-x86:lea64 :r8 (:rsp ,sys.int::+tag-object+))
    (sys.lap-x86:mov32 :ecx ,(ash 2 sys.int::+n-fixnum-bits+)) ; 2 args.
    (:gc :frame :interrupt t)
    (sys.lap-x86:call (:object :r13 ,sys.int::+fref-entry-point+))
    ;; Restore registers, then return.
    (sys.lap-x86:mov64 :r15 (:rbp -112))
    (sys.lap-x86:mov64 :r14 (:rbp -104))
    (sys.lap-x86:mov64 :r13 (:rbp -96))
    (sys.lap-x86:mov64 :r12 (:rbp -88))
    (sys.lap-x86:mov64 :r11 (:rbp -80))
    (sys.lap-x86:mov64 :r10 (:rbp -72))
    (sys.lap-x86:mov64 :r9 (:rbp -64))
    (sys.lap-x86:mov64 :r8 (:rbp -56))
    (sys.lap-x86:mov64 :rdi (:rbp -48))
    (sys.lap-x86:mov64 :rsi (:rbp -40))
    (sys.lap-x86:mov64 :rbx (:rbp -32))
    (sys.lap-x86:mov64 :rdx (:rbp -24))
    (sys.lap-x86:mov64 :rcx (:rbp -16))
    (sys.lap-x86:mov64 :rax (:rbp -8))
    (sys.lap-x86:leave)
    (sys.lap-x86:iret)))

(defparameter *common-user-interrupt-code*
  `(;; Common code for interrupts 32+.
    ;; There's an incomplete interrupt frame set up and an interrupt number in rax (fixnum).
    ;; Save registers to fill in the rest of the interrupt frame.
    (sys.lap-x86:push :rcx) ; -16 (-2)
    (sys.lap-x86:push :rdx) ; -24 (-3)
    (sys.lap-x86:push :rbx) ; -32 (-4)
    (sys.lap-x86:push :rsi) ; -40 (-5)
    (sys.lap-x86:push :rdi) ; -48 (-6)
    (sys.lap-x86:push :r8)  ; -56 (-7)
    (sys.lap-x86:push :r9)  ; -64 (-8)
    (sys.lap-x86:push :r10) ; -72 (-9)
    (sys.lap-x86:push :r11) ; -80 (-10)
    (sys.lap-x86:push :r12) ; -88 (-11)
    (sys.lap-x86:push :r13) ; -96 (-12)
    (sys.lap-x86:push :r14) ; -104 (-13)
    (sys.lap-x86:push :r15) ; -112 (-14)
    ;; Fall into the common interrupt code.
    (sys.lap-x86:mov64 :r9 :rax)
    (sys.lap-x86:mov64 :r13 (:function sys.int::%user-interrupt-handler))
    (sys.lap-x86:jmp interrupt-common)))

(defun create-exception-isr (handler error-code-p)
  (append
   (if error-code-p
       ;; Create interrupt frame and pull error code off the stack.
       ;; There aren't any free registers yet, so we have to shuffle things around.
       ;; Don't use xchg, because that has an implicit lock (slow).
       '((sys.lap-x86:push :rax)
         (sys.lap-x86:mov64 :rax (:rsp 8))  ; load error code into rax, freeing up the fp location
         (sys.lap-x86:mov64 (:rsp 8) :rbp)  ; really create the stack frame.
         (sys.lap-x86:lea64 :rbp (:rsp 8))) ; rsp is slightly offset because of the push rax.
       ;; Create interrupt frame. No error code, so no shuffling needed.
       '((sys.lap-x86:push :rbp)
         (sys.lap-x86:mov64 :rbp :rsp)
         (sys.lap-x86:push :rax)))
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
   `((sys.lap-x86:push :rcx) ; -16 (-2)
     (sys.lap-x86:push :rdx) ; -24 (-3)
     (sys.lap-x86:push :rbx) ; -32 (-4)
     (sys.lap-x86:push :rsi) ; -40 (-5)
     (sys.lap-x86:push :rdi) ; -48 (-6)
     (sys.lap-x86:push :r8)  ; -56 (-7)
     (sys.lap-x86:push :r9)  ; -64 (-8)
     (sys.lap-x86:push :r10) ; -72 (-9)
     (sys.lap-x86:push :r11) ; -80 (-10)
     (sys.lap-x86:push :r12) ; -88 (-11)
     (sys.lap-x86:push :r13) ; -96 (-12)
     (sys.lap-x86:push :r14) ; -104 (-13)
     (sys.lap-x86:push :r15) ; -112 (-14)
     ;; Jump to the common exception code.
     (sys.lap-x86:mov64 :r13 (:function ,handler)))
   (if error-code-p
       '((sys.lap-x86:lea64 :r9 (:rax :rax))) ; Convert error code to fixnum.
       '((sys.lap-x86:mov32 :r9d nil))) ; Nothing interesting
   '((sys.lap-x86:jmp interrupt-common))))

(defun create-user-interrupt-isr (index)
  ;; Create interrupt frame. No error code, so no shuffling needed.
  `((sys.lap-x86:push :rbp)
    (sys.lap-x86:mov64 :rbp :rsp)
    ;; Frame looks like:
    ;; +40 SS
    ;; +32 RSP
    ;; +24 RFlags
    ;; +16 CS
    ;; +8  RIP
    ;; +0  RBP
    ;; Save RAX, then jump to common code with the interrupt number.
    (sys.lap-x86:push :rax) ; -8 (-1)
    (sys.lap-x86:mov32 :eax ,(ash index sys.int::+n-fixnum-bits+))
    (sys.lap-x86:jmp user-interrupt-common)))

(defun create-low-level-interrupt-support ()
  "Generate the ISR thunks that call into Lisp."
  ;; For maximum flexibility, create ISRs for all the IDT entries.
  (let* ((idt-size 256)
         (isr-table (allocate (1+ idt-size) :wired)))
    ;; Create the ISR table.
    (setf (word isr-table) (array-header sys.int::+object-tag-array-t+ idt-size))
    (setf (cold-symbol-value 'sys.int::*interrupt-service-routines*) (make-value isr-table sys.int::+tag-object+))
    ;; Generate the ISR thunks.
    (let* ((exception-isrs (loop
                              for (handler error-code-p ist) in *cpu-exception-info*
                              collect
                                (when handler
                                  (create-exception-isr handler error-code-p))))
           (user-interrupt-isrs (loop
                                   for i from 32 below idt-size
                                   collect (create-user-interrupt-isr i)))
           (common-code (compile-lap-function *common-interrupt-code*
                                              :area :wired
                                              :position-independent nil
                                              :name 'sys.int::%%common-isr-thunk))
           (common-user-code (compile-lap-function *common-user-interrupt-code*
                                                   :area :wired
                                                   :position-independent nil
                                                   :extra-symbols (list (cons 'interrupt-common (+ (* common-code 8) 16)))
                                                   :name 'sys.int::%%common-user-isr-thunk)))
      (let ((fref (function-reference 'sys.int::%%common-isr-thunk)))
        (setf (word (+ fref 1 sys.int::+fref-function+)) (make-value common-code sys.int::+tag-object+)
              (word (+ fref 1 sys.int::+fref-entry-point+)) (+ (* common-code 8) 16)))
      (let ((fref (function-reference 'sys.int::%%common-user-isr-thunk)))
        (setf (word (+ fref 1 sys.int::+fref-function+)) (make-value common-user-code sys.int::+tag-object+)
              (word (+ fref 1 sys.int::+fref-entry-point+)) (+ (* common-user-code 8) 16)))
      (loop
         for isr in (append exception-isrs user-interrupt-isrs)
         for i from 0
         when isr do
           ;; Assemble the ISR and update the ISR entry.
           (let ((addr (compile-lap-function isr
                                             :area :wired
                                             :position-independent nil
                                             :extra-symbols (list (cons 'interrupt-common (+ (* common-code 8) 16))
                                                                  (cons 'user-interrupt-common (+ (* common-user-code 8) 16)))
                                             :name (intern (format nil "%%ISR-THUNK-~D" i) "SYS.INT"))))
             (setf (word (+ isr-table 1 i)) (make-value addr sys.int::+tag-object+)))
         else do
           (setf (word (+ isr-table 1 i)) (make-value (symbol-address "NIL" "COMMON-LISP") sys.int::+tag-object+))))))
