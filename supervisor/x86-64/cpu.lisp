;;;; Copyright (c) 2011-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant +msr-ia32-apic-base+ #x0000001B)
(defconstant +msr-ia32-efer+      #xC0000080)
(defconstant +msr-ia32-fs-base+   #xC0000100)
(defconstant +msr-ia32-gs-base+   #xC0000101)

(sys.int::defglobal *n-up-cpus*)
(sys.int::defglobal *cpus*)
(sys.int::defglobal *bsp-cpu*)

(sys.int::defglobal *lapic-address*)

(defconstant +lapic-reg-id+ #x02)
(defconstant +lapic-reg-version+ #x03)
(defconstant +lapic-reg-task-priority+ #x08)
(defconstant +lapic-reg-arbitration-priority+ #x09)
(defconstant +lapic-reg-processor-priority+ #x0A)
(defconstant +lapic-reg-eoi+ #x0B)
(defconstant +lapic-reg-logical-destination+ #x0D)
(defconstant +lapic-reg-destination-format+ #x0E)
(defconstant +lapic-reg-spurious-interrupt-vector+ #x0F)
(defconstant +lapic-reg-in-service-0+ #x10)
(defconstant +lapic-reg-trigger-mode-0+ #x18)
(defconstant +lapic-reg-interrupt-request-0+ #x20)
(defconstant +lapic-reg-error-status+ #x28)
(defconstant +lapic-reg-interrupt-command-low+ #x30)
(defconstant +lapic-reg-interrupt-command-high+ #x31)
(defconstant +lapic-reg-lvt-timer+ #x32)
(defconstant +lapic-reg-lvt-termal-sensor+ #x33)
(defconstant +lapic-reg-lvt-performance-monitoring-counters+ #x34)
(defconstant +lapic-reg-lvt-lint0+ #x35)
(defconstant +lapic-reg-lvt-lint1+ #x36)
(defconstant +lapic-reg-lvt-error+ #x37)
(defconstant +lapic-reg-timer-initial-count+ #x38)
(defconstant +lapic-reg-timer-current-count+ #x39)
(defconstant +lapic-reg-timer-divide-configuration+ #x3E)

(defconstant +ipi-type-fixed+ 0)
(defconstant +ipi-type-lowest-priority+ 1)
(defconstant +ipi-type-smi+ 2)
(defconstant +ipi-type-nmi+ 4)
(defconstant +ipi-type-init+ 5)
(defconstant +ipi-type-sipi+ 6)

;; Cold generator provided objects.
(sys.int::defglobal sys.int::*interrupt-service-routines*)

(sys.int::defglobal sys.int::*bsp-wired-stack-base*)
(sys.int::defglobal sys.int::*bsp-wired-stack-size*)
(sys.int::defglobal sys.int::*bsp-info-vector*)
(sys.int::defglobal sys.int::*exception-stack-base*)
(sys.int::defglobal sys.int::*exception-stack-size*)
(sys.int::defglobal sys.int::*irq-stack-base*)
(sys.int::defglobal sys.int::*irq-stack-size*)

(defconstant +cpu-info-self-offset+ 0)
(defconstant +cpu-info-wired-stack-offset+ 1)
(defconstant +cpu-info-idle-thread-offset+ 2)
(defconstant +cpu-info-cpu-object-offset+ 3)
(defconstant +cpu-info-gdt-offset+ 15)
(defconstant +cpu-info-tss-offset+ 31)
(defconstant +cpu-info-tss-size+ 104)
(defconstant +cpu-info-idt-offset+ 511)

(defconstant +tss-ist-1+ 36)
(defconstant +tss-ist-2+ 44)
(defconstant +tss-ist-3+ 52)
(defconstant +tss-ist-4+ 60)
(defconstant +tss-ist-5+ 68)
(defconstant +tss-ist-6+ 76)
(defconstant +tss-ist-7+ 84)

(defconstant +tss-io-map-base+ 102)

(defstruct (cpu
             (:area :wired))
  state
  info-vector
  apic-id
  idle-thread
  wired-stack
  exception-stack
  irq-stack)

(defconstant +ap-trampoline-physical-address+ #x7000
  "Where the AP trampoline should be copied to in physical memory.
The bootloader is loaded to #x7C00, so #x7000 should be safe.")

(defconstant +wakeup-ipi-vector+ #x80
  "This interrupt is sent to idle CPUs when a thread becomes runnable.")

(defconstant +panic-ipi-vector+ #x81
  "Broadcast to all CPUs when a panic occurs.")

(defconstant +quiesce-ipi-vector+ #x82
  "Sent to CPUs to bring them to a quiescent state.")

(defconstant +tlb-shootdown-ipi-vector+ #x83
  "Sent to CPUs to prepare them for TLB shootdown.")

(defun make-idt-entry (&key (offset 0) (segment #x0008)
                         (present t) (dpl 0) (ist nil)
                         (interrupt-gate-p t))
  "Returns the low and high words of the IDT entry."
  ;; ###: Need to be more careful avoiding bignums.
  (let ((value 0))
    ;; Don't do this, can create bignums!
    ;;(setf (ldb (byte 16 48) value) (ldb (byte 16 16) offset)
    (setf value (ash (ldb (byte 16 16) offset) 48)
          (ldb (byte 1 47) value) (if present 1 0)
          (ldb (byte 2 45) value) dpl
          (ldb (byte 4 40) value) (if interrupt-gate-p
                                      #b1110
                                      #b1111)
          (ldb (byte 3 32) value) (or ist 0)
          (ldb (byte 16 16) value) segment
          (ldb (byte 16 0) value) (ldb (byte 16 0) offset))
    (values value (ldb (byte 32 32) offset))))

(defun lapic-reg (register)
  (physical-memref-unsigned-byte-32 (+ *lapic-address* (ash register 4))))

(defun (setf lapic-reg) (value register)
  (setf (physical-memref-unsigned-byte-32 (+ *lapic-address* (ash register 4))) value))

(defun lapic-eoi ()
  "Issue an EOI to the Local APIC."
  (setf (lapic-reg +lapic-reg-eoi+) 0))

(defun send-ipi (target type vector)
  (setf (lapic-reg +lapic-reg-interrupt-command-high+) (ash target 24))
  ;; Send: No shorthand, edge triggered, assert, physical dest.
  (setf (lapic-reg +lapic-reg-interrupt-command-low+) (logior #x4000
                                                              (ash type 8)
                                                              vector)))

(defun broadcast-ipi (type vector &optional including-self)
  ;; Disable interrupts to prevent cross-cpu migration from
  ;; fouling up behaviour of INCLUDING-SELF.
  (safe-without-interrupts (type vector including-self)
    (dolist (cpu *cpus*)
      (when (and (eql (cpu-state cpu) :online)
                 (or including-self
                     (not (eql cpu (local-cpu-object)))))
        (send-ipi (cpu-apic-id cpu) type vector)))))

(defun broadcast-wakeup-ipi ()
  (broadcast-ipi +ipi-type-fixed+ +wakeup-ipi-vector+))

(defun wakeup-ipi-handler (interrupt-frame info)
  (declare (ignore info))
  (lapic-eoi)
  (maybe-preempt-via-interrupt interrupt-frame))

(defun broadcast-panic-ipi ()
  (broadcast-ipi +ipi-type-fixed+ +panic-ipi-vector+))

(defun panic-ipi-handler (interrupt-frame info)
  (declare (ignore interrupt-frame info))
  (lapic-eoi)
  (loop (%arch-panic-stop)))

(sys.int::defglobal *non-quiescent-cpus-remaining*)

;; FIXME: quiesce-cpus-for-world-stop and begin-tlb-shootdown both need to
;; prevent migration across CPUs.
(defun quiesce-cpus-for-world-stop ()
  "Bring all CPUs to a consistent state to stop the world.
Protected by the world stop lock."
  (setf *non-quiescent-cpus-remaining* (1- *n-up-cpus*))
  (broadcast-ipi +ipi-type-fixed+ +quiesce-ipi-vector+)
  (loop
     (when (eql *non-quiescent-cpus-remaining* 0)
       (return))
     (sys.int::cpu-relax)))

;; Save the current thread's state and switch to the CPU's idle thread.
(defun quiesce-ipi-handler (interrupt-frame info)
  (declare (ignore info))
  (lapic-eoi)
  (let* ((current (current-thread))
         (idle (local-cpu-idle-thread))
         (was-active (not (eql current idle))))
    (when was-active
      (acquire-global-thread-lock)
      ;; Return this thread to the run queue.
      (setf (thread-state current) :runnable)
      (push-run-queue current)
      ;; Save thread state.
      (save-fpu-state current)
      (save-interrupted-state current interrupt-frame)
      ;; Partially switch to the idle thread.
      (setf (thread-state idle) :active)
      (setf (sys.int::msr +msr-ia32-gs-base+) (sys.int::lisp-object-address idle)))
    ;; Have now reached a quiescent state.
    (sys.int::%atomic-fixnum-add-symbol '*non-quiescent-cpus-remaining*
                                        -1)
    (when was-active
      ;; Finally, return to the idle thread.
      (%%switch-to-thread-common idle
                                 idle))))

(sys.int::defglobal *tlb-shootdown-in-progress* nil)
(sys.int::defglobal *busy-tlb-shootdown-cpus*)

;; TODO: This unconditionally invalidates the entire TLB.
;; Should be more fine-grained.

(defun begin-tlb-shootdown ()
  "Bring all CPUs to state ready for TLB shootdown.
TLB shootdown must be protected by the VM lock."
  (ensure (not *tlb-shootdown-in-progress*) "TLB shootdown already in progress!")
  (setf *tlb-shootdown-in-progress* t)
  (setf *busy-tlb-shootdown-cpus* (1- *n-up-cpus*))
  (broadcast-ipi +ipi-type-fixed+ +tlb-shootdown-ipi-vector+)
  ;; Wait for other CPUs to reach the handler.
  (loop
     (when (eql *busy-tlb-shootdown-cpus* 0)
       (return))
     (sys.int::cpu-relax)))

(defun tlb-shootdown-single (address)
  (declare (ignore address))
  (ensure *tlb-shootdown-in-progress*))

(defun tlb-shootdown-range (base length)
  (declare (ignore base length))
  (ensure *tlb-shootdown-in-progress*))

(defun tlb-shootdown-all ()
  (ensure *tlb-shootdown-in-progress*))

(defun finish-tlb-shootdown ()
  (ensure *tlb-shootdown-in-progress*)
  (setf *busy-tlb-shootdown-cpus* (1- *n-up-cpus*))
  (setf *tlb-shootdown-in-progress* nil)
  ;; Wait for CPUs to leave the handler.
  (loop
     (when (eql *busy-tlb-shootdown-cpus* 0)
       (return))
     (sys.int::cpu-relax)))

(defun tlb-shootdown-ipi-handler (interrupt-frame info)
  (declare (ignore interrupt-frame info))
  (lapic-eoi)
  (sys.int::%atomic-fixnum-add-symbol '*busy-tlb-shootdown-cpus*
                                      -1)
  (loop
     (when (not *tlb-shootdown-in-progress*)
       (return))
     (sys.int::cpu-relax))
  (flush-tlb)
  (sys.int::%atomic-fixnum-add-symbol '*busy-tlb-shootdown-cpus*
                                      -1))

(sys.int::define-lap-function local-cpu-info (())
  "Return the address of the local CPU's info vector."
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:fs)
  (sys.lap-x86:mov64 :r8 (:object nil #.+cpu-info-self-offset+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function local-cpu-idle-thread (())
  "Return the idle thread associated with the local CPU."
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:fs)
  (sys.lap-x86:mov64 :r8 (:object nil #.+cpu-info-idle-thread-offset+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function local-cpu-object (())
  "Return the CPU struct associated with the local CPU."
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:fs)
  (sys.lap-x86:mov64 :r8 (:object nil #.+cpu-info-cpu-object-offset+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function %lgdt ((length address))
  "Load a new GDT."
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:sub64 :rsp 16)
  (:gc :no-frame :layout #*000)
  (sys.lap-x86:mov64 :rax :r8) ; length
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 (:rsp) :rax)
  (sys.lap-x86:mov64 :rax :r9) ; poiner
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 (:rsp 2) :rax)
  (sys.lap-x86:lgdt (:rsp))
  (sys.lap-x86:add64 :rsp 16)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))

(sys.int::define-lap-function %lidt ((length address))
  "Load a new IDT."
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:sub64 :rsp 16)
  (:gc :no-frame :layout #*000)
  (sys.lap-x86:mov64 :rax :r8) ; length
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 (:rsp) :rax)
  (sys.lap-x86:mov64 :rax :r9) ; poiner
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 (:rsp 2) :rax)
  (sys.lap-x86:lidt (:rsp))
  (sys.lap-x86:add64 :rsp 16)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))

(sys.int::define-lap-function %ltr ((selector))
  "Load the task register."
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rax :r8) ; selector
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:ltr :ax)
  (sys.lap-x86:ret))

(sys.int::define-lap-function %load-cs ((selector))
  "Load CS."
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rax :r8) ; selector
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:lea64 :rax (:rip next))
  (sys.lap-x86:push :rax)
  (sys.lap-x86:retf)
  next
  (sys.lap-x86:ret))

(sys.int::define-lap-function %wbinvd (())
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:wbinvd)
  (sys.lap-x86:ret))

(defun populate-idt (vector)
  ;; IDT completely fills the second page (256 * 16)
  (dotimes (i 256)
    (multiple-value-bind (lo hi)
        (if (svref sys.int::*interrupt-service-routines* i)
            (make-idt-entry :offset (sys.int::%object-ref-signed-byte-64
                                     (svref sys.int::*interrupt-service-routines* i)
                                     sys.int::+function-entry-point+)
                            ;; Take CPU interrupts on the exception stack
                            ;; and IRQs on the interrupt stack.
                            :ist (if (< i 32)
                                     1
                                     2))
            (values 0 0))
      (setf (sys.int::%object-ref-unsigned-byte-64 vector (+ +cpu-info-idt-offset+ (* i 2))) lo
            (sys.int::%object-ref-unsigned-byte-64 vector (+ +cpu-info-idt-offset+ (* i 2) 1)) hi))))

(defun populate-gdt (vector tss-base)
  ;; GDT.
  (setf (sys.int::%object-ref-unsigned-byte-64 vector (+ +cpu-info-gdt-offset+ 0)) 0 ; NULL seg.
        (sys.int::%object-ref-unsigned-byte-64 vector (+ +cpu-info-gdt-offset+ 1)) #x00209A0000000000 ; Kernel CS64
        ;; TSS low.
        ;; Does not fit in a fixnum when treated as a 64-bit value, depending on where the info page
        ;; was allocated. Use 32-bit accesses to work around.
        (sys.int::%object-ref-unsigned-byte-32 vector (+ (* (+ +cpu-info-gdt-offset+ 2) 2) 0)) (logior (ldb (byte 16 0) +cpu-info-tss-size+)
                                                                                                       (ash (ldb (byte 16 0) tss-base) 16))
        (sys.int::%object-ref-unsigned-byte-32 vector (+ (* (+ +cpu-info-gdt-offset+ 2) 2) 1)) (logior (ldb (byte 8 16) tss-base)
                                                                                                       (ash #x89 8)
                                                                                                       (ash (ldb (byte 4 16) +cpu-info-tss-size+) 16)
                                                                                                       (ash (ldb (byte 8 24) tss-base) 24))
        ;; TSS high.
        (sys.int::%object-ref-unsigned-byte-64 vector (+ +cpu-info-gdt-offset+ 3)) (ldb (byte 32 32) tss-base)))

(defun populate-tss (tss-base exception-stack-pointer irq-stack-pointer)
  ;; TSS, Clear memory first.
  (dotimes (i +cpu-info-tss-size+)
    (setf (sys.int::memref-unsigned-byte-16 tss-base i) 0))
  ;; IST1.
  (setf (sys.int::memref-signed-byte-64 (+ tss-base +tss-ist-1+) 0) exception-stack-pointer)
  ;; IST2.
  (setf (sys.int::memref-signed-byte-64 (+ tss-base +tss-ist-2+) 0) irq-stack-pointer)
  ;; I/O Map Base Address, follows TSS body.
  (setf (sys.int::memref-unsigned-byte-16 (+ tss-base +tss-io-map-base+) 0) +cpu-info-tss-size+))

(defun populate-cpu-info-vector (vector wired-stack-pointer exception-stack-pointer irq-stack-pointer idle-thread)
  (let* ((addr (- (sys.int::lisp-object-address vector)
                  sys.int::+tag-object+))
         (tss-base (+ addr 8 (* +cpu-info-tss-offset+ 8))))
    (populate-idt vector)
    (populate-gdt vector tss-base)
    (populate-tss tss-base exception-stack-pointer irq-stack-pointer)
    ;; Other stuff.
    (setf (sys.int::%object-ref-t vector +cpu-info-self-offset+) vector)
    (setf (sys.int::%object-ref-signed-byte-64 vector +cpu-info-wired-stack-offset+)
          wired-stack-pointer)
    (setf (sys.int::%object-ref-t vector +cpu-info-idle-thread-offset+) idle-thread)))

(defun initialize-boot-cpu ()
  "Generate GDT, IDT and TSS for the boot CPU."
  (when (not (boundp '*cpus*))
    ;; For panics early in the first boot.
    (setf *cpus* '()))
  (setf *tlb-shootdown-in-progress* nil)
  (populate-cpu-info-vector sys.int::*bsp-info-vector*
                            (+ sys.int::*bsp-wired-stack-base* sys.int::*bsp-wired-stack-size*)
                            (+ sys.int::*exception-stack-base* sys.int::*exception-stack-size*)
                            (+ sys.int::*irq-stack-base* sys.int::*irq-stack-size*)
                            sys.int::*bsp-idle-thread*)
  ;; Load various bits.
  (setf (sys.int::msr +msr-ia32-fs-base+)
        (sys.int::lisp-object-address sys.int::*bsp-info-vector*))
  (load-cpu-bits sys.int::*bsp-info-vector*))

(sys.int::defglobal *initial-pml4*)

(defconstant +ap-bootstrap-ap-entry-point-offset+ #x200)
(defconstant +ap-bootstrap-initial-pml4-offset+ #x208)
(defconstant +ap-bootstrap-real-pml4-offset+ #x210)
(defconstant +ap-bootstrap-cpu-vector-offset+ #x218)

;; This function gets copied to #x7000 physical and is used to boot the APs.
;; It needs the FREF for %%AP-ENTRY-POINT at base + #x200, the initial PML4 at +#x208,
;; the true PML4 at +#x210, and the CPU info pointer at +#x218.
(sys.int::define-lap-function %%ap-bootstrap ()
  ;; APs start in real mode, with RIP = 0, CS = bootstrap-vector,
  ;; and all other selectors = 0.
  ;; This code is position-independent.
  ;; Watch out - The assembler doesn't encode 16-bit effective addresses
  ;; properly. Liberal use of address-size-override and 32-bit addressing
  ;; modes are required in 16-bit code.
  (sys.lap-x86:!code16)
  (sys.lap-x86:wbinvd)
  ;; Unify segments.
  ;; All memory access must be relative to CS until in 64-bit mode.
  (sys.lap-x86:movseg :bx :cs)
  (sys.lap-x86:movseg :ds :bx)
  (sys.lap-x86:movseg :ss :bx)
  ;; Load the temporary stack pointer.
  (sys.lap-x86:mov16 :sp temporary-stack-top)
  ;; Convert segment to linear address.
  (sys.lap-x86:movzx16 :ebx :bx)
  (sys.lap-x86:shl32 :ebx 4)
  ;; Update the GDTR pointer.
  (sys.lap-x86:lea32 :eax (:ebx temporary-gdt))
  (sys.lap-x86:address-size-override)
  (sys.lap-x86:mov32 (gdtr-pointer) :eax)
  ;; Load the temporary GDT, and a null IDT.
  (sys.lap-x86:address-size-override)
  (sys.lap-x86:lgdt (gdtr))
  (sys.lap-x86:address-size-override)
  (sys.lap-x86:lidt (idtr))
  ;; Enable CR0.PE, protected mode.
  (sys.lap-x86:movcr :edx :cr0)
  (sys.lap-x86:or32 :edx 1)
  (sys.lap-x86:movcr :cr0 :edx)
  ;; Push the 32-bit CS segment.
  ;; Need to push 32 bits on the stack.
  ;; The assembler has no way to represent a 32-bit immediate push in 16-bit mode,
  ;; so use two 16-bit pushes.
  (sys.lap-x86:push 0)
  (sys.lap-x86:push #x10)
  ;; Push the linear address of PROT-MODE.
  (sys.lap-x86:lea32 :eax (:ebx prot-mode))
  ;; 32-bit override to push eax.
  (sys.lap-x86:operand-size-override)
  (sys.lap-x86:push :ax)
  ;; 32-bit far return to jump to the 32-bit code.
  (sys.lap-x86:operand-size-override)
  (sys.lap-x86:retf)
  prot-mode
  ;; Code here is running in 32-bit protected mode with flat segments.
  ;; EBX contains the base address of this code, and data accesses
  ;; should be relative to it.
  (sys.lap-x86:!code32)
  ;; Load the 32-bit data segment.
  (sys.lap-x86:mov32 :eax #x18)
  (sys.lap-x86:movseg :ds :ax)
  (sys.lap-x86:movseg :es :ax)
  (sys.lap-x86:movseg :ss :ax)
  ;; Load the temporary stack pointer again.
  ;; This time it's a 32-bit linear address relative to 0, not relative to the load base.
  (sys.lap-x86:lea32 :esp (:ebx temporary-stack-top))
  ;; Enable PAE, PGE, and SSE.
  (sys.lap-x86:movcr :eax :cr4)
  (sys.lap-x86:or32 :eax #.(logior (ash 1 5) ; PAE
                                   (ash 1 7) ; PGE
                                   (ash 1 9) ; OSFXSR
                                   (ash 1 10))) ; OSXMMEXCPT
  (sys.lap-x86:movcr :cr4 :eax)
  ;; Load the initial PML4.
  (sys.lap-x86:mov32 :eax (:ebx #.+ap-bootstrap-initial-pml4-offset+))
  (sys.lap-x86:movcr :cr3 :eax)
  ;; Set EFER.LME, long mode enable.
  (sys.lap-x86:mov32 :ecx #.+msr-ia32-efer+) ; EFER
  (sys.lap-x86:rdmsr)
  (sys.lap-x86:or32 :eax #.(ash 1 8)) ; LME
  (sys.lap-x86:wrmsr)
  ;; Enable CR0.PG to activate compatibility/long mode.
  ;; Enable a bunch of other stuff as well.
  (sys.lap-x86:movcr :eax :cr0)
  (sys.lap-x86:or32 :eax #.(logior (ash 1 31) ; PG
                                   (ash 1 1) ; MP
                                   (ash 1 5) ; NE
                                   (ash 1 16))) ; WP
  ;; Clear CD/NW. VirtualBox, KVM, and Bochs start CPUs with them set.
  (sys.lap-x86:and32 :eax #.(lognot #x60000000))
  (sys.lap-x86:movcr :cr0 :eax)
  ;; Clear EFLAGS.
  (sys.lap-x86:push 0)
  (sys.lap-x86:popf)
  ;; Push the temporary 64-bit code segment.
  (sys.lap-x86:push #x08)
  ;; And the address of LONG-MODE-64.
  (sys.lap-x86:lea32 :eax (:ebx long-mode-64))
  (sys.lap-x86:push :eax)
  ;; Return to 64-bit long mode.
  (sys.lap-x86:retf)
  long-mode-64
  (sys.lap-x86:!code64)
  ;; Load the null segment into the data segments.
  (sys.lap-x86:xor32 :eax :eax)
  (sys.lap-x86:movseg :ds :ax)
  (sys.lap-x86:movseg :es :ax)
  (sys.lap-x86:movseg :fs :ax)
  (sys.lap-x86:movseg :gs :ax)
  (sys.lap-x86:movseg :ss :ax)
  ;; Load FS.base with the CPU info.
  (sys.lap-x86:mov64 :rax (:rbx #.+ap-bootstrap-cpu-vector-offset+))
  (sys.lap-x86:mov64 :rdx :rax)
  (sys.lap-x86:shr64 :rdx 32)
  (sys.lap-x86:mov32 :ecx #.+msr-ia32-fs-base+)
  (sys.lap-x86:wrmsr)
  ;; This code is currently running identity-mapped, and
  ;; needs to move over to the real PML4.
  ;; The initial PML4 has an identity mapping for this page
  ;; and a mapping for this page in the physical map, which
  ;; is shared with the real PML4.
  ;; Jump there, then switch to the real PML4.
  ;; Update RBX so it points into the pmap.
  (sys.lap-x86:mov64 :rax #.+physical-map-base+)
  (sys.lap-x86:mov32 :ebx :ebx) ; Clear high bits of RBX, just in case.
  (sys.lap-x86:add64 :rbx :rax) ; RBX now relative to the pmap.
  ;; Get the address of JUMP-TO-PMAP in the physical map.
  (sys.lap-x86:lea64 :rax (:rbx jump-to-pmap))
  ;; Jump.
  (sys.lap-x86:jmp :rax)
  jump-to-pmap
  ;; Now safe to switch.
  (sys.lap-x86:mov64 :rax (:rbx #.+ap-bootstrap-real-pml4-offset+))
  (sys.lap-x86:movcr :cr3 :rax)
  ;; Load RSP with the wired stack.
  (sys.lap-x86:fs)
  (sys.lap-x86:mov64 :rsp (:object nil #.+cpu-info-wired-stack-offset+))
  ;; And finally, call %%AP-ENTRY-POINT.
  (sys.lap-x86:mov64 :rax (:rbx #.+ap-bootstrap-ap-entry-point-offset+))
  ;; Fake return address
  (sys.lap-x86:push 0)
  ;; Clear FP.
  (sys.lap-x86:xor32 :ebp :ebp)
  ;; No arguments
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:jmp (:object :rax #.sys.int::+fref-entry-point+))
  (:align 16)
  gdtr
  (:d16/le (- temporary-gdt-end temporary-gdt 1)) ; Length
  gdtr-pointer
  (:d32/le 0) ; Pointer
  idtr
  (:d16/le 0)
  (:d32/le 0)
  (:align 16)
  temporary-gdt
  (:d64/le 0)
  (:d64/le #x00209A0000000000)
  (:d64/le #x00CF9A000000FFFF)
  (:d64/le #x00CF92000000FFFF)
  temporary-gdt-end
  (:align 16)
  temporary-stack
  (:d32/le 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
  temporary-stack-top)

;; This code is called by %%AP-BOOTSTRAP after it has booted the AP and
;; done minimal configuration on it.
;; The state it gets:
;; 64-bit CS, exact value undefined.
;; DS/ES/FS/GS/SS loaded with 0.
;; FS.base set to the CPU info vector.
;; RSP set to the CPU's wired stack.
;; Other GPRs, x87, SSE, etc state undefined.
;; CR0 configured to enable native FPU exception handling.
;; CR4 configured to enable SSE.
(defun %%ap-entry-point ()
  ;; CPU vector has been configured for us, just load the required bits.
  (load-cpu-bits (local-cpu-info))
  (lapic-setup)
  ;; Signal that this CPU has booted successfully.
  (let ((old (sys.int::cas (cpu-state (local-cpu-object)) :offline :online)))
    (when (not (eql old :offline))
      ;; The system decided that this CPU failed to come up for some reason.
      (loop
         (%hlt))))
  (incf *n-up-cpus*)
  ;; Perform the initial thread switch to the idle thread.
  (let ((idle-thread (local-cpu-idle-thread)))
    (setf (sys.int::msr +msr-ia32-gs-base+) (sys.int::lisp-object-address idle-thread))
    (increment-n-running-cpus)
    (acquire-global-thread-lock)
    (%%switch-to-thread-common idle-thread idle-thread)))

(defun generate-initial-pml4 ()
  ;; Generate the initial page tables used to bring APs up.
  ;; The PML4 must be 32-bit, as it is impossible to load CR3 with a 64-bit value in 32-bit mode.
  (let ((pml4 (* (allocate-physical-pages 1 :mandatory-p "AP initial PML4" :32-bit-only t) +4k-page-size+))
        (pml3 (* (allocate-physical-pages 1 :mandatory-p "AP initial PML3") +4k-page-size+))
        (pml2 (* (allocate-physical-pages 1 :mandatory-p "AP initial PML2") +4k-page-size+)))
    (zeroize-physical-page pml4)
    (zeroize-physical-page pml3)
    (zeroize-physical-page pml2)
    ;; Identity map the first 2MB.
    (setf (physical-memref-unsigned-byte-64 pml4 0) (logior pml3
                                                            +x86-64-pte-present+
                                                            +x86-64-pte-write+))
    ;; And map the first 2MB of the pmap.
    (setf (physical-memref-unsigned-byte-64 pml4 (address-l4-bits +physical-map-base+))
          (logior pml3
                  +x86-64-pte-present+
                  +x86-64-pte-write+))
    (setf (physical-memref-unsigned-byte-64 pml3 0) (logior pml2
                                                            +x86-64-pte-present+
                                                            +x86-64-pte-write+))
    (setf (physical-memref-unsigned-byte-64 pml2 0) (logior 0
                                                            +x86-64-pte-present+
                                                            +x86-64-pte-write+
                                                            +x86-64-pte-page-size+))
    pml4))

(defun copy-ap-trampoline (trampoline entry-fn-name physical-address initial-pml4)
  "Copy and initialize the AP trampoline."
  (check-type physical-address (unsigned-byte 20) "1MB")
  ;; Copy code.
  (dotimes (i (sys.int::function-code-size trampoline))
    (setf (physical-memref-unsigned-byte-8 physical-address i)
          (sys.int::function-code-byte trampoline i)))
  ;; Overwrite the function header with a nop-slide to the real code.
  (dotimes (i 16)
    (setf (physical-memref-unsigned-byte-8 physical-address i) #x90))
  ;; Fill in fields.
  (setf (physical-memref-t (+ physical-address +ap-bootstrap-ap-entry-point-offset+))
        ;; Get the fref for this symbol.
        (sys.int::%object-ref-t entry-fn-name sys.int::+symbol-function+))
  (setf (physical-memref-unsigned-byte-64 (+ physical-address +ap-bootstrap-real-pml4-offset+))
        (sys.int::%cr3))
  (setf (physical-memref-unsigned-byte-64 (+ physical-address +ap-bootstrap-initial-pml4-offset+))
        initial-pml4)
  ;; Make sure other CPUs see the trampoline before they receive INIT.
  (%wbinvd))

(defun lapic-setup ()
  (setf (lapic-reg +lapic-reg-spurious-interrupt-vector+) #x1FF))

(defun initialize-cpu ()
  (setf *lapic-address* (logand (sys.int::msr +msr-ia32-apic-base+)
                                (lognot #xFFF)))
  (debug-print-line "Local APIC at " *lapic-address*)
  (map-physical-memory *lapic-address* #x1000 "LAPIC")
  (lapic-setup)
  (map-physical-memory +ap-trampoline-physical-address+ #x1000 "AP Bootstrap")
  (setf *initial-pml4* (generate-initial-pml4))
  (copy-ap-trampoline #'%%ap-bootstrap '%%ap-entry-point +ap-trampoline-physical-address+ *initial-pml4*)
  (when (not (boundp '*bsp-cpu*))
    (setf *bsp-cpu* (make-cpu :info-vector sys.int::*bsp-info-vector*
                              :idle-thread sys.int::*bsp-idle-thread*
                              :state :online))
    (setf (sys.int::%object-ref-t sys.int::*bsp-info-vector* +cpu-info-cpu-object-offset+) *bsp-cpu*))
  (setf (cpu-apic-id *bsp-cpu*) (ldb (byte 8 24) (lapic-reg +lapic-reg-id+)))
  (debug-print-line "BSP has LAPIC ID " (cpu-apic-id *bsp-cpu*))
  (setf *cpus* '())
  (push-wired *bsp-cpu* *cpus*)
  (setf *n-up-cpus* 1)
  (hook-user-interrupt +wakeup-ipi-vector+ 'wakeup-ipi-handler)
  (hook-user-interrupt +panic-ipi-vector+ 'panic-ipi-handler)
  (hook-user-interrupt +quiesce-ipi-vector+ 'quiesce-ipi-handler)
  (hook-user-interrupt +tlb-shootdown-ipi-vector+ 'tlb-shootdown-ipi-handler))

(defun load-cpu-bits (vector)
  (let* ((addr (- (sys.int::lisp-object-address vector)
                  sys.int::+tag-object+)))
    (%lgdt (1- (* 4 8)) (+ addr 8 (* +cpu-info-gdt-offset+ 8)))
    (%lidt (1- (* 256 16)) (+ addr 8 (* +cpu-info-idt-offset+ 8)))
    (%ltr 16)
    (%load-cs 8)))

(defun disable-page-fault-ist ()
  (let ((cpu-vec (local-cpu-info)))
    (multiple-value-bind (lo hi)
        (make-idt-entry :offset (sys.int::%object-ref-signed-byte-64
                                 (svref (sys.int::symbol-global-value 'sys.int::*interrupt-service-routines*) 14)
                                 sys.int::+function-entry-point+)
                        :ist 0)
      (setf (sys.int::%object-ref-unsigned-byte-64 cpu-vec (+ +cpu-info-idt-offset+ (* 14 2))) lo
            (sys.int::%object-ref-unsigned-byte-64 cpu-vec (+ +cpu-info-idt-offset+ (* 14 2) 1)) hi))))

(defun register-secondary-cpu (apic-id)
  (let* ((info (mezzano.runtime::%allocate-object
                sys.int::+object-tag-array-unsigned-byte-64+
                1023 1023
                :wired))
         (idle-thread (make-ephemeral-thread #'idle-thread :runnable :name "AP Idle Thread" :priority :idle))
         (wired-stack (%allocate-stack (* 128 1024) t))
         (exception-stack (%allocate-stack (* 128 1024) t))
         (irq-stack (%allocate-stack (* 128 1024) t))
         (cpu (make-cpu :state :offline
                        :info-vector info
                        :apic-id apic-id
                        :idle-thread idle-thread
                        :wired-stack wired-stack
                        :exception-stack exception-stack
                        :irq-stack irq-stack)))
    (populate-cpu-info-vector info
                              (+ (stack-base wired-stack) (stack-size wired-stack))
                              (+ (stack-base exception-stack) (stack-size exception-stack))
                              (+ (stack-base irq-stack) (stack-size irq-stack))
                              idle-thread)
    (setf (sys.int::%object-ref-t info +cpu-info-cpu-object-offset+) cpu)
    (debug-print-line "Registered new CPU " cpu " " info " " idle-thread " with APIC ID " apic-id)
    (push-wired cpu *cpus*)))

(defun boot-cpu (cpu)
  (debug-print-line "Booting CPU " cpu "/" (cpu-apic-id cpu) " " (cpu-info-vector cpu))
  (setf (physical-memref-t (+ +ap-trampoline-physical-address+ +ap-bootstrap-cpu-vector-offset+))
        (cpu-info-vector cpu))
  ;; FIXME: Delay after sending.
  (let ((boot-vector (ash +ap-trampoline-physical-address+ -12)))
    (send-ipi (cpu-apic-id cpu) +ipi-type-init+ 0)
    (sleep 0.01) ; 10ms
    (send-ipi (cpu-apic-id cpu) +ipi-type-sipi+ boot-vector)
    (sleep 0.0002) ; 200μs.
    (send-ipi (cpu-apic-id cpu) +ipi-type-sipi+ boot-vector)
    (sleep 0.0002))
  ;; Wait for the CPU to come up.
  (let ((start-time (get-internal-run-time)))
    (loop
       (when (eql (cpu-state cpu) :online)
         (return))
       (when (> (- (get-internal-run-time) start-time)
                (* 5 internal-time-units-per-second))
         (sys.int::cas (cpu-state (local-cpu-object)) :offline :timed-out)
         (return))))
  (case (cpu-state (local-cpu-object))
    (:online
     (debug-print-line "CPU " cpu "/" (cpu-apic-id cpu) " booted"))
    (t
     (debug-print-line "CPU " cpu "/" (cpu-apic-id cpu) " timed out"))))

(defun detect-secondary-cpus ()
  (let ((bsp-apic-id (cpu-apic-id *bsp-cpu*))
        (madt (acpi-get-table 'acpi-madt-table-p))
        (did-warn nil))
    (when madt
      ;; Walk the ACPI MADT table looking for enabled CPUs.
      (dotimes (i (sys.int::simple-vector-length
                   (acpi-madt-table-controllers madt)))
        (let ((entry (svref (acpi-madt-table-controllers madt) i)))
          (when (and (acpi-madt-processor-lapic-p entry)
                     (logbitp +acpi-madt-processor-lapic-flag-enabled+
                              (acpi-madt-processor-lapic-flags entry))
                     (not (eql (acpi-madt-processor-lapic-apic-id entry) bsp-apic-id)))
            (when (not did-warn)
              (debug-print-line "### Multiple CPUs detected. SMP support is currently experimental and unreliable.")
              (setf did-warn t))
            (register-secondary-cpu (acpi-madt-processor-lapic-apic-id entry))))))))

(defun boot-secondary-cpus ()
  (detect-secondary-cpus)
  (dolist (cpu *cpus*)
    (when (eql (cpu-state cpu) :offline)
      (boot-cpu cpu))))

(in-package :sys.int)

;; (%cpuid leaf ecx) -> eax ebx ecx edx
;; Must be called with the GC deferred as CPUID uses EBX.
(sys.int::define-lap-function %cpuid ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.+n-fixnum-bits+)
  (sys.lap-x86:mov64 :rcx :r9)
  (sys.lap-x86:sar64 :rcx #.+n-fixnum-bits+)
  (sys.lap-x86:cpuid)
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 +n-fixnum-bits+))))
  (sys.lap-x86:lea64 :r9 ((:rbx #.(ash 1 +n-fixnum-bits+))))
  (sys.lap-x86:lea64 :r10 ((:rcx #.(ash 1 +n-fixnum-bits+))))
  (sys.lap-x86:lea64 :r11 ((:rdx #.(ash 1 +n-fixnum-bits+))))
  (sys.lap-x86:xor32 :ebx :ebx)
  (sys.lap-x86:mov32 :ecx #.(ash 4 +n-fixnum-bits+))
  (sys.lap-x86:ret))

(defun cpuid (leaf &optional (rcx 0))
  (check-type leaf (unsigned-byte 32))
  (check-type rcx (unsigned-byte 32))
  (mezzano.supervisor:with-pseudo-atomic
    (%cpuid leaf rcx)))

(defun decode-cpuid-vendor (vendor-1 vendor-2 vendor-3)
  (let ((vendor (make-string (* 4 3))))
    (setf (char vendor 0) (code-char (ldb (byte 8 0) vendor-1))
          (char vendor 1) (code-char (ldb (byte 8 8) vendor-1))
          (char vendor 2) (code-char (ldb (byte 8 16) vendor-1))
          (char vendor 3) (code-char (ldb (byte 8 24) vendor-1))
          (char vendor 4) (code-char (ldb (byte 8 0) vendor-2))
          (char vendor 5) (code-char (ldb (byte 8 8) vendor-2))
          (char vendor 6) (code-char (ldb (byte 8 16) vendor-2))
          (char vendor 7) (code-char (ldb (byte 8 24) vendor-2))
          (char vendor 8) (code-char (ldb (byte 8 0) vendor-3))
          (char vendor 9) (code-char (ldb (byte 8 8) vendor-3))
          (char vendor 10) (code-char (ldb (byte 8 16) vendor-3))
          (char vendor 11) (code-char (ldb (byte 8 24) vendor-3)))
    vendor))
