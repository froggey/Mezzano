;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defconstant +msr-ia32-efer+    #xC0000080)
(defconstant +msr-ia32-fs-base+ #xC0000100)
(defconstant +msr-ia32-gs-base+ #xC0000101)

(sys.int::defglobal sys.int::*interrupt-service-routines*)

(sys.int::defglobal sys.int::*bsp-wired-stack-base*)
(sys.int::defglobal sys.int::*bsp-wired-stack-size*)
(sys.int::defglobal sys.int::*bsp-info-vector*)
(sys.int::defglobal sys.int::*exception-stack-base*)
(sys.int::defglobal sys.int::*exception-stack-size*)
(sys.int::defglobal sys.int::*irq-stack-base*)
(sys.int::defglobal sys.int::*irq-stack-size*)

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

(defconstant +cpu-info-self-offset+ 8)
(defconstant +cpu-info-wired-stack-offset+ 16)
(defconstant +cpu-info-gdt-offset+ 128)
(defconstant +cpu-info-tss-offset+ 256)
(defconstant +cpu-info-tss-size+ 104)
(defconstant +cpu-info-idt-offset+ 4096)

(sys.int::define-lap-function local-cpu-info (())
  "Return the address of the local CPU's info vector."
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:fs)
  (sys.lap-x86:mov64 :r8 (#.+cpu-info-self-offset+))
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

(defconstant +tss-ist-1+ 36)
(defconstant +tss-ist-2+ 44)
(defconstant +tss-ist-3+ 52)
(defconstant +tss-ist-4+ 60)
(defconstant +tss-ist-5+ 68)
(defconstant +tss-ist-6+ 76)
(defconstant +tss-ist-7+ 84)

(defconstant +tss-io-map-base+ 102)

(defun populate-cpu-info-vector (addr wired-stack-pointer exception-stack-pointer irq-stack-pointer)
  (let* ((tss-base (+ addr +cpu-info-tss-offset+)))
    ;; IDT completely fills the second page (256 * 16)
    (dotimes (i 256)
      (multiple-value-bind (lo hi)
          (if (svref sys.int::*interrupt-service-routines* i)
              (make-idt-entry :offset (sys.int::%object-ref-signed-byte-64
                                       (svref sys.int::*interrupt-service-routines* i)
                                       sys.int::+function-entry-point+)
                              :ist (cond ((eql i 14) 1) ; page fault.
                                         ((>= i 32) 2) ; IRQ
                                         (t 0)))
              (values 0 0))
        (setf (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-idt-offset+) (* i 2)) lo
              (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-idt-offset+) (1+ (* i 2))) hi)))
    ;; GDT.
    (setf (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-gdt-offset+) 0) 0 ; NULL seg.
          (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-gdt-offset+) 1) #x00209A0000000000 ; Kernel CS64
          ;; TSS low. Does not fit in a fixnum when treated as a 64-bit value, depending on where the info page
          ;; was allocated.
          (sys.int::memref-unsigned-byte-32 (+ addr +cpu-info-gdt-offset+ (* 2 8)) 0) (logior (ldb (byte 16 0) +cpu-info-tss-size+)
                                                                                              (ash (ldb (byte 16 0) tss-base) 16))
          (sys.int::memref-unsigned-byte-32 (+ addr +cpu-info-gdt-offset+ (* 2 8)) 1) (logior (ldb (byte 8 16) tss-base)
                                                                                              (ash #x89 8)
                                                                                              (ash (ldb (byte 4 16) +cpu-info-tss-size+) 16)
                                                                                              (ash (ldb (byte 8 24) tss-base) 24))
          ;; TSS high.
          (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-gdt-offset+) 3) (ldb (byte 32 32) tss-base))
    ;; TSS, Clear memory first.
    (dotimes (i +cpu-info-tss-size+)
      (setf (sys.int::memref-unsigned-byte-16 tss-base i) 0))
    ;; IST1.
    (setf (sys.int::memref-signed-byte-64 (+ tss-base +tss-ist-1+) 0) exception-stack-pointer)
    ;; IST2.
    (setf (sys.int::memref-signed-byte-64 (+ tss-base +tss-ist-2+) 0) irq-stack-pointer)
    ;; I/O Map Base Address, follows TSS body.
    (setf (sys.int::memref-unsigned-byte-16 (+ tss-base +tss-io-map-base+) 0) +cpu-info-tss-size+)
    ;; Other stuff.
    (setf (sys.int::memref-t (+ addr +cpu-info-self-offset+) 0) addr)
    (setf (sys.int::memref-signed-byte-64 (+ addr +cpu-info-wired-stack-offset+) 0)
          wired-stack-pointer)
    ;; Shove the cpu info page into FS.
    (setf (sys.int::msr +msr-ia32-fs-base+) addr)))

(defun initialize-boot-cpu ()
  "Generate GDT, IDT and TSS for the boot CPU."
  ;; Carve out a pair of pages.
  (let* ((addr (- (sys.int::lisp-object-address sys.int::*bsp-info-vector*)
                  sys.int::+tag-object+)))
    (populate-cpu-info-vector addr
                              (+ sys.int::*bsp-wired-stack-base* sys.int::*bsp-wired-stack-size*)
                              (+ sys.int::*exception-stack-base* sys.int::*exception-stack-size*)
                              (+ sys.int::*irq-stack-base* sys.int::*irq-stack-size*))
    ;; Load various bits.
    (load-cpu-bits addr)))

(defun load-cpu-bits (addr)
  (%lgdt (1- (* 4 8)) (+ addr +cpu-info-gdt-offset+))
  (%lidt (1- (* 256 16)) (+ addr +cpu-info-idt-offset+))
  (%ltr 16)
  (%load-cs 8))

(defun disable-page-fault-ist ()
  (let ((addr (local-cpu-info)))
    (multiple-value-bind (lo hi)
        (make-idt-entry :offset (sys.int::%object-ref-signed-byte-64
                                 (svref (sys.int::symbol-global-value 'sys.int::*interrupt-service-routines*) 14)
                                 sys.int::+function-entry-point+)
                        :ist 0)
      (setf (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-idt-offset+) (* 14 2)) lo
            (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-idt-offset+) (1+ (* 14 2))) hi))))

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
