;;;; Copyright (c) 2011-2015 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

(defvar sys.int::*interrupt-service-routines*)

(defun make-idt-entry (&key (offset 0) (segment #x0008)
                         (present t) (dpl 0) (ist nil)
                         (interrupt-gate-p t))
  "Returns the low and high words of the IDT entry."
  ;; ###: Need to be more careful avoiding bignums.
  (let ((value 0))
    (setf (ldb (byte 16 48) value) (ldb (byte 16 16) offset)
          (ldb (byte 1 47) value) (if present 1 0)
          (ldb (byte 2 45) value) dpl
          (ldb (byte 4 40) value) (if interrupt-gate-p
                                      #b1110
                                      #b1111)
          (ldb (byte 3 32) value) (or ist 0)
          (ldb (byte 16 16) value) segment
          (ldb (byte 16 0) value) (ldb (byte 16 0) offset))
    (values value (ldb (byte 32 32) offset))))

(defconstant +cpu-info-self-offset+ 0)
(defconstant +cpu-info-gdt-offset+ 128)
(defconstant +cpu-info-tss-offset+ 256)
(defconstant +cpu-info-tss-size+ 104)
(defconstant +cpu-info-idt-offset+ 4096)

(sys.int::define-lap-function %lgdt ()
  (:gc :no-frame)
  (sys.lap-x86:sub64 :rsp 16)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rax :r8) ; length
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 (:rsp) :rax)
  (sys.lap-x86:mov64 :rax :r9) ; poiner
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 (:rsp 2) :rax)
  (sys.lap-x86:lgdt (:rsp))
  (sys.lap-x86:add64 :rsp 16)
  (:gc :no-frame)
  (sys.lap-x86:ret))

(sys.int::define-lap-function %lidt ()
  (:gc :no-frame)
  (sys.lap-x86:sub64 :rsp 16)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rax :r8) ; length
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 (:rsp) :rax)
  (sys.lap-x86:mov64 :rax :r9) ; poiner
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 (:rsp 2) :rax)
  (sys.lap-x86:lidt (:rsp))
  (sys.lap-x86:add64 :rsp 16)
  (:gc :no-frame)
  (sys.lap-x86:ret))

(sys.int::define-lap-function %ltr ()
  (sys.lap-x86:mov64 :rax :r8) ; selector
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:ltr :ax)
  (sys.lap-x86:ret))

(sys.int::define-lap-function %load-cs ()
  (sys.lap-x86:mov64 :rax :r8) ; selector
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:lea64 :rax (:rip next))
  (sys.lap-x86:push :rax)
  (sys.lap-x86:retf)
  next
  (sys.lap-x86:ret))

(defun initialize-boot-cpu ()
  "Generate GDT, IDT and TSS for the boot CPU."
  ;; Carve out a pair of pages.
  (let* ((frame (allocate-physical-pages 2 "CPU data"))
         (addr (+ +physical-map-base+ (ash frame 12)))
         (tss-base (+ addr +cpu-info-tss-offset+)))
    ;; IDT completely fills the second page (256 * 16)
    (dotimes (i 256)
      (multiple-value-bind (lo hi)
          (if (svref sys.int::*interrupt-service-routines* i)
              (make-idt-entry :offset (sys.int::%array-like-ref-signed-byte-64
                                       (svref sys.int::*interrupt-service-routines* i)
                                       0)
                              :ist (cond ((eql i 14) 1) ; page fault.
                                         ((>= i 32) 2) ; IRQ
                                         (t 0)))
              (values 0 0))
        (setf (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-idt-offset+) (* i 2)) lo
              (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-idt-offset+) (1+ (* i 2))) hi)))
    ;; GDT.
    (setf (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-gdt-offset+) 0) 0 ; NULL seg.
          (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-gdt-offset+) 1) #x00209A0000000000 ; Kernel CS64
          ;; TSS low.
          (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-gdt-offset+) 2) (logior (ldb (byte 16 0) +cpu-info-tss-size+)
                                                                                      (ash (ldb (byte 24 0) tss-base) 16)
                                                                                      (ash #x89 40)
                                                                                      (ash (ldb (byte 4 16) +cpu-info-tss-size+) 48)
                                                                                      (ash (ldb (byte 8 24) tss-base) 56))
          ;; TSS high.
          (sys.int::memref-unsigned-byte-64 (+ addr +cpu-info-gdt-offset+) 3) (ldb (byte 32 32) tss-base))
    ;; TSS, Clear memory first.
    (dotimes (i +cpu-info-tss-size+)
      (setf (sys.int::memref-unsigned-byte-16 tss-base i) 0))
    ;; IST1.
    (setf (sys.int::memref-signed-byte-64 (+ tss-base 36) 0) (+ sys.int::*exception-stack-base*
                                                                sys.int::*exception-stack-size*))
    ;; IST2.
    (setf (sys.int::memref-signed-byte-64 (+ tss-base 44) 0) (+ sys.int::*irq-stack-base*
                                                                sys.int::*irq-stack-size*))
    ;; I/O Map Base Address, follows TSS body.
    (setf (sys.int::memref-unsigned-byte-16 (+ tss-base 102) 0) 104)
    ;; Other stuff.
    (setf (sys.int::memref-t (+ addr +cpu-info-self-offset+) 0) addr)
    ;; Shove the cpu info page into FS.
    (setf (sys.int::msr sys.int::+msr-ia32-fs-base+) addr)
    ;; Load various bits.
    (%lgdt (1- (* 4 8)) (+ addr +cpu-info-gdt-offset+))
    (%lidt (1- (* 256 16)) (+ addr +cpu-info-idt-offset+))
    (%ltr 16)
    (%load-cs 8)))
