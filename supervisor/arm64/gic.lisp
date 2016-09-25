;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.supervisor)

;;; Distributor registers.
(defconstant +gicd-ctlr+ #x000)
(defconstant +gicd-typer+ #x004)
(defconstant +gicd-iidr+ #x008)
(defconstant +gicd-igroupr0+ #x080)
(defconstant +gicd-isenabler0+ #x100)
(defconstant +gicd-icenabler0+ #x180)
(defconstant +gicd-ispendr0+ #x200)
(defconstant +gicd-icpendr0+ #x280)
(defconstant +gicd-isactiver0+ #x300)
(defconstant +gicd-icactiver0+ #x380)
(defconstant +gicd-ipriorityr0+ #x400)
(defconstant +gicd-itargetsr0+ #x800)
(defconstant +gicd-icfgr0+ #xC00)
(defconstant +gicd-nsacr0+ #xE00)
(defconstant +gicd-sgir+ #xF00)
(defconstant +gicd-cpendsgir+ #xF10)
(defconstant +gicd-spendsgir+ #xF20)

;;; CPU interface registers.
(defconstant +gicc-ctlr+ #x0000)
(defconstant +gicc-pmr+ #x0004)
(defconstant +gicc-bpr+ #x0008)
(defconstant +gicc-iar+ #x000C)
(defconstant +gicc-eoir+ #x0010)
(defconstant +gicc-rpr+ #x0014)
(defconstant +gicc-hppir+ #x0018)
(defconstant +gicc-abpr+ #x001C)
(defconstant +gicc-aiar+ #x0020)
(defconstant +gicc-aeoir+ #x0024)
(defconstant +gicc-ahppir+ #x0028)
(defconstant +gicc-apr0+ #x00D0)
(defconstant +gicc-nsapr0+ #x00E0)
(defconstant +gicc-iidr+ #x00FC)
(defconstant +gicc-dir+ #x1000)

(sys.int::defglobal *gic-distributor-base*)
(sys.int::defglobal *gic-cpu-interface-base*)
(sys.int::defglobal *gic-handlers*)

(defun gic-dist-reg (index)
  (physical-memref-unsigned-byte-32 (+ *gic-distributor-base* index)))

(defun (setf gic-dist-reg) (value index)
  (setf (physical-memref-unsigned-byte-32 (+ *gic-distributor-base* index)) value))

(defun gic-cpui-reg (index)
  (physical-memref-unsigned-byte-32 (+ *gic-cpu-interface-base* index)))

(defun (setf gic-cpui-reg) (value index)
  (setf (physical-memref-unsigned-byte-32 (+ *gic-cpu-interface-base* index)) value))

(defun gic-max-cpu ()
  (1+ (ldb (byte 3 5) (gic-dist-reg +gicd-typer+))))

(defun gic-max-interrupts ()
  (* 32 (1+ (ldb (byte 5 0) (gic-dist-reg +gicd-typer+)))))

(defun initialize-gic (distributor-address cpu-address)
  (setf *gic-distributor-base* distributor-address
        *gic-cpu-interface-base* cpu-address)
  (when (not (boundp '*gic-handlers*))
    (setf *gic-handlers* (sys.int::make-simple-vector 1024 :wired)))
  (dotimes (i 1024)
    (setf (svref *gic-handlers* i) nil))
  (let ((n-interrupts (gic-max-interrupts)))
    (debug-print-line n-interrupts " total GIC interrupts")
    ;; Mask and reset all interrupts.
    (loop
       for i from 0 below n-interrupts by 32
       for reg from 0 by 4
       do
         (setf (gic-dist-reg (+ +gicd-icenabler0+ reg)) #xFFFFFFFF)
         (setf (gic-dist-reg (+ +gicd-icpendr0+ reg)) #xFFFFFFFF))
    (when (> (gic-max-cpu) 1)
      ;; Set external interrupts to target cpu 0.
      (loop
         for i from 32 below n-interrupts by 32
         for reg from 8 by 4
         do
           (setf (gic-dist-reg (+ +gicd-itargetsr0+ reg)) #x01010101))))
  ;; Enable the distributor and local CPU.
  (setf (gic-dist-reg +gicd-ctlr+) 1)
  (setf (gic-cpui-reg +gicc-ctlr+) 1)
  ;; Allow all priority levels.
  (setf (gic-cpui-reg +gicc-pmr+) #xFF))

(defun gic-mask-interrupt (vector)
  (ensure (<= 0 vector 1019))
  (multiple-value-bind (reg index)
      (truncate vector 32)
    (setf (gic-dist-reg (+ +gicd-icenabler0+ (* reg 4))) (ash 1 index))))

(defun gic-unmask-interrupt (vector)
  (ensure (<= 0 vector 1019))
  (multiple-value-bind (reg index)
      (truncate vector 32)
    (setf (gic-dist-reg (+ +gicd-isenabler0+ (* reg 4))) (ash 1 index))))

(defun gic-hook-interrupt (vector handler)
  (check-type handler (or function symbol))
  (push-wired handler (svref *gic-handlers* vector)))

(defun gic-handle-interrupt (interrupt-frame)
  (let* ((iar (gic-cpui-reg +gicc-iar+))
         (vector (ldb (byte 9 0) iar)))
    (when (eql vector 1023)
      ;; Spurious interrupt.
      (return-from gic-handle-interrupt))
    (dolist (handler (svref *gic-handlers* vector))
      (funcall handler interrupt-frame vector))
    ;; Send EOI.
    (setf (gic-cpui-reg +gicc-eoir+) iar)
    (maybe-preempt-via-interrupt interrupt-frame)))
