;;;; ARM64 target support

(defpackage :mezzano.cold-generator.arm64
  (:use :cl)
  (:import-from #:mezzano.cold-generator
                #:configure-system-for-target)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:ser #:mezzano.cold-generator.serialize)
                    (#:util #:mezzano.cold-generator.util)
                    (#:lap #:mezzano.lap.arm64)
                    (#:sup #:mezzano.supervisor)
                    (#:sys.int #:mezzano.internals)))

(in-package :mezzano.cold-generator.arm64)

(defparameter *funcallable-instance-trampoline*
  `(;; Load the real function from the funcallable-instance.
    (lap:ldr :x6 (:object :x6 ,sys.int::+funcallable-instance-function+))
    ;; Invoke the real function via the FUNCTION calling convention.
    (lap:ldr :x9 (:object :x6 ,sys.int::+function-entry-point+))
    (lap:br :x9))
  "Trampoline used for calling a closure or funcallable-instance via a funcallable-instance.")

(defmethod configure-system-for-target (environment (target (eql :arm64)))
  (env:add-special environment :funcallable-instance-trampoline
                   (env:compile-lap environment
                                    *funcallable-instance-trampoline*
                                    :area :wired-function
                                    :name (env:translate-symbol environment 'sys.int::%%funcallable-instance-trampoline)))
  (setf (env:cross-symbol-value environment 'sys.int::*bsp-wired-stack*)
        (env:make-stack environment (* 128 1024)))
  (setf (env:cross-symbol-value environment 'sys.int::*arm64-exception-vector*)
        (env:compile-lap environment
                         (loop repeat (/ (* 1024 2) 8) ; for 1024 alignment
                               collect `(:d64/le 0))
                         :area :wired-function
                         :name 'sys.int::*arm64-exception-vector*)))

(defmethod ser:post-serialize-image-for-target (image environment (target (eql :arm64)))
  (let* ((ex-vec (env:cross-symbol-value environment 'sys.int::*arm64-exception-vector*))
         (ex-vec-val (ser:serialize-object ex-vec image environment))
         (ex-vec-addr (+ ex-vec-val (- sys.int::+tag-object+) 8)) ; slot 0
         ;; Base address of the exception vector must be 1024-byte aligned.
         (ex-vec-base (util:align-up ex-vec-addr 1024))
         (offset (- ex-vec-base ex-vec-addr))
         (slot-offset (/ offset 8)))
    (setf (ser::image-symbol-value image environment 'sys.int::*arm64-exception-vector-base*)
          (ser::serialize-object ex-vec-base image environment))
    (labels (((setf mref32) (value base index)
               (if (evenp index)
                   (setf (ldb (byte 32 0) (ser::object-slot image ex-vec-val (+ slot-offset base (ash index -1)))) value)
                   (setf (ldb (byte 32 32) (ser::object-slot image ex-vec-val (+ slot-offset base (ash index -1)))) value)))
             (gen-vector (offset common entry)
               (setf offset (/ offset 8))
               (let* ((common-fref (env:function-reference
                                    environment
                                    (env:translate-symbol environment common)))
                      (common-fn (env:function-reference-function common-fref))
                      (common-fn-val (ser:serialize-object common-fn image environment))
                      (common-entry (ser::object-slot image common-fn-val sys.int::+function-entry-point+))
                      (entry-fref (env:function-reference
                                   environment
                                   (env:translate-symbol environment entry)))
                      (entry-fref-val (ser:serialize-object entry-fref image environment)))
                 ;; sub sp, sp, #x30. Space for the iret frame & frame pointer
                 (setf (mref32 offset 0) #xD100C3FF)
                 ;; str x29, [sp]
                 (setf (mref32 offset 1) #xF90003FD)
                 ;; ldr x29, [fn]
                 (setf (mref32 offset 2) #x5800005D)
                 ;; b common-entry
                 (let ((entry-rel (- common-entry (+ ex-vec-base (* offset 8) 12))))
                   (setf (mref32 offset 3)
                         (logior #x14000000
                                 (ldb (byte 26 2) entry-rel))))
                 ;; fn: entry-fref
                 (setf (ser::object-slot image ex-vec-val (+ slot-offset offset 2)) entry-fref-val)))
             (gen-invalid (offset)
               ;; HLT #1
               (setf (mref32 offset 0) #xD4400020)))
      (gen-vector #x000 'sup::%el0-common 'sup::%synchronous-el0-handler)
      (gen-vector #x080 'sup::%el0-common 'sup::%irq-el0-handler)
      (gen-vector #x100 'sup::%el0-common 'sup::%fiq-el0-handler)
      (gen-vector #x180 'sup::%el0-common 'sup::%serror-el0-handler)
      (gen-vector #x200 'sup::%elx-common 'sup::%synchronous-elx-handler)
      (gen-vector #x280 'sup::%elx-common 'sup::%irq-elx-handler)
      (gen-vector #x300 'sup::%elx-common 'sup::%fiq-elx-handler)
      (gen-vector #x380 'sup::%elx-common 'sup::%serror-elx-handler)
      ;; These vectors are used when the CPU moves from a lower EL.
      ;; We're always running in EL1, so these are not used.
      (dotimes (i 8)
        (gen-invalid (+ #x400 (* i #x80)))))))
