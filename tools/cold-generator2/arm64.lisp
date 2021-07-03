;;;; ARM64 target support

(defpackage :mezzano.cold-generator.arm64
  (:use :cl)
  (:import-from #:mezzano.cold-generator
                #:configure-system-for-target)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:lap #:mezzano.lap.arm64)
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
  ;; TODO: Build the exception vector here instead of in initialize-boot-cpu
  (setf (env:cross-symbol-value environment 'sys.int::*arm64-exception-vector*)
        (env:make-array environment (* 2 4096))))
