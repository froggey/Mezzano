;;;; ARM64 target support

(defpackage :mezzano.cold-generator.arm64
  (:use :cl)
  (:import-from #:mezzano.cold-generator
                #:configure-system-for-target)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)
                    (#:lap #:mezzano.lap.arm64)
                    (#:sys.int #:mezzano.internals)))

(in-package :mezzano.cold-generator.arm64)

(defparameter *undefined-function-thunk*
  `(;; Call helper using the function calling convention, leaving fref intact.
    (lap:ldr :x6 (:function sys.int::raise-undefined-function))
    (lap:ldr :x6 (:object :x6 ,sys.int::+fref-function+))
    (lap:ldr :x9 (:object :x6 ,sys.int::+function-entry-point+))
    (lap:br :x9))
  "Code for the undefined function thunk.")

(defparameter *closure-trampoline*
  `(;; Load the real function from the fref.
    (lap:ldr :x6 (:object :x7 ,sys.int::+fref-function+))
    ;; Invoke the real function via the FUNCTION calling convention.
    ;; This will work even if the fref was altered or made funbound.
    ;; (SETF FUNCTION-REFERENCE-FUNCTION) will set the function to the
    ;; undefined-function thunk in that case, so everything works fine.
    (lap:ldr :x9 (:object :x6 ,sys.int::+function-entry-point+))
    (lap:br :x9))
  "Trampoline used for calling a closure or funcallable-instance via an fref.")

(defparameter *funcallable-instance-trampoline*
  `(;; Load the real function from the funcallable-instance.
    (lap:ldr :x6 (:object :x6 ,sys.int::+funcallable-instance-function+))
    ;; Invoke the real function via the FUNCTION calling convention.
    (lap:ldr :x9 (:object :x6 ,sys.int::+function-entry-point+))
    (lap:br :x9))
  "Trampoline used for calling a closure or funcallable-instance via a funcallable-instance.")

(defmethod configure-system-for-target (environment (target (eql :arm64)))
  (env:add-special environment :undefined-function
                   (env:compile-lap environment
                                    *undefined-function-thunk*
                                    :area :wired
                                    :name (env:translate-symbol environment 'sys.int::%%undefined-function-trampoline)))
  (env:add-special environment :closure-trampoline
                   (env:compile-lap environment
                                    *closure-trampoline*
                                    :area :wired
                                    :name (env:translate-symbol environment 'sys.int::%%closure-trampoline)))
  (env:add-special environment :funcallable-instance-trampoline
                   (env:compile-lap environment
                                    *funcallable-instance-trampoline*
                                    :area :wired
                                    :name (env:translate-symbol environment 'sys.int::%%funcallable-instance-trampoline))))
