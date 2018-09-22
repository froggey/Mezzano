;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.cold-generator.x86-64
  (:use :cl)
  (:import-from #:mezzano.cold-generator
                #:configure-system-for-target)
  (:local-nicknames (#:env #:mezzano.cold-generator.environment)))

(in-package :mezzano.cold-generator.x86-64)

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
              for (handler error-code-p) in cold-generator.x86-64::*cpu-exception-info*
              when handler
              append (list* '(:align 16)
                            sym
                            (cold-generator.x86-64::create-exception-isr handler error-code-p)))
           (loop
              for i from 32 below idt-size
              for sym in (nthcdr 32 syms)
              append (list* '(:align 16)
                            sym
                            (cold-generator.x86-64::create-user-interrupt-isr i)))
           (list '(:align 16)
                 'cold-generator.x86-64::user-interrupt-common)
           cold-generator.x86-64::*common-user-interrupt-code*
           (list '(:align 16)
                 'cold-generator.x86-64::interrupt-common)
           cold-generator.x86-64::*common-interrupt-code*)))
    (multiple-value-bind (fn symbols)
        (env:compile-lap environment isr-code
                         :area :wired
                         :name (env:translate-symbol environment 'sys.int::%%interrupt-service-routines))
      (loop
         for (name . value) in symbols
         for pos = (position name syms)
         when pos
         do (setf (aref isr-table pos) value))
      (env:%defun environment (env:translate-symbol environment 'sys.int::%%interrupt-service-routines) fn)
      (setf (env:cross-symbol-value environment 'sys.int::*interrupt-service-routines*) isr-table)
      (values))))

(defmethod configure-system-for-target (environment (target (eql :x86-64)))
  (env:add-special environment :undefined-function
                   (env:compile-lap environment
                                    cold-generator.x86-64:*undefined-function-thunk*
                                    :area :wired
                                    :name (env:translate-symbol environment 'sys.int::%%undefined-function-trampoline)))
  (env:add-special environment :closure-trampoline
                   (env:compile-lap environment
                                    cold-generator.x86-64:*closure-trampoline*
                                    :area :wired
                                    :name (env:translate-symbol environment 'sys.int::%%closure-trampoline)))
  (env:add-special environment :funcallable-instance-trampoline
                   (env:compile-lap environment
                                    cold-generator.x86-64:*funcallable-instance-trampoline*
                                    :area :wired
                                    :name (env:translate-symbol environment 'sys.int::%%funcallable-instance-trampoline)))
  (create-low-level-interrupt-support environment))
