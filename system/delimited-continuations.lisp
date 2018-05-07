;;;; Copyright (c) 2018 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(defpackage :mezzano.delimited-continuations
  (:use :cl)
  (:export #:call-with-delimitation)
  (:local-nicknames (:lap :sys.lap-x86)))

(in-package :mezzano.delimited-continuations)

(defun call-with-delimitation (thunk handler &key stack-size)
  (check-type thunk function)
  (check-type handler function)
  (let* ((continuation-stack (mezzano.supervisor::%allocate-stack
                              (or stack-size
                                  mezzano.supervisor::*default-stack-size*)))
         (exit-fn (sys.int::make-closure #'%capture-continuation (cons continuation-stack handler))))
    (%call-with-delimitation exit-fn continuation-stack thunk)))

(sys.int::define-lap-function %call-with-delimitation ()
  ;; exit-fn = r8
  ;; continuation-stack = r9
  ;; thunk = r10
  (lap:push :rbp)
  (lap:mov64 :rbp :rsp)
  (lap:mov64 :rsp (:object :r9 1)) ; stack.base
  (lap:sar64 :rsp #.sys.int::+n-fixnum-bits+)
  (lap:mov64 :rcx (:object :r9 2)) ; stack.size
  (lap:sar64 :rcx #.sys.int::+n-fixnum-bits+)
  (lap:add64 :rsp :rcx) ; rsp = continuation-stack top
  (lap:lea64 :rax (:rip PHONY-RETURN-ADDRESS))
  ;; Stack frame layout on the continuation stack must look like: (addresses from top, downwards)
  ;;  -8 phony return address
  ;; -16 prior stack pointer
  ;; -24 prior stack object
  ;; -32 delimitation marker for unwinding
  ;; -40 saved ssp/ssp link
  ;; -48 delimitation marker header
  (lap:push :rax) ; phony return address
  (lap:push :rbp)
  (lap:mov64 :rbp :rsp)
  ;; Insert phony ssp entry
  (lap:gs)
  (lap:mov64 :r12 (:object nil #.mezzano.supervisor::+thread-stack+))
  (lap:push :r12) ; Save the previous stack object.
  (lap:push 0) ; delimitation marker
  (lap:gs) ; link
  (lap:mov64 :rax (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+))
  (lap:push :rax)
  (lap:push #.(ash 3 8)) ; header
  (lap:lea64 :rax (:rsp #.sys.int::+tag-object+))
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+) :rax)
  ;; Call thunk.
  (lap:mov64 :rbx :r10)
  (lap:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (lap:call (:object :rbx #.sys.int::+function-entry-point+))
  ;; FIXME: Need to invalidate the exit function. (store nil in the closure env)
  ;; Pop SSP.
  (lap:mov64 :rax (:rsp 8))
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+) :rax)
  ;; Restore stack object.
  (lap:mov64 :r12 (:rsp 24))
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-stack+) :r12)
  ;; Return to old stack.
  (lap:mov64 :rsp :rbp)
  (lap:pop :rbp)
  (lap:leave)
  (lap:ret)
  PHONY-RETURN-ADDRESS)

(sys.int::define-lap-function %capture-continuation ()
  ;; TODO: Inhibit footholds over this.
  ;; arguments on stack/in regs
  ;; rbx = (cons cont-stack handler) or nil if the delimitation has been removed
  (lap:cmp64 :rbx nil)
  (lap:je DELIMITATION-EXPIRED)
  ;; Save arguments.
  (lap:push :rcx)
  (lap:push :r8)
  (lap:push :r9)
  (lap:push :r10)
  (lap:push :r11)
  (lap:push :r12)
  (lap:push :rbx) ; save stack & handler
  ;; Create the continuation object.
  (lap:mov64 :r13 (:function mezzano.runtime::%allocate-object))
  (lap:mov64 :r8 #.(ash #.sys.int::+object-tag-delimited-continuation+
                        #.sys.int::+n-fixnum-bits+)) ; tag
  (lap:xor64 :r9 :r9) ; data
  (lap:mov64 :r10 #.(ash 4 #.sys.int::+n-fixnum-bits+)) ; size, idk
  (lap:mov64 :r11 nil) ; area
  (lap:call (:object :r13 #.sys.int::+fref-entry-point+))
  (lap:pop :rbx)
  (lap:mov64 :r13 (:car :rbx)) ; stack
  (lap:mov64 :rbx (:cdr :rbx)) ; handler
  (lap:mov64 :rdx (:object :r13 1)) ; stack.base
  (lap:sar64 :rdx #.sys.int::+n-fixnum-bits+)
  (lap:mov64 :rcx (:object :r13 2)) ; stack.size
  (lap:sar64 :rcx #.sys.int::+n-fixnum-bits+)
  (lap:add64 :rdx :rcx) ; rdx = continuation-stack top
  (lap:mov64 :r13 :r8)
  ;; r8-r12 args
  ;; rbx = handler
  ;; r13 = cont
  ;; Restore saved arguments
  (lap:pop :r12)
  (lap:pop :r11)
  (lap:pop :r10)
  (lap:pop :r9)
  (lap:pop :r8)
  (lap:pop :rcx)
  ;; Save the current stack object & pointer.
  (lap:gs)
  (lap:mov64 :rax (:object nil #.mezzano.supervisor::+thread-stack+)) ; extra-regs
  (lap:mov64 (:object :r8 1) :rax)
  (lap:mov64 (:object :r8 2) :rsp)
  (lap:gs)
  (lap:mov64 :rax (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+))
  (lap:mov64 (:object :r8 3) :rax)
  ;; Restore state from the other stack.
  (lap:mov64 :rsp (:rdx -16)) ; saved stack pointer
  (lap:mov64 (:rdx -16) 0)
  (lap:mov64 :rax (:rdx -24)) ; prior stack, extra-regs
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-stack+) :rax)
  (lap:mov64 (:rdx -24) 0) ; flush it
  (lap:mov64 :rax (:rdx -40)) ; saved ssp
  (lap:gs)
  (lap:mov64 (:object nil #.mezzano.supervisor::+thread-special-stack-pointer+) :rax)
  (lap:mov64 (:rdx -40) 0)
  ;; Tail-call to the handler with the continuation in the fref register.
  (lap:pop :rbp)
  (lap:jmp (:object :rbx #.sys.int::+function-entry-point+))
  DELIMITATION-EXPIRED
  (lap:mov64 :r8 (:constant "Attempted to invoke expired delimited continuation escape function."))
  (lap:mov64 :r13 (:function error))
  (lap:mov32 :ecx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (lap:call (:object :r13 #.sys.int::+fref-entry-point+))
  (lap:ud2))
