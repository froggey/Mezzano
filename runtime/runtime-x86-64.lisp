;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(sys.int::define-lap-function values-list ((list)
                                           ((list 0)))
  "Returns the elements of LIST as multiple values."
  ENTRY-POINT
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00 :incoming-arguments :rcx)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame :incoming-arguments :rcx)
  (sys.lap-x86:sub64 :rsp 16) ; 2 slots
  (sys.lap-x86:cmp32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:jne bad-arguments)
  (:gc :frame)
  ;; RBX = iterator, (:stack 0) = list.
  (sys.lap-x86:mov64 :rbx :r8)
  (sys.lap-x86:mov64 (:stack 0) :r8)
  (:gc :frame :layout #*10)
  ;; ECX = value count.
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Pop into R8.
  ;; If LIST is NIL, then R8 must be NIL, so no need to
  ;; set R8 to NIL in the 0-values case.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r8 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R9.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r9 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R10.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r10 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R11.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r11 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Pop into R12.
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :r12 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Registers are populated, now unpack into the MV-area
  (sys.lap-x86:mov32 :edi #.(+ (- 8 sys.int::+tag-object+)
                               (* mezzano.supervisor::+thread-mv-slots-start+ 8)))
  (:gc :frame :layout #*10 :multiple-values 0)
  unpack-loop
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:cmp32 :ecx #.(ash (+ (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+) 5) sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jae too-many-values)
  (sys.lap-x86:mov64 :r13 (:car :rbx))
  (sys.lap-x86:mov64 :rbx (:cdr :rbx))
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:rdi) :r13)
  (:gc :frame :layout #*10 :multiple-values 1)
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :layout #*10 :multiple-values 0)
  (sys.lap-x86:add64 :rdi 8)
  (sys.lap-x86:jmp unpack-loop)
  done
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0 :multiple-values 0)
  (sys.lap-x86:ret)
  type-error
  (:gc :frame :layout #*10)
  (sys.lap-x86:mov64 :r8 (:stack 0))
  (sys.lap-x86:mov64 :r9 (:constant proper-list))
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-type-error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2)
  too-many-values
  (sys.lap-x86:mov64 :r8 (:constant "Too many values in list ~S."))
  (sys.lap-x86:mov64 :r9 (:stack 0))
  (sys.lap-x86:mov64 :r13 (:function error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2)
  bad-arguments
  (:gc :frame :incoming-arguments :rcx)
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-invalid-argument-error))
  (sys.lap-x86:lea64 :rbx (:rip (+ (- ENTRY-POINT 16) #.sys.int::+tag-object+)))
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(sys.int::define-lap-function sys.int::values-simple-vector ((simple-vector))
  "Returns the elements of SIMPLE-VECTOR as multiple values."
  ENTRY-POINT
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00 :incoming-arguments :rcx)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame :incoming-arguments :rcx)
  ;; Check arg count.
  (sys.lap-x86:cmp64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:jne bad-arguments)
  (:gc :frame)
  ;; Check type.
  (sys.lap-x86:mov8 :al :r8l)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-object+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:mov64 :rax (:object :r8 -1))
  ;; Simple vector object tag is zero.
  (sys.lap-x86:test8 :al #.(ash (1- (ash 1 sys.int::+object-type-size+))
                                sys.int::+object-type-shift+))
  (sys.lap-x86:jnz type-error)
  ;; Get number of values.
  (sys.lap-x86:shr64 :rax #.sys.int::+object-data-shift+)
  (sys.lap-x86:jz zero-values)
  (sys.lap-x86:cmp64 :rax #.(+ (- mezzano.supervisor::+thread-mv-slots-end+ mezzano.supervisor::+thread-mv-slots-start+) 5))
  (sys.lap-x86:jae too-many-values)
  ;; Set up. RBX = vector, RCX = number of values loaded so far, RAX = total number of values.
  (sys.lap-x86:mov64 :rbx :r8)
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Load register values.
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r8 (:object :rbx 0))
  (sys.lap-x86:cmp64 :rax 1)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r9 (:object :rbx 1))
  (sys.lap-x86:cmp64 :rax 2)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r10 (:object :rbx 2))
  (sys.lap-x86:cmp64 :rax 3)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r11 (:object :rbx 3))
  (sys.lap-x86:cmp64 :rax 4)
  (sys.lap-x86:je done)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:mov64 :r12 (:object :rbx 4))
  (sys.lap-x86:cmp64 :rax 5)
  (sys.lap-x86:je done)
  ;; Registers are populated, now unpack into the MV-area
  (sys.lap-x86:mov32 :edi #.(+ (- 8 sys.int::+tag-object+)
                               (* mezzano.supervisor::+thread-mv-slots-start+ 8)))
  (sys.lap-x86:mov32 :edx 5) ; Current value.
  (:gc :frame :multiple-values 0)
  unpack-loop
  (sys.lap-x86:mov64 :r13 (:object :rbx 0 :rdx))
  (sys.lap-x86:gs)
  (sys.lap-x86:mov64 (:rdi) :r13)
  (:gc :frame :multiple-values 1)
  (sys.lap-x86:add64 :rcx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :multiple-values 0)
  (sys.lap-x86:add64 :rdi 8)
  (sys.lap-x86:add64 :rdx 1)
  (sys.lap-x86:cmp64 :rdx :rax)
  (sys.lap-x86:jne unpack-loop)
  done
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0 :multiple-values 0)
  (sys.lap-x86:ret)
  ;; Special-case 0 values as it requires NIL in R8.
  zero-values
  (:gc :frame)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:xor32 :ecx :ecx)
  (sys.lap-x86:jmp done)
  (:gc :frame)
  type-error
  (sys.lap-x86:mov64 :r9 (:constant simple-vector))
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-type-error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2)
  too-many-values
  (sys.lap-x86:mov64 :r8 (:constant "Too many values in simple-vector ~S."))
  (sys.lap-x86:mov64 :r9 :rbx)
  (sys.lap-x86:mov64 :r13 (:function error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2)
  bad-arguments
  (:gc :frame :incoming-arguments :rcx)
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-invalid-argument-error))
  (sys.lap-x86:lea64 :rbx (:rip (+ (- ENTRY-POINT 16) #.sys.int::+tag-object+)))
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

;; (defun eql (x y)
;;   (or (eq x y)
;;       (and (%value-has-tag-p x +tag-object+)
;;            (%value-has-tag-p y +tag-object+)
;;            (eq (%object-tag x) (%object-tag y))
;;            (<= +first-numeric-object-tag+ (%object-tag x) +last-numeric-object-tag+)
;;            (= x y))))
(sys.int::define-lap-function eql ((x y))
  "Compare X and Y."
  ENTRY-POINT
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00 :incoming-arguments :rcx)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame :incoming-arguments :rcx)
  ;; Check arg count.
  (sys.lap-x86:cmp64 :rcx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:jne BAD-ARGUMENTS)
  (:gc :frame)
  (:debug ((x :r8 :value) (y :r9 :value)))
  ;; EQ test.
  ;; This additionally covers fixnums, characters and single-floats.
  (sys.lap-x86:cmp64 :r8 :r9)
  (sys.lap-x86:jne MAYBE-NUMBER-CASE)
  ;; Objects are EQ.
  (:debug ())
  (sys.lap-x86:mov32 :r8d t)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret)
  (:gc :frame)
  MAYBE-NUMBER-CASE
  (:debug ((x :r8 :value) (y :r9 :value)))
  ;; Not EQ.
  ;; Both must be objects.
  (sys.lap-x86:mov8 :al :r8l)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-object+)
  (sys.lap-x86:jne OBJECTS-UNEQUAL)
  (sys.lap-x86:mov8 :al :r9l)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-object+)
  (sys.lap-x86:jne OBJECTS-UNEQUAL)
  ;; Both are objects.
  ;; Test that both are the same kind of object.
  (sys.lap-x86:mov64 :rax (:object :r8 -1))
  (sys.lap-x86:and8 :al #.(ash (1- (ash 1 sys.int::+object-type-size+))
                               sys.int::+object-type-shift+))
  (sys.lap-x86:mov64 :rdx (:object :r9 -1))
  (sys.lap-x86:and8 :dl #.(ash (1- (ash 1 sys.int::+object-type-size+))
                               sys.int::+object-type-shift+))
  (sys.lap-x86:cmp8 :al :dl)
  (sys.lap-x86:jne OBJECTS-UNEQUAL)
  ;; They must be numbers. Characters were handled above.
  (sys.lap-x86:sub8 :al #.(ash sys.int::+first-numeric-object-tag+
                               sys.int::+object-type-shift+))
  (sys.lap-x86:cmp8 :al #.(ash (- sys.int::+last-numeric-object-tag+
                                  sys.int::+first-numeric-object-tag+)
                               sys.int::+object-type-shift+))
  (sys.lap-x86:ja OBJECTS-UNEQUAL)
  ;; Both are numbers of the same type. Tail-call to generic-=.
  ;; RCX was set to fixnum 2 on entry.
  (sys.lap-x86:mov64 :r13 (:function sys.int::generic-=))
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+))
  (:gc :frame)
  OBJECTS-UNEQUAL
  ;; Objects are not EQL.
  (:debug ())
  (sys.lap-x86:mov32 :r8d nil)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret)
  (:gc :frame)
  BAD-ARGUMENTS
  (:gc :frame :incoming-arguments :rcx)
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-invalid-argument-error))
  (sys.lap-x86:lea64 :rbx (:rip (+ (- ENTRY-POINT 16) #.sys.int::+tag-object+)))
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

;;; Support function for APPLY.
;;; Takes a function & a list of arguments.
;;; The function must be a function, but type-checking
;;; will be performed on the argument list.
;;; FIXME: should enforce CALL-ARGUMENTS-LIMIT.
(sys.int::define-lap-function %apply ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  ;; Function goes in RBX.
  (sys.lap-x86:mov64 :rbx :r8)
  ;; Argument count.
  (sys.lap-x86:xor32 :ecx :ecx)
  ;; Words pushed for alignment.
  (sys.lap-x86:xor32 :edi :edi)
  ;; Check for no arguments.
  (sys.lap-x86:cmp64 :r9 nil)
  (sys.lap-x86:je do-call)
  ;; Unpack the list.
  ;; Known to have at least one cons, so we can drop directly into the body.
  (sys.lap-x86:mov64 :r13 :r9)
  unpack-loop
  (:gc :frame :pushed-values-register :rcx)
  ;; Typecheck list, part 2. consp
  (sys.lap-x86:mov8 :al :r13l)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne list-type-error)
  ;; Push car & increment arg count
  (sys.lap-x86:push (:car :r13))
  (:gc :frame :pushed-values-register :rcx :pushed-values 1)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :pushed-values-register :rcx)
  ;; Advance.
  (sys.lap-x86:mov64 :r13 (:cdr :r13))
  ;; Typecheck list, part 1. null
  (sys.lap-x86:cmp64 :r13 nil)
  (sys.lap-x86:jne unpack-loop)
  ;; Arguments have been pushed on the stack in reverse.
  ;; Ensure the stack is misaligned.
  ;; Misalign because 5 registers will be popped off, leaving
  ;; the stack correctly aligned.
  (sys.lap-x86:test64 :rsp 8)
  (sys.lap-x86:jnz stack-aligned)
  ;; Don't push anything extra if there are 5 or fewer args.
  ;; They will all be popped off.
  (sys.lap-x86:cmp32 :ecx #.(ash 5 sys.int::+n-fixnum-bits+)) ; fixnum 5
  (sys.lap-x86:jbe stack-aligned)
  ;; Reversing will put this at the end of the stack, out of the way.
  (sys.lap-x86:push 0)
  (:gc :frame :pushed-values-register :rcx :pushed-values 1)
  (sys.lap-x86:add32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :pushed-values-register :rcx)
  (sys.lap-x86:add32 :edi #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  stack-aligned
  ;; RCX = n arguments. (fixnum)
  ;; RDX = left offset, RAX = right offset.
  (sys.lap-x86:lea32 :eax (:ecx #.(ash -1 sys.int::+n-fixnum-bits+)))
  (sys.lap-x86:shr32 :eax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:shl32 :eax 3) ; * 8
  (sys.lap-x86:xor32 :edx :edx)
  (sys.lap-x86:jmp reverse-test)
  reverse-loop
  ;; Swap stack+rax & stack+rdx
  (sys.lap-x86:mov64 :r8 (:rsp :rax))
  (sys.lap-x86:mov64 :r9 (:rsp :rdx))
  (sys.lap-x86:mov64 (:rsp :rax) :r9)
  (sys.lap-x86:mov64 (:rsp :rdx) :r8)
  ;; Advance offsets.
  (sys.lap-x86:add32 :edx 8)
  (sys.lap-x86:sub32 :eax 8)
  reverse-test
  ;; Stop when RDX > RAX.
  (sys.lap-x86:cmp32 :eax :edx)
  (sys.lap-x86:ja reverse-loop)
  ;; Drop the word pushed for alignment (if any).
  (sys.lap-x86:sub32 :ecx :edi)
  ;; Put arguments into registers.
  ;; Always at least one argument by this point.
  (sys.lap-x86:pop :r8)
  (:gc :frame :pushed-values-register :rcx :pushed-values -1)
  (sys.lap-x86:cmp32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r9)
  (:gc :frame :pushed-values-register :rcx :pushed-values -2)
  (sys.lap-x86:cmp32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r10)
  (:gc :frame :pushed-values-register :rcx :pushed-values -3)
  (sys.lap-x86:cmp32 :ecx #.(ash 3 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r11)
  (:gc :frame :pushed-values-register :rcx :pushed-values -4)
  (sys.lap-x86:cmp32 :ecx #.(ash 4 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:je do-call)
  (sys.lap-x86:pop :r12)
  (:gc :frame :pushed-values-register :rcx :pushed-values -5)
  ;; Everything is ready. Call the function!
  do-call
  ;; If there are 5 or fewer arguments (ie, only register args) the function can be tail-called to.
  (sys.lap-x86:cmp64 :rcx #.(ash 5 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jbe do-tail-call)
  (sys.lap-x86:call (:object :rbx #.sys.int::+function-entry-point+))
  (:gc :frame)
  ;; Finish up & return.
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret)
  do-tail-call
  (:gc :frame)
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:jmp (:object :rbx #.sys.int::+function-entry-point+))
  ;; R8 = function, R9 = arg-list.
  ;; (raise-type-error arg-list 'proper-list)
  list-type-error
  (:gc :frame)
  ;; Make sure that the stack is 16-byte aligned.
  ;; The list unpacking loop has been pushing values one by one.
  (sys.lap-x86:and64 :rsp #.(lognot 15))
  (sys.lap-x86:mov64 :r8 :r9)
  (sys.lap-x86:mov64 :r9 (:constant sys.int::proper-list))
  (sys.lap-x86:mov64 :r13 (:function sys.int::raise-type-error))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:ud2))

(sys.int::define-lap-function sys.int::%copy-words ((destination-address source-address count))
  "Copy COUNT words from SOURCE-ADDRESS to DESTINATION-ADDRESS.
Source & destination must both be byte addresses."
  (sys.lap-x86:mov64 :rdi :r8) ; Destination
  (sys.lap-x86:mov64 :rsi :r9) ; Source
  (sys.lap-x86:mov64 :rcx :r10) ; Count
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+) ; Unbox destination
  (sys.lap-x86:sar64 :rsi #.sys.int::+n-fixnum-bits+) ; Unbox source
  (sys.lap-x86:sar64 :rcx #.sys.int::+n-fixnum-bits+) ; Unbox count
  (sys.lap-x86:rep)
  (sys.lap-x86:movs64)
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%fill-words ((destination-address value count))
  "Store VALUE into COUNT words starting at DESTINATION-ADDRESS.
Destination must a be byte address.
VALUE must be an immediate value (fixnum, character, single-float, NIL or T) or
the GC must be deferred during FILL-WORDS."
  (sys.lap-x86:mov64 :rdi :r8) ; Destination
  (sys.lap-x86:mov64 :rax :r9) ; Value
  (sys.lap-x86:mov64 :rcx :r10) ; Count
  (sys.lap-x86:sar64 :rdi #.sys.int::+n-fixnum-bits+) ; Unbox destination
  (sys.lap-x86:sar64 :rcx #.sys.int::+n-fixnum-bits+) ; Unbox count
  (sys.lap-x86:rep)
  (sys.lap-x86:stos64)
  (sys.lap-x86:ret))

(sys.int::define-lap-function %%make-unsigned-byte-64-rax ()
  (:gc :no-frame :layout #*0)
  ;; Convert to fixnum & check for unsigned overflow.
  ;; Assumes fixnum size of 1!
  (sys.lap-x86:shl64 :rax 1)
  (sys.lap-x86:jc OVERFLOW)
  (sys.lap-x86:js OVERFLOW)
  ;; It's a fixnum.
  (sys.lap-x86:mov64 :r8 :rax)
  ;; Single-value return.
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:ret)
  OVERFLOW
  ;; Call out to bignum builder.
  ;; Undo the shift.
  (sys.lap-x86:rcr64 :rax 1)
  ;; Prod the sign flag.
  (sys.lap-x86:test64 :rax :rax)
  ;; Build bignum.
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-bignum-64-rax))
  ;; Result needs a 128-bit bignum when the high bit is set.
  (sys.lap-x86:cmov64s :r13 (:function sys.int::%%make-bignum-128-rdx-rax))
  (sys.lap-x86:xor32 :edx :edx)
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(sys.int::define-lap-function %%make-signed-byte-64-rax ()
  (:gc :no-frame :layout #*0)
  ;; Convert to fixnum & check for unsigned overflow.
  ;; Assumes fixnum size of 1!
  (sys.lap-x86:shl64 :rax 1)
  (sys.lap-x86:jo OVERFLOW)
  ;; It's a fixnum.
  (sys.lap-x86:mov64 :r8 :rax)
  ;; Single-value return.
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:ret)
  OVERFLOW
  ;; Call out to bignum builder.
  ;; Undo the shift.
  (sys.lap-x86:rcr64 :rax 1)
  ;; Build bignum.
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-bignum-64-rax))
  (sys.lap-x86:xor32 :edx :edx)
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

;; This relies on memory being initialized to zero, so it looks like
;; many simple vectors of length 0.
(sys.int::define-lap-function %do-allocate-from-general-area ((tag data words))
  (:gc :no-frame :layout #*0)
  ;; Attempt to quickly allocate from the general area.
  ;; Returns (values tag data words t) on failure, just the object on success.
  ;; R8 = tag; R9 = data; R10 = words.
  ;; Fetch symbol value cells.
  (sys.lap-x86:mov64 :r13 (:constant sys.int::*general-area-gen0-bump*))
  (sys.lap-x86:mov64 :r13 (:object :r13 #.sys.int::+symbol-value+))
  (sys.lap-x86:mov64 :r11 (:constant sys.int::*general-area-gen0-limit*))
  (sys.lap-x86:mov64 :r11 (:object :r11 #.sys.int::+symbol-value+))
    ;; R13 = bump. R11 = limit.
  ;; Assemble the final header value in RDI.
  (sys.lap-x86:mov64 :rdi :r9)
  (sys.lap-x86:shl64 :rdi #.(- sys.int::+object-data-shift+ sys.int::+n-fixnum-bits+))
  (sys.lap-x86:lea64 :rdi (:rdi (:r8 #.(ash 1 (- sys.int::+object-type-shift+ sys.int::+n-fixnum-bits+)))))
  ;; If a garbage collection occurs, it must rewind IP back here.
  (:gc :no-frame :layout #*0 :restart t)
  ;; Fetch and increment the current bump pointer.
  (sys.lap-x86:lea64 :rbx ((:r10 8))) ; words * 8
  (sys.lap-x86:lock)
  (sys.lap-x86:xadd64 (:object :r13 #.sys.int::+symbol-value-cell-value+) :rbx)
  ;; RBX is old bump pointer, the address of the cons.
  ;; Find the new bump pointer.
  (sys.lap-x86:lea64 :rsi (:rbx (:r10 8)))
    ;; Test against limit.
  (sys.lap-x86:cmp64 :rsi (:object :r11 #.sys.int::+symbol-value-cell-value+))
  (sys.lap-x86:ja SLOW-PATH)
  ;; Generate the object.
  ;; Unfixnumize address. This still looks like a fixnum due to alignment.
  (sys.lap-x86:shr64 :rbx #.sys.int::+n-fixnum-bits+)
  ;; Set address bits and the tag bits.
  ;; Set address bits, tag bits, and the mark bit.
  (sys.lap-x86:mov64 :rax #.(logior (ash sys.int::+address-tag-general+ sys.int::+address-tag-shift+)
                                    (dpb sys.int::+address-generation-0+ sys.int::+address-generation+ 0)
                                    sys.int::+tag-object+))
  (sys.lap-x86:or64 :rbx :rax)
  ;; RBX now points to a 0-element simple-vector, followed by however much empty space is required.
  ;; The gc metadata at this point has :restart t, so if a GC occurs before
  ;; writing the final header, this process will be restarted from the beginning.
  ;; This is required as the GC will only copy 2 words, leaving the rest of the memory in an invalid state.
  ;; Write back the header.
  ;; This must be done in a single write so the GC always sees a correct header.
  (sys.lap-x86:mov64 (:object :rbx -1) :rdi)
  ;; Leave restart region.
  (:gc :no-frame :layout #*0)
  ;; Done. Return everything.
  (sys.lap-x86:mov64 :r8 :rbx)
  (sys.lap-x86:mov32 :ecx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  SLOW-PATH
  (sys.lap-x86:mov64 :r11 t)
  (sys.lap-x86:mov32 :ecx #.(ash 4 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function %allocate-from-general-area ((tag data words))
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  ;; Attempt to quickly allocate from the general area. Will call
  ;; %SLOW-ALLOCATE-FROM-GENERAL-AREA if things get too hairy.
  ;; R8 = tag; R9 = data; R10 = words
  ;; Check argument count.
  (sys.lap-x86:cmp64 :rcx #.(ash 3 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jne SLOW-PATH-BAD-ARGS)
  (:gc :no-frame :layout #*0)
  ;; Update allocation meter.
  ;; *BYTES-CONSED* is updated elsewhere.
  (sys.lap-x86:mov64 :rbx (:constant *general-allocation-count*))
  (sys.lap-x86:mov64 :rbx (:object :rbx #.sys.int::+symbol-value+))
  (sys.lap-x86:lock)
  (sys.lap-x86:add64 (:object :rbx #.sys.int::+symbol-value-cell-value+) #.(ash 1 sys.int::+n-fixnum-bits+))
  ;; Try the real fast allocator.
  (sys.lap-x86:mov64 :r13 (:function %do-allocate-from-general-area))
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:cmp64 :rcx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jne SLOW-PATH)
  ;; Done. Return everything.
  (sys.lap-x86:mov64 :rbx (:constant *general-fast-path-hits*))
  (sys.lap-x86:mov64 :rbx (:object :rbx #.sys.int::+symbol-value+))
  (sys.lap-x86:lock)
  (sys.lap-x86:add64 (:object :rbx #.sys.int::+symbol-value-cell-value+) #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  SLOW-PATH
  ;; Tail call into %SLOW-ALLOCATE-FROM-GENERAL-AREA.
  (sys.lap-x86:mov32 :ecx #.(ash 3 #.sys.int::+n-fixnum-bits+))
  SLOW-PATH-BAD-ARGS
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:mov64 :r13 (:function %slow-allocate-from-general-area))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(sys.int::define-lap-function do-cons ((car cdr))
  (:gc :no-frame :layout #*0)
  ;; Attempt to quickly allocate a cons.
  ;; Returns (values car cdr t) on failure, just the cons on success.
  ;; R8 = car; R9 = cdr
  ;; Fetch symbol value cells.
  (sys.lap-x86:mov64 :r13 (:constant sys.int::*cons-area-gen0-bump*))
  (sys.lap-x86:mov64 :r13 (:object :r13 #.sys.int::+symbol-value+))
  (sys.lap-x86:mov64 :r11 (:constant sys.int::*cons-area-gen0-limit*))
  (sys.lap-x86:mov64 :r11 (:object :r11 #.sys.int::+symbol-value+))
  ;; R13 = bump. R11 = limit.
  (:gc :no-frame :layout #*0 :restart t)
  ;; Fetch and increment the current bump pointer.
  (sys.lap-x86:mov64 :rbx #.(ash 16 #.sys.int::+n-fixnum-bits+)) ; 16, size of cons
  (sys.lap-x86:lock)
  (sys.lap-x86:xadd64 (:object :r13 #.sys.int::+symbol-value-cell-value+) :rbx)
  ;; RBX is old bump pointer, the address of the cons.
  ;; Find the new bump pointer.
  (sys.lap-x86:lea64 :rsi (:rbx #.(ash 16 #.sys.int::+n-fixnum-bits+)))
  ;; Test against limit.
  (sys.lap-x86:cmp64 :rsi (:object :r11 #.sys.int::+symbol-value-cell-value+))
  (sys.lap-x86:ja SLOW-PATH)
  ;; Generate the cons object.
  ;; Unfixnumize address. This still looks like a fixnum due to alignment.
  (sys.lap-x86:shr64 :rbx #.sys.int::+n-fixnum-bits+)
  ;; Set address bits, tag bits, and the mark bit.
  (sys.lap-x86:mov64 :rax #.(logior (ash sys.int::+address-tag-cons+ sys.int::+address-tag-shift+)
                                    (dpb sys.int::+address-generation-0+ sys.int::+address-generation+ 0)
                                    sys.int::+tag-cons+))
  (sys.lap-x86:or64 :rbx :rax)
  ;; RBX now holds a valid cons, with the CAR and CDR set to zero.
  ;; It is safe to leave the restart region.
  (:gc :no-frame :layout #*0)
  ;; Initialize the CAR & CDR outside the restart region to minimise the potential restarts.
  (sys.lap-x86:mov64 (:car :rbx) :r8)
  (sys.lap-x86:mov64 (:cdr :rbx) :r9)
  ;; Done. Return everything.
  (sys.lap-x86:mov64 :r8 :rbx)
  (sys.lap-x86:mov32 :ecx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  SLOW-PATH
  (sys.lap-x86:mov64 :r10 t)
  (sys.lap-x86:mov32 :ecx #.(ash 3 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function cons ((car cdr))
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  ;; Attempt to quickly allocate a cons. Will call SLOW-CONS if things get too hairy.
  ;; R8 = car; R9 = cdr
  ;; Check argument count.
  (sys.lap-x86:cmp64 :rcx #.(ash 2 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jne SLOW-PATH-BAD-ARGS)
  (:gc :no-frame :layout #*0)
  ;; Update allocation meter.
  (sys.lap-x86:mov64 :rbx (:constant *cons-allocation-count*))
  (sys.lap-x86:mov64 :rbx (:object :rbx #.sys.int::+symbol-value+))
  (sys.lap-x86:lock)
  (sys.lap-x86:add64 (:object :rbx #.sys.int::+symbol-value-cell-value+) #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:mov64 :rbx (:constant *bytes-consed*))
  (sys.lap-x86:mov64 :rbx (:object :rbx #.sys.int::+symbol-value+))
  (sys.lap-x86:lock)
  (sys.lap-x86:add64 (:object :rbx #.sys.int::+symbol-value-cell-value+) #.(ash 16 sys.int::+n-fixnum-bits+))
  ;; Check *ENABLE-ALLOCATION-PROFILING*
  (sys.lap-x86:mov64 :rbx (:constant *enable-allocation-profiling*))
  (sys.lap-x86:mov64 :rbx (:object :rbx #.sys.int::+symbol-value+))
  (sys.lap-x86:cmp64 (:object :rbx #.sys.int::+symbol-value-cell-value+) nil)
  (sys.lap-x86:jne SLOW-PATH)
  ;; Check *GC-IN-PROGRESS*.
  (sys.lap-x86:mov64 :rbx (:constant sys.int::*gc-in-progress*))
  (sys.lap-x86:mov64 :rbx (:object :rbx #.sys.int::+symbol-value+))
  (sys.lap-x86:cmp64 (:object :rbx #.sys.int::+symbol-value-cell-value+) nil)
  (sys.lap-x86:jne SLOW-PATH)
  ;; Try the real fast allocator.
  (sys.lap-x86:mov64 :r13 (:function do-cons))
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  (sys.lap-x86:cmp64 :rcx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jne SLOW-PATH)
  ;; Done. Return everything.
  (sys.lap-x86:mov64 :rbx (:constant *cons-fast-path-hits*))
  (sys.lap-x86:mov64 :rbx (:object :rbx #.sys.int::+symbol-value+))
  (sys.lap-x86:lock)
  (sys.lap-x86:add64 (:object :rbx #.sys.int::+symbol-value-cell-value+) #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  SLOW-PATH
  ;; Tail call into SLOW-CONS.
  (sys.lap-x86:mov32 :ecx #.(ash 2 #.sys.int::+n-fixnum-bits+))
  SLOW-PATH-BAD-ARGS
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:mov64 :r13 (:function slow-cons))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))
