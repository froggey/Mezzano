;;;; x86-64 runtime support functions.

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
                               (* mezzano.supervisor::+thread-mv-slots+ 8)))
  (:gc :frame :layout #*10 :multiple-values 0)
  unpack-loop
  (sys.lap-x86:cmp64 :rbx nil)
  (sys.lap-x86:je done)
  (sys.lap-x86:mov8 :al :bl)
  (sys.lap-x86:and8 :al #b1111)
  (sys.lap-x86:cmp8 :al #.sys.int::+tag-cons+)
  (sys.lap-x86:jne type-error)
  (sys.lap-x86:cmp32 :ecx #.(ash (+ mezzano.supervisor::+thread-mv-slots-size+ 5) sys.int::+n-fixnum-bits+))
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
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:named-call sys.int::raise-type-error))
  (sys.lap-x86:ud2)
  too-many-values
  (sys.lap-x86:mov64 :r8 (:constant "Too many values in list ~S."))
  (sys.lap-x86:mov64 :r9 (:stack 0))
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:named-call error))
  (sys.lap-x86:ud2)
  bad-arguments
  (:gc :frame :incoming-arguments :rcx)
  (sys.lap-x86:lea64 :rbx (:rip (+ (- ENTRY-POINT 16) #.sys.int::+tag-object+)))
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:jmp (:named-call sys.int::raise-invalid-argument-error)))

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
  (sys.lap-x86:test8 :al :al)
  (sys.lap-x86:jnz type-error)
  ;; Get number of values.
  (sys.lap-x86:shr64 :rax #.sys.int::+object-data-shift+)
  (sys.lap-x86:jz zero-values)
  (sys.lap-x86:cmp64 :rax #.(+ mezzano.supervisor::+thread-mv-slots-size+ 5))
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
                               (* mezzano.supervisor::+thread-mv-slots+ 8)))
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
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:named-call sys.int::raise-type-error))
  (sys.lap-x86:ud2)
  too-many-values
  (sys.lap-x86:mov64 :r8 (:constant "Too many values in simple-vector ~S."))
  (sys.lap-x86:mov64 :r9 :rbx)
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:named-call error))
  (sys.lap-x86:ud2)
  bad-arguments
  (:gc :frame :incoming-arguments :rcx)
  (sys.lap-x86:lea64 :rbx (:rip (+ (- ENTRY-POINT 16) #.sys.int::+tag-object+)))
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:jmp (:named-call sys.int::raise-invalid-argument-error)))

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
  ;; Check arg count.
  (sys.lap-x86:cmp64 :rcx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:jne BAD-ARGUMENTS)
  (:gc :no-frame :layout #*0)
  (:debug ((x :r8 :value) (y :r9 :value)))
  ;; EQ test.
  ;; This additionally covers fixnums, characters and single-floats.
  (sys.lap-x86:cmp64 :r8 :r9)
  (sys.lap-x86:jne MAYBE-NUMBER-CASE)
  ;; Objects are EQ.
  (:debug ())
  OBJECTS-EQUAL
  (sys.lap-x86:mov32 :r8d t)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  MAYBE-NUMBER-CASE
  (:debug ((x :r8 :value) (y :r9 :value)))
  ;; Not EQ.
  ;; Both must be objects.
  (sys.lap-x86:lea32 :eax (:r8 #.(- sys.int::+tag-object+)))
  (sys.lap-x86:test8 :al #b1111)
  (sys.lap-x86:jnz OBJECTS-UNEQUAL)
  (sys.lap-x86:lea32 :eax (:r9 #.(- sys.int::+tag-object+)))
  (sys.lap-x86:test8 :al #b1111)
  (sys.lap-x86:jnz OBJECTS-UNEQUAL)
  ;; Both are objects.
  ;; Test that both are the same kind of object.
  (sys.lap-x86:mov8 :al (:object :r8 -1))
  (sys.lap-x86:cmp8 (:object :r9 -1) :al)
  (sys.lap-x86:jne OBJECTS-UNEQUAL)
  ;; They must be numbers. Characters were handled above.
  (sys.lap-x86:sub8 :al #.(ash sys.int::+first-numeric-object-tag+
                               sys.int::+object-type-shift+))
  (sys.lap-x86:cmp8 :al #.(ash (- sys.int::+last-numeric-object-tag+
                                  sys.int::+first-numeric-object-tag+)
                               sys.int::+object-type-shift+))
  (sys.lap-x86:ja OBJECTS-UNEQUAL)
  ;; Both are numbers of the same type.
  ;; Handle short-floats and double-floats specifically. They have
  ;; different behaviour for negative 0.0 compared to =
  (sys.lap-x86:cmp8 :al #.(ash (- sys.int::+object-tag-double-float+
                                  sys.int::+first-numeric-object-tag+)
                               sys.int::+object-type-shift+))
  (sys.lap-x86:je COMPARE-DOUBLE-FLOATS)
  (sys.lap-x86:cmp8 :al #.(ash (- sys.int::+object-tag-short-float+
                                  sys.int::+first-numeric-object-tag+)
                               sys.int::+object-type-shift+))
  (sys.lap-x86:je COMPARE-SHORT-FLOATS)
  ;; Same for short floats
  ;; Tail-call to generic-=.
  ;; RCX was set to fixnum 2 on entry.
  (sys.lap-x86:jmp (:named-call sys.int::generic-=))
  ;; Compare the two values directly.
  ;; This means +0.0 and -0.0 will be different and that NaNs can be EQL
  ;; if they have the same representation.
  COMPARE-SHORT-FLOATS
  (sys.lap-x86:mov16 :ax (:object :r8 0))
  (sys.lap-x86:cmp16 :ax (:object :r9 0))
  (sys.lap-x86:je OBJECTS-EQUAL)
  (sys.lap-x86:jmp OBJECTS-UNEQUAL)
  COMPARE-DOUBLE-FLOATS
  (sys.lap-x86:mov64 :rax (:object :r8 0))
  (sys.lap-x86:cmp64 :rax (:object :r9 0))
  (sys.lap-x86:je OBJECTS-EQUAL)
  OBJECTS-UNEQUAL
  ;; Objects are not EQL.
  (:debug ())
  (sys.lap-x86:mov32 :r8d nil)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  BAD-ARGUMENTS
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:lea64 :rbx (:rip (+ (- ENTRY-POINT 16) #.sys.int::+tag-object+)))
  (sys.lap-x86:jmp (:named-call sys.int::raise-invalid-argument-error)))

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
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (sys.lap-x86:call (:named-call sys.int::raise-type-error))
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
  ;; Result needs a 128-bit bignum when the high bit is set.
  (sys.lap-x86:test64 :rax :rax)
  ;; Build bignum.
  (sys.lap-x86:js BIG128)
  (sys.lap-x86:jmp (:named-call sys.int::%%make-bignum-64-rax))
  BIG128
  (sys.lap-x86:xor32 :edx :edx)
  (sys.lap-x86:jmp (:named-call sys.int::%%make-bignum-128-rdx-rax)))

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
  (sys.lap-x86:jmp (:named-call sys.int::%%make-bignum-64-rax)))

;; This relies on memory being initialized to zero, so it looks like
;; many simple vectors of length 0.
(sys.int::define-lap-function %do-allocate-from-general-area ((tag data words))
  (:gc :no-frame :layout #*0)
  ;; Attempt to quickly allocate from the general area.
  ;; Returns (values tag data words t) on failure, just the object on success.
  ;; R8 = tag; R9 = data; R10 = words.
  ;; Fetch symbol value cells.
  (sys.lap-x86:mov64 :r13 (:symbol-global-cell sys.int::*general-area-young-gen-bump*))
  (sys.lap-x86:mov64 :r12 (:symbol-global-cell sys.int::*young-gen-newspace-bit-raw*))
  (sys.lap-x86:mov64 :r11 (:symbol-global-cell sys.int::*general-area-young-gen-limit*))
    ;; R13 = bump. R12 = newspace-bit. R11 = limit.
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
                                    sys.int::+tag-object+))
  (sys.lap-x86:or64 :rbx :rax)
  (sys.lap-x86:or64 :rbx (:object :r12 #.sys.int::+symbol-value-cell-value+))
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
  (sys.lap-x86:mov64 :rbx (:symbol-global-cell *general-allocation-count*))
  (sys.lap-x86:lock)
  (sys.lap-x86:add64 (:object :rbx #.sys.int::+symbol-value-cell-value+) #.(ash 1 sys.int::+n-fixnum-bits+))
  ;; Try the real fast allocator.
  (sys.lap-x86:call (:named-call %do-allocate-from-general-area))
  (sys.lap-x86:cmp64 :rcx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jne SLOW-PATH)
  ;; Done. Return everything.
  (sys.lap-x86:mov64 :rbx (:symbol-global-cell *general-fast-path-hits*))
  (sys.lap-x86:lock)
  (sys.lap-x86:add64 (:object :rbx #.sys.int::+symbol-value-cell-value+) #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  SLOW-PATH
  ;; Tail call into %SLOW-ALLOCATE-FROM-GENERAL-AREA.
  (sys.lap-x86:mov32 :ecx #.(ash 3 #.sys.int::+n-fixnum-bits+))
  SLOW-PATH-BAD-ARGS
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:jmp (:named-call %slow-allocate-from-general-area)))

(sys.int::define-lap-function do-cons ((car cdr))
  (:gc :no-frame :layout #*0)
  ;; Attempt to quickly allocate a cons.
  ;; Returns (values car cdr t) on failure, just the cons on success.
  ;; R8 = car; R9 = cdr
  ;; Fetch symbol value cells.
  (sys.lap-x86:mov64 :r13 (:symbol-global-cell sys.int::*cons-area-young-gen-bump*))
  (sys.lap-x86:mov64 :r12 (:symbol-global-cell sys.int::*young-gen-newspace-bit-raw*))
  (sys.lap-x86:mov64 :r11 (:symbol-global-cell sys.int::*cons-area-young-gen-limit*))
  ;; R13 = bump. R12 = newspace-bit. R11 = limit.
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
                                    sys.int::+tag-cons+))
  (sys.lap-x86:or64 :rbx :rax)
  (sys.lap-x86:or64 :rbx (:object :r12 #.sys.int::+symbol-value-cell-value+))
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
  (sys.lap-x86:mov64 :rbx (:symbol-global-cell *cons-allocation-count*))
  (sys.lap-x86:lock)
  (sys.lap-x86:add64 (:object :rbx #.sys.int::+symbol-value-cell-value+) #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:mov64 :rbx (:symbol-global-cell *bytes-consed*))
  (sys.lap-x86:lock)
  (sys.lap-x86:add64 (:object :rbx #.sys.int::+symbol-value-cell-value+)
                     #.(ash 16 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:gs)
  (sys.lap-x86:add64 (:object nil #.mezzano.supervisor::+thread-bytes-consed+)
                     #.(ash 16 sys.int::+n-fixnum-bits+))
  ;; Check *ENABLE-ALLOCATION-PROFILING*
  (sys.lap-x86:mov64 :rbx (:symbol-global-cell *enable-allocation-profiling*))
  (sys.lap-x86:cmp64 (:object :rbx #.sys.int::+symbol-value-cell-value+) nil)
  (sys.lap-x86:jne SLOW-PATH)
  ;; Check *GC-IN-PROGRESS*.
  (sys.lap-x86:mov64 :rbx (:symbol-global-cell sys.int::*gc-in-progress*))
  (sys.lap-x86:cmp64 (:object :rbx #.sys.int::+symbol-value-cell-value+) nil)
  (sys.lap-x86:jne SLOW-PATH)
  ;; Try the real fast allocator.
  (sys.lap-x86:call (:named-call do-cons))
  (sys.lap-x86:cmp64 :rcx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jne SLOW-PATH)
  ;; Done. Return everything.
  (sys.lap-x86:mov64 :rbx (:symbol-global-cell *cons-fast-path-hits*))
  (sys.lap-x86:lock)
  (sys.lap-x86:add64 (:object :rbx #.sys.int::+symbol-value-cell-value+) #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 #.sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  SLOW-PATH
  ;; Tail call into SLOW-CONS.
  (sys.lap-x86:mov32 :ecx #.(ash 2 #.sys.int::+n-fixnum-bits+))
  SLOW-PATH-BAD-ARGS
  (:gc :no-frame :layout #*0 :incoming-arguments :rcx)
  (sys.lap-x86:jmp (:named-call slow-cons)))

(declaim (inline sys.int::msr (setf sys.int::msr)))
(defun sys.int::msr (register)
  (check-type register (unsigned-byte 32))
  (sys.int::%msr register))
(defun (setf sys.int::msr) (value register)
  (check-type register (unsigned-byte 32))
  (check-type value (unsigned-byte 64))
  (setf (sys.int::%msr register) value))

(declaim (inline sys.int::io-port/8 (setf sys.int::io-port/8)))
(defun sys.int::io-port/8 (port)
  (check-type port (unsigned-byte 16))
  (sys.int::%io-port/8 port))
(defun (setf sys.int::io-port/8) (value port)
  (check-type port (unsigned-byte 16))
  (check-type value (unsigned-byte 8))
  (setf (sys.int::%io-port/8 port) value))

(declaim (inline sys.int::io-port/16 (setf sys.int::io-port/16)))
(defun sys.int::io-port/16 (port)
  (check-type port (unsigned-byte 16))
  (sys.int::%io-port/16 port))
(defun (setf sys.int::io-port/16) (value port)
  (check-type port (unsigned-byte 16))
  (check-type value (unsigned-byte 16))
  (setf (sys.int::%io-port/16 port) value))

(declaim (inline sys.int::io-port/32 (setf sys.int::io-port/32)))
(defun sys.int::io-port/32 (port)
  (check-type port (unsigned-byte 16))
  (sys.int::%io-port/32 port))
(defun (setf sys.int::io-port/32) (value port)
  (check-type port (unsigned-byte 16))
  (check-type value (unsigned-byte 32))
  (setf (sys.int::%io-port/32 port) value))

(sys.int::define-lap-function %fixnum-left-shift ((integer count))
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:cmp64 :r9 #.(ash (- 63 sys.int::+n-fixnum-bits+)
                                sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ja DO-BIG-SHIFT)
  ;; Sign extend INTEGER into :RDX.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:cqo)
  (sys.lap-x86:mov64 :rsi :rdx)
  (sys.lap-x86:mov32 :ecx :r9d)
  (sys.lap-x86:shr32 :ecx #.sys.int::+n-fixnum-bits+) ; unbox fixnum count
  ;; Check for overflow.
  ;; Shift bits from INTEGER into :RSI and check if it stops matching the
  ;; extended sign.
  ;; TODO: This could be cleverer, do the shift and construct the bignum.
  (sys.lap-x86:shld64 :rsi :rax :cl) ; High bits
  (sys.lap-x86:cmp64 :rsi :rdx)
  (sys.lap-x86:jne DO-BIG-SHIFT) ; overflow occured.
  (sys.lap-x86:shl64 :rax :cl) ; Do the actual shift.
  (sys.lap-x86:cqo)
  (sys.lap-x86:cmp64 :rsi :rdx)
  (sys.lap-x86:jne DO-BIG-SHIFT) ; overflow occured.
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret)
  ;; Bail out, call the helper.
  DO-BIG-SHIFT
  (sys.lap-x86:mov32 :ecx #.(ash 2 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:jmp (:named-call %fixnum-left-shift-slow)))

(defun %fixnum-left-shift-slow (integer count)
  (dotimes (i count integer)
    (setf integer (+ integer integer))))

;; Avoid a trip through FUNCTION-REFERENCE.
(sys.int::define-lap-function sys.int::get-raise-undefined-function-fref (())
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :r8 (:function sys.int::raise-undefined-function))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))
