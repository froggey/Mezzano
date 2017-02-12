;;;; Copyright (c) 2011-2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(sys.int::define-lap-function sys.int::%single-float-as-integer ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:shl64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:mov64 :r8 :rax)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%integer-as-single-float ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function %%coerce-fixnum-to-single-float ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:cvtsi2ss64 :xmm0 :rax)
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function %%coerce-double-float-to-single-float ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:cvtsd2ss64 :xmm0 (:object :r8 0))
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function %%coerce-fixnum-to-double-float ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:sar64 :rax #.sys.int::+n-fixnum-bits+)
  (sys.lap-x86:cvtsi2sd64 :xmm0 :rax)
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(sys.int::define-lap-function %%coerce-single-float-to-double-float ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:cvtss2sd64 :xmm0 :xmm0)
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(sys.int::define-lap-function sys.int::%%single-float-< ()
  (:gc :no-frame :layout #*0)
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Compare.
  (sys.lap-x86:ucomiss :xmm0 :xmm1)
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:mov64 :r9 t)
  (sys.lap-x86:cmov64b :r8 :r9)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%double-float-< ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:ucomisd :xmm0 (:object :r9 0))
  (sys.lap-x86:mov64 :r8 nil)
  (sys.lap-x86:mov64 :r9 t)
  (sys.lap-x86:cmov64b :r8 :r9)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%single-float-= ()
  (:gc :no-frame :layout #*0)
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Compare.
  (sys.lap-x86:ucomiss :xmm0 :xmm1)
  (sys.lap-x86:mov64 :r8 t)
  (sys.lap-x86:mov64 :r9 nil)
  ;; If the P bit is set then the values are unorderable.
  (sys.lap-x86:cmov64p :r8 :r9)
  (sys.lap-x86:cmov64ne :r8 :r9)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%double-float-= ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:ucomisd :xmm0 (:object :r9 0))
  (sys.lap-x86:mov64 :r8 t)
  (sys.lap-x86:mov64 :r9 nil)
  ;; If the P bit is set then the values are unorderable.
  (sys.lap-x86:cmov64p :r8 :r9)
  (sys.lap-x86:cmov64ne :r8 :r9)
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%truncate-single-float ()
  (:gc :no-frame :layout #*0)
  ;; Unbox the float.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  ;; Convert to unboxed integer.
  (sys.lap-x86:cvttss2si64 :rax :xmm0)
  ;; Box fixnum.
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 sys.int::+n-fixnum-bits+))))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%truncate-double-float ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  ;; Convert to unboxed integer.
  (sys.lap-x86:cvttsd2si64 :rax :xmm0)
  ;; Box fixnum.
  (sys.lap-x86:lea64 :r8 ((:rax #.(ash 1 sys.int::+n-fixnum-bits+))))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%single-float-/ ()
  (:gc :no-frame :layout #*0)
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Divide.
  (sys.lap-x86:divss :xmm0 :xmm1)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%double-float-/ ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:divsd :xmm0 (:object :r9 0))
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(sys.int::define-lap-function sys.int::%%single-float-+ ()
  (:gc :no-frame :layout #*0)
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Add.
  (sys.lap-x86:addss :xmm0 :xmm1)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%double-float-+ ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:addsd :xmm0 (:object :r9 0))
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(sys.int::define-lap-function sys.int::%%single-float-- ()
  (:gc :no-frame :layout #*0)
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Subtract.
  (sys.lap-x86:subss :xmm0 :xmm1)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%double-float-- ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:subsd :xmm0 (:object :r9 0))
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(sys.int::define-lap-function sys.int::%%single-float-* ()
  (:gc :no-frame :layout #*0)
  ;; Unbox the floats.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  (sys.lap-x86:mov64 :rdx :r9)
  (sys.lap-x86:shr64 :rdx 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  (sys.lap-x86:movd :xmm1 :edx)
  ;; Multiply.
  (sys.lap-x86:mulss :xmm0 :xmm1)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%double-float-* ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:mulsd :xmm0 (:object :r9 0))
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(sys.int::define-lap-function sys.int::%%single-float-sqrt ()
  (:gc :no-frame :layout #*0)
  ;; Unbox the float.
  (sys.lap-x86:mov64 :rax :r8)
  (sys.lap-x86:shr64 :rax 32)
  ;; Load into XMM registers.
  (sys.lap-x86:movd :xmm0 :eax)
  ;; Sqrt.
  (sys.lap-x86:sqrtss :xmm0 :xmm0)
  ;; Box.
  (sys.lap-x86:movd :eax :xmm0)
  (sys.lap-x86:shl64 :rax 32)
  (sys.lap-x86:lea64 :r8 (:rax #.sys.int::+tag-single-float+))
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+))
  (sys.lap-x86:ret))

(sys.int::define-lap-function sys.int::%%double-float-sqrt ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:movq :xmm0 (:object :r8 0))
  (sys.lap-x86:sqrtsd :xmm0 :xmm0)
  (sys.lap-x86:movq :rax :xmm0)
  (sys.lap-x86:mov64 :r13 (:function sys.int::%%make-double-float-rax))
  (sys.lap-x86:jmp (:object :r13 #.sys.int::+fref-entry-point+)))

(sys.int::define-lap-function sys.int::%%make-double-float-rax ()
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:push :rbp)
  (:gc :no-frame :layout #*00)
  (sys.lap-x86:mov64 :rbp :rsp)
  (:gc :frame)
  (sys.lap-x86:push 0)
  (sys.lap-x86:push :rax)
  (sys.lap-x86:mov64 :rcx #.(ash 4 sys.int::+n-fixnum-bits+)) ; fixnum 4
  ;; Tag.
  (sys.lap-x86:mov64 :r8 #.(ash sys.int::+object-tag-double-float+
                                sys.int::+n-fixnum-bits+))
  ;; Header data.
  (sys.lap-x86:xor64 :r9 :r9)
  ;; Words.
  (sys.lap-x86:mov64 :r10 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Area
  (sys.lap-x86:mov64 :r11 nil)
  (sys.lap-x86:mov64 :r13 (:function %allocate-object))
  ;; Allocate object.
  (sys.lap-x86:call (:object :r13 #.sys.int::+fref-entry-point+))
  ;; Set data.
  (sys.lap-x86:pop (:object :r8 0))
  ;; Single-value return.
  (sys.lap-x86:mov32 :ecx #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (sys.lap-x86:leave)
  (:gc :no-frame :layout #*0)
  (sys.lap-x86:ret))
