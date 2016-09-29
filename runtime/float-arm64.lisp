;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(sys.int::define-lap-function sys.int::%single-float-as-integer ((single-float))
  (mezzano.lap.arm64:add :x9 :xzr :x0 :lsr 32)
  (mezzano.lap.arm64:add :x0 :xzr :x9 :lsl #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function sys.int::%integer-as-single-float ((integer))
  (mezzano.lap.arm64:add :x0 :xzr :x0 :lsl #.(- 32 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:add :x0 :x0 #.sys.int::+tag-single-float+)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(macrolet ((def (name op)
             `(sys.int::define-lap-function ,name ((x y))
                ;; Unbox the floats.
                (mezzano.lap.arm64:add :x9 :xzr :x0 :lsr 32)
                (mezzano.lap.arm64:add :x10 :xzr :x1 :lsr 32)
                ;; Load into Single registers.
                (mezzano.lap.arm64:fmov :s0 :w9)
                (mezzano.lap.arm64:fmov :s1 :w10)
                ;; Operate.
                (,op :s0 :s0 :s1)
                ;; Box result & return.
                (mezzano.lap.arm64:fmov :w9 :s0)
                (mezzano.lap.arm64:add :x9 :xzr :x9 :lsl 32)
                (mezzano.lap.arm64:add :x0 :x9 #.sys.int::+tag-single-float+)
                (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
                (mezzano.lap.arm64:ret))))
  (def sys.int::%%single-float-+ mezzano.lap.arm64:fadd)
  (def sys.int::%%single-float-- mezzano.lap.arm64:fsub)
  (def sys.int::%%single-float-* mezzano.lap.arm64:fmul)
  (def sys.int::%%single-float-/ mezzano.lap.arm64:fdiv))

(sys.int::define-lap-function sys.int::%%single-float-< ()
  ;; Unbox the floats.
  (mezzano.lap.arm64:add :x9 :xzr :x0 :lsr 32)
  (mezzano.lap.arm64:add :x10 :xzr :x1 :lsr 32)
  ;; Load into Single registers.
  (mezzano.lap.arm64:fmov :s0 :w9)
  (mezzano.lap.arm64:fmov :s1 :w10)
  ;; Compare.
  (mezzano.lap.arm64:fcmp :s0 :s1)
  (mezzano.lap.arm64:ldr :x0 (:constant t))
  (mezzano.lap.arm64:csel.cc :x0 :x0 :x26)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))

(sys.int::define-lap-function sys.int::%%single-float-= ()
  ;; Unbox the floats.
  (mezzano.lap.arm64:add :x9 :xzr :x0 :lsr 32)
  (mezzano.lap.arm64:add :x10 :xzr :x1 :lsr 32)
  ;; Load into Single registers.
  (mezzano.lap.arm64:fmov :s0 :w9)
  (mezzano.lap.arm64:fmov :s1 :w10)
  ;; Compare.
  (mezzano.lap.arm64:fcmp :s0 :s1)
  (mezzano.lap.arm64:ldr :x0 (:constant t))
  (mezzano.lap.arm64:csel.eq :x0 :x0 :x26)
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:ret))
