;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(sys.int::define-lap-function sys.int::%%make-bignum-64-r10 ()
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :incoming-arguments :rcx :layout #*0)
  (mezzano.lap.arm64:add :x29 :sp :xzr)
  (:gc :frame)
  (mezzano.lap.arm64:stp :x10 :x11 (:pre :sp -16))
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:movz :x0 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x7 (:function sys.int::%make-bignum-of-length))
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:ldp :x10 :x11 (:post :sp 16))
  (mezzano.lap.arm64:str :x10 (:object :x0 0))
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:add :sp :x29 0)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame)
  (mezzano.lap.arm64:ret))
