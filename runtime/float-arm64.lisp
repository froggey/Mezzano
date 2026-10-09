;;;; ARM64 float support functions

(in-package :mezzano.runtime)

(sys.int::define-lap-function sys.int::%%make-double-float-x10 ()
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:add :x29 :sp :xzr)
  (:gc :frame)
  (mezzano.lap.arm64:stp :x10 :x11 (:pre :sp -16))
  (mezzano.lap.arm64:movz :x5 #.(ash 4 sys.int::+n-fixnum-bits+)) ; fixnum 4
  ;; Tag.
  (mezzano.lap.arm64:movz :x0 #.(ash sys.int::+object-tag-double-float+
                                     sys.int::+n-fixnum-bits+))
  ;; Header data.
  (mezzano.lap.arm64:orr :x1 :xzr :xzr)
  ;; Words.
  (mezzano.lap.arm64:movz :x2 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  ;; Area
  (mezzano.lap.arm64:orr :x3 :xzr :x26)
  ;; Allocate object.
  (mezzano.lap.arm64:named-call %allocate-object)
  ;; Set data.
  (mezzano.lap.arm64:ldp :x10 :x11 (:post :sp 16))
  (mezzano.lap.arm64:str :x10 (:object :x0 0))
  ;; Single-value return.
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:add :sp :x29 0)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :layout #*)
  (mezzano.lap.arm64:ret))
