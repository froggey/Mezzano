;;;; Copyright (c) 2016-2017 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.runtime)

(defun values-list (values)
  (sys.int::values-simple-vector
   (make-array (length values)
               :initial-contents values)))

(sys.int::define-lap-function sys.int::values-simple-vector ((simple-vector))
  "Returns the elements of SIMPLE-VECTOR as multiple values."
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :incoming-arguments :rcx :layout #*0)
  (mezzano.lap.arm64:add :x29 :sp :xzr)
  (:gc :frame)
  ;; Check arg count.
  (mezzano.lap.arm64:subs :xzr :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:b.ne bad-arguments)
  ;; Check type.
  (mezzano.lap.arm64:and :x9 :x0 #b1111)
  (mezzano.lap.arm64:subs :xzr :x9 #.sys.int::+tag-object+)
  (mezzano.lap.arm64:b.ne type-error)
  (mezzano.lap.arm64:ldr :x9 (:object :x0 -1))
  ;; Simple vector object tag is zero.
  (mezzano.lap.arm64:ands :xzr :x9 #.(ash (1- (ash 1 sys.int::+object-type-size+))
                                          sys.int::+object-type-shift+))
  (mezzano.lap.arm64:b.ne type-error)
  ;; Get number of values.
  (mezzano.lap.arm64:adds :x9 :xzr :x9 :lsr #.sys.int::+object-data-shift+)
  (mezzano.lap.arm64:b.eq zero-values)
  (mezzano.lap.arm64:subs :xzr :x9 #.(+ (- mezzano.supervisor::+thread-mv-slots-end+
                                           mezzano.supervisor::+thread-mv-slots-start+)
                                        5))
  (mezzano.lap.arm64:b.cs too-many-values)
  ;; Set up. X6(RBX) = vector, X5(RCX) = number of values loaded so far, X9(RAX) = total number of values.
  (mezzano.lap.arm64:orr :x6 :xzr :x0)
  (mezzano.lap.arm64:orr :x5 :xzr :xzr)
  ;; Load register values.
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x0 (:object :x6 0))
  (mezzano.lap.arm64:subs :xzr :x9 1)
  (mezzano.lap.arm64:b.eq done)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x1 (:object :x6 1))
  (mezzano.lap.arm64:subs :xzr :x9 2)
  (mezzano.lap.arm64:b.eq done)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x2 (:object :x6 2))
  (mezzano.lap.arm64:subs :xzr :x9 3)
  (mezzano.lap.arm64:b.eq done)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x3 (:object :x6 3))
  (mezzano.lap.arm64:subs :xzr :x9 4)
  (mezzano.lap.arm64:b.eq done)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:ldr :x4 (:object :x6 4))
  (mezzano.lap.arm64:subs :xzr :x9 5)
  (mezzano.lap.arm64:b.eq done)
  ;; Registers are populated, now unpack into the MV-area
  (mezzano.lap.arm64:add :x12 :x28 #.(+ (- 8 sys.int::+tag-object+)
                                        (* mezzano.supervisor::+thread-mv-slots-start+ 8)))
  (mezzano.lap.arm64:movz :x10 #.(+ (- 8 sys.int::+tag-object+)
                                    (* 5 8))) ; Current index.
  (mezzano.lap.arm64:movz :x11 5)
  (:gc :frame :multiple-values 0)
  unpack-loop
  (mezzano.lap.arm64:ldr :x7 (:x6 :x10))
  (mezzano.lap.arm64:str :x7 (:x12))
  (:gc :frame :multiple-values 1)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :multiple-values 0)
  (mezzano.lap.arm64:add :x12 :x12 8)
  (mezzano.lap.arm64:add :x10 :x10 8)
  (mezzano.lap.arm64:add :x11 :x11 1)
  (mezzano.lap.arm64:subs :xzr :x11 :x9)
  (mezzano.lap.arm64:b.ne unpack-loop)
  done
  (mezzano.lap.arm64:add :sp :x29 0)
  (:gc :frame :multiple-values 0)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame :multiple-values 0)
  (mezzano.lap.arm64:ret)
  ;; Special-case 0 values as it requires NIL in X0.
  zero-values
  (:gc :frame)
  (mezzano.lap.arm64:orr :x0 :x26 :xzr)
  (mezzano.lap.arm64:orr :x5 :xzr :xzr)
  (mezzano.lap.arm64:b done)
  (:gc :frame)
  type-error
  (mezzano.lap.arm64:ldr :x1 (:constant simple-vector))
  (mezzano.lap.arm64:ldr :x7 (:function sys.int::raise-type-error))
  (mezzano.lap.arm64:movz :x5 #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:hlt 0)
  too-many-values
  (mezzano.lap.arm64:ldr :x0 (:constant "Too many values in simple-vector ~S."))
  (mezzano.lap.arm64:orr :x1 :xzr :x6)
  (mezzano.lap.arm64:ldr :x7 (:function error))
  (mezzano.lap.arm64:movz :x5 #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:hlt 0)
  bad-arguments
  (mezzano.lap.arm64:ldr :x7 (:function sys.int::raise-invalid-argument-error))
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:hlt 0))

(sys.int::define-lap-function %apply ()
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :incoming-arguments :rcx :layout #*0)
  (mezzano.lap.arm64:add :x29 :sp :xzr)
  (:gc :frame)
  ;; Function goes in X6.
  (mezzano.lap.arm64:orr :x6 :xzr :x0)
  ;; Argument count.
  (mezzano.lap.arm64:orr :x5 :xzr :xzr)
  ;; Words pushed for alignment.
  (mezzano.lap.arm64:orr :x12 :xzr :xzr)
  ;; Check for no arguments.
  (mezzano.lap.arm64:subs :xzr :x1 :x26)
  (mezzano.lap.arm64:b.eq DO-CALL)
  ;; Unpack the list.
  ;; Known to have at least one cons, so we can drop directly into the body.
  (mezzano.lap.arm64:orr :x7 :xzr :x1)
  UNPACK-LOOP
  (:gc :frame :pushed-values-register :rcx)
  ;; Typecheck list, part 2. consp
  (mezzano.lap.arm64:and :x9 :x7 #b1111)
  (mezzano.lap.arm64:subs :xzr :x9 #.sys.int::+tag-cons+)
  (mezzano.lap.arm64:b.ne LIST-TYPE-ERROR)
  ;; Push car & increment arg count
  (mezzano.lap.arm64:ldr :x0 (:x7 #.(- #.sys.int::+tag-cons+)))
  (mezzano.lap.arm64:str :x0 (:pre :sp -8))
  (:gc :frame :pushed-values-register :rcx :pushed-values 1)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :pushed-values-register :rcx)
  ;; Advance.
  (mezzano.lap.arm64:ldr :x7 (:x7 #.(+ (- #.sys.int::+tag-cons+) 8)))
  ;; Typecheck list, part 1. null
  (mezzano.lap.arm64:subs :xzr :x7 :x26)
  (mezzano.lap.arm64:b.ne UNPACK-LOOP)
  ;; Arguments have been pushed on the stack in reverse.
  ;; Ensure the stack is misaligned.
  ;; Misalign because 5 registers will be popped off, leaving
  ;; the stack correctly aligned.
  (mezzano.lap.arm64:add :x9 :sp :xzr)
  (mezzano.lap.arm64:ands :xzr :x9 8)
  (mezzano.lap.arm64:b.ne STACK-ALIGNED)
  ;; Don't push anything extra if there are 5 or fewer args.
  ;; They will all be popped off.
  (mezzano.lap.arm64:subs :xzr :x5 #.(ash 5 sys.int::+n-fixnum-bits+)) ; fixnum 5
  (mezzano.lap.arm64:b.ls stack-aligned)
  ;; Reversing will put this at the end of the stack, out of the way.
  (mezzano.lap.arm64:str :xzr (:pre :sp -8))
  (:gc :frame :pushed-values-register :rcx :pushed-values 1)
  (mezzano.lap.arm64:add :x5 :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (:gc :frame :pushed-values-register :rcx)
  (mezzano.lap.arm64:add :x12 :x12 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  STACK-ALIGNED
  ;; X5 = n arguments. (fixnum)
  ;; X10 = left offset, X9 = right offset.
  (mezzano.lap.arm64:sub :x9 :x5 #.(- (ash -1 sys.int::+n-fixnum-bits+)))
  (mezzano.lap.arm64:add :x9 :xzr :x9 :lsr #.sys.int::+n-fixnum-bits+)
  (mezzano.lap.arm64:add :x9 :xzr :x9 :lsl 3) ; * 8
  (mezzano.lap.arm64:orr :x10 :xzr :xzr)
  (mezzano.lap.arm64:b REVERSE-TEST)
  REVERSE-LOOP
  ;; Swap stack+x9 & stack+x10
  (mezzano.lap.arm64:ldr :x0 (:sp :x9))
  (mezzano.lap.arm64:ldr :x1 (:sp :x10))
  (mezzano.lap.arm64:str :x1 (:sp :x9))
  (mezzano.lap.arm64:str :x0 (:sp :x10))
  ;; Advance offsets.
  (mezzano.lap.arm64:add :x10 :x10 8)
  (mezzano.lap.arm64:sub :x9 :x9 8)
  REVERSE-TEST
  ;; Stop when X10 > X9.
  (mezzano.lap.arm64:subs :xzr :x9 :x10)
  (mezzano.lap.arm64:b.hi REVERSE-LOOP)
  ;; Drop the word pushed for alignment (if any).
  (mezzano.lap.arm64:sub :x5 :x5 :x12)
  ;; Put arguments into registers.
  ;; Always at least one argument by this point.
  (mezzano.lap.arm64:ldr :x0 (:post :sp 8))
  (:gc :frame :pushed-values-register :rcx :pushed-values -1)
  (mezzano.lap.arm64:subs :xzr :x5 #.(ash 1 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:b.eq do-call)
  (mezzano.lap.arm64:ldr :x1 (:post :sp 8))
  (:gc :frame :pushed-values-register :rcx :pushed-values -2)
  (mezzano.lap.arm64:subs :xzr :x5 #.(ash 2 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:b.eq do-call)
  (mezzano.lap.arm64:ldr :x2 (:post :sp 8))
  (:gc :frame :pushed-values-register :rcx :pushed-values -3)
  (mezzano.lap.arm64:subs :xzr :x5 #.(ash 3 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:b.eq do-call)
  (mezzano.lap.arm64:ldr :x3 (:post :sp 8))
  (:gc :frame :pushed-values-register :rcx :pushed-values -4)
  (mezzano.lap.arm64:subs :xzr :x5 #.(ash 4 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:b.eq do-call)
  (mezzano.lap.arm64:ldr :x4 (:post :sp 8))
  (:gc :frame :pushed-values-register :rcx :pushed-values -5)
  ;; Everything is ready. Call the function!
  DO-CALL
  ;; If there are 5 or fewer arguments (ie, only register args) the function can be tail-called to.
  (mezzano.lap.arm64:subs :xzr :x5 #.(ash 5 sys.int::+n-fixnum-bits+))
  (mezzano.lap.arm64:b.ls DO-TAIL-CALL)
  (mezzano.lap.arm64:ldr :x9 (:object :x6 0))
  (mezzano.lap.arm64:blr :x9)
  (:gc :frame)
  ;; Finish up & return.
  (mezzano.lap.arm64:add :sp :x29 0)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame)
  (mezzano.lap.arm64:ret)
  DO-TAIL-CALL
  (:gc :frame)
  (mezzano.lap.arm64:add :sp :x29 0)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame)
  (mezzano.lap.arm64:ldr :x9 (:object :x6 0))
  (mezzano.lap.arm64:br :x9)
  ;; X0 = function, X1 = arg-list.
  ;; (raise-type-error arg-list 'proper-list)
  LIST-TYPE-ERROR
  (:gc :frame)
  ;; Make sure that the stack is 16-byte aligned.
  ;; The list unpacking loop has been pushing values one by one.
  (mezzano.lap.arm64:add :x9 :sp :xzr)
  (mezzano.lap.arm64:and :x9 :x9 #.(lognot 15))
  (mezzano.lap.arm64:add :sp :x9 :xzr)
  (mezzano.lap.arm64:orr :x0 :xzr :x1)
  (mezzano.lap.arm64:ldr :x1 (:constant sys.int::proper-list))
  (mezzano.lap.arm64:ldr :x7 (:function sys.int::raise-type-error))
  (mezzano.lap.arm64:movz :x5 #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:hlt 0))

(defun eql (x y)
  (or (eq x y)
      (and (sys.int::%value-has-tag-p x sys.int::+tag-object+)
           (sys.int::%value-has-tag-p y sys.int::+tag-object+)
           (eq (sys.int::%object-tag x) (sys.int::%object-tag y))
           (<= sys.int::+first-numeric-object-tag+
               (sys.int::%object-tag x)
               sys.int::+last-numeric-object-tag+)
           (= x y))))

(declaim (inline sys.int::%value-has-tag-p))
(defun sys.int::%value-has-tag-p (value tag)
  (eq (sys.int::%tag-field value) tag))

(declaim (inline sys.int::%type-check))
(defun sys.int::%type-check (object object-tag expected-type)
  (unless (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
               (eq (sys.int::%object-tag object) object-tag))
    (sys.int::raise-type-error object expected-type)))

(declaim (inline lognot))
(defun lognot (integer)
  (logxor integer -1))

;; ARM makes it difficult to detect overflow when shifting left.
(declaim (inline %fixnum-left-shift))
(defun %fixnum-left-shift (integer count)
  (dotimes (i count)
     (setf integer (+ integer integer)))
  integer)

(declaim (inline sys.int::%simple-1d-array-p))
(defun sys.int::%simple-1d-array-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (<= (sys.int::%object-tag object) sys.int::+last-simple-1d-array-object-tag+)))

(declaim (inline simple-vector-p))
(defun simple-vector-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (eq (sys.int::%object-tag object) sys.int::+object-tag-array-t+)))

(declaim (inline sys.int::character-array-p))
(defun sys.int::character-array-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (let ((tag (sys.int::%object-tag object)))
         (or (eq tag sys.int::+object-tag-simple-string+)
             (eq tag sys.int::+object-tag-string+)))))

(declaim (inline arrayp))
(defun arrayp (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (<= (sys.int::%object-tag object) sys.int::+last-complex-array-object-tag+)))

(declaim (inline sys.int::complex-array-p))
(defun sys.int::complex-array-p (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (<= sys.int::+first-complex-array-object-tag+
           (sys.int::%object-tag object)
           sys.int::+last-complex-array-object-tag+)))

(declaim (inline sys.int::%bounds-check))
(defun sys.int::%bounds-check (object slot)
  (unless (sys.int::fixnump slot)
    (sys.int::raise-type-error slot 'fixnum))
  (unless (< slot (sys.int::%object-header-data object))
    (sys.int::raise-bounds-error object slot)))

(declaim (inline characterp))
(defun characterp (object)
  (sys.int::%value-has-tag-p object sys.int::+tag-character+))

(declaim (inline rem))
(defun rem (number divisor)
  (multiple-value-bind (quot rem)
      (truncate number divisor)
    (declare (ignore quot))
    rem))

(declaim (inline functionp))
(defun functionp (object)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (<= sys.int::+first-function-object-tag+
           (sys.int::%object-tag object)
           sys.int::+last-function-object-tag+)))

(declaim (inline sys.int::%object-of-type-p))
(defun sys.int::%object-of-type-p (object object-tag)
  (and (sys.int::%value-has-tag-p object sys.int::+tag-object+)
       (eq (sys.int::%object-tag object) object-tag)))

(defun sys.int::%copy-words (destination-address source-address count)
  (dotimes (i count)
    (setf (sys.int::memref-t destination-address i)
          (sys.int::memref-t source-address i))))

(defun sys.int::%fill-words (destination-address value count)
  (dotimes (i count)
    (setf (sys.int::memref-t destination-address i) value)))
