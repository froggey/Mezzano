;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :sys.int)

(sys.int::define-lap-function sys.int::%%make-bignum-64-x10 ()
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

(sys.int::define-lap-function sys.int::%%make-bignum-128-x10-x11 ()
  (mezzano.lap.arm64:stp :x29 :x30 (:pre :sp -16))
  (:gc :no-frame :incoming-arguments :rcx :layout #*0)
  (mezzano.lap.arm64:add :x29 :sp :xzr)
  (:gc :frame)
  (mezzano.lap.arm64:stp :x10 :x11 (:pre :sp -16))
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:movz :x0 #.(ash 2 sys.int::+n-fixnum-bits+)) ; fixnum 2
  (mezzano.lap.arm64:ldr :x7 (:function sys.int::%make-bignum-of-length))
  (mezzano.lap.arm64:ldr :x9 (:object :x7 #.sys.int::+fref-entry-point+))
  (mezzano.lap.arm64:blr :x9)
  (mezzano.lap.arm64:ldp :x10 :x11 (:post :sp 16))
  (mezzano.lap.arm64:str :x10 (:object :x0 0))
  (mezzano.lap.arm64:str :x11 (:object :x0 1))
  (mezzano.lap.arm64:movz :x5 #.(ash 1 sys.int::+n-fixnum-bits+)) ; fixnum 1
  (mezzano.lap.arm64:add :sp :x29 0)
  (mezzano.lap.arm64:ldp :x29 :x30 (:post :sp 16))
  (:gc :no-frame)
  (mezzano.lap.arm64:ret))

(defun bignum-sign-32 (bignum)
  (let* ((len (%n-bignum-fragments bignum))
         (final (%object-ref-unsigned-byte-32 bignum (1- (* len 2)))))
    (if (logtest final #x80000000)
        #xFFFFFFFF
        #x00000000)))

(defun %%canonicalize-bignum (bignum)
  (let ((len (%n-bignum-fragments bignum)))
    ;; Zero-size bignums are zero.
    (when (zerop len)
      (return-from %%canonicalize-bignum 0))
    ;; Read the sign bit, extending it out to 32 bits.
    (let ((sbit (bignum-sign-32 bignum)))
      ;; Now loop, until there are either no more redundant sign-extension fragments or until the length is zero.
      (loop
         (when (not (and (eql (%object-ref-unsigned-byte-32 bignum (- (* len 2) 1)) sbit)
                         (eql (%object-ref-unsigned-byte-32 bignum (- (* len 2) 2)) sbit)))
           (return))
         (when (not (zerop (logand #x80000000
                                   (logxor (%object-ref-unsigned-byte-32 bignum (- (* len 2) 3))
                                           sbit))))
           (return))
         (when (eql len 1)
           (return))
         (decf len))
      (cond ((eql len 0)
             0)
            ((and (eql len 1)
                  (let* ((high-bits (%object-ref-unsigned-byte-32 bignum 1))
                         (sign-bits (ash high-bits (- (- 32 (1+ +n-fixnum-bits+))))))
                    ;; Sign bits must all match.
                    (or (eql sign-bits 0)
                        (eql sign-bits (1- (ash 1 (1+ +n-fixnum-bits+)))))))
             ;; Convert to fixnum.
             (logior (%object-ref-unsigned-byte-32 bignum 0)
                     (ash (%object-ref-signed-byte-32 bignum 1) 32)))
            ((eql len (%n-bignum-fragments bignum))
             ;; Bignum same size. Keep as-is.
             bignum)
            (t
             ;; Bignum shortened, allocate a new one and copy the data over.
             (let ((new (%make-bignum-of-length len)))
               (dotimes (i len)
                 (setf (%object-ref-unsigned-byte-32 new (* i 2))
                       (%object-ref-unsigned-byte-32 bignum (* i 2)))
                 (setf (%object-ref-unsigned-byte-32 new (1+ (* i 2)))
                       (%object-ref-unsigned-byte-32 bignum (1+ (* i 2)))))
               new))))))

(defun operate-on-bignum (x y length-fn operation)
  (let* ((len-x (%n-bignum-fragments x))
         (len-y (%n-bignum-fragments y))
         (result (%make-bignum-of-length (funcall length-fn len-x len-y))))
    ;; Fragments are 64-bits wide, but the compiler will generate more bignums
    ;; trying to read a full 64-bit fragment. Operate on 32-bit pairs instead.
    (loop
       for i below (min len-x len-y)
       for lo from 0 by 2
       for hi = (1+ lo)
       do
         (setf (%object-ref-unsigned-byte-32 result lo)
               (funcall operation
                        (%object-ref-unsigned-byte-32 x lo)
                        (%object-ref-unsigned-byte-32 y lo)))
         (setf (%object-ref-unsigned-byte-32 result hi)
               (funcall operation
                        (%object-ref-unsigned-byte-32 x hi)
                        (%object-ref-unsigned-byte-32 y hi))))
    ;; Preserve operand order!
    (cond ((< len-x len-y)
           ;; Deal with the length mismatch, sign extending X to match Y.
           (loop
              with x-sign-extension = (bignum-sign-32 x)
              for i below (- len-y len-x)
              for lo from (* len-x 2) by 2
              for hi = (1+ lo)
              do
                (setf (%object-ref-unsigned-byte-32 result lo)
                      (funcall operation
                               x-sign-extension
                               (%object-ref-unsigned-byte-32 y lo)))
                (setf (%object-ref-unsigned-byte-32 result hi)
                      (funcall operation
                               x-sign-extension
                               (%object-ref-unsigned-byte-32 y hi)))))
          ((< len-y len-x)
           ;; Deal with the length mismatch, sign extending Y to match X.
           (loop
              with y-sign-extension = (bignum-sign-32 y)
              for i below (- len-x len-y)
              for lo from (* len-y 2) by 2
              for hi = (1+ lo)
              do
                (setf (%object-ref-unsigned-byte-32 result lo)
                      (funcall operation
                               (%object-ref-unsigned-byte-32 x lo)
                               y-sign-extension))
                (setf (%object-ref-unsigned-byte-32 result hi)
                      (funcall operation
                               (%object-ref-unsigned-byte-32 x hi)
                               y-sign-extension)))))
    result))

(defun %%bignum-+/- (x y op)
  (let* ((carry 0)
         (overflow nil)
         (size nil)
         (result (operate-on-bignum x y
                                    (lambda (len-x len-y)
                                      (setf size (1+ (max len-x len-y))))
                                    (lambda (a b)
                                      (let ((result (funcall op a b carry)))
                                        (setf overflow (logtest (logand (logxor result a)
                                                                        (logxor result b))
                                                               #x80000000))
                                        (setf carry (ldb (byte 1 32) result))
                                        (ldb (byte 32 0) result))))))
    (let* ((sign (cond (overflow
                        ;; On overflow, extend the carry bit out and populate the last fragment with that.
                        carry)
                       (t
                        ;; Otherwise, sign-extend the second-last fragment out.
                        (ash (%object-ref-unsigned-byte-32 result (- (* size 2) 3)) -31))))
           (sign-bits (if (eql sign 0)
                          #x00000000
                          #xFFFFFFFF)))
      (setf (%object-ref-unsigned-byte-32 result (- (* size 2) 2)) sign-bits
            (%object-ref-unsigned-byte-32 result (- (* size 2) 1)) sign-bits)
      (values result
              overflow
              sign))))

(defun %%bignum-+ (x y)
  (%%canonicalize-bignum (%%bignum-+/- x y (lambda (a b cin) (+ a b cin)))))

(defun %%bignum-- (x y)
  (%%canonicalize-bignum (%%bignum-+/- x y (lambda (a b cin) (- a (+ b cin))))))

(defun %%bignum-< (x y)
  (let* ((len-x (%n-bignum-fragments x))
         (len-y (%n-bignum-fragments y))
         (sign-x (ash (%object-ref-unsigned-byte-32 x (- (* len-x 2) 1)) -31))
         (sign-y (ash (%object-ref-unsigned-byte-32 y (- (* len-y 2) 1)) -31)))
    ;; Check sign bits. If they differ, then one is obviously less than the other.
    (when (not (zerop (logxor sign-x sign-y)))
      (return-from %%bignum-<
        (not (zerop sign-x))))
    ;; Same sign, check lengths.
    (when (not (eql len-x len-y))
      (cond ((zerop sign-x)
             ;; Non-negative.
             (return-from %%bignum-< (< len-x len-y)))
            (t
             ;; Negative.
             (return-from %%bignum-< (< len-y len-x)))))
    ;; Same length, same sign. Subtract them and examine the result.
    (multiple-value-bind (val overflow sign)
        (%%bignum-+/- x y (lambda (a b cin) (- a (+ b cin))))
      (declare (ignore val))
      (when overflow
        (setf sign (logxor sign 1)))
      ;; overflow xor sign == 1.
      (not (zerop (logxor (if overflow 1 0)
                          sign))))))

(defun %%bignum-= (x y)
  (let* ((len-x (%n-bignum-fragments x))
         (len-y (%n-bignum-fragments y))
         (sign-x (ash (%object-ref-unsigned-byte-32 x (- (* len-x 2) 1)) -31))
         (sign-y (ash (%object-ref-unsigned-byte-32 y (- (* len-y 2) 1)) -31)))
    ;; Check sign bits. If they differ, then one is obviously less than the other.
    (when (not (zerop (logxor sign-x sign-y)))
      (return-from %%bignum-= nil))
    ;; Same sign, check lengths.
    (when (not (eql len-x len-y))
      (return-from %%bignum-= nil))
    ;; Same length, same sign. Compare fragment-by-fragment.
    (operate-on-bignum x y
                       (lambda (len-x len-y)
                         (max len-x len-y))
                       (lambda (a b)
                         (when (not (eql a b))
                           (return-from %%bignum-= nil))
                         0))
    t))

(defun %%bignum-logand (x y)
  (%%canonicalize-bignum
   (operate-on-bignum x y
                      (lambda (len-x len-y)
                        (max len-x len-y))
                      (lambda (a b)
                        (logand a b)))))

(defun %%bignum-logior (x y)
  (%%canonicalize-bignum
   (operate-on-bignum x y
                      (lambda (len-x len-y)
                        (max len-x len-y))
                      (lambda (a b)
                        (logior a b)))))

(defun %%bignum-logxor (x y)
  (%%canonicalize-bignum
   (operate-on-bignum x y
                      (lambda (len-x len-y)
                        (max len-x len-y))
                      (lambda (a b)
                        (logxor a b)))))

(defun %%bignum-right-shift (bignum count)
  (assert (<= count 32))
  (let* ((len (%n-bignum-fragments bignum))
         (result (%make-bignum-of-length len))
         (last-word (bignum-sign-32 bignum)))
    (dotimes (i (* len 2))
      (let* ((index (1- (- (* len 2) i)))
             (word (%object-ref-unsigned-byte-32 bignum index)))
        (setf (%object-ref-unsigned-byte-32 result index)
              (logior (ash (ldb (byte count 0) last-word) (- 32 count))
                      (ldb (byte (- 32 count) count) word)))
        (setf last-word word)))
    (%%canonicalize-bignum result)))

(defun %%bignum-left-shift (bignum count)
  (dotimes (i count
            bignum)
    (setf bignum (+ bignum bignum))))
