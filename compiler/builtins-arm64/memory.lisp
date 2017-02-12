;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

;;;; Builtin functions for accessing memory.

(in-package :mezzano.compiler.codegen.arm64)

;;; Direct access to memory.
;;; (MEMREF-type base offset) directly accesses memory at BASE + OFFSET * type-width.
;;; Valid types are: (un)signed-byte-{8,16,32,64} and t.
;;; MEMREF-T reads or writes a Lisp object.

(defmacro define-u-b-memref (name width read-op write-op)
  `(progn
     (defbuiltin ,name (base offset) ()
       (load-in-reg :x1 base t)
       (fixnum-check :x1)
       (load-in-reg :x0 offset t)
       (fixnum-check :x0)
       (smash-x0)
       ;; BASE to raw integer.
       (emit '(lap:add :x9 :xzr :x1 :asr ,sys.int::+n-fixnum-bits+))
       ;; Convert OFFST to unboxed integer and scale appropriately.
       ,(ecase width
          (1
           `(emit `(lap:add :x10 :xzr :x0 :asr ,sys.int::+n-fixnum-bits+)))
          (2
           `(emit `(lap:add :x10 :xzr :x0)))
          (4
           `(emit `(lap:add :x10 :xzr :x0 :lsl 1))))
       ;; Read
       (emit '(,read-op :w9 (:x9 :x10)))
       ;; Convert to fixnum.
       (emit '(lap:add :x0 :xzr :x9 :lsl ,sys.int::+n-fixnum-bits+))
       (setf *x0-value* (list (gensym))))
     (defbuiltin (setf ,name) (new-value base offset) ()
       (let ((type-error-label (gensym)))
         (emit-trailer (type-error-label)
           (raise-type-error :x0 '(unsigned-byte ,(* width 8))))
         (load-in-reg :x1 base t)
         (fixnum-check :x1)
         (load-in-reg :x2 offset t)
         (fixnum-check :x2)
         (load-in-x0 new-value t)
         (emit `(lap:ands :xzr :x0 ,',sys.int::+fixnum-tag-mask+)
               `(lap:b.ne ,type-error-label)
               `(lap:ands :xzr :x0 ,',(ash (lognot (1- (ash 1 (* width 8)))) sys.int::+n-fixnum-bits+))
               `(lap:b.ne ,type-error-label)
               ;; Convert to raw integer.
               `(lap:add :x9 :xzr :x0 :lsr ,',sys.int::+n-fixnum-bits+))
         ;; BASE to raw integer.
         (emit '(lap:add :x11 :xzr :x1 :asr ,sys.int::+n-fixnum-bits+))
         ;; Convert OFFST to unboxed integer and scale appropriately.
         ,(ecase width
            (1
             `(emit `(lap:add :x10 :xzr :x2 :asr ,sys.int::+n-fixnum-bits+)))
            (2
             `(emit `(lap:add :x10 :xzr :x2)))
            (4
             `(emit `(lap:add :x10 :xzr :x2 :lsl 1))))
         ;; Write
         (emit '(,write-op :w9 (:x10 :x11)))
         *x0-value*))))

(define-u-b-memref sys.int::%memref-unsigned-byte-8  1 lap:ldrb lap:strb)
(define-u-b-memref sys.int::%memref-unsigned-byte-16 2 lap:ldrh lap:strh)
(define-u-b-memref sys.int::%memref-unsigned-byte-32 4 lap:ldr  lap:str)

(defbuiltin sys.int::%memref-unsigned-byte-64 (base offset) ()
  (load-in-reg :x0 base t)
  (fixnum-check :x0)
  (load-in-reg :x1 offset t)
  (fixnum-check :x1)
  (smash-x0)
  ;; Convert to raw integers.
  (emit `(lap:add :x9 :xzr :x0 :asr ,sys.int::+n-fixnum-bits+)
        `(lap:add :x5 :xzr :x1 :asr ,sys.int::+n-fixnum-bits+))
  ;; Read.
  (emit `(lap:ldr :x10 (:x9 :x5 :scale)))
  (box-unsigned-byte-64-x10)
  (setf *x0-value* (list (gensym))))

(defbuiltin (setf sys.int::%memref-unsigned-byte-64) (new-value base offset) ()
  (let ((type-error-label (gensym))
        (bignum-path (gensym "mr-ub64-bignum"))
        (len-2-bignum (gensym "mr-ub64-len-2-bignum"))
        (value-extracted (gensym "mr-ub64-value-extracted")))
    (emit-trailer (bignum-path)
      ;; Check for bignumness.
      (emit `(lap:and :x11 :x0 #b1111)
            `(lap:subs :xzr :x11 ,sys.int::+tag-object+)
            `(lap:b.ne ,type-error-label))
      (emit-object-load :x11 :x0 :slot -1)
      (emit `(lap:and :x12 :x11 ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                      sys.int::+object-type-shift+))
            `(lap:subs :xzr :x12 ,(ash sys.int::+object-tag-bignum+
                                       sys.int::+object-type-shift+))
            `(lap:b.ne ,type-error-label)
            `(lap:add :x11 :xzr :x11 :lsr 8)
            ;; X11 = bignum length.
            `(lap:subs :xzr :x11 2)
            `(lap:b.eq ,len-2-bignum)
            ;; Not length 2, must be length 1.
            `(lap:subs :xzr :x11 1)
            `(lap:b.ne ,type-error-label))
      (emit-object-load :x10 :x0 :slot 0)
            ;; And the sign bit must be clear.
      (emit `(lap:ands :xzr :x10 :x10)
            `(lap:b.mi ,type-error-label)
            `(lap:b ,value-extracted))
            ;; Length 2 bignums must have the high word be 0.
      (emit len-2-bignum)
      (emit-object-load :x11 :x0 :slot 1)
      (emit `(lap:cbnz :x11 ,type-error-label))
      (emit-object-load :x10 :x0 :slot 0)
      (emit `(lap:b ,value-extracted)
            type-error-label)
      (raise-type-error :x0 '(unsigned-byte 64)))
    (load-in-reg :x2 base t)
    (fixnum-check :x2)
    (load-in-reg :x1 offset t)
    (fixnum-check :x1)
    (load-in-x0 new-value t)
    (emit `(lap:add :x9 :xzr :x2 :asr ,sys.int::+n-fixnum-bits+)
          `(lap:add :x5 :xzr :x1 :asr ,sys.int::+n-fixnum-bits+)
          `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,bignum-path)
          `(lap:subs :xzr :x0 0)
          `(lap:b.lt ,type-error-label)
          ;; Convert to raw integer.
          `(lap:add :x10 :xzr :x0 :asr ,sys.int::+n-fixnum-bits+)
          value-extracted
          ;; Write.
          `(lap:str :x10 (:x9 :x5 :scale)))
    *x0-value*))

(defbuiltin sys.int::%memref-signed-byte-64 (base offset) ()
  (let ((overflow-label (gensym))
        (resume (gensym)))
    (emit-trailer (overflow-label)
      (emit `(lap:ldr :x7 (:function sys.int::%%make-bignum-64-x10)))
      (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
      (emit `(lap:blr :x9)
            `(lap:b ,resume)))
    (load-in-reg :x0 base t)
    (fixnum-check :x0)
    (load-in-reg :x1 offset t)
    (fixnum-check :x1)
    (smash-x0)
    ;; Convert to raw integers, scaling offset.
    (emit `(lap:add :x10 :xzr :x1 :lsl 2))
    (emit `(lap:add :x9 :xzr :x0 :asr ,sys.int::+n-fixnum-bits+))
    ;; Read.
    (emit `(lap:ldr :x10 (:x10 :x9))
          ;; Convert to fixnum & check for signed overflow.
          ;; Assumes fixnum size of 1!
          `(lap:adds :x0 :x10 :x10)
          `(lap:b.vs ,overflow-label)
          resume)
    (setf *x0-value* (list (gensym)))))

(defbuiltin (setf sys.int::%memref-signed-byte-64) (new-value base offset) ()
  (let ((type-error-label (gensym))
        (bignum-path (gensym "mr-sb64-bignum"))
        (value-extracted (gensym "mr-sb64-value-extracted")))
    (emit-trailer (bignum-path)
      ;; Check for bignumness.
      (emit `(lap:and :x9 :x0 #b1111)
            `(lap:subs :xzr :x9 ,sys.int::+tag-object+)
            `(lap:b.ne ,type-error-label))
      (emit-object-load :x11 :x0 :slot -1)
      (emit `(lap:and :x9 :x11 ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                      sys.int::+object-type-shift+))
            `(lap:subs :xzr :x9 ,(ash sys.int::+object-tag-bignum+
                                       sys.int::+object-type-shift+))
            `(lap:b.ne ,type-error-label)
            `(lap:add :x11 :xzr :x11 :lsr 8)
            ;; Must be length 1.
            `(lap:subs :xzr :x11 1)
            `(lap:b.ne ,type-error-label))
      (emit-object-load :x10 :x0 :slot 0)
      (emit `(lap:b ,value-extracted)
            type-error-label)
      (raise-type-error :x0 '(signed-byte 64)))
    (load-in-reg :x2 base t)
    (fixnum-check :x2)
    (load-in-reg :x1 offset t)
    (fixnum-check :x1)
    (load-in-x0 new-value t)
    (emit `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,bignum-path)
          ;; Convert to raw integer.
          `(lap:add :x10 :xzr :x0 :asr ,sys.int::+n-fixnum-bits+)
          value-extracted)
    ;; Convert base/offset to unboxed integers.
    (emit `(lap:add :x9 :xzr :x1 :lsl 2)
          `(lap:add :x11 :xzr :x2 :asr #.sys.int::+n-fixnum-bits+))
    ;; Write.
    (emit `(lap:str :x10 (:x11 :x9)))
    *x0-value*))

(defbuiltin sys.int::%memref-t (base offset) ()
  (load-in-reg :x1 base t)
  (fixnum-check :x1)
  (load-in-reg :x0 offset t)
  (fixnum-check :x0)
  (smash-x0)
  ;; Convert to raw integers, scaling offset.
  (emit `(lap:add :x10 :xzr :x0 :lsl 2))
  (emit `(lap:add :x9 :xzr :x1 :asr ,sys.int::+n-fixnum-bits+))
  ;; Read.
  (emit `(lap:ldr :x0 (:x9 :x10)))
  (setf *x0-value* (list (gensym))))

(defbuiltin (setf sys.int::%memref-t) (new-value base offset) ()
  (load-in-reg :x1 base t)
  (fixnum-check :x1)
  ;; Convert to raw integers, scaling offset.
  (emit `(lap:add :x9 :xzr :x1 :asr ,sys.int::+n-fixnum-bits+))
  (load-in-reg :x1 offset t)
  (fixnum-check :x1)
  (emit `(lap:add :x10 :xzr :x1 :lsl 2))
  (load-in-x0 new-value t)
   ;; Write.
  (emit `(lap:str :x0 (:x9 :x10)))
  *x0-value*)

;;; Access to slots in homogeneous objects.
;;; (%OBJECT-REF-type object slot) accesses the value in OBJECT at SLOT.
;;; Slots are stored linearly after the object's header and are assumed to all be the same size.
;;; This does not account for the additional alignment constraints of float arrays & similar.
;;; Valid types are: (un)signed-byte-{8,16,32,64} and t.
;;; %OBJECT-REF-T reads or writes a Lisp object.
;;; These functions are only valid on objects tagged with +TAG-OBJECT+, and do not test this.
;;; These functions are GC safe, while accessing a slot using MEMREF-type and
;;; LISP-OBJECT-ADDRESS is not.

(defmacro define-u-b-object-ref (name width read-op write-op)
  `(progn
     (defbuiltin ,name (object offset) ()
       (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (unless constant-offset
           (load-in-reg :x2 offset t)
           (fixnum-check :x2)
           ;; Convert to unboxed integer and scale appropriately.
           ,(ecase width
              (1
               `(emit `(lap:add :x10 :xzr :x2 :asr ,sys.int::+n-fixnum-bits+)))
              (2
               `(emit `(lap:add :x10 :xzr :x2)))
              (4
               `(emit `(lap:add :x10 :xzr :x2 :lsl 1)))))
         (load-in-reg :x1 object t)
         (smash-x0)
         ;; Read.
         (cond (constant-offset
                (let ((disp (object-slot-displacement constant-offset ',width)))
                  (cond ((or (<= -256 disp 255)
                             (and (<= 0 disp 16380)
                                  (zerop (logand disp #b111))))
                         (emit `(,',read-op :w9 (:x1 ,disp))))
                        (t
                         (load-literal :x10 disp)
                         (emit `(,',read-op :w9 (:x1 :x10)))))))
               (t
                (emit `(lap:sub :x10 :x10 ,(- (+ 8 (- sys.int::+tag-object+)))))
                (emit `(,',read-op :w9 (:x1 :x10)))))
         ;; Convert to fixnum.
         (emit `(lap:add :x0 :xzr :x9 :lsl ,sys.int::+n-fixnum-bits+))
         (setf *x0-value* (list (gensym)))))
     (defbuiltin (setf ,name) (new-value object offset) ()
       (let ((type-error-label (gensym))
             (constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (emit-trailer (type-error-label)
           (raise-type-error :x0 '(unsigned-byte ,(* width 8))))
         (unless constant-offset
           (load-in-reg :x2 offset t)
           (fixnum-check :x2)
           ;; Convert to unboxed integer and scale appropriately.
           ,(ecase width
              (1
               `(emit `(lap:add :x10 :xzr :x2 :asr ,sys.int::+n-fixnum-bits+)))
              (2
               `(emit `(lap:add :x10 :xzr :x2)))
              (4
               `(emit `(lap:add :x10 :xzr :x2 :lsl 1)))))
         (load-in-reg :x1 object t)
         (load-in-x0 new-value t)
         (emit `(lap:ands :xzr :x0 ,',sys.int::+fixnum-tag-mask+)
               `(lap:b.ne ,type-error-label)
               `(lap:ands :xzr :x0 ,',(ash (lognot (1- (ash 1 (* width 8)))) sys.int::+n-fixnum-bits+))
               `(lap:b.ne ,type-error-label)
               ;; Convert to raw integer.
               `(lap:add :x9 :xzr :x0 :lsr ,',sys.int::+n-fixnum-bits+))
         ;; Write.
         (cond (constant-offset
                (let ((disp (object-slot-displacement constant-offset ',width)))
                  (cond ((or (<= -256 disp 255)
                             (and (<= 0 disp 16380)
                                  (zerop (logand disp #b111))))
                         (emit `(,',write-op :w9 (:x1 ,disp))))
                        (t
                         (load-literal :x10 disp)
                         (emit `(,',write-op :w9 (:x1 :x10)))))))
               (t
                (emit `(lap:sub :x10 :x10 ,(- (+ 8 (- sys.int::+tag-object+)))))
                (emit `(,',write-op :w9 (:x1 :x10)))))
         *x0-value*))))

(define-u-b-object-ref sys.int::%object-ref-unsigned-byte-8  1 lap:ldrb lap:strb)
(define-u-b-object-ref sys.int::%object-ref-unsigned-byte-16 2 lap:ldrh lap:strh)
(define-u-b-object-ref sys.int::%object-ref-unsigned-byte-32 4 lap:ldr  lap:str)

(defbuiltin sys.int::%object-ref-unsigned-byte-64 (object offset) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :x2 offset t)
      (fixnum-check :x2)
      ;; Convert to unboxed integer, and scale appropriately (* 8).
      (emit `(lap:add :x9 :xzr :x2 :lsl 2)))
    (load-in-reg :x0 object t)
    ;; Read.
    (cond (constant-offset
           (emit-object-load :x10 :x0 :slot constant-offset))
          (t
           (emit `(lap:sub :x9 :x9 ,(- (+ 8 (- sys.int::+tag-object+))))
                 `(lap:ldr :x10 (:x0 :x9)))))
    (box-unsigned-byte-64-x10)
    (setf *x0-value* (list (gensym)))))

(defbuiltin (setf sys.int::%object-ref-unsigned-byte-64) (new-value object offset) ()
  (let ((type-error-label (gensym))
        (bignum-path (gensym "alr-ub64-bignum"))
        (len-2-bignum (gensym "alr-ub64-len-2-bignum"))
        (value-extracted (gensym "alr-ub64-value-extracted"))
        (constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (emit-trailer (bignum-path)
      ;; Check for bignumness.
      (emit `(lap:and :x11 :x0 #b1111)
            `(lap:subs :xzr :x11 ,sys.int::+tag-object+)
            `(lap:b.ne ,type-error-label))
      (emit-object-load :x11 :x0 :slot -1)
      (emit `(lap:and :x9 :x11 ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                      sys.int::+object-type-shift+))
            `(lap:subs :xzr :x9 ,(ash sys.int::+object-tag-bignum+
                                       sys.int::+object-type-shift+))
            `(lap:b.ne ,type-error-label)
            `(lap:add :x11 :xzr :x11 :lsr 8)
            ;; X11 = bignum length.
            `(lap:subs :xzr :x11 2)
            `(lap:b.eq ,len-2-bignum)
            ;; Not length 2, must be length 1.
            `(lap:subs :xzr :x11 1)
            `(lap:b.ne ,type-error-label))
      (emit-object-load :x10 :x0 :slot 0)
            ;; And the sign bit must be clear.
      (emit `(lap:ands :xzr :x10 :x10)
            `(lap:b.mi ,type-error-label)
            `(lap:b ,value-extracted))
            ;; Length 2 bignums must have the high word be 0.
      (emit len-2-bignum)
      (emit-object-load :x11 :x0 :slot 1)
      (emit `(lap:cbnz :x11 ,type-error-label))
      (emit-object-load :x10 :x0 :slot 0)
      (emit `(lap:b ,value-extracted)
            type-error-label)
      (raise-type-error :x0 '(unsigned-byte 64)))
    (unless constant-offset
      (load-in-reg :x2 offset t)
      (fixnum-check :x2)
      ;; Convert to unboxed integer, and scale appropriately (* 8).
      (emit `(lap:add :x9 :xzr :x2 :lsl 2)))
    (load-in-reg :x1 object t)
    (load-in-x0 new-value t)
    (emit `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,bignum-path)
          ;; Make sure it is non-negative.
          `(lap:subs :xzr :x0 0)
          `(lap:b.lt ,type-error-label)
          ;; Convert to raw integer.
          `(lap:add :x10 :xzr :x0 :asr ,sys.int::+n-fixnum-bits+)
          value-extracted)
    ;; Write.
    (cond (constant-offset
           (emit-object-store :x10 :x1 :slot constant-offset))
          (t
           (emit `(lap:sub :x9 :x9 ,(- (+ 8 (- sys.int::+tag-object+))))
                 `(lap:str :x10 (:x1 :x9)))))
    *x0-value*))

(defmacro define-s-b-object-ref (name width read-op write-op)
  `(progn
     (defbuiltin ,name (object offset) ()
       (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (unless constant-offset
           (load-in-reg :x2 offset t)
           (fixnum-check :x2)
           ;; Convert to unboxed integer and scale appropriately.
           ,(ecase width
                   (1
                    `(emit `(lap:add :x10 :xzr :x2 :asr ,sys.int::+n-fixnum-bits+)))
                   (2
                    `(emit `(lap:add :x10 :xzr :x2)))
                   (4
                    `(emit `(lap:add :x10 :xzr :x2 :lsl 1)))))
         (load-in-reg :x1 object t)
         (smash-x0)
         ;; Read.
         (cond (constant-offset
                (let ((disp (object-slot-displacement constant-offset ',width)))
                  (cond ((or (<= -256 disp 255)
                             (and (<= 0 disp 16380)
                                  (zerop (logand disp #b111))))
                         (emit `(,',read-op :x9 (:x1 ,disp))))
                        (t
                         (load-literal :x10 disp)
                         (emit `(,',read-op :x9 (:x1 :x10)))))))
               (t
                (emit `(lap:sub :x10 :x10 ,(- (+ 8 (- sys.int::+tag-object+)))))
                (emit `(,',read-op :x9 (:x1 :x10)))))
         ;; Convert to fixnum.
         (emit `(lap:add :x0 :xzr :x9 :lsl ,sys.int::+n-fixnum-bits+))
         (setf *x0-value* (list (gensym)))))
     (defbuiltin (setf ,name) (new-value object offset) ()
       (let ((type-error-label (gensym))
             (constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                                   (second offset))))
         (emit-trailer (type-error-label)
           (raise-type-error :x0 '(signed-byte ,(* width 8))))
         (unless constant-offset
           (load-in-reg :x2 offset t)
           (fixnum-check :x2)
           ;; Convert to unboxed integer and scale appropriately.
           ,(ecase width
                   (1
                    `(emit `(lap:add :x10 :xzr :x2 :asr ,sys.int::+n-fixnum-bits+)))
                   (2
                    `(emit `(lap:add :x10 :xzr :x2)))
                   (4
                    `(emit `(lap:add :x10 :xzr :x2 :lsl 1)))))
         (load-in-reg :x1 object t)
         (load-in-x0 new-value t)
         (emit `(lap:ands :xzr :x0 ,',sys.int::+fixnum-tag-mask+)
               `(lap:b.ne ,type-error-label)
               ;; Convert to raw integer.
               `(lap:add :x9 :xzr :x0 :lsr ,',sys.int::+n-fixnum-bits+))
         (load-literal :x11 ,(ash 1 (1- (* width 8))))
         (emit `(lap:add :x11 :x9 :x11)
               `(lap:ands :xzr :x0 ,',(lognot (1- (ash 1 (* width 8)))))
               `(lap:b.ne ,type-error-label))
         ;; Write.
         (cond (constant-offset
                (let ((disp (object-slot-displacement constant-offset ',width)))
                  (cond ((or (<= -256 disp 255)
                             (and (<= 0 disp 16380)
                                  (zerop (logand disp #b111))))
                         (emit `(,',write-op :w9 (:x1 ,disp))))
                        (t
                         (load-literal :x10 disp)
                         (emit `(,',write-op :w9 (:x1 :x10)))))))
               (t
                (emit `(lap:sub :x10 :x10 ,(- (+ 8 (- sys.int::+tag-object+)))))
                (emit `(,',write-op :w9 (:x1 :x10)))))
         *x0-value*))))

(define-s-b-object-ref sys.int::%object-ref-signed-byte-8  1 lap:ldrsb lap:strb)
(define-s-b-object-ref sys.int::%object-ref-signed-byte-16 2 lap:ldrsh lap:strh)
(define-s-b-object-ref sys.int::%object-ref-signed-byte-32 4 lap:ldrsw lap:str)

(defbuiltin sys.int::%object-ref-signed-byte-64 (object offset) ()
  (let ((overflow-label (gensym))
        (resume (gensym))
        (constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (emit-trailer (overflow-label)
      ;; Call the helper.
      (emit `(lap:ldr :x7 (:function sys.int::%%make-bignum-64-x10)))
      (emit-object-load :x9 :x7 :slot sys.int::+fref-entry-point+)
      (emit `(lap:blr :x9)
            `(lap:b ,resume)))
    (unless constant-offset
      (load-in-reg :x2 offset t)
      (fixnum-check :x2)
      ;; Convert to unboxed integer, and scale appropriately (* 8).
      (emit `(lap:add :x9 :xzr :x2 :lsl 2)))
    (load-in-reg :x0 object t)
    (smash-x0)
    ;; Read.
    (cond (constant-offset
           (emit-object-load :x10 :x0 :slot constant-offset))
          (t
           (emit `(lap:sub :x9 :x9 ,(- (+ 8 (- sys.int::+tag-object+))))
                 `(lap:ldr :x10 (:x0 :x9)))))
    ;; Convert to fixnum & check for signed overflow.
    ;; Assumes fixnum size of 1!
    (emit `(lap:adds :x0 :x10 :x10)
          `(lap:b.vs ,overflow-label)
          resume)
    (setf *x0-value* (list (gensym)))))

(defbuiltin (setf sys.int::%object-ref-signed-byte-64) (new-value object offset) ()
  (let ((type-error-label (gensym))
        (bignum-path (gensym "alr-sb64-bignum"))
        (value-extracted (gensym "alr-sb64-value-extracted"))
        (constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (emit-trailer (bignum-path)
      ;; Check for bignumness.
      (emit `(lap:and :x9 :x0 #b1111)
            `(lap:subs :xzr :x9 ,sys.int::+tag-object+)
            `(lap:b.ne ,type-error-label))
      (emit-object-load :x11 :x0 :slot -1)
      (emit `(lap:and :x9 :x11 ,(ash (1- (ash 1 sys.int::+object-type-size+))
                                      sys.int::+object-type-shift+))
            `(lap:subs :xzr :x9 ,(ash sys.int::+object-tag-bignum+
                                       sys.int::+object-type-shift+))
            `(lap:b.ne ,type-error-label)
            `(lap:add :x11 :xzr :x11 :lsr 8)
            ;; Must be length 1.
            `(lap:subs :xzr :x11 1)
            `(lap:b.ne ,type-error-label))
      (emit-object-load :x10 :x0 :slot 0)
      (emit `(lap:b ,value-extracted)
            type-error-label)
      (raise-type-error :x0 '(signed-byte 64)))
    (unless constant-offset
      (load-in-reg :x2 offset t)
      (fixnum-check :x2)
      ;; Convert to unboxed integer, and scale appropriately (* 8).
      (emit `(lap:add :x9 :xzr :x2 :lsl 2)))
    (load-in-reg :x1 object t)
    (load-in-x0 new-value t)
    (emit `(lap:ands :xzr :x0 ,sys.int::+fixnum-tag-mask+)
          `(lap:b.ne ,bignum-path)
          ;; Convert to raw integer.
          `(lap:add :x10 :xzr :x0 :asr ,sys.int::+n-fixnum-bits+)
          value-extracted)
    ;; Write.
    (cond (constant-offset
           (emit-object-store :x10 :x1 :slot constant-offset))
          (t
           (emit `(lap:sub :x9 :x9 ,(- (+ 8 (- sys.int::+tag-object+))))
                 `(lap:str :x10 (:x1 :x9)))))
    *x0-value*))

(defbuiltin sys.int::%object-ref-t (object offset) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :x2 offset t)
      (fixnum-check :x2)
      ;; Convert to unboxed integer, and scale appropriately (* 8).
      (emit `(lap:add :x11 :xzr :x2 :lsl 2)))
    (load-in-reg :x0 object t)
    (smash-x0)
    ;; Read.
    (cond (constant-offset
           (emit-object-load :x0 :x0 :slot constant-offset))
          (t
           (emit `(lap:sub :x11 :x11 ,(- (+ 8 (- sys.int::+tag-object+))))
                 `(lap:ldr :x0 (:x0 :x11)))))
    (setf *x0-value* (list (gensym)))))

(defbuiltin (setf sys.int::%object-ref-t) (new-value object offset) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                              (second offset))))
    (unless constant-offset
      (load-in-reg :x2 offset t)
      (fixnum-check :x2)
      ;; Convert to unboxed integer.
      (emit `(lap:add :x9 :xzr :x2 :lsl 2)))
    (load-in-reg :x1 object t)
    (load-in-reg :x0 new-value t)
    ;; Write.
    (cond (constant-offset
           (emit-object-store :x0 :x1 :slot constant-offset))
          (t
           (emit `(lap:sub :x9 :x9 ,(- (+ 8 (- sys.int::+tag-object+))))
                 `(lap:str :x0 (:x1 :x9)))))
    *x0-value*))

;;; Atomic operations.
;;; These functions index into the object like %OBJECT-REF-T.
;;; There are no atomic functions that access memory like MEMREF.

;; Add DELTA to the slot at SLOT in OBJECT.
;; Returns the old value of the slot.
;; DELTA and the value of the slot must both be fixnums.
;; (defun fixnum-add (object slot delta)
;;   (prog1 (%object-ref-t object slot)
;;     (incf (%object-ref-t object slot) delta)))
(defbuiltin sys.int::%atomic-fixnum-add-object (object offset delta) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                              (second offset)))
        (label (gensym "LOOP")))
    (unless constant-offset
      (load-in-reg :x2 offset t)
      (fixnum-check :x2)
      ;; Convert to unboxed integer, with tag adjustment.
      (emit `(lap:add :x9 :xzr :x2 :lsl ,(- 3 sys.int::+n-fixnum-bits+))
            `(lap:sub :x9 :x9 ,(- (object-slot-displacement 0)))))
    (load-in-reg :x1 object t)
    (load-in-reg :x0 delta t)
    (fixnum-check :x0)
    (smash-x0)
    ;; Generate the address.
    (when constant-offset
      (load-literal :x9 (object-slot-displacement constant-offset)))
    (emit `(lap:add :x9 :x9 :x1))
    ;; Move to linked gc mode.
    ;; x9 is interior pointer into x1.
    (emit-gc-info :extra-registers :rax)
    ;; Add loop.
    (emit label
          ;; Load old value into X10
          `(lap:ldaxr :x10 (:x9))
          ;; Increment by delta. New value in X11.
          `(lap:add :x11 :x10 :x0)
          ;; Store linked new value, status in X11.
          `(lap:stlxr :w11 :x11 (:x9))
          ;; Retry on failure.
          `(lap:cbnz :x11 ,label))
    ;; Finish up.
    (emit-gc-info)
    (emit `(lap:orr :x0 :xzr :x10))
    (setf *x0-value* (list (gensym)))))

;; Set the value in SLOT to NEW, and return the old value.
;; (defun xchg (object slot new)
;;   (prog1 (%object-ref-t object slot)
;;     (setf (%object-ref-t object slot) new)))
(defbuiltin sys.int::%xchg-object (object offset new) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                              (second offset)))
        (label (gensym "LOOP")))
    (unless constant-offset
      (load-in-reg :x2 offset t)
      (fixnum-check :x2)
      ;; Convert to unboxed integer, with tag adjustment.
      (emit `(lap:add :x9 :xzr :x2 :lsl ,(- 3 sys.int::+n-fixnum-bits+))
            `(lap:sub :x9 :x9 ,(- (object-slot-displacement 0)))))
    (load-in-reg :x1 object t)
    (load-in-reg :x0 new t)
    (smash-x0)
    ;; Generate the address.
    (when constant-offset
      (load-literal :x9 (object-slot-displacement constant-offset)))
    (emit `(lap:add :x9 :x9 :x1))
    ;; Move to linked gc mode.
    ;; x9 is interior pointer into x1.
    (emit-gc-info :extra-registers :rax)
    ;; Add loop.
    (emit label
          ;; Load old value into X2
          `(lap:ldaxr :x2 (:x9))
          ;; Store linked new value, status in X11.
          `(lap:stlxr :w11 :x0 (:x9))
          ;; Retry on failure.
          `(lap:cbnz :x11 ,label))
    ;; Finish up.
    (emit-gc-info)
    (emit `(lap:orr :x0 :xzr :x2))
    (setf *x0-value* (list (gensym)))))

;; If the value in SLOT matches OLD, set it to NEW; otherwise do nothing.
;; Returns true as the primary value if the slot was modified, false otherwise.
;; Additionally returns the old value of SLOT as the second value.
;; (defun cas (object offset old new)
;;   (let ((slot-value (%object-ref-t object slot)))
;;     (values (cond ((eq slot-value old)
;;                    (setf (%object-ref-t object slot) new)
;;                    t)
;;                   (t nil))
;;             slot-value)))
(defbuiltin sys.int::%cas-object (object offset old new) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                              (second offset)))
        (loop-label (gensym "LOOP"))
        (out-label (gensym "OUT")))
    (unless constant-offset
      (load-in-reg :x2 offset t)
      (fixnum-check :x2)
      ;; Convert to unboxed integer, with tag adjustment.
      (emit `(lap:add :x9 :xzr :x2 :lsl ,(- 3 sys.int::+n-fixnum-bits+))
            `(lap:sub :x9 :x9 ,(- (object-slot-displacement 0)))))
    (load-in-reg :x1 object t)
    (load-in-reg :x0 new t)
    (load-in-reg :x2 old t)
    (smash-x0)
    ;; Generate the address.
    (when constant-offset
      (load-literal :x9 (object-slot-displacement constant-offset)))
    (emit `(lap:add :x9 :x9 :x1))
    ;; Move to linked gc mode.
    ;; x9 is interior pointer into x1.
    (emit-gc-info :extra-registers :rax)
    (emit `(lap:ldr :x4 (:constant t)))
    (emit loop-label
          ;; Load old value into X3
          `(lap:ldaxr :x3 (:x9))
          ;; Test against old value.
          `(lap:subs :xzr :x2 :x3)
          `(lap:csel.ne :x4 :x26 :x4)
          `(lap:b.ne ,out-label)
          ;; Store linked new value, status in X11.
          `(lap:stlxr :w11 :x0 (:x9))
          ;; Retry on failure.
          `(lap:cbnz :x11 ,loop-label))
    ;; Finish up.
    (emit-gc-info)
    ;; Return success and the old value.
    (emit out-label)
    (cond ((member *for-value* '(:multiple :tail))
           (emit `(lap:orr :x1 :xzr :x3)
                 `(lap:orr :x0 :xzr :x4))
           (load-constant :x5 2)
           :multiple)
          (t
           (emit `(lap:orr :x0 :xzr :x4))
           (setf *x0-value* (list (gensym)))))))

;; Similar to %CAS-OBJECT, but performs two CAS operations on adjacent slots.
;; Returns the two old slot values and a success boolean.
;; (defun dcas (object offset old-1 old-2 new-1 new-2)
;;   (let ((slot-value-1 (%object-ref-t object slot))
;;         (slot-value-2 (%object-ref-t object (1+ slot))))
;;     (values (cond ((and (eq slot-value-1 old-1)
;;                         (eq slot-value-2 old-2))
;;                    (setf (%object-ref-t object slot) new-1
;;                          (%object-ref-t object (1+ slot)) new-2)
;;                    t)
;;                   (t nil))
;;             slot-value-1
;;             slot-value-2)))
(defbuiltin sys.int::%dcas-object (object offset old-1 old-2 new-1 new-2) ()
  (let ((constant-offset (and (constant-type-p offset `(signed-byte ,(- 64 sys.int::+n-fixnum-bits+)))
                              (second offset)))
        (loop-label (gensym "LOOP"))
        (differ-label (gensym "DIFFER"))
        (out-label (gensym "OUT")))
    (unless constant-offset
      (load-in-reg :x2 offset t)
      (fixnum-check :x2)
      ;; Convert to unboxed integer, with tag adjustment.
      (emit `(lap:add :x9 :xzr :x2 :lsl ,(- 3 sys.int::+n-fixnum-bits+))
            `(lap:sub :x9 :x9 ,(- (object-slot-displacement 0)))))
    (load-in-reg :x1 object t)
    (load-in-reg :x0 new-1 t)
    (load-in-reg :x2 new-2 t)
    (load-in-reg :x3 old-1 t)
    (load-in-reg :x4 old-2 t)
    (smash-x0)
    ;; Generate the address.
    (when constant-offset
      (load-literal :x9 (object-slot-displacement constant-offset)))
    (emit `(lap:add :x9 :x9 :x1))
    ;; Move to linked gc mode.
    ;; x9 is interior pointer into x1.
    (emit-gc-info :extra-registers :rax)
    (emit loop-label
          ;; Load old values into X6 and X7
          `(lap:ldaxp :x6 :x7 (:x9))
          ;; Test against old value.
          `(lap:subs :xzr :x6 :x3)
          `(lap:b.ne ,differ-label)
          `(lap:subs :xzr :x7 :x4)
          `(lap:b.ne ,differ-label)
          ;; Store linked new value, status in X11.
          `(lap:stlxp :w11 :x0 :x2 (:x9))
          ;; Retry on failure.
          `(lap:cbnz :x11 ,loop-label))
    ;; Finish up.
    (emit-gc-info)
    ;; Return success and the old value.
    (emit `(lap:ldr :x0 (:constant t))
          `(lap:b ,out-label)
          differ-label
          `(lap:orr :x0 :xzr :x26)
          out-label)
    (cond ((member *for-value* '(:multiple :tail))
           (emit `(lap:orr :x1 :xzr :x6)
                 `(lap:orr :x2 :xzr :x7))
           (load-constant :x5 3)
           :multiple)
          (t
           (setf *x0-value* (list (gensym)))))))
