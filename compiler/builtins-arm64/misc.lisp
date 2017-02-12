;;;; Copyright (c) 2016 Henry Harrington <henry.harrington@gmail.com>
;;;; This code is licensed under the MIT license.

(in-package :mezzano.compiler.codegen.arm64)

;;; EQ.

(defbuiltin eq (x y) ()
  ;; Ensure constants are on the right-hand side.
  (when (quoted-constant-p x)
    (rotatef x y))
  (cond ((quoted-constant-p y)
         (let ((constant (second y)))
           (load-in-reg :x0 x t)
           (cond
             ((and (integerp constant)
                   (<= -4095 (fixnum-to-raw constant) 4095))
              (if (minusp constant)
                  (emit `(lap:adds :xzr :x0 ,(fixnum-to-raw (- constant))))
                  (emit `(lap:subs :xzr :x0 ,(fixnum-to-raw constant)))))
             ((null constant)
              (emit `(lap:subs :xzr :x0 :x26)))
             (t
              (load-in-reg :x1 y t)
              (emit `(lap:subs :xzr :x0 :x1))))
           (predicate-result :eq)))
        (t (load-in-reg :x0 y t)
           (load-in-reg :x1 x t)
           (emit `(lap:subs :xzr :x0 :x1))
           (predicate-result :eq))))

;;; Constructing and deconstructing Lisp values.

(defbuiltin sys.int::%%assemble-value (address tag) ()
  (load-in-reg :x9 tag t)
  (load-in-reg :x0 address t)
  (smash-x0)
  (emit `(lap:add :x9 :xzr :x9 :lsr ,sys.int::+n-fixnum-bits+)
        `(lap:add :x0 :xzr :x0 :asr ,sys.int::+n-fixnum-bits+)
        `(lap:orr :x0 :x0 :x9))
  (setf *x0-value* (list (gensym))))

(defbuiltin sys.int::%pointer-field (value) ()
  (load-in-reg :x0 value t)
  (smash-x0)
  (emit `(lap:and :x0 :x0 -16)
        `(lap:add :x0 :xzr :x0 :asr ,(- 4 sys.int::+n-fixnum-bits+)))
  (setf *x0-value* (list (gensym))))

(defbuiltin sys.int::%tag-field (value) ()
  (load-in-reg :x0 value t)
  (smash-x0)
  (emit `(lap:add :x0 :xzr :x0 :lsl ,sys.int::+n-fixnum-bits+)
        `(lap:and :x0 :x0 ,(ash (1- (ash 1 4)) sys.int::+n-fixnum-bits+)))
  (setf *x0-value* (list (gensym))))

(defbuiltin sys.int::lisp-object-address (value) ()
  (load-in-reg :x0 value t)
  (smash-x0)
  ;; Convert to fixnum.
  (emit `(lap:add :x0 :xzr :x0 :lsl ,sys.int::+n-fixnum-bits+))
  (setf *x0-value* (list (gensym))))

;;; Grovelling in the machine.

(defbuiltin system.internals::read-frame-pointer () (nil)
  (smash-x0)
  (emit `(lap:add :x0 :xzr :x29 :lsl ,sys.int::+n-fixnum-bits+))
  (setf *x0-value* (list (gensym))))

;;; Cold-generator-supplied support objects.

(defmacro define-support-object (name symbol)
  (let ((predicate-name (intern (format nil "~A-P" name) (symbol-package name))))
    `(progn
       (defbuiltin ,name () ()
         (smash-x0)
         (load-literal :x0 ',symbol)
         (setf *x0-value* (list (gensym))))
       (defbuiltin ,predicate-name (value) ()
         (load-in-x0 value t)
         (load-literal :x9 ',symbol)
         (emit `(lap:subs :xzr :x0 :x9))
         (predicate-result :eq)))))

(define-support-object sys.int::%unbound-value :unbound-value)
(define-support-object sys.int::%undefined-function :undefined-function)
(define-support-object sys.int::%closure-trampoline :closure-trampoline)
(define-support-object sys.int::%funcallable-instance-trampoline :funcallable-instance-trampoline)

;;; Fixed-size dynamic-extent object creation.

(defbuiltin sys.c::make-dx-closure (code env) (nil)
  (smash-x0)
  (let ((slots (allocate-control-stack-slots 4 t)))
    (load-in-reg :x1 code t)
    (load-in-reg :x2 env t)
    (load-literal :x9 (control-stack-frame-offset (+ slots 4 -1)))
    (emit `(lap:add :x9 :x29 :x9))
    ;; Function tag, flags and MC size.
    (load-literal :x10 (logior (ash 3 sys.int::+object-data-shift+)
                               (ash sys.int::+object-tag-closure+
                                    sys.int::+object-type-shift+)))
    (emit `(lap:str :w10 (:x9)))
    ;; Constant pool size and slot count.
    (load-literal :x10 #x00000002)
    (emit `(lap:str :w10 (:x9 4)))
    ;; Entry point is CODE's entry point.
    (emit-object-load :x5 :x1 :slot 0)
    (emit `(lap:str :x5 (:x9 8)))
    ;; Clear constant pool.
    (emit `(lap:str :x26 (:x9 16))
          `(lap:str :x26 (:x9 24)))
    ;; Generate pointer.
    (emit `(lap:add :x0 :x9 ,sys.int::+tag-object+))
    ;; Initiaize constant pool.
    (emit-object-store :x1 :x0 :slot 1)
    (emit-object-store :x2 :x0 :slot 2))
  (setf *x0-value* (list (gensym))))

(defbuiltin mezzano.runtime::fast-symbol-value-cell (symbol) ()
  (smash-x0)
  (let ((cache-miss (gensym "SYMBOL-CACHE-MISS"))
        (resume (gensym "RESUME")))
    (emit-trailer (cache-miss)
      ;; Call the slow function.
      (emit `(lap:orr :x0 :xzr :x1))
      (call-support-function 'mezzano.runtime::symbol-value-cell 1)
      ;; Log a cache miss.
      (emit-object-load :x9 :x28 :slot 23) ; miss count
      (emit `(lap:add :x9 :x9 ,(ash 1 sys.int::+n-fixnum-bits+)))
      (emit-object-store :x9 :x28 :slot 23) ; miss count
      ;; Recompute the hash.
      (emit-object-load :x9 :x0 :slot sys.int::+symbol-value-cell-symbol+)
      (emit `(lap:add :x9 :xzr :x9 :lsr 1)
            `(lap:and :x9 :x9 ,(ash (1- 128) 3)))
      ;; Write the entry into the cache.
      (emit `(lap:add :x9 :x9 ,(object-slot-displacement 128))
            `(lap:str :x0 (:x28 :x9)))
      ;; Done.
      (emit `(lap:b ,resume)))
    (load-in-reg :x1 symbol t)
    ;; Compute symbol hash. Symbols are wired, so use the address.
    ;; Ignore the low 4 bits, but scale by 8.
    (emit `(lap:add :x9 :xzr :x1 :lsr 1)
          `(lap:and :x9 :x9 ,(ash (1- 128) 3)))
    ;; Load cache entry.
    (emit `(lap:add :x9 :x9 ,(object-slot-displacement 128))
          `(lap:ldr :x0 (:x28 :x9)))
    ;; Do symbols match?
    ;; Be careful here. The entry may be 0.
    (emit `(lap:cbz :x0 ,cache-miss))
    (emit-object-load :x2 :x0 :slot sys.int::+symbol-value-cell-symbol+)
    (emit `(lap:subs :xzr :x1 :x2)
          `(lap:b.ne ,cache-miss))
    ;; Cache hit. Log.
    (emit-object-load :x9 :x28 :slot 22) ; miss count
    (emit `(lap:add :x9 :x9 ,(ash 1 sys.int::+n-fixnum-bits+)))
    (emit-object-store :x9 :x28 :slot 22) ; miss count
    (emit resume))
  (setf *x0-value* (list (gensym))))
